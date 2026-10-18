//! Applies every editing operation to every collection in the valid YAML
//! test suite cases and checks that the result is still sound.
//!
//! For each valid case, each document and each mapping and sequence in it,
//! every operation runs on a fresh parse. Afterwards:
//!
//! - `cst`: the tree passes `debug::validate_tree` and `debug::roundtrip_ok`;
//! - `shape`: the tree is the same as a re-parse of its text;
//! - `validator`: the validator accepts the output (skipped when it already
//!   rejects the input);
//! - `semantic`: the `YamlValue` of the output is that of the input with the
//!   same operation applied to it (skipped where that cannot be worked out,
//!   see `semantic_paths` and `expected_value`);
//! - `panic`: the operation does not panic.
//!
//! Failures are listed in `tests/edit_invariants_known_failures.txt`. A
//! failure that is not listed fails the test. Listed failures that no longer
//! happen are printed so they can be pruned, but do not fail the test, so
//! that fixes landing in parallel do not conflict over the list. Run with
//! `UPDATE_KNOWN_FAILURES=1` to rewrite it, and with
//! `EDIT_INVARIANTS_DETAILS=<path>` to write the input, output and message
//! of each failure to `<path>`. `EDIT_INVARIANTS_INPUT=<file>` runs the
//! checks on `<file>` instead and prints every failure.

use rowan::ast::AstNode;
use rowan::TextRange;
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::fmt::Write as _;
use std::fs;
use std::panic::{catch_unwind, AssertUnwindSafe};
use std::path::{Path, PathBuf};
use std::str::FromStr;
use yaml_edit::validator::{Severity, Validator};
use yaml_edit::{
    debug, Document, Lang, Mapping, Sequence, SyntaxKind, YamlFile, YamlNode, YamlValue,
};

type SyntaxNode = rowan::SyntaxNode<Lang>;

const KNOWN_FAILURES: &str = "tests/edit_invariants_known_failures.txt";

/// Valid test suite inputs as `(id, text)`, sorted by id.
fn valid_cases() -> Vec<(String, String)> {
    fn add(cases: &mut Vec<(String, String)>, id: String, dir: &Path) {
        if dir.join("error").exists() {
            return;
        }
        if let Ok(text) = fs::read_to_string(dir.join("in.yaml")) {
            // Undo the CRLF conversion a Windows checkout may apply; none of
            // the inputs contain a carriage return.
            cases.push((id, text.replace("\r\n", "\n")));
        }
    }
    let mut cases = Vec::new();
    let entries = fs::read_dir("test-data").expect("test-data missing, see yaml_test_suite.rs");
    for entry in entries {
        let path: PathBuf = entry.unwrap().path();
        let name = path.file_name().unwrap().to_str().unwrap().to_string();
        if !path.is_dir() || name == "name" || name == "tags" {
            continue;
        }
        if path.join("in.yaml").exists() || path.join("===").exists() {
            add(&mut cases, name, &path);
            continue;
        }
        for sub in fs::read_dir(&path).unwrap() {
            let sub = sub.unwrap().path();
            let sub_name = sub.file_name().unwrap().to_str().unwrap().to_string();
            add(&mut cases, format!("{name}/{sub_name}"), &sub);
        }
    }
    cases.sort();
    cases
}

#[derive(Clone, Debug)]
enum Step {
    Key(String),
    Index(usize),
}

/// A mapping or sequence to edit, found again in a fresh parse by its kind
/// and range.
struct Target {
    kind: SyntaxKind,
    range: TextRange,
    label: String,
    /// Where the collection sits in the document's `YamlValue`, if the
    /// semantic check applies to it.
    path: Option<Vec<Step>>,
}

/// Record the `YamlValue` path of every collection under `node`. Collections
/// inside keys, or under a key whose string form is unknown, get no path.
fn semantic_paths(
    node: &YamlNode,
    path: Option<Vec<Step>>,
    out: &mut HashMap<(SyntaxKind, TextRange), Option<Vec<Step>>>,
) {
    match node {
        YamlNode::Mapping(m) => {
            out.insert((SyntaxKind::MAPPING, m.syntax().text_range()), path.clone());
            for entry in m.entries() {
                let key = entry.key_node();
                if let Some(key) = &key {
                    semantic_paths(key, None, out);
                }
                if let Some(value) = entry.value_node() {
                    let child = path.clone().and_then(|p| {
                        let k = key
                            .as_ref()
                            .map_or(Some(String::new()), debug::key_string)?;
                        Some([p, vec![Step::Key(k)]].concat())
                    });
                    semantic_paths(&value, child, out);
                }
            }
        }
        YamlNode::Sequence(s) => {
            out.insert(
                (SyntaxKind::SEQUENCE, s.syntax().text_range()),
                path.clone(),
            );
            for (i, item) in s.values().enumerate() {
                let child = path.clone().map(|p| [p, vec![Step::Index(i)]].concat());
                semantic_paths(&item, child, out);
            }
        }
        YamlNode::TaggedNode(t) => {
            if let Some(m) = t.as_mapping() {
                semantic_paths(&YamlNode::Mapping(m), path, out);
            } else if let Some(s) = t.as_sequence() {
                semantic_paths(&YamlNode::Sequence(s), path, out);
            }
        }
        YamlNode::Scalar(_) | YamlNode::Alias(_) => {}
    }
}

fn anchor_names(node: &SyntaxNode) -> Vec<String> {
    node.children_with_tokens()
        .filter_map(|c| c.into_token())
        .filter(|t| t.kind() == SyntaxKind::ANCHOR)
        .map(|t| t.text().trim_start_matches('&').to_string())
        .collect()
}

fn alias_names(root: &SyntaxNode) -> BTreeSet<String> {
    root.descendants_with_tokens()
        .filter_map(|c| c.into_token())
        .filter(|t| t.kind() == SyntaxKind::REFERENCE)
        .map(|t| t.text().trim_start_matches('*').to_string())
        .collect()
}

fn root_node(doc: &Document) -> Option<YamlNode> {
    doc.syntax().children().find_map(YamlNode::from_syntax)
}

fn targets(doc: &Document) -> Vec<Target> {
    let mut paths = HashMap::new();
    if let Some(root) = root_node(doc) {
        semantic_paths(&root, Some(Vec::new()), &mut paths);
    }
    let aliases = alias_names(doc.syntax());
    let mut counts = (0, 0);
    let mut out = Vec::new();
    for node in doc.syntax().descendants() {
        let label = match node.kind() {
            SyntaxKind::MAPPING => {
                counts.0 += 1;
                format!("map{}", counts.0 - 1)
            }
            SyntaxKind::SEQUENCE => {
                counts.1 += 1;
                format!("seq{}", counts.1 - 1)
            }
            _ => continue,
        };
        let range = node.text_range();
        // If the collection or an enclosing node is aliased, an edit also
        // changes the alias, which the expected value does not model.
        let aliased = node
            .ancestors()
            .flat_map(|n| anchor_names(&n))
            .any(|a| aliases.contains(&a));
        let path = if aliased {
            None
        } else {
            paths.get(&(node.kind(), range)).cloned().flatten()
        };
        out.push(Target {
            kind: node.kind(),
            range,
            label,
            path,
        });
    }
    out
}

fn find_node(doc: &Document, kind: SyntaxKind, range: TextRange) -> SyntaxNode {
    doc.syntax()
        .descendants()
        .find(|n| n.kind() == kind && n.text_range() == range)
        .expect("target not found in a fresh parse")
}

#[derive(Clone, Copy, Debug)]
enum MapOp {
    Remove(usize),
    Clear,
    SetExisting(usize),
    SetNew,
    ReorderReversed,
    MoveBeforeFirst(usize),
    MoveAfterLast(usize),
    Rename(usize),
}

#[derive(Clone, Copy, Debug)]
enum SeqOp {
    Remove(usize),
    Clear,
    Push,
    InsertFront,
    Set(usize),
}

#[derive(Clone, Copy, Debug)]
enum Op {
    Map(MapOp),
    Seq(SeqOp),
}

const NEW_VALUE: &str = "xval";
const NEW_KEY: &str = "xnew";
const RENAMED_KEY: &str = "xrenamed";

impl Op {
    fn name(&self) -> String {
        match self {
            Op::Map(MapOp::Remove(i)) => format!("remove[{i}]"),
            Op::Map(MapOp::Clear) | Op::Seq(SeqOp::Clear) => "clear".to_string(),
            Op::Map(MapOp::SetExisting(i)) => format!("set[{i}]"),
            Op::Map(MapOp::SetNew) => "set-new".to_string(),
            Op::Map(MapOp::ReorderReversed) => "reorder-reversed".to_string(),
            Op::Map(MapOp::MoveBeforeFirst(i)) => format!("move-before-first[{i}]"),
            Op::Map(MapOp::MoveAfterLast(i)) => format!("move-after-last[{i}]"),
            Op::Map(MapOp::Rename(i)) => format!("rename[{i}]"),
            Op::Seq(SeqOp::Remove(i)) => format!("remove[{i}]"),
            Op::Seq(SeqOp::Push) => "push".to_string(),
            Op::Seq(SeqOp::InsertFront) => "insert[0]".to_string(),
            Op::Seq(SeqOp::Set(i)) => format!("set[{i}]"),
        }
    }
}

fn ops_for(node: &SyntaxNode) -> Vec<Op> {
    if let Some(m) = Mapping::cast(node.clone()) {
        let n = m.entries().count();
        let mut ops = vec![
            Op::Map(MapOp::Clear),
            Op::Map(MapOp::SetNew),
            Op::Map(MapOp::ReorderReversed),
        ];
        for i in 0..n {
            ops.push(Op::Map(MapOp::Remove(i)));
            ops.push(Op::Map(MapOp::SetExisting(i)));
            ops.push(Op::Map(MapOp::Rename(i)));
            if i > 0 {
                ops.push(Op::Map(MapOp::MoveBeforeFirst(i)));
            }
            if i + 1 < n {
                ops.push(Op::Map(MapOp::MoveAfterLast(i)));
            }
        }
        ops
    } else {
        let s = Sequence::cast(node.clone()).unwrap();
        let mut ops = vec![
            Op::Seq(SeqOp::Clear),
            Op::Seq(SeqOp::Push),
            Op::Seq(SeqOp::InsertFront),
        ];
        for i in 0..s.len() {
            ops.push(Op::Seq(SeqOp::Remove(i)));
            ops.push(Op::Seq(SeqOp::Set(i)));
        }
        ops
    }
}

/// The key of the `i`th entry, or `None` for an entry without one.
fn nth_key(m: &Mapping, i: usize) -> Option<YamlNode> {
    m.entries().nth(i).unwrap().key_node()
}

/// Apply `op`; `Err` means the operation does not apply here (an entry
/// without a key) and was not run.
fn apply(node: &SyntaxNode, op: Op) -> Result<(), ()> {
    match op {
        Op::Map(op) => {
            let m = Mapping::cast(node.clone()).unwrap();
            let key = |i| nth_key(&m, i).ok_or(());
            let value = |i: usize| m.entries().nth(i).unwrap().value_node();
            let last = m.entries().count().saturating_sub(1);
            match op {
                MapOp::Remove(i) => {
                    m.remove(key(i)?);
                }
                MapOp::Clear => m.clear(),
                MapOp::SetExisting(i) => m.set(key(i)?, NEW_VALUE),
                MapOp::SetNew => m.set(NEW_KEY, NEW_VALUE),
                MapOp::ReorderReversed => {
                    let keys: Vec<YamlNode> = m.entries().filter_map(|e| e.key_node()).collect();
                    m.reorder_fields(keys.into_iter().rev());
                }
                MapOp::MoveBeforeFirst(i) => {
                    m.move_before(key(0)?, key(i)?, value(i));
                }
                MapOp::MoveAfterLast(i) => {
                    m.move_after(key(last)?, key(i)?, value(i));
                }
                MapOp::Rename(i) => {
                    m.rename_key(key(i)?, RENAMED_KEY);
                }
            }
        }
        Op::Seq(op) => {
            let s = Sequence::cast(node.clone()).unwrap();
            match op {
                SeqOp::Remove(i) => {
                    s.remove(i);
                }
                SeqOp::Clear => s.clear(),
                SeqOp::Push => s.push(NEW_VALUE),
                SeqOp::InsertFront => s.insert(0, NEW_VALUE),
                SeqOp::Set(i) => {
                    s.set(i, NEW_VALUE);
                }
            }
        }
    }
    Ok(())
}

fn navigate<'a>(mut value: &'a mut YamlValue, path: &[Step]) -> Option<&'a mut YamlValue> {
    for step in path {
        while let YamlValue::Tagged(_, inner) = value {
            value = inner;
        }
        value = match (step, value) {
            (Step::Key(k), YamlValue::Mapping(m)) => m.get_mut(k)?,
            (Step::Index(i), YamlValue::Sequence(s)) => s.get_mut(*i)?,
            _ => return None,
        };
    }
    while let YamlValue::Tagged(_, inner) = value {
        value = inner;
    }
    Some(value)
}

/// Whether `holder`, a KEY, VALUE or SEQUENCE_ENTRY, has an anchor that
/// replacing its content keeps, and that `aliases` refer to.
fn holds_aliased_anchor(holder: &SyntaxNode, aliases: &BTreeSet<String>) -> bool {
    let tagged = holder
        .children()
        .next()
        .filter(|n| n.kind() == SyntaxKind::TAGGED_NODE);
    std::iter::once(holder.clone())
        .chain(tagged)
        .flat_map(|n| anchor_names(&n))
        .any(|a| aliases.contains(&a))
}

fn entry_part(m: &Mapping, i: usize, kind: SyntaxKind) -> Option<SyntaxNode> {
    m.entries()
        .nth(i)?
        .syntax()
        .children()
        .find(|n| n.kind() == kind)
}

/// The value of the document after `op`, or `None` if it cannot be worked
/// out from `before`.
///
/// Replacing a value or key keeps its anchor, so aliases to it then refer
/// to the new content; that is not modelled, so such edits are skipped.
fn expected_value(
    before: &YamlValue,
    path: &[Step],
    node: &SyntaxNode,
    op: Op,
    aliases: &BTreeSet<String>,
) -> Option<YamlValue> {
    let mut value = before.clone();
    let target = navigate(&mut value, path)?;
    let new_value = || YamlValue::scalar(NEW_VALUE);
    match (op, target) {
        (Op::Map(op), YamlValue::Mapping(map)) => {
            let m = Mapping::cast(node.clone())?;
            // Merge keys and the like make the value differ from the entries.
            if map.len() != m.entries().count() {
                return None;
            }
            let ks = |i: usize| -> Option<String> {
                nth_key(&m, i)
                    .as_ref()
                    .map_or(Some(String::new()), debug::key_string)
            };
            match op {
                MapOp::Remove(i) => {
                    map.remove(&ks(i)?);
                }
                MapOp::Clear => map.clear(),
                MapOp::SetExisting(i) => {
                    let value = entry_part(&m, i, SyntaxKind::VALUE);
                    if value.is_some_and(|v| holds_aliased_anchor(&v, aliases)) {
                        return None;
                    }
                    map.insert(ks(i)?, new_value());
                }
                MapOp::SetNew => {
                    map.insert(NEW_KEY.to_string(), new_value());
                }
                MapOp::ReorderReversed | MapOp::MoveBeforeFirst(_) | MapOp::MoveAfterLast(_) => {}
                MapOp::Rename(i) => {
                    let key = entry_part(&m, i, SyntaxKind::KEY);
                    if key.is_some_and(|k| holds_aliased_anchor(&k, aliases)) {
                        return None;
                    }
                    let v = map.remove(&ks(i)?)?;
                    map.insert(RENAMED_KEY.to_string(), v);
                }
            }
        }
        (Op::Seq(op), YamlValue::Sequence(seq)) => {
            let s = Sequence::cast(node.clone())?;
            if seq.len() != s.len() {
                return None;
            }
            if let SeqOp::Set(i) = op {
                let holder = node
                    .children()
                    .filter(|n| n.kind() == SyntaxKind::SEQUENCE_ENTRY)
                    .nth(i)?;
                if holds_aliased_anchor(&holder, aliases) {
                    return None;
                }
            }
            match op {
                SeqOp::Remove(i) => {
                    seq.remove(i);
                }
                SeqOp::Clear => seq.clear(),
                SeqOp::Push => seq.push(new_value()),
                SeqOp::InsertFront => seq.insert(0, new_value()),
                SeqOp::Set(i) => seq[i] = new_value(),
            }
        }
        _ => return None,
    }
    Some(value)
}

/// Like `==`, but ignoring scalar styles, which edits may legitimately
/// change.
fn same_value(a: &YamlValue, b: &YamlValue) -> bool {
    match (a, b) {
        (YamlValue::Scalar(a), YamlValue::Scalar(b)) => {
            a.value() == b.value() && a.scalar_type() == b.scalar_type() && a.tag() == b.tag()
        }
        (YamlValue::Sequence(a), YamlValue::Sequence(b)) => {
            a.len() == b.len() && a.iter().zip(b).all(|(a, b)| same_value(a, b))
        }
        (YamlValue::Mapping(a), YamlValue::Mapping(b)) => {
            a.len() == b.len()
                && a.iter()
                    .zip(b)
                    .all(|((ka, va), (kb, vb))| ka == kb && same_value(va, vb))
        }
        (YamlValue::OrderedMapping(a), YamlValue::OrderedMapping(b))
        | (YamlValue::Pairs(a), YamlValue::Pairs(b)) => {
            a.len() == b.len()
                && a.iter()
                    .zip(b)
                    .all(|((ka, va), (kb, vb))| ka == kb && same_value(va, vb))
        }
        (YamlValue::Tagged(ta, a), YamlValue::Tagged(tb, b)) => ta == tb && same_value(a, b),
        _ => a == b,
    }
}

fn cst_ok(file: &YamlFile) -> Result<(), String> {
    debug::validate_tree(file.syntax())?;
    debug::roundtrip_ok(file.syntax())
}

fn validator_errors(file: &YamlFile) -> Vec<String> {
    Validator::new()
        .validate_syntax(file.syntax())
        .into_iter()
        .filter(|v| v.severity == Severity::Error)
        .map(|v| v.to_string())
        .collect()
}

fn panic_message(err: &(dyn std::any::Any + Send)) -> String {
    err.downcast_ref::<String>()
        .cloned()
        .or_else(|| err.downcast_ref::<&str>().map(|s| s.to_string()))
        .unwrap_or_else(|| "<non-string panic>".to_string())
}

struct Failure {
    id: String,
    detail: String,
}

/// Run `op` on `target` in a fresh parse of `input`, returning the checks
/// that fail with a message each.
fn check_op(
    input: &str,
    doc_index: usize,
    target: &Target,
    op: Op,
    before: Option<&YamlValue>,
    input_valid: bool,
    input_cst_ok: bool,
) -> (Vec<(&'static str, String)>, String) {
    let file = YamlFile::from_str(input).unwrap();
    let doc = file.documents().nth(doc_index).unwrap();
    let node = find_node(&doc, target.kind, target.range);
    let expected = before
        .zip(target.path.as_ref())
        .and_then(|(b, p)| expected_value(b, p, &node, op, &alias_names(doc.syntax())));
    match catch_unwind(AssertUnwindSafe(|| apply(&node, op))) {
        Ok(Ok(())) => {}
        Ok(Err(())) => return (Vec::new(), String::new()),
        Err(e) => return (vec![("panic", panic_message(&*e))], String::new()),
    }
    let output = file.to_string();
    let mut failures = Vec::new();
    let mut check = |name: &'static str, f: &dyn Fn() -> Result<(), String>| match catch_unwind(
        AssertUnwindSafe(f),
    ) {
        Ok(Ok(())) => {}
        Ok(Err(msg)) => failures.push((name, msg)),
        Err(e) => failures.push((name, format!("check panicked: {}", panic_message(&*e)))),
    };
    if input_cst_ok {
        check("cst", &|| cst_ok(&file));
    }
    let reparsed = match YamlFile::from_str(&output) {
        Ok(r) => r,
        Err(e) => {
            failures.push(("reparse", e.to_string()));
            return (failures, output);
        }
    };
    check("shape", &|| {
        let edited = debug::shape_to_string(file.syntax());
        let fresh = debug::shape_to_string(reparsed.syntax());
        if edited == fresh {
            Ok(())
        } else {
            Err(format!("edited tree:\n{edited}\nre-parsed tree:\n{fresh}"))
        }
    });
    if input_valid {
        check("validator", &|| {
            let errors = validator_errors(&reparsed);
            if errors.is_empty() {
                Ok(())
            } else {
                Err(errors.join("\n"))
            }
        });
    }
    if let Some(expected) = &expected {
        check("semantic", &|| {
            let doc = reparsed
                .documents()
                .nth(doc_index)
                .ok_or("document missing after edit")?;
            let actual = YamlValue::from_document(&doc).map_err(|e| e.to_string())?;
            if same_value(&actual, expected) {
                Ok(())
            } else {
                Err(format!("expected {expected:?}\nactual   {actual:?}"))
            }
        });
    }
    (failures, output)
}

fn run_case(id: &str, input: &str, failures: &mut Vec<Failure>) {
    let file = match YamlFile::from_str(input) {
        Ok(f) => f,
        // A parse failure on a valid case is yaml_test_suite's business.
        Err(_) => return,
    };
    let input_valid = validator_errors(&file).is_empty();
    // A tree that is already broken would fail the check after every edit.
    let input_cst = cst_ok(&file);
    if let Err(msg) = &input_cst {
        let fid = format!("{id} input cst");
        let detail = format!("== {fid}\n-- input\n{input}\n-- message\n{msg}\n");
        failures.push(Failure { id: fid, detail });
    }
    for (d, doc) in file.documents().enumerate() {
        let before = YamlValue::from_document(&doc).ok();
        for target in targets(&doc) {
            let node = find_node(&doc, target.kind, target.range);
            let len = Mapping::cast(node.clone())
                .map(|m| m.entries().count())
                .or_else(|| Sequence::cast(node.clone()).map(|s| s.len()))
                .unwrap();
            for op in ops_for(&node) {
                let (failed, output) = check_op(
                    input,
                    d,
                    &target,
                    op,
                    before.as_ref(),
                    input_valid,
                    input_cst.is_ok(),
                );
                for (check, msg) in failed {
                    let fid = format!("{id} d{d} {} {} {check}", target.label, op.name());
                    let detail = format!(
                        "== {fid}\n-- target ({len} items)\n{node}\n-- input\n{input}\n\
                         -- output\n{output}\n-- message\n{msg}\n"
                    );
                    failures.push(Failure { id: fid, detail });
                }
            }
        }
    }
}

#[test]
fn edit_invariants() {
    let cases = match std::env::var("EDIT_INVARIANTS_INPUT") {
        Ok(path) => vec![(path.clone(), fs::read_to_string(&path).unwrap())],
        Err(_) => valid_cases(),
    };
    assert!(!cases.is_empty(), "no test suite cases found");

    // Panics are caught and reported; keep the default hook from printing
    // a backtrace for each one.
    let hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(|_| {}));
    let mut failures = Vec::new();
    for (id, input) in &cases {
        run_case(id, input, &mut failures);
    }
    std::panic::set_hook(hook);

    if let Ok(path) = std::env::var("EDIT_INVARIANTS_DETAILS") {
        let mut out = String::new();
        for f in &failures {
            writeln!(out, "{}", f.detail).unwrap();
        }
        fs::write(path, out).unwrap();
    }

    if std::env::var("EDIT_INVARIANTS_INPUT").is_ok() {
        for f in &failures {
            println!("{}", f.detail);
        }
        return;
    }

    let actual: BTreeSet<String> = failures.iter().map(|f| f.id.clone()).collect();
    if std::env::var("UPDATE_KNOWN_FAILURES").is_ok() {
        let mut out = String::from(
            "# Known failures of tests/edit_invariants.rs, as\n\
             # <case> <document> <collection> <operation> <check>.\n\
             # Regenerate with UPDATE_KNOWN_FAILURES=1 cargo test --test edit_invariants\n",
        );
        for id in &actual {
            writeln!(out, "{id}").unwrap();
        }
        fs::write(KNOWN_FAILURES, out).unwrap();
        return;
    }
    let known_text = fs::read_to_string(KNOWN_FAILURES)
        .unwrap_or_else(|e| panic!("cannot read {KNOWN_FAILURES}: {e}"));
    let known: BTreeSet<String> = known_text
        .lines()
        .filter(|l| !l.is_empty() && !l.starts_with('#'))
        .map(str::to_string)
        .collect();
    let details: BTreeMap<&str, &str> = failures
        .iter()
        .map(|f| (f.id.as_str(), f.detail.as_str()))
        .collect();
    let new: Vec<&String> = actual.difference(&known).collect();
    let fixed: Vec<&String> = known.difference(&actual).collect();
    if !fixed.is_empty() {
        println!("No longer failing, can be removed from {KNOWN_FAILURES}:");
        for id in &fixed {
            println!("{id}");
        }
    }
    let mut report = String::new();
    for id in &new {
        writeln!(report, "{}", details[id.as_str()]).unwrap();
    }
    assert!(new.is_empty(), "{} new failure(s):\n{report}", new.len());
}
