#![no_main]

//! Apply a sequence of edits to any document that parses cleanly, and check
//! the result after every step against the document's own re-parse and
//! against a model of the edits.
//!
//! The input is YAML text, a NUL byte, and bytes that choose the edits. Each
//! edit picks one of the document's mappings or sequences and runs a
//! mapping or sequence operation on it, or a path operation through it.
//! New keys and values are random strings and `YamlValue` trees, so this also
//! checks that what a builder writes reads back as the value it was given.
//!
//! After every edit of input that the validator accepts:
//! - the CST passes `debug::validate_tree` and `debug::roundtrip_ok`;
//! - the text parses to the same tree (`debug::shape_to_string`);
//! - the validator reports the same for the edited tree as for the re-parse;
//! - the validator accepts it, apart from duplicate keys: an edit inside a
//!   key may make it equal to another key, as may replacing an alias in a
//!   key with a copy, since the validator compares a key holding an alias
//!   with nothing; later edits must not add more;
//! - its `YamlValue` equals a model of the document updated alongside it.
//!   Where an edit cannot be modelled (merge keys, aliases to what changed),
//!   the model is taken from the edited document again.
//!
//! Edits of other input only have to not panic, in the editor or in the
//! validator run on the edited tree. Set `EDIT_SEQUENCE_TRACE`
//! to print each edit, and the tree it is made on, before it is made.

use arbitrary::{Result as ArbResult, Unstructured};
use libfuzzer_sys::fuzz_target;
use std::collections::BTreeMap;
use std::str::FromStr;
use yaml_edit::path::YamlPath;
use yaml_edit::{ScalarValue, Sequence};

include!("../../tests/common/edit_checks.rs");

/// Characters for new strings: indicators, white space, line breaks and
/// characters that need escaping, along with plain letters.
const PALETTE: &[char] = &[
    'a',
    'b',
    'k',
    'z',
    '0',
    '1',
    ' ',
    ' ',
    '\t',
    '\n',
    '\r',
    ':',
    '#',
    '-',
    '?',
    ',',
    '[',
    ']',
    '{',
    '}',
    '&',
    '*',
    '!',
    '|',
    '>',
    '\'',
    '"',
    '%',
    '@',
    '`',
    '\\',
    '.',
    '~',
    '<',
    '=',
    '\u{e9}',
    '\u{85}',
    '\u{a0}',
    '\u{2028}',
    '\u{feff}',
    '\u{1}',
    '\u{7f}',
    '\u{1f600}',
];

fn arb_text(u: &mut Unstructured) -> ArbResult<String> {
    let len = u.int_in_range(0..=6)?;
    (0..len).map(|_| Ok(*u.choose(PALETTE)?)).collect()
}

/// A short key that a path can name without escaping.
fn arb_path_key(u: &mut Unstructured) -> ArbResult<String> {
    let len = u.int_in_range(1..=3)?;
    (0..len)
        .map(|_| Ok(*u.choose(&['a', 'b', 'k', 'z', '_'])?))
        .collect()
}

fn arb_value(u: &mut Unstructured, depth: usize) -> ArbResult<YamlValue> {
    let max = if depth >= 2 { 3 } else { 5 };
    Ok(match u.int_in_range(0..=max)? {
        0 => YamlValue::scalar(ScalarValue::null()),
        1 => YamlValue::scalar(u.arbitrary::<bool>()?),
        2 => YamlValue::scalar(u.arbitrary::<i64>()?),
        3 => YamlValue::scalar(ScalarValue::string(arb_text(u)?)),
        4 => {
            let n = u.int_in_range(0..=3)?;
            YamlValue::Sequence(
                (0..n)
                    .map(|_| arb_value(u, depth + 1))
                    .collect::<ArbResult<_>>()?,
            )
        }
        _ => {
            let n = u.int_in_range(0..=3)?;
            let mut m = BTreeMap::new();
            for _ in 0..n {
                m.insert(arb_text(u)?, arb_value(u, depth + 1)?);
            }
            YamlValue::Mapping(m)
        }
    })
}

#[derive(Debug)]
enum MapOp {
    Remove(usize),
    Clear,
    SetExisting(usize, YamlValue),
    SetNew(String, YamlValue),
    Reorder(Vec<usize>),
    MoveBefore(usize, usize),
    MoveAfter(usize, usize),
    Rename(usize, String),
    InsertAfter(usize, String, YamlValue),
    InsertBefore(usize, String, YamlValue),
    InsertAtIndex(usize, String, YamlValue),
    SetWithFieldOrder(String, YamlValue, Vec<usize>, usize),
    /// Set, or with `None` remove, the entry's path.
    Path(usize, Option<YamlValue>),
    SetPathNew(String, YamlValue),
}

#[derive(Debug)]
enum SeqOp {
    Remove(usize),
    Clear,
    Push(YamlValue),
    Insert(usize, YamlValue),
    Set(usize, YamlValue),
    Pop,
    /// Set, or with `None` remove, the item's path.
    Path(usize, Option<YamlValue>),
}

#[derive(Debug)]
enum Op {
    Map(MapOp),
    Seq(SeqOp),
}

fn arb_index(u: &mut Unstructured) -> ArbResult<usize> {
    u.int_in_range(0..=7)
}

fn arb_map_op(u: &mut Unstructured) -> ArbResult<MapOp> {
    Ok(match u.int_in_range(0..=14)? {
        0 => MapOp::Remove(arb_index(u)?),
        1 => MapOp::Clear,
        2 => MapOp::SetExisting(arb_index(u)?, arb_value(u, 0)?),
        3 => MapOp::SetNew(arb_text(u)?, arb_value(u, 0)?),
        4 => {
            let n = u.int_in_range(0..=4)?;
            MapOp::Reorder((0..n).map(|_| arb_index(u)).collect::<ArbResult<_>>()?)
        }
        5 => MapOp::MoveBefore(arb_index(u)?, arb_index(u)?),
        6 => MapOp::MoveAfter(arb_index(u)?, arb_index(u)?),
        7 => MapOp::Rename(arb_index(u)?, arb_text(u)?),
        8 => MapOp::InsertAfter(arb_index(u)?, arb_text(u)?, arb_value(u, 0)?),
        9 => MapOp::InsertBefore(arb_index(u)?, arb_text(u)?, arb_value(u, 0)?),
        10 => MapOp::InsertAtIndex(arb_index(u)?, arb_text(u)?, arb_value(u, 0)?),
        11 => {
            let key = arb_text(u)?;
            let value = arb_value(u, 0)?;
            let n = u.int_in_range(0..=4)?;
            let order = (0..n).map(|_| arb_index(u)).collect::<ArbResult<_>>()?;
            MapOp::SetWithFieldOrder(key, value, order, arb_index(u)?)
        }
        12 => MapOp::Path(arb_index(u)?, Some(arb_value(u, 0)?)),
        13 => MapOp::SetPathNew(arb_path_key(u)?, arb_value(u, 0)?),
        _ => MapOp::Path(arb_index(u)?, None),
    })
}

fn arb_seq_op(u: &mut Unstructured) -> ArbResult<SeqOp> {
    Ok(match u.int_in_range(0..=7)? {
        0 => SeqOp::Remove(arb_index(u)?),
        1 => SeqOp::Clear,
        2 => SeqOp::Push(arb_value(u, 0)?),
        3 => SeqOp::Insert(arb_index(u)?, arb_value(u, 0)?),
        4 => SeqOp::Set(arb_index(u)?, arb_value(u, 0)?),
        5 => SeqOp::Pop,
        6 => SeqOp::Path(arb_index(u)?, Some(arb_value(u, 0)?)),
        _ => SeqOp::Path(arb_index(u)?, None),
    })
}

/// A path naming `steps`, or `None` if a key in it cannot be written in a
/// path as it is.
fn path_string(steps: &[Step]) -> Option<String> {
    let mut out = String::new();
    for step in steps {
        match step {
            Step::Key(k) => {
                if k.is_empty() || !k.chars().all(|c| c.is_ascii_alphanumeric() || c == '_') {
                    return None;
                }
                if !out.is_empty() {
                    out.push('.');
                }
                out.push_str(k);
            }
            Step::Index(i) => out.push_str(&format!("[{i}]")),
        }
    }
    (!out.is_empty()).then_some(out)
}

/// The range of `node` in its tree.
fn node_range(node: &YamlNode) -> TextRange {
    match node {
        YamlNode::Scalar(n) => n.syntax().text_range(),
        YamlNode::Mapping(n) => n.syntax().text_range(),
        YamlNode::Sequence(n) => n.syntax().text_range(),
        YamlNode::Alias(n) => n.syntax().text_range(),
        YamlNode::TaggedNode(n) => n.syntax().text_range(),
    }
}

/// Whether `path` leads to `node` in `doc`. A key's string form in the
/// model is not always how a path names it (`0x10` is "16", a tag is left
/// out), and a path that names another node, or none, would have the edit
/// go elsewhere than the model expects.
fn path_reaches(doc: &Document, path: &str, node: Option<YamlNode>) -> bool {
    let found = doc.try_get_path(path).ok();
    found.as_ref().map(node_range) == node.as_ref().map(node_range)
}

/// Whether `key` can be added to `m` as a string key without clashing with
/// a key of another type in the model.
fn is_string_key_free(map: &BTreeMap<String, YamlValue>, m: &Mapping, key: &str) -> bool {
    // A key of another type with the same string form, such as `1` for
    // "1", would make the edited document's YamlValue fail, as it does
    // for any such pair.
    !map.contains_key(key) || m.find_entry_by_key(key).is_some()
}

/// The collection an edit applies to.
struct Ctx<'a> {
    doc: &'a Document,
    node: SyntaxNode,
    path: Option<Vec<Step>>,
    aliases: BTreeSet<String>,
}

/// Apply `op`, returning the expected document value afterwards if it can
/// be worked out from `model`.
fn apply(ctx: &Ctx, op: &Op, model: Option<&YamlValue>) -> Option<YamlValue> {
    let mut expected = model.cloned();
    let target = match (&mut expected, &ctx.path) {
        (Some(v), Some(p)) => navigate(v, p),
        _ => None,
    };
    let modelled = match op {
        Op::Map(op) => apply_map(ctx, op, target),
        Op::Seq(op) => apply_seq(ctx, op, target),
    };
    if modelled {
        expected
    } else {
        None
    }
}

fn apply_map(ctx: &Ctx, op: &MapOp, target: Option<&mut YamlValue>) -> bool {
    let m = Mapping::cast(ctx.node.clone()).unwrap();
    let n = m.entries().count();
    let mut map = match target {
        Some(YamlValue::Mapping(map)) if map.len() == n => Some(map),
        _ => None,
    };
    let key_at = |i: usize| (n > 0).then(|| nth_key(&m, i % n)).flatten();
    let key_string_at =
        |i: usize| -> Option<String> { key_at(i).as_ref().and_then(debug::key_string) };
    let value_aliased = |i: usize| {
        n > 0
            && entry_part(&m, i % n, SyntaxKind::VALUE)
                .is_some_and(|v| holds_aliased_anchor(&v, &ctx.aliases))
    };
    let key_aliased = |i: usize| {
        n > 0
            && entry_part(&m, i % n, SyntaxKind::KEY)
                .is_some_and(|v| holds_aliased_anchor(&v, &ctx.aliases))
    };
    match op {
        MapOp::Remove(i) => {
            let Some(key) = key_at(*i) else { return false };
            let ks = key_string_at(*i);
            m.remove(key);
            match (map, ks) {
                (Some(map), Some(ks)) => map.remove(&ks).is_some(),
                _ => false,
            }
        }
        MapOp::Clear => {
            m.clear();
            map.map(|map| map.clear()).is_some()
        }
        MapOp::SetExisting(i, v) => {
            let Some(key) = key_at(*i) else { return false };
            let ks = key_string_at(*i);
            let aliased = value_aliased(*i);
            m.set(key, v);
            match (map, ks) {
                (Some(map), Some(ks)) if !aliased => {
                    map.insert(ks, v.clone());
                    true
                }
                _ => false,
            }
        }
        MapOp::SetNew(k, v) | MapOp::InsertAtIndex(_, k, v) => {
            if let Some(map) = &map {
                if !is_string_key_free(map, &m, k) {
                    return true;
                }
            }
            let aliased = m
                .find_entry_by_key(k.as_str())
                .and_then(|e| {
                    e.syntax()
                        .children()
                        .find(|c| c.kind() == SyntaxKind::VALUE)
                })
                .is_some_and(|v| holds_aliased_anchor(&v, &ctx.aliases));
            match op {
                MapOp::InsertAtIndex(i, ..) => m.insert_at_index(*i, k.as_str(), v),
                _ => m.set(k.as_str(), v),
            }
            match map {
                Some(map) if !aliased => {
                    map.insert(k.clone(), v.clone());
                    true
                }
                _ => false,
            }
        }
        MapOp::Reorder(order) => {
            let keys: Vec<YamlNode> = order.iter().filter_map(|i| key_at(*i)).collect();
            m.reorder_fields(keys);
            map.is_some()
        }
        MapOp::MoveBefore(i, j) | MapOp::MoveAfter(i, j) => {
            let (Some(anchor), Some(key)) = (key_at(*i), key_at(*j)) else {
                return false;
            };
            let value = m.entries().nth(*j % n).unwrap().value_node();
            match op {
                MapOp::MoveBefore(..) => m.move_before(anchor, key, value),
                _ => m.move_after(anchor, key, value),
            };
            map.is_some()
        }
        MapOp::Rename(i, k) => {
            let Some(key) = key_at(*i) else { return false };
            if let Some(map) = &map {
                if !is_string_key_free(map, &m, k) {
                    return true;
                }
            }
            let ks = key_string_at(*i);
            let aliased = key_aliased(*i);
            if !m.rename_key(key, k.as_str()) {
                panic!("rename_key refused an existing key");
            }
            match (map, ks) {
                (Some(map), Some(ks)) if !aliased => {
                    let Some(v) = map.remove(&ks) else {
                        return false;
                    };
                    map.insert(k.clone(), v);
                    true
                }
                _ => false,
            }
        }
        MapOp::InsertAfter(i, k, v) | MapOp::InsertBefore(i, k, v) => {
            let Some(anchor) = key_at(*i) else {
                return false;
            };
            if let Some(map) = &map {
                if !is_string_key_free(map, &m, k) {
                    return true;
                }
            }
            let aliased = m
                .find_entry_by_key(k.as_str())
                .and_then(|e| {
                    e.syntax()
                        .children()
                        .find(|c| c.kind() == SyntaxKind::VALUE)
                })
                .is_some_and(|v| holds_aliased_anchor(&v, &ctx.aliases));
            let done = match op {
                MapOp::InsertAfter(..) => m.insert_after(anchor, k.as_str(), v),
                _ => m.insert_before(anchor, k.as_str(), v),
            };
            if !done {
                panic!("insert next to an existing key refused");
            }
            match map {
                Some(map) if !aliased => {
                    map.insert(k.clone(), v.clone());
                    true
                }
                _ => false,
            }
        }
        MapOp::SetWithFieldOrder(k, v, order, pos) => {
            if let Some(map) = &map {
                if !is_string_key_free(map, &m, k) {
                    return true;
                }
            }
            let aliased = m
                .find_entry_by_key(k.as_str())
                .and_then(|e| {
                    e.syntax()
                        .children()
                        .find(|c| c.kind() == SyntaxKind::VALUE)
                })
                .is_some_and(|v| holds_aliased_anchor(&v, &ctx.aliases));
            let mut fields: Vec<YamlValue> = order
                .iter()
                .filter_map(|i| key_at(*i))
                .filter_map(|key| {
                    key.as_scalar()
                        .map(|s| YamlValue::Scalar(ScalarValue::from_scalar(s)))
                })
                .collect();
            let at = (*pos).min(fields.len());
            fields.insert(at, YamlValue::scalar(ScalarValue::string(k.as_str())));
            m.set_with_field_order(k.as_str(), v, fields);
            match map {
                Some(map) if !aliased => {
                    map.insert(k.clone(), v.clone());
                    true
                }
                _ => false,
            }
        }
        MapOp::Path(i, v) => {
            let Some(base) = &ctx.path else { return false };
            let Some(ks) = key_string_at(*i) else {
                return false;
            };
            let steps = [base.clone(), vec![Step::Key(ks.clone())]].concat();
            let Some(path) = path_string(&steps) else {
                return false;
            };
            let value = m.entries().nth(*i % n).and_then(|e| e.value_node());
            if !path_reaches(ctx.doc, &path, value) {
                return false;
            }
            let aliased = value_aliased(*i);
            let before = ctx.doc.to_string();
            let result = match v {
                Some(v) => ctx.doc.try_set_path(&path, v).map(|_| ()),
                None => ctx.doc.try_remove_path(&path).map(|_| ()),
            };
            if result.is_err() {
                assert_eq!(
                    ctx.doc.to_string(),
                    before,
                    "failed path edit changed {path:?}"
                );
                return map.is_some();
            }
            match (map, v) {
                (Some(map), Some(v)) if !aliased => {
                    map.insert(ks, v.clone());
                    true
                }
                (Some(map), None) => map.remove(&ks).is_some(),
                _ => false,
            }
        }
        MapOp::SetPathNew(k, v) => {
            let Some(base) = &ctx.path else { return false };
            let steps = [base.clone(), vec![Step::Key(k.clone())]].concat();
            let Some(path) = path_string(&steps) else {
                return false;
            };
            let parent = path_string(base);
            let reaches_target = parent.as_ref().is_none_or(|parent| {
                let found = ctx.doc.try_get_path(parent).ok();
                found.and_then(|n| n.as_mapping().map(|m| m.syntax().text_range()))
                    == Some(ctx.node.text_range())
            });
            if !reaches_target {
                return false;
            }
            if let Some(map) = &map {
                if !is_string_key_free(map, &m, k) {
                    return true;
                }
            }
            let aliased = m
                .find_entry_by_key(k.as_str())
                .and_then(|e| {
                    e.syntax()
                        .children()
                        .find(|c| c.kind() == SyntaxKind::VALUE)
                })
                .is_some_and(|v| holds_aliased_anchor(&v, &ctx.aliases));
            let before = ctx.doc.to_string();
            if ctx.doc.try_set_path(&path, v).is_err() {
                assert_eq!(
                    ctx.doc.to_string(),
                    before,
                    "failed path edit changed {path:?}"
                );
                return map.is_some();
            }
            match map.as_mut() {
                Some(map) if !aliased => {
                    map.insert(k.clone(), v.clone());
                    true
                }
                _ => false,
            }
        }
    }
}

fn apply_seq(ctx: &Ctx, op: &SeqOp, target: Option<&mut YamlValue>) -> bool {
    let s = Sequence::cast(ctx.node.clone()).unwrap();
    let n = s.len();
    let seq = match target {
        Some(YamlValue::Sequence(seq)) if seq.len() == n => Some(seq),
        _ => None,
    };
    let item_aliased = |i: usize| {
        ctx.node
            .children()
            .filter(|c| c.kind() == SyntaxKind::SEQUENCE_ENTRY)
            .nth(i)
            .is_some_and(|e| holds_aliased_anchor(&e, &ctx.aliases))
    };
    match op {
        SeqOp::Remove(i) => {
            let removed = s.remove(*i);
            assert_eq!(removed.is_some(), *i < n, "remove({i}) of {n} items");
            if let Some(seq) = seq {
                if *i < n {
                    seq.remove(*i);
                }
                return true;
            }
            false
        }
        SeqOp::Clear => {
            s.clear();
            seq.map(|seq| seq.clear()).is_some()
        }
        SeqOp::Push(v) => {
            s.push(v);
            seq.map(|seq| seq.push(v.clone())).is_some()
        }
        SeqOp::Insert(i, v) => {
            s.insert(*i, v);
            seq.map(|seq| seq.insert((*i).min(n), v.clone())).is_some()
        }
        SeqOp::Set(i, v) => {
            let aliased = item_aliased(*i);
            let done = s.set(*i, v);
            assert_eq!(done, *i < n, "set({i}) of {n} items");
            match seq {
                Some(seq) if !aliased => {
                    if *i < n {
                        seq[*i] = v.clone();
                    }
                    true
                }
                _ => false,
            }
        }
        SeqOp::Pop => {
            let popped = s.pop();
            assert_eq!(popped.is_some(), n > 0, "pop of {n} items");
            seq.map(|seq| {
                seq.pop();
            })
            .is_some()
        }
        SeqOp::Path(i, v) => {
            if n == 0 {
                return seq.is_some();
            }
            let i = i % n;
            let Some(base) = &ctx.path else { return false };
            let steps = [base.clone(), vec![Step::Index(i)]].concat();
            let Some(path) = path_string(&steps) else {
                return false;
            };
            if !path_reaches(ctx.doc, &path, s.get(i)) {
                return false;
            }
            let aliased = item_aliased(i);
            let before = ctx.doc.to_string();
            let result = match v {
                Some(v) => ctx.doc.try_set_path(&path, v).map(|_| ()),
                None => ctx.doc.try_remove_path(&path).map(|_| ()),
            };
            if result.is_err() {
                assert_eq!(
                    ctx.doc.to_string(),
                    before,
                    "failed path edit changed {path:?}"
                );
                return seq.is_some();
            }
            match (seq, v) {
                (Some(seq), Some(v)) if !aliased => {
                    seq[i] = v.clone();
                    true
                }
                (Some(seq), None) => {
                    seq.remove(i);
                    true
                }
                _ => false,
            }
        }
    }
}

/// The number of keys under `node` that are or hold an alias.
fn alias_key_count(node: &SyntaxNode) -> usize {
    node.descendants()
        .filter(|n| n.kind() == SyntaxKind::KEY)
        .filter(|k| k.descendants().any(|n| n.kind() == SyntaxKind::ALIAS))
        .count()
}

fn check(
    file: &YamlFile,
    doc_index: usize,
    input_valid: bool,
    keys_changed: bool,
    duplicate_keys: &mut usize,
    expected: Option<&YamlValue>,
    context: &str,
) -> Option<YamlValue> {
    // The validator has to cope with whatever an edit leaves behind, so it
    // runs on the edited tree itself, not only on a re-parse of its text.
    let edited_errors = validator_errors(file);
    // Edits of input the validator rejects are only held to not panicking.
    if !input_valid {
        return None;
    }
    let output = file.to_string();
    if let Err(e) = cst_ok(file) {
        panic!("{context}: CST broken: {e}\noutput: {output:?}");
    }
    let reparsed = YamlFile::from_str(&output)
        .unwrap_or_else(|e| panic!("{context}: output does not parse: {e}\noutput: {output:?}"));
    let edited = debug::shape_to_string(file.syntax());
    let fresh = debug::shape_to_string(reparsed.syntax());
    if edited != fresh {
        panic!("{context}: tree differs from a re-parse\noutput: {output:?}\nedited:\n{edited}\nre-parsed:\n{fresh}");
    }
    let errors = validator_errors(&reparsed);
    if edited_errors != errors {
        panic!("{context}: validator disagrees between the edited tree and a re-parse\noutput: {output:?}\nedited: {edited_errors:?}\nre-parsed: {errors:?}");
    }
    let allowed = if keys_changed {
        usize::MAX
    } else {
        *duplicate_keys
    };
    *duplicate_keys = duplicate_key_count(&errors);
    let errors = unexpected_errors(&errors, allowed);
    if !errors.is_empty() {
        panic!("{context}: validator rejects the output: {errors:?}\noutput: {output:?}");
    }
    let doc = reparsed
        .documents()
        .nth(doc_index)
        .unwrap_or_else(|| panic!("{context}: document {doc_index} missing\noutput: {output:?}"));
    let actual = YamlValue::from_document(&doc);
    if let Some(expected) = expected {
        match &actual {
            Ok(actual) if same_value(actual, expected) => {}
            _ => panic!(
                "{context}: value differs from the model\noutput: {output:?}\nexpected: {expected:?}\nactual:   {actual:?}"
            ),
        }
    }
    actual.ok()
}

fuzz_target!(|data: &[u8]| {
    let Some(split) = data.iter().position(|b| *b == 0) else {
        return;
    };
    let Ok(input) = std::str::from_utf8(&data[..split]) else {
        return;
    };
    if input.len() > 4000 {
        return;
    }
    let Ok(file) = YamlFile::from_str(input) else {
        return;
    };
    // A tree that is broken already would fail every check.
    if cst_ok(&file).is_err() {
        return;
    }
    let input_valid = validator_errors(&file).is_empty();
    let mut u = Unstructured::new(&data[split + 1..]);
    let ndocs = file.documents().count();
    if ndocs == 0 {
        return;
    }
    let Ok(doc_index) = u.int_in_range(0..=ndocs - 1) else {
        return;
    };
    let doc = file.documents().nth(doc_index).unwrap();
    let mut model = YamlValue::from_document(&doc).ok();
    let mut log = Vec::new();
    let mut duplicate_keys = 0;
    let trace = std::env::var_os("EDIT_SEQUENCE_TRACE").is_some();
    for _ in 0..12 {
        if u.is_empty() {
            break;
        }
        let Ok(target_choice) = u.arbitrary::<u8>() else {
            break;
        };
        let targets = targets(&doc);
        if targets.is_empty() {
            break;
        }
        let target = &targets[target_choice as usize % targets.len()];
        let node = find_node(&doc, target.kind, target.range);
        let op = if target.kind == SyntaxKind::MAPPING {
            arb_map_op(&mut u).map(Op::Map)
        } else {
            arb_seq_op(&mut u).map(Op::Seq)
        };
        let Ok(op) = op else { break };
        let ctx = Ctx {
            doc: &doc,
            node,
            path: target.path.clone(),
            aliases: alias_names(doc.syntax()),
        };
        log.push(format!("{} {op:?}", target.label));
        // Shows the edits leading up to a panic in the library itself.
        if trace {
            eprintln!(
                "input {input:?}, doc {doc_index}, edit {}",
                log.last().unwrap()
            );
            eprintln!("{}", debug::tree_to_string(file.syntax()));
        }
        let alias_keys = alias_key_count(doc.syntax());
        let expected = apply(&ctx, &op, model.as_ref());
        let keys_changed = target.in_key || alias_key_count(doc.syntax()) < alias_keys;
        let context = format!("input {input:?}, doc {doc_index}, edits {log:#?}");
        model = check(
            &file,
            doc_index,
            input_valid,
            keys_changed,
            &mut duplicate_keys,
            expected.as_ref(),
            &context,
        );
    }
});
