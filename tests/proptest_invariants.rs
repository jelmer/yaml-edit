//! Property-based tests for CST invariants under random mutations.
//!
//! For each generated seed document + random mutation sequence, we
//! assert `debug::validate_tree` and `debug::roundtrip_ok` after every
//! step. The mutation shrinker finds the smallest violating input.

use proptest::prelude::*;
use rowan::ast::AstNode;
use std::str::FromStr;
use yaml_edit::path::YamlPath;
use yaml_edit::{debug, Document};

mod common;
use common::mutation_checks::*;

/// A random-but-plausible mutation to apply.
///
/// Keep in sync with `fuzz/fuzz_targets/mutation_invariants.rs::Op`.
/// Both fuzzers should exercise the same mutation surface; the fuzz
/// target uses `SafeStr` values, this one uses plain `String`.
#[derive(Debug, Clone)]
enum Op {
    // --- Mapping ops (top-level) ---
    SetString(String, String),
    SetInt(String, i32),
    Remove(String),
    Rename(String, String),
    Clear,
    InsertAfter(String, String, String),
    InsertBefore(String, String, String),
    InsertAtIndex(usize, String, String),
    MoveAfter(String, String, String),
    MoveBefore(String, String, String),
    RemoveNthOccurrence(String, usize),
    ReorderFields(Vec<String>),

    // --- Sequence ops (indexed by a mapping key that holds a sequence) ---
    SeqPush(String, String),
    SeqPop(String),
    SeqInsert(String, usize, String),
    SeqSet(String, usize, String),
    SeqRemove(String, usize),
    SeqClear(String),

    // --- Nested mapping ops (indexed by a mapping key that holds a mapping) ---
    NestedSet(String, String, String),
    NestedRemove(String, String),

    // --- Path-based ops ---
    SetPath(String, String),
    RemovePath(String),
}

/// A YAML key that only uses simple characters -- no colons, no quotes,
/// no whitespace edge cases. Keeps generation focused on mutation logic,
/// not parser edge cases.
fn key_strat() -> impl Strategy<Value = String> {
    "[a-z][a-z0-9_]{0,5}".prop_map(String::from)
}

/// Similarly simple scalar values.
fn value_strat() -> impl Strategy<Value = String> {
    "[a-z0-9_]{0,8}".prop_map(String::from)
}

/// A small non-negative index (0..=6). Most seed docs are tiny; keeping
/// the range small maximizes the chance of hitting a valid index.
fn index_strat() -> impl Strategy<Value = usize> {
    0usize..7
}

/// A path with 1..=3 segments. Bare keys (`a`), or a bare key
/// followed by at most one index (`items[0]`, `items[0].name`).
/// Nested-sequence indices (`s[0][0]`) are known-broken (see
/// set_path_nested_sequence_indices in tests/invariants.rs) and
/// excluded here so the fuzz can keep exploring.
fn path_strat() -> impl Strategy<Value = String> {
    (
        key_strat(),
        prop::option::of(0usize..4),
        prop::option::of(key_strat()),
    )
        .prop_map(|(k0, idx, k1)| {
            let mut out = k0;
            if let Some(i) = idx {
                out.push('[');
                out.push_str(&i.to_string());
                out.push(']');
            }
            if let Some(k) = k1 {
                out.push('.');
                out.push_str(&k);
            }
            out
        })
}

fn op_strat() -> impl Strategy<Value = Op> {
    prop_oneof![
        (key_strat(), value_strat()).prop_map(|(k, v)| Op::SetString(k, v)),
        (key_strat(), any::<i32>()).prop_map(|(k, v)| Op::SetInt(k, v)),
        key_strat().prop_map(Op::Remove),
        (key_strat(), key_strat()).prop_map(|(a, b)| Op::Rename(a, b)),
        Just(Op::Clear),
        (key_strat(), key_strat(), value_strat()).prop_map(|(a, k, v)| Op::InsertAfter(a, k, v)),
        (key_strat(), key_strat(), value_strat()).prop_map(|(a, k, v)| Op::InsertBefore(a, k, v)),
        (index_strat(), key_strat(), value_strat())
            .prop_map(|(i, k, v)| Op::InsertAtIndex(i, k, v)),
        (key_strat(), key_strat(), value_strat()).prop_map(|(a, k, v)| Op::MoveAfter(a, k, v)),
        (key_strat(), key_strat(), value_strat()).prop_map(|(a, k, v)| Op::MoveBefore(a, k, v)),
        (key_strat(), index_strat()).prop_map(|(k, n)| Op::RemoveNthOccurrence(k, n)),
        prop::collection::vec(key_strat(), 0..=4).prop_map(Op::ReorderFields),
        (key_strat(), value_strat()).prop_map(|(k, v)| Op::SeqPush(k, v)),
        key_strat().prop_map(Op::SeqPop),
        (key_strat(), index_strat(), value_strat()).prop_map(|(k, i, v)| Op::SeqInsert(k, i, v)),
        (key_strat(), index_strat(), value_strat()).prop_map(|(k, i, v)| Op::SeqSet(k, i, v)),
        (key_strat(), index_strat()).prop_map(|(k, i)| Op::SeqRemove(k, i)),
        key_strat().prop_map(Op::SeqClear),
        (key_strat(), key_strat(), value_strat()).prop_map(|(k, ik, v)| Op::NestedSet(k, ik, v)),
        (key_strat(), key_strat()).prop_map(|(k, ik)| Op::NestedRemove(k, ik)),
        (path_strat(), value_strat()).prop_map(|(p, v)| Op::SetPath(p, v)),
        path_strat().prop_map(Op::RemovePath),
    ]
}

/// A seed YAML document -- one of a handful of hand-picked shapes that
/// exercise the common structures without depending on the parser
/// handling weird inputs.
///
/// Kept as valid, parseable YAML -- the goal is to stress the *mutation*
/// paths against a spread of representative starting shapes (block,
/// flow, anchors, tags, block scalars, comments), not to fuzz the
/// parser itself.
fn seed_strat() -> impl Strategy<Value = &'static str> {
    prop_oneof![
        // Simple block shapes.
        Just("a: 1\n"),
        Just("a: 1\nb: 2\nc: 3\n"),
        Just("items:\n  - one\n  - two\n"),
        Just("root:\n  a: 1\n  b: 2\n"),
        Just("mixed:\n  list:\n    - x\n    - y\n  map:\n    k: v\n"),
        Just("existing: value\n"),
        // Flow-style collections.
        Just("flow: {a: 1, b: 2}\n"),
        Just("nums: [1, 2, 3]\n"),
        Just("mixed_flow: {a: [1, 2], b: {x: y}}\n"),
        // Anchors and aliases.
        Just("defaults: &d\n  timeout: 30\nprod:\n  <<: *d\n  host: prod\n"),
        Just("first: &ref value\nsecond: *ref\n"),
        // Tagged scalars and collections.
        Just("count: !!int '42'\n"),
        Just("keys: !!set\n  ? a\n  ? b\n"),
        Just("mapping: !!map\n  a: 1\n  b: 2\n"),
        // Block scalars.
        Just("literal: |\n  line1\n  line2\n"),
        Just("folded: >\n  wrapped\n  paragraph\n"),
        // Comments interleaved with data.
        Just("a: 1  # trailing\nb: 2\n"),
        Just("items:  # a list\n  - one\n  - two  # inline\n"),
        // Empty and near-empty.
        Just("a: null\n"),
        Just("a: \"\"\n"),
        Just("empty_map: {}\nempty_seq: []\n"),
        // Short-name seeds so key_strat's <=6-char keys can actually
        // reach the sequences and mappings inside.
        Just("s: []\n"),
        Just("s: [x]\n"),
        Just("s:\n  - a\n"),
        Just("s:\n  - a\n  - b\n  - c\n"),
        Just("m: {}\n"),
        Just("m: {a: 1}\n"),
        // Explicit-key mappings (`? k` / `: v`). These are parsed without
        // a trailing NEWLINE inside the entry -- it sits beside it as a
        // sibling -- so insertion paths that assume entries own their
        // terminator get it wrong here.
        //
        Just("? a\n: 1\n"),
        Just("? a\n: 1\n? b\n: 2\n"),
        Just("m:\n  ? a\n  : 1\n"),
        Just("m:\n  ? a\n  : 1\n  ? b\n  : 2\n"),
        // Nested mappings at indent widths other than 2.
        Just("m:\n    a: 1\n    b: 2\n"),
        Just("m:\n   a: 1\n   b: 2\n"),
        // A comment indented past its sibling keys must not set the
        // column for newly inserted entries.
        Just("m:\n  a: 1\n      # deep\n  b: 2\n"),
    ]
}

/// A mapping key normalized for comparison.
///
/// The two paths disagree on whether a quoted key keeps its quotes
/// (`get_resolved` builds its map with them, `MergedMapping` decodes them),
/// which is a formatting difference, not a resolution one. Strip the quotes
/// so this property stays about which keys are reachable.
fn normalized_key(text: &str) -> String {
    let t = text.trim();
    for q in ['\'', '"'] {
        if t.len() >= 2 && t.starts_with(q) && t.ends_with(q) {
            return t[1..t.len() - 1].to_string();
        }
    }
    t.to_string()
}

/// Assert `MergedMapping` and `get_resolved` agree for every top-level key
/// whose value is a mapping.
///
/// Compares the full merged key set both ways, so a key that only one path
/// finds -- or invents -- fails regardless of which direction it went.
///
/// Keys are compared by their decoded scalar text: `'0'` and `0` name the
/// same key, and the two paths need not agree on how it was quoted.
fn check_resolution_agrees(doc: &Document, context: &str) -> Result<(), TestCaseError> {
    use yaml_edit::{DocumentResolvedExt, MappingMergedExt};

    let Some(root) = doc.as_mapping() else {
        return Ok(());
    };
    let registry = doc.build_anchor_registry();

    for key in root.keys() {
        let key_text = key.to_string();
        let Some(value) = root.get(key_text.as_str()) else {
            continue;
        };
        let Some(mapping) = value.as_mapping() else {
            continue;
        };

        let mut view_keys: Vec<String> = mapping
            .merged(&registry)
            .keys()
            .map(|k| normalized_key(&k.to_string()))
            .collect();
        view_keys.sort();
        view_keys.dedup();

        // get_resolved works from the document root by key.
        let Some(resolved) = doc.get_resolved(key_text.as_str()) else {
            // MergedMapping always yields a view; if it found keys while
            // get_resolved found no value at all, they disagree.
            if view_keys.is_empty() {
                continue;
            }
            return Err(TestCaseError::fail(format!(
                "after {context}: get_resolved({key_text:?}) returned None but \
                 MergedMapping sees {view_keys:?}\ntext: {:?}",
                doc.to_string()
            )));
        };
        let Some(resolved_map) = resolved.as_mapping() else {
            continue;
        };
        let mut resolved_keys: Vec<String> = resolved_map
            .keys()
            .map(|k| normalized_key(k))
            .filter(|k| k != "<<")
            .collect();
        resolved_keys.sort();
        resolved_keys.dedup();

        if view_keys != resolved_keys {
            return Err(TestCaseError::fail(format!(
                "after {context}: the two resolution paths disagree on {key_text:?}\n  \
                 MergedMapping: {view_keys:?}\n  get_resolved:  {resolved_keys:?}\ntext: {:?}",
                doc.to_string()
            )));
        }
    }
    Ok(())
}

/// Seeds built around anchors, aliases and merge keys.
///
/// These drive [`anchor_resolution_paths_agree`]: every shape here has at
/// least one anchor, alias or `<<` so the resolution code actually runs.
/// `<<: *a` and `<<: [*a, *b]` reach the merge code as an ALIAS node and a
/// sequence of them, which is the distinction a Scalar-only match missed.
fn anchor_seed_strat() -> impl Strategy<Value = &'static str> {
    prop_oneof![
        // Single-alias merge.
        Just("a: &a\n  x: 1\nm:\n  <<: *a\n  z: 3\n"),
        Just("a: &a\n  x: 1\nm:\n  <<: *a\n"),
        // Sequence-of-aliases merge, including override order.
        Just("a: &a\n  x: 1\nb: &b\n  y: 2\nm:\n  <<: [*a, *b]\n  z: 3\n"),
        Just("a: &a\n  x: 1\nb: &b\n  x: 2\nm:\n  <<: [*a, *b]\n"),
        Just("a: &a\n  x: 1\nb: &b\n  x: 2\nm:\n  <<: [*b, *a]\n"),
        // A direct key must win over a merged one.
        Just("a: &a\n  x: 1\nm:\n  <<: *a\n  x: override\n"),
        // Flow-style merge sources.
        Just("a: &a {x: 1}\nm:\n  <<: *a\n  z: 3\n"),
        Just("a: &a {x: 1}\nb: &b {y: 2}\nm: {<<: [*a, *b], z: 3}\n"),
        // Plain aliases with no merge key.
        Just("first: &ref value\nsecond: *ref\n"),
        Just("a: &a\n  x: 1\nm: *a\n"),
        // Nested anchors.
        Just("a: &a\n  inner: &i\n    deep: 1\nm:\n  <<: *a\n  other: *i\n"),
        // A dangling alias must not panic or invent keys.
        Just("m:\n  <<: *missing\n  z: 3\n"),
        Just("m:\n  <<: [*missing, *alsomissing]\n  z: 3\n"),
        // Merge chains: the source itself merges from another anchor.
        Just("a: &a\n  x: 1\nb: &b\n  <<: *a\n  y: 2\nm:\n  <<: *b\n  z: 3\n"),
        // An empty merge source.
        Just("a: &a {}\nm:\n  <<: *a\n  z: 3\n"),
    ]
}

fn apply(doc: &Document, op: &Op) {
    let Some(mapping) = doc.as_mapping() else {
        return;
    };
    match op {
        Op::SetString(k, v) => {
            mapping.set(k.as_str(), v.as_str());
            assert_mapping_set_stuck(&mapping, k.as_str(), v.as_str(), doc);
        }
        Op::SetInt(k, v) => {
            mapping.set(k.as_str(), *v as i64);
            let expected = (*v as i64).to_string();
            assert_mapping_set_stuck(&mapping, k.as_str(), &expected, doc);
        }
        Op::Remove(k) => {
            let existed = mapping.contains_key(k.as_str());
            let entry = mapping.remove(k.as_str());
            // remove() returns Some iff the key existed.
            if entry.is_some() != existed {
                panic!(
                    "MappingRemove({:?}): returned {:?} but contains_key was {}",
                    k,
                    entry.is_some(),
                    existed
                );
            }
            assert_mapping_remove_stuck(&mapping, k.as_str(), existed, doc);
        }
        Op::Rename(a, b) => {
            let renamed = mapping.rename_key(a.as_str(), b.as_str());
            assert_mapping_rename_stuck(&mapping, b.as_str(), renamed, doc);
        }
        Op::Clear => {
            mapping.clear();
            assert_mapping_clear_stuck(&mapping, doc);
        }
        Op::InsertAfter(a, k, v) => {
            let inserted = mapping.insert_after(a.as_str(), k.as_str(), v.as_str());
            if inserted {
                assert_mapping_insert_stuck(&mapping, k.as_str(), v.as_str(), doc, "InsertAfter");
            }
        }
        Op::InsertBefore(a, k, v) => {
            let inserted = mapping.insert_before(a.as_str(), k.as_str(), v.as_str());
            if inserted {
                assert_mapping_insert_stuck(&mapping, k.as_str(), v.as_str(), doc, "InsertBefore");
            }
        }
        Op::InsertAtIndex(i, k, v) => {
            mapping.insert_at_index(*i, k.as_str(), v.as_str());
            assert_mapping_insert_stuck(&mapping, k.as_str(), v.as_str(), doc, "InsertAtIndex");
        }
        Op::MoveAfter(a, k, v) => {
            let _ = mapping.move_after(a.as_str(), k.as_str(), v.as_str());
        }
        Op::MoveBefore(a, k, v) => {
            let _ = mapping.move_before(a.as_str(), k.as_str(), v.as_str());
        }
        Op::RemoveNthOccurrence(k, n) => {
            let _ = mapping.remove_nth_occurrence(k.as_str(), *n);
        }
        Op::ReorderFields(order) => {
            mapping.reorder_fields(order.iter().map(|s| s.as_str()));
        }
        Op::SeqPush(k, v) => {
            if let Some(seq) = mapping.get_sequence(k.as_str()) {
                let before = seq.len();
                seq.push(v.as_str());
                assert_seq_push_stuck(&seq, before, v.as_str(), doc, k.as_str());
            }
        }
        Op::SeqPop(k) => {
            if let Some(seq) = mapping.get_sequence(k.as_str()) {
                let before = seq.len();
                let popped = seq.pop();
                assert_seq_pop_stuck(&seq, before, popped.is_some(), doc, k.as_str());
            }
        }
        Op::SeqInsert(k, i, v) => {
            if let Some(seq) = mapping.get_sequence(k.as_str()) {
                let before = seq.len();
                seq.insert(*i, v.as_str());
                assert_seq_insert_stuck(&seq, before, *i, v.as_str(), doc, k.as_str());
            }
        }
        Op::SeqSet(k, i, v) => {
            if let Some(seq) = mapping.get_sequence(k.as_str()) {
                let before = seq.len();
                let ok = seq.set(*i, v.as_str());
                assert_seq_set_stuck(&seq, before, *i, v.as_str(), ok, doc, k.as_str());
            }
        }
        Op::SeqRemove(k, i) => {
            if let Some(seq) = mapping.get_sequence(k.as_str()) {
                let before = seq.len();
                let removed = seq.remove(*i);
                assert_seq_remove_stuck(&seq, before, *i, removed.is_some(), doc, k.as_str());
            }
        }
        Op::SeqClear(k) => {
            if let Some(seq) = mapping.get_sequence(k.as_str()) {
                seq.clear();
                assert_seq_clear_stuck(&seq, doc, k.as_str());
            }
        }
        Op::NestedSet(k, ik, v) => {
            if let Some(nested) = mapping.get_mapping(k.as_str()) {
                nested.set(ik.as_str(), v.as_str());
            }
        }
        Op::NestedRemove(k, ik) => {
            if let Some(nested) = mapping.get_mapping(k.as_str()) {
                let _ = nested.remove(ik.as_str());
            }
        }
        Op::SetPath(p, v) => {
            // Use try_set_path so we only assert the post-condition when
            // the set actually succeeded. Errors (type mismatches,
            // empty path) are legitimate outcomes and shouldn't fail
            // the post-condition check.
            if doc.try_set_path(p, v.as_str()).is_ok() {
                assert_set_path_stuck(doc, p, v.as_str());
            }
        }
        Op::RemovePath(p) => {
            if doc.try_remove_path(p).is_ok() {
                assert_remove_path_stuck(doc, p, true);
            }
        }
    }
}

fn check(doc: &Document, context: &str) -> Result<(), TestCaseError> {
    let syntax = doc.syntax();
    if let Err(e) = debug::validate_tree(syntax) {
        return Err(TestCaseError::fail(format!(
            "invariant violated after {context}: {e}\ntext: {:?}",
            doc.to_string()
        )));
    }
    if let Err(e) = debug::roundtrip_ok(syntax) {
        return Err(TestCaseError::fail(format!(
            "roundtrip failed after {context}: {e}\ntext: {:?}",
            doc.to_string()
        )));
    }
    Ok(())
}

/// After a `set(k, v)`, `get(k)` must yield the same value we just wrote.
///
/// Complements roundtrip_ok: syntactic stability doesn't imply the
/// mutation actually stuck. If `set` silently no-ops or writes garbage,
/// roundtrip stays happy but semantics diverge. We decode via
/// `Scalar::as_string()` so quoting differences (`""` vs `''`) don't
/// register as mismatches.
fn check_set_stuck(doc: &Document, key: &str, expected: &str) -> Result<(), TestCaseError> {
    let Some(mapping) = doc.as_mapping() else {
        return Ok(());
    };
    let Some(actual) = mapping.get(key) else {
        return Err(TestCaseError::fail(format!(
            "set(\"{key}\", \"{expected}\") didn't stick: key missing after write\ntext: {:?}",
            doc.to_string(),
        )));
    };
    let Some(scalar) = actual.as_scalar() else {
        return Err(TestCaseError::fail(format!(
            "set(\"{key}\", \"{expected}\") didn't stick: value is not a scalar\ntext: {:?}",
            doc.to_string(),
        )));
    };
    let decoded = scalar.as_string();
    if decoded != expected {
        return Err(TestCaseError::fail(format!(
            "set(\"{key}\", \"{expected}\") didn't stick: decoded = {decoded:?}\ntext: {:?}",
            doc.to_string(),
        )));
    }
    Ok(())
}

/// The emitted text must re-parse to the same top-level key sequence.
///
/// `roundtrip_ok` only proves the CST still prints byte-for-byte; it says
/// nothing about whether those bytes still *mean* the same thing. An entry
/// emitted at the wrong column (escaping its parent mapping, say) prints
/// back fine but re-parses with a different structure. Comparing the
/// top-level keys before and after catches exactly that.
fn check_reparse_structure(doc: &Document, context: &str) -> Result<(), TestCaseError> {
    let text = doc.to_string();
    let Some(before) = doc.as_mapping() else {
        return Ok(());
    };
    let before_keys: Vec<String> = before.keys().map(|k| k.to_string()).collect();

    let reparsed = match Document::from_str(&text) {
        Ok(d) => d,
        Err(e) => {
            return Err(TestCaseError::fail(format!(
                "emitted text no longer parses after {context}: {e}\ntext: {text:?}"
            )))
        }
    };
    // A document emptied of every key legitimately renders as nothing,
    // which parses back as an empty document rather than a mapping.
    let after_keys: Vec<String> = match reparsed.as_mapping() {
        Some(after) => after.keys().map(|k| k.to_string()).collect(),
        None if before_keys.is_empty() => return Ok(()),
        None => {
            return Err(TestCaseError::fail(format!(
                "emitted text no longer parses as a mapping after {context}\n  \
                 expected keys: {before_keys:?}\ntext: {text:?}"
            )))
        }
    };

    if before_keys != after_keys {
        return Err(TestCaseError::fail(format!(
            "re-parsing changed the top-level keys after {context}:\n               before: {before_keys:?}\n  after:  {after_keys:?}\ntext: {text:?}"
        )));
    }
    Ok(())
}

proptest! {
    #![proptest_config(ProptestConfig {
        cases: 256,
        // Every mutation runs validate + roundtrip; large sequences
        // slow the suite without adding coverage.
        max_shrink_iters: 1024,
        ..ProptestConfig::default()
    })]

    /// The two ways to read through anchors must give the same answer.
    ///
    /// `MergedMapping` walks the CST on demand; `get_resolved` builds a
    /// BTreeMap up front. They are separate implementations of the same
    /// semantics, so any document where they disagree is a bug in one of
    /// them -- that is how `<<: [*a, *b]` was found silently dropping every
    /// inherited key.
    #[test]
    fn anchor_resolution_paths_agree(
        seed in anchor_seed_strat(),
        ops in prop::collection::vec(op_strat(), 0..4),
    ) {
        let doc = Document::from_str(seed).unwrap();
        check(&doc, "parse")?;
        check_resolution_agrees(&doc, "parse")?;

        // Mutating the document must not make the two paths diverge either.
        for (i, op) in ops.iter().enumerate() {
            apply(&doc, op);
            let context = format!("op[{i}] = {op:?}");
            check(&doc, &context)?;
            check_resolution_agrees(&doc, &context)?;
        }
    }

    #[test]
    fn seeded_document_mutations_preserve_invariants(
        seed in seed_strat(),
        ops in prop::collection::vec(op_strat(), 0..8),
    ) {
        let doc = Document::from_str(seed).unwrap();
        check(&doc, "parse")?;
        for (i, op) in ops.iter().enumerate() {
            apply(&doc, op);
            check(&doc, &format!("op[{i}] = {op:?}"))?;
            check_reparse_structure(&doc, &format!("op[{i}] = {op:?}"))?;
            // Semantic check: a set-like op that succeeded should be
            // observable via get(). Skip ops whose semantics depend on
            // preconditions we haven't tracked (nested-mapping/seq ops
            // silently no-op if the target isn't the right kind).
            match op {
                Op::SetString(k, v) => check_set_stuck(&doc, k, v)?,
                Op::SetInt(k, v) => check_set_stuck(&doc, k, &v.to_string())?,
                _ => {}
            }
        }
    }
}
