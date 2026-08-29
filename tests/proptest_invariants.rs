//! Property-based tests for CST invariants under random mutations.
//!
//! For each generated seed document + random mutation sequence, we
//! assert `debug::validate_tree` and `debug::roundtrip_ok` after every
//! step. The mutation shrinker finds the smallest violating input.

use proptest::prelude::*;
use rowan::ast::AstNode;
use std::str::FromStr;
use yaml_edit::path::YamlPath;
use yaml_edit::{debug, Document, Mapping, Sequence};

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
        // The !!set seed is intentionally omitted: the parser has a
        // pre-existing bug where a column-0 mapping entry appended
        // after a !!set block gets absorbed into the set. Adding it
        // back once the parser is fixed will let this proptest cover
        // set-adjacent shapes too.
        // Just("keys: !!set\n  ? a\n  ? b\n"),
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
    ]
}

// Sequence-mutation post-conditions. Kept in sync with the fuzz target's
// versions -- see fuzz/fuzz_targets/mutation_invariants.rs for the full
// rationale on each check.

fn reparse_seq(doc: &Document, key: &str, op: &str) -> Sequence {
    let text = doc.to_string();
    let reparsed = Document::from_str(&text)
        .unwrap_or_else(|e| panic!("{op}: re-parse failed ({e}), text: {text:?}"));
    reparsed
        .as_mapping()
        .and_then(|m| m.get_sequence(key))
        .unwrap_or_else(|| {
            panic!("{op}: re-parsed doc has no sequence at key {key:?}, text: {text:?}")
        })
}

fn scalar_at(seq: &Sequence, index: usize) -> Option<String> {
    seq.get(index)
        .as_ref()
        .and_then(|n| n.as_scalar().map(|s| s.as_string()))
}

fn assert_seq_push_stuck(seq: &Sequence, before: usize, v: &str, doc: &Document, key: &str) {
    let op = format!("SeqPush({v:?})");
    let after = seq.len();
    if after != before + 1 {
        panic!(
            "{op}: len {before} -> {after} (expected {}), text: {:?}",
            before + 1,
            doc.to_string()
        );
    }
    let last = scalar_at(seq, after - 1);
    if last.as_deref() != Some(v) {
        panic!("{op}: last item = {last:?}, text: {:?}", doc.to_string());
    }
    let reparsed = reparse_seq(doc, key, &op);
    let r_len = reparsed.len();
    let r_last = scalar_at(&reparsed, r_len.saturating_sub(1));
    if r_len != after || r_last.as_deref() != Some(v) {
        panic!(
            "{op}: reparse drift: len {after} vs {r_len}, last {:?} vs {r_last:?}, text: {:?}",
            Some(v),
            doc.to_string()
        );
    }
}

fn assert_seq_pop_stuck(seq: &Sequence, before: usize, popped: bool, doc: &Document, key: &str) {
    let op = "SeqPop";
    let after = seq.len();
    let expected = if popped { before - 1 } else { before };
    if after != expected {
        panic!(
            "{op}: popped={popped} len {before} -> {after} (expected {expected}), text: {:?}",
            doc.to_string()
        );
    }
    if popped && before == 0 {
        panic!(
            "{op}: reported pop of an empty sequence, text: {:?}",
            doc.to_string()
        );
    }
    if !popped && before > 0 {
        panic!(
            "{op}: refused to pop a non-empty sequence, text: {:?}",
            doc.to_string()
        );
    }
    let reparsed = reparse_seq(doc, key, op);
    if reparsed.len() != after {
        panic!(
            "{op}: reparse drift: len {after} vs {}, text: {:?}",
            reparsed.len(),
            doc.to_string()
        );
    }
}

fn assert_seq_insert_stuck(
    seq: &Sequence,
    before: usize,
    i: usize,
    v: &str,
    doc: &Document,
    key: &str,
) {
    let op = format!("SeqInsert({i}, {v:?})");
    let after = seq.len();
    if after != before + 1 {
        panic!(
            "{op}: len {before} -> {after} (expected {}), text: {:?}",
            before + 1,
            doc.to_string()
        );
    }
    let effective = i.min(before);
    let at = scalar_at(seq, effective);
    if at.as_deref() != Some(v) {
        panic!(
            "{op}: item at effective index {effective} = {at:?}, text: {:?}",
            doc.to_string()
        );
    }
    let reparsed = reparse_seq(doc, key, &op);
    let r_at = scalar_at(&reparsed, effective);
    if reparsed.len() != after || r_at.as_deref() != Some(v) {
        panic!(
            "{op}: reparse drift: len {after} vs {}, at {effective} {:?} vs {r_at:?}, text: {:?}",
            reparsed.len(),
            Some(v),
            doc.to_string()
        );
    }
}

fn assert_seq_set_stuck(
    seq: &Sequence,
    before: usize,
    i: usize,
    v: &str,
    ok: bool,
    doc: &Document,
    key: &str,
) {
    let op = format!("SeqSet({i}, {v:?})");
    let after = seq.len();
    if after != before {
        panic!(
            "{op}: ok={ok} len {before} -> {after} (expected unchanged), text: {:?}",
            doc.to_string()
        );
    }
    if ok != (i < before) {
        panic!(
            "{op}: ok={ok} but i={i} vs before={before}, text: {:?}",
            doc.to_string()
        );
    }
    if ok {
        let at = scalar_at(seq, i);
        if at.as_deref() != Some(v) {
            panic!("{op}: item at {i} = {at:?}, text: {:?}", doc.to_string());
        }
        let reparsed = reparse_seq(doc, key, &op);
        let r_at = scalar_at(&reparsed, i);
        if reparsed.len() != after || r_at.as_deref() != Some(v) {
            panic!(
                "{op}: reparse drift: len {after} vs {}, at {i} {:?} vs {r_at:?}, text: {:?}",
                reparsed.len(),
                Some(v),
                doc.to_string()
            );
        }
    }
}

fn assert_seq_remove_stuck(
    seq: &Sequence,
    before: usize,
    i: usize,
    removed: bool,
    doc: &Document,
    key: &str,
) {
    let op = format!("SeqRemove({i})");
    let after = seq.len();
    let expected = if removed { before - 1 } else { before };
    if after != expected {
        panic!(
            "{op}: removed={removed} len {before} -> {after} (expected {expected}), text: {:?}",
            doc.to_string()
        );
    }
    if removed != (i < before) {
        panic!(
            "{op}: removed={removed} but i={i} vs before={before}, text: {:?}",
            doc.to_string()
        );
    }
    let reparsed = reparse_seq(doc, key, &op);
    if reparsed.len() != after {
        panic!(
            "{op}: reparse drift: len {after} vs {}, text: {:?}",
            reparsed.len(),
            doc.to_string()
        );
    }
}

fn assert_seq_clear_stuck(seq: &Sequence, doc: &Document, key: &str) {
    let op = "SeqClear";
    if !seq.is_empty() {
        panic!(
            "{op}: seq not empty after clear (len={}), text: {:?}",
            seq.len(),
            doc.to_string()
        );
    }
    let reparsed = reparse_seq(doc, key, op);
    if !reparsed.is_empty() {
        panic!(
            "{op}: reparse drift: reparsed len {} (expected 0), text: {:?}",
            reparsed.len(),
            doc.to_string()
        );
    }
}

// Mapping-mutation post-conditions.

fn reparse_mapping(doc: &Document, op: &str) -> Mapping {
    let text = doc.to_string();
    let reparsed = Document::from_str(&text)
        .unwrap_or_else(|e| panic!("{op}: re-parse failed ({e}), text: {text:?}"));
    reparsed
        .as_mapping()
        .unwrap_or_else(|| panic!("{op}: re-parsed doc is not a mapping, text: {text:?}"))
}

fn scalar_value(m: &Mapping, key: &str) -> Option<String> {
    m.get(key)
        .as_ref()
        .and_then(|n| n.as_scalar().map(|s| s.as_string()))
}

/// True if `mapping`'s enclosing container is a MAPPING_ENTRY (nested
/// under a key). Top-level mappings live directly under the DOCUMENT
/// node.
fn is_nested_mapping(mapping: &Mapping) -> bool {
    use yaml_edit::SyntaxKind;
    mapping
        .syntax()
        .parent()
        .filter(|p| p.kind() == SyntaxKind::VALUE)
        .and_then(|v| v.parent())
        .is_some_and(|e| e.kind() == SyntaxKind::MAPPING_ENTRY)
}

/// Verify that `mapping.set(k, v)` for a string value landed: the key
/// exists in memory with scalar value `v`, and the same holds after
/// re-parse.
fn assert_mapping_set_stuck(mapping: &Mapping, k: &str, v: &str, doc: &Document) {
    let op = format!("MappingSet({k:?}, {v:?})");
    if !mapping.contains_key(k) {
        panic!("{op}: key missing after set, text: {:?}", doc.to_string());
    }
    let got = scalar_value(mapping, k);
    if got.as_deref() != Some(v) {
        panic!(
            "{op}: value at {k:?} = {got:?}, text: {:?}",
            doc.to_string()
        );
    }
    let reparsed = reparse_mapping(doc, &op);
    if !reparsed.contains_key(k) {
        panic!(
            "{op}: reparse drift: key {k:?} missing, text: {:?}",
            doc.to_string()
        );
    }
    let r_got = scalar_value(&reparsed, k);
    if r_got.as_deref() != Some(v) {
        panic!(
            "{op}: reparse drift: value at {k:?} = {r_got:?} (expected {v:?}), text: {:?}",
            doc.to_string()
        );
    }
}

/// Verify that `mapping.remove(k)` did what it claimed.
fn assert_mapping_remove_stuck(mapping: &Mapping, k: &str, existed: bool, doc: &Document) {
    let op = format!("MappingRemove({k:?})");
    if existed && mapping.contains_key(k) {
        panic!(
            "{op}: reported removed but key still present, text: {:?}",
            doc.to_string()
        );
    }
    if !existed && mapping.contains_key(k) {
        // remove returned None but key is present -- that's a
        // pre-existing state, not our concern.
        return;
    }
    // An emptied top-level mapping renders as `""` which re-parses as
    // no mapping (see the note in Mapping::remove_entry_at). That's
    // an accepted intermediate state, not a bug: nothing was lost
    // because there was nothing left to save. Skip the re-parse check
    // in that case.
    if mapping.is_empty() && !is_nested_mapping(mapping) {
        return;
    }
    let reparsed = reparse_mapping(doc, &op);
    if existed && reparsed.contains_key(k) {
        panic!(
            "{op}: reparse drift: key {k:?} came back, text: {:?}",
            doc.to_string()
        );
    }
}

/// Verify that `mapping.rename_key(old, new)` returned truthfully.
/// `rename_key` renames only the first occurrence of `old`, so with
/// duplicate keys `old` may legitimately still be present after a
/// successful rename; only the presence of `new` is a hard requirement.
fn assert_mapping_rename_stuck(
    mapping: &Mapping,
    _old: &str,
    new: &str,
    renamed: bool,
    doc: &Document,
) {
    let op = format!("MappingRename(-> {new:?})");
    if renamed {
        if !mapping.contains_key(new) {
            panic!(
                "{op}: reported rename but new key missing, text: {:?}",
                doc.to_string()
            );
        }
        let reparsed = reparse_mapping(doc, &op);
        if !reparsed.contains_key(new) {
            panic!(
                "{op}: reparse drift: new key {new:?} missing, text: {:?}",
                doc.to_string()
            );
        }
    }
}

/// Verify that `mapping.clear()` emptied the mapping in memory and
/// after re-parse.
fn assert_mapping_clear_stuck(mapping: &Mapping, doc: &Document) {
    let op = "MappingClear";
    if !mapping.is_empty() {
        panic!("{op}: not empty in memory, text: {:?}", doc.to_string());
    }
    let reparsed = reparse_mapping(doc, op);
    if !reparsed.is_empty() {
        panic!("{op}: not empty after reparse, text: {:?}", doc.to_string());
    }
}

/// Verify that a mapping-insert op (insert_after/before/at_index) put
/// the key in the mapping with the requested scalar value, and that
/// this survives re-parse.
fn assert_mapping_insert_stuck(mapping: &Mapping, k: &str, v: &str, doc: &Document, op_name: &str) {
    let op = format!("{op_name}({k:?}, {v:?})");
    if !mapping.contains_key(k) {
        panic!(
            "{op}: key missing after insert, text: {:?}",
            doc.to_string()
        );
    }
    let got = scalar_value(mapping, k);
    if got.as_deref() != Some(v) {
        panic!(
            "{op}: value at {k:?} = {got:?}, text: {:?}",
            doc.to_string()
        );
    }
    let reparsed = reparse_mapping(doc, &op);
    if !reparsed.contains_key(k) {
        panic!(
            "{op}: reparse drift: key {k:?} missing, text: {:?}",
            doc.to_string()
        );
    }
    let r_got = scalar_value(&reparsed, k);
    if r_got.as_deref() != Some(v) {
        panic!(
            "{op}: reparse drift: value at {k:?} = {r_got:?}, text: {:?}",
            doc.to_string()
        );
    }
}

/// Verify that `doc.set_path(path, v)` landed `v` at `path` -- both
/// in-memory and after re-parse. set_path is a no-op when the doc has
/// no root mapping, so an empty-doc call is not a failure.
fn assert_set_path_stuck(doc: &Document, path: &str, v: &str) {
    let op = format!("SetPath({path:?}, {v:?})");
    // If the doc has no root mapping, set_path silently returned.
    if doc.as_mapping().is_none() {
        return;
    }
    let got = doc
        .get_path(path)
        .as_ref()
        .and_then(|n| n.as_scalar().map(|s| s.as_string()));
    if got.as_deref() != Some(v) {
        panic!(
            "{op}: get_path returned {got:?} in memory, text: {:?}",
            doc.to_string()
        );
    }
    let text = doc.to_string();
    let Ok(reparsed) = Document::from_str(&text) else {
        panic!("{op}: re-parse failed, text: {text:?}");
    };
    let r_got = reparsed
        .get_path(path)
        .as_ref()
        .and_then(|n| n.as_scalar().map(|s| s.as_string()));
    if r_got.as_deref() != Some(v) {
        panic!("{op}: reparse drift: get_path returned {r_got:?}, text: {text:?}");
    }
}

/// Verify that `doc.remove_path(path)` did what it claimed and that
/// the path is gone after re-parse.
fn assert_remove_path_stuck(doc: &Document, path: &str, removed: bool) {
    let op = format!("RemovePath({path:?})");
    if doc.as_mapping().is_none() {
        return;
    }
    if removed && doc.get_path(path).is_some() {
        panic!(
            "{op}: reported removed but path still present, text: {:?}",
            doc.to_string()
        );
    }
    // Only require re-parse to hide the path when remove reported
    // success; a `false` return may leave a pre-existing state.
    if !removed {
        return;
    }
    let text = doc.to_string();
    let Ok(reparsed) = Document::from_str(&text) else {
        panic!("{op}: re-parse failed, text: {text:?}");
    };
    if reparsed.get_path(path).is_some() {
        panic!("{op}: reparse drift: path came back, text: {text:?}");
    }
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
            assert_mapping_rename_stuck(&mapping, a.as_str(), b.as_str(), renamed, doc);
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
            doc.set_path(p, v.as_str());
            assert_set_path_stuck(doc, p, v.as_str());
        }
        Op::RemovePath(p) => {
            let removed = doc.remove_path(p);
            assert_remove_path_stuck(doc, p, removed);
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

proptest! {
    #![proptest_config(ProptestConfig {
        cases: 256,
        // Every mutation runs validate + roundtrip; large sequences
        // slow the suite without adding coverage.
        max_shrink_iters: 1024,
        ..ProptestConfig::default()
    })]

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
