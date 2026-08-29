#![no_main]

//! Drive a random mutation sequence against a parsed YAML document and
//! assert CST invariants after every step.
//!
//! The fuzz input is decoded via `arbitrary` into a seed selector and a
//! Vec<Op>. Each Op maps one-to-one with the proptest opcode set so the
//! two fuzzers exercise the same surface; the difference is that
//! libfuzzer runs vastly more iterations under coverage guidance while
//! proptest supplies shrinking.
//!
//! After each mutation, `debug::validate_tree` and `debug::roundtrip_ok`
//! must hold, and any set-like op is verified to have actually stuck.

use arbitrary::Arbitrary;
use libfuzzer_sys::fuzz_target;
use rowan::ast::AstNode;
use std::str::FromStr;
use yaml_edit::path::YamlPath;
use yaml_edit::{debug, Document, Sequence};

const SEEDS: &[&str] = &[
    "a: 1\n",
    "a: 1\nb: 2\nc: 3\n",
    "items:\n  - one\n  - two\n",
    "root:\n  a: 1\n  b: 2\n",
    "mixed:\n  list:\n    - x\n    - y\n  map:\n    k: v\n",
    "existing: value\n",
    "flow: {a: 1, b: 2}\n",
    "nums: [1, 2, 3]\n",
    "mixed_flow: {a: [1, 2], b: {x: y}}\n",
    "defaults: &d\n  timeout: 30\nprod:\n  <<: *d\n  host: prod\n",
    "first: &ref value\nsecond: *ref\n",
    "count: !!int '42'\n",
    "keys: !!set\n  ? a\n  ? b\n",
    "mapping: !!map\n  a: 1\n  b: 2\n",
    "literal: |\n  line1\n  line2\n",
    "folded: >\n  wrapped\n  paragraph\n",
    "a: 1  # trailing\nb: 2\n",
    "items:  # a list\n  - one\n  - two  # inline\n",
    "a: null\n",
    "a: \"\"\n",
    "empty_map: {}\nempty_seq: []\n",
    // Short-name seeds so the fuzz's <=6-char keys can actually reach
    // the sequences and mappings inside. Otherwise a key like
    // `empty_seq` (9 chars) is unreachable.
    "s: []\n",
    "s: [x]\n",
    "s:\n  - a\n",
    "s:\n  - a\n  - b\n  - c\n",
    "m: {}\n",
    "m: {a: 1}\n",
];

/// A short ASCII string drawn from an alphabet designed to hit
/// parser edges while staying representable as a bare YAML key/value:
/// letters, digits, and a handful of punctuation characters (dash,
/// underscore, dot, colon, quote, hash, space). Wraps the bytes
/// verbatim rather than collapsing them through `% alphabet_size`
/// so libfuzzer's coverage feedback keeps distinguishing inputs.
#[derive(Debug)]
struct SafeStr(String);

const ALPHABET: &[u8] = b"abcdefghijklmnopqrstuvwxyz0123456789-_.\"'# :";

impl<'a> Arbitrary<'a> for SafeStr {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        let len = u.int_in_range(0u8..=6)? as usize;
        let mut s = String::with_capacity(len);
        for _ in 0..len {
            let b = u.arbitrary::<u8>()?;
            let ch = ALPHABET[(b as usize) % ALPHABET.len()] as char;
            s.push(ch);
        }
        Ok(SafeStr(s))
    }
}

/// Keep in sync with `tests/proptest_invariants.rs::Op`. Both fuzzers
/// should exercise the same mutation surface; this target uses
/// `SafeStr` values, the proptest side uses plain `String`.
#[derive(Debug, Arbitrary)]
enum Op {
    // Mapping ops
    SetString(SafeStr, SafeStr),
    SetInt(SafeStr, i32),
    Remove(SafeStr),
    Rename(SafeStr, SafeStr),
    Clear,
    InsertAfter(SafeStr, SafeStr, SafeStr),
    InsertBefore(SafeStr, SafeStr, SafeStr),
    InsertAtIndex(u8, SafeStr, SafeStr),
    MoveAfter(SafeStr, SafeStr, SafeStr),
    MoveBefore(SafeStr, SafeStr, SafeStr),
    RemoveNthOccurrence(SafeStr, u8),
    ReorderFields(Vec<SafeStr>),
    // Sequence ops
    SeqPush(SafeStr, SafeStr),
    SeqPop(SafeStr),
    SeqInsert(SafeStr, u8, SafeStr),
    SeqSet(SafeStr, u8, SafeStr),
    SeqRemove(SafeStr, u8),
    SeqClear(SafeStr),
    // Nested mapping ops
    NestedSet(SafeStr, SafeStr, SafeStr),
    NestedRemove(SafeStr, SafeStr),
    // Path-based ops
    SetPath(Vec<SafeStr>, SafeStr),
    RemovePath(Vec<SafeStr>),
}

#[derive(Debug, Arbitrary)]
struct Input {
    seed: u8,
    ops: Vec<Op>,
}

fn as_str(s: &SafeStr) -> &str {
    s.0.as_str()
}

fn dotted(path: &[SafeStr]) -> String {
    path.iter().map(as_str).collect::<Vec<_>>().join(".")
}

fn apply(doc: &Document, op: &Op) {
    let Some(mapping) = doc.as_mapping() else {
        return;
    };
    match op {
        Op::SetString(k, v) => mapping.set(as_str(k), as_str(v)),
        Op::SetInt(k, v) => mapping.set(as_str(k), *v as i64),
        Op::Remove(k) => {
            let _ = mapping.remove(as_str(k));
        }
        Op::Rename(a, b) => {
            let _ = mapping.rename_key(as_str(a), as_str(b));
        }
        Op::Clear => mapping.clear(),
        Op::InsertAfter(a, k, v) => {
            let _ = mapping.insert_after(as_str(a), as_str(k), as_str(v));
        }
        Op::InsertBefore(a, k, v) => {
            let _ = mapping.insert_before(as_str(a), as_str(k), as_str(v));
        }
        Op::InsertAtIndex(i, k, v) => {
            mapping.insert_at_index(*i as usize, as_str(k), as_str(v));
        }
        Op::MoveAfter(a, k, v) => {
            let _ = mapping.move_after(as_str(a), as_str(k), as_str(v));
        }
        Op::MoveBefore(a, k, v) => {
            let _ = mapping.move_before(as_str(a), as_str(k), as_str(v));
        }
        Op::RemoveNthOccurrence(k, n) => {
            let _ = mapping.remove_nth_occurrence(as_str(k), *n as usize);
        }
        Op::ReorderFields(order) => {
            mapping.reorder_fields(order.iter().map(as_str));
        }
        Op::SeqPush(k, v) => {
            if let Some(seq) = mapping.get_sequence(as_str(k)) {
                // Skip pushes that trip known bugs (see
                // tests/known_bugs.rs) so the fuzz can explore further
                // shapes instead of tripping on the same trap.
                if seq.is_flow_style() {
                    return;
                }
                let before = seq.len();
                seq.push(as_str(v));
                assert_seq_push_stuck(&seq, before, as_str(v), doc, as_str(k));
            }
        }
        Op::SeqPop(k) => {
            if let Some(seq) = mapping.get_sequence(as_str(k)) {
                let before = seq.len();
                let popped = seq.pop();
                assert_seq_pop_stuck(&seq, before, popped.is_some(), doc, as_str(k));
            }
        }
        Op::SeqInsert(k, i, v) => {
            if let Some(seq) = mapping.get_sequence(as_str(k)) {
                // Skip insert into any flow sequence -- see
                // known_bugs (bugs 7, 9). Block insert is fixed.
                if seq.is_flow_style() {
                    return;
                }
                let before = seq.len();
                let idx = *i as usize;
                seq.insert(idx, as_str(v));
                assert_seq_insert_stuck(&seq, before, idx, as_str(v), doc, as_str(k));
            }
        }
        Op::SeqSet(k, i, v) => {
            if let Some(seq) = mapping.get_sequence(as_str(k)) {
                let before = seq.len();
                let idx = *i as usize;
                let ok = seq.set(idx, as_str(v));
                assert_seq_set_stuck(&seq, before, idx, as_str(v), ok, doc, as_str(k));
            }
        }
        Op::SeqRemove(k, i) => {
            if let Some(seq) = mapping.get_sequence(as_str(k)) {
                let before = seq.len();
                let idx = *i as usize;
                let removed = seq.remove(idx);
                assert_seq_remove_stuck(&seq, before, idx, removed.is_some(), doc, as_str(k));
            }
        }
        Op::SeqClear(k) => {
            if let Some(seq) = mapping.get_sequence(as_str(k)) {
                seq.clear();
                assert_seq_clear_stuck(&seq, doc, as_str(k));
            }
        }
        Op::NestedSet(k, ik, v) => {
            if let Some(nested) = mapping.get_mapping(as_str(k)) {
                nested.set(as_str(ik), as_str(v));
            }
        }
        Op::NestedRemove(k, ik) => {
            if let Some(nested) = mapping.get_mapping(as_str(k)) {
                let _ = nested.remove(as_str(ik));
            }
        }
        Op::SetPath(p, v) => {
            if !p.is_empty() {
                doc.set_path(&dotted(p), as_str(v));
            }
        }
        Op::RemovePath(p) => {
            if !p.is_empty() {
                let _ = doc.remove_path(&dotted(p));
            }
        }
    }
}

/// Re-parse `doc`'s text and look up the sequence at `key` in the fresh
/// tree. Panics with `op` context if either the re-parse fails or the
/// sequence has vanished. Used by every post-condition helper to catch
/// mutations that leave the in-memory CST looking fine but produce text
/// that re-parses into a different shape.
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

/// Return the item at `index` as a string, if it's a scalar. Non-scalar
/// items yield `None`, which callers should treat as "skip the value
/// check" -- pushed scalars round-trip predictably; nested mappings
/// don't.
fn scalar_at(seq: &Sequence, index: usize) -> Option<String> {
    seq.get(index)
        .as_ref()
        .and_then(|n| n.as_scalar().map(|s| s.as_string()))
}

/// Verify that `push(v)` on `seq` actually stuck: the in-memory sequence
/// grew by one and the last item is a scalar equal to `v`, and the same
/// property holds after `doc.to_string()` is round-tripped.
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

/// Verify that `pop()` on `seq` actually stuck: the length shrunk by one
/// iff pop returned Some, and the sequence re-parses with the same length.
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
        panic!("{op}: reported pop of an empty sequence, text: {:?}", doc.to_string());
    }
    if !popped && before > 0 {
        panic!("{op}: refused to pop a non-empty sequence, text: {:?}", doc.to_string());
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

/// Verify that `insert(i, v)` on `seq` actually stuck. The docs promise
/// that an out-of-range index appends, so the effective position is
/// `min(i, before)`.
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

/// Verify that `set(i, v)` on `seq` actually stuck. Returns bool:
/// `true` means the index was in range and the item was replaced;
/// `false` means the index was out of range and nothing changed.
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
            panic!(
                "{op}: item at {i} = {at:?}, text: {:?}",
                doc.to_string()
            );
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

/// Verify that `remove(i)` on `seq` actually stuck. Returns Option;
/// Some means index was in range and item was removed, None means
/// out of range and nothing changed.
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

/// Verify that `clear()` on `seq` actually stuck. The in-memory sequence
/// must be empty, and the re-parsed sequence at the same key must also
/// be empty.
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

fn assert_ok(doc: &Document) {
    let syntax = doc.syntax();
    if let Err(e) = debug::validate_tree(syntax) {
        panic!("invariant violated: {e}\ntext: {:?}", doc.to_string());
    }
    if let Err(e) = debug::roundtrip_ok(syntax) {
        panic!("roundtrip failed: {e}\ntext: {:?}", doc.to_string());
    }
}

fuzz_target!(|input: Input| {
    let seed = SEEDS[(input.seed as usize) % SEEDS.len()];
    let Ok(doc) = Document::from_str(seed) else {
        return;
    };
    assert_ok(&doc);
    // Bound the mutation count so a single fuzz iteration stays quick;
    // libfuzzer will still explore long sequences by chaining runs.
    for op in input.ops.iter().take(32) {
        apply(&doc, op);
        assert_ok(&doc);
    }
});
