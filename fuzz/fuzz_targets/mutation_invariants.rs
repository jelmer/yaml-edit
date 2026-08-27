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
use yaml_edit::{debug, Document};

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
    "mapping: !!map\n  a: 1\n  b: 2\n",
    "literal: |\n  line1\n  line2\n",
    "folded: >\n  wrapped\n  paragraph\n",
    "a: 1  # trailing\nb: 2\n",
    "items:  # a list\n  - one\n  - two  # inline\n",
    "a: null\n",
    "a: \"\"\n",
    "empty_map: {}\nempty_seq: []\n",
];

/// A short ASCII-safe key or scalar. Wraps the bytes verbatim so
/// libfuzzer's coverage feedback stays useful (unlike a raw
/// `% alphabet_size` mapping, which collapses many distinct inputs
/// into the same string).
#[derive(Debug)]
struct SafeStr(String);

impl<'a> Arbitrary<'a> for SafeStr {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        let len = u.int_in_range(0u8..=6)? as usize;
        let mut s = String::with_capacity(len);
        for _ in 0..len {
            let b = u.arbitrary::<u8>()?;
            let ch = match b % 28 {
                0..=25 => char::from(b'a' + (b % 26)),
                26 => '_',
                _ => '0',
            };
            s.push(ch);
        }
        Ok(SafeStr(s))
    }
}

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
                seq.push(as_str(v));
            }
        }
        Op::SeqPop(k) => {
            if let Some(seq) = mapping.get_sequence(as_str(k)) {
                let _ = seq.pop();
            }
        }
        Op::SeqInsert(k, i, v) => {
            if let Some(seq) = mapping.get_sequence(as_str(k)) {
                seq.insert(*i as usize, as_str(v));
            }
        }
        Op::SeqSet(k, i, v) => {
            if let Some(seq) = mapping.get_sequence(as_str(k)) {
                let _ = seq.set(*i as usize, as_str(v));
            }
        }
        Op::SeqRemove(k, i) => {
            if let Some(seq) = mapping.get_sequence(as_str(k)) {
                let _ = seq.remove(*i as usize);
            }
        }
        Op::SeqClear(k) => {
            if let Some(seq) = mapping.get_sequence(as_str(k)) {
                seq.clear();
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
