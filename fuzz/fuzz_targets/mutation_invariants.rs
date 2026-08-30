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
use yaml_edit::debug;

// The mutation post-conditions are shared with the proptest target.
// `include!` pulls them in verbatim because fuzz targets are separate
// crates and can't `mod common;` into the tests directory. The include
// brings its own `use` statements for Document, FromStr, YamlPath.
include!("../../tests/common/mutation_checks.rs");

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

/// True when the path parses into a non-empty segment list, i.e.
/// something set_path / remove_path can actually address. Filters out
/// empty strings, stray dots (`"..."`), and unclosed brackets. Skipping
/// these in the fuzz mirrors the library's silent no-op behaviour on
/// invalid paths so the post-condition check only runs when set_path
/// was expected to succeed.
fn is_settable_path(path: &str) -> bool {
    yaml_edit::path::try_parse_path(path)
        .map(|s| !s.is_empty())
        .unwrap_or(false)
}

fn apply(doc: &Document, op: &Op) {
    let Some(mapping) = doc.as_mapping() else {
        return;
    };
    match op {
        Op::SetString(k, v) => {
            mapping.set(as_str(k), as_str(v));
            assert_mapping_set_stuck(&mapping, as_str(k), as_str(v), doc);
        }
        Op::SetInt(k, v) => {
            mapping.set(as_str(k), *v as i64);
            let expected = (*v as i64).to_string();
            assert_mapping_set_stuck(&mapping, as_str(k), &expected, doc);
        }
        Op::Remove(k) => {
            let existed = mapping.contains_key(as_str(k));
            let entry = mapping.remove(as_str(k));
            if entry.is_some() != existed {
                panic!(
                    "MappingRemove({:?}): returned {:?} but contains_key was {}",
                    as_str(k),
                    entry.is_some(),
                    existed
                );
            }
            assert_mapping_remove_stuck(&mapping, as_str(k), existed, doc);
        }
        Op::Rename(a, b) => {
            let renamed = mapping.rename_key(as_str(a), as_str(b));
            assert_mapping_rename_stuck(&mapping, as_str(b), renamed, doc);
        }
        Op::Clear => {
            mapping.clear();
            assert_mapping_clear_stuck(&mapping, doc);
        }
        Op::InsertAfter(a, k, v) => {
            let inserted = mapping.insert_after(as_str(a), as_str(k), as_str(v));
            if inserted {
                assert_mapping_insert_stuck(&mapping, as_str(k), as_str(v), doc, "InsertAfter");
            }
        }
        Op::InsertBefore(a, k, v) => {
            let inserted = mapping.insert_before(as_str(a), as_str(k), as_str(v));
            if inserted {
                assert_mapping_insert_stuck(&mapping, as_str(k), as_str(v), doc, "InsertBefore");
            }
        }
        Op::InsertAtIndex(i, k, v) => {
            mapping.insert_at_index(*i as usize, as_str(k), as_str(v));
            assert_mapping_insert_stuck(&mapping, as_str(k), as_str(v), doc, "InsertAtIndex");
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
            // Skip inputs that parse into an empty segment list (empty
            // string, "..", stray dots): set_path silently no-ops on
            // those and the follow-up assertion would falsely fail.
            if !p.is_empty() {
                let path = dotted(p);
                if is_settable_path(&path) {
                    doc.set_path(&path, as_str(v));
                    assert_set_path_stuck(doc, &path, as_str(v));
                }
            }
        }
        Op::RemovePath(p) => {
            if !p.is_empty() {
                let path = dotted(p);
                if is_settable_path(&path) {
                    let removed = doc.remove_path(&path);
                    assert_remove_path_stuck(doc, &path, removed);
                }
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
