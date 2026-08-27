#![no_main]

//! Drive a random mutation sequence against a parsed YAML document and
//! assert CST invariants after every step.
//!
//! The first byte of the fuzz input selects a seed document. The rest
//! is consumed as an opcode stream: each opcode reads one byte for the
//! operation kind and small byte-slices for its parameters. After each
//! mutation, `debug::validate_tree` and `debug::roundtrip_ok` must hold.

use libfuzzer_sys::fuzz_target;
use rowan::ast::AstNode;
use std::str::FromStr;
use yaml_edit::{debug, Document};

const SEEDS: &[&str] = &[
    "a: 1\n",
    "a: 1\nb: 2\nc: 3\n",
    "items:\n  - one\n  - two\n",
    "root:\n  a: 1\n  b: 2\n",
    "mixed:\n  list:\n    - x\n    - y\n  map:\n    k: v\n",
    "existing: value\n",
];

/// Read `n` bytes from `data` (advancing the cursor), decoded as an
/// ASCII-safe key/value string. Returns `None` if `data` is exhausted.
fn read_str(data: &mut &[u8], n: usize) -> Option<String> {
    if data.len() < n {
        return None;
    }
    let (head, tail) = data.split_at(n);
    *data = tail;
    let s: String = head
        .iter()
        .map(|&b| {
            let idx = (b as usize) % 27;
            if idx == 0 {
                '_'
            } else {
                char::from(b'a' + (idx as u8 - 1))
            }
        })
        .collect();
    Some(s)
}

fn read_byte(data: &mut &[u8]) -> Option<u8> {
    let (&first, tail) = data.split_first()?;
    *data = tail;
    Some(first)
}

fn apply_op(doc: &Document, data: &mut &[u8]) -> Option<()> {
    let mapping = doc.as_mapping()?;
    let op = read_byte(data)? % 7;
    match op {
        0 => {
            let k = read_str(data, 3)?;
            let v = read_str(data, 3)?;
            mapping.set(k.as_str(), v.as_str());
        }
        1 => {
            let k = read_str(data, 3)?;
            let v = read_byte(data)? as i64;
            mapping.set(k.as_str(), v);
        }
        2 => {
            let k = read_str(data, 3)?;
            let _ = mapping.remove(k.as_str());
        }
        3 => {
            let a = read_str(data, 3)?;
            let b = read_str(data, 3)?;
            let _ = mapping.rename_key(a.as_str(), b.as_str());
        }
        4 => {
            let k = read_str(data, 3)?;
            let v = read_str(data, 3)?;
            if let Some(seq) = mapping.get_sequence(k.as_str()) {
                seq.push(v.as_str());
            }
        }
        5 => {
            let k = read_str(data, 3)?;
            if let Some(seq) = mapping.get_sequence(k.as_str()) {
                let _ = seq.pop();
            }
        }
        _ => {
            let k = read_str(data, 3)?;
            let ik = read_str(data, 3)?;
            let v = read_str(data, 3)?;
            if let Some(nested) = mapping.get_mapping(k.as_str()) {
                nested.set(ik.as_str(), v.as_str());
            }
        }
    }
    Some(())
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

fuzz_target!(|data: &[u8]| {
    let Some((&seed_byte, mut rest)) = data.split_first() else {
        return;
    };
    let seed = SEEDS[(seed_byte as usize) % SEEDS.len()];
    let Ok(doc) = Document::from_str(seed) else {
        return;
    };
    assert_ok(&doc);
    // Bound the mutation count so a single fuzz iteration is quick.
    for _ in 0..16 {
        if apply_op(&doc, &mut rest).is_none() {
            break;
        }
        assert_ok(&doc);
    }
});
