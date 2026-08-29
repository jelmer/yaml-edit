// Post-condition helpers used by both the proptest and libfuzzer
// mutation targets. Every helper verifies the in-memory state after
// the mutation, then re-parses `doc.to_string()` and re-checks: this
// catches mutations that leave the CST looking fine but produce text
// that re-parses into a different shape.
//
// Shared via `mod common;` from tests and `include!` from the fuzz
// crate, so this file uses only outer comments (no `//!`) and no
// module attributes. Callers add `#![allow(dead_code)]` themselves.

use std::str::FromStr;
use yaml_edit::path::YamlPath;
use yaml_edit::{Document, Mapping, Sequence, SyntaxKind};

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

fn reparse_mapping(doc: &Document, op: &str) -> Mapping {
    let text = doc.to_string();
    let reparsed = Document::from_str(&text)
        .unwrap_or_else(|e| panic!("{op}: re-parse failed ({e}), text: {text:?}"));
    reparsed
        .as_mapping()
        .unwrap_or_else(|| panic!("{op}: re-parsed doc is not a mapping, text: {text:?}"))
}

fn scalar_at(seq: &Sequence, index: usize) -> Option<String> {
    seq.get(index)
        .as_ref()
        .and_then(|n| n.as_scalar().map(|s| s.as_string()))
}

fn scalar_value(m: &Mapping, key: &str) -> Option<String> {
    m.get(key)
        .as_ref()
        .and_then(|n| n.as_scalar().map(|s| s.as_string()))
}

/// True if `mapping`'s enclosing container is a MAPPING_ENTRY.
/// Top-level mappings live directly under DOCUMENT.
fn is_nested_mapping(mapping: &Mapping) -> bool {
    use rowan::ast::AstNode;
    mapping
        .syntax()
        .parent()
        .filter(|p| p.kind() == SyntaxKind::VALUE)
        .and_then(|v| v.parent())
        .is_some_and(|e| e.kind() == SyntaxKind::MAPPING_ENTRY)
}

// -- Sequence-op post-conditions ------------------------------------

pub fn assert_seq_push_stuck(seq: &Sequence, before: usize, v: &str, doc: &Document, key: &str) {
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

pub fn assert_seq_pop_stuck(
    seq: &Sequence,
    before: usize,
    popped: bool,
    doc: &Document,
    key: &str,
) {
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
        panic!("{op}: reported pop of an empty sequence");
    }
    if !popped && before > 0 {
        panic!("{op}: refused to pop a non-empty sequence");
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

pub fn assert_seq_insert_stuck(
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
    // Out-of-range index appends per the docs, so the effective slot
    // is `min(i, before)`.
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

pub fn assert_seq_set_stuck(
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
    if !ok {
        return;
    }
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

pub fn assert_seq_remove_stuck(
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

pub fn assert_seq_clear_stuck(seq: &Sequence, doc: &Document, key: &str) {
    let op = "SeqClear";
    if !seq.is_empty() {
        panic!(
            "{op}: seq not empty (len={}), text: {:?}",
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

// -- Mapping-op post-conditions -------------------------------------

pub fn assert_mapping_set_stuck(mapping: &Mapping, k: &str, v: &str, doc: &Document) {
    let op = format!("MappingSet({k:?}, {v:?})");
    if !mapping.contains_key(k) {
        panic!("{op}: key missing, text: {:?}", doc.to_string());
    }
    let got = scalar_value(mapping, k);
    if got.as_deref() != Some(v) {
        panic!(
            "{op}: value at {k:?} = {got:?}, text: {:?}",
            doc.to_string()
        );
    }
    let reparsed = reparse_mapping(doc, &op);
    let r_got = scalar_value(&reparsed, k);
    if !reparsed.contains_key(k) || r_got.as_deref() != Some(v) {
        panic!(
            "{op}: reparse drift: value at {k:?} = {r_got:?}, text: {:?}",
            doc.to_string()
        );
    }
}

pub fn assert_mapping_remove_stuck(mapping: &Mapping, k: &str, existed: bool, doc: &Document) {
    let op = format!("MappingRemove({k:?})");
    if existed && mapping.contains_key(k) {
        panic!("{op}: key still present, text: {:?}", doc.to_string());
    }
    if !existed && mapping.contains_key(k) {
        // No-op call over a pre-existing state; nothing to check.
        return;
    }
    // Emptying a top-level mapping renders as `""` which re-parses as
    // no mapping. Accepted intermediate state -- callers who care
    // about that shape should follow up with Mapping::clear.
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

/// `rename_key` renames only the first occurrence of `old`, so with
/// duplicate keys `old` may legitimately still be present after a
/// successful rename; only the presence of `new` is a hard check.
pub fn assert_mapping_rename_stuck(mapping: &Mapping, new: &str, renamed: bool, doc: &Document) {
    if !renamed {
        return;
    }
    let op = format!("MappingRename(-> {new:?})");
    if !mapping.contains_key(new) {
        panic!("{op}: new key missing, text: {:?}", doc.to_string());
    }
    let reparsed = reparse_mapping(doc, &op);
    if !reparsed.contains_key(new) {
        panic!(
            "{op}: reparse drift: new key {new:?} missing, text: {:?}",
            doc.to_string()
        );
    }
}

pub fn assert_mapping_clear_stuck(mapping: &Mapping, doc: &Document) {
    let op = "MappingClear";
    if !mapping.is_empty() {
        panic!("{op}: not empty in memory, text: {:?}", doc.to_string());
    }
    let reparsed = reparse_mapping(doc, op);
    if !reparsed.is_empty() {
        panic!("{op}: not empty after reparse, text: {:?}", doc.to_string());
    }
}

pub fn assert_mapping_insert_stuck(
    mapping: &Mapping,
    k: &str,
    v: &str,
    doc: &Document,
    op_name: &str,
) {
    let op = format!("{op_name}({k:?}, {v:?})");
    if !mapping.contains_key(k) {
        panic!("{op}: key missing, text: {:?}", doc.to_string());
    }
    let got = scalar_value(mapping, k);
    if got.as_deref() != Some(v) {
        panic!(
            "{op}: value at {k:?} = {got:?}, text: {:?}",
            doc.to_string()
        );
    }
    let reparsed = reparse_mapping(doc, &op);
    let r_got = scalar_value(&reparsed, k);
    if !reparsed.contains_key(k) || r_got.as_deref() != Some(v) {
        panic!(
            "{op}: reparse drift: value at {k:?} = {r_got:?}, text: {:?}",
            doc.to_string()
        );
    }
}

// -- Path-op post-conditions ----------------------------------------

pub fn assert_set_path_stuck(doc: &Document, path: &str, v: &str) {
    // set_path is a no-op when the doc has no root mapping.
    if doc.as_mapping().is_none() {
        return;
    }
    let op = format!("SetPath({path:?}, {v:?})");
    let got = doc
        .get_path(path)
        .as_ref()
        .and_then(|n| n.as_scalar().map(|s| s.as_string()));
    if got.as_deref() != Some(v) {
        panic!(
            "{op}: get_path returned {got:?}, text: {:?}",
            doc.to_string()
        );
    }
    let text = doc.to_string();
    let reparsed = Document::from_str(&text)
        .unwrap_or_else(|e| panic!("{op}: re-parse failed ({e}), text: {text:?}"));
    let r_got = reparsed
        .get_path(path)
        .as_ref()
        .and_then(|n| n.as_scalar().map(|s| s.as_string()));
    if r_got.as_deref() != Some(v) {
        panic!("{op}: reparse drift: get_path returned {r_got:?}, text: {text:?}");
    }
}

pub fn assert_remove_path_stuck(doc: &Document, path: &str, removed: bool) {
    if doc.as_mapping().is_none() || !removed {
        return;
    }
    let op = format!("RemovePath({path:?})");
    if doc.get_path(path).is_some() {
        panic!("{op}: path still present, text: {:?}", doc.to_string());
    }
    let text = doc.to_string();
    let reparsed = Document::from_str(&text)
        .unwrap_or_else(|e| panic!("{op}: re-parse failed ({e}), text: {text:?}"));
    if reparsed.get_path(path).is_some() {
        panic!("{op}: reparse drift: path came back, text: {text:?}");
    }
}
