//! Structural-invariant tests that exercise mutation methods and then
//! assert `debug::validate_tree` + `debug::roundtrip_ok` on the result.
//!
//! Failures here indicate bugs in the mutation logic that would otherwise
//! only surface as odd rendering.

use rowan::ast::AstNode;
use std::str::FromStr;
use yaml_edit::{debug, Document, Mapping, YamlFile};

fn check(doc: &Document) {
    let syntax = doc.syntax();
    if let Err(e) = debug::validate_tree(syntax) {
        panic!("invariant violated: {e}\n---\n{}\n---", doc);
    }
    if let Err(e) = debug::roundtrip_ok(syntax) {
        panic!("roundtrip failed: {e}\n---\n{}\n---", doc);
    }
}

#[test]
fn parsed_document_is_valid() {
    let inputs = [
        "a: 1\n",
        "a: 1\nb: 2\n",
        "items:\n  - a\n  - b\n",
        "nested:\n  a: 1\n  b: 2\n",
        "mixed:\n  list:\n    - 1\n    - 2\n  map:\n    x: y\n",
        "flow: {a: 1, b: 2}\n",
        "flow_seq: [1, 2, 3]\n",
    ];
    for input in inputs {
        let file = YamlFile::from_str(input).unwrap();
        let doc = file.document().unwrap();
        check(&doc);
    }
}

#[test]
fn mapping_set_new_key() {
    let doc = Document::from_str("a: 1\n").unwrap();
    doc.as_mapping().unwrap().set("b", 2);
    check(&doc);
}

#[test]
fn mapping_set_replace_key() {
    let doc = Document::from_str("a: 1\nb: 2\n").unwrap();
    doc.as_mapping().unwrap().set("a", 99);
    check(&doc);
}

#[test]
fn mapping_remove_key() {
    let doc = Document::from_str("a: 1\nb: 2\nc: 3\n").unwrap();
    doc.as_mapping().unwrap().remove("b");
    check(&doc);
}

#[test]
fn nested_mapping_set() {
    let doc = Document::from_str("root:\n  a: 1\n").unwrap();
    let nested = doc.as_mapping().unwrap().get_mapping("root").unwrap();
    nested.set("b", 2);
    check(&doc);
}

#[test]
fn sequence_push_existing() {
    let doc = Document::from_str("items:\n  - a\n  - b\n").unwrap();
    let seq = doc.as_mapping().unwrap().get_sequence("items").unwrap();
    seq.push("c");
    check(&doc);
}

#[test]
fn set_empty_mapping_then_populate() {
    let doc = Document::from_str("existing: v\n").unwrap();
    let mapping = doc.as_mapping().unwrap();
    mapping.set("nested", Mapping::new());
    let nested = mapping.get_mapping("nested").unwrap();
    nested.set("a", 1);
    nested.set("b", 2);
    check(&doc);
}

#[test]
fn parse_empty_sequence_under_key_then_push() {
    // Analogous to the "set empty Sequence + push" bug: an empty
    // sequence under a key parsed from source, then pushed to.
    let doc = Document::from_str("items: []\nother: v\n").unwrap();
    let seq = doc.as_mapping().unwrap().get_sequence("items").unwrap();
    seq.push("a");
    check(&doc);
}

#[test]
fn sequence_insert_into_existing() {
    let doc = Document::from_str("items:\n  - a\n  - c\n").unwrap();
    let seq = doc.as_mapping().unwrap().get_sequence("items").unwrap();
    seq.insert(1, "b");
    check(&doc);
}

#[test]
fn sequence_remove_middle() {
    let doc = Document::from_str("items:\n  - a\n  - b\n  - c\n").unwrap();
    let seq = doc.as_mapping().unwrap().get_sequence("items").unwrap();
    seq.remove(1);
    check(&doc);
}

#[test]
fn rename_key() {
    let doc = Document::from_str("old: 1\nkeep: 2\n").unwrap();
    doc.as_mapping().unwrap().rename_key("old", "new");
    check(&doc);
}

#[test]
fn mapping_clear() {
    let doc = Document::from_str("a: 1\nb: 2\n").unwrap();
    doc.as_mapping().unwrap().clear();
    check(&doc);
}

#[test]
#[ignore = "known bug: set_path followed by remove_path on a deep nested key leaves the intermediate mappings as empty scaffolds with dangling INDENT tokens, producing text with trailing whitespace and an unterminated block value. The correct fix requires either collapsing now-empty mappings up the chain or rendering empty inner mappings as `{}`."]
fn set_path_then_remove_path_leaves_empty_scaffold() {
    use yaml_edit::path::YamlPath;
    let doc = Document::from_str("a: 1\nb: 2\nc: 3\n").unwrap();
    doc.set_path("vvv.vvv.x", "");
    doc.remove_path("vvv.vvv.x");
    check(&doc);
}

#[test]
fn set_path_after_explicit_key_does_not_leave_blank_line() {
    use yaml_edit::path::YamlPath;
    let doc = Document::from_str("keys: !!set\n  ? a\n  ? b\n").unwrap();
    doc.set_path("a", "");
    check(&doc);
    assert!(
        !doc.to_string().contains("\n\n"),
        "unexpected blank line: {:?}",
        doc.to_string()
    );
}
