//! Strings written into flow collections are quoted where a plain scalar
//! would not read back as the same string there, and only there.

use rowan::ast::AstNode;
use std::str::FromStr;
use yaml_edit::{debug, Document, Mapping, Scalar, Sequence};

/// Check that `doc` has the same tree as a re-parse of its text, and
/// return the scalars the re-parse holds, in order.
#[track_caller]
fn reparsed_scalars(doc: &Document) -> Vec<String> {
    let reparsed = Document::from_str(&doc.to_string()).unwrap();
    assert_eq!(
        debug::tree_to_string(doc.syntax()),
        debug::tree_to_string(reparsed.syntax())
    );
    reparsed
        .syntax()
        .descendants()
        .filter_map(Scalar::cast)
        .map(|s| s.as_string())
        .collect()
}

fn seq(doc: &Document) -> Sequence {
    doc.as_mapping().unwrap().get_sequence("x").unwrap()
}

fn map(doc: &Document) -> Mapping {
    doc.as_mapping().unwrap().get_mapping("x").unwrap()
}

#[test]
fn test_push_string_with_comma_onto_flow_sequence() {
    let doc = Document::from_str("x: [1]\n").unwrap();
    seq(&doc).push("a, b");
    assert_eq!(doc.to_string(), "x: [1, \"a, b\"]\n");
    assert_eq!(reparsed_scalars(&doc), ["x", "1", "a, b"]);
}

#[test]
fn test_strings_in_flow_sequence() {
    for (value, rendered) in [
        ("a, b", "\"a, b\""),
        ("x]", "\"x]\""),
        ("x{y", "\"x{y\""),
        ("x\ny", "\"x\\ny\""),
        ("a: b", "'a: b'"),
        ("1", "'1'"),
        ("plain", "plain"),
        ("a:b", "a:b"),
    ] {
        let doc = Document::from_str("x: [1]\n").unwrap();
        seq(&doc).push(value);
        assert_eq!(doc.to_string(), format!("x: [1, {rendered}]\n"));
        assert_eq!(reparsed_scalars(&doc), ["x", "1", value]);

        let doc = Document::from_str("x: [1]\n").unwrap();
        seq(&doc).insert(0, value);
        assert_eq!(doc.to_string(), format!("x: [{rendered}, 1]\n"));
        assert_eq!(reparsed_scalars(&doc), ["x", value, "1"]);

        let doc = Document::from_str("x: [1, 2]\n").unwrap();
        assert!(seq(&doc).set(0, value));
        assert_eq!(doc.to_string(), format!("x: [{rendered}, 2]\n"));
        assert_eq!(reparsed_scalars(&doc), ["x", value, "2"]);
    }
}

#[test]
fn test_strings_in_flow_mapping() {
    for (value, rendered) in [
        ("a, b", "\"a, b\""),
        ("x]", "\"x]\""),
        ("x\ny", "\"x\\ny\""),
        ("1", "'1'"),
        ("plain", "plain"),
    ] {
        let doc = Document::from_str("x: {k: 1}\n").unwrap();
        map(&doc).set("k", value);
        assert_eq!(doc.to_string(), format!("x: {{k: {rendered}}}\n"));
        assert_eq!(reparsed_scalars(&doc), ["x", "k", value]);

        let doc = Document::from_str("x: {k: 1}\n").unwrap();
        map(&doc).set(value, 2);
        assert_eq!(doc.to_string(), format!("x: {{k: 1, {rendered}: 2}}\n"));
        assert_eq!(reparsed_scalars(&doc), ["x", "k", "1", value, "2"]);

        let doc = Document::from_str("x: {k: 1}\n").unwrap();
        assert!(map(&doc).rename_key("k", value));
        assert_eq!(doc.to_string(), format!("x: {{{rendered}: 1}}\n"));
        assert_eq!(reparsed_scalars(&doc), ["x", value, "1"]);

        let doc = Document::from_str("x: {k: 1, m: 2}\n").unwrap();
        assert!(map(&doc).insert_after("k", value, value));
        assert_eq!(
            doc.to_string(),
            format!("x: {{k: 1, {rendered}: {rendered}, m: 2}}\n")
        );
        assert_eq!(
            reparsed_scalars(&doc),
            ["x", "k", "1", value, value, "m", "2"]
        );
    }
}

#[test]
fn test_strings_in_flow_mapping_nested_in_flow_sequence() {
    let doc = Document::from_str("x: [{k: 1}]\n").unwrap();
    let nested = seq(&doc).get(0).unwrap();
    nested.as_mapping().unwrap().set("a, b", "c]");
    assert_eq!(doc.to_string(), "x: [{k: 1, \"a, b\": \"c]\"}]\n");
    assert_eq!(reparsed_scalars(&doc), ["x", "k", "1", "a, b", "c]"]);
}

#[test]
fn test_plain_scalar_node_copied_into_flow_collections() {
    let source = Document::from_str("a: a, b\n").unwrap();
    let value = source.as_mapping().unwrap().get("a").unwrap();

    let doc = Document::from_str("x: [1]\n").unwrap();
    seq(&doc).push(&value);
    assert_eq!(doc.to_string(), "x: [1, \"a, b\"]\n");
    assert_eq!(reparsed_scalars(&doc), ["x", "1", "a, b"]);

    let doc = Document::from_str("x: {k: 1}\n").unwrap();
    map(&doc).set(&value, &value);
    assert_eq!(doc.to_string(), "x: {k: 1, \"a, b\": \"a, b\"}\n");
    assert_eq!(reparsed_scalars(&doc), ["x", "k", "1", "a, b", "a, b"]);
}
