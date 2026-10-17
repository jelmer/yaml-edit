//! Strings written into flow collections are quoted where a plain scalar
//! would not read back as the same string there, and only there.

use rowan::ast::AstNode;
use std::collections::BTreeMap;
use std::str::FromStr;
use yaml_edit::{debug, Document, Mapping, Scalar, ScalarValue, Sequence, YamlValue};

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

#[test]
fn test_strings_in_double_quoted_flow_mapping() {
    let doc = Document::from_str("{\"a\": \"b\", \"n\": 1}\n").unwrap();
    let m = doc.as_mapping().unwrap();
    m.set("c", "d");
    m.set("a", "x");
    m.set("s", "1");
    m.set("i", 2);
    m.set("t", true);
    assert_eq!(
        doc.to_string(),
        "{\"a\": \"x\", \"n\": 1, \"c\": \"d\", \"s\": \"1\", \"i\": 2, \"t\": true}\n"
    );
    assert_eq!(
        reparsed_scalars(&doc),
        ["a", "x", "n", "1", "c", "d", "s", "1", "i", "2", "t", "true"]
    );

    let doc = Document::from_str("{\"a\": \"b\", \"c\": \"d\"}\n").unwrap();
    let m = doc.as_mapping().unwrap();
    assert!(m.rename_key("a", "z"));
    assert!(m.insert_after("z", "k", "v"));
    assert!(m.insert_before("c", "a, b", "x]"));
    assert_eq!(
        doc.to_string(),
        "{\"z\": \"b\", \"k\": \"v\", \"a, b\": \"x]\", \"c\": \"d\"}\n"
    );
    assert_eq!(
        reparsed_scalars(&doc),
        ["z", "b", "k", "v", "a, b", "x]", "c", "d"]
    );
}

#[test]
fn test_strings_in_double_quoted_flow_sequence() {
    let doc = Document::from_str("x: [\"a\", 1]\n").unwrap();
    let s = seq(&doc);
    s.push("b");
    s.insert(0, "c");
    assert!(s.set(1, "d"));
    s.push(3);
    assert_eq!(doc.to_string(), "x: [\"c\", \"d\", 1, \"b\", 3]\n");
    assert_eq!(reparsed_scalars(&doc), ["x", "c", "d", "1", "b", "3"]);
}

#[test]
fn test_strings_in_mixed_flow_collections() {
    let doc = Document::from_str("{\"a\": b}\n").unwrap();
    doc.as_mapping().unwrap().set("c", "d");
    assert_eq!(doc.to_string(), "{\"a\": b, c: d}\n");
    assert_eq!(reparsed_scalars(&doc), ["a", "b", "c", "d"]);

    let doc = Document::from_str("x: [\"a\", 'b']\n").unwrap();
    seq(&doc).push("c");
    assert_eq!(doc.to_string(), "x: [\"a\", 'b', c]\n");
    assert_eq!(reparsed_scalars(&doc), ["x", "a", "b", "c"]);
}

#[test]
fn test_strings_in_flow_collections_without_strings() {
    let doc = Document::from_str("x: {}\n").unwrap();
    map(&doc).set("k", "v");
    assert_eq!(doc.to_string(), "x: {k: v}\n");
    assert_eq!(reparsed_scalars(&doc), ["x", "k", "v"]);

    let doc = Document::from_str("x: [1, 2]\n").unwrap();
    seq(&doc).push("a");
    assert_eq!(doc.to_string(), "x: [1, 2, a]\n");
    assert_eq!(reparsed_scalars(&doc), ["x", "1", "2", "a"]);

    let doc = Document::from_str("[]\n").unwrap();
    doc.as_sequence().unwrap().push("a");
    assert_eq!(doc.to_string(), "[a]\n");
    assert_eq!(reparsed_scalars(&doc), ["a"]);
}

#[test]
fn test_strings_in_nested_double_quoted_flow_collections() {
    // A collection without strings of its own follows the nearest
    // enclosing flow collection that has some.
    let doc = Document::from_str("{\"a\": [], \"b\": [1], \"c\": {}}\n").unwrap();
    let m = doc.as_mapping().unwrap();
    m.get_sequence("a").unwrap().push("x");
    m.get_sequence("b").unwrap().push("y");
    m.get_mapping("c").unwrap().set("k", "v");
    assert_eq!(
        doc.to_string(),
        "{\"a\": [\"x\"], \"b\": [1, \"y\"], \"c\": {\"k\": \"v\"}}\n"
    );
    assert_eq!(
        reparsed_scalars(&doc),
        ["a", "x", "b", "1", "y", "c", "k", "v"]
    );

    // A collection with strings of its own goes by those.
    let doc = Document::from_str("{\"a\": [b]}\n").unwrap();
    let m = doc.as_mapping().unwrap();
    m.get_sequence("a").unwrap().push("c");
    assert_eq!(doc.to_string(), "{\"a\": [b, c]}\n");
    assert_eq!(reparsed_scalars(&doc), ["a", "b", "c"]);

    // Raw collections written into a double-quoted collection follow it.
    let doc = Document::from_str("{\"a\": \"b\"}\n").unwrap();
    let m = doc.as_mapping().unwrap();
    m.set(
        "m",
        YamlValue::Mapping(BTreeMap::from([("k".to_string(), YamlValue::from("v"))])),
    );
    m.set("s", vec![YamlValue::from("x"), YamlValue::from(1)]);
    assert_eq!(
        doc.to_string(),
        "{\"a\": \"b\", \"m\": {\"k\": \"v\"}, \"s\": [\"x\", 1]}\n"
    );
    assert_eq!(
        reparsed_scalars(&doc),
        ["a", "b", "m", "k", "v", "s", "x", "1"]
    );
}

#[test]
fn test_double_quoted_flow_collection_in_block_collection() {
    // Only enclosing flow collections count.
    let doc = Document::from_str("\"a\": \"b\"\nx: []\n").unwrap();
    seq(&doc).push("c");
    assert_eq!(doc.to_string(), "\"a\": \"b\"\nx: [c]\n");
    assert_eq!(reparsed_scalars(&doc), ["a", "b", "x", "c"]);
}

#[test]
fn test_explicit_style_kept_in_double_quoted_flow_collection() {
    let doc = Document::from_str("x: [\"a\"]\n").unwrap();
    seq(&doc).push(ScalarValue::single_quoted("b"));
    assert_eq!(doc.to_string(), "x: [\"a\", 'b']\n");
    assert_eq!(reparsed_scalars(&doc), ["x", "a", "b"]);

    // A scalar node keeps the style it was written in.
    let source = Document::from_str("a: plain\n").unwrap();
    let value = source.as_mapping().unwrap().get("a").unwrap();
    let doc = Document::from_str("x: [\"a\"]\n").unwrap();
    seq(&doc).push(&value);
    assert_eq!(doc.to_string(), "x: [\"a\", plain]\n");
    assert_eq!(reparsed_scalars(&doc), ["x", "a", "plain"]);
}
