//! Replacing an anchored value keeps the anchor, so aliases to it still
//! resolve.

mod common;

use rowan::ast::AstNode;
use std::str::FromStr;
use yaml_edit::anchor_resolution::DocumentResolvedExt;
use yaml_edit::path::YamlPath;
use yaml_edit::{debug, Document, YamlValue};

#[track_caller]
fn assert_edited(doc: &Document, expected: &str) {
    assert_eq!(doc.to_string(), expected);
    common::assert_cst_ok(doc);
    let reparsed = Document::from_str(expected).unwrap();
    assert_eq!(
        debug::tree_to_string(doc.syntax()),
        debug::tree_to_string(reparsed.syntax())
    );
}

/// The value `key` of `source`, a node with whatever anchor and tag it has.
fn node_from(source: &str, key: &str) -> yaml_edit::YamlNode {
    let doc = Document::from_str(source).unwrap();
    let mapping = doc.as_mapping().unwrap();
    mapping.get(key).unwrap()
}

#[test]
fn mapping_set_keeps_anchor() {
    let cases = [
        ("b: &x 2\nc: *x\n", "b: &x 6\nc: *x\n"),
        ("b: &x 2  # c\nc: *x\n", "b: &x 6  # c\nc: *x\n"),
        ("b: &x !!int 2\nc: *x\n", "b: &x 6\nc: *x\n"),
        ("b: !!int &x 2\nc: *x\n", "b: &x 6\nc: *x\n"),
        ("b: &x\n  d: 1\nc: *x\n", "b: &x 6\nc: *x\n"),
        ("b: &x [1, 2]\nc: *x\n", "b: &x 6\nc: *x\n"),
        ("b: &x |\n  hi\nc: *x\n", "b: &x 6\nc: *x\n"),
        ("{b: &x 2, c: *x}\n", "{b: &x 6, c: *x}\n"),
    ];
    for (source, expected) in cases {
        let doc = Document::from_str(source).unwrap();
        doc.as_mapping().unwrap().set("b", 6);
        assert_edited(&doc, expected);
        assert_eq!(doc.get_resolved("c").unwrap(), Some(YamlValue::from(6)));
    }
}

#[test]
fn mapping_set_block_value_keeps_anchor() {
    let doc = Document::from_str("b: &x 2\nc: *x\n").unwrap();
    let new = Document::from_str("e: 1\nf: 2\n").unwrap();
    doc.as_mapping()
        .unwrap()
        .set("b", new.as_mapping().unwrap());
    assert_edited(&doc, "b: &x\n  e: 1\n  f: 2\nc: *x\n");
    assert_eq!(
        doc.get_resolved("c").unwrap(),
        doc.get_resolved("b").unwrap()
    );

    let doc = Document::from_str("b: !!int &x 2\nc: *x\n").unwrap();
    doc.as_mapping()
        .unwrap()
        .set("b", node_from("k: |\n  text\n", "k"));
    assert_edited(&doc, "b: &x |\n  text\nc: *x\n");
    assert_eq!(
        doc.get_resolved("c").unwrap(),
        Some(YamlValue::from("text\n"))
    );
}

#[test]
fn mapping_set_tagged_value_keeps_anchor() {
    let doc = Document::from_str("b: &x 2\nc: *x\n").unwrap();
    doc.as_mapping()
        .unwrap()
        .set("b", node_from("k: !!str 6\n", "k"));
    assert_edited(&doc, "b: &x !!str 6\nc: *x\n");
    assert_eq!(
        doc.get_resolved("c").unwrap(),
        doc.get_resolved("b").unwrap()
    );
}

#[test]
fn mapping_set_old_anchor_wins_over_new() {
    // The old anchor is what the document's aliases refer to; the new
    // value's anchor stays behind on the node it was copied from.
    let cases = [
        ("k: &y 5\n", "b: &x 5\nc: *x\n"),
        ("k: &y !!int 5\n", "b: &x !!int 5\nc: *x\n"),
        ("k: !!int &y 5\n", "b: &x !!int 5\nc: *x\n"),
        ("k: &y\n  e: 5\n", "b: &x\n  e: 5\nc: *x\n"),
        ("k: !!map &y\n  e: 5\n", "b: &x !!map\n  e: 5\nc: *x\n"),
    ];
    for (source, expected) in cases {
        for old in ["b: &x 2\nc: *x\n", "b: !!int &x 2\nc: *x\n"] {
            let doc = Document::from_str(old).unwrap();
            doc.as_mapping().unwrap().set("b", node_from(source, "k"));
            assert_edited(&doc, expected);
        }
    }
}

#[test]
fn mapping_set_keeps_new_anchor_without_old() {
    let cases = [
        ("k: &y 5\n", "b: &y 5\n"),
        ("k: !!int &y 5\n", "b: !!int &y 5\n"),
        ("k: &y\n  e: 5\n", "b: &y\n  e: 5\n"),
    ];
    for (source, expected) in cases {
        let doc = Document::from_str("b: 2\n").unwrap();
        doc.as_mapping().unwrap().set("b", node_from(source, "k"));
        assert_edited(&doc, expected);
    }
}

#[test]
fn sequence_set_keeps_anchor() {
    let cases = [
        ("- &x 2\n- *x\n", "- &x 6\n- *x\n"),
        ("- !!int &x 2\n- *x\n", "- &x 6\n- *x\n"),
        ("- &x !!int 2\n- *x\n", "- &x 6\n- *x\n"),
        ("[&x 2, *x]\n", "[&x 6, *x]\n"),
        ("[!!int &x 2, *x]\n", "[&x 6, *x]\n"),
        ("- &x\n  a: 1\n- *x\n", "- &x 6\n- *x\n"),
    ];
    for (source, expected) in cases {
        let doc = Document::from_str(source).unwrap();
        doc.as_sequence().unwrap().set(0, 6);
        assert_edited(&doc, expected);
    }
}

#[test]
fn nested_sequence_set_keeps_anchor() {
    let doc = Document::from_str("- - &x 2\n- *x\n").unwrap();
    let outer = doc.as_sequence().unwrap();
    outer.get(0).unwrap().as_sequence().unwrap().set(0, 6);
    assert_edited(&doc, "- - &x 6\n- *x\n");
}

#[test]
fn sequence_set_block_value_keeps_anchor() {
    for source in ["- &x 2\n- *x\n", "- !!int &x 2\n- *x\n"] {
        let doc = Document::from_str(source).unwrap();
        doc.as_sequence()
            .unwrap()
            .set(0, node_from("k:\n  e: 1\n  f: 2\n", "k"));
        assert_edited(&doc, "- &x\n  e: 1\n  f: 2\n- *x\n");
    }

    let doc = Document::from_str("- &x 2\n- *x\n").unwrap();
    doc.as_sequence()
        .unwrap()
        .set(0, node_from("k: |\n  text\n", "k"));
    assert_edited(&doc, "- &x |\n  text\n- *x\n");
    assert_eq!(
        doc.as_sequence()
            .unwrap()
            .get(1)
            .unwrap()
            .as_alias()
            .unwrap()
            .name(),
        "x"
    );
}

#[test]
fn sequence_set_old_anchor_wins_over_new() {
    let cases = [
        ("k: &y 5\n", "- &x 5\n- *x\n"),
        ("k: !!int &y 5\n", "- &x !!int 5\n- *x\n"),
        ("k: &y !!int 5\n", "- &x !!int 5\n- *x\n"),
        ("k: &y\n  e: 5\n", "- &x\n  e: 5\n- *x\n"),
    ];
    for (source, expected) in cases {
        for old in ["- &x 2\n- *x\n", "- !!int &x 2\n- *x\n"] {
            let doc = Document::from_str(old).unwrap();
            doc.as_sequence().unwrap().set(0, node_from(source, "k"));
            assert_edited(&doc, expected);
        }
    }
}

#[test]
fn sequence_set_keeps_new_anchor_without_old() {
    let cases = [
        ("k: &y 5\n", "- &y 5\n"),
        ("k: !!int &y 5\n", "- !!int &y 5\n"),
        ("k: &y !!int 5\n", "- &y !!int 5\n"),
        ("k: &y\n  e: 5\n", "- &y\n  e: 5\n"),
    ];
    for (source, expected) in cases {
        let doc = Document::from_str("- 2\n").unwrap();
        doc.as_sequence().unwrap().set(0, node_from(source, "k"));
        assert_edited(&doc, expected);
    }
}

#[test]
fn set_path_keeps_anchor() {
    let doc = Document::from_str("a:\n  b: &x 2\n  l: [&y 1]\nc: *x\nd: *y\n").unwrap();
    doc.try_set_path("a.b", 6).unwrap();
    doc.try_set_path("a.l[0]", 7).unwrap();
    assert_edited(&doc, "a:\n  b: &x 6\n  l: [&y 7]\nc: *x\nd: *y\n");
    assert_eq!(doc.get_resolved("c").unwrap(), Some(YamlValue::from(6)));
    assert_eq!(doc.get_resolved("d").unwrap(), Some(YamlValue::from(7)));
}
