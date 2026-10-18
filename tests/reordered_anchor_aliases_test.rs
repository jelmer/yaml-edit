//! Moving entries around keeps every alias resolving to the node it did:
//! an anchor that would come after an alias to it moves to that alias.

mod common;

use rowan::ast::AstNode;
use std::str::FromStr;
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

#[track_caller]
fn assert_same_values(doc: &Document, source: &str) {
    let source = Document::from_str(source).unwrap();
    assert_eq!(
        YamlValue::from_document(doc).unwrap(),
        YamlValue::from_document(&source).unwrap()
    );
}

#[test]
fn reorder_fields_moves_anchor_to_first_alias() {
    let cases: [(&str, &[&str], &str); 13] = [
        (
            "a: &x 1\nb: *x\nc: 2\n",
            &["b", "a"],
            "b: &x 1\na: *x\nc: 2\n",
        ),
        (
            "a: &x 1\nb: *x\nc: *x\n",
            &["b", "c", "a"],
            "b: &x 1\nc: *x\na: *x\n",
        ),
        (
            "a: &x 1\nb: *x\nc: 3\n",
            &["c", "b", "a"],
            "c: 3\nb: &x 1\na: *x\n",
        ),
        (
            "a: {k: &x 1}\nb: *x\n",
            &["b", "a"],
            "b: &x 1\na: {k: *x}\n",
        ),
        (
            "a: [&x 1, 2]\nb: *x\n",
            &["b", "a"],
            "b: &x 1\na: [*x, 2]\n",
        ),
        (
            "a: &x\n  k: v\nb: *x\n",
            &["b", "a"],
            "b: &x\n  k: v\na: *x\n",
        ),
        ("a: &x !t 1\nb: *x\n", &["b", "a"], "b: &x !t 1\na: *x\n"),
        ("a: !t &x 1\nb: *x\n", &["b", "a"], "b: !t &x 1\na: *x\n"),
        ("a: &x 1\nb: [*x]\n", &["b", "a"], "b: [&x 1]\na: *x\n"),
        (
            "{a: &x 1, b: *x, c: 2}\n",
            &["b", "a"],
            "{b: &x 1, a: *x, c: 2}\n",
        ),
        // Without anchors of its own inside, the old node could not become
        // an alias without losing `&y`.
        (
            "a: &x {k: &y 1}\nb: *x\nc: *y\n",
            &["b", "a", "c"],
            "b: &x {k: 1}\na: &x {k: &y 1}\nc: *y\n",
        ),
        (
            "base: &x {a: 1}\nderived:\n  <<: *x\n  b: 2\n",
            &["derived"],
            "derived:\n  <<: &x {a: 1}\n  b: 2\nbase: *x\n",
        ),
        // An alias cannot go in a key, so the old key keeps its anchor.
        ("&k a: 1\nb: *k\n", &["b"], "b: &k a\n&k a: 1\n"),
    ];
    for (source, order, expected) in cases {
        let doc = Document::from_str(source).unwrap();
        assert!(doc.reorder_fields(order.iter().copied()));
        assert_edited(&doc, expected);
        assert_same_values(&doc, source);
    }
}

#[test]
fn reorder_fields_keeps_repeated_anchor_names_apart() {
    let cases: [(&str, &[&str], &str); 2] = [
        // `c` referred to `b`, which `a` now comes between.
        (
            "a: &x 1\nb: &x 2\nc: *x\n",
            &["b", "a"],
            "b: &x 2\na: &x 1\nc: 2\n",
        ),
        // `b` gets `a`'s anchor, but `a` cannot become an alias as `c`
        // redefines the name in between.
        (
            "a: &x 1\nb: *x\nc: &x 2\nd: *x\n",
            &["b", "c", "a", "d"],
            "b: &x 1\nc: &x 2\na: &x 1\nd: 2\n",
        ),
    ];
    for (source, order, expected) in cases {
        let doc = Document::from_str(source).unwrap();
        assert!(doc.reorder_fields(order.iter().copied()));
        assert_edited(&doc, expected);
        assert_same_values(&doc, source);
    }
}

#[test]
fn move_keeps_aliases_resolving() {
    let doc = Document::from_str("a: &x 1\nb: *x\nc: 2\n").unwrap();
    let mapping = doc.as_mapping().unwrap();
    assert!(mapping.move_after("c", "a", mapping.get("a").unwrap()));
    assert_edited(&doc, "b: &x 1\nc: 2\na: *x\n");
    assert_same_values(&doc, "a: 1\nb: 1\nc: 2\n");

    let doc = Document::from_str("a: &x 1\nb: *x\n").unwrap();
    let mapping = doc.as_mapping().unwrap();
    assert!(mapping.move_before("a", "b", mapping.get("b").unwrap()));
    assert_edited(&doc, "b: &x 1\na: *x\n");
    assert_same_values(&doc, "a: 1\nb: 1\n");

    let doc = Document::from_str("{a: &x [1], b: *x, c: 2}\n").unwrap();
    let mapping = doc.as_mapping().unwrap();
    assert!(mapping.move_after("c", "a", mapping.get("a").unwrap()));
    assert_edited(&doc, "{b: &x [1], c: 2, a: *x}\n");
    assert_same_values(&doc, "{a: [1], b: [1], c: 2}\n");

    let doc = Document::from_str("a: {k: &x 1}\nb: *x\n").unwrap();
    assert!(doc.move_before("a", "b", doc.as_mapping().unwrap().get("b").unwrap()));
    assert_edited(&doc, "b: &x 1\na: {k: *x}\n");
    assert_same_values(&doc, "a: {k: 1}\nb: 1\n");
}

#[test]
fn move_keeps_block_and_tagged_values() {
    let cases = [
        (
            "a: &x\n  k: v\nb: *x\nc: 2\n",
            "c",
            "b: &x\n  k: v\nc: 2\na: *x\n",
        ),
        (
            "a: !t &x 1\nb: *x\nc: 2\n",
            "c",
            "b: &x !t 1\nc: 2\na: *x\n",
        ),
        ("a: &x 1\nb: 2\nc: *x\n", "b", "b: 2\na: &x 1\nc: *x\n"),
        ("a: &x 1\nb: 2\nc: 3\n", "b", "b: 2\na: &x 1\nc: 3\n"),
        ("&k a: 1\nb: *k\nc: 2\n", "c", "b: &k a\nc: 2\n&k a: 1\n"),
    ];
    for (source, after, expected) in cases {
        let doc = Document::from_str(source).unwrap();
        let mapping = doc.as_mapping().unwrap();
        assert!(mapping.move_after(after, "a", mapping.get("a").unwrap()));
        assert_edited(&doc, expected);
        assert_same_values(&doc, source);
    }
}

#[test]
fn reorder_fields_copies_alias_with_properties() {
    // The copy takes over the tag or anchor of the alias, so the node it
    // was copied from keeps its anchor.
    let cases: [(&str, &[&str], &str); 2] = [
        ("a: &x 1\nb: !t *x\n", &["b", "a"], "b: !t 1\na: &x 1\n"),
        (
            "a: &x 1\nb: &b *x\nc: *b\n",
            &["b", "a", "c"],
            "b: &b 1\na: &x 1\nc: *b\n",
        ),
    ];
    for (source, order, expected) in cases {
        let doc = Document::from_str(source).unwrap();
        assert!(doc.reorder_fields(order.iter().copied()));
        assert_edited(&doc, expected);
    }
}

#[test]
fn reorder_fields_copies_tagged_alias_to_tagged_node() {
    let doc = Document::from_str("a: &x !u 1\nb: !t *x\n").unwrap();
    assert!(doc.reorder_fields(["b", "a"]));
    assert_edited(&doc, "b: !t 1\na: &x !u 1\n");
}
