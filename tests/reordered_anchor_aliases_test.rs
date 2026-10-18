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
        debug::shape_to_string(doc.syntax()),
        debug::shape_to_string(reparsed.syntax())
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
    let cases: [(&str, &[&str], &str); 17] = [
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
        // A key becomes an alias as a value does.
        ("&k a: 1\nb: *k\n", &["b"], "b: &k a\n*k : 1\n"),
        ("!t &k a: 1\nb: *k\n", &["b"], "b: !t &k a\n*k : 1\n"),
        ("{&k a: 1, b: *k}\n", &["b"], "{b: &k a, *k : 1}\n"),
        ("? &k a\n: 1\nb: *k\n", &["b"], "b: &k a\n? *k\n: 1\n"),
        // An alias key takes the anchor as an alias value does.
        (
            "a: &a 1\nb: {*a : 2}\n",
            &["b", "a"],
            "b: {&a 1 : 2}\na: *a\n",
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
            "b: !t &x 1\nc: 2\na: *x\n",
        ),
        ("a: &x 1\nb: 2\nc: *x\n", "b", "b: 2\na: &x 1\nc: *x\n"),
        ("a: &x 1\nb: 2\nc: 3\n", "b", "b: 2\na: &x 1\nc: 3\n"),
        ("&k a: 1\nb: *k\nc: 2\n", "c", "b: &k a\nc: 2\n*k : 1\n"),
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

#[test]
fn anchor_name_with_colon() {
    // `:` is allowed in an anchor name (the test suite's W5VH).
    let doc = Document::from_str("a: &a: 1\nb: *a:\n").unwrap();
    assert!(doc.reorder_fields(["b", "a"]));
    assert_edited(&doc, "b: &a: 1\na: *a:\n");
    assert_same_values(&doc, "a: 1\nb: 1\n");

    let doc = Document::from_str("a: &a: 1\nb: *a:\nc: 2\n").unwrap();
    let mapping = doc.as_mapping().unwrap();
    assert!(mapping.move_after("c", "a", mapping.get("a").unwrap()));
    assert_edited(&doc, "b: &a: 1\nc: 2\na: *a:\n");
}

/// The alias key `*a`, read from another document so that only its name
/// ties it to an entry.
fn alias_key() -> yaml_edit::YamlNode {
    let doc = Document::from_str("- *a\n").unwrap();
    doc.as_sequence().unwrap().get(0).unwrap()
}

#[test]
fn moving_alias_key_takes_anchor() {
    for source in ["a: &a 1\n*a : 2\nc: 3\n", "{a: &a 1, *a : 2, c: 3}\n"] {
        let doc = Document::from_str(source).unwrap();
        assert!(doc.reorder_fields([alias_key()]));
        assert_same_values(&doc, source);

        let doc = Document::from_str(source).unwrap();
        let mapping = doc.as_mapping().unwrap();
        let value = mapping.get(alias_key()).unwrap();
        assert!(mapping.move_before("a", alias_key(), value));
        assert_same_values(&doc, source);

        let doc = Document::from_str(source).unwrap();
        let mapping = doc.as_mapping().unwrap();
        assert!(mapping.move_after("c", "a", mapping.get("a").unwrap()));
        assert_same_values(&doc, source);
    }

    let doc = Document::from_str("a: &a 1\n*a : 2\nc: 3\n").unwrap();
    let mapping = doc.as_mapping().unwrap();
    assert!(mapping.move_before("a", alias_key(), mapping.get(alias_key()).unwrap()));
    assert_edited(&doc, "&a 1 : 2\na: *a\nc: 3\n");

    let doc = Document::from_str("{a: &a 1, *a : 2, c: 3}\n").unwrap();
    let mapping = doc.as_mapping().unwrap();
    assert!(mapping.move_after("c", "a", mapping.get("a").unwrap()));
    assert_edited(&doc, "{&a 1 : 2, c: 3, a: *a}\n");
}

#[test]
fn removing_anchor_of_alias_key_copies_it() {
    let cases = [
        ("a: &a 1\n*a : 2\nc: 3\n", "&a 1 : 2\nc: 3\n"),
        ("{a: &a 1, *a : 2, c: 3}\n", "{&a 1 : 2, c: 3}\n"),
        ("&k a: 1\nb: 2\n*k : 3\n", "b: 2\n&k a : 3\n"),
    ];
    for (source, expected) in cases {
        let doc = Document::from_str(source).unwrap();
        assert!(doc.as_mapping().unwrap().remove("a").is_some());
        assert_edited(&doc, expected);
    }
}

#[test]
fn move_with_new_value_keeps_alias_key_resolving() {
    let cases = [
        ("a: &a 1\n*a : 2\nc: 3\n", "&a 1 : 5\na: *a\nc: 3\n"),
        ("{a: &a 1, *a : 2, c: 3}\n", "{&a 1 : 5, a: *a, c: 3}\n"),
    ];
    for (source, expected) in cases {
        let doc = Document::from_str(source).unwrap();
        let mapping = doc.as_mapping().unwrap();
        assert!(mapping.move_before("a", alias_key(), 5));
        assert_edited(&doc, expected);
    }

    // The old value still goes as with remove.
    let doc = Document::from_str("a: &a 1\n*a : 2\nb: *a\n").unwrap();
    let mapping = doc.as_mapping().unwrap();
    assert!(mapping.move_after("b", "a", 5));
    assert_edited(&doc, "&a 1 : 2\nb: *a\na: 5\n");
}
