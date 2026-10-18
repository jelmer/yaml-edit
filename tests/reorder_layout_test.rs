//! Moving entries around a mapping leaves the layout that belongs to a
//! position where it was: the indentation of a line, the separators of a
//! flow mapping and the line break ending the last entry, or its absence.

mod common;

use rowan::ast::AstNode;
use std::str::FromStr;
use yaml_edit::{debug, Document, Mapping};

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
fn check_reorder(
    source: &str,
    order: &[&str],
    expected: &str,
    mapping: impl Fn(&Document) -> Mapping,
) {
    let doc = Document::from_str(source).unwrap();
    mapping(&doc).reorder_fields(order.iter().copied());
    assert_edited(&doc, expected);
}

fn root(doc: &Document) -> Mapping {
    doc.as_mapping().unwrap()
}

fn top(doc: &Document) -> Mapping {
    doc.as_mapping().unwrap().get_mapping("top").unwrap()
}

#[test]
fn reorder_flow_mapping() {
    for (source, expected) in [
        ("{a: 1, b: 2}", "{b: 2, a: 1}"),
        ("{a: 1, b: 2}\n", "{b: 2, a: 1}\n"),
        ("{ a: 1, b: 2 }\n", "{ b: 2, a: 1 }\n"),
        ("{a: 1, b: 2, }\n", "{b: 2, a: 1, }\n"),
        ("{a: 1, b: 2,}\n", "{b: 2, a: 1,}\n"),
        ("{\n  a: 1,\n  b: 2\n}\n", "{\n  b: 2,\n  a: 1\n}\n"),
    ] {
        check_reorder(source, &["b"], expected, root);
    }
    check_reorder("{a: 1, b: 2, c: 3}", &["c"], "{c: 3, a: 1, b: 2}", root);
    check_reorder("top: {a: 1, b: 2}\n", &["b"], "top: {b: 2, a: 1}\n", top);
}

#[test]
fn reorder_flow_mapping_with_comments() {
    check_reorder(
        "{a: 1, # c\n b: 2 # d\n}\n",
        &["b"],
        "{b: 2, # d\na: 1 # c\n }\n",
        root,
    );
}

#[test]
fn reorder_unterminated_block_mapping() {
    for (source, order, expected) in [
        ("a: 1\nb: 2", &["b", "a"][..], "b: 2\na: 1"),
        ("a: 1\nb: 2\nc: 3", &["c", "a"], "c: 3\na: 1\nb: 2"),
        ("a: 1  # c\nb: 2  # d", &["b", "a"], "b: 2  # d\na: 1  # c"),
        ("a: 1\n# c\nb: 2", &["b", "a"], "b: 2\na: 1\n# c"),
        ("a: 1\nb:\n  x: 1", &["b", "a"], "b:\n  x: 1\na: 1"),
        ("a: 1\nb: 2\n", &["b", "a"], "b: 2\na: 1\n"),
        ("? a\n: 1\nb: 2", &["b", "a"], "b: 2\n? a\n: 1"),
        ("a: 1\n? b\n: 2", &["b", "a"], "? b\n: 2\na: 1"),
        ("a: 1\nb: |\n  x", &["b", "a"], "b: |\n  x\na: 1"),
        // Dropping the line break of a block scalar would change its value.
        ("a: |\n  x\nb: 2", &["b", "a"], "b: 2\na: |\n  x\n"),
    ] {
        check_reorder(source, order, expected, root);
    }
}

#[test]
fn reorder_nested_block_mapping() {
    for (source, expected) in [
        ("top:\n  a: 1\n  b: 2\n", "top:\n  b: 2\n  a: 1\n"),
        ("top:\n  a: 1\n  b: 2", "top:\n  b: 2\n  a: 1"),
        (
            "top:\n  a: 1\n  b: 2\nz: 3\n",
            "top:\n  b: 2\n  a: 1\nz: 3\n",
        ),
        (
            "top:\n  a: 1  # c\n  # between\n  b: 2  # d\n",
            "top:\n  b: 2  # d\n  a: 1  # c\n  # between\n",
        ),
        ("top:\n  a: 1\n\n  b: 2\n", "top:\n  b: 2\n  a: 1\n\n"),
    ] {
        check_reorder(source, &["b", "a"], expected, top);
    }
}

#[test]
fn reorder_unterminated_keeps_aliases() {
    check_reorder("a: &x 1\nb: *x", &["b", "a"], "b: &x 1\na: *x", root);
}

#[track_caller]
fn check_move(source: &str, after: bool, expected: &str, mapping: impl Fn(&Document) -> Mapping) {
    let doc = Document::from_str(source).unwrap();
    let m = mapping(&doc);
    let moved = if after {
        m.move_after("b", "a", m.get("a").unwrap())
    } else {
        m.move_before("a", "b", m.get("b").unwrap())
    };
    assert!(moved);
    assert_edited(&doc, expected);
}

#[test]
fn move_in_flow_mapping() {
    for (source, expected) in [
        ("{a: 1, b: 2}\n", "{b: 2, a: 1}\n"),
        ("{a: 1, b: 2, }\n", "{b: 2, a: 1, }\n"),
        ("{a: 1, b: 2,}", "{b: 2, a: 1,}"),
        ("{ a: 1, b: 2 }\n", "{ b: 2, a: 1 }\n"),
    ] {
        check_move(source, true, expected, root);
        check_move(source, false, expected, root);
    }
}

#[test]
fn move_in_unterminated_block_mapping() {
    for (source, expected) in [
        ("a: 1\nb: 2", "b: 2\na: 1"),
        ("a: 1\nb: 2\n", "b: 2\na: 1\n"),
    ] {
        check_move(source, true, expected, root);
        check_move(source, false, expected, root);
    }
    check_move("top:\n  a: 1\n  b: 2", true, "top:\n  b: 2\n  a: 1", top);
    check_move("top:\n  a: 1\n  b: 2", false, "top:\n  b: 2\n  a: 1", top);
    check_move("a: &x 1\nb: *x", true, "b: &x 1\na: *x", root);
}

#[test]
fn move_with_new_value() {
    for (source, expected) in [
        ("{a: 1, b: 2, }\n", "{b: 2, a: 9, }\n"),
        ("{a: 1, b: 2,}", "{b: 2, a: 9,}"),
        ("a: 1\nb: 2", "b: 2\na: 9"),
    ] {
        let doc = Document::from_str(source).unwrap();
        assert!(root(&doc).move_after("b", "a", 9));
        assert_edited(&doc, expected);
    }
}
