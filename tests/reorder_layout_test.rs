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
        debug::shape_to_string(doc.syntax()),
        debug::shape_to_string(reparsed.syntax())
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
        "{b: 2, # d\n a: 1 # c\n}\n",
        root,
    );
    for (source, expected) in [
        (
            "{\n  a: 1, # c\n  b: 2 # d\n}\n",
            "{\n  b: 2, # d\n  a: 1 # c\n}\n",
        ),
        (
            "{\n  a: 1, # c\n  b: 2, # d\n}\n",
            "{\n  b: 2, # d\n  a: 1, # c\n}\n",
        ),
        ("{a: 1,\n # c\n b: 2}\n", "{b: 2, a: 1\n # c\n }\n"),
    ] {
        check_reorder(source, &["b"], expected, root);
    }
    check_reorder(
        "top: {a: 1, # c\n   b: 2 # d\n  }\n",
        &["b"],
        "top: {b: 2, # d\n   a: 1 # c\n  }\n",
        top,
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
        ("top:\n  a: 1\n\n  b: 2\n", "top:\n  b: 2\n\n  a: 1\n"),
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
fn move_keeps_comments() {
    for (source, expected) in [
        ("a: 1  # c\nb: 2  # d", "b: 2  # d\na: 1  # c"),
        ("a: 1  # c\nb: 2  # d\n", "b: 2  # d\na: 1  # c\n"),
        ("a:  # c\n  x: 1\nb: 2\n", "b: 2\na:  # c\n  x: 1\n"),
        ("? a  # c\n: 1  # e\nb: 2\n", "b: 2\n? a  # c\n: 1  # e\n"),
        // A comment line goes with the entry above it, as in reorder_fields.
        ("a: 1\n# c\nb: 2\n", "b: 2\na: 1\n# c\n"),
        ("{a: 1, # c\n b: 2 # d\n}", "{b: 2, # d\n a: 1 # c\n}"),
        ("{a: 1 # c\n, b: 2 # d\n}\n", "{b: 2, # d\na: 1 # c\n}\n"),
        ("{a: 1, b: 2 # d\n}\n", "{b: 2, # d\na: 1}\n"),
    ] {
        check_move(source, true, expected, root);
        check_move(source, false, expected, root);
    }
    for after in [true, false] {
        check_move(
            "top:\n  # e\n  a: 1  # c\n  # f\n  b: 2  # d\n",
            after,
            "top:\n  # e\n  b: 2  # d\n  a: 1  # c\n  # f\n",
            top,
        );
    }
}

#[test]
fn reorder_keeps_blank_lines_in_place() {
    for (source, order, expected) in [
        (
            "a: 1  # c\n\nb: 2  # d\n\n",
            &["b", "a"][..],
            "b: 2  # d\n\na: 1  # c\n\n",
        ),
        ("a: 1\n\nb: 2", &["b", "a"], "b: 2\n\na: 1"),
        ("a: 1\n  \nb: 2\n", &["b", "a"], "b: 2\n  \na: 1\n"),
        (
            "a: 1\n\nb: 2\nc: 3\n",
            &["c", "b", "a"],
            "c: 3\n\nb: 2\na: 1\n",
        ),
        // Blank lines among the comment lines after an entry go with them.
        (
            "a: 1\n\n# c\n\nb: 2\n",
            &["b", "a"],
            "b: 2\n\na: 1\n\n# c\n",
        ),
        (
            "a: 1\n# c\n\nb: 2\nc: 3\n",
            &["b", "a", "c"],
            "b: 2\n\na: 1\n# c\nc: 3\n",
        ),
        // A block scalar would take the blank lines after it into its
        // value, so they go after the next entry that can have them.
        (
            "a: 1\n\nb: |\n  x\nc: 3\n",
            &["b", "a", "c"],
            "b: |\n  x\na: 1\n\nc: 3\n",
        ),
        ("a: 1\n\nb: |+\n  x\n", &["b", "a"], "b: |+\n  x\na: 1\n\n"),
        ("b: |+\n  x\na: 1\n\n", &["a", "b"], "a: 1\n\nb: |+\n  x\n"),
        (
            "a: 1\n\nb: |\n  x\n# c\nc: 3\n",
            &["b", "a", "c"],
            "b: |\n  x\n# c\n\na: 1\nc: 3\n",
        ),
    ] {
        check_reorder(source, order, expected, root);
    }
    check_reorder(
        "top:\n  a: 1\n\n  b: 2\n  c: 3\nz: 4\n",
        &["c", "b", "a"],
        "top:\n  c: 3\n\n  b: 2\n  a: 1\nz: 4\n",
        top,
    );
    check_move("a: 1\n\nb: 2\n", true, "b: 2\n\na: 1\n", root);
    check_move("a: 1\n\nb: 2\n", false, "b: 2\n\na: 1\n", root);
}

#[test]
fn reorder_keeps_comments() {
    check_reorder(
        "a: 1  # c\n# e\nb: 2  # d\nc: 3\n",
        &["b", "c", "a"],
        "b: 2  # d\nc: 3\na: 1  # c\n# e\n",
        root,
    );
    check_reorder(
        "top:\n  # e\n  a: 1\n  b: 2\n",
        &["b", "a"],
        "top:\n  # e\n  b: 2\n  a: 1\n",
        top,
    );
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

#[test]
fn move_with_new_value_keeps_position_layout() {
    // The entry moves as it would with its own value, comment lines and
    // blank lines included.
    for (source, expected) in [
        (
            "a: 1\n# before b\nb: 2\nc: 3\n",
            "b: 2\nc: 3\na: 5\n# before b\n",
        ),
        ("a: 1\n\nb: 2\n\nc: 3\n", "b: 2\n\nc: 3\n\na: 5\n"),
        ("a: 1  # c\nb: 2\nc: 3", "b: 2\nc: 3\na: 5"),
        ("{a: 1, b: 2, c: 3,}\n", "{b: 2, c: 3, a: 5,}\n"),
    ] {
        let doc = Document::from_str(source).unwrap();
        assert!(root(&doc).move_after("c", "a", 5));
        assert_edited(&doc, expected);
    }
}

#[test]
fn reorder_keeps_comment_on_explicit_key_value_line() {
    for (source, expected) in [
        ("b: 2\n? a\n: 1  # c\n", "? a\n: 1  # c\nb: 2\n"),
        ("b: 2\n? a\n: 1  # c", "? a\n: 1  # c\nb: 2"),
    ] {
        check_reorder(source, &["a", "b"], expected, root);
    }
}
