//! Where an entry added at the end of a collection goes.
//!
//! When the collection ends in blank lines, the entry goes before them if
//! the collection is nested and after them at the root.
//!
//! Adding an entry at the end of input that has no final line break keeps
//! it without one, whichever method adds it.
//!
//! Appending to an indented mapping whose last line is a comment: the
//! parser leaves that comment line's INDENT at the end of the nested
//! mapping and lifts the comment itself to the parent.

mod common;
use common::{assert_file_cst_ok, assert_file_parser_shaped, check_edit};
use std::str::FromStr;
use yaml_edit::{Document, Mapping, Sequence, YamlFile};

fn edit(source: &str, f: impl FnOnce(&YamlFile)) -> String {
    let file = YamlFile::from_str(source).unwrap();
    f(&file);
    assert_file_parser_shaped(&file);
    file.to_string()
}

fn root_mapping(file: &YamlFile) -> Mapping {
    file.document().unwrap().as_mapping().unwrap()
}

fn root_sequence(file: &YamlFile) -> Sequence {
    file.document().unwrap().as_sequence().unwrap()
}

#[test]
fn nested_collection_keeps_blank_lines_at_its_end() {
    assert_eq!(
        edit("a:\n  x: 1\n\nb: 2\n", |f| root_mapping(f)
            .get_mapping("a")
            .unwrap()
            .set("y", 3)),
        "a:\n  x: 1\n  y: 3\n\nb: 2\n"
    );
    assert_eq!(
        edit("a:\n  - x\n\nb: 2\n", |f| root_mapping(f)
            .get_sequence("a")
            .unwrap()
            .push("y")),
        "a:\n  - x\n  - y\n\nb: 2\n"
    );
    assert_eq!(
        edit("a:\n  x: 1\n  # c\n\n", |f| root_mapping(f)
            .get_mapping("a")
            .unwrap()
            .set("y", 3)),
        "a:\n  x: 1\n  y: 3\n  # c\n\n"
    );
}

#[test]
fn root_collection_appends_after_blank_lines() {
    assert_eq!(
        edit("x: 1\n\n", |f| root_mapping(f).set("y", 2)),
        "x: 1\n\ny: 2\n"
    );
    assert_eq!(
        edit("- x\n\n", |f| root_sequence(f).push("y")),
        "- x\n\n- y\n"
    );
    assert_eq!(
        edit("- x\n\n", |f| root_sequence(f).insert(1, "y")),
        "- x\n\n- y\n"
    );
    assert_eq!(
        edit("- x\n\n# c\n\n", |f| root_sequence(f).push("y")),
        "- x\n\n# c\n\n- y\n"
    );
}

fn root(doc: &Document) -> Mapping {
    doc.as_mapping().unwrap()
}

fn seq(doc: &Document) -> Sequence {
    doc.as_sequence().unwrap()
}

#[test]
fn mapping_methods_keep_unterminated_end() {
    for source in ["a: 1", "? a\n: 1"] {
        let prefix = source.to_string() + "\n";
        let expected = prefix.clone() + "b: 2";
        let explicit = source.starts_with('?');
        let expected = if explicit {
            prefix.clone() + "? b\n: 2"
        } else {
            expected
        };
        check_edit(source, |d| root(d).set("b", 2), &expected);
        check_edit(
            source,
            |d| {
                root(d).insert_after("a", "b", 2);
            },
            &expected,
        );
        check_edit(source, |d| root(d).insert_at_index(1, "b", 2), &expected);
        check_edit(
            source,
            |d| root(d).set_with_field_order("b", 2, ["a", "b"]),
            &expected,
        );
    }
    check_edit("a: 1\n# c", |d| root(d).set("b", 2), "a: 1\n# c\nb: 2");
    check_edit("a: |\n  x", |d| root(d).set("b", 2), "a: |\n  x\nb: 2");
    check_edit(
        "a:\n  x: 1",
        |d| root(d).get_mapping("a").unwrap().set("y", 2),
        "a:\n  x: 1\n  y: 2",
    );
    check_edit("a:\n  x: 1", |d| root(d).set("b", 2), "a:\n  x: 1\nb: 2");
}

#[test]
fn sequence_methods_keep_unterminated_end() {
    check_edit("- a", |d| seq(d).push("b"), "- a\n- b");
    check_edit("- a", |d| seq(d).insert(1, "b"), "- a\n- b");
    check_edit("- a", |d| seq(d).insert(5, "b"), "- a\n- b");
    check_edit(
        "k:\n  - a",
        |d| root(d).get_sequence("k").unwrap().insert(1, "b"),
        "k:\n  - a\n  - b",
    );
}

#[test]
fn set_value_keeps_unterminated_end() {
    let value = |text: &str| Document::from_str(text).unwrap();
    for (source, new, expected) in [
        ("a:\n  - x", "z", "a: z"),
        ("a:\n  b: 1", "[1]", "a: [1]"),
        ("a: 1", "- p\n- q", "a:\n  - p\n  - q"),
        ("a:", "p: 1", "a:\n  p: 1"),
        ("a: 1", "!t\n  p: 1", "a: !t\n  p: 1"),
        ("a: # c", "- p", "a: # c\n  - p"),
        ("x: 1\na:\n  - x", "- p", "x: 1\na:\n  - p"),
        ("? a\n:\n  - x", "z", "? a\n: z"),
        ("? a\n: 1", "- p", "? a\n:\n  - p"),
        // The line break of a block scalar is part of its value.
        ("a: 1", "|\n  x\n", "a: |\n  x\n"),
    ] {
        check_edit(source, |d| root(d).set("a", value(new)), expected);
    }
    check_edit(
        "o:\n  a:\n    - x",
        |d| root(d).get_mapping("o").unwrap().set("a", value("z")),
        "o:\n  a: z",
    );
    check_edit(
        "o:\n  a: 1",
        |d| root(d).get_mapping("o").unwrap().set("a", value("- p")),
        "o:\n  a:\n    - p",
    );
    check_edit(
        "- a\n- b",
        |d| assert!(seq(d).set(1, value("- c"))),
        "- a\n- - c",
    );
}

const SRC: &str = "x:\n  a: 1\n  # end\ny: 1\n";

fn edit_x(src: &str, f: impl FnOnce(&Mapping)) -> String {
    let file = YamlFile::from_str(src).unwrap();
    let root = file.document().unwrap().as_mapping().unwrap();
    f(&root.get_mapping("x").unwrap());
    assert_file_parser_shaped(&file);
    file.to_string()
}

fn assert_x_keys(yaml: &str, expected: &[&str]) {
    let file = YamlFile::from_str(yaml).unwrap();
    let root = file.document().unwrap().as_mapping().unwrap();
    let keys: Vec<String> = root.keys().map(|k| k.to_string()).collect();
    assert_eq!(keys, ["x", "y"]);
    let x = root.get_mapping("x").unwrap();
    let keys: Vec<String> = x.keys().map(|k| k.to_string()).collect();
    assert_eq!(keys, expected);
}

#[test]
fn test_set_appends_before_trailing_comment() {
    let out = edit_x(SRC, |x| x.set("z", 0));
    assert_eq!(out, "x:\n  a: 1\n  z: 0\n  # end\ny: 1\n");
    assert_x_keys(&out, &["a", "z"]);
}

#[test]
fn test_insert_at_index_appends_before_trailing_comment() {
    let out = edit_x(SRC, |x| x.insert_at_index(5, "z", 0));
    assert_eq!(out, "x:\n  a: 1\n  z: 0\n  # end\ny: 1\n");
    assert_x_keys(&out, &["a", "z"]);
}

// A comment indented deeper than the mapping's keys leaves an INDENT of
// its own width at the end of the mapping; the new key must not take it.
const DEEPER_SRC: &str = "x:\n  a: 1\n    # c\ny: 1\n";

#[test]
fn test_set_before_deeper_trailing_comment() {
    let out = edit_x(DEEPER_SRC, |x| x.set("k", "v"));
    assert_eq!(out, "x:\n  a: 1\n  k: v\n    # c\ny: 1\n");
    assert_x_keys(&out, &["a", "k"]);
}

#[test]
fn test_insert_at_index_before_deeper_trailing_comment() {
    let out = edit_x(DEEPER_SRC, |x| x.insert_at_index(5, "k", "v"));
    assert_eq!(out, "x:\n  a: 1\n  k: v\n    # c\ny: 1\n");
    assert_x_keys(&out, &["a", "k"]);
}

#[test]
fn test_set_two_entries_before_deeper_trailing_comment() {
    let src = "x:\n  a: 1\n  b: 2\n    # c\ny: 1\n";
    let out = edit_x(src, |x| x.set("k", "v"));
    assert_eq!(out, "x:\n  a: 1\n  b: 2\n  k: v\n    # c\ny: 1\n");
    assert_x_keys(&out, &["a", "b", "k"]);
}

#[test]
fn set_after_comment_ending_the_input() {
    assert_eq!(
        edit_x("x:\n  a: 1\n  # end", |m| m.set("b", 2)),
        "x:\n  a: 1\n  b: 2\n  # end"
    );
}

#[test]
fn root_set_after_comment_ending_the_input() {
    let file = YamlFile::from_str("a: 1\n# end").unwrap();
    let root = file.document().unwrap().as_mapping().unwrap();
    root.set("b", 2);
    assert_file_cst_ok(&file);
    assert_eq!(file.to_string(), "a: 1\n# end\nb: 2");
}
