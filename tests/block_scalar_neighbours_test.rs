//! A block scalar written by an edit must not take in the lines after it:
//! a whitespace-only line with spaces past its body's column, or a comment
//! line indented as far as its body, would otherwise read as body text.

mod common;

use common::assert_file_cst_ok;
use rowan::ast::AstNode;
use std::str::FromStr;
use yaml_edit::path::YamlPath;
use yaml_edit::{debug, YamlFile, YamlNode};

/// Check that `file` renders as `expected` and that the scalar at `path`
/// reads back as `value`.
#[track_caller]
fn check(file: &YamlFile, expected: &str, path: &str, value: &str) {
    let text = file.to_string();
    assert_eq!(text, expected);
    assert_file_cst_ok(file);
    let reparsed = YamlFile::from_str(&text).unwrap();
    let node = reparsed.document().unwrap().try_get_path(path).unwrap();
    assert_eq!(node.as_scalar().unwrap().as_string(), value);
}

/// Like [`check`], and the tree is also the one the parser builds for the
/// text. Block scalars built from a string are a single token, unlike
/// parsed ones, so this only holds for scalars copied from a parsed file.
#[track_caller]
fn check_parser_shaped(file: &YamlFile, expected: &str, path: &str, value: &str) {
    check(file, expected, path, value);
    let reparsed = YamlFile::from_str(expected).unwrap();
    assert_eq!(
        debug::tree_to_string(file.syntax()),
        debug::tree_to_string(reparsed.syntax())
    );
}

/// The value of `v` in `src`, as a node of a parsed file.
fn value_of(src: &str) -> YamlNode {
    let file = YamlFile::from_str(src).unwrap();
    file.document()
        .unwrap()
        .as_mapping()
        .unwrap()
        .get("v")
        .unwrap()
}

#[test]
fn test_insert_after_before_deep_whitespace_line() {
    let f = YamlFile::from_str("a: 1\n     \nb: 2\n").unwrap();
    assert!(f.insert_after("a", "k", "text\n"));
    check(&f, "a: 1\nk: |\n  text\n\nb: 2\n", "k", "text\n");
}

#[test]
fn test_insert_after_keeps_shallow_whitespace_line() {
    let f = YamlFile::from_str("a: 1\n  \nb: 2\n").unwrap();
    assert!(f.insert_after("a", "k", "text\n"));
    check(&f, "a: 1\nk: |\n  text\n  \nb: 2\n", "k", "text\n");
}

#[test]
fn test_insert_after_stripped_before_deep_whitespace_line() {
    let f = YamlFile::from_str("a: 1\n     \nb: 2\n").unwrap();
    assert!(f.insert_after("a", "k", "x\ny"));
    check(&f, "a: 1\nk: |-\n  x\n  y\n\nb: 2\n", "k", "x\ny");
}

#[test]
fn test_insert_after_before_tab_line() {
    let f = YamlFile::from_str("a: 1\n  \t\nb: 2\n").unwrap();
    assert!(f.insert_after("a", "k", "text\n"));
    check(&f, "a: 1\nk: |\n  text\n\nb: 2\n", "k", "text\n");
}

#[test]
fn test_insert_after_nested_before_deep_whitespace_line() {
    let f = YamlFile::from_str("x:\n  a: 1\n       \n  b: 2\n").unwrap();
    let x = f.document().unwrap().get_mapping("x").unwrap();
    assert!(x.insert_after("a", "k", "text\n"));
    check(
        &f,
        "x:\n  a: 1\n  k: |\n    text\n\n  b: 2\n",
        "x.k",
        "text\n",
    );
}

#[test]
fn test_insert_before_after_deep_whitespace_line() {
    let f = YamlFile::from_str("a: 1\nb: 2\n     \nc: 3\n").unwrap();
    assert!(f.insert_before("b", "k", "text\n"));
    check(&f, "a: 1\nk: |\n  text\nb: 2\n     \nc: 3\n", "k", "text\n");
}

#[test]
fn test_insert_after_parsed_scalar_before_deep_whitespace_line() {
    let f = YamlFile::from_str("a: 1\n     \nb: 2\n").unwrap();
    assert!(f.insert_after("a", "k", value_of("v: |\n  text\n")));
    check_parser_shaped(&f, "a: 1\nk: |\n  text\n\nb: 2\n", "k", "text\n");
}

#[test]
fn test_set_before_deep_whitespace_line() {
    let f = YamlFile::from_str("a: 1\n     \nb: 2\n").unwrap();
    f.set("a", "text\n");
    check(&f, "a: |\n  text\n\nb: 2\n", "a", "text\n");
}

#[test]
fn test_set_kept_before_blank_line() {
    let f = YamlFile::from_str("a: 1\n\nb: 2\n").unwrap();
    f.set("a", "text\n\n");
    check(&f, "a: |+\n  text\n\nb: 2\n", "a", "text\n\n");
}

#[test]
fn test_set_path_before_deep_whitespace_line() {
    let f = YamlFile::from_str("x:\n  a: 1\n       \ny: 2\n").unwrap();
    f.document().unwrap().try_set_path("x.a", "text\n").unwrap();
    check(&f, "x:\n  a: |\n    text\n\ny: 2\n", "x.a", "text\n");
}

#[test]
fn test_sequence_set_before_deep_whitespace_line() {
    let f = YamlFile::from_str("- 1\n     \n- 2\n").unwrap();
    let seq = f.document().unwrap().as_sequence().unwrap();
    assert!(seq.set(0, "text\n"));
    check(&f, "- |\n  text\n\n- 2\n", "[0]", "text\n");
}

#[test]
fn test_insert_after_before_deep_comment_line() {
    let f = YamlFile::from_str("a: 1\n    # c\nb: 2\n").unwrap();
    assert!(f.insert_after("a", "k", "text\n"));
    check(&f, "a: 1\nk: |\n  text\n# c\nb: 2\n", "k", "text\n");
}

#[test]
fn test_insert_after_keeps_shallow_comment_line() {
    let f = YamlFile::from_str("x:\n  a: 1\n   # c\n  b: 2\n").unwrap();
    let x = f.document().unwrap().get_mapping("x").unwrap();
    assert!(x.insert_after("a", "k", "text\n"));
    check(
        &f,
        "x:\n  a: 1\n  k: |\n    text\n   # c\n  b: 2\n",
        "x.k",
        "text\n",
    );
}

#[test]
fn test_insert_after_nested_before_deep_comment_line() {
    let f = YamlFile::from_str("x:\n  a: 1\n    # c\n  b: 2\n").unwrap();
    let x = f.document().unwrap().get_mapping("x").unwrap();
    assert!(x.insert_after("a", "k", "text\n"));
    check(
        &f,
        "x:\n  a: 1\n  k: |\n    text\n  # c\n  b: 2\n",
        "x.k",
        "text\n",
    );
}

#[test]
fn test_insert_after_before_blank_and_deep_comment_lines() {
    let f = YamlFile::from_str("a: 1\n\n    # c\nb: 2\n").unwrap();
    assert!(f.insert_after("a", "k", "text\n"));
    check(&f, "a: 1\nk: |\n  text\n\n# c\nb: 2\n", "k", "text\n");
}

#[test]
fn test_insert_after_before_two_deep_comment_lines() {
    let f = YamlFile::from_str("a: 1\n    # c\n    # d\nb: 2\n").unwrap();
    assert!(f.insert_after("a", "k", "text\n"));
    check(
        &f,
        "a: 1\nk: |\n  text\n# c\n    # d\nb: 2\n",
        "k",
        "text\n",
    );
}

#[test]
fn test_insert_after_kept_before_deep_comment_line() {
    let f = YamlFile::from_str("a: 1\n    # c\nb: 2\n").unwrap();
    assert!(f.insert_after("a", "k", "text\n\n"));
    check(&f, "a: 1\nk: |+\n  text\n\n# c\nb: 2\n", "k", "text\n\n");
}

#[test]
fn test_insert_after_parsed_scalar_before_deep_comment_line() {
    let f = YamlFile::from_str("a: 1\n\n    # c\nb: 2\n").unwrap();
    assert!(f.insert_after("a", "k", value_of("v: |\n  text\n")));
    check_parser_shaped(&f, "a: 1\nk: |\n  text\n\n# c\nb: 2\n", "k", "text\n");
}

#[test]
fn test_set_before_deep_comment_line() {
    let f = YamlFile::from_str("a: 1\n    # c\nb: 2\n").unwrap();
    f.set("a", "text\n");
    check(&f, "a: |\n  text\n# c\nb: 2\n", "a", "text\n");
}

#[test]
fn test_sequence_set_before_deep_comment_line() {
    let f = YamlFile::from_str("- 1\n    # c\n- 2\n").unwrap();
    let seq = f.document().unwrap().as_sequence().unwrap();
    assert!(seq.set(0, "text\n"));
    check(&f, "- |\n  text\n# c\n- 2\n", "[0]", "text\n");
}
