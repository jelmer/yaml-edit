//! Inserting a value written as a kept (`|+`, `>+`) block scalar in front of
//! blank lines: those lines would be read as part of the scalar, so the new
//! entry goes after them.

mod common;

use common::assert_file_cst_ok;
use std::str::FromStr;
use yaml_edit::path::YamlPath;
use yaml_edit::{ScalarValue, Sequence, YamlFile};

/// Check that `file` renders as `expected` and that the scalar at `path`
/// reads back as `value`.
#[track_caller]
fn check(file: &YamlFile, expected: &str, path: &str, value: &str) {
    let text = file.to_string();
    assert_eq!(text, expected);
    assert_file_cst_ok(file);
    let reparsed = YamlFile::from_str(&text).unwrap();
    let node = reparsed.document().unwrap().try_get_path(path).unwrap();
    let scalar = match node.as_tagged() {
        Some(tagged) => tagged.value().unwrap(),
        None => node.as_scalar().unwrap().clone(),
    };
    assert_eq!(scalar.as_string(), value);
}

#[test]
fn test_insert_after_before_blank_line() {
    let f = YamlFile::from_str("a: 1\n\nb: 2\n").unwrap();
    assert!(f.insert_after("a", "k", "text\n\n"));
    check(&f, "a: 1\n\nk: |+\n  text\n\nb: 2\n", "k", "text\n\n");
}

#[test]
fn test_insert_after_before_blank_lines_and_comment() {
    let f = YamlFile::from_str("a: 1\n\n\n# c\nb: 2\n").unwrap();
    assert!(f.insert_after("a", "k", "text\n\n"));
    check(
        &f,
        "a: 1\n\n\nk: |+\n  text\n\n# c\nb: 2\n",
        "k",
        "text\n\n",
    );
}

#[test]
fn test_insert_after_before_whitespace_line() {
    let f = YamlFile::from_str("a: 1\n    \nb: 2\n").unwrap();
    assert!(f.insert_after("a", "k", "text\n\n"));
    check(&f, "a: 1\n    \nk: |+\n  text\n\nb: 2\n", "k", "text\n\n");
}

#[test]
fn test_insert_after_folded_before_blank_line() {
    let f = YamlFile::from_str("a: 1\n\nb: 2\n").unwrap();
    assert!(f.insert_after("a", "k", ScalarValue::folded("text\n\n")));
    check(&f, "a: 1\n\nk: >+\n  text\n\nb: 2\n", "k", "text\n\n");
}

#[test]
fn test_insert_after_tagged_before_blank_line() {
    let f = YamlFile::from_str("a: 1\n\nb: 2\n").unwrap();
    assert!(f.insert_after("a", "k", ScalarValue::string("text\n\n").with_tag("!t")));
    check(&f, "a: 1\n\nk: !t |+\n  text\n\nb: 2\n", "k", "text\n\n");
}

#[test]
fn test_insert_after_parsed_scalar_before_blank_line() {
    let source = YamlFile::from_str("s: |+\n  text\n\n").unwrap();
    let value = source.document().unwrap().get("s").unwrap();
    let f = YamlFile::from_str("a: 1\n\nb: 2\n").unwrap();
    assert!(f.insert_after("a", "k", value));
    check(&f, "a: 1\n\nk: |+\n  text\n\nb: 2\n", "k", "text\n\n");
}

#[test]
fn test_insert_after_mapping_ending_in_kept_scalar() {
    let source = YamlFile::from_str("m:\n  s: |+\n    text\n\n").unwrap();
    let value = source.document().unwrap().get("m").unwrap();
    let f = YamlFile::from_str("a: 1\n\nb: 2\n").unwrap();
    assert!(f.insert_after("a", "k", value));
    check(
        &f,
        "a: 1\n\nk:\n  s: |+\n    text\n\nb: 2\n",
        "k.s",
        "text\n\n",
    );
}

#[test]
fn test_insert_after_nested_before_blank_line() {
    let f = YamlFile::from_str("x:\n  a: 1\n\n  b: 2\n").unwrap();
    let x = f.document().unwrap().get_mapping("x").unwrap();
    assert!(x.insert_after("a", "k", "text\n\n"));
    check(
        &f,
        "x:\n  a: 1\n\n  k: |+\n    text\n\n  b: 2\n",
        "x.k",
        "text\n\n",
    );
}

#[test]
fn test_insert_after_explicit_key_before_blank_line() {
    let f = YamlFile::from_str("? a\n: 1\n\n? b\n: 2\n").unwrap();
    assert!(f.insert_after("a", "k", "text\n\n"));
    check(
        &f,
        "? a\n: 1\n\n? k\n: |+\n  text\n\n? b\n: 2\n",
        "k",
        "text\n\n",
    );
}

#[test]
fn test_move_after_before_blank_line() {
    let f = YamlFile::from_str("a: 1\n\nb: 2\nk: 3\n").unwrap();
    assert!(f.move_after("a", "k", "text\n\n"));
    check(&f, "a: 1\n\nk: |+\n  text\n\nb: 2\n", "k", "text\n\n");
}

#[test]
fn test_set_with_field_order_before_blank_line() {
    let f = YamlFile::from_str("a: 1\n\nb: 2\n").unwrap();
    f.document()
        .unwrap()
        .set_with_field_order("k", "text\n\n", ["a", "k", "b"]);
    check(&f, "a: 1\n\nk: |+\n  text\n\nb: 2\n", "k", "text\n\n");
}

#[test]
fn test_insert_before_after_blank_line() {
    let f = YamlFile::from_str("a: 1\n\nb: 2\n").unwrap();
    assert!(f.insert_before("b", "k", "text\n\n"));
    check(&f, "a: 1\n\nk: |+\n  text\n\nb: 2\n", "k", "text\n\n");
}

#[test]
fn test_insert_at_index_after_blank_line() {
    let f = YamlFile::from_str("a: 1\n\nb: 2\n").unwrap();
    f.insert_at_index(1, "k", "text\n\n");
    check(&f, "a: 1\n\nk: |+\n  text\n\nb: 2\n", "k", "text\n\n");
}

#[test]
fn test_set_at_end_after_blank_line() {
    let f = YamlFile::from_str("a: 1\n\n").unwrap();
    f.set("k", "text\n\n");
    check(&f, "a: 1\n\nk: |+\n  text\n\n", "k", "text\n\n");
}

#[test]
fn test_clipped_scalar_stays_before_blank_line() {
    let f = YamlFile::from_str("a: 1\n\nb: 2\n").unwrap();
    assert!(f.insert_after("a", "k", "text\n"));
    check(&f, "a: 1\nk: |\n  text\n\nb: 2\n", "k", "text\n");
}

#[test]
fn test_push_before_blank_line() {
    let f = YamlFile::from_str("- 1\n\n").unwrap();
    f.document()
        .unwrap()
        .as_sequence()
        .unwrap()
        .push("text\n\n");
    check(&f, "- 1\n\n- |+\n  text\n\n", "[1]", "text\n\n");
}

#[test]
fn test_push_nested_before_blank_line() {
    let f = YamlFile::from_str("x:\n  - 1\n\ny: 2\n").unwrap();
    f.document()
        .unwrap()
        .get_sequence("x")
        .unwrap()
        .push("text\n\n");
    check(
        &f,
        "x:\n  - 1\n\n  - |+\n    text\n\ny: 2\n",
        "x[1]",
        "text\n\n",
    );
}

#[test]
fn test_push_clipped_scalar_stays_before_blank_line() {
    let f = YamlFile::from_str("x:\n  - 1\n\ny: 2\n").unwrap();
    f.document()
        .unwrap()
        .get_sequence("x")
        .unwrap()
        .push("text\n");
    check(&f, "x:\n  - 1\n  - |\n    text\n\ny: 2\n", "x[1]", "text\n");
}

#[test]
fn test_sequence_insert_after_blank_line() {
    let f = YamlFile::from_str("- 1\n\n- 2\n").unwrap();
    f.document()
        .unwrap()
        .as_sequence()
        .unwrap()
        .insert(1, "text\n\n");
    check(&f, "- 1\n\n- |+\n  text\n\n- 2\n", "[1]", "text\n\n");
}

#[test]
fn test_set_path_nested_after_blank_line() {
    let f = YamlFile::from_str("x:\n  a: 1\n\ny: 2\n").unwrap();
    f.document()
        .unwrap()
        .try_set_path("x.k", "text\n\n")
        .unwrap();
    check(
        &f,
        "x:\n  a: 1\n\n  k: |+\n    text\n\ny: 2\n",
        "x.k",
        "text\n\n",
    );
}

#[test]
fn test_push_onto_new_sequence_before_blank_line() {
    let f = YamlFile::from_str("a: 1\n\nb: 2\n").unwrap();
    assert!(f.insert_after("a", "x", Sequence::new_pending_block()));
    f.document()
        .unwrap()
        .get_sequence("x")
        .unwrap()
        .push("text\n\n");
    check(
        &f,
        "a: 1\nx:\n\n  - |+\n    text\n\nb: 2\n",
        "x[0]",
        "text\n\n",
    );
}

#[test]
fn test_set_in_compact_mapping_before_blank_line() {
    let f = YamlFile::from_str("- a: 1\n\n- 2\n").unwrap();
    let item = f.document().unwrap().as_sequence().unwrap().get(0).unwrap();
    item.as_mapping().unwrap().set("k", "text\n\n");
    check(
        &f,
        "- a: 1\n\n  k: |+\n    text\n\n- 2\n",
        "[0].k",
        "text\n\n",
    );
}
