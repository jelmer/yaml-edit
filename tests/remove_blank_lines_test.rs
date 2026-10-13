//! Removing a mapping entry that sits between blank lines.

use std::str::FromStr;
use yaml_edit::YamlFile;

mod common;
use common::assert_file_cst_ok;

fn remove(yaml: &str, path: &[&str], key: &str) -> String {
    let file = YamlFile::from_str(yaml).unwrap();
    let mut m = file.document().unwrap().as_mapping().unwrap();
    for p in path {
        m = m.get_mapping(p).unwrap();
    }
    m.remove(key).unwrap();
    assert_file_cst_ok(&file);
    file.to_string()
}

#[test]
fn test_remove_between_blank_lines() {
    let out = remove("a: 1\n\nb: 2\n\nc: 3\n", &[], "b");
    assert_eq!(out, "a: 1\n\nc: 3\n");
    let reparsed = YamlFile::from_str(&out).unwrap();
    let keys: Vec<String> = reparsed
        .document()
        .unwrap()
        .as_mapping()
        .unwrap()
        .keys()
        .map(|k| k.to_string())
        .collect();
    assert_eq!(keys, ["a", "c"]);
}

#[test]
fn test_remove_nested_between_blank_lines() {
    let out = remove("x:\n  a: 1\n\n  b: 2\n\n  c: 3\ny: 1\n", &["x"], "b");
    assert_eq!(out, "x:\n  a: 1\n\n  c: 3\ny: 1\n");
}

#[test]
fn test_remove_keeps_single_blank_line_side() {
    assert_eq!(remove("a: 1\n\nb: 2\nc: 3\n", &[], "b"), "a: 1\n\nc: 3\n");
    assert_eq!(remove("a: 1\nb: 2\n\nc: 3\n", &[], "b"), "a: 1\n\nc: 3\n");
}

#[test]
fn test_remove_explicit_key_between_blank_lines() {
    let out = remove("? a\n: 1\n\n? b\n: 2\n\n? c\n: 3\n", &[], "b");
    assert_eq!(out, "? a\n: 1\n\n? c\n: 3\n");
}

#[test]
fn test_remove_explicit_key_after_blank_line() {
    let out = remove("? a\n: 1\n\n? b\n: 2\n? c\n: 3\n", &[], "b");
    assert_eq!(out, "? a\n: 1\n\n? c\n: 3\n");
}

#[test]
fn test_remove_explicit_key_takes_its_line_break() {
    let out = remove("? a\n: 1\n? b\n: 2\n? c\n: 3\n", &[], "b");
    assert_eq!(out, "? a\n: 1\n? c\n: 3\n");
}
