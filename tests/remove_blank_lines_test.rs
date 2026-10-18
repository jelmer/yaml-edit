//! Removing a collection entry that sits between blank lines.

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

fn remove_item(yaml: &str, path: &[&str], index: usize) -> String {
    let file = YamlFile::from_str(yaml).unwrap();
    let doc = file.document().unwrap();
    let seq = match path.split_last() {
        None => doc.as_sequence().unwrap(),
        Some((last, init)) => {
            let mut m = doc.as_mapping().unwrap();
            for p in init {
                m = m.get_mapping(p).unwrap();
            }
            m.get_sequence(last).unwrap()
        }
    };
    seq.remove(index).unwrap();
    assert_file_cst_ok(&file);
    file.to_string()
}

#[test]
fn test_remove_item_between_blank_lines() {
    assert_eq!(remove_item("- a\n\n- b\n\n- c\n", &[], 1), "- a\n\n- c\n");
    assert_eq!(
        remove_item("x:\n  - a\n\n  - b\n\n  - c\ny: 1\n", &["x"], 1),
        "x:\n  - a\n\n  - c\ny: 1\n"
    );
}

#[test]
fn test_remove_item_keeps_single_blank_line_side() {
    assert_eq!(remove_item("- a\n\n- b\n- c\n", &[], 1), "- a\n\n- c\n");
    assert_eq!(remove_item("- a\n- b\n\n- c\n", &[], 1), "- a\n\n- c\n");
}

#[test]
fn test_remove_item_after_comment() {
    assert_eq!(
        remove_item("- a\n# c\n- b\n\n- c\n", &[], 1),
        "- a\n# c\n\n- c\n"
    );
    assert_eq!(
        remove_item("- a\n# c\n\n- b\n\n- c\n", &[], 1),
        "- a\n# c\n\n- c\n"
    );
    assert_eq!(
        remove_item("- a  # k\n\n- b\n\n- c\n", &[], 1),
        "- a  # k\n\n- c\n"
    );
}

#[test]
fn test_remove_after_comment() {
    assert_eq!(
        remove("a: 1\n# c\nb: 2\n\nc: 3\n", &[], "b"),
        "a: 1\n# c\n\nc: 3\n"
    );
    assert_eq!(
        remove("a: 1\n# c\n\nb: 2\n\nc: 3\n", &[], "b"),
        "a: 1\n# c\n\nc: 3\n"
    );
}

/// The blank lines after a block value are the mapping's, as after a
/// scalar one.
#[test]
fn test_remove_block_value_before_blank_line() {
    assert_eq!(
        remove("a: 1\nk:\n  x: 1\n\nz: 2\n", &[], "k"),
        "a: 1\n\nz: 2\n"
    );
    assert_eq!(remove("a: 1\nk: 1\n\nz: 2\n", &[], "k"), "a: 1\n\nz: 2\n");
}
