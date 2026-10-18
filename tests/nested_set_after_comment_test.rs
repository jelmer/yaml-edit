//! Appending to an indented mapping whose last line is a comment.
//!
//! The parser leaves that comment line's INDENT at the end of the nested
//! mapping and lifts the comment itself to the parent.

use rowan::ast::AstNode;
use std::str::FromStr;
use yaml_edit::{debug, Mapping, YamlFile};

mod common;
use common::assert_file_cst_ok;

const SRC: &str = "x:\n  a: 1\n  # end\ny: 1\n";

fn edit(f: impl FnOnce(&Mapping)) -> String {
    edit_src(SRC, f)
}

fn edit_src(src: &str, f: impl FnOnce(&Mapping)) -> String {
    let file = YamlFile::from_str(src).unwrap();
    let root = file.document().unwrap().as_mapping().unwrap();
    f(&root.get_mapping("x").unwrap());
    assert_file_cst_ok(&file);
    let out = file.to_string();
    let reparsed = YamlFile::from_str(&out).unwrap();
    assert_eq!(
        debug::shape_to_string(file.syntax()),
        debug::shape_to_string(reparsed.syntax())
    );
    out
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
    let out = edit(|x| x.set("z", 0));
    assert_eq!(out, "x:\n  a: 1\n  z: 0\n  # end\ny: 1\n");
    assert_x_keys(&out, &["a", "z"]);
}

#[test]
fn test_insert_at_index_appends_before_trailing_comment() {
    let out = edit(|x| x.insert_at_index(5, "z", 0));
    assert_eq!(out, "x:\n  a: 1\n  z: 0\n  # end\ny: 1\n");
    assert_x_keys(&out, &["a", "z"]);
}

// A comment indented deeper than the mapping's keys leaves an INDENT of
// its own width at the end of the mapping; the new key must not take it.
const DEEPER_SRC: &str = "x:\n  a: 1\n    # c\ny: 1\n";

#[test]
fn test_set_before_deeper_trailing_comment() {
    let out = edit_src(DEEPER_SRC, |x| x.set("k", "v"));
    assert_eq!(out, "x:\n  a: 1\n  k: v\n    # c\ny: 1\n");
    assert_x_keys(&out, &["a", "k"]);
}

#[test]
fn test_insert_at_index_before_deeper_trailing_comment() {
    let out = edit_src(DEEPER_SRC, |x| x.insert_at_index(5, "k", "v"));
    assert_eq!(out, "x:\n  a: 1\n  k: v\n    # c\ny: 1\n");
    assert_x_keys(&out, &["a", "k"]);
}

#[test]
fn test_set_two_entries_before_deeper_trailing_comment() {
    let src = "x:\n  a: 1\n  b: 2\n    # c\ny: 1\n";
    let out = edit_src(src, |x| x.set("k", "v"));
    assert_eq!(out, "x:\n  a: 1\n  b: 2\n  k: v\n    # c\ny: 1\n");
    assert_x_keys(&out, &["a", "b", "k"]);
}

#[test]
fn set_after_comment_ending_the_input() {
    assert_eq!(
        edit_src("x:\n  a: 1\n  # end", |m| m.set("b", 2)),
        "x:\n  a: 1\n  # end\n  b: 2\n"
    );
}

#[test]
fn root_set_after_comment_ending_the_input() {
    let file = YamlFile::from_str("a: 1\n# end").unwrap();
    let root = file.document().unwrap().as_mapping().unwrap();
    root.set("b", 2);
    assert_file_cst_ok(&file);
    assert_eq!(file.to_string(), "a: 1\n# end\nb: 2\n");
}
