//! Appending to an indented mapping whose last line is a comment.
//!
//! The parser leaves that comment line's INDENT at the end of the nested
//! mapping and lifts the comment itself to the parent.

use std::str::FromStr;
use yaml_edit::{Mapping, YamlFile};

mod common;
use common::assert_file_cst_ok;

const SRC: &str = "x:\n  a: 1\n  # end\ny: 1\n";

fn edit(f: impl FnOnce(&Mapping)) -> String {
    let file = YamlFile::from_str(SRC).unwrap();
    let root = file.document().unwrap().as_mapping().unwrap();
    f(&root.get_mapping("x").unwrap());
    assert_file_cst_ok(&file);
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
