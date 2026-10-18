//! Removing the last entry or item of a collection leaves an empty
//! collection in its place, written `{}` or `[]` where a block one would
//! be empty text.

mod common;

use rowan::ast::AstNode;
use std::str::FromStr;
use yaml_edit::{debug, Document};

#[track_caller]
fn check_edit(source: &str, edit: impl Fn(&Document), expected: &str) {
    let doc = Document::from_str(source).unwrap();
    edit(&doc);
    assert_eq!(doc.to_string(), expected, "editing {source:?}");
    common::assert_cst_ok(&doc);
    let reparsed = Document::from_str(expected).unwrap();
    assert_eq!(
        debug::tree_to_string(doc.syntax()),
        debug::tree_to_string(reparsed.syntax())
    );
}

#[test]
fn edit_through_handle_to_drained_nested_collection() {
    check_edit(
        "top:\n  b: 2\nx: 1\n",
        |d| {
            let m = d.as_mapping().unwrap().get_mapping("top").unwrap();
            m.remove("b");
            m.set("c", 3);
        },
        "top: {c: 3}\nx: 1\n",
    );
    check_edit(
        "top:\n  - 1\n",
        |d| {
            let s = d.as_mapping().unwrap().get_sequence("top").unwrap();
            s.remove(0);
            s.push(3);
        },
        "top: [3]\n",
    );
    check_edit(
        "top:\n  # c\n  b: 2\n",
        |d| {
            let m = d.as_mapping().unwrap().get_mapping("top").unwrap();
            m.remove("b");
            m.set("c", 3);
        },
        "top:\n  # c\n  {c: 3}\n",
    );
}
