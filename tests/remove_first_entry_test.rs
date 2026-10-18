//! Removing the first entry or item of a block collection leaves the
//! comment and blank lines after it in front of the collection, where the
//! parser puts them.

mod common;

use rowan::ast::AstNode;
use std::str::FromStr;
use yaml_edit::{debug, Document, YamlFile};

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

fn item(d: &Document) -> yaml_edit::YamlNode {
    d.as_sequence().unwrap().get(0).unwrap()
}

fn remove_a_from_item(d: &Document) {
    assert!(item(d).as_mapping().unwrap().remove("a").is_some());
}

fn remove_first_from_item(d: &Document) {
    assert!(item(d).as_sequence().unwrap().remove(0).is_some());
}

fn remove_a_from_top(d: &Document) {
    let top = d.as_mapping().unwrap().get_mapping("top").unwrap();
    assert!(top.remove("a").is_some());
}

#[test]
fn remove_first_entry_of_mapping_in_sequence_item() {
    check_edit(
        "- a: 1\n  # c\n  b: 2\n- 3\n",
        remove_a_from_item,
        "-\n  # c\n  b: 2\n- 3\n",
    );
    check_edit(
        "- a: 1  # k\n  # c\n  b: 2\n- 3\n",
        remove_a_from_item,
        "-\n  # c\n  b: 2\n- 3\n",
    );
    check_edit(
        "-   a: 1\n      # c\n    b: 2\n",
        remove_a_from_item,
        "-\n      # c\n    b: 2\n",
    );
    check_edit(
        "- ? a\n  : 1\n  # c\n  b: 2\n",
        remove_a_from_item,
        "-\n  # c\n  b: 2\n",
    );
    check_edit(
        "- &x\n  a: 1\n  # c\n  b: 2\n",
        remove_a_from_item,
        "- &x\n  # c\n  b: 2\n",
    );
    check_edit(
        "- a: 1  # k\n  b: 2\n- 3\n",
        remove_a_from_item,
        "- b: 2\n- 3\n",
    );
}

#[test]
fn remove_first_entry_before_blank_line_in_sequence_item() {
    check_edit(
        "- a: 1\n\n  b: 2\n- 3\n",
        remove_a_from_item,
        "-\n\n  b: 2\n- 3\n",
    );
    check_edit(
        "- a: 1\n\n  # c\n  b: 2\n- 3\n",
        remove_a_from_item,
        "-\n\n  # c\n  b: 2\n- 3\n",
    );
}

#[test]
fn remove_first_item_of_sequence_in_sequence_item() {
    check_edit(
        "- - 1\n  # c\n  - 2\n- 3\n",
        remove_first_from_item,
        "-\n  # c\n  - 2\n- 3\n",
    );
    check_edit(
        "- - 1\n\n  - 2\n- 3\n",
        remove_first_from_item,
        "-\n\n  - 2\n- 3\n",
    );
    // The comment on the removed item's line goes with it.
    check_edit(
        "- - 1  # k\n  - 2\n- 3\n",
        remove_first_from_item,
        "- - 2\n- 3\n",
    );
}

#[test]
fn remove_first_entry_of_mapping_value() {
    check_edit(
        "top:\n  a: 1\n  # c\n  b: 2\n",
        remove_a_from_top,
        "top:\n  # c\n  b: 2\n",
    );
    check_edit(
        "top:\n  a: 1\n\n  b: 2\n",
        remove_a_from_top,
        "top:\n\n  b: 2\n",
    );
    check_edit(
        "top:\n  a: 1  # k\n  b: 2\n",
        remove_a_from_top,
        "top:\n  b: 2\n",
    );
    check_edit(
        "top: !!map\n  a: 1\n  # c\n  b: 2\n",
        |d| {
            let top = d.as_mapping().unwrap().get("top").unwrap();
            let m = top.as_tagged().unwrap().as_mapping().unwrap();
            assert!(m.remove("a").is_some());
        },
        "top: !!map\n  # c\n  b: 2\n",
    );
    check_edit(
        "top:\n- 1  # k\n- 2\n",
        |d| {
            let top = d.as_mapping().unwrap().get_sequence("top").unwrap();
            assert!(top.remove(0).is_some());
        },
        "top:\n- 2\n",
    );
}

#[test]
fn remove_first_entry_of_explicit_key_mapping() {
    check_edit(
        "? a: 1\n  # c\n  b: 2\n: v\n",
        |d| {
            let key = d.as_mapping().unwrap().keys().next().unwrap();
            assert!(key.as_mapping().unwrap().remove("a").is_some());
        },
        "?\n  # c\n  b: 2\n: v\n",
    );
}

#[test]
fn remove_first_entry_of_document_root() {
    let check = |source: &str, expected: &str| {
        let file = YamlFile::from_str(source).unwrap();
        let m = file.document().unwrap().as_mapping().unwrap();
        assert!(m.remove("a").is_some());
        assert_eq!(file.to_string(), expected);
        let reparsed = YamlFile::from_str(expected).unwrap();
        assert_eq!(
            debug::tree_to_string(file.syntax()),
            debug::tree_to_string(reparsed.syntax())
        );
    };
    check("---\na: 1\n# c\nb: 2\n", "---\n# c\nb: 2\n");
    check("a: 1\n\nb: 2\n", "\nb: 2\n");
}

#[test]
fn edit_after_removing_first_entry() {
    check_edit(
        "- a: 1\n  # c\n  b: 2\n- 3\n",
        |d| {
            let m = item(d).as_mapping().cloned().unwrap();
            assert!(m.remove("a").is_some());
            m.insert_at_index(0, "y", 4);
            m.set("z", 5);
        },
        "-\n  # c\n  y: 4\n  b: 2\n  z: 5\n- 3\n",
    );
    check_edit(
        "- a: 1\n  # c\n  b: 2\n- 3\n",
        |d| {
            let m = item(d).as_mapping().cloned().unwrap();
            assert!(m.remove("a").is_some());
            assert!(m.remove("b").is_some());
        },
        "-\n  # c\n  {}\n- 3\n",
    );
}

#[test]
fn remove_first_entry_of_multiline_flow_mapping() {
    check_edit(
        "a: {\n    x: 1,\n    y: 2\n  }\n",
        |d| {
            let a = d.as_mapping().unwrap().get_mapping("a").unwrap();
            assert!(a.remove("x").is_some());
        },
        "a: {\n    y: 2\n  }\n",
    );
}
