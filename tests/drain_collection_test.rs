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

fn item_mapping(d: &Document) -> yaml_edit::Mapping {
    d.as_sequence()
        .unwrap()
        .get(0)
        .unwrap()
        .as_mapping()
        .cloned()
        .unwrap()
}

fn item_sequence(d: &Document) -> yaml_edit::Sequence {
    d.as_sequence()
        .unwrap()
        .get(0)
        .unwrap()
        .as_sequence()
        .cloned()
        .unwrap()
}

#[test]
fn remove_only_entry_of_mapping_in_sequence_item() {
    let remove = |d: &Document| {
        assert!(item_mapping(d).remove("b").is_some());
    };
    check_edit("- b: 2\n- 3\n", remove, "- {}\n- 3\n");
    check_edit("- b: 2\n", remove, "- {}\n");
    check_edit("- b: 2", remove, "- {}");
    check_edit("- &a\n  b: 2\n- 3\n", remove, "- &a\n  {}\n- 3\n");
    check_edit("- ? b\n- 3\n", remove, "- {}\n- 3\n");
}

#[test]
fn remove_only_item_of_sequence_in_sequence_item() {
    let remove = |d: &Document| {
        assert!(item_sequence(d).remove(0).is_some());
    };
    check_edit("- - 1\n- 3\n", remove, "- []\n- 3\n");
    check_edit("- - 1\n", remove, "- []\n");
    check_edit("- - 1", remove, "- []");
}

#[test]
fn clear_collection_in_sequence_item() {
    check_edit(
        "- - 1\n  - 2\n- 3\n",
        |d| item_sequence(d).clear(),
        "- []\n- 3\n",
    );
    check_edit(
        "- a: 1\n  b: 2\n- 3\n",
        |d| item_mapping(d).clear(),
        "- {}\n- 3\n",
    );
}

#[test]
fn clear_top_level_sequence() {
    let clear = |d: &Document| d.as_sequence().unwrap().clear();
    check_edit("- 1\n", clear, "[]\n");
    check_edit("- 1\n- 2\n", clear, "[]\n");
    check_edit("- 1\n- 2", clear, "[]");
    check_edit("---\n- 1\n", clear, "---\n[]\n");
}

#[test]
fn edit_through_handle_to_drained_sequence_item() {
    check_edit(
        "- b: 2\n- 3\n",
        |d| {
            let m = item_mapping(d);
            m.remove("b");
            m.set("c", 3);
        },
        "- {c: 3}\n- 3\n",
    );
    check_edit(
        "- - 1\n- 3\n",
        |d| {
            let s = item_sequence(d);
            s.remove(0);
            s.push(2);
        },
        "- [2]\n- 3\n",
    );
}

fn key_node(d: &Document) -> yaml_edit::YamlNode {
    d.as_mapping().unwrap().keys().next().unwrap()
}

fn key_mapping(d: &Document) -> yaml_edit::Mapping {
    let key = key_node(d);
    match key.as_tagged() {
        Some(t) => t.as_mapping().unwrap(),
        None => key.as_mapping().cloned().unwrap(),
    }
}

#[test]
fn remove_only_entry_of_explicit_key_mapping() {
    let remove = |d: &Document| {
        assert!(key_mapping(d).remove("a").is_some());
    };
    check_edit("? a: 1\n: v\n", remove, "? {}\n: v\n");
    check_edit("? a: 1\n: v\nz: 2\n", remove, "? {}\n: v\nz: 2\n");
    check_edit("? a: 1\n", remove, "? {}\n");
    check_edit("? a: 1", remove, "? {}");
    check_edit("? &x\n  a: 1\n: v\n", remove, "? &x\n  {}\n: v\n");
    check_edit("? !t\n  a: 1\n: v\n", remove, "? !t\n  {}\n: v\n");
}

#[test]
fn remove_only_item_of_explicit_key_sequence() {
    let remove = |d: &Document| {
        assert!(key_node(d).as_sequence().unwrap().remove(0).is_some());
    };
    check_edit("? - 1\n: v\n", remove, "? []\n: v\n");
    check_edit("? - 1\n: v\nz: 2\n", remove, "? []\n: v\nz: 2\n");
    check_edit("? - 1\n", remove, "? []\n");
    check_edit("? - 1", remove, "? []");
}

#[test]
fn clear_explicit_key_collection() {
    check_edit(
        "? a: 1\n  b: 2\n: v\n",
        |d| key_mapping(d).clear(),
        "? {}\n: v\n",
    );
    check_edit(
        "? - 1\n  - 2\n: v\n",
        |d| key_node(d).as_sequence().unwrap().clear(),
        "? []\n: v\n",
    );
}
