//! Removing the last entry of a block mapping leaves the end of the
//! input as it was: with a final line break if the removed entry had one,
//! without one otherwise, and the entries before it whole.

mod common;

use rowan::ast::AstNode;
use std::str::FromStr;
use yaml_edit::{debug, Document};

#[track_caller]
fn check_remove(source: &str, expected: &str) {
    let doc = Document::from_str(source).unwrap();
    let root = doc.as_mapping().unwrap();
    let mapping = root.get_mapping("top").unwrap_or(root);
    assert!(mapping.remove("b").is_some());
    assert_eq!(doc.to_string(), expected, "removing b from {source:?}");
    common::assert_cst_ok(&doc);
    let reparsed = Document::from_str(expected).unwrap();
    assert_eq!(
        debug::tree_to_string(doc.syntax()),
        debug::tree_to_string(reparsed.syntax())
    );
}

#[test]
fn remove_unterminated_last_entry_after_block_value() {
    check_remove("a:\n  x: 1\nb: 2", "a:\n  x: 1");
    check_remove("a:\n  - 1\nb: 2", "a:\n  - 1");
    check_remove("a:\n  x:\n    y: 1\nb: 2", "a:\n  x:\n    y: 1");
    check_remove("a: !t\n  x: 1\nb: 2", "a: !t\n  x: 1");
    check_remove("a: &x\n  x: 1\nb: 2", "a: &x\n  x: 1");
    check_remove("top:\n  a:\n    x: 1\n  b: 2", "top:\n  a:\n    x: 1");
    check_remove("a:\n  x: 1\n# c\nb: 2", "a:\n  x: 1\n# c");
}

#[test]
fn remove_unterminated_last_entry_after_block_scalar() {
    // The line break is part of the scalar's value, so it stays.
    check_remove("a: |\n  text\nb: 2", "a: |\n  text\n");
    check_remove("a: >\n  t\nb: 2", "a: >\n  t\n");
}

#[test]
fn remove_unterminated_last_entry_after_other_entries() {
    check_remove("a: 1\nb: 2", "a: 1");
    check_remove("a: 1 # c\nb: 2", "a: 1 # c");
    check_remove("a:\nb: 2", "a:");
    check_remove("? a\n: 1\nb: 2", "? a\n: 1");
}

#[test]
fn remove_terminated_last_entry() {
    check_remove("a: 1\n? b\n", "a: 1\n");
    check_remove("a: 1\nb:\n", "a: 1\n");
    check_remove("a: 1\nb: |\n  x\n", "a: 1\n");
    check_remove("a:\n  x: 1\nb: 2\n", "a:\n  x: 1\n");
}

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
fn remove_only_entry_of_nested_mapping() {
    check_remove("top:\n  b: 2", "top: {}");
    check_remove("top:\n  b: 2\n", "top: {}\n");
    check_remove("x: 1\ntop:\n  b: 2", "x: 1\ntop: {}");
    check_remove("top: # k\n  b: 2", "top: {} # k");
    check_remove("top: !t\n  b: 2", "top: !t {}");
    check_remove("top: &a\n  b: 2", "top: &a {}");
    check_remove("top:\n  # c\n  b: 2", "top:\n  # c\n  {}");
    check_remove("top:\n  # c\n  b: 2\n", "top:\n  # c\n  {}\n");
}

#[test]
fn clear_nested_mapping() {
    let clear = |d: &Document| d.as_mapping().unwrap().get_mapping("top").unwrap().clear();
    check_edit("top:\n  b: 2", clear, "top: {}");
    check_edit("top:\n  a: 1\n  b: 2", clear, "top: {}");
    check_edit("top:\n  a: 1\n  b: 2\n", clear, "top: {}\n");
}

#[test]
fn remove_only_item_of_nested_sequence() {
    let remove = |d: &Document| {
        assert!(d
            .as_mapping()
            .unwrap()
            .get_sequence("top")
            .unwrap()
            .remove(0)
            .is_some());
    };
    check_edit("top:\n  - 2", remove, "top: []");
    check_edit("top:\n- 2", remove, "top: []");
    check_edit("top:\n  - 2\n", remove, "top: []\n");
    check_edit("x: 1\ntop:\n  - 2", remove, "x: 1\ntop: []");
}

#[test]
fn clear_nested_sequence() {
    let clear = |d: &Document| d.as_mapping().unwrap().get_sequence("top").unwrap().clear();
    check_edit("top:\n  - 2", clear, "top: []");
    check_edit("top:\n  - 1\n  - 2", clear, "top: []");
    check_edit("top:\n  - 1\n  - 2\n", clear, "top: []\n");
}

#[test]
fn remove_entry_after_blank_line() {
    // A blank line before the removed entry stays, as it does when the
    // entry is not last. Unterminated, it ends the input in place of the
    // removed line, so the line break before it is the last one.
    check_remove("a: 1\n\nb: 2\nc: 3", "a: 1\n\nc: 3");
    check_remove("a: 1\n\nb: 2\n", "a: 1\n\n");
    check_remove("a: 1\n\nb: 2", "a: 1\n");
    check_remove("a: 1\n\n\nb: 2", "a: 1\n\n");
    // With blank lines on both sides, only one gap is kept.
    check_remove("a: 1\n\nb: 2\n\nc: 3", "a: 1\n\nc: 3");
}

#[test]
fn remove_last_item_of_sequence() {
    let top = |d: &Document| {
        assert!(d.as_sequence().unwrap().remove(1).is_some());
    };
    check_edit("- 1\n- 2\n", top, "- 1\n");
    check_edit("- 1\n- 2", top, "- 1");
    check_edit("- 1 # c\n- 2", top, "- 1 # c");
    check_edit("- a:\n    x: 1\n- 2", top, "- a:\n    x: 1");
    // The line break is part of the scalar's value, so it stays.
    check_edit("- |\n  t\n- 2", top, "- |\n  t\n");

    let nested = |d: &Document| {
        assert!(d
            .as_mapping()
            .unwrap()
            .get_sequence("top")
            .unwrap()
            .remove(1)
            .is_some());
    };
    check_edit("top:\n  - 1\n  - 2\n", nested, "top:\n  - 1\n");
    check_edit("top:\n  - 1\n  - 2", nested, "top:\n  - 1");
    check_edit("top:\n- 1\n- 2\n", nested, "top:\n- 1\n");
    check_edit("top:\n  - 1\n  - 2\nx: 1\n", nested, "top:\n  - 1\nx: 1\n");
    check_edit("top:\n  - 1\n\n  - 2\n", nested, "top:\n  - 1\n\n");

    let deeper = |d: &Document| {
        assert!(d
            .as_mapping()
            .unwrap()
            .get_mapping("o")
            .unwrap()
            .get_sequence("top")
            .unwrap()
            .remove(1)
            .is_some());
    };
    check_edit(
        "o:\n  top:\n    - 1\n    - 2\n",
        deeper,
        "o:\n  top:\n    - 1\n",
    );
    check_edit(
        "o:\n  top:\n    - 1\n    - 2\nx: 1\n",
        deeper,
        "o:\n  top:\n    - 1\nx: 1\n",
    );
}
