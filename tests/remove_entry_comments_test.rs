//! Removing an entry takes the comment at the end of its line along, and
//! leaves comment lines before and after it.

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
        debug::shape_to_string(doc.syntax()),
        debug::shape_to_string(reparsed.syntax())
    );
}

fn remove_top(index: usize) -> impl Fn(&Document) {
    move |d| assert!(d.as_sequence().unwrap().remove(index).is_some())
}

fn remove_from_x(index: usize) -> impl Fn(&Document) {
    move |d| {
        let x = d.as_mapping().unwrap().get_sequence("x").unwrap();
        assert!(x.remove(index).is_some());
    }
}

#[test]
fn remove_item_with_comment_on_its_line() {
    check_edit("- x\n- a  # c\n", remove_top(1), "- x\n");
    check_edit("- x\n- a  # c\n- y\n", remove_top(1), "- x\n- y\n");
    check_edit("- a  # c\n- y\n", remove_top(0), "- y\n");
    check_edit(
        "x:\n  - a\n  - b  # c\ny: 1\n",
        remove_from_x(1),
        "x:\n  - a\ny: 1\n",
    );
    check_edit(
        "x:\n  - a  # c\n  - b\ny: 1\n",
        remove_from_x(0),
        "x:\n  - b\ny: 1\n",
    );
}

#[test]
fn remove_item_keeps_comment_lines() {
    check_edit("- x\n# c\n- a\n- y\n", remove_top(1), "- x\n# c\n- y\n");
    check_edit("- x\n- a\n# c\n- y\n", remove_top(1), "- x\n# c\n- y\n");
    check_edit(
        "- x\n# c\n- a  # d\n- y\n",
        remove_top(1),
        "- x\n# c\n- y\n",
    );
    check_edit(
        "- x\n- a  # c\n  # d\n- y\n",
        remove_top(1),
        "- x\n  # d\n- y\n",
    );
    check_edit(
        "x:\n  - a\n  # c\n  - b  # d\n  # e\n  - f\ny: 1\n",
        remove_from_x(1),
        "x:\n  - a\n  # c\n  # e\n  - f\ny: 1\n",
    );
    // The parser leaves the INDENT of a comment line indented past the
    // sequence in it, and the comment in the mapping.
    check_edit(
        "x:\n  - a\n  - b  # d\n    # e\ny: 1\n",
        remove_from_x(1),
        "x:\n  - a\n    # e\ny: 1\n",
    );
    check_edit(
        "x:\n  - b  # d\n    # e\ny: 1\n",
        remove_from_x(0),
        "x: []\n    # e\ny: 1\n",
    );
}

#[test]
fn remove_entry_keeps_comment_lines() {
    let remove = |key: &'static str| {
        move |d: &Document| {
            assert!(d.as_mapping().unwrap().remove(key).is_some());
        }
    };
    check_edit("x: 1\n# c\na: 1\ny: 1\n", remove("a"), "x: 1\n# c\ny: 1\n");
    check_edit("x: 1\na: 1\n# c\ny: 1\n", remove("a"), "x: 1\n# c\ny: 1\n");
    check_edit("x: 1\na: 1  # c\ny: 1\n", remove("a"), "x: 1\ny: 1\n");
}

#[test]
fn remove_explicit_key_entry_with_comment_on_its_line() {
    let remove = |d: &Document| {
        assert!(d.as_mapping().unwrap().remove("b").is_some());
    };
    check_edit("? a\n: 1\n? b\n: 2 # t\nc: 3\n", remove, "? a\n: 1\nc: 3\n");
    check_edit("? a\n: 1\n? b\n: 2 # t\n", remove, "? a\n: 1\n");
    check_edit("? b\n: 2 # t\nc: 3\n", remove, "c: 3\n");
}

#[test]
fn remove_keeps_comment_line_out_of_block_scalar_before() {
    // A comment line indented as far as the body of a block scalar now
    // before it would be read as part of that.
    check_edit(
        "c: |\n  x\nb: 1\n  # t\n",
        |d| assert!(d.as_mapping().unwrap().remove("b").is_some()),
        "c: |\n  x\n# t\n",
    );
    check_edit("- |\n  x\n- 1\n  # t\n", remove_top(1), "- |\n  x\n# t\n");
}

#[test]
fn remove_first_root_entry_before_comment_line() {
    // The comment line stays in the mapping rather than at stream level,
    // where the parser has it; debug::shape_to_string allows for that.
    let source = "a: 1\n# c\nb: 2\n";
    let file = yaml_edit::YamlFile::from_str(source).unwrap();
    let doc = file.documents().next().unwrap();
    assert!(doc.as_mapping().unwrap().remove("a").is_some());
    assert_eq!(file.to_string(), "# c\nb: 2\n");
    assert_eq!(doc.to_string(), "# c\nb: 2\n");
    common::assert_file_cst_ok(&file);
    let reparsed = yaml_edit::YamlFile::from_str("# c\nb: 2\n").unwrap();
    assert_eq!(
        debug::shape_to_string(file.syntax()),
        debug::shape_to_string(reparsed.syntax())
    );
}

#[test]
fn remove_first_root_item_before_comment_line() {
    // As for a root mapping, the comment line stays in the sequence.
    let source = "- a\n# c\n- b\n";
    let file = yaml_edit::YamlFile::from_str(source).unwrap();
    let doc = file.documents().next().unwrap();
    assert!(doc.as_sequence().unwrap().remove(0).is_some());
    assert_eq!(file.to_string(), "# c\n- b\n");
    assert_eq!(doc.to_string(), "# c\n- b\n");
    common::assert_file_cst_ok(&file);
    let reparsed = yaml_edit::YamlFile::from_str("# c\n- b\n").unwrap();
    assert_eq!(
        debug::shape_to_string(file.syntax()),
        debug::shape_to_string(reparsed.syntax())
    );
}
