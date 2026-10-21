//! Adding entries and items to collections and removing them again.
//!
//! When a block collection ends in blank lines, an entry added at its end
//! goes before them if the collection is nested and after them at the
//! root. Adding an entry at the end of input that has no final line break
//! keeps it without one.
//!
//! Removing the first entry or item of a block collection leaves the
//! comment and blank lines after it in front of the collection, where the
//! parser puts them.
//!
//! Removing the last entry of a block mapping leaves the end of the
//! input as it was: with a final line break if the removed entry had one,
//! without one otherwise, and the entries before it whole.
//!
//! Removing an entry takes the comment at the end of its line along, and
//! leaves comment lines before and after it.
//!
//! Removing the last entry of a flow collection drops the separator that
//! the entry before it carried.
//!
//! Removing the last entry or item of a collection leaves an empty
//! collection in its place, written `{}` or `[]` where a block one would
//! be empty text.

mod common;
use common::{
    assert_cst_ok, assert_file_cst_ok, assert_file_edited, assert_file_parser_shaped,
    assert_parser_shaped, check_edit,
};
use std::str::FromStr;
use yaml_edit::{Document, Mapping, Sequence, YamlFile, YamlNode};

fn item(d: &Document) -> YamlNode {
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
        assert_file_edited(&file, expected);
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

#[track_caller]
fn check_remove(source: &str, expected: &str) {
    let doc = Document::from_str(source).unwrap();
    let root = doc.as_mapping().unwrap();
    let mapping = root.get_mapping("top").unwrap_or(root);
    assert!(mapping.remove("b").is_some());
    assert_eq!(doc.to_string(), expected, "removing b from {source:?}");
    assert_parser_shaped(&doc);
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

/// Comment lines between the entries of a collection that is emptied stay
/// before the `{}` or `[]` that takes its place.
#[test]
fn clear_collection_with_comment_lines_between_entries() {
    let clear_top = |d: &Document| d.as_mapping().unwrap().get_mapping("top").unwrap().clear();
    check_edit(
        "top:\n  a: 1\n  # c\n  b: 2\nz: 0\n",
        clear_top,
        "top:\n  # c\n  {}\nz: 0\n",
    );
    let clear_item = |d: &Document| {
        d.as_sequence()
            .unwrap()
            .get(0)
            .unwrap()
            .as_sequence()
            .unwrap()
            .clear()
    };
    check_edit(
        "- - a\n  # c\n  - b\n- z\n",
        clear_item,
        "-\n  # c\n  []\n- z\n",
    );
    let clear_nested = |d: &Document| {
        d.as_sequence()
            .unwrap()
            .get(0)
            .unwrap()
            .as_sequence()
            .unwrap()
            .get(0)
            .unwrap()
            .as_sequence()
            .unwrap()
            .clear()
    };
    check_edit(
        "- - - a\n    # c\n    - b\n",
        clear_nested,
        "- -\n    # c\n    []\n",
    );
}

#[test]
fn clear_mapping_beside_another_node_in_a_value() {
    // The validator rejects `: :`, whose value holds a null and a mapping,
    // but the edit must still be made.
    use rowan::ast::AstNode;
    let doc = Document::from_str(": :\n").unwrap();
    let inner = doc
        .syntax()
        .descendants()
        .filter_map(Mapping::cast)
        .nth(1)
        .unwrap();
    inner.clear();
    assert_eq!(doc.to_string(), ": {}\n");
}

#[test]
fn remove_first_root_entry_of_later_document() {
    // A document without a `---` holds the lines before its root only at
    // the start of the stream; after a `...` they are the stream's.
    for (source, expected) in [
        ("...\nk:\n\nb:\n", "...\n\nb:\n"),
        ("...\nk:\n# c\nb:\n", "...\n# c\nb:\n"),
        ("a\n...\nk:\n\nb:\n", "a\n...\n\nb:\n"),
        ("---\nk:\n\nb:\n", "---\n\nb:\n"),
        ("k:\n\nb:\n", "\nb:\n"),
    ] {
        let file = YamlFile::from_str(source).unwrap();
        let doc = file.documents().last().unwrap();
        assert!(doc.as_mapping().unwrap().remove("k").is_some());
        assert_file_edited(&file, expected);
    }
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
    check_edit("- 1\n- 2  # c\n", top, "- 1\n");
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

#[test]
fn clear_top_level_mapping() {
    let clear = |d: &Document| d.as_mapping().unwrap().clear();
    check_edit("b: 2\n", clear, "{}\n");
    check_edit("b: 2", clear, "{}");
    check_edit("a: 1\nb: 2\n", clear, "{}\n");
    check_edit("a: 1\nb: 2", clear, "{}");
    check_edit("? b\n", clear, "{}\n");
    check_edit("b: |\n  x\n", clear, "{}\n");
    check_edit("b:\n  c: 1\n", clear, "{}\n");
    check_edit("--- b: 2\n", clear, "--- {}\n");
    check_edit("!t\nb: 2\n", clear, "!t\n{}\n");
    check_edit("!t\nb: 2", clear, "!t\n{}");
}

#[test]
fn clear_mapping_in_sequence_item() {
    let clear = |d: &Document| {
        d.as_sequence()
            .unwrap()
            .get(0)
            .unwrap()
            .as_mapping()
            .unwrap()
            .clear()
    };
    check_edit("- b: 2\n- 3\n", clear, "- {}\n- 3\n");
    check_edit("- a: 1\n  b: 2\n- 3\n", clear, "- {}\n- 3\n");
    check_edit("- b: 2\n", clear, "- {}\n");
    check_edit("- b: 2", clear, "- {}");
    check_edit("- &a\n  b: 2\n- 3\n", clear, "- &a\n  {}\n- 3\n");
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
fn remove_last_nested_item_after_comment_lines() {
    // The comment lines now end the sequence, which the parser leaves to
    // the collection holding the next entry.
    check_edit(
        "x:\n  - a\n  # c\n  - b\ny: 1\n",
        remove_from_x(1),
        "x:\n  - a\n  # c\ny: 1\n",
    );
    check_edit(
        "x:\n  - a\n\n  # c\n  # d\n\n  - b\ny: 1\n",
        remove_from_x(1),
        "x:\n  - a\n\n  # c\n  # d\n\ny: 1\n",
    );
    check_edit(
        "m:\n  x:\n    - a\n    # c\n    - b\n  y: 1\n",
        |d| {
            let m = d.as_mapping().unwrap().get_mapping("m").unwrap();
            assert!(m.get_sequence("x").unwrap().remove(1).is_some());
        },
        "m:\n  x:\n    - a\n    # c\n  y: 1\n",
    );
    check_edit(
        "x:\n  - a\n  # c\n  - b\n",
        remove_from_x(1),
        "x:\n  - a\n  # c\n",
    );
    check_edit(
        "x:\n  a: 1\n  # c\n  b: 2\ny: 1\n",
        |d| {
            let x = d.as_mapping().unwrap().get_mapping("x").unwrap();
            assert!(x.remove("b").is_some());
        },
        "x:\n  a: 1\n  # c\ny: 1\n",
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
fn remove_leaves_lines_after_plain_scalar_starting_with_pipe() {
    // `|x` after a flow node, which the validator rejects, is plain text
    // rather than a block scalar header, so the lines after it are left
    // as they were.
    let doc = Document::from_str("- [a]|x\n\n  # t\n- b\n").unwrap();
    remove_top(1)(&doc);
    assert_eq!(doc.to_string(), "- [a]|x\n\n  # t\n");
}

#[test]
fn remove_first_root_entry_before_comment_line() {
    // The comment line stays in the mapping rather than at stream level,
    // where the parser has it; debug::shape_to_string allows for that.
    let source = "a: 1\n# c\nb: 2\n";
    let file = YamlFile::from_str(source).unwrap();
    let doc = file.documents().next().unwrap();
    assert!(doc.as_mapping().unwrap().remove("a").is_some());
    assert_eq!(file.to_string(), "# c\nb: 2\n");
    assert_eq!(doc.to_string(), "# c\nb: 2\n");
    assert_file_parser_shaped(&file);
}

#[test]
fn remove_first_root_item_before_comment_line() {
    // As for a root mapping, the comment line stays in the sequence.
    let source = "- a\n# c\n- b\n";
    let file = YamlFile::from_str(source).unwrap();
    let doc = file.documents().next().unwrap();
    assert!(doc.as_sequence().unwrap().remove(0).is_some());
    assert_eq!(file.to_string(), "# c\n- b\n");
    assert_eq!(doc.to_string(), "# c\n- b\n");
    assert_file_parser_shaped(&file);
}

/// Remove `key` from the mapping at `path` of `yaml` and return the text.
#[track_caller]
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
fn remove_entry_next_to_blank_lines() {
    for (yaml, path, key, expected) in [
        ("a: 1\n\nb: 2\n\nc: 3\n", &[][..], "b", "a: 1\n\nc: 3\n"),
        (
            "x:\n  a: 1\n\n  b: 2\n\n  c: 3\ny: 1\n",
            &["x"][..],
            "b",
            "x:\n  a: 1\n\n  c: 3\ny: 1\n",
        ),
        (
            "x:\n\n  b: 1\n\n  c: 2\ny: 1\n",
            &["x"],
            "b",
            "x:\n\n  c: 2\ny: 1\n",
        ),
        (
            "x: &a\n\n  b: 1\n\n  c: 2\n",
            &["x"],
            "b",
            "x: &a\n\n  c: 2\n",
        ),
        // Blank lines on one side only stay.
        ("x:\n\n  b: 1\n  c: 2\n", &["x"], "b", "x:\n\n  c: 2\n"),
        ("a: 1\n\nb: 2\nc: 3\n", &[], "b", "a: 1\n\nc: 3\n"),
        ("a: 1\nb: 2\n\nc: 3\n", &[], "b", "a: 1\n\nc: 3\n"),
        // A blank line after the first root entry keeps its spaces.
        ("a: 1\n \t\nb: 2\n", &[], "a", " \t\nb: 2\n"),
        ("a: 1\n    \nb: 2\n", &[], "a", "    \nb: 2\n"),
        (
            "? a\n: 1\n\n? b\n: 2\n\n? c\n: 3\n",
            &[],
            "b",
            "? a\n: 1\n\n? c\n: 3\n",
        ),
        (
            "? a\n: 1\n\n? b\n: 2\n? c\n: 3\n",
            &[],
            "b",
            "? a\n: 1\n\n? c\n: 3\n",
        ),
        // An explicit key entry takes its line break along.
        (
            "? a\n: 1\n? b\n: 2\n? c\n: 3\n",
            &[],
            "b",
            "? a\n: 1\n? c\n: 3\n",
        ),
        ("a: 1\n# c\nb: 2\n\nc: 3\n", &[], "b", "a: 1\n# c\n\nc: 3\n"),
        (
            "a: 1\n# c\n\nb: 2\n\nc: 3\n",
            &[],
            "b",
            "a: 1\n# c\n\nc: 3\n",
        ),
        // The blank lines after a block value are the mapping's, as after a
        // scalar one.
        ("a: 1\nk:\n  x: 1\n\nz: 2\n", &[], "k", "a: 1\n\nz: 2\n"),
        ("a: 1\nk: 1\n\nz: 2\n", &[], "k", "a: 1\n\nz: 2\n"),
    ] {
        assert_eq!(remove(yaml, path, key), expected, "{yaml:?}");
    }
    let reparsed = YamlFile::from_str("a: 1\n\nc: 3\n").unwrap();
    let keys: Vec<String> = reparsed
        .document()
        .unwrap()
        .as_mapping()
        .unwrap()
        .keys()
        .map(|k| k.to_string())
        .collect();
    assert_eq!(keys, ["a", "c"]);
    check_edit(
        "-\n\n  b: 1\n\n  c: 2\n",
        |d| {
            let m = d.as_sequence().unwrap().get(0).unwrap();
            assert!(m.as_mapping().unwrap().remove("b").is_some());
        },
        "-\n\n  c: 2\n",
    );
}

/// Remove item `index` of the sequence at `path` (a mapping path ending
/// in the sequence's key, or the root sequence when empty) of `yaml` and
/// return the text.
#[track_caller]
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
fn remove_item_next_to_blank_lines() {
    for (yaml, path, expected) in [
        ("- a\n\n- b\n\n- c\n", &[][..], "- a\n\n- c\n"),
        (
            "x:\n  - a\n\n  - b\n\n  - c\ny: 1\n",
            &["x"][..],
            "x:\n  - a\n\n  - c\ny: 1\n",
        ),
        ("- a\n\n- b\n- c\n", &[], "- a\n\n- c\n"),
        ("- a\n- b\n\n- c\n", &[], "- a\n\n- c\n"),
        ("- a\n# c\n- b\n\n- c\n", &[], "- a\n# c\n\n- c\n"),
        ("- a\n# c\n\n- b\n\n- c\n", &[], "- a\n# c\n\n- c\n"),
        ("- a  # k\n\n- b\n\n- c\n", &[], "- a  # k\n\n- c\n"),
    ] {
        assert_eq!(remove_item(yaml, path, 1), expected, "{yaml:?}");
    }
}

#[test]
fn remove_last_flow_entry() {
    let remove_key = |key: &'static str| {
        move |d: &Document| {
            d.as_mapping().unwrap().remove(key).unwrap();
        }
    };
    check_edit(
        "{a: 1, b: 2, c: 3}",
        |d| {
            remove_key("c")(d);
            assert_eq!(d.as_mapping().unwrap().len(), 2);
        },
        "{a: 1, b: 2}",
    );
    check_edit("{a: 1, b: 2,}", remove_key("b"), "{a: 1,}");
    check_edit("{a: 1,\n  b: 2}\n", remove_key("b"), "{a: 1}\n");
    check_edit(
        "[a, b, c]",
        |d| {
            let seq = d.as_sequence().unwrap();
            seq.remove(2).unwrap();
            assert_eq!(seq.len(), 2);
        },
        "[a, b]",
    );
    // The entry before takes over the line break, where the parser puts it.
    check_edit("{r, z,\n\n}", remove_key("z"), "{r,\n\n}");
    check_edit("{r, z: 1,\n}", remove_key("z"), "{r,\n}");
}

const MAPPING_APPENDS: &[(&str, &str)] = &[
    ("{a: 1, b: 2,}", "{a: 1, b: 2, c: 9,}"),
    ("{a: 1, b: 2, }\n", "{a: 1, b: 2, c: 9, }\n"),
    ("{a: 1,}", "{a: 1, c: 9,}"),
    ("{ a: 1, b: 2 }", "{ a: 1, b: 2, c: 9 }"),
    ("{a: 1,\n  b: 2,\n}\n", "{a: 1,\n  b: 2,\n  c: 9,\n}\n"),
    ("{a: 1, # x\n b: 2,}", "{a: 1, # x\n b: 2, c: 9,}"),
    ("{a: 1, b: 2, # x\n}", "{a: 1, b: 2, # x\nc: 9,}"),
    (
        "top: {a: 1, b: 2, # x\n  }\n",
        "top: {a: 1, b: 2, # x\n  c: 9,}\n",
    ),
];

type Insertion = dyn Fn(&Mapping);

fn flow_mapping(file: &YamlFile) -> Mapping {
    let root = file.document().unwrap().as_mapping().unwrap();
    root.get_mapping("top").unwrap_or(root)
}

#[test]
fn append_to_flow_mapping_keeps_trailing_comma() {
    for &(source, expected) in MAPPING_APPENDS {
        let insertions: [(&str, &Insertion); 5] = [
            ("set", &|m| m.set("c", 9)),
            ("insert_after", &|m| {
                assert!(m.insert_after("b", "c", 9) || m.insert_after("a", "c", 9))
            }),
            ("insert_at_index", &|m| m.insert_at_index(2, "c", 9)),
            ("move_after", &|m| {
                let last = m.keys().last().unwrap();
                assert!(m.move_after(last, "c", 9));
            }),
            ("set_with_field_order", &|m| {
                m.set_with_field_order("c", 9, ["a", "b", "c"])
            }),
        ];
        for (name, insert) in insertions {
            let file = YamlFile::from_str(source).unwrap();
            insert(&flow_mapping(&file));
            assert_eq!(file.to_string(), expected, "{name} on {source:?}");
            assert_file_edited(&file, expected);
        }
    }
}

#[test]
fn append_to_flow_sequence_keeps_trailing_comma() {
    for (source, expected) in [
        ("[1, 2,]", "[1, 2, 9,]"),
        ("[1, 2, ]\n", "[1, 2, 9, ]\n"),
        ("[1,]", "[1, 9,]"),
        ("[ 1, 2 ]", "[ 1, 2, 9 ]"),
        ("[1,\n  2,\n]\n", "[1,\n  2,\n  9,\n]\n"),
        ("[1, # x\n 2,]", "[1, # x\n 2, 9,]"),
        ("[1, 2, # x\n]", "[1, 2, # x\n9,]"),
        ("[1, 2 # x\n]", "[1, 2 # x\n, 9]"),
    ] {
        for push in [true, false] {
            let file = YamlFile::from_str(source).unwrap();
            let seq = file.document().unwrap().as_sequence().unwrap();
            if push {
                seq.push(9);
            } else {
                seq.insert(2, 9);
            }
            assert_eq!(file.to_string(), expected, "push: {push}");
            assert_file_edited(&file, expected);
        }
    }
}

#[test]
fn insert_before_flow_sequence_item() {
    for (source, index, expected) in [
        ("[1, 2]", 0, "[9, 1, 2]"),
        ("[1, 2]", 1, "[1, 9, 2]"),
        ("[1, 2,]", 1, "[1, 9, 2,]"),
        ("[1, # x\n 2]", 1, "[1, # x\n 9, 2]"),
        ("[a: 1, b]", 0, "[9, a: 1, b]"),
    ] {
        let file = YamlFile::from_str(source).unwrap();
        let seq = file.document().unwrap().as_sequence().unwrap();
        seq.insert(index, 9);
        assert_eq!(
            file.to_string(),
            expected,
            "insert at {index} in {source:?}"
        );
        assert_file_edited(&file, expected);
    }
}

#[test]
fn insert_into_flow_mapping_after_comment() {
    let source = "{a: 1, # x\n b: 2}\n";
    let expected = "{a: 1, # x\n c: 9, b: 2}\n";
    let file = YamlFile::from_str(source).unwrap();
    flow_mapping(&file).insert_at_index(1, "c", 9);
    assert_eq!(file.to_string(), expected);
    assert_file_edited(&file, expected);
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
        "top:\n  c: 3\nx: 1\n",
    );
    check_edit(
        "top:\n  - 1\n",
        |d| {
            let s = d.as_mapping().unwrap().get_sequence("top").unwrap();
            s.remove(0);
            s.push(3);
        },
        "top:\n  - 3\n",
    );
    check_edit(
        "top:\n  # c\n  b: 2\n",
        |d| {
            let m = d.as_mapping().unwrap().get_mapping("top").unwrap();
            m.remove("b");
            m.set("c", 3);
        },
        "top:\n  # c\n  c: 3\n",
    );
}

fn item_mapping(d: &Document) -> Mapping {
    d.as_sequence()
        .unwrap()
        .get(0)
        .unwrap()
        .as_mapping()
        .cloned()
        .unwrap()
}

fn item_sequence(d: &Document) -> Sequence {
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

/// Refilling a drained root collection takes the lines after it back from
/// the document, up to any `...`.
#[test]
fn refill_drained_root_collection_takes_lines_after_it() {
    let refill_mapping = |d: &Document| {
        let m = d.as_mapping().unwrap();
        m.remove("a");
        m.set("b", 2);
    };
    check_edit("a: 1\n# c\n", refill_mapping, "b: 2\n# c\n");
    check_edit("a: 1\n# c", refill_mapping, "b: 2\n# c");
    check_edit("!!map\na: 1\n# c\n", refill_mapping, "!!map\nb: 2\n# c\n");
    check_edit(
        "a: 1\n# c\n  # d\n\n...\n",
        refill_mapping,
        "b: 2\n# c\n  # d\n\n...\n",
    );
    check_edit(
        "- 1\n# c\n",
        |d| {
            let s = d.as_sequence().unwrap();
            s.remove(0);
            s.push(2);
        },
        "- 2\n# c\n",
    );
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
        "- c: 3\n- 3\n",
    );
    check_edit(
        "- - 1\n- 3\n",
        |d| {
            let s = item_sequence(d);
            s.remove(0);
            s.push(2);
        },
        "- - 2\n- 3\n",
    );
}

fn key_node(d: &Document) -> YamlNode {
    d.as_mapping().unwrap().keys().next().unwrap()
}

fn key_mapping(d: &Document) -> Mapping {
    let key = key_node(d);
    match key.as_tagged() {
        Some(t) => t.as_mapping().unwrap(),
        None => key.as_mapping().cloned().unwrap(),
    }
}

fn key_sequence(d: &Document) -> Sequence {
    let key = key_node(d);
    match key.as_tagged() {
        Some(t) => t.as_sequence().unwrap(),
        None => key.as_sequence().cloned().unwrap(),
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

#[test]
fn drain_sequence_item_keeps_comment_lines_after_it() {
    let remove_entry = |d: &Document| {
        assert!(item_mapping(d).remove("b").is_some());
    };
    check_edit("- b: 2\n  # c\n- 3\n", remove_entry, "- {}\n  # c\n- 3\n");
    check_edit("- b: 2\n  # c\n", remove_entry, "- {}\n  # c\n");
    check_edit("- b: 2\n  # c", remove_entry, "- {}\n  # c");
    check_edit(
        "- b: 2\n  # c\n\n  # d\n- 3\n",
        remove_entry,
        "- {}\n  # c\n\n  # d\n- 3\n",
    );
    check_edit(
        "- a: 1\n  b: 2\n  # c\n- 3\n",
        |d| item_mapping(d).clear(),
        "- {}\n  # c\n- 3\n",
    );
    let remove_item = |d: &Document| {
        assert!(item_sequence(d).remove(0).is_some());
    };
    check_edit("- - 1\n  # c\n- 3\n", remove_item, "- []\n  # c\n- 3\n");
    check_edit("- - 1\n  # c\n", remove_item, "- []\n  # c\n");
    check_edit(
        "- &a\n  b: 2\n  # c\n- 3\n",
        remove_entry,
        "- &a\n  {}\n  # c\n- 3\n",
    );
    check_edit(
        "x:\n  - - 2\n    # c\ny: 1\n",
        |d| {
            let s = d.as_mapping().unwrap().get_sequence("x").unwrap();
            s.get(0).unwrap().as_sequence().unwrap().remove(0);
        },
        "x:\n  - []\n    # c\ny: 1\n",
    );
    check_edit(
        "- - 1\n  - 2\n  # c\n- 3\n",
        |d| item_sequence(d).clear(),
        "- []\n  # c\n- 3\n",
    );
}

#[test]
fn drain_mapping_value_keeps_comment_line_indent() {
    let remove = |d: &Document| {
        let v = d.as_mapping().unwrap().get("top").unwrap();
        match v.as_mapping() {
            Some(m) => assert!(m.remove("b").is_some()),
            None => assert!(v.as_sequence().unwrap().remove(0).is_some()),
        }
    };
    check_edit(
        "top:\n  b: 2\n  # c\nx: 1\n",
        remove,
        "top: {}\n  # c\nx: 1\n",
    );
    check_edit(
        "top:\n  - 2\n  # c\nx: 1\n",
        remove,
        "top: []\n  # c\nx: 1\n",
    );
    check_edit(
        "top:\n  # a\n  b: 2\n  # c\nx: 1\n",
        remove,
        "top:\n  # a\n  {}\n  # c\nx: 1\n",
    );
    check_edit(
        "a:\n  top:\n    b: 2\n    # c\nx: 1\n",
        |d| {
            let a = d.as_mapping().unwrap().get_mapping("a").unwrap();
            a.get_mapping("top").unwrap().remove("b");
        },
        "a:\n  top: {}\n    # c\nx: 1\n",
    );
}

#[test]
fn drain_removed_sequence_item_with_comment_line() {
    let doc = Document::from_str("- b: 2\n  # c\n").unwrap();
    let item = doc.as_sequence().unwrap().remove(0).unwrap();
    let m = item.as_mapping().unwrap();
    assert!(m.remove("b").is_some());
    assert_eq!(m.to_string(), "{}");
}

#[test]
fn drain_compact_sequence_in_explicit_key() {
    let remove = |d: &Document| {
        assert!(key_sequence(d).remove(0).is_some());
    };
    let clear = |d: &Document| key_sequence(d).clear();
    for edit in [&remove as &dyn Fn(&Document), &clear] {
        check_edit("?\n- a\n: b\n", edit, "?\n  []\n: b\n");
        check_edit("?\n- a\n", edit, "?\n  []\n");
        check_edit("? &x\n- a\n: b\n", edit, "? &x\n  []\n: b\n");
        check_edit("? !t\n- a\n: b\n", edit, "? !t\n  []\n: b\n");
        check_edit("? # c\n- a\n: b\n", edit, "? # c\n  []\n: b\n");
    }
    check_edit(
        "x:\n  ?\n  - a\n  : b\n",
        |d| {
            let m = d.as_mapping().unwrap().get_mapping("x").unwrap();
            let k = m.keys().next().unwrap();
            k.as_sequence().unwrap().remove(0);
        },
        "x:\n  ?\n    []\n  : b\n",
    );
}

#[test]
fn remove_later_item_of_compact_sequence_in_explicit_key() {
    // The blank lines before a compact sequence in an explicit key are the
    // key's; they stay there.
    let pop = |d: &Document| {
        assert!(key_sequence(d).pop().is_some());
    };
    check_edit("?\n\n\n- a\n- b\n", pop, "?\n\n\n- a\n");
    check_edit("?\r\r\r- a\n-\r", pop, "?\r\r\r- a\n");
    check_edit("?\n\n- a\n- b\n- c\n: v\n", pop, "?\n\n- a\n- b\n: v\n");
}

#[test]
fn pop_unterminated_item_after_lines_in_mapping_value() {
    // The lines before the removed item leave the sequence, so it is the
    // last of them that loses its line break. Taking the item's own left
    // the blank line after the sequence, where the parser would not, and
    // joined a comment line to the item.
    let pop = |d: &Document| {
        let v = d.as_mapping().unwrap().values().next().unwrap();
        assert!(v.as_sequence().unwrap().pop().is_some());
    };
    check_edit(":\n -\n\n -", pop, ":\n -\n");
    check_edit(":\r -\r\r -", pop, ":\r -\r");
    check_edit("a:\n - x\n # c\n - y", pop, "a:\n - x\n # c");
}

#[test]
fn drain_compact_sequence_value_under_comment_line() {
    // A flow collection on a line of its own has to be indented past the
    // key, as a block sequence does not.
    let value_sequence = |d: &Document| {
        let m = d.as_mapping().unwrap();
        let v = m.values().next().unwrap();
        v.as_sequence().cloned().unwrap()
    };
    let pop = |d: &Document| {
        assert!(value_sequence(d).pop().is_some());
    };
    let clear = |d: &Document| value_sequence(d).clear();
    for edit in [&pop as &dyn Fn(&Document), &clear] {
        check_edit("a:\n# c\n- x\n", edit, "a:\n# c\n  []\n");
        check_edit("a:\n#\n \n- e: 1\n", edit, "a:\n#\n \n  []\n");
        check_edit("? a\n:\n# c\n- x\n", edit, "? a\n:\n# c\n  []\n");
        check_edit("a:\n# c\n- x\nb: 1\n", edit, "a:\n# c\n  []\nb: 1\n");
    }
    check_edit("a:\n- x\n# c\n- y\n", clear, "a:\n# c\n  []\n");
    check_edit(
        "x:\n  a:\n  # c\n  - x\n",
        |d| {
            let m = d.as_mapping().unwrap().get_mapping("x").unwrap();
            m.get_sequence("a").unwrap().pop();
        },
        "x:\n  a:\n  # c\n    []\n",
    );
}

#[test]
fn drain_indented_collection_in_explicit_key() {
    check_edit(
        "?\n  a: 1\n: b\n",
        |d| {
            assert!(key_mapping(d).remove("a").is_some());
        },
        "?\n  {}\n: b\n",
    );
    check_edit(
        "?\n  - a\n: b\n",
        |d| {
            assert!(key_sequence(d).remove(0).is_some());
        },
        "?\n  []\n: b\n",
    );
}

#[test]
fn drain_value_of_explicit_key_entry() {
    check_edit(
        "? a\n:\n  - x\nb: 1\n",
        |d| {
            let s = d.as_mapping().unwrap().get_sequence("a").unwrap();
            s.remove(0);
        },
        "? a\n: []\nb: 1\n",
    );
}

#[test]
fn drain_sequence_whose_last_item_ends_in_a_comment() {
    let clear = |d: &Document| d.as_mapping().unwrap().get_sequence("a").unwrap().clear();
    let remove = |d: &Document| {
        d.as_mapping()
            .unwrap()
            .get_sequence("a")
            .unwrap()
            .remove(0)
            .unwrap();
    };
    check_edit("? a\n:\n  - x  # c\nb: 1\n", clear, "? a\n: []\nb: 1\n");
    check_edit("? a\n:\n  - x  # c\nb: 1\n", remove, "? a\n: []\nb: 1\n");
    check_edit("a:\n  - x  # c\nb: 1\n", clear, "a: []\nb: 1\n");
    check_edit(
        "- - x  # c\n- 1\n",
        |d| item_sequence(d).clear(),
        "- []\n- 1\n",
    );
}

#[test]
fn remove_last_entry_of_top_level_collection() {
    let remove = |d: &Document| {
        d.as_mapping().unwrap().remove("a").unwrap();
    };
    check_edit("a: 1\n", remove, "{}\n");
    check_edit("a: 1", remove, "{}");
    check_edit("---\na: 1\n", remove, "---\n{}\n");
    check_edit(
        "- 1\n",
        |d| {
            d.as_sequence().unwrap().remove(0).unwrap();
        },
        "[]\n",
    );
}

/// A block collection drained by `remove` or `clear` is written `{}` or
/// `[]`, but goes back to block style when it gets an entry again.
#[test]
fn refill_drained_top_level_collection() {
    let top = |d: &Document| d.as_mapping().unwrap();
    let seq = |d: &Document| d.as_sequence().unwrap();
    check_edit(
        "a: 1\n",
        |d| {
            top(d).remove("a");
            top(d).set("c", 3);
        },
        "c: 3\n",
    );
    check_edit(
        "a: 1\nb: 2\n",
        |d| {
            top(d).clear();
            top(d).set("c", 3);
        },
        "c: 3\n",
    );
    check_edit(
        "---\na: 1\n",
        |d| {
            top(d).remove("a");
            top(d).set("c", 3);
        },
        "---\nc: 3\n",
    );
    check_edit(
        "- 1\n",
        |d| {
            seq(d).remove(0);
            seq(d).push(3);
        },
        "- 3\n",
    );
    check_edit(
        "- 1\n- 2\n",
        |d| {
            seq(d).clear();
            seq(d).insert(0, 3);
        },
        "- 3\n",
    );
}

/// The comment and blank lines after a block root collection are part of
/// it, so they go back into it when it is refilled.
#[test]
fn refill_drained_top_level_collection_with_lines_after_it() {
    let top = |d: &Document| d.as_mapping().unwrap();
    let seq = |d: &Document| d.as_sequence().unwrap();
    check_edit(
        "- a\n# d\n",
        |d| {
            seq(d).clear();
            seq(d).push("x");
        },
        "- x\n# d\n",
    );
    check_edit(
        "a: 1\n# d\n\n# e\n",
        |d| {
            top(d).remove("a");
            top(d).set("x", 1);
        },
        "x: 1\n# d\n\n# e\n",
    );
    check_edit(
        "- a\n  # d\n...\n",
        |d| {
            seq(d).clear();
            seq(d).insert(0, "x");
        },
        "- x\n  # d\n...\n",
    );
}

/// The line break after a drained collection is the document's own.
#[test]
fn drained_collection_keeps_crlf_line_breaks() {
    let top = |d: &Document| d.as_mapping().unwrap();
    let seq = |d: &Document| d.as_sequence().unwrap();
    check_edit("a: 1\r\nb: 2\r\n", |d| top(d).clear(), "{}\r\n");
    check_edit("- a\r\n# c\r\n", |d| seq(d).clear(), "[]\r\n# c\r\n");
    check_edit(
        "k:\r\n  - 1\r\n",
        |d| top(d).get_sequence("k").unwrap().clear(),
        "k: []\r\n",
    );
    check_edit(
        "- - 1\r\n",
        |d| seq(d).get(0).unwrap().as_sequence().unwrap().clear(),
        "- []\r\n",
    );
    check_edit(
        "? - 1\r\n",
        |d| {
            let key = top(d).keys().next().unwrap();
            key.as_sequence().unwrap().clear();
        },
        "? []\r\n",
    );
}

#[test]
fn refill_drained_mapping_value() {
    let refill = |d: &Document| {
        let m = d.as_mapping().unwrap().get_mapping("k").unwrap();
        m.clear();
        m.set("c", 3);
    };
    check_edit("k:\n  a: 1\n", refill, "k:\n  c: 3\n");
    check_edit("k:\n  a: 1\nz: 2\n", refill, "k:\n  c: 3\nz: 2\n");
    check_edit("k: !t\n  a: 1\n", refill, "k: !t\n  c: 3\n");
    check_edit("k: &x\n  a: 1\n", refill, "k: &x\n  c: 3\n");
    check_edit("k: # c\n  a: 1\n", refill, "k: # c\n  c: 3\n");
    check_edit("k: !t # c\n  a: 1\n", refill, "k: !t # c\n  c: 3\n");
    check_edit(
        "k:\n  a: 1\n  # c\nz: 2\n",
        refill,
        "k:\n  c: 3\n  # c\nz: 2\n",
    );
    check_edit(
        "k:\n  - 1\nz: 2\n",
        |d| {
            let s = d.as_mapping().unwrap().get_sequence("k").unwrap();
            s.remove(0);
            s.push(3);
        },
        "k:\n  - 3\nz: 2\n",
    );
    check_edit(
        "? a\n:\n  - x\nb: 1\n",
        |d| {
            let s = d.as_mapping().unwrap().get_sequence("a").unwrap();
            s.remove(0);
            s.push("y");
        },
        "? a\n:\n  - y\nb: 1\n",
    );
}

#[test]
fn refill_drained_sequence_item() {
    check_edit(
        "- &a\n  b: 2\n- 3\n",
        |d| {
            let m = item_mapping(d);
            m.remove("b");
            m.set("c", 3);
        },
        "- &a\n  c: 3\n- 3\n",
    );
    check_edit(
        "- - 1\n  - 2\n- 3\n",
        |d| {
            let s = item_sequence(d);
            s.clear();
            s.push(4);
            s.push(5);
        },
        "- - 4\n  - 5\n- 3\n",
    );
    check_edit(
        "- b: 2\n  # c\n- 3\n",
        |d| {
            let m = item_mapping(d);
            m.remove("b");
            m.set("c", 3);
        },
        "- c: 3\n  # c\n- 3\n",
    );
}

#[test]
fn refill_drained_explicit_key() {
    check_edit(
        "? a: 1\n: v\n",
        |d| {
            let m = key_mapping(d);
            m.remove("a");
            m.set("c", 3);
        },
        "? c: 3\n: v\n",
    );
    check_edit(
        "? - 1\n: v\n",
        |d| {
            let s = key_sequence(d);
            s.remove(0);
            s.push(3);
            s.push(4);
        },
        "? - 3\n  - 4\n: v\n",
    );
}

/// Only a collection that was block style goes back to it: a `{}` or `[]`
/// written in the source, or a drained flow collection, stays flow.
#[test]
fn refill_flow_collection_stays_flow() {
    check_edit("{}\n", |d| d.as_mapping().unwrap().set("c", 3), "{c: 3}\n");
    check_edit(
        "a: []\n",
        |d| d.as_mapping().unwrap().get_sequence("a").unwrap().push(3),
        "a: [3]\n",
    );
    check_edit("- {}\n", |d| item_mapping(d).set("c", 3), "- {c: 3}\n");
    check_edit(
        "a: {x: 1}\n",
        |d| {
            let m = d.as_mapping().unwrap().get_mapping("a").unwrap();
            m.remove("x");
            m.set("c", 3);
        },
        "a: {c: 3}\n",
    );
}

#[test]
fn remove_first_item_of_compact_sequence_after_blank_line() {
    let remove = |d: &Document| {
        assert!(key_sequence(d).remove(0).is_some());
    };
    check_edit("?\n\n- a\n- b\n", remove, "?\n\n- b\n");
    check_edit("?\n# c\n- a\n- b\n: v\n", remove, "?\n# c\n- b\n: v\n");
    check_edit("? !t\n\n- a\n- b\n", remove, "? !t\n\n- b\n");
    check_edit("?\n\n  - a\n  - b\n", remove, "?\n\n  - b\n");
    let value_sequence = |d: &Document| {
        let m = d.as_mapping().unwrap();
        let v = m.values().next().unwrap();
        v.as_sequence().cloned().unwrap()
    };
    check_edit(
        "? x\n:\n\n- a\n- b\n",
        |d| assert!(value_sequence(d).remove(0).is_some()),
        "? x\n:\n\n- b\n",
    );
}

#[test]
fn remove_anchored_entry_moves_anchor_to_alias() {
    let file = YamlFile::from_str("orig: &x value\nref: *x").unwrap();
    let mapping = file.document().unwrap().as_mapping().unwrap();
    mapping.remove("orig");
    assert_file_edited(&file, "ref: &x value");
    assert_eq!(
        mapping.get("ref").unwrap().as_scalar().unwrap().as_string(),
        "value"
    );
}

#[test]
fn remove_every_entry_of_nested_mapping() {
    let file = YamlFile::from_str("config:\n  a: 1\n  b: 2\n  c: 3").unwrap();
    let mapping = file.document().unwrap().as_mapping().unwrap();
    let config = mapping.get_mapping("config").unwrap();
    assert_eq!(config.keys().count(), 3);
    for key in ["a", "b", "c"] {
        config.remove(key);
        assert_file_cst_ok(&file);
    }
    assert_file_edited(&file, "config: {}");
    assert_eq!(config.keys().count(), 0);
}

#[test]
fn push_to_empty_flow_sequence() {
    let file = YamlFile::from_str("items: []").unwrap();
    let mapping = file.document().unwrap().as_mapping().unwrap();
    let items = mapping.get_sequence("items").unwrap();
    assert_eq!(items.len(), 0);
    for item in ["first", "second", "third"] {
        items.push(item);
        assert_file_cst_ok(&file);
    }
    assert_file_edited(&file, "items: [first, second, third]");
    let values: Vec<String> = items
        .values()
        .map(|v| v.as_scalar().unwrap().as_string())
        .collect();
    assert_eq!(values, ["first", "second", "third"]);
}

/// Parse `source`, apply `f` to it and check the result is parser-shaped.
fn edit_file(source: &str, f: impl FnOnce(&YamlFile)) -> String {
    let file = YamlFile::from_str(source).unwrap();
    f(&file);
    assert_file_parser_shaped(&file);
    file.to_string()
}

fn root_mapping(file: &YamlFile) -> Mapping {
    file.document().unwrap().as_mapping().unwrap()
}

fn root_sequence(file: &YamlFile) -> Sequence {
    file.document().unwrap().as_sequence().unwrap()
}

/// When a collection ends in blank lines, an entry added at its end goes
/// before them if the collection is nested and after them at the root.
#[test]
fn append_next_to_blank_lines_at_end() {
    assert_eq!(
        edit_file("a:\n  x: 1\n\nb: 2\n", |f| root_mapping(f)
            .get_mapping("a")
            .unwrap()
            .set("y", 3)),
        "a:\n  x: 1\n  y: 3\n\nb: 2\n"
    );
    assert_eq!(
        edit_file("a:\n  - x\n\nb: 2\n", |f| root_mapping(f)
            .get_sequence("a")
            .unwrap()
            .push("y")),
        "a:\n  - x\n  - y\n\nb: 2\n"
    );
    assert_eq!(
        edit_file("a:\n  x: 1\n  # c\n\n", |f| root_mapping(f)
            .get_mapping("a")
            .unwrap()
            .set("y", 3)),
        "a:\n  x: 1\n  y: 3\n  # c\n\n"
    );
    assert_eq!(
        edit_file("x: 1\n\n", |f| root_mapping(f).set("y", 2)),
        "x: 1\n\ny: 2\n"
    );
    assert_eq!(
        edit_file("- x\n\n", |f| root_sequence(f).push("y")),
        "- x\n\n- y\n"
    );
    assert_eq!(
        edit_file("- x\n\n", |f| root_sequence(f).insert(1, "y")),
        "- x\n\n- y\n"
    );
    assert_eq!(
        edit_file("- x\n\n# c\n\n", |f| root_sequence(f).push("y")),
        "- x\n\n# c\n\n- y\n"
    );
}

/// Adding an entry at the end of input that has no final line break keeps
/// it without one, whichever method adds it.
#[test]
fn append_keeps_unterminated_end() {
    let root = |d: &Document| d.as_mapping().unwrap();
    for source in ["a: 1", "? a\n: 1"] {
        let entry = if source.starts_with('?') {
            "? b\n: 2"
        } else {
            "b: 2"
        };
        let expected = format!("{source}\n{entry}");
        check_edit(source, |d| root(d).set("b", 2), &expected);
        check_edit(
            source,
            |d| {
                root(d).insert_after("a", "b", 2);
            },
            &expected,
        );
        check_edit(source, |d| root(d).insert_at_index(1, "b", 2), &expected);
        check_edit(
            source,
            |d| root(d).set_with_field_order("b", 2, ["a", "b"]),
            &expected,
        );
    }
    check_edit("a: 1\n# c", |d| root(d).set("b", 2), "a: 1\n# c\nb: 2");
    check_edit("a: |\n  x", |d| root(d).set("b", 2), "a: |\n  x\nb: 2");
    check_edit(
        "a:\n  x: 1",
        |d| root(d).get_mapping("a").unwrap().set("y", 2),
        "a:\n  x: 1\n  y: 2",
    );
    check_edit("a:\n  x: 1", |d| root(d).set("b", 2), "a:\n  x: 1\nb: 2");

    let seq = |d: &Document| d.as_sequence().unwrap();
    check_edit("- a", |d| seq(d).push("b"), "- a\n- b");
    check_edit("- a", |d| seq(d).insert(1, "b"), "- a\n- b");
    check_edit("- a", |d| seq(d).insert(5, "b"), "- a\n- b");
    check_edit(
        "k:\n  - a",
        |d| root(d).get_sequence("k").unwrap().insert(1, "b"),
        "k:\n  - a\n  - b",
    );
}

/// Apply `f` to the mapping under `x` in `src` and return the text.
fn edit_x(src: &str, f: impl FnOnce(&Mapping)) -> String {
    edit_file(src, |file| f(&root_mapping(file).get_mapping("x").unwrap()))
}

fn keys(mapping: &Mapping) -> Vec<String> {
    mapping.keys().map(|k| k.to_string()).collect()
}

/// Appending to an indented mapping whose last line is a comment: the
/// parser leaves that comment line's INDENT at the end of the nested
/// mapping and lifts the comment itself to the parent. A comment indented
/// deeper than the mapping's keys leaves an INDENT of its own width there,
/// which the new key must not take.
#[test]
fn append_before_trailing_comment_of_nested_mapping() {
    let set_z = |x: &Mapping| x.set("z", 0);
    let insert_z = |x: &Mapping| x.insert_at_index(5, "z", 0);
    let set_k = |x: &Mapping| x.set("k", "v");
    let insert_k = |x: &Mapping| x.insert_at_index(5, "k", "v");
    let deeper = "x:\n  a: 1\n    # c\ny: 1\n";
    for (src, edit, expected, x_keys) in [
        (
            "x:\n  a: 1\n  # end\ny: 1\n",
            &set_z as &dyn Fn(&Mapping),
            "x:\n  a: 1\n  z: 0\n  # end\ny: 1\n",
            &["a", "z"][..],
        ),
        (
            "x:\n  a: 1\n  # end\ny: 1\n",
            &insert_z,
            "x:\n  a: 1\n  z: 0\n  # end\ny: 1\n",
            &["a", "z"],
        ),
        (
            deeper,
            &set_k,
            "x:\n  a: 1\n  k: v\n    # c\ny: 1\n",
            &["a", "k"],
        ),
        (
            deeper,
            &insert_k,
            "x:\n  a: 1\n  k: v\n    # c\ny: 1\n",
            &["a", "k"],
        ),
        (
            "x:\n  a: 1\n  b: 2\n    # c\ny: 1\n",
            &set_k,
            "x:\n  a: 1\n  b: 2\n  k: v\n    # c\ny: 1\n",
            &["a", "b", "k"],
        ),
    ] {
        let out = edit_x(src, edit);
        assert_eq!(out, expected, "{src:?}");
        let reparsed = YamlFile::from_str(&out).unwrap();
        let root = root_mapping(&reparsed);
        assert_eq!(keys(&root), ["x", "y"]);
        assert_eq!(keys(&root.get_mapping("x").unwrap()), x_keys);
    }
}

#[test]
fn append_after_comment_ending_the_input() {
    assert_eq!(
        edit_x("x:\n  a: 1\n  # end", |m| m.set("b", 2)),
        "x:\n  a: 1\n  b: 2\n  # end"
    );
    let file = YamlFile::from_str("a: 1\n# end").unwrap();
    root_mapping(&file).set("b", 2);
    assert_file_cst_ok(&file);
    assert_eq!(file.to_string(), "a: 1\n# end\nb: 2");
}

#[test]
fn file_insert_at_index() {
    let len = |f: &YamlFile| {
        f.document()
            .and_then(|d| d.as_mapping())
            .map_or(0, |m| m.len())
    };
    for (source, inserts, expected) in [
        ("", &[(0, "first", "value")][..], "first: value\n"),
        (
            "first: 1\nsecond: 2\nthird: 3",
            &[(2, "first", "updated")],
            "first: updated\nsecond: 2\nthird: 3",
        ),
        (
            "first: 1\nsecond: 2",
            &[(0, "zero", "0")],
            "zero: '0'\nfirst: 1\nsecond: 2",
        ),
        (
            "first: 1\nthird: 3",
            &[(1, "second", "2")],
            "first: 1\nsecond: '2'\nthird: 3",
        ),
        (
            "name: project\nversion: 1.0",
            &[(1, "author", "developer")],
            "name: project\nauthor: developer\nversion: 1.0",
        ),
        (
            "key1: value1",
            &[
                (1, "special:key", "value:with:colons"),
                (0, "key with spaces", "value with spaces"),
                (2, "key@symbol", "value#hash"),
            ],
            "key with spaces: value with spaces\nkey1: value1\nkey@symbol: value#hash\nspecial:key: value:with:colons",
        ),
        (
            "a: 1\nb: 2\nc: 3",
            &[(1, "b", "updated"), (2, "d", "4")],
            "a: 1\nb: updated\nd: '4'\nc: 3",
        ),
    ] {
        let yaml = YamlFile::from_str(source).unwrap();
        for &(index, key, value) in inserts {
            let existed = yaml
                .document()
                .and_then(|d| d.as_mapping())
                .is_some_and(|m| m.contains_key(key));
            let before = len(&yaml);
            yaml.insert_at_index(index, key, value);
            assert_file_cst_ok(&yaml);
            assert_eq!(len(&yaml), before + usize::from(!existed), "{key:?}");
            let mapping = root_mapping(&yaml);
            let inserted = mapping.get(key).unwrap();
            assert_eq!(inserted.as_scalar().unwrap().as_string(), value);
        }
        assert_file_edited(&yaml, expected);
    }
}

#[test]
fn document_insert_at_index() {
    let doc = Document::new();
    doc.set("first", 1);
    assert_cst_ok(&doc);
    doc.set("second", 2);
    assert_cst_ok(&doc);
    doc.insert_at_index(1, "middle", 1.5);
    assert_cst_ok(&doc);
    assert_eq!(doc.to_string(), "---\nfirst: 1\nmiddle: 1.5\nsecond: 2\n");
    for key in ["first", "middle", "second"] {
        assert!(doc.get(key).is_some(), "{key:?}");
    }
}

#[test]
fn insert_at_index_keeps_document_markers() {
    for (input, expected) in [
        ("--- \n...\n", "--- \nkey: value\n...\n"),
        ("---\n", "---\nkey: value\n"),
    ] {
        let file = YamlFile::from_str(input).unwrap();
        file.document().unwrap().insert_at_index(0, "key", "value");
        assert_eq!(file.to_string(), expected, "{input:?}");
    }
}

/// Inserting into a nested mapping indents the new entry to its siblings'
/// column at every position, measured from the document rather than
/// assumed, and leaves the indent of whichever entry it displaces; even
/// before the only entry, where insert_pos is 0 yet an entry is still
/// displaced. Root-level mappings have no indent to apply.
#[test]
fn insert_at_index_indents_to_siblings() {
    for (source, index, expected) in [
        ("m:\n  a: 1\n  b: 2\n", 0, "m:\n  zz: '9'\n  a: 1\n  b: 2\n"),
        ("m:\n  a: 1\n  b: 2\n", 1, "m:\n  a: 1\n  zz: '9'\n  b: 2\n"),
        ("m:\n  a: 1\n  b: 2\n", 2, "m:\n  a: 1\n  b: 2\n  zz: '9'\n"),
        (
            "m:\n    a: 1\n    b: 2\n",
            0,
            "m:\n    zz: '9'\n    a: 1\n    b: 2\n",
        ),
        (
            "m:\n    a: 1\n    b: 2\n",
            1,
            "m:\n    a: 1\n    zz: '9'\n    b: 2\n",
        ),
        (
            "m:\n    a: 1\n    b: 2\n",
            2,
            "m:\n    a: 1\n    b: 2\n    zz: '9'\n",
        ),
        ("m:\n  a: 1\n", 0, "m:\n  zz: '9'\n  a: 1\n"),
        ("m:\n  a: 1\n", 1, "m:\n  a: 1\n  zz: '9'\n"),
        ("a: 1\nb: 2\n", 0, "zz: '9'\na: 1\nb: 2\n"),
        ("a: 1\nb: 2\n", 1, "a: 1\nzz: '9'\nb: 2\n"),
        ("a: 1\nb: 2\n", 2, "a: 1\nb: 2\nzz: '9'\n"),
    ] {
        let yaml = YamlFile::from_str(source).unwrap();
        let root = root_mapping(&yaml);
        let mapping = root.get_mapping("m").unwrap_or(root);
        mapping.insert_at_index(index, "zz", "9");
        assert_file_cst_ok(&yaml);
        assert_eq!(yaml.to_string(), expected, "index {index} in {source:?}");
    }
}

#[test]
fn insert_at_index_into_pending_block_mapping() {
    let doc = Document::from_str("x: 1\n").unwrap();
    let mapping = doc.as_mapping().unwrap();
    mapping.set("k", Mapping::new_pending_block());
    let nested = mapping.get_mapping("k").unwrap();
    nested.insert_at_index(0, "a", 1);
    assert_cst_ok(&doc);
    assert_eq!(doc.to_string(), "x: 1\nk:\n  a: 1\n");
}
