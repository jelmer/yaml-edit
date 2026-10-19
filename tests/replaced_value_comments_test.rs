//! Comments around a value that is replaced: those inside it go with it,
//! the key-line or dash-line comment stays with the entry, and everything
//! outside the value is left as it was.

use rowan::ast::AstNode;
use std::str::FromStr;
use yaml_edit::path::YamlPath;
use yaml_edit::{debug, Mapping, MappingBuilder, MappingEntry, Sequence, YamlFile, YamlNode};

mod common;
use common::assert_file_cst_ok;

#[track_caller]
fn assert_parser_shaped(file: &YamlFile, expected: &str, src: &str) {
    assert_eq!(file.to_string(), expected, "{src:?}");
    assert_file_cst_ok(file);
    let reparsed = YamlFile::from_str(expected).unwrap();
    assert_eq!(
        debug::shape_to_string(file.syntax()),
        debug::shape_to_string(reparsed.syntax()),
        "{src:?}"
    );
}

/// The mapping at `path` in `file`, following mapping keys and sequence
/// indexes.
fn mapping_at(file: &YamlFile, path: &str) -> Mapping {
    let doc = file.document().unwrap();
    if path.is_empty() {
        return doc.as_mapping().unwrap();
    }
    match doc.try_get_path(path).unwrap() {
        YamlNode::Mapping(m) => m,
        other => panic!("not a mapping at {path:?}: {other:?}"),
    }
}

fn sequence_at(file: &YamlFile, path: &str) -> Sequence {
    let doc = file.document().unwrap();
    if path.is_empty() {
        return doc.as_sequence().unwrap();
    }
    match doc.try_get_path(path).unwrap() {
        YamlNode::Sequence(s) => s,
        other => panic!("not a sequence at {path:?}: {other:?}"),
    }
}

fn block_mapping() -> Mapping {
    MappingBuilder::new()
        .pair("y", 2)
        .build_document()
        .as_mapping()
        .unwrap()
}

fn value_of(src: &str) -> YamlNode {
    let file = YamlFile::from_str(src).unwrap();
    file.document()
        .unwrap()
        .as_mapping()
        .unwrap()
        .get("v")
        .unwrap()
}

#[test]
fn mapping_set_scalar_over_commented_value() {
    for (src, path, expected) in [
        ("a:  # c\n  # d\n  x: 1\n", "", "a: 5  # c\n"),
        (
            "# before\na:  # c\n  x: 1  # e\n  # f\n# after\nb: 2\n",
            "",
            "# before\na: 5  # c\n  # f\n# after\nb: 2\n",
        ),
        ("a:\n  - 1  # e\nb: 2\n", "", "a: 5\nb: 2\n"),
        ("a:\n  - 1\n  - 2  # e\nb: 2\n", "", "a: 5\nb: 2\n"),
        ("a:\n  - 1  # e\n", "", "a: 5\n"),
        ("o:\n  a:\n    - 1  # e\nz: 1\n", "o", "o:\n  a: 5\nz: 1\n"),
        ("a:\n  x: 1\n\nb: 2\n", "", "a: 5\n\nb: 2\n"),
        ("a:\n  x: 1\n\n  # f\nb: 2\n", "", "a: 5\n\n  # f\nb: 2\n"),
        (
            "o:\n  a:\n    x: 1\n    # f\nz: 1\n",
            "o",
            "o:\n  a: 5\n    # f\nz: 1\n",
        ),
        (
            "a: |  # h\n  text\n# after\nb: 2\n",
            "",
            "a: 5  # h\n# after\nb: 2\n",
        ),
        (
            "a: 1  # c\n# after\nb: 2\n",
            "",
            "a: 5  # c\n# after\nb: 2\n",
        ),
        ("a: [1,  # e\n  2]\nb: 2\n", "", "a: 5  # e\nb: 2\n"),
        ("? a\n:\n  - 1  # e\nb: 2\n", "", "? a\n: 5\nb: 2\n"),
        (
            "? a\n:\n  x: 1\n  # f\nb: 2\n",
            "",
            "? a\n: 5\n  # f\nb: 2\n",
        ),
    ] {
        let file = YamlFile::from_str(src).unwrap();
        mapping_at(&file, path).set("a", 5);
        assert_parser_shaped(&file, expected, src);
    }
}

#[test]
fn mapping_set_block_over_commented_value() {
    for (src, expected) in [
        ("a:  # c\n  # d\n  x: 1\n", "a:  # c\n  y: 2\n"),
        (
            "a:  # c\n  x: 1  # e\n  # f\n# after\nb: 2\n",
            "a:  # c\n  y: 2\n  # f\n# after\nb: 2\n",
        ),
        ("a:\n  - 1  # e\nb: 2\n", "a:\n  y: 2\nb: 2\n"),
        ("a:\n  x: 1\n\nb: 2\n", "a:\n  y: 2\n\nb: 2\n"),
        (
            "a: 1  # c\n# after\nb: 2\n",
            "a:  # c\n  y: 2\n# after\nb: 2\n",
        ),
    ] {
        let file = YamlFile::from_str(src).unwrap();
        mapping_at(&file, "").set("a", block_mapping());
        assert_parser_shaped(&file, expected, src);
    }
}

#[test]
fn sequence_set_over_commented_value() {
    for (src, expected) in [
        ("- x: 1  # c\n- 2\n", "- 5  # c\n- 2\n"),
        ("- x: 1  # c\n", "- 5  # c\n"),
        ("- x: 1  # c\n  y: 2  # d\n- 3\n", "- 5  # c\n- 3\n"),
        ("- x: |  # h\n    t\n- 2\n", "- 5  # h\n- 2\n"),
        ("- - x: 1  # c\n- 2\n", "- 5  # c\n- 2\n"),
        ("- - 1  # c\n  - 2\n- 3\n", "- 5  # c\n- 3\n"),
        ("- - 1\n  - 2  # c\n- 3\n", "- 5\n- 3\n"),
        ("- - 1  # c\n- 3\n", "- 5  # c\n- 3\n"),
        ("- - 1  # c\n", "- 5  # c\n"),
        ("- {x: 1,  # c\n  y: 2}\n- 3\n", "- 5  # c\n- 3\n"),
        ("- 1  # c\n- 2\n", "- 5  # c\n- 2\n"),
        ("- # c\n  - 1\n  # d\n- 2\n", "- 5 # c\n  # d\n- 2\n"),
        ("-  # c\n  x: 1\n- 2\n", "- 5  # c\n- 2\n"),
        ("- |  # h\n  t\n- 2\n", "- 5  # h\n- 2\n"),
        ("- [1,  # c\n  2]\n- 2\n", "- 5  # c\n- 2\n"),
        ("- 1  # c\n", "- 5  # c\n"),
        ("-\n  x  # c\n- 2\n", "-\n  5\n- 2\n"),
        ("- x: 1\n\n- 2\n", "- 5\n\n- 2\n"),
        (
            "- x: 1\n  # inner\n# outer\n- 2\n",
            "- 5\n  # inner\n# outer\n- 2\n",
        ),
    ] {
        let file = YamlFile::from_str(src).unwrap();
        sequence_at(&file, "").set(0, 5);
        assert_parser_shaped(&file, expected, src);
    }
}

/// The spaces of a last line with no line break are part of the block
/// scalar they end, and go with it.
#[test]
fn set_over_block_scalar_ending_in_spaces() {
    for (src, expected) in [
        ("- |+\n   ", "- 5\n"),
        ("- |\n  a\n   ", "- 5\n"),
        ("- |\n  a\n  ", "- 5\n"),
    ] {
        let file = YamlFile::from_str(src).unwrap();
        sequence_at(&file, "").set(0, 5);
        assert_parser_shaped(&file, expected, src);
    }
    for (src, expected) in [("a: |+\n   ", "a: 5\n"), ("a: |\n  a\n   ", "a: 5\n")] {
        let file = YamlFile::from_str(src).unwrap();
        mapping_at(&file, "").set("a", 5);
        assert_parser_shaped(&file, expected, src);
    }
}

#[test]
fn set_to_empty_node_leaves_no_trailing_space() {
    let null = value_of("v:\n");
    let file = YamlFile::from_str("- 1\n- 2\n").unwrap();
    sequence_at(&file, "").set(0, &null);
    assert_parser_shaped(&file, "-\n- 2\n", "- 1\n- 2\n");

    let file = YamlFile::from_str("a: 1\nb: 2\n").unwrap();
    mapping_at(&file, "").set("a", &null);
    assert_parser_shaped(&file, "a:\nb: 2\n", "a: 1\nb: 2\n");
}

#[test]
fn nested_sequence_set_over_commented_value() {
    for (src, expected) in [
        ("a:\n  - x: 1\n\nb: 2\n", "a:\n  - 5\n\nb: 2\n"),
        ("a:\n  - - 1\n    - 2  # c\nb: 2\n", "a:\n  - 5\nb: 2\n"),
        ("a:\n  - x: 1  # c\n  - 2\n", "a:\n  - 5  # c\n  - 2\n"),
        ("a:\n  - - 1  # c\nb: 2\n", "a:\n  - 5  # c\nb: 2\n"),
        ("a:\n  - |  # h\n    t\nb: 2\n", "a:\n  - 5  # h\nb: 2\n"),
        ("a:\n  - |  # h\n    t\n  - 2\n", "a:\n  - 5  # h\n  - 2\n"),
        ("a:\n  - |  # h\n    t\n", "a:\n  - 5  # h\n"),
    ] {
        let file = YamlFile::from_str(src).unwrap();
        sequence_at(&file, "a").set(0, 5);
        assert_parser_shaped(&file, expected, src);
    }
}

#[test]
fn sequence_set_block_over_commented_value() {
    for (src, expected) in [
        ("- x: 1  # c\n- 2\n", "-  # c\n  y: 2\n- 2\n"),
        ("- - 1  # c\n- 3\n", "-  # c\n  y: 2\n- 3\n"),
        ("- - 1  # c\n  - 2\n- 3\n", "-  # c\n  y: 2\n- 3\n"),
        ("- x: 1\n\n- 2\n", "- y: 2\n\n- 2\n"),
        ("- x: 1\n  # inner\n- 2\n", "- y: 2\n  # inner\n- 2\n"),
        ("-  # c\n  x: 1\n- 2\n", "-  # c\n  y: 2\n- 2\n"),
        ("- 1  # c\n- 2\n", "-  # c\n  y: 2\n- 2\n"),
        ("- |  # h\n  t\n- 2\n", "-  # h\n  y: 2\n- 2\n"),
        ("-\n  x  # c\n- 2\n", "-\n  y: 2\n- 2\n"),
        ("-\n  x  # c\n", "-\n  y: 2\n"),
    ] {
        let file = YamlFile::from_str(src).unwrap();
        sequence_at(&file, "").set(0, block_mapping());
        assert_parser_shaped(&file, expected, src);
    }
}

#[test]
fn sequence_set_block_scalar_keeps_dash_line_comment() {
    for (src, expected) in [
        ("- 1  # c\n- 2\n", "- |  # c\n  text\n- 2\n"),
        ("-  # c\n  x: 1\n- 2\n", "- |  # c\n  text\n- 2\n"),
        ("- x: 1  # c\n- 2\n", "- |  # c\n  text\n- 2\n"),
        ("- - 1  # c\n- 2\n", "- |  # c\n  text\n- 2\n"),
    ] {
        let file = YamlFile::from_str(src).unwrap();
        sequence_at(&file, "").set(0, value_of("v: |\n  text\n"));
        assert_parser_shaped(&file, expected, src);
    }
}

/// A block scalar header with a comment of its own leaves no room on the
/// line for the entry's, which moves to a line of its own above the entry.
#[test]
fn block_scalar_header_comment_displaces_key_line_comment() {
    for (src, path, expected) in [
        ("k: 1  # note\n", "", "# note\nk: |  # src\n  text\n"),
        ("k:  # note\n  x: 1\n", "", "# note\nk: |  # src\n  text\n"),
        (
            "a: 0\nk: 1  # note\n",
            "",
            "a: 0\n# note\nk: |  # src\n  text\n",
        ),
        (
            "---\nk: 1  # note\n",
            "",
            "---\n# note\nk: |  # src\n  text\n",
        ),
        (
            "o:\n  k: 1  # note\n",
            "o",
            "o:\n  # note\n  k: |  # src\n    text\n",
        ),
        (
            "o:\n  j: 0\n  k: 1  # note\n",
            "o",
            "o:\n  j: 0\n  # note\n  k: |  # src\n    text\n",
        ),
        (
            "- k: 1  # note\n  j: 2\n",
            "[0]",
            "- # note\n  k: |  # src\n    text\n  j: 2\n",
        ),
    ] {
        let file = YamlFile::from_str(src).unwrap();
        mapping_at(&file, path).set("k", value_of("v: |  # src\n  text\n"));
        assert_parser_shaped(&file, expected, src);
    }
    for (value, expected) in [
        (
            "v: &a |  # src\n  text\n",
            "# note\nk: &a |  # src\n  text\n",
        ),
        (
            "v: !t |  # src\n  text\n",
            "# note\nk: !t |  # src\n  text\n",
        ),
        ("v: |\n  text\n", "k: |  # note\n  text\n"),
    ] {
        let src = "k: 1  # note\n";
        let file = YamlFile::from_str(src).unwrap();
        mapping_at(&file, "").set("k", value_of(value));
        assert_parser_shaped(&file, expected, value);
    }
}

#[test]
fn sequence_block_scalar_header_comment_displaces_dash_line_comment() {
    for (src, path, expected) in [
        ("- 1  # c\n- 2\n", "", "# c\n- |  # src\n  text\n- 2\n"),
        ("- 0\n- 1  # c\n", "", "- 0\n# c\n- |  # src\n  text\n"),
        (
            "a:\n  - 1  # c\n",
            "a",
            "a:\n  # c\n  - |  # src\n    text\n",
        ),
    ] {
        let file = YamlFile::from_str(src).unwrap();
        let index = sequence_at(&file, path).len() - 1;
        let index = if path.is_empty() && src.starts_with("- 1") {
            0
        } else {
            index
        };
        sequence_at(&file, path).set(index, value_of("v: |  # src\n  text\n"));
        assert_parser_shaped(&file, expected, src);
    }
}

/// The lines after the entry are laid out as the parser does after the new
/// value. A comment line indented as far as a new block scalar's body would
/// read as part of it, so it moves back to the entry's column.
#[test]
fn lines_after_entry_follow_the_new_value() {
    for (src, path, block, expected) in [
        (
            "a: 1\n  # f\nb: 2\n",
            "",
            false,
            "a:\n  y: 2\n  # f\nb: 2\n",
        ),
        ("a: 1\n\nb: 2\n", "", false, "a:\n  y: 2\n\nb: 2\n"),
        (
            "o:\n  a: 1\n\n  b: 2\n",
            "o",
            false,
            "o:\n  a:\n    y: 2\n\n  b: 2\n",
        ),
        (
            "a: 1\n\n  # f\nb: 2\n",
            "",
            false,
            "a:\n  y: 2\n\n  # f\nb: 2\n",
        ),
        ("a: 1\n  # f\nb: 2\n", "", true, "a: |\n  text\n# f\nb: 2\n"),
        (
            "o:\n  a: 1\n    # f\n  b: 2\n",
            "o",
            true,
            "o:\n  a: |\n    text\n  # f\n  b: 2\n",
        ),
        (
            "a:\n  x: 1\n  # f\nb: 2\n",
            "",
            true,
            "a: |\n  text\n# f\nb: 2\n",
        ),
        (
            "a:\n  - 1  # e\n  # f\nb: 2\n",
            "",
            true,
            "a: |\n  text\n# f\nb: 2\n",
        ),
    ] {
        let file = YamlFile::from_str(src).unwrap();
        let mapping = mapping_at(&file, path);
        if block {
            mapping.set("a", value_of("v: |\n  text\n"));
        } else {
            mapping.set("a", block_mapping());
        }
        assert_parser_shaped(&file, expected, src);
    }
}

#[test]
fn set_path_over_commented_value() {
    for (src, path, expected) in [
        (
            "o:\n  a:  # c\n    x: 1  # e\n  b: 2\n",
            "o.a",
            "o:\n  a: 5  # c\n  b: 2\n",
        ),
        (
            "o:\n  -  # c\n    x: 1\n  - 2\n",
            "o[0]",
            "o:\n  - 5  # c\n  - 2\n",
        ),
        ("o:\n  - 1  # c\n  - 2\n", "o[0]", "o:\n  - 5  # c\n  - 2\n"),
    ] {
        let file = YamlFile::from_str(src).unwrap();
        file.document().unwrap().try_set_path(path, 5).unwrap();
        assert_parser_shaped(&file, expected, src);
    }
}

/// An entry that is not in a mapping has no lines around it, so whatever
/// would go above or after it stays inside it.
#[test]
fn detached_entry_set_value() {
    let entry = MappingEntry::new("k", 1, false, false);
    entry.set_value(2, false);
    assert_eq!(entry.to_string(), "k: 2\n");
    debug::validate_tree(entry.syntax()).unwrap();

    for (src, value, expected) in [
        ("k: 1\nz: 2\n", "v: 2\n", "k: 2\n"),
        (
            "k: 1  # note\nz: 2\n",
            "v: |  # src\n  text\n",
            "# note\nk: |  # src\n  text\n",
        ),
        ("k:\n  x: 1\n\nz: 2\n", "v:\n  y: 1\n", "k:\n  y: 1\n"),
    ] {
        let file = YamlFile::from_str(src).unwrap();
        let entry = mapping_at(&file, "").remove("k").unwrap();
        entry.set_value(value_of(value), false);
        assert_eq!(entry.to_string(), expected, "{src:?}");
        debug::validate_tree(entry.syntax()).unwrap();
    }
}

fn scalar_string(node: &YamlNode) -> String {
    node.as_scalar().unwrap().as_string()
}

/// The parser reads the blank lines after a block scalar as part of its
/// body, so they move into a new one. A kept (`|+`) scalar would take them
/// into its value, so there they are dropped.
#[test]
fn blank_lines_after_new_block_scalar() {
    for (src, path, value, expected) in [
        (
            "a:\n  x: 1\n\nb: 2\n",
            "",
            "v: |\n  text\n",
            "a: |\n  text\n\nb: 2\n",
        ),
        (
            "a: 1\n\nb: 2\n",
            "",
            "v: |\n  text\n",
            "a: |\n  text\n\nb: 2\n",
        ),
        (
            "a:\n  x: 1\n\n\nb: 2\n",
            "",
            "v: |-\n  text\n",
            "a: |-\n  text\n\n\nb: 2\n",
        ),
        ("a: 1\n\n", "", "v: >\n  text\n", "a: >\n  text\n\n"),
        (
            "o:\n  a:\n    x: 1\n\nb: 2\n",
            "o",
            "v: |\n  text\n",
            "o:\n  a: |\n    text\n\nb: 2\n",
        ),
        (
            "a:\n  x: 1\n\n  # f\n\nb: 2\n",
            "",
            "v: |\n  text\n",
            "a: |\n  text\n\n# f\n\nb: 2\n",
        ),
        (
            "a:\n  x: 1\n \nb: 2\n",
            "",
            "v: |\n  text\n",
            "a: |\n  text\n \nb: 2\n",
        ),
        (
            "a:\n  x: 1\n    \nb: 2\n",
            "",
            "v: |\n  text\n",
            "a: |\n  text\n\nb: 2\n",
        ),
        (
            "a: 1\n   \nb: 2\n",
            "",
            "v: |1\n  text\n",
            "a: |1\n  text\n\nb: 2\n",
        ),
        (
            "a: 1\n # f\nb: 2\n",
            "",
            "v: |1\n  text\n",
            "a: |1\n  text\n# f\nb: 2\n",
        ),
        (
            "a:\n  x: 1\n\t\nb: 2\n",
            "",
            "v: |\n  text\n",
            "a: |\n  text\n\nb: 2\n",
        ),
        (
            "a:\n  x: 1\n\nb: 2\n",
            "",
            "v: |+\n  text\n",
            "a: |+\n  text\nb: 2\n",
        ),
        (
            "a: 1\n\nb: 2\n",
            "",
            "v: |+\n  text\n",
            "a: |+\n  text\nb: 2\n",
        ),
        (
            "a: 1\n\n# f\n\nb: 2\n",
            "",
            "v: |+\n  text\n",
            "a: |+\n  text\n# f\n\nb: 2\n",
        ),
    ] {
        let file = YamlFile::from_str(src).unwrap();
        let source = value_of(value);
        mapping_at(&file, path).set("a", &source);
        assert_parser_shaped(&file, expected, src);
        let set = mapping_at(&YamlFile::from_str(expected).unwrap(), path)
            .get("a")
            .unwrap();
        assert_eq!(scalar_string(&set), scalar_string(&source), "{src:?}");
    }

    for (src, value, expected) in [
        ("- 1\n\n- 2\n", "v: |\n  text\n", "- |\n  text\n\n- 2\n"),
        ("- x: 1\n\n- 2\n", "v: |\n  text\n", "- |\n  text\n\n- 2\n"),
        ("- 1\n\n- 2\n", "v: |+\n  text\n", "- |+\n  text\n- 2\n"),
    ] {
        let file = YamlFile::from_str(src).unwrap();
        let source = value_of(value);
        sequence_at(&file, "").set(0, &source);
        assert_parser_shaped(&file, expected, src);
        let set = sequence_at(&YamlFile::from_str(expected).unwrap(), "")
            .get(0)
            .unwrap();
        assert_eq!(scalar_string(&set), scalar_string(&source), "{src:?}");
    }
}

#[test]
fn set_over_block_mapping_with_flow_key() {
    // A block mapping whose first key is a flow collection keeps the lines
    // after it at its end, as one with a plain key does.
    for (src, expected) in [
        ("a:\n  {x: 1}: 1\n\nb: 2\n", "a: 5\n\nb: 2\n"),
        ("a:\n  [x]: 1\n\n  # f\nb: 2\n", "a: 5\n\n  # f\nb: 2\n"),
    ] {
        let file = YamlFile::from_str(src).unwrap();
        mapping_at(&file, "").set("a", 5);
        assert_parser_shaped(&file, expected, src);
    }
    let src = "- {x: 1}: 1\n\n- 2\n";
    let file = YamlFile::from_str(src).unwrap();
    file.document().unwrap().as_sequence().unwrap().set(0, 5);
    assert_parser_shaped(&file, "- 5\n\n- 2\n", src);
}

/// The parser keeps the blank lines after an empty value in it, where it
/// looked for the value. They stay after a new value.
#[test]
fn blank_lines_after_empty_value() {
    for (src, path, expected_scalar, expected_block) in [
        ("a:\n\nb: 1\n", "", "a: 5\n\nb: 1\n", "a:\n  y: 2\n\nb: 1\n"),
        (
            "a:  # c\n\n\nb: 1\n",
            "",
            "a: 5  # c\n\n\nb: 1\n",
            "a:  # c\n  y: 2\n\n\nb: 1\n",
        ),
        (
            "x:\n  a:\n\n  b: 1\n",
            "x",
            "x:\n  a: 5\n\n  b: 1\n",
            "x:\n  a:\n    y: 2\n\n  b: 1\n",
        ),
    ] {
        let file = YamlFile::from_str(src).unwrap();
        mapping_at(&file, path).set("a", 5);
        assert_parser_shaped(&file, expected_scalar, src);
        let file = YamlFile::from_str(src).unwrap();
        mapping_at(&file, path).set("a", block_mapping());
        assert_parser_shaped(&file, expected_block, src);
    }
}

/// A comment ending the last line of a value that spans lines goes with
/// it, wherever the parser keeps it.
#[test]
fn comment_after_multi_line_value_goes_with_it() {
    for (src, expected) in [
        ("a: [x,\n  y]  # c\nb: 1\n", "a: 5\nb: 1\n"),
        ("? a\n: [x,\n  y]  # c\nb: 1\n", "? a\n: 5\nb: 1\n"),
    ] {
        let file = YamlFile::from_str(src).unwrap();
        mapping_at(&file, "").set("a", 5);
        assert_parser_shaped(&file, expected, src);
    }
    for (src, expected) in [
        ("- [x,\n  y]  # c\n- 1\n", "- 5\n- 1\n"),
        ("- a: [x,\n    y]  # c\n- 1\n", "- 5\n- 1\n"),
    ] {
        let file = YamlFile::from_str(src).unwrap();
        sequence_at(&file, "").set(0, 5);
        assert_parser_shaped(&file, expected, src);
    }
}
