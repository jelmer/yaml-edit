//! Comments around a value that is replaced: those inside it go with it,
//! the key-line or dash-line comment stays with the entry, and everything
//! outside the value is left as it was.

use rowan::ast::AstNode;
use std::str::FromStr;
use yaml_edit::path::YamlPath;
use yaml_edit::{debug, Mapping, MappingBuilder, Sequence, YamlFile, YamlNode};

mod common;
use common::assert_file_cst_ok;

#[track_caller]
fn assert_parser_shaped(file: &YamlFile, expected: &str, src: &str) {
    assert_eq!(file.to_string(), expected, "{src:?}");
    assert_file_cst_ok(file);
    let reparsed = YamlFile::from_str(expected).unwrap();
    assert_eq!(
        debug::tree_to_string(file.syntax()),
        debug::tree_to_string(reparsed.syntax()),
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
        ("- x: 1  # c\n- 2\n", "- 5\n- 2\n"),
        ("- - 1  # c\n  - 2\n- 3\n", "- 5\n- 3\n"),
        ("- - 1\n  - 2  # c\n- 3\n", "- 5\n- 3\n"),
        ("- - 1  # c\n- 3\n", "- 5\n- 3\n"),
        ("- 1  # c\n- 2\n", "- 5  # c\n- 2\n"),
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

#[test]
fn nested_sequence_set_over_commented_value() {
    for (src, expected) in [
        ("a:\n  - x: 1\n\nb: 2\n", "a:\n  - 5\n\nb: 2\n"),
        ("a:\n  - - 1\n    - 2  # c\nb: 2\n", "a:\n  - 5\nb: 2\n"),
    ] {
        let file = YamlFile::from_str(src).unwrap();
        sequence_at(&file, "a").set(0, 5);
        assert_parser_shaped(&file, expected, src);
    }
}

#[test]
fn sequence_set_block_over_commented_value() {
    for (src, expected) in [
        ("- x: 1  # c\n- 2\n", "- y: 2\n- 2\n"),
        ("- x: 1\n\n- 2\n", "- y: 2\n\n- 2\n"),
        ("- x: 1\n  # inner\n- 2\n", "- y: 2\n  # inner\n- 2\n"),
    ] {
        let file = YamlFile::from_str(src).unwrap();
        sequence_at(&file, "").set(0, block_mapping());
        assert_parser_shaped(&file, expected, src);
    }
}
