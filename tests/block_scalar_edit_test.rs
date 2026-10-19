//! Block scalars written or moved by an edit.
//!
//! A block scalar with an explicit indentation indicator (`|2`) moved to
//! another column. The indicator counts from the parent's column, so the
//! body has to move with the parent for the value to stay the same.
//!
//! A block scalar written by an edit must not take in the lines after it:
//! a whitespace-only line with spaces past its body's column, or a comment
//! line indented as far as its body, would otherwise read as body text.
//!
//! Strings written as literal or folded block scalars must read back as the
//! same string, falling back to a quoted style where a block scalar can't
//! hold them.
//!
//! Inserting a value written as a kept (`|+`, `>+`) block scalar in front of
//! blank lines: those lines would be read as part of the scalar, so the new
//! entry goes after them.

mod common;
use common::{assert_file_cst_ok, assert_file_edited};
use rowan::ast::AstNode;
use std::str::FromStr;
use yaml_edit::path::YamlPath;
use yaml_edit::validator::Validator;
use yaml_edit::{debug, ScalarValue, Sequence, YamlFile, YamlNode};

fn scalar_string(node: &YamlNode) -> String {
    match node {
        YamlNode::Scalar(s) => s.as_string(),
        YamlNode::TaggedNode(t) => t.as_string().unwrap(),
        other => panic!("not a scalar: {other:?}"),
    }
}

/// The value at `path` (`k`, `o.k`, `[0]`, `o[0]`) in `src`.
fn node_at(src: &str, path: &str) -> YamlNode {
    let file = YamlFile::from_str(src).unwrap();
    file.document().unwrap().try_get_path(path).unwrap()
}

#[track_caller]
fn assert_moved(file: &YamlFile, expected: &str, path: &str, source: &YamlNode) {
    assert_file_edited(file, expected);
    assert_eq!(
        scalar_string(&node_at(expected, path)),
        scalar_string(source)
    );
}

const SOURCES: [(&str, &str); 4] = [
    ("v: |2\n   x\n", "v"),
    ("- v: |2\n     x\n", "[0].v"),
    ("v: !t |2\n   x\n", "v"),
    ("v: >1-\n   x\n  y\n", "v"),
];

#[test]
fn mapping_set_keeps_indicator_relative_to_entry() {
    let expected = [
        (
            "k: |2\n   x\n",
            "o:\n  k: |2\n     x\n",
            "o:\n  a: 1\n  k: |2\n     x\n",
        ),
        (
            "k: |2\n   x\n",
            "o:\n  k: |2\n     x\n",
            "o:\n  a: 1\n  k: |2\n     x\n",
        ),
        (
            "k: !t |2\n   x\n",
            "o:\n  k: !t |2\n     x\n",
            "o:\n  a: 1\n  k: !t |2\n     x\n",
        ),
        (
            "k: >1-\n   x\n  y\n",
            "o:\n  k: >1-\n     x\n    y\n",
            "o:\n  a: 1\n  k: >1-\n     x\n    y\n",
        ),
    ];
    for ((src, path), (root, nested, added)) in SOURCES.into_iter().zip(expected) {
        let value = node_at(src, path);

        let file = YamlFile::from_str("k: 1\n").unwrap();
        let doc = file.document().unwrap();
        doc.as_mapping().unwrap().set("k", &value);
        assert_moved(&file, root, "k", &value);

        let file = YamlFile::from_str("o:\n  k: 1\n").unwrap();
        let doc = file.document().unwrap();
        doc.as_mapping()
            .unwrap()
            .get_mapping("o")
            .unwrap()
            .set("k", &value);
        assert_moved(&file, nested, "o.k", &value);

        let file = YamlFile::from_str("o:\n  a: 1\n").unwrap();
        let doc = file.document().unwrap();
        doc.as_mapping()
            .unwrap()
            .get_mapping("o")
            .unwrap()
            .set("k", &value);
        assert_moved(&file, added, "o.k", &value);
    }
}

#[test]
fn sequence_set_keeps_indicator_relative_to_entry() {
    let expected = [
        ("- |2\n   x\n", "o:\n  - |2\n     x\n"),
        ("- |2\n   x\n", "o:\n  - |2\n     x\n"),
        ("- !t |2\n   x\n", "o:\n  - !t |2\n     x\n"),
        ("- >1-\n   x\n  y\n", "o:\n  - >1-\n     x\n    y\n"),
    ];
    for ((src, path), (root, nested)) in SOURCES.into_iter().zip(expected) {
        let value = node_at(src, path);

        let file = YamlFile::from_str("- 1\n").unwrap();
        file.document()
            .unwrap()
            .as_sequence()
            .unwrap()
            .set(0, &value);
        assert_moved(&file, root, "[0]", &value);

        let file = YamlFile::from_str("o:\n  - 1\n").unwrap();
        file.document()
            .unwrap()
            .as_mapping()
            .unwrap()
            .get_sequence("o")
            .unwrap()
            .set(0, &value);
        assert_moved(&file, nested, "o[0]", &value);
    }
}

/// Like [`check`], and the tree is also the one the parser builds for the
/// text. Block scalars built from a string are a single token, unlike
/// parsed ones, so this only holds for scalars copied from a parsed file.
#[track_caller]
fn check_parser_shaped(file: &YamlFile, expected: &str, path: &str, value: &str) {
    check(file, expected, path, value);
    let reparsed = YamlFile::from_str(expected).unwrap();
    assert_eq!(
        debug::shape_to_string(file.syntax()),
        debug::shape_to_string(reparsed.syntax())
    );
}

/// The value of `v` in `src`, as a node of a parsed file.
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
fn test_insert_after_before_deep_whitespace_line() {
    let f = YamlFile::from_str("a: 1\n     \nb: 2\n").unwrap();
    assert!(f.insert_after("a", "k", "text\n"));
    check(&f, "a: 1\nk: |\n  text\n\nb: 2\n", "k", "text\n");
}

#[test]
fn test_insert_after_keeps_shallow_whitespace_line() {
    let f = YamlFile::from_str("a: 1\n  \nb: 2\n").unwrap();
    assert!(f.insert_after("a", "k", "text\n"));
    check(&f, "a: 1\nk: |\n  text\n  \nb: 2\n", "k", "text\n");
}

#[test]
fn test_insert_after_stripped_before_deep_whitespace_line() {
    let f = YamlFile::from_str("a: 1\n     \nb: 2\n").unwrap();
    assert!(f.insert_after("a", "k", "x\ny"));
    check(&f, "a: 1\nk: |-\n  x\n  y\n\nb: 2\n", "k", "x\ny");
}

#[test]
fn test_insert_after_before_tab_line() {
    let f = YamlFile::from_str("a: 1\n  \t\nb: 2\n").unwrap();
    assert!(f.insert_after("a", "k", "text\n"));
    check(&f, "a: 1\nk: |\n  text\n\nb: 2\n", "k", "text\n");
}

#[test]
fn test_insert_after_nested_before_deep_whitespace_line() {
    let f = YamlFile::from_str("x:\n  a: 1\n       \n  b: 2\n").unwrap();
    let x = f.document().unwrap().get_mapping("x").unwrap();
    assert!(x.insert_after("a", "k", "text\n"));
    check(
        &f,
        "x:\n  a: 1\n  k: |\n    text\n\n  b: 2\n",
        "x.k",
        "text\n",
    );
}

#[test]
fn test_insert_before_after_deep_whitespace_line() {
    let f = YamlFile::from_str("a: 1\nb: 2\n     \nc: 3\n").unwrap();
    assert!(f.insert_before("b", "k", "text\n"));
    check(&f, "a: 1\nk: |\n  text\nb: 2\n     \nc: 3\n", "k", "text\n");
}

#[test]
fn test_insert_after_parsed_scalar_before_deep_whitespace_line() {
    let f = YamlFile::from_str("a: 1\n     \nb: 2\n").unwrap();
    assert!(f.insert_after("a", "k", value_of("v: |\n  text\n")));
    check_parser_shaped(&f, "a: 1\nk: |\n  text\n\nb: 2\n", "k", "text\n");
}

#[test]
fn test_set_before_deep_whitespace_line() {
    let f = YamlFile::from_str("a: 1\n     \nb: 2\n").unwrap();
    f.set("a", "text\n");
    check(&f, "a: |\n  text\n\nb: 2\n", "a", "text\n");
}

#[test]
fn test_set_kept_before_blank_line() {
    let f = YamlFile::from_str("a: 1\n\nb: 2\n").unwrap();
    f.set("a", "text\n\n");
    check(&f, "a: |+\n  text\n\nb: 2\n", "a", "text\n\n");
}

#[test]
fn test_set_path_before_deep_whitespace_line() {
    let f = YamlFile::from_str("x:\n  a: 1\n       \ny: 2\n").unwrap();
    f.document().unwrap().try_set_path("x.a", "text\n").unwrap();
    check(&f, "x:\n  a: |\n    text\n\ny: 2\n", "x.a", "text\n");
}

#[test]
fn test_sequence_set_before_deep_whitespace_line() {
    let f = YamlFile::from_str("- 1\n     \n- 2\n").unwrap();
    let seq = f.document().unwrap().as_sequence().unwrap();
    assert!(seq.set(0, "text\n"));
    check(&f, "- |\n  text\n\n- 2\n", "[0]", "text\n");
}

#[test]
fn test_insert_after_before_deep_comment_line() {
    let f = YamlFile::from_str("a: 1\n    # c\nb: 2\n").unwrap();
    assert!(f.insert_after("a", "k", "text\n"));
    check(&f, "a: 1\nk: |\n  text\n# c\nb: 2\n", "k", "text\n");
}

#[test]
fn test_insert_after_keeps_shallow_comment_line() {
    let f = YamlFile::from_str("x:\n  a: 1\n   # c\n  b: 2\n").unwrap();
    let x = f.document().unwrap().get_mapping("x").unwrap();
    assert!(x.insert_after("a", "k", "text\n"));
    check(
        &f,
        "x:\n  a: 1\n  k: |\n    text\n   # c\n  b: 2\n",
        "x.k",
        "text\n",
    );
}

#[test]
fn test_insert_after_nested_before_deep_comment_line() {
    let f = YamlFile::from_str("x:\n  a: 1\n    # c\n  b: 2\n").unwrap();
    let x = f.document().unwrap().get_mapping("x").unwrap();
    assert!(x.insert_after("a", "k", "text\n"));
    check(
        &f,
        "x:\n  a: 1\n  k: |\n    text\n  # c\n  b: 2\n",
        "x.k",
        "text\n",
    );
}

#[test]
fn test_insert_after_before_blank_and_deep_comment_lines() {
    let f = YamlFile::from_str("a: 1\n\n    # c\nb: 2\n").unwrap();
    assert!(f.insert_after("a", "k", "text\n"));
    check(&f, "a: 1\nk: |\n  text\n\n# c\nb: 2\n", "k", "text\n");
}

#[test]
fn test_insert_after_before_whitespace_and_deep_comment_lines() {
    let f = YamlFile::from_str("a: 1\n  \n    # c\nb: 2\n").unwrap();
    assert!(f.insert_after("a", "k", "text\n"));
    check(&f, "a: 1\nk: |\n  text\n  \n# c\nb: 2\n", "k", "text\n");
}

#[test]
fn test_insert_after_before_two_deep_comment_lines() {
    let f = YamlFile::from_str("a: 1\n    # c\n    # d\nb: 2\n").unwrap();
    assert!(f.insert_after("a", "k", "text\n"));
    check(
        &f,
        "a: 1\nk: |\n  text\n# c\n    # d\nb: 2\n",
        "k",
        "text\n",
    );
}

#[test]
fn test_insert_after_kept_before_deep_comment_line() {
    let f = YamlFile::from_str("a: 1\n    # c\nb: 2\n").unwrap();
    assert!(f.insert_after("a", "k", "text\n\n"));
    check(&f, "a: 1\nk: |+\n  text\n\n# c\nb: 2\n", "k", "text\n\n");
}

#[test]
fn test_insert_after_parsed_scalar_before_deep_comment_line() {
    let f = YamlFile::from_str("a: 1\n\n    # c\nb: 2\n").unwrap();
    assert!(f.insert_after("a", "k", value_of("v: |\n  text\n")));
    check_parser_shaped(&f, "a: 1\nk: |\n  text\n\n# c\nb: 2\n", "k", "text\n");
}

#[test]
fn test_set_before_deep_comment_line() {
    let f = YamlFile::from_str("a: 1\n    # c\nb: 2\n").unwrap();
    f.set("a", "text\n");
    check(&f, "a: |\n  text\n# c\nb: 2\n", "a", "text\n");
}

#[test]
fn test_set_before_whitespace_and_deep_comment_lines() {
    let f = YamlFile::from_str("a: 1\n  \n    # c\nb: 2\n").unwrap();
    f.set("a", "text\n");
    check(&f, "a: |\n  text\n  \n# c\nb: 2\n", "a", "text\n");
}

#[test]
fn test_sequence_set_before_deep_comment_line() {
    let f = YamlFile::from_str("- 1\n    # c\n- 2\n").unwrap();
    let seq = f.document().unwrap().as_sequence().unwrap();
    assert!(seq.set(0, "text\n"));
    check(&f, "- |\n  text\n# c\n- 2\n", "[0]", "text\n");
}

#[test]
fn test_set_before_final_whitespace_line() {
    for (source, value, expected) in [
        ("a: 1\n ", "text\n", "a: |\n  text\n "),
        ("a: 1\n  ", "text\n", "a: |\n  text\n  "),
        ("a: 1\n     ", "text\n", "a: |\n  text\n"),
        ("a: 1\n\t", "text\n", "a: |\n  text\n"),
        ("a: 1\n\n ", "x\ny", "a: |-\n  x\n  y\n\n "),
        ("a: 1\n ", "text\n\n", "a: |+\n  text\n\n"),
    ] {
        let f = YamlFile::from_str(source).unwrap();
        f.set("a", value);
        check_parser_shaped(&f, expected, "a", value);
    }
}

#[test]
fn test_set_block_mapping_before_final_whitespace_line() {
    let f = YamlFile::from_str("a: 1\n ").unwrap();
    f.set("a", value_of("v:\n  x: |\n    t\n"));
    check_parser_shaped(&f, "a:\n  x: |\n    t\n ", "a.x", "t\n");
}

/// Strings that are edge cases for block scalars.
const CASES: &[&str] = &[
    "",
    "\n",
    "\n\n",
    "\n\n\n",
    " ",
    "\t",
    " x",
    "  x\n",
    "x\n ",
    "x \n",
    "x  ",
    "x\n\n",
    "x\n\n\n",
    "\nx",
    "\n\nx\n",
    "\n x",
    "\n \n",
    "  \n",
    " \nx",
    "x\n \n",
    "x\n  y\n",
    "# c\n",
    "#",
    "x\n# c\n",
    "x\n#",
    "x\r\ny\r\n",
    "x\ry",
    "\r",
    "\u{7}\n",
    "x\u{85}y\n",
    "\u{feff}x\n",
    "x\u{2028}y",
    "\u{e9}\n",
    " a\nb\n",
    "a\n b\nc\n",
    "a\n\n b\n\nc",
    "a\n\nb",
    "a\n\n\nb\n",
    "a b\nc d\n",
    "a \nb",
    "a\n \nb",
    "x\n\n y",
    "\ta\nb",
    "a\n\tb\n",
    "\t\n",
];

fn read_root(file: &YamlFile) -> String {
    let mapping = file.document().unwrap().as_mapping().unwrap();
    mapping.get("k").unwrap().as_scalar().unwrap().as_string()
}

fn read_nested(file: &YamlFile) -> String {
    let mapping = file.document().unwrap().as_mapping().unwrap();
    let inner = mapping.get("a").unwrap();
    let inner = inner.as_mapping().unwrap();
    inner.get("k").unwrap().as_scalar().unwrap().as_string()
}

fn read_seq(file: &YamlFile) -> String {
    let seq = file.document().unwrap().as_sequence().unwrap();
    seq.get(0).unwrap().as_scalar().unwrap().as_string()
}

fn set_root(value: ScalarValue) -> YamlFile {
    let file = YamlFile::from_str("k: v\nz: 1\n").unwrap();
    file.document()
        .unwrap()
        .as_mapping()
        .unwrap()
        .set("k", value);
    file
}

fn set_nested(value: ScalarValue) -> YamlFile {
    let file = YamlFile::from_str("a:\n  k: v\n  z: 1\n").unwrap();
    let mapping = file.document().unwrap().as_mapping().unwrap();
    mapping
        .get("a")
        .unwrap()
        .as_mapping()
        .unwrap()
        .set("k", value);
    file
}

fn set_seq(value: ScalarValue) -> YamlFile {
    let file = YamlFile::from_str("- v\n- 1\n").unwrap();
    file.document()
        .unwrap()
        .as_sequence()
        .unwrap()
        .set(0, value);
    file
}

type Setter = fn(ScalarValue) -> YamlFile;
type Reader = fn(&YamlFile) -> String;

const PLACES: &[(&str, Setter, Reader)] = &[
    ("root", set_root, read_root),
    ("nested", set_nested, read_nested),
    ("sequence", set_seq, read_seq),
];

#[test]
fn test_block_scalar_edge_cases_round_trip() {
    for &value in CASES {
        for (style, scalar) in [
            ("literal", ScalarValue::literal(value)),
            ("folded", ScalarValue::folded(value)),
            ("string", ScalarValue::string(value)),
        ] {
            for &(place, set, read) in PLACES {
                let file = set(scalar.clone());
                assert_file_cst_ok(&file);
                let text = file.to_string();
                let reparsed = YamlFile::from_str(&text)
                    .unwrap_or_else(|e| panic!("{style} {value:?} at {place}: {text:?}: {e}"));
                assert_eq!(
                    read(&reparsed),
                    value,
                    "{style} {value:?} at {place}: {text:?}"
                );
                assert_eq!(
                    Validator::new().validate(&reparsed.document().unwrap()),
                    vec![],
                    "{style} {value:?} at {place}: {text:?}"
                );
            }
        }
    }
}

#[test]
fn test_line_breaks_only_keep_them() {
    // Clipping keeps the break after the last content line, and a value of
    // only line breaks has none.
    assert_eq!(
        set_root(ScalarValue::literal("\n")).to_string(),
        "k: |+\n\nz: 1\n"
    );
    assert_eq!(
        set_root(ScalarValue::folded("\n")).to_string(),
        "k: >+\n\nz: 1\n"
    );
    assert_eq!(
        set_nested(ScalarValue::literal("\n")).to_string(),
        "a:\n  k: |+\n\n  z: 1\n"
    );
    assert_eq!(
        set_seq(ScalarValue::literal("\n")).to_string(),
        "- |+\n\n- 1\n"
    );
    assert_eq!(
        set_root(ScalarValue::string("\n")).to_string(),
        "k: |+\n\nz: 1\n"
    );
}

#[test]
fn test_empty_block_scalar_has_no_body() {
    assert_eq!(
        set_root(ScalarValue::literal("")).to_string(),
        "k: |-\nz: 1\n"
    );
    assert_eq!(
        set_root(ScalarValue::folded("")).to_string(),
        "k: >-\nz: 1\n"
    );
}

#[test]
fn test_block_scalar_with_carriage_return_is_double_quoted() {
    // A block scalar reads a carriage return as a line break, and `\r\n`
    // as a single `\n`.
    assert_eq!(
        set_root(ScalarValue::literal("x\r\ny\r\n")).to_string(),
        "k: \"x\\r\\ny\\r\\n\"\nz: 1\n"
    );
    assert_eq!(
        set_root(ScalarValue::folded("x\ry")).to_string(),
        "k: \"x\\ry\"\nz: 1\n"
    );
}

#[test]
fn test_block_scalar_with_unprintable_character_is_double_quoted() {
    assert_eq!(
        set_root(ScalarValue::literal("\u{7}\n")).to_string(),
        "k: \"\\a\\n\"\nz: 1\n"
    );
    assert_eq!(
        set_root(ScalarValue::folded("\u{feff}x\n")).to_string(),
        "k: \"\\uFEFFx\\n\"\nz: 1\n"
    );
}

/// Check that `file` renders as `expected` and that the scalar at `path`
/// reads back as `value`.
#[track_caller]
fn check(file: &YamlFile, expected: &str, path: &str, value: &str) {
    let text = file.to_string();
    assert_eq!(text, expected);
    assert_file_cst_ok(file);
    let reparsed = YamlFile::from_str(&text).unwrap();
    let node = reparsed.document().unwrap().try_get_path(path).unwrap();
    let scalar = match node.as_tagged() {
        Some(tagged) => tagged.value().unwrap(),
        None => node.as_scalar().unwrap().clone(),
    };
    assert_eq!(scalar.as_string(), value);
}

#[test]
fn test_insert_after_before_blank_line() {
    let f = YamlFile::from_str("a: 1\n\nb: 2\n").unwrap();
    assert!(f.insert_after("a", "k", "text\n\n"));
    check(&f, "a: 1\n\nk: |+\n  text\n\nb: 2\n", "k", "text\n\n");
}

#[test]
fn test_insert_after_before_blank_lines_and_comment() {
    let f = YamlFile::from_str("a: 1\n\n\n# c\nb: 2\n").unwrap();
    assert!(f.insert_after("a", "k", "text\n\n"));
    check(
        &f,
        "a: 1\n\n\nk: |+\n  text\n\n# c\nb: 2\n",
        "k",
        "text\n\n",
    );
}

#[test]
fn test_insert_after_before_whitespace_line() {
    let f = YamlFile::from_str("a: 1\n    \nb: 2\n").unwrap();
    assert!(f.insert_after("a", "k", "text\n\n"));
    check(&f, "a: 1\n    \nk: |+\n  text\n\nb: 2\n", "k", "text\n\n");
}

#[test]
fn test_insert_after_folded_before_blank_line() {
    let f = YamlFile::from_str("a: 1\n\nb: 2\n").unwrap();
    assert!(f.insert_after("a", "k", ScalarValue::folded("text\n\n")));
    check(&f, "a: 1\n\nk: >+\n  text\n\nb: 2\n", "k", "text\n\n");
}

#[test]
fn test_insert_after_tagged_before_blank_line() {
    let f = YamlFile::from_str("a: 1\n\nb: 2\n").unwrap();
    assert!(f.insert_after("a", "k", ScalarValue::string("text\n\n").with_tag("!t")));
    check(&f, "a: 1\n\nk: !t |+\n  text\n\nb: 2\n", "k", "text\n\n");
}

#[test]
fn test_insert_after_parsed_scalar_before_blank_line() {
    let source = YamlFile::from_str("s: |+\n  text\n\n").unwrap();
    let value = source.document().unwrap().get("s").unwrap();
    let f = YamlFile::from_str("a: 1\n\nb: 2\n").unwrap();
    assert!(f.insert_after("a", "k", value));
    check(&f, "a: 1\n\nk: |+\n  text\n\nb: 2\n", "k", "text\n\n");
}

#[test]
fn test_insert_after_mapping_ending_in_kept_scalar() {
    let source = YamlFile::from_str("m:\n  s: |+\n    text\n\n").unwrap();
    let value = source.document().unwrap().get("m").unwrap();
    let f = YamlFile::from_str("a: 1\n\nb: 2\n").unwrap();
    assert!(f.insert_after("a", "k", value));
    check(
        &f,
        "a: 1\n\nk:\n  s: |+\n    text\n\nb: 2\n",
        "k.s",
        "text\n\n",
    );
}

#[test]
fn test_insert_after_nested_before_blank_line() {
    let f = YamlFile::from_str("x:\n  a: 1\n\n  b: 2\n").unwrap();
    let x = f.document().unwrap().get_mapping("x").unwrap();
    assert!(x.insert_after("a", "k", "text\n\n"));
    check(
        &f,
        "x:\n  a: 1\n\n  k: |+\n    text\n\n  b: 2\n",
        "x.k",
        "text\n\n",
    );
}

#[test]
fn test_insert_after_explicit_key_before_blank_line() {
    let f = YamlFile::from_str("? a\n: 1\n\n? b\n: 2\n").unwrap();
    assert!(f.insert_after("a", "k", "text\n\n"));
    check(
        &f,
        "? a\n: 1\n\n? k\n: |+\n  text\n\n? b\n: 2\n",
        "k",
        "text\n\n",
    );
}

#[test]
fn test_move_after_before_blank_line() {
    let f = YamlFile::from_str("a: 1\n\nb: 2\nk: 3\n").unwrap();
    assert!(f.move_after("a", "k", "text\n\n"));
    check(&f, "a: 1\n\nk: |+\n  text\n\nb: 2\n", "k", "text\n\n");
}

#[test]
fn test_set_with_field_order_before_blank_line() {
    let f = YamlFile::from_str("a: 1\n\nb: 2\n").unwrap();
    f.document()
        .unwrap()
        .set_with_field_order("k", "text\n\n", ["a", "k", "b"]);
    check(&f, "a: 1\n\nk: |+\n  text\n\nb: 2\n", "k", "text\n\n");
}

#[test]
fn test_insert_before_after_blank_line() {
    let f = YamlFile::from_str("a: 1\n\nb: 2\n").unwrap();
    assert!(f.insert_before("b", "k", "text\n\n"));
    check(&f, "a: 1\n\nk: |+\n  text\n\nb: 2\n", "k", "text\n\n");
}

#[test]
fn test_insert_at_index_after_blank_line() {
    let f = YamlFile::from_str("a: 1\n\nb: 2\n").unwrap();
    f.insert_at_index(1, "k", "text\n\n");
    check(&f, "a: 1\n\nk: |+\n  text\n\nb: 2\n", "k", "text\n\n");
}

#[test]
fn test_set_at_end_after_blank_line() {
    let f = YamlFile::from_str("a: 1\n\n").unwrap();
    f.set("k", "text\n\n");
    check(&f, "a: 1\n\nk: |+\n  text\n\n", "k", "text\n\n");
}

#[test]
fn test_clipped_scalar_stays_before_blank_line() {
    let f = YamlFile::from_str("a: 1\n\nb: 2\n").unwrap();
    assert!(f.insert_after("a", "k", "text\n"));
    check(&f, "a: 1\nk: |\n  text\n\nb: 2\n", "k", "text\n");
}

#[test]
fn test_push_before_blank_line() {
    let f = YamlFile::from_str("- 1\n\n").unwrap();
    f.document()
        .unwrap()
        .as_sequence()
        .unwrap()
        .push("text\n\n");
    check(&f, "- 1\n\n- |+\n  text\n\n", "[1]", "text\n\n");
}

#[test]
fn test_push_nested_before_blank_line() {
    let f = YamlFile::from_str("x:\n  - 1\n\ny: 2\n").unwrap();
    f.document()
        .unwrap()
        .get_sequence("x")
        .unwrap()
        .push("text\n\n");
    check(
        &f,
        "x:\n  - 1\n\n  - |+\n    text\n\ny: 2\n",
        "x[1]",
        "text\n\n",
    );
}

#[test]
fn test_push_clipped_scalar_stays_before_blank_line() {
    let f = YamlFile::from_str("x:\n  - 1\n\ny: 2\n").unwrap();
    f.document()
        .unwrap()
        .get_sequence("x")
        .unwrap()
        .push("text\n");
    check(&f, "x:\n  - 1\n  - |\n    text\n\ny: 2\n", "x[1]", "text\n");
}

#[test]
fn test_sequence_insert_after_blank_line() {
    let f = YamlFile::from_str("- 1\n\n- 2\n").unwrap();
    f.document()
        .unwrap()
        .as_sequence()
        .unwrap()
        .insert(1, "text\n\n");
    check(&f, "- 1\n\n- |+\n  text\n\n- 2\n", "[1]", "text\n\n");
}

#[test]
fn test_set_path_nested_after_blank_line() {
    let f = YamlFile::from_str("x:\n  a: 1\n\ny: 2\n").unwrap();
    f.document()
        .unwrap()
        .try_set_path("x.k", "text\n\n")
        .unwrap();
    check(
        &f,
        "x:\n  a: 1\n\n  k: |+\n    text\n\ny: 2\n",
        "x.k",
        "text\n\n",
    );
}

#[test]
fn test_push_onto_new_sequence_before_blank_line() {
    let f = YamlFile::from_str("a: 1\n\nb: 2\n").unwrap();
    assert!(f.insert_after("a", "x", Sequence::new_pending_block()));
    f.document()
        .unwrap()
        .get_sequence("x")
        .unwrap()
        .push("text\n\n");
    check(
        &f,
        "a: 1\nx:\n\n  - |+\n    text\n\nb: 2\n",
        "x[0]",
        "text\n\n",
    );
}

#[test]
fn test_set_in_compact_mapping_before_blank_line() {
    let f = YamlFile::from_str("- a: 1\n\n- 2\n").unwrap();
    let item = f.document().unwrap().as_sequence().unwrap().get(0).unwrap();
    item.as_mapping().unwrap().set("k", "text\n\n");
    check(
        &f,
        "- a: 1\n\n  k: |+\n    text\n\n- 2\n",
        "[0].k",
        "text\n\n",
    );
}
