//! Strings written as literal or folded block scalars must read back as the
//! same string, falling back to a quoted style where a block scalar can't
//! hold them.

mod common;

use common::assert_file_cst_ok;
use std::str::FromStr;
use yaml_edit::validator::Validator;
use yaml_edit::{ScalarValue, YamlFile};

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
