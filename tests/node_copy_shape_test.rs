//! A CST node passed as a key or value is copied in whole, wrapper
//! included, so the result has the shape the parser gives the same text.

use rowan::ast::AstNode;
use std::str::FromStr;
use yaml_edit::{debug, YamlFile, YamlNode};

mod common;
use common::assert_file_cst_ok;

fn value_of(src: &str, key: &str) -> YamlNode {
    let file = YamlFile::from_str(src).unwrap();
    file.document()
        .unwrap()
        .as_mapping()
        .unwrap()
        .get(key)
        .unwrap()
}

#[track_caller]
fn assert_parser_shaped(file: &YamlFile, expected: &str) {
    assert_eq!(file.to_string(), expected);
    assert_file_cst_ok(file);
    let reparsed = YamlFile::from_str(expected).unwrap();
    assert_eq!(
        debug::tree_to_string(file.syntax()),
        debug::tree_to_string(reparsed.syntax())
    );
}

#[test]
fn test_set_scalar_node_value() {
    let file = YamlFile::from_str("a: 1\n").unwrap();
    let mapping = file.document().unwrap().as_mapping().unwrap();
    mapping.set("b", value_of("k: v\n", "k"));
    assert_parser_shaped(&file, "a: 1\nb: v\n");
    assert_eq!(mapping.get("b").unwrap().to_string(), "v");
}

#[test]
fn test_rename_key_to_scalar_node() {
    let src = YamlFile::from_str("k: v\n").unwrap();
    let key = src
        .document()
        .unwrap()
        .as_mapping()
        .unwrap()
        .keys()
        .next()
        .unwrap();
    let file = YamlFile::from_str("a: 1\n").unwrap();
    let mapping = file.document().unwrap().as_mapping().unwrap();
    assert!(mapping.rename_key("a", key));
    assert_parser_shaped(&file, "k: 1\n");
    assert!(mapping.contains_key("k"));
}

#[test]
fn test_set_alias_node_value() {
    let file = YamlFile::from_str("a: &x 1\n").unwrap();
    let mapping = file.document().unwrap().as_mapping().unwrap();
    mapping.set("b", value_of("a: &x 1\nb: *x\n", "b"));
    assert_parser_shaped(&file, "a: &x 1\nb: *x\n");
}

#[test]
fn test_set_tagged_node_value() {
    let file = YamlFile::from_str("a: 1\n").unwrap();
    let mapping = file.document().unwrap().as_mapping().unwrap();
    let tagged = value_of("c: !t v\n", "c");
    mapping.set("b", tagged.as_tagged().unwrap());
    assert_parser_shaped(&file, "a: 1\nb: !t v\n");
}

/// Indent every line of `text` after the first by two more columns.
fn shift(text: &str) -> String {
    text.replace('\n', "\n  ").trim_end_matches(' ').to_string()
}

/// The text after `v:` in each source: block values that start on the key
/// line (anchor, tag, block scalar indicator) and ones that don't.
const BLOCK_VALUES: &[&str] = &[
    " |\n  x\n",
    " >-\n  x\n  y\n",
    " &a |\n  x\n",
    " &a\n  x: 1\n",
    " &a\n  - a\n",
    " !t\n  x: 1\n",
    " &a !t\n  x: 1\n",
    " !t |\n  x\n",
    " !!str >\n  x\n",
    "\n  x: 1\n",
    "\n  - a\n",
];

#[test]
fn test_set_block_value_node() {
    for value in BLOCK_VALUES {
        let source = value_of(&format!("v:{value}"), "v");
        for (src, nested, key, expected) in [
            ("k: 1\n", false, "k", format!("k:{value}")),
            ("k: 1\n", false, "n", format!("k: 1\nn:{value}")),
            (
                "m:\n  k: 1\n",
                true,
                "k",
                format!("m:\n  k:{}", shift(value)),
            ),
            (
                "m:\n  k: 1\n",
                true,
                "n",
                format!("m:\n  k: 1\n  n:{}", shift(value)),
            ),
        ] {
            let file = YamlFile::from_str(src).unwrap();
            let root = file.document().unwrap().as_mapping().unwrap();
            let mapping = if nested {
                root.get_mapping("m").unwrap()
            } else {
                root
            };
            mapping.set(key, &source);
            assert_parser_shaped(&file, &expected);
        }
    }
}

#[test]
fn test_set_block_value_node_keeps_key_line_comment() {
    for (value, expected) in [
        (" |\n  x\n", "k: |  # note\n  x\n"),
        (" &a |\n  x\n", "k: &a |  # note\n  x\n"),
        (" &a\n  x: 1\n", "k: &a  # note\n  x: 1\n"),
        (" !t\n  x: 1\n", "k: !t  # note\n  x: 1\n"),
        (" &a !t\n  x: 1\n", "k: &a !t  # note\n  x: 1\n"),
        (" !t |\n  x\n", "k: !t |  # note\n  x\n"),
        ("\n  x: 1\n", "k:  # note\n  x: 1\n"),
    ] {
        let source = value_of(&format!("v:{value}"), "v");
        for src in ["k: 1  # note\nj: 2\n", "k:  # note\n  - old\nj: 2\n"] {
            let file = YamlFile::from_str(src).unwrap();
            file.document()
                .unwrap()
                .as_mapping()
                .unwrap()
                .set("k", &source);
            assert_parser_shaped(&file, &format!("{expected}j: 2\n"));
        }
    }
}

#[test]
fn test_builder_pair_block_value_node() {
    for value in BLOCK_VALUES {
        let source = value_of(&format!("v:{value}"), "v");
        let doc = yaml_edit::MappingBuilder::new()
            .pair("n", &source)
            .pair("j", 1)
            .build_document();
        let expected = format!("n:{value}j: 1\n");
        assert_eq!(doc.to_string(), expected);
        let reparsed = yaml_edit::Document::from_str(&expected).unwrap();
        assert_eq!(
            debug::tree_to_string(doc.syntax()),
            debug::tree_to_string(reparsed.syntax())
        );
    }
}
