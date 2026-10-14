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
