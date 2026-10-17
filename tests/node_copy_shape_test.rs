//! A CST node passed as a key or value is copied in whole, wrapper
//! included, so the result has the shape the parser gives the same text.

use rowan::ast::AstNode;
use std::str::FromStr;
use yaml_edit::{debug, validator::Validator, YamlFile, YamlNode};

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

/// Like [`assert_parser_shaped`], and the result has no spec violations.
#[track_caller]
fn assert_valid_copy(file: &YamlFile, expected: &str) {
    assert_parser_shaped(file, expected);
    assert_eq!(Validator::new().validate_syntax(file.syntax()), vec![]);
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

/// Continuation lines of a multi-line flow value copied under a deeper key
/// are shifted along with it, so they still clear the key's column.
#[test]
fn test_set_new_nested_key_to_multiline_flow_sequence() {
    let file = YamlFile::from_str("m:\n  k: 1\n").unwrap();
    let mapping = file.document().unwrap().as_mapping().unwrap();
    let nested = mapping.get_mapping("m").unwrap();
    nested.set("n", value_of("v: [a,\n b]\n", "v"));
    assert_valid_copy(&file, "m:\n  k: 1\n  n: [a,\n   b]\n");
}

#[test]
fn test_set_new_nested_key_to_multiline_flow_mapping_with_comment() {
    let file = YamlFile::from_str("m:\n  k: 1\n").unwrap();
    let mapping = file.document().unwrap().as_mapping().unwrap();
    let nested = mapping.get_mapping("m").unwrap();
    nested.set("n", value_of("v: {a: 1, # c\n b: 2}\n", "v"));
    assert_valid_copy(&file, "m:\n  k: 1\n  n: {a: 1, # c\n   b: 2}\n");
}

#[test]
fn test_set_new_key_in_sequence_item_to_multiline_flow_value() {
    let file = YamlFile::from_str("m:\n  - k: 1\n").unwrap();
    let mapping = file.document().unwrap().as_mapping().unwrap();
    let item = mapping.get_sequence("m").unwrap().get(0).unwrap();
    item.as_mapping()
        .unwrap()
        .set("n", value_of("v: [a,\n b]\n", "v"));
    assert_valid_copy(&file, "m:\n  - k: 1\n    n: [a,\n     b]\n");
}

#[test]
fn test_set_new_key_in_nested_flow_mapping_to_multiline_flow_value() {
    let file = YamlFile::from_str("m:\n  f: {x: 1}\n").unwrap();
    let mapping = file.document().unwrap().as_mapping().unwrap();
    let flow = mapping.get_mapping("m").unwrap().get_mapping("f").unwrap();
    flow.set("n", value_of("v: [a,\n b]\n", "v"));
    assert_valid_copy(&file, "m:\n  f: {x: 1, n: [a,\n      b]}\n");
}

#[test]
fn test_push_multiline_flow_value_to_nested_flow_sequence() {
    let file = YamlFile::from_str("m:\n  f: [x]\n").unwrap();
    let mapping = file.document().unwrap().as_mapping().unwrap();
    let flow = mapping.get_mapping("m").unwrap().get_sequence("f").unwrap();
    flow.push(value_of("v: [a,\n b]\n", "v"));
    assert_valid_copy(&file, "m:\n  f: [x, [a,\n   b]]\n");
}

#[test]
fn test_set_root_key_to_tagged_multiline_flow_value() {
    let file = YamlFile::from_str("k: 1\n").unwrap();
    let mapping = file.document().unwrap().as_mapping().unwrap();
    mapping.set("n", value_of("v: !t [a,\n b]\n", "v"));
    assert_valid_copy(&file, "k: 1\nn: !t [a,\n b]\n");
}

#[test]
fn test_set_new_nested_key_to_tagged_multiline_flow_value() {
    let file = YamlFile::from_str("m:\n  k: 1\n").unwrap();
    let mapping = file.document().unwrap().as_mapping().unwrap();
    let nested = mapping.get_mapping("m").unwrap();
    nested.set("n", value_of("v: !t [a,\n b]\n", "v"));
    assert_valid_copy(&file, "m:\n  k: 1\n  n: !t [a,\n   b]\n");
}

#[test]
fn test_set_sequence_item_to_tagged_multiline_flow_value() {
    let file = YamlFile::from_str("m:\n  - x\n").unwrap();
    let mapping = file.document().unwrap().as_mapping().unwrap();
    let seq = mapping.get_sequence("m").unwrap();
    seq.set(0, value_of("v: !t [a,\n b]\n", "v"));
    assert_valid_copy(&file, "m:\n  - !t [a,\n   b]\n");
}

/// A block value rewritten in flow form to keep a comment is placed one
/// level past the line holding it, not shifted twice.
#[test]
fn test_set_existing_flow_mapping_key_to_commented_block_value() {
    let file = YamlFile::from_str("a:\n  f: {n: 1}\n").unwrap();
    let mapping = file.document().unwrap().as_mapping().unwrap();
    let flow = mapping.get_mapping("a").unwrap().get_mapping("f").unwrap();
    flow.set("n", value_of("v:\n  x: 1 # c\n  y: 2\n", "v"));
    assert_valid_copy(&file, "a:\n  f: {n: {x: 1, # c\n        y: 2}}\n");
}

#[test]
fn test_set_flow_sequence_item_to_commented_block_value() {
    let file = YamlFile::from_str("a:\n  f: [1]\n").unwrap();
    let mapping = file.document().unwrap().as_mapping().unwrap();
    let flow = mapping.get_mapping("a").unwrap().get_sequence("f").unwrap();
    flow.set(0, value_of("v:\n  x: 1 # c\n  y: 2\n", "v"));
    assert_valid_copy(&file, "a:\n  f: [{x: 1, # c\n        y: 2}]\n");
}
