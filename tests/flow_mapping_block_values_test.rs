//! Collections written into flow mappings, or used as keys, are rendered in
//! flow style, since block syntax is not allowed there.

use std::str::FromStr;
use yaml_edit::{Mapping, Sequence, YamlFile, YamlValue};

mod common;
use common::assert_file_cst_ok;

fn parse_seq(yaml: &str) -> Sequence {
    YamlFile::from_str(yaml)
        .unwrap()
        .document()
        .unwrap()
        .as_sequence()
        .unwrap()
}

fn parse_map(yaml: &str) -> Mapping {
    YamlFile::from_str(yaml)
        .unwrap()
        .document()
        .unwrap()
        .as_mapping()
        .unwrap()
}

#[test]
fn test_set_sequence_in_flow_mapping() {
    let file = YamlFile::from_str("{x: 1}\n").unwrap();
    let root = file.document().unwrap().as_mapping().unwrap();
    root.set("y", parse_seq("- 1\n- 2\n"));
    assert_file_cst_ok(&file);
    assert_eq!(file.to_string(), "{x: 1, y: [1, 2]}\n");
    let reparsed = parse_map(&file.to_string());
    assert_eq!(reparsed.get_sequence("y").unwrap().len(), 2);
}

#[test]
fn test_set_mapping_in_nested_flow_mapping() {
    let file = YamlFile::from_str("a: {x: 1}").unwrap();
    let root = file.document().unwrap().as_mapping().unwrap();
    let a = root.get_mapping("a").unwrap();
    a.set("y", parse_map("p: 1\nq: 2\n"));
    assert_file_cst_ok(&file);
    assert_eq!(file.to_string(), "a: {x: 1, y: {p: 1, q: 2}}");
    let reparsed = parse_map(&file.to_string());
    let y = reparsed.get_mapping("a").unwrap().get_mapping("y").unwrap();
    assert_eq!(
        y.keys().map(|k| k.to_string()).collect::<Vec<_>>(),
        ["p", "q"]
    );
}

#[test]
fn test_replace_scalar_with_sequence_in_flow_mapping() {
    let file = YamlFile::from_str("{x: 1, y: 2}\n").unwrap();
    let root = file.document().unwrap().as_mapping().unwrap();
    root.set("x", parse_seq("- a\n- b\n"));
    assert_file_cst_ok(&file);
    assert_eq!(file.to_string(), "{x: [a, b], y: 2}\n");
}

#[test]
fn test_flow_rendering_keeps_anchors_tags_and_quotes() {
    let file = YamlFile::from_str("{x: 1}\n").unwrap();
    let root = file.document().unwrap().as_mapping().unwrap();
    let value = parse_map(
        "p: &a 1 # comment\nq: !t [*a, 'c, d']\nr: |\n  line\ns:\n  - x, y\n  - {k: v}\nt:\n",
    );
    root.set("y", value);
    assert_file_cst_ok(&file);
    assert_eq!(
        file.to_string(),
        "{x: 1, y: {p: &a 1, q: !t [*a, 'c, d'], r: \"line\\n\", s: [\"x, y\", {k: v}], t: null}}\n"
    );
}

#[test]
fn test_insert_yaml_value_sequence_in_flow_mapping() {
    let file = YamlFile::from_str("{x: 1}\n").unwrap();
    let root = file.document().unwrap().as_mapping().unwrap();
    let value: Vec<YamlValue> = vec![YamlValue::scalar(1), YamlValue::scalar("a")];
    root.insert_at_index(0, "y", value);
    assert_file_cst_ok(&file);
    assert_eq!(file.to_string(), "{y: [1, a], x: 1}\n");
}

#[test]
fn test_sequence_key_in_block_mapping() {
    let file = YamlFile::from_str("a: 1\n").unwrap();
    let root = file.document().unwrap().as_mapping().unwrap();
    let key = parse_seq("- 1\n- 2\n");
    root.set(&key, 1);
    assert_file_cst_ok(&file);
    assert_eq!(file.to_string(), "a: 1\n[1, 2]: 1\n");
    let reparsed = parse_map(&file.to_string());
    assert_eq!(reparsed.len(), 2);
    assert!(reparsed.contains_key(&key));
}
