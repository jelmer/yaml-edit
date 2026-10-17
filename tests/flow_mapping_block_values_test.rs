//! Collections written into flow mappings, or used as keys, are rendered in
//! flow style, since block syntax is not allowed there.

use rowan::ast::AstNode;
use std::str::FromStr;
use yaml_edit::{debug, Mapping, ScalarValue, Sequence, YamlFile, YamlValue};

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

/// Check that `file` has the same tree shape as a re-parse of its text.
#[track_caller]
fn assert_parser_shaped(file: &YamlFile) {
    assert_file_cst_ok(file);
    let reparsed = YamlFile::from_str(&file.to_string()).unwrap();
    assert_eq!(
        debug::tree_to_string(file.syntax()),
        debug::tree_to_string(reparsed.syntax())
    );
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
    let value =
        parse_map("p: &a 1\nq: !t [*a, 'c, d']\nr: |\n  line\ns:\n  - x, y\n  - {k: v}\nt:\n");
    root.set("y", value);
    assert_parser_shaped(&file);
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

/// YamlValues with block content, and how each is written in flow style.
fn block_yaml_values() -> Vec<(YamlValue, &'static str)> {
    vec![
        (
            YamlValue::parse_raw("!local\n  a: 1\n  b: 2"),
            "!local {a: 1, b: 2}",
        ),
        (YamlValue::parse_raw("a: 1\nb: [x]\n"), "{a: 1, b: [x]}"),
        (YamlValue::parse_raw("- a\n- b, c\n"), "[a, \"b, c\"]"),
        (YamlValue::parse_raw("!!set\n  a: ~\n"), "!!set {a: null}"),
        (
            YamlValue::parse_raw("!!omap\n  - a: 1\n"),
            "!!omap [{a: 1}]",
        ),
        (YamlValue::parse_raw("!t\n  - a\n"), "!t [a]"),
        (
            YamlValue::Sequence(vec![YamlValue::Scalar(ScalarValue::literal("a\nb\n"))]),
            "[\"a\\nb\\n\"]",
        ),
    ]
}

#[test]
fn test_push_block_yaml_value_into_flow_sequence() {
    for (value, flow) in block_yaml_values() {
        let file = YamlFile::from_str("k: [1]\n").unwrap();
        let k = file
            .document()
            .unwrap()
            .as_mapping()
            .unwrap()
            .get_sequence("k")
            .unwrap();
        k.push(value);
        assert_parser_shaped(&file);
        assert_eq!(file.to_string(), format!("k: [1, {flow}]\n"));
    }
}

#[test]
fn test_set_block_yaml_value_in_flow_mapping() {
    for (value, flow) in block_yaml_values() {
        let file = YamlFile::from_str("k: {x: 1}\n").unwrap();
        let k = file
            .document()
            .unwrap()
            .as_mapping()
            .unwrap()
            .get_mapping("k")
            .unwrap();
        k.set("y", value);
        assert_parser_shaped(&file);
        assert_eq!(file.to_string(), format!("k: {{x: 1, y: {flow}}}\n"));
    }
}

#[test]
fn test_set_scalar_with_flow_indicators_in_flow_mapping() {
    for (value, flow) in [
        (ScalarValue::string("a, b"), "\"a, b\""),
        (ScalarValue::literal("x\ny\n"), "\"x\\ny\\n\""),
        (ScalarValue::string("plain"), "plain"),
    ] {
        let file = YamlFile::from_str("{x: 1}\n").unwrap();
        let root = file.document().unwrap().as_mapping().unwrap();
        root.set("y", YamlValue::Scalar(value));
        assert_parser_shaped(&file);
        assert_eq!(file.to_string(), format!("{{x: 1, y: {flow}}}\n"));
    }
}

#[test]
fn test_implicit_null_entries_keep_their_count() {
    for (block, flow, len) in [
        ("-\n", "[null]", 1),
        ("- a\n-\n", "[a, null]", 2),
        ("-\n- a\n", "[null, a]", 2),
    ] {
        let file = YamlFile::from_str("{x: 1}\n").unwrap();
        let root = file.document().unwrap().as_mapping().unwrap();
        root.set("y", parse_seq(block));
        assert_parser_shaped(&file);
        assert_eq!(file.to_string(), format!("{{x: 1, y: {flow}}}\n"));
        let reparsed = parse_map(&file.to_string());
        assert_eq!(reparsed.get_sequence("y").unwrap().len(), len);
    }
}

#[test]
fn test_decorated_implicit_nulls_stay_empty() {
    let file = YamlFile::from_str("{x: 1}\n").unwrap();
    let root = file.document().unwrap().as_mapping().unwrap();
    root.set("y", parse_seq("- &n\n- !t\n"));
    // TODO: check the tree shape too once the parser gives `[&n, !t]` the
    // same NULL token it gives the block form.
    assert_file_cst_ok(&file);
    assert_eq!(file.to_string(), "{x: 1, y: [&n, !t]}\n");
    let reparsed = parse_map(&file.to_string());
    assert_eq!(reparsed.get_sequence("y").unwrap().len(), 2);
}

#[test]
fn test_multiline_flow_value_keeps_indentation() {
    let source = parse_map("v: [a,\n    b]\n").get("v").unwrap();
    let file = YamlFile::from_str("k:\n  j: 1\n").unwrap();
    let k = file
        .document()
        .unwrap()
        .as_mapping()
        .unwrap()
        .get_mapping("k")
        .unwrap();
    k.set("n", &source);
    assert_parser_shaped(&file);
    assert_eq!(file.to_string(), "k:\n  j: 1\n  n: [a,\n    b]\n");
}

#[test]
fn test_comments_kept_in_flow_rendering() {
    let file = YamlFile::from_str("{x: 1}\n").unwrap();
    let root = file.document().unwrap().as_mapping().unwrap();
    root.set("y", parse_map("p: 1 # keep me\nq: 2\n"));
    assert_parser_shaped(&file);
    assert_eq!(file.to_string(), "{x: 1, y: {p: 1, # keep me\n  q: 2}}\n");
}

#[test]
fn test_comments_in_nested_and_last_entries_kept() {
    let file = YamlFile::from_str("a: {x: 1}\n").unwrap();
    let a = file
        .document()
        .unwrap()
        .as_mapping()
        .unwrap()
        .get_mapping("a")
        .unwrap();
    a.set(
        "y",
        parse_map("p:  # head\n  - 1  # one\n  # own line\n  - 2\nq: 3  # last\n# trailing\n"),
    );
    assert_parser_shaped(&file);
    assert_eq!(
        file.to_string(),
        "a: {x: 1, y: {p: [  # head\n       1,  # one\n       # own line\n       2], q: 3  # last\n     # trailing\n     }}\n"
    );
}

#[test]
fn test_commented_sequence_key_uses_explicit_key() {
    let file = YamlFile::from_str("a: 1\n").unwrap();
    let root = file.document().unwrap().as_mapping().unwrap();
    let key = parse_seq("- 1  # one\n- 2\n");
    root.set(&key, 1);
    assert_parser_shaped(&file);
    assert_eq!(file.to_string(), "a: 1\n? [1,  # one\n  2]\n: 1\n");
}
