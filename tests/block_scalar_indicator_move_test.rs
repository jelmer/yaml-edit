//! A block scalar with an explicit indentation indicator (`|2`) moved to
//! another column. The indicator counts from the parent's column, so the
//! body has to move with the parent for the value to stay the same.

use rowan::ast::AstNode;
use std::str::FromStr;
use yaml_edit::path::YamlPath;
use yaml_edit::{debug, YamlFile, YamlNode};

mod common;
use common::assert_file_cst_ok;

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
    assert_eq!(file.to_string(), expected);
    assert_file_cst_ok(file);
    let reparsed = YamlFile::from_str(expected).unwrap();
    assert_eq!(
        debug::shape_to_string(file.syntax()),
        debug::shape_to_string(reparsed.syntax())
    );
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
