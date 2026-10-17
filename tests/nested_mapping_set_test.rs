//! Regression tests for issue #16: `Mapping::set` produces malformed YAML
//! when setting block sequences in nested mappings.

use rowan::ast::AstNode;
use std::str::FromStr;
use yaml_edit::{debug, Mapping, MappingBuilder, Sequence, SequenceBuilder, YamlFile, YamlValue};

mod common;
use common::{assert_cst_ok, assert_file_cst_ok};

/// Parse `yaml` and return the file plus its root mapping.
fn parse(yaml: &str) -> (YamlFile, Mapping) {
    let file = YamlFile::from_str(yaml).unwrap();
    let root = file.document().unwrap().as_mapping().unwrap();
    (file, root)
}

/// Navigate into nested mappings via successive `get_mapping` calls.
fn dig(root: &Mapping, path: &[&str]) -> Mapping {
    let mut m = root.clone();
    for seg in path {
        m = m.get_mapping(seg).unwrap();
    }
    m
}

/// Build a standalone sequence of scalar items.
fn seq(items: &[&str]) -> Sequence {
    let mut b = SequenceBuilder::new();
    for it in items {
        b = b.item(*it);
    }
    b.build_document().as_sequence().unwrap()
}

/// Build a standalone mapping of scalar pairs.
fn map(pairs: &[(&str, &str)]) -> Mapping {
    let mut b = MappingBuilder::new();
    for (k, v) in pairs {
        b = b.pair(*k, *v);
    }
    b.build_document().as_mapping().unwrap()
}

#[test]
fn block_sequence_in_nested_mapping() {
    let (tf, root) = parse(
        "gui:\n  theme:\n    activeBorderColor:\n      - \"#old\"\n      - bold\n    other: keep\n",
    );
    dig(&root, &["gui", "theme"]).set("activeBorderColor", seq(&["\"#new\"", "bold"]));
    assert_file_cst_ok(&tf);
    // See block_sequence_in_root_mapping about the single-quoting of `"#new"`.
    assert_eq!(
        tf.to_string(),
        "gui:\n  theme:\n    activeBorderColor:\n      - '\"#new\"'\n      - bold\n    other: keep\n"
    );
}

#[test]
fn block_mapping_in_nested_mapping() {
    let (tf, root) = parse("outer:\n  inner:\n    replaced:\n      old_key: old\n    keep: yes\n");
    dig(&root, &["outer", "inner"])
        .set("replaced", map(&[("new_key", "new"), ("another", "value")]));
    assert_file_cst_ok(&tf);
    assert_eq!(
        tf.to_string(),
        "outer:\n  inner:\n    replaced:\n      new_key: new\n      another: value\n    keep: yes\n"
    );
}

#[test]
fn block_sequence_in_root_mapping() {
    let (tf, root) = parse("activeBorderColor:\n  - \"#old\"\n  - bold\nother: keep\n");
    root.set("activeBorderColor", seq(&["\"#new\"", "bold"]));
    assert_file_cst_ok(&tf);
    // The scalar `"#new"` starts with `#`, so the SequenceBuilder wraps
    // it in single quotes to keep it a scalar rather than a comment start.
    assert_eq!(
        tf.to_string(),
        "activeBorderColor:\n  - '\"#new\"'\n  - bold\nother: keep\n"
    );
}

#[test]
fn replaces_inline_scalar_with_block_sequence() {
    // Old value is an inline scalar. Replacing with a block sequence must
    // drop the WHITESPACE between COLON and the old VALUE and must not
    // duplicate the entry's trailing NEWLINE.
    let (tf, root) = parse("a: 1\nb: 2\nc: 3\n");
    root.set("b", seq(&["x", "y"]));
    assert_file_cst_ok(&tf);
    assert_eq!(tf.to_string(), "a: 1\nb:\n  - x\n  - y\nc: 3\n");
}

#[test]
fn preserves_relative_indent_of_nested_source() {
    // Placing a nested block into a deeper column preserves relative indent.
    // (Uses a parsed source because the shape isn't reachable via the
    // scalar-only builders above.)
    let src_file =
        YamlFile::from_str("root:\n  a:\n    - x:\n        nested: 1\n      other: v\n").unwrap();
    let src = src_file
        .document()
        .unwrap()
        .as_mapping()
        .unwrap()
        .get("root")
        .unwrap();

    let (tf, root) = parse("outer:\n  inner:\n    tgt: old\n");
    dig(&root, &["outer", "inner"]).set("tgt", &src);
    assert_file_cst_ok(&tf);
    assert_eq!(
        tf.to_string(),
        "outer:\n  inner:\n    tgt:\n      a:\n        - x:\n            nested: 1\n          other: v\n"
    );
}

#[test]
fn inserts_new_block_key_with_proper_indent() {
    let (tf, root) = parse("outer:\n  inner:\n    old: v\n");
    dig(&root, &["outer", "inner"]).set("newkey", seq(&["a", "b"]));
    assert_file_cst_ok(&tf);
    assert_eq!(
        tf.to_string(),
        "outer:\n  inner:\n    old: v\n    newkey:\n      - a\n      - b\n"
    );
}

#[test]
fn preserves_inline_comment_on_replaced_value() {
    // Comment on the old inline value line survives the switch to a block
    // value by moving up to the `key:` line.
    let (tf, root) = parse("outer:\n  key: old  # important comment\n");
    dig(&root, &["outer"]).set("key", seq(&["a", "b"]));
    assert_file_cst_ok(&tf);
    assert_eq!(
        tf.to_string(),
        "outer:\n  key:  # important comment\n    - a\n    - b\n"
    );
}

#[test]
fn replaces_scalar_with_literal_block_scalar() {
    // Block scalars (`|` and `>`) keep the indicator on the key line.
    // The block-scalar shape isn't reachable via the plain builders, so
    // this one parses its source.
    let src_file = YamlFile::from_str("k: |\n  line1\n  line2\n").unwrap();
    let src = src_file
        .document()
        .unwrap()
        .as_mapping()
        .unwrap()
        .get("k")
        .unwrap();
    let (tf, root) = parse("outer:\n  key: old\n");
    dig(&root, &["outer"]).set("key", &src);
    assert_file_cst_ok(&tf);
    assert_eq!(tf.to_string(), "outer:\n  key: |\n    line1\n    line2\n");
}

#[test]
fn scalar_replaces_block_value_keeps_space() {
    let (tf, root) = parse("k:\n  - a\n  - b\n");
    root.set("k", "modified");
    assert_file_cst_ok(&tf);
    assert_eq!(tf.to_string(), "k: modified\n");
}

#[test]
fn preserves_anchor_on_block_source() {
    // Anchors aren't reachable via the plain builders - parse the source.
    let src_file = YamlFile::from_str("k: &myanchor\n  - a\n  - b\n").unwrap();
    let src = src_file
        .document()
        .unwrap()
        .as_mapping()
        .unwrap()
        .get("k")
        .unwrap();
    let (tf, root) = parse("outer:\n  key: old\n");
    dig(&root, &["outer"]).set("key", &src);
    assert_file_cst_ok(&tf);
    assert_eq!(
        tf.to_string(),
        "outer:\n  key: &myanchor\n    - a\n    - b\n"
    );
}

#[test]
fn reindents_tagged_block_value() {
    // Tags aren't reachable via the plain builders - parse the source.
    let src_file = YamlFile::from_str("k: !!omap\n  - alpha: 1\n  - beta: 2\n").unwrap();
    let src = src_file
        .document()
        .unwrap()
        .as_mapping()
        .unwrap()
        .get("k")
        .unwrap();
    let (tf, root) = parse("outer:\n  key: old\n");
    dig(&root, &["outer"]).set("key", &src);
    assert_file_cst_ok(&tf);
    assert_eq!(
        tf.to_string(),
        "outer:\n  key: !!omap\n    - alpha: 1\n    - beta: 2\n"
    );
}

#[test]
fn preserves_anchor_on_inline_source() {
    let src_file = YamlFile::from_str("k: &a hello\n").unwrap();
    let src = src_file
        .document()
        .unwrap()
        .as_mapping()
        .unwrap()
        .get("k")
        .unwrap();
    let (tf, root) = parse("outer:\n  key: old\n");
    dig(&root, &["outer"]).set("key", &src);
    assert_file_cst_ok(&tf);
    assert_eq!(tf.to_string(), "outer:\n  key: &a hello\n");
}

#[test]
fn scalar_replaces_block_value_with_anchor_or_tag() {
    for (target, expected) in [
        ("k: &a\n  - old\n", "k: &a new\n"),
        ("k: !!seq\n  - old\n", "k: new\n"),
    ] {
        let (tf, root) = parse(target);
        root.set("k", "new");
        assert_file_cst_ok(&tf);
        assert_eq!(tf.to_string(), expected);
    }
}

#[test]
fn explicit_key_scalar_to_block_no_blank_line() {
    let (tf, root) = parse("? mykey\n: old_scalar\n");
    root.set("mykey", seq(&["a"]));
    assert_file_cst_ok(&tf);
    assert_eq!(tf.to_string(), "? mykey\n:\n  - a\n");
}

#[test]
fn preserves_blank_line_between_entries() {
    let (tf, root) = parse("a: 1\n\nb: 2\n");
    root.set("a", seq(&["x"]));
    assert_file_cst_ok(&tf);
    assert_eq!(tf.to_string(), "a:\n  - x\n\nb: 2\n");
}

/// A block scalar's body owns the line break that ends the entry, so an
/// inline value replacing it needs one of its own.
#[test]
fn scalar_over_block_scalar_keeps_line_break() {
    for (src, path, expected) in [
        ("k: |\n  text\nb: 2\n", &[][..], "k: 5\nb: 2\n"),
        ("k: >-\n  text\n", &[], "k: 5\n"),
        (
            "o:\n  k: |\n    text\n  b: 2\n",
            &["o"],
            "o:\n  k: 5\n  b: 2\n",
        ),
        ("? k\n: |\n  text\nb: 2\n", &[], "? k\n: 5\nb: 2\n"),
    ] {
        let (tf, root) = parse(src);
        dig(&root, path).set("k", 5);
        assert_file_cst_ok(&tf);
        assert_eq!(tf.to_string(), expected, "{src:?}");
        let reparsed = YamlFile::from_str(expected).unwrap();
        assert_eq!(
            debug::tree_to_string(tf.syntax()),
            debug::tree_to_string(reparsed.syntax()),
            "{src:?}"
        );
    }
}

#[test]
fn scalar_over_block_preserves_key_line_comment() {
    let (tf, root) = parse("k:  # sticky\n  - old\n");
    root.set("k", "new");
    assert_file_cst_ok(&tf);
    assert_eq!(tf.to_string(), "k: new  # sticky\n");
}

/// Replacing a commented block value with a scalar moves it onto the key
/// line, leaving what precedes the colon alone and giving the tree the
/// parser's shape.
#[test]
fn scalar_over_block_on_key_line() {
    for (src, path, expected) in [
        ("k :  # c\n  x: 1\n", &[][..], "k : 5  # c\n"),
        ("k:   \n  x: 1\n", &[], "k: 5\n"),
        (
            "o:\n  k:  # c\n    x: 1\n  b: 2\n",
            &["o"],
            "o:\n  k: 5  # c\n  b: 2\n",
        ),
    ] {
        let (tf, root) = parse(src);
        dig(&root, path).set("k", 5);
        assert_file_cst_ok(&tf);
        assert_eq!(tf.to_string(), expected, "{src:?}");
        let reparsed = YamlFile::from_str(expected).unwrap();
        assert_eq!(
            debug::tree_to_string(tf.syntax()),
            debug::tree_to_string(reparsed.syntax()),
            "{src:?}"
        );
    }
}

#[test]
fn set_path_creates_intermediate_mappings_with_indent() {
    // Regression for issue #18: set_path drilling through several missing
    // intermediate mappings must indent each level correctly and must not
    // leave stray blank lines behind.
    use yaml_edit::path::YamlPath;
    use yaml_edit::Document;

    let doc = Document::from_str("managers:\n  pm:\n    runtime: claude-code\n").unwrap();
    doc.try_set_path("managers.pm.interfaces.telegram.bot_token_env", "TOKEN")
        .expect("set_path");
    assert_cst_ok(&doc);
    assert_eq!(
        doc.to_string(),
        "managers:\n  pm:\n    runtime: claude-code\n    interfaces:\n      telegram:\n        bot_token_env: TOKEN\n"
    );
}

#[test]
fn round_trip_scalar_block_preserves_comment() {
    let (tf, root) = parse("key: initial  # sticky\n");
    // scalar to block
    root.set("key", seq(&["item0"]));
    assert_file_cst_ok(&tf);
    assert_eq!(tf.to_string(), "key:  # sticky\n  - item0\n");
    // block to scalar
    root.set("key", "back");
    assert_file_cst_ok(&tf);
    assert_eq!(tf.to_string(), "key: back  # sticky\n");
    // scalar to mapping. MappingBuilder passes string values through, so
    // numeric-looking string values get quoted to preserve their type.
    root.set("key", map(&[("a", "1"), ("b", "2")]));
    assert_file_cst_ok(&tf);
    assert_eq!(tf.to_string(), "key:  # sticky\n  a: '1'\n  b: '2'\n");
}

/// Block YamlValues set as `k` (existing) and `n` (new) in a root mapping
/// and in a nested one, with the expected output for each.
#[test]
fn block_yaml_values_in_mappings() {
    let cases = [
        (
            YamlValue::parse_raw("!local\n  a: 1\n  b: 2").unwrap(),
            "k: !local\n  a: 1\n  b: 2\nj: 1\n",
            "k: 1\nj: 1\nn: !local\n  a: 1\n  b: 2\n",
            "p:\n  k: !local\n    a: 1\n    b: 2\n  j: 1\n",
            "p:\n  k: 1\n  j: 1\n  n: !local\n    a: 1\n    b: 2\n",
        ),
        (
            YamlValue::parse_raw("a: 1\nb: [x]\n").unwrap(),
            "k:\n  a: 1\n  b:\n    - x\nj: 1\n",
            "k: 1\nj: 1\nn:\n  a: 1\n  b:\n    - x\n",
            "p:\n  k:\n    a: 1\n    b:\n      - x\n  j: 1\n",
            "p:\n  k: 1\n  j: 1\n  n:\n    a: 1\n    b:\n      - x\n",
        ),
        (
            YamlValue::parse_raw("- a\n- b\n").unwrap(),
            "k:\n  - a\n  - b\nj: 1\n",
            "k: 1\nj: 1\nn:\n- a\n- b\n",
            "p:\n  k:\n    - a\n    - b\n  j: 1\n",
            "p:\n  k: 1\n  j: 1\n  n:\n    - a\n    - b\n",
        ),
        (
            YamlValue::parse_raw("!!set\n  a: ~\n").unwrap(),
            "k: !!set\n  a: null\nj: 1\n",
            "k: 1\nj: 1\nn: !!set\n  a: null\n",
            "p:\n  k: !!set\n    a: null\n  j: 1\n",
            "p:\n  k: 1\n  j: 1\n  n: !!set\n    a: null\n",
        ),
    ];
    for (value, root_k, root_n, nested_k, nested_n) in cases {
        for (src, path, key, expected) in [
            ("k: 1\nj: 1\n", &[][..], "k", root_k),
            ("k: 1\nj: 1\n", &[][..], "n", root_n),
            ("p:\n  k: 1\n  j: 1\n", &["p"][..], "k", nested_k),
            ("p:\n  k: 1\n  j: 1\n", &["p"][..], "n", nested_n),
        ] {
            let (file, root) = parse(src);
            dig(&root, path).set(key, value.clone());
            assert_file_cst_ok(&file);
            assert_eq!(file.to_string(), expected);
            let reparsed = YamlFile::from_str(expected).unwrap();
            assert_eq!(
                debug::tree_to_string(file.syntax()),
                debug::tree_to_string(reparsed.syntax()),
                "{expected:?}"
            );
        }
    }
}

#[test]
fn parsed_block_mapping_as_new_root_key() {
    let (_, source) = parse("a: 1\nb: 2\n");
    let (file, root) = parse("k: 1\n");
    root.set("n", &source);
    assert_file_cst_ok(&file);
    assert_eq!(file.to_string(), "k: 1\nn:\n  a: 1\n  b: 2\n");
}
