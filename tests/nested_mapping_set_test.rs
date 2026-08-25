//! Regression tests for issue #16: `Mapping::set` produces malformed YAML
//! when setting block sequences in nested mappings.

use std::str::FromStr;
use yaml_edit::{Mapping, MappingBuilder, Sequence, SequenceBuilder, YamlFile};

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
    assert_eq!(
        tf.to_string(),
        "outer:\n  inner:\n    replaced:\n      new_key: new\n      another: value\n    keep: yes\n"
    );
}

#[test]
fn block_sequence_in_root_mapping() {
    let (tf, root) = parse("activeBorderColor:\n  - \"#old\"\n  - bold\nother: keep\n");
    root.set("activeBorderColor", seq(&["\"#new\"", "bold"]));
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
    assert_eq!(
        tf.to_string(),
        "outer:\n  inner:\n    tgt:\n      a:\n        - x:\n            nested: 1\n          other: v\n"
    );
}

#[test]
fn inserts_new_block_key_with_proper_indent() {
    let (tf, root) = parse("outer:\n  inner:\n    old: v\n");
    dig(&root, &["outer", "inner"]).set("newkey", seq(&["a", "b"]));
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
    assert_eq!(tf.to_string(), "outer:\n  key: |\n    line1\n    line2\n");
}

#[test]
fn scalar_replaces_block_value_keeps_space() {
    let (tf, root) = parse("k:\n  - a\n  - b\n");
    root.set("k", "modified");
    assert_eq!(tf.to_string(), "k: modified\n");
}

#[test]
fn preserves_anchor_on_block_source() {
    // Anchors aren't reachable via the plain builders — parse the source.
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
    assert_eq!(
        tf.to_string(),
        "outer:\n  key: &myanchor\n    - a\n    - b\n"
    );
}

#[test]
fn reindents_tagged_block_value() {
    // Tags aren't reachable via the plain builders — parse the source.
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
    assert_eq!(tf.to_string(), "outer:\n  key: &a hello\n");
}

#[test]
fn scalar_replaces_block_value_with_anchor_or_tag() {
    for target in ["k: &a\n  - old\n", "k: !!seq\n  - old\n"] {
        let (tf, root) = parse(target);
        root.set("k", "new");
        assert_eq!(tf.to_string(), "k: new\n");
    }
}

#[test]
fn explicit_key_scalar_to_block_no_blank_line() {
    let (tf, root) = parse("? mykey\n: old_scalar\n");
    root.set("mykey", seq(&["a"]));
    assert_eq!(tf.to_string(), "? mykey\n:\n  - a\n");
}

#[test]
fn preserves_blank_line_between_entries() {
    let (tf, root) = parse("a: 1\n\nb: 2\n");
    root.set("a", seq(&["x"]));
    assert_eq!(tf.to_string(), "a:\n  - x\n\nb: 2\n");
}

#[test]
fn scalar_over_block_preserves_key_line_comment() {
    let (tf, root) = parse("k:  # sticky\n  - old\n");
    root.set("k", "new");
    assert_eq!(tf.to_string(), "k:  new  # sticky\n");
}

#[test]
fn round_trip_scalar_block_preserves_comment() {
    let (tf, root) = parse("key: initial  # sticky\n");
    // scalar → block
    root.set("key", seq(&["item0"]));
    assert_eq!(tf.to_string(), "key:  # sticky\n  - item0\n");
    // block → scalar
    root.set("key", "back");
    assert_eq!(tf.to_string(), "key: back  # sticky\n");
    // scalar → mapping. MappingBuilder passes string values through, so
    // numeric-looking string values get quoted to preserve their type.
    root.set("key", map(&[("a", "1"), ("b", "2")]));
    assert_eq!(tf.to_string(), "key:  # sticky\n  a: '1'\n  b: '2'\n");
}
