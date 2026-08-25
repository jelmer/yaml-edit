//! Regression tests for issue #16: `Mapping::set` produces malformed YAML
//! when setting block sequences in nested mappings.

use std::str::FromStr;
use yaml_edit::YamlFile;

#[test]
fn set_block_sequence_in_nested_mapping() {
    let target = "\
gui:
  theme:
    activeBorderColor:
      - \"#old\"
      - bold
    other: keep
";
    let source_yaml = "\
activeBorderColor:
  - \"#new\"
  - bold
";

    let target_file = YamlFile::from_str(target).unwrap();
    let source_file = YamlFile::from_str(source_yaml).unwrap();

    let source_map = source_file.document().unwrap().as_mapping().unwrap();
    let source_node = source_map.get("activeBorderColor").unwrap();

    let doc = target_file.document().unwrap();
    let gui = doc.as_mapping().unwrap().get_mapping("gui").unwrap();
    let theme = gui.get_mapping("theme").unwrap();
    theme.set("activeBorderColor", &source_node);

    let expected = "\
gui:
  theme:
    activeBorderColor:
      - \"#new\"
      - bold
    other: keep
";
    assert_eq!(target_file.to_string(), expected);

    let reparsed = YamlFile::from_str(&target_file.to_string()).unwrap();
    assert!(reparsed.document().is_some());
}

#[test]
fn set_block_mapping_in_nested_mapping() {
    let target = "\
outer:
  inner:
    replaced:
      old_key: old
    keep: yes
";
    let source_yaml = "\
replaced:
  new_key: new
  another: value
";

    let target_file = YamlFile::from_str(target).unwrap();
    let source_file = YamlFile::from_str(source_yaml).unwrap();

    let source_map = source_file.document().unwrap().as_mapping().unwrap();
    let source_node = source_map.get("replaced").unwrap();

    let doc = target_file.document().unwrap();
    let outer = doc.as_mapping().unwrap().get_mapping("outer").unwrap();
    let inner = outer.get_mapping("inner").unwrap();
    inner.set("replaced", &source_node);

    let expected = "\
outer:
  inner:
    replaced:
      new_key: new
      another: value
    keep: yes
";
    assert_eq!(target_file.to_string(), expected);
}

#[test]
fn set_block_sequence_in_root_mapping() {
    let target = "\
activeBorderColor:
  - \"#old\"
  - bold
other: keep
";
    let source_yaml = "\
activeBorderColor:
  - \"#new\"
  - bold
";

    let target_file = YamlFile::from_str(target).unwrap();
    let source_file = YamlFile::from_str(source_yaml).unwrap();

    let source_map = source_file.document().unwrap().as_mapping().unwrap();
    let source_node = source_map.get("activeBorderColor").unwrap();

    let doc = target_file.document().unwrap();
    let root = doc.as_mapping().unwrap();
    root.set("activeBorderColor", &source_node);

    let expected = "\
activeBorderColor:
  - \"#new\"
  - bold
other: keep
";
    assert_eq!(target_file.to_string(), expected);
}

#[test]
fn set_replaces_inline_scalar_with_block_sequence() {
    // Old value is an inline scalar (`key: old`). Replacing with a block
    // sequence must drop the WHITESPACE between COLON and the old VALUE
    // (otherwise `key:` gets a stray trailing space) and must not duplicate
    // the entry's trailing NEWLINE (the block value carries its own).
    let target = "a: 1\nb: 2\nc: 3\n";
    let target_file = YamlFile::from_str(target).unwrap();

    let source = "k:\n  - x\n  - y\n";
    let source_file = YamlFile::from_str(source).unwrap();
    let src = source_file
        .document()
        .unwrap()
        .as_mapping()
        .unwrap()
        .get("k")
        .unwrap();

    target_file
        .document()
        .unwrap()
        .as_mapping()
        .unwrap()
        .set("b", &src);
    let expected = "a: 1\nb:\n  - x\n  - y\nc: 3\n";
    assert_eq!(target_file.to_string(), expected);
}

#[test]
fn set_preserves_relative_indent_of_nested_source() {
    // Source has nested structure inside its top-level mapping. When placed
    // at a deeper column, the relative indentation between nested levels
    // must be preserved.
    let source = "root:\n  a:\n    - x:\n        nested: 1\n      other: v\n";
    let source_file = YamlFile::from_str(source).unwrap();
    let src = source_file
        .document()
        .unwrap()
        .as_mapping()
        .unwrap()
        .get("root")
        .unwrap();

    let target = "outer:\n  inner:\n    tgt: old\n";
    let target_file = YamlFile::from_str(target).unwrap();
    target_file
        .document()
        .unwrap()
        .as_mapping()
        .unwrap()
        .get_mapping("outer")
        .unwrap()
        .get_mapping("inner")
        .unwrap()
        .set("tgt", &src);

    let expected =
        "outer:\n  inner:\n    tgt:\n      a:\n        - x:\n            nested: 1\n          other: v\n";
    assert_eq!(target_file.to_string(), expected);
}

#[test]
fn set_inserts_new_block_key_with_proper_indent() {
    // Setting a NEW key (not replacing) with a node-backed block value
    // must also re-indent the source content to line up under the new
    // entry's column. Previously this went through MappingEntry::new
    // which did a verbatim copy and produced inconsistent indentation.
    let target = "outer:\n  inner:\n    old: v\n";
    let target_file = YamlFile::from_str(target).unwrap();
    let source = "k:\n  - a\n  - b\n";
    let source_file = YamlFile::from_str(source).unwrap();
    let src = source_file
        .document()
        .unwrap()
        .as_mapping()
        .unwrap()
        .get("k")
        .unwrap();

    target_file
        .document()
        .unwrap()
        .as_mapping()
        .unwrap()
        .get_mapping("outer")
        .unwrap()
        .get_mapping("inner")
        .unwrap()
        .set("newkey", &src);
    let expected = "outer:\n  inner:\n    old: v\n    newkey:\n      - a\n      - b\n";
    assert_eq!(target_file.to_string(), expected);
}

#[test]
fn set_preserves_inline_comment_on_replaced_value() {
    // When the old value had a trailing inline comment (`key: old # note`),
    // the comment survives the switch to a block value by moving up to the
    // `key:` line.
    let target = "outer:\n  key: old  # important comment\n";
    let target_file = YamlFile::from_str(target).unwrap();
    let source = "k:\n  - a\n  - b\n";
    let source_file = YamlFile::from_str(source).unwrap();
    let src = source_file
        .document()
        .unwrap()
        .as_mapping()
        .unwrap()
        .get("k")
        .unwrap();

    target_file
        .document()
        .unwrap()
        .as_mapping()
        .unwrap()
        .get_mapping("outer")
        .unwrap()
        .set("key", &src);
    let expected = "outer:\n  key:  # important comment\n    - a\n    - b\n";
    assert_eq!(target_file.to_string(), expected);
}

#[test]
fn set_replaces_scalar_with_literal_block_scalar() {
    // Block scalars (`|` and `>`) are structurally different from block
    // collections: the indicator stays on the key line and content follows
    // on subsequent lines, indented from the key by 2.
    let source = "k: |\n  line1\n  line2\n";
    let source_file = YamlFile::from_str(source).unwrap();
    let src = source_file
        .document()
        .unwrap()
        .as_mapping()
        .unwrap()
        .get("k")
        .unwrap();

    let target = "outer:\n  key: old\n";
    let target_file = YamlFile::from_str(target).unwrap();
    target_file
        .document()
        .unwrap()
        .as_mapping()
        .unwrap()
        .get_mapping("outer")
        .unwrap()
        .set("key", &src);
    let expected = "outer:\n  key: |\n    line1\n    line2\n";
    assert_eq!(target_file.to_string(), expected);
}

#[test]
fn set_scalar_replaces_block_value_keeps_space() {
    // Replacing a block sequence (or mapping) with an inline scalar must
    // insert the required WHITESPACE between COLON and the new VALUE and
    // append a trailing NEWLINE.
    let src = "k:\n  - a\n  - b\n";
    let sf = YamlFile::from_str(src).unwrap();
    sf.document()
        .unwrap()
        .as_mapping()
        .unwrap()
        .set("k", "modified");
    assert_eq!(sf.to_string(), "k: modified\n");
}

#[test]
fn set_preserves_anchor_on_block_source() {
    // A source with a leading `&anchor` before its block content keeps
    // the anchor on the key line so aliases still resolve.
    let source = "k: &myanchor\n  - a\n  - b\n";
    let source_file = YamlFile::from_str(source).unwrap();
    let src = source_file
        .document()
        .unwrap()
        .as_mapping()
        .unwrap()
        .get("k")
        .unwrap();

    let target = "outer:\n  key: old\n";
    let target_file = YamlFile::from_str(target).unwrap();
    target_file
        .document()
        .unwrap()
        .as_mapping()
        .unwrap()
        .get_mapping("outer")
        .unwrap()
        .set("key", &src);
    let expected = "outer:\n  key: &myanchor\n    - a\n    - b\n";
    assert_eq!(target_file.to_string(), expected);
}

#[test]
fn set_reindents_tagged_block_value() {
    // Tagged values (!!set, !!omap, custom !tag) keep the tag on the key
    // line and re-indent the wrapped block content under the new column.
    let source = "k: !!omap\n  - alpha: 1\n  - beta: 2\n";
    let source_file = YamlFile::from_str(source).unwrap();
    let src = source_file
        .document()
        .unwrap()
        .as_mapping()
        .unwrap()
        .get("k")
        .unwrap();

    let target = "outer:\n  key: old\n";
    let target_file = YamlFile::from_str(target).unwrap();
    target_file
        .document()
        .unwrap()
        .as_mapping()
        .unwrap()
        .get_mapping("outer")
        .unwrap()
        .set("key", &src);
    let expected = "outer:\n  key: !!omap\n    - alpha: 1\n    - beta: 2\n";
    assert_eq!(target_file.to_string(), expected);
}

#[test]
fn set_preserves_anchor_on_inline_source() {
    // Anchor on an inline scalar (`&a hello`) survives replacement into
    // another entry. The default `set_value` path now checks the source
    // for a preceding `&anchor` and inserts it before the new value.
    let source = "k: &a hello\n";
    let source_file = YamlFile::from_str(source).unwrap();
    let src = source_file
        .document()
        .unwrap()
        .as_mapping()
        .unwrap()
        .get("k")
        .unwrap();
    let target = "outer:\n  key: old\n";
    let target_file = YamlFile::from_str(target).unwrap();
    target_file
        .document()
        .unwrap()
        .as_mapping()
        .unwrap()
        .get_mapping("outer")
        .unwrap()
        .set("key", &src);
    assert_eq!(target_file.to_string(), "outer:\n  key: &a hello\n");
}

#[test]
fn set_scalar_replaces_block_value_with_anchor_or_tag() {
    // The block-old-value detector should recognise block form even when
    // it's fronted by an ANCHOR or a TAGGED_NODE — replacing with an
    // inline scalar has to drop the old space+block and rebuild with a
    // single space and a trailing newline.
    let target = "k: &a\n  - old\n";
    let tf = YamlFile::from_str(target).unwrap();
    tf.document().unwrap().as_mapping().unwrap().set("k", "new");
    assert_eq!(tf.to_string(), "k: new\n");

    let target = "k: !!seq\n  - old\n";
    let tf = YamlFile::from_str(target).unwrap();
    tf.document().unwrap().as_mapping().unwrap().set("k", "new");
    assert_eq!(tf.to_string(), "k: new\n");
}

#[test]
fn set_explicit_key_scalar_to_block_no_blank_line() {
    // Explicit-key entries have their trailing NEWLINE at the parent MAPPING
    // level (not inside the MAPPING_ENTRY). When we replace the value with
    // a block whose own content ends with NEWLINE, that sibling NEWLINE
    // would render as a spurious blank line — we drop it.
    let target = "? mykey\n: old_scalar\n";
    let tf = YamlFile::from_str(target).unwrap();
    let source = "k:\n  - a\n";
    let sf = YamlFile::from_str(source).unwrap();
    let src = sf
        .document()
        .unwrap()
        .as_mapping()
        .unwrap()
        .get("k")
        .unwrap();
    tf.document()
        .unwrap()
        .as_mapping()
        .unwrap()
        .set("mykey", &src);
    assert_eq!(tf.to_string(), "? mykey\n:\n  - a\n");
}

#[test]
fn set_preserves_blank_line_between_entries() {
    // The trailing-blank-line cleanup must not touch a genuine blank line
    // between two entries.
    let target = "a: 1\n\nb: 2\n";
    let tf = YamlFile::from_str(target).unwrap();
    let source = "s:\n  - x\n";
    let sf = YamlFile::from_str(source).unwrap();
    let src = sf
        .document()
        .unwrap()
        .as_mapping()
        .unwrap()
        .get("s")
        .unwrap();
    tf.document().unwrap().as_mapping().unwrap().set("a", &src);
    assert!(tf.to_string().contains("\n\nb: 2"));
}
