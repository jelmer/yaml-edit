//! A CST node passed as a key or value is copied in whole, wrapper
//! included, so the result has the shape the parser gives the same text.

use rowan::ast::AstNode;
use std::str::FromStr;
use yaml_edit::{debug, validator::Validator, MappingEntry, YamlFile, YamlNode};

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

/// Indent every non-blank line of `text` after the first by two more
/// columns.
fn shift(text: &str) -> String {
    text.split('\n')
        .enumerate()
        .map(|(i, line)| match (i, line) {
            (0, _) | (_, "") => line.to_string(),
            _ => format!("  {line}"),
        })
        .collect::<Vec<_>>()
        .join("\n")
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

/// The decoded value of the scalar at `key` in `src`.
fn scalar_value_of(src: &str, key: &str) -> String {
    value_of(src, key).as_scalar().unwrap().as_string()
}

/// Copy the scalar at `v` in `src` under a new key `n` in the mapping
/// `m` of `target`, checking the result and that the value survived.
#[track_caller]
fn assert_scalar_copied_to_nested_key(src: &str, target: &str, expected: &str) {
    let file = YamlFile::from_str(target).unwrap();
    let mapping = file.document().unwrap().as_mapping().unwrap();
    mapping
        .get_mapping("m")
        .unwrap()
        .set("n", value_of(src, "v"));
    assert_valid_copy(&file, expected);
    let copied = mapping.get_mapping("m").unwrap().get("n").unwrap();
    assert_eq!(
        copied.as_scalar().unwrap().as_string(),
        scalar_value_of(src, "v")
    );
}

/// Continuation lines of a multi-line quoted scalar copied under a deeper
/// key are shifted with it, so they still clear the key's column.
#[test]
fn test_set_new_nested_key_to_multiline_double_quoted_scalar() {
    assert_scalar_copied_to_nested_key(
        "v: \"a\n b\"\n",
        "m:\n  k: 1\n",
        "m:\n  k: 1\n  n: \"a\n   b\"\n",
    );
}

#[test]
fn test_set_existing_nested_key_to_multiline_double_quoted_scalar() {
    assert_scalar_copied_to_nested_key("v: \"a\n b\"\n", "m:\n  n: 1\n", "m:\n  n: \"a\n   b\"\n");
}

#[test]
fn test_set_new_nested_key_to_multiline_single_quoted_scalar() {
    assert_scalar_copied_to_nested_key(
        "v: 'a\n\n b ''c'''\n",
        "m:\n  k: 1\n",
        "m:\n  k: 1\n  n: 'a\n\n   b ''c'''\n",
    );
}

/// Escaped line breaks, escapes at the start of a line and blank lines
/// all keep their meaning; only the line prefixes move.
#[test]
fn test_set_new_nested_key_to_double_quoted_scalar_with_escapes() {
    assert_scalar_copied_to_nested_key(
        "v: \"a \\\n  \\tb\n \n\n \\  c\\\n d\"\n",
        "m:\n  k: 1\n",
        "m:\n  k: 1\n  n: \"a \\\n    \\tb\n   \n\n   \\  c\\\n   d\"\n",
    );
}

#[test]
fn test_set_new_nested_key_to_multiline_plain_scalar() {
    assert_scalar_copied_to_nested_key(
        "v: x\n  y\n\n  \n \t z\n",
        "m:\n  k: 1\n",
        "m:\n  k: 1\n  n: x\n    y\n\n    \n   \t z\n",
    );
}

#[test]
fn test_set_new_nested_key_to_multiline_crlf_scalar() {
    assert_scalar_copied_to_nested_key(
        "v: \"a\r\n b\"\r\nw: x\r\n y\r\n",
        "m:\n  k: 1\n",
        "m:\n  k: 1\n  n: \"a\r\n   b\"\n",
    );
}

/// Copied to a shallower key, continuation lines move in by the same
/// amount, keeping their offset past the line the scalar starts on.
#[test]
fn test_set_root_key_to_nested_multiline_scalars() {
    let src = "m:\n  q: \"a\n    b\"\n  p: x\n   y\n";
    let source = value_of(src, "m");
    let source = source.as_mapping().unwrap();
    let file = YamlFile::from_str("k: 1\n").unwrap();
    let mapping = file.document().unwrap().as_mapping().unwrap();
    mapping.set("q", source.get("q").unwrap());
    mapping.set("p", source.get("p").unwrap());
    assert_valid_copy(&file, "k: 1\nq: \"a\n  b\"\np: x\n y\n");
    assert_eq!(
        mapping.get("q").unwrap().as_scalar().unwrap().as_string(),
        "a b"
    );
    assert_eq!(
        mapping.get("p").unwrap().as_scalar().unwrap().as_string(),
        "x y"
    );
}

#[test]
fn test_set_sequence_item_to_multiline_scalar() {
    let file = YamlFile::from_str("m:\n  - x\n").unwrap();
    let mapping = file.document().unwrap().as_mapping().unwrap();
    let seq = mapping.get_sequence("m").unwrap();
    seq.set(0, value_of("v: \"a\n b\"\n", "v"));
    seq.push(value_of("v: x\n y\n", "v"));
    assert_valid_copy(&file, "m:\n  - \"a\n   b\"\n  - x\n   y\n");
}

#[test]
fn test_set_new_key_in_nested_flow_mapping_to_multiline_scalar() {
    let file = YamlFile::from_str("m:\n  f: {x: 1}\n").unwrap();
    let mapping = file.document().unwrap().as_mapping().unwrap();
    let flow = mapping.get_mapping("m").unwrap().get_mapping("f").unwrap();
    flow.set("n", value_of("v: \"a\n b\"\n", "v"));
    assert_valid_copy(&file, "m:\n  f: {x: 1, n: \"a\n      b\"}\n");
}

#[test]
fn test_push_multiline_scalar_to_nested_flow_sequence() {
    let file = YamlFile::from_str("m:\n  f: [x]\n").unwrap();
    let mapping = file.document().unwrap().as_mapping().unwrap();
    let flow = mapping.get_mapping("m").unwrap().get_sequence("f").unwrap();
    flow.push(value_of("v: x\n y\n", "v"));
    assert_valid_copy(&file, "m:\n  f: [x, x\n   y]\n");
}

#[test]
fn test_replace_deeply_nested_entry_with_multiline_scalar() {
    let file = YamlFile::from_str("m:\n  o:\n    k: 1\n").unwrap();
    let mapping = file.document().unwrap().as_mapping().unwrap();
    let nested = mapping.get_mapping("m").unwrap().get_mapping("o").unwrap();
    nested.insert_at_index_preserving(0, "k", value_of("v: \"a\n b\"\n", "v"));
    assert_valid_copy(&file, "m:\n  o:\n    k: \"a\n     b\"\n");
}

/// A multi-line explicit key is shifted to the entry's column like a value.
#[test]
fn test_new_entry_with_multiline_explicit_key() {
    let src = YamlFile::from_str("? \"a\n  b\"\n: 1\n").unwrap();
    let key = src
        .document()
        .unwrap()
        .as_mapping()
        .unwrap()
        .keys()
        .next()
        .unwrap();
    let entry = MappingEntry::new_at_indent(&key, 1, false, true, 2);
    assert_eq!(entry.to_string(), "? \"a\n    b\"\n  : 1\n");
    let file = YamlFile::from_str(&format!("m:\n  {}", entry)).unwrap();
    assert_eq!(Validator::new().validate_syntax(file.syntax()), vec![]);
    let mapping = file.document().unwrap().as_mapping().unwrap();
    let nested = mapping.get_mapping("m").unwrap();
    let copied = nested.keys().next().unwrap();
    assert_eq!(copied.as_scalar().unwrap().as_string(), "a b");
}

#[test]
fn test_set_sequence_item_to_block_scalar() {
    for (target, expected) in [
        ("- 1\n- 2\n", "- |\n  text\n- 2\n"),
        ("- 1\n", "- |\n  text\n"),
        ("a:\n  - 1\n  - 2\n", "a:\n  - |\n    text\n  - 2\n"),
    ] {
        let file = YamlFile::from_str(target).unwrap();
        let root = file.document().unwrap();
        let sequence = match root.as_sequence() {
            Some(s) => s,
            None => root.as_mapping().unwrap().get_sequence("a").unwrap(),
        };
        sequence.set(0, value_of("k: |\n      text\n", "k"));
        assert_parser_shaped(&file, expected);
    }
}

/// Everything a TAGGED_NODE holds between its tag and its content (an
/// anchor after the tag, a comment, comment lines) comes along when it is
/// copied under another key.
#[test]
fn test_set_tagged_node_value_keeps_properties_and_comments() {
    for value in [
        " !t  # c\n  x: 1\n",
        " !t &a\n  x: 1\n",
        " !t &a  # c\n  x: 1\n",
        " !t\n  # c\n  x: 1\n",
        " !t\n\n  x: 1\n",
        " !t &a 1\n",
        " !t &a |\n  x\n",
        " !t |  # c\n  x\n",
    ] {
        let source = value_of(&format!("v:{value}"), "v");
        for (src, nested, key, expected) in [
            ("k: 1\n", false, "n", format!("k: 1\nn:{value}")),
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

/// A timestamp, binary or regex scalar carries its tag even when none was
/// given, and the tag is built as the parser reads it.
#[test]
fn test_set_implicitly_tagged_scalar_value() {
    use yaml_edit::{ScalarValue, YamlValue};
    let mut cases = vec![
        (
            ScalarValue::timestamp("2001-12-14"),
            "a: 1\nb: !!timestamp 2001-12-14\n",
        ),
        (ScalarValue::regex("^a+$"), "a: 1\nb: !!regex ^a+$\n"),
    ];
    #[cfg(feature = "base64")]
    cases.push((ScalarValue::binary(b"hi"), "a: 1\nb: !!binary aGk=\n"));
    for (value, expected) in cases {
        let file = YamlFile::from_str("a: 1\n").unwrap();
        let mapping = file.document().unwrap().as_mapping().unwrap();
        mapping.set("b", &value);
        assert_parser_shaped(&file, expected);

        let file = YamlFile::from_str("a: 1\n").unwrap();
        let mapping = file.document().unwrap().as_mapping().unwrap();
        mapping.set("b", YamlValue::Scalar(value));
        assert_parser_shaped(&file, expected);
    }
}

#[test]
fn test_set_implicitly_tagged_scalar_key_matches_tagged_key() {
    use yaml_edit::ScalarValue;
    for (src, expected) in [
        (
            "? !!timestamp 2001-12-14\n: 1\n",
            "? !!timestamp 2001-12-14\n: 2\n",
        ),
        (
            "{!!timestamp 2001-12-14: 1}\n",
            "{!!timestamp 2001-12-14: 2}\n",
        ),
        ("!!timestamp 2001-12-14: 1\n", "!!timestamp 2001-12-14: 2\n"),
    ] {
        let file = YamlFile::from_str(src).unwrap();
        let mapping = file.document().unwrap().as_mapping().unwrap();
        mapping.set(ScalarValue::timestamp("2001-12-14"), 2);
        assert_parser_shaped(&file, expected);
    }
}

/// Tabs after a continuation line's indentation separate tokens rather
/// than indent, so shifting the line keeps them.
#[test]
fn test_set_multiline_flow_value_keeps_tabs() {
    for (value, expected) in [
        ("[a,\n \tb]\n", "m:\n  k: 1\n  n: [a,\n   \tb]\n"),
        ("[a,\n  \t b]\n", "m:\n  k: 1\n  n: [a,\n    \t b]\n"),
        (
            "{a: 1,\n\t\n b: 2}\n",
            "m:\n  k: 1\n  n: {a: 1,\n  \t\n   b: 2}\n",
        ),
    ] {
        let file = YamlFile::from_str("m:\n  k: 1\n").unwrap();
        let mapping = file.document().unwrap().as_mapping().unwrap();
        let nested = mapping.get_mapping("m").unwrap();
        nested.set("n", value_of(&format!("v: {value}"), "v"));
        assert_valid_copy(&file, expected);
    }
}

#[test]
fn test_set_dedented_multiline_flow_value_keeps_tabs() {
    let source = value_of("m:\n  v: [a,\n   \tb]\n", "m");
    let source = source.as_mapping().unwrap().get("v").unwrap();
    let file = YamlFile::from_str("k: 1\n").unwrap();
    file.document()
        .unwrap()
        .as_mapping()
        .unwrap()
        .set("n", source);
    assert_valid_copy(&file, "k: 1\nn: [a,\n \tb]\n");
}

#[test]
fn test_set_tagged_scalar_key_in_block_mapping() {
    use yaml_edit::ScalarValue;
    let file = YamlFile::from_str("a: 1\n").unwrap();
    let mapping = file.document().unwrap().as_mapping().unwrap();
    mapping.set(ScalarValue::timestamp("2001-12-14"), 2);
    mapping.set(ScalarValue::regex("^a+$"), 3);
    assert_parser_shaped(&file, "a: 1\n!!timestamp 2001-12-14: 2\n!!regex ^a+$: 3\n");
}

/// Continuation lines of a multi-line quoted scalar inside a copied
/// collection are shifted with the collection's other lines.
#[test]
fn test_set_nested_key_to_flow_sequence_with_multiline_quoted_scalar() {
    let file = YamlFile::from_str("m:\n  k: 1\n").unwrap();
    let mapping = file.document().unwrap().as_mapping().unwrap();
    mapping
        .get_mapping("m")
        .unwrap()
        .set("n", value_of("v: [\"a\n\n b\"]\n", "v"));
    assert_valid_copy(&file, "m:\n  k: 1\n  n: [\"a\n\n   b\"]\n");
}

#[test]
fn test_set_nested_key_to_block_mapping_with_multiline_quoted_scalar() {
    let file = YamlFile::from_str("m:\n  k: 1\n").unwrap();
    let mapping = file.document().unwrap().as_mapping().unwrap();
    mapping
        .get_mapping("m")
        .unwrap()
        .set("n", value_of("v:\n  q: 'a\n   b'\n", "v"));
    assert_valid_copy(&file, "m:\n  k: 1\n  n:\n    q: 'a\n     b'\n");
}

#[test]
fn test_set_nested_key_to_block_sequence_with_multiline_quoted_scalar() {
    let file = YamlFile::from_str("m:\n  k: 1\n").unwrap();
    let mapping = file.document().unwrap().as_mapping().unwrap();
    mapping
        .get_mapping("m")
        .unwrap()
        .set("n", value_of("v:\n- \"a\n  b\"\n", "v"));
    assert_valid_copy(&file, "m:\n  k: 1\n  n:\n    - \"a\n      b\"\n");
}
