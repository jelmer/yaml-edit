//! Copying nodes between documents.
//!
//! A CST node passed as a key or value is copied in whole, wrapper
//! included, so the result has the shape the parser gives the same text.
//! Continuation lines of a copied multi-line value move with it, so they
//! still clear the column of the key or item they end up under.
//!
//! A collection that is no longer in its document, from a handle kept
//! from before the value, item or entry holding it was replaced or
//! removed, can still be edited: the edit happens on the detached tree,
//! and the document is left as it is.

use rowan::ast::AstNode;
use std::str::FromStr;
use yaml_edit::validator::Validator;
use yaml_edit::{
    debug, AsYaml, Document, Mapping, MappingBuilder, MappingEntry, ScalarValue, Sequence,
    YamlFile, YamlNode, YamlValue,
};

mod common;
use common::{assert_file_edited, assert_parser_shaped};

fn value_of(src: &str, key: &str) -> YamlNode {
    let file = YamlFile::from_str(src).unwrap();
    file.document()
        .unwrap()
        .as_mapping()
        .unwrap()
        .get(key)
        .unwrap()
}

fn root(src: &str) -> Document {
    Document::from_str(src).unwrap()
}

fn nested(mapping: &Mapping) -> Mapping {
    mapping.get_mapping("m").unwrap()
}

/// Like [`assert_file_edited`], and the result has no spec violations.
#[track_caller]
fn assert_valid_copy(file: &YamlFile, expected: &str) {
    assert_file_edited(file, expected);
    assert_eq!(Validator::new().validate_syntax(file.syntax()), vec![]);
}

/// Apply `edit` to the root mapping of `target` and check the result as
/// [`assert_valid_copy`] does.
#[track_caller]
fn check_copy(target: &str, edit: impl FnOnce(&Mapping), expected: &str) -> YamlFile {
    let file = YamlFile::from_str(target).unwrap();
    edit(&file.document().unwrap().as_mapping().unwrap());
    assert_valid_copy(&file, expected);
    file
}

#[test]
fn set_node_value() {
    let file = YamlFile::from_str("a: 1\n").unwrap();
    let mapping = file.document().unwrap().as_mapping().unwrap();
    mapping.set("b", value_of("k: v\n", "k"));
    assert_file_edited(&file, "a: 1\nb: v\n");
    assert_eq!(mapping.get("b").unwrap().to_string(), "v");

    let file = YamlFile::from_str("a: &x 1\n").unwrap();
    let mapping = file.document().unwrap().as_mapping().unwrap();
    mapping.set("b", value_of("a: &x 1\nb: *x\n", "b"));
    assert_file_edited(&file, "a: &x 1\nb: *x\n");

    let file = YamlFile::from_str("a: 1\n").unwrap();
    let mapping = file.document().unwrap().as_mapping().unwrap();
    let tagged = value_of("c: !t v\n", "c");
    mapping.set("b", tagged.as_tagged().unwrap());
    assert_file_edited(&file, "a: 1\nb: !t v\n");
}

#[test]
fn rename_key_to_scalar_node() {
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
    assert_file_edited(&file, "k: 1\n");
    assert!(mapping.contains_key("k"));
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

/// Set `key` to `source` in the root mapping of `src`, or in its nested
/// mapping `m`, and check the result.
#[track_caller]
fn check_set_node(src: &str, nested_mapping: bool, key: &str, source: &YamlNode, expected: &str) {
    let file = YamlFile::from_str(src).unwrap();
    let root = file.document().unwrap().as_mapping().unwrap();
    let mapping = if nested_mapping { nested(&root) } else { root };
    mapping.set(key, source);
    assert_file_edited(&file, expected);
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
fn set_block_value_node() {
    for value in BLOCK_VALUES {
        let source = value_of(&format!("v:{value}"), "v");
        check_set_node("k: 1\n", false, "k", &source, &format!("k:{value}"));
        check_set_node("k: 1\n", false, "n", &source, &format!("k: 1\nn:{value}"));
        check_set_node(
            "m:\n  k: 1\n",
            true,
            "k",
            &source,
            &format!("m:\n  k:{}", shift(value)),
        );
        check_set_node(
            "m:\n  k: 1\n",
            true,
            "n",
            &source,
            &format!("m:\n  k: 1\n  n:{}", shift(value)),
        );
    }
}

/// Everything a TAGGED_NODE holds between its tag and its content (an
/// anchor after the tag, a comment, comment lines) comes along when it is
/// copied under another key.
#[test]
fn set_tagged_node_value_keeps_properties_and_comments() {
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
        check_set_node("k: 1\n", false, "n", &source, &format!("k: 1\nn:{value}"));
        check_set_node(
            "m:\n  k: 1\n",
            true,
            "n",
            &source,
            &format!("m:\n  k: 1\n  n:{}", shift(value)),
        );
    }
}

#[test]
fn set_block_value_node_keeps_key_line_comment() {
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
            check_set_node(src, false, "k", &source, &format!("{expected}j: 2\n"));
        }
    }
}

#[test]
fn builder_pair_block_value_node() {
    for value in BLOCK_VALUES {
        let source = value_of(&format!("v:{value}"), "v");
        let doc = MappingBuilder::new()
            .pair("n", &source)
            .pair("j", 1)
            .build_document();
        let expected = format!("n:{value}j: 1\n");
        assert_eq!(doc.to_string(), expected);
        assert_parser_shaped(&doc);
    }
}

#[test]
fn set_sequence_item_to_block_scalar() {
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
        assert_file_edited(&file, expected);
    }
}

#[test]
fn copy_multiline_flow_value() {
    let list = || value_of("v: [a,\n b]\n", "v");
    check_copy(
        "m:\n  k: 1\n",
        |m| nested(m).set("n", list()),
        "m:\n  k: 1\n  n: [a,\n   b]\n",
    );
    check_copy(
        "m:\n  k: 1\n",
        |m| nested(m).set("n", value_of("v: {a: 1, # c\n b: 2}\n", "v")),
        "m:\n  k: 1\n  n: {a: 1, # c\n   b: 2}\n",
    );
    check_copy(
        "m:\n  - k: 1\n",
        |m| {
            let item = m.get_sequence("m").unwrap().get(0).unwrap();
            item.as_mapping().unwrap().set("n", list());
        },
        "m:\n  - k: 1\n    n: [a,\n     b]\n",
    );
    check_copy(
        "m:\n  f: {x: 1}\n",
        |m| nested(m).get_mapping("f").unwrap().set("n", list()),
        "m:\n  f: {x: 1, n: [a,\n      b]}\n",
    );
    check_copy(
        "m:\n  f: [x]\n",
        |m| nested(m).get_sequence("f").unwrap().push(list()),
        "m:\n  f: [x, [a,\n   b]]\n",
    );

    let tagged = || value_of("v: !t [a,\n b]\n", "v");
    check_copy("k: 1\n", |m| m.set("n", tagged()), "k: 1\nn: !t [a,\n b]\n");
    check_copy(
        "m:\n  k: 1\n",
        |m| nested(m).set("n", tagged()),
        "m:\n  k: 1\n  n: !t [a,\n   b]\n",
    );
    check_copy(
        "m:\n  - x\n",
        |m| {
            m.get_sequence("m").unwrap().set(0, tagged());
        },
        "m:\n  - !t [a,\n   b]\n",
    );
}

/// A block value rewritten in flow form to keep a comment is placed one
/// level past the line holding it, not shifted twice.
#[test]
fn copy_commented_block_value_into_flow_collection() {
    let block = || value_of("v:\n  x: 1 # c\n  y: 2\n", "v");
    check_copy(
        "a:\n  f: {n: 1}\n",
        |m| {
            let a = m.get_mapping("a").unwrap();
            a.get_mapping("f").unwrap().set("n", block());
        },
        "a:\n  f: {n: {x: 1, # c\n        y: 2}}\n",
    );
    check_copy(
        "a:\n  f: [1]\n",
        |m| {
            let a = m.get_mapping("a").unwrap();
            a.get_sequence("f").unwrap().set(0, block());
        },
        "a:\n  f: [{x: 1, # c\n        y: 2}]\n",
    );
}

/// Copy the scalar at `v` in `src` under key `n` in the mapping `m` of
/// `target`, checking the result and that the value survived.
#[track_caller]
fn assert_scalar_copied_to_nested_key(src: &str, target: &str, expected: &str) {
    let file = check_copy(target, |m| nested(m).set("n", value_of(src, "v")), expected);
    let mapping = file.document().unwrap().as_mapping().unwrap();
    let copied = nested(&mapping).get("n").unwrap();
    assert_eq!(
        copied.as_scalar().unwrap().as_string(),
        value_of(src, "v").as_scalar().unwrap().as_string()
    );
}

#[test]
fn copy_multiline_scalar_to_nested_key() {
    for (src, target, expected) in [
        (
            "v: \"a\n b\"\n",
            "m:\n  k: 1\n",
            "m:\n  k: 1\n  n: \"a\n   b\"\n",
        ),
        ("v: \"a\n b\"\n", "m:\n  n: 1\n", "m:\n  n: \"a\n   b\"\n"),
        (
            "v: 'a\n\n b ''c'''\n",
            "m:\n  k: 1\n",
            "m:\n  k: 1\n  n: 'a\n\n   b ''c'''\n",
        ),
        // Escaped line breaks, escapes at the start of a line and blank
        // lines all keep their meaning; only the line prefixes move.
        (
            "v: \"a \\\n  \\tb\n \n\n \\  c\\\n d\"\n",
            "m:\n  k: 1\n",
            "m:\n  k: 1\n  n: \"a \\\n    \\tb\n   \n\n   \\  c\\\n   d\"\n",
        ),
        (
            "v: x\n  y\n\n  \n \t z\n",
            "m:\n  k: 1\n",
            "m:\n  k: 1\n  n: x\n    y\n\n    \n   \t z\n",
        ),
        (
            "v: \"a\r\n b\"\r\nw: x\r\n y\r\n",
            "m:\n  k: 1\n",
            "m:\n  k: 1\n  n: \"a\r\n   b\"\n",
        ),
    ] {
        assert_scalar_copied_to_nested_key(src, target, expected);
    }
}

/// Copied to a shallower key, continuation lines move in by the same
/// amount, keeping their offset past the line the scalar starts on.
#[test]
fn copy_nested_multiline_scalars_to_root_keys() {
    let source = value_of("m:\n  q: \"a\n    b\"\n  p: x\n   y\n", "m");
    let source = source.as_mapping().unwrap();
    let file = check_copy(
        "k: 1\n",
        |m| {
            m.set("q", source.get("q").unwrap());
            m.set("p", source.get("p").unwrap());
        },
        "k: 1\nq: \"a\n  b\"\np: x\n y\n",
    );
    let mapping = file.document().unwrap().as_mapping().unwrap();
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
fn copy_multiline_scalar_into_collections() {
    check_copy(
        "m:\n  - x\n",
        |m| {
            let seq = m.get_sequence("m").unwrap();
            seq.set(0, value_of("v: \"a\n b\"\n", "v"));
            seq.push(value_of("v: x\n y\n", "v"));
        },
        "m:\n  - \"a\n   b\"\n  - x\n   y\n",
    );
    check_copy(
        "m:\n  f: {x: 1}\n",
        |m| {
            let flow = nested(m).get_mapping("f").unwrap();
            flow.set("n", value_of("v: \"a\n b\"\n", "v"));
        },
        "m:\n  f: {x: 1, n: \"a\n      b\"}\n",
    );
    check_copy(
        "m:\n  f: [x]\n",
        |m| {
            let flow = nested(m).get_sequence("f").unwrap();
            flow.push(value_of("v: x\n y\n", "v"));
        },
        "m:\n  f: [x, x\n   y]\n",
    );
    check_copy(
        "m:\n  o:\n    k: 1\n",
        |m| {
            let o = nested(m).get_mapping("o").unwrap();
            o.insert_at_index_preserving(0, "k", value_of("v: \"a\n b\"\n", "v"));
        },
        "m:\n  o:\n    k: \"a\n     b\"\n",
    );
}

/// Continuation lines of a multi-line quoted scalar inside a copied
/// collection are shifted with the collection's other lines.
#[test]
fn copy_collection_holding_multiline_quoted_scalar() {
    for (value, expected) in [
        ("v: [\"a\n\n b\"]\n", "m:\n  k: 1\n  n: [\"a\n\n   b\"]\n"),
        (
            "v:\n  q: 'a\n   b'\n",
            "m:\n  k: 1\n  n:\n    q: 'a\n     b'\n",
        ),
        (
            "v:\n- \"a\n  b\"\n",
            "m:\n  k: 1\n  n:\n    - \"a\n      b\"\n",
        ),
    ] {
        check_copy(
            "m:\n  k: 1\n",
            |m| nested(m).set("n", value_of(value, "v")),
            expected,
        );
    }
}

/// A multi-line explicit key is shifted to the entry's column like a value.
#[test]
fn new_entry_with_multiline_explicit_key() {
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
    let copied = nested(&mapping).keys().next().unwrap();
    assert_eq!(copied.as_scalar().unwrap().as_string(), "a b");
}

/// Tabs after a continuation line's indentation separate tokens rather
/// than indent, so shifting the line keeps them.
#[test]
fn copy_multiline_flow_value_keeps_tabs() {
    for (value, expected) in [
        ("[a,\n \tb]\n", "m:\n  k: 1\n  n: [a,\n   \tb]\n"),
        ("[a,\n  \t b]\n", "m:\n  k: 1\n  n: [a,\n    \t b]\n"),
        (
            "{a: 1,\n\t\n b: 2}\n",
            "m:\n  k: 1\n  n: {a: 1,\n  \t\n   b: 2}\n",
        ),
    ] {
        check_copy(
            "m:\n  k: 1\n",
            |m| nested(m).set("n", value_of(&format!("v: {value}"), "v")),
            expected,
        );
    }
    let source = value_of("m:\n  v: [a,\n   \tb]\n", "m");
    let source = source.as_mapping().unwrap().get("v").unwrap();
    check_copy("k: 1\n", |m| m.set("n", source), "k: 1\nn: [a,\n \tb]\n");
}

/// A document's flow collection may continue at column 0, but under a key
/// its continuation lines have to clear the key's column.
#[test]
fn copy_root_value_continued_at_column_zero() {
    fn set_n(value: impl AsYaml) -> impl FnOnce(&Mapping) {
        move |m| nested(m).set("n", value)
    }
    for (value, expected) in [
        ("[a,\nb]\n", "m:\n  k: 1\n  n: [a,\n   b]\n"),
        ("[\n  a\n]\n", "m:\n  k: 1\n  n: [\n     a\n   ]\n"),
    ] {
        check_copy(
            "m:\n  k: 1\n",
            set_n(root(value).as_sequence().unwrap()),
            expected,
        );
    }
    check_copy(
        "m:\n  k: 1\n",
        set_n(root("{a: 1,\nb: 2}\n").as_mapping().unwrap()),
        "m:\n  k: 1\n  n: {a: 1,\n   b: 2}\n",
    );
    check_copy(
        "m:\n  k: 1\n",
        set_n(root("!t [a,\nb]\n").as_tagged().unwrap()),
        "m:\n  k: 1\n  n: !t [a,\n   b]\n",
    );
    check_copy(
        "m:\n  k: 1\n",
        set_n(root("\"a\nb\"\n").as_scalar().unwrap()),
        "m:\n  k: 1\n  n: \"a\n   b\"\n",
    );
    // A tab before a continuation line separates rather than indents, so
    // it is kept and the line still has to be moved past the key's column.
    check_copy(
        "k: 1\n",
        |m| m.set("n", root("[a,\n\t b]\n").as_sequence().unwrap()),
        "k: 1\nn: [a,\n \t b]\n",
    );
    check_copy(
        "m:\n  - x\n",
        |m| {
            let seq = m.get_sequence("m").unwrap();
            seq.set(0, root("[a,\nb]\n").as_sequence().unwrap());
            seq.push(root("[c,\nd]\n").as_sequence().unwrap());
        },
        "m:\n  - [a,\n   b]\n  - [c,\n   d]\n",
    );
}

/// A block collection copied from the end of unterminated input ends its
/// last line in its last entry, as the parser has it.
#[test]
fn set_value_to_unterminated_block_collection() {
    for (source, expected) in [
        ("a: 1", "a:\n  - x"),
        ("a: 1\n", "a:\n  - x\n"),
        ("a: 1\nb: 2\n", "a:\n  - x\nb: 2\n"),
    ] {
        let seq = YamlFile::from_str("- x").unwrap();
        let seq = seq.document().unwrap().as_sequence().unwrap();
        check_copy(source, |m| m.set("a", seq), expected);
    }
    let map = YamlFile::from_str("k: v").unwrap();
    let map = map.document().unwrap().as_mapping().unwrap();
    check_copy("a: 1\n", |m| m.set("a", map), "a:\n  k: v\n");
}

/// A timestamp, binary or regex scalar carries its tag even when none was
/// given, and the tag is built as the parser reads it.
#[test]
fn set_implicitly_tagged_scalar_value() {
    #[cfg_attr(not(feature = "base64"), allow(unused_mut))]
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
        assert_file_edited(&file, expected);

        let file = YamlFile::from_str("a: 1\n").unwrap();
        let mapping = file.document().unwrap().as_mapping().unwrap();
        mapping.set("b", YamlValue::Scalar(value));
        assert_file_edited(&file, expected);
    }
}

#[test]
fn set_implicitly_tagged_scalar_key() {
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
        assert_file_edited(&file, expected);
    }

    let file = YamlFile::from_str("a: 1\n").unwrap();
    let mapping = file.document().unwrap().as_mapping().unwrap();
    mapping.set(ScalarValue::timestamp("2001-12-14"), 2);
    mapping.set(ScalarValue::regex("^a+$"), 3);
    assert_file_edited(&file, "a: 1\n!!timestamp 2001-12-14: 2\n!!regex ^a+$: 3\n");
}

/// The text of the detached tree `node` is now in, after checking it.
#[track_caller]
fn detached_text(node: &rowan::SyntaxNode<yaml_edit::Lang>) -> String {
    let root = node.ancestors().last().unwrap();
    debug::validate_tree(&root).unwrap();
    root.to_string()
}

/// The mapping value of `k` in `src`, and the file, after `k` is set to 1.
fn replaced_mapping(src: &str) -> (YamlFile, Mapping) {
    let file = YamlFile::from_str(src).unwrap();
    let root = file.document().unwrap().as_mapping().unwrap();
    let inner = root.get_mapping("k").unwrap();
    root.set("k", 1);
    (file, inner)
}

/// The sequence value of `k` in `src`, and the file, after `k` is set to 1.
fn replaced_sequence(src: &str) -> (YamlFile, Sequence) {
    let file = YamlFile::from_str(src).unwrap();
    let root = file.document().unwrap().as_mapping().unwrap();
    let inner = root.get_sequence("k").unwrap();
    root.set("k", 1);
    (file, inner)
}

/// The sequence that is the first item of `src`, and the file, after that
/// item is set to 1.
fn replaced_item(src: &str) -> (YamlFile, Sequence) {
    let file = YamlFile::from_str(src).unwrap();
    let root = file.document().unwrap().as_sequence().unwrap();
    let inner = root.get(0).unwrap().as_sequence().cloned().unwrap();
    root.set(0, 1);
    (file, inner)
}

#[test]
fn drain_replaced_mapping_value() {
    let (file, inner) = replaced_mapping("k:\n  a: 1\n  b: 2\nz: 1\n");
    inner.remove("a");
    inner.remove("b");
    assert_eq!(file.to_string(), "k: 1\nz: 1\n");
    assert_eq!(detached_text(inner.syntax()), "\n  {}\n");
    inner.set("n", 3);
    assert_eq!(file.to_string(), "k: 1\nz: 1\n");
    assert_eq!(detached_text(inner.syntax()), "\n  n: 3\n");

    let (file, inner) = replaced_mapping("k:\n  a: 1\n  # c\n  b: 2\nz: 1\n");
    inner.clear();
    assert_eq!(file.to_string(), "k: 1\nz: 1\n");
    assert_eq!(detached_text(inner.syntax()), "\n  # c\n  {}\n");

    // One emptied before it was replaced, which is `{}` on the key's line.
    let file = YamlFile::from_str("k:\n  a: 1\nz: 1\n").unwrap();
    let root = file.document().unwrap().as_mapping().unwrap();
    let inner = root.get_mapping("k").unwrap();
    inner.remove("a");
    root.set("k", 1);
    assert_eq!(detached_text(inner.syntax()), "{}");
    inner.set("n", 3);
    assert_eq!(file.to_string(), "k: 1\nz: 1\n");
    assert_eq!(detached_text(inner.syntax()), "\n  n: 3\n");
}

#[test]
fn drain_replaced_sequence_value() {
    let (file, inner) = replaced_sequence("k:\n  - a\n  - b\nz: 1\n");
    inner.clear();
    assert_eq!(file.to_string(), "k: 1\nz: 1\n");
    assert_eq!(detached_text(inner.syntax()), "\n  []\n");
    inner.push(3);
    assert_eq!(detached_text(inner.syntax()), "\n  - 3\n");
}

#[test]
fn remove_last_entry_of_replaced_item() {
    let (file, inner) = replaced_item("- - a\n  - b\n- x\n");
    inner.remove(1);
    assert_eq!(file.to_string(), "- 1\n- x\n");
    assert_eq!(detached_text(inner.syntax()), "- - a\n");

    // With no collection around it, the comment line after the last item
    // stays in the sequence, also once that is empty.
    let (file, inner) = replaced_item("- - a\n  # c\n  - b\n- x\n");
    inner.remove(1);
    assert_eq!(detached_text(inner.syntax()), "- - a\n  # c\n");
    inner.remove(0);
    assert_eq!(file.to_string(), "- 1\n- x\n");
    assert_eq!(detached_text(inner.syntax()), "- []\n  # c\n");
}

#[test]
fn remove_last_entry_of_removed_key() {
    let file = YamlFile::from_str("? a: 1\n  b: 2\n: v\nz: 1\n").unwrap();
    let root = file.document().unwrap().as_mapping().unwrap();
    let key = root.keys().next().unwrap();
    let inner = key.as_mapping().cloned().unwrap();
    root.remove(&key);
    inner.remove("b");
    assert_eq!(file.to_string(), "z: 1\n");
    assert_eq!(detached_text(inner.syntax()), "? a: 1\n: v\n");
}

#[test]
fn refill_removed_explicit_key() {
    let file = YamlFile::from_str("? a: 1\n  b: 2\nz: 1\n").unwrap();
    let root = file.document().unwrap().as_mapping().unwrap();
    let key = root.keys().next().unwrap();
    let inner = key.as_mapping().cloned().unwrap();
    root.remove(&key);
    inner.clear();
    assert_eq!(detached_text(inner.syntax()), "? {}\n");
    inner.set("n", 3);
    assert_eq!(file.to_string(), "z: 1\n");
    assert_eq!(detached_text(inner.syntax()), "? n: 3\n");
}
