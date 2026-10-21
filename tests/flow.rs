//! Flow collections: how they parse, and how edits write into them. A
//! collection written into a flow mapping, or used as a key, is rendered in
//! flow style, since block syntax is not allowed there.

use std::str::FromStr;
use yaml_edit::{Mapping, ScalarValue, Sequence, YamlFile, YamlNode, YamlValue};

mod common;
use common::{assert_file_cst_ok, assert_file_edited};

fn parse(yaml: &str) -> YamlFile {
    YamlFile::from_str(yaml).unwrap()
}

/// Parse `yaml`, check it round trips exactly and return its root node.
#[track_caller]
fn parse_root(yaml: &str) -> YamlNode {
    let file = parse(yaml);
    assert_eq!(file.to_string(), yaml);
    let doc = file.document().unwrap();
    doc.as_mapping()
        .map(YamlNode::Mapping)
        .or_else(|| doc.as_sequence().map(YamlNode::Sequence))
        .unwrap()
}

fn parse_seq(yaml: &str) -> Sequence {
    parse(yaml).document().unwrap().as_sequence().unwrap()
}

fn parse_map(yaml: &str) -> Mapping {
    parse(yaml).document().unwrap().as_mapping().unwrap()
}

/// The text of a scalar, or None if it is null. Also checks that a value
/// that reads as an integer has one.
#[track_caller]
fn scalar_text(node: &YamlNode) -> Option<String> {
    let scalar = node.as_scalar().unwrap();
    if scalar.is_null() {
        return None;
    }
    let text = scalar.as_string();
    assert_eq!(scalar.as_i64(), text.parse().ok(), "{text:?}");
    Some(text)
}

/// The entries of a mapping with scalar keys and values, in order.
#[track_caller]
fn entries(mapping: &Mapping) -> Vec<(String, Option<String>)> {
    mapping
        .iter()
        .map(|(k, v)| (k.as_scalar().unwrap().as_string(), scalar_text(&v)))
        .collect()
}

#[track_caller]
fn items(seq: &Sequence) -> Vec<Option<String>> {
    seq.values().map(|v| scalar_text(&v)).collect()
}

/// Expected mapping entries, None for a null value.
type Entries<'a> = &'a [(&'a str, Option<&'a str>)];

fn owned(expected: Entries) -> Vec<(String, Option<String>)> {
    expected
        .iter()
        .map(|(k, v)| (k.to_string(), v.map(str::to_string)))
        .collect()
}

/// Flow mappings with the keys and values they hold, None for null.
#[test]
fn flow_mapping_entries() {
    let cases: &[(&str, Entries)] = &[
        // `a` has no value, and `b:` an omitted one.
        (
            r#"{a, b:, c: d}"#,
            &[("a", None), ("b", None), ("c", Some("d"))],
        ),
        (
            r#"{http://foo.com, bar: baz}"#,
            &[("http://foo.com", None), ("bar", Some("baz"))],
        ),
        // Whitespace before a key's separator stays outside the key.
        ("{foo : bar}\n", &[("foo", Some("bar"))]),
        ("{? foo : bar}\n", &[("foo", Some("bar"))]),
        // 4ABK: the space before the `:` separates the key from it, as its
        // test.event has it (`=VAL :unquoted`), and the trailing `:` in
        // `omitted value:,` is the mapping indicator, not part of the key.
        (
            r#"{
unquoted : "separate",
http://foo.com,
omitted value:,
}"#,
            &[
                ("unquoted", Some("separate")),
                ("http://foo.com", None),
                ("omitted value", None),
            ],
        ),
        // 652Z: a `?` opens an explicit key only when a space or line break
        // follows it, so `?foo` is the plain scalar `?foo`. The suite's
        // test.event has `=VAL :?foo` and in.json `"?foo" : "bar"`.
        (
            r#"{ ?foo: bar,
bar: 42
}"#,
            &[("?foo", Some("bar")), ("bar", Some("42"))],
        ),
        // The same holds next to other keys; 2EBW also lists `?foo` among
        // its "safe" plain keys. PyYAML reads the key as `b` instead, but
        // saphyr and the suite's own expectations keep the `?`.
        (
            "{a, ?b: c, d: e}",
            &[("a", None), ("?b", Some("c")), ("d", Some("e"))],
        ),
        // 5MUD: the suite marks a colon on the line after the key valid.
        (
            r#"---
{ "foo"
  :bar }"#,
            &[("foo", Some("bar"))],
        ),
        (
            "{foo: 1, bar: 2}",
            &[("foo", Some("1")), ("bar", Some("2"))],
        ),
        (
            "{
  foo: 1,
  bar: 2
}",
            &[("foo", Some("1")), ("bar", Some("2"))],
        ),
        // T833: the lenient parser accepts a missing comma, reading the
        // second `:` as an entry with an empty key.
        (
            "---
{
 foo: 1
 bar: 2 }
",
            &[("foo", Some("1 bar")), ("", Some("2"))],
        ),
        ("{a, b, c}", &[("a", None), ("b", None), ("c", None)]),
    ];
    for (yaml, expected) in cases {
        let root = parse_root(yaml);
        assert_eq!(
            entries(root.as_mapping().unwrap()),
            owned(expected),
            "{yaml:?}"
        );
    }
}

/// Flow sequences of flow mappings, with the entries of each mapping.
#[test]
fn flow_mappings_in_sequence() {
    let cases: &[(&str, &[Entries])] = &[
        ("[foo : bar]\n", &[&[("foo", Some("bar"))]]),
        // 5T43: in `"key"::value` the second `:` starts the plain scalar
        // `:value`, since a colon not followed by whitespace is content.
        (
            r#"- { "key":value }
- { "key"::value }"#,
            &[&[("key", Some("value"))], &[("key", Some(":value"))]],
        ),
        // 8KB6: the multi-line plain key folds to "multi line".
        (
            r#"---
- { single line, a: b}
- { multi
  line, a: b}"#,
            &[
                &[("single line", None), ("a", Some("b"))],
                &[("multi line", None), ("a", Some("b"))],
            ],
        ),
        // CML9: the lenient parser accepts a missing comma, as for T833.
        ("[{a: b c: d}]", &[&[("a", Some("b c")), ("", Some("d"))]]),
    ];
    for (yaml, expected) in cases {
        let root = parse_root(yaml);
        let got: Vec<_> = root
            .as_sequence()
            .unwrap()
            .values()
            .map(|item| entries(item.as_mapping().unwrap()))
            .collect();
        let expected: Vec<_> = expected.iter().map(|m| owned(m)).collect();
        assert_eq!(got, expected, "{yaml:?}");
    }
}

#[test]
fn flow_sequence_items() {
    for (yaml, expected) in [
        // A trailing comma adds no item.
        ("[a, b, ]", &[Some("a"), Some("b")][..]),
        // CTN5: the lenient parser reads an extra comma as an empty item.
        ("[a,,b]", &[Some("a"), None, Some("b")][..]),
        // A plain scalar still continues across line breaks, including
        // blank lines.
        (
            "[a\n  b,\n  c\n\n  d\n]\n",
            &[Some("a b"), Some("c\nd")][..],
        ),
    ] {
        let root = parse_root(yaml);
        let expected: Vec<_> = expected.iter().map(|v| v.map(str::to_string)).collect();
        assert_eq!(items(root.as_sequence().unwrap()), expected, "{yaml:?}");
    }
}

#[test]
fn deeply_nested_flow_sequences() {
    let mut node = parse_root("[[[[[a]]]]]");
    for _ in 0..5 {
        let seq = node.as_sequence().unwrap();
        assert_eq!(seq.len(), 1);
        node = seq.get(0).unwrap();
    }
    assert_eq!(node.as_scalar().unwrap().as_string(), "a");
}

/// 4FJ6: nested implicit complex keys.
#[test]
fn nested_complex_flow_keys() {
    let root = parse_root(
        r#"---
[
  [ a, [ [[b,c]]: d, e]]: 23
]"#,
    );
    let seq = root.as_sequence().unwrap();
    assert_eq!(seq.len(), 1);
    assert_eq!(seq.get(0).unwrap().as_mapping().unwrap().keys().count(), 1);
}

/// 87E4: single quoted implicit keys in and around a flow sequence.
#[test]
fn quoted_implicit_keys_in_flow() {
    let root = parse_root(
        r#"'implicit block key' : [
  'implicit flow key' : value,
 ]"#,
    );
    let mapping = root.as_mapping().unwrap();
    assert_eq!(mapping.len(), 1);
    let seq = mapping.get_sequence("implicit block key").unwrap();
    assert_eq!(seq.len(), 1);
    assert_eq!(
        entries(seq.get(0).unwrap().as_mapping().unwrap()),
        owned(&[("implicit flow key", Some("value"))])
    );
}

#[test]
fn empty_flow_collections_with_whitespace() {
    let root = parse_root("empty_seq: [ ]\nempty_map: { }\n");
    let mapping = root.as_mapping().unwrap();
    assert_eq!(mapping.len(), 2);
    assert_eq!(mapping.get_sequence("empty_seq").unwrap().len(), 0);
    assert_eq!(mapping.get_mapping("empty_map").unwrap().len(), 0);
}

/// A line break before the closing bracket ends the last plain scalar
/// rather than being part of it, just as spaces do.
#[test]
fn line_break_before_closing_bracket_is_outside_scalar() {
    let yaml = "m: {\n  a: 1,\n  b: 2\n}\nl: [x,\n  y\n  ]\n";
    let root = parse_root(yaml);
    let mapping = root.as_mapping().unwrap();
    let text = |node: YamlNode| {
        let range = node.as_scalar().unwrap().byte_range();
        &yaml[range.start as usize..range.end as usize]
    };
    assert_eq!(
        text(mapping.get_mapping("m").unwrap().get("b").unwrap()),
        "2"
    );
    assert_eq!(
        text(mapping.get_sequence("l").unwrap().get(1).unwrap()),
        "y"
    );
}

#[test]
fn remove_last_flow_item_on_closing_bracket_line() {
    let file = parse("- [\n foo]\n");
    let seq = file.document().unwrap().as_sequence().unwrap();
    seq.get(0).unwrap().as_sequence().unwrap().clear();
    assert_eq!(file.to_string(), "- [\n ]\n");

    let file = parse("- [a,\n  b]\n");
    let seq = file.document().unwrap().as_sequence().unwrap();
    seq.get(0).unwrap().as_sequence().unwrap().remove(1);
    assert_eq!(file.to_string(), "- [a]\n");
}

/// The mapping at `path` in `file`: the root one, or the one under the
/// root key `path`.
fn mapping_at(file: &YamlFile, path: &str) -> Mapping {
    let root = file.document().unwrap().as_mapping().unwrap();
    if path.is_empty() {
        root
    } else {
        root.get_mapping(path).unwrap()
    }
}

/// Parse `target`, set `key` in the mapping at `path` to the root of
/// `value` and check the result is `expected`.
#[track_caller]
fn check_set(target: &str, path: &str, key: &str, value: &str, expected: &str) {
    let file = parse(target);
    mapping_at(&file, path).set(key, parse_root(value));
    assert_file_edited(&file, expected);
}

/// A block sequence set in a flow mapping is written in flow style, with
/// the same number of items; implicit nulls stay, and decorated ones stay
/// empty.
#[test]
fn set_block_sequence_in_flow_mapping() {
    for (value, expected, len) in [
        ("- 1\n- 2\n", "{x: 1, y: [1, 2]}\n", 2),
        ("-\n", "{x: 1, y: [null]}\n", 1),
        ("- a\n-\n", "{x: 1, y: [a, null]}\n", 2),
        ("-\n- a\n", "{x: 1, y: [null, a]}\n", 2),
        ("- &n\n- !t\n", "{x: 1, y: [&n, !t]}\n", 2),
    ] {
        check_set("{x: 1}\n", "", "y", value, expected);
        let reparsed = parse_map(expected);
        assert_eq!(reparsed.get_sequence("y").unwrap().len(), len, "{value:?}");
    }
    check_set(
        "{x: 1, y: 2}\n",
        "",
        "x",
        "- a\n- b\n",
        "{x: [a, b], y: 2}\n",
    );
}

/// A block mapping set in a flow mapping is written in flow style, keeping
/// anchors, tags, quotes and comments.
#[test]
fn set_block_mapping_in_flow_mapping() {
    for (target, path, value, expected) in [
        (
            "a: {x: 1}",
            "a",
            "p: 1\nq: 2\n",
            "a: {x: 1, y: {p: 1, q: 2}}",
        ),
        (
            "{x: 1}\n",
            "",
            "p: &a 1\nq: !t [*a, 'c, d']\nr: |\n  line\ns:\n  - x, y\n  - {k: v}\nt:\n",
            "{x: 1, y: {p: &a 1, q: !t [*a, 'c, d'], r: \"line\\n\", s: [\"x, y\", {k: v}], t: null}}\n",
        ),
        (
            "{x: 1}\n",
            "",
            "p: 1 # keep me\nq: 2\n",
            "{x: 1, y: {p: 1, # keep me\n  q: 2}}\n",
        ),
        (
            "a: {x: 1}\n",
            "a",
            "p:  # head\n  - 1  # one\n  # own line\n  - 2\nq: 3  # last\n# trailing\n",
            "a: {x: 1, y: {p: [  # head\n       1,  # one\n       # own line\n       2], q: 3  # last\n     # trailing\n     }}\n",
        ),
        // An explicit key that fits on one line becomes implicit.
        (
            "{x: 1}\n",
            "",
            "? - 1\n  - 2\n: x\n",
            "{x: 1, y: {[1, 2]: x}}\n",
        ),
    ] {
        check_set(target, path, "y", value, expected);
    }
}

/// An explicit key with a comment stays explicit when its mapping is set
/// in a flow mapping, and still maps to the same value.
#[test]
fn set_commented_explicit_key_in_flow_mapping() {
    for (value, expected, read_back) in [
        (
            "? - 1  # c\n  - 2\n: x\n",
            "{x: 1, y: {? [1,  # c\n    2]: x}}\n",
            Some("x"),
        ),
        (
            "? - 1  # c\n  - 2\n",
            "{x: 1, y: {? [1,  # c\n    2]: null}}\n",
            None,
        ),
    ] {
        check_set("{x: 1}\n", "", "y", value, expected);
        let y = parse_map(expected).get_mapping("y").unwrap();
        assert_eq!(y.len(), 1);
        let key = parse_seq("- 1\n- 2\n");
        assert_eq!(scalar_text(&y.get(&key).unwrap()).as_deref(), read_back);
    }
}

#[test]
fn insert_yaml_value_sequence_in_flow_mapping() {
    let file = parse("{x: 1}\n");
    let value: Vec<YamlValue> = vec![YamlValue::scalar(1), YamlValue::scalar("a")];
    mapping_at(&file, "").insert_at_index(0, "y", value);
    assert_file_cst_ok(&file);
    assert_eq!(file.to_string(), "{y: [1, a], x: 1}\n");
}

/// YamlValues with block content, and how each is written in flow style.
fn block_yaml_values() -> Vec<(YamlValue, &'static str)> {
    vec![
        (
            YamlValue::parse_raw("!local\n  a: 1\n  b: 2").unwrap(),
            "!local {a: 1, b: 2}",
        ),
        (
            YamlValue::parse_raw("a: 1\nb: [x]\n").unwrap(),
            "{a: 1, b: [x]}",
        ),
        (
            YamlValue::parse_raw("- a\n- b, c\n").unwrap(),
            "[a, \"b, c\"]",
        ),
        (
            YamlValue::parse_raw("!!set\n  a: ~\n").unwrap(),
            "!!set {a: null}",
        ),
        (
            YamlValue::parse_raw("!!omap\n  - a: 1\n").unwrap(),
            "!!omap [{a: 1}]",
        ),
        (YamlValue::parse_raw("!t\n  - a\n").unwrap(), "!t [a]"),
        (
            YamlValue::Sequence(vec![YamlValue::Scalar(ScalarValue::literal("a\nb\n"))]),
            "[\"a\\nb\\n\"]",
        ),
    ]
}

#[test]
fn push_block_yaml_value_into_flow_sequence() {
    for (value, flow) in block_yaml_values() {
        let file = parse("k: [1]\n");
        mapping_at(&file, "").get_sequence("k").unwrap().push(value);
        assert_file_edited(&file, &format!("k: [1, {flow}]\n"));
    }
}

#[test]
fn set_block_yaml_value_in_flow_mapping() {
    for (value, flow) in block_yaml_values() {
        let file = parse("k: {x: 1}\n");
        mapping_at(&file, "k").set("y", value);
        assert_file_edited(&file, &format!("k: {{x: 1, y: {flow}}}\n"));
    }
}

#[test]
fn set_scalar_with_flow_indicators_in_flow_mapping() {
    for (value, flow) in [
        (ScalarValue::string("a, b"), "\"a, b\""),
        (ScalarValue::literal("x\ny\n"), "\"x\\ny\\n\""),
        (ScalarValue::string("plain"), "plain"),
    ] {
        let file = parse("{x: 1}\n");
        mapping_at(&file, "").set("y", YamlValue::Scalar(value));
        assert_file_edited(&file, &format!("{{x: 1, y: {flow}}}\n"));
    }
}

/// Continuation lines move with the value, keeping their offset from the
/// line it starts on.
#[test]
fn multiline_flow_value_keeps_relative_indentation() {
    let source = parse_map("v: [a,\n    b]\n").get("v").unwrap();
    let file = parse("k:\n  j: 1\n");
    mapping_at(&file, "k").set("n", &source);
    assert_file_edited(&file, "k:\n  j: 1\n  n: [a,\n      b]\n");
}

#[test]
fn sequence_key_in_block_mapping() {
    let file = parse("a: 1\n");
    let key = parse_seq("- 1\n- 2\n");
    mapping_at(&file, "").set(&key, 1);
    assert_file_cst_ok(&file);
    assert_eq!(file.to_string(), "a: 1\n[1, 2]: 1\n");
    let reparsed = parse_map(&file.to_string());
    assert_eq!(reparsed.len(), 2);
    assert!(reparsed.contains_key(&key));
}

#[test]
fn commented_sequence_key_uses_explicit_key() {
    let file = parse("a: 1\n");
    mapping_at(&file, "").set(parse_seq("- 1  # one\n- 2\n"), 1);
    assert_file_edited(&file, "a: 1\n? [1,  # one\n  2]\n: 1\n");
}

#[test]
fn commented_sequence_key_in_flow_mapping() {
    let file = parse("{x: 1}\n");
    let root = mapping_at(&file, "");
    let key = parse_seq("- 1  # one\n- 2\n");
    root.set(&key, 1);
    assert_file_edited(&file, "{x: 1, ? [1,  # one\n  2]: 1}\n");
    let reparsed = parse_map(&file.to_string());
    assert_eq!(reparsed.len(), 2);
    assert_eq!(
        reparsed.get(&key).unwrap().as_scalar().unwrap().as_i64(),
        Some(1)
    );

    root.set(&key, 2);
    assert_file_edited(&file, "{x: 1, ? [1,  # one\n  2]: 2}\n");
}

#[test]
fn commented_mapping_key_in_nested_flow_mapping() {
    let file = parse("a: {x: 1}\n");
    let key = parse_map("p: 1  # one\nq: 2\n");
    mapping_at(&file, "a").set(&key, 3);
    assert_file_edited(&file, "a: {x: 1, ? {p: 1,  # one\n     q: 2}: 3}\n");
    let reparsed = parse_map(&file.to_string());
    let a = reparsed.get_mapping("a").unwrap();
    assert_eq!(a.len(), 2);
    assert_eq!(a.get(&key).unwrap().as_scalar().unwrap().as_i64(), Some(3));
}

#[test]
fn commented_explicit_key_in_item_appended_to_flow_sequence() {
    let file = parse("[1]\n");
    let root = file.document().unwrap().as_sequence().unwrap();
    root.push(parse_map("? - 1  # c\n  - 2\n: x\n"));
    assert_file_edited(&file, "[1, {? [1,  # c\n    2]: x}]\n");
}
