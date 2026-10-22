//! Flow collections: how they parse, and how edits write into them. A
//! collection written into a flow mapping, or used as a key, is rendered in
//! flow style, since block syntax is not allowed there.

mod common;
use common::{
    assert_cst_ok, assert_edited, assert_file_cst_ok, assert_file_edited, assert_file_edited_valid,
    assert_parser_shaped, check_edit, check_renders, count_nodes, parse_clean, render_documents,
};
use rowan::ast::AstNode;
use std::str::FromStr;
use yaml_edit::debug::{roundtrip_ok, validate_tree};
use yaml_edit::path::YamlPath;
use yaml_edit::{
    debug, AsYaml, Document, Mapping, Scalar, ScalarValue, Sequence, SyntaxKind, YamlFile,
    YamlNode, YamlValue,
};

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
    let root = parse_root("nested: [[[[[a]]]]]");
    for mut node in [parse_root("[[[[[a]]]]]"), root.get("nested").unwrap()] {
        for _ in 0..5 {
            let seq = node.as_sequence().unwrap();
            assert_eq!(seq.len(), 1);
            node = seq.get(0).unwrap();
        }
        assert_eq!(node.as_scalar().unwrap().as_string(), "a");
    }
}

/// A trailing comma adds no entry to a flow collection that is a value
/// either.
#[test]
fn trailing_comma_in_nested_flow_collection() {
    check_renders(&[
        ("array: [a, b, ]", "map{array: [a, b]}"),
        ("config: {a: 1, b: 2, }", "map{config: {a: 1, b: 2}}"),
    ]);
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

/// A comment line before the removed last entry stays on its own line,
/// rather than joining the new last entry's line and becoming part of it.
#[test]
fn remove_last_flow_entry_after_comment_line() {
    let remove_last = |d: &Document| {
        d.as_sequence().unwrap().pop();
    };
    check_edit("[1,\n# c\n2]\n", remove_last, "[1\n# c\n]\n");
    check_edit("[1,\n\n# c\n2]\n", remove_last, "[1\n\n# c\n]\n");
    check_edit("[1,\n  # c\n  2]\n", remove_last, "[1\n  # c\n  ]\n");
    check_edit("['x',\n# c\n2]\n", remove_last, "['x'\n# c\n]\n");
    check_edit(
        "{a: 1,\n# c\nb: 2}\n",
        |d| {
            d.as_mapping().unwrap().remove("b");
        },
        "{a: 1\n# c\n}\n",
    );
    check_edit(
        "{a,\n# c\nb}\n",
        |d| {
            d.as_mapping().unwrap().remove("b");
        },
        "{a\n# c\n}\n",
    );
    check_edit(
        "x: {a: 1,\n  # c\n  b: 2}\n",
        |d| {
            d.as_mapping()
                .unwrap()
                .get_mapping("x")
                .unwrap()
                .remove("b");
        },
        "x: {a: 1\n  # c\n  }\n",
    );
    check_edit(
        "x: [[1,\n# c\n2]]\n",
        |d| {
            let outer = d.as_mapping().unwrap().get_sequence("x").unwrap();
            outer.get(0).unwrap().as_sequence().unwrap().pop();
        },
        "x: [[1\n# c\n]]\n",
    );
    // A comment on the line of the comma stays where it is.
    check_edit("[1, # c\n2]\n", remove_last, "[1 # c\n]\n");
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

/// An explicit key or a JSON-like key in a flow sequence makes a single
/// pair mapping, counted here by the MAPPING nodes the tree holds.
#[test]
fn single_pair_mappings_in_flow_sequences() {
    for (yaml, mappings, expected) in [
        // An explicit key is already a complete mapping, so it is not also
        // wrapped as an implicit single-pair one. The YAML test suite's
        // CT4Q has `[\n? foo\n bar : baz\n]` as one mapping inside one
        // sequence; the wrapper made it two.
        ("[? a : b]\n", 1, "[{a: b}]"),
        ("[a: b]\n", 1, "[{a: b}]"),
        ("{? a : b}\n", 1, "{a: b}"),
        ("[? a\n b : c]\n", 1, "[{a b: c}]"),
        // A `:` directly after a JSON-like node separates that key from its
        // value, with no space required (YAML 1.2 section 7.4). The YAML
        // test suite's 9MMW has `[ {JSON: like}:adjacent ]` as a mapping
        // keyed by the flow mapping; the colon was glued into a plain
        // scalar, so the key and value became two sequence entries.
        (
            "[ {JSON: like}:adjacent ]\n",
            2,
            "[{{JSON: like}: adjacent}]",
        ),
        ("[ \"JSON like\":adjacent ]\n", 1, "[{JSON like: adjacent}]"),
        ("[ YAML : separate ]\n", 1, "[{YAML: separate}]"),
        ("{a: b}\n", 1, "{a: b}"),
    ] {
        assert_eq!(render_documents(yaml), [expected], "{yaml:?}");
        let file = parse_clean(yaml);
        assert_eq!(
            count_nodes(&file, SyntaxKind::MAPPING),
            mappings,
            "{yaml:?}"
        );
    }
}

/// YAML test suite FRK4: a plain flow key followed by a space and the
/// separating colon used to keep the space as part of the key.
#[test]
fn flow_plain_key_before_spaced_colon() {
    let yaml = "{\n  ? foo :,\n  : bar,\n}\n";
    check_renders(&[
        (yaml, "{foo: ~, ~: bar}"),
        ("{ foo : bar, a b : c }", "{foo: bar, a b: c}"),
    ]);
    let mapping = parse_map(yaml);
    assert!(mapping.get("foo").unwrap().as_scalar().unwrap().is_null());
    let null_key = mapping.get(ScalarValue::null()).unwrap();
    assert_eq!(null_key.as_scalar().unwrap().as_string(), "bar");
}

/// The `?` of an explicit key goes inside KEY in a flow sequence's single
/// pair, as it does in a flow mapping, so the two entries share one shape.
#[test]
fn explicit_key_entry_in_a_flow_pair_matches_a_flow_mapping() {
    let entry_tree = |yaml: &str| {
        let file = YamlFile::from_str(yaml).unwrap();
        assert_eq!(file.to_string(), yaml);
        let entry = file
            .syntax()
            .descendants()
            .find(|n| n.kind() == SyntaxKind::MAPPING_ENTRY)
            .unwrap();
        debug::tree_to_string(&entry)
    };
    for entry in [
        "? a : 1",
        "? : x",
        "? a",
        "? ",
        "?\n a : b",
        "? a # c\n : b",
        "? a\n b : c",
        "? [a] : b",
    ] {
        assert_eq!(
            entry_tree(&format!("[{entry}]\n")),
            entry_tree(&format!("{{{entry}}}\n")),
            "{entry:?}"
        );
    }
}

/// A block-scalar header opens a block scalar only in block context, so
/// inside a flow collection `|` and `>` are plain content.
///
/// YAML 1.2 section 8.1 gives block scalars a block-context production
/// only, and saphyr reads `[|, x]` as the two entries `|` and `x`. The
/// lexer emitted a header there, so the rest of the collection was consumed
/// as its body and the parse failed with a spurious unclosed-collection
/// error.
#[test]
fn block_header_in_flow_context_is_content() {
    for (yaml, scalars) in [
        ("[|, x]\n", &["|", "x"][..]),
        ("[|]\n", &["|"]),
        ("{a: |, b: 1}\n", &["a", "|", "b", "1"]),
        ("[>, x]\n", &[">", "x"]),
        ("k: [|, x]\n", &["k", "|", "x"]),
    ] {
        let file = parse_clean(yaml);
        let texts: Vec<String> = file
            .syntax()
            .descendants()
            .filter_map(Scalar::cast)
            .map(|s| s.as_string())
            .collect();
        assert_eq!(texts, scalars, "{yaml:?}");
    }
}

/// The text of every block mapping entry in `src`, in document order.
fn entry_texts(src: &str) -> Vec<String> {
    let file = YamlFile::from_str(src).expect("should parse");
    file.syntax()
        .descendants()
        .filter(|n| n.kind() == yaml_edit::SyntaxKind::MAPPING_ENTRY)
        .map(|n| n.to_string())
        .collect()
}

/// A flow entry owns its comma, with anything before it, and the rest
/// of that line; the lines after it are the collection's.
#[test]
fn flow_entry_owns_its_line() {
    assert_eq!(
        entry_texts("{a: 1, # c\n # d\n b: 2 # e\n}\n"),
        ["a: 1, # c\n", "b: 2 # e\n"]
    );
    assert_eq!(entry_texts("{\n?\n}\n"), ["?\n"]);
    assert_eq!(
        entry_texts("{a: 1 # c\n , b: 2}\n"),
        ["a: 1 # c\n , ", "b: 2"]
    );
    assert_eq!(
        entry_texts("{\n  a: 1,\n\n  b: 2\n  }\n"),
        ["a: 1,\n", "b: 2\n"]
    );
}

/// Content after a flow collection value stays in the VALUE rather
/// than being left after the document.
#[test]
fn content_after_flow_value_stays_in_value() {
    let parsed = YamlFile::from_str("k:\n  [q] x\n").unwrap();
    assert_eq!(
        debug::tree_to_string(parsed.syntax()),
        "\
DOCUMENT
  MAPPING
    MAPPING_ENTRY
      KEY
        SCALAR
          STRING: \"k\"
      COLON: \":\"
      VALUE
        NEWLINE: \"\\\\n\"
        INDENT: \"  \"
        SEQUENCE
          LEFT_BRACKET: \"[\"
          SEQUENCE_ENTRY
            SCALAR
              STRING: \"q\"
          RIGHT_BRACKET: \"]\"
        WHITESPACE: \" \"
        SCALAR
          STRING: \"x\"
      NEWLINE: \"\\\\n\"
"
    );
}

/// Content after a flow collection value may continue on, or start on,
/// a more indented line, as a plain scalar would.
#[test]
fn content_after_flow_value_on_later_lines() {
    let parsed = YamlFile::from_str("k:\n  [q] x\n  y\n").unwrap();
    assert_eq!(
        debug::tree_to_string(parsed.syntax()),
        "\
DOCUMENT
  MAPPING
    MAPPING_ENTRY
      KEY
        SCALAR
          STRING: \"k\"
      COLON: \":\"
      VALUE
        NEWLINE: \"\\\\n\"
        INDENT: \"  \"
        SEQUENCE
          LEFT_BRACKET: \"[\"
          SEQUENCE_ENTRY
            SCALAR
              STRING: \"q\"
          RIGHT_BRACKET: \"]\"
        WHITESPACE: \" \"
        SCALAR
          STRING: \"x\"
          NEWLINE: \"\\\\n\"
          INDENT: \"  \"
          STRING: \"y\"
      NEWLINE: \"\\\\n\"
"
    );

    let parsed = YamlFile::from_str("k:\n  [q]\n  y\n").unwrap();
    assert_eq!(
        debug::tree_to_string(parsed.syntax()),
        "\
DOCUMENT
  MAPPING
    MAPPING_ENTRY
      KEY
        SCALAR
          STRING: \"k\"
      COLON: \":\"
      VALUE
        NEWLINE: \"\\\\n\"
        INDENT: \"  \"
        SEQUENCE
          LEFT_BRACKET: \"[\"
          SEQUENCE_ENTRY
            SCALAR
              STRING: \"q\"
          RIGHT_BRACKET: \"]\"
        NEWLINE: \"\\\\n\"
        INDENT: \"  \"
        SCALAR
          STRING: \"y\"
      NEWLINE: \"\\\\n\"
"
    );
}

/// Content between a flow collection key and its colon stays in the
/// KEY, so `x: v` does not become a second entry on the same line.
#[test]
fn content_after_flow_key_stays_in_key() {
    let parsed = YamlFile::from_str("k:\n  [q] x: v\n").unwrap();
    assert_eq!(
        debug::tree_to_string(parsed.syntax()),
        "\
DOCUMENT
  MAPPING
    MAPPING_ENTRY
      KEY
        SCALAR
          STRING: \"k\"
      COLON: \":\"
      VALUE
        NEWLINE: \"\\\\n\"
        INDENT: \"  \"
        MAPPING
          MAPPING_ENTRY
            KEY
              SEQUENCE
                LEFT_BRACKET: \"[\"
                SEQUENCE_ENTRY
                  SCALAR
                    STRING: \"q\"
                RIGHT_BRACKET: \"]\"
              WHITESPACE: \" \"
              SCALAR
                STRING: \"x\"
            COLON: \":\"
            WHITESPACE: \" \"
            VALUE
              SCALAR
                STRING: \"v\"
            NEWLINE: \"\\\\n\"
"
    );
}

#[test]
fn push_commented_value_into_nested_flow_sequence() {
    // Keeping the comment breaks the flow mapping across lines; the
    // continuation has to stay inside `a`.
    let doc = Document::from_str("a:\n  seq: [1]\n").unwrap();
    let seq = doc
        .try_get_path("a.seq")
        .unwrap()
        .as_sequence()
        .unwrap()
        .clone();
    let item = Document::from_str("x: 1 # c\ny: 2\n").unwrap();
    seq.push(item.as_mapping().unwrap());
    assert_edited(&doc, "a:\n  seq: [1, {x: 1, # c\n    y: 2}]\n");
}

/// `[a, , c]` is three entries per YAML spec, the middle one an implicit
/// null; `[,]` is one null entry and `[,,]` two. A trailing comma is a
/// terminator, not a separator introducing a null entry.
#[test]
fn flow_sequence_null_entries() {
    check_renders(&[
        ("[a, , c]", "[a, ~, c]"),
        ("[,]", "[~]"),
        ("[,,]", "[~, ~]"),
        ("[a, b,]", "[a, b]"),
    ]);
}

/// Accessors agree with mutators on the indexes of a flow sequence with
/// an implicit null entry, as in a block sequence.
#[test]
fn flow_sequence_implicit_null_shares_indexes() {
    let doc = Document::from_str("[a, , c]").unwrap();
    let seq = doc.as_sequence().unwrap();
    assert_eq!(seq.len(), 3);
    assert_eq!(seq.get(0).unwrap().as_scalar().unwrap().as_string(), "a");
    assert_eq!(seq.get(1).unwrap().as_scalar().unwrap().as_string(), "");
    assert_eq!(seq.get(2).unwrap().as_scalar().unwrap().as_string(), "c");
    assert!(seq.set(1, "x"));
    assert_edited(&doc, "[a, x, c]");

    let doc = Document::from_str("[a, , c]").unwrap();
    let seq = doc.as_sequence().unwrap();
    let removed = seq.remove(1);
    assert_eq!(
        removed.and_then(|n| n.as_scalar().map(|s| s.as_string())),
        Some(String::new())
    );
    assert_eq!(seq.len(), 2);
    assert_edited(&doc, "[a, c]");
}

#[test]
fn flow_mapping_implicit_null_key_and_value_shapes() {
    // The parser emits `SCALAR { NULL "" }` at every "missing"
    // position so KEYs and VALUEs always hold exactly one child.
    for src in [
        "{,}",
        "{: v}",
        "{:}",
        "{k:}",
        "{k}",
        "{k, m}",
        "{a:, :b, :}",
    ] {
        let doc = Document::from_str(src).unwrap();
        validate_tree(doc.as_node().unwrap())
            .unwrap_or_else(|e| panic!("validate_tree({:?}) failed: {}", src, e));
        roundtrip_ok(doc.as_node().unwrap())
            .unwrap_or_else(|e| panic!("roundtrip({:?}) failed: {}", src, e));
    }
}

/// A single pair in a flow sequence with no key holds the same zero-width
/// null key as a flow mapping does.
#[test]
fn flow_pair_implicit_null_key_shape() {
    let yaml = YamlFile::from_str("[: 1]").unwrap();
    assert_eq!(
        yaml_edit::debug::tree_to_string(yaml.syntax()),
        "DOCUMENT
  SEQUENCE
    LEFT_BRACKET: \"[\"
    SEQUENCE_ENTRY
      MAPPING
        MAPPING_ENTRY
          KEY
            SCALAR
              NULL: \"\"
          COLON: \":\"
          WHITESPACE: \" \"
          VALUE
            SCALAR
              INT: \"1\"
    RIGHT_BRACKET: \"]\"
"
    );
}

/// `Mapping::new_pending_block` serializes as `{}` so an empty mapping
/// survives a round-trip, and turns into a block mapping once it has an
/// entry. Before this, it rendered as `key:` followed by blank space, which
/// reparses as null: the empty mapping was silently lost.
#[test]
fn pending_block_mapping_round_trips_and_becomes_block() {
    let doc = Document::from_str("a: 1\n").unwrap();
    doc.as_mapping()
        .unwrap()
        .set("k", Mapping::new_pending_block());
    assert_cst_ok(&doc);
    let text = doc.to_string();
    assert_eq!(text, "a: 1\nk: {}\n");

    let reparsed = Document::from_str(&text).unwrap();
    let value = reparsed.as_mapping().and_then(|m| m.get("k")).unwrap();
    assert!(
        value.as_mapping().is_some(),
        "empty mapping must survive as a mapping"
    );

    let doc = Document::from_str("a: 1\n").unwrap();
    let root = doc.as_mapping().unwrap();
    root.set("k", Mapping::new_pending_block());
    assert_cst_ok(&doc);
    let nested = root.get("k").unwrap();
    nested.as_mapping().unwrap().set("x", "1");
    assert_edited(&doc, "a: 1\nk:\n  x: '1'\n");
}

/// A `{}` that was in the source keeps its flow style when an entry is
/// added; only a pending-block placeholder is rewritten.
#[test]
fn parsed_flow_mapping_stays_flow_when_filled() {
    let doc = Document::from_str("m: {}\n").unwrap();
    let nested = doc.as_mapping().unwrap().get("m").unwrap();
    nested.as_mapping().unwrap().set("x", "1");
    assert_edited(&doc, "m: {x: '1'}\n");

    let doc = Document::from_str("a: 1\n").unwrap();
    let root = doc.as_mapping().unwrap();
    root.set("k", Mapping::new_flow());
    assert_cst_ok(&doc);
    let nested = root.get("k").unwrap();
    nested.as_mapping().unwrap().set("x", "1");
    assert_edited(&doc, "a: 1\nk: {x: '1'}\n");
}

/// The placeholder marker lives only in the tree: once serialized and
/// reparsed there is nothing left to distinguish it from a source `{}`, so
/// it stays flow. YAML has nowhere to record the intent.
#[test]
fn pending_block_marker_does_not_survive_reparse() {
    let doc = Document::from_str("a: 1\n").unwrap();
    doc.as_mapping()
        .unwrap()
        .set("k", Mapping::new_pending_block());
    assert_cst_ok(&doc);
    let reparsed = Document::from_str(&doc.to_string()).unwrap();
    let nested = reparsed.as_mapping().unwrap().get("k").unwrap();
    nested.as_mapping().unwrap().set("x", "1");
    assert_edited(&reparsed, "a: 1\nk: {x: '1'}\n");
}

/// An edit applied to a mapping.
type MappingEdit<'a> = dyn Fn(&Mapping) + 'a;

/// A single pair in a flow sequence (`[a: 1]`) is a flow mapping without
/// braces. Edits to it give flow output, and it gains braces once it no
/// longer holds exactly one pair, which keeps it one mapping.
#[test]
fn edit_single_pair_in_flow_sequence() {
    let block_seq = || {
        yaml_edit::Document::from_str("- 1\n- 2\n")
            .unwrap()
            .as_sequence()
            .unwrap()
    };
    let cases: &[(&str, &MappingEdit<'_>, &str)] = &[
        ("[a: 1]\n", &|m| m.set("a", 2), "[a: 2]\n"),
        ("[a: 1]\n", &|m| m.set("a", block_seq()), "[a: [1, 2]]\n"),
        (
            "[a: 1]\n",
            &|m| m.set("a", vec![YamlValue::from("x"), YamlValue::from("y")]),
            "[a: [x, y]]\n",
        ),
        ("[a: 1]\n", &|m| m.set("b", 2), "[{a: 1, b: 2}]\n"),
        ("[a: 1, x]\n", &|m| m.set("b", 2), "[{a: 1, b: 2}, x]\n"),
        (
            "[x, a: 1]\n",
            &|m| m.insert_at_index(0, "b", 2),
            "[x, {b: 2, a: 1}]\n",
        ),
        (
            "[a: 1]\n",
            &|m| assert!(m.insert_after("a", "b", 2)),
            "[{a: 1, b: 2}]\n",
        ),
        ("[a: 1]\n", &|m| assert!(m.remove("a").is_some()), "[{}]\n"),
        ("[a: 1]\n", &|m| m.clear(), "[{}]\n"),
        ("[a: 1]\n", &|m| assert!(m.rename_key("a", "b")), "[b: 1]\n"),
        ("[? a : 1]\n", &|m| m.set("b", 2), "[{? a : 1, b: 2}]\n"),
        ("[? a : 1]\n", &|m| m.set("a", 2), "[? a : 2]\n"),
        (
            "[? a : 1]\n",
            &|m| assert!(m.remove("a").is_some()),
            "[{}]\n",
        ),
        (
            "[? a : 1, x]\n",
            &|m| assert!(m.rename_key("a", "b")),
            "[? b : 1, x]\n",
        ),
        ("[: 1]\n", &|m| m.set("b", 2), "[{: 1, b: 2}]\n"),
        ("[\"a\": 1]\n", &|m| m.set("a", "x"), "[\"a\": \"x\"]\n"),
        (
            "[\"a\": 1]\n",
            &|m| m.set("b", "x"),
            "[{\"a\": 1, \"b\": \"x\"}]\n",
        ),
    ];
    for (source, edit, expected) in cases {
        let yaml = YamlFile::from_str(source).unwrap();
        let doc = yaml.document().unwrap();
        let seq = doc.as_sequence().unwrap();
        let mapping = (0..seq.len())
            .find_map(|i| seq.get(i).and_then(|n| n.as_mapping().cloned()))
            .unwrap();
        assert!(mapping.is_flow_style());
        edit(&mapping);
        assert_file_edited_valid(&yaml, expected);
    }
}

/// Edits keep the `?` of an explicit key in a flow mapping, and new entries
/// beside it are implicit, as in a single pair in a flow sequence.
#[test]
fn edit_flow_mapping_with_explicit_key() {
    let cases: &[(&str, &MappingEdit<'_>, &str)] = &[
        ("{? a : 1}\n", &|m| m.set("b", 2), "{? a : 1, b: 2}\n"),
        ("{? a : 1}\n", &|m| m.set("a", 2), "{? a : 2}\n"),
        (
            "{? a : 1}\n",
            &|m| assert!(m.rename_key("a", "b")),
            "{? b : 1}\n",
        ),
        (
            "{? a : 1, c: 3}\n",
            &|m| assert!(m.remove("a").is_some()),
            "{c: 3}\n",
        ),
    ];
    for (source, edit, expected) in cases {
        let yaml = YamlFile::from_str(source).unwrap();
        let mapping = yaml.document().unwrap().as_mapping().unwrap();
        edit(&mapping);
        assert_file_edited_valid(&yaml, expected);
    }
}

/// A block collection set as a value in a flow mapping is written in flow
/// form.
#[test]
fn set_block_value_in_flow_mapping() {
    let source = YamlFile::from_str("k:\n  - x\n  - y\n")
        .unwrap()
        .document()
        .unwrap();
    let block = source.as_mapping().unwrap().get("k").unwrap();
    let doc = YamlFile::from_str("{a: 1}\n").unwrap().document().unwrap();
    doc.as_mapping().unwrap().set("a", block);
    assert_cst_ok(&doc);
    assert_parser_shaped(&doc);
    assert_eq!(doc.to_string(), "{a: [x, y]}\n");
}

/// Setting into a flow mapping inserts entries inside the braces with `, `
/// separators, not on new lines after `}`.
#[test]
fn set_into_flow_mapping_inserts_inside_braces() {
    for (input, key, expected) in [
        ("outer: {}", "a", "outer: {a: X}"),
        ("outer: {a: 1}", "b", "outer: {a: 1, b: X}"),
        // The trailing comma stays at the end.
        ("outer: {a: 1,}", "b", "outer: {a: 1, b: X,}"),
        // Update-in-place stays inside the braces.
        ("outer: {a: 1}", "a", "outer: {a: X}"),
    ] {
        check_edit(
            input,
            |d| {
                d.as_mapping()
                    .unwrap()
                    .get_mapping("outer")
                    .unwrap()
                    .set(key, "X")
            },
            expected,
        );
    }
}

/// Quoted strings in flow sequences do not take the line break and
/// indentation after them into their SCALAR node.
#[test]
fn quoted_strings_in_flow_sequence() {
    for yaml in [
        "{\n  \"items\": [\n    \"first\",\n    \"second\"\n  ]\n}",
        r#"{"items": ["first", "second"]}"#,
        "\nitems: [\"first\", \"second\"]\n",
    ] {
        let doc = Document::from_str(yaml).unwrap();
        let items = doc.as_mapping().unwrap().get_sequence("items").unwrap();
        let values: Vec<_> = items
            .values()
            .map(|node| {
                let scalar = node.as_scalar().unwrap().clone();
                (scalar.value(), scalar.as_string())
            })
            .collect();
        assert_eq!(
            values,
            [
                ("\"first\"".to_string(), "first".to_string()),
                ("\"second\"".to_string(), "second".to_string())
            ],
            "{yaml:?}"
        );
    }
}
