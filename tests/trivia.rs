//! Comments and blank lines: where the parser puts them, and how edits keep
//! them. Comments around a value that is replaced: those inside it go with
//! it, the key-line or dash-line comment stays with the entry, and
//! everything outside the value is left as it was.

mod common;
use common::{
    assert_file_edited, check_renders, count_nodes, count_tokens, edited, parse_clean,
    render_documents,
};
use rowan::ast::AstNode;
use std::str::FromStr;
use yaml_edit::path::YamlPath;
use yaml_edit::{
    debug, Mapping, MappingBuilder, MappingEntry, Sequence, SyntaxKind, YamlFile, YamlNode,
    YamlValue,
};

/// Parse `yaml` and check it round trips exactly.
#[track_caller]
fn parse(yaml: &str) -> YamlFile {
    let file = YamlFile::from_str(yaml).unwrap();
    assert_eq!(file.to_string(), yaml);
    file
}

fn text(node: &YamlNode) -> String {
    node.as_scalar().unwrap().as_string()
}

/// Comments in flow collections round trip and leave the content as it is
/// without them.
#[test]
fn comments_in_flow_collections() {
    for (yaml, without_comments) in [
        ("[a, # comment\n b]", "[a, b]"),
        (
            "{key1: value1, # comment\n key2: value2}",
            "{key1: value1, key2: value2}",
        ),
        ("[a, # c1\n b, # c2\n c]", "[a, b, c]"),
        ("[a, b # comment\n]", "[a, b]"),
        ("{key: # comment\n value}", "{key: value}"),
        ("[# comment\na, b]", "[a, b]"),
        ("{# comment\nkey: value}", "{key: value}"),
        ("[a,# comment\nb]", "[a, b]"),
        ("[[a,# c1\nb],# c2\nc]", "[[a, b], c]"),
        // YAML spec example 6.1 (test 6HB6).
        (
            r#"Flow style: [    # Leading spaces
   By two,        # in flow style
  Also by two,    # are neither
  	Still by two   # content nor
    ]             # indentation."#,
            "Flow style: [By two, Also by two, Still by two]",
        ),
    ] {
        parse(yaml);
        assert_eq!(
            YamlValue::parse_raw(yaml).unwrap(),
            YamlValue::parse_raw(without_comments).unwrap(),
            "{yaml:?}"
        );
    }
}

#[test]
fn comments_after_multi_line_flow_entries() {
    let file = parse(
        r#"flow_sequence: [
    item1,  # Comment 1
    item2,  # Comment 2
    item3   # Comment 3
]
flow_mapping: {
    key1: value1,  # Comment A
    key2: value2,  # Comment B
    key3: value3   # Comment C
}"#,
    );
    let root = file.document().unwrap().as_mapping().unwrap();
    assert_eq!(root.len(), 2);

    let seq = root.get_sequence("flow_sequence").unwrap();
    let items: Vec<String> = seq.values().map(|item| text(&item)).collect();
    assert_eq!(items, ["item1", "item2", "item3"]);

    let map = root.get_mapping("flow_mapping").unwrap();
    let entries: Vec<(String, String)> = map.iter().map(|(k, v)| (text(&k), text(&v))).collect();
    assert_eq!(
        entries,
        [("key1", "value1"), ("key2", "value2"), ("key3", "value3")]
            .map(|(k, v)| (k.to_string(), v.to_string()))
    );
}

#[test]
fn comment_forms() {
    for (yaml, key, value) in [
        // The spec allows a comment with nothing after the `#`.
        ("key: value  #  \n", "key", "value"),
        ("key: value  #   \n", "key", "value"),
        ("key: value\n# Final comment", "key", "value"),
        // A `#` inside a quoted string is not a comment.
        (
            "text: \"This is # not a comment\"\n",
            "text",
            "This is # not a comment",
        ),
        (
            "text: \"This is # not a comment\"",
            "text",
            "This is # not a comment",
        ),
        ("key: value  # 🎉 important! 日本語\n", "key", "value"),
        ("key: value  ## comment ### more\n", "key", "value"),
        (
            "%YAML 1.2 # this is a comment\n---\nkey: value\n",
            "key",
            "value",
        ),
    ] {
        let file = parse(yaml);
        let mapping = file.document().unwrap().as_mapping().unwrap();
        assert_eq!(mapping.len(), 1, "{yaml:?}");
        assert_eq!(text(&mapping.get(key).unwrap()), value, "{yaml:?}");
    }
}

/// A comment on its own line does not end the block it sits in, whatever
/// its own column. What ends the block is the next line with content.
///
/// `a:\n  - x\n# c\n  - y\n` keeps both entries, as both saphyr and PyYAML
/// read it. The comment loops compared the comment's own indentation, so a
/// column-0 comment ended the collection: the sequence case stranded the
/// later entries in an ERROR node, and the mapping case dropped them with
/// no ERROR node at all. The dedent check only excused a comment at column
/// 0, so an indented one ended the mapping just the same.
#[test]
fn comment_line_column_does_not_end_a_block() {
    check_renders(&[
        ("a:\n  - x\n # c\n  - y\n", "map{a: seq[x, y]}"),
        ("a:\n  - x\n# c\n  - y\n", "map{a: seq[x, y]}"),
        ("a:\n  - x\n  # c\n  - y\n", "map{a: seq[x, y]}"),
        ("a:\n  - x\n# c\n# d\n  - y\n", "map{a: seq[x, y]}"),
        ("a:\n  - x\n# c\n\n  - y\n", "map{a: seq[x, y]}"),
        ("a:\n  k: 1\n# c\n  j: 2\n", "map{a: map{k: 1, j: 2}}"),
        ("a:\n  b: 1\n # c\n  d: 2\n", "map{a: map{b: 1, d: 2}}"),
        ("k: x\n\n  #\nk2: w\n", "map{k: x, k2: w}"),
        ("k: x\n  # c\nk2: w\n", "map{k: x, k2: w}"),
        ("- v\n\n  #\n- x\n", "seq[v, x]"),
        ("a:\n  b: 1\n\n    # c\nc: 2\n", "map{a: map{b: 1}, c: 2}"),
        // The block does end when the next content line is dedented, or
        // when nothing follows the comment at all.
        ("a:\n  - x\n# c\nb: 1\n", "map{a: seq[x], b: 1}"),
        (
            "items:  # a list\n  - one\n  - two  # inline\n",
            "map{items: seq[one, two]}",
        ),
        // Nor does a comment line between a bare `-` and its value say
        // where the value sits. The value used to be hoisted out to the
        // top-level mapping.
        ("-\n# c\n  x: 1\n", "seq[map{x: 1}]"),
        ("-\n      # c\n  x: 1\n", "seq[map{x: 1}]"),
        ("- # c\n# d\n  x: 1\n", "seq[map{x: 1}]"),
        ("-\n   # c\n  - x\n", "seq[seq[x]]"),
        (
            "q:\n    - -\n      # c\n          x: 1\n",
            "map{q: seq[seq[map{x: 1}]]}",
        ),
    ]);
}

/// Each comment of a run used to rescan the rest of it, so this took
/// quadratic time and ran for minutes.
#[test]
fn long_comment_run_between_entries() {
    for comment in ["  # c\n", "# c\n"] {
        let yaml = format!("a:\n  b: 1\n{}  d: 2\n", comment.repeat(50_000));
        let file = YamlFile::from_str(&yaml).unwrap();
        let a = mapping_at(&file, "a");
        let keys: Vec<String> = a.keys().map(|k| text(&k)).collect();
        assert_eq!(keys, ["b", "d"]);
    }
}

/// A comment is not a value, so a sequence entry carrying only one is an
/// implicit null, unless the entry's value follows on a later line.
///
/// The YAML test suite's W42U has `- # Empty` as a null entry; parsing the
/// comment as a value left an empty SEQUENCE behind. Its RZP5 has
/// `- #lala\n  seq2`, where the value really is on the next line.
#[test]
fn comment_only_sequence_entry_is_null() {
    for (yaml, expected) in [
        ("- # c\n- a\n", "seq[~, a]"),
        ("- # c\n- |\n b\n", "seq[~, b\n]"),
        ("- # c\n  v\n", "seq[v]"),
        ("- a # c\n- b\n", "seq[a, b]"),
    ] {
        assert_eq!(render_documents(yaml), [expected], "{yaml:?}");
        let file = parse_clean(yaml);
        assert_eq!(count_nodes(&file, SyntaxKind::SEQUENCE), 1, "{yaml:?}");
    }
}

/// A `#` is a comment only when whitespace precedes it, wherever it sits.
///
/// On a plain scalar's continuation line the previous token is the INDENT,
/// so requiring the previous *token* to be scalar content made `#` a
/// comment there: the test suite's FBC9 keeps `!"#$%` as content on both
/// its lines. Glued to a flow delimiter (`c,#x`) it is still the 6.6
/// violation CVW2 expects, and a quote ends the scalar.
#[test]
fn hash_glued_to_content_on_a_continuation_line() {
    for (yaml, comments) in [
        ("safe: a!\"#$%\n     !\"#$%\n", 0),
        ("k: v# c\n", 0),
        ("k: \"v\"# c\n", 1),
        ("[ a, b,#x\n]\n", 1),
        ("k: v # c\n", 1),
    ] {
        let file = YamlFile::from_str(yaml).unwrap();
        assert_eq!(
            count_tokens(&file, SyntaxKind::COMMENT),
            comments,
            "{yaml:?}"
        );
    }
}

/// A comment line between two flow entries describes the one below it, as
/// in a block collection: it moves with that entry, and stays when the
/// entry above is removed.
#[test]
fn comment_line_belongs_to_flow_entry_below() {
    let source = "{\n  a: 1,\n  # about b\n  b: 2\n}\n";
    let moved = "{\n  # about b\n  b: 2,\n  a: 1\n}\n";
    assert_eq!(
        edited(source, |d| d
            .as_mapping()
            .unwrap()
            .reorder_fields(["b", "a"])),
        moved
    );
    assert_eq!(
        edited(source, |d| {
            assert!(d.as_mapping().unwrap().move_after("b", "a", 1));
        }),
        moved
    );
    assert_eq!(
        edited(source, |d| {
            d.as_mapping().unwrap().remove("a").unwrap();
        }),
        "{\n  # about b\n  b: 2\n}\n"
    );
    assert_eq!(
        edited("[\n  a,\n  # c\n  b\n]\n", |d| {
            d.as_sequence().unwrap().remove(0).unwrap();
        }),
        "[\n  # c\n  b\n]\n"
    );
}

/// Blank lines between entries leave the entries after them siblings of
/// the ones before, rather than nesting them.
#[test]
fn root_mapping_with_blank_lines() {
    let file = parse(
        r#"
first: value1

second: value2

third: value3
"#,
    );
    let mapping = file.document().unwrap().as_mapping().unwrap();
    let entries: Vec<(String, String)> =
        mapping.iter().map(|(k, v)| (text(&k), text(&v))).collect();
    assert_eq!(
        entries,
        [
            ("first", "value1"),
            ("second", "value2"),
            ("third", "value3")
        ]
        .map(|(k, v)| (k.to_string(), v.to_string()))
    );
}

#[test]
fn nested_mapping_with_blank_lines() {
    let file = parse(
        r#"
config:
  database:
    host: localhost
    port: 5432

  cache:
    host: redis
    port: 6379

  logging:
    level: info
"#,
    );
    let mapping = file.document().unwrap().as_mapping().unwrap();
    assert_eq!(mapping.len(), 1);
    let config = mapping.get_mapping("config").unwrap();
    assert_eq!(config.len(), 3);
    for (path, value) in [
        ("config.database.host", "localhost"),
        ("config.database.port", "5432"),
        ("config.cache.host", "redis"),
        ("config.cache.port", "6379"),
        ("config.logging.level", "info"),
    ] {
        let doc = file.document().unwrap();
        assert_eq!(text(&doc.try_get_path(path).unwrap()), value, "{path}");
    }
}

/// The case that first showed blank lines nesting the entries after them.
#[test]
fn mapping_with_anchors_and_blank_lines() {
    let file = parse(
        r#"
defaults: &defaults
  timeout: 30
  retries: 3

production:
  <<: *defaults
  host: prod.example.com

development:
  <<: *defaults
  host: dev.example.com
"#,
    );
    let doc = file.document().unwrap();
    assert_eq!(doc.as_mapping().unwrap().len(), 3);
    for (path, value) in [
        ("defaults.timeout", "30"),
        ("production.host", "prod.example.com"),
        ("development.host", "dev.example.com"),
    ] {
        assert_eq!(text(&doc.try_get_path(path).unwrap()), value, "{path}");
    }
}

#[test]
fn anchor_with_no_value_followed_by_alias() {
    let file = parse(
        r#"
anchor: &test
ref: *test
another: value
"#,
    );
    let mapping = file.document().unwrap().as_mapping().unwrap();
    assert_eq!(mapping.len(), 3);
    assert!(mapping
        .get("anchor")
        .unwrap()
        .as_scalar()
        .unwrap()
        .is_null());
    let ref_val = mapping.get("ref").unwrap();
    assert!(ref_val.is_alias());
    assert_eq!(ref_val.as_alias().unwrap().name(), "test");
    assert_eq!(text(&mapping.get("another").unwrap()), "value");
}

/// <https://github.com/jelmer/yaml-edit/issues/26>
#[test]
fn issue_26_blank_line_between_key_and_nested_value() {
    let file = parse("foo: bar\n\nbaz:\n\n  qux: ~\n");
    let mapping = file.document().unwrap().as_mapping().unwrap();
    assert_eq!(mapping.len(), 2);
    assert_eq!(text(&mapping.get("foo").unwrap()), "bar");
    let baz = mapping.get_mapping("baz").unwrap();
    assert_eq!(baz.len(), 1);
    assert_eq!(text(&baz.get("qux").unwrap()), "~");
}

/// From a comment on issue #26: a key with no value, followed by one at the
/// same indent.
#[test]
fn issue_26_null_value_without_blank_line() {
    let file = parse("foo:\n  bar1: 42\n  bar2:\n  bar3: \"hello\"\n");
    let foo = file
        .document()
        .unwrap()
        .as_mapping()
        .unwrap()
        .get_mapping("foo")
        .unwrap();
    assert_eq!(foo.len(), 3);
    assert_eq!(text(&foo.get("bar1").unwrap()), "42");
    assert!(foo.get("bar2").unwrap().as_scalar().unwrap().is_null());
    assert_eq!(text(&foo.get("bar3").unwrap()), "hello");
}

/// A blank line carrying trailing whitespace shallower than the block it sits
/// in must not end that block. The dedent check compared the line's INDENT
/// against the base indent without noticing the line was blank, so `- y` and
/// everything after it was swept into an ERROR node while `from_str` still
/// reported success.
#[test]
fn underindented_blank_line_inside_block() {
    let file = parse("a:\n  - x\n \n  - y\n");
    let seq = file
        .document()
        .unwrap()
        .as_mapping()
        .unwrap()
        .get_sequence("a")
        .unwrap();
    let items: Vec<String> = seq.values().map(|v| text(&v)).collect();
    assert_eq!(items, ["x", "y"]);

    let file = parse("a:\n  b: x\n \n  c: y\n");
    let inner = file
        .document()
        .unwrap()
        .as_mapping()
        .unwrap()
        .get_mapping("a")
        .unwrap();
    let entries: Vec<(String, String)> = inner.iter().map(|(k, v)| (text(&k), text(&v))).collect();
    assert_eq!(
        entries,
        [("b", "x"), ("c", "y")].map(|(k, v)| (k.to_string(), v.to_string()))
    );
}

/// Every blank-line indent shallower than the block must behave the same, so
/// sweep the range rather than pinning the one width the fuzzer happened to
/// find.
#[test]
fn blank_line_indent_never_ends_a_block() {
    for width in 0..6 {
        let yaml = format!("a:\n    - x\n{}\n    - y\n", " ".repeat(width));
        let file = parse(&yaml);
        let mapping = file.document().unwrap().as_mapping().unwrap();
        let a = mapping.get_sequence("a").unwrap();
        assert_eq!(a.len(), 2, "blank line indented by {width}");
    }
}

/// The mapping at `path` in `file`, following mapping keys and sequence
/// indexes.
fn mapping_at(file: &YamlFile, path: &str) -> Mapping {
    let doc = file.document().unwrap();
    if path.is_empty() {
        return doc.as_mapping().unwrap();
    }
    match doc.try_get_path(path).unwrap() {
        YamlNode::Mapping(m) => m,
        other => panic!("not a mapping at {path:?}: {other:?}"),
    }
}

fn sequence_at(file: &YamlFile, path: &str) -> Sequence {
    let doc = file.document().unwrap();
    if path.is_empty() {
        return doc.as_sequence().unwrap();
    }
    match doc.try_get_path(path).unwrap() {
        YamlNode::Sequence(s) => s,
        other => panic!("not a sequence at {path:?}: {other:?}"),
    }
}

fn block_mapping() -> Mapping {
    MappingBuilder::new()
        .pair("y", 2)
        .build_document()
        .as_mapping()
        .unwrap()
}

fn value_of(src: &str) -> YamlNode {
    let file = YamlFile::from_str(src).unwrap();
    file.document()
        .unwrap()
        .as_mapping()
        .unwrap()
        .get("v")
        .unwrap()
}

#[test]
fn mapping_set_scalar_over_commented_value() {
    for (src, path, expected) in [
        ("a:  # c\n  # d\n  x: 1\n", "", "a: 5  # c\n"),
        (
            "# before\na:  # c\n  x: 1  # e\n  # f\n# after\nb: 2\n",
            "",
            "# before\na: 5  # c\n  # f\n# after\nb: 2\n",
        ),
        ("a:\n  - 1  # e\nb: 2\n", "", "a: 5\nb: 2\n"),
        ("a:\n  - 1\n  - 2  # e\nb: 2\n", "", "a: 5\nb: 2\n"),
        ("a:\n  - 1  # e\n", "", "a: 5\n"),
        ("o:\n  a:\n    - 1  # e\nz: 1\n", "o", "o:\n  a: 5\nz: 1\n"),
        ("a:\n  x: 1\n\nb: 2\n", "", "a: 5\n\nb: 2\n"),
        ("a:\n  x: 1\n\n  # f\nb: 2\n", "", "a: 5\n\n  # f\nb: 2\n"),
        (
            "o:\n  a:\n    x: 1\n    # f\nz: 1\n",
            "o",
            "o:\n  a: 5\n    # f\nz: 1\n",
        ),
        (
            "a: |  # h\n  text\n# after\nb: 2\n",
            "",
            "a: 5  # h\n# after\nb: 2\n",
        ),
        (
            "a: 1  # c\n# after\nb: 2\n",
            "",
            "a: 5  # c\n# after\nb: 2\n",
        ),
        ("a: [1,  # e\n  2]\nb: 2\n", "", "a: 5  # e\nb: 2\n"),
        ("? a\n:\n  - 1  # e\nb: 2\n", "", "? a\n: 5\nb: 2\n"),
        (
            "? a\n:\n  x: 1\n  # f\nb: 2\n",
            "",
            "? a\n: 5\n  # f\nb: 2\n",
        ),
    ] {
        let file = YamlFile::from_str(src).unwrap();
        mapping_at(&file, path).set("a", 5);
        assert_file_edited(&file, expected);
    }
}

#[test]
fn mapping_set_block_over_commented_value() {
    for (src, expected) in [
        ("a:  # c\n  # d\n  x: 1\n", "a:  # c\n  y: 2\n"),
        (
            "a:  # c\n  x: 1  # e\n  # f\n# after\nb: 2\n",
            "a:  # c\n  y: 2\n  # f\n# after\nb: 2\n",
        ),
        ("a:\n  - 1  # e\nb: 2\n", "a:\n  y: 2\nb: 2\n"),
        ("a:\n  x: 1\n\nb: 2\n", "a:\n  y: 2\n\nb: 2\n"),
        (
            "a: 1  # c\n# after\nb: 2\n",
            "a:  # c\n  y: 2\n# after\nb: 2\n",
        ),
    ] {
        let file = YamlFile::from_str(src).unwrap();
        mapping_at(&file, "").set("a", block_mapping());
        assert_file_edited(&file, expected);
    }
}

#[test]
fn sequence_set_over_commented_value() {
    for (src, expected) in [
        ("- x: 1  # c\n- 2\n", "- 5  # c\n- 2\n"),
        ("- x: 1  # c\n", "- 5  # c\n"),
        ("- x: 1  # c\n  y: 2  # d\n- 3\n", "- 5  # c\n- 3\n"),
        ("- x: |  # h\n    t\n- 2\n", "- 5  # h\n- 2\n"),
        ("- - x: 1  # c\n- 2\n", "- 5  # c\n- 2\n"),
        ("- - 1  # c\n  - 2\n- 3\n", "- 5  # c\n- 3\n"),
        ("- - 1\n  - 2  # c\n- 3\n", "- 5\n- 3\n"),
        ("- - 1  # c\n- 3\n", "- 5  # c\n- 3\n"),
        ("- - 1  # c\n", "- 5  # c\n"),
        ("- {x: 1,  # c\n  y: 2}\n- 3\n", "- 5  # c\n- 3\n"),
        ("- 1  # c\n- 2\n", "- 5  # c\n- 2\n"),
        ("- # c\n  - 1\n  # d\n- 2\n", "- 5 # c\n  # d\n- 2\n"),
        ("-  # c\n  x: 1\n- 2\n", "- 5  # c\n- 2\n"),
        ("- |  # h\n  t\n- 2\n", "- 5  # h\n- 2\n"),
        ("- [1,  # c\n  2]\n- 2\n", "- 5  # c\n- 2\n"),
        ("- 1  # c\n", "- 5  # c\n"),
        ("-\n  x  # c\n- 2\n", "-\n  5\n- 2\n"),
        ("- x: 1\n\n- 2\n", "- 5\n\n- 2\n"),
        (
            "- x: 1\n  # inner\n# outer\n- 2\n",
            "- 5\n  # inner\n# outer\n- 2\n",
        ),
    ] {
        let file = YamlFile::from_str(src).unwrap();
        sequence_at(&file, "").set(0, 5);
        assert_file_edited(&file, expected);
    }
}

/// The spaces of a last line with no line break are part of the block
/// scalar they end, and go with it.
#[test]
fn set_over_block_scalar_ending_in_spaces() {
    for (src, expected) in [
        ("- |+\n   ", "- 5\n"),
        ("- |\n  a\n   ", "- 5\n"),
        ("- |\n  a\n  ", "- 5\n"),
    ] {
        let file = YamlFile::from_str(src).unwrap();
        sequence_at(&file, "").set(0, 5);
        assert_file_edited(&file, expected);
    }
    for (src, expected) in [("a: |+\n   ", "a: 5\n"), ("a: |\n  a\n   ", "a: 5\n")] {
        let file = YamlFile::from_str(src).unwrap();
        mapping_at(&file, "").set("a", 5);
        assert_file_edited(&file, expected);
    }
}

#[test]
fn set_to_empty_node_leaves_no_trailing_space() {
    let null = value_of("v:\n");
    let file = YamlFile::from_str("- 1\n- 2\n").unwrap();
    sequence_at(&file, "").set(0, &null);
    assert_file_edited(&file, "-\n- 2\n");

    let file = YamlFile::from_str("a: 1\nb: 2\n").unwrap();
    mapping_at(&file, "").set("a", &null);
    assert_file_edited(&file, "a:\nb: 2\n");
}

#[test]
fn nested_sequence_set_over_commented_value() {
    for (src, expected) in [
        ("a:\n  - x: 1\n\nb: 2\n", "a:\n  - 5\n\nb: 2\n"),
        ("a:\n  - - 1\n    - 2  # c\nb: 2\n", "a:\n  - 5\nb: 2\n"),
        ("a:\n  - x: 1  # c\n  - 2\n", "a:\n  - 5  # c\n  - 2\n"),
        ("a:\n  - - 1  # c\nb: 2\n", "a:\n  - 5  # c\nb: 2\n"),
        ("a:\n  - |  # h\n    t\nb: 2\n", "a:\n  - 5  # h\nb: 2\n"),
        ("a:\n  - |  # h\n    t\n  - 2\n", "a:\n  - 5  # h\n  - 2\n"),
        ("a:\n  - |  # h\n    t\n", "a:\n  - 5  # h\n"),
    ] {
        let file = YamlFile::from_str(src).unwrap();
        sequence_at(&file, "a").set(0, 5);
        assert_file_edited(&file, expected);
    }
}

#[test]
fn sequence_set_block_over_commented_value() {
    for (src, expected) in [
        ("- x: 1  # c\n- 2\n", "-  # c\n  y: 2\n- 2\n"),
        ("- - 1  # c\n- 3\n", "-  # c\n  y: 2\n- 3\n"),
        ("- - 1  # c\n  - 2\n- 3\n", "-  # c\n  y: 2\n- 3\n"),
        ("- x: 1\n\n- 2\n", "- y: 2\n\n- 2\n"),
        ("- x: 1\n  # inner\n- 2\n", "- y: 2\n  # inner\n- 2\n"),
        ("-  # c\n  x: 1\n- 2\n", "-  # c\n  y: 2\n- 2\n"),
        ("- 1  # c\n- 2\n", "-  # c\n  y: 2\n- 2\n"),
        ("- |  # h\n  t\n- 2\n", "-  # h\n  y: 2\n- 2\n"),
        ("-\n  x  # c\n- 2\n", "-\n  y: 2\n- 2\n"),
        ("-\n  x  # c\n", "-\n  y: 2\n"),
    ] {
        let file = YamlFile::from_str(src).unwrap();
        sequence_at(&file, "").set(0, block_mapping());
        assert_file_edited(&file, expected);
    }
}

#[test]
fn sequence_set_block_scalar_keeps_dash_line_comment() {
    for (src, expected) in [
        ("- 1  # c\n- 2\n", "- |  # c\n  text\n- 2\n"),
        ("-  # c\n  x: 1\n- 2\n", "- |  # c\n  text\n- 2\n"),
        ("- x: 1  # c\n- 2\n", "- |  # c\n  text\n- 2\n"),
        ("- - 1  # c\n- 2\n", "- |  # c\n  text\n- 2\n"),
    ] {
        let file = YamlFile::from_str(src).unwrap();
        sequence_at(&file, "").set(0, value_of("v: |\n  text\n"));
        assert_file_edited(&file, expected);
    }
}

/// A block scalar header with a comment of its own leaves no room on the
/// line for the entry's, which moves to a line of its own above the entry.
#[test]
fn block_scalar_header_comment_displaces_key_line_comment() {
    for (src, path, expected) in [
        ("k: 1  # note\n", "", "# note\nk: |  # src\n  text\n"),
        ("k:  # note\n  x: 1\n", "", "# note\nk: |  # src\n  text\n"),
        (
            "a: 0\nk: 1  # note\n",
            "",
            "a: 0\n# note\nk: |  # src\n  text\n",
        ),
        (
            "---\nk: 1  # note\n",
            "",
            "---\n# note\nk: |  # src\n  text\n",
        ),
        (
            "o:\n  k: 1  # note\n",
            "o",
            "o:\n  # note\n  k: |  # src\n    text\n",
        ),
        (
            "o:\n  j: 0\n  k: 1  # note\n",
            "o",
            "o:\n  j: 0\n  # note\n  k: |  # src\n    text\n",
        ),
        (
            "- k: 1  # note\n  j: 2\n",
            "[0]",
            "- # note\n  k: |  # src\n    text\n  j: 2\n",
        ),
    ] {
        let file = YamlFile::from_str(src).unwrap();
        mapping_at(&file, path).set("k", value_of("v: |  # src\n  text\n"));
        assert_file_edited(&file, expected);
    }
    for (value, expected) in [
        (
            "v: &a |  # src\n  text\n",
            "# note\nk: &a |  # src\n  text\n",
        ),
        (
            "v: !t |  # src\n  text\n",
            "# note\nk: !t |  # src\n  text\n",
        ),
        ("v: |\n  text\n", "k: |  # note\n  text\n"),
    ] {
        let src = "k: 1  # note\n";
        let file = YamlFile::from_str(src).unwrap();
        mapping_at(&file, "").set("k", value_of(value));
        assert_file_edited(&file, expected);
    }
}

#[test]
fn sequence_block_scalar_header_comment_displaces_dash_line_comment() {
    for (src, path, index, expected) in [
        ("- 1  # c\n- 2\n", "", 0, "# c\n- |  # src\n  text\n- 2\n"),
        ("- 0\n- 1  # c\n", "", 1, "- 0\n# c\n- |  # src\n  text\n"),
        (
            "a:\n  - 1  # c\n",
            "a",
            0,
            "a:\n  # c\n  - |  # src\n    text\n",
        ),
    ] {
        let file = YamlFile::from_str(src).unwrap();
        sequence_at(&file, path).set(index, value_of("v: |  # src\n  text\n"));
        assert_file_edited(&file, expected);
    }
}

/// The lines after the entry are laid out as the parser does after the new
/// value. A comment line indented as far as a new block scalar's body would
/// read as part of it, so it moves back to the entry's column.
#[test]
fn lines_after_entry_follow_the_new_value() {
    for (src, path, block, expected) in [
        (
            "a: 1\n  # f\nb: 2\n",
            "",
            false,
            "a:\n  y: 2\n  # f\nb: 2\n",
        ),
        ("a: 1\n\nb: 2\n", "", false, "a:\n  y: 2\n\nb: 2\n"),
        (
            "o:\n  a: 1\n\n  b: 2\n",
            "o",
            false,
            "o:\n  a:\n    y: 2\n\n  b: 2\n",
        ),
        (
            "a: 1\n\n  # f\nb: 2\n",
            "",
            false,
            "a:\n  y: 2\n\n  # f\nb: 2\n",
        ),
        ("a: 1\n  # f\nb: 2\n", "", true, "a: |\n  text\n# f\nb: 2\n"),
        (
            "o:\n  a: 1\n    # f\n  b: 2\n",
            "o",
            true,
            "o:\n  a: |\n    text\n  # f\n  b: 2\n",
        ),
        (
            "a:\n  x: 1\n  # f\nb: 2\n",
            "",
            true,
            "a: |\n  text\n# f\nb: 2\n",
        ),
        (
            "a:\n  - 1  # e\n  # f\nb: 2\n",
            "",
            true,
            "a: |\n  text\n# f\nb: 2\n",
        ),
    ] {
        let file = YamlFile::from_str(src).unwrap();
        let mapping = mapping_at(&file, path);
        if block {
            mapping.set("a", value_of("v: |\n  text\n"));
        } else {
            mapping.set("a", block_mapping());
        }
        assert_file_edited(&file, expected);
    }
}

#[test]
fn set_path_over_commented_value() {
    for (src, path, expected) in [
        (
            "o:\n  a:  # c\n    x: 1  # e\n  b: 2\n",
            "o.a",
            "o:\n  a: 5  # c\n  b: 2\n",
        ),
        (
            "o:\n  -  # c\n    x: 1\n  - 2\n",
            "o[0]",
            "o:\n  - 5  # c\n  - 2\n",
        ),
        ("o:\n  - 1  # c\n  - 2\n", "o[0]", "o:\n  - 5  # c\n  - 2\n"),
    ] {
        let file = YamlFile::from_str(src).unwrap();
        file.document().unwrap().try_set_path(path, 5).unwrap();
        assert_file_edited(&file, expected);
    }
}

/// An entry that is not in a mapping has no lines around it, so whatever
/// would go above or after it stays inside it.
#[test]
fn detached_entry_set_value() {
    let entry = MappingEntry::new("k", 1, false, false);
    entry.set_value(2, false);
    assert_eq!(entry.to_string(), "k: 2\n");
    debug::validate_tree(entry.syntax()).unwrap();

    for (src, value, expected) in [
        ("k: 1\nz: 2\n", "v: 2\n", "k: 2\n"),
        (
            "k: 1  # note\nz: 2\n",
            "v: |  # src\n  text\n",
            "# note\nk: |  # src\n  text\n",
        ),
        ("k:\n  x: 1\n\nz: 2\n", "v:\n  y: 1\n", "k:\n  y: 1\n"),
    ] {
        let file = YamlFile::from_str(src).unwrap();
        let entry = mapping_at(&file, "").remove("k").unwrap();
        entry.set_value(value_of(value), false);
        assert_eq!(entry.to_string(), expected, "{src:?}");
        debug::validate_tree(entry.syntax()).unwrap();
    }
}

/// The parser reads the blank lines after a block scalar as part of its
/// body, so they move into a new one. A kept (`|+`) scalar would take them
/// into its value, so there they are dropped.
#[test]
fn blank_lines_after_new_block_scalar() {
    for (src, path, value, expected) in [
        (
            "a:\n  x: 1\n\nb: 2\n",
            "",
            "v: |\n  text\n",
            "a: |\n  text\n\nb: 2\n",
        ),
        (
            "a: 1\n\nb: 2\n",
            "",
            "v: |\n  text\n",
            "a: |\n  text\n\nb: 2\n",
        ),
        (
            "a:\n  x: 1\n\n\nb: 2\n",
            "",
            "v: |-\n  text\n",
            "a: |-\n  text\n\n\nb: 2\n",
        ),
        ("a: 1\n\n", "", "v: >\n  text\n", "a: >\n  text\n\n"),
        (
            "o:\n  a:\n    x: 1\n\nb: 2\n",
            "o",
            "v: |\n  text\n",
            "o:\n  a: |\n    text\n\nb: 2\n",
        ),
        (
            "a:\n  x: 1\n\n  # f\n\nb: 2\n",
            "",
            "v: |\n  text\n",
            "a: |\n  text\n\n# f\n\nb: 2\n",
        ),
        (
            "a:\n  x: 1\n \nb: 2\n",
            "",
            "v: |\n  text\n",
            "a: |\n  text\n \nb: 2\n",
        ),
        (
            "a:\n  x: 1\n    \nb: 2\n",
            "",
            "v: |\n  text\n",
            "a: |\n  text\n\nb: 2\n",
        ),
        (
            "a: 1\n   \nb: 2\n",
            "",
            "v: |1\n  text\n",
            "a: |1\n  text\n\nb: 2\n",
        ),
        (
            "a: 1\n # f\nb: 2\n",
            "",
            "v: |1\n  text\n",
            "a: |1\n  text\n# f\nb: 2\n",
        ),
        (
            "a:\n  x: 1\n\t\nb: 2\n",
            "",
            "v: |\n  text\n",
            "a: |\n  text\n\nb: 2\n",
        ),
        (
            "a:\n  x: 1\n\nb: 2\n",
            "",
            "v: |+\n  text\n",
            "a: |+\n  text\nb: 2\n",
        ),
        (
            "a: 1\n\nb: 2\n",
            "",
            "v: |+\n  text\n",
            "a: |+\n  text\nb: 2\n",
        ),
        (
            "a: 1\n\n# f\n\nb: 2\n",
            "",
            "v: |+\n  text\n",
            "a: |+\n  text\n# f\n\nb: 2\n",
        ),
    ] {
        let file = YamlFile::from_str(src).unwrap();
        let source = value_of(value);
        mapping_at(&file, path).set("a", &source);
        assert_file_edited(&file, expected);
        let set = mapping_at(&YamlFile::from_str(expected).unwrap(), path)
            .get("a")
            .unwrap();
        assert_eq!(text(&set), text(&source), "{src:?}");
    }

    for (src, value, expected) in [
        ("- 1\n\n- 2\n", "v: |\n  text\n", "- |\n  text\n\n- 2\n"),
        ("- x: 1\n\n- 2\n", "v: |\n  text\n", "- |\n  text\n\n- 2\n"),
        ("- 1\n\n- 2\n", "v: |+\n  text\n", "- |+\n  text\n- 2\n"),
    ] {
        let file = YamlFile::from_str(src).unwrap();
        let source = value_of(value);
        sequence_at(&file, "").set(0, &source);
        assert_file_edited(&file, expected);
        let set = sequence_at(&YamlFile::from_str(expected).unwrap(), "")
            .get(0)
            .unwrap();
        assert_eq!(text(&set), text(&source), "{src:?}");
    }
}

#[test]
fn set_over_block_mapping_with_flow_key() {
    // A block mapping whose first key is a flow collection keeps the lines
    // after it at its end, as one with a plain key does.
    for (src, expected) in [
        ("a:\n  {x: 1}: 1\n\nb: 2\n", "a: 5\n\nb: 2\n"),
        ("a:\n  [x]: 1\n\n  # f\nb: 2\n", "a: 5\n\n  # f\nb: 2\n"),
    ] {
        let file = YamlFile::from_str(src).unwrap();
        mapping_at(&file, "").set("a", 5);
        assert_file_edited(&file, expected);
    }
    let src = "- {x: 1}: 1\n\n- 2\n";
    let file = YamlFile::from_str(src).unwrap();
    file.document().unwrap().as_sequence().unwrap().set(0, 5);
    assert_file_edited(&file, "- 5\n\n- 2\n");
}

/// The parser keeps the blank lines after an empty value in it, where it
/// looked for the value. They stay after a new value.
#[test]
fn blank_lines_after_empty_value() {
    for (src, path, expected_scalar, expected_block) in [
        ("a:\n\nb: 1\n", "", "a: 5\n\nb: 1\n", "a:\n  y: 2\n\nb: 1\n"),
        (
            "a:  # c\n\n\nb: 1\n",
            "",
            "a: 5  # c\n\n\nb: 1\n",
            "a:  # c\n  y: 2\n\n\nb: 1\n",
        ),
        (
            "x:\n  a:\n\n  b: 1\n",
            "x",
            "x:\n  a: 5\n\n  b: 1\n",
            "x:\n  a:\n    y: 2\n\n  b: 1\n",
        ),
    ] {
        let file = YamlFile::from_str(src).unwrap();
        mapping_at(&file, path).set("a", 5);
        assert_file_edited(&file, expected_scalar);
        let file = YamlFile::from_str(src).unwrap();
        mapping_at(&file, path).set("a", block_mapping());
        assert_file_edited(&file, expected_block);
    }
}

/// A comment ending the last line of a value that spans lines goes with
/// it, wherever the parser keeps it.
#[test]
fn comment_after_multi_line_value_goes_with_it() {
    for (src, expected) in [
        ("a: [x,\n  y]  # c\nb: 1\n", "a: 5\nb: 1\n"),
        ("? a\n: [x,\n  y]  # c\nb: 1\n", "? a\n: 5\nb: 1\n"),
    ] {
        let file = YamlFile::from_str(src).unwrap();
        mapping_at(&file, "").set("a", 5);
        assert_file_edited(&file, expected);
    }
    for (src, expected) in [
        ("- [x,\n  y]  # c\n- 1\n", "- 5\n- 1\n"),
        ("- a: [x,\n    y]  # c\n- 1\n", "- 5\n- 1\n"),
    ] {
        let file = YamlFile::from_str(src).unwrap();
        sequence_at(&file, "").set(0, 5);
        assert_file_edited(&file, expected);
    }
}

/// A comment line at the body column of a block scalar written as a value
/// moves back to the entry's column, also after blank lines, which may
/// hold spaces.
#[test]
fn comment_line_after_blank_lines_leaves_new_block_scalar_body() {
    for (src, expected) in [
        ("a: 1\n\n  # c\nb: 2\n", "a: |\n  x\n\n# c\nb: 2\n"),
        ("a: 1\n  \n  # c\nb: 2\n", "a: |\n  x\n  \n# c\nb: 2\n"),
        ("a: 1\n  # c\nb: 2\n", "a: |\n  x\n# c\nb: 2\n"),
        ("a: 1\n \n  # c\n", "a: |\n  x\n \n# c\n"),
    ] {
        let file = YamlFile::from_str(src).unwrap();
        file.document()
            .unwrap()
            .as_mapping()
            .unwrap()
            .set("a", "x\n");
        assert_file_edited(&file, expected);
    }
}

#[test]
fn preserve_formatting() {
    let yaml = "# Comment at start\nkey:   value    # inline comment\n\n# Another comment\nlist:\n  - item1   \n  - item2\n";
    let parsed = YamlFile::from_str(yaml).unwrap();

    let doc = parsed.document().unwrap();
    let mapping = doc.as_mapping().unwrap();
    assert_eq!(
        mapping.get("key").unwrap().as_scalar().unwrap().as_string(),
        "value"
    );
    let list = mapping.get_sequence("list").unwrap();
    assert_eq!(list.len(), 2);
    // Note: to_string() preserves trailing spaces, so we check content with trim
    let items: Vec<String> = list
        .values()
        .map(|v| v.to_string().trim().to_string())
        .collect();
    assert_eq!(items, vec!["item1", "item2"]);

    // Verify exact lossless round-trip
    let output = parsed.to_string();
    assert_eq!(output, yaml);
}

#[test]
fn enhanced_comment_support() {
    // Test improvements: mid-line comments, comments in flow collections,
    // and better comment positioning preservation

    // Test 1: Comments in flow sequences
    let yaml1 = r#"flow_seq: [
    item1, # comment after item1
    item2, # comment after item2
    item3  # comment after item3
]"#;
    let parsed1 = YamlFile::from_str(yaml1).unwrap();
    let output1 = parsed1.to_string();

    assert_eq!(output1, yaml1);

    // Test 2: Comments in flow mappings
    let yaml2 = r#"flow_map: {
    key1: val1, # comment after first pair
    key2: val2, # comment after second pair
    key3: val3  # comment after third pair
}"#;
    let parsed2 = YamlFile::from_str(yaml2).unwrap();
    let output2 = parsed2.to_string();

    assert_eq!(output2, yaml2);

    // Test 3: Mixed nested structures with comments
    let yaml3 = r#"config:
  servers: [
    {name: web1, port: 80},   # Web server 1
    {name: web2, port: 80},   # Web server 2
    {name: db1, port: 5432}   # Database server
  ] # End servers array"#;
    let parsed3 = YamlFile::from_str(yaml3).unwrap();
    let output3 = parsed3.to_string();

    assert_eq!(output3, yaml3);

    // Test 4: Comments between sequence items (block style)
    let yaml4 = r#"items:
  - first   # First item comment
  - second  # Second item comment
  # Comment between items
  - third   # Third item comment"#;
    let parsed4 = YamlFile::from_str(yaml4).unwrap();
    let output4 = parsed4.to_string();

    assert_eq!(output4, yaml4);

    // Test 5: Round-trip preservation (verify all can be reparsed)
    for yaml in [yaml1, yaml2, yaml3, yaml4] {
        let parsed = YamlFile::from_str(yaml).unwrap();
        let output = parsed.to_string();
        let reparsed = YamlFile::from_str(&output);
        assert!(reparsed.is_ok(), "Round-trip parsing should succeed");
    }
}

/// An entry moved in after another goes right after it, before the
/// comments and blank lines that follow it.
#[test]
fn move_after_keeps_comments_and_blank_lines() {
    for (src, after, key, value, expected) in [
        (
            r#"---
# Header comment
key1: value1  # Inline comment 1
# Middle comment
key2: value2  # Inline comment 2
# Footer comment
"#,
            "key1",
            "new_key",
            "new_value",
            r#"---
# Header comment
key1: value1  # Inline comment 1
new_key: new_value
# Middle comment
key2: value2  # Inline comment 2
# Footer comment
"#,
        ),
        (
            r#"---
key1:    value1


key2:        value2
"#,
            "key1",
            "new_key",
            "new_value",
            r#"---
key1:    value1
new_key: new_value


key2:        value2
"#,
        ),
        (
            "---\n# Configuration file\ndatabase:  # Database settings\n  host: localhost  # Default host\n  port: 5432      # Default port\n  # Connection pool settings  \n  pool:\n    min: 1    # Minimum connections\n    max: 10   # Maximum connections\n\n# Server configuration\nserver:\n  port: 8080  # HTTP port\n",
            "database",
            "logging",
            "debug",
            "---\n# Configuration file\ndatabase:  # Database settings\n  host: localhost  # Default host\n  port: 5432      # Default port\n  # Connection pool settings  \n  pool:\n    min: 1    # Minimum connections\n    max: 10   # Maximum connections\nlogging: debug\n\n# Server configuration\nserver:\n  port: 8080  # HTTP port\n",
        ),
    ] {
        let text = edited(src, |doc| {
            doc.as_mapping().unwrap().move_after(after, key, value);
        });
        assert_eq!(text, expected, "{src:?}");
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

/// An empty value leaves the lines after it, which the parser looked
/// at for a value, to the collection.
#[test]
fn empty_value_leaves_following_lines() {
    assert_eq!(entry_texts("a:\n\nb: 1\n"), ["a:\n", "b: 1\n"]);
    assert_eq!(entry_texts("a: &x\n\n  # c\nb: 1\n"), ["a: &x\n", "b: 1\n"]);
    assert_eq!(
        entry_texts("x:\n  a:  # c\n  b: 1\n"),
        ["x:\n  a:  # c\n  b: 1\n", "a:  # c\n", "b: 1\n"]
    );
    assert_eq!(entry_texts("? a\n:\n\n# c\nb: 1\n"), ["? a\n:\n", "b: 1\n"]);
    assert_eq!(entry_texts("? a  # c\n\nb: 1\n"), ["? a  # c\n", "b: 1\n"]);
    assert_eq!(entry_texts("- ? a\n  b: 1\n"), ["? a\n", "b: 1\n"]);
}

/// The lines after a nested collection's last entry belong to the
/// collection holding the next entry, or the root one at the end.
#[test]
fn nested_collection_leaves_following_lines() {
    assert_eq!(
        entry_texts("a:\n  - x\n\n  # c\nb: 1\n"),
        ["a:\n  - x\n", "b: 1\n"]
    );
    assert_eq!(entry_texts("? - a\n\n: b\n"), ["? - a\n\n: b\n"]);
    assert_eq!(entry_texts("a:\n  x: 1\n\n"), ["a:\n  x: 1\n", "x: 1\n"]);
    assert_eq!(
        entry_texts("a:\n  b:\n    - x\n\n  c: 1\n"),
        ["a:\n  b:\n    - x\n\n  c: 1\n", "b:\n    - x\n", "c: 1\n"]
    );
}

/// A mapping entry owns the comment ending its last line, and the line
/// break after it, whether its key is explicit or its value on a later
/// line.
#[test]
fn mapping_entry_owns_line_end() {
    assert_eq!(
        entry_texts("? a\n: b  # c\nd: 1\n"),
        ["? a\n: b  # c\n", "d: 1\n"]
    );
    assert_eq!(
        entry_texts("a:\n  b  # c\nd: 1\n"),
        ["a:\n  b  # c\n", "d: 1\n"]
    );
    assert_eq!(entry_texts("? - a\n# c\nd: 1\n"), ["? - a\n", "d: 1\n"]);
}

/// A sequence entry owns the comment ending its line, and the line
/// break after it, as a mapping entry does. A comment line after a
/// block item is the sequence's.
#[test]
fn sequence_entry_owns_line_comment() {
    let file = YamlFile::from_str("- a  # c\n- |\n  x\n# d\n- b # e").unwrap();
    let entries: Vec<String> = file
        .syntax()
        .descendants()
        .filter(|n| n.kind() == yaml_edit::SyntaxKind::SEQUENCE_ENTRY)
        .map(|n| n.to_string())
        .collect();
    assert_eq!(entries, ["- a  # c\n", "- |\n  x\n", "- b # e"]);
}

/// Comment lines after a nested block collection's last entry with no
/// content after them belong to the root collection, as do those
/// before a line that does not continue a nested sequence.
#[test]
fn lines_ending_nested_collection_go_to_root() {
    for src in [
        "a:\n  b: 1\n  # c\n\n",
        "a:\n  b: 1\n  # c",
        "a:\n  b:\n    c: 1\n    # c\n  # d\n",
        "a:\n- x\n# c\n",
        "a:\n- x\n# c\nb: 1\n",
        "- - a\n  # c\n",
        "a:\n  b: 1\n  # c\n---\nx\n",
    ] {
        let parsed = YamlFile::from_str(src).unwrap();
        let syntax = parsed.syntax();
        assert_eq!(debug::validate_tree(syntax), Ok(()), "{src:?}");
        let root = syntax.first_child().unwrap().first_child().unwrap();
        for comment in syntax
            .descendants_with_tokens()
            .filter(|c| c.kind() == yaml_edit::SyntaxKind::COMMENT)
        {
            assert_eq!(comment.parent().as_ref(), Some(&root), "{src:?}");
        }
    }

    let parsed = YamlFile::from_str("a:\n  b: 1\n  # c\n\n").unwrap();
    assert_eq!(
        debug::tree_to_string(parsed.syntax()),
        "\
DOCUMENT
  MAPPING
    MAPPING_ENTRY
      KEY
        SCALAR
          STRING: \"a\"
      COLON: \":\"
      VALUE
        NEWLINE: \"\\\\n\"
        INDENT: \"  \"
        MAPPING
          MAPPING_ENTRY
            KEY
              SCALAR
                STRING: \"b\"
            COLON: \":\"
            WHITESPACE: \" \"
            VALUE
              SCALAR
                INT: \"1\"
            NEWLINE: \"\\\\n\"
    INDENT: \"  \"
    COMMENT: \"# c\"
    NEWLINE: \"\\\\n\"
    NEWLINE: \"\\\\n\"
"
    );
}

/// Comment lines that a nested collection steps over looking for its
/// next entry are not its own when no entry follows: they go to the
/// node that comes next, here the root collection or the flow sequence.
#[test]
fn comment_lines_before_non_entry_leave_nested_collection() {
    let comment_parents = |src: &str| {
        let parsed = YamlFile::parse(src).tree();
        let syntax = parsed.syntax();
        assert_eq!(debug::validate_tree(syntax), Ok(()), "{src:?}");
        assert_eq!(parsed.to_string(), src);
        syntax
            .descendants_with_tokens()
            .filter(|c| c.kind() == yaml_edit::SyntaxKind::COMMENT)
            .map(|c| {
                let parent = c.parent().unwrap();
                (parent.kind(), parent.text().to_string())
            })
            .collect::<Vec<_>>()
    };
    assert_eq!(
        comment_parents("a:\n  b: 1\n  # c\n  0\n"),
        [(SyntaxKind::MAPPING, "a:\n  b: 1\n  # c\n  ".to_string())]
    );
    assert_eq!(
        comment_parents("[a:\n?\nb:\n# c\n"),
        [(SyntaxKind::SEQUENCE, "[a:\n?\nb:\n# c\n".to_string())]
    );
}
