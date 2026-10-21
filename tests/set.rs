//! Setting values: `Mapping::set`, `Scalar::set_value`, `set_path` and
//! `set_with_field_order`.
//!
//! A value set in place keeps the layout around it: comments, blank lines
//! and the end of the input with or without a final line break. A block
//! value replacing an inline one, or the other way round, moves the
//! comment on the key line along.

mod common;
use common::{
    assert_cst_ok, assert_edited, assert_file_cst_ok, assert_file_edited, assert_file_edited_valid,
    assert_parser_shaped, check_edit, edit_root, render_documents, value_of,
};
use rowan::ast::AstNode;
use std::str::FromStr;
use yaml_edit::path::{PathError, YamlPath};
use yaml_edit::validator::Validator;
use yaml_edit::{
    debug, AsYaml, Document, Entry, Mapping, MappingBuilder, ScalarValue, Sequence,
    SequenceBuilder, YamlFile, YamlNode, YamlValue,
};

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

/// Set `key` in the mapping at `path` of `src` to `value` and check the
/// result.
#[track_caller]
fn check_set(src: &str, path: &[&str], key: &str, value: impl AsYaml, expected: &str) {
    let (file, root) = parse(src);
    dig(&root, path).set(key, value);
    assert_file_edited(&file, expected);
}

type MappingEdit<'a> = &'a dyn Fn(&Mapping);

/// Apply `edits` in turn to the mapping at `path` of `src`, checking the
/// tree after each, and check the result.
#[track_caller]
fn check_edits(src: &str, path: &[&str], edits: &[MappingEdit], expected: &str) {
    let (file, root) = parse(src);
    let mapping = dig(&root, path);
    for edit in edits {
        edit(&mapping);
        assert_file_cst_ok(&file);
    }
    assert_file_edited(&file, expected);
}

#[test]
fn set_scalar_values() {
    check_edits(
        "key: value",
        &[],
        &[&|m| m.set("key", "new_value")],
        "key: new_value",
    );
    check_edits(
        "key: value  # comment",
        &[],
        &[&|m| m.set("key", "new_value")],
        "key: new_value  # comment",
    );
    check_edits(
        "debug: true  # For now",
        &[],
        &[&|m| m.set("debug", false)],
        "debug: false  # For now",
    );
    check_edits(
        "host: localhost\nport: 8080\ndebug: true",
        &[],
        &[
            &|m| m.set("host", "0.0.0.0"),
            &|m| m.set("port", 3000),
            &|m| m.set("debug", false),
        ],
        "host: 0.0.0.0\nport: 3000\ndebug: false",
    );
    check_edits(
        "existing: value",
        &[],
        &[&|m| m.set("new_key", "new_value")],
        "existing: value\nnew_key: new_value",
    );
}

#[test]
fn set_in_nested_mapping() {
    check_edits(
        "database:\n  name: dev_db\n  user: admin",
        &[],
        &[&|m| {
            m.modify_mapping("database", |db| {
                db.set("name", "prod_db");
            });
        }],
        "database:\n  name: prod_db\n  user: admin",
    );
    check_edits(
        "database:\n  name: dev_db\n  user: admin",
        &[],
        &[&|m| {
            m.modify_mapping("database", |db| {
                db.set("password", "secret123");
            });
        }],
        "database:\n  name: dev_db\n  user: admin\n  password: secret123",
    );
    check_edits(
        "# Config\nserver:\n  host: localhost  # Default host\n  port: 8080",
        &[],
        &[&|m| {
            m.modify_mapping("server", |server| {
                server.set("host", "0.0.0.0");
                server.set("timeout", 30);
            });
        }],
        "# Config\nserver:\n  host: 0.0.0.0  # Default host\n  port: 8080\n  timeout: 30",
    );
    check_edits(
        "database:\n  name: dev_db\n  user: admin\n  max_connections: 10",
        &["database"],
        &[
            &|m| m.set("name", "prod_db"),
            &|m| m.set("password", "secret123"),
            &|m| m.set("max_connections", 50),
        ],
        "database:\n  name: prod_db\n  user: admin\n  max_connections: 50\n  password: secret123",
    );
    check_edits(
        "server:\n  database:\n    primary:\n      host: localhost\n      port: 5432",
        &["server", "database", "primary"],
        &[
            &|m| m.set("host", "prod.example.com"),
            &|m| m.set("ssl", true),
        ],
        "server:\n  database:\n    primary:\n      host: prod.example.com\n      port: 5432\n      ssl: true",
    );
}

#[test]
fn scalar_set_value_replaces_multiline_scalar() {
    let set_a = |d: &Document| {
        let m = d.as_mapping().unwrap();
        m.get("a").unwrap().as_scalar().unwrap().set_value("x");
    };
    check_edit("a: foo\n  bar\nb: 2\n", set_a, "a: x\nb: 2\n");
    check_edit("a: |\n  hello\nb: 2\n", set_a, "a: x\nb: 2\n");
}

/// Changing an anchored value leaves the aliases to it pointing at it.
#[test]
fn set_anchored_value_keeps_aliases() {
    let (file, mapping) = parse("anchor: &x original\nref: *x");
    assert_eq!(mapping.get("ref").unwrap().as_alias().unwrap().name(), "x");
    assert_eq!(
        mapping
            .get("anchor")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "original"
    );
    mapping.set("anchor", "modified");
    assert_file_edited(&file, "anchor: &x modified\nref: *x");
    assert_eq!(
        mapping
            .get("anchor")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "modified"
    );
    let alias = mapping.get("ref").unwrap();
    assert_eq!(alias.as_alias().unwrap().name(), "x");

    let (file, mapping) =
        parse("config: &defaults\n  timeout: 30\n  retries: 3\nserver: *defaults");
    let config = mapping.get_mapping("config").unwrap();
    assert_eq!(
        config.get("timeout").unwrap().as_scalar().unwrap().as_i64(),
        Some(30)
    );
    config.set("timeout", 60);
    assert_file_cst_ok(&file);
    config.set("max_connections", 100);
    assert_file_edited(
        &file,
        "config: &defaults\n  timeout: 60\n  retries: 3\n  max_connections: 100\nserver: *defaults",
    );
    assert_eq!(
        config.get("timeout").unwrap().as_scalar().unwrap().as_i64(),
        Some(60)
    );
    assert!(config.contains_key("max_connections"));
    let alias = mapping.get("server").unwrap();
    assert_eq!(alias.as_alias().unwrap().name(), "defaults");
}

#[test]
fn set_over_block_scalar() {
    let (file, mapping) = parse("text: |\n  Line 1\n  Line 2\n");
    assert_eq!(
        mapping
            .get("text")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "Line 1\nLine 2\n"
    );
    mapping.set("text", "New single line");
    assert_file_edited(&file, "text: New single line\n");
    assert_eq!(
        mapping
            .get("text")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "New single line"
    );

    // The value ends without a line break, so it is written with `|-`
    // rather than a `|` that would add one.
    let new_content = "New line 1\nNew line 2\nNew line 3";
    let (file, mapping) = parse("description: |\n  Original text\n  on multiple lines\n");
    mapping.set("description", new_content);
    assert_file_edited(
        &file,
        "description: |-\n  New line 1\n  New line 2\n  New line 3\n",
    );
    let value = mapping.get("description").unwrap();
    assert_eq!(value.as_scalar().unwrap().as_string(), new_content);
    let reparsed = parse(&file.to_string()).1;
    let value = reparsed.get("description").unwrap();
    assert_eq!(value.as_scalar().unwrap().as_string(), new_content);
}

/// Setting a plain value over a tagged one drops the tag.
#[test]
fn set_over_tagged_value() {
    let (file, mapping) = parse("date: !!timestamp 2024-01-01\ncount: !!int 42");
    assert!(mapping.contains_key("date"));
    assert!(mapping.contains_key("count"));
    mapping.set("date", "2024-12-31");
    assert_file_cst_ok(&file);
    mapping.set("count", 100);
    assert_file_edited(&file, "date: 2024-12-31\ncount: 100");
    let date = mapping.get("date").unwrap();
    assert_eq!(date.as_scalar().unwrap().as_string(), "2024-12-31");
    let count = mapping.get("count").unwrap();
    assert_eq!(count.as_scalar().unwrap().as_i64(), Some(100));
}

#[test]
fn set_in_empty_flow_mapping() {
    let (file, mapping) = parse("empty: {}");
    let empty = mapping.get_mapping("empty").unwrap();
    assert_eq!(empty.keys().count(), 0);
    empty.set("a", 1);
    assert_file_cst_ok(&file);
    empty.set("b", 2);
    assert_file_edited(&file, "empty: {a: 1, b: 2}");
    assert_eq!(empty.keys().count(), 2);
    assert_eq!(
        empty.get("a").unwrap().as_scalar().unwrap().as_i64(),
        Some(1)
    );
    assert_eq!(
        empty.get("b").unwrap().as_scalar().unwrap().as_i64(),
        Some(2)
    );
}

/// Replacing a multi-line flow value with a scalar inside a flow mapping
/// must not leave stray whitespace behind.
///
/// Regression: the lintian-brush upstream-metadata `repository-as-list`
/// fixer produced a spurious blank line and lost indentation on the
/// following entry when the `Repository` list was replaced with a string.
/// The other cases are adjacent shapes: a flow mapping value, an ANCHOR
/// that sits beside the flow sequence in VALUE with no TAGGED_NODE wrapper,
/// a TAG wrapping the value in a TAGGED_NODE, and nested flow mappings.
#[test]
fn replace_multiline_flow_value_with_scalar() {
    check_set(
        "{\n  \"Name\": \"yep\",\n  \"Repository\": [\n    \":extssh:_anoncvs@anoncvs.example.org:/cvs\",\n    \"yep\"\n  ],\n  \"Repository-Browse\": \"http://www.example.org/cvs.cgi/contrib/code/yep/\"\n}",
        &[],
        "Repository",
        "cvs+ssh://_anoncvs@anoncvs.example.org/cvs#yep",
        "{\n  \"Name\": \"yep\",\n  \"Repository\": \"cvs+ssh://_anoncvs@anoncvs.example.org/cvs#yep\",\n  \"Repository-Browse\": \"http://www.example.org/cvs.cgi/contrib/code/yep/\"\n}",
    );
    for (input, key, expected) in [
        (
            "{\n  \"a\": \"1\",\n  \"nested\": {\n    \"x\": 1,\n    \"y\": 2\n  },\n  \"z\": \"3\"\n}",
            "nested",
            "{\n  \"a\": \"1\",\n  \"nested\": \"replaced\",\n  \"z\": \"3\"\n}",
        ),
        (
            "a: 1\nb: &x [\n  1,\n  2\n]\nc: 3\n",
            "b",
            "a: 1\nb: &x replaced\nc: 3\n",
        ),
        (
            "a: 1\nb: !!seq [\n  1,\n  2\n]\nc: 3\n",
            "b",
            "a: 1\nb: replaced\nc: 3\n",
        ),
        (
            "a: 1\nb: [\n  {\n    x: 1\n  },\n  {\n    y: 2\n  }\n]\nc: 3\n",
            "b",
            "a: 1\nb: replaced\nc: 3\n",
        ),
    ] {
        check_set(input, &[], key, "replaced", expected);
    }
}

/// New content in a document closed by `...` goes before the marker.
#[test]
fn set_in_document_ended_by_marker() {
    for (input, expected) in [
        ("--- \n...\n", "--- \nkey: value\n...\n"),
        ("---\n...\n", "---\nkey: value\n...\n"),
        ("a: 1\n...\n", "a: 1\nkey: value\n...\n"),
        ("--- \n...\n--- b\n", "--- \nkey: value\n...\n--- b\n"),
    ] {
        let file = YamlFile::from_str(input).unwrap();
        assert!(file.ensure_document().set("key", "value"));
        assert_eq!(file.to_string(), expected, "{input:?}");

        let file = YamlFile::from_str(input).unwrap();
        assert!(file
            .document()
            .unwrap()
            .set_with_field_order("key", "value", ["key"]));
        assert_eq!(file.to_string(), expected, "{input:?}");
    }
}

/// Setting a value that was omitted at the end of a line in a multi-line
/// flow collection keeps the line break before the closing bracket.
#[test]
fn set_omitted_flow_value_before_line_break() {
    for (input, expected) in [
        ("{\n  k: v,\n  m:\n}\n", "{\n  k: v,\n  m: 5\n}\n"),
        ("{\n  k: v,\n  m:  \n}\n", "{\n  k: v,\n  m:  5\n}\n"),
        ("{\n  m:\n  ,\n  k: v\n}\n", "{\n  m: 5\n  ,\n  k: v\n}\n"),
        ("{\n  k: v,\n  m\n}\n", "{\n  k: v,\n  m: 5\n}\n"),
        ("{m}\n", "{m: 5}\n"),
        ("{ m }\n", "{ m: 5 }\n"),
        ("{m:}\n", "{m: 5}\n"),
        ("{m: }\n", "{m: 5}\n"),
        ("{m: , k: v}\n", "{m: 5, k: v}\n"),
    ] {
        let yaml = YamlFile::from_str(input).unwrap();
        yaml.document().unwrap().as_mapping().unwrap().set("m", 5);
        assert_eq!(yaml.to_string(), expected, "input: {input:?}");
        common::assert_file_parser_shaped(&yaml);
    }
    // The same for the single-pair mapping a flow sequence entry `a:`
    // stands for.
    for (input, expected) in [("[\n  a:\n]\n", "[\n  a: 5\n]\n"), ("[a:]\n", "[a: 5]\n")] {
        let yaml = YamlFile::from_str(input).unwrap();
        let seq = yaml.document().unwrap().as_sequence().unwrap();
        seq.get(0).unwrap().as_mapping().unwrap().set("a", 5);
        assert_eq!(yaml.to_string(), expected, "input: {input:?}");
        common::assert_file_parser_shaped(&yaml);
    }
}

/// A value set where the input ends right after the colon is separated from
/// it, rather than turning `a:` into the plain scalar `a:5`.
#[test]
fn set_omitted_value_at_end_of_input() {
    let yaml = YamlFile::from_str("x: 1\na:").unwrap();
    yaml.document().unwrap().as_mapping().unwrap().set("a", 5);
    assert_eq!(yaml.to_string(), "x: 1\na: 5");
    assert_file_cst_ok(&yaml);
}

/// A comment after a flow value stays where it is when the value is
/// replaced, rather than being copied next to the new value.
#[test]
fn set_flow_value_keeps_trailing_comment_once() {
    for (input, expected) in [
        ("{\n  m: 1 # c\n}\n", "{\n  m: 5 # c\n}\n"),
        ("{m: 1 # c\n}\n", "{m: 5 # c\n}\n"),
        ("{\n  m: # c\n}\n", "{\n  m: 5 # c\n}\n"),
        ("{\n  m: # c\n    1\n}\n", "{\n  m: # c\n    5\n}\n"),
    ] {
        check_set(input, &[], "m", 5, expected);
    }
}

/// Set the first value of `source` (by key, or index 0 in a sequence) to
/// `xval`, check the result is shaped as the parser would shape it and
/// validates, and return its text.
#[track_caller]
fn set_first(source: &str) -> String {
    let file = YamlFile::from_str(source).unwrap();
    let doc = file.document().unwrap();
    match doc.as_mapping() {
        Some(m) => m.set(m.keys().next().unwrap(), "xval"),
        None => assert!(doc.as_sequence().unwrap().set(0, "xval")),
    }
    assert_file_cst_ok(&file);
    let text = file.to_string();
    let reparsed = YamlFile::from_str(&text).unwrap();
    assert_eq!(
        debug::tree_to_string(file.syntax()),
        debug::tree_to_string(reparsed.syntax())
    );
    assert_eq!(
        Validator::new().validate_syntax(reparsed.syntax()),
        vec![],
        "{text:?}"
    );
    text
}

#[test]
fn set_first_value() {
    for (source, expected) in [
        // A tagged value on the line after its key.
        ("a:\n  !t b\n", "a: xval\n"),
        ("\"a\":\n  !!str b\nc: d\n", "\"a\": xval\nc: d\n"),
        ("a:\n  &x !t b\nc: d\n", "a: &x xval\nc: d\n"),
        ("a:\n  !t\nb: c\n", "a: xval\nb: c\n"),
        // An item that ends in an omitted value.
        ("-\n  ? a\n- c\n", "-\n  xval\n- c\n"),
        ("- ? a\n- c\n", "- xval\n- c\n"),
        ("- a:\n- c\n", "- xval\n- c\n"),
        ("-\n  - a\n  -\n- c\n", "-\n  xval\n- c\n"),
    ] {
        assert_eq!(set_first(source), expected, "{source:?}");
    }
}

/// A block collection set as an item starts on the dash's line, indented
/// by the white space after the dash, where a tab is not allowed.
#[test]
fn set_block_collection_after_dash_and_tab() {
    let file = YamlFile::from_str("-\tb\n").unwrap();
    let seq = file.document().unwrap().as_sequence().unwrap();
    seq.set(0, YamlValue::from(vec![1i64, 2]));
    assert_eq!(file.to_string(), "- - 1\n  - 2\n");
    assert_file_cst_ok(&file);
    assert_eq!(Validator::new().validate_syntax(file.syntax()), vec![]);

    let file = YamlFile::from_str("-\tb\n").unwrap();
    let seq = file.document().unwrap().as_sequence().unwrap();
    let map = YamlFile::from_str("x: 1\ny: 2\n").unwrap();
    seq.set(0, map.document().unwrap().as_mapping().unwrap());
    assert_eq!(file.to_string(), "- x: 1\n  y: 2\n");
    assert_file_cst_ok(&file);
}

#[test]
fn set_value_of_flow_key_ending_in_property() {
    // A key of only a tag or anchor needs a space before the `:` that the
    // value brings, or the `:` reads as part of the property's name.
    let set_first = |d: &Document| {
        let m = d.as_mapping().unwrap();
        let key = m.keys().next().unwrap();
        m.set(key, ScalarValue::null());
    };
    check_edit("{!}", set_first, "{! : null}");
    check_edit("{&a}", set_first, "{&a : null}");
}

#[test]
fn set_value_keeps_unterminated_end() {
    let root = |d: &Document| d.as_mapping().unwrap();
    let value = |text: &str| Document::from_str(text).unwrap();
    for (source, new, expected) in [
        ("a:\n  - x", "z", "a: z"),
        ("a:\n  b: 1", "[1]", "a: [1]"),
        ("a: 1", "- p\n- q", "a:\n  - p\n  - q"),
        ("a:", "p: 1", "a:\n  p: 1"),
        ("a: 1", "!t\n  p: 1", "a: !t\n  p: 1"),
        ("a: # c", "- p", "a: # c\n  - p"),
        ("x: 1\na:\n  - x", "- p", "x: 1\na:\n  - p"),
        ("? a\n:\n  - x", "z", "? a\n: z"),
        ("? a\n: 1", "- p", "? a\n:\n  - p"),
        // The line break of a block scalar is part of its value.
        ("a: 1", "|\n  x\n", "a: |\n  x\n"),
    ] {
        check_edit(source, |d| root(d).set("a", value(new)), expected);
    }
    check_edit(
        "o:\n  a:\n    - x",
        |d| root(d).get_mapping("o").unwrap().set("a", value("z")),
        "o:\n  a: z",
    );
    check_edit(
        "o:\n  a: 1",
        |d| root(d).get_mapping("o").unwrap().set("a", value("- p")),
        "o:\n  a:\n    - p",
    );
    check_edit(
        "- a\n- b",
        |d| assert!(d.as_sequence().unwrap().set(1, value("- c"))),
        "- a\n- - c",
    );
}

/// Regression tests for issue #16: `Mapping::set` produced malformed YAML
/// when setting block sequences in nested mappings.
#[test]
fn set_built_block_collection() {
    // The scalar `"#new"` starts with `#`, so the SequenceBuilder wraps it
    // in single quotes to keep it a scalar rather than a comment start.
    check_set(
        "gui:\n  theme:\n    activeBorderColor:\n      - \"#old\"\n      - bold\n    other: keep\n",
        &["gui", "theme"],
        "activeBorderColor",
        seq(&["\"#new\"", "bold"]),
        "gui:\n  theme:\n    activeBorderColor:\n      - '\"#new\"'\n      - bold\n    other: keep\n",
    );
    check_set(
        "activeBorderColor:\n  - \"#old\"\n  - bold\nother: keep\n",
        &[],
        "activeBorderColor",
        seq(&["\"#new\"", "bold"]),
        "activeBorderColor:\n  - '\"#new\"'\n  - bold\nother: keep\n",
    );
    check_set(
        "outer:\n  inner:\n    replaced:\n      old_key: old\n    keep: yes\n",
        &["outer", "inner"],
        "replaced",
        map(&[("new_key", "new"), ("another", "value")]),
        "outer:\n  inner:\n    replaced:\n      new_key: new\n      another: value\n    keep: yes\n",
    );
    // The WHITESPACE between COLON and the old inline VALUE goes, and the
    // entry's NEWLINE is not duplicated.
    check_set(
        "a: 1\nb: 2\nc: 3\n",
        &[],
        "b",
        seq(&["x", "y"]),
        "a: 1\nb:\n  - x\n  - y\nc: 3\n",
    );
    check_set(
        "outer:\n  inner:\n    old: v\n",
        &["outer", "inner"],
        "newkey",
        seq(&["a", "b"]),
        "outer:\n  inner:\n    old: v\n    newkey:\n      - a\n      - b\n",
    );
    // The comment on the old inline value's line moves up to the key line.
    check_set(
        "outer:\n  key: old  # important comment\n",
        &["outer"],
        "key",
        seq(&["a", "b"]),
        "outer:\n  key:  # important comment\n    - a\n    - b\n",
    );
    check_set(
        "? mykey\n: old_scalar\n",
        &[],
        "mykey",
        seq(&["a"]),
        "? mykey\n:\n  - a\n",
    );
    check_set(
        "a: 1\n\nb: 2\n",
        &[],
        "a",
        seq(&["x"]),
        "a:\n  - x\n\nb: 2\n",
    );
}

/// Shapes the scalar-only builders can't make, copied from parsed sources:
/// nested blocks keep their relative indent, and block scalar indicators,
/// anchors and tags stay on the key line.
#[test]
fn set_parsed_value() {
    check_set(
        "outer:\n  inner:\n    tgt: old\n",
        &["outer", "inner"],
        "tgt",
        value_of(
            "root:\n  a:\n    - x:\n        nested: 1\n      other: v\n",
            "root",
        ),
        "outer:\n  inner:\n    tgt:\n      a:\n        - x:\n            nested: 1\n          other: v\n",
    );
    for (source, expected) in [
        (
            "k: |\n  line1\n  line2\n",
            "outer:\n  key: |\n    line1\n    line2\n",
        ),
        (
            "k: &myanchor\n  - a\n  - b\n",
            "outer:\n  key: &myanchor\n    - a\n    - b\n",
        ),
        (
            "k: !!omap\n  - alpha: 1\n  - beta: 2\n",
            "outer:\n  key: !!omap\n    - alpha: 1\n    - beta: 2\n",
        ),
        ("k: &a hello\n", "outer:\n  key: &a hello\n"),
    ] {
        check_set(
            "outer:\n  key: old\n",
            &["outer"],
            "key",
            value_of(source, "k"),
            expected,
        );
    }
    check_set(
        "k: 1\n",
        &[],
        "n",
        &parse("a: 1\nb: 2\n").1,
        "k: 1\nn:\n  a: 1\n  b: 2\n",
    );
}

#[test]
fn set_scalar_over_block_value() {
    check_set("k:\n  - a\n  - b\n", &[], "k", "modified", "k: modified\n");
    check_set("k: &a\n  - old\n", &[], "k", "new", "k: &a new\n");
    check_set("k: !!seq\n  - old\n", &[], "k", "new", "k: new\n");
    check_set(
        "k:  # sticky\n  - old\n",
        &[],
        "k",
        "new",
        "k: new  # sticky\n",
    );
}

/// A block scalar's body owns the line break that ends the entry, so an
/// inline value replacing it needs one of its own.
#[test]
fn set_scalar_over_block_scalar_keeps_line_break() {
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
        check_set(src, path, "k", 5, expected);
    }
}

/// Replacing a commented block value with a scalar moves it onto the key
/// line, leaving what precedes the colon alone and giving the tree the
/// parser's shape.
#[test]
fn set_scalar_over_block_on_key_line() {
    for (src, path, expected) in [
        ("k :  # c\n  x: 1\n", &[][..], "k : 5  # c\n"),
        ("k:   \n  x: 1\n", &[], "k: 5\n"),
        (
            "o:\n  k:  # c\n    x: 1\n  b: 2\n",
            &["o"],
            "o:\n  k: 5  # c\n  b: 2\n",
        ),
    ] {
        check_set(src, path, "k", 5, expected);
    }
}

#[test]
fn switch_between_scalar_and_block_keeps_comment() {
    let (file, root) = parse("key: initial  # sticky\n");
    root.set("key", seq(&["item0"]));
    assert_file_edited(&file, "key:  # sticky\n  - item0\n");
    root.set("key", "back");
    assert_file_edited(&file, "key: back  # sticky\n");
    // MappingBuilder passes string values through, so numeric-looking
    // string values get quoted to preserve their type.
    root.set("key", map(&[("a", "1"), ("b", "2")]));
    assert_file_edited(&file, "key:  # sticky\n  a: '1'\n  b: '2'\n");
}

/// Block YamlValues set as `k` (existing) and `n` (new) in a root mapping
/// and in a nested one, with the expected output for each.
#[test]
fn set_block_yaml_values() {
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
            check_set(src, path, key, value.clone(), expected);
        }
    }
}

/// Regression for issue #18: set_path drilling through several missing
/// intermediate mappings must indent each level correctly and must not
/// leave stray blank lines behind.
#[test]
fn set_path_creates_intermediate_mappings_with_indent() {
    let doc = Document::from_str("managers:\n  pm:\n    runtime: claude-code\n").unwrap();
    doc.try_set_path("managers.pm.interfaces.telegram.bot_token_env", "TOKEN")
        .expect("set_path");
    assert_cst_ok(&doc);
    assert_eq!(
        doc.to_string(),
        "managers:\n  pm:\n    runtime: claude-code\n    interfaces:\n      telegram:\n        bot_token_env: TOKEN\n"
    );
}

/// Regression from mutation_invariants fuzz: `set_path("997", "")` on a
/// document whose root is a mapping did not insert the new key.
#[test]
fn set_path_new_numeric_key() {
    let doc = Document::from_str("s:\n  - a\n  - b\n  - c\n").unwrap();
    doc.try_set_path("997", "").expect("set_path");
    assert_cst_ok(&doc);
    assert_eq!(doc.to_string(), "s:\n  - a\n  - b\n  - c\n'997': ''\n");
    let got = doc
        .try_get_path("997")
        .ok()
        .as_ref()
        .and_then(|n| n.as_scalar().map(|s| s.as_string()));
    assert_eq!(got.as_deref(), Some(""));
}

/// Regression from mutation_invariants fuzz: an empty path is invalid, so
/// the loose (deprecated) shims leave the document unchanged and
/// `get_path("")` returns `None`, while the `try_` variants report
/// `PathError::EmptyPath`.
#[test]
fn set_empty_path_is_a_noop() {
    let seed = "folded: >\n  wrapped\n  paragraph\n";
    let doc = Document::from_str(seed).unwrap();

    #[allow(deprecated)]
    {
        doc.set_path("", "anything");
        assert_cst_ok(&doc);
        assert_eq!(doc.to_string(), seed);
        assert!(doc.get_path("").is_none());
        assert!(!doc.remove_path(""));
        assert_cst_ok(&doc);
    }

    assert_eq!(doc.try_get_path(""), Err(PathError::EmptyPath));
    assert_eq!(doc.try_set_path("", "anything"), Err(PathError::EmptyPath));
    assert_cst_ok(&doc);
    assert!(matches!(doc.try_remove_path(""), Err(PathError::EmptyPath)));
    assert_cst_ok(&doc);
}

/// The try_ variants surface the specific failure modes the silent
/// versions used to swallow.
#[test]
fn try_path_reports_specific_errors() {
    // A document whose root is a scalar has no mapping to set paths under.
    let doc = Document::from_str("just_a_scalar\n").unwrap();
    assert!(matches!(
        doc.try_set_path("foo", "bar"),
        Err(PathError::NoRoot)
    ));
    assert_cst_ok(&doc);

    let doc = Document::from_str("a: 1\n").unwrap();
    assert!(matches!(
        doc.try_set_path("a[0", "x"),
        Err(PathError::Parse(_))
    ));
    assert_cst_ok(&doc);

    let doc = Document::from_str("a:\n  b: 1\n").unwrap();
    assert!(matches!(
        doc.try_get_path("a.missing"),
        Err(PathError::NotFound { .. })
    ));

    let doc = Document::from_str("a: hello\n").unwrap();
    assert!(matches!(
        doc.try_get_path("a.b"),
        Err(PathError::TypeMismatch { .. })
    ));

    let doc = Document::from_str("a: hello\n").unwrap();
    assert!(matches!(
        doc.try_set_path("a.b", "x"),
        Err(PathError::TypeMismatch { .. })
    ));
    assert_cst_ok(&doc);

    // An index past the growth bound is rejected. The intermediate
    // sequence used to be created first, leaving `nn:\n  \n` behind: a
    // half-built entry that no longer reparses as a sequence.
    let doc = Document::from_str("a: 1\n").unwrap();
    assert_eq!(
        doc.try_set_path("nn.9487", "xx"),
        Err(PathError::IndexTooFar {
            at: "[9487]".to_string(),
            len: 0,
            index: 9487
        })
    );
    assert_cst_ok(&doc);
    assert_eq!(doc.to_string(), "a: 1\n");
}

/// Draining a nested mapping collapses it to `key: {}`. The space
/// separating the colon from the value belongs to the entry, as it does
/// when parsed; left inside the VALUE, a later edit replacing that value
/// carried the space off and wrote `t:a`, which is a plain scalar.
#[test]
fn collapsed_empty_mapping_keeps_its_separator_in_the_entry() {
    let doc = Document::from_str("a: 1\n").unwrap();
    doc.try_set_path("t.t", "9").unwrap();
    assert_cst_ok(&doc);
    doc.try_remove_path("t.t").unwrap();
    assert_cst_ok(&doc);
    assert_eq!(doc.to_string(), "a: 1\nt: {}\n");
    doc.try_set_path("t", "b").unwrap();
    assert_eq!(doc.to_string(), "a: 1\nt: b\n");
    assert_parser_shaped(&doc);
}

/// Setting a multi-line value on a nested entry wrote the block body at a
/// fixed two spaces, which put it at the key's own column and made the
/// document unparseable.
#[test]
fn multiline_value_indents_its_block_body_past_the_entry() {
    check_edit(
        "a:\n  b: 1\n",
        |d| {
            let inner = d.as_mapping().unwrap().get(ScalarValue::from("a")).unwrap();
            inner.as_mapping().unwrap().set("b", "x\ny");
        },
        "a:\n  b: |-\n    x\n    y\n",
    );
}

const PACKAGE_ORDER: &[&str] = &["name", "version", "description"];
const PACKAGE_ORDER_WITH_AUTHOR: &[&str] = &["name", "version", "description", "author"];

#[test]
fn set_with_field_order_position() {
    let order = PACKAGE_ORDER;
    check_edits(
        "name: my-app\nversion: 1.0\ndescription: A test app",
        &[],
        &[&|m| m.set_with_field_order("version", 2.0, order)],
        "name: my-app\nversion: 2.0\ndescription: A test app",
    );
    check_edits(
        "name: my-app\ndescription: A test app",
        &[],
        &[&|m| m.set_with_field_order("version", 1.0, order)],
        "name: my-app\nversion: 1.0\ndescription: A test app",
    );
    check_edits(
        "version: 1.0\ndescription: A test app",
        &[],
        &[&|m| m.set_with_field_order("name", "my-app", order)],
        "name: my-app\nversion: 1.0\ndescription: A test app",
    );
    check_edits(
        "name: my-app\nversion: 1.0",
        &[],
        &[&|m| m.set_with_field_order("description", "A test app", order)],
        "name: my-app\nversion: 1.0\ndescription: A test app",
    );
    // A key missing from the order goes at the end.
    check_edits(
        "name: my-app\nversion: 1.0",
        &[],
        &[&|m| m.set_with_field_order("author", "John Doe", order)],
        "name: my-app\nversion: 1.0\nauthor: John Doe",
    );
    check_edits(
        "name: my-app\nversion: 1.0",
        &[],
        &[&|m| m.set_with_field_order("author", "Alice", &[] as &[&str])],
        "name: my-app\nversion: 1.0\nauthor: Alice",
    );
    check_edits(
        "name: my-app\nversion: 1.0\ndescription: test",
        &[],
        &[&|m| m.set_with_field_order("author", "Solo", ["version"])],
        "name: my-app\nversion: 1.0\ndescription: test\nauthor: Solo",
    );
    check_edits(
        "name: my-app\nversion: 1.0",
        &[],
        &[&|m| {
            m.set_with_field_order(
                "author",
                "Bob",
                ["name", "name", "author", "version", "version"],
            )
        }],
        "name: my-app\nauthor: Bob\nversion: 1.0",
    );
    // Keys missing from the order don't move the insertion point.
    check_edits(
        "name: my-app\nrandom: value\nversion: 1.0\nanother: data",
        &[],
        &[&|m| m.set_with_field_order("author", "Partial", ["name", "author", "version"])],
        "name: my-app\nauthor: Partial\nrandom: value\nversion: 1.0\nanother: data",
    );
    check_edits(
        "start: value\nend: value",
        &[],
        &[&|m| {
            m.set_with_field_order(
                "middle",
                "inserted",
                [
                    "field1", "field2", "field3", "field4", "field5", "start", "middle", "field6",
                    "field7", "field8", "field9", "field10", "end", "field11", "field12",
                ],
            )
        }],
        "start: value\nmiddle: inserted\nend: value",
    );
    check_edits(
        "名前: アプリ\nversion: 1.0",
        &[],
        &[&|m| m.set_with_field_order("作者", "山田太郎", ["名前", "作者", "version"])],
        "名前: アプリ\n作者: 山田太郎\nversion: 1.0",
    );
    check_edits(
        "regular-key: value1\n\"key with spaces\": value2\n'single-quoted': value3",
        &[],
        &[&|m| {
            m.set_with_field_order(
                "new-key",
                "new-value",
                ["regular-key", "new-key", "key with spaces", "single-quoted"],
            )
        }],
        "regular-key: value1\nnew-key: new-value\n\"key with spaces\": value2\n'single-quoted': value3",
    );
}

#[test]
fn set_with_field_order_several_fields() {
    let order = PACKAGE_ORDER_WITH_AUTHOR;
    check_edits(
        "version: 1.0\nauthor: John Doe\n",
        &[],
        &[
            &|m| m.set_with_field_order("description", "A test app", order),
            &|m| m.set_with_field_order("name", "my-app", order),
        ],
        "name: my-app\nversion: 1.0\ndescription: A test app\nauthor: John Doe\n",
    );
    let order = ["a", "b", "c", "d", "e"];
    check_edits(
        "d: value_d\ne: value_e",
        &[],
        &[
            &|m| m.set_with_field_order("a", "value_a", order),
            &|m| m.set_with_field_order("b", "value_b", order),
            &|m| m.set_with_field_order("c", "value_c", order),
        ],
        "a: value_a\nb: value_b\nc: value_c\nd: value_d\ne: value_e",
    );
    let order = ["field1", "field2", "field3", "field4", "field5"];
    check_edits(
        "field1: value1\nfield5: value5",
        &[],
        &[
            &|m| m.set_with_field_order("field3", "value3", order),
            &|m| m.set_with_field_order("field2", "value2", order),
            &|m| m.set_with_field_order("field4", "value4", order),
            &|m| m.set_with_field_order("field1", "updated1", order),
        ],
        "field1: updated1\nfield2: value2\nfield3: value3\nfield4: value4\nfield5: value5",
    );
    let order = ["name", "version", "count"];
    check_edits(
        "name: my-app\nversion: \"1.0\"\ncount: 42",
        &[],
        &[&|m| m.set_with_field_order("version", 2.0, order), &|m| {
            m.set_with_field_order("count", 100, order)
        }],
        "name: my-app\nversion: 2.0\ncount: 100",
    );
}

/// No blank lines are added around inserted fields.
#[test]
fn set_with_field_order_adds_no_blank_lines() {
    check_edits(
        "a: value_a\nc: value_c",
        &[],
        &[&|m| m.set_with_field_order("b", "value_b", ["a", "b", "c"])],
        "a: value_a\nb: value_b\nc: value_c",
    );
    check_edits(
        "b: value_b\nc: value_c",
        &[],
        &[&|m| m.set_with_field_order("a", "value_a", ["a", "b", "c"])],
        "a: value_a\nb: value_b\nc: value_c",
    );
    let order = ["a", "b", "c", "d"];
    check_edits(
        "a: value_a\nc: value_c",
        &[],
        &[&|m| m.set_with_field_order("b", "value_b", order), &|m| {
            m.set_with_field_order("d", "value_d", order)
        }],
        "a: value_a\nb: value_b\nc: value_c\nd: value_d",
    );
    check_edits(
        "multi:\n - item1\n - item2\n",
        &[],
        &[&|m| m.set_with_field_order("next", "value", ["multi", "next"])],
        "multi:\n - item1\n - item2\nnext: value\n",
    );
    // Blank lines after the entry before stay together, after the new one.
    check_edits(
        "a: 1\n\n\nc: 3\n",
        &[],
        &[&|m| m.set_with_field_order("b", 2, ["a", "b", "c"])],
        "a: 1\nb: 2\n\n\nc: 3\n",
    );
    check_edits(
        "? a\n: 1\n\n? c\n: 3\n",
        &[],
        &[&|m| m.set_with_field_order("b", 2, ["a", "b", "c"])],
        "? a\n: 1\n? b\n: 2\n\n? c\n: 3\n",
    );
}

#[test]
fn set_with_field_order_around_other_values() {
    check_edits(
        "# Header comment\nname: my-app  # inline comment\nversion: 1.0\n# Footer comment",
        &[],
        &[&|m| {
            m.set_with_field_order("author", "John Doe", ["name", "author", "version", "description"])
        }],
        "# Header comment\nname: my-app  # inline comment\nauthor: John Doe\nversion: 1.0\n# Footer comment",
    );
    check_edits(
        "name: my-app\ndescription: |\n  This is a long\n  multiline description\n  that spans multiple lines\nversion: 1.0",
        &[],
        &[&|m| {
            m.set_with_field_order("author", "Jane Smith", ["name", "author", "description", "version"])
        }],
        "name: my-app\nauthor: Jane Smith\ndescription: |\n  This is a long\n  multiline description\n  that spans multiple lines\nversion: 1.0",
    );
    check_edits(
        "name: my-app\nconfig:\n  host: localhost\n  port: 8080\nversion: 1.0",
        &[],
        &[&|m| {
            m.set_with_field_order(
                "author",
                "Developer",
                ["name", "author", "config", "version"],
            )
        }],
        "name: my-app\nauthor: Developer\nconfig:\n  host: localhost\n  port: 8080\nversion: 1.0",
    );
}

#[test]
fn set_with_field_order_nested_keeps_indent() {
    for (source, key, order, expected) in [
        (
            "outer:\n  name: a\n  desc: c\n",
            "version",
            ["name", "version", "desc"],
            "outer:\n  name: a\n  version: '1'\n  desc: c\n",
        ),
        (
            "outer:\n  name: a\n  desc: c\n",
            "alpha",
            ["alpha", "name", "desc"],
            "outer:\n  alpha: '1'\n  name: a\n  desc: c\n",
        ),
        (
            "outer:\n  b: 1\n  d: 2\n  f: 3\n",
            "e",
            ["zz", "e", "f"],
            "outer:\n  b: 1\n  d: 2\n  e: '1'\n  f: 3\n",
        ),
        (
            "outer:\n  b: 1\n  # mid\n  d: 2\n",
            "c",
            ["zz", "c", "d"],
            "outer:\n  b: 1\n  # mid\n  c: '1'\n  d: 2\n",
        ),
        (
            "outer:\n  ? b\n  : 1\n  ? d\n  : 2\n",
            "c",
            ["b", "c", "d"],
            "outer:\n  ? b\n  : 1\n  ? c\n  : '1'\n  ? d\n  : 2\n",
        ),
    ] {
        check_edits(
            source,
            &["outer"],
            &[&|m| m.set_with_field_order(key, "1", order)],
            expected,
        );
    }
}

#[test]
fn set_with_field_order_ignores_comment_indent() {
    let (file, root) = parse("outer:\n  b: 1\n      # deep\n  d: 2\n");
    let outer = root.get_mapping("outer").unwrap();
    assert_eq!(outer.detect_indentation_level(), 2);
    outer.set_with_field_order("c", "1", ["b", "c", "d"]);
    assert_file_edited(&file, "outer:\n  b: 1\n  c: '1'\n      # deep\n  d: 2\n");
}

/// Fields set in an empty `{}` land inside the braces in the requested
/// order (rather than as broken block-style entries after `}`).
#[test]
fn set_with_field_order_in_empty_flow_mapping() {
    let order = ["name", "version", "author", "description"];
    check_edits(
        "---\n{}",
        &[],
        &[
            &|m| m.set_with_field_order("description", "A test app", order),
            &|m| m.set_with_field_order("name", "test-app", order),
            &|m| m.set_with_field_order("author", "Tester", order),
            &|m| m.set_with_field_order("version", "0.1.0", order),
        ],
        "---\n{name: test-app, version: 0.1.0, author: Tester, description: A test app}",
    );

    let yaml = YamlFile::from_str("---\n{}").unwrap();
    let doc = yaml.document().unwrap();
    doc.set_with_field_order("name", "my-app", PACKAGE_ORDER);
    assert_file_cst_ok(&yaml);
    doc.set_with_field_order("version", 1.0, PACKAGE_ORDER);
    assert_file_edited(&yaml, "---\n{name: my-app, version: 1.0}");
}

#[test]
fn document_set_with_field_order() {
    let yaml = YamlFile::from_str("version: 1.0\nauthor: John Doe\n").unwrap();
    let doc = yaml.document().unwrap();
    doc.set_with_field_order("name", "my-app", PACKAGE_ORDER_WITH_AUTHOR);
    assert_file_edited(&yaml, "name: my-app\nversion: 1.0\nauthor: John Doe\n");
}

/// Real-world cases from lintian-brush, setting DEP-12 fields.
#[test]
fn set_with_field_order_lintian_brush() {
    // Name goes first, before Archive, not somewhere in the middle.
    check_edits(
        "Archive: pypi.python.org\nBug-Database: https://gitlab.com/fdroid/blah/issues\nBug-Submit: https://gitlab.com/fdroid/blah/issues/new\nCite-As: https://example.com\nContact: https://forum.example.com",
        &[],
        &[&|m| {
            m.set_with_field_order(
                "Name",
                "blah",
                ["Name", "Contact", "Archive", "Bug-Database", "Bug-Submit", "Cite-As"],
            )
        }],
        "Name: blah\nArchive: pypi.python.org\nBug-Database: https://gitlab.com/fdroid/blah/issues\nBug-Submit: https://gitlab.com/fdroid/blah/issues/new\nCite-As: https://example.com\nContact: https://forum.example.com",
    );

    let order = [
        "Name",
        "Contact",
        "Archive",
        "Bug-Database",
        "Bug-Submit",
        "Registry",
        "Repository",
        "Repository-Browse",
    ];
    check_edits(
        "Registry:\n - Name: conda:conda-forge\n   Entry: r-tsne\n",
        &[],
        &[
            &|m| m.set_with_field_order("Name", "tsne", order),
            &|m| {
                m.set_with_field_order("Contact", "Justin Donaldson <jdonaldson@gmail.com>", order)
            },
            &|m| m.set_with_field_order("Archive", "CRAN", order),
            &|m| {
                m.set_with_field_order(
                    "Bug-Database",
                    "https://github.com/jdonaldson/rtsne/issues",
                    order,
                )
            },
            &|m| {
                m.set_with_field_order(
                    "Bug-Submit",
                    "https://github.com/jdonaldson/rtsne/issues/new",
                    order,
                )
            },
            &|m| {
                m.set_with_field_order(
                    "Repository",
                    "https://github.com/jdonaldson/rtsne.git",
                    order,
                )
            },
            &|m| {
                m.set_with_field_order(
                    "Repository-Browse",
                    "https://github.com/jdonaldson/rtsne",
                    order,
                )
            },
        ],
        "Name: tsne\nContact: Justin Donaldson <jdonaldson@gmail.com>\nArchive: CRAN\nBug-Database: https://github.com/jdonaldson/rtsne/issues\nBug-Submit: https://github.com/jdonaldson/rtsne/issues/new\nRegistry:\n - Name: conda:conda-forge\n   Entry: r-tsne\nRepository: https://github.com/jdonaldson/rtsne.git\nRepository-Browse: https://github.com/jdonaldson/rtsne\n",
    );

    let order = [
        "Name",
        "Bug-Database",
        "Bug-Submit",
        "Repository",
        "Repository-Browse",
        "Security-Contact",
    ];
    check_edits(
        "---\nRepository: https://github.com/example/blah.git\nRepository-Browse: https://github.com/example/blah\n",
        &[],
        &[
            &|m| m.set_with_field_order("Name", "blah", order),
            &|m| {
                m.set_with_field_order(
                    "Bug-Database",
                    "https://github.com/example/blah/issues",
                    order,
                )
            },
            &|m| {
                m.set_with_field_order(
                    "Bug-Submit",
                    "https://github.com/example/blah/issues/new",
                    order,
                )
            },
            &|m| {
                m.set_with_field_order(
                    "Security-Contact",
                    "https://github.com/example/blah/tree/HEAD/SECURITY.md",
                    order,
                )
            },
        ],
        "---\nName: blah\nBug-Database: https://github.com/example/blah/issues\nBug-Submit: https://github.com/example/blah/issues/new\nRepository: https://github.com/example/blah.git\nRepository-Browse: https://github.com/example/blah\nSecurity-Contact: https://github.com/example/blah/tree/HEAD/SECURITY.md\n",
    );
}

#[test]
fn set_reports_whether_the_key_was_set() {
    let file = YamlFile::from_str("a: 1\n").unwrap();
    assert!(file.set("b", 2));
    assert_file_edited(&file, "a: 1\nb: 2\n");

    for yaml in ["scalar\n", "- a\n"] {
        let file = YamlFile::from_str(yaml).unwrap();
        assert!(!file.set("b", 2), "{yaml:?}");
        assert_cst_ok(&file);
        assert_eq!(file.to_string(), yaml);
    }
}

#[test]
fn editing_operations() {
    let yaml = YamlFile::from_str("name: old-name\nversion: 1.0.0").unwrap();
    let doc = yaml.document().unwrap();
    doc.set("name", "new-name");
    assert_cst_ok(&yaml);
    doc.set("version", "2.0.0");
    assert_file_edited(&yaml, "name: new-name\nversion: 2.0.0");
    assert_eq!(doc.get_string("name"), Some("new-name".to_string()));
    assert_eq!(doc.get_string("version"), Some("2.0.0".to_string()));
}

/// `Document::set` replaces a value where it is, adds an entry before a
/// `...` line, and after leading blank lines.
#[test]
fn document_set() {
    check_edit(
        "Name: original\nVersion: 1.0\nAuthor: Someone\n",
        |doc| assert!(doc.set("Version", 2.0)),
        "Name: original\nVersion: 2.0\nAuthor: Someone\n",
    );
    check_edit("\na: 1\n", |doc| assert!(doc.set("b", 2)), "\na: 1\nb: 2\n");
    check_edit("", |doc| assert!(doc.set("a", 1)), "a: 1\n");
    // A comment before a document without `---` is part of it.
    check_edit(
        "# c\nkey: value\n",
        |doc| assert!(doc.set("other", 1)),
        "# c\nkey: value\nother: 1\n",
    );
    check_edit(
        "a: 1\n... # c\n",
        |doc| assert!(doc.set("b", 2)),
        "a: 1\nb: 2\n... # c\n",
    );
}

/// Setting a key on a document whose root is not a mapping does nothing.
#[test]
fn document_set_on_non_mapping_root() {
    let doc = Document::from_str("- a\n- b\n").unwrap();
    assert!(!doc.set("k", "v"));
    assert_edited(&doc, "- a\n- b\n");
    assert!(doc.get("k").is_none());
    assert!(doc.as_sequence().is_some());
    assert!(doc.as_mapping().is_none());

    let doc = Document::from_str("- a\n").unwrap();
    assert!(!doc.set_with_field_order("k", "v", ["k"]));
    assert_edited(&doc, "- a\n");

    let empty = Document::new();
    assert!(empty.set("k", "v"));
    assert_edited(&empty, "---\nk: v\n");
    assert_eq!(empty.get_string("k"), Some("v".to_string()));
}

#[test]
fn document_set_sequence() {
    let doc = Document::new();
    let array_value = SequenceBuilder::new()
        .item(ScalarValue::string("https://github.com/user/repo.git"))
        .item(ScalarValue::string("https://gitlab.com/user/repo.git"))
        .build_document()
        .as_sequence()
        .unwrap();
    doc.set("Repository", &array_value);
    assert_edited(
        &doc,
        "---\nRepository:\n- https://github.com/user/repo.git\n- https://gitlab.com/user/repo.git\n",
    );
    assert!(doc.is_sequence("Repository"));
    assert_eq!(doc.get_sequence("Repository").unwrap().len(), 2);
}

/// Replacing an item spanning lines takes the comment ending its last
/// line along, but leaves white space without one.
#[test]
fn set_multi_line_item_keeps_trailing_space() {
    for (src, expected) in [
        ("- [a,\n  b]   \n- c\n", "- x   \n- c\n"),
        ("- [a,\n  b]  # x\n- c\n", "- x\n- c\n"),
        ("-\n  [a, b]   \n", "-\n  x   \n"),
    ] {
        let doc = YamlFile::from_str(src).unwrap().document().unwrap();
        assert!(doc.as_sequence().unwrap().set(0, "x"));
        assert_edited(&doc, expected);
    }
}

/// A block mapping set as a bare `-` item starts on the dash's line,
/// and a tab in the comment after a replaced item stays.
#[test]
fn set_item_after_bare_dash_and_before_comment() {
    let src = YamlFile::from_str("k:\n  a: 1\n  b: [x,\n    y]\n").unwrap();
    let block = src
        .document()
        .unwrap()
        .as_mapping()
        .unwrap()
        .get("k")
        .unwrap();
    let doc = YamlFile::from_str("-\n- b\n").unwrap().document().unwrap();
    assert!(doc.as_sequence().unwrap().set(0, &block));
    assert_edited(&doc, "- a: 1\n  b: [x,\n    y]\n- b\n");

    // A tab after the dash may separate a scalar from it, but cannot
    // indent a block collection.
    let doc = YamlFile::from_str("-\tx\n").unwrap().document().unwrap();
    assert!(doc.as_sequence().unwrap().set(0, &block));
    assert_edited(&doc, "- a: 1\n  b: [x,\n    y]\n");
    let doc = YamlFile::from_str("-\tx\n").unwrap().document().unwrap();
    assert!(doc.as_sequence().unwrap().set(0, "y"));
    assert_edited(&doc, "-\ty\n");

    let doc = YamlFile::from_str("- x  # a\tb\n")
        .unwrap()
        .document()
        .unwrap();
    assert!(doc.as_sequence().unwrap().set(0, "y"));
    assert_edited(&doc, "- y  # a\tb\n");
}

#[test]
fn set_out_of_bounds() {
    let doc = YamlFile::from_str("- a\n").unwrap().document().unwrap();
    let seq = doc.as_sequence().unwrap();
    assert!(!seq.set(1, "b"));
    assert_edited(&doc, "- a\n");
}

/// `set_path` builds sequence intermediates as pending-block sequences,
/// so an indexed path produces readable block YAML.
#[test]
fn set_path_builds_block_sequence_intermediates() {
    let doc = Document::from_str("name: test\n").unwrap();
    doc.try_set_path("a.b[0]", "v").unwrap();
    assert_edited(&doc, "name: test\na:\n  b:\n    - v\n");
}

#[test]
fn sequence_set_tagged_node() {
    // Sequence::set() was missing TAGGED_NODE from its kind filter, so
    // replacing a tagged-scalar item would leave the original tag+value in place
    // and insert the new value alongside it.
    let yaml = "- !custom foo\n- bar\n";
    let parsed = YamlFile::from_str(yaml).unwrap();
    let doc = parsed.document().unwrap();
    let seq = doc.as_sequence().unwrap();

    assert!(seq.set(0, "replaced"));
    assert_file_edited(&parsed, "- replaced\n- bar\n");

    let values: Vec<_> = seq.values().collect();
    assert_eq!(values.len(), 2);
    assert_eq!(
        values[0].as_scalar().map(|s| s.as_string()),
        Some("replaced".to_string())
    );
    assert_eq!(
        values[1].as_scalar().map(|s| s.as_string()),
        Some("bar".to_string())
    );
}

#[test]
fn sequence_set_scalar_over_commented_block() {
    for (src, expected) in [
        ("-  # c\n  x: 1\n- 2\n", "- 5  # c\n- 2\n"),
        ("- # c\n  - 1\n", "- 5 # c\n"),
    ] {
        let doc = Document::from_str(src).unwrap();
        assert!(doc.as_sequence().unwrap().set(0, 5));
        assert_edited(&doc, expected);
    }
}

#[test]
fn sequence_set_block_mapping_adds_no_blank_line() {
    let value = Document::from_str("k: 1\n").unwrap();
    let value = value.as_mapping().unwrap();
    for (src, index, expected) in [
        ("- a\n- b\n", 1, "- a\n- k: 1\n"),
        ("- a\n- b\n- c\n", 1, "- a\n- k: 1\n- c\n"),
        ("- a\n- b  \n- c\n", 1, "- a\n- k: 1\n- c\n"),
        ("- a\n- b", 1, "- a\n- k: 1\n"),
        ("- j: 2\n- b\n", 0, "- k: 1\n- b\n"),
        ("s:\n  - a\n  - b\nt: 1\n", 0, "s:\n  - k: 1\n  - b\nt: 1\n"),
    ] {
        let doc = Document::from_str(src).unwrap();
        let seq = doc
            .as_sequence()
            .or_else(|| doc.as_mapping()?.get_sequence("s"))
            .unwrap();
        assert!(seq.set(index, value.clone()));
        assert_edited(&doc, expected);
    }
}

#[test]
fn block_collection_values_take_the_entry_indent() {
    let map_doc = Document::from_str("k: 1\nj: 2\n").unwrap();
    let map = YamlNode::Mapping(map_doc.as_mapping().unwrap());
    let nested_doc = Document::from_str("m:\n    k: 1\n    j: 2\n").unwrap();
    let nested = YamlNode::Mapping(nested_doc.as_mapping().unwrap().get_mapping("m").unwrap());
    let seq_doc = Document::from_str("- x\n- y\n").unwrap();
    let seq = YamlNode::Sequence(seq_doc.as_sequence().unwrap());

    let push = |s: &Sequence, v: &dyn AsYaml| s.push(v);
    let insert0 = |s: &Sequence, v: &dyn AsYaml| s.insert(0, v);
    let set0 = |s: &Sequence, v: &dyn AsYaml| assert!(s.set(0, v));
    type Op<'a> = &'a dyn Fn(&Sequence, &dyn AsYaml);
    let cases: Vec<(&str, Op, &dyn AsYaml, &str)> = vec![
        (
            "s:\n  - a\n",
            &push,
            &map,
            "s:\n  - a\n  - k: 1\n    j: 2\n",
        ),
        ("- a\n", &push, &map, "- a\n- k: 1\n  j: 2\n"),
        ("- a\n", &push, &nested, "- a\n- k: 1\n  j: 2\n"),
        (
            "a:\n  s:\n    - a\n",
            &push,
            &map,
            "a:\n  s:\n    - a\n    - k: 1\n      j: 2\n",
        ),
        ("s: [a]\n", &push, &map, "s: [a, {k: 1, j: 2}]\n"),
        ("s:\n  - a\n", &push, &seq, "s:\n  - a\n  - - x\n    - y\n"),
        ("- a\n", &push, &seq, "- a\n- - x\n  - y\n"),
        ("s: [a]\n", &push, &seq, "s: [a, [x, y]]\n"),
        (
            "s:\n  - a\n",
            &insert0,
            &map,
            "s:\n  - k: 1\n    j: 2\n  - a\n",
        ),
        ("- a\n", &insert0, &map, "- k: 1\n  j: 2\n- a\n"),
        ("s: [a]\n", &insert0, &map, "s: [{k: 1, j: 2}, a]\n"),
        (
            "s:\n  - a\n  - b\n",
            &set0,
            &map,
            "s:\n  - k: 1\n    j: 2\n  - b\n",
        ),
        ("- a\n", &set0, &map, "- k: 1\n  j: 2\n"),
        (
            "a:\n  s:\n    - a\n",
            &set0,
            &map,
            "a:\n  s:\n    - k: 1\n      j: 2\n",
        ),
        (
            "s:\n  - &x\n    k: 0\n",
            &set0,
            &map,
            "s:\n  - &x\n    k: 1\n    j: 2\n",
        ),
        ("s: [a, b]\n", &set0, &map, "s: [{k: 1, j: 2}, b]\n"),
    ];
    for (src, op, value, expected) in cases {
        let doc = Document::from_str(src).unwrap();
        let target = doc
            .as_sequence()
            .or_else(|| doc.as_mapping()?.get_sequence("s"))
            .or_else(|| doc.as_mapping()?.get_mapping("a")?.get_sequence("s"))
            .unwrap();
        op(&target, value);
        assert_edited(&doc, expected);
    }
}

#[test]
fn sequence_set_alias() {
    let yaml = "colors:\n  - *red\n  - keep\n";
    let parsed = YamlFile::from_str(yaml).unwrap();
    let doc = parsed.document().unwrap();
    let mapping = doc.as_mapping().unwrap();
    let colors_node = mapping.get("colors").unwrap();
    let seq = colors_node.as_sequence().unwrap();

    assert!(seq.set(0, "blue"));
    assert_file_edited(&parsed, "colors:\n  - blue\n  - keep\n");

    let first = seq.get(0).unwrap();
    assert_eq!(
        first.as_scalar().map(|s| s.as_string()),
        Some("blue".to_string())
    );
}

#[test]
fn sequence_set_with_nested_mapping() {
    let yaml_str = "items:\n  - name: first\n    value: 1\n  - name: second\n    value: 2\n";
    let doc = Document::from_str(yaml_str).unwrap();

    let items_node = doc.try_get_path("items").unwrap();
    let items = items_node.as_sequence().unwrap();

    assert_eq!(items.len(), 2);
    let first = items.get(0).unwrap();
    assert!(first.is_mapping());

    items.set(0, "replaced");
    assert_edited(
        &doc,
        "items:\n  - replaced\n  - name: second\n    value: 2\n",
    );
}

/// `set_path` builds nested intermediates as pending-block mappings, so a
/// deep path produces readable block YAML rather than nested flow.
#[test]
fn set_path_builds_block_intermediates() {
    let doc = Document::from_str("name: test\n").unwrap();
    doc.try_set_path("server.database.host", "localhost")
        .unwrap();
    assert_edited(
        &doc,
        "name: test\nserver:\n  database:\n    host: localhost\n",
    );
}

/// The line break ending an unterminated last entry goes where the parser
/// puts it: in the innermost entry or block scalar on that line.
#[test]
fn set_after_unterminated_nested_value() {
    for (source, expected) in [
        ("a:\n  - 1", "a:\n  - 1\nb: 2"),
        ("a:\n  x: 1", "a:\n  x: 1\nb: 2"),
        ("a: |\n  x", "a: |\n  x\nb: 2"),
    ] {
        let yaml = YamlFile::from_str(source).unwrap();
        yaml.document().unwrap().as_mapping().unwrap().set("b", 2);
        assert_file_edited_valid(&yaml, expected);
    }
}

#[test]
fn set_after_bom_lines_up_with_first_key() {
    // A BOM is not content, so the first key is at column 0.
    let doc = yaml_edit::Document::from_str("\u{FEFF}a: 1\n").unwrap();
    doc.as_mapping().unwrap().set("c", 3);
    assert_edited(&doc, "\u{FEFF}a: 1\nc: 3\n");
}

#[test]
fn set_nested_with_cr_line_breaks() {
    let doc = yaml_edit::Document::from_str("x: 1\ra:\r  b: 1\r").unwrap();
    let mapping = doc.as_mapping().unwrap();
    mapping.get_mapping("a").unwrap().set("d", 4);
    assert_eq!(doc.to_string(), "x: 1\ra:\r  b: 1\r  d: 4\r");
}

#[test]
fn set_writes_crlf_line_breaks() {
    let file = YamlFile::from_str("a: 1\r\n").unwrap();
    let doc = file.document().unwrap();
    let mapping = doc.as_mapping().unwrap();
    mapping.set("b", MappingBuilder::new().pair("c", 1).build_document());
    mapping.set("d", ScalarValue::literal("x\ny\n"));
    mapping.set("e", ScalarValue::single_quoted("x\ny"));
    assert_file_edited_valid(
        &file,
        "a: 1\r\nb:\r\n  c: 1\r\nd: |\r\n  x\r\n  y\r\ne: 'x\r\n\r\n  y'\r\n",
    );
}

#[test]
fn set_without_line_breaks_writes_lf() {
    let file = YamlFile::from_str("a: 1").unwrap();
    file.document().unwrap().as_mapping().unwrap().set("b", 2);
    // The input's last line stays unterminated.
    assert_file_edited_valid(&file, "a: 1\nb: 2");
}

#[test]
fn set_block_scalar_before_blank_line_with_cr_line_breaks() {
    let file = YamlFile::from_str("a: 1\r    \rb: 2\r").unwrap();
    let doc = file.document().unwrap();
    doc.as_mapping()
        .unwrap()
        .set("a", ScalarValue::literal("x\n"));
    assert_file_edited_valid(&file, "a: |\r  x\r\rb: 2\r");
}

#[test]
fn modify_mapping_of_missing_or_scalar_value() {
    let doc = YamlFile::from_str("a: 1\n").unwrap().document().unwrap();
    let mapping = doc.as_mapping().unwrap();
    assert!(!mapping.modify_mapping("a", |_| panic!("a is no mapping")));
    assert!(!mapping.modify_mapping("b", |_| panic!("b is missing")));
    assert_eq!(doc.to_string(), "a: 1\n");
}

/// A new entry goes before the spaces of a last line that holds nothing
/// else.
#[test]
fn set_new_key_before_trailing_space_line() {
    for (src, expected) in [
        ("a: 1\n  ", "a: 1\nb: 2\n  "),
        ("a: 1\n# c\n  ", "a: 1\n# c\nb: 2\n  "),
    ] {
        let doc = YamlFile::from_str(src).unwrap().document().unwrap();
        doc.as_mapping().unwrap().set("b", 2);
        assert_cst_ok(&doc);
        assert_eq!(doc.to_string(), expected, "{src:?}");
    }
}

/// An empty collection set as a value is written in flow form, so it reads
/// back as a collection rather than as null (issue #37).
#[test]
fn set_empty_built_collection() {
    let seq = SequenceBuilder::new()
        .build_document()
        .as_sequence()
        .unwrap();
    let text = edit_root("name: Alice", |m| m.set("foo", seq));
    assert_eq!(text, "name: Alice\nfoo: []");
    assert_eq!(render_documents(&text), ["map{name: Alice, foo: []}"]);

    let map = MappingBuilder::new().build_document().as_mapping().unwrap();
    let text = edit_root("name: Alice", |m| m.set("foo", map));
    assert_eq!(text, "name: Alice\nfoo: {}");
    assert_eq!(render_documents(&text), ["map{name: Alice, foo: {}}"]);
}

/// Setting a key in a mapping updates it where it is, or adds it at the
/// end.
#[test]
fn set_on_root_mapping() {
    assert_eq!(
        edit_root("existing: value", |m| m.set("new_key", "new_value")),
        "existing: value\nnew_key: new_value"
    );
    assert_eq!(
        edit_root("key1: value1", |m| m.set("key2", "value2")),
        "key1: value1\nkey2: value2"
    );
    assert_eq!(
        edit_root("name: Alice", |m| m.set("age", 30)),
        "name: Alice\nage: 30"
    );
    assert_eq!(
        edit_root("name: Alice\nage: 30", |m| m.set("age", 31)),
        "name: Alice\nage: 31"
    );
    assert_eq!(
        edit_root("key1: value1\nkey2: value2\n", |m| m
            .set("key1", "new_value")),
        "key1: new_value\nkey2: value2\n"
    );
    assert_eq!(
        edit_root(
            "Name: original_name\nContact: original_contact\nRepository: https://github.com/example/repo.git\n",
            |m| m.set("Contact", "updated_contact")
        ),
        "Name: original_name\nContact: updated_contact\nRepository: https://github.com/example/repo.git\n"
    );
    assert_eq!(
        edit_root(
            "Name: tsne\nContact: Justin Donaldson <jdonaldson@gmail.com>\nArchive: CRAN\nRepository: https://github.com/jdonaldson/rtsne.git\n",
            |m| {
                m.set("Contact", "New Contact <new@example.com>");
                assert_cst_ok(m);
                m.set("Archive", "PyPI");
            }
        ),
        "Name: tsne\nContact: New Contact <new@example.com>\nArchive: PyPI\nRepository: https://github.com/jdonaldson/rtsne.git\n"
    );
    assert_eq!(
        edit_root("key: value", |m| {
            m.set("number", 123);
            assert_cst_ok(m);
            m.set("bool", false);
            assert_cst_ok(m);
            m.set("text", "hello");
        }),
        "key: value\nnumber: 123\nbool: false\ntext: hello"
    );
    assert_eq!(
        edit_root("name: Alice", |m| {
            assert_eq!(m.len(), 1);
            m.set("age", 30);
            assert_eq!(m.len(), 2);
            m.set("city", "NYC");
            assert_eq!(m.len(), 3);
        }),
        "name: Alice\nage: 30\ncity: NYC"
    );
}

#[test]
fn set_remove_and_rename_together() {
    let original = r#"name: my-app
version: 1.0
author: Alice
year: 2023

features:
  - logging
  - auth"#;
    let text = edit_root(original, |m| {
        m.set("license", "MIT");
        assert_cst_ok(m);
        m.set("published", true);
        assert_cst_ok(m);
        m.set("downloads", 1000);
        assert_cst_ok(m);
        assert!(m.remove("author").is_some());
        assert_cst_ok(m);
        assert!(m.rename_key("version", "app_version"));
        assert_cst_ok(m);
        m.set("year", 2024);
    });
    assert_eq!(
        text,
        r#"name: my-app
app_version: 1.0
year: 2024

features:
  - logging
  - auth
license: MIT
published: true
downloads: 1000"#
    );
}

/// `Mapping::entry`: the `or_insert*` methods leave an existing value
/// alone and add a missing one at the end, returning the value.
#[test]
fn entry_or_insert() {
    let root = |d: &Document| d.as_mapping().unwrap();
    check_edit(
        "name: Alice\n",
        |d| assert_eq!(root(d).entry("age").or_insert(30).to_string(), "30"),
        "name: Alice\nage: 30\n",
    );
    check_edit(
        "name: Alice\n",
        |d| assert_eq!(root(d).entry("name").or_insert("Bob").to_string(), "Alice"),
        "name: Alice\n",
    );
    check_edit(
        "count: 5\n",
        |d| {
            let mut called = false;
            let _ = root(d).entry("count").or_insert_with(|| {
                called = true;
                99_i64
            });
            assert!(!called);
            let _ = root(d).entry("other").or_insert_with(|| {
                called = true;
                123_i64
            });
            assert!(called);
        },
        "count: 5\nother: 123\n",
    );
    check_edit(
        "a: 1\n",
        |d| {
            let _ = root(d).entry("s").or_insert_with(|| "hello");
            let _ = root(d).entry("n").or_insert_with(|| 42_i64);
            let _ = root(d).entry("b").or_insert_with(|| true);
        },
        "a: 1\ns: hello\nn: 42\nb: true\n",
    );
    // Comments on the keys left alone stay.
    check_edit(
        "keep: this  # inline\nother: value\n",
        |d| {
            root(d).entry("added").or_insert(1_i64);
        },
        "keep: this  # inline\nother: value\nadded: 1\n",
    );
}

/// `or_insert_mapping` and `or_insert_sequence` return the existing
/// collection, or replace a value of another kind with an empty one.
#[test]
fn entry_or_insert_collection() {
    let root = |d: &Document| d.as_mapping().unwrap();
    check_edit(
        "existing: value\n",
        |d| {
            let nested = root(d).entry("nested").or_insert_mapping();
            assert_cst_ok(d);
            nested.set("inner", "value");
        },
        "existing: value\nnested:\n  inner: value\n",
    );
    check_edit(
        "root:\n  a: 1\n",
        |d| {
            let nested = root(d).entry("root").or_insert_mapping();
            assert_cst_ok(d);
            nested.set("b", 2);
        },
        "root:\n  a: 1\n  b: 2\n",
    );
    check_edit(
        "k: scalar\n",
        |d| {
            let nested = root(d).entry("k").or_insert_mapping();
            assert_cst_ok(d);
            nested.set("inner", "v");
        },
        "k:\n  inner: v\n",
    );
    check_edit(
        "existing: value\n",
        |d| {
            let list = root(d).entry("items").or_insert_sequence();
            assert_cst_ok(d);
            list.push("apple");
            assert_cst_ok(d);
            list.push("banana");
        },
        "existing: value\nitems:\n  - apple\n  - banana\n",
    );
    check_edit(
        "items:\n  - apple\n",
        |d| {
            let list = root(d).entry("items").or_insert_sequence();
            assert_cst_ok(d);
            list.push("banana");
        },
        "items:\n  - apple\n  - banana\n",
    );
    check_edit(
        "a: 1\nb: [x]\n",
        |d| {
            let mapping = root(d);
            assert_eq!(mapping.entry("a").or_insert_sequence().to_string(), "[]");
            assert_cst_ok(d);
            assert_eq!(d.to_string(), "a: []\nb: [x]\n");
            assert_eq!(mapping.entry("b").or_insert_sequence().len(), 1);
            match mapping.entry("zz") {
                Entry::Vacant(v) => assert_eq!(v.into_key(), "zz"),
                Entry::Occupied(_) => panic!("zz is not in the mapping"),
            }
        },
        "a: []\nb: [x]\n",
    );
}

#[test]
fn entry_and_modify() {
    let root = |d: &Document| d.as_mapping().unwrap();
    check_edit(
        "count: 1\n",
        |d| {
            root(d).entry("count").and_modify(|e| e.set_value(42));
        },
        "count: 42\n",
    );
    check_edit(
        "count: 1\n",
        |d| {
            let mut called = false;
            root(d).entry("missing").and_modify(|_| called = true);
            assert!(!called);
        },
        "count: 1\n",
    );
    check_edit(
        "count: 1\n",
        |d| {
            let mapping = root(d);
            mapping
                .entry("count")
                .and_modify(|e| e.set_value(2))
                .or_insert(0);
            assert_cst_ok(d);
            assert_eq!(mapping.get("count").unwrap().to_string(), "2");
            mapping
                .entry("new")
                .and_modify(|e| e.set_value("never"))
                .or_insert("first");
            assert_eq!(mapping.get("new").unwrap().to_string(), "first");
        },
        "count: 2\nnew: first\n",
    );
}

#[test]
fn entry_variants() {
    let doc = Document::from_str("k: v\n").unwrap();
    let mapping = doc.as_mapping().unwrap();
    match mapping.entry("k") {
        Entry::Occupied(o) => {
            assert_eq!(o.key().unwrap().to_string(), "k");
            assert_eq!(o.get().unwrap().to_string(), "v");
        }
        Entry::Vacant(_) => panic!("expected occupied"),
    }
    match mapping.entry("missing") {
        Entry::Vacant(v) => assert_eq!(*v.key(), "missing"),
        Entry::Occupied(_) => panic!("expected vacant"),
    }
    assert_eq!(mapping.entry("k").key().unwrap().to_string(), "k");
    assert_eq!(
        mapping.entry("missing").key().unwrap().to_string(),
        "missing"
    );

    // Keys match by value, not by spelling.
    let doc = Document::from_str("\"name\": Alice\n").unwrap();
    assert!(matches!(
        doc.as_mapping().unwrap().entry("name"),
        Entry::Occupied(_)
    ));
}

#[test]
fn entry_insert_and_remove() {
    let root = |d: &Document| d.as_mapping().unwrap();
    check_edit(
        "a: 1\n",
        |d| {
            let mapping = root(d);
            let Entry::Vacant(v) = mapping.entry("b") else {
                panic!("expected vacant");
            };
            assert_eq!(v.insert(2_i64).to_string(), "2");
        },
        "a: 1\nb: 2\n",
    );
    check_edit(
        "a: 1\nb: 2\n",
        |d| {
            let mapping = root(d);
            let Entry::Occupied(o) = mapping.entry("a") else {
                panic!("expected occupied");
            };
            let removed = o.remove_entry();
            assert_eq!(removed.key_node().unwrap().to_string(), "a");
            assert!(!mapping.contains_key("a"));
        },
        "b: 2\n",
    );
}
