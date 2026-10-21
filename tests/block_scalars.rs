//! Block scalars: how literal (`|`) and folded (`>`) scalars with their
//! chomping and indentation indicators parse, and how edits write them.
//!
//! A block scalar with an explicit indentation indicator (`|2`) moved to
//! another column. The indicator counts from the parent's column, so the
//! body has to move with the parent for the value to stay the same.
//!
//! A block scalar written by an edit must not take in the lines after it:
//! a whitespace-only line with spaces past its body's column, or a comment
//! line indented as far as its body, would otherwise read as body text.
//!
//! Strings written as literal or folded block scalars must read back as the
//! same string, falling back to a quoted style where a block scalar can't
//! hold them.
//!
//! Inserting a value written as a kept (`|+`, `>+`) block scalar in front of
//! blank lines: those lines would be read as part of the scalar, so the new
//! entry goes after them.

mod common;
use common::{assert_file_cst_ok, assert_file_edited};
use rowan::ast::AstNode;
use std::str::FromStr;
use yaml_edit::path::YamlPath;
use yaml_edit::validator::Validator;
use yaml_edit::{debug, ScalarValue, Sequence, YamlFile, YamlNode};

/// Every scalar under `node`, with its path of mapping keys and sequence
/// indexes joined by `.`.
fn collect_leaves(node: &YamlNode, path: String, out: &mut Vec<(String, String)>) {
    let join = |step: String| {
        if path.is_empty() {
            step
        } else {
            format!("{path}.{step}")
        }
    };
    match node {
        YamlNode::Scalar(s) => out.push((path, s.as_string())),
        YamlNode::Sequence(s) => {
            for (i, item) in s.values().enumerate() {
                collect_leaves(&item, join(i.to_string()), out);
            }
        }
        YamlNode::Mapping(m) => {
            for (k, v) in m.iter() {
                collect_leaves(&v, join(k.as_scalar().unwrap().as_string()), out);
            }
        }
        other => panic!("unexpected node {other:?}"),
    }
}

/// Parse `yaml`, check it round-trips, and return every scalar value in it
/// with its path.
#[track_caller]
fn leaves(yaml: &str) -> Vec<(String, String)> {
    let file = YamlFile::from_str(yaml).unwrap_or_else(|e| panic!("{yaml:?}: {e}"));
    assert_eq!(file.to_string(), yaml);
    let doc = file.document().unwrap();
    let root = doc
        .syntax()
        .children()
        .find_map(YamlNode::from_syntax)
        .unwrap();
    let mut out = Vec::new();
    collect_leaves(&root, String::new(), &mut out);
    out
}

/// Expected scalar values by path, as [`leaves`] lists them.
type Leaves<'a> = &'a [(&'a str, &'a str)];

#[track_caller]
fn assert_leaves(yaml: &str, expected: &[(&str, &str)]) {
    let expected: Vec<_> = expected
        .iter()
        .map(|(p, v)| (p.to_string(), v.to_string()))
        .collect();
    assert_eq!(leaves(yaml), expected, "{yaml:?}");
}

fn header_violations(file: &YamlFile) -> Vec<(String, Option<String>)> {
    Validator::new()
        .validate_syntax(file.syntax())
        .into_iter()
        .map(|v| (v.message, v.location))
        .collect()
}

fn under_indented(location: &str) -> Vec<(String, Option<String>)> {
    vec![(
        "Block scalar header must be indented past its parent node".to_string(),
        Some(location.to_string()),
    )]
}

#[test]
fn literal_scalars() {
    let cases: &[(&str, Leaves)] = &[
        // Default chomping (clip) keeps a single trailing newline.
        (
            r#"
text: |
  Line 1
  Line 2
  Line 3
"#,
            &[("text", "Line 1\nLine 2\nLine 3\n")],
        ),
        // Strip chomping (-) removes all trailing newlines.
        (
            r#"
text: |-
  Line 1
  Line 2
  Line 3
"#,
            &[("text", "Line 1\nLine 2\nLine 3")],
        ),
        // Keep chomping (+) keeps all trailing newlines.
        (
            r#"
text: |+
  Line 1
  Line 2
  Line 3


"#,
            &[("text", "Line 1\nLine 2\nLine 3\n\n\n")],
        ),
        (
            r#"
text: |2
  Line 1
  Line 2
"#,
            &[("text", "Line 1\nLine 2\n")],
        ),
        (
            r#"
strip_with_indent: |-2
  Line 1
  Line 2

keep_with_indent: |+2
  Line 1
  Line 2
"#,
            &[
                ("strip_with_indent", "Line 1\nLine 2"),
                ("keep_with_indent", "Line 1\nLine 2\n"),
            ],
        ),
        (
            r#"
text: |
  Paragraph 1
  continues here

  Paragraph 2
  after blank line
"#,
            &[(
                "text",
                "Paragraph 1\ncontinues here\n\nParagraph 2\nafter blank line\n",
            )],
        ),
        (
            r#"
code: |
  def hello():
      print("Hello")
      if True:
          print("World")
"#,
            &[(
                "code",
                "def hello():\n    print(\"Hello\")\n    if True:\n        print(\"World\")\n",
            )],
        ),
        (
            r#"
empty: |
"#,
            &[("empty", "")],
        ),
        // The base indentation is stripped.
        (
            r#"
text: |
    Leading spaces
    are preserved
"#,
            &[("text", "Leading spaces\nare preserved\n")],
        ),
        (
            r#"
text: |
  Single line
"#,
            &[("text", "Single line\n")],
        ),
        (
            "text: |\n  Line with spaces  \n  Another line  \n",
            &[("text", "Line with spaces  \nAnother line  \n")],
        ),
        (
            r#"
text: |
  Special chars: !@#$%^&*()
  Quotes: "double" and 'single'
  Colons: key: value
  Dashes: - item
"#,
            &[(
                "text",
                "Special chars: !@#$%^&*()\nQuotes: \"double\" and 'single'\nColons: key: value\nDashes: - item\n",
            )],
        ),
        // From the YAML spec.
        (
            r#"
example: |
  Several lines of text,
  with some "quotes" of various 'types',
  and also a blank line:

  and some more text.
"#,
            &[(
                "example",
                "Several lines of text,\nwith some \"quotes\" of various 'types',\nand also a blank line:\n\nand some more text.\n",
            )],
        ),
        // YAML allows only a single digit, 1 to 9, as indentation
        // indicator, and both saphyr and PyYAML reject `|10`; the lenient
        // parser reads the `1` and ignores the `0`. Content therefore
        // starts at column 1, so the nine further spaces are part of the
        // value.
        ("text: |10\n          Deep\n", &[("text", "         Deep\n")]),
        // Trailing whitespace is allowed after the indicator (spec
        // 8.1.1.2).
        ("text: |  \n  Content here\n", &[("text", "Content here\n")]),
        // A CRLF line break reads as a `\n` in the value, and is kept in
        // the text.
        (
            "text: |\r\n  Line 1\r\n  Line 2\r\n",
            &[("text", "Line 1\nLine 2\n")],
        ),
        // Tabs are not allowed for indentation, but the parser accepts the
        // tab and keeps it in the value.
        ("text: |\n\t  Mixed\n", &[("text", "\t  Mixed\n")]),
    ];
    for (yaml, expected) in cases {
        assert_leaves(yaml, expected);
    }
}

#[test]
fn folded_scalars() {
    let cases: &[(&str, Leaves)] = &[
        (
            r#"
text: >
  This is a long line
  that should be folded
  into a single line.
"#,
            &[(
                "text",
                "This is a long line that should be folded into a single line.\n",
            )],
        ),
        (
            r#"
text: >-
  This is folded text
  with strip chomping
"#,
            &[("text", "This is folded text with strip chomping")],
        ),
        (
            r#"
text: >+
  This is folded text
  with keep chomping


"#,
            &[("text", "This is folded text with keep chomping\n\n\n")],
        ),
        (
            r#"
text: >1
 Indented text
 continues here
"#,
            &[("text", "Indented text continues here\n")],
        ),
        (
            r#"
strip_folded: >-2
  Text here
  continues

keep_folded: >+1
 Text here
 continues
"#,
            &[
                ("strip_folded", "Text here continues"),
                ("keep_folded", "Text here continues\n"),
            ],
        ),
        // A blank line becomes a line break.
        (
            r#"
text: >
  First paragraph
  continues here.

  Second paragraph
  after blank line.
"#,
            &[(
                "text",
                "First paragraph continues here.\nSecond paragraph after blank line.\n",
            )],
        ),
        // More-indented lines are not folded.
        (
            r#"
text: >
  Normal text
    Indented text
      More indented
  Back to normal
"#,
            &[(
                "text",
                "Normal text\n  Indented text\n    More indented\nBack to normal\n",
            )],
        ),
        (
            r#"
empty: >
"#,
            &[("empty", "")],
        ),
        (
            r#"
text: >
  Single line
"#,
            &[("text", "Single line\n")],
        ),
        // From the YAML spec.
        (
            r#"
example: >
  Several lines of text,
  with some "quotes" of various 'types',
  and also a blank line:

  and some more text.
"#,
            &[(
                "example",
                "Several lines of text, with some \"quotes\" of various 'types', and also a blank line:\nand some more text.\n",
            )],
        ),
    ];
    for (yaml, expected) in cases {
        assert_leaves(yaml, expected);
    }
}

#[test]
fn block_scalars_among_other_nodes() {
    let cases: &[(&str, Leaves)] = &[
        (
            r#"
description: |
  This is a description
  with multiple lines.

notes: >
  These are notes
  that will be folded.

code: |-
  def function():
      pass
"#,
            &[
                (
                    "description",
                    "This is a description\nwith multiple lines.\n",
                ),
                ("notes", "These are notes that will be folded.\n"),
                ("code", "def function():\n    pass"),
            ],
        ),
        (
            r#"
items:
  - |
    First item
    with multiple lines
  - >
    Second item
    folded text
  - |-
    Third item
    stripped
"#,
            &[
                ("items.0", "First item\nwith multiple lines\n"),
                ("items.1", "Second item folded text\n"),
                ("items.2", "Third item\nstripped"),
            ],
        ),
        (
            r#"
clip: |
  text

strip: |-
  text

keep: |+
  text

"#,
            &[("clip", "text\n"), ("strip", "text"), ("keep", "text\n\n")],
        ),
        (
            "literal: |\n\nfolded: >\n\nstrip: |-\n\nkeep: |+\n",
            &[("literal", ""), ("folded", ""), ("strip", ""), ("keep", "")],
        ),
        // The parser switches from flow to block context.
        (
            r#"flow: [a, b, c]
block: |
  Content
  Here
"#,
            &[
                ("flow.0", "a"),
                ("flow.1", "b"),
                ("flow.2", "c"),
                ("block", "Content\nHere\n"),
            ],
        ),
        (
            "- foo: >+\n      \n- baz\n",
            &[("0.foo", "\n"), ("1", "baz")],
        ),
        ("- a: |\n  b: 1\n", &[("0.a", ""), ("0.b", "1")]),
        (
            "- k:\n    a: |\n\n    b: 1\n",
            &[("0.k.a", ""), ("0.k.b", "1")],
        ),
    ];
    for (yaml, expected) in cases {
        assert_leaves(yaml, expected);
    }
}

#[test]
fn whitespace_only_line_does_not_swallow_next_entry() {
    // The second is the YAML test suite's Y79Y/001.
    for (yaml, value) in [
        ("foo: |\n  \nbar: 1\n", ""),
        ("foo: |\n \t\nbar: 1\n", "\t\n"),
    ] {
        let doc = YamlFile::from_str(yaml).unwrap();
        let mapping = doc.document().unwrap().as_mapping().unwrap();
        let foo = mapping.get("foo").unwrap();
        assert_eq!(foo.to_string(), yaml[5..yaml.len() - 7], "{yaml:?}");
        assert_leaves(yaml, &[("foo", value), ("bar", "1")]);
    }
}

#[test]
fn header_comment_does_not_set_indicators() {
    for (yaml, expected) in [
        ("k: > # a-b\n  x\n", "x\n"),
        ("k: | # a+b\n  x\n\n", "x\n"),
        ("k: | # 1\n   x\n", "x\n"),
        ("k: |-2 # a+\n    x\n", "  x"),
    ] {
        assert_leaves(yaml, &[("k", expected)]);
    }
}

/// The indentation indicator counts from the enclosing collection's column
/// (spec 8.1.1.1), not from column 0.
#[test]
fn indentation_indicator_is_relative_to_parent() {
    let cases: &[(&str, Leaves)] = &[
        // The YAML test suite's 4WA9.
        (
            "- aaa: |2\n    xxx\n  bbb: |\n    xxx\n",
            &[("0.aaa", "xxx\n"), ("0.bbb", "xxx\n")],
        ),
        (
            "a:\n  b: |1\n    x\n  c: 1\n",
            &[("a.b", " x\n"), ("a.c", "1")],
        ),
        ("o:\n  k: |2\n      x\n", &[("o.k", "  x\n")]),
        ("- - |1\n    x\n", &[("0.0", " x\n")]),
        // A header on its own line still counts from the mapping's column.
        (
            "o:\n  k:\n    |1\n     x\n  z: 1\n",
            &[("o.k", "  x\n"), ("o.z", "1")],
        ),
        // In an explicit entry the indicator counts from the mapping's
        // column, wherever the header sits.
        (
            "x:\n  ? a\n  : |1\n     x\n  b: 1\n",
            &[("x.a", "  x\n"), ("x.b", "1")],
        ),
        (
            "x:\n  ? a\n  :\n    |1\n     x\n  b: 1\n",
            &[("x.a", "  x\n"), ("x.b", "1")],
        ),
        (
            "x:\n  ? a\n  : # c\n    |1\n     x\n  b: 1\n",
            &[("x.a", "  x\n"), ("x.b", "1")],
        ),
        (
            "- ? a\n  : |1\n     x\n  b: 1\n",
            &[("0.a", "  x\n"), ("0.b", "1")],
        ),
        (
            "- ? a\n  :\n    |1\n     x\n  b: 1\n",
            &[("0.a", "  x\n"), ("0.b", "1")],
        ),
        (
            "x:\n  c: 1\n  ? a\n  :\n    |1\n     x\n  b: 1\n",
            &[("x.c", "1"), ("x.a", "  x\n"), ("x.b", "1")],
        ),
        (
            "? a\n: b: |1\n     x\n  c: 1\n",
            &[("a.b", "  x\n"), ("a.c", "1")],
        ),
        (
            "? a\n: b:\n    |1\n     x\n  c: 1\n",
            &[("a.b", "  x\n"), ("a.c", "1")],
        ),
    ];
    for (yaml, expected) in cases {
        assert_leaves(yaml, expected);
    }

    // A nested explicit mapping as the key, at column 2.
    let yaml = "? ? a\n  :\n    |1\n     x\n  b: 1\n: v\n";
    let doc = YamlFile::from_str(yaml).unwrap();
    assert_eq!(doc.to_string(), yaml);
    let mapping = doc.document().unwrap().as_mapping().unwrap();
    let (key, value) = mapping.iter().next().unwrap();
    let mut key_leaves = Vec::new();
    collect_leaves(&key, String::new(), &mut key_leaves);
    assert_eq!(
        key_leaves,
        [("a", "  x\n"), ("b", "1")].map(|(p, v)| (p.to_string(), v.to_string()))
    );
    assert_eq!(value.as_scalar().unwrap().as_string(), "v");

    // The key's own header counts from the `?`.
    let yaml = "x:\n  ?\n    |1\n     x\n  : v\n";
    let doc = YamlFile::from_str(yaml).unwrap();
    assert_eq!(doc.to_string(), yaml);
    let x = doc.document().unwrap().get_mapping("x").unwrap();
    let (key, value) = x.iter().next().unwrap();
    assert_eq!(key.as_scalar().unwrap().as_string(), "  x\n");
    assert_eq!(value.as_scalar().unwrap().as_string(), "v");
}

/// A block scalar header on the line after its key, but not indented past
/// it, is still the key's value: nothing else starts with `|` or `>`. The
/// validator reports it, as `s-separate(n+1)` rules it out. The body still
/// has to clear the key's column, so `b` stays a sibling.
#[test]
fn header_at_key_column_is_the_value() {
    let cases: &[(&str, Leaves, &str)] = &[
        ("a:\n|\n x\n", &[("a", "x\n")], "2:1"),
        ("a: &x\n|\n x\n", &[("a", "x\n")], "2:1"),
        ("a:\n>\n x\n y\n", &[("a", "x y\n")], "2:1"),
        ("a:\n|2\n  x\n", &[("a", "x\n")], "2:1"),
        ("a:\n|\nb: 1\n", &[("a", ""), ("b", "1")], "2:1"),
        (
            "- a:\n  |\n   x\n  b: 1\n",
            &[("0.a", "x\n"), ("0.b", "1")],
            "2:3",
        ),
    ];
    for (yaml, expected, location) in cases {
        assert_leaves(yaml, expected);
        let file = YamlFile::from_str(yaml).unwrap();
        assert_eq!(
            header_violations(&file),
            under_indented(location),
            "{yaml:?}"
        );
    }
}

/// A block scalar on the line after its key, in a mapping that is a
/// sequence item, ends at a line back at the key's column.
#[test]
fn block_value_body_bounded_by_key_column_in_sequence_item() {
    for (yaml, expected) in [
        ("- a:\n   |\n  b: 1\n", ""),
        ("- a:\n   >\n  b: 1\n", ""),
        ("- a:\n   |\n    x\n  b: 1\n", "x\n"),
    ] {
        assert_leaves(yaml, &[("0.a", expected), ("0.b", "1")]);
        let file = YamlFile::from_str(yaml).unwrap();
        assert_eq!(header_violations(&file), vec![], "{yaml:?}");
    }
}

/// Render a node as a compact string, so a test can compare a whole
/// structure at once.
fn render(node: &YamlNode) -> String {
    match node {
        YamlNode::Scalar(s) if s.is_null() => "null".to_string(),
        YamlNode::Scalar(s) => format!("{:?}", s.as_string()),
        YamlNode::Sequence(s) => {
            let items: Vec<_> = s.values().map(|v| render(&v)).collect();
            format!("[{}]", items.join(", "))
        }
        YamlNode::Mapping(m) => {
            let entries: Vec<_> = m
                .iter()
                .map(|(k, v)| format!("{}: {}", render(&k), render(&v)))
                .collect();
            format!("{{{}}}", entries.join(", "))
        }
        YamlNode::TaggedNode(t) => format!("{} {:?}", t.tag().unwrap(), t.as_string()),
        YamlNode::Alias(a) => format!("*{}", a.name()),
    }
}

/// A block scalar header on the line after a `-`, `?` or explicit `:`, but
/// not indented past it, can only be that indicator's node: nothing else
/// starts with `|` or `>`. `s-separate(n+1)` rules it out, so the validator
/// reports it.
#[test]
fn header_at_indicator_column_is_the_node() {
    for (yaml, expected, location) in [
        ("-\n|\n x\n", r#"["x\n"]"#, Some("2:1")),
        ("-\n>\n x\n y\n", r#"["x y\n"]"#, Some("2:1")),
        ("-\n|\n x\n- y\n", r#"["x\n", "y"]"#, Some("2:1")),
        ("-\n|\n- y\n", r#"["", "y"]"#, Some("2:1")),
        ("- # c\n|\n x\n", r#"["x\n"]"#, Some("2:1")),
        ("-\n# c\n|\n x\n", r#"["x\n"]"#, Some("3:1")),
        ("- -\n  |\n   x\n", r#"[["x\n"]]"#, Some("2:3")),
        ("- -\n  |\n  - y\n", r#"[["", "y"]]"#, Some("2:3")),
        ("-\n |\n  x\n", r#"["x\n"]"#, None),
        ("?\n|\n x\n", r#"{"x\n": null}"#, Some("2:1")),
        ("?\n|\n x\n: v\n", r#"{"x\n": "v"}"#, Some("2:1")),
        ("? # c\n|\n x\n", r#"{"x\n": null}"#, Some("2:1")),
        ("? a\n:\n|\n x\n", r#"{"a": "x\n"}"#, Some("3:1")),
        (
            "? a\n:\n>\n x\n? b\n",
            r#"{"a": "x\n", "b": null}"#,
            Some("3:1"),
        ),
        ("? a\n: # c\n|\n x\n", r#"{"a": "x\n"}"#, Some("3:1")),
        ("- ? a\n  :\n  |\n   x\n", r#"[{"a": "x\n"}]"#, Some("3:3")),
        ("- ?\n  |\n   x\n", r#"[{"x\n": null}]"#, Some("2:3")),
        ("? a\n:\n |\n  x\n", r#"{"a": "x\n"}"#, None),
    ] {
        let file = YamlFile::from_str(yaml).unwrap_or_else(|e| panic!("{yaml:?}: {e}"));
        assert_eq!(file.to_string(), yaml);
        debug::validate_tree(file.syntax()).unwrap();
        let root = file
            .document()
            .unwrap()
            .syntax()
            .children()
            .find_map(YamlNode::from_syntax)
            .unwrap();
        assert_eq!(render(&root), expected, "{yaml:?}");
        let expected_violations = location.map_or(vec![], under_indented);
        assert_eq!(header_violations(&file), expected_violations, "{yaml:?}");
    }
}

/// A block scalar's body is literal text, so an indicator opening a body
/// line starts nothing: a `|` or `>` there is not another header, and a
/// quote after it does not run on into the following entries.
#[test]
fn indicators_opening_block_scalar_body_lines() {
    for (yaml, a, b) in [
        ("a: |\n  >'\nb: ''\n", ">'\n", ""),
        ("a: |\n  |'\nb: ''\n", "|'\n", ""),
        ("a: >\n  |\"\nb: \"\"\n", "|\"\n", ""),
        ("a: |\n  >\n  {x\nb: c, d\n", ">\n{x\n", "c, d"),
        ("a: |\n  &'\nb: ''\n", "&'\n", ""),
        ("a: |\n  *'\nb: ''\n", "*'\n", ""),
        ("a: |\n  !'\nb: ''\n", "!'\n", ""),
        ("a: |\n  ? '\nb: ''\n", "? '\n", ""),
        ("a: |\n  - '\nb: ''\n", "- '\n", ""),
    ] {
        assert_leaves(yaml, &[("a", a), ("b", b)]);
    }
    assert_leaves("- |\n  |'\n- ''\n", &[("0", "|'\n"), ("1", "")]);
}

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

/// The value of `v` in `src`, as a node of a parsed file.
fn value_of(src: &str) -> YamlNode {
    node_at(src, "v")
}

/// Check that `file` renders as `expected` and that the scalar at `path`
/// reads back as `value`.
#[track_caller]
fn check(file: &YamlFile, expected: &str, path: &str, value: &str) {
    let text = file.to_string();
    assert_eq!(text, expected);
    assert_file_cst_ok(file);
    assert_eq!(scalar_string(&node_at(&text, path)), value);
}

/// Like [`check`], and the tree is also the one the parser builds for the
/// text. Block scalars built from a string are a single token, unlike
/// parsed ones, so this only holds for scalars copied from a parsed file.
#[track_caller]
fn check_parser_shaped(file: &YamlFile, expected: &str, path: &str, value: &str) {
    check(file, expected, path, value);
    let reparsed = YamlFile::from_str(expected).unwrap();
    assert_eq!(
        debug::shape_to_string(file.syntax()),
        debug::shape_to_string(reparsed.syntax())
    );
}

/// An edit of a parsed file, the text it should give, and the path and
/// value of the scalar it wrote.
type EditCase = (
    &'static str,
    fn(&YamlFile),
    &'static str,
    &'static str,
    &'static str,
);

#[track_caller]
fn check_edits(cases: &[EditCase], check: fn(&YamlFile, &str, &str, &str)) {
    for &(source, edit, expected, path, value) in cases {
        let f = YamlFile::from_str(source).unwrap();
        edit(&f);
        check(&f, expected, path, value);
    }
}

#[track_caller]
fn assert_moved(file: &YamlFile, expected: &str, path: &str, source: &YamlNode) {
    assert_file_edited(file, expected);
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
        for (source, key, expected) in [
            ("k: 1\n", None, root),
            ("o:\n  k: 1\n", Some("o"), nested),
            ("o:\n  a: 1\n", Some("o"), added),
        ] {
            let file = YamlFile::from_str(source).unwrap();
            let doc = file.document().unwrap();
            let mapping = match key {
                Some(key) => doc.get_mapping(key).unwrap(),
                None => doc.as_mapping().unwrap(),
            };
            mapping.set("k", &value);
            let path = if key.is_some() { "o.k" } else { "k" };
            assert_moved(&file, expected, path, &value);
        }
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
            .get_sequence("o")
            .unwrap()
            .set(0, &value);
        assert_moved(&file, nested, "o[0]", &value);
    }
}

#[test]
fn written_block_scalar_leaves_deep_whitespace_and_comment_lines_alone() {
    check_edits(
        &[
            (
                "a: 1\n     \nb: 2\n",
                |f| assert!(f.insert_after("a", "k", "text\n")),
                "a: 1\nk: |\n  text\n\nb: 2\n",
                "k",
                "text\n",
            ),
            (
                "a: 1\n  \nb: 2\n",
                |f| assert!(f.insert_after("a", "k", "text\n")),
                "a: 1\nk: |\n  text\n  \nb: 2\n",
                "k",
                "text\n",
            ),
            (
                "a: 1\n     \nb: 2\n",
                |f| assert!(f.insert_after("a", "k", "x\ny")),
                "a: 1\nk: |-\n  x\n  y\n\nb: 2\n",
                "k",
                "x\ny",
            ),
            (
                "a: 1\n  \t\nb: 2\n",
                |f| assert!(f.insert_after("a", "k", "text\n")),
                "a: 1\nk: |\n  text\n\nb: 2\n",
                "k",
                "text\n",
            ),
            (
                "x:\n  a: 1\n       \n  b: 2\n",
                |f| {
                    assert!(f
                        .document()
                        .unwrap()
                        .get_mapping("x")
                        .unwrap()
                        .insert_after("a", "k", "text\n"))
                },
                "x:\n  a: 1\n  k: |\n    text\n\n  b: 2\n",
                "x.k",
                "text\n",
            ),
            (
                "a: 1\nb: 2\n     \nc: 3\n",
                |f| assert!(f.insert_before("b", "k", "text\n")),
                "a: 1\nk: |\n  text\nb: 2\n     \nc: 3\n",
                "k",
                "text\n",
            ),
            (
                "a: 1\n     \nb: 2\n",
                |f| assert!(f.set("a", "text\n")),
                "a: |\n  text\n\nb: 2\n",
                "a",
                "text\n",
            ),
            (
                "a: 1\n\nb: 2\n",
                |f| assert!(f.set("a", "text\n\n")),
                "a: |+\n  text\n\nb: 2\n",
                "a",
                "text\n\n",
            ),
            (
                "x:\n  a: 1\n       \ny: 2\n",
                |f| f.document().unwrap().try_set_path("x.a", "text\n").unwrap(),
                "x:\n  a: |\n    text\n\ny: 2\n",
                "x.a",
                "text\n",
            ),
            (
                "- 1\n     \n- 2\n",
                |f| {
                    assert!(f
                        .document()
                        .unwrap()
                        .as_sequence()
                        .unwrap()
                        .set(0, "text\n"))
                },
                "- |\n  text\n\n- 2\n",
                "[0]",
                "text\n",
            ),
            (
                "a: 1\n    # c\nb: 2\n",
                |f| assert!(f.insert_after("a", "k", "text\n")),
                "a: 1\nk: |\n  text\n# c\nb: 2\n",
                "k",
                "text\n",
            ),
            (
                "x:\n  a: 1\n   # c\n  b: 2\n",
                |f| {
                    assert!(f
                        .document()
                        .unwrap()
                        .get_mapping("x")
                        .unwrap()
                        .insert_after("a", "k", "text\n"))
                },
                "x:\n  a: 1\n  k: |\n    text\n   # c\n  b: 2\n",
                "x.k",
                "text\n",
            ),
            (
                "x:\n  a: 1\n    # c\n  b: 2\n",
                |f| {
                    assert!(f
                        .document()
                        .unwrap()
                        .get_mapping("x")
                        .unwrap()
                        .insert_after("a", "k", "text\n"))
                },
                "x:\n  a: 1\n  k: |\n    text\n  # c\n  b: 2\n",
                "x.k",
                "text\n",
            ),
            (
                "a: 1\n\n    # c\nb: 2\n",
                |f| assert!(f.insert_after("a", "k", "text\n")),
                "a: 1\nk: |\n  text\n\n# c\nb: 2\n",
                "k",
                "text\n",
            ),
            (
                "a: 1\n  \n    # c\nb: 2\n",
                |f| assert!(f.insert_after("a", "k", "text\n")),
                "a: 1\nk: |\n  text\n  \n# c\nb: 2\n",
                "k",
                "text\n",
            ),
            (
                "a: 1\n    # c\n    # d\nb: 2\n",
                |f| assert!(f.insert_after("a", "k", "text\n")),
                "a: 1\nk: |\n  text\n# c\n    # d\nb: 2\n",
                "k",
                "text\n",
            ),
            (
                "a: 1\n    # c\nb: 2\n",
                |f| assert!(f.insert_after("a", "k", "text\n\n")),
                "a: 1\nk: |+\n  text\n\n# c\nb: 2\n",
                "k",
                "text\n\n",
            ),
            (
                "a: 1\n    # c\nb: 2\n",
                |f| assert!(f.set("a", "text\n")),
                "a: |\n  text\n# c\nb: 2\n",
                "a",
                "text\n",
            ),
            (
                "a: 1\n  \n    # c\nb: 2\n",
                |f| assert!(f.set("a", "text\n")),
                "a: |\n  text\n  \n# c\nb: 2\n",
                "a",
                "text\n",
            ),
            (
                "- 1\n    # c\n- 2\n",
                |f| {
                    assert!(f
                        .document()
                        .unwrap()
                        .as_sequence()
                        .unwrap()
                        .set(0, "text\n"))
                },
                "- |\n  text\n# c\n- 2\n",
                "[0]",
                "text\n",
            ),
        ],
        check,
    );
    check_edits(
        &[
            (
                "a: 1\n     \nb: 2\n",
                |f| assert!(f.insert_after("a", "k", value_of("v: |\n  text\n"))),
                "a: 1\nk: |\n  text\n\nb: 2\n",
                "k",
                "text\n",
            ),
            (
                "a: 1\n\n    # c\nb: 2\n",
                |f| assert!(f.insert_after("a", "k", value_of("v: |\n  text\n"))),
                "a: 1\nk: |\n  text\n\n# c\nb: 2\n",
                "k",
                "text\n",
            ),
            (
                "a: 1\n ",
                |f| assert!(f.set("a", value_of("v:\n  x: |\n    t\n"))),
                "a:\n  x: |\n    t\n ",
                "a.x",
                "t\n",
            ),
        ],
        check_parser_shaped,
    );
}

#[test]
fn set_before_final_whitespace_line() {
    for (source, value, expected) in [
        ("a: 1\n ", "text\n", "a: |\n  text\n "),
        ("a: 1\n  ", "text\n", "a: |\n  text\n  "),
        ("a: 1\n     ", "text\n", "a: |\n  text\n"),
        ("a: 1\n\t", "text\n", "a: |\n  text\n"),
        ("a: 1\n\n ", "x\ny", "a: |-\n  x\n  y\n\n "),
        ("a: 1\n ", "text\n\n", "a: |+\n  text\n\n"),
    ] {
        let f = YamlFile::from_str(source).unwrap();
        f.set("a", value);
        check_parser_shaped(&f, expected, "a", value);
    }
}

#[test]
fn kept_block_scalar_goes_after_blank_lines() {
    check_edits(
        &[
            (
                "a: 1\n\nb: 2\n",
                |f| assert!(f.insert_after("a", "k", "text\n\n")),
                "a: 1\n\nk: |+\n  text\n\nb: 2\n",
                "k",
                "text\n\n",
            ),
            (
                "a: 1\n\n\n# c\nb: 2\n",
                |f| assert!(f.insert_after("a", "k", "text\n\n")),
                "a: 1\n\n\nk: |+\n  text\n\n# c\nb: 2\n",
                "k",
                "text\n\n",
            ),
            (
                "a: 1\n    \nb: 2\n",
                |f| assert!(f.insert_after("a", "k", "text\n\n")),
                "a: 1\n    \nk: |+\n  text\n\nb: 2\n",
                "k",
                "text\n\n",
            ),
            (
                "a: 1\n\nb: 2\n",
                |f| assert!(f.insert_after("a", "k", ScalarValue::folded("text\n\n"))),
                "a: 1\n\nk: >+\n  text\n\nb: 2\n",
                "k",
                "text\n\n",
            ),
            (
                "a: 1\n\nb: 2\n",
                |f| {
                    assert!(f.insert_after(
                        "a",
                        "k",
                        ScalarValue::string("text\n\n").with_tag("!t")
                    ))
                },
                "a: 1\n\nk: !t |+\n  text\n\nb: 2\n",
                "k",
                "text\n\n",
            ),
            (
                "a: 1\n\nb: 2\n",
                |f| assert!(f.insert_after("a", "k", node_at("s: |+\n  text\n\n", "s"))),
                "a: 1\n\nk: |+\n  text\n\nb: 2\n",
                "k",
                "text\n\n",
            ),
            (
                "a: 1\n\nb: 2\n",
                |f| assert!(f.insert_after("a", "k", node_at("m:\n  s: |+\n    text\n\n", "m"))),
                "a: 1\n\nk:\n  s: |+\n    text\n\nb: 2\n",
                "k.s",
                "text\n\n",
            ),
            (
                "x:\n  a: 1\n\n  b: 2\n",
                |f| {
                    assert!(f
                        .document()
                        .unwrap()
                        .get_mapping("x")
                        .unwrap()
                        .insert_after("a", "k", "text\n\n"))
                },
                "x:\n  a: 1\n\n  k: |+\n    text\n\n  b: 2\n",
                "x.k",
                "text\n\n",
            ),
            (
                "? a\n: 1\n\n? b\n: 2\n",
                |f| assert!(f.insert_after("a", "k", "text\n\n")),
                "? a\n: 1\n\n? k\n: |+\n  text\n\n? b\n: 2\n",
                "k",
                "text\n\n",
            ),
            (
                "a: 1\n\nb: 2\nk: 3\n",
                |f| assert!(f.move_after("a", "k", "text\n\n")),
                "a: 1\n\nk: |+\n  text\n\nb: 2\n",
                "k",
                "text\n\n",
            ),
            (
                "a: 1\n\nb: 2\n",
                |f| {
                    assert!(f.document().unwrap().set_with_field_order(
                        "k",
                        "text\n\n",
                        ["a", "k", "b"]
                    ))
                },
                "a: 1\n\nk: |+\n  text\n\nb: 2\n",
                "k",
                "text\n\n",
            ),
            (
                "a: 1\n\nb: 2\n",
                |f| assert!(f.insert_before("b", "k", "text\n\n")),
                "a: 1\n\nk: |+\n  text\n\nb: 2\n",
                "k",
                "text\n\n",
            ),
            (
                "a: 1\n\nb: 2\n",
                |f| assert!(f.insert_at_index(1, "k", "text\n\n")),
                "a: 1\n\nk: |+\n  text\n\nb: 2\n",
                "k",
                "text\n\n",
            ),
            (
                "a: 1\n\n",
                |f| assert!(f.set("k", "text\n\n")),
                "a: 1\n\nk: |+\n  text\n\n",
                "k",
                "text\n\n",
            ),
            // A clipped scalar takes in no blank line, so stays before it.
            (
                "a: 1\n\nb: 2\n",
                |f| assert!(f.insert_after("a", "k", "text\n")),
                "a: 1\nk: |\n  text\n\nb: 2\n",
                "k",
                "text\n",
            ),
            (
                "- 1\n\n",
                |f| {
                    f.document()
                        .unwrap()
                        .as_sequence()
                        .unwrap()
                        .push("text\n\n")
                },
                "- 1\n\n- |+\n  text\n\n",
                "[1]",
                "text\n\n",
            ),
            (
                "x:\n  - 1\n\ny: 2\n",
                |f| {
                    f.document()
                        .unwrap()
                        .get_sequence("x")
                        .unwrap()
                        .push("text\n\n")
                },
                "x:\n  - 1\n\n  - |+\n    text\n\ny: 2\n",
                "x[1]",
                "text\n\n",
            ),
            (
                "x:\n  - 1\n\ny: 2\n",
                |f| {
                    f.document()
                        .unwrap()
                        .get_sequence("x")
                        .unwrap()
                        .push("text\n")
                },
                "x:\n  - 1\n  - |\n    text\n\ny: 2\n",
                "x[1]",
                "text\n",
            ),
            (
                "- 1\n\n- 2\n",
                |f| {
                    f.document()
                        .unwrap()
                        .as_sequence()
                        .unwrap()
                        .insert(1, "text\n\n")
                },
                "- 1\n\n- |+\n  text\n\n- 2\n",
                "[1]",
                "text\n\n",
            ),
            (
                "x:\n  a: 1\n\ny: 2\n",
                |f| {
                    f.document()
                        .unwrap()
                        .try_set_path("x.k", "text\n\n")
                        .unwrap()
                },
                "x:\n  a: 1\n\n  k: |+\n    text\n\ny: 2\n",
                "x.k",
                "text\n\n",
            ),
            (
                "a: 1\n\nb: 2\n",
                |f| {
                    assert!(f.insert_after("a", "x", Sequence::new_pending_block()));
                    f.document()
                        .unwrap()
                        .get_sequence("x")
                        .unwrap()
                        .push("text\n\n");
                },
                "a: 1\nx:\n\n  - |+\n    text\n\nb: 2\n",
                "x[0]",
                "text\n\n",
            ),
            (
                "- a: 1\n\n- 2\n",
                |f| {
                    let item = f.document().unwrap().as_sequence().unwrap().get(0).unwrap();
                    item.as_mapping().unwrap().set("k", "text\n\n");
                },
                "- a: 1\n\n  k: |+\n    text\n\n- 2\n",
                "[0].k",
                "text\n\n",
            ),
        ],
        check,
    );
}

/// Strings that are edge cases for block scalars.
const CASES: &[&str] = &[
    "",
    "\n",
    "\n\n",
    "\n\n\n",
    " ",
    "\t",
    " x",
    "  x\n",
    "x\n ",
    "x \n",
    "x  ",
    "x\n\n",
    "x\n\n\n",
    "\nx",
    "\n\nx\n",
    "\n x",
    "\n \n",
    "  \n",
    " \nx",
    "x\n \n",
    "x\n  y\n",
    "# c\n",
    "#",
    "x\n# c\n",
    "x\n#",
    "x\r\ny\r\n",
    "x\ry",
    "\r",
    "\u{7}\n",
    "x\u{85}y\n",
    "\u{feff}x\n",
    "x\u{2028}y",
    "\u{e9}\n",
    " a\nb\n",
    "a\n b\nc\n",
    "a\n\n b\n\nc",
    "a\n\nb",
    "a\n\n\nb\n",
    "a b\nc d\n",
    "a \nb",
    "a\n \nb",
    "x\n\n y",
    "\ta\nb",
    "a\n\tb\n",
    "\t\n",
];

fn set_root(value: ScalarValue) -> YamlFile {
    let file = YamlFile::from_str("k: v\nz: 1\n").unwrap();
    file.document()
        .unwrap()
        .as_mapping()
        .unwrap()
        .set("k", value);
    file
}

fn set_nested(value: ScalarValue) -> YamlFile {
    let file = YamlFile::from_str("a:\n  k: v\n  z: 1\n").unwrap();
    file.document()
        .unwrap()
        .get_mapping("a")
        .unwrap()
        .set("k", value);
    file
}

fn set_seq(value: ScalarValue) -> YamlFile {
    let file = YamlFile::from_str("- v\n- 1\n").unwrap();
    file.document()
        .unwrap()
        .as_sequence()
        .unwrap()
        .set(0, value);
    file
}

type Setter = fn(ScalarValue) -> YamlFile;

const PLACES: &[(&str, Setter, &str)] = &[
    ("root", set_root, "k"),
    ("nested", set_nested, "a.k"),
    ("sequence", set_seq, "[0]"),
];

#[test]
fn block_scalar_edge_cases_round_trip() {
    for &value in CASES {
        for (style, scalar) in [
            ("literal", ScalarValue::literal(value)),
            ("folded", ScalarValue::folded(value)),
            ("string", ScalarValue::string(value)),
        ] {
            for &(place, set, path) in PLACES {
                let file = set(scalar.clone());
                assert_file_cst_ok(&file);
                let text = file.to_string();
                let reparsed = YamlFile::from_str(&text)
                    .unwrap_or_else(|e| panic!("{style} {value:?} at {place}: {text:?}: {e}"));
                let read = reparsed.document().unwrap().try_get_path(path).unwrap();
                assert_eq!(
                    read.as_scalar().unwrap().as_string(),
                    value,
                    "{style} {value:?} at {place}: {text:?}"
                );
                assert_eq!(
                    Validator::new().validate(&reparsed.document().unwrap()),
                    vec![],
                    "{style} {value:?} at {place}: {text:?}"
                );
            }
        }
    }
}

#[test]
fn written_block_scalar_style() {
    for (file, expected) in [
        // Clipping keeps the break after the last content line, and a value
        // of only line breaks has none.
        (set_root(ScalarValue::literal("\n")), "k: |+\n\nz: 1\n"),
        (set_root(ScalarValue::folded("\n")), "k: >+\n\nz: 1\n"),
        (
            set_nested(ScalarValue::literal("\n")),
            "a:\n  k: |+\n\n  z: 1\n",
        ),
        (set_seq(ScalarValue::literal("\n")), "- |+\n\n- 1\n"),
        (set_root(ScalarValue::string("\n")), "k: |+\n\nz: 1\n"),
        // An empty block scalar has no body.
        (set_root(ScalarValue::literal("")), "k: |-\nz: 1\n"),
        (set_root(ScalarValue::folded("")), "k: >-\nz: 1\n"),
        // A block scalar reads a carriage return as a line break, and
        // `\r\n` as a single `\n`, so these are double-quoted.
        (
            set_root(ScalarValue::literal("x\r\ny\r\n")),
            "k: \"x\\r\\ny\\r\\n\"\nz: 1\n",
        ),
        (
            set_root(ScalarValue::folded("x\ry")),
            "k: \"x\\ry\"\nz: 1\n",
        ),
        // As are unprintable characters.
        (
            set_root(ScalarValue::literal("\u{7}\n")),
            "k: \"\\a\\n\"\nz: 1\n",
        ),
        (
            set_root(ScalarValue::folded("\u{feff}x\n")),
            "k: \"\\uFEFFx\\n\"\nz: 1\n",
        ),
    ] {
        assert_eq!(file.to_string(), expected);
    }
}

/// Set `k` in `k: 0\n` to `value`, and return the text and the value read
/// back from it.
fn set_and_reread(value: &str) -> (String, String) {
    let file = YamlFile::from_str("k: 0\n").unwrap();
    file.document()
        .unwrap()
        .as_mapping()
        .unwrap()
        .set("k", value);
    let text = file.to_string();
    let read = scalar_string(&node_at(&text, "k"));
    (text, read)
}

#[test]
fn set_value_needing_indentation_indicator_or_quotes() {
    for (value, expected) in [
        ("  x\n y", "k: |2-\n    x\n   y\n"),
        ("  a\nb", "k: |2-\n    a\n  b\n"),
        ("a\n ", "k: \"a\\n \"\n"),
        ("a\n   \nb", "k: \"a\\n   \\nb\"\n"),
        (" \n", "k: \" \\n\"\n"),
    ] {
        assert_eq!(
            set_and_reread(value),
            (expected.to_string(), value.to_string())
        );
    }

    let file = YamlFile::from_str("o:\n  k: v\n").unwrap();
    file.document()
        .unwrap()
        .get_mapping("o")
        .unwrap()
        .set("k", "  x\n  y");
    let output = file.to_string();
    assert_eq!(output, "o:\n  k: |2-\n      x\n      y\n");
    assert_leaves(&output, &[("o.k", "  x\n  y")]);
}
