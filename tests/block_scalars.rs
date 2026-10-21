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
use common::{
    assert_edited, assert_file_cst_ok, assert_file_edited, assert_parser_shaped, parse_clean,
    render_documents,
};
use rowan::ast::AstNode;
use std::str::FromStr;
use yaml_edit::path::YamlPath;
use yaml_edit::validator::Validator;
use yaml_edit::{debug, yaml_eq, Document, ScalarValue, Sequence, SyntaxKind, YamlFile, YamlNode};

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

/// Check [`assert_leaves`], and that `yaml` parses with a valid CST and no
/// ERROR node.
#[track_caller]
fn assert_clean_leaves(yaml: &str, expected: &[(&str, &str)]) {
    parse_clean(yaml);
    assert_leaves(yaml, expected);
}

/// Where a block scalar's body starts and ends. Each of these used to
/// strand part of the input in an ERROR node, or swallow a following entry,
/// while from_str still reported success.
#[test]
fn block_scalar_body_bounds() {
    let cases: &[(&str, Leaves)] = &[
        // A block scalar that is the whole document may hold body lines at
        // column 0, and takes its base indent from the first of them, as
        // saphyr reads it. The base was only recorded from an INDENT token,
        // so a body starting at column 0 left it unset; where a later line
        // was indented it set the base instead.
        ("|-\nx\ny\n", &[("", "x\ny")]),
        ("|-\n x\n y\n", &[("", "x\ny")]),
        ("|-\n  x\n  y\n", &[("", "x\ny")]),
        (
            "|-\n?  >\n ems+\nco+ors:\n  - &.\n",
            &[("", "?  >\n ems+\nco+ors:\n  - &.")],
        ),
        // Nested under a key or in a sequence entry, the body still has to
        // be indented and a dedented sibling survives.
        ("a: |\n  x\nb: 1\n", &[("a", "x\n"), ("b", "1")]),
        ("a: |-\n  x\n  y\nb: 1\n", &[("a", "x\ny"), ("b", "1")]),
        ("- |-\n  x\n- y\n", &[("0", "x"), ("1", "y")]),
        ("k: |\n  a\nj: 1\n", &[("k", "a\n"), ("j", "1")]),
        // With no indented body the scalar is empty, and the next line at
        // the key's column is a sibling, as both saphyr and PyYAML read it.
        // The header consumes its own line break, so the content loop
        // started at a line start while believing it was mid-line and the
        // entry after the scalar was swallowed.
        ("empty: |\nnext: 1\n", &[("empty", ""), ("next", "1")]),
        ("empty: >\nnext: 1\n", &[("empty", ""), ("next", "1")]),
        ("a: |\nb: |\nc: 1\n", &[("a", ""), ("b", ""), ("c", "1")]),
        ("- |\n- x\n", &[("0", ""), ("1", "x")]),
        ("literal: |\n", &[("literal", "")]),
        ("folded: >\n", &[("folded", "")]),
        ("strip: |-\n", &[("strip", "")]),
        ("keep: |+\n", &[("keep", "")]),
        // A header on its own line is not the document's own node, so its
        // body is not read from column 0, which swallowed `z: 1`.
        ("k:\n  |\nz: 1\n", &[("k", ""), ("z", "1")]),
        // An indented `- x` is still body text.
        ("k: |\n  - x\n  - y\n", &[("k", "- x\n- y\n")]),
        // A whitespace-only line belongs to the body, however short it is.
        // The dedent check compared its INDENT against the body's base, so
        // the scalar ended there; this accounted for five of the YAML test
        // suite's valid documents (93WF, K527, MJS9, H2RW, R4YG).
        ("k: |\n  a\n \n  b\n", &[("k", "a\n\nb\n")]),
        ("k: |\n  a\n\n  b\n", &[("k", "a\n\nb\n")]),
        (
            "k: >-\n  trimmed\n  \n \n\n  as\n  space\n",
            &[("k", "trimmed\n\n\nas space")],
        ),
        // Nor does it set the base indent: it may be indented further than
        // the body that follows it (suite case R4YG).
        ("- >\n \t\n detected\n", &[("0", "\t\ndetected\n")]),
        // An explicit indentation indicator states the body's column
        // outright, so the first body line does not get to set it (suite
        // case F6MC).
        ("a: >2\n   more\n  regular\n", &[("a", " more\nregular\n")]),
        (
            "b: >2\n\n\n   more\n  regular\n",
            &[("b", "\n\n more\nregular\n")],
        ),
        (
            "a: >2\n   more\n  regular\nb: 1\n",
            &[("a", " more\nregular\n"), ("b", "1")],
        ),
        // A flow collection after the scalar has ended is still one.
        (
            "a: |\n  x\nb: [1, 2]\nc: {d: 1}\n",
            &[("a", "x\n"), ("b.0", "1"), ("b.1", "2"), ("c.d", "1")],
        ),
        (
            "flow: [a, b, c]\nblock: |\n  Content here\n",
            &[
                ("flow.0", "a"),
                ("flow.1", "b"),
                ("flow.2", "c"),
                ("block", "Content here\n"),
            ],
        ),
        ("k: |\n  x\nk2: >\n  y\n", &[("k", "x\n"), ("k2", "y\n")]),
        // A comment line less indented than the body ends it, even at
        // column 0 (the spec's l-trail-comments, as in the YAML test
        // suite's 7T8X).
        ("k: |\n  a\n# c\nz: 1\n", &[("k", "a\n"), ("z", "1")]),
        (
            "a: >\n line\n# Comment\nb: 1\n",
            &[("a", "line\n"), ("b", "1")],
        ),
        (
            "a: |\n  line\n\n# Comment\nb: 1\n",
            &[("a", "line\n"), ("b", "1")],
        ),
    ];
    for (yaml, expected) in cases {
        assert_clean_leaves(yaml, expected);
    }

    // A leading blank line with more spaces than the first content line is
    // an error the validator reports; the parser keeps the extra spaces.
    let yaml = "- >\n   \n a\n";
    assert_clean_leaves(yaml, &[("0", "  \na\n")]);
    common::parse_with_violations(
        yaml,
        &[(
            "Block scalar content under-indented (1 spaces) relative to preceding blank line \
             (3 spaces)",
            "3:1",
        )],
    );
}

/// A header's indentation and chomping indicators survive however they
/// lex, and a kept block keeps its trailing newlines.
#[test]
fn block_scalar_header_forms() {
    let cases: &[(&str, Leaves)] = &[
        ("k: |2+\n  x\n", &[("k", "x\n")]),
        ("k: |+2\n  x\n", &[("k", "x\n")]),
        ("k: |-2\n  x\n", &[("k", "x")]),
        ("k: |10\n          Deep\n", &[("k", "         Deep\n")]),
        ("k: |-\n  x\n", &[("k", "x")]),
        ("k: |2-\n  x\n", &[("k", "x")]),
        ("k: >+\n  x\n\n", &[("k", "x\n\n")]),
        ("k: |+\n  a\n\n", &[("k", "a\n\n")]),
        ("k: >+\n  a\n\n", &[("k", "a\n\n")]),
        ("k: |2+\n  a\n\n", &[("k", "a\n\n")]),
        // A keep-chomped block whose body is nothing but blank lines has no
        // content line to terminate, so it keeps only the blanks, as saphyr
        // and PyYAML both read it. The decoder always added one newline for
        // a content line, so it came back one newline too long.
        ("k: |+\n\n\n", &[("k", "\n\n")]),
        ("k: |+\n\n\n\n", &[("k", "\n\n\n")]),
        ("k: |+\n  x\n\n\n", &[("k", "x\n\n\n")]),
        ("k: |+\n  x\n", &[("k", "x\n")]),
        ("k: |\n  x\n", &[("k", "x\n")]),
    ];
    for (yaml, expected) in cases {
        assert_clean_leaves(yaml, expected);
    }
}

/// A block-scalar header's indicators are ASCII, so it must be matched by
/// character rather than by byte.
///
/// `">\u{a0}"` panicked: the no-break space is two bytes, so the two-byte
/// arm split it at byte 1, inside the character. from_str is safe to call on
/// untrusted input, so it has to report rather than abort.
#[test]
fn block_scalar_header_does_not_panic_on_a_multibyte_char() {
    for yaml in [
        ">\u{a0}",
        "|\u{a0}",
        "k: >\u{a0}\n",
        "|\u{2028}",
        ">\u{3000}",
    ] {
        // Parsing must not panic; either outcome is acceptable.
        let _ = YamlFile::from_str(yaml);
    }
}

/// Indicators, quotes and line breaks in a block scalar's body are literal
/// text.
#[test]
fn block_scalar_body_is_literal() {
    let cases: &[(&str, Leaves)] = &[
        // An unmatched `{` raised the lexer's flow depth for the rest of
        // the file, so every later `,` lexed as a delimiter and both
        // following entries were lost to an ERROR node. saphyr keeps all
        // three.
        (
            "a: |\n {\nb,c: 1\nd: 2\n",
            &[("a", "{\n"), ("b,c", "1"), ("d", "2")],
        ),
        ("a: |\n [\nb,c: 1\n", &[("a", "[\n"), ("b,c", "1")]),
        ("a: >\n {\nb,c: 1\n", &[("a", "{\n"), ("b,c", "1")]),
        ("a: |\n {\n\nb,c: 1\n", &[("a", "{\n"), ("b,c", "1")]),
        // A quote opening a body line does not start a quoted scalar: an
        // unbalanced one ran across line breaks to the next matching quote,
        // swallowing every following key.
        (
            "a: |\n  'tis the season\nb: c's\n",
            &[("a", "'tis the season\n"), ("b", "c's")],
        ),
        ("a: |\n  \"x\nb: 1\n", &[("a", "\"x\n"), ("b", "1")]),
        (
            "a: |\n  \"x\n  y\nb: [1, 2]\n",
            &[("a", "\"x\ny\n"), ("b.0", "1"), ("b.1", "2")],
        ),
        ("- >\n  'x\n- y\n", &[("0", "'x\n"), ("1", "y")]),
    ];
    for (yaml, expected) in cases {
        assert_clean_leaves(yaml, expected);
    }

    // Later documents included.
    let yaml = "--- |\n\"open\n---\nx\n";
    let file = YamlFile::from_str(yaml).unwrap();
    assert_eq!(file.to_string(), yaml);
    let docs: Vec<String> = file
        .documents()
        .map(|d| d.as_scalar().unwrap().as_string())
        .collect();
    assert_eq!(docs, vec!["\"open\n", "x"]);

    // Every line of the body at the body's column is body, not just the
    // first: a `{` on the second line must not open a flow collection
    // whose depth then leaks into the rest of the file.
    let tokens = yaml_edit::lex("a: |\n  x\n  {y\nb: c,d\n");
    assert!(
        !tokens
            .iter()
            .any(|(kind, _)| matches!(kind, SyntaxKind::LEFT_BRACE | SyntaxKind::COMMA)),
        "{tokens:?}"
    );
    let tokens = yaml_edit::lex("a: |\n  x\n  # y\n");
    assert!(
        !tokens.iter().any(|(kind, _)| *kind == SyntaxKind::COMMENT),
        "{tokens:?}"
    );
}

/// A `#` inside a block scalar's body is literal text; one on a line that
/// has left the body is a comment again.
///
/// The body's own column, set by its first content line, is what decides:
/// 4QFQ's `- >\n \n  \n  # detected\n` holds the content `# detected` at the
/// body's indent, while T26H's `# Comment` sits left of it and really is a
/// comment. Measuring against the header's indent alone could not tell them
/// apart.
#[test]
fn hash_inside_a_block_scalar_body_is_content() {
    for (yaml, comments) in [
        ("- >\n \n  \n  # detected\n", 0),
        ("a: |\n  # in body\n", 0),
        ("--- |\n  text\n\n # Comment\n", 1),
        ("k: v # real\n", 1),
    ] {
        let file = YamlFile::from_str(yaml).unwrap();
        assert_eq!(
            common::count_tokens(&file, SyntaxKind::COMMENT),
            comments,
            "{yaml:?}"
        );
    }
}

/// A comment line after a block scalar is a line of the mapping, wherever
/// it starts: at column 0 it is not a trailing comment of the value.
#[test]
fn block_scalar_followed_by_comment_line_at_column_zero() {
    for (yaml, entry) in [
        ("k: |\n  a\n# c\nz: 1\n", "k: |\n  a\n"),
        ("k: |\n  a\n\n# c\nz: 1\n", "k: |\n  a\n\n"),
        ("k: >  # h\n  a\n# c\n", "k: >  # h\n  a\n"),
    ] {
        let file = YamlFile::from_str(yaml).unwrap();
        assert_eq!(file.to_string(), yaml);
        let mapping = file.document().unwrap().as_mapping().unwrap();
        let first = mapping.entries().next().unwrap();
        assert_eq!(first.syntax().to_string(), entry, "{yaml:?}");
        let comments: Vec<_> = mapping
            .syntax()
            .children_with_tokens()
            .filter(|c| c.kind() == SyntaxKind::COMMENT)
            .map(|c| c.to_string())
            .collect();
        assert_eq!(comments, vec!["# c".to_string()], "{yaml:?}");
    }
}

/// A block scalar's body has to be indented past the mapping the scalar is
/// a value of, not just past the header's line. In `- a: |+\n\n  b: !t ''\n`
/// the lexer took the `b` line for body text and read `!t ''` as literal
/// content, while the parser made it the mapping's next entry.
#[test]
fn block_scalar_body_past_a_compact_mapping() {
    for (yaml, expected) in [
        ("- a: |+\n\n  b: !t ''\n", "!t ''"),
        ("- a: |\n  b: [1]\n", "[1]"),
    ] {
        let doc = Document::from_str(yaml).unwrap();
        let item = doc.as_sequence().unwrap().get(0).unwrap();
        let b = item.as_mapping().unwrap().get("b").unwrap();
        assert_eq!(b.to_string(), expected, "{yaml:?}");
        assert!(b.as_scalar().is_none(), "{yaml:?}");
    }
}

/// A document's root block scalar may put its body at column 0, since the
/// root sits at indentation -1. Such a body is literal text up to the next
/// document marker (test suite FP8R).
///
/// The lexer took the column-0 lines as having left the body, so
/// `%!PS-Adobe-2.0` became a DIRECTIVE (making the validator reject the
/// valid suite case W4TN) and `{` opened a flow collection that swallowed the
/// following document.
#[test]
fn root_block_scalar_body_at_column_zero() {
    for (yaml, values) in [
        ("--- |\n%!PS-Adobe-2.0\n", vec!["%!PS-Adobe-2.0\n"]),
        ("|\n%a\n%b\n", vec!["%a\n%b\n"]),
        ("--- |\n# x\n", vec!["# x\n"]),
        ("--- !!str |\n%a\n", vec!["%a\n"]),
        ("--- |\n{x\n--- [y]\n", vec!["{x\n"]),
        (
            "--- |\n%!PS-Adobe-2.0\n...\n%YAML 1.2\n---\n# Empty\n...\n",
            vec!["%!PS-Adobe-2.0\n"],
        ),
        ("--- >\nline1\nline2\n", vec!["line1 line2\n"]),
    ] {
        assert!(
            !yaml_edit::lex(yaml)
                .iter()
                .skip(1)
                .any(|(kind, text)| *kind == SyntaxKind::DIRECTIVE && *text != "%YAML 1.2"),
            "{yaml:?}: {:?}",
            yaml_edit::lex(yaml)
        );
        let file = YamlFile::from_str(yaml).unwrap();
        assert_eq!(file.to_string(), yaml);
        let scalars: Vec<String> = file
            .documents()
            .filter_map(|d| d.as_scalar().map(|s| s.as_string()))
            .collect();
        assert_eq!(scalars, values, "{yaml:?}");
        assert_eq!(common::violations(&file), vec![], "{yaml:?}");
    }

    let yaml = "--- |\n{x\n---\na: b,c\n";
    let file = YamlFile::from_str(yaml).unwrap();
    assert_eq!(file.to_string(), yaml);
    let docs: Vec<_> = file.documents().collect();
    assert_eq!(docs.len(), 2);
    assert_eq!(docs[0].as_scalar().unwrap().as_string(), "{x\n");
    assert_eq!(
        docs[1]
            .as_mapping()
            .unwrap()
            .get("a")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "b,c"
    );

    // The document's mapping is not reached through it.
    let doc = Document::from_str("--- >\nline1\nline2\n").unwrap();
    assert!(doc.as_mapping().is_none());
}

/// A block scalar's body ends at the first line that dedents out of it,
/// whatever the line ending. With CRLF the lexer kept treating later lines
/// as body text, so a flow collection after a block scalar was read as
/// literal content and never closed.
///
/// The second input is the YAML test suite's 6HB6. The suite runner only
/// sees it if `test-data/` has been cloned, and reads it however git
/// checked it out, so on Linux it never exercises the CRLF path that broke
/// here.
#[test]
fn block_scalar_ends_at_a_dedent_with_any_line_ending() {
    let suite_6hb6 = concat!(
        "  # Leading comment line spaces are\n",
        "   # neither content nor indentation.\n",
        "    \n",
        "Not indented:\n",
        " By one space: |\n",
        "    By four\n",
        "      spaces\n",
        " Flow style: [    # Leading spaces\n",
        "   By two,        # in flow style\n",
        "  Also by two,    # are neither\n",
        "  \tStill by two   # content nor\n",
        "    ]             # indentation.\n",
    );
    let cases: &[(&str, Leaves)] = &[
        (
            "Not indented:\r\n By one space: |\r\n    By four\r\n Flow style: [\r\n   a,\r\n  ]\r\n",
            &[
                ("Not indented.By one space", "By four\n"),
                ("Not indented.Flow style.0", "a"),
            ],
        ),
        (
            suite_6hb6,
            &[
                ("Not indented.By one space", "By four\n  spaces\n"),
                ("Not indented.Flow style.0", "By two"),
                ("Not indented.Flow style.1", "Also by two"),
                ("Not indented.Flow style.2", "Still by two"),
            ],
        ),
    ];
    for (yaml, expected) in cases {
        let lf = yaml.replace("\r\n", "\n");
        for source in [lf.clone(), lf.replace('\n', "\r\n")] {
            assert_clean_leaves(&source, expected);
        }
    }
}

/// Literal and folded scalars with their chomping and indentation
/// indicators, among other nodes, round trip and read as expected.
#[test]
fn block_scalar_values() {
    let cases: &[(&str, Leaves)] = &[
        (
            r#"literal: |
  Line 1
  Line 2
  Line 3
"#,
            &[("literal", "Line 1\nLine 2\nLine 3\n")],
        ),
        (
            r#"folded: >
  This is a very long line that will be folded
  into a single line in the output
  but preserves paragraph breaks.

  This is a new paragraph.
"#,
            &[("folded", "This is a very long line that will be folded into a single line in the output but preserves paragraph breaks.\nThis is a new paragraph.\n")],
        ),
        (
            r#"strip: |-
  Line 1
  Line 2

"#,
            &[("strip", "Line 1\nLine 2")],
        ),
        (
            r#"keep: |+
  Line 1
  Line 2

"#,
            &[("keep", "Line 1\nLine 2\n\n")],
        ),
        (
            r#"strip: >-
  Folded content that should
  be stripped of final newlines
"#,
            &[("strip", "Folded content that should be stripped of final newlines")],
        ),
        (
            r#"keep: >+
  Folded content that should
  keep all final newlines

"#,
            &[("keep", "Folded content that should keep all final newlines\n\n")],
        ),
        // `|2` puts the content at column 2, so the two further spaces on each
        // line are part of the value, as saphyr and PyYAML both read it.
        (
            r#"explicit: |2
    Two space indent
    Another line
"#,
            &[("explicit", "  Two space indent\n  Another line\n")],
        ),
        // `>3` puts the content at column 3, so the three further spaces are
        // part of the value, and a more-indented line in a folded scalar keeps
        // its break rather than folding.
        (
            r#"folded_explicit: >3
      Three space indent
      Another folded line
"#,
            &[("folded_explicit", "   Three space indent\n   Another folded line\n")],
        ),
        (
            r#"description: |
  This is a multi-line
  description that should
  preserve line breaks.

  It can have multiple paragraphs too.

summary: >
  This is a summary that
  should be folded into
  a single line.

version: "1.0"
"#,
            &[("description", "This is a multi-line\ndescription that should\npreserve line breaks.\n\nIt can have multiple paragraphs too.\n"), ("summary", "This is a summary that should be folded into a single line.\n"), ("version", "1.0")],
        ),
        (
            r#"config:
  name: "My App"
  description: |
    This application does many things:
    - Feature 1
    - Feature 2
    - Feature 3
  summary: >
    A brief summary that spans
    multiple lines but should
    be folded together.
  version: 1.0
  enabled: true
"#,
            &[("config.name", "My App"), ("config.description", "This application does many things:\n- Feature 1\n- Feature 2\n- Feature 3\n"), ("config.summary", "A brief summary that spans multiple lines but should be folded together.\n"), ("config.version", "1.0"), ("config.enabled", "true")],
        ),
        // A block scalar with no indented content is empty, and the next line
        // at the key's own column is a sibling entry rather than its content,
        // as saphyr and PyYAML read it.
        (
            r#"empty_literal: |
empty_folded: >
"#,
            &[("empty_literal", ""), ("empty_folded", "")],
        ),
        (
            r#"whitespace: |


"#,
            &[("whitespace", "")],
        ),
        (
            r#"first: |
  Content
second: value
"#,
            &[("first", "Content\n"), ("second", "value")],
        ),
        (
            r#"poem: |
  Roses are red,
  Violets are blue,
  YAML is great,
  And so are you!

  This is another stanza
  with different content.
    And this line has extra indentation.
  Back to normal indentation.

  Final stanza.
"#,
            &[("poem", "Roses are red,\nViolets are blue,\nYAML is great,\nAnd so are you!\n\nThis is another stanza\nwith different content.\n  And this line has extra indentation.\nBack to normal indentation.\n\nFinal stanza.\n")],
        ),
        (
            r#"description: >
  This is the first paragraph that should
  be folded into a single line when processed
  by a YAML parser.

  This is a second paragraph that should
  also be folded but kept separate from
  the first paragraph.


  This is a third paragraph after
  multiple blank lines.

  Final paragraph.
"#,
            &[("description", "This is the first paragraph that should be folded into a single line when processed by a YAML parser.\nThis is a second paragraph that should also be folded but kept separate from the first paragraph.\n\nThis is a third paragraph after multiple blank lines.\nFinal paragraph.\n")],
        ),
        (
            r#"special_chars: |
  Line with colons: key: value
  Line with dashes - and more - dashes
  Line with quotes "double" and 'single'
  Line with brackets [array] and braces {object}
  Line with pipes | and greater than >
  Line with at @ and hash # symbols
  Line with percent % and exclamation !

backslash_test: >
  This line has a backslash \ in it
  And this line has multiple \\ backslashes

unicode_test: |
  This line has unicode: 你好世界
  And emojis: 🚀 🎉 ✨
"#,
            &[("special_chars", "Line with colons: key: value\nLine with dashes - and more - dashes\nLine with quotes \"double\" and 'single'\nLine with brackets [array] and braces {object}\nLine with pipes | and greater than >\nLine with at @ and hash # symbols\nLine with percent % and exclamation !\n"), ("backslash_test", "This line has a backslash \\ in it And this line has multiple \\\\ backslashes\n"), ("unicode_test", "This line has unicode: 你好世界\nAnd emojis: 🚀 🎉 ✨\n")],
        ),
        (
            r#"clip: |
  Line 1
  Line 2

"#,
            &[("clip", "Line 1\nLine 2\n")],
        ),
        (
            r#"strip: |-
  Line 1
  Line 2



"#,
            &[("strip", "Line 1\nLine 2")],
        ),
        (
            r#"keep: |+
  Line 1
  Line 2



"#,
            &[("keep", "Line 1\nLine 2\n\n\n\n")],
        ),
        (
            r#"indent1: |1
 Single space indent
"#,
            &[("indent1", "Single space indent\n")],
        ),
        (
            r#"indent2: |2
  Two space indent
"#,
            &[("indent2", "Two space indent\n")],
        ),
        (
            r#"folded_indent: >2
  Two space folded
  content spans lines
"#,
            &[("folded_indent", "Two space folded content spans lines\n")],
        ),
        (
            r#"strip_with_indent: |2-
  Content with explicit indent
  and strip chomping


keep_with_indent: >3+
   Content with explicit indent
   and keep chomping



folded_strip: >-
  Folded content
  with strip indicator

literal_keep: |+
  Literal content
  with keep indicator


"#,
            &[("strip_with_indent", "Content with explicit indent\nand strip chomping"), ("keep_with_indent", "Content with explicit indent and keep chomping\n\n\n\n"), ("folded_strip", "Folded content with strip indicator"), ("literal_keep", "Literal content\nwith keep indicator\n\n\n")],
        ),
        (
            r#"whitespace_only: |



"#,
            &[("whitespace_only", "")],
        ),
        (
            r#"mixed_indent: |
  Normal line
    Indented line
  Back to normal
      More indented
  Normal again
"#,
            &[("mixed_indent", "Normal line\n  Indented line\nBack to normal\n    More indented\nNormal again\n")],
        ),
        (
            r#"first: |
  Content
immediate: value
another: |
  More content
final: end
"#,
            &[("first", "Content\n"), ("immediate", "value"), ("another", "More content\n"), ("final", "end")],
        ),
        // The config block scalar ends at the less-indented "# Outside comment",
        // and a `#` inside a body is content.
        (
            r#"# Main configuration
config: |  # This is a literal block
  # This comment is inside the block
  line1: value1
  # Another internal comment
  line2: value2

# Outside comment
other: >  # Folded block comment
  This content spans
  # This hash is part of the content, not a comment
  multiple lines
"#,
            &[("config", "# This comment is inside the block\nline1: value1\n# Another internal comment\nline2: value2\n"), ("other", "This content spans # This hash is part of the content, not a comment multiple lines\n")],
        ),
        (
            r#"empty_literal: |

empty_folded: >

minimal_literal: |
  x

minimal_folded: >
  y

just_newlines: |



just_spaces: |



"#,
            &[("empty_literal", ""), ("empty_folded", ""), ("minimal_literal", "x\n"), ("minimal_folded", "y\n"), ("just_newlines", ""), ("just_spaces", "")],
        ),
        // Only the first document's values are listed.
        (
            r#"---
doc1: |
  This is the first document
  with a literal block scalar.

next_key: value
---
doc2: >
  This is the second document
  with a folded block scalar.

another_key: another_value
...
"#,
            &[("doc1", "This is the first document\nwith a literal block scalar.\n"), ("next_key", "value")],
        ),
        (
            "preserve_me: |\n  Line with    multiple    spaces\n  Line with\ttabs\there\n  Line with trailing spaces\n\n  Empty line above and below\n\n  Final line\n",
            &[("preserve_me", "Line with    multiple    spaces\nLine with\ttabs\there\nLine with trailing spaces\n\nEmpty line above and below\n\nFinal line\n")],
        ),
        (
            r#"yaml_content: |
  # This block contains YAML-like content
  nested:
    - item: value
    - item: another

  mapping:
    key1: |
      Even more nested literal content
    key2: value

  anchors: &anchor
    anchor_content: data

  reference: *anchor

quoted_yaml: >
  This folded block contains
  YAML structures: {key: value, array: [1, 2, 3]}
  that should be treated as plain text.
"#,
            &[("yaml_content", "# This block contains YAML-like content\nnested:\n  - item: value\n  - item: another\n\nmapping:\n  key1: |\n    Even more nested literal content\n  key2: value\n\nanchors: &anchor\n  anchor_content: data\n\nreference: *anchor\n"), ("quoted_yaml", "This folded block contains YAML structures: {key: value, array: [1, 2, 3]} that should be treated as plain text.\n")],
        ),
        (
            r#"mixed_styles: |
  This literal block contains:
  - A flow sequence: [1, 2, 3]
  - A flow mapping: {key: value, other: data}
  - Mixed content: [a, {nested: true}, c]

flow_then_block:
  flow_seq: [item1, item2]
  block_literal: |
    This comes after flow style
    and should work fine.
  flow_map: {after: block}
"#,
            &[("mixed_styles", "This literal block contains:\n- A flow sequence: [1, 2, 3]\n- A flow mapping: {key: value, other: data}\n- Mixed content: [a, {nested: true}, c]\n"), ("flow_then_block.flow_seq.0", "item1"), ("flow_then_block.flow_seq.1", "item2"), ("flow_then_block.block_literal", "This comes after flow style\nand should work fine.\n"), ("flow_then_block.flow_map.after", "block")],
        ),
        (
            r#"empty: |
next: value"#,
            &[("empty", ""), ("next", "value")],
        ),
        (
            "inconsistent: |\n  normal indent\n    more indent  \n  back to normal\n      even more\n  normal\n",
            &[("inconsistent", "normal indent\n  more indent  \nback to normal\n    even more\nnormal\n")],
        ),
        (
            "tabs: |\n\tTab indented line\n\tAnother tab line\n",
            &[("tabs", "\tTab indented line\n\tAnother tab line\n")],
        ),
        // A tab is not indentation, so the body is indented two and its first
        // line holds the tab.
        (
            "k: |-\n  \ta\n  b\nz: 1\n",
            &[("k", "\ta\nb"), ("z", "1")],
        ),
        // The body's column is two, not three, so `]"` is body text rather than
        // a dedent followed by an unterminated quoted scalar.
        (
            "- |-\n  \t[\n  ]\"\n- b\n",
            &[("0", "\t[\n]\""), ("1", "b")],
        ),
        (
            r#"unix: |
  Line 1
  Line 2
"#,
            &[("unix", "Line 1\nLine 2\n")],
        ),
        (
            "windows: |\r\n  Line 1\r\n  Line 2\r\n",
            &[("windows", "Line 1\nLine 2\n")],
        ),
        (
            r#"config:
  description: |
    This is a configuration
    with multiple lines.

  name: "MyApp"
  version: 1.0

  settings: >
    These are settings that
    span multiple lines too.

  debug: true
"#,
            &[("config.description", "This is a configuration\nwith multiple lines.\n"), ("config.name", "MyApp"), ("config.version", "1.0"), ("config.settings", "These are settings that span multiple lines too.\n"), ("config.debug", "true")],
        ),
        (
            r#"numbers_as_text: |
  123
  45.67
  -89
  +100
  0xFF
  1e5
  true
  false
  null

calculations: >
  The result is: 2 + 2 = 4
  And 10 * 5 = 50
  Also: 100% complete
"#,
            &[("numbers_as_text", "123\n45.67\n-89\n+100\n0xFF\n1e5\ntrue\nfalse\nnull\n"), ("calculations", "The result is: 2 + 2 = 4 And 10 * 5 = 50 Also: 100% complete\n")],
        ),
        (
            r#"strip: |-
  Content
"#,
            &[("strip", "Content")],
        ),
        (
            r#"keep: |+
  Content

"#,
            &[("keep", "Content\n\n")],
        ),
        (
            r#"folded: >-
  Content
"#,
            &[("folded", "Content")],
        ),
        (
            r#"indent1: |1
 Single space
"#,
            &[("indent1", "Single space\n")],
        ),
        (
            r#"indent2: |2
  Two spaces
"#,
            &[("indent2", "Two spaces\n")],
        ),
        (
            r#"combined: |3+
   Content with keep

"#,
            &[("combined", "Content with keep\n\n")],
        ),
        (
            r#"description: |
  Line 1
  Line 2

summary: >
  Folded content

version: "1.0"
"#,
            &[("description", "Line 1\nLine 2\n"), ("summary", "Folded content\n"), ("version", "1.0")],
        ),
        (
            "items:\n  - |\n    First item content\n    with multiple lines\n  \n  - >\n    Second item folded\n    content\n  \n  - regular_item\n",
            &[("items.0", "First item content\nwith multiple lines\n"), ("items.1", "Second item folded content\n"), ("items.2", "regular_item")],
        ),
        (
            r#"empty: |

"#,
            &[("empty", "")],
        ),
        (
            r#"empty_folded: >

"#,
            &[("empty_folded", "")],
        ),
        (
            r#"simple: |
  Hello World
"#,
            &[("simple", "Hello World\n")],
        ),
        (
            r#"folded: >
  Hello World
"#,
            &[("folded", "Hello World\n")],
        ),
        (
            r#"strip: |-
  Content

keep: |+
  Content

"#,
            &[("strip", "Content"), ("keep", "Content\n\n")],
        ),
        (
            r#"explicit: |2
  Two space indent
"#,
            &[("explicit", "Two space indent\n")],
        ),
        (
            r#"config:
  script: |
    #!/bin/bash
    echo "Starting deployment"

    for service in api web worker; do
        echo "Deploying $service"
        kubectl apply -f $service.yaml
    done

  description: >
    This configuration defines a deployment
    script that will be executed during
    the CI/CD pipeline.
"#,
            &[("config.script", "#!/bin/bash\necho \"Starting deployment\"\n\nfor service in api web worker; do\n    echo \"Deploying $service\"\n    kubectl apply -f $service.yaml\ndone\n"), ("config.description", "This configuration defines a deployment script that will be executed during the CI/CD pipeline.\n")],
        ),
    ];
    for (yaml, expected) in cases {
        assert_leaves(yaml, expected);
    }
}

#[test]
fn block_scalar_performance_large_content() {
    // Test with a reasonably large block scalar
    let mut large_content = String::new();
    for i in 1..=100 {
        large_content.push_str(&format!(
            "  Line number {} with some content that makes it longer\n",
            i
        ));
    }

    let yaml = format!(
        "large_literal: |\n{}\nlarge_folded: >\n{}\n",
        large_content, large_content
    );

    let parsed =
        YamlFile::from_str(&yaml).expect("Should parse large block scalars without errors");

    let doc = parsed.document().expect("Should have document");
    let mapping = doc.as_mapping().expect("Should be a mapping");

    let literal_value = mapping
        .get("large_literal")
        .expect("Should have large_literal key")
        .as_scalar()
        .expect("Should be scalar")
        .as_string();

    // Build expected literal content (literal preserves newlines exactly)
    let mut expected_literal = String::new();
    for i in 1..=100 {
        expected_literal.push_str(&format!(
            "Line number {} with some content that makes it longer\n",
            i
        ));
    }
    assert_eq!(literal_value, expected_literal);

    let folded_value = mapping
        .get("large_folded")
        .expect("Should have large_folded key")
        .as_scalar()
        .expect("Should be scalar")
        .as_string();

    // Build expected folded content (folded folds lines into spaces, preserves double newlines)
    let mut expected_folded = String::new();
    for i in 1..=100 {
        if i > 1 {
            expected_folded.push(' ');
        }
        expected_folded.push_str(&format!(
            "Line number {} with some content that makes it longer",
            i
        ));
    }
    expected_folded.push('\n');
    assert_eq!(folded_value, expected_folded);

    let output = parsed.to_string();
    assert_eq!(output, yaml);
}

/// A block scalar can be anchored and aliased; `<<:` and `*template` in
/// a body are text, not a merge key and an alias.
#[test]
fn block_scalar_with_anchors_and_aliases() {
    let yaml = r#"template: &template |
  This is a template
  with multiple lines
  that can be referenced.

instance1: *template

instance2:
  content: *template
  other: value

modified: |
  <<: *template
  Additional content here
"#;
    assert_eq!(
        render_documents(yaml),
        [
            "map{template: This is a template\nwith multiple lines\nthat can be referenced.\n, \
          instance1: *template, instance2: map{content: *template, other: value}, \
          modified: <<: *template\nAdditional content here\n}"
        ]
    );
}

/// A block scalar can sit under a tag (`k: !!str >\n  ...`).
#[test]
fn block_scalar_under_tagged_node() {
    assert_eq!(
        render_documents("k: !!str >\n  wrapped\n  paragraph\n"),
        ["map{k: !!str wrapped paragraph\n}"]
    );
}

/// Write `value` into a mapping and a sequence, at the top level and
/// nested, both as a new entry and over an existing one. Each result
/// must have the tree the parser gives its text, and read `value` back.
fn check_block_scalar_edits(value: impl yaml_edit::AsYaml + Clone) {
    type Edit<V> = fn(&Document, V) -> Option<yaml_edit::YamlNode>;
    let cases: [(&str, Edit<_>); 7] = [
        ("a: 1\n", |d, v| {
            d.set("b", v);
            d.get("b")
        }),
        ("a: 1\nc: 2\n", |d, v| {
            d.set("a", v);
            d.get("a")
        }),
        ("a:\n  k: 1\n", |d, v| {
            let m = d.get_mapping("a").unwrap();
            m.set("j", v);
            m.get("j")
        }),
        ("a:\n  k: 1\nb: 2\n", |d, v| {
            let m = d.get_mapping("a").unwrap();
            m.set("k", v);
            m.get("k")
        }),
        ("- 1\n", |d, v| {
            let s = d.as_sequence().unwrap();
            s.push(v);
            s.get(1)
        }),
        ("s:\n  - a\nb: 2\n", |d, v| {
            let s = d.get_sequence("s").unwrap();
            s.push(v);
            s.get(1)
        }),
        ("- 1\n- 2\n", |d, v| {
            let s = d.as_sequence().unwrap();
            assert!(s.set(0, v));
            s.get(0)
        }),
    ];
    for (src, edit) in cases {
        let doc = Document::from_str(src).unwrap();
        let written = edit(&doc, value.clone()).unwrap();
        assert!(
            yaml_eq(&written, &value),
            "{src:?} -> {:?}",
            doc.to_string()
        );
        assert_parser_shaped(&doc);
    }
}

#[test]
fn block_scalars_are_parser_shaped() {
    for value in [
        ScalarValue::literal("x\ny\n"),
        ScalarValue::literal("x\ny"),
        ScalarValue::literal("x\ny\n\n"),
        ScalarValue::folded("x\ny"),
        ScalarValue::folded("x\n\n"),
        ScalarValue::literal("  x\ny\n"),
        ScalarValue::literal("x: 1 # no comment\n  [a, b]\n"),
        ScalarValue::literal("x\n\ny\n").with_tag("!t"),
        ScalarValue::folded("  x\n").with_tag("!!str"),
    ] {
        check_block_scalar_edits(value);
    }
    check_block_scalar_edits("x\ny\n");
    check_block_scalar_edits("  x\ny".to_string());
}

#[test]
fn block_scalar_text() {
    let doc = Document::from_str("a:\n  k: 1\n").unwrap();
    let m = doc.get_mapping("a").unwrap();
    m.set("j", ScalarValue::literal("x\n  y\n"));
    m.set("k", ScalarValue::literal("  x\n").with_tag("!t"));
    assert_edited(&doc, "a:\n  k: !t |2\n      x\n  j: |\n    x\n      y\n");
}
