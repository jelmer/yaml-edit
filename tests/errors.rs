//! Parse errors, their messages and positions, and recovery from them.
//!
//! The parser is forgiving: on invalid input it must terminate, keep the
//! valid parts accessible and give a tree that edits still work on.

mod common;
use common::{assert_strings, keys, parse, parse_with_errors, parse_with_violations};
use rowan::ast::AstNode;
use std::str::FromStr;
use yaml_edit::validator::{Rule, Severity, Validator};
use yaml_edit::{
    debug, Document, LineColumn, Parse, ScalarType, ScalarValue, Sequence, SyntaxKind, YamlFile,
};

#[test]
fn error_messages() {
    for (yaml, expected) in [
        (
            r#"name: "unclosed
age: 30"#,
            &["1:7: Unterminated quoted string"][..],
        ),
        ("name: 'unclosed\nage: 30", &["1:7: Unterminated quoted string"]),
        (
            r#"name: "unterminated string
age: 30"#,
            &["1:7: Unterminated quoted string"],
        ),
        (
            r#"valid: "good string"
also_valid: 42
broken: "unclosed
still_broken: value"#,
            &["3:9: Unterminated quoted string"],
        ),
        (
            "line1: value\nline2: value\nbad: [",
            &["3:7: Unclosed flow sequence. Expected: ']' to close sequence. Found: end of input. Context: in flow sequence. Suggestion: Add ']' to close the array, or check for missing commas between elements"],
        ),
        (
            r#"valid:
  nested:
    deep:
      error: [unclosed"#,
            &["4:23: Unclosed flow sequence. Expected: ']' to close sequence. Found: end of input. Context: in flow sequence. Suggestion: Add ']' to close the array, or check for missing commas between elements"],
        ),
        (
            "config: {host: localhost",
            &["1:25: Unclosed flow mapping. Expected: '}' to close mapping. Found: end of input. Context: in flow mapping. Suggestion: Add '}' to close the object, or check for missing commas between key-value pairs"],
        ),
        (
            "items: [a, b, c",
            &["1:16: Unclosed flow sequence. Expected: ']' to close sequence. Found: end of input. Context: in flow sequence. Suggestion: Add ']' to close the array, or check for missing commas between elements"],
        ),
        (
            "items: [a, b",
            &["1:13: Unclosed flow sequence. Expected: ']' to close sequence. Found: end of input. Context: in flow sequence. Suggestion: Add ']' to close the array, or check for missing commas between elements"],
        ),
        (
            r#"config: {
  host: localhost,
  port: 8080
"#,
            &["4:1: Unclosed flow mapping. Expected: '}' to close mapping. Found: end of input. Context: in flow mapping. Suggestion: Add '}' to close the object, or check for missing commas between key-value pairs"],
        ),
        (
            r#"items: [
  first,
  second,
  third
"#,
            &["5:1: Unclosed flow sequence. Expected: ']' to close sequence. Found: end of input. Context: in flow sequence. Suggestion: Add ']' to close the array, or check for missing commas between elements"],
        ),
        (
            r#"outer:
  inner: {key: value"#,
            &["2:21: Unclosed flow mapping. Expected: '}' to close mapping. Found: end of input. Context: in flow mapping. Suggestion: Add '}' to close the object, or check for missing commas between key-value pairs"],
        ),
        (
            r#"outer:
  inner: [a, b, c"#,
            &["2:18: Unclosed flow sequence. Expected: ']' to close sequence. Found: end of input. Context: in flow sequence. Suggestion: Add ']' to close the array, or check for missing commas between elements"],
        ),
        (
            r#"config: {host: localhost, port: 8080
server: running"#,
            &["2:16: Unclosed flow mapping. Expected: '}' to close mapping. Found: end of input. Context: in flow mapping. Suggestion: Add '}' to close the object, or check for missing commas between key-value pairs"],
        ),
        (
            r#"items: [a, b, c
config: {host localhost}"#,
            &["2:25: Unclosed flow sequence. Expected: ']' to close sequence. Found: end of input. Context: in flow sequence. Suggestion: Add ']' to close the array, or check for missing commas between elements"],
        ),
    ] {
        parse_with_errors(yaml, expected);
    }
}

/// An unclosed flow collection runs to the end of the input, swallowing
/// the lines after it.
#[test]
fn unclosed_flow_collection_swallows_later_lines() {
    for (yaml, expected, keys_left) in [
        (
            r#"valid1: value1
broken: [unclosed
valid2: value2
valid3: value3"#,
            &["4:15: Unclosed flow sequence. Expected: ']' to close sequence. Found: end of input. Context: in flow sequence. Suggestion: Add ']' to close the array, or check for missing commas between elements"][..],
            &["valid1", "broken"][..],
        ),
        (
            r#"start: value
broken: [
fixed: works
end: value"#,
            &["4:11: Unclosed flow sequence. Expected: ']' to close sequence. Found: end of input. Context: in flow sequence. Suggestion: Add ']' to close the array, or check for missing commas between elements"],
            &["start", "broken"],
        ),
        (
            "items: [a, b, c\nnext: value",
            &["2:12: Unclosed flow sequence. Expected: ']' to close sequence. Found: end of input. Context: in flow sequence. Suggestion: Add ']' to close the array, or check for missing commas between elements"],
            &["items"],
        ),
        (
            "config: {host localhost\nother: value",
            &["2:13: Unclosed flow mapping. Expected: '}' to close mapping. Found: end of input. Context: in flow mapping. Suggestion: Add '}' to close the object, or check for missing commas between key-value pairs"],
            &["config"],
        ),
        (
            "flow: [a, b\nblock:\n  key: value",
            &["3:13: Unclosed flow sequence. Expected: ']' to close sequence. Found: end of input. Context: in flow sequence. Suggestion: Add ']' to close the array, or check for missing commas between elements"],
            &["flow"],
        ),
        (
            "outer: [inner: {broken\nrescue: works",
            &[
                "2:14: Unclosed flow mapping. Expected: '}' to close mapping. Found: end of input. Context: in flow mapping. Suggestion: Add '}' to close the object, or check for missing commas between key-value pairs",
                "2:14: Unclosed flow sequence. Expected: ']' to close sequence. Found: end of input. Context: in flow sequence. Suggestion: Add ']' to close the array, or check for missing commas between elements",
            ],
            &["outer"],
        ),
        (
            "broken: [a, b\nmissing colon here\nvalid: works",
            &["3:13: Unclosed flow sequence. Expected: ']' to close sequence. Found: end of input. Context: in flow sequence. Suggestion: Add ']' to close the array, or check for missing commas between elements"],
            &["broken"],
        ),
        (
            r#"bad1: [unclosed
bad2: {also_unclosed"#,
            &[
                "2:21: Unclosed flow mapping. Expected: '}' to close mapping. Found: end of input. Context: in flow mapping. Suggestion: Add '}' to close the object, or check for missing commas between key-value pairs",
                "2:21: Unclosed flow sequence. Expected: ']' to close sequence. Found: end of input. Context: in flow sequence. Suggestion: Add ']' to close the array, or check for missing commas between elements",
            ],
            &["bad1"],
        ),
        // Deeply nested malformed structures must not overflow the stack or hang.
        (
            r#"
level1: [
  level2: {
    level3: [broken structure here
    level3b: another broken
  level2b: {missing colon
level1b: value
"#,
            &[
                "8:1: Unclosed flow mapping. Expected: '}' to close mapping. Found: end of input. Context: in flow mapping. Suggestion: Add '}' to close the object, or check for missing commas between key-value pairs",
                "8:1: Unclosed flow sequence. Expected: ']' to close sequence. Found: end of input. Context: in flow sequence. Suggestion: Add ']' to close the array, or check for missing commas between elements",
                "8:1: Unclosed flow mapping. Expected: '}' to close mapping. Found: end of input. Context: in flow mapping. Suggestion: Add '}' to close the object, or check for missing commas between key-value pairs",
                "8:1: Unclosed flow sequence. Expected: ']' to close sequence. Found: end of input. Context: in flow sequence. Suggestion: Add ']' to close the array, or check for missing commas between elements",
            ],
            &["level1"],
        ),
    ] {
        let file = parse_with_errors(yaml, expected);
        assert_eq!(keys(&file), keys_left, "{yaml:?}");
    }
}

#[test]
fn error_positions() {
    let yaml = r#"
key1: value1
key2: "unclosed quote
key3: value3
"#;
    let parse = Parse::parse_yaml(yaml);
    assert_eq!(parse.errors(), vec!["3:7: Unterminated quoted string"]);

    let positioned = parse.positioned_errors();
    assert_eq!(positioned.len(), 1);
    assert_eq!(positioned[0].message, "Unterminated quoted string");
    assert_eq!(positioned[0].start_position(yaml), LineColumn::new(3, 7));

    assert_eq!(
        parse.to_result().unwrap_err().to_string(),
        "Parse error at line 3, column 7: Unterminated quoted string"
    );
}

/// The tree each input recovers to, with the errors reported for it.
#[test]
fn recovered_trees() {
    for (yaml, errors, tree) in [
        (
            "[c:",
            &["1:4: Unclosed flow sequence. Expected: ']' to close sequence. Found: end of input. Context: in flow sequence. Suggestion: Add ']' to close the array, or check for missing commas between elements"][..],
            r#"DOCUMENT
  SEQUENCE
    LEFT_BRACKET: "["
    SEQUENCE_ENTRY
      MAPPING
        MAPPING_ENTRY
          KEY
            SCALAR
              STRING: "c"
          COLON: ":"
          VALUE
            SCALAR
              NULL: ""
    RIGHT_BRACKET: ""
"#,
        ),
        (
            "{c:",
            &["1:4: Unclosed flow mapping. Expected: '}' to close mapping. Found: end of input. Context: in flow mapping. Suggestion: Add '}' to close the object, or check for missing commas between key-value pairs"],
            r#"DOCUMENT
  MAPPING
    LEFT_BRACE: "{"
    MAPPING_ENTRY
      KEY
        SCALAR
          STRING: "c"
      COLON: ":"
      VALUE
        SCALAR
          NULL: ""
    RIGHT_BRACE: ""
"#,
        ),
        (
            "{?",
            &["1:3: Unclosed flow mapping. Expected: '}' to close mapping. Found: end of input. Context: in flow mapping. Suggestion: Add '}' to close the object, or check for missing commas between key-value pairs"],
            r#"DOCUMENT
  MAPPING
    LEFT_BRACE: "{"
    MAPPING_ENTRY
      KEY
        QUESTION: "?"
        SCALAR
          NULL: ""
      VALUE
        SCALAR
          NULL: ""
    RIGHT_BRACE: ""
"#,
        ),
        (
            "[]:\n]:",
            &["2:1: Unexpected token in mapping: \"]\""],
            r#"DOCUMENT
  MAPPING
    MAPPING_ENTRY
      KEY
        SEQUENCE
          LEFT_BRACKET: "["
          RIGHT_BRACKET: "]"
      COLON: ":"
      VALUE
        SCALAR
          NULL: ""
      NEWLINE: "\\n"
    ERROR
      RIGHT_BRACKET: "]"
    MAPPING_ENTRY
      KEY
        SCALAR
          NULL: ""
      COLON: ":"
      VALUE
        SCALAR
          NULL: ""
"#,
        ),
        (
            "?\n] ?:",
            &["2:1: Unexpected token in explicit-key mapping: \"]\""],
            r#"DOCUMENT
  MAPPING
    MAPPING_ENTRY
      QUESTION: "?"
      KEY
        SCALAR
          NULL: ""
      VALUE
        SCALAR
          NULL: ""
      NEWLINE: "\\n"
    ERROR
      RIGHT_BRACKET: "]"
    WHITESPACE: " "
    MAPPING_ENTRY
      KEY
        SCALAR
          STRING: "?"
      COLON: ":"
      VALUE
        SCALAR
          NULL: ""
"#,
        ),
        (
            "[a, }]",
            &["1:5: Unexpected token in flow sequence: \"}\""],
            r#"DOCUMENT
  SEQUENCE
    LEFT_BRACKET: "["
    SEQUENCE_ENTRY
      SCALAR
        STRING: "a"
      COMMA: ","
      WHITESPACE: " "
    SEQUENCE_ENTRY
      SCALAR
        NULL: ""
    ERROR
      RIGHT_BRACE: "}"
    RIGHT_BRACKET: "]"
"#,
        ),
    ] {
        let file = parse_with_errors(yaml, errors);
        assert_eq!(debug::tree_to_string(file.syntax()), tree, "{yaml:?}");
    }
}

/// Input the parser accepts without errors although it is invalid YAML;
/// the validator reports what is wrong with it.
#[test]
fn invalid_input_reported_by_validator() {
    for (yaml, expected) in [
        // A plain scalar may contain spaces, so `key1 value` is the
        // document's own scalar rather than a key. A mapping entry cannot
        // follow one, which both saphyr and PyYAML reject; the parser keeps
        // the mapping as a second node of the document.
        (
            "key1 value\nkey2: value",
            &[("Document holds more than one node", "2:1")][..],
        ),
        // The entries after {} cannot be attached to it.
        (
            r#"---
{}
key1: value1
key2: value2"#,
            &[("Document holds more than one node", "3:1")],
        ),
        // A node has at most one tag; the parser keeps a second one as a
        // tagged node of its own.
        ("!a !b\n- x\n", &[("Multiple tags on the same node", "1:1")]),
        ("k: !a !b x\n", &[("Multiple tags on the same node", "1:4")]),
        (
            "- !a\n  !b\n  - x\n",
            &[("Multiple tags on the same node", "1:3")],
        ),
        (
            "%INVALID directive\nkey: value",
            &[("Directive without document start marker (---)", "1:1")],
        ),
        ("ref: *undefined", &[("Undefined alias *undefined", "1:6")]),
    ] {
        parse_with_violations(yaml, expected);
    }
}

/// A plain scalar cannot contain ": ", but the parser reads the rest of
/// the line as the value anyway.
#[test]
fn colon_space_in_plain_value() {
    for (yaml, expected, violations) in [
        (
            r#"not_url_2: key: value on same line"#,
            &[("not_url_2", "key: value on same line")][..],
            &[(
                "Plain scalar value cannot contain mapping syntax (colon)",
                "1:15",
            )][..],
        ),
        (
            "bad: key: value",
            &[("bad", "key: value")],
            &[(
                "Plain scalar value cannot contain mapping syntax (colon)",
                "1:9",
            )],
        ),
        (
            r#"
good_key: good_value
bad: key: with: many: colons
another_good: value
"#,
            &[
                ("good_key", "good_value"),
                ("bad", "key: with: many: colons"),
                ("another_good", "value"),
            ],
            &[(
                "Plain scalar value cannot contain mapping syntax (colon)",
                "3:9",
            )],
        ),
        ("key: value", &[("key", "value")], &[]),
        (
            "url: http://example.com",
            &[("url", "http://example.com")],
            &[],
        ),
        ("url: example.com:8080", &[("url", "example.com:8080")], &[]),
        // A colon not followed by a space is part of the scalar.
        ("good: key:value", &[("good", "key:value")], &[]),
        ("quoted: \"key: value\"", &[("quoted", "key: value")], &[]),
    ] {
        let file = parse_with_violations(yaml, violations);
        let doc = file.document().unwrap();
        let expected_keys: Vec<&str> = expected.iter().map(|(k, _)| *k).collect();
        assert_eq!(keys(&file), expected_keys, "{yaml:?}");
        for (key, value) in expected {
            assert_eq!(doc.get_string(key).unwrap(), *value, "{yaml:?}");
        }
    }
}

/// `ipv6: [::1]:8080` without quotes is a flow sequence followed by
/// unexpected text. The `[::1]` part recovers as a flow sequence with one
/// plain scalar "::1" (a colon not followed by whitespace is scalar
/// content), and the `:8080` tail stays in the value as a second node.
#[test]
fn text_after_flow_sequence_value() {
    let yaml = r#"ipv6: [::1]:8080"#;
    let file = parse_with_violations(yaml, &[("Value holds more than one node", "1:12")]);
    assert_eq!(keys(&file), ["ipv6"]);
    let doc = file.document().unwrap();
    let seq = doc.get_sequence("ipv6").unwrap();
    assert_eq!(seq.len(), 1);
    assert_eq!(seq.get(0).unwrap().as_scalar().unwrap().as_string(), "::1");
}

/// Tabs are not allowed for indentation.
#[test]
fn tab_indentation() {
    let yaml = "key: value\n\tindented: invalid";
    let file = parse_with_violations(
        yaml,
        &[
            ("Tabs are not allowed for indentation in YAML", "2:1"),
            (
                "Sibling block mapping entries have inconsistent indentation (expected \"\", found \"\\t\")",
                "2:1",
            ),
        ],
    );
    let doc = file.document().unwrap();
    assert_eq!(doc.get_string("key").unwrap(), "value");
}

#[test]
fn mixed_indentation_levels() {
    let yaml_mixed_indent = r#"root:
  level1:
    level2:
     level2_off: value
      level3:
       level3_off: value"#;
    let file = parse_with_violations(
        yaml_mixed_indent,
        &[(
            "Sibling block mapping entries have inconsistent indentation (expected \"     \", found \"      \")",
            "5:1",
        )],
    );
    let doc = file.document().unwrap();
    let level2 = doc
        .get_mapping("root")
        .unwrap()
        .get_mapping("level1")
        .unwrap()
        .get_mapping("level2")
        .unwrap();
    assert_eq!(
        level2
            .get("level2_off")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "value"
    );
    let level3 = level2.get_mapping("level3").unwrap();
    assert_eq!(
        level3
            .get("level3_off")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "value"
    );
}

/// Input that looks suspicious but is valid for the parser.
#[test]
fn accepted_without_errors() {
    // An anchor name may start with a digit.
    let file = parse_with_violations("&123invalid value", &[]);
    assert_eq!(
        file.document().unwrap().as_scalar().unwrap().as_string(),
        "value"
    );

    // A later anchor of the same name replaces the earlier one.
    let file = parse_with_violations(
        r#"first: &x value1
second: &x value2
ref: *x"#,
        &[],
    );
    assert_eq!(keys(&file), ["first", "second", "ref"]);
}

/// The text after `{}` is kept, so it survives a reparse; the mapping
/// itself stays empty.
#[test]
fn reparse_keeps_content_after_flow_mapping() {
    let invalid_yaml = r#"---
{}
key1: value1
key2: value2"#;
    let yaml = YamlFile::parse(invalid_yaml).tree();
    let result = yaml.to_string();
    assert_eq!(result, invalid_yaml);

    let reparsed = YamlFile::parse(&result).tree();
    assert_eq!(reparsed.to_string(), invalid_yaml);
    assert_eq!(keys(&reparsed), Vec::<String>::new());
}

/// Setting into the `{}` inserts inside it; the block content after it
/// stays where it was.
#[test]
fn set_in_flow_mapping_followed_by_block_entries() {
    for (invalid_yaml, key, value, expected) in [
        (
            r#"---
{}
key1: value1"#,
            "key2",
            "value2",
            "---\n{key2: value2}\nkey1: value1",
        ),
        (
            r#"---
{}
name: test
version: 1.0"#,
            "author",
            "John",
            "---\n{author: John}\nname: test\nversion: 1.0",
        ),
    ] {
        let yaml = YamlFile::parse(invalid_yaml).tree();
        assert_eq!(yaml.to_string(), invalid_yaml);
        yaml.document()
            .unwrap()
            .as_mapping()
            .unwrap()
            .set(key, value);
        debug::validate_tree(yaml.syntax()).unwrap();
        assert_eq!(yaml.to_string(), expected);
    }
}

/// A key the parser recovered without its `:` gets one when its value is set.
#[test]
fn set_value_of_key_missing_colon() {
    fn set_last(src: &str, value: impl yaml_edit::AsYaml) -> String {
        let doc = YamlFile::parse(src).tree().document().unwrap();
        let entry = doc.as_mapping().unwrap().entries().last().unwrap();
        entry.set_value(value, false);
        debug::validate_tree(doc.syntax()).unwrap();
        doc.to_string()
    }
    let block = YamlFile::from_str("k:\n  a: 1\n").unwrap();
    let block = block.document().unwrap().get("k").unwrap();
    for (src, inline, nested) in [
        // An unclosed flow collection key runs to the end of the input,
        // where its `:` was missing.
        ("[{]:", "[{]:: x", "[{]::\n  a: 1"),
        ("?\n[:\n", "?\n[:\n: x", "?\n[:\n:\n  a: 1\n"),
        (":\n{:", ":\n{:: x", ":\n{::\n  a: 1"),
    ] {
        assert_eq!(set_last(src, "x"), inline, "{src:?}");
        assert_eq!(set_last(src, block.clone()), nested, "{src:?}");
    }
}

fn nth_sequence(file: &YamlFile, n: usize) -> Sequence {
    file.syntax()
        .descendants()
        .filter(|c| c.kind() == SyntaxKind::SEQUENCE)
        .nth(n)
        .and_then(Sequence::cast)
        .unwrap()
}

#[test]
fn remove_last_item_of_sequence_in_recovered_tree() {
    for (yaml, n, expected) in [
        ("!a !b\n- x\n", 0, "!a !b\n[]\n"),
        ("k: !a !b\n  - x\n", 0, "k: !a !b []\n"),
        (
            "- [a, b ,!str\n-\n  !!null : a\n  b: !!str\n- !!s     ]\n",
            2,
            "- [a, b ,!str\n[]: a\n  b: !!str\n- !!s     ]\n",
        ),
    ] {
        let file = YamlFile::from_str(yaml).unwrap();
        nth_sequence(&file, n).remove(0);
        assert_eq!(file.to_string(), expected, "{yaml:?}");
    }
}

#[test]
fn refill_value_with_two_tags() {
    let file = YamlFile::from_str("k: !a !b\n  - x\n").unwrap();
    let seq = nth_sequence(&file, 0);
    seq.remove(0);
    seq.push("y");
    assert_eq!(file.to_string(), "k: !a !b\n  - y\n");
}

#[test]
fn refill_root_sequence_followed_by_mapping() {
    let file = YamlFile::from_str(" - x\nb: 1").unwrap();
    let seq = nth_sequence(&file, 0);
    seq.remove(0);
    assert_eq!(file.to_string(), " []\nb: 1");
    seq.insert(0, "y");
    common::assert_file_edited(&file, " - y\nb: 1");
}

#[test]
fn clear_sequence_with_anchor_text_in_scalar() {
    // `&SS` is part of the plain scalar, so `*SS` refers to nothing.
    let file = YamlFile::from_str("- f\n  - &SS Sammy\nrb: *SS\n").unwrap();
    nth_sequence(&file, 0).clear();
    assert_eq!(file.to_string(), "[]\nrb: *SS\n");
}

/// What the validator reports about input that parses: valid YAML it used
/// to flag, mostly found through the YAML test suite, and the genuine
/// faults next to it.
#[test]
fn validator_findings() {
    for (yaml, expected) in [
        // YAML 1.2 `l-explicit-document` lets a node share the marker's
        // line, as PyYAML reads it; flagging every node there hit 11 files
        // the test suite marks valid. A block mapping there is reported.
        ("--- a\n", &[][..]),
        ("--- >\n ab\n", &[]),
        ("--- {a: 1}\n", &[]),
        ("--- [1]\n", &[]),
        (
            "--- key1: value1\n    key2: value2\n",
            &[("Content on same line as document start marker", "1:1")],
        ),
        // A block scalar's indentation indicator is a single digit 1-9,
        // either side of a chomping indicator (D83L).
        (
            "--- |0\n",
            &[(
                "Block scalar indentation indicator must be a digit 1-9",
                "1:6",
            )],
        ),
        (
            "--- |10\n",
            &[(
                "Block scalar indentation indicator must be a digit 1-9",
                "1:6",
            )],
        ),
        ("k: |-\n  x\n", &[]),
        ("k: |2-\n    x\n", &[]),
        ("k: |2\n    x\n", &[]),
        ("k: |-2\n  x\n", &[]),
        ("k: |2-\n  x\n", &[]),
        // A flow collection's continuation only has to clear a block
        // context it sits inside. The test suite's 9C9N makes
        // `flow: [a,\nb,\nc]` an error because the continuation would read
        // as a new entry of the block mapping around it; 4ABK's
        // `{\nunquoted : "separate",\n...}` has no such neighbour and is
        // valid; requiring indentation regardless hit 16 valid files.
        (
            "---\nflow: [a,\nb,\nc]\n",
            &[("Flow collection continuation line must be indented", "2:7")],
        ),
        ("{\na: 1,\nb: 2,\n}\n", &[]),
        ("k: [a,\n  b]\n", &[]),
        ("[a, b]\n", &[]),
        // A `---` or `...` at the start of a line ends the document, so it
        // cannot appear inside a flow collection (the test suite's N782).
        // The lexer gives them as plain scalars there, so no
        // document-marker rule sees them.
        (
            "[\n--- ,\n...\n]\n",
            &[("Document marker inside a flow collection", "2:1")],
        ),
        // A mapping's expected entry column comes from wherever its
        // indentation really is, which for a sequence entry's value is not
        // a VALUE node: `-\n  a: 1\n` keeps its INDENT in the
        // SEQUENCE_ENTRY, and a compact `- a: 1\n  b: 2\n` has none at all.
        // Both fell back to the empty string, so every entry looked wrongly
        // indented.
        ("-\n  a: 1\n  b: 2\n", &[]),
        ("- a: 1\n  b: 2\n", &[]),
        (" - key: value\n   key2: value2\n", &[]),
        ("k:\n  a: 1\n  b: 2\n", &[]),
        (
            "a: 1\n  b: 2\n",
            &[(
                "Sibling block mapping entries have inconsistent indentation \
                 (expected \"\", found \"  \")",
                "2:1",
            )],
        ),
        // An anchor at document level annotates the document's own node.
        // Flagging every such anchor hit 7 files the test suite marks
        // valid. Here it annotates a plain scalar, which cannot start with
        // "- ".
        ("&sequence\n- a\n", &[]),
        ("&a a: b\n", &[]),
        ("&flowseq [ a ]\n", &[]),
        (
            "&anchor - sequence entry\n",
            &[("Plain scalar cannot start with '-'", "1:9")],
        ),
        // An explicit key's entry ends with the zero-width implicit-null
        // scalar of its VALUE, so the newline that separates it from the
        // next entry sits inside it. Looking only at the entry's very last
        // token found the null scalar and reported the entries as sharing a
        // line; 6 files the test suite marks valid were flagged.
        ("? a\n? b\n", &[]),
        ("a: 1\nb: 2\n", &[]),
        ("? a\n: 1\n? b\n: 2\n", &[]),
        // A tab is illegal only where YAML requires indentation
        // (`s-indent`): 4EJS's nested mapping, Y79Y's `-\t-` where a block
        // collection follows, DK95/01's quoted continuation in a mapping
        // value. Checking every token flagged 19 files the test suite marks
        // valid: a tab inside a scalar's continuation, after a document
        // marker, or in a root-level flow collection is content or legal
        // separation.
        (
            "a:\n\tb: 1\n",
            &[("Tabs are not allowed for indentation in YAML", "2:1")],
        ),
        (
            "-\t-\n",
            &[("Tabs are not allowed for indentation in YAML", "1:2")],
        ),
        (
            "foo: \"bar\n\tbaz\"\n",
            &[("Tabs are not allowed for indentation in YAML", "1:6")],
        ),
        ("1st\n\t2nd\n", &[]),
        ("x:\n - x\n  \tx\n", &[]),
        ("a: |\n  x\n   \ty\n", &[]),
        ("\t[\n\t]\n", &[]),
        ("a: b\t\nseq:\t\n - a\t\n", &[]),
        ("-\t-1\n", &[]),
        // The multi-line restriction is on an implicit key in block
        // context. A flow collection may spread its entries over lines and
        // its keys may span them (8KB6), and a flow mapping may put the `:`
        // on a later line (5MUD). What stays an error is a block mapping
        // whose key spans lines (C2SP's `[23\n]: 42`, where the flow
        // sequence is the key) and ZXT5's flow sequence with the same shape.
        ("{\nunquoted : \"separate\",\n}\n", &[]),
        ("- { multi\n  line: value}\n", &[]),
        ("---\n{ \"foo\"\n  :bar }\n", &[]),
        ("{\n? explicit: entry,\n?\n}\n", &[]),
        (
            "[23\n]: 42\n",
            &[
                ("Implicit key cannot span multiple lines", "1:1"),
                ("Flow collection continuation line must be indented", "1:1"),
            ],
        ),
        (
            "[ \"key\"\n  :value ]\n",
            &[("Implicit key cannot span multiple lines", "1:3")],
        ),
        (
            "\"a\nb\": 1\n",
            &[
                ("Implicit key cannot span multiple lines", "1:1"),
                ("Wrong indented multiline quoted scalar", "1:1"),
            ],
        ),
        // A comment's separating whitespace may sit a level up in the
        // tree: `hr: # c` puts the COMMENT first inside the VALUE, so it
        // has no previous sibling. Checking siblings alone reported 5 files
        // the test suite marks valid.
        ("hr: # c\n", &[]),
        ("key:    # c\n", &[]),
        ("- # Empty\n", &[]),
        ("a: 1\n# c\nb: 2\n", &[]),
        (
            "k: \"v\"# c\n",
            &[("Comment without whitespace separation", "1:7")],
        ),
        // A verbatim tag's URI may hold commas and brackets (7FWL, UGM3).
        ("!<tag:e.com,2002:x> a\n", &[]),
        // `\\` takes the character after it, so `\\$` is two valid escapes
        // (6SLA), and `\` before a line break folds the line (565N).
        ("k: \"a\\\\$b\"\n", &[]),
        ("k: \"a\\\n  b\"\n", &[]),
        ("k: \"a\\qb\"\n", &[("Invalid escape sequence: \\q", "1:4")]),
        (
            "!!str, x\n",
            &[("Plain scalar cannot start with ','", "1:6")],
        ),
        // An anchor and an alias on different nodes are fine: `&node3`
        // anchors the value's mapping, `*alias1` is that mapping's key
        // (26DV). A block scalar's body is literal text, colons included
        // (4WA9). Both on one node, and a tab opening a body, are errors.
        ("top1: &alias1 k\ntop3: &node3\n  *alias1 : v\n", &[]),
        ("- aaa: |2\n    xxx\n  bbb: |\n    xxx\n", &[]),
        (
            "a: &b *c\n",
            &[
                ("Undefined alias *c", "1:7"),
                ("Node cannot have both an anchor and be an alias", "1:4"),
            ],
        ),
        (
            "foo: |\n\t\nbar: 1\n",
            &[("Tabs are not allowed for indentation in YAML", "2:1")],
        ),
        (
            "k: a: b\n",
            &[(
                "Plain scalar value cannot contain mapping syntax (colon)",
                "1:5",
            )],
        ),
        // A directive after a document is fine once a `...` has ended it,
        // whatever the document's root.
        ("a\n...\n%YAML 1.2\n---\nb\n", &[]),
        ("a: 1\n...\n%YAML 1.2\n---\nb\n", &[]),
    ] {
        parse_with_violations(yaml, expected);
    }
}

/// Documents [`yaml_edit::validator::Validator::validate`] has to accept.
#[test]
fn validator_accepts_document() {
    for yaml in [
        // R4YG: the " \t" line is indented one space; the tab is content,
        // so the following one-space "detected" is not under-indented.
        "- >\n \t\n detected\n",
        // H2RW: once content sets the body indent, a deeper blank line
        // contributes content rather than raising the indentation bar.
        "text: |\n  a\n    \n  b\n",
        // H2RW: whitespace padding a blank line between entries says
        // nothing about how the next entry is indented.
        "foo: 1\nbar: 2\n    \ntext: 3\n",
        // V9D5: the indent before the outer `:` belongs to the
        // explicit-key entry, not to the nested mapping that forms the key.
        "- sun: yellow\n- ? earth: blue\n  : moon: white\n",
    ] {
        let doc = Document::from_str(yaml).unwrap();
        let violations = Validator::new().validate(&doc);
        assert_eq!(violations, vec![], "{yaml:?}");
    }
}

/// A bare `\r` ends a line in YAML, so it resets the reported column.
///
/// The error context counted only `\n`, so line and column climbed across a
/// whole CR-delimited file and an error on its third line was reported at
/// `1:15` rather than `3:5`.
#[test]
fn carriage_return_ends_a_line_for_error_positions() {
    let cr = YamlFile::parse("a: 1\rb: 2\r... x\r");
    let lf = YamlFile::parse("a: 1\nb: 2\n... x\n");
    let position = |p: &Parse<YamlFile>| -> Option<String> {
        let text: String = p.errors().first()?.to_string();
        Some(text.split(':').take(2).collect::<Vec<_>>().join(":"))
    };
    assert_eq!(position(&cr), position(&lf));
    assert_eq!(position(&cr).as_deref(), Some("3:5"));
}

/// A plain scalar that a YAML 1.1 reader reads differently gets a warning
/// that says how, and what to write to keep the 1.1 meaning.
#[test]
fn legacy_yaml_1_1_warnings() {
    let warn = |src: &str| -> Vec<(Rule, Severity, String)> {
        let doc = Document::from_str(src).unwrap();
        Validator::new()
            .validate(&doc)
            .into_iter()
            .map(|v| (v.rule, v.severity, v.message))
            .collect()
    };
    for (src, message) in [
        // A bare octal changes value between versions with no syntax
        // error, so it is the case worth flagging. The readings are not
        // limited to a fixed-width integer, and 1.2 has no signed `0o`
        // form, so a negative one is kept as decimal.
        (
            "mode: 0755\n",
            "`0755` is YAML 1.1 octal, read as 493; YAML 1.2 reads it as 755. \
             Write `493` to keep the octal meaning",
        ),
        (
            "mode: 0777777777777777777777777\n",
            "`0777777777777777777777777` is YAML 1.1 octal, read as \
             4722366482869645213695; YAML 1.2 reads it as 777777777777777777777777. \
             Write `4722366482869645213695` to keep the octal meaning",
        ),
        (
            "mode: -00777\n",
            "`-00777` is YAML 1.1 octal, read as -511; YAML 1.2 reads it as -777. \
             Write `-511` to keep the octal meaning",
        ),
        (
            "mode: 000\n",
            "`000` is YAML 1.1 octal, read as 0; YAML 1.2 reads it as 0. \
             Write `0` to keep the octal meaning",
        ),
        // A leading zero with an 8 or 9 is not 1.1 octal, and 1.1 decimal
        // has no leading zero, so 1.1 reads it as a string.
        (
            "n: 089\n",
            "`089` is a string in YAML 1.1; YAML 1.2 reads it as the integer 89. \
             Write `'089'` to keep the string meaning",
        ),
        (
            "n: -0789\n",
            "`-0789` is a string in YAML 1.1; YAML 1.2 reads it as the integer -789. \
             Write `'-0789'` to keep the string meaning",
        ),
        (
            "n: +09\n",
            "`+09` is a string in YAML 1.1; YAML 1.2 reads it as the integer 9. \
             Write `'+09'` to keep the string meaning",
        ),
        // The 1.1 booleans are already strings here, but a 1.1 reader
        // differs.
        (
            "a: yes\n",
            "`yes` is the string \"yes\" in YAML 1.2, but a boolean to a YAML 1.1 reader; \
             quote it or write true/false to be unambiguous",
        ),
        (
            "a: OFF\n",
            "`OFF` is the string \"OFF\" in YAML 1.2, but a boolean to a YAML 1.1 reader; \
             quote it or write true/false to be unambiguous",
        ),
        // Numbers to a 1.1 reader, strings in 1.2.
        (
            "a: 0b1010\n",
            "`0b1010` is the integer 10 in YAML 1.1; YAML 1.2 reads it as a string. \
             Write `10` to keep the integer meaning",
        ),
        (
            "a: -0x1F\n",
            "`-0x1F` is the integer -31 in YAML 1.1; YAML 1.2 reads it as a string. \
             Write `-31` to keep the integer meaning",
        ),
        (
            "a: 0x_1F\n",
            "`0x_1F` is the integer 31 in YAML 1.1; YAML 1.2 reads it as a string. \
             Write `0x1F` to keep the integer meaning",
        ),
        (
            "a: 0_755\n",
            "`0_755` is the integer 493 in YAML 1.1; YAML 1.2 reads it as a string. \
             Write `493` to keep the integer meaning",
        ),
        (
            "a: 1_000\n",
            "`1_000` is the integer 1000 in YAML 1.1; YAML 1.2 reads it as a string. \
             Write `1000` to keep the integer meaning",
        ),
        (
            "a: 1_000.5\n",
            "`1_000.5` is the float 1000.5 in YAML 1.1; YAML 1.2 reads it as a string. \
             Write `1000.5` to keep the float meaning",
        ),
        (
            "t: 1:30\n",
            "`1:30` is the integer 90 in YAML 1.1; YAML 1.2 reads it as a string. \
             Write `90` to keep the integer meaning",
        ),
        (
            "t: 1:30.5\n",
            "`1:30.5` is the float 90.5 in YAML 1.1; YAML 1.2 reads it as a string. \
             Write `90.5` to keep the float meaning",
        ),
        // Strings to a 1.1 reader, numbers in 1.2.
        (
            "mode: 0o17\n",
            "`0o17` is a string in YAML 1.1; YAML 1.2 reads it as the integer 15. \
             Write `'0o17'` to keep the string meaning",
        ),
        (
            "a: 1e3\n",
            "`1e3` is a string in YAML 1.1; YAML 1.2 reads it as a float. \
             Write `'1e3'` to keep the string meaning",
        ),
        (
            "a: -1.0E3\n",
            "`-1.0E3` is a string in YAML 1.1; YAML 1.2 reads it as a float. \
             Write `'-1.0E3'` to keep the string meaning",
        ),
    ] {
        assert_eq!(
            warn(src),
            [(Rule::LegacyYaml11, Severity::Warning, message.to_string())],
            "{src:?}"
        );
    }
    assert_eq!(
        ScalarValue::parse("-0o777").scalar_type(),
        ScalarType::String
    );

    // A suggestion reads the same in both versions, so it is not flagged
    // itself.
    for src in [
        "a: 0755\n",
        "a: -00777\n",
        "a: 000\n",
        "a: 0b1010\n",
        "a: -0x1F\n",
        "a: 0x_1F\n",
        "a: 0_755\n",
        "a: 1_000\n",
        "a: 1_000.5_0\n",
        "a: 1_0.5e+5\n",
        "a: 0_5.5\n",
        "t: 1:30\n",
        "t: 0:30.\n",
        "t: 1:30.5\n",
        "a: 089\n",
        "a: 0o17\n",
        "a: 1e3\n",
    ] {
        let message = &warn(src)[0].2;
        let suggestion = message
            .split("Write `")
            .nth(1)
            .and_then(|rest| rest.split('`').next())
            .unwrap();
        assert_eq!(warn(&format!("a: {suggestion}\n")), vec![], "{src:?}");
    }

    // Quoting settles it, and the 1.2 spellings are clean.
    for clean in [
        "a: 'yes'\n",
        "a: \"no\"\n",
        "mode: 755\n",
        "a: true\n",
        "z: 0\n",
        "f: 0.5\n",
        "n: 89\n",
        "n: 0x89\n",
        "n: '089'\n",
        "a: 1.0e+3\n",
        "a: 0x1F\n",
        "a: 1000\n",
        "a: 1.5\n",
        "a: 0b102\n",
        "a: 1:60\n",
        "a: '1e3'\n",
        "a: \"0b1010\"\n",
    ] {
        assert_eq!(warn(clean), vec![], "{clean:?}");
    }
}

/// Input that used to make the parser loop forever or run out of memory
/// because a loop did not make progress on an unexpected token.
#[test]
fn terminates_on_stray_tokens() {
    let unclosed_sequence = "Unclosed flow sequence. Expected: ']' to close sequence. \
        Found: end of input. Context: in flow sequence. Suggestion: Add ']' to close the \
        array, or check for missing commas between elements";
    let unclosed_mapping = "Unclosed flow mapping. Expected: '}' to close mapping. \
        Found: end of input. Context: in flow mapping. Suggestion: Add '}' to close the \
        object, or check for missing commas between key-value pairs";
    for (yaml, expected) in [
        // parse_flow_sequence did not make progress on `}`.
        (
            "[}9[Z",
            vec![
                "1:2: Unexpected token in flow sequence: \"}\"".to_string(),
                format!("1:6: {unclosed_sequence}"),
            ],
        ),
        // Nor parse_flow_mapping on `]`.
        (
            "{]",
            vec![
                "1:2: Unexpected token in flow mapping: \"]\"".to_string(),
                format!("1:3: {unclosed_mapping}"),
            ],
        ),
        // The multi-line explicit-key scalar loop, when the indented
        // continuation begins with a token parse_scalar() cannot consume
        // (e.g. COMMENT).
        ("?\u{18}\0\0\0\0\0\0\n\t#\t\t?\t", vec![]),
        // parse_explicit_key_mapping's continuation loop has to re-enter
        // explicit-key handling when current is `?` instead of calling
        // parse_mapping_key_value_pair (which cannot consume a `?`).
        (
            "<<?: ?:  \n<<\n ?:    : 0\n:@",
            vec!["2:1: Content after the document could not be parsed: \"<<\"".to_string()],
        ),
        // The mapping loop after a complex key, when is_mapping_key() is
        // fooled by `]:`.
        (
            "[%:%:]: \n]: \n\n",
            vec!["2:1: Unexpected token in mapping: \"]\"".to_string()],
        ),
        // parse_mapping_with_base_indent, when is_mapping_key() is fooled
        // by `}:` (or `]:`) appearing as a stray delimiter: the
        // missing-colon recovery inserts a synthetic token without
        // advancing the input.
        (
            "\0:\n\n}:\n\n{",
            vec![
                "3:1: Unexpected token in mapping: \"}\"".to_string(),
                "5:1: Content after the document could not be parsed: \"{\"".to_string(),
            ],
        ),
    ] {
        let expected: Vec<&str> = expected.iter().map(String::as_str).collect();
        parse_with_errors(yaml, &expected);
    }
}

/// The parser leaves undefined aliases, and a line indented under a plain
/// value, which continues it, to the validator.
#[test]
fn accepted_by_the_parser() {
    for yaml in [
        "key: *undefined",
        "key: value\n  invalid indentation for key",
    ] {
        parse_with_errors(yaml, &[]);
    }
    assert_strings(
        &parse("key: value\n  invalid indentation for key"),
        &[("key", "value invalid indentation for key")],
    );
}

#[test]
fn block_scalar_error_recovery() {
    // `bad_block: |` has no indented body, so it is an empty scalar and
    // `incomplete_key` is not its content. That bare line makes the rest
    // invalid YAML -- both saphyr and PyYAML reject this input -- and the
    // lines after it cannot be attached to the document at all.
    let yaml = r#"good_key: value
bad_block: |
incomplete_key
another_good: works
"#;
    parse_with_errors(
        yaml,
        &["3:1: Content after the document could not be parsed: \"incomplete_key\""],
    );
    assert_eq!(
        YamlFile::from_str(yaml).unwrap_err().to_string(),
        "Parse error at line 3, column 1: Content after the document could not be parsed: \
         \"incomplete_key\""
    );
}

/// Deep nesting is cut off with an error after the nesting limit, rather
/// than blowing the stack or memory.
#[test]
fn deep_nesting_is_cut_off() {
    let mut block = String::new();
    for i in 0..2000 {
        block.push_str(&"  ".repeat(i));
        block.push_str("k:\n");
    }
    block.push_str(&"  ".repeat(2000));
    block.push_str("v\n");
    for input in [
        "{".repeat(2000),
        "[".repeat(2000),
        block,
        format!("{}v", "&x ".repeat(2000)),
    ] {
        let parse = Parse::parse_yaml(&input);
        let _ = parse.tree();
        assert!(parse.has_errors());
    }
}

/// Past the nesting limit every key, value and sequence entry still
/// holds a node.
#[test]
fn deeply_nested_slots_hold_a_node() {
    for input in ["{".repeat(300), "{a: ".repeat(300), "[".repeat(300)] {
        let parse = yaml_edit::Parse::parse_yaml(&input);
        let empty: Vec<_> = parse
            .tree()
            .syntax()
            .descendants()
            .filter(|n| {
                matches!(
                    n.kind(),
                    SyntaxKind::KEY | SyntaxKind::VALUE | SyntaxKind::SEQUENCE_ENTRY
                ) && n.first_child().is_none()
            })
            .map(|n| n.kind())
            .collect();
        assert_eq!(empty, Vec::<SyntaxKind>::new(), "{input:?}");
    }
}

/// Top-level keys of `src`, in document order.
fn top_level_keys(src: &str) -> Vec<String> {
    YamlFile::from_str(src)
        .expect("should parse")
        .document()
        .expect("should have a document")
        .as_mapping()
        .expect("root should be a mapping")
        .keys()
        .map(|k| k.to_string())
        .collect()
}

/// The value of the single document in `src`, with a check that the
/// tree is well-formed and that the validator accepts it.
fn checked_value(src: &str) -> yaml_edit::YamlValue {
    let file = YamlFile::from_str(src).expect("should parse");
    debug::validate_tree(file.syntax()).expect("tree should be valid");
    let doc = file.document().expect("should have a document");
    assert_eq!(
        yaml_edit::validator::Validator::new().validate(&doc),
        vec![]
    );
    yaml_edit::YamlValue::from_document(&doc).expect("should convert")
}

/// What is left of a block entry's line after its node is more content
/// of the same slot, for the validator to report, rather than another
/// entry opening on that line, which left this one unterminated.
#[test]
fn rest_of_entry_line_stays_in_slot() {
    let unclosed = |kind: &str, position: &str, found: &str| {
        let (close, what, fix) = match kind {
            "mapping" => (
                '}',
                "mapping",
                "object, or check for missing commas between key-value pairs",
            ),
            _ => (
                ']',
                "sequence",
                "array, or check for missing commas between elements",
            ),
        };
        format!(
            "{position}: Unclosed flow {what}. Expected: '{close}' to close {what}. Found: {found}. \
             Context: in flow {what}. Suggestion: Add '{close}' to close the {fix}"
        )
    };
    let block_mapping = "Block mapping cannot start on same line as mapping key";
    let value_nodes = "Value holds more than one node";
    for (src, errors, violations) in [
        (": :", vec![], vec![block_mapping, value_nodes]),
        ("a: :\nb: 1\n", vec![], vec![block_mapping, value_nodes]),
        (
            "a: ]: x\n",
            vec![],
            vec![
                block_mapping,
                value_nodes,
                "Plain scalar cannot start with ']'",
            ],
        ),
        (
            "a: * c: x\n",
            vec![],
            vec![block_mapping, value_nodes, "Alias without a name"],
        ),
        (
            "a: {}{: x\n",
            vec![unclosed("mapping", "2:1", "end of input")],
            vec![
                value_nodes,
                "Flow collection continuation line must be indented",
            ],
        ),
        (
            "- {: -\n- b\n",
            vec![unclosed("mapping", "1:6", "-")],
            vec!["Sequence entry holds more than one node"],
        ),
        (
            "- [: -\n",
            vec![unclosed("sequence", "1:6", "-")],
            vec!["Sequence entry holds more than one node"],
        ),
        (
            ":\n * c: x\n",
            vec![],
            vec![value_nodes, "Alias without a name"],
        ),
        (
            ":\n|: x\n",
            vec![],
            vec![
                value_nodes,
                "Block scalar header must be indented past its parent node",
            ],
        ),
        ("?\n: >: x\n", vec![], vec![value_nodes]),
        ("? |b: x\n", vec![], vec!["Key holds more than one node"]),
    ] {
        let parse = YamlFile::parse(src);
        let file = parse.tree();
        assert_eq!(debug::validate_tree(file.syntax()), Ok(()), "{src:?}");
        assert_eq!(file.to_string(), src);
        assert_eq!(parse.errors(), errors, "{src:?}");
        let found: Vec<String> = file
            .documents()
            .flat_map(|d| Validator::new().validate(&d))
            .map(|v| v.message)
            .collect();
        assert_eq!(found, violations, "{src:?}");
    }
    // A `<<` with more on its line is one plain scalar, not a merge key.
    assert_eq!(
        checked_value("<< <<\n"),
        yaml_edit::YamlValue::Scalar(yaml_edit::ScalarValue::plain("<< <<"))
    );
    assert_eq!(top_level_keys("a: :\nb: 1\n"), ["a", "b"]);
    assert_eq!(top_level_keys("x:\n  a: :\n  b: 1\nc: 2\n"), ["x", "c"]);
    let parsed = YamlFile::from_str("a: :\nb: 1\n").unwrap();
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
      WHITESPACE: \" \"
      VALUE
        SCALAR
          NULL: \"\"
        MAPPING
          MAPPING_ENTRY
            KEY
              SCALAR
                NULL: \"\"
            COLON: \":\"
            VALUE
              SCALAR
                NULL: \"\"
            NEWLINE: \"\\\\n\"
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
"
    );
}

/// A key without its `:` is read as a key whose value was omitted, with
/// a zero-width COLON where the `:` should be, whatever the key sits in.
#[test]
fn missing_colon_reads_as_omitted_value() {
    let tree = |src: &str, errors: usize| {
        let parsed = YamlFile::parse(src);
        assert_eq!(
            parsed.errors().len(),
            errors,
            "{src:?}: {:?}",
            parsed.errors()
        );
        debug::tree_to_string(parsed.tree().syntax())
    };
    let explicit_entry = "    MAPPING_ENTRY
      QUESTION: \"?\"
      KEY
        SCALAR
          NULL: \"\"
      VALUE
        SCALAR
          NULL: \"\"
      NEWLINE: \"\\\\n\"
";
    // `<<` with no `:` is the plain scalar `<<`, not a merge key.
    assert_eq!(
        tree("<<\n", 0),
        "DOCUMENT\n  SCALAR\n    MERGE_KEY: \"<<\"\n  NEWLINE: \"\\\\n\"\n"
    );
    assert_eq!(
            tree("?\n<<\n", 1),
            format!(
                "DOCUMENT\n  MAPPING\n{explicit_entry}ERROR\n  MERGE_KEY: \"<<\"\n  NEWLINE: \"\\\\n\"\n"
            )
        );
    // An unclosed flow collection ends the input before the key's `:`.
    assert_eq!(
        tree("?\n[:\n", 2),
        format!(
            "\
DOCUMENT
  MAPPING
{explicit_entry}    MAPPING_ENTRY
      KEY
        SEQUENCE
          LEFT_BRACKET: \"[\"
          SEQUENCE_ENTRY
            MAPPING
              MAPPING_ENTRY
                KEY
                  SCALAR
                    NULL: \"\"
                COLON: \":\"
                VALUE
                  SCALAR
                    NULL: \"\"
            NEWLINE: \"\\\\n\"
          RIGHT_BRACKET: \"\"
      COLON: \"\"
      VALUE
        SCALAR
          NULL: \"\"
"
        )
    );
}

/// A line that is no entry of the mapping, like `<<` without its `:`,
/// does not swallow the lines after it: here, a document.
#[test]
fn missing_colon_keeps_following_document() {
    let parsed = YamlFile::parse("?\n<<\n---\na: 1\n");
    let docs: Vec<String> = parsed.tree().documents().map(|d| d.to_string()).collect();
    assert_eq!(docs, ["?\n", "---\na: 1\n"]);
}
