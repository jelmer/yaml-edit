//! Parse errors, their messages and positions, and recovery from them.
//!
//! The parser is forgiving: on invalid input it must terminate, keep the
//! valid parts accessible and give a tree that edits still work on.

mod common;
use rowan::ast::AstNode;
use std::str::FromStr;
use yaml_edit::{debug, LineColumn, Parse, Sequence, SyntaxKind, YamlFile};

/// The validator's findings for `file` as (message, location) pairs.
fn violations(file: &YamlFile) -> Vec<(String, String)> {
    yaml_edit::validator::Validator::new()
        .validate_syntax(file.syntax())
        .into_iter()
        .map(|v| (v.message, v.location.unwrap()))
        .collect()
}

/// Parse `yaml`, check the errors reported for it, and that the tree still
/// holds a document and all of the text.
#[track_caller]
fn parse_with_errors(yaml: &str, expected: &[&str]) -> YamlFile {
    let parsed = YamlFile::parse(yaml);
    assert_eq!(parsed.errors(), expected, "{yaml:?}");
    let file = parsed.tree();
    debug::validate_tree(file.syntax()).unwrap();
    assert!(file.document().is_some(), "{yaml:?}");
    assert_eq!(file.to_string(), yaml);
    file
}

/// Parse `yaml`, which has no parse errors, and check what the validator
/// reports about it.
#[track_caller]
fn parse_with_violations(yaml: &str, expected: &[(&str, &str)]) -> YamlFile {
    let file = parse_with_errors(yaml, &[]);
    let expected: Vec<(String, String)> = expected
        .iter()
        .map(|(m, l)| (m.to_string(), l.to_string()))
        .collect();
    assert_eq!(violations(&file), expected, "{yaml:?}");
    file
}

/// The keys of the first document's root mapping.
fn keys(file: &YamlFile) -> Vec<String> {
    let doc = file.document().unwrap();
    let mapping = doc.as_mapping().unwrap();
    mapping.keys().map(|k| k.to_string()).collect()
}

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
