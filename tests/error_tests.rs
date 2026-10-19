//! Parse errors, their messages and positions, and recovery from them.
//!
//! The parser is forgiving: on invalid input it must terminate, keep the
//! valid parts accessible and give a tree that edits still work on.

mod common;
use rowan::ast::AstNode;
use std::str::FromStr;
use yaml_edit::{debug, LineColumn, Parse, Sequence, SyntaxKind, YamlFile};

/// Test unterminated quote error with exact message
#[test]
fn test_unterminated_quote_error() {
    let yaml = r#"name: "unclosed
age: 30"#;

    let parsed = YamlFile::parse(yaml);
    let errors = parsed.errors();

    assert_eq!(errors.len(), 1, "Should have exactly 1 error");
    assert_eq!(errors[0], "1:7: Unterminated quoted string");
}

/// Test unterminated single quote
#[test]
fn test_unterminated_single_quote() {
    let yaml = "name: 'unclosed\nage: 30";

    let parsed = YamlFile::parse(yaml);
    let errors = parsed.errors();

    assert_eq!(errors.len(), 1, "Should have exactly 1 error");
    assert_eq!(errors[0], "1:7: Unterminated quoted string");
}

/// Test invalid YAML directive
#[test]
fn test_invalid_directive() {
    let yaml = "%INVALID directive\nkey: value";

    let parsed = YamlFile::parse(yaml);

    // Parser should handle invalid directive
    assert!(parsed.tree().document().is_some());

    // Check if there are errors and their exact messages
    let errors = parsed.errors();
    if !errors.is_empty() {
        // Document the actual error message for invalid directives
        eprintln!("Invalid directive error: {}", errors[0]);
    }
}

/// Test invalid anchor name (starting with number)
#[test]
fn test_invalid_anchor_name() {
    let yaml = "&123invalid value";

    let parsed = YamlFile::parse(yaml);

    // Parser should handle this gracefully
    assert!(parsed.tree().document().is_some());

    let errors = parsed.errors();
    if !errors.is_empty() {
        eprintln!("Invalid anchor name error: {}", errors[0]);
    }
}

/// Test undefined alias reference
#[test]
fn test_undefined_alias_error() {
    let yaml = "ref: *undefined";

    let parsed = YamlFile::parse(yaml);

    // Parser accepts undefined alias (CST preserves it)
    assert!(parsed.tree().document().is_some());
    assert_eq!(
        parsed.errors().len(),
        0,
        "Undefined alias should parse without error in CST"
    );
}

/// Test duplicate anchor names
#[test]
fn test_duplicate_anchor_names() {
    let yaml = r#"first: &x value1
second: &x value2
ref: *x"#;

    let parsed = YamlFile::parse(yaml);

    // Duplicate anchors are allowed in YAML (last one wins)
    assert_eq!(
        parsed.errors().len(),
        0,
        "Duplicate anchors should parse without error"
    );
    assert!(parsed.tree().document().is_some());
}

/// Test error position accuracy for unclosed bracket
#[test]
fn test_error_position_unclosed_bracket() {
    let yaml = "line1: value\nline2: value\nbad: [";

    let parsed = YamlFile::parse(yaml);
    let errors = parsed.errors();

    assert_eq!(errors.len(), 1, "Should have exactly 1 error");
    assert_eq!(
        errors[0],
        "3:7: Unclosed flow sequence. Expected: ']' to close sequence. Found: end of input. Context: in flow sequence. Suggestion: Add ']' to close the array, or check for missing commas between elements"
    );
}

/// Test error position accuracy for unterminated string
#[test]
fn test_error_position_unterminated_string() {
    let yaml = r#"valid: "good string"
also_valid: 42
broken: "unclosed
still_broken: value"#;

    let parsed = YamlFile::parse(yaml);
    let errors = parsed.errors();

    assert_eq!(errors.len(), 1, "Should have exactly 1 error");
    assert_eq!(errors[0], "3:9: Unterminated quoted string");
}

/// Test that plain scalar with spaces is valid YAML
#[test]
fn test_plain_scalar_with_spaces_valid() {
    // A plain scalar may contain spaces, so `key1 value` is the document's
    // own scalar rather than a key. A mapping entry cannot follow one, which
    // both saphyr and PyYAML reject. The parser keeps the mapping as a
    // second node of the document, which the validator reports.
    let yaml = "key1 value\nkey2: value";

    let parsed = YamlFile::parse(yaml);
    assert_eq!(parsed.errors(), Vec::<String>::new());

    let tree = parsed.tree();
    assert_eq!(tree.to_string(), yaml);
    let violations: Vec<_> = yaml_edit::validator::Validator::new()
        .validate_syntax(rowan::ast::AstNode::syntax(&tree))
        .into_iter()
        .map(|v| (v.message, v.location.unwrap()))
        .collect();
    assert_eq!(
        violations,
        [(
            "Document holds more than one node".to_string(),
            "2:1".to_string()
        )]
    );
}

/// A plain scalar with spaces is a perfectly good mapping key.
#[test]
fn test_plain_scalar_with_spaces_as_a_key() {
    let yaml = "key1 value: 1\nkey2: value\n";

    let parsed = YamlFile::parse(yaml);
    assert_eq!(parsed.errors().len(), 0);
    assert_eq!(parsed.tree().to_string(), yaml);
}

/// Test multiple errors in single document
#[test]
fn test_multiple_errors_single_document() {
    let yaml = r#"bad1: [unclosed
bad2: {also_unclosed"#;

    let parsed = YamlFile::parse(yaml);
    let errors = parsed.errors();

    // Parser reports first error, then may stop or continue
    assert!(
        !errors.is_empty(),
        "Should have at least one error for malformed document"
    );

    // Should still produce a tree despite errors
    assert!(parsed.tree().document().is_some());
}

/// Test error recovery continues parsing after error
#[test]
fn test_error_recovery_continues_parsing() {
    let yaml = r#"valid1: value1
broken: [unclosed
valid2: value2
valid3: value3"#;

    let parsed = YamlFile::parse(yaml);

    // Should have error for unclosed bracket
    let errors = parsed.errors();
    assert_eq!(errors.len(), 1, "Should have 1 error");
    assert_eq!(
        errors[0],
        "4:15: Unclosed flow sequence. Expected: ']' to close sequence. Found: end of input. Context: in flow sequence. Suggestion: Add ']' to close the array, or check for missing commas between elements"
    );

    // Should still parse and produce a tree
    let tree = parsed.tree();
    assert!(tree.document().is_some());

    // Verify we can access valid keys (error recovery worked)
    let doc = tree.document().unwrap();
    let mapping = doc.as_mapping().expect("Should have mapping");
    assert!(
        mapping.keys().count() >= 2,
        "Should have parsed at least some keys"
    );
}

/// Test error context shows where in structure error occurred
#[test]
fn test_error_context_nested_structure() {
    let yaml = r#"valid:
  nested:
    deep:
      error: [unclosed"#;

    let parsed = YamlFile::parse(yaml);
    let errors = parsed.errors();

    assert_eq!(errors.len(), 1, "Should have exactly 1 error");
    assert_eq!(
        errors[0],
        "4:23: Unclosed flow sequence. Expected: ']' to close sequence. Found: end of input. Context: in flow sequence. Suggestion: Add ']' to close the array, or check for missing commas between elements"
    );
}

/// Test unclosed flow mapping error message
#[test]
fn test_unclosed_flow_mapping_error() {
    let yaml = "config: {host: localhost";

    let parsed = YamlFile::parse(yaml);
    let errors = parsed.errors();

    assert_eq!(errors.len(), 1, "Should have exactly 1 error");
    assert_eq!(
        errors[0],
        "1:25: Unclosed flow mapping. Expected: '}' to close mapping. Found: end of input. Context: in flow mapping. Suggestion: Add '}' to close the object, or check for missing commas between key-value pairs"
    );
}

/// Test unclosed flow sequence error message
#[test]
fn test_unclosed_flow_sequence_error() {
    let yaml = "items: [a, b, c";

    let parsed = YamlFile::parse(yaml);
    let errors = parsed.errors();

    assert_eq!(errors.len(), 1, "Should have exactly 1 error");
    assert_eq!(
        errors[0],
        "1:16: Unclosed flow sequence. Expected: ']' to close sequence. Found: end of input. Context: in flow sequence. Suggestion: Add ']' to close the array, or check for missing commas between elements"
    );
}

/// Test error message format consistency
#[test]
fn test_error_message_format() {
    let yaml = "items: [a, b";

    let parsed = YamlFile::parse(yaml);
    let errors = parsed.errors();

    assert_eq!(errors.len(), 1, "Should have exactly 1 error");
    assert_eq!(
        errors[0],
        "1:13: Unclosed flow sequence. Expected: ']' to close sequence. Found: end of input. Context: in flow sequence. Suggestion: Add ']' to close the array, or check for missing commas between elements"
    );
}

/// Test error in flow mapping with multiline content
#[test]
fn test_multiline_flow_mapping_error() {
    let yaml = r#"config: {
  host: localhost,
  port: 8080
"#;

    let parsed = YamlFile::parse(yaml);
    let errors = parsed.errors();

    assert_eq!(errors.len(), 1, "Should have exactly 1 error");
    assert_eq!(
        errors[0],
        "4:1: Unclosed flow mapping. Expected: '}' to close mapping. Found: end of input. Context: in flow mapping. Suggestion: Add '}' to close the object, or check for missing commas between key-value pairs"
    );

    // Should still produce a tree
    assert!(parsed.tree().document().is_some());
}

/// Test error in flow sequence with multiline content
#[test]
fn test_multiline_flow_sequence_error() {
    let yaml = r#"items: [
  first,
  second,
  third
"#;

    let parsed = YamlFile::parse(yaml);
    let errors = parsed.errors();

    assert_eq!(errors.len(), 1, "Should have exactly 1 error");
    assert_eq!(
        errors[0],
        "5:1: Unclosed flow sequence. Expected: ']' to close sequence. Found: end of input. Context: in flow sequence. Suggestion: Add ']' to close the array, or check for missing commas between elements"
    );

    // Should still produce a tree
    assert!(parsed.tree().document().is_some());
}

/// Test that valid YAML after error is still processed
#[test]
fn test_valid_content_after_error_processed() {
    let yaml = r#"start: value
broken: [
fixed: works
end: value"#;

    let parsed = YamlFile::parse(yaml);

    // Should have error for broken line
    assert_eq!(parsed.errors().len(), 1);

    // Should still produce a tree and process valid content
    let tree = parsed.tree();
    assert!(tree.document().is_some());

    let doc = tree.document().unwrap();
    let mapping = doc.as_mapping().expect("Should have mapping");
    // Parser attempts to process all lines despite error
    assert!(
        mapping.keys().count() >= 2,
        "Should have parsed multiple keys despite error"
    );
}

/// Test exact error format for missing closing brace in nested structure
#[test]
fn test_nested_unclosed_brace_exact_error() {
    let yaml = r#"outer:
  inner: {key: value"#;

    let parsed = YamlFile::parse(yaml);
    let errors = parsed.errors();

    assert_eq!(errors.len(), 1, "Should have exactly 1 error");
    assert_eq!(
        errors[0],
        "2:21: Unclosed flow mapping. Expected: '}' to close mapping. Found: end of input. Context: in flow mapping. Suggestion: Add '}' to close the object, or check for missing commas between key-value pairs"
    );
}

/// Test exact error format for missing closing bracket in nested structure
#[test]
fn test_nested_unclosed_bracket_exact_error() {
    let yaml = r#"outer:
  inner: [a, b, c"#;

    let parsed = YamlFile::parse(yaml);
    let errors = parsed.errors();

    assert_eq!(errors.len(), 1, "Should have exactly 1 error");
    assert_eq!(
        errors[0],
        "2:18: Unclosed flow sequence. Expected: ']' to close sequence. Found: end of input. Context: in flow sequence. Suggestion: Add ']' to close the array, or check for missing commas between elements"
    );
}

/// Test that plain scalar with spaces works as a key (issue #30).
#[test]
fn test_key_with_spaces_issue_30() {
    let yaml = "xyz: 1\nabc cba: 2\nfoo: 3\n";
    let parsed = YamlFile::parse(yaml);
    let errors = parsed.errors();
    assert_eq!(
        errors.len(),
        0,
        "Should parse without errors, got: {:?}",
        errors
    );

    let tree = parsed.tree();
    let doc = tree.document().expect("Should have document");
    let mapping = doc.as_mapping().expect("Should be a mapping");

    let keys: Vec<String> = mapping
        .iter()
        .map(|(k, _)| k.as_scalar().map(|s| s.as_string()).unwrap_or_default())
        .collect();

    assert_eq!(
        keys,
        vec!["xyz".to_string(), "abc cba".to_string(), "foo".to_string()]
    );

    // Round-trip: parsed tree must serialize back to the original input.
    use rowan::ast::AstNode;
    assert_eq!(tree.syntax().text().to_string(), yaml);
}

/// Multi-word keys should work in nested (indented) mappings too.
#[test]
fn test_multi_word_key_nested() {
    let yaml = "outer:\n  abc cba: 2\n  foo: 3\n";
    let parsed = YamlFile::parse(yaml);
    assert_eq!(parsed.errors(), Vec::<String>::new());
    let doc = parsed.tree().document().unwrap();
    let outer = doc.as_mapping().unwrap();
    let inner = outer.get("outer").unwrap();
    let inner_map = inner.as_mapping().expect("nested mapping");
    let val = inner_map
        .get("abc cba")
        .and_then(|n| n.as_scalar().cloned())
        .expect("multi-word nested key");
    assert_eq!(val.as_string(), "2");
}

/// A multi-word scalar with no colon must NOT be treated as a mapping.
#[test]
fn test_multi_word_scalar_without_colon_is_scalar() {
    let yaml = "abc cba\n";
    let parsed = YamlFile::parse(yaml);
    let doc = parsed.tree().document().unwrap();
    let scalar = doc.as_scalar().expect("should be a scalar, not mapping");
    assert_eq!(scalar.as_string(), "abc cba");
}

#[test]
fn test_all_problematic_cases_no_hangs() {
    // These cases previously caused infinite loops - verify they complete
    let test_cases = [
        // Multiline unclosed flow sequence
        "items: [a, b, c\nnext: value",
        // Multiline unclosed flow mapping
        "config: {host localhost\nother: value",
        // Mixed flow and block structures
        "flow: [a, b\nblock:\n  key: value",
        // Complex nested case
        "outer: [inner: {broken\nrescue: works",
        // Multiple structural issues
        "broken: [a, b\nmissing colon here\nvalid: works",
    ];

    for (i, yaml_text) in test_cases.iter().enumerate() {
        // This should complete without hanging
        let parsed = YamlFile::parse(yaml_text);

        // Should have some errors
        let errors = parsed.errors();
        assert!(!errors.is_empty(), "Case {} should have errors", i + 1);

        // Should still produce a tree (even if malformed)
        let tree = parsed.tree();
        assert!(
            tree.document().is_some(),
            "Case {} should produce a document",
            i + 1
        );
    }
}

#[test]
fn test_deeply_nested_malformed_structures() {
    // Test deeply nested malformed structures don't cause stack overflow or hangs
    let yaml_text = r#"
level1: [
  level2: {
    level3: [broken structure here
    level3b: another broken
  level2b: {missing colon
level1b: value
"#;

    let parsed = YamlFile::parse(yaml_text);
    let errors = parsed.errors();

    // Should complete and report errors (important part is that it doesn't hang)
    assert!(!errors.is_empty());

    // Should still produce a tree
    let tree = parsed.tree();
    assert!(tree.document().is_some());
}

// Tests from error_recovery_test.rs

#[test]
fn test_flow_mapping_with_implicit_null_values() {
    // Per YAML 1.2 spec section 7.3.3, plain scalars in flow context CAN contain spaces
    // So {host localhost, port 8080} is valid YAML with multi-word keys and implicit null values
    // This is confirmed by PyYAML: {'host localhost': None, 'port 8080': None}
    let yaml_text = r#"config: {host localhost, port 8080}"#;

    let parsed = YamlFile::parse(yaml_text);

    // This is valid YAML per spec, should parse without errors
    let errors = parsed.errors();
    assert_eq!(errors, Vec::<String>::new());

    // Verify structure via API
    let yaml_tree = parsed.tree();
    let doc = yaml_tree.document().unwrap();
    let mapping = doc.as_mapping().expect("Should be mapping");

    // Verify the config key exists and is a mapping with the expected keys
    let config = mapping
        .get_mapping("config")
        .expect("Should have config key");
    assert_eq!(config.keys().count(), 2, "Config should have 2 keys");

    // Verify round-trip produces valid YAML
    let output = doc.to_string();
    let reparsed = YamlFile::parse(&output);
    assert_eq!(
        reparsed.errors().len(),
        0,
        "Round-trip should be valid YAML"
    );
}

#[test]
fn test_unterminated_string_error_recovery() {
    let yaml_text = r#"name: "unterminated string
age: 30"#;

    let parsed = YamlFile::parse(yaml_text);

    // Should have errors due to unterminated string
    let errors = parsed.errors();
    assert_eq!(errors.len(), 1);

    // Check exact error message
    assert_eq!(errors[0], "1:7: Unterminated quoted string");
}

#[test]
fn test_unclosed_flow_sequence_recovery() {
    let yaml_text = r#"items: [a, b, c
next: value"#;

    let parsed = YamlFile::parse(yaml_text);

    // Should have errors due to unclosed sequence
    let errors = parsed.errors();
    assert_eq!(errors.len(), 1);

    // Check exact error message
    assert_eq!(
        errors[0],
        "2:12: Unclosed flow sequence. Expected: ']' to close sequence. Found: end of input. Context: in flow sequence. Suggestion: Add ']' to close the array, or check for missing commas between elements"
    );
}

#[test]
fn test_unclosed_flow_mapping_recovery() {
    let yaml_text = r#"config: {host: localhost, port: 8080
server: running"#;

    let parsed = YamlFile::parse(yaml_text);

    // Should have errors due to unclosed mapping
    let errors = parsed.errors();
    assert_eq!(errors.len(), 1);

    // Check exact error message
    assert_eq!(
        errors[0],
        "2:16: Unclosed flow mapping. Expected: '}' to close mapping. Found: end of input. Context: in flow mapping. Suggestion: Add '}' to close the object, or check for missing commas between key-value pairs"
    );
}

#[test]
fn test_error_recovery_detailed_messages() {
    // Test that our enhanced error messages contain helpful information
    let yaml_text = r#"items: [a, b, c
config: {host localhost}"#;

    let parsed = YamlFile::parse(yaml_text);

    let errors = parsed.errors();
    assert_eq!(errors.len(), 1);

    // Check exact error message with context and suggestions
    assert_eq!(
        errors[0],
        "2:25: Unclosed flow sequence. Expected: ']' to close sequence. Found: end of input. Context: in flow sequence. Suggestion: Add ']' to close the array, or check for missing commas between elements"
    );
}

#[test]
fn test_simple_valid_yaml_still_works() {
    // Ensure our error recovery doesn't break normal parsing
    let yaml_text = r#"
name: John
age: 30
items:
  - apple
  - banana
config:
  host: localhost
  port: 8080
"#;

    let parsed = YamlFile::parse(yaml_text);

    // Should have no errors for valid YAML
    let errors = parsed.errors();
    assert_eq!(errors, Vec::<String>::new());

    let yaml_tree = parsed.tree();
    let doc = yaml_tree.document().unwrap();
    let content = doc.to_string();
    let expected = "\nname: John\nage: 30\nitems:\n  - apple\n  - banana\nconfig:\n  host: localhost\n  port: 8080\n";
    assert_eq!(content, expected);
}

fn parsed_tree(text: &str) -> String {
    use rowan::ast::AstNode;
    let parsed = YamlFile::parse(text);
    let root = parsed.tree().syntax().clone();
    yaml_edit::debug::validate_tree(&root).unwrap();
    yaml_edit::debug::tree_to_string(&root)
}

#[test]
fn test_unclosed_flow_sequence_pair_at_end_of_input() {
    assert_eq!(
        parsed_tree("[c:"),
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
"#
    );
}

#[test]
fn test_unclosed_flow_mapping_pair_at_end_of_input() {
    assert_eq!(
        parsed_tree("{c:"),
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
"#
    );
}

#[test]
fn test_stray_token_in_block_mapping() {
    let parsed = YamlFile::parse("[]:\n]:");
    assert_eq!(
        parsed.errors(),
        vec!["2:1: Unexpected token in mapping: \"]\"".to_string()]
    );
    assert_eq!(
        parsed_tree("[]:\n]:"),
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
"#
    );
}

#[test]
fn test_stray_token_in_explicit_key_mapping() {
    let parsed = YamlFile::parse("?\n] ?:");
    assert_eq!(
        parsed.errors(),
        vec!["2:1: Unexpected token in explicit-key mapping: \"]\"".to_string()]
    );
    assert_eq!(
        parsed_tree("?\n] ?:"),
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
"#
    );
}

#[test]
fn test_stray_token_in_flow_sequence() {
    let parsed = YamlFile::parse("[a, }]");
    assert_eq!(
        parsed.errors(),
        vec!["1:5: Unexpected token in flow sequence: \"}\"".to_string()]
    );
    assert_eq!(
        parsed_tree("[a, }]"),
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
"#
    );
}

#[test]
fn test_unclosed_flow_mapping_empty_explicit_key() {
    assert_eq!(
        parsed_tree("{?"),
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
"#
    );
}

#[test]
fn test_basic_error_reporting() {
    let yaml = "key: value";
    let parse = Parse::parse_yaml(yaml);

    // Should have no errors for valid YAML
    assert!(parse.errors().is_empty());

    // Tree should be valid
    let doc = parse.tree();
    assert_eq!(doc.to_string(), "key: value");
}

#[test]
fn test_error_with_line_info() {
    // Use unclosed quote as a clear syntax error
    let yaml = r#"
key1: value1
key2: "unclosed quote
key3: value3
"#;

    let parse = Parse::parse_yaml(yaml);

    // Should have errors for syntax error
    let errors = parse.errors();
    assert_eq!(errors.len(), 1);
    assert_eq!(errors[0], "3:7: Unterminated quoted string");

    // Should have positioned errors
    let positioned = parse.positioned_errors();
    assert_eq!(positioned.len(), 1);
    assert_eq!(positioned[0].message, "Unterminated quoted string");
    assert_eq!(positioned[0].start_position(yaml), LineColumn::new(3, 7));

    assert_eq!(
        parse.to_result().unwrap_err().to_string(),
        "Parse error at line 3, column 7: Unterminated quoted string"
    );
}

#[test]
fn test_invalid_colon_in_plain_scalar() {
    // According to YAML spec, a plain scalar cannot contain ": " (colon followed by space)
    // However, our parser uses degraded parsing and treats "key: value on same line"
    // as a plain scalar value (forgiving interpretation)
    let yaml = r#"not_url_2: key: value on same line"#;

    let parsed = YamlFile::from_str(yaml).expect("Parser uses degraded parsing");
    let doc = parsed.document().expect("Should have document");
    let mapping = doc.as_mapping().expect("Should be mapping");

    // Verify structure: single key "not_url_2" with scalar value
    assert_eq!(mapping.keys().count(), 1);

    let keys: Vec<_> = mapping.keys().collect();
    assert_eq!(keys[0], "not_url_2");

    let value = mapping.get("not_url_2").expect("Should have value");
    let scalar = value.as_scalar().expect("Value should be scalar");
    assert_eq!(scalar.as_string(), "key: value on same line");

    // Verify round-trip preserves input
    assert_eq!(doc.to_string(), yaml);
}

#[test]
fn test_invalid_bracket_colon_combination() {
    // `ipv6: [::1]:8080` without quotes is invalid YAML overall (a flow
    // sequence followed by unexpected text). Degraded parsing recovers
    // the [::1] part as a flow sequence with one plain-scalar item
    // "::1" (colon-not-followed-by-whitespace is scalar content per
    // YAML 1.2), and keeps the `:8080` tail in the value as a second
    // node, which the validator reports.
    let yaml = r#"ipv6: [::1]:8080"#;

    let parsed = YamlFile::parse(yaml);
    assert_eq!(parsed.errors(), Vec::<String>::new());
    let parsed = parsed.tree();
    let violations: Vec<_> = yaml_edit::validator::Validator::new()
        .validate_syntax(rowan::ast::AstNode::syntax(&parsed))
        .into_iter()
        .map(|v| (v.message, v.location.unwrap()))
        .collect();
    assert_eq!(
        violations,
        [(
            "Value holds more than one node".to_string(),
            "1:12".to_string()
        )]
    );
    let doc = parsed.document().expect("Should have document");
    let mapping = doc.as_mapping().expect("Should be mapping");

    assert_eq!(mapping.keys().count(), 1);
    let keys: Vec<_> = mapping.keys().collect();
    assert_eq!(keys[0], "ipv6");

    let value = mapping.get("ipv6").expect("Should have value");
    let seq = value.as_sequence().expect("Value should be sequence");
    assert_eq!(seq.len(), 1);
    let elem = seq.get(0).expect("Should have element");
    let scalar = elem.as_scalar().expect("Element should be scalar");
    assert_eq!(scalar.as_string(), "::1");

    assert_eq!(doc.to_string(), yaml);
}

#[test]
fn test_multiple_colons_in_value() {
    // Test various cases with colons in values
    let test_cases = vec![
        ("simple", "key: value", true),                  // Valid
        ("with_colon", "url: http://example.com", true), // Valid
        ("with_port", "url: example.com:8080", true),    // Valid
        ("invalid_space", "bad: key: value", false),     // Invalid - colon+space in value
        ("no_space", "good: key:value", true),           // Valid - colon without space after
        ("quoted", "quoted: \"key: value\"", true),      // Valid - quoted
    ];

    for (name, yaml, should_be_valid) in test_cases {
        let parsed = YamlFile::from_str(yaml);

        if should_be_valid {
            assert!(
                parsed.is_ok(),
                "{}: '{}' should parse successfully",
                name,
                yaml
            );

            // Verify we get the expected structure
            if let Ok(yaml_doc) = parsed {
                if let Some(doc) = yaml_doc.document() {
                    if let Some(mapping) = doc.as_mapping() {
                        assert_eq!(
                            mapping.keys().count(),
                            1,
                            "{}: Should have exactly 1 key",
                            name
                        );
                    }
                }
            }
        } else {
            // For invalid cases, we accept either:
            // 1. Parse error
            // 2. Parsing as nested structure (degraded parsing)
            match parsed {
                Ok(yaml_doc) => {
                    // Degraded parsing - might create nested mappings
                    if let Some(_doc) = yaml_doc.document() {
                        // As long as it doesn't crash, this is acceptable (degraded parsing)
                    }
                }
                Err(_e) => {
                    // This is also fine - proper error reporting as expected
                }
            }
        }
    }
}

#[test]
fn test_error_recovery() {
    // Test that parser can recover from errors and continue
    let yaml = r#"
good_key: good_value
bad: key: with: many: colons
another_good: value
"#;

    let parsed = YamlFile::from_str(yaml);

    // The parser should handle this somehow
    match parsed {
        Ok(yaml_doc) => {
            if let Some(doc) = yaml_doc.document() {
                if let Some(mapping) = doc.as_mapping() {
                    // Should at least parse the good keys
                    let has_good_key = mapping.keys().any(|k| k == "good_key");
                    let has_another_good = mapping.keys().any(|k| k == "another_good");

                    assert!(
                        has_good_key || has_another_good,
                        "Should parse at least some valid keys"
                    );
                }
            }
        }
        Err(_) => {
            // Full parse failure is also acceptable for invalid input
            // The important thing is it doesn't crash or hang
        }
    }
}

/// A node has at most one tag; the parser keeps a second one as a tagged
/// node of its own, which the validator reports.
#[test]
fn test_two_tags_on_one_node() {
    for (yaml, location) in [
        ("!a !b\n- x\n", "1:1"),
        ("k: !a !b x\n", "1:4"),
        ("- !a\n  !b\n  - x\n", "1:3"),
    ] {
        let parsed = YamlFile::parse(yaml);
        assert_eq!(parsed.errors(), Vec::<String>::new());
        let violations: Vec<_> = yaml_edit::validator::Validator::new()
            .validate_syntax(rowan::ast::AstNode::syntax(&parsed.tree()))
            .into_iter()
            .map(|v| (v.message, v.location.unwrap()))
            .collect();
        assert_eq!(
            violations,
            [(
                "Multiple tags on the same node".to_string(),
                location.to_string()
            )],
            "{yaml:?}"
        );
    }
}

/// A key the parser recovered without its `:` gets one when its value is set.
#[test]
fn test_set_value_of_key_missing_colon() {
    fn set_last(src: &str, value: impl yaml_edit::AsYaml) -> String {
        let doc = YamlFile::parse(src).tree().document().unwrap();
        let entry = doc.as_mapping().unwrap().entries().last().unwrap();
        entry.set_value(value, false);
        yaml_edit::debug::validate_tree(rowan::ast::AstNode::syntax(&doc)).unwrap();
        doc.to_string()
    }
    let block = YamlFile::from_str("k:\n  a: 1\n").unwrap();
    let block = block
        .document()
        .unwrap()
        .as_mapping()
        .unwrap()
        .get("k")
        .unwrap();
    for (src, inline, nested) in [
        ("<<\n", "<<: x\n", "<<:\n  a: 1\n"),
        ("?\n<<\n", "?\n<<: x\n", "?\n<<:\n  a: 1\n"),
        ("a: 1\n<< \n", "a: 1\n<< : x\n", "a: 1\n<< :\n  a: 1\n"),
    ] {
        assert_eq!(set_last(src, "x"), inline, "{src:?}");
        assert_eq!(set_last(src, block.clone()), nested, "{src:?}");
    }
}

#[test]
fn test_preserve_invalid_flow_block_mix() {
    // This is INVALID YAML according to the spec:
    // You cannot have a flow mapping {} followed by block-style entries
    let invalid_yaml = r#"---
{}
key1: value1
key2: value2"#;

    // The entries after {} cannot be attached to it. The parser keeps them
    // as a second node of the document, which the validator reports, and
    // the text is preserved for lossless editing.
    let parsed = YamlFile::parse(invalid_yaml);
    assert_eq!(parsed.errors(), Vec::<String>::new());
    assert_eq!(parsed.tree().to_string(), invalid_yaml);
    let violations: Vec<_> = yaml_edit::validator::Validator::new()
        .validate_syntax(parsed.tree().syntax())
        .into_iter()
        .map(|v| (v.message, v.location.unwrap()))
        .collect();
    assert_eq!(
        violations,
        [(
            "Document holds more than one node".to_string(),
            "3:1".to_string()
        )]
    );
}

#[test]
fn test_set_with_field_order_on_invalid_flow_block_mix() {
    // Start with invalid YAML that has both flow and block style
    let invalid_yaml = r#"---
{}
key1: value1"#;

    let yaml = YamlFile::parse(invalid_yaml).tree();

    if let Some(doc) = yaml.document() {
        if let Some(mapping) = doc.as_mapping() {
            // Add a new key - since the mapping is {}, the new key is added there
            // The invalid content after {} is preserved as a second node
            mapping.set("key2", "value2");
            debug::validate_tree(yaml.syntax()).unwrap();
        }
    }

    let result = yaml.to_string();

    // Setting into a flow mapping now inserts inside its `{}` (was: appended
    // a broken block-style entry after `}`). The trailing invalid block
    // content is still preserved.
    assert_eq!(result, "---\n{key2: value2}\nkey1: value1");
}

#[test]
fn test_operations_preserve_invalid_structure() {
    // Test that operations on the mapping {} preserve the invalid ERROR content
    let invalid_yaml = r#"---
{}
name: test
version: 1.0"#;

    let yaml = YamlFile::parse(invalid_yaml).tree();

    // Initially, the invalid content is preserved as-is
    assert_eq!(yaml.to_string(), invalid_yaml);

    if let Some(doc) = yaml.document() {
        if let Some(mapping) = doc.as_mapping() {
            // The mapping is empty {} - operations on it don't affect the
            // mapping after it, "name: test\nversion: 1.0"

            // Add a key to the empty mapping
            mapping.set("author", "John");
            debug::validate_tree(yaml.syntax()).unwrap();

            // The new key is added inside the `{}`; invalid trailing block
            // content stays where it was.
            let expected = "---\n{author: John}\nname: test\nversion: 1.0";
            assert_eq!(yaml.to_string(), expected);
        }
    }
}

#[test]
fn test_reparse_invalid_yaml_loses_data() {
    // This test demonstrates lossless round-tripping even for invalid YAML
    let invalid_yaml = r#"---
{}
key1: value1
key2: value2"#;

    let yaml = YamlFile::parse(invalid_yaml).tree();
    let result = yaml.to_string();

    // Our parser preserves everything for lossless editing
    assert_eq!(result, invalid_yaml);

    // Reparse with our parser - still preserves everything
    let reparsed = YamlFile::parse(&result).tree();

    // Lossless round-trip: parse -> serialize -> parse -> serialize
    assert_eq!(reparsed.to_string(), invalid_yaml);

    // The mapping {} is empty, but invalid content is preserved after it
    if let Some(doc) = reparsed.document() {
        if let Some(mapping) = doc.as_mapping() {
            let keys: Vec<_> = mapping.keys().collect();
            // The mapping is empty {} - invalid content is in a second node
            assert_eq!(keys.len(), 0);
        }
    }
}

#[test]
fn test_valid_yaml_alternative() {
    // Test the valid alternative - no flow mapping
    let valid_yaml = r#"---
key1: value1
key2: value2"#;

    let yaml = YamlFile::from_str(valid_yaml).unwrap();
    assert_eq!(yaml.to_string(), valid_yaml);

    // This can be correctly reparsed
    let reparsed = YamlFile::from_str(&yaml.to_string()).unwrap();
    assert_eq!(reparsed.to_string(), valid_yaml);

    if let Some(doc) = reparsed.document() {
        if let Some(mapping) = doc.as_mapping() {
            let keys: Vec<_> = mapping.keys().collect();
            assert_eq!(keys, vec!["key1", "key2"]);
        }
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
fn test_remove_last_item_of_sequence_with_two_tags() {
    let file = YamlFile::from_str("!a !b\n- x\n").unwrap();
    nth_sequence(&file, 0).remove(0);
    assert_eq!(file.to_string(), "!a !b\n[]\n");
}

#[test]
fn test_remove_last_item_of_value_with_two_tags() {
    let file = YamlFile::from_str("k: !a !b\n  - x\n").unwrap();
    nth_sequence(&file, 0).remove(0);
    assert_eq!(file.to_string(), "k: !a !b []\n");
}

#[test]
fn test_refill_value_with_two_tags() {
    let file = YamlFile::from_str("k: !a !b\n  - x\n").unwrap();
    let seq = nth_sequence(&file, 0);
    seq.remove(0);
    seq.push("y");
    assert_eq!(file.to_string(), "k: !a !b\n  - y\n");
}

#[test]
fn test_remove_last_item_of_block_sequence_in_implicit_key() {
    let file =
        YamlFile::from_str("- [a, b ,!str\n-\n  !!null : a\n  b: !!str\n- !!s     ]\n").unwrap();
    nth_sequence(&file, 2).remove(0);
    assert_eq!(
        file.to_string(),
        "- [a, b ,!str\n[]: a\n  b: !!str\n- !!s     ]\n"
    );
}

#[test]
fn test_refill_root_sequence_followed_by_mapping() {
    let file = YamlFile::from_str(" - x\nb: 1").unwrap();
    let seq = nth_sequence(&file, 0);
    seq.remove(0);
    assert_eq!(file.to_string(), " []\nb: 1");
    seq.insert(0, "y");
    common::assert_file_edited(&file, " - y\nb: 1");
}

#[test]
fn test_clear_sequence_with_anchor_text_in_scalar() {
    // `&SS` is part of the plain scalar, so `*SS` refers to nothing.
    let file = YamlFile::from_str("- f\n  - &SS Sammy\nrb: *SS\n").unwrap();
    nth_sequence(&file, 0).clear();
    assert_eq!(file.to_string(), "[]\nrb: *SS\n");
}
