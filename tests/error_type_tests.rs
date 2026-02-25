//! Specific error type tests
//!
//! Tests verify:
//! - Specific error types and messages
//! - Error position accuracy (line/column)
//! - Multiple errors in single document
//! - Error context and recovery

use yaml_edit::YamlFile;

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
    let yaml = "key1 value\nkey2: value";

    let parsed = YamlFile::parse(yaml);
    let errors = parsed.errors();

    // Plain scalars can contain spaces, so "key1 value" is valid as a key
    // with implicit null value. No errors expected.
    assert_eq!(errors.len(), 0, "Plain scalar with space is valid YAML");

    // Verify document exists
    let tree = parsed.tree();
    assert!(tree.document().is_some());
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
