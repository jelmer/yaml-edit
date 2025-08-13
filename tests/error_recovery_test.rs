//! Tests for enhanced error recovery functionality

use yaml_edit::Yaml;

#[test]
fn test_missing_colon_error_recovery() {
    let yaml_text = r#"
name John
age: 30
"#;

    let parsed = Yaml::parse(yaml_text);

    // Should have errors due to missing colon
    let errors = parsed.errors();
    assert!(!errors.is_empty(), "Should detect missing colon error");

    // Should mention the issue in error messages
    let has_colon_error = errors.iter().any(|err| {
        err.contains("Missing colon") || err.contains("Expected ':'") || err.contains("':'")
    });
    assert!(
        has_colon_error,
        "Should mention missing colon: {:?}",
        errors
    );

    // Should still be able to get the tree (with partial structure)
    let yaml_tree = parsed.tree();
    if let Some(doc) = yaml_tree.document() {
        let content = doc.to_string();
        // Even with errors, should recover some content
        assert!(!content.trim().is_empty(), "Should recover some content");
    }
}

#[test]
fn test_unterminated_string_error_recovery() {
    let yaml_text = r#"name: "unterminated string
age: 30"#;

    let parsed = Yaml::parse(yaml_text);

    // Should have errors due to unterminated string
    let errors = parsed.errors();
    assert!(!errors.is_empty(), "Should detect unterminated string");

    // Look for string-related error messages
    let has_string_error = errors
        .iter()
        .any(|err| err.contains("Unterminated") || err.contains("quote") || err.contains("string"));
    assert!(
        has_string_error,
        "Should mention unterminated string issue: {:?}",
        errors
    );
}

#[test]
fn test_unclosed_flow_sequence_recovery() {
    let yaml_text = r#"items: [a, b, c
next: value"#;

    let parsed = Yaml::parse(yaml_text);

    // Should have errors due to unclosed sequence
    let errors = parsed.errors();
    assert!(!errors.is_empty(), "Should detect unclosed sequence");

    let has_sequence_error = errors
        .iter()
        .any(|err| err.contains("Unclosed") || err.contains("Expected ']'") || err.contains("]"));
    assert!(
        has_sequence_error,
        "Should mention unclosed sequence: {:?}",
        errors
    );
}

#[test]
fn test_unclosed_flow_mapping_recovery() {
    let yaml_text = r#"config: {host: localhost, port: 8080
server: running"#;

    let parsed = Yaml::parse(yaml_text);

    // Should have errors due to unclosed mapping
    let errors = parsed.errors();
    assert!(!errors.is_empty(), "Should detect unclosed mapping");

    let has_mapping_error = errors
        .iter()
        .any(|err| err.contains("Unclosed") || err.contains("Expected '}'") || err.contains("}"));
    assert!(
        has_mapping_error,
        "Should mention unclosed mapping: {:?}",
        errors
    );
}

#[test]
fn test_error_recovery_detailed_messages() {
    // Test that our enhanced error messages contain helpful information
    let yaml_text = r#"
name John
items: [a, b, c
config: {host localhost}
"#;

    let parsed = Yaml::parse(yaml_text);

    let errors = parsed.errors();
    assert!(!errors.is_empty(), "Should have multiple errors");

    // Join all error messages to check for helpful information
    let all_errors = errors.join(" ");

    // Should contain contextual information
    let has_context = all_errors.contains("in mapping")
        || all_errors.contains("in flow")
        || all_errors.contains("Context:");

    // Should contain suggestions
    let has_suggestions = all_errors.contains("Suggestion:")
        || all_errors.contains("Add")
        || all_errors.contains("check");

    println!("Error messages: {:?}", errors);

    // At least one of these should be present in enhanced error messages
    assert!(
        has_context || has_suggestions,
        "Should have contextual information or suggestions in errors"
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

    let parsed = Yaml::parse(yaml_text);

    // Should have no errors for valid YAML
    let errors = parsed.errors();
    assert!(
        errors.is_empty(),
        "Valid YAML should have no errors: {:?}",
        errors
    );

    let yaml_tree = parsed.tree();
    if let Some(doc) = yaml_tree.document() {
        let content = doc.to_string();
        assert!(content.contains("John"));
        assert!(content.contains("8080"));
        assert!(content.contains("apple"));
    }
}
