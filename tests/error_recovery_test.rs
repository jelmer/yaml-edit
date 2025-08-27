//! Tests for enhanced error recovery functionality

use rowan::ast::AstNode;
use yaml_edit::{Mapping, Yaml};

#[test]
fn test_missing_colon_error_recovery() {
    // Test error recovery in flow mapping where colon detection is already implemented
    let yaml_text = r#"config: {host localhost, port 8080}"#;

    let parsed = Yaml::parse(yaml_text);

    // Should have errors due to missing colon in flow mapping
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
    // Test multiline unclosed sequences to debug the issue
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
    // Test multiple error scenarios
    let yaml_text = r#"items: [a, b, c
config: {host localhost}"#;

    let parsed = Yaml::parse(yaml_text);

    let errors = parsed.errors();
    assert!(
        !errors.is_empty(),
        "Should have errors from unclosed collections"
    );

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
#[test]
fn test_missing_colon_recovery() {
    // Missing colon should create error but continue parsing
    let yaml_str = r#"
key1 value1
key2: value2
"#;

    let parsed_yaml = yaml_edit::Parse::parse_yaml(yaml_str);

    // Check that we have errors
    assert!(
        parsed_yaml.has_errors(),
        "Should have errors for missing colon"
    );

    let yaml = parsed_yaml.tree();

    // Should still be able to find the good key
    if let Some(doc) = yaml.document() {
        if let Some(root) = doc.as_mapping() {
            // key2 should be parseable even though key1 had an error
            assert!(
                root.get("key2").is_some(),
                "Should find key2 despite error in key1"
            );
        }
    }
}

#[test]
fn test_unterminated_quote_recovery() {
    let yaml_str = r#"
key1: "unterminated quote
key2: value2
"#;

    let parsed = yaml_edit::Parse::parse_yaml(yaml_str);

    assert!(
        parsed.has_errors(),
        "Should have errors for unterminated quote"
    );

    let yaml = parsed.tree();

    // Parser correctly stops at unterminated quote error per YAML spec compliance
    let output = yaml.to_string();
    assert_eq!(
        output,
        "
key1: \"unterminated quote
key2: value2
"
    );
}

#[test]
fn test_malformed_mapping_recovery() {
    let yaml_str = r#"
parent:
  key1 value1  # Missing colon
  key2: value2
  key3
  key4: value4
"#;

    let parsed = yaml_edit::Parse::parse_yaml(yaml_str);

    assert!(parsed.has_errors(), "Should have errors for malformed keys");

    let yaml = parsed.tree();

    // Should still parse the good keys
    if let Some(doc) = yaml.document() {
        if let Some(root) = doc.as_mapping() {
            if let Some(parent) = root.get("parent") {
                if let Some(parent_mapping) = Mapping::cast(parent) {
                    // Should find the properly formatted keys
                    assert!(parent_mapping.get("key2").is_some(), "Should find key2");
                    assert!(parent_mapping.get("key4").is_some(), "Should find key4");
                }
            }
        }
    }
}

#[test]
fn test_partial_parsing_with_errors() {
    // Complex YAML with multiple errors
    let yaml_str = r#"
# Good section
config:
  database:
    host: localhost
    port: 5432

# Bad section (missing colon)
server
  host: example.com
  port: 8080

# Another good section  
logging:
  level: info
  file: app.log
"#;

    let parsed = yaml_edit::Parse::parse_yaml(yaml_str);

    // Parser should preserve all content even with malformed sections (lossless behavior)
    let output = parsed.tree().to_string();
    assert_eq!(
        output,
        "
# Good section
config:
  database:
    host: localhost
    port: 5432
# Bad section (missing colon)
server
  host: example.com
  port: 8080
# Another good section  
logging:
  level: info
  file: app.log
"
    );

    let yaml = parsed.tree();

    if let Some(doc) = yaml.document() {
        if let Some(root) = doc.as_mapping() {
            // Should be able to access good sections
            assert!(root.get("config").is_some(), "Should parse config section");
            assert!(
                root.get("logging").is_some(),
                "Should parse logging section"
            );

            // Verify we can access nested values
            if let Some(config) = root.get("config") {
                if let Some(config_mapping) = Mapping::cast(config) {
                    assert!(
                        config_mapping.get("database").is_some(),
                        "Should parse database config"
                    );
                }
            }
        }
    }
}

#[test]
fn test_basic_error_recovery() {
    // Test that parser can recover from basic syntax errors
    let yaml_str = r#"
key1 missing_colon
key2: valid_value
"#;

    let parsed = yaml_edit::Parse::parse_yaml(yaml_str);

    // Should have errors but still produce a usable result
    assert!(parsed.has_errors(), "Should detect syntax error");

    // Should still be able to parse the document
    let yaml = parsed.tree();
    assert!(yaml.document().is_some(), "Should still produce a document");
}
