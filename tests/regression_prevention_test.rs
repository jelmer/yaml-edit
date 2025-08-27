//! Specific regression tests for the error recovery and parsing fixes.
//! These tests target the exact issues that were fixed to prevent regressions.

use std::str::FromStr;
use yaml_edit::{Parse, Yaml};

#[test]
fn test_anchor_in_nested_sequences() {
    // Test anchors work in deeply nested sequences
    let yaml_str = r#"
levels:
  - level1:
    - &anchor_item
      name: test
      value: 42
    - normal_item
  - level2:
    - *anchor_item
    - other_item
"#;

    let parsed = Parse::parse_yaml(yaml_str);
    assert!(
        !parsed.has_errors(),
        "Nested sequence anchors should parse without errors"
    );

    let yaml = parsed.tree();
    let output = yaml.to_string();
    assert!(output.contains("&anchor_item"));
    assert!(output.contains("*anchor_item"));
}

#[test]
fn test_anchor_with_complex_values() {
    // Test anchors with mappings and sequences as values
    let yaml_str = r#"
templates:
  - &complex_anchor
    database:
      host: localhost
      ports: [5432, 5433]
    settings:
      debug: true
  - simple_item
  - *complex_anchor
"#;

    let parsed = Parse::parse_yaml(yaml_str);
    assert!(
        !parsed.has_errors(),
        "Complex anchor values should parse correctly"
    );
}

#[test]
fn test_special_characters_no_false_positive_error_recovery() {
    // This is the exact pattern that caused the regression:
    // single quoted string with valid YAML escaping should NOT trigger error recovery
    let yaml_str = r#"
normal_key: value
escaped: 'it''s escaped'
another_key: value
"#;

    let parsed = Parse::parse_yaml(yaml_str);
    assert!(
        !parsed.has_errors(),
        "Valid single quoted strings should not trigger error recovery"
    );

    let yaml = Yaml::from_str(yaml_str);
    assert!(yaml.is_ok(), "Should parse valid single quoted strings");
}

#[test]
fn test_error_recovery_with_comments_various_positions() {
    // Test error recovery works with comments in different positions
    let test_cases = vec![
        // Comment after missing colon
        "key1 value1  # Missing colon\nkey2: value2",
        // Comment on same line as value
        "key1 value1  # comment\nkey2: value2",
        // Comment after key without value (newline triggers error)
        "key1\nkey2: value2",
        // Multiple comments
        "key1 value1  # comment1\n# comment2\nkey2: value2",
    ];

    for (i, yaml_str) in test_cases.iter().enumerate() {
        let parsed = Parse::parse_yaml(yaml_str);
        assert!(
            parsed.has_errors(),
            "Case {}: Should detect missing colon",
            i
        );

        let yaml = parsed.tree();
        if let Some(doc) = yaml.document() {
            if let Some(root) = doc.as_mapping() {
                assert!(
                    root.get("key2").is_some(),
                    "Case {}: Should still parse key2",
                    i
                );
            }
        }
    }
}

#[test]
fn test_error_recovery_nested_context() {
    // Test error recovery works in nested mappings
    let yaml_str = r#"
parent:
  child1:
    key1 value1  # Missing colon
    key2: value2
  child2:
    key3: value3
"#;

    let parsed = Parse::parse_yaml(yaml_str);
    assert!(parsed.has_errors(), "Should detect nested missing colon");

    // Should still be able to access the good parts
    let yaml = parsed.tree();
    if let Some(doc) = yaml.document() {
        if let Some(root) = doc.as_mapping() {
            assert!(root.get("parent").is_some(), "Should parse parent");
        }
    }
}

#[test]
fn test_combined_anchor_and_error_recovery() {
    // Test that anchors still work when error recovery is triggered
    let yaml_str = r#"
templates:
  - &tmpl
    name: template
    value: 42
broken_key missing_colon  # Error here
items:
  - *tmpl  # Anchor reference should still work
  - normal_item
"#;

    let parsed = Parse::parse_yaml(yaml_str);
    assert!(parsed.has_errors(), "Should detect missing colon");

    let yaml = parsed.tree();
    let output = yaml.to_string();
    // Anchor should still be preserved despite error
    assert!(output.contains("&tmpl"), "Anchor should be preserved");
    assert!(output.contains("*tmpl"), "Reference should be preserved");
}

#[test]
fn test_sequence_item_parsing_explicit_context() {
    // Test that sequence items are parsed with explicit context
    // This prevents error recovery from interfering with normal sequence parsing
    let yaml_str = r#"
items:
  - first item
  - "quoted item"
  - 'single quoted'
  - &anchor third item
  - *anchor
  - simple_item
"#;

    let parsed = Parse::parse_yaml(yaml_str);
    assert!(
        !parsed.has_errors(),
        "Sequence items should parse without triggering error recovery"
    );

    let yaml = parsed.tree();
    let output = yaml.to_string();
    assert!(output.contains("first item"));
    assert!(output.contains("&anchor"));
    assert!(output.contains("*anchor"));
}

#[test]
fn test_sequence_item_single_words() {
    // Test that sequence items with single words work correctly
    let yaml_str = r#"
items:
  - first
  - second
  - &anchor third
  - *anchor
"#;

    let parsed = Parse::parse_yaml(yaml_str);
    assert!(
        !parsed.has_errors(),
        "Single-word sequence items should parse correctly"
    );

    let yaml = parsed.tree();
    let output = yaml.to_string();
    assert!(output.contains("first"));
    assert!(output.contains("&anchor"));
    assert!(output.contains("*anchor"));
}

#[test]
fn test_sequence_item_with_mapping() {
    // Test that sequence items containing mappings work correctly
    let yaml_str = r#"
items:
  - item: with_colon
  - other_item: value
"#;

    let parsed = Parse::parse_yaml(yaml_str);
    assert!(
        !parsed.has_errors(),
        "Sequence items with mappings should parse correctly"
    );
}

#[test]
fn test_multiple_error_types_combination() {
    // Test handling multiple types of errors together
    let yaml_str = r#"
config:
  key1 value1  # Missing colon
  key2: "unterminated quote
  key3
  key4: valid_value
"#;

    let parsed = Parse::parse_yaml(yaml_str);
    assert!(parsed.has_errors(), "Should detect multiple error types");

    // Should still parse the valid parts
    let yaml = parsed.tree();
    if let Some(doc) = yaml.document() {
        if let Some(root) = doc.as_mapping() {
            assert!(
                root.get("config").is_some(),
                "Should parse config despite errors"
            );
        }
    }
}

#[test]
fn test_conservative_error_recovery_boundaries() {
    // Test that error recovery doesn't trigger on legitimate YAML constructs
    let legitimate_cases = vec![
        // Flow sequences with strings
        "items: [first, second, third]",
        // Flow mappings
        "config: {host: localhost, port: 8080}",
        // Multi-line strings
        "description: |\n  This is a\n  multi-line string",
        // Complex nested structures
        "data:\n  - item1\n  - item2: value\n  - item3",
    ];

    for (i, yaml_str) in legitimate_cases.iter().enumerate() {
        let parsed = Parse::parse_yaml(yaml_str);
        assert!(
            !parsed.has_errors(),
            "Case {}: Should not trigger error recovery on: {}",
            i,
            yaml_str
        );
    }
}
