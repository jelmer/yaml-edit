//! Error recovery tests to verify no infinite loops

use yaml_edit::YamlFile;

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
    let expected = "name: John\nage: 30\nitems:\n  - apple\n  - banana\nconfig:\n  host: localhost\n  port: 8080\n";
    assert_eq!(content, expected);
}
