//! Tests for mid-line comments in flow collections
//!
//! These tests verify that comments inside flow collections (sequences and mappings)
//! are correctly parsed and preserved during round-trip operations.

use std::str::FromStr;
use yaml_edit::YamlFile;

#[test]
fn test_flow_sequence_with_comments() {
    // Comments after elements in flow sequences
    let yaml = "[a, # comment\n b]";
    let parsed = YamlFile::from_str(yaml).unwrap();
    assert_eq!(parsed.to_string(), yaml);

    let doc = parsed.document().unwrap();
    let seq = doc.as_sequence().unwrap();
    assert_eq!(seq.len(), 2);
}

#[test]
fn test_flow_mapping_with_comments() {
    // Comments after elements in flow mappings
    let yaml = "{key1: value1, # comment\n key2: value2}";
    let parsed = YamlFile::from_str(yaml).unwrap();
    assert_eq!(parsed.to_string(), yaml);

    let doc = parsed.document().unwrap();
    let map = doc.as_mapping().unwrap();
    assert_eq!(map.len(), 2);
}

#[test]
fn test_multiple_comments_in_flow() {
    // Multiple comments in flow sequence
    let yaml = "[a, # c1\n b, # c2\n c]";
    let parsed = YamlFile::from_str(yaml).unwrap();
    assert_eq!(parsed.to_string(), yaml);

    let doc = parsed.document().unwrap();
    let seq = doc.as_sequence().unwrap();
    assert_eq!(seq.len(), 3);
}

#[test]
fn test_comment_before_closing_bracket() {
    // Comment before closing bracket
    let yaml = "[a, b # comment\n]";
    let parsed = YamlFile::from_str(yaml).unwrap();
    assert_eq!(parsed.to_string(), yaml);
}

#[test]
fn test_comment_after_colon() {
    // Comment after colon in flow mapping
    let yaml = "{key: # comment\n value}";
    let parsed = YamlFile::from_str(yaml).unwrap();
    assert_eq!(parsed.to_string(), yaml);
}

#[test]
fn test_comment_after_opening_bracket() {
    // Comment after opening bracket
    let yaml = "[# comment\na, b]";
    let parsed = YamlFile::from_str(yaml).unwrap();
    assert_eq!(parsed.to_string(), yaml);
}

#[test]
fn test_comment_after_opening_brace() {
    // Comment after opening brace
    let yaml = "{# comment\nkey: value}";
    let parsed = YamlFile::from_str(yaml).unwrap();
    assert_eq!(parsed.to_string(), yaml);
}

#[test]
fn test_comment_immediately_after_comma() {
    // Comment immediately after comma (no space)
    let yaml = "[a,# comment\nb]";
    let parsed = YamlFile::from_str(yaml).unwrap();
    assert_eq!(parsed.to_string(), yaml);
}

#[test]
fn test_nested_flow_with_comments() {
    // Nested flow collections with comments
    let yaml = "[[a,# c1\nb],# c2\nc]";
    let parsed = YamlFile::from_str(yaml).unwrap();
    assert_eq!(parsed.to_string(), yaml);
}

#[test]
fn test_multiline_flow_with_comments() {
    // Multi-line flow collections with proper indentation and comments
    let yaml = r#"flow_sequence: [
    item1,  # Comment 1
    item2,  # Comment 2
    item3   # Comment 3
]
flow_mapping: {
    key1: value1,  # Comment A
    key2: value2,  # Comment B
    key3: value3   # Comment C
}"#;

    let parsed = YamlFile::from_str(yaml).unwrap();
    let output = parsed.to_string();

    // Verify exact round-trip preservation
    assert_eq!(output, yaml);

    // Verify structure
    let doc = parsed.document().unwrap();
    let root = doc.as_mapping().unwrap();
    assert_eq!(root.len(), 2);
}

#[test]
fn test_yaml_spec_example_with_flow_comments() {
    // Based on YAML spec example 6.1 (test 6HB6)
    let yaml = r#"Flow style: [    # Leading spaces
   By two,        # in flow style
  Also by two,    # are neither
  	Still by two   # content nor
    ]             # indentation."#;

    let parsed = YamlFile::from_str(yaml).unwrap();
    let output = parsed.to_string();

    // Verify exact round-trip preservation
    assert_eq!(output, yaml);
}

#[test]
fn test_flow_comments_api_access() {
    let yaml = r#"flow_sequence: [
    item1,  # Comment 1
    item2,  # Comment 2
    item3   # Comment 3
]
flow_mapping: {
    key1: value1,  # Comment A
    key2: value2,  # Comment B
    key3: value3   # Comment C
}"#;

    let parsed = YamlFile::from_str(yaml).unwrap();
    let doc = parsed.document().unwrap();
    let root = doc.as_mapping().unwrap();

    // Test API access to flow sequence with comments
    let seq_value = root
        .get("flow_sequence")
        .expect("Should have flow_sequence key");
    let seq = seq_value
        .as_sequence()
        .expect("flow_sequence should be a sequence");

    assert_eq!(seq.len(), 3);

    // Use the values() method which returns an iterator
    let items: Vec<String> = seq
        .values()
        .map(|item| item.as_scalar().unwrap().as_string())
        .collect();

    assert_eq!(items[0], "item1");
    assert_eq!(items[1], "item2");
    assert_eq!(items[2], "item3");

    // Test API access to flow mapping with comments
    let map_value = root
        .get("flow_mapping")
        .expect("Should have flow_mapping key");
    let map = map_value
        .as_mapping()
        .expect("flow_mapping should be a mapping");

    assert_eq!(map.len(), 3);

    let val1 = map.get("key1").expect("Should have key1");
    assert_eq!(val1.as_scalar().unwrap().as_string(), "value1");

    let val2 = map.get("key2").expect("Should have key2");
    assert_eq!(val2.as_scalar().unwrap().as_string(), "value2");

    let val3 = map.get("key3").expect("Should have key3");
    assert_eq!(val3.as_scalar().unwrap().as_string(), "value3");
}

// ========================================
// Comment Edge Cases
// ========================================

#[test]
fn test_comment_with_only_whitespace() {
    // Comment with only whitespace after # character
    // YAML spec allows empty comments
    let yaml = "key: value  #  \n";

    let parsed = YamlFile::from_str(yaml).unwrap();
    let doc = parsed.document().unwrap();
    let mapping = doc.as_mapping().unwrap();

    // Verify exact content
    let key_val = mapping.get("key").unwrap();
    assert_eq!(key_val.as_scalar().unwrap().as_string(), "value");

    // Verify exact round-trip
    let output = parsed.to_string();
    assert_eq!(output, yaml);
}

#[test]
fn test_comment_at_eof_no_trailing_newline() {
    // Comment at end of file with no trailing newline
    // Tests EOF handling in lexer
    let yaml = "key: value\n# Final comment";

    let parsed = YamlFile::from_str(yaml).unwrap();
    let doc = parsed.document().unwrap();
    let mapping = doc.as_mapping().unwrap();

    // Verify exact content
    let key_val = mapping.get("key").unwrap();
    assert_eq!(key_val.as_scalar().unwrap().as_string(), "value");

    // Verify exact round-trip
    let output = parsed.to_string();
    assert_eq!(output, yaml);
}

#[test]
fn test_hash_inside_quoted_strings() {
    // Hash character inside quoted strings should be literal, not a comment
    // Common source of user confusion
    let yaml = "text: \"This is # not a comment\"\n";

    let parsed = YamlFile::from_str(yaml).unwrap();
    let doc = parsed.document().unwrap();
    let mapping = doc.as_mapping().unwrap();

    // Verify exact content - # should be part of the string value
    let text_val = mapping.get("text").unwrap();
    assert_eq!(
        text_val.as_scalar().unwrap().as_string(),
        "This is # not a comment"
    );

    // Verify exact round-trip
    let output = parsed.to_string();
    assert_eq!(output, yaml);
}

#[test]
fn test_comment_with_unicode_emoji() {
    // Comment with Unicode characters and emoji
    // YAML allows any Unicode in comments
    let yaml = "key: value  # 🎉 important! 日本語\n";

    let parsed = YamlFile::from_str(yaml).unwrap();
    let doc = parsed.document().unwrap();
    let mapping = doc.as_mapping().unwrap();

    // Verify exact content
    let key_val = mapping.get("key").unwrap();
    assert_eq!(key_val.as_scalar().unwrap().as_string(), "value");

    // Verify exact round-trip
    let output = parsed.to_string();
    assert_eq!(output, yaml);
}

#[test]
fn test_multiple_hash_characters() {
    // Multiple # characters in comment
    // All # after the first are just comment content
    let yaml = "key: value  ## comment ### more\n";

    let parsed = YamlFile::from_str(yaml).unwrap();
    let doc = parsed.document().unwrap();
    let mapping = doc.as_mapping().unwrap();

    // Verify exact content
    let key_val = mapping.get("key").unwrap();
    assert_eq!(key_val.as_scalar().unwrap().as_string(), "value");

    // Verify exact round-trip
    let output = parsed.to_string();
    assert_eq!(output, yaml);
}

#[test]
fn test_comment_after_directive() {
    // Comment immediately after directive
    // YAML spec allows comments after directives
    let yaml = "%YAML 1.2 # this is a comment\n---\nkey: value\n";

    let parsed = YamlFile::from_str(yaml).unwrap();
    let doc = parsed.document().unwrap();
    let mapping = doc.as_mapping().unwrap();

    // Verify exact content
    let key_val = mapping.get("key").unwrap();
    assert_eq!(key_val.as_scalar().unwrap().as_string(), "value");

    // Verify exact round-trip
    let output = parsed.to_string();
    assert_eq!(output, yaml);
}
