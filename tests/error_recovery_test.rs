//! Tests for error recovery during YAML parsing

use yaml_edit::{Yaml, Parse};

#[test]
fn test_missing_colon_recovery() {
    let yaml = r#"
key1 value1
key2: value2
key3: value3
"#;
    
    let parse = Parse::parse_yaml(yaml);
    
    // Should have an error for missing colon
    assert!(!parse.errors().is_empty());
    
    // Should contain line/column info
    let error = &parse.errors()[0];
    assert!(error.contains("2:"));
    
    // Should still parse the rest of the document
    let doc = parse.tree();
    let text = doc.to_string();
    assert!(text.contains("key2"));
    assert!(text.contains("value2"));
}

#[test]
fn test_unterminated_string_recovery() {
    let yaml = r#"
key1: "unterminated string
key2: value2
key3: 'another unterminated
key4: value4
"#;
    
    let parse = Parse::parse_yaml(yaml);
    
    // Should have errors for unterminated strings
    assert!(parse.errors().len() >= 2);
    
    // Should still parse other keys
    let doc = parse.tree();
    let text = doc.to_string();
    assert!(text.contains("key2"));
    assert!(text.contains("value2"));
    assert!(text.contains("key4"));
    assert!(text.contains("value4"));
}

#[test]
fn test_unclosed_flow_sequence_recovery() {
    let yaml = r#"
list1: [1, 2, 3
list2: [4, 5, 6]
key: value
"#;
    
    let parse = Parse::parse_yaml(yaml);
    
    // Should have error for unclosed bracket
    assert!(!parse.errors().is_empty());
    assert!(parse.errors()[0].contains("]"));
    
    // Should still parse subsequent content
    let doc = parse.tree();
    let text = doc.to_string();
    assert!(text.contains("list2"));
    assert!(text.contains("key"));
    assert!(text.contains("value"));
}

#[test]
fn test_unclosed_flow_mapping_recovery() {
    let yaml = r#"
map1: {a: 1, b: 2
map2: {c: 3, d: 4}
key: value
"#;
    
    let parse = Parse::parse_yaml(yaml);
    
    // Should have error for unclosed brace
    assert!(!parse.errors().is_empty());
    assert!(parse.errors()[0].contains("}"));
    
    // Should still parse subsequent content
    let doc = parse.tree();
    let text = doc.to_string();
    assert!(text.contains("map2"));
    assert!(text.contains("key"));
}

#[test]
fn test_invalid_alias_recovery() {
    let yaml = r#"
anchor: &test value
invalid: *nonexistent
valid: *test
key: value
"#;
    
    let parse = Parse::parse_yaml(yaml);
    
    // Should have error for undefined alias
    assert!(!parse.errors().is_empty());
    assert!(parse.errors().iter().any(|e| e.contains("nonexistent")));
    
    // Should still parse valid alias and other content
    let doc = parse.tree();
    let text = doc.to_string();
    assert!(text.contains("valid"));
    assert!(text.contains("key"));
    assert!(text.contains("value"));
}

#[test]
fn test_positioned_errors() {
    let yaml = r#"
key1 missing colon
key2: value2
"#;
    
    let parse = Parse::parse_yaml(yaml);
    
    // Should have positioned errors
    let positioned_errors = parse.positioned_errors();
    assert!(!positioned_errors.is_empty());
    
    // Error should have line/column info in message
    let error = &positioned_errors[0];
    assert!(error.message.contains("2:")); // Line 2
    
    // Error should have a text range
    assert!(error.range.len() > 0.into());
}

#[test]
fn test_multiple_errors_continue_parsing() {
    let yaml = r#"
# Multiple errors in one document
key1 missing_colon
list: [unclosed
map: {also: unclosed
string: "unclosed too
valid_key: valid_value
another_valid: key
"#;
    
    let parse = Parse::parse_yaml(yaml);
    
    // Should have multiple errors
    assert!(parse.errors().len() >= 3);
    
    // Should still parse valid content
    let doc = parse.tree();
    let text = doc.to_string();
    assert!(text.contains("valid_key"));
    assert!(text.contains("valid_value"));
    assert!(text.contains("another_valid"));
}

#[test]
fn test_error_in_nested_structure() {
    let yaml = r#"
outer:
  inner1: value1
  inner2 missing_colon
  inner3: value3
  nested:
    deep1: value1
    deep2 another_missing
    deep3: value3
"#;
    
    let parse = Parse::parse_yaml(yaml);
    
    // Should have errors for missing colons
    assert!(parse.errors().len() >= 2);
    
    // Errors should indicate context
    let errors_text = parse.errors().join(" ");
    assert!(errors_text.contains("4:") || errors_text.contains("line"));
    
    // Should still parse valid nested content
    let doc = parse.tree();
    let text = doc.to_string();
    assert!(text.contains("inner1"));
    assert!(text.contains("inner3"));
    assert!(text.contains("deep1"));
    assert!(text.contains("deep3"));
}

#[test]
fn test_recovery_after_block_scalar_error() {
    let yaml = r#"
block: |
  This block scalar
  has proper indentation
bad_block: |2
This line is not indented enough
good_key: good_value
"#;
    
    let parse = Parse::parse_yaml(yaml);
    
    // Should continue parsing after block scalar error
    let doc = parse.tree();
    let text = doc.to_string();
    assert!(text.contains("good_key"));
    assert!(text.contains("good_value"));
}

#[test]
fn test_error_recovery_preserves_comments() {
    let yaml = r#"
# Comment before error
key1 missing_colon  # inline comment
# Comment after error
key2: value2  # another comment
"#;
    
    let parse = Parse::parse_yaml(yaml);
    
    // Should have error
    assert!(!parse.errors().is_empty());
    
    // Should preserve comments
    let doc = parse.tree();
    let text = doc.to_string();
    assert!(text.contains("# Comment before error"));
    assert!(text.contains("# Comment after error"));
    assert!(text.contains("# inline comment"));
    assert!(text.contains("# another comment"));
}