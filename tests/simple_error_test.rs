//! Simple test for error recovery using the Parse API

use yaml_edit::Parse;

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
    assert_eq!(positioned[0].message, "3:7: Unterminated quoted string");
}
