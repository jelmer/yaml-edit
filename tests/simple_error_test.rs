//! Simple test for error recovery

use yaml_edit::Parse;

#[test]
fn test_basic_error_reporting() {
    let yaml = "key: value";
    let parse = Parse::parse_yaml(yaml);

    // Should have no errors for valid YAML
    assert!(parse.errors().is_empty());

    // Tree should be valid
    let doc = parse.tree();
    assert!(doc.to_string().contains("key"));
}

#[test]
fn test_error_with_line_info() {
    let yaml = r#"
key1: value1
alias: *undefined
key2: value2
"#;

    let parse = Parse::parse_yaml(yaml);

    // Should have errors
    let errors = parse.errors();
    assert!(!errors.is_empty());

    // Should have positioned errors
    let positioned = parse.positioned_errors();
    assert!(!positioned.is_empty());

    // Error should contain line info
    let error_msg = &positioned[0].message;
    println!("Error message: {}", error_msg);
    assert!(error_msg.contains(":") || error_msg.contains("line"));
}
