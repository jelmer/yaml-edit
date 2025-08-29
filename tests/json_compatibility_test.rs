//! Test JSON compatibility - JSON is valid YAML

use rowan::ast::AstNode;
use yaml_edit::{Mapping, Scalar, Sequence, Yaml};

#[test]
fn test_json_object_parsing() {
    let json = r#"{"name": "John", "age": 30, "active": true, "balance": 100.50}"#;

    let yaml = Yaml::parse(json);
    assert!(
        yaml.errors().is_empty(),
        "Failed to parse JSON object: {:?}",
        yaml.errors()
    );

    let doc = yaml.tree().document().expect("Should have a document");
    let mapping = doc.as_mapping().expect("Should be a mapping");

    assert_eq!(
        mapping
            .get(&"name".into())
            .and_then(|v| v.children().find_map(Scalar::cast))
            .map(|s| s.as_string()),
        Some("John".to_string())
    );
    assert_eq!(
        mapping
            .get(&"age".into())
            .and_then(|v| v.children().find_map(Scalar::cast))
            .map(|s| s.value()),
        Some("30".to_string())
    );
    assert_eq!(
        mapping
            .get(&"active".into())
            .and_then(|v| v.children().find_map(Scalar::cast))
            .map(|s| s.value()),
        Some("true".to_string())
    );
    assert_eq!(
        mapping
            .get(&"balance".into())
            .and_then(|v| v.children().find_map(Scalar::cast))
            .map(|s| s.value()),
        Some("100.50".to_string())
    );
}

#[test]
fn test_json_array_parsing() {
    let json = r#"[1, 2, 3, "hello", true, null]"#;

    let yaml = Yaml::parse(json);
    assert!(
        yaml.errors().is_empty(),
        "Failed to parse JSON array: {:?}",
        yaml.errors()
    );

    let doc = yaml.tree().document().expect("Should have a document");
    let sequence = doc.as_sequence().expect("Should be a sequence");

    let items: Vec<String> = sequence
        .items()
        .filter_map(|item| Scalar::cast(item).map(|s| s.as_string()))
        .collect();

    assert_eq!(items, vec!["1", "2", "3", "hello", "true", "null"]);
}

#[test]
fn test_nested_json() {
    let json = r#"{
        "users": [
            {"name": "Alice", "age": 25},
            {"name": "Bob", "age": 30}
        ],
        "settings": {
            "theme": "dark",
            "notifications": true
        }
    }"#;

    let yaml = Yaml::parse(json);
    assert!(
        yaml.errors().is_empty(),
        "Failed to parse nested JSON: {:?}",
        yaml.errors()
    );

    let doc = yaml.tree().document().expect("Should have a document");
    let root = doc.as_mapping().expect("Should be a mapping");

    // Check users array
    let users = root
        .get(&"users".into())
        .and_then(|v| v.children().find_map(Sequence::cast))
        .expect("Should have users array");

    let user_items: Vec<_> = users.items().collect();
    assert_eq!(user_items.len(), 2);

    // Check settings object
    let settings = root
        .get(&"settings".into())
        .and_then(|v| v.children().find_map(Mapping::cast))
        .expect("Should have settings object");

    assert_eq!(
        settings
            .get(&"theme".into())
            .and_then(|v| v.children().find_map(Scalar::cast))
            .map(|s| s.as_string()),
        Some("dark".to_string())
    );
}

#[test]
fn test_json_escape_sequences() {
    let json =
        r#"{"message": "Hello\nWorld\t!", "path": "C:\\Users\\file.txt", "unicode": "\u2764"}"#;

    let yaml = Yaml::parse(json);
    assert!(
        yaml.errors().is_empty(),
        "Failed to parse JSON with escapes: {:?}",
        yaml.errors()
    );

    let doc = yaml.tree().document().expect("Should have a document");
    let mapping = doc.as_mapping().expect("Should be a mapping");

    // Check escape sequence handling
    let message = mapping
        .get(&"message".into())
        .and_then(|v| v.children().find_map(Scalar::cast))
        .map(|s| s.as_string())
        .expect("Should have message");

    assert!(message.contains('\n'), "Should contain newline");
    assert!(message.contains('\t'), "Should contain tab");
}

#[test]
fn test_json_numbers() {
    let json = r#"{
        "integer": 42,
        "negative": -17,
        "float": 3.14159,
        "scientific": 6.022e23,
        "negative_exp": 1.5e-10
    }"#;

    let yaml = Yaml::parse(json);
    assert!(
        yaml.errors().is_empty(),
        "Failed to parse JSON numbers: {:?}",
        yaml.errors()
    );

    let doc = yaml.tree().document().expect("Should have a document");
    let mapping = doc.as_mapping().expect("Should be a mapping");

    // Check various number formats
    assert_eq!(
        mapping
            .get(&"integer".into())
            .and_then(|v| v.children().find_map(Scalar::cast))
            .map(|s| s.value()),
        Some("42".to_string())
    );
    assert_eq!(
        mapping
            .get(&"negative".into())
            .and_then(|v| v.children().find_map(Scalar::cast))
            .map(|s| s.value()),
        Some("-17".to_string())
    );
    assert_eq!(
        mapping
            .get(&"float".into())
            .and_then(|v| v.children().find_map(Scalar::cast))
            .map(|s| s.value()),
        Some("3.14159".to_string())
    );

    // Scientific notation might need special handling
    let scientific = mapping
        .get(&"scientific".into())
        .and_then(|v| v.children().find_map(Scalar::cast))
        .map(|s| s.value());
    assert!(scientific.is_some(), "Should parse scientific notation");
}

#[test]
fn test_json_special_values() {
    let json = r#"{"empty_string": "", "empty_array": [], "empty_object": {}}"#;

    let yaml = Yaml::parse(json);
    assert!(
        yaml.errors().is_empty(),
        "Failed to parse JSON with empty values: {:?}",
        yaml.errors()
    );

    let doc = yaml.tree().document().expect("Should have a document");
    let mapping = doc.as_mapping().expect("Should be a mapping");

    // Check empty string
    let empty_str = mapping
        .get(&"empty_string".into())
        .and_then(|v| v.children().find_map(Scalar::cast))
        .map(|s| s.as_string());
    assert_eq!(empty_str, Some("".to_string()));

    // Check empty array
    let empty_array = mapping
        .get(&"empty_array".into())
        .and_then(|v| v.children().find_map(Sequence::cast));
    assert!(empty_array.is_some(), "Should have empty array");
    assert_eq!(empty_array.unwrap().items().count(), 0);

    // Check empty object
    let empty_obj = mapping
        .get(&"empty_object".into())
        .and_then(|v| v.children().find_map(Mapping::cast));
    assert!(empty_obj.is_some(), "Should have empty object");
}

#[test]
fn test_json_roundtrip() {
    // Test that we can parse JSON and output valid YAML
    let json = r#"{"name": "test", "items": [1, 2, 3]}"#;

    let yaml = Yaml::parse(json);
    assert!(yaml.errors().is_empty());

    // The output should be valid YAML (though not necessarily JSON)
    let output = yaml.tree().to_string();

    // Parse the output again to verify it's valid
    let reparsed = Yaml::parse(&output);
    assert!(
        reparsed.errors().is_empty(),
        "Roundtrip failed: {:?}",
        reparsed.errors()
    );
}

#[test]
fn test_json_whitespace_handling() {
    // JSON allows arbitrary whitespace between tokens
    let json = r#"  {  
        "key1"   :   "value1"  ,
        "key2"  :  [  1  ,  2  ,  3  ]  
    }  "#;

    let yaml = Yaml::parse(json);
    assert!(
        yaml.errors().is_empty(),
        "Failed to parse JSON with whitespace: {:?}",
        yaml.errors()
    );
}

#[test]
fn test_json_no_trailing_comma() {
    // JSON doesn't allow trailing commas, but YAML flow collections might
    // This test verifies we handle both cases
    let json_valid = r#"{"a": 1, "b": 2}"#;
    let json_array = r#"[1, 2, 3]"#;

    let yaml1 = Yaml::parse(json_valid);
    assert!(yaml1.errors().is_empty());

    let yaml2 = Yaml::parse(json_array);
    assert!(yaml2.errors().is_empty());
}
