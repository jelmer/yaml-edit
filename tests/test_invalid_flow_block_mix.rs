use std::str::FromStr;
use yaml_edit::{Yaml, YamlValue};

#[test]
fn test_preserve_invalid_flow_block_mix() {
    // This is INVALID YAML according to the spec:
    // You cannot have a flow mapping {} followed by block-style entries
    let invalid_yaml = r#"---
{}
key1: value1
key2: value2"#;

    // Our parser, like Ruby's YAML parser, sees the {} and stops parsing
    // The block entries after {} are lost because this is invalid YAML
    let yaml = Yaml::from_str(invalid_yaml).unwrap();
    let result = yaml.to_string();

    // The parser only preserves the valid part (up to and including {})
    assert_eq!(result, "---\n{}\n");

    // But we should detect this is invalid
    // The parser should have registered an error or warning about this
    // Note: The exact error handling mechanism depends on the implementation
}

#[test]
fn test_set_with_field_order_on_invalid_flow_block_mix() {
    // Start with invalid YAML that has both flow and block style
    let invalid_yaml = r#"---
{}
key1: value1"#;

    let mut yaml = Yaml::from_str(invalid_yaml).unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mut mapping) = doc.as_mapping() {
            // Add a new key - but since the parser only sees {},
            // the new key is added to the empty mapping
            mapping.set(&YamlValue::scalar("key2"), &YamlValue::scalar("value2"));
        }
    }

    let result = yaml.to_string();

    // The parser only sees the {} (empty mapping), so key2 is the only key added
    // Note: includes trailing newline(s)
    assert_eq!(result, "---\n{}\nkey2: value2\n\n");
}

#[test]
fn test_operations_preserve_invalid_structure() {
    // Test that various operations preserve the invalid structure
    let invalid_yaml = r#"---
{}
name: test
version: 1.0"#;

    let mut yaml = Yaml::from_str(invalid_yaml).unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mut mapping) = doc.as_mapping() {
            // Test various operations

            // 1. Update existing key
            mapping.set(&YamlValue::scalar("version"), &YamlValue::scalar("2.0"));
            assert!(yaml.to_string().contains("{}"));
            assert!(
                yaml.to_string().contains("version: 2.0")
                    || yaml.to_string().contains("version: '2.0'")
            );

            // 2. Add new key with field order
            let field_order = &["name", "author", "version"];
            mapping.set_with_field_order("author", "John", field_order);
            assert!(yaml.to_string().contains("{}"));
            assert!(yaml.to_string().contains("author: John"));

            // 3. Remove a key
            mapping.remove(&YamlValue::scalar("name"));
            assert!(yaml.to_string().contains("{}"));
            assert!(!yaml.to_string().contains("name:"));
        }
    }
}

#[test]
fn test_reparse_invalid_yaml_loses_data() {
    // This test demonstrates that the invalid YAML we produce
    // cannot be correctly reparsed by compliant parsers
    let invalid_yaml = r#"---
{}
key1: value1
key2: value2"#;

    let yaml = Yaml::from_str(invalid_yaml).unwrap();
    let result = yaml.to_string();

    // When reparsed, compliant parsers will either:
    // 1. Throw an error (Python's PyYAML, js-yaml, yq)
    // 2. Only see {} and lose the rest (Ruby's YAML)
    // Our parser preserves it for lossless editing

    // Reparse with our parser
    let reparsed = Yaml::from_str(&result).unwrap();

    // The reparsed version will also only see the {} part
    assert_eq!(reparsed.to_string(), "---\n{}\n");

    // But if we look at what's actually accessible via the API
    if let Some(doc) = reparsed.document() {
        if let Some(mapping) = doc.as_mapping() {
            // The keys after {} might not be accessible via normal API
            // This depends on how the parser handles the invalid structure

            // Check if we can see the keys
            let keys: Vec<_> = mapping.keys().collect();
            println!("Keys found in reparsed invalid YAML: {:?}", keys);

            // The behavior here is implementation-specific
            // Some possibilities:
            // - Keys are empty (only {} is recognized)
            // - Keys contain ["key1", "key2"] (block entries are recognized)
            // - Mixed behavior
        }
    }
}

#[test]
fn test_valid_yaml_alternative() {
    // Test the valid alternative - no flow mapping
    let valid_yaml = r#"---
key1: value1
key2: value2"#;

    let yaml = Yaml::from_str(valid_yaml).unwrap();
    assert_eq!(yaml.to_string(), valid_yaml);

    // This can be correctly reparsed
    let reparsed = Yaml::from_str(&yaml.to_string()).unwrap();
    assert_eq!(reparsed.to_string(), valid_yaml);

    if let Some(doc) = reparsed.document() {
        if let Some(mapping) = doc.as_mapping() {
            let keys: Vec<_> = mapping.keys().collect();
            assert_eq!(keys, vec!["key1", "key2"]);
        }
    }
}
