use std::str::FromStr;
use yaml_edit::YamlFile;

#[test]
fn test_preserve_invalid_flow_block_mix() {
    // This is INVALID YAML according to the spec:
    // You cannot have a flow mapping {} followed by block-style entries
    let invalid_yaml = r#"---
{}
key1: value1
key2: value2"#;

    // Our parser is LOSSLESS - it preserves everything, even invalid content
    // The block entries after {} are wrapped in ERROR nodes but preserved
    let yaml = YamlFile::from_str(invalid_yaml).unwrap();
    let result = yaml.to_string();

    // The parser preserves ALL content for lossless editing
    assert_eq!(result, invalid_yaml);

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

    let yaml = YamlFile::from_str(invalid_yaml).unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mapping) = doc.as_mapping() {
            // Add a new key - since the mapping is {}, the new key is added there
            // The invalid content after {} is preserved in ERROR nodes
            mapping.set("key2", "value2");
        }
    }

    let result = yaml.to_string();

    // The parser preserves BOTH the new key and the original invalid content
    assert_eq!(result, "---\n{}\nkey2: value2\n\nkey1: value1");
}

#[test]
fn test_operations_preserve_invalid_structure() {
    // Test that operations on the mapping {} preserve the invalid ERROR content
    let invalid_yaml = r#"---
{}
name: test
version: 1.0"#;

    let yaml = YamlFile::from_str(invalid_yaml).unwrap();

    // Initially, the invalid content is preserved as-is
    assert_eq!(yaml.to_string(), invalid_yaml);

    if let Some(doc) = yaml.document() {
        if let Some(mapping) = doc.as_mapping() {
            // The mapping is empty {} - operations on it don't affect ERROR nodes
            // which preserve "name: test\nversion: 1.0"

            // Add a key to the empty mapping
            mapping.set("author", "John");

            // The new key is added to {}, invalid content remains in ERROR nodes
            let expected = "---\n{}\nauthor: John\n\nname: test\nversion: 1.0";
            assert_eq!(yaml.to_string(), expected);
        }
    }
}

#[test]
fn test_reparse_invalid_yaml_loses_data() {
    // This test demonstrates lossless round-tripping even for invalid YAML
    let invalid_yaml = r#"---
{}
key1: value1
key2: value2"#;

    let yaml = YamlFile::from_str(invalid_yaml).unwrap();
    let result = yaml.to_string();

    // Our parser preserves everything for lossless editing
    assert_eq!(result, invalid_yaml);

    // Reparse with our parser - still preserves everything
    let reparsed = YamlFile::from_str(&result).unwrap();

    // Lossless round-trip: parse -> serialize -> parse -> serialize
    assert_eq!(reparsed.to_string(), invalid_yaml);

    // The mapping {} is empty, but invalid content is preserved in ERROR nodes
    if let Some(doc) = reparsed.document() {
        if let Some(mapping) = doc.as_mapping() {
            let keys: Vec<_> = mapping.keys().collect();
            // The mapping is empty {} - invalid content is in ERROR nodes
            assert_eq!(keys.len(), 0);
        }
    }
}

#[test]
fn test_valid_yaml_alternative() {
    // Test the valid alternative - no flow mapping
    let valid_yaml = r#"---
key1: value1
key2: value2"#;

    let yaml = YamlFile::from_str(valid_yaml).unwrap();
    assert_eq!(yaml.to_string(), valid_yaml);

    // This can be correctly reparsed
    let reparsed = YamlFile::from_str(&yaml.to_string()).unwrap();
    assert_eq!(reparsed.to_string(), valid_yaml);

    if let Some(doc) = reparsed.document() {
        if let Some(mapping) = doc.as_mapping() {
            let keys: Vec<_> = mapping.keys().collect();
            assert_eq!(keys, vec!["key1", "key2"]);
        }
    }
}
