use rowan::ast::AstNode;
use std::str::FromStr;
use yaml_edit::{debug, YamlFile};

#[test]
fn test_preserve_invalid_flow_block_mix() {
    // This is INVALID YAML according to the spec:
    // You cannot have a flow mapping {} followed by block-style entries
    let invalid_yaml = r#"---
{}
key1: value1
key2: value2"#;

    // The entries after {} cannot be attached to it. The parser keeps them
    // as a second node of the document, which the validator reports, and
    // the text is preserved for lossless editing.
    let parsed = YamlFile::parse(invalid_yaml);
    assert_eq!(parsed.errors(), Vec::<String>::new());
    assert_eq!(parsed.tree().to_string(), invalid_yaml);
    let violations: Vec<_> = yaml_edit::validator::Validator::new()
        .validate_syntax(parsed.tree().syntax())
        .into_iter()
        .map(|v| (v.message, v.location.unwrap()))
        .collect();
    assert_eq!(
        violations,
        [(
            "Document holds more than one node".to_string(),
            "3:1".to_string()
        )]
    );
}

#[test]
fn test_set_with_field_order_on_invalid_flow_block_mix() {
    // Start with invalid YAML that has both flow and block style
    let invalid_yaml = r#"---
{}
key1: value1"#;

    let yaml = YamlFile::parse(invalid_yaml).tree();

    if let Some(doc) = yaml.document() {
        if let Some(mapping) = doc.as_mapping() {
            // Add a new key - since the mapping is {}, the new key is added there
            // The invalid content after {} is preserved as a second node
            mapping.set("key2", "value2");
            debug::validate_tree(yaml.syntax()).unwrap();
        }
    }

    let result = yaml.to_string();

    // Setting into a flow mapping now inserts inside its `{}` (was: appended
    // a broken block-style entry after `}`). The trailing invalid block
    // content is still preserved.
    assert_eq!(result, "---\n{key2: value2}\nkey1: value1");
}

#[test]
fn test_operations_preserve_invalid_structure() {
    // Test that operations on the mapping {} preserve the invalid ERROR content
    let invalid_yaml = r#"---
{}
name: test
version: 1.0"#;

    let yaml = YamlFile::parse(invalid_yaml).tree();

    // Initially, the invalid content is preserved as-is
    assert_eq!(yaml.to_string(), invalid_yaml);

    if let Some(doc) = yaml.document() {
        if let Some(mapping) = doc.as_mapping() {
            // The mapping is empty {} - operations on it don't affect the
            // mapping after it, "name: test\nversion: 1.0"

            // Add a key to the empty mapping
            mapping.set("author", "John");
            debug::validate_tree(yaml.syntax()).unwrap();

            // The new key is added inside the `{}`; invalid trailing block
            // content stays where it was.
            let expected = "---\n{author: John}\nname: test\nversion: 1.0";
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

    let yaml = YamlFile::parse(invalid_yaml).tree();
    let result = yaml.to_string();

    // Our parser preserves everything for lossless editing
    assert_eq!(result, invalid_yaml);

    // Reparse with our parser - still preserves everything
    let reparsed = YamlFile::parse(&result).tree();

    // Lossless round-trip: parse -> serialize -> parse -> serialize
    assert_eq!(reparsed.to_string(), invalid_yaml);

    // The mapping {} is empty, but invalid content is preserved after it
    if let Some(doc) = reparsed.document() {
        if let Some(mapping) = doc.as_mapping() {
            let keys: Vec<_> = mapping.keys().collect();
            // The mapping is empty {} - invalid content is in a second node
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
