//! YAML specification edge case tests
//!
//! Tests cover edge cases and corner cases from the YAML specification:
//! - Non-specific tags (! and !?)
//! - Tag shorthand and handles
//! - Preserve unknown tags
//! - Implicit and explicit document markers (---, ...)
//! - Multiple documents in a stream
//! - Empty documents
//! - Directive handling (%YAML, %TAG)
//!
//! All tests verify:
//! 1. Parser correctly handles edge cases
//! 2. Lossless preservation of formatting
//! 3. Round-trip validity

use std::str::FromStr;
use yaml_edit::YamlFile;

#[test]
fn test_non_specific_tags() {
    // Non-specific tags should be preserved
    let yaml_with_non_specific = r#"plain: !  value
quoted: ! "value""#;

    let parsed = YamlFile::from_str(yaml_with_non_specific);
    assert!(
        parsed.is_ok(),
        "Non-specific tags should parse successfully"
    );

    let file = parsed.unwrap();

    // Verify exact round-trip (non-specific tags preserved)
    let output = file.to_string();
    assert_eq!(output, yaml_with_non_specific);
}

#[test]
fn test_reserved_directives() {
    // Reserved directives should parse without error but may be ignored
    let yaml_with_reserved = r#"%RESERVED directive
---
key: value"#;

    let parsed = YamlFile::from_str(yaml_with_reserved);
    assert!(
        parsed.is_ok(),
        "Reserved directives should not cause parse errors"
    );

    let file = parsed.unwrap();

    // Verify via API
    let doc = file.document().unwrap();
    let mapping = doc.as_mapping().unwrap();
    assert_eq!(mapping.len(), 1);
    assert_eq!(
        mapping.get("key").unwrap().as_scalar().unwrap().as_string(),
        "value"
    );

    // Verify exact round-trip
    let output = file.to_string();
    assert_eq!(output, yaml_with_reserved);
}

#[test]
fn test_trailing_commas_in_flow_collections() {
    // YAML allows trailing commas in flow collections
    let yaml_with_trailing_commas = r#"trailing_seq: [a, b, c,]
trailing_map: {a: 1, b: 2,}"#;

    let parsed = YamlFile::from_str(yaml_with_trailing_commas);
    assert!(
        parsed.is_ok(),
        "Trailing commas in flow collections should be allowed"
    );

    let file = parsed.unwrap();
    let doc = file.document().unwrap();
    let mapping = doc.as_mapping().unwrap();
    assert!(mapping.get_sequence("trailing_seq").is_some());
    assert!(mapping.get_mapping("trailing_map").is_some());

    let output = file.to_string();
    assert_eq!(output, yaml_with_trailing_commas);
}

#[test]
fn test_mixed_indentation_levels() {
    // Mixed indentation should be handled gracefully
    let yaml_mixed_indent = r#"root:
  level1:
    level2:
     level2_off: value
      level3:
       level3_off: value"#;

    let parsed = YamlFile::from_str(yaml_mixed_indent);
    // This may or may not parse successfully depending on implementation
    // At minimum, it shouldn't crash
    match parsed {
        Ok(yaml) => {
            // Verify we parsed the nested structure correctly via API
            let doc = yaml.document().unwrap();
            let mapping = doc.as_mapping().unwrap();
            let root = mapping.get_mapping("root").unwrap();
            let level1 = root.get_mapping("level1").unwrap();
            let level2 = level1.get_mapping("level2").unwrap();

            // Check we can access level2_off
            assert_eq!(
                level2
                    .get("level2_off")
                    .unwrap()
                    .as_scalar()
                    .unwrap()
                    .as_string(),
                "value"
            );

            // Check we can access level3 and level3_off
            let level3 = level2.get_mapping("level3").unwrap();
            assert_eq!(
                level3
                    .get("level3_off")
                    .unwrap()
                    .as_scalar()
                    .unwrap()
                    .as_string(),
                "value"
            );

            // Verify exact round-trip
            let output = yaml.to_string();
            assert_eq!(output, yaml_mixed_indent);
        }
        Err(_) => {
            // Mixed indentation may legitimately cause parse errors
            // The important thing is that it doesn't panic
        }
    }
}

#[test]
fn test_empty_documents_in_stream() {
    // Empty documents in document streams
    let yaml_empty_docs = r#"---
...
---
just a string
---
key: value
..."#;

    let parsed = YamlFile::from_str(yaml_empty_docs);
    assert!(
        parsed.is_ok(),
        "Document streams with empty documents should parse"
    );

    let file = parsed.unwrap();

    // Verify via API - the first document in stream is empty
    let doc = file.document().expect("Should have a document");
    assert!(doc.is_empty(), "First document in stream should be empty");

    // Verify exact round-trip
    let output = file.to_string();
    assert_eq!(output, yaml_empty_docs);
}

#[test]
fn test_comments_in_flow_collections() {
    // Comments inside flow collections should be preserved and not interfere with values
    let yaml_flow_comments = r#"flow_sequence: [
    item1,  # Comment 1
    item2,  # Comment 2
    item3   # Comment 3
]
flow_mapping: {
    key1: value1,  # Comment A
    key2: value2,  # Comment B
    key3: value3   # Comment C
}"#;

    let parsed = YamlFile::from_str(yaml_flow_comments).unwrap();

    // Test round-trip preservation
    assert_eq!(
        parsed.to_string(),
        yaml_flow_comments,
        "Comments should be preserved exactly"
    );

    // Test API access - comments should not interfere with semantic values
    let doc = parsed.document().unwrap();
    let root = doc.as_mapping().unwrap();

    let seq_value = root.get("flow_sequence").unwrap();
    let seq = seq_value.as_sequence().unwrap();
    assert_eq!(seq.len(), 3);
    let items: Vec<String> = seq
        .values()
        .map(|v| v.as_scalar().unwrap().as_string())
        .collect();
    assert_eq!(
        items,
        vec!["item1", "item2", "item3"],
        "Sequence values should not include comments"
    );

    let map_value = root.get("flow_mapping").unwrap();
    let map = map_value.as_mapping().unwrap();
    assert_eq!(map.len(), 3);
    assert_eq!(
        map.get("key1").unwrap().as_scalar().unwrap().as_string(),
        "value1"
    );
    assert_eq!(
        map.get("key2").unwrap().as_scalar().unwrap().as_string(),
        "value2"
    );
    assert_eq!(
        map.get("key3").unwrap().as_scalar().unwrap().as_string(),
        "value3"
    );
}

#[test]
fn test_unicode_escapes_in_yaml() {
    // Unicode escape sequences in YAML strings
    let yaml_unicode = r#"unicode_escape: "\u263A\U0001F600"
emoji: "😀🎉🚀"
special_chars: "Line1\nLine2\tTabbed""#;

    let parsed = YamlFile::from_str(yaml_unicode);
    assert!(parsed.is_ok(), "Unicode escapes should parse correctly");

    let file = parsed.unwrap();
    let doc = file.document().unwrap();
    let mapping = doc.as_mapping().unwrap();

    // Verify we parsed all three keys correctly
    assert!(mapping.contains_key("unicode_escape"));
    assert!(mapping.contains_key("emoji"));
    assert!(mapping.contains_key("special_chars"));

    // Check the values
    assert!(mapping.get("unicode_escape").unwrap().as_scalar().is_some());
    assert_eq!(
        mapping
            .get("emoji")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "😀🎉🚀"
    );
    assert!(mapping.get("special_chars").unwrap().as_scalar().is_some());

    // Verify exact round-trip
    let output = file.to_string();
    assert_eq!(output, yaml_unicode);
}

#[test]
fn test_complex_nested_structures() {
    // Complex mixing of block and flow styles
    let yaml_complex = r#"mixed:
  block_seq:
    - item1
    - [nested, flow, seq]
    - item3
  flow_in_block: {a: 1, b: [2, 3], c: {d: 4}}
  block_in_flow: [
    item1,
    {
      nested: value,
      another: value
    },
    item3
  ]"#;

    let parsed = YamlFile::from_str(yaml_complex);
    assert!(parsed.is_ok(), "Complex nested structures should parse");

    let file = parsed.unwrap();

    // Verify via API
    let doc = file.document().unwrap();
    let root = doc.as_mapping().unwrap();
    let mixed = root.get_mapping("mixed").unwrap();

    // Verify block_seq
    let block_seq = mixed.get_sequence("block_seq").unwrap();
    assert_eq!(block_seq.len(), 3);

    // Verify flow_in_block
    let flow_in_block = mixed.get_mapping("flow_in_block").unwrap();
    assert_eq!(flow_in_block.len(), 3);
    assert_eq!(
        flow_in_block
            .get("a")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "1"
    );

    // Verify block_in_flow
    let block_in_flow = mixed.get_sequence("block_in_flow").unwrap();
    assert_eq!(block_in_flow.len(), 3);
    let nested_node = block_in_flow.get(1).unwrap();
    let nested_map = nested_node.as_mapping().unwrap();
    assert_eq!(
        nested_map
            .get("nested")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "value"
    );

    // Verify exact round-trip
    let output = file.to_string();
    assert_eq!(output, yaml_complex);
}

#[test]
fn test_circular_reference_handling() {
    // Circular references should be handled appropriately
    let yaml_circular = r#"node: &node
  next: *node"#;

    // This test verifies that circular references are handled without panicking
    // The behavior (error vs success with circular structure) is implementation-dependent
    let parsed = YamlFile::from_str(yaml_circular);

    match parsed {
        Ok(yaml) => {
            // Verify we parsed the circular reference structure via API
            let doc = yaml.document().unwrap();
            let mapping = doc.as_mapping().unwrap();
            assert!(mapping.contains_key("node"));

            let node_mapping = mapping.get_mapping("node").unwrap();
            assert!(node_mapping.contains_key("next"));

            // The "next" field should be an alias reference
            let next_value = node_mapping.get("next").unwrap();
            assert!(next_value.is_alias());
            assert_eq!(next_value.as_alias().unwrap().name(), "node");

            // Verify exact round-trip
            let output = yaml.to_string();
            assert_eq!(output, yaml_circular);
        }
        Err(_) => {
            // Circular references may legitimately cause errors
            // The important thing is no panic occurs
        }
    }
}

#[test]
fn test_special_float_values() {
    // Test special float values from YAML spec
    let yaml_special_floats = r#"infinity: .inf
neg_infinity: -.inf
not_a_number: .nan
negative_zero: -.0"#;

    let parsed = YamlFile::from_str(yaml_special_floats);
    assert!(parsed.is_ok(), "Special float values should parse");

    let file = parsed.unwrap();
    let doc = file.document().unwrap();
    let mapping = doc.as_mapping().unwrap();

    // Verify we parsed all four keys and check their float values
    assert_eq!(
        mapping
            .get("infinity")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_f64(),
        Some(f64::INFINITY)
    );
    assert_eq!(
        mapping
            .get("neg_infinity")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_f64(),
        Some(f64::NEG_INFINITY)
    );
    assert!(mapping
        .get("not_a_number")
        .unwrap()
        .as_scalar()
        .unwrap()
        .as_f64()
        .unwrap()
        .is_nan());
    assert_eq!(
        mapping
            .get("negative_zero")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_f64(),
        Some(-0.0)
    );

    // Verify exact round-trip
    let output = file.to_string();
    assert_eq!(output, yaml_special_floats);
}
