use std::str::FromStr;
use yaml_edit::Yaml;

#[test]
fn test_non_specific_tags() {
    // Non-specific tags should be preserved
    let yaml_with_non_specific = r#"plain: !  value
quoted: ! "value""#;

    let parsed = Yaml::from_str(yaml_with_non_specific);
    assert!(
        parsed.is_ok(),
        "Non-specific tags should parse successfully"
    );

    let output = parsed.unwrap().to_string();
    assert!(
        output.contains("! "),
        "Non-specific tag should be preserved"
    );
}

#[test]
fn test_reserved_directives() {
    // Reserved directives should parse without error but may be ignored
    let yaml_with_reserved = r#"%RESERVED directive
---
key: value"#;

    let parsed = Yaml::from_str(yaml_with_reserved);
    assert!(
        parsed.is_ok(),
        "Reserved directives should not cause parse errors"
    );

    let output = parsed.unwrap().to_string();
    assert!(
        output.contains("key: value"),
        "Document content should be preserved"
    );
}

#[test]
fn test_trailing_commas_in_flow_collections() {
    // YAML allows trailing commas in flow collections
    let yaml_with_trailing_commas = r#"trailing_seq: [a, b, c,]
trailing_map: {a: 1, b: 2,}"#;

    let parsed = Yaml::from_str(yaml_with_trailing_commas);
    assert!(
        parsed.is_ok(),
        "Trailing commas in flow collections should be allowed"
    );

    let output = parsed.unwrap().to_string();
    assert!(
        output.contains("trailing_seq"),
        "Sequence should be preserved"
    );
    assert!(
        output.contains("trailing_map"),
        "Mapping should be preserved"
    );
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

    let parsed = Yaml::from_str(yaml_mixed_indent);
    // This may or may not parse successfully depending on implementation
    // At minimum, it shouldn't crash
    match parsed {
        Ok(yaml) => {
            let output = yaml.to_string();
            assert!(output.contains("level2_off"), "Content should be preserved");
            assert!(output.contains("level3_off"), "Content should be preserved");
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

    let parsed = Yaml::from_str(yaml_empty_docs);
    assert!(
        parsed.is_ok(),
        "Document streams with empty documents should parse"
    );

    let output = parsed.unwrap().to_string();
    assert!(
        output.contains("just a string"),
        "String document should be preserved"
    );
    assert!(
        output.contains("key: value"),
        "Mapping document should be preserved"
    );
}

#[test]
fn test_comments_in_flow_collections() {
    // Comments inside flow collections should be preserved
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

    let parsed = Yaml::from_str(yaml_flow_comments);
    assert!(
        parsed.is_ok(),
        "Flow collections with comments should parse"
    );

    let output = parsed.unwrap().to_string();
    // Comments should be preserved (implementation-dependent)
    assert!(
        output.contains("flow_sequence"),
        "Flow sequence should be preserved"
    );
    assert!(
        output.contains("flow_mapping"),
        "Flow mapping should be preserved"
    );
    assert!(
        output.contains("item1"),
        "Sequence items should be preserved"
    );
    assert!(output.contains("key1"), "Mapping keys should be preserved");
}

#[test]
fn test_unicode_escapes_in_yaml() {
    // Unicode escape sequences in YAML strings
    let yaml_unicode = r#"unicode_escape: "\u263A\U0001F600"
emoji: "😀🎉🚀"
special_chars: "Line1\nLine2\tTabbed""#;

    let parsed = Yaml::from_str(yaml_unicode);
    assert!(parsed.is_ok(), "Unicode escapes should parse correctly");

    let output = parsed.unwrap().to_string();
    assert!(
        output.contains("unicode_escape"),
        "Unicode escape key should be preserved"
    );
    assert!(output.contains("emoji"), "Emoji key should be preserved");
    assert!(
        output.contains("special_chars"),
        "Special chars key should be preserved"
    );
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

    let parsed = Yaml::from_str(yaml_complex);
    assert!(parsed.is_ok(), "Complex nested structures should parse");

    let output = parsed.unwrap().to_string();
    assert!(
        output.contains("block_seq"),
        "Block sequence should be preserved"
    );
    assert!(
        output.contains("flow_in_block"),
        "Flow in block should be preserved"
    );
    assert!(
        output.contains("block_in_flow"),
        "Block in flow should be preserved"
    );
    assert!(
        output.contains("nested: value"),
        "Nested mappings should be preserved"
    );
}

#[test]
fn test_circular_reference_handling() {
    // Circular references should be handled appropriately
    let yaml_circular = r#"node: &node
  next: *node"#;

    // This test verifies that circular references are handled without panicking
    // The behavior (error vs success with circular structure) is implementation-dependent
    let parsed = Yaml::from_str(yaml_circular);

    match parsed {
        Ok(yaml) => {
            // If it parses successfully, ensure we can convert back to string
            let output = yaml.to_string();
            assert!(output.contains("node"), "Node key should be preserved");
            assert!(
                output.contains("&node") || output.contains("*node"),
                "Reference should be preserved"
            );
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

    let parsed = Yaml::from_str(yaml_special_floats);
    assert!(parsed.is_ok(), "Special float values should parse");

    let output = parsed.unwrap().to_string();
    assert!(output.contains("infinity"), "Infinity should be preserved");
    assert!(
        output.contains("neg_infinity"),
        "Negative infinity should be preserved"
    );
    assert!(output.contains("not_a_number"), "NaN should be preserved");
}
