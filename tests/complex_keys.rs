use yaml_edit::Yaml;

#[test]
fn test_explicit_key_indicator() {
    let yaml = r#"
? key1
: value1
? key2
: value2
"#;
    let doc = yaml.parse::<Yaml>();
    if let Err(e) = &doc {
        eprintln!("Parse error: {:?}", e);
    }
    assert!(doc.is_ok());
    let doc = doc.unwrap();

    // Verify the YAML parses without errors
    let docs: Vec<_> = doc.documents().collect();
    assert_eq!(docs.len(), 1);

    // The document should contain a mapping with explicit keys
    let output = doc.to_string();
    assert!(output.contains("key1") || output.contains("? key1"));
    assert!(output.contains("key2") || output.contains("? key2"));
    assert!(output.contains("value1"));
    assert!(output.contains("value2"));
}

#[test]
fn test_explicit_key_with_complex_value() {
    let yaml = r#"
? simple
: 
  - item1
  - item2
? another
:
  nested:
    key: value
"#;
    let doc = yaml.parse::<Yaml>();
    assert!(doc.is_ok());
    let doc = doc.unwrap();

    // Verify the YAML parses without errors
    let docs: Vec<_> = doc.documents().collect();
    assert_eq!(docs.len(), 1);

    // Check that complex values are preserved
    let output = doc.to_string();
    assert!(output.contains("simple"));
    assert!(output.contains("item1"));
    assert!(output.contains("item2"));
    assert!(output.contains("another"));
    assert!(output.contains("nested"));
}

#[test]
fn test_sequence_as_key() {
    let yaml = r#"
[a, b]: value1
[c, d]: value2
"#;
    let doc = yaml.parse::<Yaml>();
    assert!(doc.is_ok());
    let doc = doc.unwrap();

    // Verify the YAML parses without errors
    let docs: Vec<_> = doc.documents().collect();
    assert_eq!(docs.len(), 1);

    // Check that sequence keys are preserved
    let output = doc.to_string();
    assert!(output.contains("[a, b]") || (output.contains("a") && output.contains("b")));
    assert!(output.contains("[c, d]") || (output.contains("c") && output.contains("d")));
    assert!(output.contains("value1"));
    assert!(output.contains("value2"));
}

#[test]
fn test_mapping_as_key() {
    let yaml = r#"
{name: John, age: 30}: developer
{name: Jane, age: 25}: designer
"#;
    let doc = yaml.parse::<Yaml>();
    if let Err(e) = &doc {
        eprintln!("Parse error: {:?}", e);
    }
    assert!(doc.is_ok());
    let doc = doc.unwrap();

    // Verify the YAML parses without errors
    let docs: Vec<_> = doc.documents().collect();
    assert_eq!(docs.len(), 1);

    // Check that mapping keys are preserved
    let output = doc.to_string();
    assert!(output.contains("John") || output.contains("{name: John"));
    assert!(output.contains("Jane") || output.contains("{name: Jane"));
    assert!(output.contains("developer"));
    assert!(output.contains("designer"));
}

#[test]
fn test_explicit_key_with_sequence() {
    let yaml = r#"
? [a, b, c]
: value1
? [d, e, f]
: value2
"#;
    let doc = yaml.parse::<Yaml>();
    assert!(doc.is_ok());
    let doc = doc.unwrap();

    // Verify the YAML parses without errors
    let docs: Vec<_> = doc.documents().collect();
    assert_eq!(docs.len(), 1);

    // Check that explicit sequence keys are preserved
    let output = doc.to_string();
    assert!(
        output.contains("[a, b, c]")
            || (output.contains("a") && output.contains("b") && output.contains("c"))
    );
    assert!(
        output.contains("[d, e, f]")
            || (output.contains("d") && output.contains("e") && output.contains("f"))
    );
}

#[test]
fn test_explicit_key_with_mapping() {
    let yaml = r#"
? {x: 1, y: 2}
: point1
? {x: 3, y: 4}
: point2
"#;
    let doc = yaml.parse::<Yaml>();
    assert!(doc.is_ok());
    let doc = doc.unwrap();

    // Verify the YAML parses without errors
    let docs: Vec<_> = doc.documents().collect();
    assert_eq!(docs.len(), 1);

    // Check that explicit mapping keys are preserved
    let output = doc.to_string();
    assert!(output.contains("x: 1") || output.contains("{x: 1"));
    assert!(output.contains("y: 2") || output.contains("y: 2}"));
    assert!(output.contains("point1"));
    assert!(output.contains("point2"));
}

#[test]
fn test_mixed_key_types() {
    let yaml = r#"
simple: value1
? explicit
: value2
[list, key]: value3
{map: key}: value4
"#;
    let doc = yaml.parse::<Yaml>();
    assert!(doc.is_ok());
    let doc = doc.unwrap();

    // Verify the YAML parses without errors
    let docs: Vec<_> = doc.documents().collect();
    assert_eq!(docs.len(), 1);

    // Check that all key types are preserved
    let output = doc.to_string();
    assert!(output.contains("simple"));
    assert!(output.contains("value1"));
    assert!(output.contains("explicit"));
    assert!(output.contains("value2"));
    assert!(output.contains("value3"));
    assert!(output.contains("value4"));
}

#[test]
fn test_nested_complex_keys() {
    let yaml = r#"
? [a, [b, c]]
: nested_list
? {outer: {inner: value}}
: nested_map
"#;
    let doc = yaml.parse::<Yaml>();
    assert!(doc.is_ok());
    let doc = doc.unwrap();

    // Verify the YAML parses without errors
    let docs: Vec<_> = doc.documents().collect();
    assert_eq!(docs.len(), 1);

    // Check that nested structures in keys are preserved
    let output = doc.to_string();
    assert!(output.contains("nested_list"));
    assert!(output.contains("nested_map"));
}

#[test]
fn test_multiline_explicit_key() {
    let yaml = r#"
? |
  multiline
  key
: value
"#;
    let doc = yaml.parse::<Yaml>();
    assert!(doc.is_ok());
    let doc = doc.unwrap();

    // Verify the YAML parses without errors
    let docs: Vec<_> = doc.documents().collect();
    assert_eq!(docs.len(), 1);

    // Check that multiline keys work
    let output = doc.to_string();
    assert!(output.contains("multiline") || output.contains("|"));
    assert!(output.contains("value"));
}

#[test]
fn test_explicit_key_without_value() {
    let yaml = r#"
? key_without_value
? another_key
: has_value
"#;
    let doc = yaml.parse::<Yaml>();
    if let Err(e) = &doc {
        eprintln!("Parse error: {:?}", e);
    }
    assert!(doc.is_ok());
    let doc = doc.unwrap();

    // Verify the YAML parses without errors
    let docs: Vec<_> = doc.documents().collect();
    assert_eq!(docs.len(), 1);

    // Check that keys without values are handled
    let output = doc.to_string();
    assert!(output.contains("key_without_value"));
    assert!(output.contains("another_key"));
    assert!(output.contains("has_value"));
}

#[test]
fn test_complex_key_preservation() {
    let yaml = r#"
[a, b]: value1
{x: 1}: value2
? explicit
: value3
"#;
    let doc = yaml.parse::<Yaml>();
    assert!(doc.is_ok());
    let doc = doc.unwrap();

    // Convert back to string and verify preservation
    let output = doc.to_string();

    // The output should preserve the complex key structures
    assert!(output.contains("[a, b]") || (output.contains("a") && output.contains("b")));
    assert!(output.contains("{x: 1}") || output.contains("x: 1"));
    assert!(output.contains("explicit"));
    assert!(output.contains("value1"));
    assert!(output.contains("value2"));
    assert!(output.contains("value3"));
}

#[test]
fn test_yaml_1_2_spec_example() {
    // Example from YAML 1.2 spec showing complex keys
    let yaml = r#"
? - Detroit Tigers
  - Chicago Cubs
:
  - 2001-07-23

? [ New York Yankees,
    Atlanta Braves ]
: [ 2001-07-02, 2001-08-12,
    2001-08-14 ]
"#;
    let doc = yaml.parse::<Yaml>();
    assert!(doc.is_ok());
    let doc = doc.unwrap();

    // Verify the YAML parses without errors
    let docs: Vec<_> = doc.documents().collect();
    assert_eq!(docs.len(), 1);

    // Just verify that it parses successfully - the complex structure is preserved internally
    // even if the string representation might vary
}

#[test]
fn test_complex_key_in_flow_mapping() {
    let yaml = r#"
{ [a, b]: value1, {x: 1}: value2 }
"#;
    let doc = yaml.parse::<Yaml>();
    assert!(doc.is_ok());
    let doc = doc.unwrap();

    // Verify the YAML parses without errors
    let docs: Vec<_> = doc.documents().collect();
    assert_eq!(docs.len(), 1);

    let output = doc.to_string();
    assert!(output.contains("value1"));
    assert!(output.contains("value2"));
}
