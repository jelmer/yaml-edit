use yaml_edit::YamlFile;

#[test]
fn test_explicit_key_indicator() {
    let yaml = r#"
? key1
: value1
? key2
: value2
"#;
    let doc = yaml.parse::<YamlFile>();
    assert!(doc.is_ok());
    let doc = doc.unwrap();

    // Verify the YAML parses without errors
    let docs: Vec<_> = doc.documents().collect();
    assert_eq!(docs.len(), 1);

    // Verify API: can access values through the document
    let document = doc.document().expect("Should have a document");
    let mapping = document.as_mapping().expect("Document should be a mapping");
    assert_eq!(mapping.len(), 2);

    // Can access values by key
    assert_eq!(document.get_string("key1"), Some("value1".to_string()));
    assert_eq!(document.get_string("key2"), Some("value2".to_string()));

    // Verify exact round-trip (lossless preservation)
    let output = doc.to_string();
    assert_eq!(output, yaml);
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
    let doc = yaml.parse::<YamlFile>();
    assert!(doc.is_ok());
    let doc = doc.unwrap();

    // Verify the YAML parses without errors
    let docs: Vec<_> = doc.documents().collect();
    assert_eq!(docs.len(), 1);

    // Verify API: can access values through the mapping
    let document = doc.document().expect("Should have a document");
    let mapping = document.as_mapping().expect("Document should be a mapping");

    // Verify the sequence value under "simple" key
    let simple_val = mapping.get("simple").expect("Should have 'simple' key");
    let simple_seq = simple_val
        .as_sequence()
        .expect("'simple' should be a sequence");
    assert_eq!(simple_seq.len(), 2);

    // Verify the nested mapping under "another" key
    let another_val = mapping.get("another").expect("Should have 'another' key");
    let another_map = another_val
        .as_mapping()
        .expect("'another' should be a mapping");
    let nested = another_map.get("nested").expect("Should have 'nested' key");
    let nested_map = nested.as_mapping().expect("'nested' should be a mapping");
    let key_value = nested_map.get("key").expect("Should have 'key'");
    assert_eq!(key_value.as_scalar().unwrap().as_string(), "value");

    // Verify exact round-trip (lossless preservation)
    let output = doc.to_string();
    assert_eq!(output, yaml);
}

#[test]
fn test_sequence_as_key() {
    let yaml = r#"
[a, b]: value1
[c, d]: value2
"#;
    let doc = yaml.parse::<YamlFile>();
    assert!(doc.is_ok());
    let doc = doc.unwrap();

    // Verify the YAML parses without errors
    let docs: Vec<_> = doc.documents().collect();
    assert_eq!(docs.len(), 1);

    // Verify API: can access the mapping structure
    let document = doc.document().expect("Should have a document");
    let mapping = document.as_mapping().expect("Document should be a mapping");
    assert_eq!(mapping.len(), 2);

    // Can iterate over entries with flow sequence keys
    let entries: Vec<_> = mapping.iter().collect();
    assert_eq!(entries.len(), 2);

    // First entry: verify it's a flow sequence key
    let key1 = entries[0]
        .0
        .as_sequence()
        .expect("Key should be a sequence");
    assert!(key1.is_flow_style(), "Key should be flow-style sequence");
    assert_eq!(key1.len(), 2);
    assert_eq!(key1.get(0).unwrap().as_scalar().unwrap().as_string(), "a");
    assert_eq!(key1.get(1).unwrap().as_scalar().unwrap().as_string(), "b");
    assert_eq!(entries[0].1.as_scalar().unwrap().as_string(), "value1");

    // Second entry: verify it's a flow sequence key
    let key2 = entries[1]
        .0
        .as_sequence()
        .expect("Key should be a sequence");
    assert!(key2.is_flow_style(), "Key should be flow-style sequence");
    assert_eq!(key2.len(), 2);
    assert_eq!(key2.get(0).unwrap().as_scalar().unwrap().as_string(), "c");
    assert_eq!(key2.get(1).unwrap().as_scalar().unwrap().as_string(), "d");
    assert_eq!(entries[1].1.as_scalar().unwrap().as_string(), "value2");

    // Verify exact round-trip (lossless preservation)
    let output = doc.to_string();
    assert_eq!(output, yaml);
}

#[test]
fn test_mapping_as_key() {
    let yaml = r#"
{name: John, age: 30}: developer
{name: Jane, age: 25}: designer
"#;
    let doc = yaml.parse::<YamlFile>();
    assert!(doc.is_ok());
    let doc = doc.unwrap();

    // Verify the YAML parses without errors
    let docs: Vec<_> = doc.documents().collect();
    assert_eq!(docs.len(), 1);

    // Verify API: can access the mapping structure
    let document = doc.document().expect("Should have a document");
    let mapping = document.as_mapping().expect("Document should be a mapping");
    assert_eq!(mapping.len(), 2);

    // Can iterate over entries with flow mapping keys
    let entries: Vec<_> = mapping.iter().collect();
    assert_eq!(entries.len(), 2);

    // First entry: verify it's a flow mapping key
    let key1 = entries[0].0.as_mapping().expect("Key should be a mapping");
    assert!(key1.is_flow_style(), "Key should be flow-style mapping");
    assert_eq!(key1.len(), 2);
    assert_eq!(
        key1.get("name").unwrap().as_scalar().unwrap().as_string(),
        "John"
    );
    assert_eq!(
        key1.get("age").unwrap().as_scalar().unwrap().as_string(),
        "30"
    );
    assert_eq!(entries[0].1.as_scalar().unwrap().as_string(), "developer");

    // Second entry: verify it's a flow mapping key
    let key2 = entries[1].0.as_mapping().expect("Key should be a mapping");
    assert!(key2.is_flow_style(), "Key should be flow-style mapping");
    assert_eq!(key2.len(), 2);
    assert_eq!(
        key2.get("name").unwrap().as_scalar().unwrap().as_string(),
        "Jane"
    );
    assert_eq!(
        key2.get("age").unwrap().as_scalar().unwrap().as_string(),
        "25"
    );
    assert_eq!(entries[1].1.as_scalar().unwrap().as_string(), "designer");

    // Verify exact round-trip (lossless preservation)
    let output = doc.to_string();
    assert_eq!(output, yaml);
}

#[test]
fn test_explicit_key_with_sequence() {
    let yaml = r#"
? [a, b, c]
: value1
? [d, e, f]
: value2
"#;
    let doc = yaml.parse::<YamlFile>();
    assert!(doc.is_ok());
    let doc = doc.unwrap();

    // Verify the YAML parses without errors
    let docs: Vec<_> = doc.documents().collect();
    assert_eq!(docs.len(), 1);

    // Verify API: can access the mapping structure
    let document = doc.document().expect("Should have a document");
    let mapping = document.as_mapping().expect("Document should be a mapping");
    assert_eq!(mapping.len(), 2);

    // Can iterate over entries with explicit flow sequence keys
    let entries: Vec<_> = mapping.iter().collect();
    assert_eq!(entries.len(), 2);

    // First key: explicit flow sequence [a, b, c]
    let key1 = entries[0]
        .0
        .as_sequence()
        .expect("Key should be a sequence");
    assert!(key1.is_flow_style(), "Key should be flow-style sequence");
    assert_eq!(key1.len(), 3);
    assert_eq!(key1.get(0).unwrap().as_scalar().unwrap().as_string(), "a");
    assert_eq!(key1.get(1).unwrap().as_scalar().unwrap().as_string(), "b");
    assert_eq!(key1.get(2).unwrap().as_scalar().unwrap().as_string(), "c");
    assert_eq!(entries[0].1.as_scalar().unwrap().as_string(), "value1");

    // Second key: explicit flow sequence [d, e, f]
    let key2 = entries[1]
        .0
        .as_sequence()
        .expect("Key should be a sequence");
    assert!(key2.is_flow_style(), "Key should be flow-style sequence");
    assert_eq!(key2.len(), 3);
    assert_eq!(key2.get(0).unwrap().as_scalar().unwrap().as_string(), "d");
    assert_eq!(key2.get(1).unwrap().as_scalar().unwrap().as_string(), "e");
    assert_eq!(key2.get(2).unwrap().as_scalar().unwrap().as_string(), "f");
    assert_eq!(entries[1].1.as_scalar().unwrap().as_string(), "value2");

    // Verify exact round-trip (lossless preservation)
    let output = doc.to_string();
    assert_eq!(output, yaml);
}

#[test]
fn test_explicit_key_with_mapping() {
    let yaml = r#"
? {x: 1, y: 2}
: point1
? {x: 3, y: 4}
: point2
"#;
    let doc = yaml.parse::<YamlFile>();
    assert!(doc.is_ok());
    let doc = doc.unwrap();

    // Verify the YAML parses without errors
    let docs: Vec<_> = doc.documents().collect();
    assert_eq!(docs.len(), 1);

    // Verify API: can access the mapping structure
    let document = doc.document().expect("Should have a document");
    let mapping = document.as_mapping().expect("Document should be a mapping");
    assert_eq!(mapping.len(), 2);

    // Can iterate over entries with explicit flow mapping keys
    let entries: Vec<_> = mapping.iter().collect();
    assert_eq!(entries.len(), 2);

    // First key: explicit flow mapping {x: 1, y: 2}
    let key1 = entries[0].0.as_mapping().expect("Key should be a mapping");
    assert!(key1.is_flow_style(), "Key should be flow-style mapping");
    assert_eq!(key1.len(), 2);
    assert_eq!(key1.get("x").unwrap().as_scalar().unwrap().as_string(), "1");
    assert_eq!(key1.get("y").unwrap().as_scalar().unwrap().as_string(), "2");
    assert_eq!(entries[0].1.as_scalar().unwrap().as_string(), "point1");

    // Second key: explicit flow mapping {x: 3, y: 4}
    let key2 = entries[1].0.as_mapping().expect("Key should be a mapping");
    assert!(key2.is_flow_style(), "Key should be flow-style mapping");
    assert_eq!(key2.len(), 2);
    assert_eq!(key2.get("x").unwrap().as_scalar().unwrap().as_string(), "3");
    assert_eq!(key2.get("y").unwrap().as_scalar().unwrap().as_string(), "4");
    assert_eq!(entries[1].1.as_scalar().unwrap().as_string(), "point2");

    // Verify exact round-trip (lossless preservation)
    let output = doc.to_string();
    assert_eq!(output, yaml);
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
    let doc = yaml.parse::<YamlFile>();
    assert!(doc.is_ok());
    let doc = doc.unwrap();

    // Verify the YAML parses without errors
    let docs: Vec<_> = doc.documents().collect();
    assert_eq!(docs.len(), 1);

    // Verify API: can access the mapping structure
    let document = doc.document().expect("Should have a document");
    let mapping = document.as_mapping().expect("Document should be a mapping");
    assert_eq!(mapping.len(), 4);

    // Can access simple string keys
    assert_eq!(document.get_string("simple"), Some("value1".to_string()));
    assert_eq!(document.get_string("explicit"), Some("value2".to_string()));

    // Can iterate over all entries including complex keys
    let entries: Vec<_> = mapping.iter().collect();
    assert_eq!(entries.len(), 4);

    // Verify exact round-trip (lossless preservation)
    let output = doc.to_string();
    assert_eq!(output, yaml);
}

#[test]
fn test_nested_complex_keys() {
    let yaml = r#"
? [a, [b, c]]
: nested_list
? {outer: {inner: value}}
: nested_map
"#;
    let doc = yaml.parse::<YamlFile>();
    assert!(doc.is_ok());
    let doc = doc.unwrap();

    // Verify the YAML parses without errors
    let docs: Vec<_> = doc.documents().collect();
    assert_eq!(docs.len(), 1);

    // Verify API: can access the mapping structure
    let document = doc.document().expect("Should have a document");
    let mapping = document.as_mapping().expect("Document should be a mapping");
    assert_eq!(mapping.len(), 2);

    // Can iterate over entries with nested complex keys
    let entries: Vec<_> = mapping.iter().collect();
    assert_eq!(entries.len(), 2);

    // First key: [a, [b, c]] - sequence containing a nested sequence
    let key1 = entries[0]
        .0
        .as_sequence()
        .expect("Key should be a sequence");
    assert!(key1.is_flow_style(), "Key should be flow-style sequence");
    assert_eq!(key1.len(), 2);
    assert_eq!(key1.get(0).unwrap().as_scalar().unwrap().as_string(), "a");
    let second_element = key1.get(1).unwrap();
    let nested_seq = second_element
        .as_sequence()
        .expect("Second element should be a sequence");
    assert!(nested_seq.is_flow_style());
    assert_eq!(nested_seq.len(), 2);
    assert_eq!(
        nested_seq.get(0).unwrap().as_scalar().unwrap().as_string(),
        "b"
    );
    assert_eq!(
        nested_seq.get(1).unwrap().as_scalar().unwrap().as_string(),
        "c"
    );
    assert_eq!(entries[0].1.as_scalar().unwrap().as_string(), "nested_list");

    // Second key: {outer: {inner: value}} - mapping containing a nested mapping
    let key2 = entries[1].0.as_mapping().expect("Key should be a mapping");
    assert!(key2.is_flow_style(), "Key should be flow-style mapping");
    assert_eq!(key2.len(), 1);
    let outer_value = key2.get("outer").unwrap();
    let nested_map = outer_value
        .as_mapping()
        .expect("'outer' value should be a mapping");
    assert!(nested_map.is_flow_style());
    assert_eq!(nested_map.len(), 1);
    assert_eq!(
        nested_map
            .get("inner")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "value"
    );
    assert_eq!(entries[1].1.as_scalar().unwrap().as_string(), "nested_map");

    // Verify exact round-trip (lossless preservation)
    let output = doc.to_string();
    assert_eq!(output, yaml);
}

#[test]
fn test_multiline_explicit_key() {
    let yaml = r#"
? |
  multiline
  key
: value
"#;
    let doc = yaml.parse::<YamlFile>();
    assert!(doc.is_ok());
    let doc = doc.unwrap();

    // Verify the YAML parses without errors
    let docs: Vec<_> = doc.documents().collect();
    assert_eq!(docs.len(), 1);

    // Verify API: can access the mapping structure
    let document = doc.document().expect("Should have a document");
    let mapping = document.as_mapping().expect("Document should be a mapping");
    assert_eq!(mapping.len(), 1);

    // Can iterate and access the entry
    let entries: Vec<_> = mapping.iter().collect();
    assert_eq!(entries.len(), 1);
    // Value should be a scalar "value"
    assert_eq!(entries[0].1.as_scalar().unwrap().as_string(), "value");

    // Verify exact round-trip (lossless preservation)
    let output = doc.to_string();
    assert_eq!(output, yaml);
}

#[test]
fn test_explicit_key_without_value() {
    let yaml = r#"
? key_without_value
? another_key
: has_value
"#;
    let doc = yaml.parse::<YamlFile>();
    assert!(doc.is_ok());
    let doc = doc.unwrap();

    // Verify the YAML parses without errors
    let docs: Vec<_> = doc.documents().collect();
    assert_eq!(docs.len(), 1);

    // Verify API: can access the mapping structure
    let document = doc.document().expect("Should have a document");
    let mapping = document.as_mapping().expect("Document should be a mapping");
    assert_eq!(mapping.len(), 2);

    // Can iterate over all entries including keys without values
    let entries: Vec<_> = mapping.iter().collect();
    assert_eq!(entries.len(), 2);
    // First entry has null/empty value
    assert_eq!(entries[0].0.to_string(), "key_without_value");
    // Second entry has a value
    assert_eq!(entries[1].0.to_string(), "another_key");
    assert_eq!(entries[1].1.to_string(), "has_value");

    // Can also access using get_string
    assert_eq!(
        document.get_string("another_key"),
        Some("has_value".to_string())
    );

    // Verify exact round-trip (lossless preservation)
    let output = doc.to_string();
    assert_eq!(output, yaml);
}

#[test]
fn test_complex_key_preservation() {
    let yaml = r#"
[a, b]: value1
{x: 1}: value2
? explicit
: value3
"#;
    let doc = yaml.parse::<YamlFile>();
    assert!(doc.is_ok());
    let doc = doc.unwrap();

    // Verify API: can access the mapping structure
    let document = doc.document().expect("Should have a document");
    let mapping = document.as_mapping().expect("Document should be a mapping");
    assert_eq!(mapping.len(), 3);

    // Can iterate over all entries
    let entries: Vec<_> = mapping.iter().collect();
    assert_eq!(entries.len(), 3);

    // Can access the simple explicit key
    assert_eq!(document.get_string("explicit"), Some("value3".to_string()));

    // Verify exact round-trip (lossless preservation)
    let output = doc.to_string();
    assert_eq!(output, yaml);
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
    let doc = yaml.parse::<YamlFile>();
    assert!(doc.is_ok());
    let doc = doc.unwrap();

    // Verify the YAML parses without errors
    let docs: Vec<_> = doc.documents().collect();
    assert_eq!(docs.len(), 1);

    // Verify API: can access the mapping structure
    let document = doc.document().expect("Should have a document");
    let mapping = document.as_mapping().expect("Document should be a mapping");
    assert_eq!(mapping.len(), 2);

    // Can iterate over entries with sequence keys and values (YAML 1.2 spec example)
    let entries: Vec<_> = mapping.iter().collect();
    assert_eq!(entries.len(), 2);

    // First entry: block sequence key (? - Detroit Tigers\n  - Chicago Cubs) -> block sequence value
    let key1 = entries[0]
        .0
        .as_sequence()
        .expect("First key should be a sequence");
    assert!(
        !key1.is_flow_style(),
        "First key should be block-style sequence"
    );
    assert_eq!(key1.len(), 2);
    assert_eq!(
        key1.get(0).unwrap().as_scalar().unwrap().as_string(),
        "Detroit Tigers"
    );
    assert_eq!(
        key1.get(1).unwrap().as_scalar().unwrap().as_string(),
        "Chicago Cubs"
    );
    let val1 = entries[0]
        .1
        .as_sequence()
        .expect("First value should be a sequence");
    assert!(
        !val1.is_flow_style(),
        "First value should be block-style sequence"
    );
    assert_eq!(val1.len(), 1);
    assert_eq!(
        val1.get(0).unwrap().as_scalar().unwrap().as_string(),
        "2001-07-23"
    );

    // Second entry: flow sequence key [ New York Yankees, Atlanta Braves ] -> flow sequence value
    let key2 = entries[1]
        .0
        .as_sequence()
        .expect("Second key should be a sequence");
    assert!(
        key2.is_flow_style(),
        "Second key should be flow-style sequence"
    );
    assert_eq!(key2.len(), 2);
    assert_eq!(
        key2.get(0).unwrap().as_scalar().unwrap().as_string(),
        "New York Yankees"
    );
    assert_eq!(
        key2.get(1).unwrap().as_scalar().unwrap().as_string(),
        "Atlanta Braves"
    );
    let val2 = entries[1]
        .1
        .as_sequence()
        .expect("Second value should be a sequence");
    assert!(
        val2.is_flow_style(),
        "Second value should be flow-style sequence"
    );
    assert_eq!(val2.len(), 3);
    assert_eq!(
        val2.get(0).unwrap().as_scalar().unwrap().as_string(),
        "2001-07-02"
    );
    assert_eq!(
        val2.get(1).unwrap().as_scalar().unwrap().as_string(),
        "2001-08-12"
    );
    assert_eq!(
        val2.get(2).unwrap().as_scalar().unwrap().as_string(),
        "2001-08-14"
    );

    // Verify exact round-trip (lossless preservation)
    let output = doc.to_string();
    assert_eq!(output, yaml);
}

#[test]
fn test_complex_key_in_flow_mapping() {
    let yaml = r#"
{ [a, b]: value1, {x: 1}: value2 }
"#;
    let doc = yaml.parse::<YamlFile>();
    assert!(doc.is_ok());
    let doc = doc.unwrap();

    // Verify the YAML parses without errors
    let docs: Vec<_> = doc.documents().collect();
    assert_eq!(docs.len(), 1);

    // Verify API: document is a flow mapping with complex keys
    let document = doc.document().expect("Should have a document");
    let mapping = document.as_mapping().expect("Document should be a mapping");
    assert_eq!(mapping.len(), 2, "Should have 2 key-value pairs");

    // Verify exact round-trip (lossless preservation)
    let output = doc.to_string();
    assert_eq!(output, yaml);
}
