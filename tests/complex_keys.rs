use rowan::ast::AstNode;
use yaml_edit::validator::{Rule, Validator};
use yaml_edit::YamlFile;

mod common;
use common::assert_file_cst_ok;

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

/// Scalar keys of the root mapping of `yaml`, with `None` for any other key.
fn root_scalar_keys(yaml: &str) -> Vec<Option<String>> {
    let doc = yaml.parse::<YamlFile>().unwrap();
    let mapping = doc.document().unwrap().as_mapping().unwrap();
    mapping
        .iter()
        .map(|(k, _)| k.as_scalar().map(|s| s.as_string()))
        .collect()
}

#[test]
fn test_plain_key_after_complex_key() {
    // The second key used to be a bare STRING with no SCALAR node, so it
    // was invisible to keys() and get().
    let doc = "[1]: 2\nk: v\n".parse::<YamlFile>().unwrap();
    let document = doc.document().unwrap();
    assert_eq!(document.get_string("k"), Some("v".to_string()));
    assert_eq!(root_scalar_keys("[1]: 2\nk: v\n"), [None, Some("k".into())]);
}

#[test]
fn test_annotated_key_after_complex_key() {
    // Used to be the single value `2 &a k: v`.
    assert_eq!(
        root_scalar_keys("[1]: 2\n&a k: v\n"),
        [None, Some("k".into())]
    );
    assert_eq!(root_scalar_keys("&k [a]: v\n&j {b: 1}: w\n"), [None, None]);
}

#[test]
fn test_explicit_key_after_complex_key() {
    assert_eq!(
        root_scalar_keys("[1]: 2\n? x\n: y\nz: 1\n"),
        [None, Some("x".into()), Some("z".into())]
    );
}

#[test]
fn test_nested_complex_key_mapping_ends_at_dedent() {
    assert_eq!(
        root_scalar_keys("a:\n  [1]: 2\nj: w\n"),
        [Some("a".into()), Some("j".into())]
    );
}

#[test]
fn test_sequence_entry_after_complex_key_is_an_error() {
    assert!("[1]: 2\n- x\n".parse::<YamlFile>().is_err());
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

/// Entries of a block mapping as `(key, value)` text, with `~` for a null.
fn entry_texts(mapping: &yaml_edit::Mapping) -> Vec<(String, String)> {
    mapping
        .iter()
        .map(|(k, v)| {
            let value = match v.as_scalar() {
                Some(s) if s.is_null() => "~".to_string(),
                _ => v.to_string().trim_end().to_string(),
            };
            (k.to_string(), value)
        })
        .collect()
}

#[test]
fn test_nested_explicit_key_with_empty_value() {
    // The `b: c` line used to become the value of `a`.
    let expected = [
        ("a".to_string(), "~".to_string()),
        ("b".to_string(), "c".to_string()),
    ];

    let doc = "k:\n  ? a\n  :\n  b: c\n".parse::<YamlFile>().unwrap();
    let k = doc.document().unwrap().as_mapping().unwrap().get("k");
    assert_eq!(entry_texts(k.unwrap().as_mapping().unwrap()), expected);

    let doc = "- ? a\n  :\n  b: c\n".parse::<YamlFile>().unwrap();
    let first = doc.document().unwrap().as_sequence().unwrap().first();
    assert_eq!(entry_texts(first.unwrap().as_mapping().unwrap()), expected);
}

#[test]
fn test_nested_explicit_key_with_indentless_sequence_value() {
    let doc = "k:\n  ? a\n  :\n  - x\n".parse::<YamlFile>().unwrap();
    let k = doc.document().unwrap().as_mapping().unwrap().get("k");
    assert_eq!(
        entry_texts(k.unwrap().as_mapping().unwrap()),
        [("a".to_string(), "- x".to_string())]
    );
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

/// A mapping's scalar entries as `(key, value)` strings.
fn entries(yaml: &str) -> Vec<(String, String)> {
    let doc = yaml.parse::<YamlFile>().unwrap();
    assert_eq!(doc.to_string(), yaml);
    let document = doc.document().unwrap();
    let mapping = document.as_mapping().unwrap();
    mapping
        .iter()
        .map(|(k, v)| {
            (
                k.as_scalar().unwrap().as_string(),
                v.as_scalar().unwrap().as_string(),
            )
        })
        .collect()
}

fn pairs(expected: &[(&str, &str)]) -> Vec<(String, String)> {
    expected
        .iter()
        .map(|(k, v)| (k.to_string(), v.to_string()))
        .collect()
}

#[test]
fn test_block_scalar_in_explicit_entry_does_not_swallow_next_entry() {
    assert_eq!(entries("? a\n: |\nb: 1\n"), pairs(&[("a", ""), ("b", "1")]));
    assert_eq!(entries("? a\n: >\n? b\n"), pairs(&[("a", ""), ("b", "")]));
    assert_eq!(entries("? |\nb: 1\n"), pairs(&[("", ""), ("b", "1")]));
}

#[test]
fn test_block_scalar_in_nested_explicit_entry_does_not_swallow_next_entry() {
    let yaml = "x:\n  ? a\n  : |\n  b: 1\n";
    let doc = yaml.parse::<YamlFile>().unwrap();
    assert_eq!(doc.to_string(), yaml);
    let document = doc.document().unwrap();
    let x = document.as_mapping().unwrap().get("x").unwrap();
    let mapping = x.as_mapping().unwrap();
    assert_eq!(mapping.len(), 2);
    let a = mapping.get("a").unwrap();
    assert_eq!(a.as_scalar().unwrap().as_string(), "");
    let b = mapping.get("b").unwrap();
    assert_eq!(b.as_scalar().unwrap().as_string(), "1");
}

/// A comment after `?` or an explicit `:` ends its line just as a line
/// break does, so the next line belongs to the entry only if it is
/// indented past the indicator.
#[test]
fn test_comment_after_explicit_indicator_leaves_next_entry_alone() {
    let yaml = "? # c\nb: 1\n";
    let doc = yaml.parse::<YamlFile>().unwrap();
    assert_eq!(doc.to_string(), yaml);
    assert_eq!(
        yaml_edit::debug::tree_to_string(doc.syntax()),
        r##"DOCUMENT
  MAPPING
    MAPPING_ENTRY
      QUESTION: "?"
      WHITESPACE: " "
      KEY
        COMMENT: "# c"
        SCALAR
          NULL: ""
      VALUE
        SCALAR
          NULL: ""
      NEWLINE: "\\n"
    MAPPING_ENTRY
      KEY
        SCALAR
          STRING: "b"
      COLON: ":"
      WHITESPACE: " "
      VALUE
        SCALAR
          INT: "1"
      NEWLINE: "\\n"
"##
    );

    assert_eq!(
        entries("? a\n: # c\nb: 1\n"),
        pairs(&[("a", ""), ("b", "1")])
    );
    assert_eq!(entries("? # c"), pairs(&[("", "")]));
    assert_eq!(entries("? a\n: # c"), pairs(&[("a", "")]));

    for yaml in ["- ? # c\n- 1\n", "- ? a\n  : # c\n- 1\n"] {
        let doc = yaml.parse::<YamlFile>().unwrap();
        assert_eq!(doc.to_string(), yaml);
        let document = doc.document().unwrap();
        let sequence = document.as_sequence().unwrap();
        assert_eq!(sequence.len(), 2, "{yaml:?}");
        let first = sequence.get(0).unwrap();
        let mapping = first.as_mapping().unwrap();
        assert_eq!(mapping.len(), 1, "{yaml:?}");
        let (_, value) = mapping.iter().next().unwrap();
        assert!(value.as_scalar().unwrap().is_null(), "{yaml:?}");
        let second = sequence.get(1).unwrap();
        assert_eq!(second.as_scalar().unwrap().as_string(), "1");
    }
}

/// Content on the line after the comment still belongs to the entry when
/// it is indented to, or is an indentless sequence.
#[test]
fn test_comment_after_explicit_indicator_keeps_next_line_node() {
    let yaml = "? # c\n  k: v\n: x\n";
    let doc = yaml.parse::<YamlFile>().unwrap();
    assert_eq!(doc.to_string(), yaml);
    let document = doc.document().unwrap();
    let mapping = document.as_mapping().unwrap();
    assert_eq!(mapping.len(), 1);
    let (key, value) = mapping.iter().next().unwrap();
    let key = key.as_mapping().unwrap();
    assert_eq!(key.get("k").unwrap().as_scalar().unwrap().as_string(), "v");
    assert_eq!(value.as_scalar().unwrap().as_string(), "x");

    let yaml = "? # c\n- a\n: v\n";
    let doc = yaml.parse::<YamlFile>().unwrap();
    assert_eq!(doc.to_string(), yaml);
    let document = doc.document().unwrap();
    let mapping = document.as_mapping().unwrap();
    assert_eq!(mapping.len(), 1);
    let (key, value) = mapping.iter().next().unwrap();
    let key = key.as_sequence().unwrap();
    assert_eq!(key.len(), 1);
    assert_eq!(key.get(0).unwrap().as_scalar().unwrap().as_string(), "a");
    assert_eq!(value.as_scalar().unwrap().as_string(), "v");

    for yaml in ["? a\n: # c\n- b\n", "? a\n: # c\n  - b\n"] {
        let doc = yaml.parse::<YamlFile>().unwrap();
        assert_eq!(doc.to_string(), yaml);
        let document = doc.document().unwrap();
        let mapping = document.as_mapping().unwrap();
        assert_eq!(mapping.len(), 1, "{yaml:?}");
        let value = mapping.get("a").unwrap();
        let value = value.as_sequence().unwrap();
        assert_eq!(value.len(), 1, "{yaml:?}");
        assert_eq!(value.get(0).unwrap().as_scalar().unwrap().as_string(), "b");
    }
}

/// A string key that would be written as a block scalar is written
/// double-quoted instead, tagged or not: a block scalar cannot be an
/// implicit key.
#[test]
fn test_set_multiline_string_key_is_quoted() {
    use yaml_edit::{ScalarValue, YamlValue};
    let plain = ScalarValue::string("x\ny\n");
    let tagged = ScalarValue::string("x\ny\n").with_tag("!t");
    let tagged_value = YamlValue::tagged("!t", YamlValue::Scalar(ScalarValue::string("x\ny\n")));
    for (key, expected) in [
        (YamlValue::Scalar(plain), "a: 1\n\"x\\ny\\n\": 2\n"),
        (YamlValue::Scalar(tagged), "a: 1\n!t \"x\\ny\\n\": 2\n"),
        (tagged_value, "a: 1\n!t \"x\\ny\\n\": 2\n"),
    ] {
        let file: YamlFile = "a: 1\n".parse().unwrap();
        let mapping = file.document().unwrap().as_mapping().unwrap();
        mapping.set(&key, 2);
        assert_eq!(file.to_string(), expected);
        assert_file_cst_ok(&file);
        assert_eq!(mapping.len(), 2);
        assert_eq!(mapping.get(&key).unwrap().to_string(), "2");
    }
}

/// A lone tag or anchor in an explicit key or value annotates an implicit
/// null when the next line is not indented past the `?` or `:`, as PyYAML
/// reads it: the line is the mapping's next entry.
#[test]
fn test_lone_annotation_in_explicit_entry_leaves_next_entry_alone() {
    let yaml = "? !!str\nb: 1\n";
    let doc = yaml.parse::<YamlFile>().unwrap();
    assert_eq!(doc.to_string(), yaml);
    assert_eq!(
        yaml_edit::debug::tree_to_string(doc.syntax()),
        r##"DOCUMENT
  MAPPING
    MAPPING_ENTRY
      QUESTION: "?"
      WHITESPACE: " "
      KEY
        TAGGED_NODE
          TAG: "!!str"
          SCALAR
            NULL: ""
      VALUE
        SCALAR
          NULL: ""
      NEWLINE: "\\n"
    MAPPING_ENTRY
      KEY
        SCALAR
          STRING: "b"
      COLON: ":"
      WHITESPACE: " "
      VALUE
        SCALAR
          INT: "1"
      NEWLINE: "\\n"
"##
    );

    for yaml in [
        "? !!str\nb: 1\n",
        "? &a\nb: 1\n",
        "? !!str &a\nb: 1\n",
        "? a\n: !!str\nb: 1\n",
        "? a\n: &x\nb: 1\n",
        "- ? !!str\n  b: 1\n",
        "- ? a\n  : &x\n  b: 1\n",
        "? !!map\n? b\n: 1\n",
        "? a\n: &x\n? b\n: 1\n",
    ] {
        let doc = yaml.parse::<YamlFile>().unwrap();
        assert_eq!(doc.to_string(), yaml);
        let document = doc.document().unwrap();
        let mapping = match document.as_sequence() {
            Some(sequence) => {
                assert_eq!(sequence.len(), 1, "{yaml:?}");
                sequence.get(0).unwrap().as_mapping().unwrap().clone()
            }
            None => document.as_mapping().unwrap(),
        };
        assert_eq!(mapping.len(), 2, "{yaml:?}");
        let (_, value) = mapping.iter().next().unwrap();
        assert!(value.as_mapping().is_none(), "{yaml:?}");
        assert_eq!(
            mapping.get("b").unwrap().as_scalar().unwrap().as_string(),
            "1",
            "{yaml:?}"
        );
    }
}

/// Indented past the `?` or `:`, the next line is still the annotated
/// node.
#[test]
fn test_lone_annotation_in_explicit_entry_keeps_indented_node() {
    let yaml = "? !!map\n  b: 1\n: v\n";
    let doc = yaml.parse::<YamlFile>().unwrap();
    assert_eq!(doc.to_string(), yaml);
    let document = doc.document().unwrap();
    let mapping = document.as_mapping().unwrap();
    assert_eq!(mapping.len(), 1);
    let (key, value) = mapping.iter().next().unwrap();
    let key = key.as_tagged().unwrap().as_mapping().unwrap();
    assert_eq!(key.get("b").unwrap().as_scalar().unwrap().as_string(), "1");
    assert_eq!(value.as_scalar().unwrap().as_string(), "v");

    let yaml = "? a\n: &x\n  b: 1\n";
    let doc = yaml.parse::<YamlFile>().unwrap();
    assert_eq!(doc.to_string(), yaml);
    let document = doc.document().unwrap();
    let mapping = document.as_mapping().unwrap();
    assert_eq!(mapping.len(), 1);
    let value = mapping.get("a").unwrap();
    let value = value.as_mapping().unwrap();
    assert_eq!(
        value.get("b").unwrap().as_scalar().unwrap().as_string(),
        "1"
    );
}

/// An explicit value's tagged sequence may sit at the `:`'s own column,
/// but a `-` left of it is the enclosing sequence's next entry.
#[test]
fn test_tagged_sequence_in_explicit_value_is_bounded_by_colon() {
    let yaml = "? a\n: !!seq\n- b\n";
    let doc = yaml.parse::<YamlFile>().unwrap();
    assert_eq!(doc.to_string(), yaml);
    let document = doc.document().unwrap();
    let mapping = document.as_mapping().unwrap();
    assert_eq!(mapping.len(), 1);
    let value = mapping.get("a").unwrap();
    let value = value.as_tagged().unwrap().as_sequence().unwrap();
    assert_eq!(value.len(), 1);
    assert_eq!(value.get(0).unwrap().as_scalar().unwrap().as_string(), "b");

    let yaml = "- ? a\n  : !!seq\n- b\n";
    let doc = yaml.parse::<YamlFile>().unwrap();
    assert_eq!(doc.to_string(), yaml);
    let document = doc.document().unwrap();
    let sequence = document.as_sequence().unwrap();
    assert_eq!(sequence.len(), 2);
    let first = sequence.get(0).unwrap();
    let value = first.as_mapping().unwrap().get("a").unwrap();
    let value = value.as_tagged().unwrap();
    assert_eq!(value.tag(), Some("!!seq".to_string()));
    assert_eq!(value.as_sequence(), None);
    assert!(value.value().unwrap().is_null());
    let second = sequence.get(1).unwrap();
    assert_eq!(second.as_scalar().unwrap().as_string(), "b");
}

/// An explicit key's tagged or anchored sequence may sit at the `?`'s own
/// column, as `seq-spaces` allows, but a `-` left of it is the enclosing
/// sequence's next entry.
#[test]
fn test_annotated_sequence_in_explicit_key_at_question_column() {
    for (yaml, tag) in [
        ("? !!seq\n- a\n: v\n", Some("!!seq")),
        ("? &x\n- a\n: v\n", None),
        ("- ? !!seq\n  - a\n  : v\n", Some("!!seq")),
        ("- ? &x\n  - a\n  : v\n", None),
    ] {
        let doc = yaml.parse::<YamlFile>().unwrap();
        assert_eq!(doc.to_string(), yaml);
        assert_file_cst_ok(&doc);
        assert_eq!(
            yaml_edit::validator::Validator::new()
                .validate_syntax(doc.syntax())
                .into_iter()
                .map(|v| v.message)
                .collect::<Vec<_>>(),
            Vec::<String>::new(),
            "{yaml:?}"
        );
        let document = doc.document().unwrap();
        let mapping = match document.as_sequence() {
            Some(sequence) => {
                assert_eq!(sequence.len(), 1, "{yaml:?}");
                sequence.get(0).unwrap().as_mapping().unwrap().clone()
            }
            None => document.as_mapping().unwrap(),
        };
        assert_eq!(mapping.len(), 1, "{yaml:?}");
        let (key, value) = mapping.iter().next().unwrap();
        let key = match tag {
            Some(tag) => {
                let tagged = key.as_tagged().unwrap();
                assert_eq!(tagged.tag(), Some(tag.to_string()));
                tagged.as_sequence().unwrap()
            }
            None => key.as_sequence().unwrap().clone(),
        };
        assert_eq!(key.len(), 1, "{yaml:?}");
        assert_eq!(key.get(0).unwrap().as_scalar().unwrap().as_string(), "a");
        assert_eq!(value.as_scalar().unwrap().as_string(), "v");
    }

    let yaml = "- ? !!seq\n- a\n";
    let doc = yaml.parse::<YamlFile>().unwrap();
    assert_eq!(doc.to_string(), yaml);
    let document = doc.document().unwrap();
    let sequence = document.as_sequence().unwrap();
    assert_eq!(sequence.len(), 2);
    let first = sequence.get(0).unwrap();
    let (key, value) = first.as_mapping().unwrap().iter().next().unwrap();
    let key = key.as_tagged().unwrap();
    assert_eq!(key.tag(), Some("!!seq".to_string()));
    assert!(key.value().unwrap().is_null());
    assert!(value.as_scalar().unwrap().is_null());
    let second = sequence.get(1).unwrap();
    assert_eq!(second.as_scalar().unwrap().as_string(), "a");
}

#[test]
fn test_explicit_key_after_implicit_with_indentless_sequence_value() {
    for (yaml, len) in [
        ("a: 1\n? b\n:\n- x\n", 2),
        ("a: 1\n? b\n: # c\n- x\n", 2),
        ("a: 1\n? b\n:\n# c\n- x\n", 2),
        ("a: 1\n? b\n:\n- x\nc: 2\n", 3),
    ] {
        let doc = yaml.parse::<YamlFile>().unwrap();
        assert_eq!(doc.to_string(), yaml);
        assert_file_cst_ok(&doc);
        let mapping = doc.document().unwrap().as_mapping().unwrap();
        assert_eq!(mapping.len(), len, "{yaml:?}");
        let value = mapping.get("b").unwrap();
        let value = value.as_sequence().unwrap();
        assert_eq!(value.len(), 1, "{yaml:?}");
        assert_eq!(value.get(0).unwrap().as_scalar().unwrap().as_string(), "x");
    }
}

/// A `:` on a later line left of a nested `?` is the value of the entry
/// enclosing it. `? ? a\n: b\n` gave the inner entry the value `b` and
/// left the outer one without, so setting the outer value wrote text that
/// re-parsed differently.
#[test]
fn test_value_left_of_a_nested_explicit_key() {
    for (yaml, inner_key, inner_value) in [
        ("? ? \n: v\n", "", ""),
        ("? ? a\n: v\n", "a", ""),
        ("? ? a\n  : b\n: v\n", "a", "b"),
    ] {
        let file = yaml.parse::<YamlFile>().unwrap();
        let mapping = file.document().unwrap().as_mapping().unwrap();
        let entries: Vec<_> = mapping.iter().collect();
        assert_eq!(entries.len(), 1, "{yaml:?}");
        assert_eq!(entries[0].1.as_scalar().unwrap().as_string(), "v");
        let key = entries[0].0.as_mapping().unwrap();
        let inner: Vec<_> = key.iter().collect();
        assert_eq!(inner.len(), 1, "{yaml:?}");
        assert_eq!(inner[0].0.as_scalar().unwrap().as_string(), inner_key);
        assert_eq!(inner[0].1.as_scalar().unwrap().as_string(), inner_value);
        assert_eq!(file.to_string(), yaml);
    }
}

/// Indented comment lines after a bare `?` do not make the next line its
/// key. `?\n # c\nk: v\n` gave the key an empty block mapping, which no
/// edit could write back, so the entry before `k` read back differently
/// once entries were moved.
#[test]
fn test_comment_lines_after_a_bare_explicit_key() {
    let yaml = "?\n # c\nk: v\n";
    let file = yaml.parse::<YamlFile>().unwrap();
    let mapping = file.document().unwrap().as_mapping().unwrap();
    let entries: Vec<_> = mapping.iter().collect();
    assert_eq!(entries.len(), 2);
    assert!(entries[0].0.as_scalar().unwrap().is_null());
    assert_eq!(entries[1].0.as_scalar().unwrap().as_string(), "k");

    mapping.reorder_fields(["k"]);
    assert_eq!(file.to_string(), " # c\nk: v\n?\n");
    assert_file_cst_ok(&file);
    let reparsed = file.to_string().parse::<YamlFile>().unwrap();
    assert_eq!(
        yaml_edit::debug::shape_to_string(file.syntax()),
        yaml_edit::debug::shape_to_string(reparsed.syntax())
    );
}

/// An edit to a collection that is a key does what it is asked even when
/// that makes the key equal to another one; the validator then reports the
/// duplicate.
#[test]
fn test_edit_inside_key_making_it_a_duplicate() {
    let file = "? [a]\n: 1\n? []\n: 2\n".parse::<YamlFile>().unwrap();
    let mapping = file.document().unwrap().as_mapping().unwrap();
    let key = mapping.iter().next().unwrap().0;
    assert_eq!(
        key.as_sequence()
            .unwrap()
            .pop()
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "a"
    );
    assert_eq!(file.to_string(), "? []\n: 1\n? []\n: 2\n");
    assert_file_cst_ok(&file);
    let reparsed = file.to_string().parse::<YamlFile>().unwrap();
    assert_eq!(
        yaml_edit::debug::shape_to_string(file.syntax()),
        yaml_edit::debug::shape_to_string(reparsed.syntax())
    );
    let violations: Vec<_> = Validator::new()
        .validate_syntax(reparsed.syntax())
        .into_iter()
        .map(|v| (v.rule, v.to_string()))
        .collect();
    assert_eq!(
        violations,
        vec![(
            Rule::DuplicateKeys,
            "[ERROR] 3:1: Duplicate key: \"[]\" (semantically equal to \"[]\") (DuplicateKeys)"
                .to_string()
        )]
    );
}
