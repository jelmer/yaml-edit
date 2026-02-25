//! Test mutation and editing edge cases
//!
//! Tests cover:
//! - Mutating anchor targets and alias behavior
//! - Removing keys referenced by aliases
//! - Editing block scalars
//! - Mutating tags on tagged nodes
//! - Modifying empty collections

use std::str::FromStr;
use yaml_edit::YamlFile;

/// Test mutating the target of an anchor
/// When an anchored value is changed, aliases should still reference the anchor
#[test]
fn test_mutate_anchor_target() {
    let yaml = r#"anchor: &x original
ref: *x"#;

    let parsed = YamlFile::from_str(yaml).expect("Should parse");
    let doc = parsed.document().expect("Should have document");
    let mapping = doc.as_mapping().expect("Should be mapping");

    // Verify initial state
    let anchor_val = mapping.get("anchor").expect("Should have anchor");
    assert_eq!(anchor_val.as_scalar().unwrap().as_string(), "original");

    let ref_val = mapping.get("ref").expect("Should have ref");
    assert!(ref_val.is_alias(), "ref should be an alias");
    assert_eq!(ref_val.as_alias().unwrap().name(), "x");

    // Mutate the anchored value
    mapping.set("anchor", "modified");

    // Verify the anchor value changed
    let new_anchor_val = mapping.get("anchor").expect("Should have anchor");
    assert_eq!(new_anchor_val.as_scalar().unwrap().as_string(), "modified");

    // The alias should still be an alias node pointing to 'x'
    let ref_val_after = mapping.get("ref").expect("Should have ref");
    assert!(ref_val_after.is_alias(), "ref should still be an alias");
    assert_eq!(ref_val_after.as_alias().unwrap().name(), "x");

    // Verify output is valid YAML
    let output = doc.to_string();
    let reparsed = YamlFile::from_str(&output).expect("Output should be valid YAML");
    assert!(reparsed.document().is_some());
}

/// Test mutating nested mapping that has an anchor
#[test]
fn test_mutate_nested_anchor_target() {
    let yaml = r#"config: &defaults
  timeout: 30
  retries: 3
server: *defaults"#;

    let parsed = YamlFile::from_str(yaml).expect("Should parse");
    let doc = parsed.document().expect("Should have document");
    let mapping = doc.as_mapping().expect("Should be mapping");

    // Verify initial state
    let config = mapping
        .get_mapping("config")
        .expect("Should have config mapping");
    assert_eq!(
        config.get("timeout").unwrap().as_scalar().unwrap().as_i64(),
        Some(30)
    );

    // Mutate the anchored mapping
    config.set("timeout", 60);
    config.set("max_connections", 100);

    // Verify the config mapping changed
    assert_eq!(
        config.get("timeout").unwrap().as_scalar().unwrap().as_i64(),
        Some(60)
    );
    assert!(config.contains_key("max_connections"));

    // The alias should still point to 'defaults'
    let server_val = mapping.get("server").expect("Should have server");
    assert!(server_val.is_alias(), "server should be an alias");
    assert_eq!(server_val.as_alias().unwrap().name(), "defaults");

    // Verify output is valid YAML
    let output = doc.to_string();
    let reparsed = YamlFile::from_str(&output).expect("Output should be valid YAML");
    assert!(reparsed.document().is_some());
}

/// Test removing a key that has an anchor
/// The alias should remain in the structure (as a dangling reference)
#[test]
fn test_remove_anchored_key() {
    let yaml = r#"orig: &x value
ref: *x"#;

    let parsed = YamlFile::from_str(yaml).expect("Should parse");
    let doc = parsed.document().expect("Should have document");
    let mapping = doc.as_mapping().expect("Should be mapping");

    // Verify initial state
    assert_eq!(mapping.keys().count(), 2);
    assert!(mapping.contains_key("orig"));
    assert!(mapping.contains_key("ref"));

    // Remove the anchored key
    mapping.remove("orig");

    // Verify orig is removed
    assert_eq!(mapping.keys().count(), 1);
    assert!(!mapping.contains_key("orig"));
    assert!(mapping.contains_key("ref"));

    // The alias should still exist (as a dangling reference)
    let ref_val = mapping.get("ref").expect("Should have ref");
    assert!(ref_val.is_alias(), "ref should still be an alias");
    assert_eq!(ref_val.as_alias().unwrap().name(), "x");

    // Verify output is valid YAML (with dangling reference)
    let output = doc.to_string();
    let reparsed = YamlFile::from_str(&output).expect("Output should be valid YAML");
    assert!(reparsed.document().is_some());
}

/// Test editing block scalar content
/// Verify we can replace block scalar values
#[test]
fn test_edit_block_scalar_content() {
    let yaml = r#"text: |
  Line 1
  Line 2
"#;

    let parsed = YamlFile::from_str(yaml).expect("Should parse");
    let doc = parsed.document().expect("Should have document");
    let mapping = doc.as_mapping().expect("Should be mapping");

    // Verify initial content
    let text_val = mapping.get("text").expect("Should have text");
    assert!(text_val.is_scalar(), "text should be scalar");
    let initial_text = text_val.as_scalar().unwrap().as_string();
    assert_eq!(initial_text, "Line 1\nLine 2\n");

    // Replace the block scalar with new content
    mapping.set("text", "New single line");

    // Verify the content changed
    let new_text_val = mapping.get("text").expect("Should have text");
    assert_eq!(
        new_text_val.as_scalar().unwrap().as_string(),
        "New single line"
    );

    // Verify output is valid YAML
    let output = doc.to_string();
    let reparsed = YamlFile::from_str(&output).expect("Output should be valid YAML");
    assert!(reparsed.document().is_some());
}

/// Test replacing block scalar with another block scalar
#[test]
fn test_replace_block_scalar_with_multiline() {
    let yaml = r#"description: |
  Original text
  on multiple lines
"#;

    let parsed = YamlFile::from_str(yaml).expect("Should parse");
    let doc = parsed.document().expect("Should have document");
    let mapping = doc.as_mapping().expect("Should be mapping");

    // Replace with new multiline content
    // Note: We're setting a string value, which may render as plain/quoted scalar
    // depending on the implementation's formatting choices
    let new_content = "New line 1\nNew line 2\nNew line 3";
    mapping.set("description", new_content);

    // Verify the content changed
    let new_val = mapping.get("description").expect("Should have description");
    let actual_content = new_val.as_scalar().unwrap().as_string();
    // Block scalars preserve trailing newline
    assert_eq!(actual_content, format!("{}\n", new_content));

    // Verify output is valid YAML
    let output = doc.to_string();
    let reparsed = YamlFile::from_str(&output).expect("Output should be valid YAML");
    assert!(reparsed.document().is_some());

    // Verify re-parsed content matches (with trailing newline from block scalar)
    let reparsed_doc = reparsed.document().unwrap();
    let reparsed_mapping = reparsed_doc.as_mapping().unwrap();
    let reparsed_val = reparsed_mapping.get("description").unwrap();
    assert_eq!(
        reparsed_val.as_scalar().unwrap().as_string(),
        format!("{}\n", new_content)
    );
}

/// Test mutating tagged nodes
/// Verify we can change values of tagged nodes
#[test]
fn test_mutate_tagged_node_value() {
    let yaml = r#"date: !!timestamp 2024-01-01
count: !!int 42"#;

    let parsed = YamlFile::from_str(yaml).expect("Should parse");
    let doc = parsed.document().expect("Should have document");
    let mapping = doc.as_mapping().expect("Should be mapping");

    // Verify initial tagged values exist
    assert!(mapping.contains_key("date"));
    assert!(mapping.contains_key("count"));

    // Mutate the tagged values
    // Note: This replaces the tagged node with a plain scalar
    mapping.set("date", "2024-12-31");
    mapping.set("count", 100);

    // Verify the values changed
    let new_date = mapping.get("date").expect("Should have date");
    assert_eq!(new_date.as_scalar().unwrap().as_string(), "2024-12-31");

    let new_count = mapping.get("count").expect("Should have count");
    assert_eq!(new_count.as_scalar().unwrap().as_i64(), Some(100));

    // Verify output is valid YAML
    let output = doc.to_string();
    let reparsed = YamlFile::from_str(&output).expect("Output should be valid YAML");
    assert!(reparsed.document().is_some());
}

/// Test adding keys to an empty flow mapping
#[test]
fn test_add_to_empty_flow_mapping() {
    let yaml = "empty: {}";

    let parsed = YamlFile::from_str(yaml).expect("Should parse");
    let doc = parsed.document().expect("Should have document");
    let mapping = doc.as_mapping().expect("Should be mapping");

    // Verify initial state - empty mapping
    let empty_map = mapping
        .get_mapping("empty")
        .expect("Should have empty mapping");
    assert_eq!(empty_map.keys().count(), 0, "Should be empty initially");

    // Add keys to the empty mapping
    empty_map.set("a", 1);
    empty_map.set("b", 2);

    // Verify keys were added
    assert_eq!(empty_map.keys().count(), 2, "Should have 2 keys");
    assert_eq!(
        empty_map.get("a").unwrap().as_scalar().unwrap().as_i64(),
        Some(1)
    );
    assert_eq!(
        empty_map.get("b").unwrap().as_scalar().unwrap().as_i64(),
        Some(2)
    );

    // Verify output is valid YAML
    let output = doc.to_string();
    let reparsed = YamlFile::from_str(&output).expect("Output should be valid YAML");
    assert!(reparsed.document().is_some());
}

/// Test adding items to an empty flow sequence
#[test]
fn test_add_to_empty_flow_sequence() {
    let yaml = "items: []";

    let parsed = YamlFile::from_str(yaml).expect("Should parse");
    let doc = parsed.document().expect("Should have document");
    let mapping = doc.as_mapping().expect("Should be mapping");

    // Verify initial state - empty sequence
    let empty_seq = mapping
        .get_sequence("items")
        .expect("Should have empty sequence");
    assert_eq!(empty_seq.len(), 0, "Should be empty initially");

    // Add items to the empty sequence
    empty_seq.push("first");
    empty_seq.push("second");
    empty_seq.push("third");

    // Verify items were added
    assert_eq!(empty_seq.len(), 3, "Should have 3 items");
    assert_eq!(
        empty_seq.get(0).unwrap().as_scalar().unwrap().as_string(),
        "first"
    );
    assert_eq!(
        empty_seq.get(1).unwrap().as_scalar().unwrap().as_string(),
        "second"
    );
    assert_eq!(
        empty_seq.get(2).unwrap().as_scalar().unwrap().as_string(),
        "third"
    );

    // Verify output is valid YAML
    let output = doc.to_string();
    let reparsed = YamlFile::from_str(&output).expect("Output should be valid YAML");
    assert!(reparsed.document().is_some());
}

/// Test removing all keys from a mapping (making it empty)
#[test]
fn test_empty_mapping_by_removal() {
    let yaml = r#"config:
  a: 1
  b: 2
  c: 3"#;

    let parsed = YamlFile::from_str(yaml).expect("Should parse");
    let doc = parsed.document().expect("Should have document");
    let mapping = doc.as_mapping().expect("Should be mapping");

    let config = mapping.get_mapping("config").expect("Should have config");
    assert_eq!(config.keys().count(), 3, "Should have 3 keys initially");

    // Remove all keys
    config.remove("a");
    config.remove("b");
    config.remove("c");

    // Verify empty
    assert_eq!(config.keys().count(), 0, "Should be empty after removal");

    // Verify output is valid YAML
    let output = doc.to_string();
    let reparsed = YamlFile::from_str(&output).expect("Output should be valid YAML");
    assert!(reparsed.document().is_some());
}
