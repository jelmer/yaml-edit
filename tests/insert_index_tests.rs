use std::str::FromStr;
use yaml_edit::{Document, YamlFile};

#[test]
fn test_insert_at_index_empty_document() {
    let yaml = YamlFile::from_str("").unwrap();
    yaml.insert_at_index(0, "first", "value");

    // Verify via API first
    let doc = yaml.document().expect("Should have document");
    let mapping = doc.as_mapping().expect("Should be mapping");
    assert_eq!(mapping.len(), 1);
    let value_node = mapping.get("first").expect("Should have 'first' key");
    let value_scalar = value_node.as_scalar().expect("Should be scalar");
    assert_eq!(value_scalar.as_string(), "value");

    // Verify exact output
    let output = yaml.to_string();
    assert_eq!(output, "first: value\n");
}

#[test]
fn test_insert_at_index_update_existing() {
    let yaml = YamlFile::from_str("first: 1\nsecond: 2\nthird: 3").unwrap();

    // Update existing key
    yaml.insert_at_index(2, "first", "updated");

    // Verify via API first
    let doc = yaml.document().expect("Should have document");
    let mapping = doc.as_mapping().expect("Should be mapping");
    assert_eq!(mapping.len(), 3);
    let value_node = mapping.get("first").expect("Should have 'first' key");
    let value_scalar = value_node.as_scalar().expect("Should be scalar");
    assert_eq!(value_scalar.as_string(), "updated");

    // Verify exact output
    let output = yaml.to_string();
    assert_eq!(output, "first: updated\nsecond: 2\nthird: 3");
}

#[test]
fn test_insert_at_index_new_key_at_beginning() {
    let yaml = YamlFile::from_str("first: 1\nsecond: 2").unwrap();

    // Insert new key at beginning
    yaml.insert_at_index(0, "zero", "0");

    // Verify via API first
    let doc = yaml.document().expect("Should have document");
    let mapping = doc.as_mapping().expect("Should be mapping");
    assert_eq!(mapping.len(), 3);
    assert_eq!(
        mapping
            .get("zero")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "0"
    );
    assert_eq!(
        mapping
            .get("first")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "1"
    );
    assert_eq!(
        mapping
            .get("second")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "2"
    );

    // Verify exact output
    let output = yaml.to_string();
    assert_eq!(output, "zero: '0'\nfirst: 1\nsecond: 2");
}

#[test]
fn test_insert_at_index_new_key_in_middle() {
    let yaml = YamlFile::from_str("first: 1\nthird: 3").unwrap();

    // Insert new key in middle
    yaml.insert_at_index(1, "second", "2");

    // Verify via API first
    let doc = yaml.document().expect("Should have document");
    let mapping = doc.as_mapping().expect("Should be mapping");
    assert_eq!(mapping.len(), 3);
    assert_eq!(
        mapping
            .get("first")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "1"
    );
    assert_eq!(
        mapping
            .get("second")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "2"
    );
    assert_eq!(
        mapping
            .get("third")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "3"
    );

    // Verify exact output
    let output = yaml.to_string();
    assert_eq!(output, "first: 1\nsecond: '2'\nthird: 3");
}

#[test]
fn test_insert_at_index_preserves_document_structure() {
    let yaml = YamlFile::from_str("name: project\nversion: 1.0").unwrap();

    // Get initial document structure
    let doc_before = yaml.document().expect("Should have document");
    let mapping_before = doc_before.as_mapping().expect("Should be mapping");
    let pairs_before = mapping_before.iter().count();
    assert_eq!(pairs_before, 2, "Should have 2 pairs initially");

    // Insert new key
    yaml.insert_at_index(1, "author", "developer");

    // Document structure should still be valid
    let doc_after = yaml.document().expect("Should still have document");
    let mapping_after = doc_after.as_mapping().expect("Should still be mapping");
    let pairs_after = mapping_after.iter().count();
    assert!(
        pairs_after >= 2,
        "Should have at least 2 pairs after insertion"
    );

    // Should be able to access all values
    assert!(doc_after.get("name").is_some(), "Should have 'name' key");
    assert!(
        doc_after.get("version").is_some(),
        "Should have 'version' key"
    );
    assert!(
        doc_after.get("author").is_some(),
        "Should have 'author' key"
    );
}

#[test]
fn test_document_insert_at_index() {
    let doc = Document::new();
    doc.set("first", 1);
    doc.set("second", 2);

    // Insert new key
    doc.insert_at_index(1, "middle", 1.5);

    let output = doc.to_string();
    let expected = "---
first: 1
middle: 1.5
second: 2
";
    assert_eq!(output, expected);

    // Should be able to access values
    assert!(doc.get("first").is_some(), "Should have 'first' key");
    assert!(doc.get("middle").is_some(), "Should have 'middle' key");
    assert!(doc.get("second").is_some(), "Should have 'second' key");
}

#[test]
fn test_insert_special_characters() {
    let yaml = YamlFile::from_str("key1: value1").unwrap();

    // Test various special characters
    yaml.insert_at_index(1, "special:key", "value:with:colons");
    yaml.insert_at_index(0, "key with spaces", "value with spaces");
    yaml.insert_at_index(2, "key@symbol", "value#hash");

    // Verify via API first
    let doc = yaml.document().expect("Should have document");
    let mapping = doc.as_mapping().expect("Should be mapping");
    assert_eq!(mapping.len(), 4);

    // Verify all keys and values via API
    assert_eq!(
        mapping
            .get("key1")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "value1"
    );
    assert_eq!(
        mapping
            .get("special:key")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "value:with:colons"
    );
    assert_eq!(
        mapping
            .get("key with spaces")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "value with spaces"
    );
    assert_eq!(
        mapping
            .get("key@symbol")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "value#hash"
    );

    // Verify exact output (keys are sorted alphabetically)
    let output = yaml.to_string();
    let expected = "key with spaces: value with spaces\nkey1: value1\nkey@symbol: value#hash\nspecial:key: value:with:colons\n";
    assert_eq!(output, expected);
}

#[test]
fn test_insert_maintains_pairs_count() {
    let yaml = YamlFile::from_str("a: 1\nb: 2\nc: 3").unwrap();

    let doc_before = yaml.document().expect("Should have document");
    let mapping_before = doc_before.as_mapping().expect("Should be mapping");
    let pairs_before = mapping_before.iter().count();
    assert_eq!(pairs_before, 3, "Should have 3 pairs initially");

    // Update existing key
    yaml.insert_at_index(1, "b", "updated");

    let doc_after = yaml.document().expect("Should have document");
    let mapping_after = doc_after.as_mapping().expect("Should be mapping");
    let pairs_after = mapping_after.iter().count();
    assert_eq!(pairs_after, 3, "Should still have 3 pairs after update");

    // Insert new key
    yaml.insert_at_index(2, "d", "4");

    let doc_final = yaml.document().expect("Should have document");
    let mapping_final = doc_final.as_mapping().expect("Should be mapping");
    let pairs_final = mapping_final.iter().count();
    assert_eq!(pairs_final, 4, "Should have 4 pairs after insertion");
}
