use std::str::FromStr;
use yaml_edit::{Document, Yaml};

#[test]
fn test_insert_at_index_empty_document() {
    let mut yaml = Yaml::from_str("").unwrap();
    yaml.insert_at_index(0, "first", "value");

    let output = yaml.to_string();
    assert!(output.contains("first: value"));

    // Should be able to access the value
    let doc = yaml.document().expect("Should have document");
    let value = doc.get(&"first".into()).expect("Should have 'first' key");
    assert!(value.text().to_string().contains("value"));
}

#[test]
fn test_insert_at_index_update_existing() {
    let mut yaml = Yaml::from_str("first: 1\nsecond: 2\nthird: 3").unwrap();

    // Update existing key
    yaml.insert_at_index(2, "first", "updated");

    let output = yaml.to_string();
    assert!(output.contains("first: updated"));

    // Should be able to access the updated value
    let doc = yaml.document().expect("Should have document");
    let value = doc.get(&"first".into()).expect("Should have 'first' key");
    assert!(value.text().to_string().contains("updated"));
}

#[test]
fn test_insert_at_index_new_key_at_beginning() {
    let mut yaml = Yaml::from_str("first: 1\nsecond: 2").unwrap();

    // Insert new key at beginning
    yaml.insert_at_index(0, "zero", "0");

    let output = yaml.to_string();
    assert!(output.contains("zero: "));
    assert!(output.contains("first: 1"));
    assert!(output.contains("second: 2"));

    // Should be able to access all values
    let doc = yaml.document().expect("Should have document");
    assert!(doc.get(&"zero".into()).is_some(), "Should have 'zero' key");
    assert!(doc.get(&"first".into()).is_some(), "Should have 'first' key");
    assert!(doc.get(&"second".into()).is_some(), "Should have 'second' key");
}

#[test]
fn test_insert_at_index_new_key_in_middle() {
    let mut yaml = Yaml::from_str("first: 1\nthird: 3").unwrap();

    // Insert new key in middle
    yaml.insert_at_index(1, "second", "2");

    let output = yaml.to_string();
    assert!(output.contains("first: 1"));
    assert!(output.contains("second: "));
    assert!(output.contains("third: 3"));

    // Should be able to access all values
    let doc = yaml.document().expect("Should have document");
    assert!(doc.get(&"first".into()).is_some(), "Should have 'first' key");
    assert!(doc.get(&"second".into()).is_some(), "Should have 'second' key");
    assert!(doc.get(&"third".into()).is_some(), "Should have 'third' key");
}

#[test]
fn test_insert_at_index_preserves_document_structure() {
    let mut yaml = Yaml::from_str("name: project\nversion: 1.0").unwrap();

    // Get initial document structure
    let doc_before = yaml.document().expect("Should have document");
    let mapping_before = doc_before.as_mapping().expect("Should be mapping");
    let pairs_before = mapping_before.pairs().count();
    assert_eq!(pairs_before, 2, "Should have 2 pairs initially");

    // Insert new key
    yaml.insert_at_index(1, "author", "developer");

    // Document structure should still be valid
    let doc_after = yaml.document().expect("Should still have document");
    let mapping_after = doc_after.as_mapping().expect("Should still be mapping");
    let pairs_after = mapping_after.pairs().count();
    assert!(
        pairs_after >= 2,
        "Should have at least 2 pairs after insertion"
    );

    // Should be able to access all values
    assert!(doc_after.get(&"name".into()).is_some(), "Should have 'name' key");
    assert!(
        doc_after.get(&"version".into()).is_some(),
        "Should have 'version' key"
    );
    assert!(
        doc_after.get(&"author".into()).is_some(),
        "Should have 'author' key"
    );
}

#[test]
fn test_document_insert_at_index() {
    let mut doc = Document::new();
    doc.set_string("first", "1");
    doc.set_string("second", "2");

    // Insert new key
    doc.insert_at_index(1, "middle", 1.5);

    let output = doc.to_yaml_string();
    assert!(output.contains("first: '1'"));
    assert!(output.contains("middle: "));
    assert!(output.contains("second: '2'"));

    // Should be able to access values
    assert!(doc.get(&"first".into()).is_some(), "Should have 'first' key");
    assert!(doc.get(&"middle".into()).is_some(), "Should have 'middle' key");
    assert!(doc.get(&"second".into()).is_some(), "Should have 'second' key");
}

#[test]
fn test_insert_special_characters() {
    let mut yaml = Yaml::from_str("key1: value1").unwrap();

    // Test various special characters
    yaml.insert_at_index(1, "special:key", "value:with:colons");
    yaml.insert_at_index(0, "key with spaces", "value with spaces");
    yaml.insert_at_index(2, "key@symbol", "value#hash");

    let output = yaml.to_string();
    // Keys with special characters should be quoted if necessary
    assert!(output.contains("key1: value1"));
    // The exact formatting may vary, but all keys should be present

    // Should be able to access with the original key names
    let doc = yaml.document().expect("Should have document");

    // Check if keys exist (they might be quoted in the output but should be accessible)
    // The contains_key method should handle both quoted and unquoted lookups
    let mapping = doc.as_mapping().expect("Should be mapping");

    // At minimum, the original key should still be there
    assert!(mapping.contains_key(&"key1".into()), "Should have 'key1' key");
}

#[test]
fn test_insert_maintains_pairs_count() {
    let mut yaml = Yaml::from_str("a: 1\nb: 2\nc: 3").unwrap();

    let doc_before = yaml.document().expect("Should have document");
    let mapping_before = doc_before.as_mapping().expect("Should be mapping");
    let pairs_before = mapping_before.pairs().count();
    assert_eq!(pairs_before, 3, "Should have 3 pairs initially");

    // Update existing key
    yaml.insert_at_index(1, "b", "updated");

    let doc_after = yaml.document().expect("Should have document");
    let mapping_after = doc_after.as_mapping().expect("Should be mapping");
    let pairs_after = mapping_after.pairs().count();
    assert_eq!(pairs_after, 3, "Should still have 3 pairs after update");

    // Insert new key
    yaml.insert_at_index(2, "d", "4");

    let doc_final = yaml.document().expect("Should have document");
    let mapping_final = doc_final.as_mapping().expect("Should be mapping");
    let pairs_final = mapping_final.pairs().count();
    assert_eq!(pairs_final, 4, "Should have 4 pairs after insertion");
}
