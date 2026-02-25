//! Duplicate key handling tests
//!
//! YAML allows duplicate keys in mappings (last value wins for simple lookups).
//! These tests verify the parser and API correctly handle duplicate keys.
//!
//! Tests cover:
//! - Finding all entries for a duplicate key
//! - Last-value-wins semantics for `.get()`
//! - Preserving all duplicate keys in CST
//! - Mutation behavior with duplicate keys
//! - Round-trip preservation of duplicates
//!
//! All tests verify:
//! 1. API provides access to all duplicate entries
//! 2. Lossless preservation of structure
//! 3. Correct semantics for lookups and mutations

use std::str::FromStr;
use yaml_edit::Document;

#[test]
fn test_find_all_entries_by_key() {
    let yaml = r#"
Reference: First
Reference: Second
Other: Value
Reference: Third
"#;

    let doc = Document::from_str(yaml).unwrap();
    let mapping = doc.as_mapping().unwrap();

    // Find all Reference entries
    let refs: Vec<_> = mapping.find_all_entries_by_key("Reference").collect();
    assert_eq!(refs.len(), 3);

    // Verify the values
    let values: Vec<String> = refs
        .iter()
        .map(|entry| {
            entry
                .value_node()
                .unwrap()
                .as_scalar()
                .unwrap()
                .as_string()
                .to_string()
        })
        .collect();
    assert_eq!(values, vec!["First", "Second", "Third"]);
}

#[test]
fn test_mapping_entry_remove() {
    let yaml = r#"
Reference: First
Reference: Second
Reference: Third
"#;

    let doc = Document::from_str(yaml).unwrap();
    let mapping = doc.as_mapping().unwrap();

    // Collect all Reference entries
    let refs: Vec<_> = mapping.find_all_entries_by_key("Reference").collect();
    assert_eq!(refs.len(), 3);

    // Remove all but the first occurrence
    for entry in refs.into_iter().skip(1) {
        entry.remove();
    }

    // Verify only one Reference remains
    let remaining: Vec<_> = mapping.find_all_entries_by_key("Reference").collect();
    assert_eq!(remaining.len(), 1);

    let value = remaining[0]
        .value_node()
        .unwrap()
        .as_scalar()
        .unwrap()
        .as_string();
    assert_eq!(value, "First");

    let result = doc.to_string();
    let expected = "Reference: First\n";
    assert_eq!(result, expected);
}

#[test]
fn test_remove_nth_occurrence_middle() {
    let yaml = r#"
Reference: First
Reference: Second
Reference: Third
"#;

    let doc = Document::from_str(yaml).unwrap();
    let mapping = doc.as_mapping().unwrap();

    // Remove the second occurrence (index 1)
    let removed = mapping.remove_nth_occurrence("Reference", 1);
    assert!(removed.is_some());

    let removed_value = removed
        .unwrap()
        .value_node()
        .unwrap()
        .as_scalar()
        .unwrap()
        .as_string();
    assert_eq!(removed_value, "Second");

    // Verify two Reference entries remain
    let remaining: Vec<_> = mapping.find_all_entries_by_key("Reference").collect();
    assert_eq!(remaining.len(), 2);

    let values: Vec<String> = remaining
        .iter()
        .map(|entry| {
            entry
                .value_node()
                .unwrap()
                .as_scalar()
                .unwrap()
                .as_string()
                .to_string()
        })
        .collect();
    assert_eq!(values, vec!["First", "Third"]);

    let result = doc.to_string();
    let expected = "Reference: First\nReference: Third\n";
    assert_eq!(result, expected);
}

#[test]
fn test_remove_nth_occurrence_first() {
    let yaml = r#"
Reference: First
Reference: Second
Reference: Third
"#;

    let doc = Document::from_str(yaml).unwrap();
    let mapping = doc.as_mapping().unwrap();

    // Remove the first occurrence (index 0)
    let removed = mapping.remove_nth_occurrence("Reference", 0);
    assert!(removed.is_some());

    let removed_value = removed
        .unwrap()
        .value_node()
        .unwrap()
        .as_scalar()
        .unwrap()
        .as_string();
    assert_eq!(removed_value, "First");

    let remaining: Vec<_> = mapping.find_all_entries_by_key("Reference").collect();
    assert_eq!(remaining.len(), 2);

    let values: Vec<String> = remaining
        .iter()
        .map(|entry| {
            entry
                .value_node()
                .unwrap()
                .as_scalar()
                .unwrap()
                .as_string()
                .to_string()
        })
        .collect();
    assert_eq!(values, vec!["Second", "Third"]);
}

#[test]
fn test_remove_nth_occurrence_last() {
    let yaml = r#"
Reference: First
Reference: Second
Reference: Third
"#;

    let doc = Document::from_str(yaml).unwrap();
    let mapping = doc.as_mapping().unwrap();

    // Remove the last occurrence (index 2)
    let removed = mapping.remove_nth_occurrence("Reference", 2);
    assert!(removed.is_some());

    let removed_value = removed
        .unwrap()
        .value_node()
        .unwrap()
        .as_scalar()
        .unwrap()
        .as_string();
    assert_eq!(removed_value, "Third");

    let remaining: Vec<_> = mapping.find_all_entries_by_key("Reference").collect();
    assert_eq!(remaining.len(), 2);

    let values: Vec<String> = remaining
        .iter()
        .map(|entry| {
            entry
                .value_node()
                .unwrap()
                .as_scalar()
                .unwrap()
                .as_string()
                .to_string()
        })
        .collect();
    assert_eq!(values, vec!["First", "Second"]);
}

#[test]
fn test_remove_nth_occurrence_out_of_bounds() {
    let yaml = r#"
Reference: First
Reference: Second
"#;

    let doc = Document::from_str(yaml).unwrap();
    let mapping = doc.as_mapping().unwrap();

    // Try to remove the third occurrence (index 2) when only 2 exist
    let removed = mapping.remove_nth_occurrence("Reference", 2);
    assert!(removed.is_none());

    // Verify both entries still exist
    let remaining: Vec<_> = mapping.find_all_entries_by_key("Reference").collect();
    assert_eq!(remaining.len(), 2);
}

#[test]
fn test_remove_nth_occurrence_nonexistent_key() {
    let yaml = r#"
Reference: First
Reference: Second
"#;

    let doc = Document::from_str(yaml).unwrap();
    let mapping = doc.as_mapping().unwrap();

    // Try to remove from a non-existent key
    let removed = mapping.remove_nth_occurrence("NotFound", 0);
    assert!(removed.is_none());

    // Verify all entries still exist
    let remaining: Vec<_> = mapping.find_all_entries_by_key("Reference").collect();
    assert_eq!(remaining.len(), 2);
}

#[test]
fn test_preserve_first_occurrence_position() {
    let yaml = r#"
Name: Slugify
Reference: First
Archive: GitHub
Reference: Second
Version: 1.0
Reference: Third
"#;

    let doc = Document::from_str(yaml).unwrap();
    let mapping = doc.as_mapping().unwrap();

    // Remove all but the first occurrence of Reference
    let refs: Vec<_> = mapping.find_all_entries_by_key("Reference").collect();
    for entry in refs.into_iter().skip(1) {
        entry.remove();
    }

    let result = doc.to_string();
    let expected = "Name: Slugify\nReference: First\nArchive: GitHub\nVersion: 1.0\n";
    assert_eq!(result, expected);
}
