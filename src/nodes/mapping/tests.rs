//! Tests for `Mapping` and `MappingEntry`. Split out of
//! `nodes/mapping/mod.rs` unchanged.

use crate::debug::assert_cst_ok;
use crate::scalar::ScalarValue;
use crate::yaml::YamlFile;
use std::str::FromStr;

/// Regression test for issue #37: setting an empty mapping/sequence built
/// via the builder API used to serialize as `key:` (which reparses as
/// null) instead of `key: {}` / `key: []`. Also verifies the output
/// round-trips: reparsing gives back a collection of the same kind.
#[test]
fn test_set_empty_collection_serializes_as_flow_empty() {
    use crate::as_yaml::YamlKind;
    use crate::builder::{MappingBuilder, SequenceBuilder};
    use crate::yaml::Document;

    let seq = SequenceBuilder::new()
        .build_document()
        .as_sequence()
        .unwrap();
    let doc = Document::from_str("name: Alice").unwrap();
    doc.as_mapping().unwrap().set("foo", seq);
    assert_cst_ok(&doc);
    assert_eq!(doc.to_string().trim(), "name: Alice\nfoo: []");
    let reparsed = Document::from_str(&doc.to_string()).unwrap();
    assert_eq!(
        reparsed.as_mapping().unwrap().get("foo").unwrap().kind(),
        YamlKind::Sequence
    );

    let map = MappingBuilder::new().build_document().as_mapping().unwrap();
    let doc = Document::from_str("name: Alice").unwrap();
    doc.as_mapping().unwrap().set("foo", map);
    assert_cst_ok(&doc);
    assert_eq!(doc.to_string().trim(), "name: Alice\nfoo: {}");
    let reparsed = Document::from_str(&doc.to_string()).unwrap();
    assert_eq!(
        reparsed.as_mapping().unwrap().get("foo").unwrap().kind(),
        YamlKind::Mapping
    );
}

#[test]
fn test_insert_after_existing_key_missing_ref_returns_false() {
    use crate::yaml::Document;
    let doc = Document::from_str("a: 1\nb: 2\n").unwrap();
    let mapping = doc.as_mapping().unwrap();
    assert!(!mapping.insert_after("missing", "a", "99"));
    assert_cst_ok(&doc);
    assert_eq!(
        mapping.get("a").unwrap().as_scalar().unwrap().as_string(),
        "99"
    );
    assert!(!mapping.insert_before("missing", "b", "88"));
    assert_cst_ok(&doc);
    assert_eq!(
        mapping.get("b").unwrap().as_scalar().unwrap().as_string(),
        "88"
    );
}

#[test]
fn test_move_after_missing_ref_leaves_mapping_unchanged() {
    use crate::yaml::Document;
    let doc = Document::from_str("a: 1\nb: 2\n").unwrap();
    let mapping = doc.as_mapping().unwrap();
    assert!(!mapping.move_after("missing", "a", "3"));
    assert_cst_ok(&doc);
    assert_eq!(doc.to_string(), "a: 1\nb: 2\n");
    assert!(!mapping.move_before("missing", "a", "3"));
    assert_cst_ok(&doc);
    assert_eq!(doc.to_string(), "a: 1\nb: 2\n");
    assert!(mapping.move_after("a", "a", "3"));
    assert_cst_ok(&doc);
    assert_eq!(doc.to_string(), "a: '3'\nb: 2\n");

    let doc = Document::from_str("{a: 1, b: 2}").unwrap();
    let mapping = doc.as_mapping().unwrap();
    assert!(!mapping.move_after("missing", "a", "3"));
    assert_cst_ok(&doc);
    assert_eq!(doc.to_string(), "{a: 1, b: 2}");
}

/// Setting into a flow-style mapping (`{...}`) inserts entries inside
/// the braces with proper `, ` separators, not on new lines after `}`
/// (which would produce broken YAML).
#[test]
fn test_set_into_flow_mapping_inserts_inside_braces() {
    use crate::yaml::Document;

    // Cases: (input, key to set, expected output)
    let cases = [
        ("outer: {}", "a", "outer: {a: X}"),
        ("outer: {a: 1}", "b", "outer: {a: 1, b: X}"),
        // The trailing comma stays at the end.
        ("outer: {a: 1,}", "b", "outer: {a: 1, b: X,}"),
        // Update-in-place stays inside the braces.
        ("outer: {a: 1}", "a", "outer: {a: X}"),
    ];
    for (input, key, expected) in cases {
        let doc = Document::from_str(input).unwrap();
        let inner_val = doc.as_mapping().unwrap().get("outer").unwrap();
        let inner = inner_val.as_mapping().unwrap();
        inner.set(key, "X");
        assert_cst_ok(&doc);
        assert_eq!(doc.to_string().trim(), expected, "input was {input:?}");
    }
}

#[test]
fn test_mapping_set_new_key() {
    let yaml = "existing: value";
    let parsed = YamlFile::from_str(yaml).unwrap();

    // Get the document and set on it
    let doc = parsed.document().expect("Should have a document");
    doc.set("new_key", "new_value");
    assert_cst_ok(&doc);

    let output = doc.to_string();

    let expected = r#"existing: value
new_key: new_value"#;
    assert_eq!(output.trim(), expected);
}
#[test]
fn test_mapping_rename_key() {
    let yaml = "old_name: value";
    let parsed = YamlFile::from_str(yaml).unwrap();

    let doc = parsed.document().expect("expected a document");
    let mapping = doc.as_mapping().expect("expected a mapping");
    let renamed = mapping.rename_key("old_name", "new_name");
    assert_cst_ok(&parsed);
    assert!(renamed);
    assert!(doc.contains_key("new_name"));
    assert!(!doc.contains_key("old_name"));
}

#[test]
fn test_mapping_remove_key() {
    let yaml = "key1: value1\nkey2: value2";
    let parsed = YamlFile::from_str(yaml).unwrap();

    let doc = parsed.document().expect("expected a document");
    let mapping = doc.as_mapping().expect("expected a mapping");
    let removed = mapping.remove("key1");
    assert_cst_ok(&parsed);
    assert!(removed.is_some());
    assert!(!doc.contains_key("key1"));
    assert!(doc.contains_key("key2"));
}
#[test]
fn test_mapping_simple_set() {
    let yaml = "key1: value1";
    let parsed = YamlFile::from_str(yaml).unwrap();

    // Get document and add a new key
    let doc = parsed.document().expect("Should have a document");
    doc.set("key2", "value2");
    assert_cst_ok(&doc);

    let output = doc.to_string();

    let expected = r#"key1: value1
key2: value2"#;
    assert_eq!(output.trim(), expected);
}
#[test]
fn test_mapping_set_preserves_position() {
    // Test that set() preserves the position of existing fields when updating
    let yaml = r#"Name: original_name
Contact: original_contact
Repository: https://github.com/example/repo.git
"#;
    let parsed = YamlFile::from_str(yaml).unwrap();
    let doc = parsed.document().expect("Should have a document");

    // Update Contact - it should stay in position 2, not move to the end
    doc.set("Contact", "updated_contact");
    assert_cst_ok(&doc);

    let output = doc.to_string();
    let expected = r#"Name: original_name
Contact: updated_contact
Repository: https://github.com/example/repo.git
"#;
    assert_eq!(output, expected);
}
#[test]
fn test_mapping_set_preserves_multiple_fields() {
    // Test updating multiple existing fields preserves all positions
    let yaml = r#"Name: tsne
Contact: Justin Donaldson <jdonaldson@gmail.com>
Archive: CRAN
Repository: https://github.com/jdonaldson/rtsne.git
"#;
    let parsed = YamlFile::from_str(yaml).unwrap();
    let doc = parsed.document().expect("Should have a document");

    if let Some(mapping) = doc.as_mapping() {
        // Update Contact - should stay in position 2
        mapping.set("Contact", "New Contact <new@example.com>");
        assert_cst_ok(&parsed);
        // Update Archive - should stay in position 3
        mapping.set("Archive", "PyPI");
        assert_cst_ok(&parsed);
    }

    let output = doc.to_string();
    let expected = r#"Name: tsne
Contact: New Contact <new@example.com>
Archive: PyPI
Repository: https://github.com/jdonaldson/rtsne.git
"#;
    assert_eq!(output, expected);
}
#[test]
fn test_mapping_insert_after() {
    let yaml = r#"first: 1
second: 2
fourth: 4"#;

    let parsed = YamlFile::from_str(yaml).unwrap();

    let doc = parsed.document().expect("Should have a document");

    // Insert after "second"
    let success = doc.insert_after("second", "third", 3);
    assert_cst_ok(&doc);
    assert!(
        success,
        "insert_after should succeed when reference key exists"
    );

    let output = doc.to_string();

    // Check exact output - should preserve original structure and insert correctly
    let expected = r#"first: 1
second: 2
third: 3
fourth: 4"#;
    assert_eq!(output.trim(), expected);

    // Test inserting after non-existent key
    let failed = doc.insert_after("nonexistent", "new_key", "new_value");
    assert_cst_ok(&doc);
    assert!(
        !failed,
        "insert_after should fail when reference key doesn't exist"
    );

    // Test updating existing key through insert_after
    let updated = doc.insert_after("first", "second", "2_updated");
    assert_cst_ok(&doc);
    assert!(updated, "insert_after should update existing key");
    let updated_output = doc.to_string();
    let expected_updated = r#"first: 1
second: 2_updated
third: 3
fourth: 4"#;
    assert_eq!(updated_output.trim(), expected_updated);
}
#[test]
fn test_mapping_insert_before() {
    let yaml = r#"first: 1
third: 3
fourth: 4"#;

    let parsed = YamlFile::from_str(yaml).unwrap();
    let doc = parsed.document().expect("Should have a document");

    // Insert before "third"
    let success = doc.insert_before("third", "second", 2);
    assert_cst_ok(&doc);
    assert!(
        success,
        "insert_before should succeed when reference key exists"
    );

    let output = doc.to_string();

    // Check exact output - should preserve original structure and insert correctly
    let expected = r#"first: 1
second: 2
third: 3
fourth: 4"#;
    assert_eq!(output.trim(), expected);

    // Test inserting before non-existent key
    let failed = doc.insert_before("nonexistent", "new_key", "new_value");
    assert_cst_ok(&doc);
    assert!(
        !failed,
        "insert_before should fail when reference key doesn't exist"
    );

    // Test updating existing key through insert_before
    let updated = doc.insert_before("fourth", "third", "3_updated");
    assert_cst_ok(&doc);
    assert!(updated, "insert_before should update existing key");
    let output = doc.to_string();
    let expected_updated = r#"first: 1
second: 2
third: 3_updated
fourth: 4"#;
    assert_eq!(output.trim(), expected_updated);
}
#[test]
fn test_mapping_insert_at_index() {
    let yaml = r#"first: 1
third: 3"#;

    let parsed = YamlFile::from_str(yaml).unwrap();
    let doc = parsed.document().expect("Should have a document");

    // Insert at index 1 (between first and third)
    doc.insert_at_index(1, "second", 2);
    assert_cst_ok(&doc);

    let output = doc.to_string();

    // Check exact output - should preserve original structure and insert correctly
    let expected = r#"first: 1
second: 2
third: 3"#;
    assert_eq!(output.trim(), expected);

    // Insert at index 0 (beginning)
    doc.insert_at_index(0, "zero", 0);
    assert_cst_ok(&doc);
    let output2 = doc.to_string();
    let expected2 = r#"zero: 0
first: 1
second: 2
third: 3"#;
    assert_eq!(output2.trim(), expected2);

    // Insert at out-of-bounds index (should append at end)
    doc.insert_at_index(100, "last", "999");
    assert_cst_ok(&doc);
    let output3 = doc.to_string();
    let expected3 = r#"zero: 0
first: 1
second: 2
third: 3
last: '999'"#;
    assert_eq!(output3.trim(), expected3);

    // Test updating existing key through insert_at_index
    doc.insert_at_index(2, "first", "1_updated");
    assert_cst_ok(&doc);
    let final_output = doc.to_string();
    let expected_final = r#"zero: 0
first: 1_updated
second: 2
third: 3
last: '999'"#;
    assert_eq!(final_output.trim(), expected_final);
}
#[test]
fn test_mapping_insert_special_characters() {
    let yaml = "key1: value1";

    let parsed = YamlFile::from_str(yaml).unwrap();
    let doc = parsed.document().expect("Should have a document");

    // Test with special characters that need escaping
    doc.insert_after("key1", "special:key", "value:with:colons");
    assert_cst_ok(&doc);
    doc.insert_before("key1", "key with spaces", "value with spaces");
    assert_cst_ok(&doc);
    doc.insert_at_index(1, "key@symbol", "value#hash");
    assert_cst_ok(&doc);

    // Verify all keys are present
    assert!(doc.contains_key("special:key"));
    assert!(doc.contains_key("key with spaces"));
    assert!(doc.contains_key("key@symbol"));

    // Parse the output to verify it's valid YAML
    let output = doc.to_string();
    let reparsed = YamlFile::from_str(&output);
    assert!(reparsed.is_ok(), "Output should be valid YAML");
}
#[test]
fn test_mapping_insert_empty_values() {
    let yaml = "key1: value1";

    let parsed = YamlFile::from_str(yaml).unwrap();
    let doc = parsed.document().expect("Should have a document");

    // Test with empty values
    doc.insert_after("key1", "empty", "");
    assert_cst_ok(&doc);
    doc.insert_before("key1", "null_key", ScalarValue::null());
    assert_cst_ok(&doc);

    assert!(doc.contains_key("empty"));
    assert!(doc.contains_key("null_key"));

    // Verify the output is valid YAML
    let output = parsed.to_string();
    let reparsed = YamlFile::from_str(&output);
    assert!(
        reparsed.is_ok(),
        "Output with empty values should be valid YAML"
    );
}

// Iterator tests

#[test]
fn test_mapping_into_iterator() {
    use crate::Document;
    let text = "name: Alice\nage: 30\ncity: Boston";
    let doc = Document::from_str(text).unwrap();
    let mapping = doc.as_mapping().unwrap();

    // Test that we can use for loops directly
    let mut count = 0;
    for (key, value) in &mapping {
        count += 1;

        // Check that we get scalar nodes
        assert!(key.is_scalar());
        assert!(value.is_scalar());
    }

    assert_eq!(count, 3);
}

#[test]
fn test_mapping_into_iterator_collect() {
    use crate::Document;
    let text = "a: 1\nb: 2\nc: 3";
    let doc = Document::from_str(text).unwrap();
    let mapping = doc.as_mapping().unwrap();

    // Collect into a Vec
    let pairs: Vec<_> = (&mapping).into_iter().collect();
    assert_eq!(pairs.len(), 3);

    // Check we can get scalars
    for (key, value) in pairs {
        assert!(key.as_scalar().is_some());
        assert!(value.as_scalar().is_some());
    }
}

#[test]
fn test_mapping_iterator_filter() {
    use crate::Document;
    let text = "a: 1\nb: 2\nc: 3\nd: 4";
    let doc = Document::from_str(text).unwrap();
    let mapping = doc.as_mapping().unwrap();

    // Filter for even values
    let even_count = (&mapping)
        .into_iter()
        .filter(|(_, value)| {
            value
                .as_scalar()
                .and_then(|s| s.to_string().parse::<i32>().ok())
                .is_some_and(|n| n % 2 == 0)
        })
        .count();

    assert_eq!(even_count, 2); // b: 2 and d: 4
}

#[test]
fn test_empty_mapping_iterator() {
    let empty = crate::Mapping::new();

    let count = (&empty).into_iter().count();
    assert_eq!(count, 0);
}

#[test]
fn test_nested_mapping_iteration() {
    use crate::Document;
    let text = "server:\n  host: localhost\n  port: 8080";
    let doc = Document::from_str(text).unwrap();
    let mapping = doc.as_mapping().unwrap();

    // Iterate outer mapping
    for (key, _value) in &mapping {
        if let Some(key_scalar) = key.as_scalar() {
            if key_scalar.to_string() == "server" {
                // Get nested mapping
                if let Some(nested_mapping) = mapping.get_mapping("server") {
                    let nested_count = (&nested_mapping).into_iter().count();
                    assert_eq!(nested_count, 2); // host and port
                }
            }
        }
    }
}

#[test]
fn test_mapping_keys() {
    let yaml = YamlFile::from_str("name: Alice\nage: 30\ncity: NYC").unwrap();
    let doc = yaml.document().unwrap();
    let mapping = doc.as_mapping().unwrap();

    let keys: Vec<String> = mapping.keys().map(|k| k.to_string()).collect();
    assert_eq!(keys, vec!["name", "age", "city"]);
}

#[test]
fn test_mapping_is_empty() {
    let yaml = YamlFile::from_str("{}").unwrap();
    let doc = yaml.document().unwrap();
    let mapping = doc.as_mapping().unwrap();
    assert!(mapping.is_empty());

    let yaml2 = YamlFile::from_str("key: value").unwrap();
    let doc2 = yaml2.document().unwrap();
    let mapping2 = doc2.as_mapping().unwrap();
    assert!(!mapping2.is_empty());
}

#[test]
fn test_mapping_contains_key() {
    let yaml = YamlFile::from_str("name: Alice\nage: 30").unwrap();
    let doc = yaml.document().unwrap();
    let mapping = doc.as_mapping().unwrap();

    assert!(mapping.contains_key("name"));
    assert!(mapping.contains_key("age"));
    assert!(!mapping.contains_key("city"));
}

#[test]
fn test_mapping_get() {
    let yaml = YamlFile::from_str("name: Alice\nage: 30").unwrap();
    let doc = yaml.document().unwrap();
    let mapping = doc.as_mapping().unwrap();

    assert_eq!(
        mapping
            .get("name")
            .and_then(|v| v.as_scalar().map(|s| s.as_string())),
        Some("Alice".to_string())
    );
    assert_eq!(mapping.get("age").and_then(|v| v.to_i64()), Some(30));
    assert!(mapping.get("city").is_none());
}

#[test]
fn test_mapping_single_entry() {
    let yaml = YamlFile::from_str("key: value").unwrap();
    let doc = yaml.document().unwrap();
    let mapping = doc.as_mapping().unwrap();

    let keys: Vec<String> = mapping.keys().map(|k| k.to_string()).collect();
    assert_eq!(keys, vec!["key"]);
    assert!(!mapping.is_empty());
    assert!(mapping.contains_key("key"));
}

#[test]
fn test_mapping_ops_set_new_key() {
    let yaml = YamlFile::from_str("name: Alice").unwrap();
    let doc = yaml.document().unwrap();
    let mapping = doc.as_mapping().unwrap();

    mapping.set("age", 30);
    assert_cst_ok(&yaml);

    let keys: Vec<String> = mapping.keys().map(|k| k.to_string()).collect();
    assert_eq!(keys, vec!["name", "age"]);
    assert_eq!(mapping.get("age").and_then(|v| v.to_i64()), Some(30));
}

#[test]
fn test_mapping_set_existing_key() {
    let yaml = YamlFile::from_str("name: Alice\nage: 30").unwrap();
    let doc = yaml.document().unwrap();
    let mapping = doc.as_mapping().unwrap();

    mapping.set("age", 31);
    assert_cst_ok(&yaml);

    assert_eq!(mapping.get("age").and_then(|v| v.to_i64()), Some(31));
    let keys: Vec<String> = mapping.keys().map(|k| k.to_string()).collect();
    assert_eq!(keys, vec!["name", "age"]);
}

#[test]
fn test_mapping_remove_existing_key() {
    let yaml = YamlFile::from_str("name: Alice\nage: 30\ncity: NYC").unwrap();
    let doc = yaml.document().unwrap();
    let mapping = doc.as_mapping().unwrap();

    let removed = mapping.remove("age");
    assert_cst_ok(&yaml);
    assert!(removed.is_some());

    let keys: Vec<String> = mapping.keys().map(|k| k.to_string()).collect();
    assert_eq!(keys, vec!["name", "city"]);
    assert!(!mapping.contains_key("age"));
}

#[test]
fn test_mapping_remove_nonexistent_key() {
    let yaml = YamlFile::from_str("name: Alice").unwrap();
    let doc = yaml.document().unwrap();
    let mapping = doc.as_mapping().unwrap();

    let removed = mapping.remove("age");
    assert_cst_ok(&yaml);
    assert!(removed.is_none());

    let keys: Vec<String> = mapping.keys().map(|k| k.to_string()).collect();
    assert_eq!(keys, vec!["name"]);
}

#[test]
fn test_mapping_remove_all_keys() {
    let yaml = YamlFile::from_str("a: 1\nb: 2").unwrap();
    let doc = yaml.document().unwrap();
    let mapping = doc.as_mapping().unwrap();

    assert!(mapping.remove("a").is_some());
    assert_cst_ok(&yaml);
    assert!(mapping.remove("b").is_some());
    assert_cst_ok(&yaml);
    assert!(mapping.is_empty());
}

#[test]
fn test_rename_key_basic() {
    let original = r#"name: my-app
version: 1.0
author: Alice"#;

    let yaml = YamlFile::from_str(original).unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mapping) = doc.as_mapping() {
            let success = mapping.rename_key("version", "app_version");
            assert_cst_ok(&yaml);
            assert!(success);
        }
    }

    let expected = r#"name: my-app
app_version: 1.0
author: Alice"#;
    assert_eq!(yaml.to_string(), expected);
}

#[test]
fn test_rename_key_preserves_value() {
    let original = r#"count: 42
enabled: true"#;

    let yaml = YamlFile::from_str(original).unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mapping) = doc.as_mapping() {
            mapping.rename_key("count", "total");
            assert_cst_ok(&yaml);
        }
    }

    let expected = r#"total: 42
enabled: true"#;
    assert_eq!(yaml.to_string(), expected);
}

#[test]
fn test_remove_field() {
    let original = r#"name: my-app
version: 1.0
author: Alice"#;

    let yaml = YamlFile::from_str(original).unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mapping) = doc.as_mapping() {
            let removed = mapping.remove("author");
            assert_cst_ok(&yaml);
            assert!(removed.is_some());
        }
    }

    let expected = r#"name: my-app
version: 1.0"#;
    assert_eq!(yaml.to_string(), expected);
}

#[test]
fn test_complex_operations_combined() {
    let original = r#"name: my-app
version: 1.0
author: Alice
year: 2023

features:
  - logging
  - auth"#;

    let yaml = YamlFile::from_str(original).unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mapping) = doc.as_mapping() {
            // Add new fields
            mapping.set("license", "MIT");
            assert_cst_ok(&yaml);
            mapping.set("published", true);
            assert_cst_ok(&yaml);
            mapping.set("downloads", 1000);
            assert_cst_ok(&yaml);

            // Remove a field
            mapping.remove("author");
            assert_cst_ok(&yaml);

            // Rename a field
            mapping.rename_key("version", "app_version");
            assert_cst_ok(&yaml);

            // Update existing field
            mapping.set("year", 2024);
            assert_cst_ok(&yaml);
        }
    }

    let expected = r#"name: my-app
app_version: 1.0
year: 2024

features:
  - logging
  - auth
license: MIT
published: true
downloads: 1000"#;
    assert_eq!(yaml.to_string(), expected);
}

#[test]
fn test_mapping_get_nested_mapping() {
    let yaml = YamlFile::from_str("user:\n  name: Alice\n  age: 30").unwrap();
    let doc = yaml.document().unwrap();
    let mapping = doc.as_mapping().unwrap();

    let nested = mapping.get_mapping("user");
    assert!(nested.is_some());

    let nested = nested.unwrap();
    assert_eq!(
        nested
            .get("name")
            .and_then(|v| v.as_scalar().map(|s| s.as_string())),
        Some("Alice".to_string())
    );
    assert_eq!(nested.get("age").and_then(|v| v.to_i64()), Some(30));
}

#[test]
fn test_mapping_get_nested_sequence() {
    let yaml = YamlFile::from_str("items:\n  - a\n  - b\n  - c").unwrap();
    let doc = yaml.document().unwrap();
    let mapping = doc.as_mapping().unwrap();

    let seq = mapping.get_sequence("items");
    assert!(seq.is_some());

    let seq = seq.unwrap();
    assert_eq!(seq.len(), 3);
    let values: Vec<String> = seq.values().map(|v| v.to_string()).collect();
    assert_eq!(values, vec!["a", "b", "c"]);
}

#[test]
fn test_mapping_get_nonexistent_nested() {
    let yaml = YamlFile::from_str("name: Alice").unwrap();
    let doc = yaml.document().unwrap();
    let mapping = doc.as_mapping().unwrap();

    assert_eq!(mapping.get_mapping("user"), None);
    assert_eq!(mapping.get_sequence("items"), None);
}

#[test]
fn test_rename_key_nonexistent() {
    let yaml = YamlFile::from_str("name: Alice").unwrap();
    let doc = yaml.document().unwrap();
    let mapping = doc.as_mapping().unwrap();

    let success = mapping.rename_key("age", "years");
    assert_cst_ok(&yaml);
    assert!(!success);

    let keys: Vec<String> = mapping.keys().map(|k| k.to_string()).collect();
    assert_eq!(keys, vec!["name"]);
}

#[test]
fn test_rename_key_first_entry() {
    let yaml = YamlFile::from_str("name: Alice\nage: 30\ncity: NYC").unwrap();
    let doc = yaml.document().unwrap();
    let mapping = doc.as_mapping().unwrap();

    let success = mapping.rename_key("name", "username");
    assert_cst_ok(&yaml);
    assert!(success);

    let keys: Vec<String> = mapping.keys().map(|k| k.to_string()).collect();
    assert_eq!(keys, vec!["username", "age", "city"]);
}

#[test]
fn test_rename_key_middle_entry() {
    let yaml = YamlFile::from_str("name: Alice\nage: 30\ncity: NYC").unwrap();
    let doc = yaml.document().unwrap();
    let mapping = doc.as_mapping().unwrap();

    let success = mapping.rename_key("age", "years");
    assert_cst_ok(&yaml);
    assert!(success);

    let keys: Vec<String> = mapping.keys().map(|k| k.to_string()).collect();
    assert_eq!(keys, vec!["name", "years", "city"]);
}

#[test]
fn test_rename_key_last_entry() {
    let yaml = YamlFile::from_str("name: Alice\nage: 30\ncity: NYC").unwrap();
    let doc = yaml.document().unwrap();
    let mapping = doc.as_mapping().unwrap();

    let success = mapping.rename_key("city", "location");
    assert_cst_ok(&yaml);
    assert!(success);

    let keys: Vec<String> = mapping.keys().map(|k| k.to_string()).collect();
    assert_eq!(keys, vec!["name", "age", "location"]);
}

#[test]
fn test_mapping_with_different_value_types() {
    let yaml = YamlFile::from_str("string: hello\nnumber: 42\nbool: true").unwrap();
    let doc = yaml.document().unwrap();
    let mapping = doc.as_mapping().unwrap();

    assert_eq!(
        mapping
            .get("string")
            .and_then(|v| v.as_scalar().map(|s| s.as_string())),
        Some("hello".to_string())
    );
    assert_eq!(mapping.get("number").and_then(|v| v.to_i64()), Some(42));
    assert_eq!(mapping.get("bool").and_then(|v| v.to_bool()), Some(true));
}

#[test]
fn test_mapping_set_different_value_types() {
    let yaml = YamlFile::from_str("key: value").unwrap();
    let doc = yaml.document().unwrap();
    let mapping = doc.as_mapping().unwrap();

    mapping.set("number", 123);
    assert_cst_ok(&yaml);
    mapping.set("bool", false);
    assert_cst_ok(&yaml);
    mapping.set("text", "hello");
    assert_cst_ok(&yaml);

    assert_eq!(mapping.get("number").and_then(|v| v.to_i64()), Some(123));
    assert_eq!(mapping.get("bool").and_then(|v| v.to_bool()), Some(false));
    assert_eq!(
        mapping
            .get("text")
            .and_then(|v| v.as_scalar().map(|s| s.as_string())),
        Some("hello".to_string())
    );
}

#[test]
fn test_empty_mapping_operations() {
    let yaml = YamlFile::from_str("{}").unwrap();
    let doc = yaml.document().unwrap();
    let mapping = doc.as_mapping().unwrap();

    assert!(mapping.is_empty());
    assert!(!mapping.contains_key("any"));
    assert_eq!(mapping.get("any"), None);
    assert!(mapping.remove("any").is_none());
    assert_cst_ok(&yaml);
    assert!(!mapping.rename_key("old", "new"));
    assert_cst_ok(&yaml);

    // Can still add to empty mapping
    mapping.set("first", "value");
    assert_cst_ok(&yaml);
    assert!(!mapping.is_empty());
    assert_eq!(
        mapping.get("first").map(|v| v.to_string()),
        Some("value".to_string())
    );
}

#[test]
fn test_mapping_remove_first_of_three() {
    let yaml = YamlFile::from_str("a: 1\nb: 2\nc: 3").unwrap();
    let doc = yaml.document().unwrap();
    let mapping = doc.as_mapping().unwrap();

    assert!(mapping.remove("a").is_some());
    assert_cst_ok(&yaml);

    let keys: Vec<String> = mapping.keys().map(|k| k.to_string()).collect();
    assert_eq!(keys, vec!["b", "c"]);
}

#[test]
fn test_mapping_remove_middle_of_three() {
    let yaml = YamlFile::from_str("a: 1\nb: 2\nc: 3").unwrap();
    let doc = yaml.document().unwrap();
    let mapping = doc.as_mapping().unwrap();

    assert!(mapping.remove("b").is_some());
    assert_cst_ok(&yaml);

    let keys: Vec<String> = mapping.keys().map(|k| k.to_string()).collect();
    assert_eq!(keys, vec!["a", "c"]);
}

#[test]
fn test_mapping_remove_last_of_three() {
    let yaml = YamlFile::from_str("a: 1\nb: 2\nc: 3").unwrap();
    let doc = yaml.document().unwrap();
    let mapping = doc.as_mapping().unwrap();

    assert!(mapping.remove("c").is_some());
    assert_cst_ok(&yaml);

    let keys: Vec<String> = mapping.keys().map(|k| k.to_string()).collect();
    assert_eq!(keys, vec!["a", "b"]);
}

#[test]
fn test_mapping_len_empty() {
    let yaml = YamlFile::from_str("{}").unwrap();
    let doc = yaml.document().unwrap();
    let mapping = doc.as_mapping().unwrap();

    assert_eq!(mapping.len(), 0);
    assert!(mapping.is_empty());
}

#[test]
fn test_mapping_len_single() {
    let yaml = YamlFile::from_str("name: Alice").unwrap();
    let doc = yaml.document().unwrap();
    let mapping = doc.as_mapping().unwrap();

    assert_eq!(mapping.len(), 1);
    assert!(!mapping.is_empty());
}

#[test]
fn test_mapping_len_multiple() {
    let yaml = YamlFile::from_str("name: Alice\nage: 30\ncity: NYC").unwrap();
    let doc = yaml.document().unwrap();
    let mapping = doc.as_mapping().unwrap();

    assert_eq!(mapping.len(), 3);
    assert!(!mapping.is_empty());
}

#[test]
fn test_mapping_len_after_adding() {
    let yaml = YamlFile::from_str("name: Alice").unwrap();
    let doc = yaml.document().unwrap();
    let mapping = doc.as_mapping().unwrap();

    assert_eq!(mapping.len(), 1);

    mapping.set("age", 30);
    assert_cst_ok(&yaml);
    assert_eq!(mapping.len(), 2);

    mapping.set("city", "NYC");
    assert_cst_ok(&yaml);
    assert_eq!(mapping.len(), 3);
}

#[test]
fn test_mapping_len_after_removing() {
    let yaml = YamlFile::from_str("name: Alice\nage: 30\ncity: NYC").unwrap();
    let doc = yaml.document().unwrap();
    let mapping = doc.as_mapping().unwrap();

    assert_eq!(mapping.len(), 3);

    mapping.remove("age");
    assert_cst_ok(&yaml);
    assert_eq!(mapping.len(), 2);

    mapping.remove("city");
    assert_cst_ok(&yaml);
    assert_eq!(mapping.len(), 1);

    mapping.remove("name");
    assert_cst_ok(&yaml);
    assert_eq!(mapping.len(), 0);
    assert!(mapping.is_empty());
}

#[test]
fn test_mapping_values_empty() {
    let yaml = YamlFile::from_str("{}").unwrap();
    let doc = yaml.document().unwrap();
    let mapping = doc.as_mapping().unwrap();

    let values: Vec<_> = mapping.values().collect();
    assert_eq!(values.len(), 0);
}

#[test]
fn test_mapping_values_single() {
    let yaml = YamlFile::from_str("name: Alice").unwrap();
    let doc = yaml.document().unwrap();
    let mapping = doc.as_mapping().unwrap();

    let values: Vec<_> = mapping.values().collect();
    assert_eq!(values.len(), 1);
    assert_eq!(
        values[0].as_scalar().map(|s| s.as_string()),
        Some("Alice".to_string())
    );
}

#[test]
fn test_mapping_values_multiple() {
    let yaml = YamlFile::from_str("name: Alice\nage: 30\nactive: true").unwrap();
    let doc = yaml.document().unwrap();
    let mapping = doc.as_mapping().unwrap();

    let values: Vec<_> = mapping.values().collect();
    assert_eq!(values.len(), 3);
    assert_eq!(
        values[0].as_scalar().map(|s| s.as_string()),
        Some("Alice".to_string())
    );
    assert_eq!(values[1].to_i64(), Some(30));
    assert_eq!(values[2].to_bool(), Some(true));
}

#[test]
fn test_mapping_values_different_types() {
    let yaml = YamlFile::from_str("string: hello\nnumber: 42\nbool: false").unwrap();
    let doc = yaml.document().unwrap();
    let mapping = doc.as_mapping().unwrap();

    // Collect values and check types
    let values: Vec<_> = mapping.values().collect();
    assert_eq!(values.len(), 3);

    assert_eq!(
        values[0].as_scalar().map(|s| s.as_string()),
        Some("hello".to_string())
    );
    assert_eq!(values[1].to_i64(), Some(42));
    assert_eq!(values[2].to_bool(), Some(false));
}

#[test]
fn test_mapping_iter_empty() {
    let yaml = YamlFile::from_str("{}").unwrap();
    let doc = yaml.document().unwrap();
    let mapping = doc.as_mapping().unwrap();

    let pairs: Vec<_> = mapping.iter().collect();
    assert_eq!(pairs.len(), 0);
}

#[test]
fn test_mapping_iter_single() {
    let yaml = YamlFile::from_str("name: Alice").unwrap();
    let doc = yaml.document().unwrap();
    let mapping = doc.as_mapping().unwrap();

    let pairs: Vec<_> = mapping.iter().collect();
    assert_eq!(pairs.len(), 1);
    assert_eq!(
        pairs[0].0.as_scalar().map(|s| s.as_string()),
        Some("name".to_string())
    );
    assert_eq!(
        pairs[0].1.as_scalar().map(|s| s.as_string()),
        Some("Alice".to_string())
    );
}

#[test]
fn test_mapping_iter_multiple() {
    let yaml = YamlFile::from_str("name: Alice\nage: 30\nactive: true").unwrap();
    let doc = yaml.document().unwrap();
    let mapping = doc.as_mapping().unwrap();

    let pairs: Vec<_> = mapping.iter().collect();

    assert_eq!(pairs.len(), 3);
    assert_eq!(
        pairs[0].0.as_scalar().map(|s| s.as_string()),
        Some("name".to_string())
    );
    assert_eq!(
        pairs[0].1.as_scalar().map(|s| s.as_string()),
        Some("Alice".to_string())
    );
    assert_eq!(
        pairs[1].0.as_scalar().map(|s| s.as_string()),
        Some("age".to_string())
    );
    assert_eq!(pairs[1].1.to_i64(), Some(30));
    assert_eq!(
        pairs[2].0.as_scalar().map(|s| s.as_string()),
        Some("active".to_string())
    );
    assert_eq!(pairs[2].1.to_bool(), Some(true));
}

#[test]
fn test_mapping_iter_different_types() {
    let yaml = YamlFile::from_str("string: hello\nnumber: 42\nbool: false").unwrap();
    let doc = yaml.document().unwrap();
    let mapping = doc.as_mapping().unwrap();

    let pairs: Vec<_> = mapping.iter().collect();
    assert_eq!(pairs.len(), 3);

    // Check first pair (string: hello)
    assert_eq!(
        pairs[0].0.as_scalar().map(|s| s.as_string()),
        Some("string".to_string())
    );
    assert_eq!(
        pairs[0].1.as_scalar().map(|s| s.as_string()),
        Some("hello".to_string())
    );

    // Check second pair (number: 42)
    assert_eq!(
        pairs[1].0.as_scalar().map(|s| s.as_string()),
        Some("number".to_string())
    );
    assert_eq!(pairs[1].1.to_i64(), Some(42));

    // Check third pair (bool: false)
    assert_eq!(
        pairs[2].0.as_scalar().map(|s| s.as_string()),
        Some("bool".to_string())
    );
    assert_eq!(pairs[2].1.to_bool(), Some(false));
}

#[test]
fn test_mapping_iter_preserves_order() {
    let yaml = YamlFile::from_str("z: 1\na: 2\nm: 3").unwrap();
    let doc = yaml.document().unwrap();
    let mapping = doc.as_mapping().unwrap();

    let pairs: Vec<_> = mapping.iter().collect();
    assert_eq!(pairs.len(), 3);
    assert_eq!(
        pairs[0].0.as_scalar().map(|s| s.as_string()),
        Some("z".to_string())
    );
    assert_eq!(
        pairs[1].0.as_scalar().map(|s| s.as_string()),
        Some("a".to_string())
    );
    assert_eq!(
        pairs[2].0.as_scalar().map(|s| s.as_string()),
        Some("m".to_string())
    );
}

#[test]
fn test_mapping_clear_empty() {
    let yaml = YamlFile::from_str("{}").unwrap();
    let doc = yaml.document().unwrap();
    let mapping = doc.as_mapping().unwrap();

    assert_eq!(mapping.len(), 0);
    mapping.clear();
    assert_cst_ok(&yaml);
    assert_eq!(mapping.len(), 0);
}

#[test]
fn test_mapping_clear_single() {
    let yaml = YamlFile::from_str("name: Alice").unwrap();
    let doc = yaml.document().unwrap();
    let mapping = doc.as_mapping().unwrap();

    assert_eq!(mapping.len(), 1);
    mapping.clear();
    assert_cst_ok(&yaml);
    assert_eq!(mapping.len(), 0);
    assert!(mapping.is_empty());

    let keys: Vec<String> = mapping.keys().map(|k| k.to_string()).collect();
    assert_eq!(keys, Vec::<String>::new());
}

#[test]
fn test_mapping_clear_multiple() {
    let yaml = YamlFile::from_str("name: Alice\nage: 30\ncity: NYC").unwrap();
    let doc = yaml.document().unwrap();
    let mapping = doc.as_mapping().unwrap();

    assert_eq!(mapping.len(), 3);
    mapping.clear();
    assert_cst_ok(&yaml);
    assert_eq!(mapping.len(), 0);
    assert!(mapping.is_empty());

    let keys: Vec<String> = mapping.keys().map(|k| k.to_string()).collect();
    assert_eq!(keys, Vec::<String>::new());
}

#[test]
fn test_mapping_clear_and_add() {
    let yaml = YamlFile::from_str("name: Alice\nage: 30").unwrap();
    let doc = yaml.document().unwrap();
    let mapping = doc.as_mapping().unwrap();

    assert_eq!(mapping.len(), 2);
    mapping.clear();
    assert_cst_ok(&yaml);
    assert_eq!(mapping.len(), 0);

    // Add new entries after clearing
    mapping.set("new_key", "new_value");
    assert_cst_ok(&yaml);
    assert_eq!(mapping.len(), 1);
    let value = mapping.get("new_key").unwrap();
    assert_eq!(
        value.as_scalar().map(|s| s.as_string()),
        Some("new_value".to_string())
    );
}

#[test]
fn test_mapping_clear_large() {
    // Build a large mapping
    let yaml_str = (0..100)
        .map(|i| format!("key{}: value{}", i, i))
        .collect::<Vec<_>>()
        .join("\n");
    let yaml = YamlFile::from_str(&yaml_str).unwrap();
    let doc = yaml.document().unwrap();
    let mapping = doc.as_mapping().unwrap();

    assert_eq!(mapping.len(), 100);
    mapping.clear();
    assert_cst_ok(&yaml);
    assert_eq!(mapping.len(), 0);
    assert!(mapping.is_empty());
}

#[test]
fn test_mapping_newline_handling_block_style() {
    // Block-style mappings should end with newline
    let yaml_with_newline = "key1: value1\nkey2: value2\n";
    let yaml = YamlFile::from_str(yaml_with_newline).unwrap();

    // Convert back to string - should preserve the newline
    let output = yaml.to_string();
    assert!(
        output.ends_with('\n'),
        "Block-style mapping should preserve trailing newline"
    );
    assert_eq!(output, yaml_with_newline);
}

#[test]
fn test_mapping_newline_handling_no_trailing() {
    // Mapping without trailing newline
    let yaml_no_newline = "key: value";
    let yaml = YamlFile::from_str(yaml_no_newline).unwrap();

    // Convert back to string - should not add newline
    let output = yaml.to_string();
    assert!(
        !output.ends_with('\n'),
        "Mapping without trailing newline should not add one"
    );
    assert_eq!(output, yaml_no_newline);
}

#[test]
fn test_mapping_newline_handling_flow_style() {
    // Flow-style mappings typically don't have trailing newlines
    let yaml_flow = "data: {key1: value1, key2: value2}";
    let yaml = YamlFile::from_str(yaml_flow).unwrap();

    // The flow mapping should serialize exactly as parsed
    let output = yaml.to_string();
    assert_eq!(output, yaml_flow);
}

#[test]
fn test_mapping_set_preserves_newline_context() {
    // When setting values in a mapping, newline context should be preserved
    let yaml_str = "key1: value1\nkey2: value2\n";
    let yaml = YamlFile::from_str(yaml_str).unwrap();
    let doc = yaml.document().unwrap();
    let mapping = doc.as_mapping().unwrap();

    // Modify a value
    mapping.set("key1", "new_value");
    assert_cst_ok(&yaml);

    // Should still end with newline
    let output = yaml.to_string();
    assert!(
        output.ends_with('\n'),
        "Newline should be preserved after modification"
    );
}

#[test]
fn test_flow_mapping_implicit_null_key_and_value_shapes() {
    use crate::debug::{roundtrip_ok, validate_tree};
    use crate::{AsYaml, Document};

    // The parser emits `SCALAR { NULL "" }` at every "missing"
    // position so KEYs and VALUEs always hold exactly one child.
    for src in [
        "{,}",
        "{: v}",
        "{:}",
        "{k:}",
        "{k}",
        "{k, m}",
        "{a:, :b, :}",
    ] {
        let doc = Document::from_str(src).unwrap();
        validate_tree(doc.as_node().unwrap())
            .unwrap_or_else(|e| panic!("validate_tree({:?}) failed: {}", src, e));
        roundtrip_ok(doc.as_node().unwrap())
            .unwrap_or_else(|e| panic!("roundtrip({:?}) failed: {}", src, e));
    }
}

/// A single pair in a flow sequence with no key holds the same zero-width
/// null key as a flow mapping does.
#[test]
fn test_flow_pair_implicit_null_key_shape() {
    use rowan::ast::AstNode;
    let yaml = YamlFile::from_str("[: 1]").unwrap();
    assert_eq!(
        crate::debug::tree_to_string(yaml.syntax()),
        "DOCUMENT
  SEQUENCE
    LEFT_BRACKET: \"[\"
    SEQUENCE_ENTRY
      MAPPING
        MAPPING_ENTRY
          KEY
            SCALAR
              NULL: \"\"
          COLON: \":\"
          WHITESPACE: \" \"
          VALUE
            SCALAR
              INT: \"1\"
    RIGHT_BRACKET: \"]\"
"
    );
}

/// `Mapping::new_pending_block` serializes as `{}` so an empty mapping
/// survives a round-trip, and turns into a block mapping once it has an
/// entry. Before this, it rendered as `key:` followed by blank space, which
/// reparses as null: the empty mapping was silently lost.
#[test]
fn pending_block_mapping_round_trips_and_becomes_block() {
    use crate::nodes::Mapping;
    use crate::Document;

    let doc = Document::from_str("a: 1\n").unwrap();
    doc.as_mapping()
        .unwrap()
        .set("k", Mapping::new_pending_block());
    assert_cst_ok(&doc);
    let text = doc.to_string();
    assert_eq!(text, "a: 1\nk: {}\n");

    let reparsed = Document::from_str(&text).unwrap();
    let value = reparsed.as_mapping().and_then(|m| m.get("k")).unwrap();
    assert!(
        value.as_mapping().is_some(),
        "empty mapping must survive as a mapping"
    );

    let doc = Document::from_str("a: 1\n").unwrap();
    let root = doc.as_mapping().unwrap();
    root.set("k", Mapping::new_pending_block());
    assert_cst_ok(&doc);
    let nested = root.get("k").unwrap();
    nested.as_mapping().unwrap().set("x", "1");
    assert_cst_ok(&doc);
    assert_eq!(doc.to_string(), "a: 1\nk:\n  x: '1'\n");
}

/// A `{}` that was in the source keeps its flow style when an entry is
/// added; only a pending-block placeholder is rewritten.
#[test]
fn parsed_flow_mapping_stays_flow_when_filled() {
    use crate::nodes::Mapping;
    use crate::Document;

    let doc = Document::from_str("m: {}\n").unwrap();
    let nested = doc.as_mapping().unwrap().get("m").unwrap();
    nested.as_mapping().unwrap().set("x", "1");
    assert_cst_ok(&doc);
    assert_eq!(doc.to_string(), "m: {x: '1'}\n");

    let doc = Document::from_str("a: 1\n").unwrap();
    let root = doc.as_mapping().unwrap();
    root.set("k", Mapping::new_flow());
    assert_cst_ok(&doc);
    let nested = root.get("k").unwrap();
    nested.as_mapping().unwrap().set("x", "1");
    assert_cst_ok(&doc);
    assert_eq!(doc.to_string(), "a: 1\nk: {x: '1'}\n");
}

/// The placeholder marker lives only in the tree: once serialized and
/// reparsed there is nothing left to distinguish it from a source `{}`, so
/// it stays flow. YAML has nowhere to record the intent.
#[test]
fn pending_block_marker_does_not_survive_reparse() {
    use crate::nodes::Mapping;
    use crate::Document;

    let doc = Document::from_str("a: 1\n").unwrap();
    doc.as_mapping()
        .unwrap()
        .set("k", Mapping::new_pending_block());
    assert_cst_ok(&doc);
    let reparsed = Document::from_str(&doc.to_string()).unwrap();
    let nested = reparsed.as_mapping().unwrap().get("k").unwrap();
    nested.as_mapping().unwrap().set("x", "1");
    assert_cst_ok(&reparsed);
    assert_eq!(reparsed.to_string(), "a: 1\nk: {x: '1'}\n");
}

/// `set_path` builds nested intermediates as pending-block mappings, so a
/// deep path produces readable block YAML rather than nested flow.
#[test]
fn set_path_builds_block_intermediates() {
    use crate::path::YamlPath;
    use crate::Document;

    let doc = Document::from_str("name: test\n").unwrap();
    doc.try_set_path("server.database.host", "localhost")
        .unwrap();
    assert_cst_ok(&doc);
    assert_eq!(
        doc.to_string(),
        "name: test\nserver:\n  database:\n    host: localhost\n"
    );
}

/// Inserting by index into an empty mapping follows the same style rules
/// as `set`: a pending-block placeholder becomes block, a `{}` from the
/// source or from `new_flow` stays flow.
#[test]
fn insert_at_index_into_empty_mapping() {
    use crate::nodes::Mapping;
    use crate::Document;

    for preserving in [false, true] {
        let insert = |m: &Mapping| {
            if preserving {
                m.insert_at_index_preserving(0, "k", "v");
            } else {
                m.insert_at_index(0, "k", "v");
            }
        };

        let m = Mapping::new();
        insert(&m);
        assert_cst_ok(&m);
        assert_eq!(m.to_string(), "k: v\n", "preserving={preserving}");

        let m = Mapping::new_flow();
        insert(&m);
        assert_cst_ok(&m);
        assert_eq!(m.to_string(), "{k: v}", "preserving={preserving}");

        let doc = Document::from_str("m: {}\n").unwrap();
        insert(&doc.as_mapping().unwrap().get_mapping("m").unwrap());
        assert_cst_ok(&doc);
        assert_eq!(doc.to_string(), "m: {k: v}\n", "preserving={preserving}");
    }
}

/// A new key goes in with the mapping's own style and indentation.
#[test]
fn insert_at_index_preserving_new_key_matches_mapping_style() {
    use crate::Document;

    for (yaml, index, expected) in [
        ("m: {a: 1}\n", 0, "m: {k: v, a: 1}\n"),
        ("m: {a: 1}\n", 5, "m: {a: 1, k: v}\n"),
        ("m:\n  a: 1\n", 0, "m:\n  k: v\n  a: 1\n"),
        ("m:\n  a: 1\n", 5, "m:\n  a: 1\n  k: v\n"),
        ("a: 1\nb: 2\n", 1, "a: 1\nk: v\nb: 2\n"),
    ] {
        let doc = Document::from_str(yaml).unwrap();
        let root = doc.as_mapping().unwrap();
        let m = root.get_mapping("m").unwrap_or(root);
        m.insert_at_index_preserving(index, "k", "v");
        assert_cst_ok(&doc);
        assert_eq!(doc.to_string(), expected, "{yaml:?} at {index}");
    }
}

/// A root-level collection parsed from another document sits at column 0
/// and has to be shifted under the new key, including the INDENTs the
/// parser puts between its entries.
#[test]
fn test_insert_root_level_collection_from_other_document() {
    use crate::debug;
    use rowan::ast::AstNode;

    let src = YamlFile::from_str("host: localhost\nport: 5432\n").unwrap();
    let db = src.document().unwrap().as_mapping().unwrap();
    let src = YamlFile::from_str("- a\n- b\n").unwrap();
    let features = src.document().unwrap().as_sequence().unwrap();

    let doc = YamlFile::from_str("name: project\nversion: 1\n")
        .unwrap()
        .document()
        .unwrap();
    doc.set("database", &db);
    assert_eq!(
        doc.to_string(),
        "name: project\nversion: 1\ndatabase:\n  host: localhost\n  port: 5432\n"
    );
    debug::validate_tree(doc.syntax()).unwrap();
    debug::roundtrip_ok(doc.syntax()).unwrap();

    let doc = YamlFile::from_str("name: project\nversion: 1\n")
        .unwrap()
        .document()
        .unwrap();
    assert!(doc.insert_before("version", "database", &db));
    assert!(doc.insert_after("name", "features", &features));
    assert_eq!(
        doc.to_string(),
        "name: project\nfeatures:\n- a\n- b\ndatabase:\n  host: localhost\n  port: 5432\nversion: 1\n"
    );
    debug::validate_tree(doc.syntax()).unwrap();
    debug::roundtrip_ok(doc.syntax()).unwrap();
}

#[test]
fn test_set_block_value_keeps_implicit_null() {
    use crate::debug;
    use rowan::ast::AstNode;

    let src = YamlFile::from_str("s:\n    ? q\n").unwrap();
    let value = src.document().unwrap().get("s").unwrap();
    let doc = YamlFile::from_str("a: 1\n").unwrap();
    doc.document().unwrap().set("s", &value);
    assert_eq!(doc.to_string(), "a: 1\ns:\n  ? q\n");
    let reparsed = YamlFile::from_str(&doc.to_string()).unwrap();
    assert_eq!(
        debug::shape_to_string(doc.syntax()),
        debug::shape_to_string(reparsed.syntax())
    );
}

fn rename_in(src: &str, old: &str, new: &str) -> (bool, String) {
    let yaml = YamlFile::from_str(src).unwrap();
    let renamed = yaml
        .document()
        .unwrap()
        .as_mapping()
        .unwrap()
        .rename_key(old, new);
    (renamed, yaml.to_string())
}

#[test]
fn test_rename_key_onto_existing_key_replaces_it() {
    assert_eq!(
        rename_in("a: 1\nb: 2\nc: 3\n", "a", "b"),
        (true, "b: 1\nc: 3\n".to_string())
    );
    assert_eq!(
        rename_in("b: 2\na: 1\nc: 3\n", "a", "b"),
        (true, "b: 1\nc: 3\n".to_string())
    );
    assert_eq!(
        rename_in("x: 0\na: 1\nc: 3\nb: 2", "a", "b"),
        (true, "x: 0\nb: 1\nc: 3".to_string())
    );
    assert_eq!(
        rename_in("a: 1\nb: 2\nb: 3\n", "a", "b"),
        (true, "b: 1\n".to_string())
    );
    assert_eq!(
        rename_in("{a: 1, b: 2, c: 3}", "a", "b"),
        (true, "{b: 1, c: 3}".to_string())
    );
}

#[test]
fn test_rename_key_onto_existing_key_keeps_comments_and_anchor() {
    let yaml = YamlFile::from_str("top:\n  b: 2\n  # about a\n  a: &x 1\n").unwrap();
    let top = yaml
        .document()
        .unwrap()
        .as_mapping()
        .unwrap()
        .get_mapping("top")
        .unwrap();
    assert!(top.rename_key("a", "b"));
    assert_eq!(yaml.to_string(), "top:\n  # about a\n  b: &x 1\n");
}

#[test]
fn test_rename_key_to_itself_keeps_duplicates() {
    assert_eq!(
        rename_in("a: 1\na: 2\n", "a", "a"),
        (true, "a: 1\na: 2\n".to_string())
    );
}

#[test]
fn test_document_rename_key_onto_existing_key() {
    let doc = crate::yaml::Document::from_str("a: 1\nb: 2\n").unwrap();
    assert!(doc.rename_key("a", "b"));
    assert_eq!(doc.to_string(), "b: 1\n");
}

/// Entries placed relative to another in a nested block mapping line up
/// with their siblings.
#[test]
fn move_and_insert_relative_in_nested_mapping_keep_indent() {
    use crate::Document;

    let source = "outer:\n  a: 1\n  c: 3\nz: 0\n";
    let cases: [(&dyn Fn(&super::Mapping) -> bool, &str); 5] = [
        (
            &|m| m.insert_after("a", "b", 2),
            "outer:\n  a: 1\n  b: 2\n  c: 3\nz: 0\n",
        ),
        (
            &|m| m.insert_before("a", "b", 2),
            "outer:\n  b: 2\n  a: 1\n  c: 3\nz: 0\n",
        ),
        (
            &|m| m.insert_after("c", "b", 2),
            "outer:\n  a: 1\n  c: 3\n  b: 2\nz: 0\n",
        ),
        (
            &|m| m.move_after("c", "a", 9),
            "outer:\n  c: 3\n  a: 9\nz: 0\n",
        ),
        (
            &|m| m.move_before("a", "c", 9),
            "outer:\n  c: 9\n  a: 1\nz: 0\n",
        ),
    ];
    for (edit, expected) in cases {
        let doc = Document::from_str(source).unwrap();
        assert!(edit(
            &doc.as_mapping().unwrap().get_mapping("outer").unwrap()
        ));
        assert_cst_ok(&doc);
        assert_eq!(doc.to_string(), expected);
    }
}

/// A single pair in a flow sequence (`[a: 1]`) is a flow mapping without
/// braces. Edits to it give flow output, and it gains braces once it no
/// longer holds exactly one pair, which keeps it one mapping.
#[test]
fn test_edit_single_pair_in_flow_sequence() {
    use crate::debug::assert_edited;
    use crate::nodes::Mapping;
    use crate::value::YamlValue;
    let block_seq = || {
        crate::Document::from_str("- 1\n- 2\n")
            .unwrap()
            .as_sequence()
            .unwrap()
    };
    let cases: &[(&str, &dyn Fn(&Mapping), &str)] = &[
        ("[a: 1]\n", &|m| m.set("a", 2), "[a: 2]\n"),
        ("[a: 1]\n", &|m| m.set("a", block_seq()), "[a: [1, 2]]\n"),
        (
            "[a: 1]\n",
            &|m| m.set("a", vec![YamlValue::from("x"), YamlValue::from("y")]),
            "[a: [x, y]]\n",
        ),
        ("[a: 1]\n", &|m| m.set("b", 2), "[{a: 1, b: 2}]\n"),
        ("[a: 1, x]\n", &|m| m.set("b", 2), "[{a: 1, b: 2}, x]\n"),
        (
            "[x, a: 1]\n",
            &|m| m.insert_at_index(0, "b", 2),
            "[x, {b: 2, a: 1}]\n",
        ),
        (
            "[a: 1]\n",
            &|m| assert!(m.insert_after("a", "b", 2)),
            "[{a: 1, b: 2}]\n",
        ),
        ("[a: 1]\n", &|m| assert!(m.remove("a").is_some()), "[{}]\n"),
        ("[a: 1]\n", &|m| m.clear(), "[{}]\n"),
        ("[a: 1]\n", &|m| assert!(m.rename_key("a", "b")), "[b: 1]\n"),
        ("[? a : 1]\n", &|m| m.set("b", 2), "[{? a : 1, b: 2}]\n"),
        ("[? a : 1]\n", &|m| m.set("a", 2), "[? a : 2]\n"),
        (
            "[? a : 1]\n",
            &|m| assert!(m.remove("a").is_some()),
            "[{}]\n",
        ),
        (
            "[? a : 1, x]\n",
            &|m| assert!(m.rename_key("a", "b")),
            "[? b : 1, x]\n",
        ),
        ("[: 1]\n", &|m| m.set("b", 2), "[{: 1, b: 2}]\n"),
        ("[\"a\": 1]\n", &|m| m.set("a", "x"), "[\"a\": \"x\"]\n"),
        (
            "[\"a\": 1]\n",
            &|m| m.set("b", "x"),
            "[{\"a\": 1, \"b\": \"x\"}]\n",
        ),
    ];
    for (source, edit, expected) in cases {
        let yaml = YamlFile::from_str(source).unwrap();
        let doc = yaml.document().unwrap();
        let seq = doc.as_sequence().unwrap();
        let mapping = (0..seq.len())
            .find_map(|i| seq.get(i).and_then(|n| n.as_mapping().cloned()))
            .unwrap();
        assert!(mapping.is_flow_style());
        edit(&mapping);
        assert_edited(&yaml, expected);
    }
}

/// Giving a value to a block explicit key that has none adds the `:` line
/// the value goes on.
#[test]
fn test_set_value_of_explicit_key_without_one() {
    use crate::debug::assert_edited;
    for (source, expected) in [
        ("? a", "? a\n: 5"),
        ("? a\n", "? a\n: 5\n"),
        ("? a\nb: 1\n", "? a\n: 5\nb: 1\n"),
        ("? a # c", "? a # c\n: 5"),
        ("? a # c\nb: 1\n", "? a # c\n: 5\nb: 1\n"),
    ] {
        let yaml = YamlFile::from_str(source).unwrap();
        yaml.document().unwrap().as_mapping().unwrap().set("a", 5);
        assert_edited(&yaml, expected);
    }
    for (source, expected) in [
        ("x:\n  ? a", "x:\n  ? a\n  : 5"),
        ("x:\n  ? a\ny: 1\n", "x:\n  ? a\n  : 5\ny: 1\n"),
    ] {
        let yaml = YamlFile::from_str(source).unwrap();
        let doc = yaml.document().unwrap();
        doc.as_mapping()
            .unwrap()
            .get_mapping("x")
            .unwrap()
            .set("a", 5);
        assert_edited(&yaml, expected);
    }
    let seq = || {
        crate::Document::from_str("- 1\n- 2\n")
            .unwrap()
            .as_sequence()
            .unwrap()
    };
    for (source, expected) in [
        ("? a", "? a\n:\n  - 1\n  - 2\n"),
        ("? a\n", "? a\n:\n  - 1\n  - 2\n"),
        ("? a\nb: 1\n", "? a\n:\n  - 1\n  - 2\nb: 1\n"),
    ] {
        let yaml = YamlFile::from_str(source).unwrap();
        yaml.document()
            .unwrap()
            .as_mapping()
            .unwrap()
            .set("a", seq());
        assert_edited(&yaml, expected);
    }
}

/// The line break ending an unterminated last entry goes where the parser
/// puts it: in the innermost entry or block scalar on that line.
#[test]
fn test_set_after_unterminated_nested_value() {
    use crate::debug::assert_edited;
    for (source, expected) in [
        ("a:\n  - 1", "a:\n  - 1\nb: 2"),
        ("a:\n  x: 1", "a:\n  x: 1\nb: 2"),
        ("a: |\n  x", "a: |\n  x\nb: 2"),
    ] {
        let yaml = YamlFile::from_str(source).unwrap();
        yaml.document().unwrap().as_mapping().unwrap().set("b", 2);
        assert_edited(&yaml, expected);
    }
}

/// A new entry after one whose value ends in a comment on its last line
/// goes after that comment's line, where a re-parse keeps the comment.
#[test]
fn test_insert_after_nested_value_with_line_comment() {
    use crate::debug::assert_edited;
    use rowan::ast::AstNode;
    let cases = [
        ("x:\n  - 1 # c\n", "x:\n  - 1 # c\ny: 2\n"),
        ("x:\n  - 1 # c", "x:\n  - 1 # c\ny: 2"),
        ("x:\n  - 1 # c\nz: 3\n", "x:\n  - 1 # c\ny: 2\nz: 3\n"),
        ("x:\n- 1 # c\nz: 3\n", "x:\n- 1 # c\ny: 2\nz: 3\n"),
        ("x:\n  - - 1 # c\n", "x:\n  - - 1 # c\ny: 2\n"),
        ("x:\n  - [1] # c\n", "x:\n  - [1] # c\ny: 2\n"),
        ("x:\n  - | # c\n    t\n", "x:\n  - | # c\n    t\ny: 2\n"),
        ("x:\n  a: 1 # c\n", "x:\n  a: 1 # c\ny: 2\n"),
        ("x:\n  - a: 1 # c\n", "x:\n  - a: 1 # c\ny: 2\n"),
        ("x:\n  a:\n    - 1 # c\n", "x:\n  a:\n    - 1 # c\ny: 2\n"),
        ("x: | # c\n  t\n", "x: | # c\n  t\ny: 2\n"),
        (
            "- x:\n    a: 1 # c\n- 2\n",
            "- x:\n    a: 1 # c\n  y: 2\n- 2\n",
        ),
        (
            "a:\n  x:\n    - 1 # c\nb: 1\n",
            "a:\n  x:\n    - 1 # c\n  y: 2\nb: 1\n",
        ),
    ];
    // `set` appends and `insert_before` needs a following key, so each
    // stands in for the other where it would not put `y` after `x`.
    let set_or_insert_before = |m: &crate::Mapping| {
        if m.contains_key("z") {
            assert!(m.insert_before("z", "y", 2));
        } else {
            m.set("y", 2);
        }
    };
    let ops: [&dyn Fn(&crate::Mapping); 4] = [
        &set_or_insert_before,
        &|m| m.insert_at_index(1, "y", 2),
        &|m| assert!(m.insert_after("x", "y", 2)),
        &|m| m.set_with_field_order("y", 2, ["x", "y", "z"]),
    ];
    for (source, expected) in cases {
        for op in ops {
            let yaml = YamlFile::from_str(source).unwrap();
            let mapping = yaml
                .syntax()
                .descendants()
                .filter_map(crate::Mapping::cast)
                .find(|m| m.contains_key("x"))
                .unwrap();
            op(&mapping);
            assert_edited(&yaml, expected);
        }
    }
}

/// The same after a sequence at the column of the mapping's own keys,
/// with or without a comment ending it.
#[test]
fn test_insert_after_nested_indentless_sequence() {
    use crate::debug::assert_edited;
    use rowan::ast::AstNode;
    let cases = [
        ("- x:\n  - 1\n- 2\n", "- x:\n  - 1\n  y: 2\n- 2\n"),
        ("- x:\n  - 1 # c\n- 2\n", "- x:\n  - 1 # c\n  y: 2\n- 2\n"),
        ("- x:\n  - 1 # c\n", "- x:\n  - 1 # c\n  y: 2\n"),
        ("- x:\n  - 1\n  z: 3\n", "- x:\n  - 1\n  y: 2\n  z: 3\n"),
        (
            "- x:\n  - 1 # c\n  z: 3\n- 2\n",
            "- x:\n  - 1 # c\n  y: 2\n  z: 3\n- 2\n",
        ),
        (
            "- x:\n  - 1\n\n  z: 3\n- 2\n",
            "- x:\n  - 1\n\n  y: 2\n  z: 3\n- 2\n",
        ),
        (
            "- x:\n  - | # c\n    t\n- 2\n",
            "- x:\n  - | # c\n    t\n  y: 2\n- 2\n",
        ),
        (
            "a:\n  x:\n  - 1 # c\n  z: 3\n",
            "a:\n  x:\n  - 1 # c\n  y: 2\n  z: 3\n",
        ),
    ];
    let set_or_insert_before = |m: &crate::Mapping| {
        if m.contains_key("z") {
            assert!(m.insert_before("z", "y", 2));
        } else {
            m.set("y", 2);
        }
    };
    let ops: [&dyn Fn(&crate::Mapping); 4] = [
        &set_or_insert_before,
        &|m| m.insert_at_index(1, "y", 2),
        &|m| assert!(m.insert_after("x", "y", 2)),
        &|m| m.set_with_field_order("y", 2, ["x", "y", "z"]),
    ];
    for (source, expected) in cases {
        for op in ops {
            let yaml = YamlFile::from_str(source).unwrap();
            let mapping = yaml
                .syntax()
                .descendants()
                .filter_map(crate::Mapping::cast)
                .find(|m| m.contains_key("x"))
                .unwrap();
            op(&mapping);
            assert_edited(&yaml, expected);
        }
    }
}

/// Edits keep the `?` of an explicit key in a flow mapping, and new entries
/// beside it are implicit, as in a single pair in a flow sequence.
#[test]
fn test_edit_flow_mapping_with_explicit_key() {
    use crate::debug::assert_edited;
    use crate::nodes::Mapping;
    let cases: &[(&str, &dyn Fn(&Mapping), &str)] = &[
        ("{? a : 1}\n", &|m| m.set("b", 2), "{? a : 1, b: 2}\n"),
        ("{? a : 1}\n", &|m| m.set("a", 2), "{? a : 2}\n"),
        (
            "{? a : 1}\n",
            &|m| assert!(m.rename_key("a", "b")),
            "{? b : 1}\n",
        ),
        (
            "{? a : 1, c: 3}\n",
            &|m| assert!(m.remove("a").is_some()),
            "{c: 3}\n",
        ),
    ];
    for (source, edit, expected) in cases {
        let yaml = YamlFile::from_str(source).unwrap();
        let mapping = yaml.document().unwrap().as_mapping().unwrap();
        edit(&mapping);
        assert_edited(&yaml, expected);
    }
}

/// Setting a value next to a comment keeps the comment out of a block
/// scalar's text, and the entries after it on their own lines.
///
/// Regression test for issues #220, #221 and #222.
#[test]
fn test_set_block_scalar_beside_comment() {
    use crate::debug::assert_edited;
    for (source, key, value, expected) in [
        (
            "first: | # c\n  x\nsecond: 1\n",
            "first",
            "Z",
            "first: Z # c\nsecond: 1\n",
        ),
        (
            "first: a # c\nsecond: x\n",
            "first",
            "line1\nline2\n",
            "first: | # c\n  line1\n  line2\nsecond: x\n",
        ),
        (
            "first: |\n    x\n  # note\nsecond: 1\n",
            "first",
            "a\nb\n",
            "first: |\n  a\n  b\n# note\nsecond: 1\n",
        ),
        (
            "first: 1\n\nsecond: 1\nlast: end # t\n",
            "last",
            "kept\n\n\n",
            "first: 1\n\nsecond: 1\nlast: |+ # t\n  kept\n\n\n",
        ),
    ] {
        let yaml = YamlFile::from_str(source).unwrap();
        let mapping = yaml.document().unwrap().as_mapping().unwrap();
        mapping.set(key, value);
        assert_edited(&yaml, expected);
        let mapping = yaml.document().unwrap().as_mapping().unwrap();
        assert_eq!(
            mapping.get(key).unwrap().as_scalar().unwrap().as_string(),
            value,
            "{source:?}"
        );
    }
}

/// A `:` right after an alias would be read as part of its name (`*a:`),
/// so an alias key is followed by a space.
#[test]
fn test_alias_key_is_separated_from_colon() {
    use crate::debug::assert_edited;
    use crate::yaml::Alias;

    let edits: [(&str, fn(&crate::Mapping), &str); 6] = [
        (
            "x: &a k\n",
            |m| m.set(Alias::new("a"), 1),
            "x: &a k\n*a : 1\n",
        ),
        (
            "{x: &a k}\n",
            |m| m.set(Alias::new("a"), 1),
            "{x: &a k, *a : 1}\n",
        ),
        (
            "x: &a k\ny: 1\n",
            |m| assert!(m.rename_key("y", Alias::new("a"))),
            "x: &a k\n*a : 1\n",
        ),
        (
            "{x: &a k, y: 1}\n",
            |m| assert!(m.rename_key("y", Alias::new("a"))),
            "{x: &a k, *a : 1}\n",
        ),
        (
            "x: &a k\ny: 1\n",
            |m| assert!(m.insert_after("x", Alias::new("a"), 2)),
            "x: &a k\n*a : 2\ny: 1\n",
        ),
        (
            "x: &a k\nb: [1]\nc:\n  *a : 1\n",
            |m| {
                let c = m.get("c").unwrap();
                m.get_sequence("b").unwrap().push(c);
            },
            "x: &a k\nb: [1, {*a : 1}]\nc:\n  *a : 1\n",
        ),
    ];
    for (source, edit, expected) in edits {
        let file = YamlFile::from_str(source).unwrap();
        edit(&file.document().unwrap().as_mapping().unwrap());
        assert_edited(&file, expected);
    }
}

#[test]
fn test_set_plain_scalar_with_indicator_line() {
    // The `>` token opening the scalar's second line is not a block scalar
    // header, so the scalar stays on its key's or dash's line.
    let source = YamlFile::from_str("x: b\n  > c\n").unwrap();
    let scalar = source
        .document()
        .unwrap()
        .as_mapping()
        .unwrap()
        .get("x")
        .unwrap()
        .as_scalar()
        .unwrap()
        .clone();

    let file = YamlFile::from_str("a: 1\nz: 2\n").unwrap();
    file.document()
        .unwrap()
        .as_mapping()
        .unwrap()
        .set("a", scalar.clone());
    crate::debug::assert_edited(&file, "a: b\n  > c\nz: 2\n");

    let file = YamlFile::from_str("k:\n  - 1\n").unwrap();
    file.document()
        .unwrap()
        .as_mapping()
        .unwrap()
        .get_sequence("k")
        .unwrap()
        .push(scalar);
    crate::debug::assert_edited(&file, "k:\n  - 1\n  - b\n    > c\n");
}

#[test]
fn test_set_after_bom_lines_up_with_first_key() {
    // A BOM is not content, so the first key is at column 0.
    let doc = crate::yaml::Document::from_str("\u{FEFF}a: 1\n").unwrap();
    doc.as_mapping().unwrap().set("c", 3);
    assert_cst_ok(&doc);
    assert_eq!(doc.to_string(), "\u{FEFF}a: 1\nc: 3\n");
}

#[test]
fn test_set_nested_with_cr_line_breaks() {
    let doc = crate::yaml::Document::from_str("x: 1\ra:\r  b: 1\r").unwrap();
    let mapping = doc.as_mapping().unwrap();
    mapping.get_mapping("a").unwrap().set("d", 4);
    assert_eq!(doc.to_string(), "x: 1\ra:\r  b: 1\r  d: 4\r");
}

#[test]
fn test_set_writes_crlf_line_breaks() {
    use crate::builder::MappingBuilder;
    let file = YamlFile::from_str("a: 1\r\n").unwrap();
    let doc = file.document().unwrap();
    let mapping = doc.as_mapping().unwrap();
    mapping.set("b", MappingBuilder::new().pair("c", 1).build_document());
    mapping.set("d", ScalarValue::literal("x\ny\n"));
    mapping.set("e", ScalarValue::single_quoted("x\ny"));
    crate::debug::assert_edited(
        &file,
        "a: 1\r\nb:\r\n  c: 1\r\nd: |\r\n  x\r\n  y\r\ne: 'x\r\n\r\n  y'\r\n",
    );
}

#[test]
fn test_set_without_line_breaks_writes_lf() {
    let file = YamlFile::from_str("a: 1").unwrap();
    file.document().unwrap().as_mapping().unwrap().set("b", 2);
    // The input's last line stays unterminated.
    crate::debug::assert_edited(&file, "a: 1\nb: 2");
}

#[test]
fn test_set_copied_multi_line_scalar_with_cr_line_breaks() {
    let source = crate::yaml::Document::from_str("k: 'x\r y\r\r z'\r").unwrap();
    let value = source.as_mapping().unwrap().get("k").unwrap();
    let file = YamlFile::from_str("a:\r  b:\r    c: 1\r").unwrap();
    let doc = file.document().unwrap();
    let a = doc.as_mapping().unwrap().get_mapping("a").unwrap();
    a.get_mapping("b").unwrap().set("c", &value);
    crate::debug::assert_edited(&file, "a:\r  b:\r    c: 'x\r     y\r\r     z'\r");
}

#[test]
fn test_set_copied_flow_sequence_with_cr_line_breaks() {
    let source = crate::yaml::Document::from_str("[a,\rb]\r").unwrap();
    let value = source.as_sequence().unwrap();
    let file = YamlFile::from_str("a:\r  b:\r    c: 1\r").unwrap();
    let doc = file.document().unwrap();
    let a = doc.as_mapping().unwrap().get_mapping("a").unwrap();
    a.get_mapping("b").unwrap().set("c", &value);
    crate::debug::assert_edited(&file, "a:\r  b:\r    c: [a,\r     b]\r");
}

#[test]
fn test_set_explicit_key_value_with_cr_line_breaks() {
    let file = YamlFile::from_str("? a\r? b\r").unwrap();
    file.document().unwrap().as_mapping().unwrap().set("a", "x");
    crate::debug::assert_edited(&file, "? a\r: x\r? b\r");
}

#[test]
fn test_set_block_scalar_before_blank_line_with_cr_line_breaks() {
    let file = YamlFile::from_str("a: 1\r    \rb: 2\r").unwrap();
    let doc = file.document().unwrap();
    doc.as_mapping()
        .unwrap()
        .set("a", ScalarValue::literal("x\n"));
    crate::debug::assert_edited(&file, "a: |\r  x\r\rb: 2\r");
}
