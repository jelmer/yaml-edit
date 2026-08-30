//! Tests for `Mapping` and `MappingEntry`. Split out of
//! `nodes/mapping/mod.rs` unchanged.

use super::*;
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
    assert_eq!(doc.to_string().trim(), "name: Alice\nfoo: []");
    let reparsed = Document::from_str(&doc.to_string()).unwrap();
    assert_eq!(
        reparsed.as_mapping().unwrap().get("foo").unwrap().kind(),
        YamlKind::Sequence
    );

    let map = MappingBuilder::new().build_document().as_mapping().unwrap();
    let doc = Document::from_str("name: Alice").unwrap();
    doc.as_mapping().unwrap().set("foo", map);
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
    assert_eq!(
        mapping.get("a").unwrap().as_scalar().unwrap().as_string(),
        "99"
    );
    assert!(!mapping.insert_before("missing", "b", "88"));
    assert_eq!(
        mapping.get("b").unwrap().as_scalar().unwrap().as_string(),
        "88"
    );
}

/// Setting into a flow-style mapping (`{...}`) inserts entries inside
/// the braces with proper `, ` separators, not on new lines after `}`
/// (which would produce broken YAML).
#[test]
fn test_set_into_flow_mapping_inserts_inside_braces() {
    use crate::yaml::Document;

    // Cases: (input, key to set, expected output)
    let cases = [
        ("outer: {}", "a", r#"outer: {a: "X"}"#),
        ("outer: {a: 1}", "b", r#"outer: {a: 1, b: "X"}"#),
        // Trailing-comma style: the existing `,` is reused as the
        // separator instead of stacking a second one.
        ("outer: {a: 1,}", "b", r#"outer: {a: 1,b: "X"}"#),
        // Update-in-place stays inside the braces.
        ("outer: {a: 1}", "a", "outer: {a: \"X\"}"),
    ];
    for (input, key, expected) in cases {
        let doc = Document::from_str(input).unwrap();
        let inner_val = doc.as_mapping().unwrap().get("outer").unwrap();
        let inner = inner_val.as_mapping().unwrap();
        inner.set(key, "X");
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
        // Update Archive - should stay in position 3
        mapping.set("Archive", "PyPI");
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
    assert!(
        !failed,
        "insert_after should fail when reference key doesn't exist"
    );

    // Test updating existing key through insert_after
    let updated = doc.insert_after("first", "second", "2_updated");
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
    assert!(
        !failed,
        "insert_before should fail when reference key doesn't exist"
    );

    // Test updating existing key through insert_before
    let updated = doc.insert_before("fourth", "third", "3_updated");
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

    let output = doc.to_string();

    // Check exact output - should preserve original structure and insert correctly
    let expected = r#"first: 1
second: 2
third: 3"#;
    assert_eq!(output.trim(), expected);

    // Insert at index 0 (beginning)
    doc.insert_at_index(0, "zero", 0);
    let output2 = doc.to_string();
    let expected2 = r#"zero: 0
first: 1
second: 2
third: 3"#;
    assert_eq!(output2.trim(), expected2);

    // Insert at out-of-bounds index (should append at end)
    doc.insert_at_index(100, "last", "999");
    let output3 = doc.to_string();
    let expected3 = r#"zero: 0
first: 1
second: 2
third: 3
last: '999'"#;
    assert_eq!(output3.trim(), expected3);

    // Test updating existing key through insert_at_index
    doc.insert_at_index(2, "first", "1_updated");
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
    doc.insert_before("key1", "key with spaces", "value with spaces");
    doc.insert_at_index(1, "key@symbol", "value#hash");

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
    doc.insert_before("key1", "null_key", ScalarValue::null());

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
    assert!(mapping.remove("b").is_some());
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
            mapping.set("published", true);
            mapping.set("downloads", 1000);

            // Remove a field
            mapping.remove("author");

            // Rename a field
            mapping.rename_key("version", "app_version");

            // Update existing field
            mapping.set("year", 2024);
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
downloads: 1000
"#;
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
    mapping.set("bool", false);
    mapping.set("text", "hello");

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
    assert!(!mapping.rename_key("old", "new"));

    // Can still add to empty mapping
    mapping.set("first", "value");
    assert!(!mapping.is_empty());
    // In flow-style (JSON) context, strings are quoted
    assert_eq!(
        mapping.get("first").map(|v| v.to_string()),
        Some("\"value\"".to_string())
    );
}

#[test]
fn test_mapping_remove_first_of_three() {
    let yaml = YamlFile::from_str("a: 1\nb: 2\nc: 3").unwrap();
    let doc = yaml.document().unwrap();
    let mapping = doc.as_mapping().unwrap();

    assert!(mapping.remove("a").is_some());

    let keys: Vec<String> = mapping.keys().map(|k| k.to_string()).collect();
    assert_eq!(keys, vec!["b", "c"]);
}

#[test]
fn test_mapping_remove_middle_of_three() {
    let yaml = YamlFile::from_str("a: 1\nb: 2\nc: 3").unwrap();
    let doc = yaml.document().unwrap();
    let mapping = doc.as_mapping().unwrap();

    assert!(mapping.remove("b").is_some());

    let keys: Vec<String> = mapping.keys().map(|k| k.to_string()).collect();
    assert_eq!(keys, vec!["a", "c"]);
}

#[test]
fn test_mapping_remove_last_of_three() {
    let yaml = YamlFile::from_str("a: 1\nb: 2\nc: 3").unwrap();
    let doc = yaml.document().unwrap();
    let mapping = doc.as_mapping().unwrap();

    assert!(mapping.remove("c").is_some());

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
    assert_eq!(mapping.len(), 2);

    mapping.set("city", "NYC");
    assert_eq!(mapping.len(), 3);
}

#[test]
fn test_mapping_len_after_removing() {
    let yaml = YamlFile::from_str("name: Alice\nage: 30\ncity: NYC").unwrap();
    let doc = yaml.document().unwrap();
    let mapping = doc.as_mapping().unwrap();

    assert_eq!(mapping.len(), 3);

    mapping.remove("age");
    assert_eq!(mapping.len(), 2);

    mapping.remove("city");
    assert_eq!(mapping.len(), 1);

    mapping.remove("name");
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
    assert_eq!(mapping.len(), 0);
}

#[test]
fn test_mapping_clear_single() {
    let yaml = YamlFile::from_str("name: Alice").unwrap();
    let doc = yaml.document().unwrap();
    let mapping = doc.as_mapping().unwrap();

    assert_eq!(mapping.len(), 1);
    mapping.clear();
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
    assert_eq!(mapping.len(), 0);

    // Add new entries after clearing
    mapping.set("new_key", "new_value");
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

    // Should still end with newline
    let output = yaml.to_string();
    assert!(
        output.ends_with('\n'),
        "Newline should be preserved after modification"
    );
}
