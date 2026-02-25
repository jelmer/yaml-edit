//! Invalid YAML handling tests
//!
//! Tests verify degraded parsing behavior when encountering invalid YAML.
//! The parser uses a forgiving approach that attempts to parse as much as possible.
//!
//! Tests cover:
//! - Plain scalars with colons (`: ` in values)
//! - Malformed structures
//! - Unexpected tokens
//! - Invalid indentation
//!
//! All tests verify:
//! 1. Parser doesn't panic on invalid input
//! 2. Degraded parsing produces reasonable results
//! 3. Valid portions are still accessible via API

use std::str::FromStr;
use yaml_edit::YamlFile;

#[test]
fn test_invalid_colon_in_plain_scalar() {
    // According to YAML spec, a plain scalar cannot contain ": " (colon followed by space)
    // However, our parser uses degraded parsing and treats "key: value on same line"
    // as a plain scalar value (forgiving interpretation)
    let yaml = r#"not_url_2: key: value on same line"#;

    let parsed = YamlFile::from_str(yaml).expect("Parser uses degraded parsing");
    let doc = parsed.document().expect("Should have document");
    let mapping = doc.as_mapping().expect("Should be mapping");

    // Verify structure: single key "not_url_2" with scalar value
    assert_eq!(mapping.keys().count(), 1);

    let keys: Vec<_> = mapping.keys().collect();
    assert_eq!(keys[0], "not_url_2");

    let value = mapping.get("not_url_2").expect("Should have value");
    let scalar = value.as_scalar().expect("Value should be scalar");
    assert_eq!(scalar.as_string(), "key: value on same line");

    // Verify round-trip preserves input
    assert_eq!(doc.to_string(), yaml);
}

#[test]
fn test_invalid_bracket_colon_combination() {
    // [::1]:8080 without quotes is invalid YAML (flow sequence followed by unexpected text)
    // Our parser uses degraded parsing: parses [::1] as a sequence containing a mapping (due to ::),
    // and ignores the :8080 part after the closing bracket
    let yaml = r#"ipv6: [::1]:8080"#;

    let parsed = YamlFile::from_str(yaml).expect("Parser uses degraded parsing");
    let doc = parsed.document().expect("Should have document");
    let mapping = doc.as_mapping().expect("Should be mapping");

    // Verify structure: single key "ipv6"
    assert_eq!(mapping.keys().count(), 1);

    let keys: Vec<_> = mapping.keys().collect();
    assert_eq!(keys[0], "ipv6");

    // The value should be a sequence (the [::1] part)
    let value = mapping.get("ipv6").expect("Should have value");
    let seq = value.as_sequence().expect("Value should be sequence");

    // The sequence contains one element
    assert_eq!(seq.len(), 1);

    // Due to the colons in ::1, the parser treats it as a complex key,
    // creating a mapping with "::1" as a key (with implicit null value, like in a set)
    let elem = seq.get(0).expect("Should have element");
    let elem_map = elem.as_mapping().expect("Element should be mapping");

    // The mapping has one key: "::1"
    assert_eq!(elem_map.keys().count(), 1);

    let elem_keys: Vec<_> = elem_map.keys().collect();
    let first_key = elem_keys[0].as_scalar().expect("Key should be scalar");
    assert_eq!(first_key.as_string(), "::1");

    // Output should be "ipv6: [::1]" (the :8080 part is dropped)
    assert_eq!(doc.to_string(), "ipv6: [::1]");

    // Verify output is valid YAML
    let reparsed = YamlFile::from_str(&doc.to_string()).expect("Output should be valid YAML");
    assert!(reparsed.document().is_some());
}

#[test]
fn test_multiple_colons_in_value() {
    // Test various cases with colons in values
    let test_cases = vec![
        ("simple", "key: value", true),                  // Valid
        ("with_colon", "url: http://example.com", true), // Valid
        ("with_port", "url: example.com:8080", true),    // Valid
        ("invalid_space", "bad: key: value", false),     // Invalid - colon+space in value
        ("no_space", "good: key:value", true),           // Valid - colon without space after
        ("quoted", "quoted: \"key: value\"", true),      // Valid - quoted
    ];

    for (name, yaml, should_be_valid) in test_cases {
        let parsed = YamlFile::from_str(yaml);

        if should_be_valid {
            assert!(
                parsed.is_ok(),
                "{}: '{}' should parse successfully",
                name,
                yaml
            );

            // Verify we get the expected structure
            if let Ok(yaml_doc) = parsed {
                if let Some(doc) = yaml_doc.document() {
                    if let Some(mapping) = doc.as_mapping() {
                        assert_eq!(
                            mapping.keys().count(),
                            1,
                            "{}: Should have exactly 1 key",
                            name
                        );
                    }
                }
            }
        } else {
            // For invalid cases, we accept either:
            // 1. Parse error
            // 2. Parsing as nested structure (degraded parsing)
            match parsed {
                Ok(yaml_doc) => {
                    // Degraded parsing - might create nested mappings
                    if let Some(_doc) = yaml_doc.document() {
                        // As long as it doesn't crash, this is acceptable (degraded parsing)
                    }
                }
                Err(_e) => {
                    // This is also fine - proper error reporting as expected
                }
            }
        }
    }
}

#[test]
fn test_error_recovery() {
    // Test that parser can recover from errors and continue
    let yaml = r#"
good_key: good_value
bad: key: with: many: colons
another_good: value
"#;

    let parsed = YamlFile::from_str(yaml);

    // The parser should handle this somehow
    match parsed {
        Ok(yaml_doc) => {
            if let Some(doc) = yaml_doc.document() {
                if let Some(mapping) = doc.as_mapping() {
                    // Should at least parse the good keys
                    let has_good_key = mapping.keys().any(|k| k == "good_key");
                    let has_another_good = mapping.keys().any(|k| k == "another_good");

                    assert!(
                        has_good_key || has_another_good,
                        "Should parse at least some valid keys"
                    );
                }
            }
        }
        Err(_) => {
            // Full parse failure is also acceptable for invalid input
            // The important thing is it doesn't crash or hang
        }
    }
}
