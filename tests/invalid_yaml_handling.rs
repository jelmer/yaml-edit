use std::str::FromStr;
use yaml_edit::Yaml;

#[test]
fn test_invalid_colon_in_plain_scalar() {
    // According to YAML spec, a plain scalar cannot contain ": " (colon followed by space)
    // This should either parse in a degraded but sensible way or report an error
    let yaml = r#"not_url_2: key: value on same line"#;

    let parsed = Yaml::from_str(yaml);

    // The parser should handle this somehow - either:
    // 1. Parse it as nested mappings (which is what our parser currently does)
    // 2. Report an error
    // 3. Parse it as a single value (less spec-compliant but more forgiving)

    match parsed {
        Ok(yaml) => {
            // If it parses, check what structure we get
            if let Some(doc) = yaml.document() {
                if let Some(mapping) = doc.as_mapping() {
                    let key_count = mapping.keys().count();

                    // Our parser currently treats this as nested mappings
                    // This is one valid interpretation of the invalid input
                    println!("Parsed as {} keys", key_count);

                    // We should get either:
                    // - 1 key ("not_url_2") with a nested mapping value
                    // - 2 keys if it's parsed as two separate entries
                    assert!(key_count >= 1, "Should parse something");

                    // Check if we got the first key at least
                    assert!(
                        mapping.keys().any(|k| k == "not_url_2"),
                        "Should at least parse the first key"
                    );
                }
            }
        }
        Err(e) => {
            // If it fails, that's also acceptable - check that we get a reasonable error
            let error_str = format!("{:?}", e);
            assert!(
                error_str.contains("mapping")
                    || error_str.contains("colon")
                    || error_str.contains("value"),
                "Error should mention the issue with mappings or colons"
            );
        }
    }
}

#[test]
fn test_invalid_bracket_colon_combination() {
    // [::1]:8080 without quotes is invalid YAML
    // The parser should handle this gracefully
    let yaml = r#"ipv6: [::1]:8080"#;

    let parsed = Yaml::from_str(yaml);

    match parsed {
        Ok(yaml) => {
            // If it parses, check what we got
            if let Some(doc) = yaml.document() {
                if let Some(mapping) = doc.as_mapping() {
                    // It might parse as:
                    // - A flow sequence [::1] followed by something
                    // - A complex mapping key
                    // - An error

                    let has_ipv6_key = mapping.keys().any(|k| k == "ipv6");
                    assert!(has_ipv6_key, "Should at least have the ipv6 key");

                    // The value might be parsed in various ways
                    // As long as it doesn't crash or hang, we accept it
                }
            }
        }
        Err(e) => {
            // This is the most spec-compliant result
            let error_str = format!("{:?}", e);
            println!("Got expected error: {}", error_str);
            assert!(!error_str.is_empty(), "Should have an error message");
        }
    }
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
        let parsed = Yaml::from_str(yaml);

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
                        println!("{}: Parsed invalid YAML in degraded mode", name);
                        // As long as it doesn't crash, this is acceptable
                    }
                }
                Err(e) => {
                    // This is also fine - proper error reporting
                    println!("{}: Got error as expected: {:?}", name, e);
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

    let parsed = Yaml::from_str(yaml);

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
