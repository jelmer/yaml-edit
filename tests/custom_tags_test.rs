//! Custom tag handlers, the bundled ones and one implemented outside the
//! crate: `CustomTagHandler` is public API, so its signature has to be
//! nameable.

use std::str::FromStr;
use yaml_edit::custom_tags::{
    CompressedBinaryHandler, CustomTagError, CustomTagHandler, CustomTagRegistry, EnvVarHandler,
    JsonHandler, TimestampHandler,
};
use yaml_edit::{Document, ScalarValue, YamlFile, YamlValue};

#[test]
fn test_registry_basic_usage() {
    let mut registry = CustomTagRegistry::new();

    // Register handlers
    registry
        .register("!timestamp", TimestampHandler::new("%Y-%m-%d"))
        .unwrap();
    registry.register("!json", JsonHandler).unwrap();

    // Check if tags are registered
    assert!(registry.has_tag("!timestamp"));
    assert!(registry.has_tag("!json"));

    // List all registered tags
    let tags = registry.registered_tags();
    assert_eq!(tags.len(), 2);
    let mut sorted_tags = tags.clone();
    sorted_tags.sort();
    assert_eq!(
        sorted_tags,
        vec!["!json".to_string(), "!timestamp".to_string()]
    );
}

#[test]
fn test_parsing_with_custom_tags() {
    // YAML with custom tags
    let yaml_str = r#"config:
  created: !timestamp 2024-01-15
  data: !json {"status": "active"}
"#;

    // Parse the YAML (tags are preserved in the syntax tree)
    let yaml = YamlFile::from_str(yaml_str).unwrap();

    // Tags are preserved exactly as they appear
    assert_eq!(yaml.to_string(), yaml_str);
}

#[test]
fn test_format_preserving_round_trip() {
    let original = r#"config:
  timestamp: !timestamp  2024-01-15    # Creation date
  data:      !json       {"key": "value"}     # Config data
"#;

    let yaml = YamlFile::from_str(original).unwrap();

    // All formatting, spacing, and comments are preserved during round-trip
    assert_eq!(yaml.to_string(), original);

    // Tags are preserved in the parsed document
    if let Some(doc) = yaml.document() {
        if let Some(config) = doc.as_mapping() {
            if let Some(mapping) = config.get_mapping("config") {
                // Verify we can read the tagged values
                if let Some(timestamp_node) = mapping.get("timestamp") {
                    if let Some(tagged) = timestamp_node.as_tagged() {
                        assert_eq!(tagged.tag(), Some("!timestamp".to_string()));
                    }
                }

                if let Some(data_node) = mapping.get("data") {
                    if let Some(tagged) = data_node.as_tagged() {
                        assert_eq!(tagged.tag(), Some("!json".to_string()));
                    }
                }
            }
        }
    }
}

#[test]
fn test_validate_tagged_content() {
    let mut registry = CustomTagRegistry::new();
    registry
        .register("!timestamp", TimestampHandler::new("%Y-%m-%d"))
        .unwrap();

    // Validate content
    assert!(registry.validate("!timestamp", "2024-01-15").is_ok());

    // This will fail validation
    assert!(registry.validate("!timestamp", "").is_err());
}

#[test]
fn test_timestamp_handler_example() {
    let mut registry = CustomTagRegistry::new();
    registry
        .register("!timestamp", TimestampHandler::new("%Y-%m-%d"))
        .unwrap();

    let yaml_str = "created: !timestamp 2024-01-15\n";
    let yaml = YamlFile::from_str(yaml_str).unwrap();

    // Format is preserved
    assert_eq!(yaml.to_string(), yaml_str);
}

#[test]
fn test_json_handler_example() {
    let mut registry = CustomTagRegistry::new();
    registry.register("!json", JsonHandler).unwrap();

    let yaml_str = r#"data: !json {"name": "Alice", "age": 30}"#;
    let yaml = YamlFile::from_str(yaml_str).unwrap();

    // JSON content is preserved exactly
    assert_eq!(yaml.to_string(), yaml_str);
}

#[test]
fn test_env_var_handler_example() {
    let mut registry = CustomTagRegistry::new();
    registry.register("!env", EnvVarHandler).unwrap();

    // Set an environment variable
    std::env::set_var("TEST_PATH", "/home/user");

    // The handler can validate env var names
    assert!(registry.validate("!env", "TEST_PATH").is_ok());
    assert!(registry.validate("!env", "invalid var name").is_err());

    std::env::remove_var("TEST_PATH");
}

#[test]
fn test_compressed_binary_handler_example() {
    let mut registry = CustomTagRegistry::new();
    registry
        .register("!compressed", CompressedBinaryHandler::new(6))
        .unwrap();

    let yaml_str = "binary: !compressed SGVsbG8gV29ybGQ=\n"; // "Hello World" in base64
    let yaml = YamlFile::from_str(yaml_str).unwrap();

    // Base64 content is preserved
    assert_eq!(yaml.to_string(), yaml_str);
}

#[test]
fn test_error_handling() {
    let mut registry = CustomTagRegistry::new();
    registry
        .register("!timestamp", TimestampHandler::new("%Y-%m-%d"))
        .unwrap();

    // Validation errors include helpful context
    match registry.validate("!timestamp", "invalid-date") {
        Ok(_) => {}
        Err(e) => {
            assert_eq!(e.tag, "!timestamp");
            assert!(!e.message.is_empty());
        }
    }

    // Unregistered tags return errors
    match registry.validate("!unknown", "content") {
        Err(e) => assert_eq!(e.message, "Tag not registered"),
        _ => panic!("Should have failed"),
    }
}

#[test]
fn test_registry_errors() {
    let mut registry = CustomTagRegistry::new();

    // Invalid tag name (missing !)
    match registry.register("timestamp", TimestampHandler::new("%Y-%m-%d")) {
        Ok(_) => panic!("Should have failed"),
        Err(e) => assert_eq!(
            e.message,
            "Invalid tag name: must be a YAML tag such as '!name', '!!name' or '!<uri>'"
        ),
    }

    // Valid tag name
    registry
        .register("!timestamp", TimestampHandler::new("%Y-%m-%d"))
        .unwrap();
}

#[test]
fn test_preserve_whitespace_in_tags() {
    let original = "date: !timestamp   2024-01-15   # Important date\n";
    let yaml = YamlFile::from_str(original).unwrap();

    // Whitespace and comments are preserved
    assert_eq!(yaml.to_string(), original);
}

#[test]
fn test_working_with_tagged_nodes() {
    let yaml_str = r#"email: !email user@example.com
id: !uuid 550e8400-e29b-41d4-a716-446655440000
"#;

    let doc = Document::from_str(yaml_str).unwrap();

    if let Some(mapping) = doc.as_mapping() {
        // Get the email node
        if let Some(email_node) = mapping.get("email") {
            // Tagged values are accessed via as_tagged()
            if let Some(tagged) = email_node.as_tagged() {
                assert_eq!(tagged.tag(), Some("!email".to_string()));

                // Get the actual value from the tagged node
                if let Some(scalar) = tagged.value() {
                    assert_eq!(scalar.to_string(), "user@example.com");
                } else {
                    panic!("Tagged node should have a scalar value");
                }
            } else {
                panic!("Should be a tagged node");
            }

            // The tag information is preserved in the syntax tree
            // and will be maintained during round-trip serialization
        } else {
            panic!("Should have email key");
        }
    } else {
        panic!("Should be a mapping");
    }
}

struct Upper;

impl CustomTagHandler for Upper {
    fn serialize(&self, value: &YamlValue) -> Result<String, CustomTagError> {
        match value {
            YamlValue::Scalar(s) => Ok(s.value().to_uppercase()),
            _ => Err(CustomTagError::new("!upper", "not a scalar")),
        }
    }

    fn deserialize(&self, content: &str) -> Result<YamlValue, CustomTagError> {
        Ok(YamlValue::Scalar(ScalarValue::string(content)))
    }

    fn description(&self) -> &str {
        "uppercase"
    }
}

/// The trait names `YamlValue`, so that type has to be exported too: while it
/// was not, no caller outside the crate could write this impl at all.
#[test]
fn a_handler_can_be_implemented_outside_the_crate() {
    let handler = Upper;
    let value = YamlValue::Scalar(ScalarValue::string("abc"));
    assert_eq!(handler.serialize(&value).unwrap(), "ABC");
    assert_eq!(
        handler.deserialize("xyz").unwrap(),
        YamlValue::Scalar(ScalarValue::string("xyz"))
    );
    assert_eq!(handler.description(), "uppercase");
}
