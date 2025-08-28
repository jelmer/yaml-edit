use std::str::FromStr;
use yaml_edit::{Yaml, YamlValue};

#[test]
fn test_rename_key_basic() {
    let original = r#"name: my-app
version: 1.0
author: Alice"#;

    let mut yaml = Yaml::from_str(original).unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mut mapping) = doc.as_mapping() {
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

    let mut yaml = Yaml::from_str(original).unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mut mapping) = doc.as_mapping() {
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

    let mut yaml = Yaml::from_str(original).unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mut mapping) = doc.as_mapping() {
            let removed = mapping.remove(&YamlValue::scalar("author"));
            assert!(removed);
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

    let mut yaml = Yaml::from_str(original).unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mut mapping) = doc.as_mapping() {
            // Add new fields
            mapping.set(&YamlValue::scalar("license"), &YamlValue::scalar("MIT"));
            mapping.set(&YamlValue::scalar("published"), &YamlValue::scalar("true"));
            mapping.set(&YamlValue::scalar("downloads"), &YamlValue::scalar("1000"));

            // Remove a field
            mapping.remove(&YamlValue::scalar("author"));

            // Rename a field
            mapping.rename_key("version", "app_version");

            // Update existing field
            mapping.set(&YamlValue::scalar("year"), &YamlValue::scalar("2024"));
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
