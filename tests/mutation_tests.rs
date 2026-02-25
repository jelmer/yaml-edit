//! Mutation and editing tests
//!
//! Tests verify:
//! - Simple value replacement (scalars, booleans, numbers)
//! - Nested mapping mutations
//! - Adding new keys to existing mappings
//! - Multiple root-level changes
//! - Preservation of comments and whitespace during mutations
//! - Deep nested structure mutations
//!
//! All tests verify:
//! 1. API correctness - mutations work as expected
//! 2. Lossless editing - formatting and structure preserved
//! 3. Round-trip validity - output can be re-parsed

use std::str::FromStr;
use yaml_edit::YamlFile;

#[test]
fn test_simple_value_replacement() {
    let yaml = YamlFile::from_str("key: value").unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mapping) = doc.as_mapping() {
            mapping.set("key", "new_value");
        }
    }

    assert_eq!(yaml.to_string(), "key: new_value");

    // Verify output is valid YAML
    let output = yaml.to_string();
    let reparsed = YamlFile::from_str(&output).expect("Output should be valid YAML");
    assert!(reparsed.document().is_some());
}

#[test]
fn test_value_replacement_preserves_whitespace() {
    let yaml = YamlFile::from_str("key: value  # comment").unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mapping) = doc.as_mapping() {
            mapping.set("key", "new_value");
        }
    }

    assert_eq!(yaml.to_string(), "key: new_value  # comment");

    // Verify output is valid YAML
    let output = yaml.to_string();
    let reparsed = YamlFile::from_str(&output).expect("Output should be valid YAML");
    assert!(reparsed.document().is_some());
}

#[test]
fn test_boolean_value_replacement() {
    let yaml = YamlFile::from_str("debug: true  # For now").unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mapping) = doc.as_mapping() {
            mapping.set("debug", false);
        }
    }

    assert_eq!(yaml.to_string(), "debug: false  # For now");

    // Verify output is valid YAML
    let output = yaml.to_string();
    let reparsed = YamlFile::from_str(&output).expect("Output should be valid YAML");
    assert!(reparsed.document().is_some());
}

#[test]
fn test_nested_mapping_mutation() {
    let yaml_str = r#"database:
  name: dev_db
  user: admin"#;

    let yaml = YamlFile::from_str(yaml_str).unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mapping) = doc.as_mapping() {
            mapping.modify_mapping("database", |db| {
                db.set("name", "prod_db");
            });
        }
    }

    let expected = r#"database:
  name: prod_db
  user: admin"#;
    assert_eq!(yaml.to_string(), expected);

    // Verify output is valid YAML
    let output = yaml.to_string();
    let reparsed = YamlFile::from_str(&output).expect("Output should be valid YAML");
    assert!(reparsed.document().is_some());
}

#[test]
fn test_nested_mapping_add_new_key() {
    let yaml_str = r#"database:
  name: dev_db
  user: admin"#;

    let yaml = YamlFile::from_str(yaml_str).unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mapping) = doc.as_mapping() {
            mapping.modify_mapping("database", |db| {
                db.set("password", "secret123");
            });
        }
    }

    let expected = r#"database:
  name: dev_db
  user: admin
  password: secret123
"#;
    assert_eq!(yaml.to_string(), expected);

    // Verify output is valid YAML
    let output = yaml.to_string();
    let reparsed = YamlFile::from_str(&output).expect("Output should be valid YAML");
    assert!(reparsed.document().is_some());
}

#[test]
fn test_multiple_root_level_changes() {
    let yaml_str = r#"host: localhost
port: 8080
debug: true"#;

    let yaml = YamlFile::from_str(yaml_str).unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mapping) = doc.as_mapping() {
            mapping.set("host", "0.0.0.0");
            mapping.set("port", 3000);
            mapping.set("debug", false);
        }
    }

    let expected = r#"host: 0.0.0.0
port: 3000
debug: false"#;
    assert_eq!(yaml.to_string(), expected);

    // Verify output is valid YAML
    let output = yaml.to_string();
    let reparsed = YamlFile::from_str(&output).expect("Output should be valid YAML");
    assert!(reparsed.document().is_some());
}

#[test]
fn test_add_new_root_key() {
    let yaml = YamlFile::from_str("existing: value").unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mapping) = doc.as_mapping() {
            mapping.set("new_key", "new_value");
        }
    }

    let expected = r#"existing: value
new_key: new_value
"#;
    assert_eq!(yaml.to_string(), expected);

    // Verify output is valid YAML
    let output = yaml.to_string();
    let reparsed = YamlFile::from_str(&output).expect("Output should be valid YAML");
    assert!(reparsed.document().is_some());
}

#[test]
fn test_nested_with_comments_and_structure() {
    let yaml_str = r#"# Config
server:
  host: localhost  # Default host
  port: 8080"#;

    let yaml = YamlFile::from_str(yaml_str).unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mapping) = doc.as_mapping() {
            mapping.modify_mapping("server", |server| {
                server.set("host", "0.0.0.0");
                server.set("timeout", 30);
            });
        }
    }

    let expected = r#"# Config
server:
  host: 0.0.0.0  # Default host
  port: 8080
  timeout: 30
"#;
    assert_eq!(yaml.to_string(), expected);

    // Verify output is valid YAML
    let output = yaml.to_string();
    let reparsed = YamlFile::from_str(&output).expect("Output should be valid YAML");
    assert!(reparsed.document().is_some());
}

// Tests from nested_mutation_test.rs

#[test]
fn test_nested_mapping_mutations_propagate() {
    let original = r#"database:
  name: dev_db
  user: admin
  max_connections: 10"#;

    let yaml = YamlFile::from_str(original).unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mapping) = doc.as_mapping() {
            // Get the nested database mapping and modify it
            if let Some(db) = mapping.get_mapping("database") {
                db.set("name", "prod_db");
                db.set("password", "secret123");
                db.set("max_connections", 50);
            }
        }
    }

    let expected =
        "database:\n  name: prod_db\n  user: admin\n  max_connections: 50\n  password: secret123\n";

    assert_eq!(yaml.to_string(), expected);

    // Verify output is valid YAML
    let output = yaml.to_string();
    let reparsed = YamlFile::from_str(&output).expect("Output should be valid YAML");
    assert!(reparsed.document().is_some());
}

#[test]
fn test_deeply_nested_mutations() {
    let original = r#"server:
  database:
    primary:
      host: localhost
      port: 5432"#;

    let yaml = YamlFile::from_str(original).unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(root) = doc.as_mapping() {
            if let Some(server) = root.get_mapping("server") {
                if let Some(db) = server.get_mapping("database") {
                    if let Some(primary) = db.get_mapping("primary") {
                        primary.set("host", "prod.example.com");
                        primary.set("ssl", true);
                    }
                }
            }
        }
    }

    let expected = "server:\n  database:\n    primary:\n      host: prod.example.com\n      port: 5432\n      ssl: true\n";

    assert_eq!(yaml.to_string(), expected);

    // Verify output is valid YAML
    let output = yaml.to_string();
    let reparsed = YamlFile::from_str(&output).expect("Output should be valid YAML");
    assert!(reparsed.document().is_some());
}
