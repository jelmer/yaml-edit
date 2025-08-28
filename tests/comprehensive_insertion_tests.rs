use std::collections::BTreeSet;
use std::str::FromStr;
use yaml_edit::{Yaml, YamlValue};

#[test]
fn test_index_based_insertion_new_keys() {
    let mut yaml = Yaml::from_str("first: 1\nthird: 3").unwrap();

    // Insert at index 1 (between first and third)
    yaml.insert_at_index(1, "second", "2");

    let output = yaml.to_string();
    let expected = "first: 1\nsecond: '2'\nthird: 3";
    assert_eq!(output.trim(), expected);

    // Insert at index 0 (beginning)
    yaml.insert_at_index(0, "zero", "0");
    let output = yaml.to_string();
    let lines: Vec<&str> = output.trim().lines().collect();
    assert!(
        lines[0].starts_with("zero:"),
        "Expected zero at index 0, but lines are: {:?}",
        lines
    );
}

#[test]
fn test_index_based_insertion_preserves_complex_values() {
    let yaml_str = r#"
config:
  host: localhost
  port: 8080
data: [1, 2, 3]
"#
    .trim();

    let mut yaml = Yaml::from_str(yaml_str).unwrap();

    // Insert between config and data
    yaml.insert_at_index(1, "middleware", "auth");

    let output = yaml.to_string();
    assert!(output.contains("config:"));
    assert!(output.contains("middleware: auth"));
    assert!(output.contains("data: [1, 2, 3]"));

    // Check order
    let config_pos = output.find("config:").unwrap();
    let middleware_pos = output.find("middleware:").unwrap();
    let data_pos = output.find("data:").unwrap();

    assert!(config_pos < middleware_pos);
    assert!(middleware_pos < data_pos);
}

#[test]
fn test_document_complex_insertion_sets() {
    let mut doc = yaml_edit::Document::new();
    doc.set_string("name", "project");

    let mut tags = BTreeSet::new();
    tags.insert("production".to_string());
    tags.insert("database".to_string());
    let tag_set = YamlValue::from_set(tags);

    doc.insert_at_index(1, "tags", tag_set);

    let output = doc.to_yaml_string();
    assert!(output.contains("name: project"));
    assert!(output.contains("tags: !!set"));
    assert!(output.contains("database: null"));
    assert!(output.contains("production: null"));

    // Verify structure
    let name_pos = output.find("name: project").unwrap();
    let tags_pos = output.find("tags: !!set").unwrap();
    assert!(name_pos < tags_pos, "name should come before tags");
}

#[test]
fn test_document_complex_insertion_mappings() {
    let mut doc = yaml_edit::Document::new();
    doc.set_string("name", "project");

    let mut db_map = std::collections::BTreeMap::new();
    db_map.insert(
        "host".to_string(),
        YamlValue::Scalar(yaml_edit::ScalarValue::new("localhost")),
    );
    db_map.insert(
        "port".to_string(),
        YamlValue::Scalar(yaml_edit::ScalarValue::new("5432")),
    );
    let db_mapping = YamlValue::Mapping(db_map);

    doc.insert_at_index(1, "database", db_mapping);

    let output = doc.to_yaml_string();
    assert!(output.contains("name: project"));
    assert!(output.contains("database:"));
    assert!(output.contains("host: localhost"));
    assert!(output.contains("port: '5432'"));
}

#[test]
fn test_document_complex_insertion_sequences() {
    let mut doc = yaml_edit::Document::new();
    doc.set_string("name", "project");

    let features = vec![
        YamlValue::Scalar(yaml_edit::ScalarValue::new("auth")),
        YamlValue::Scalar(yaml_edit::ScalarValue::new("api")),
        YamlValue::Scalar(yaml_edit::ScalarValue::new("web")),
    ];
    let feature_seq = YamlValue::Sequence(features);

    doc.insert_at_index(1, "features", feature_seq);

    let output = doc.to_yaml_string();
    assert!(output.contains("name: project"));
    assert!(output.contains("features:"));
    assert!(output.contains("- auth"));
    assert!(output.contains("- api"));
    assert!(output.contains("- web"));
}

#[test]
fn test_nested_mapping_mutation_propagation() {
    let yaml_str = r#"
server:
  host: localhost
  port: 8080
  config:
    debug: true
    timeout: 30
database:
  host: db.example.com
  port: 5432
"#
    .trim();

    let mut yaml = Yaml::from_str(yaml_str).unwrap();

    // Test nested mutations propagate to root
    if let Some(doc) = yaml.document() {
        if let Some(mapping) = doc.as_mapping() {
            // Modify deeply nested value
            if let Some(mut server) = mapping.get_mapping(&YamlValue::scalar("server")) {
                if let Some(mut config) = server.get_mapping(&YamlValue::scalar("config")) {
                    config.set(&YamlValue::scalar("debug"), &YamlValue::scalar("false"));
                    config.set(&YamlValue::scalar("timeout"), &YamlValue::scalar("60"));
                    config.set(&YamlValue::scalar("new_option"), &YamlValue::scalar("enabled"));
                }
            }

            // Modify another branch
            if let Some(mut database) = mapping.get_mapping(&YamlValue::scalar("database")) {
                database.set(&YamlValue::scalar("host"), &YamlValue::scalar("prod-db.example.com"));
                database.set(&YamlValue::scalar("ssl"), &YamlValue::scalar("true"));
            }
        }
    }

    // Verify all changes are visible in final output
    let output = yaml.to_string();
    assert!(output.contains("debug: false"));
    assert!(output.contains("timeout: 60"));
    assert!(output.contains("new_option: enabled"));
    assert!(output.contains("host: prod-db.example.com"));
    assert!(output.contains("ssl: true"));

    // Verify structure is maintained
    assert!(output.contains("server:"));
    assert!(output.contains("config:"));
    assert!(output.contains("database:"));
}

#[test]
fn test_whitespace_and_comment_preservation() {
    let yaml_str = r#"# Main config
host: localhost  # Default host
port: 8080       # Default port

# Database settings  
database:
  name: mydb     # Database name
  user: admin    # Admin user
"#;

    let mut yaml = Yaml::from_str(yaml_str).unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mut mapping) = doc.as_mapping() {
            mapping.set(&YamlValue::scalar("host"), &YamlValue::scalar("0.0.0.0"));
            mapping.set(&YamlValue::scalar("environment"), &YamlValue::scalar("production"));

            if let Some(mut db) = mapping.get_mapping(&YamlValue::scalar("database")) {
                db.set(&YamlValue::scalar("name"), &YamlValue::scalar("prod_db"));
                db.set(&YamlValue::scalar("timeout"), &YamlValue::scalar("30"));
            }
        }
    }

    let output = yaml.to_string();

    // Verify comments are preserved
    assert!(output.contains("# Main config"));
    assert!(output.contains("# Default host"));
    assert!(output.contains("# Default port"));
    assert!(output.contains("# Database settings"));
    assert!(output.contains("# Admin user"));

    // Verify values were updated
    assert!(output.contains("host: 0.0.0.0"));
    assert!(output.contains("name: prod_db"));
    assert!(output.contains("timeout: 30"));
    assert!(output.contains("environment: production"));
}

#[test]
fn test_multiple_consecutive_insertions() {
    let mut yaml = Yaml::from_str("").unwrap();

    // Build up a document with multiple insertions
    yaml.insert_at_index(0, "first", "1");
    yaml.insert_at_index(1, "second", "2");
    yaml.insert_at_index(2, "third", "3");
    yaml.insert_at_index(1, "inserted", "middle"); // Insert in middle
    yaml.insert_at_index(0, "zero", "0"); // Insert at beginning

    let output = yaml.to_string();
    let lines: Vec<&str> = output.trim().lines().collect();

    // Verify order
    assert!(lines[0].starts_with("zero:"));
    assert!(lines[1].starts_with("first:"));
    assert!(lines[2].starts_with("inserted:"));
    assert!(lines[3].starts_with("second:"));
    assert!(lines[4].starts_with("third:"));
}

#[test]
fn test_mixed_insertion_and_complex_values() {
    let mut yaml = Yaml::from_str("name: project").unwrap();

    // Insert simple value
    yaml.insert_at_index(1, "version", "1.0.0");

    // Insert complex set
    let mut tags = BTreeSet::new();
    tags.insert("web".to_string());
    tags.insert("api".to_string());
    let tag_set = YamlValue::from_set(tags);
    yaml.insert_at_index(2, "tags", tag_set);

    // Insert simple value after complex
    yaml.insert_at_index(3, "author", "dev-team");

    let output = yaml.to_string();

    // Verify all elements present and in order
    assert!(output.contains("name: project"));
    assert!(output.contains("version: 1.0.0"));
    assert!(output.contains("tags: !!set"));
    assert!(output.contains("api: null"));
    assert!(output.contains("web: null"));
    assert!(output.contains("author: dev-team"));

    // Verify order by position
    let name_pos = output.find("name: project").unwrap();
    let version_pos = output.find("version:").unwrap();
    let tags_pos = output.find("tags:").unwrap();
    let author_pos = output.find("author:").unwrap();

    assert!(name_pos < version_pos);
    assert!(version_pos < tags_pos);
    assert!(tags_pos < author_pos);
}

#[test]
fn test_edge_case_empty_document_insertion() {
    let mut yaml = Yaml::from_str("").unwrap();

    // Insert into completely empty document
    yaml.insert_at_index(0, "first", "value");

    let output = yaml.to_string();
    assert!(output.contains("first: value"));
}

#[test]
fn test_edge_case_large_index() {
    let mut yaml = Yaml::from_str("a: 1\nb: 2").unwrap();

    // Insert at index larger than current size (should append)
    yaml.insert_at_index(100, "z", "last");

    let output = yaml.to_string();
    assert!(output.contains("a: 1"));
    assert!(output.contains("b: 2"));
    assert!(output.contains("z: last"));

    // z should be at the end
    let z_pos = output.find("z: last").unwrap();
    let a_pos = output.find("a: 1").unwrap();
    let b_pos = output.find("b: 2").unwrap();

    assert!(a_pos < b_pos);
    assert!(b_pos < z_pos);
}
