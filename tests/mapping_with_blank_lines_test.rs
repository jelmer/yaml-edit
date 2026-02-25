//! Test for correct mapping entry parsing with blank lines between entries.
//!
//! This is a regression test for a parser bug where blank lines between mapping
//! entries would cause subsequent entries to be incorrectly nested under the previous
//! entry instead of being parsed as siblings at the same indentation level.

use yaml_edit::YamlFile;

#[test]
fn test_root_mapping_with_blank_lines() {
    let yaml = r#"
first: value1

second: value2

third: value3
"#;

    let doc = YamlFile::parse(yaml).to_result().unwrap();
    let document = doc.document().unwrap();
    let mapping = document.as_mapping().unwrap();

    // All three keys should be siblings at root level
    assert_eq!(mapping.len(), 3);
    assert_eq!(
        mapping
            .get("first")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "value1"
    );
    assert_eq!(
        mapping
            .get("second")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "value2"
    );
    assert_eq!(
        mapping
            .get("third")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "value3"
    );

    // Verify lossless round-trip
    let output = doc.to_string();
    assert_eq!(output, yaml);
}

#[test]
fn test_nested_mapping_with_blank_lines() {
    let yaml = r#"
config:
  database:
    host: localhost
    port: 5432

  cache:
    host: redis
    port: 6379

  logging:
    level: info
"#;

    let doc = YamlFile::parse(yaml).to_result().unwrap();
    let document = doc.document().unwrap();
    let mapping = document.as_mapping().unwrap();

    // Should have one root key "config"
    assert_eq!(mapping.len(), 1);

    let config = mapping.get_mapping("config").unwrap();

    // Config should have three sibling keys: database, cache, logging
    assert_eq!(config.len(), 3);

    let database = config.get_mapping("database").unwrap();
    assert_eq!(
        database
            .get("host")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "localhost"
    );
    assert_eq!(
        database
            .get("port")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "5432"
    );

    let cache = config.get_mapping("cache").unwrap();
    assert_eq!(
        cache.get("host").unwrap().as_scalar().unwrap().as_string(),
        "redis"
    );
    assert_eq!(
        cache.get("port").unwrap().as_scalar().unwrap().as_string(),
        "6379"
    );

    let logging = config.get_mapping("logging").unwrap();
    assert_eq!(
        logging
            .get("level")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "info"
    );

    // Verify lossless round-trip
    let output = doc.to_string();
    assert_eq!(output, yaml);
}

#[test]
fn test_mapping_with_anchors_and_blank_lines() {
    // This was the original failing case
    let yaml = r#"
defaults: &defaults
  timeout: 30
  retries: 3

production:
  <<: *defaults
  host: prod.example.com

development:
  <<: *defaults
  host: dev.example.com
"#;

    let doc = YamlFile::parse(yaml).to_result().unwrap();
    let document = doc.document().unwrap();
    let mapping = document.as_mapping().unwrap();

    // Should have three root-level keys
    assert_eq!(mapping.len(), 3);

    let defaults = mapping.get_mapping("defaults").unwrap();
    assert_eq!(
        defaults
            .get("timeout")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "30"
    );

    let production = mapping.get_mapping("production").unwrap();
    assert_eq!(
        production
            .get("host")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "prod.example.com"
    );

    let development = mapping.get_mapping("development").unwrap();
    assert_eq!(
        development
            .get("host")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "dev.example.com"
    );

    // Verify lossless round-trip (preserves anchors and aliases)
    let output = doc.to_string();
    assert_eq!(output, yaml);
}

#[test]
fn test_anchor_with_no_value_followed_by_alias() {
    // Test the anchor parsing fix
    let yaml = r#"
anchor: &test
ref: *test
another: value
"#;

    let doc = YamlFile::parse(yaml).to_result().unwrap();
    let document = doc.document().unwrap();
    let mapping = document.as_mapping().unwrap();

    // Should have all three keys
    assert_eq!(mapping.len(), 3);

    // Anchor value should be an empty/null scalar
    assert!(mapping.get("anchor").unwrap().as_scalar().is_some());

    // Ref should be an alias
    let ref_val = mapping.get("ref").unwrap();
    assert!(ref_val.is_alias());
    assert_eq!(ref_val.as_alias().unwrap().name(), "test");

    assert_eq!(
        mapping
            .get("another")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "value"
    );

    // Verify lossless round-trip
    let output = doc.to_string();
    assert_eq!(output, yaml);
}
