use std::str::FromStr;
use yaml_edit::Yaml;

#[test]
fn test_simple_value_replacement() {
    let mut yaml = Yaml::from_str("key: value").unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mut mapping) = doc.as_mapping() {
            mapping.set("key", "new_value");
        }
    }

    assert_eq!(yaml.to_string(), "key: new_value");
}

#[test]
fn test_value_replacement_preserves_whitespace() {
    let mut yaml = Yaml::from_str("key: value  # comment").unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mut mapping) = doc.as_mapping() {
            mapping.set("key", "new_value");
        }
    }

    assert_eq!(yaml.to_string(), "key: new_value  # comment");
}

#[test]
fn test_boolean_value_replacement() {
    let mut yaml = Yaml::from_str("debug: true  # For now").unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mut mapping) = doc.as_mapping() {
            mapping.set("debug", false);
        }
    }

    println!("Output: '{}'", yaml.to_string());
    assert_eq!(yaml.to_string(), "debug: false  # For now");
}

#[test]
fn test_nested_mapping_mutation() {
    let yaml_str = r#"database:
  name: dev_db
  user: admin"#;

    let mut yaml = Yaml::from_str(yaml_str).unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mut mapping) = doc.as_mapping() {
            mapping.modify_mapping("database", |db| {
                db.set("name", "prod_db");
            });
        }
    }

    let expected = r#"database:
  name: prod_db
  user: admin"#;
    assert_eq!(yaml.to_string(), expected);
}

#[test]
fn test_nested_mapping_add_new_key() {
    let yaml_str = r#"database:
  name: dev_db
  user: admin"#;

    let mut yaml = Yaml::from_str(yaml_str).unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mut mapping) = doc.as_mapping() {
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
}

#[test]
fn test_multiple_root_level_changes() {
    let yaml_str = r#"host: localhost
port: 8080
debug: true"#;

    let mut yaml = Yaml::from_str(yaml_str).unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mut mapping) = doc.as_mapping() {
            mapping.set("host", "0.0.0.0");
            mapping.set("port", 3000);
            mapping.set("debug", false);
        }
    }

    let expected = r#"host: 0.0.0.0
port: 3000
debug: false"#;
    assert_eq!(yaml.to_string(), expected);
}

#[test]
fn test_add_new_root_key() {
    let mut yaml = Yaml::from_str("existing: value").unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mut mapping) = doc.as_mapping() {
            mapping.set("new_key", "new_value");
        }
    }

    let expected = r#"existing: value
new_key: new_value
"#;
    assert_eq!(yaml.to_string(), expected);
}

#[test]
fn test_nested_with_comments_and_structure() {
    let yaml_str = r#"# Config
server:
  host: localhost  # Default host
  port: 8080"#;

    let mut yaml = Yaml::from_str(yaml_str).unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mut mapping) = doc.as_mapping() {
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
}
