use std::str::FromStr;
use yaml_edit::{Yaml, YamlValue};

#[test]
fn test_nested_mapping_mutations_propagate() {
    let original = r#"database:
  name: dev_db
  user: admin
  max_connections: 10"#;

    let mut yaml = Yaml::from_str(original).unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mut mapping) = doc.as_mapping() {
            // Get the nested database mapping and modify it
            if let Some(mut db) = mapping.get_mapping(&YamlValue::scalar("database")) {
                db.set(&YamlValue::scalar("name"), &YamlValue::scalar("prod_db"));
                db.set(
                    &YamlValue::scalar("password"),
                    &YamlValue::scalar("secret123"),
                );
                db.set(&YamlValue::scalar("max_connections"), &YamlValue::from(50));

                // Changes should propagate automatically through splice_children
                // No manual propagation needed!
            }
        }
    }

    let expected = r#"database:
  name: prod_db
  user: admin
  max_connections: 50
  password: secret123"#;

    assert_eq!(yaml.to_string().trim(), expected);
}

#[test]
fn test_deeply_nested_mutations() {
    let original = r#"server:
  database:
    primary:
      host: localhost
      port: 5432"#;

    let mut yaml = Yaml::from_str(original).unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mut root) = doc.as_mapping() {
            if let Some(mut server) = root.get_mapping(&YamlValue::scalar("server")) {
                if let Some(mut db) = server.get_mapping(&YamlValue::scalar("database")) {
                    if let Some(mut primary) = db.get_mapping(&YamlValue::scalar("primary")) {
                        primary.set(
                            &YamlValue::scalar("host"),
                            &YamlValue::scalar("prod.example.com"),
                        );
                        primary.set(&YamlValue::scalar("ssl"), &YamlValue::from(true));

                        // Changes should propagate automatically through splice_children
                        // No manual propagation needed!
                    }
                }
            }
        }
    }

    let result = yaml.to_string();
    assert!(result.contains("host: prod.example.com"));
    assert!(result.contains("ssl: true"));
}
