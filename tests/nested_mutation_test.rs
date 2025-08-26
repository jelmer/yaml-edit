use std::str::FromStr;
use yaml_edit::Yaml;

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
            if let Some(mut db) = mapping.get_mapping("database") {
                db.set("name", "prod_db");
                db.set("password", "secret123");
                db.set("max_connections", 50);

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
            if let Some(mut server) = root.get_mapping("server") {
                if let Some(mut db) = server.get_mapping("database") {
                    if let Some(mut primary) = db.get_mapping("primary") {
                        primary.set("host", "prod.example.com");
                        primary.set("ssl", true);

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
