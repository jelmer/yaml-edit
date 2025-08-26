use std::str::FromStr;
use yaml_edit::Yaml;

#[test]
fn test_sequence_push_single() {
    let original = r#"team:
  - Alice
  - Bob"#;

    let mut yaml = Yaml::from_str(original).unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mut mapping) = doc.as_mapping() {
            if let Some(mut team) = mapping.get_sequence("team") {
                team.push("Charlie");
            }
        }
    }

    let expected = r#"team:
  - Alice
  - Bob
  - Charlie"#;
    assert_eq!(yaml.to_string(), expected);
}

#[test]
fn test_sequence_push_multiple() {
    let original = r#"team:
  - Alice
  - Bob"#;

    let mut yaml = Yaml::from_str(original).unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mut mapping) = doc.as_mapping() {
            if let Some(mut team) = mapping.get_sequence("team") {
                team.push("Charlie");
                team.push("Diana");
            }
        }
    }

    let expected = r#"team:
  - Alice
  - Bob
  - Charlie
  - Diana"#;
    assert_eq!(yaml.to_string(), expected);
}

#[test]
fn test_sequence_set_item() {
    let original = r#"team:
  - Alice
  - Bob
  - Charlie"#;

    let mut yaml = Yaml::from_str(original).unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mut mapping) = doc.as_mapping() {
            if let Some(mut team) = mapping.get_sequence("team") {
                team.set_item(1, "Robert");
            }
        }
    }

    let expected = r#"team:
  - Alice
  - Robert
  - Charlie"#;
    assert_eq!(yaml.to_string(), expected);
}

#[test]
fn test_multiple_sequences() {
    let original = r#"team:
  - Alice
  - Bob

scores:
  - 95
  - 87"#;

    let mut yaml = Yaml::from_str(original).unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mut mapping) = doc.as_mapping() {
            if let Some(mut team) = mapping.get_sequence("team") {
                team.push("Charlie");
            }
            if let Some(mut scores) = mapping.get_sequence("scores") {
                scores.push(92);
                scores.set_item(0, 100);
            }
        }
    }

    let expected = r#"team:
  - Alice
  - Bob
  - Charlie

scores:
  - 100
  - 87
  - 92"#;
    assert_eq!(yaml.to_string(), expected);
}

#[test]
fn test_nested_structure_with_sequences() {
    let original = r#"config:
  enabled: true
  retries: 3
  servers:
    - host1
    - host2"#;

    let mut yaml = Yaml::from_str(original).unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mut mapping) = doc.as_mapping() {
            if let Some(mut config) = mapping.get_mapping("config") {
                config.set("enabled", false);
                config.set("retries", 5);

                if let Some(mut servers) = config.get_sequence("servers") {
                    servers.push("host3");
                    servers.set_item(0, "primary-host");
                }
            }
        }
    }

    let expected = r#"config:
  enabled: false
  retries: 5
  servers:
    - primary-host
    - host2
    - host3"#;
    assert_eq!(yaml.to_string(), expected);
}
