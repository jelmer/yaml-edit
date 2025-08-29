use std::str::FromStr;
use yaml_edit::Yaml;

#[test]
fn test_set_with_field_order_update_existing_key() {
    let original = r#"name: my-app
version: 1.0
description: A test app"#;

    let mut yaml = Yaml::from_str(original).unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mut mapping) = doc.as_mapping() {
            let field_order = &["name", "version", "description"];
            mapping.set_with_field_order("version", "2.0", field_order);
        }
    }

    let result = yaml.to_string();
    let expected = r#"name: my-app
version: 2.0
description: A test app"#;
    assert_eq!(result, expected);
}

#[test]
fn test_set_with_field_order_insert_new_key_in_order() {
    let original = r#"name: my-app
description: A test app"#;

    let mut yaml = Yaml::from_str(original).unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mut mapping) = doc.as_mapping() {
            let field_order = &["name", "version", "description"];
            mapping.set_with_field_order("version", "1.0", field_order);
        }
    }

    let result = yaml.to_string();
    let expected = r#"name: my-app
version: 1.0
description: A test app"#;
    assert_eq!(result, expected);
}

#[test]
fn test_set_with_field_order_insert_at_beginning() {
    let original = r#"version: 1.0
description: A test app"#;

    let mut yaml = Yaml::from_str(original).unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mut mapping) = doc.as_mapping() {
            let field_order = &["name", "version", "description"];
            mapping.set_with_field_order("name", "my-app", field_order);
        }
    }

    let result = yaml.to_string();
    let expected = r#"name: my-app
version: 1.0
description: A test app"#;
    assert_eq!(result, expected);
}

#[test]
fn test_set_with_field_order_insert_at_end() {
    let original = r#"name: my-app
version: 1.0"#;

    let mut yaml = Yaml::from_str(original).unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mut mapping) = doc.as_mapping() {
            let field_order = &["name", "version", "description"];
            mapping.set_with_field_order("description", "A test app", field_order);
        }
    }

    let result = yaml.to_string();
    let expected = r#"name: my-app
version: 1.0
description: A test app
"#;
    assert_eq!(result, expected);
}

#[test]
fn test_set_with_field_order_key_not_in_order() {
    let original = r#"name: my-app
version: 1.0"#;

    let mut yaml = Yaml::from_str(original).unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mut mapping) = doc.as_mapping() {
            let field_order = &["name", "version", "description"];
            mapping.set_with_field_order("author", "John Doe", field_order);
        }
    }

    let result = yaml.to_string();
    let expected = r#"name: my-app
version: 1.0
author: John Doe
"#;
    assert_eq!(result, expected);
}

#[test]
fn test_set_with_field_order_complex_ordering() {
    let original = r#"version: 1.0
author: John Doe
"#;

    let mut yaml = Yaml::from_str(original).unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mut mapping) = doc.as_mapping() {
            let field_order = &["name", "version", "description", "author"];

            // Add multiple keys in different orders
            mapping.set_with_field_order("description", "A test app", field_order);
            mapping.set_with_field_order("name", "my-app", field_order);
        }
    }

    let result = yaml.to_string();
    let expected = r#"name: my-app
version: 1.0
description: A test app
author: John Doe
"#;
    assert_eq!(result, expected);
}

#[test]
fn test_document_set_with_field_order() {
    let original = r#"version: 1.0
author: John Doe
"#;

    let mut yaml = Yaml::from_str(original).unwrap();

    if let Some(mut doc) = yaml.document() {
        let field_order = &["name", "version", "description", "author"];
        doc.set_with_field_order("name", "my-app", field_order);
    }

    let result = yaml.to_string();
    let expected = r#"name: my-app
version: 1.0
author: John Doe
"#;
    assert_eq!(result, expected);
}

#[test]
fn test_set_with_field_order_empty_document() {
    // Start with an empty mapping document
    let mut yaml = Yaml::from_str("---\n{}").unwrap(); // Empty mapping

    if let Some(mut doc) = yaml.document() {
        let field_order = &["name", "version", "description"];
        doc.set_with_field_order("name", "my-app", field_order);
        doc.set_with_field_order("version", "1.0", field_order);
    }

    let result = yaml.to_string();
    let expected = r#"---
{}
name: my-app
version: 1.0
"#;
    assert_eq!(result, expected);
}
