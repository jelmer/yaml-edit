use std::str::FromStr;
use yaml_edit::Yaml;

fn main() {
    // Test case from the failing test
    let original = r#"name: my-app
description: A test app"#;

    let mut yaml = Yaml::from_str(original).unwrap();

    println!("Original YAML:");
    println!("{}", yaml.to_string());
    println!("---");

    if let Some(doc) = yaml.document() {
        if let Some(mut mapping) = doc.as_mapping() {
            let field_order = &["name", "version", "description"];
            println!(
                "Setting 'version' = '1.0' with field order: {:?}",
                field_order
            );
            mapping.set_with_field_order("version", "1.0", field_order);
        }
    }

    println!("\nResult:");
    println!("{}", yaml.to_string());

    println!("\nExpected:");
    println!("name: my-app");
    println!("version: '1.0'");
    println!("description: A test app");
}
