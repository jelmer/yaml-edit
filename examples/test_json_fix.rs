use yaml_edit::{Yaml, YamlValue};
use rowan::ast::AstNode;
use std::str::FromStr;

fn main() {
    // Test JSON flow syntax
    let json = r#"{ "name": "John", "age": 30 }"#;
    println!("Testing: {}", json);
    
    let yaml = Yaml::from_str(json).unwrap();
    let doc = yaml.document().unwrap();
    let mapping = doc.as_mapping().unwrap();
    
    // Try to get the name value using unquoted key
    let name_key = YamlValue::from("name");
    if let Some(value_node) = mapping.get(&name_key) {
        println!("✓ Successfully got value for 'name'");
        if let Some(scalar) = value_node.children().find_map(yaml_edit::Scalar::cast) {
            println!("  Value: '{}'", scalar.as_string());
        }
    } else {
        println!("✗ Could not get value for 'name'");
        
        // Debug what keys we have
        println!("Available keys:");
        for key in doc.keys() {
            println!("  - {:?}", key);
        }
    }
    
    // Try age too
    let age_key = YamlValue::from("age");
    if let Some(value_node) = mapping.get(&age_key) {
        println!("✓ Successfully got value for 'age'");
        if let Some(scalar) = value_node.children().find_map(yaml_edit::Scalar::cast) {
            println!("  Value: '{}'", scalar.as_string());
        }
    } else {
        println!("✗ Could not get value for 'age'");
    }
}