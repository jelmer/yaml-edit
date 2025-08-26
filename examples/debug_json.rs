use yaml_edit::{Yaml, Mapping};
use rowan::ast::AstNode;
use std::str::FromStr;

fn main() {
    // Test JSON flow syntax
    let json = r#"{ "name": "John", "age": 30 }"#;
    println!("Parsing: {}", json);
    
    let yaml = Yaml::from_str(json).unwrap();
    let doc = yaml.document().unwrap();
    let mapping = doc.as_mapping().unwrap();
    
    println!("\nMapping entries:");
    for entry in mapping.entries() {
        if let Some(key) = entry.key() {
            println!("  KEY node:");
            println!("    text: '{}'", key.text());
            for child in key.children() {
                println!("    child kind: {:?}, text: '{}'", child.kind(), child.text());
                if child.kind() == yaml_edit::SyntaxKind::SCALAR {
                    if let Some(scalar) = yaml_edit::Scalar::cast(child) {
                        println!("      scalar.value(): '{}'", scalar.value());
                        println!("      scalar.as_string(): '{}'", scalar.as_string());
                    }
                }
            }
        }
    }
    
    // Test regular YAML for comparison
    println!("\n--- Regular YAML ---");
    let yaml2 = Yaml::from_str("name: John\nage: 30").unwrap();
    let doc2 = yaml2.document().unwrap();
    let mapping2 = doc2.as_mapping().unwrap();
    
    println!("\nMapping entries:");
    for entry in mapping2.entries() {
        if let Some(key) = entry.key() {
            println!("  KEY node:");
            println!("    text: '{}'", key.text());
            for child in key.children() {
                println!("    child kind: {:?}, text: '{}'", child.kind(), child.text());
                if child.kind() == yaml_edit::SyntaxKind::SCALAR {
                    if let Some(scalar) = yaml_edit::Scalar::cast(child) {
                        println!("      scalar.value(): '{}'", scalar.value());
                        println!("      scalar.as_string(): '{}'", scalar.as_string());
                    }
                }
            }
        }
    }
}