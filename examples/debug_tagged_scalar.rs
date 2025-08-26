use yaml_edit::{Yaml, TaggedScalar};
use rowan::ast::AstNode;
use std::str::FromStr;

fn main() {
    let yaml_text = r#"
timestamp: !!timestamp "2023-01-01"
pattern: !!regex '\d+'
"#;
    
    println!("Testing YAML:");
    println!("{}", yaml_text);
    
    let parsed = Yaml::from_str(yaml_text).unwrap();
    let doc = parsed.document().unwrap();
    
    if let Some(mapping) = doc.as_mapping() {
        for (key, value) in mapping.pairs() {
            if let (Some(key_node), Some(value_node)) = (key, value) {
                for key_child in key_node.children() {
                    if let Some(key_scalar) = yaml_edit::Scalar::cast(key_child.clone()) {
                        println!("Key: '{}' (kind: {:?})", key_scalar.as_string(), key_child.kind());
                    }
                }
                
                println!("VALUE node kind: {:?}, text: '{}'", value_node.kind(), value_node.text());
                for value_child in value_node.children() {
                    println!("  VALUE child kind: {:?}, text: '{}'", value_child.kind(), value_child.text());
                    
                    if let Some(tagged_scalar) = TaggedScalar::cast(value_child.clone()) {
                        println!("    Found TaggedScalar: '{}'", tagged_scalar.to_string());
                    } else if let Some(scalar) = yaml_edit::Scalar::cast(value_child.clone()) {
                        println!("    Found regular Scalar: '{}'", scalar.as_string());
                    }
                }
            }
        }
    }
}