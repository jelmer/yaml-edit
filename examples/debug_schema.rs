use yaml_edit::{Yaml, CustomSchema, SchemaValidator, ScalarType, ScalarValue, Scalar};
use rowan::ast::AstNode;
use std::str::FromStr;

fn create_test_document(yaml_str: &str) -> yaml_edit::Document {
    let parsed = Yaml::from_str(yaml_str).unwrap();
    parsed.document().unwrap()
}

fn main() {
    let custom_schema = CustomSchema::new("test")
        .allow_types(&[ScalarType::String, ScalarType::Integer])
        .strict(); // No coercion
        
    let validator = SchemaValidator::custom(custom_schema);
    
    // Test the problematic case
    let invalid_yaml = r#"
name: hello world
enabled: true
"#;
    
    println!("Testing YAML:");
    println!("{}", invalid_yaml);
    
    let invalid_doc = create_test_document(invalid_yaml);
    if let Some(mapping) = invalid_doc.as_mapping() {
        for (key, value) in mapping.pairs() {
            if let (Some(key_node), Some(value_node)) = (key, value) {
                for key_child in key_node.children() {
                    if let Some(key_scalar) = Scalar::cast(key_child.clone()) {
                        println!("Key: '{}', raw text: '{}'", key_scalar.as_string(), key_scalar.to_string());
                    }
                }
                for value_child in value_node.children() {
                    if let Some(value_scalar) = Scalar::cast(value_child.clone()) {
                        let content = value_scalar.as_string();
                        let scalar_value = ScalarValue::from_yaml(content.trim());
                        let scalar_type = scalar_value.scalar_type();
                        println!("Value: '{}', raw text: '{}', detected type: {:?}", 
                                content, value_scalar.to_string(), scalar_type);
                    }
                }
            }
        }
    }
    
    let result = validator.validate(&invalid_doc);
    println!("\nValidation result: {:?}", result);
    if let Err(errors) = result {
        for error in errors {
            println!("Error: {}", error.message());
        }
    } else {
        println!("Validation unexpectedly passed!");
    }
}