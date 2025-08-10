use rowan::ast::AstNode;
use std::str::FromStr;
use yaml_edit::{Scalar, Yaml};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let yaml_content = r#"literal: |
  This is a literal scalar
  that preserves line breaks
  and trailing whitespace  

folded: >
  This is a folded scalar
  that should fold lines
  into a single line

plain: |
  line 1
  line 2
  line 3"#;

    println!("Original YAML:");
    println!("{}", yaml_content);

    // Parse the YAML
    let yaml = Yaml::from_str(yaml_content)?;

    println!("\nParsed and formatted YAML:");
    println!("{}", yaml);

    // Check the actual scalar values
    if let Some(doc) = yaml.document() {
        println!("\nActual scalar values:");

        if let Some(literal_value) = doc.get_string("literal") {
            println!("literal value: {:?}", literal_value);
        }
        if let Some(literal_node) = doc.get("literal") {
            if let Some(scalar) = Scalar::cast(literal_node) {
                println!("literal scalar: {:?}", scalar.to_string());
            }
        }

        if let Some(folded_value) = doc.get_string("folded") {
            println!("folded value: {:?}", folded_value);
        }
        if let Some(folded_node) = doc.get("folded") {
            if let Some(scalar) = Scalar::cast(folded_node) {
                println!("folded scalar: {:?}", scalar.to_string());
            }
        }

        if let Some(plain_value) = doc.get_string("plain") {
            println!("plain value: {:?}", plain_value);
        }
        if let Some(plain_node) = doc.get("plain") {
            if let Some(scalar) = Scalar::cast(plain_node) {
                println!("plain scalar: {:?}", scalar.to_string());
            }
        }
    }

    Ok(())
}
