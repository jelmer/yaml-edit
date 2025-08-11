use std::str::FromStr;
use yaml_edit::Yaml;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Example demonstrating special collections parsing
    let yaml_content = r#"!!set
item1: null
item2: null
item3: null"#;

    println!("Testing YAML special collections:");
    println!("{}", yaml_content);

    match Yaml::from_str(yaml_content) {
        Ok(yaml) => {
            println!("Successfully parsed YAML");

            if let Some(doc) = yaml.document() {
                println!("Found document");

                if let Some(root) = doc.root_node() {
                    println!("Found root node of kind: {:?}", root.kind());
                } else {
                    println!("No root node found");
                }
            } else {
                println!("No document found");
            }
        }
        Err(e) => {
            println!("Failed to parse YAML: {}", e);
        }
    }

    Ok(())
}
