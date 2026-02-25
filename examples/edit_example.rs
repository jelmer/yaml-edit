use std::str::FromStr;
use yaml_edit::YamlFile;

fn main() {
    let original = r#"# This is my config file
name: old-project  # Project name
version: 1.0.0

# Dependencies section
dependencies:
  - serde  # For serialization
  - tokio  # Async runtime

# Feature flags
features:
  default: []
  full:
    - "serde"
    - "tokio"
"#;

    println!("Original YAML:");
    println!("{}", original);

    // Parse the YAML while preserving formatting
    let yaml = match YamlFile::from_str(original) {
        Ok(yaml) => yaml,
        Err(e) => {
            println!("Parse error: {}", e);
            return;
        }
    };

    println!("Parsed and reproduced:");
    println!("{}", yaml);

    // Edit the document
    if let Some(doc) = yaml.document() {
        doc.set("name", "my-awesome-project");
        doc.set("version", "2.1.0");

        println!("After editing:");
        println!("{}", doc);
    }
}
