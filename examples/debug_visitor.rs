use rowan::ast::AstNode;
use std::str::FromStr;
use yaml_edit::visitor::{ScalarCollector, YamlAccept};
use yaml_edit::{Scalar, Yaml};

fn main() {
    let yaml_text = r#"
name: John Doe
age: 30
"#;

    let parsed = Yaml::parse(yaml_text);
    let yaml = parsed.tree();

    println!("YAML structure:");
    if let Some(doc) = yaml.document() {
        if let Some(mapping) = doc.as_mapping() {
            for entry in mapping.entries() {
                if let Some(key) = entry.key() {
                    println!("Key node kind: {:?}, text: '{}'", key.kind(), key.text());
                    for child in key.children() {
                        println!("  Child kind: {:?}, text: '{}'", child.kind(), child.text());
                    }
                }
                if let Some(value) = entry.value() {
                    println!(
                        "Value node kind: {:?}, text: '{}'",
                        value.kind(),
                        value.text()
                    );
                    for child in value.children() {
                        println!("  Child kind: {:?}, text: '{}'", child.kind(), child.text());
                    }
                }
            }
        }
    }

    println!("\n--- Running visitor ---");
    let mut collector = ScalarCollector::new();
    yaml.accept(&mut collector);

    println!("Collected scalars: {:?}", collector.scalars);

    // Expected scalars: "name", "John Doe", "age", "30"
    let expected = ["name", "John Doe", "age", "30"];
    for exp in &expected {
        if collector.scalars.contains(&exp.to_string()) {
            println!("✓ Found: '{}'", exp);
        } else {
            println!("✗ Missing: '{}'", exp);
        }
    }
}
