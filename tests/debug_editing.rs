use rowan::ast::AstNode;
use std::str::FromStr;
use yaml_edit::{Scalar, Yaml, YamlValue};

#[test]
fn test_editing_operations_debug() {
    // Test basic editing operations
    let yaml = Yaml::from_str("name: old-name\nversion: 1.0.0").unwrap();
    println!("Initial YAML:\n{}", yaml.to_string());

    if let Some(mut doc) = yaml.document() {
        println!("\nGot document");

        // Check initial values
        println!(
            "Initial name: {:?}",
            doc.get_string(&YamlValue::scalar("name"))
        );
        println!(
            "Initial version: {:?}",
            doc.get_string(&YamlValue::scalar("version"))
        );

        doc.set(&YamlValue::scalar("name"), &YamlValue::scalar("new-name"));
        println!("Set name to new-name");

        doc.set(&YamlValue::scalar("version"), &YamlValue::scalar("2.0.0"));
        println!("Set version to 2.0.0");

        let output = doc.to_yaml_string();
        println!("\nDocument output after modifications:\n{}", output);

        // Check if as_mapping still works after modification
        println!("\nChecking document structure after modification:");
        if let Some(mapping) = doc.as_mapping() {
            println!("Document still has a mapping");
            println!("Mapping has {} entries", mapping.entries().count());
            for entry in mapping.entries() {
                println!("  Entry found");
                if let Some(key) = entry.key() {
                    println!("    KEY node found, text: '{}'", key.text());
                    // Look for SCALAR child in KEY
                    for child in key.children() {
                        println!("      KEY child: {:?} = '{}'", child.kind(), child.text());
                    }
                } else {
                    println!("    No KEY node found!");
                }
                if let Some(value) = entry.value() {
                    println!("    VALUE node found, text: '{}'", value.text());
                }
            }
        } else {
            println!("Document no longer has a mapping!");
        }

        // Check what kind of node get() returns for name after modification
        let key_val = yaml_edit::YamlValue::from("name");
        let get_result = doc.get(&key_val);
        println!("\nget() after modification: {:?}", get_result.is_some());
        if let Some(node) = &get_result {
            println!("Node kind: {:?}", node.kind());
            println!("Node text: '{}'", node.text());
            println!("Node children:");
            for child in node.children() {
                println!("  Child: {:?} = '{}'", child.kind(), child.text());
                if child.kind() == yaml_edit::SyntaxKind::SCALAR {
                    println!("    Found SCALAR child");
                    if let Some(scalar) = Scalar::cast(child) {
                        println!("    SCALAR as_string: '{}'", scalar.as_string());
                    }
                }
            }
        }

        // Verify values can be retrieved
        println!(
            "Final name: {:?}",
            doc.get_string(&YamlValue::scalar("name"))
        );
        println!(
            "Final version: {:?}",
            doc.get_string(&YamlValue::scalar("version"))
        );

        assert!(output.contains("new-name"));
        assert!(output.contains("2.0.0"));

        assert_eq!(
            doc.get_string(&YamlValue::scalar("name")),
            Some("new-name".to_string())
        );
        assert_eq!(
            doc.get_string(&YamlValue::scalar("version")),
            Some("2.0.0".to_string())
        );
    }
}
