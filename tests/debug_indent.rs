use rowan::ast::AstNode;
use std::str::FromStr;
use yaml_edit::{SyntaxKind, Yaml};

#[test]
fn test_nested_indentation_detect() {
    let yaml_str = r#"database:
  name: dev_db
  user: admin"#;
    let mut yaml = Yaml::from_str(yaml_str).unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mapping) = doc.as_mapping() {
            // Find the database mapping
            for entry in mapping.entries() {
                if let Some(key) = entry.key() {
                    if key.text().to_string().trim() == "database" {
                        println!("Found database entry");
                        if let Some(value) = entry.value() {
                            // Look for the mapping inside VALUE
                            for child in value.children() {
                                if child.kind() == SyntaxKind::MAPPING {
                                    let nested_mapping = yaml_edit::Mapping::cast(child).unwrap();
                                    let indent = nested_mapping.detect_indentation_level();
                                    println!("Detected indentation level: {}", indent);

                                    // Print all children to see structure
                                    println!("\nNested mapping children:");
                                    for mc in nested_mapping.syntax().children_with_tokens() {
                                        match mc {
                                            rowan::NodeOrToken::Node(n) => {
                                                println!("  Node: {:?}", n.kind());
                                            }
                                            rowan::NodeOrToken::Token(t) => {
                                                println!(
                                                    "  Token: {:?} = '{}'",
                                                    t.kind(),
                                                    t.text()
                                                );
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
