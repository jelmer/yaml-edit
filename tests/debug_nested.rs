use rowan::ast::AstNode;
use std::str::FromStr;
use yaml_edit::{SyntaxKind, Yaml};

#[test]
fn test_nested_structure() {
    let yaml_str = r#"database:
  name: dev_db
  user: admin"#;
    let yaml = Yaml::from_str(yaml_str).unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mapping) = doc.as_mapping() {
            // Find the database entry
            for entry in mapping.entries() {
                if let Some(key) = entry.key() {
                    if key.text().to_string().trim() == "database" {
                        println!("Found database entry");
                        if let Some(value) = entry.value() {
                            println!("Database VALUE node children:");
                            for child in value.children_with_tokens() {
                                match child {
                                    rowan::NodeOrToken::Node(n) => {
                                        println!("  Node: kind={:?}", n.kind());
                                        if n.kind() == SyntaxKind::MAPPING {
                                            println!("    Nested MAPPING children:");
                                            for nested_child in n.children_with_tokens() {
                                                match nested_child {
                                                    rowan::NodeOrToken::Node(nn) => {
                                                        println!(
                                                            "      Node: kind={:?}, text='{}'",
                                                            nn.kind(),
                                                            nn.text()
                                                        );
                                                    }
                                                    rowan::NodeOrToken::Token(nt) => {
                                                        println!(
                                                            "      Token: kind={:?}, text='{}'",
                                                            nt.kind(),
                                                            nt.text()
                                                        );
                                                    }
                                                }
                                            }
                                        }
                                    }
                                    rowan::NodeOrToken::Token(t) => {
                                        println!(
                                            "  Token: kind={:?}, text='{}'",
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
