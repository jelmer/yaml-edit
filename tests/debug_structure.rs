use yaml_edit::{Yaml, SyntaxKind};
use std::str::FromStr;
use rowan::ast::AstNode;

#[test]
fn test_structure() {
    let yaml = Yaml::from_str("debug: true  # For now").unwrap();
    
    if let Some(doc) = yaml.document() {
        if let Some(mapping) = doc.as_mapping() {
            for entry in mapping.entries() {
                println!("MAPPING_ENTRY children:");
                for child in entry.syntax().children_with_tokens() {
                    match child {
                        rowan::NodeOrToken::Node(n) => {
                            println!("  Node: kind={:?}, text='{}'", n.kind(), n.text());
                            if n.kind() == SyntaxKind::VALUE {
                                println!("    VALUE children:");
                                for vchild in n.children_with_tokens() {
                                    match vchild {
                                        rowan::NodeOrToken::Node(vn) => {
                                            println!("      Node: kind={:?}, text='{}'", vn.kind(), vn.text());
                                        }
                                        rowan::NodeOrToken::Token(vt) => {
                                            println!("      Token: kind={:?}, text='{}'", vt.kind(), vt.text());
                                        }
                                    }
                                }
                            }
                        }
                        rowan::NodeOrToken::Token(t) => {
                            println!("  Token: kind={:?}, text='{}'", t.kind(), t.text());
                        }
                    }
                }
            }
        }
    }
}