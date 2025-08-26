// Test utilities module for internal testing
#[cfg(test)]
pub mod test_utils {
    use crate::{Yaml, SyntaxKind};
    use rowan::NodeOrToken;
    use std::str::FromStr;

    pub fn analyze_structure(yaml_str: &str) {
        let yaml = Yaml::from_str(yaml_str).unwrap();
        
        println!("Analyzing YAML: '{}'", yaml_str);
        if let Some(doc) = yaml.document() {
            if let Some(mapping) = doc.as_mapping() {
                for entry in mapping.entries() {
                    println!("MAPPING_ENTRY children:");
                    for child in entry.syntax().children_with_tokens() {
                        match child {
                            NodeOrToken::Node(n) => {
                                println!("  Node: kind={:?}, text='{}'", n.kind(), n.text());
                            }
                            NodeOrToken::Token(t) => {
                                println!("  Token: kind={:?}, text='{}'", t.kind(), t.text());
                            }
                        }
                    }
                }
            }
        }
    }
}