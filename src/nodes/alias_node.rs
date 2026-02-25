use super::{Lang, SyntaxNode};
use crate::as_yaml::{AsYaml, YamlKind};
use crate::lex::SyntaxKind;
use rowan::ast::AstNode;

ast_node!(Alias, ALIAS, "A YAML alias reference (e.g., *anchor_name)");

impl Alias {
    /// Get the full text of this alias including the `*` prefix
    pub fn value(&self) -> String {
        self.0.text().to_string()
    }

    /// Get the anchor name this alias refers to (without the `*` prefix)
    ///
    /// # Examples
    ///
    /// ```
    /// use yaml_edit::Document;
    /// use std::str::FromStr;
    ///
    /// let yaml = r#"
    /// anchor: &my_anchor
    ///   key: value
    /// reference: *my_anchor
    /// "#;
    ///
    /// let doc = Document::from_str(yaml).unwrap();
    /// let mapping = doc.as_mapping().unwrap();
    /// let reference = mapping.get("reference").unwrap();
    ///
    /// if let yaml_edit::YamlNode::Alias(alias) = reference {
    ///     assert_eq!(alias.name(), "my_anchor");
    /// }
    /// ```
    pub fn name(&self) -> String {
        let text = self.value();
        // Remove the leading '*' character
        if let Some(stripped) = text.strip_prefix('*') {
            stripped.to_string()
        } else {
            text
        }
    }
}

impl AsYaml for Alias {
    fn as_node(&self) -> Option<&SyntaxNode> {
        Some(&self.0)
    }

    fn kind(&self) -> YamlKind {
        YamlKind::Alias
    }

    fn build_content(
        &self,
        builder: &mut rowan::GreenNodeBuilder,
        _indent: usize,
        _flow_context: bool,
    ) -> bool {
        crate::as_yaml::copy_node_content(builder, &self.0);
        // Aliases don't end with newlines
        false
    }

    fn is_inline(&self) -> bool {
        true
    }
}

#[cfg(test)]
mod tests {
    use crate::{Document, YamlNode};
    use std::str::FromStr;

    #[test]
    fn test_simple_alias() {
        let yaml = r#"
anchor: &my_anchor value
reference: *my_anchor
"#;

        let doc = Document::from_str(yaml).unwrap();
        let mapping = doc.as_mapping().unwrap();

        // Get the reference which should be an alias
        let reference = mapping
            .get("reference")
            .expect("Should have 'reference' key");

        match reference {
            YamlNode::Alias(alias) => {
                assert_eq!(alias.name(), "my_anchor");
                assert_eq!(alias.value(), "*my_anchor");
            }
            _ => panic!("Expected alias, got {:?}", reference),
        }
    }

    #[test]
    fn test_alias_in_sequence() {
        let yaml = r#"
colors:
  - &red '#FF0000'
  - &green '#00FF00'
  - &blue '#0000FF'

theme:
  - *red
  - *blue
"#;

        let doc = Document::from_str(yaml).unwrap();
        let mapping = doc.as_mapping().unwrap();

        let theme = mapping.get("theme").expect("Should have 'theme' key");
        let theme_seq = theme.as_sequence().expect("Should be a sequence");
        assert_eq!(theme_seq.len(), 2);

        // First element should be an alias to 'red'
        let first = theme_seq.get(0).unwrap();
        match first {
            YamlNode::Alias(alias) => {
                assert_eq!(alias.name(), "red");
            }
            _ => panic!("Expected alias, got {:?}", first),
        }

        // Second element should be an alias to 'blue'
        let second = theme_seq.get(1).unwrap();
        match second {
            YamlNode::Alias(alias) => {
                assert_eq!(alias.name(), "blue");
            }
            _ => panic!("Expected alias, got {:?}", second),
        }
    }

    #[test]
    fn test_alias_in_mapping() {
        let yaml = r#"
defaults: &defaults
  adapter: postgres
  host: localhost

development:
  database: dev_db
  <<: *defaults
"#;

        let doc = Document::from_str(yaml).unwrap();
        let mapping = doc.as_mapping().unwrap();

        let development = mapping
            .get("development")
            .expect("Should have 'development' key");
        let dev_mapping = development.as_mapping().expect("Should be a mapping");

        // Find the merge key entry by iterating (can't use get() since << is MERGE_KEY token, not STRING)
        let merge_entry = dev_mapping
            .iter()
            .find(|(k, _)| {
                k.as_scalar()
                    .map(|s| s.as_string() == "<<")
                    .unwrap_or(false)
            })
            .expect("Should have merge key entry");

        // The value should be an alias
        match merge_entry.1 {
            YamlNode::Alias(alias) => {
                assert_eq!(alias.name(), "defaults");
                assert_eq!(alias.value(), "*defaults");
            }
            _ => panic!("Expected alias for merge key, got {:?}", merge_entry.1),
        }
    }

    #[test]
    fn test_multiple_aliases_to_same_anchor() {
        let yaml = r#"
value: &shared 42
first: *shared
second: *shared
third: *shared
"#;

        let doc = Document::from_str(yaml).unwrap();
        let mapping = doc.as_mapping().unwrap();

        for key in &["first", "second", "third"] {
            let node = mapping
                .get(key)
                .unwrap_or_else(|| panic!("Should have '{}' key", key));
            match node {
                YamlNode::Alias(alias) => {
                    assert_eq!(alias.name(), "shared");
                }
                _ => panic!("Expected alias for '{}', got {:?}", key, node),
            }
        }
    }

    #[test]
    fn test_alias_round_trip() {
        let yaml = r#"anchor: &test value
reference: *test"#;

        let doc = Document::from_str(yaml).unwrap();
        let output = doc.to_string();

        // Should preserve exact formatting
        assert_eq!(output, yaml);
    }

    #[test]
    fn test_alias_with_complex_anchor_name() {
        let yaml = r#"
data: &complex_anchor_name_123 some_value
ref: *complex_anchor_name_123
"#;

        let doc = Document::from_str(yaml).unwrap();
        let mapping = doc.as_mapping().unwrap();

        let ref_node = mapping.get("ref").expect("Should have 'ref' key");
        match ref_node {
            YamlNode::Alias(alias) => {
                assert_eq!(alias.name(), "complex_anchor_name_123");
            }
            _ => panic!("Expected alias, got {:?}", ref_node),
        }
    }

    #[test]
    fn test_nested_structure_with_aliases() {
        let yaml = r#"
base: &base
  x: 1
  y: 2

items:
  - name: first
    config: *base
  - name: second
    config: *base
"#;

        let doc = Document::from_str(yaml).unwrap();
        let mapping = doc.as_mapping().unwrap();

        let items = mapping.get("items").expect("Should have 'items' key");
        let items_seq = items.as_sequence().expect("Should be a sequence");
        assert_eq!(items_seq.len(), 2);

        // Check both items have alias to 'base'
        for i in 0..2 {
            let item = items_seq.get(i).unwrap();
            let item_mapping = item.as_mapping().expect("Should be a mapping");
            let config = item_mapping
                .get("config")
                .expect("Should have 'config' key");

            match config {
                YamlNode::Alias(alias) => {
                    assert_eq!(alias.name(), "base");
                }
                _ => panic!("Expected alias for item {}, got {:?}", i, config),
            }
        }
    }

    #[test]
    fn test_alias_preserves_whitespace() {
        // Test that whitespace around alias is preserved
        let yaml = "reference:  *test  ";

        let doc = Document::from_str(yaml).unwrap();
        let output = doc.to_string();

        // Should preserve the exact whitespace
        assert_eq!(output, yaml);
    }
}
