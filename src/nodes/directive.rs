use super::{Lang, SyntaxNode};
use crate::lex::SyntaxKind;
use rowan::ast::AstNode;
use rowan::GreenNodeBuilder;

ast_node!(Directive, DIRECTIVE, "A YAML directive like %YAML 1.2");

impl Directive {
    /// Get the full directive text (e.g., "%YAML 1.2")
    pub fn text(&self) -> String {
        self.0.text().to_string()
    }

    /// Get the directive name (e.g., "YAML" from "%YAML 1.2")
    pub fn name(&self) -> Option<String> {
        let text = self.text();
        if let Some(directive_content) = text.strip_prefix('%') {
            directive_content
                .split_whitespace()
                .next()
                .map(|s| s.to_string())
        } else {
            None
        }
    }

    /// Get the directive value (e.g., "1.2" from "%YAML 1.2")
    pub fn value(&self) -> Option<String> {
        let text = self.text();
        if let Some(directive_content) = text.strip_prefix('%') {
            let parts: Vec<&str> = directive_content.split_whitespace().collect();
            if parts.len() > 1 {
                Some(parts[1..].join(" "))
            } else {
                None
            }
        } else {
            None
        }
    }

    /// Check if this is a YAML version directive
    pub fn is_yaml_version(&self) -> bool {
        self.name().as_deref() == Some("YAML")
    }

    /// Check if this is a TAG directive
    pub fn is_tag(&self) -> bool {
        self.name().as_deref() == Some("TAG")
    }

    /// Create a new YAML version directive
    pub fn new_yaml_version(version: &str) -> Self {
        let directive_text = format!("%YAML {}", version);
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(SyntaxKind::DIRECTIVE.into());
        builder.token(SyntaxKind::DIRECTIVE.into(), &directive_text);
        builder.finish_node();
        Directive(SyntaxNode::new_root_mut(builder.finish()))
    }

    /// Create a new TAG directive
    pub fn new_tag(handle: &str, prefix: &str) -> Self {
        let directive_text = format!("%TAG {} {}", handle, prefix);
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(SyntaxKind::DIRECTIVE.into());
        builder.token(SyntaxKind::DIRECTIVE.into(), &directive_text);
        builder.finish_node();
        Directive(SyntaxNode::new_root_mut(builder.finish()))
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    use crate::nodes::Document;
    use crate::yaml::YamlFile;
    use std::str::FromStr;

    #[test]
    fn test_parse_yaml_version_directive() {
        let yaml = "%YAML 1.2\n---\nkey: value";
        let parsed = YamlFile::from_str(yaml).unwrap();

        let directives: Vec<_> = parsed.directives().collect();
        assert_eq!(directives.len(), 1);

        let directive = &directives[0];
        assert!(directive.is_yaml_version());
        assert_eq!(directive.name(), Some("YAML".to_string()));
        assert_eq!(directive.value(), Some("1.2".to_string()));
        assert_eq!(directive.text(), "%YAML 1.2");
    }
    #[test]
    fn test_parse_tag_directive() {
        let yaml = "%TAG ! tag:example.com,2000:app/\n---\nkey: value";
        let parsed = YamlFile::from_str(yaml).unwrap();

        let directives: Vec<_> = parsed.directives().collect();
        assert_eq!(directives.len(), 1);

        let directive = &directives[0];
        assert!(directive.is_tag());
        assert_eq!(directive.name(), Some("TAG".to_string()));
        assert_eq!(
            directive.value(),
            Some("! tag:example.com,2000:app/".to_string())
        );
    }
    #[test]
    fn test_multiple_directives() {
        let yaml = "%YAML 1.2\n%TAG ! tag:example.com,2000:app/\n---\nkey: value";
        let parsed = YamlFile::from_str(yaml).unwrap();

        let directives: Vec<_> = parsed.directives().collect();
        assert_eq!(directives.len(), 2);

        assert!(directives[0].is_yaml_version());
        assert!(directives[1].is_tag());
    }
    #[test]
    fn test_create_yaml_version_directive() {
        let directive = Directive::new_yaml_version("1.2");
        assert!(directive.is_yaml_version());
        assert_eq!(directive.value(), Some("1.2".to_string()));
        assert_eq!(directive.text(), "%YAML 1.2");
    }
    #[test]
    fn test_create_tag_directive() {
        let directive = Directive::new_tag("!", "tag:example.com,2000:app/");
        assert!(directive.is_tag());
        assert_eq!(directive.text(), "%TAG ! tag:example.com,2000:app/");
    }
    #[test]
    fn test_add_directive_to_yaml() {
        let yaml = YamlFile::new();
        yaml.add_directive("%YAML 1.2");

        let directives: Vec<_> = yaml.directives().collect();
        assert_eq!(directives.len(), 1);
        assert_eq!(yaml.to_string(), "%YAML 1.2");
    }
    #[test]
    fn test_yaml_with_directive_and_content() {
        let yaml = YamlFile::new();
        yaml.add_directive("%YAML 1.2");

        let doc = Document::new_mapping();
        doc.set("name", "test");
        yaml.push_document(doc);

        let output = yaml.to_string();
        assert_eq!(output, "%YAML 1.2name: test\n");

        // Should have both directive and document
        let directives: Vec<_> = yaml.directives().collect();
        let documents: Vec<_> = yaml.documents().collect();
        assert_eq!(directives.len(), 1);
        assert_eq!(documents.len(), 1);
    }
    #[test]
    fn test_directive_preservation_in_parsing() {
        let input = "%YAML 1.2\n%TAG ! tag:example.com,2000:app/\n---\nkey: value\n";
        let parsed = YamlFile::from_str(input).unwrap();
        let output = parsed.to_string();

        // Check that directives are preserved
        assert_eq!(output, input);
    }
}
