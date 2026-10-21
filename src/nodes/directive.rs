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
        let directive_text = format!("%YAML {version}");
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(SyntaxKind::DIRECTIVE.into());
        builder.token(SyntaxKind::DIRECTIVE.into(), &directive_text);
        builder.finish_node();
        Directive(SyntaxNode::new_root_mut(builder.finish()))
    }

    /// Create a new TAG directive
    pub fn new_tag(handle: &str, prefix: &str) -> Self {
        let directive_text = format!("%TAG {handle} {prefix}");
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(SyntaxKind::DIRECTIVE.into());
        builder.token(SyntaxKind::DIRECTIVE.into(), &directive_text);
        builder.finish_node();
        Directive(SyntaxNode::new_root_mut(builder.finish()))
    }
}
