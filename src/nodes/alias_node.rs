use super::{Lang, SyntaxNode};
use crate::as_yaml::{AsYaml, YamlKind};
use crate::lex::SyntaxKind;
use rowan::ast::AstNode;
use rowan::GreenNodeBuilder;

ast_node!(Alias, ALIAS, "A YAML alias reference (e.g., *anchor_name)");

impl Alias {
    /// Construct an alias that serializes as `*name`.
    ///
    /// `name` is the anchor name without the `*` prefix. The node implements
    /// [`AsYaml`], so it can be passed to [`Mapping::set`](crate::Mapping::set)
    /// and other mutation APIs without parsing a snippet.
    ///
    /// # Panics
    ///
    /// Panics if `name` is not a valid anchor name: it must be non-empty and
    /// hold no whitespace, line breaks, flow indicators (`,[]{}`) or
    /// non-printable characters.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::str::FromStr;
    /// use yaml_edit::{Alias, Document};
    ///
    /// let doc = Document::from_str("shared: &shared value\nservice_a: old\n").unwrap();
    /// let mapping = doc.as_mapping().unwrap();
    /// mapping.set("service_a", Alias::new("shared"));
    /// assert_eq!(doc.to_string(), "shared: &shared value\nservice_a: *shared\n");
    /// ```
    pub fn new(name: impl AsRef<str>) -> Self {
        let name = name.as_ref();
        assert!(
            crate::lex::is_valid_anchor_name(name),
            "alias name must be a valid anchor name, got {name:?}"
        );
        let text = format!("*{name}");
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(SyntaxKind::ALIAS.into());
        builder.token(SyntaxKind::REFERENCE.into(), &text);
        builder.finish_node();
        Alias(SyntaxNode::new_root_mut(builder.finish()))
    }

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
        builder.start_node(SyntaxKind::ALIAS.into());
        crate::as_yaml::copy_node_content(builder, &self.0);
        builder.finish_node();
        false
    }

    fn is_inline(&self) -> bool {
        true
    }
}
