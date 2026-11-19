use tower_lsp_server::ls_types::Uri;
use yaml_edit::{Parse, TextPosition, YamlFile};

#[salsa::input]
pub struct SourceFile {
    pub url: Uri,
    pub text: String,
}

/// An anchor definition (`&name`) or alias reference (`*name`).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AnchorEntry {
    /// The anchor name (without `&` or `*` prefix)
    pub name: String,
    /// Whether this is a definition (`&`) or reference (`*`)
    pub is_definition: bool,
    /// Byte range of the full token (including prefix)
    pub range: TextPosition,
}

#[salsa::tracked]
pub fn parse_yaml(db: &dyn salsa::Database, file: SourceFile) -> Parse<YamlFile> {
    let text = file.text(db);
    Parse::parse_yaml(&text)
}

#[salsa::tracked]
pub fn anchors(db: &dyn salsa::Database, file: SourceFile) -> Vec<AnchorEntry> {
    let parsed = parse_yaml(db, file);
    let tree = parsed.tree();
    let syntax: &rowan::SyntaxNode<yaml_edit::Lang> =
        <yaml_edit::YamlFile as rowan::ast::AstNode>::syntax(&tree);

    let mut result = Vec::new();

    for token in syntax.descendants_with_tokens() {
        let token = match token {
            rowan::NodeOrToken::Token(t) => t,
            _ => continue,
        };

        match token.kind() {
            yaml_edit::SyntaxKind::ANCHOR => {
                if let Some(name) = token.text().strip_prefix('&') {
                    let r = token.text_range();
                    result.push(AnchorEntry {
                        name: name.to_string(),
                        is_definition: true,
                        range: TextPosition::new(u32::from(r.start()), u32::from(r.end())),
                    });
                }
            }
            yaml_edit::SyntaxKind::REFERENCE => {
                if let Some(name) = token.text().strip_prefix('*') {
                    let r = token.text_range();
                    result.push(AnchorEntry {
                        name: name.to_string(),
                        is_definition: false,
                        range: TextPosition::new(u32::from(r.start()), u32::from(r.end())),
                    });
                }
            }
            _ => {}
        }
    }

    result
}

#[salsa::db]
#[derive(Clone, Default)]
pub struct Workspace {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for Workspace {}

impl Workspace {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn update_file(&mut self, url: Uri, text: String) -> SourceFile {
        SourceFile::new(self, url, text)
    }

    pub fn get_parsed(&self, file: SourceFile) -> Parse<YamlFile> {
        parse_yaml(self, file)
    }

    pub fn source_text(&self, file: SourceFile) -> String {
        file.text(self).clone()
    }

    pub fn get_anchors(&self, file: SourceFile) -> Vec<AnchorEntry> {
        anchors(self, file)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_valid_yaml() {
        let mut workspace = Workspace::new();
        let url: Uri = "file:///test.yaml".parse().unwrap();
        let content = "name: Alice\nage: 30\n";

        let file = workspace.update_file(url, content.to_string());
        let parsed = workspace.get_parsed(file);

        assert!(!parsed.has_errors());
    }

    #[test]
    fn test_parse_invalid_yaml() {
        let mut workspace = Workspace::new();
        let url: Uri = "file:///test.yaml".parse().unwrap();
        let content = "key: value\n  bad indent: here\n";

        let file = workspace.update_file(url, content.to_string());
        let parsed = workspace.get_parsed(file);

        // The parser may or may not produce errors for this, but it should not panic
        let _ = parsed.has_errors();
    }

    #[test]
    fn test_source_text_retrieval() {
        let mut workspace = Workspace::new();
        let url: Uri = "file:///test.yaml".parse().unwrap();
        let content = "hello: world\n";

        let file = workspace.update_file(url, content.to_string());
        assert_eq!(workspace.source_text(file), content);
    }
}
