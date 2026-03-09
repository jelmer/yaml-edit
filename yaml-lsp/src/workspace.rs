use tower_lsp_server::ls_types::Uri;
use yaml_edit::{Parse, YamlFile};

#[salsa::input]
pub struct SourceFile {
    pub url: Uri,
    pub text: String,
}

#[salsa::tracked]
pub fn parse_yaml(db: &dyn salsa::Database, file: SourceFile) -> Parse<YamlFile> {
    let text = file.text(db);
    Parse::parse_yaml(&text)
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
