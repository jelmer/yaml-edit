//! Comment trivia.

use super::{SyntaxNode, SyntaxToken};
use crate::lex::SyntaxKind;
use rowan::GreenNodeBuilder;

/// A single comment token in a YAML document.
///
/// Comments are trivia: they are preserved in the syntax tree but do not affect
/// the parsed value. Use [`YamlFile::comments`](crate::YamlFile::comments) or
/// [`Document::comments`](crate::Document::comments) to iterate over them.
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Comment(pub(crate) SyntaxToken);

impl Comment {
    /// Wrap a syntax token as a comment, if it is a `COMMENT` token.
    pub(crate) fn cast(token: SyntaxToken) -> Option<Self> {
        (token.kind() == SyntaxKind::COMMENT).then_some(Comment(token))
    }

    /// The full comment text, including the leading `#`.
    ///
    /// For `# hello` this returns `"# hello"`.
    pub fn text(&self) -> &str {
        self.0.text()
    }

    /// The comment content with the leading `#` and a single optional space removed.
    ///
    /// For `# hello` this returns `"hello"`; for `#hello` it returns `"hello"`.
    /// Trailing whitespace is left intact.
    pub fn content(&self) -> &str {
        let text = self.0.text();
        let without_hash = text.strip_prefix('#').unwrap_or(text);
        without_hash.strip_prefix(' ').unwrap_or(without_hash)
    }

    /// Get the byte offset range of this comment in the source text.
    pub fn byte_range(&self) -> crate::TextPosition {
        self.0.text_range().into()
    }

    /// Get the text range of this comment as a rowan [`TextRange`](rowan::TextRange).
    ///
    /// The range spans the full comment text, including the leading `#`.
    pub fn text_range(&self) -> rowan::TextRange {
        self.0.text_range()
    }

    /// Replace the full comment text, including the leading `#`.
    ///
    /// The replacement must be a single valid comment token starting with `#`
    /// and containing no newline; otherwise this panics, since writing it back
    /// would corrupt the tree.
    ///
    /// # Example
    /// ```
    /// use yaml_edit::Document;
    /// use std::str::FromStr;
    ///
    /// let doc = Document::from_str("key: value # teh value\n").unwrap();
    /// let comment = doc.comments().next().unwrap();
    /// comment.set_text("# the value");
    /// assert_eq!(doc.to_string(), "key: value # the value\n");
    /// ```
    pub fn set_text(&self, text: &str) {
        assert!(
            text.starts_with('#'),
            "comment text must start with '#', got {text:?}"
        );
        assert!(
            !text.contains('\n') && !text.contains('\r'),
            "comment text must not contain a newline, got {text:?}"
        );
        let parent = self
            .0
            .parent()
            .expect("a comment token always has a parent node");
        let index = self.0.index();
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(SyntaxKind::ROOT.into());
        builder.token(SyntaxKind::COMMENT.into(), text);
        builder.finish_node();
        let temp = SyntaxNode::new_root_mut(builder.finish());
        let new_token = temp
            .first_token()
            .expect("builder always emits a COMMENT token");
        parent.splice_children(index..index + 1, vec![new_token.into()]);
    }

    /// Replace the comment content, preserving the leading `# ` marker.
    ///
    /// This is the counterpart to [`content`](Self::content): it sets the text
    /// after the `#` and a single space. Use [`set_text`](Self::set_text) when
    /// you need full control over the marker and spacing.
    ///
    /// # Example
    /// ```
    /// use yaml_edit::Document;
    /// use std::str::FromStr;
    ///
    /// let doc = Document::from_str("key: value # teh value\n").unwrap();
    /// let comment = doc.comments().next().unwrap();
    /// comment.set_content("the value");
    /// assert_eq!(doc.to_string(), "key: value # the value\n");
    /// ```
    pub fn set_content(&self, content: &str) {
        self.set_text(&format!("# {content}"));
    }

    /// Get the line and column where this comment starts.
    ///
    /// Line and column numbers are 1-indexed.
    ///
    /// # Arguments
    ///
    /// * `source_text` - The original YAML source text
    pub fn start_position(&self, source_text: &str) -> crate::LineColumn {
        let range = self.byte_range();
        crate::byte_offset_to_line_column(source_text, range.start as usize)
    }

    /// Get the line and column where this comment ends.
    ///
    /// Line and column numbers are 1-indexed.
    ///
    /// # Arguments
    ///
    /// * `source_text` - The original YAML source text
    pub fn end_position(&self, source_text: &str) -> crate::LineColumn {
        let range = self.byte_range();
        crate::byte_offset_to_line_column(source_text, range.end as usize)
    }

    /// Access the underlying syntax token.
    pub fn syntax(&self) -> &SyntaxToken {
        &self.0
    }
}

impl std::fmt::Debug for Comment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Comment")
            .field("text", &self.text())
            .finish()
    }
}

impl std::fmt::Display for Comment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.text())
    }
}

/// Iterate over every comment in `node` and its descendants, in source order.
pub(crate) fn comments(node: &SyntaxNode) -> impl Iterator<Item = Comment> {
    node.descendants_with_tokens()
        .filter_map(|element| element.into_token())
        .filter_map(Comment::cast)
}

#[cfg(test)]
mod tests {
    use crate::YamlFile;
    use std::str::FromStr;

    #[test]
    fn iterates_comments_in_order() {
        let text = "# header\nkey: value # trailing\n# footer\n";
        let file = YamlFile::from_str(text).unwrap();
        let texts: Vec<_> = file.comments().map(|c| c.text().to_string()).collect();
        assert_eq!(texts, vec!["# header", "# trailing", "# footer"]);
    }

    #[test]
    fn content_strips_hash_and_single_space() {
        let file = YamlFile::from_str("#nospace\n# one space\n#  two spaces\n").unwrap();
        let contents: Vec<_> = file.comments().map(|c| c.content().to_string()).collect();
        assert_eq!(contents, vec!["nospace", "one space", " two spaces"]);
    }

    #[test]
    fn no_comments_yields_empty() {
        let file = YamlFile::from_str("key: value\n").unwrap();
        assert_eq!(file.comments().count(), 0);
    }

    #[test]
    fn positions_are_one_indexed() {
        let text = "key: value # trailing\n";
        let file = YamlFile::from_str(text).unwrap();
        let comment = file.comments().next().unwrap();
        let start = comment.start_position(text);
        assert_eq!(start.line, 1);
        assert_eq!(start.column, 12);
    }

    #[test]
    fn text_range_covers_full_comment() {
        let text = "key: value # trailing\n";
        let file = YamlFile::from_str(text).unwrap();
        let comment = file.comments().next().unwrap();
        let range = comment.text_range();
        assert_eq!(&text[range], "# trailing");
    }

    #[test]
    fn set_text_replaces_in_place() {
        let file = YamlFile::from_str("key: value # teh value\n").unwrap();
        let comment = file.comments().next().unwrap();
        comment.set_text("# the value");
        assert_eq!(file.to_string(), "key: value # the value\n");
    }

    #[test]
    fn set_content_preserves_marker() {
        let file = YamlFile::from_str("key: value #teh value\n").unwrap();
        let comment = file.comments().next().unwrap();
        comment.set_content("the value");
        assert_eq!(file.to_string(), "key: value # the value\n");
    }

    #[test]
    fn set_text_leaves_other_comments_intact() {
        let file = YamlFile::from_str("# header\nkey: value # teh value\n# footer\n").unwrap();
        let target = file.comments().nth(1).unwrap();
        target.set_text("# the value");
        let texts: Vec<_> = file.comments().map(|c| c.text().to_string()).collect();
        assert_eq!(texts, vec!["# header", "# the value", "# footer"]);
    }

    #[test]
    #[should_panic(expected = "must start with '#'")]
    fn set_text_rejects_missing_hash() {
        let file = YamlFile::from_str("key: value # comment\n").unwrap();
        file.comments().next().unwrap().set_text("no hash");
    }

    #[test]
    #[should_panic(expected = "must not contain a newline")]
    fn set_text_rejects_newline() {
        let file = YamlFile::from_str("key: value # comment\n").unwrap();
        file.comments().next().unwrap().set_text("# a\n# b");
    }
}
