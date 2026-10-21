//! Comment trivia.

use super::{fresh_token, SyntaxNode, SyntaxToken};
use crate::lex::SyntaxKind;

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

    crate::nodes::ast_node_spans!("comment");

    /// Get the text range of this comment as a rowan [`TextRange`](rowan::TextRange).
    ///
    /// The range spans the full comment text, including the leading `#`.
    pub fn text_range(&self) -> rowan::TextRange {
        self.0.text_range()
    }

    /// Replace the full comment text, including the leading `#`.
    ///
    /// # Errors
    ///
    /// Returns a [`CommentTextError`], leaving the tree unchanged, if `text`
    /// does not start with `#` or contains a line break: either would turn
    /// the token into something other than a single comment.
    ///
    /// Afterwards this handle refers to the new comment token.
    ///
    /// # Example
    /// ```
    /// use yaml_edit::Document;
    /// use std::str::FromStr;
    ///
    /// let doc = Document::from_str("key: value # teh value\n").unwrap();
    /// let mut comment = doc.comments().next().unwrap();
    /// comment.set_text("# the value").unwrap();
    /// assert_eq!(doc.to_string(), "key: value # the value\n");
    /// ```
    pub fn set_text(&mut self, text: &str) -> Result<(), CommentTextError> {
        if !text.starts_with('#') {
            return Err(CommentTextError::MissingHash);
        }
        if text.contains(['\n', '\r']) {
            return Err(CommentTextError::LineBreak);
        }
        let parent = self
            .0
            .parent()
            .expect("a comment token always has a parent node");
        let index = self.0.index();
        let new_token = fresh_token(SyntaxKind::COMMENT, text);
        super::replace_children(&parent, index..index + 1, vec![new_token.into()]);
        self.0 = parent
            .children_with_tokens()
            .nth(index)
            .and_then(|element| element.into_token())
            .expect("the replacement comment sits at the old comment's index");
        Ok(())
    }

    /// Replace the comment content, preserving the leading `# ` marker.
    ///
    /// This is the counterpart to [`content`](Self::content): it sets the text
    /// after the `#` and a single space. Use [`set_text`](Self::set_text) when
    /// you need full control over the marker and spacing.
    ///
    /// # Errors
    ///
    /// Returns [`CommentTextError::LineBreak`] if `content` contains a line
    /// break.
    ///
    /// # Example
    /// ```
    /// use yaml_edit::Document;
    /// use std::str::FromStr;
    ///
    /// let doc = Document::from_str("key: value # teh value\n").unwrap();
    /// let mut comment = doc.comments().next().unwrap();
    /// comment.set_content("the value").unwrap();
    /// assert_eq!(doc.to_string(), "key: value # the value\n");
    /// ```
    pub fn set_content(&mut self, content: &str) -> Result<(), CommentTextError> {
        self.set_text(&format!("# {content}"))
    }

    /// Access the underlying syntax token.
    pub fn syntax(&self) -> &SyntaxToken {
        &self.0
    }
}

/// Why [`Comment::set_text`] or [`Comment::set_content`] refused new text.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CommentTextError {
    /// The text does not start with `#`.
    MissingHash,
    /// The text contains a line break.
    LineBreak,
}

impl std::fmt::Display for CommentTextError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CommentTextError::MissingHash => write!(f, "comment text must start with '#'"),
            CommentTextError::LineBreak => write!(f, "comment text must not contain a line break"),
        }
    }
}

impl std::error::Error for CommentTextError {}

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
