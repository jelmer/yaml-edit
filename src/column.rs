//! Columns and indentation of tokens and nodes in a syntax tree.
//!
//! As in [`LineIndex`](crate::line_index::LineIndex), `\n`, `\r` and `\r\n`
//! end a line, columns count Unicode scalar values and a byte order mark
//! takes no column.

use crate::lex::SyntaxKind;
use crate::nodes::{SyntaxNode, SyntaxToken};
use rowan::NodeOrToken;

/// The zero-based column `token` starts at.
pub(crate) fn token_column(token: &SyntaxToken) -> usize {
    line_prefix(token.prev_token()).chars().count()
}

/// The columns of all tokens of a subtree, worked out in one pass.
///
/// [`token_column`] walks back to the start of the line, so asking it for
/// many tokens of one long line costs the square of its length.
pub(crate) struct ColumnIndex {
    /// The start of each token, in order, with its column.
    starts: Vec<(rowan::TextSize, usize)>,
    /// The offset of each `\n` and `\r`, in order.
    line_breaks: Vec<rowan::TextSize>,
}

impl ColumnIndex {
    pub(crate) fn new(node: &SyntaxNode) -> Self {
        let mut column = node.first_token().map_or(0, |t| token_column(&t));
        let mut starts = Vec::new();
        let mut line_breaks = Vec::new();
        for token in node
            .descendants_with_tokens()
            .filter_map(|e| e.into_token())
        {
            let start = token.text_range().start();
            starts.push((start, column));
            line_breaks.extend(
                token
                    .text()
                    .match_indices(['\n', '\r'])
                    .map(|(i, _)| start + rowan::TextSize::try_from(i).expect("offset in a token")),
            );
            column = if token.kind() == SyntaxKind::BOM {
                0
            } else {
                column_after(column, token.text())
            };
        }
        Self {
            starts,
            line_breaks,
        }
    }

    /// Whether the text in `range`, which lies in the indexed subtree,
    /// holds a line break.
    pub(crate) fn has_line_break(&self, range: rowan::TextRange) -> bool {
        let i = self.line_breaks.partition_point(|&b| b < range.start());
        self.line_breaks.get(i).is_some_and(|&b| b < range.end())
    }

    /// The zero-based column `token` starts at, as [`token_column`] gives it.
    pub(crate) fn column(&self, token: &SyntaxToken) -> usize {
        self.column_at(token.text_range().start())
            .unwrap_or_else(|| token_column(token))
    }

    /// The zero-based column of the indexed token starting at `offset`.
    pub(crate) fn column_at(&self, offset: rowan::TextSize) -> Option<usize> {
        // Tokens sharing a start are empty ones before a real one, and so
        // share its column too.
        let i = self
            .starts
            .binary_search_by_key(&offset, |&(s, _)| s)
            .ok()?;
        Some(self.starts[i].1)
    }
}

/// The zero-based column `node` starts at.
///
/// For a MAPPING or SEQUENCE this is the column of its first entry; for a
/// MAPPING_ENTRY it is the column of the key.
pub(crate) fn node_column(node: &SyntaxNode) -> usize {
    line_prefix(token_before(node)).chars().count()
}

/// The indentation of the line `node` starts on.
pub(crate) fn line_indent(node: &SyntaxNode) -> usize {
    space_indent(&line_prefix(token_before(node)))
}

/// The column after writing `text` starting at `column`.
pub(crate) fn column_after(column: usize, text: &str) -> usize {
    match text.rfind(['\n', '\r']) {
        Some(i) => text[i + 1..].chars().count(),
        None => column + text.chars().count(),
    }
}

/// The number of spaces `text` starts with.
///
/// Only spaces indent in YAML (s-indent), so a tab ends the indentation.
pub(crate) fn space_indent(text: &str) -> usize {
    text.len() - text.trim_start_matches(' ').len()
}

/// The text from the start of the line `last` ends on up to the end of
/// `last`.
///
/// Walks back through tokens rather than serializing the tree: this runs
/// for many tokens, and rebuilding the whole text each time made validation
/// quadratic in the file size.
fn line_prefix(last: Option<SyntaxToken>) -> String {
    let mut pieces = Vec::new();
    let mut cursor = last;
    while let Some(token) = cursor {
        if token.kind() == SyntaxKind::BOM {
            break;
        }
        let text = token.text();
        if let Some(i) = text.rfind(['\n', '\r']) {
            pieces.push(text[i + 1..].to_string());
            break;
        }
        pieces.push(text.to_string());
        cursor = token.prev_token();
    }
    pieces.reverse();
    pieces.concat()
}

/// The last token before `node`, which may be empty.
fn token_before(node: &SyntaxNode) -> Option<SyntaxToken> {
    if let Some(first) = node.first_token() {
        return first.prev_token();
    }
    let mut current = node.clone();
    loop {
        let mut cursor = current.prev_sibling_or_token();
        while let Some(element) = cursor {
            match element {
                NodeOrToken::Token(token) => return Some(token),
                NodeOrToken::Node(n) => match n.last_token() {
                    Some(token) => return Some(token),
                    None => cursor = n.prev_sibling_or_token(),
                },
            }
        }
        current = current.parent()?;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::yaml::YamlFile;
    use rowan::ast::AstNode;
    use std::str::FromStr;

    fn columns(text: &str, kind: SyntaxKind) -> Vec<(usize, usize)> {
        let file = YamlFile::from_str(text).unwrap();
        file.syntax()
            .descendants()
            .filter(|n| n.kind() == kind)
            .map(|n| (node_column(&n), line_indent(&n)))
            .collect()
    }

    #[test]
    fn test_node_column_and_line_indent() {
        assert_eq!(
            columns("a:\n  - b: [x, y]\n    c: 1\n", SyntaxKind::MAPPING),
            vec![(0, 0), (4, 2)]
        );
        assert_eq!(
            columns("a:\r  - b: 1\r", SyntaxKind::SEQUENCE),
            vec![(2, 2)]
        );
        assert_eq!(
            columns("\u{FEFF}a: {b: \u{e9}}\n", SyntaxKind::MAPPING),
            vec![(0, 0), (3, 0)]
        );
    }

    #[test]
    fn test_token_column() {
        let file = YamlFile::from_str("- \u{e9}\r\n- x\n").unwrap();
        let cols: Vec<_> = file
            .syntax()
            .descendants_with_tokens()
            .filter_map(|e| e.into_token())
            .filter(|t| !t.text().is_empty())
            .map(|t| (t.text().to_string(), token_column(&t)))
            .collect();
        assert_eq!(
            cols,
            vec![
                ("-".to_string(), 0),
                (" ".to_string(), 1),
                ("\u{e9}".to_string(), 2),
                ("\r\n".to_string(), 3),
                ("-".to_string(), 0),
                (" ".to_string(), 1),
                ("x".to_string(), 2),
                ("\n".to_string(), 3),
            ]
        );
    }

    #[test]
    fn test_column_index() {
        for text in [
            "- \u{e9}\r\n- x\n",
            "\u{FEFF}a: {b: \u{e9}, c: [1,\n  2]}\r  d: \"x\n  y\"\n",
            "--- |\n  text\n...\n\u{FEFF}--- - a\n    - b\n",
            "k:\n    # c\n  - \u{FEFF}x\n",
            // The parser leaves an empty SEQUENCE in the inner `?` key, past
            // which rowan's `prev_token` cannot see, so `token_column`
            // starts the line there.
            "---\n- ? - a\n    - - ? - - b\n            - c\n        : d\n      - e\n  : 23\n",
        ] {
            let file = YamlFile::from_str(text).unwrap();
            let index = ColumnIndex::new(file.syntax());
            let lines = crate::line_index::LineIndex::new(text);
            for token in file
                .syntax()
                .descendants_with_tokens()
                .filter_map(|e| e.into_token())
            {
                assert_eq!(
                    index.column(&token),
                    lines.column(token.text_range().start().into()),
                    "{token:?} in {text:?}"
                );
            }
        }
    }

    #[test]
    fn test_column_after() {
        assert_eq!(column_after(2, "ab"), 4);
        assert_eq!(column_after(2, "\u{e9}"), 3);
        assert_eq!(column_after(2, "a\n  b"), 3);
        assert_eq!(column_after(2, "a\r\n  b"), 3);
        assert_eq!(column_after(2, "a\r  b"), 3);
        assert_eq!(column_after(2, "\r"), 0);
    }

    #[test]
    fn test_space_indent() {
        assert_eq!(space_indent(""), 0);
        assert_eq!(space_indent("  \tx"), 2);
        assert_eq!(space_indent("   "), 3);
    }
}
