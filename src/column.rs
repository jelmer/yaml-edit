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
