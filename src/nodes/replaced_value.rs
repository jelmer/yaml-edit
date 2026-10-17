//! Trivia around a value that is being replaced in its entry.
//!
//! Comments inside the old value go with it. The parser does not keep all
//! of them inside the value node, though, and it keeps some trivia of the
//! lines after the value inside it, so the replacement has to sort these
//! out first.

use super::{append_children, fresh_token, SyntaxNode, SyntaxToken};
use crate::lex::SyntaxKind;

fn is_block_collection(node: &SyntaxNode) -> bool {
    matches!(node.kind(), SyntaxKind::MAPPING | SyntaxKind::SEQUENCE)
        && node
            .first_token()
            .is_some_and(|t| !matches!(t.kind(), SyntaxKind::LEFT_BRACE | SyntaxKind::LEFT_BRACKET))
}

/// Does `value` reach past the line its entry starts on, or open a block
/// collection there whose entries own whatever follows on that line?
fn owns_line_end(value: &SyntaxNode) -> bool {
    is_block_collection(value)
        || value
            .descendants_with_tokens()
            .any(|c| c.kind() == SyntaxKind::NEWLINE)
}

/// Remove the comment that ends the last line of `value`, `entry`'s value,
/// where the parser keeps it after the value rather than in it, unless it
/// is on the entry's own first line. Its line break becomes `entry`'s.
pub(crate) fn remove_lifted_comment(entry: &SyntaxNode, value: &SyntaxNode) {
    if !owns_line_end(value) {
        return;
    }
    let mut next = value
        .last_token()
        .filter(|t| t.kind() != SyntaxKind::NEWLINE)
        .and_then(|t| t.next_token());
    let mut spaces = Vec::new();
    while let Some(t) = next.clone().filter(|t| t.kind() == SyntaxKind::WHITESPACE) {
        next = t.next_token();
        spaces.push(t);
    }
    let Some(comment) = next.filter(|t| t.kind() == SyntaxKind::COMMENT) else {
        return;
    };
    let line_break = comment
        .next_token()
        .filter(|t| t.kind() == SyntaxKind::NEWLINE);
    for t in spaces {
        t.detach();
    }
    comment.detach();
    if let Some(line_break) = line_break {
        line_break.detach();
        append_children(
            entry,
            vec![fresh_token(SyntaxKind::NEWLINE, line_break.text()).into()],
        );
    }
}

/// Detach and return the blank lines, and the indentation of a comment
/// line, that the parser keeps at the end of the innermost collection
/// `value` ends with. They belong to the lines after the value.
pub(crate) fn take_trailing_lines(value: &SyntaxNode) -> Vec<SyntaxToken> {
    let mut lines = Vec::new();
    let mut token = value.last_token();
    while let Some(t) = token.filter(|t| {
        t.parent().is_some_and(|p| is_block_collection(&p)) && starts_or_is_blank_line(t)
    }) {
        token = t.prev_token();
        lines.push(t);
    }
    lines.reverse();
    for t in &lines {
        t.detach();
    }
    lines
}

/// Is `token` the indentation of a line, or the line break of a blank
/// one, rather than the line break ending a line with content?
fn starts_or_is_blank_line(token: &SyntaxToken) -> bool {
    let is_newline = |t: &SyntaxToken| t.kind() == SyntaxKind::NEWLINE;
    let prev = token.prev_token();
    match token.kind() {
        SyntaxKind::INDENT | SyntaxKind::WHITESPACE => prev.is_some_and(|p| is_newline(&p)),
        SyntaxKind::NEWLINE => prev.is_some_and(|p| {
            is_newline(&p)
                || (matches!(p.kind(), SyntaxKind::INDENT | SyntaxKind::WHITESPACE)
                    && p.prev_token().is_some_and(|pp| is_newline(&pp)))
        }),
        _ => false,
    }
}

/// Put `lines` from [`take_trailing_lines`] back after `entry`, which now
/// holds the new value: at the end of the innermost block collection the
/// entry ends with, as the parser keeps them, or else right after the
/// entry.
pub(crate) fn restore_trailing_lines(entry: &SyntaxNode, lines: Vec<SyntaxToken>) {
    if lines.is_empty() {
        return;
    }
    // TODO: after a block scalar, blank lines and an indented comment line
    // read as part of its body, so they are dropped there rather than
    // changing the scalar.
    let ends_with_block_scalar = entry
        .last_token()
        .and_then(|t| t.parent())
        .is_some_and(|p| crate::nodes::mapping::is_block_scalar(&p));
    if ends_with_block_scalar {
        return;
    }
    let elements = lines
        .iter()
        .map(|t| fresh_token(t.kind(), t.text()).into())
        .collect();
    match innermost_trailing_collection(entry) {
        Some(collection) => append_children(&collection, elements),
        None => {
            let parent = entry
                .parent()
                .expect("a replaced value's entry is attached");
            // An explicit-key entry leaves its line break to the mapping.
            let handed_off = !super::trailing_newline_reachable(entry)
                && entry
                    .next_sibling_or_token()
                    .is_some_and(|c| c.kind() == SyntaxKind::NEWLINE);
            let at = entry.index() + 1 + usize::from(handed_off);
            parent.splice_children(at..at, elements);
        }
    }
}

/// The innermost block collection that ends where `entry` ends.
fn innermost_trailing_collection(entry: &SyntaxNode) -> Option<SyntaxNode> {
    let mut found = None;
    let mut node = entry.clone();
    while let Some(child) = node.last_child_or_token().and_then(|c| c.into_node()) {
        if is_block_collection(&child) {
            found = Some(child.clone());
        }
        node = child;
    }
    found
}
