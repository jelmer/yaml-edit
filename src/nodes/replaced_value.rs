//! Trivia around a value that is being replaced in its entry.
//!
//! Comments inside the old value go with it. The parser does not keep all
//! of them inside the value node, though, so the replacement has to sort
//! these out first.

use super::{append_children, fresh_token, insert_children, SyntaxNode, SyntaxToken};
use crate::lex::SyntaxKind;

fn is_block_collection(node: &SyntaxNode) -> bool {
    matches!(node.kind(), SyntaxKind::MAPPING | SyntaxKind::SEQUENCE)
        && !super::starts_a_flow_collection(node)
}

/// Remove the comment that ends the last line of `item`, a sequence item
/// spanning lines, which its entry holds after it. It goes with the item;
/// one on the dash line belongs to the entry.
pub(crate) fn remove_comment_after_item(item: &SyntaxNode) {
    if !item
        .descendants_with_tokens()
        .any(|c| c.kind() == SyntaxKind::NEWLINE)
    {
        return;
    }
    let after: Vec<_> = item
        .siblings_with_tokens(rowan::Direction::Next)
        .skip(1)
        .take_while(|c| matches!(c.kind(), SyntaxKind::WHITESPACE | SyntaxKind::COMMENT))
        .collect();
    if after.iter().any(|c| c.kind() == SyntaxKind::COMMENT) {
        for c in after {
            c.detach();
        }
    }
}

/// Lay out the lines after `entry`, whose value now ends in a block
/// scalar, so that they stay out of its body and its value is the one
/// that was written.
pub(crate) fn settle_lines_after_block_scalar(entry: &SyntaxNode) {
    let Some(scalar) = super::ending_block_scalar(entry) else {
        return;
    };
    absorb_blank_lines_after(&scalar);
    dedent_comment_line_after(entry, &scalar);
}

/// The column of the body of `scalar`, a block scalar, if it is known:
/// its explicit indentation indicator counted from the parent's column,
/// or else that of its first line that is not empty.
fn block_scalar_body_column(scalar: &SyntaxNode) -> Option<usize> {
    if let Some(indent) = crate::nodes::scalar_node::block_scalar_indicators(scalar).1 {
        return Some(crate::nodes::scalar_node::block_parent_column(scalar) + indent);
    }
    let text = scalar.text().to_string();
    text.split(['\n', '\r'])
        .skip(1)
        .find(|line| !line.trim().is_empty())
        .map(crate::column::space_indent)
}

/// The line break ending the last line of `scalar`, a block scalar: its
/// own last token if the parser put it there, or else the token after it.
fn block_scalar_line_end(scalar: &SyntaxNode) -> Option<SyntaxToken> {
    let last = scalar.last_token()?;
    if last.kind() == SyntaxKind::NEWLINE {
        return Some(last);
    }
    last.next_token()
        .filter(|t| t.kind() == SyntaxKind::NEWLINE)
}

/// Fit the blank lines after `scalar`, a block scalar a value was just
/// written as, to it, as the parser reads them as part of its body.
///
/// They must not change its value. Spaces on such a line past the body's
/// column would be body text, and a tab cannot start a body line at all,
/// so those lines lose their white space. A kept (`|+`) scalar's value
/// takes in every trailing blank line, and only a comment line could end
/// it before them, so after one the blank lines are dropped.
///
/// The parser keeps those lines in the scalar, so they move into one
/// that holds its own last line break, as a parsed one does. A scalar
/// built from a value is a single token and leaves them where they are.
fn absorb_blank_lines_after(scalar: &SyntaxNode) {
    let Some(end) = block_scalar_line_end(scalar) else {
        return;
    };
    let into_scalar = end.parent().as_ref() == Some(scalar);
    let mut lines = Vec::new();
    let mut next = end.next_token();
    loop {
        let space = next
            .clone()
            .filter(|t| matches!(t.kind(), SyntaxKind::INDENT | SyntaxKind::WHITESPACE));
        let Some(line_break) = space
            .as_ref()
            .map_or(next.clone(), |t| t.next_token())
            .filter(|t| t.kind() == SyntaxKind::NEWLINE)
        else {
            break;
        };
        next = line_break.next_token();
        lines.push((space, line_break));
    }
    let keep = crate::nodes::scalar_node::block_scalar_indicators(scalar).0
        == crate::nodes::scalar_node::Chomping::Keep;
    let body = block_scalar_body_column(scalar);
    let fits =
        |text: &str| text.bytes().all(|b| b == b' ') && body.map_or(true, |b| text.len() <= b);
    // A line stays (in the scalar or where it is) unless the scalar is
    // kept, and keeps its spaces only if they fit.
    let mut elements = Vec::new();
    for (space, line_break) in lines {
        let fitting_space = space
            .as_ref()
            .filter(|t| !keep && fits(t.text()))
            .map(|t| t.text().to_string());
        if into_scalar || fitting_space.is_none() {
            space.iter().for_each(SyntaxToken::detach);
        }
        if into_scalar || keep {
            line_break.detach();
        }
        if into_scalar && !keep {
            elements.extend(fitting_space.map(|t| fresh_token(SyntaxKind::INDENT, &t).into()));
            elements.push(fresh_token(SyntaxKind::NEWLINE, line_break.text()).into());
        }
    }
    append_children(scalar, elements);
}

/// Move a comment line after `entry` that is indented as far as the body
/// of `scalar`, the block scalar `entry` ends with, back to the entry's
/// column, so that it stays a comment rather than becoming body text.
fn dedent_comment_line_after(entry: &SyntaxNode, scalar: &SyntaxNode) {
    let column = crate::column::node_column(entry);
    let body = block_scalar_body_column(scalar).unwrap_or(column + 1);
    let is_space =
        |t: &SyntaxToken| matches!(t.kind(), SyntaxKind::INDENT | SyntaxKind::WHITESPACE);
    let mut next = block_scalar_line_end(scalar).and_then(|t| t.next_token());
    // Blank lines, which may hold spaces up to the body's column, do not
    // end the body.
    while let Some(t) = next.clone() {
        let line_break = match t.kind() {
            SyntaxKind::NEWLINE => t,
            _ if is_space(&t) => match t.next_token() {
                Some(nl) if nl.kind() == SyntaxKind::NEWLINE => nl,
                _ => break,
            },
            _ => break,
        };
        next = line_break.next_token();
    }
    let Some(indent) = next.filter(|t| {
        is_space(t)
            && t.text().len() >= body
            && t.next_token()
                .is_some_and(|c| c.kind() == SyntaxKind::COMMENT)
    }) else {
        return;
    };
    let parent = indent.parent().expect("a token has a parent");
    let at = indent.index();
    indent.detach();
    if column > 0 {
        insert_children(
            &parent,
            at,
            vec![fresh_token(SyntaxKind::INDENT, &" ".repeat(column)).into()],
        );
    }
}

/// A line comment: the whitespace before it and its text.
pub(crate) type LineComment = (String, String);

/// Detach and return the comment on the line of `entry`'s dash, which
/// belongs to the entry even where the item nests a collection starting
/// on that line. The parser keeps it before the item when the item starts
/// on a later line, inside the item, or, after a single-line item, at the
/// end of the entry.
pub(crate) fn take_dash_line_comment(entry: &SyntaxNode) -> Option<LineComment> {
    let dash = entry
        .children_with_tokens()
        .filter_map(|c| c.into_token())
        .find(|t| t.kind() == SyntaxKind::DASH)?;
    let mut spaces: Vec<SyntaxToken> = Vec::new();
    let mut next = dash.next_token();
    let comment = loop {
        let t = next?;
        match t.kind() {
            SyntaxKind::COMMENT => break t,
            SyntaxKind::NEWLINE => return None,
            SyntaxKind::WHITESPACE => spaces.push(t.clone()),
            _ if t.text().is_empty() => {}
            _ => spaces.clear(),
        }
        next = t.next_token();
    };
    let ws = spaces
        .last()
        .map_or_else(|| "  ".to_string(), |t| t.text().to_string());
    // The item starts on the next line if the line break after the
    // comment is not the one ending the entry: join it to the dash line.
    let line_break = comment.next_token().filter(|t| {
        t.kind() == SyntaxKind::NEWLINE
            && t.parent().as_ref() == Some(entry)
            && t.next_sibling_or_token().is_some()
    });
    comment.detach();
    if let Some(last) = spaces.last() {
        last.detach();
    }
    if let Some(line_break) = line_break {
        let indent = line_break
            .next_sibling_or_token()
            .and_then(|c| c.into_token())
            .filter(|t| t.kind() == SyntaxKind::INDENT);
        line_break.detach();
        if let Some(indent) = indent {
            indent.detach();
        }
    }
    Some((ws, comment.text().to_string()))
}

/// Put `comment`, taken by [`take_dash_line_comment`], back on the dash
/// line of `entry`, which now holds the new item.
pub(crate) fn add_dash_line_comment(entry: &SyntaxNode, comment: LineComment) {
    let (ws, text) = comment;
    let first_line: Vec<SyntaxToken> = entry
        .descendants_with_tokens()
        .filter_map(|c| c.into_token())
        .take_while(|t| t.kind() != SyntaxKind::NEWLINE)
        .collect();
    if first_line.iter().any(|t| t.kind() == SyntaxKind::COMMENT) {
        insert_comment_line_above(entry, &text);
        return;
    }
    let item_on_dash_line = entry.children().find(is_block_collection).filter(|item| {
        first_line
            .iter()
            .any(|t| item.text_range().contains_range(t.text_range()))
    });
    if let Some(item) = item_on_dash_line {
        // A collection opening on the dash line would make the comment its
        // first entry's, so it moves to the next line.
        let column = crate::column::node_column(&item);
        if let Some(space) = item
            .prev_sibling_or_token()
            .and_then(|c| c.into_token())
            .filter(|t| t.kind() == SyntaxKind::WHITESPACE)
        {
            space.detach();
        }
        let at = item.index();
        insert_children(
            entry,
            at,
            vec![
                fresh_token(SyntaxKind::WHITESPACE, &ws).into(),
                fresh_token(SyntaxKind::COMMENT, &text).into(),
                fresh_token(SyntaxKind::NEWLINE, "\n").into(),
                fresh_token(SyntaxKind::INDENT, &" ".repeat(column)).into(),
            ],
        );
        return;
    }
    let last = first_line.last().expect("an entry starts with its dash");
    let line_break = match last.next_token() {
        Some(nl) if nl.parent().as_ref() == Some(entry) && nl.next_sibling_or_token().is_none() => {
            Some(nl)
        }
        Some(t) if entry.text_range().contains_range(t.text_range()) => {
            // The item goes on past this line, after a block scalar
            // indicator, tag or anchor: the comment goes with them.
            let parent = last.parent().expect("a token has a parent");
            let at = last.index() + 1;
            insert_children(
                &parent,
                at,
                vec![
                    fresh_token(SyntaxKind::WHITESPACE, &ws).into(),
                    fresh_token(SyntaxKind::COMMENT, &text).into(),
                ],
            );
            return;
        }
        _ => None,
    };
    // After a single-line item the comment goes before the entry's line
    // break.
    let at = line_break.map_or_else(|| entry.children_with_tokens().count(), |nl| nl.index());
    insert_children(
        entry,
        at,
        vec![
            fresh_token(SyntaxKind::WHITESPACE, &ws).into(),
            fresh_token(SyntaxKind::COMMENT, &text).into(),
        ],
    );
}

/// Put `comment` on a line of its own directly above `entry`, at the
/// entry's column, where the parser keeps such a line. A detached entry
/// has no line above it, so the comment line goes first in the entry.
pub(crate) fn insert_comment_line_above(entry: &SyntaxNode, comment: &str) {
    let column = crate::column::node_column(entry);
    // The line starts in the outermost node that `entry` starts.
    let mut node = entry.clone();
    while node.prev_sibling_or_token().is_none() && node.kind() != SyntaxKind::DOCUMENT {
        match node.parent() {
            Some(parent) => node = parent,
            None => break,
        }
    }
    let mut elements = vec![
        fresh_token(SyntaxKind::COMMENT, comment).into(),
        fresh_token(SyntaxKind::NEWLINE, "\n").into(),
    ];
    if column > 0 {
        elements.push(fresh_token(SyntaxKind::INDENT, &" ".repeat(column)).into());
    }
    match node.parent() {
        Some(parent) => {
            let at = node.index();
            insert_children(&parent, at, elements);
        }
        None => insert_children(&node, 0, elements),
    }
}

/// Put `comment` at the end of the first line of `value`, which starts on
/// its entry's line. Returns false, changing nothing, if that line already
/// has a comment.
pub(crate) fn add_to_first_line(value: &SyntaxNode, comment: LineComment) -> bool {
    let first_line: Vec<_> = value
        .descendants_with_tokens()
        .filter_map(|c| c.into_token())
        .take_while(|t| t.kind() != SyntaxKind::NEWLINE)
        .collect();
    if first_line.iter().any(|t| t.kind() == SyntaxKind::COMMENT) {
        return false;
    }
    let last = first_line
        .last()
        .expect("a value on the key line starts with a token");
    let parent = last.parent().expect("a token has a parent");
    let at = last.index() + 1;
    insert_children(
        &parent,
        at,
        vec![
            fresh_token(SyntaxKind::WHITESPACE, &comment.0).into(),
            fresh_token(SyntaxKind::COMMENT, &comment.1).into(),
        ],
    );
    true
}
