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
/// is on the entry's own first line, where it belongs to the entry. Its
/// line break becomes `entry`'s.
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
    let on_first_line = !entry.descendants_with_tokens().any(|c| {
        c.kind() == SyntaxKind::NEWLINE && c.text_range().end() <= comment.text_range().start()
    });
    if on_first_line {
        return;
    }
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
/// `value` ends with, or in front of an empty value, where it looked for
/// one. They belong to the lines after the value.
pub(crate) fn take_trailing_lines(value: &SyntaxNode) -> Vec<SyntaxToken> {
    let mut lines = Vec::new();
    let mut token = value.last_token();
    while let Some(t) = token.clone().filter(|t| t.text().is_empty()) {
        token = t.prev_token();
    }
    while let Some(t) = token.filter(|t| {
        t.parent()
            .is_some_and(|p| is_block_collection(&p) || p.kind() == SyntaxKind::VALUE)
            && starts_or_is_blank_line(t)
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
/// holds the new value, and lay out the lines after it as the parser does
/// after that value. A detached entry has nothing after it, so they go at
/// its end.
pub(crate) fn restore_trailing_lines(entry: &SyntaxNode, lines: Vec<SyntaxToken>) {
    let elements: Vec<_> = lines
        .iter()
        .map(|t| fresh_token(t.kind(), t.text()).into())
        .collect();
    let (parent, after) = match entry.parent() {
        Some(parent) => (parent, super::trivia::index_after_line(entry)),
        None => (entry.clone(), entry.children_with_tokens().count()),
    };
    parent.splice_children(after..after, elements);

    if super::ending_block_scalar(entry).is_some() {
        settle_lines_after_block_scalar(entry);
        return;
    }
    // The parser keeps blank lines and the indentation of a comment line
    // at the end of the innermost collection still open, but leaves that
    // of an entry's line to the collection holding the entry.
    let Some(collection) = innermost_trailing_collection(entry) else {
        return;
    };
    let starts_comment_line = |t: &SyntaxToken| {
        t.kind() != SyntaxKind::INDENT
            || t.next_sibling_or_token()
                .is_some_and(|c| c.kind() == SyntaxKind::COMMENT)
    };
    let following: Vec<_> = parent
        .children_with_tokens()
        .skip(after)
        .map_while(|c| {
            c.into_token()
                .filter(|t| starts_or_is_blank_line(t) && starts_comment_line(t))
        })
        .collect();
    for t in &following {
        t.detach();
    }
    append_children(
        &collection,
        following
            .iter()
            .map(|t| fresh_token(t.kind(), t.text()).into())
            .collect(),
    );
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
    text.lines()
        .skip(1)
        .find(|line| !line.trim().is_empty())
        .map(|line| line.len() - line.trim_start_matches(' ').len())
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
    let column = crate::as_yaml::source_base_indent(entry);
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
        parent.splice_children(
            at..at,
            vec![fresh_token(SyntaxKind::INDENT, &" ".repeat(column)).into()],
        );
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

/// A line comment: the whitespace before it and its text.
pub(crate) type LineComment = (String, String);

/// Detach and return the comment on the line of `entry`'s dash, which
/// belongs to the entry even where the item nests a collection starting
/// on that line. The parser keeps it before the item when the item starts
/// on a later line, inside the item, or, after a single-line item, after
/// the entry.
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
    let inside = entry.text_range().contains_range(comment.text_range());
    let line_break = comment
        .next_token()
        .filter(|t| t.kind() == SyntaxKind::NEWLINE);
    comment.detach();
    if let Some(last) = spaces.last() {
        last.detach();
    }
    if let Some(line_break) = line_break {
        if inside && line_break.parent().as_ref() == Some(entry) {
            // The item starts on the next line: join it to the dash line.
            let indent = line_break
                .next_sibling_or_token()
                .and_then(|c| c.into_token())
                .filter(|t| t.kind() == SyntaxKind::INDENT);
            line_break.detach();
            if let Some(indent) = indent {
                indent.detach();
            }
        } else if !inside {
            line_break.detach();
            append_children(
                entry,
                vec![fresh_token(SyntaxKind::NEWLINE, line_break.text()).into()],
            );
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
        let column = crate::as_yaml::source_base_indent(&item);
        if let Some(space) = item
            .prev_sibling_or_token()
            .and_then(|c| c.into_token())
            .filter(|t| t.kind() == SyntaxKind::WHITESPACE)
        {
            space.detach();
        }
        let at = item.index();
        entry.splice_children(
            at..at,
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
            parent.splice_children(
                at..at,
                vec![
                    fresh_token(SyntaxKind::WHITESPACE, &ws).into(),
                    fresh_token(SyntaxKind::COMMENT, &text).into(),
                ],
            );
            return;
        }
        _ => None,
    };
    // After a single-line item the parser keeps the comment and its line
    // break out of the entry, before whatever comes next.
    let line_break = line_break.map(|nl| {
        let text = nl.text().to_string();
        nl.detach();
        text
    });
    append_children(entry, vec![fresh_token(SyntaxKind::WHITESPACE, &ws).into()]);
    let mut elements = vec![fresh_token(SyntaxKind::COMMENT, &text).into()];
    if let Some(line_break) = line_break {
        elements.push(fresh_token(SyntaxKind::NEWLINE, &line_break).into());
    }
    let parent = entry.parent().expect("an entry is attached");
    let at = entry.index() + 1;
    parent.splice_children(at..at, elements);
}

/// Put `comment` on a line of its own directly above `entry`, at the
/// entry's column, where the parser keeps such a line. A detached entry
/// has no line above it, so the comment line goes first in the entry.
pub(crate) fn insert_comment_line_above(entry: &SyntaxNode, comment: &str) {
    let column = crate::as_yaml::source_base_indent(entry);
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
            parent.splice_children(at..at, elements);
        }
        None => node.splice_children(0..0, elements),
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
    parent.splice_children(
        at..at,
        vec![
            fresh_token(SyntaxKind::WHITESPACE, &comment.0).into(),
            fresh_token(SyntaxKind::COMMENT, &comment.1).into(),
        ],
    );
    true
}
