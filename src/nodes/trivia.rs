//! Where the line break ending a block collection entry's line lives.
//!
//! An entry normally owns the NEWLINE ending its last line, but the parser
//! puts it deeper when the entry ends in a nested block value, leaves it to
//! the mapping after an explicit-key entry (`? k\n: v\n`), and there is
//! none at the end of unterminated input. These helpers answer and change
//! that in one place.

use super::{
    append_children, entry_value, fresh_token, has_child_token, is_block_scalar, SyntaxNode,
    SyntaxToken,
};
use crate::lex::SyntaxKind;

/// Where an entry's last line ends.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum LineEnd {
    /// A NEWLINE that is the entry's own last child.
    Own,
    /// A NEWLINE inside a child, such as a nested block value, possibly
    /// followed by zero-width tokens.
    Nested,
    /// A NEWLINE right after an explicit-key entry in its parent. After
    /// any other entry, that is a blank line.
    Sibling,
    /// No line break: the entry ends unterminated input.
    Open,
}

impl LineEnd {
    pub(crate) fn is_terminated(self) -> bool {
        self != LineEnd::Open
    }
}

/// Where `entry`'s last line ends.
pub(crate) fn line_end(entry: &SyntaxNode) -> LineEnd {
    match last_non_empty_token(entry) {
        Some(t) if t.kind() == SyntaxKind::NEWLINE => {
            if t.parent().as_ref() == Some(entry) {
                LineEnd::Own
            } else {
                LineEnd::Nested
            }
        }
        _ if has_child_token(entry, |k| k == SyntaxKind::QUESTION)
            && entry
                .next_sibling_or_token()
                .is_some_and(|s| s.kind() == SyntaxKind::NEWLINE) =>
        {
            LineEnd::Sibling
        }
        _ => LineEnd::Open,
    }
}

/// Does `node`'s text end in a NEWLINE, ignoring zero-width tokens such as
/// an implicit null after it?
pub(crate) fn ends_with_line_break(node: &SyntaxNode) -> bool {
    last_non_empty_token(node).is_some_and(|t| t.kind() == SyntaxKind::NEWLINE)
}

/// The last token in `node`'s subtree that has text.
fn last_non_empty_token(node: &SyntaxNode) -> Option<SyntaxToken> {
    // descendants_with_tokens isn't double-ended; prev_token() would walk
    // out of the subtree.
    node.descendants_with_tokens()
        .filter_map(|c| c.into_token())
        .filter(|t| !t.text().is_empty())
        .last()
}

/// The index in `entry`'s parent just past the line break ending its line,
/// where an entry on the next line goes.
pub(crate) fn index_after_line(entry: &SyntaxNode) -> usize {
    entry.index() + 1 + usize::from(line_end(entry) == LineEnd::Sibling)
}

/// Give `entry`, an attached block collection entry, a line break ending
/// its line if it has none, where the parser would put it.
pub(crate) fn terminate(entry: &SyntaxNode) {
    if !line_end(entry).is_terminated() {
        // An explicit-key entry gets it at its end, from where
        // hand_line_break_to_mapping moves it out with the comment ending
        // its line, if any.
        if has_child_token(entry, |k| k == SyntaxKind::QUESTION) {
            append_children(entry, vec![fresh_token(SyntaxKind::NEWLINE, "\n").into()]);
        } else {
            terminate_last_line(entry);
        }
    }
    hand_line_break_to_mapping(entry);
}

/// Remove the line break that ends `collection`, a block collection whose
/// last line had none before it was edited. One that is part of a block
/// scalar's value stays, as removing it would change the value.
pub(crate) fn unterminate_last(collection: &SyntaxNode) {
    let Some(last) = last_non_empty_token(collection) else {
        return;
    };
    let in_scalar = last
        .parent_ancestors()
        .take_while(|n| n != collection)
        .any(|n| n.kind() == SyntaxKind::SCALAR);
    if last.kind() == SyntaxKind::NEWLINE && !in_scalar {
        last.detach();
    }
}

/// Move the NEWLINE that `entry` ends with out to follow it in its mapping,
/// if `entry` is an explicit-key entry: the parser leaves the break after
/// such an entry's value line to the mapping, along with a comment on that
/// line and the whitespace before it. Entries built standalone carry the
/// break themselves, so call this once one is attached.
pub(crate) fn hand_line_break_to_mapping(entry: &SyntaxNode) {
    if entry.kind() != SyntaxKind::MAPPING_ENTRY
        || !has_child_token(entry, |k| k == SyntaxKind::QUESTION)
    {
        return;
    }
    let Some(parent) = entry.parent() else {
        return;
    };
    let Some(newline) = entry
        .last_child_or_token()
        .and_then(|c| c.into_token())
        .filter(|t| t.kind() == SyntaxKind::NEWLINE)
    else {
        return;
    };
    let value = entry_value(entry);
    let mut line_end: Vec<SyntaxToken> =
        std::iter::successors(newline.prev_token(), |t| t.prev_token())
            .take_while(|t| {
                matches!(t.kind(), SyntaxKind::WHITESPACE | SyntaxKind::COMMENT)
                    && t.parent()
                        .is_some_and(|p| p == *entry || Some(&p) == value.as_ref())
            })
            .collect();
    if !line_end.iter().any(|t| t.kind() == SyntaxKind::COMMENT) {
        line_end.clear();
    }
    line_end.reverse();
    line_end.push(newline);
    let moved: Vec<_> = line_end
        .into_iter()
        .map(|t| {
            t.detach();
            fresh_token(t.kind(), t.text()).into()
        })
        .collect();
    let at = entry.index() + 1;
    parent.splice_children(at..at, moved);
}

/// Add the line break that ends the last line of `node`, where the parser
/// puts it when another document follows.
///
/// The parser hands the break to the innermost construct still open at the
/// end of the line: the last block entry, a block scalar, the collection
/// holding a trailing comment line, or the spot in front of an empty value
/// it went looking for on the next line. `node` must not already end in a
/// NEWLINE.
pub(crate) fn terminate_last_line(node: &SyntaxNode) {
    let newline = || vec![fresh_token(SyntaxKind::NEWLINE, "\n").into()];
    let insert_before = |n: &SyntaxNode| {
        let parent = n.parent().expect("a descendant has a parent");
        parent.splice_children(n.index()..n.index(), newline());
    };

    let mut cur = node.clone();
    while let Some(rowan::NodeOrToken::Node(last)) = cur.last_child_or_token() {
        if last.text_range().is_empty() {
            match cur.kind() {
                SyntaxKind::MAPPING_ENTRY if has_child_token(&cur, |k| k == SyntaxKind::COLON) => {}
                SyntaxKind::MAPPING_ENTRY | SyntaxKind::VALUE => {
                    insert_before(&last);
                    return;
                }
                _ => break,
            }
        } else if !holds_line_end(&last) {
            break;
        }
        cur = last;
    }

    // An explicit-key entry leaves the break to its mapping.
    if cur.kind() == SyntaxKind::MAPPING_ENTRY
        && has_child_token(&cur, |k| k == SyntaxKind::QUESTION)
    {
        let parent = cur.parent().expect("a mapping entry has a mapping");
        let after = cur.index() + 1;
        parent.splice_children(after..after, newline());
    } else {
        append_children(&cur, newline());
    }
}

/// Does the parser put the line break after `node`'s last line inside it?
fn holds_line_end(node: &SyntaxNode) -> bool {
    match node.kind() {
        SyntaxKind::MAPPING | SyntaxKind::SEQUENCE => !has_child_token(node, |k| {
            matches!(k, SyntaxKind::LEFT_BRACE | SyntaxKind::LEFT_BRACKET)
        }),
        SyntaxKind::MAPPING_ENTRY | SyntaxKind::SEQUENCE_ENTRY => true,
        SyntaxKind::SCALAR => is_block_scalar(node),
        SyntaxKind::VALUE => node
            .last_child()
            .is_some_and(|last| last.text_range().is_empty() || holds_line_end(&last)),
        SyntaxKind::TAGGED_NODE => node.last_child().is_some_and(|last| holds_line_end(&last)),
        _ => false,
    }
}
