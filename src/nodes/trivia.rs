//! The trivia between collection entries: the line break ending a block
//! collection entry's line, and the separator after a flow collection entry,
//! and adding or removing an entry along with the trivia that belongs to it.
//!
//! A block entry normally owns the NEWLINE ending its last line, but the
//! parser puts it deeper when the entry ends in a nested block value, leaves
//! it to the mapping after an explicit-key entry (`? k\n: v\n`), and there
//! is none at the end of unterminated input. A flow entry owns the comma
//! after it and the white space around that.

use super::empty_collection::{detach_empty_collection_placeholder_newline, render_empty};
use super::{
    append_children, entry_value, fresh_token, has_child_token, insert_children, is_block_scalar,
    replace_children, SyntaxNode, SyntaxToken,
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
    /// A NEWLINE after a comment that ends the entry's line but which the
    /// parser put after the entry in its parent, as after a sequence item.
    AfterComment,
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
        _ if comment_line_end(entry) => LineEnd::AfterComment,
        _ => LineEnd::Open,
    }
}

/// Is `entry` followed in its parent by a comment on its last line and the
/// NEWLINE ending that?
fn comment_line_end(entry: &SyntaxNode) -> bool {
    comment_after(entry)
        .and_then(|c| c.next_sibling_or_token())
        .is_some_and(|s| s.kind() == SyntaxKind::NEWLINE)
}

/// The comment after `entry` in its parent, on its last line.
fn comment_after(entry: &SyntaxNode) -> Option<rowan::SyntaxElement<super::Lang>> {
    std::iter::successors(entry.next_sibling_or_token(), |s| s.next_sibling_or_token())
        .find(|s| s.kind() != SyntaxKind::WHITESPACE)
        .filter(|s| s.kind() == SyntaxKind::COMMENT)
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
/// where an entry on the next line goes. If there is no line break, it is
/// the index past the comment ending the line, if the parser put that
/// beside the entry.
pub(crate) fn index_after_line(entry: &SyntaxNode) -> usize {
    match line_end(entry) {
        LineEnd::Sibling => entry.index() + 2,
        LineEnd::AfterComment => {
            comment_after(entry)
                .expect("a comment ends the line")
                .index()
                + 2
        }
        LineEnd::Open => comment_after(entry).map_or(entry.index(), |c| c.index()) + 1,
        LineEnd::Own | LineEnd::Nested => entry.index() + 1,
    }
}

/// Where a new entry goes in its collection.
pub(crate) enum Slot {
    /// After the last entry.
    End,
    /// On the line after this entry.
    After(SyntaxNode),
    /// In place of this entry, which moves to the next line.
    Before(SyntaxNode),
}

/// The column at which the entries of `collection` start: the INDENT
/// before its second entry, or failing that the column the collection
/// starts at.
pub(crate) fn entry_column(collection: &SyntaxNode) -> usize {
    let mut entries = collection.children().filter(|n| {
        matches!(
            n.kind(),
            SyntaxKind::MAPPING_ENTRY | SyntaxKind::SEQUENCE_ENTRY
        )
    });
    entries
        .nth(1)
        .and_then(|second| second.prev_sibling_or_token())
        .and_then(|c| c.into_token())
        .filter(|t| t.kind() == SyntaxKind::INDENT)
        .map_or_else(
            || crate::column::node_column(collection),
            |t| t.text().len(),
        )
}

/// Splice `entry`, a block collection entry built standalone, into
/// `collection` at `slot`, on a line of its own.
///
/// The entry before it gets a line break if it lacks one, the new entry
/// an INDENT at the collection's column (unless it comes first, where the
/// parser has the column outside the collection), and an explicit-key
/// entry's line break goes to the collection. Should the new entry end in
/// a block scalar, the lines after it are laid out so that it does not
/// take them in.
///
/// Should `entry` go after the end of input that had no final line
/// break, it ends without one.
pub(crate) fn attach_entry(collection: &SyntaxNode, slot: Slot, entry: &SyntaxNode) {
    let kind = entry.kind();
    let column = entry_column(collection);
    let indent =
        || (column > 0).then(|| fresh_token(SyntaxKind::INDENT, &" ".repeat(column)).into());
    let mut first = false;
    let mut open_end = false;
    let (at, elements) = match slot {
        Slot::Before(next) => {
            let prev = next
                .siblings(rowan::Direction::Prev)
                .skip(1)
                .find(|n| n.kind() == kind);
            if let Some(prev) = prev {
                terminate(&prev);
            }
            // The INDENT before `next`, if any, is the new entry's now.
            let mut elements = vec![entry.clone().into()];
            elements.extend(indent());
            (next.index(), elements)
        }
        Slot::After(prev) => {
            open_end = !line_end(&prev).is_terminated();
            terminate(&prev);
            let mut elements: Vec<_> = indent().into_iter().collect();
            elements.push(entry.clone().into());
            (index_after_line(&prev), elements)
        }
        Slot::End => {
            let mut elements = Vec::new();
            let at = match last_entry(collection, kind) {
                Some(last) => {
                    elements.extend(indent());
                    // Blank lines ending a nested collection separate it
                    // from what follows; those ending the document stay
                    // there.
                    if is_root(collection) {
                        end_slot_index(collection)
                    } else {
                        index_after_last_line(collection, &last)
                    }
                }
                None => {
                    first = true;
                    end_slot_index(collection)
                }
            };
            elements.push(entry.clone().into());
            let (at, opened) = end_line(collection, at);
            open_end = opened;
            (at, elements)
        }
    };
    insert_children(collection, at, elements);
    if first {
        detach_empty_collection_placeholder_newline(collection, entry);
    }
    hand_line_break_to_mapping(entry);
    super::settle_lines_after_inserted_entry(entry);
    if open_end {
        match line_end(entry) {
            LineEnd::Sibling => {
                let newline = entry.next_sibling_or_token().expect("a line break follows");
                newline.detach();
            }
            _ => unterminate_last(entry),
        }
    }
}

/// The last entry of kind `kind` in `collection`.
fn last_entry(collection: &SyntaxNode, kind: SyntaxKind) -> Option<SyntaxNode> {
    std::iter::successors(collection.last_child(), |n| n.prev_sibling()).find(|n| n.kind() == kind)
}

/// Is `collection` the document's own node, rather than a value in
/// another collection?
fn is_root(collection: &SyntaxNode) -> bool {
    !collection.ancestors().skip(1).any(|n| {
        matches!(
            n.kind(),
            SyntaxKind::MAPPING_ENTRY | SyntaxKind::SEQUENCE_ENTRY
        )
    })
}

/// The index in `collection` where an entry after its last one goes: at
/// the end, ahead of the INDENT of a comment line the parser lifted out.
fn end_slot_index(collection: &SyntaxNode) -> usize {
    let count = collection.green().children().len();
    let lifted_comment_indent = collection
        .last_child_or_token()
        .is_some_and(|c| c.kind() == SyntaxKind::INDENT);
    count - usize::from(lifted_comment_indent)
}

/// The child of `node` at index `i`, found from the nearer end, as
/// `push` needs to stay cheap on a long sequence.
fn child_at(node: &SyntaxNode, i: usize) -> Option<rowan::SyntaxElement<super::Lang>> {
    let from_end = node.green().children().len().checked_sub(i + 1)?;
    if from_end < i {
        std::iter::successors(node.last_child_or_token(), |c| c.prev_sibling_or_token())
            .nth(from_end)
    } else {
        node.children_with_tokens().nth(i)
    }
}

/// The index in `collection` just past the line of `last`, its last entry,
/// or past the last comment line after it.
fn index_after_last_line(collection: &SyntaxNode, last: &SyntaxNode) -> usize {
    std::iter::successors(collection.last_child_or_token(), |c| {
        c.prev_sibling_or_token()
    })
    .take_while(|c| c.as_node() != Some(last))
    .find(|c| c.kind() == SyntaxKind::COMMENT)
    .map_or_else(|| last.index(), |c| c.index())
        + 1
}

/// End the line that the child of `collection` before index `at` is on,
/// if it is open. Returns the index of the start of the next line, and
/// whether the line was open.
fn end_line(collection: &SyntaxNode, at: usize) -> (usize, bool) {
    let before = at.checked_sub(1).and_then(|i| child_at(collection, i));
    match before {
        Some(rowan::NodeOrToken::Node(entry))
            if matches!(
                entry.kind(),
                SyntaxKind::MAPPING_ENTRY | SyntaxKind::SEQUENCE_ENTRY
            ) =>
        {
            let open = !line_end(&entry).is_terminated();
            terminate(&entry);
            (index_after_line(&entry), open)
        }
        Some(rowan::NodeOrToken::Token(t)) if t.kind() == SyntaxKind::COMMENT => {
            if t.next_sibling_or_token()
                .is_some_and(|c| c.kind() == SyntaxKind::NEWLINE)
            {
                return (at + 1, false);
            }
            insert_children(
                collection,
                at,
                vec![fresh_token(SyntaxKind::NEWLINE, "\n").into()],
            );
            (at + 1, true)
        }
        _ => (at, false),
    }
}

/// Splice `entry`, a flow collection entry built standalone, into
/// `collection`, a flow collection, at `slot`, with the separators that
/// keep the entries apart.
pub(crate) fn attach_flow_entry(collection: &SyntaxNode, slot: Slot, entry: &SyntaxNode) {
    let at = match slot {
        Slot::End => collection
            .children_with_tokens()
            .position(|c| {
                matches!(
                    c.kind(),
                    SyntaxKind::RIGHT_BRACE | SyntaxKind::RIGHT_BRACKET
                )
            })
            .unwrap_or_else(|| collection.children_with_tokens().count()),
        Slot::After(prev) => prev.index() + 1,
        Slot::Before(next) => next.index(),
    };

    let kind = entry.kind();
    let mut entries_before: Vec<SyntaxNode> = collection
        .children_with_tokens()
        .take(at)
        .filter_map(|c| c.into_node())
        .filter(|n| n.kind() == kind)
        .collect();
    let has_following_entry = collection
        .children_with_tokens()
        .skip(at)
        .any(|c| c.as_node().is_some_and(|n| n.kind() == kind));
    if has_following_entry {
        // The new entry separates from the next one with `, `, and one
        // before it gets the same if it lacked a separator.
        if let Some(prev) = entries_before.last() {
            FlowSeparator::ensure(prev);
        }
        FlowSeparator::ensure(entry);
    } else if let Some(last) = entries_before.pop() {
        FlowSeparator::hand_over_end(&last, entries_before.last(), entry);
    }
    insert_children(collection, at, vec![entry.clone().into()]);
}

/// Splice `entry` out of `collection`, a block or flow collection, with
/// the trivia that belongs to it.
///
/// In a block collection that is the INDENT before it, the comment ending
/// its line and the line break after that when the parser put those beside
/// it, and blank lines before it when there are blank lines after it too,
/// so that the gap doesn't double. Comment lines around it stay. In a flow
/// collection, the entry already holds its separator; the new last entry
/// loses the one leading to the removed one, unless that had its own.
pub(crate) fn detach_entry(collection: &SyntaxNode, entry: &SyntaxNode) {
    let kind = entry.kind();
    let is_last = !std::iter::successors(entry.next_sibling(), |n| n.next_sibling())
        .any(|n| n.kind() == kind);
    let prev = std::iter::successors(entry.prev_sibling(), |n| n.prev_sibling())
        .find(|n| n.kind() == kind);
    let end = line_end(entry);
    let flow = has_child_token(collection, |k| {
        matches!(k, SyntaxKind::LEFT_BRACE | SyntaxKind::LEFT_BRACKET)
    });

    let mut trivia = Vec::new();
    if !flow {
        trivia.extend(
            entry
                .prev_sibling_or_token()
                .filter(|c| c.kind() == SyntaxKind::INDENT),
        );
        let mut after =
            std::iter::successors(entry.next_sibling_or_token(), |c| c.next_sibling_or_token())
                .peekable();
        if matches!(end, LineEnd::Sibling | LineEnd::AfterComment) {
            for c in after.by_ref() {
                let newline = c.kind() == SyntaxKind::NEWLINE;
                trivia.push(c);
                if newline {
                    break;
                }
            }
        }
        if after
            .peek()
            .is_some_and(|c| c.kind() == SyntaxKind::NEWLINE)
        {
            trivia.extend(blank_lines_before(entry));
        }
    }

    entry.detach();
    for c in trivia {
        c.detach();
    }

    let drained = !collection.children().any(|n| n.kind() == kind);
    if flow {
        if is_last && !has_comma(entry) {
            if let Some(last) = collection.children().filter(|n| n.kind() == kind).last() {
                FlowSeparator::strip(&last);
            }
        }
    } else {
        // An INDENT now leading the collection would double the indent
        // its parent supplies, unless it starts a comment line.
        let leading_indent = collection
            .first_child_or_token()
            .and_then(|c| c.into_token())
            .filter(|t| t.kind() == SyntaxKind::INDENT && !crate::yaml::indents_comment_line(t));
        if let Some(indent) = leading_indent {
            indent.detach();
        }
        if !drained {
            crate::yaml::move_leading_lines_out(collection);
            if is_last {
                move_trailing_comment_lines_out(collection);
            }
        }
        if let Some(prev) = &prev {
            super::replaced_value::settle_lines_after_block_scalar(prev);
        }
    }

    if drained {
        render_empty(collection, end.is_terminated());
    }
    // The end of the input keeps its lack of a final line break.
    if is_last && !end.is_terminated() && !flow {
        unterminate_last(collection);
    }
}

/// Move the comment lines ending `collection`, a block collection, out
/// after the entry holding it in the collection that has an entry after
/// that, where the parser puts comment lines before a dedented line. The
/// blank lines and the INDENT before the first one stay. Lines with
/// nothing after them stay too.
fn move_trailing_comment_lines_out(collection: &SyntaxNode) {
    let mut trailing: Vec<_> = std::iter::successors(collection.last_child_or_token(), |c| {
        c.prev_sibling_or_token()
    })
    .map_while(|c| c.into_token())
    .collect();
    trailing.reverse();
    let starts_line = |t: &SyntaxToken| {
        t.kind() == SyntaxKind::COMMENT
            && t.prev_sibling_or_token()
                .is_some_and(|p| matches!(p.kind(), SyntaxKind::INDENT | SyntaxKind::NEWLINE))
    };
    let Some(first) = trailing.iter().position(starts_line) else {
        return;
    };
    let holder = collection
        .ancestors()
        .skip(1)
        .filter(|n| {
            matches!(
                n.kind(),
                SyntaxKind::MAPPING_ENTRY | SyntaxKind::SEQUENCE_ENTRY
            )
        })
        .find(|e| e.next_sibling().is_some_and(|n| n.kind() == e.kind()));
    let Some(holder) = holder else {
        return;
    };
    let lines = trailing.split_off(first);
    for t in &lines {
        t.detach();
    }
    insert_children(
        &holder.parent().expect("an entry is in a collection"),
        holder.index() + 1,
        lines
            .iter()
            .map(|t| fresh_token(t.kind(), t.text()).into())
            .collect(),
    );
}

/// The blank lines directly before `entry`, past the INDENT leading it.
fn blank_lines_before(entry: &SyntaxNode) -> Vec<rowan::SyntaxElement<super::Lang>> {
    let mut before =
        std::iter::successors(entry.prev_sibling_or_token(), |c| c.prev_sibling_or_token())
            .skip_while(|c| c.kind() == SyntaxKind::INDENT)
            .peekable();
    let mut newlines = Vec::new();
    while let Some(newline) = before.next_if(|c| c.kind() == SyntaxKind::NEWLINE) {
        newlines.push(newline);
    }
    // The first line break ends a line with content, unless that content
    // holds its own: a comment line, or an explicit-key entry.
    let ends_line = match before.next() {
        Some(rowan::NodeOrToken::Node(n)) => !ends_with_line_break(&n),
        Some(rowan::NodeOrToken::Token(_)) => true,
        None => false,
    };
    if ends_line {
        newlines.pop();
    }
    newlines
}

/// Give `entry`, an attached block collection entry, a line break ending
/// its line if it has none, where the parser would put it.
pub(crate) fn terminate(entry: &SyntaxNode) {
    if !line_end(entry).is_terminated() {
        // An explicit-key entry gets it at its end, from where
        // hand_line_break_to_mapping moves it out with the comment ending
        // its line, if any.
        if let Some(comment) = comment_after(entry) {
            let parent = entry.parent().expect("an attached entry has a parent");
            let at = comment.index() + 1;
            insert_children(
                &parent,
                at,
                vec![fresh_token(SyntaxKind::NEWLINE, "\n").into()],
            );
        } else if has_child_token(entry, |k| k == SyntaxKind::QUESTION) {
            append_children(entry, vec![fresh_token(SyntaxKind::NEWLINE, "\n").into()]);
        } else {
            terminate_last_line(entry);
        }
    }
    hand_line_break_to_mapping(entry);
}

/// Remove the line break that ends `node`, a block collection or entry
/// ending input whose last line had none before it was edited. One that is
/// part of a block scalar's value stays, as removing it would change the
/// value.
pub(crate) fn unterminate_last(node: &SyntaxNode) {
    let Some(last) = last_non_empty_token(node) else {
        return;
    };
    let in_scalar = last
        .parent_ancestors()
        .take_while(|n| n != node)
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
    insert_children(&parent, at, moved);
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
        insert_children(&parent, n.index(), newline());
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
        insert_children(&parent, after, newline());
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

/// The separator a flow collection entry ends with: its comma, if any, and
/// the white space around it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct FlowSeparator(Vec<(SyntaxKind, String)>);

impl FlowSeparator {
    fn comma_space() -> Self {
        Self(vec![
            (SyntaxKind::COMMA, ",".to_string()),
            (SyntaxKind::WHITESPACE, " ".to_string()),
        ])
    }

    /// The separator `entry` ends with. `None` if a comment comes before
    /// the comma or ends the entry, since a comma can't go after one on its
    /// line.
    pub(crate) fn of(entry: &SyntaxNode) -> Option<Self> {
        let tokens = separator_tokens(entry)?;
        Some(Self(
            tokens
                .iter()
                .map(|t| (t.kind(), t.text().to_string()))
                .collect(),
        ))
    }

    fn build(&self) -> Vec<rowan::SyntaxElement<super::Lang>> {
        self.0
            .iter()
            .map(|(kind, text)| fresh_token(*kind, text).into())
            .collect()
    }

    /// Replace the separator `entry` ends with by `separator`. Returns
    /// `false`, leaving `entry` alone, if its own can't be swapped (see
    /// [`of`](Self::of)).
    pub(crate) fn swap(entry: &SyntaxNode, separator: &Self) -> bool {
        let Some(own) = separator_tokens(entry) else {
            return false;
        };
        let end = entry.children_with_tokens().count();
        replace_children(entry, end - own.len()..end, separator.build());
        true
    }

    /// Append `, ` to `entry` unless it already has a comma.
    pub(crate) fn ensure(entry: &SyntaxNode) {
        if !has_comma(entry) {
            append_children(entry, Self::comma_space().build());
        }
    }

    /// Put a comma right after the content of `entry` if it has none,
    /// ahead of any comment that follows it.
    pub(crate) fn insert_comma(entry: &SyntaxNode) {
        if has_comma(entry) {
            return;
        }
        let at = entry
            .children_with_tokens()
            .filter(|c| c.as_node().is_some())
            .last()
            .map_or(0, |n| n.index() + 1);
        let mut tokens = vec![fresh_token(SyntaxKind::COMMA, ",").into()];
        let spaced = entry.children_with_tokens().nth(at).is_some_and(|c| {
            matches!(
                c.kind(),
                SyntaxKind::WHITESPACE | SyntaxKind::NEWLINE | SyntaxKind::INDENT
            )
        });
        if !spaced {
            tokens.push(fresh_token(SyntaxKind::WHITESPACE, " ").into());
        }
        insert_children(entry, at, tokens);
    }

    /// Drop the comma at the end of `entry`, which has become the last
    /// entry. White space and line breaks after the comma go too, unless a
    /// comment follows it.
    pub(crate) fn strip(entry: &SyntaxNode) {
        let Some(comma) = entry
            .children_with_tokens()
            .filter_map(|c| c.into_token())
            .filter(|t| t.kind() == SyntaxKind::COMMA)
            .last()
        else {
            return;
        };
        let mut tail = vec![comma.clone()];
        let mut next = comma.next_sibling_or_token();
        while let Some(c) = next {
            let Some(t) = c.into_token() else { break };
            next = t.next_sibling_or_token();
            tail.push(t);
        }
        let only_whitespace = tail[1..].iter().all(|t| {
            matches!(
                t.kind(),
                SyntaxKind::WHITESPACE | SyntaxKind::NEWLINE | SyntaxKind::INDENT
            )
        });
        if !only_whitespace {
            tail.truncate(1);
        }
        for t in tail {
            t.detach();
        }
    }

    /// Make room for `new_entry` after `last`, the last entry of a flow
    /// collection, keeping the layout of the end of the collection:
    /// `new_entry` takes over the separator `last` ends with, trailing comma
    /// included, and `last` gets the one between the entries before it, or
    /// `, ` if there is none. `before_last` is the entry before `last`.
    pub(crate) fn hand_over_end(
        last: &SyntaxNode,
        before_last: Option<&SyntaxNode>,
        new_entry: &SyntaxNode,
    ) {
        let Some(separator) = separator_tokens(last) else {
            // A comment ends the line of `last`, which keeps its separator;
            // the new entry goes on the next line.
            let comma = fresh_token(SyntaxKind::COMMA, ",").into();
            if has_comma(last) {
                append_children(new_entry, vec![comma]);
            } else {
                let space = fresh_token(SyntaxKind::WHITESPACE, " ").into();
                append_children(last, vec![comma, space]);
            }
            return;
        };
        let usual = before_last
            .filter(|e| has_comma(e))
            .and_then(Self::of)
            .unwrap_or_else(Self::comma_space);
        for t in &separator {
            t.detach();
        }
        append_children(new_entry, separator.into_iter().map(Into::into).collect());
        append_children(last, usual.build());
    }
}

/// Does `entry`, a flow collection entry, have a comma after it?
pub(crate) fn has_comma(entry: &SyntaxNode) -> bool {
    has_child_token(entry, |k| k == SyntaxKind::COMMA)
}

/// The tokens of [`FlowSeparator::of`].
fn separator_tokens(entry: &SyntaxNode) -> Option<Vec<SyntaxToken>> {
    let children: Vec<_> = entry.children_with_tokens().collect();
    let start = children
        .iter()
        .rposition(|c| {
            !matches!(
                c.kind(),
                SyntaxKind::COMMA
                    | SyntaxKind::WHITESPACE
                    | SyntaxKind::NEWLINE
                    | SyntaxKind::INDENT
            ) || c.as_node().is_some()
        })
        .map_or(0, |i| i + 1);
    let separator: Vec<SyntaxToken> = children[start..]
        .iter()
        .filter_map(|c| c.as_token().cloned())
        .collect();
    let comma_in_separator = separator.iter().any(|t| t.kind() == SyntaxKind::COMMA);
    if comma_in_separator != has_comma(entry) {
        return None;
    }
    let before = children[..start]
        .iter()
        .rev()
        .filter_map(|c| match c {
            rowan::NodeOrToken::Node(n) => n.last_token(),
            rowan::NodeOrToken::Token(t) => Some(t.clone()),
        })
        .next();
    if before.is_some_and(|t| t.kind() == SyntaxKind::COMMENT) {
        return None;
    }
    Some(separator)
}
