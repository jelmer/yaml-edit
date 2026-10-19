//! The trivia between collection entries: the line break ending a block
//! collection entry's line, and the separator after a flow collection entry,
//! and adding or removing an entry along with the trivia that belongs to it.
//!
//! A block entry owns the NEWLINE ending its last line, and the comment
//! before it, but the parser puts it deeper when the entry ends in a nested
//! block node, and there is none at the end of unterminated input. A flow
//! entry owns the comma after it and the white space around that.

use super::empty_collection::{detach_empty_collection_placeholder_newline, render_empty};
use super::{
    append_children, fresh_token, has_child_token, insert_children, is_block_scalar,
    replace_children, SyntaxNode, SyntaxToken,
};
use crate::lex::SyntaxKind;

/// The line break ending `entry`'s last line, if it has one.
pub(crate) fn line_break(entry: &SyntaxNode) -> Option<String> {
    last_non_empty_token(entry)
        .filter(|t| t.kind() == SyntaxKind::NEWLINE)
        .map(|t| t.text().to_string())
}

/// Does `node`'s text end in a NEWLINE, ignoring zero-width tokens such as
/// an implicit null after it?
pub(crate) fn ends_with_line_break(node: &SyntaxNode) -> bool {
    last_non_empty_token(node).is_some_and(|t| t.kind() == SyntaxKind::NEWLINE)
}

/// Does `node` end its last line: does it end with a line break, perhaps
/// followed by the spaces of a last line that has none? Those belong to a
/// block scalar's body.
pub(crate) fn ends_line(node: &SyntaxNode) -> bool {
    last_non_empty_token(node).is_some_and(|t| match t.kind() {
        SyntaxKind::NEWLINE => true,
        SyntaxKind::INDENT => t
            .prev_token()
            .is_some_and(|p| p.kind() == SyntaxKind::NEWLINE),
        _ => false,
    })
}

/// Drop the space before `content`, a value or item just written, if it
/// is empty and ends its line, where the space would trail.
pub(crate) fn drop_space_before_empty(content: &SyntaxNode) {
    if !content.text().is_empty() {
        return;
    }
    let ends_line = content
        .last_token()
        .and_then(|t| t.next_token())
        .map_or(true, |t| t.kind() == SyntaxKind::NEWLINE);
    let space = content
        .first_token()
        .and_then(|t| t.prev_token())
        .filter(|t| t.kind() == SyntaxKind::WHITESPACE);
    if let Some(space) = space.filter(|_| ends_line) {
        space.detach();
    }
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
/// The entry before it gets a line break if it lacks one, and the new entry
/// an INDENT at the collection's column (unless it comes first, where the
/// parser has the column outside the collection). Should the new entry end
/// in a block scalar, the lines after it are laid out so that it does not
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
            open_end = !ends_with_line_break(&prev);
            terminate(&prev);
            let mut elements: Vec<_> = indent().into_iter().collect();
            elements.push(entry.clone().into());
            (prev.index() + 1, elements)
        }
        Slot::End => {
            // Blank lines after a nested collection are its parent's, so
            // they stay after the new entry; those ending the document are
            // the root collection's, and stay before it.
            let mut elements = Vec::new();
            if last_entry(collection, kind).is_some() {
                elements.extend(indent());
            } else {
                first = true;
            }
            elements.push(entry.clone().into());
            let (at, opened) = end_line(collection, end_slot_index(collection));
            open_end = opened;
            (at, elements)
        }
    };
    insert_children(collection, at, elements);
    if first {
        detach_empty_collection_placeholder_newline(collection, entry);
    }
    super::settle_lines_after_inserted_entry(entry);
    if open_end {
        unterminate_last(entry);
    }
}

/// The last entry of kind `kind` in `collection`.
fn last_entry(collection: &SyntaxNode, kind: SyntaxKind) -> Option<SyntaxNode> {
    std::iter::successors(collection.last_child(), |n| n.prev_sibling()).find(|n| n.kind() == kind)
}

/// The index in `collection` where an entry after its last one goes: at
/// the end, ahead of the spaces of a last line holding nothing else.
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
            let open = !ends_with_line_break(&entry);
            terminate(&entry);
            (entry.index() + 1, open)
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
    let kind = entry.kind();
    // An entry after another goes where the one after that starts, past
    // the lines between them, or after the last.
    let next = match slot {
        Slot::After(prev) => std::iter::successors(prev.next_sibling(), |n| n.next_sibling())
            .find(|n| n.kind() == kind),
        Slot::Before(next) => Some(next),
        Slot::End => None,
    };
    let at = match (next, last_entry(collection, kind)) {
        (Some(next), _) => next.index(),
        (None, Some(last)) => last.index() + 1,
        (None, None) => collection
            .children_with_tokens()
            .position(|c| {
                matches!(
                    c.kind(),
                    SyntaxKind::RIGHT_BRACE | SyntaxKind::RIGHT_BRACKET
                )
            })
            .unwrap_or_else(|| collection.children_with_tokens().count()),
    };

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
        let mut elements = FlowSeparator::hand_over_end(&last, entries_before.last(), entry);
        elements.push(entry.clone().into());
        insert_children(collection, at, elements);
        return;
    }
    insert_children(collection, at, vec![entry.clone().into()]);
}

/// Splice `entry` out of `collection`, a block or flow collection, with
/// the trivia that belongs to it.
///
/// That is the indentation of its line, and blank lines before it when
/// there are blank lines after it too, so that the gap doesn't double.
/// Comment lines around it stay. A flow entry sharing its line with
/// another or with the closing bracket leaves the indentation, and one
/// ending a line that it does not start takes the next line's, which then
/// joins its line. A flow entry already holds its separator; the new last
/// entry loses the one leading to the removed one, unless that had its own.
pub(crate) fn detach_entry(collection: &SyntaxNode, entry: &SyntaxNode) {
    let kind = entry.kind();
    let is_last = !std::iter::successors(entry.next_sibling(), |n| n.next_sibling())
        .any(|n| n.kind() == kind);
    let prev = std::iter::successors(entry.prev_sibling(), |n| n.prev_sibling())
        .find(|n| n.kind() == kind);
    let terminated = ends_with_line_break(entry);
    let flow = has_child_token(collection, |k| {
        matches!(k, SyntaxKind::LEFT_BRACE | SyntaxKind::LEFT_BRACKET)
    });

    let mut trivia = Vec::new();
    if !flow || terminated {
        // The entry's line goes, unless another entry or the closing
        // bracket shares it. In a flow collection the next line joins the
        // line the entry started on.
        match line_indentation_before(entry) {
            Some(indentation) => trivia.extend(indentation),
            None if flow && terminated => trivia.extend(
                std::iter::successors(entry.next_sibling_or_token(), |c| c.next_sibling_or_token())
                    .take_while(|c| {
                        matches!(c.kind(), SyntaxKind::INDENT | SyntaxKind::WHITESPACE)
                    }),
            ),
            None => {}
        }
        if entry
            .next_sibling_or_token()
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
            // Only the first entry's removal leaves lines at the start;
            // any there already are where the parser put them.
            if prev.is_none() {
                crate::yaml::move_leading_lines_out(collection);
            }
            if is_last {
                move_trailing_lines_out(collection);
            }
        }
        if let Some(prev) = &prev {
            super::replaced_value::settle_lines_after_block_scalar(prev);
        }
    }

    if drained {
        render_empty(collection, line_break(entry).as_deref());
    }
    // The end of the input keeps its lack of a final line break.
    if is_last && !terminated && !flow {
        unterminate_last(collection);
    }
}

/// Move the lines ending `collection`, a nested block collection, out to
/// where the parser puts them: after the entry holding it in the
/// collection that has an entry after that, or the root collection at the
/// end of the input.
fn move_trailing_lines_out(collection: &SyntaxNode) {
    let mut lines: Vec<_> = std::iter::successors(collection.last_child_or_token(), |c| {
        c.prev_sibling_or_token()
    })
    .map_while(|c| c.into_token())
    .collect();
    lines.reverse();
    let holders: Vec<_> = collection
        .ancestors()
        .filter(|n| {
            matches!(
                n.kind(),
                SyntaxKind::MAPPING_ENTRY | SyntaxKind::SEQUENCE_ENTRY
            )
        })
        .collect();
    let Some(holder) = holders
        .iter()
        .find(|e| e.next_sibling().is_some_and(|n| n.kind() == e.kind()))
        .or(holders.last())
    else {
        return;
    };
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

/// The indentation of the line `entry` starts, if it starts one: the
/// INDENT and white space before it back to a line break.
fn line_indentation_before(entry: &SyntaxNode) -> Option<Vec<rowan::SyntaxElement<super::Lang>>> {
    let mut before =
        std::iter::successors(entry.prev_sibling_or_token(), |c| c.prev_sibling_or_token())
            .peekable();
    let mut indentation = Vec::new();
    while let Some(c) =
        before.next_if(|c| matches!(c.kind(), SyntaxKind::INDENT | SyntaxKind::WHITESPACE))
    {
        indentation.push(c);
    }
    let starts_line = match before.next()? {
        rowan::NodeOrToken::Node(n) => ends_with_line_break(&n),
        rowan::NodeOrToken::Token(t) => t.kind() == SyntaxKind::NEWLINE,
    };
    starts_line.then_some(indentation)
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
    // holds its own, as an entry does.
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
    terminate_with(entry, "\n");
}

/// [`terminate`] with `line_break`, which a document without line breaks
/// left keeps rather than `\n`.
pub(crate) fn terminate_with(entry: &SyntaxNode, line_break: &str) {
    if !ends_with_line_break(entry) {
        terminate_last_line(entry, line_break);
    }
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

/// Add the line break that ends the last line of `node`, where the parser
/// puts it when another document follows.
///
/// The parser hands the break to the innermost construct still open at the
/// end of the line: the last block entry, a block scalar or the collection
/// holding a trailing comment line. `node` must not already end in a
/// NEWLINE.
pub(crate) fn terminate_last_line(node: &SyntaxNode, line_break: &str) {
    let newline = vec![fresh_token(SyntaxKind::NEWLINE, line_break).into()];
    append_children(&line_end_holder(node), newline);
}

/// The node the parser puts the line break after `node`'s last line in:
/// the innermost node at its end that holds its line end, or `node` itself.
pub(crate) fn line_end_holder(node: &SyntaxNode) -> SyntaxNode {
    let mut cur = node.clone();
    while let Some(last) = last_line_node(&cur) {
        if !holds_line_end(&last) {
            break;
        }
        cur = last;
    }
    cur
}

/// The child of `node` its last line ends in, if that is a node. An
/// explicit-key entry without a `:` ends in its KEY, before the empty
/// VALUE.
fn last_line_node(node: &SyntaxNode) -> Option<SyntaxNode> {
    let last = node.last_child_or_token()?.into_node()?;
    if last.kind() == SyntaxKind::VALUE && !has_child_token(node, |k| k == SyntaxKind::COLON) {
        return node.children().find(|c| c.kind() == SyntaxKind::KEY);
    }
    Some(last)
}

/// Does the parser put the line break after `node`'s last line inside it?
fn holds_line_end(node: &SyntaxNode) -> bool {
    match node.kind() {
        SyntaxKind::MAPPING | SyntaxKind::SEQUENCE => !has_child_token(node, |k| {
            matches!(k, SyntaxKind::LEFT_BRACE | SyntaxKind::LEFT_BRACKET)
        }),
        SyntaxKind::MAPPING_ENTRY | SyntaxKind::SEQUENCE_ENTRY => true,
        SyntaxKind::SCALAR => is_block_scalar(node),
        SyntaxKind::KEY | SyntaxKind::VALUE | SyntaxKind::TAGGED_NODE => {
            node.last_child().is_some_and(|last| holds_line_end(&last))
        }
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
        separate_comma_from_indicator(entry);
        true
    }

    /// Append `, ` to `entry` unless it already has a comma.
    pub(crate) fn ensure(entry: &SyntaxNode) {
        if !has_comma(entry) {
            append_children(entry, Self::comma_space().build());
            separate_comma_from_indicator(entry);
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
        separate_comma_from_indicator(entry);
    }

    /// Drop the comma at the end of `entry`, which has become the last
    /// entry. White space and line breaks after the comma go too, up to
    /// the end of the collection, unless a comment follows it.
    pub(crate) fn strip(entry: &SyntaxNode) {
        let Some(comma) = entry
            .children_with_tokens()
            .filter_map(|c| c.into_token())
            .filter(|t| t.kind() == SyntaxKind::COMMA)
            .last()
        else {
            return;
        };
        let after_entry =
            std::iter::successors(entry.next_sibling_or_token(), |c| c.next_sibling_or_token())
                .map_while(|c| c.into_token())
                .take_while(|t| {
                    matches!(
                        t.kind(),
                        SyntaxKind::WHITESPACE | SyntaxKind::NEWLINE | SyntaxKind::INDENT
                    )
                });
        let mut tail = vec![comma.clone()];
        let mut next = comma.next_sibling_or_token();
        while let Some(c) = next {
            let Some(t) = c.into_token() else { break };
            next = t.next_sibling_or_token();
            tail.push(t);
        }
        tail.extend(after_entry);
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
        // The entry now ends with the line its content ends; the lines
        // after that, which led up to the comma, are the collection's.
        let content_end = entry.children().last().map_or(0, |n| n.index() + 1);
        let lines: Vec<_> = entry
            .children_with_tokens()
            .skip(content_end)
            .skip_while(|c| c.kind() != SyntaxKind::NEWLINE)
            .skip(1)
            .collect();
        for c in &lines {
            c.detach();
        }
        let parent = entry.parent().expect("a flow entry is in a collection");
        insert_children(
            &parent,
            entry.index() + 1,
            lines
                .iter()
                .map(|c| fresh_token(c.kind(), &c.to_string()).into())
                .collect(),
        );
    }

    /// Make room for `new_entry` after `last`, the last entry of a flow
    /// collection, keeping the layout of the end of the collection:
    /// `new_entry` takes over the separator `last` ends with, trailing comma
    /// included, and `last` gets the one between the entries before it, or
    /// `, ` if there is none. `before_last` is the entry before `last`.
    /// Returns the indentation to put before `new_entry`.
    fn hand_over_end(
        last: &SyntaxNode,
        before_last: Option<&SyntaxNode>,
        new_entry: &SyntaxNode,
    ) -> Vec<rowan::SyntaxElement<super::Lang>> {
        let Some(separator) = separator_tokens(last) else {
            // A comment ends the line of `last`, which keeps it; the new
            // entry starts the line after, ahead of the closing bracket.
            let indentation: Vec<_> =
                std::iter::successors(last.next_sibling_or_token(), |c| c.next_sibling_or_token())
                    .take_while(|c| matches!(c.kind(), SyntaxKind::INDENT | SyntaxKind::WHITESPACE))
                    .collect();
            for c in &indentation {
                c.detach();
            }
            if has_comma(last) {
                append_children(new_entry, vec![fresh_token(SyntaxKind::COMMA, ",").into()]);
                return indentation;
            }
            let mut separator = indentation;
            separator.push(fresh_token(SyntaxKind::COMMA, ",").into());
            separator.push(fresh_token(SyntaxKind::WHITESPACE, " ").into());
            append_children(last, separator);
            return Vec::new();
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
        separate_comma_from_indicator(last);
        // A new line is indented as the last entry's.
        if !ends_with_line_break(last) {
            return Vec::new();
        }
        let indentation = line_indentation_before(last).unwrap_or_default();
        indentation
            .iter()
            .rev()
            .map(|c| fresh_token(c.kind(), &c.to_string()).into())
            .collect()
    }
}

/// Put a space between the comma of `entry` and a `?` right before it.
fn separate_comma_from_indicator(entry: &SyntaxNode) {
    let comma = entry
        .children_with_tokens()
        .filter_map(|c| c.into_token())
        .find(|t| t.kind() == SyntaxKind::COMMA);
    if let Some(comma) = comma {
        separate_from_question(&comma);
    }
}

/// Put a space after the `?` that `token`, a `,` or `:` in a flow entry,
/// comes right after: a `?` only opens an explicit key when white space
/// follows it.
pub(crate) fn separate_from_question(token: &SyntaxToken) {
    let question = std::iter::successors(token.prev_token(), |t| t.prev_token())
        .find(|t| !t.text().is_empty())
        .filter(|t| t.kind() == SyntaxKind::QUESTION);
    if let Some(question) = question {
        insert_children(
            &question.parent().expect("a token has a parent"),
            question.index() + 1,
            vec![fresh_token(SyntaxKind::WHITESPACE, " ").into()],
        );
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
