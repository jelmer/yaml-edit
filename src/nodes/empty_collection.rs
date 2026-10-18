//! Empty collections. YAML has no block syntax for a mapping or sequence
//! without entries, so an empty one is written `{}` or `[]`: a block
//! collection that loses its last entry turns into that, and an empty
//! placeholder turns back into a block collection when it gets its first.

use super::{
    append_children, fresh_token, has_child_token, insert_children, replace_children, trivia,
    SyntaxNode, SyntaxToken,
};
use crate::lex::SyntaxKind;
use rowan::ast::AstNode;

type Element = rowan::NodeOrToken<SyntaxNode, SyntaxToken>;

/// Write `collection`, a block MAPPING or SEQUENCE left without entries,
/// as `{}` or `[]`, so that it does not read back as null or merge into
/// what follows. Does nothing to a flow collection or one with entries.
///
/// The collection itself becomes the flow-empty, rather than a new node, so
/// handles to it stay attached. As a mapping value it moves up onto the
/// key's line, with its node properties and a comment ending that line:
/// `key: !t {}  # c`. Elsewhere, as a sequence item, explicit key or
/// document root, it stays where it is. Comment lines are kept.
///
/// `terminated` says whether the collection's last line ended in a line
/// break before it was drained; if so, the flow-empty line gets one too.
///
/// The result carries the pending-block marker, so that the collection
/// returns to block style when it gets an entry again.
pub(crate) fn render_empty(collection: &SyntaxNode, terminated: bool) {
    let Some((open, _)) = brackets(collection.kind()) else {
        return;
    };
    if has_child_token(collection, |k| k == open.0)
        || collection.children().any(|c| {
            matches!(
                c.kind(),
                SyntaxKind::MAPPING_ENTRY | SyntaxKind::SEQUENCE_ENTRY
            )
        })
    {
        return;
    }
    let node = collection
        .parent()
        .filter(|p| p.kind() == SyntaxKind::TAGGED_NODE)
        .unwrap_or_else(|| collection.clone());
    match node.parent().map(|p| p.kind()) {
        Some(SyntaxKind::VALUE) => render_as_value(collection, terminated),
        Some(SyntaxKind::SEQUENCE_ENTRY | SyntaxKind::KEY | SyntaxKind::DOCUMENT) | None => {
            render_in_place(collection, terminated)
        }
        Some(kind) => panic!("unexpected {kind:?} holding a block collection"),
    }
}

/// The flow-empty brackets for a collection of `kind`, if it is one.
fn brackets(kind: SyntaxKind) -> Option<((SyntaxKind, &'static str), (SyntaxKind, &'static str))> {
    match kind {
        SyntaxKind::MAPPING => Some((
            (SyntaxKind::LEFT_BRACE, "{"),
            (SyntaxKind::RIGHT_BRACE, "}"),
        )),
        SyntaxKind::SEQUENCE => Some((
            (SyntaxKind::LEFT_BRACKET, "["),
            (SyntaxKind::RIGHT_BRACKET, "]"),
        )),
        _ => None,
    }
}

/// Replace the contents of the drained `collection` with flow-empty
/// brackets, returning the comment lines and line breaks it held.
fn replace_with_brackets(collection: &SyntaxNode) -> Vec<SyntaxToken> {
    let leftovers: Vec<_> = collection
        .children_with_tokens()
        .map(|el| match el {
            rowan::NodeOrToken::Token(t) => t,
            rowan::NodeOrToken::Node(n) => {
                panic!("unexpected {:?} left in an empty collection", n.kind())
            }
        })
        .collect();
    if let Some(t) = leftovers.iter().find(|t| {
        !matches!(
            t.kind(),
            SyntaxKind::COMMENT | SyntaxKind::INDENT | SyntaxKind::WHITESPACE | SyntaxKind::NEWLINE
        )
    }) {
        panic!("unexpected {:?} left in an empty collection", t.kind());
    }
    let (open, close) = brackets(collection.kind()).expect("a collection");
    replace_children(
        collection,
        0..leftovers.len(),
        vec![
            fresh_token(open.0, open.1).into(),
            pending_block_marker().into(),
            fresh_token(close.0, close.1).into(),
        ],
    );
    leftovers
}

fn copies(tokens: &[SyntaxToken]) -> Vec<Element> {
    tokens
        .iter()
        .map(|t| fresh_token(t.kind(), t.text()).into())
        .collect()
}

/// [`render_empty`] for a collection that is a sequence item, explicit key,
/// document root or detached.
fn render_in_place(collection: &SyntaxNode, terminated: bool) {
    // Entries take their trailing NEWLINE with them when removed. Comment
    // lines that followed an entry stay after the flow-empty line,
    // indented as they were.
    let leftovers = replace_with_brackets(collection);
    let tail_start = leftovers
        .iter()
        .position(|t| t.kind() == SyntaxKind::COMMENT)
        .map(|i| {
            if i > 0 && leftovers[i - 1].kind() != SyntaxKind::NEWLINE {
                i - 1
            } else {
                i
            }
        })
        .unwrap_or(leftovers.len());
    let tail = copies(&leftovers[tail_start..]);

    indent_past_question(collection);

    if !terminated && tail.is_empty() {
        return;
    }
    // The line break follows the node, properties included, as a parsed
    // flow-empty line has it. It ends an explicit-key entry without a `:`,
    // and its comment lines follow that in the mapping.
    let node = line_node(collection);
    if node.kind() == SyntaxKind::MAPPING_ENTRY {
        append_children(&node, vec![fresh_token(SyntaxKind::NEWLINE, "\n").into()]);
        match node.parent() {
            Some(mapping) => insert_children(&mapping, node.index() + 1, tail),
            None => append_children(&node, tail),
        }
        return;
    }
    let Some(parent) = node.parent() else {
        // A detached collection has no line of its own; it keeps its
        // comment lines at its end.
        if !tail.is_empty() {
            let end = node.children_with_tokens().count();
            let mut lines = vec![fresh_token(SyntaxKind::NEWLINE, "\n").into()];
            lines.extend(tail);
            insert_children(&node, end, lines);
        }
        return;
    };
    let at = node.index() + 1;
    insert_children(
        &parent,
        at,
        vec![fresh_token(SyntaxKind::NEWLINE, "\n").into()],
    );
    if tail.is_empty() {
        return;
    }
    // A parsed comment line after a sequence item belongs to the sequence;
    // a detached item keeps it at its end.
    let (host, at) = match parent.parent() {
        Some(sequence) if parent.kind() == SyntaxKind::SEQUENCE_ENTRY => {
            (sequence, parent.index() + 1)
        }
        _ => (parent, at + 1),
    };
    insert_children(&host, at, tail);
}

/// [`render_empty`] for a mapping value: replace the `NEWLINE INDENT
/// COLLECTION` scaffold with an inline ` {}` or ` []`, so the entry
/// renders as `key: {}` instead of an unterminated `key:\n    `. With
/// comment lines before or inside the collection, which cannot share the
/// key's line, the flow-empty takes the collection's place instead.
fn render_as_value(collection: &SyntaxNode, terminated: bool) {
    // A tag sits between the VALUE and the collection; the rewrite
    // happens inside the TAGGED_NODE so the tag survives.
    let host = collection.parent().expect("a mapping value has a parent");
    let value_node = if host.kind() == SyntaxKind::TAGGED_NODE {
        host.parent().expect("a tagged mapping value has a VALUE")
    } else {
        host.clone()
    };
    let entry_node = value_node
        .parent()
        .filter(|p| p.kind() == SyntaxKind::MAPPING_ENTRY)
        .expect("a VALUE is in a MAPPING_ENTRY");

    let mut properties = Vec::new();
    let mut comment = None;
    let mut comment_lines = false;
    let mut seen_newline = false;
    for el in host.children_with_tokens() {
        let token = match el {
            rowan::NodeOrToken::Node(n) if &n == collection => continue,
            rowan::NodeOrToken::Node(n) => {
                panic!("unexpected {:?} next to a block collection", n.kind())
            }
            rowan::NodeOrToken::Token(t) => t,
        };
        match token.kind() {
            SyntaxKind::TAG | SyntaxKind::ANCHOR => properties.push(token),
            SyntaxKind::COMMENT if seen_newline => comment_lines = true,
            SyntaxKind::COMMENT => comment = Some(token),
            SyntaxKind::NEWLINE => seen_newline = true,
            SyntaxKind::INDENT | SyntaxKind::WHITESPACE => {}
            kind => panic!("unexpected {kind:?} next to a block collection"),
        }
    }

    // Comment lines left inside the drained collection are kept too,
    // minus the indentation and line ends around them.
    let leftovers = replace_with_brackets(collection);
    let start = leftovers
        .iter()
        .position(|t| !matches!(t.kind(), SyntaxKind::INDENT | SyntaxKind::WHITESPACE))
        .unwrap_or(leftovers.len());
    let end = leftovers
        .iter()
        .rposition(|t| {
            !matches!(
                t.kind(),
                SyntaxKind::INDENT | SyntaxKind::WHITESPACE | SyntaxKind::NEWLINE
            )
        })
        .map_or(start, |i| i + 1);
    let inner = &leftovers[start..end];

    if comment_lines || !inner.is_empty() {
        let mut replacement = copies(inner);
        if !inner.is_empty() {
            let indent = collection
                .prev_sibling_or_token()
                .and_then(|el| el.into_token())
                .filter(|t| t.kind() == SyntaxKind::INDENT)
                .expect("block collection under a comment line is indented");
            replacement.push(fresh_token(SyntaxKind::NEWLINE, "\n").into());
            replacement.push(fresh_token(SyntaxKind::INDENT, indent.text()).into());
        }
        replacement.push(collection.clone().into());
        let index = collection.index();
        replace_children(&host, index..index + 1, replacement);
        if terminated {
            trivia::terminate(&entry_node);
        }
        return;
    }

    // Keep the host node and its properties, and replace everything after
    // them (comment, line break, indent and the block collection) with the
    // flow-empty collection.
    let start = properties.last().map_or(0, |p| p.index() + 1);
    let end = host.children_with_tokens().count();
    // Each separator must be its own token: rowan cannot attach one
    // mutable token in two places.
    let ws = || fresh_token(SyntaxKind::WHITESPACE, " ");
    let has_properties = !properties.is_empty();
    let mut replacement: Vec<Element> = Vec::new();
    if has_properties {
        replacement.push(ws().into());
    }
    replacement.push(collection.clone().into());
    let mut comment: Vec<Element> = comment
        .into_iter()
        .flat_map(|c| {
            [
                ws().into(),
                fresh_token(SyntaxKind::COMMENT, c.text()).into(),
            ]
        })
        .collect();
    // A comment after a TAGGED_NODE belongs to the VALUE around it.
    if host.kind() != SyntaxKind::TAGGED_NODE {
        replacement.append(&mut comment);
    }
    replace_children(&host, start..end, replacement);
    if !comment.is_empty() {
        let after_host = host.index() + 1;
        insert_children(&value_node, after_host, comment);
    }

    // The space separating the colon from its value belongs to the
    // entry, not the value: a parsed `key: {}` puts it between COLON
    // and VALUE. Left inside, a later edit that replaces the whole
    // VALUE took the space with it and wrote `key:value`, which is a
    // plain scalar rather than an entry.
    if !has_properties {
        let index = value_node.index();
        let separated = value_node
            .prev_sibling_or_token()
            .is_some_and(|el| el.kind() == SyntaxKind::WHITESPACE);
        if !separated {
            insert_children(&entry_node, index, vec![ws().into()]);
        }
    }

    if terminated {
        trivia::terminate(&entry_node);
    }
}

/// A block sequence in an explicit key may start on the line after the
/// `?`, at the `?`'s own column, but a flow collection there would end the
/// key. Indent `collection` past the `?` if it sits there.
fn indent_past_question(collection: &SyntaxNode) {
    let Some(host) = collection.parent() else {
        return;
    };
    let Some(key) = host
        .ancestors()
        .find(|n| n.kind() != SyntaxKind::TAGGED_NODE)
        .filter(|n| n.kind() == SyntaxKind::KEY)
    else {
        return;
    };
    let question = key
        .parent()
        .and_then(|e| {
            e.children_with_tokens()
                .filter_map(|el| el.into_token())
                .find(|t| t.kind() == SyntaxKind::QUESTION)
        })
        .expect("explicit key has a ?");
    let Some(prev) = collection
        .prev_sibling_or_token()
        .and_then(|el| el.into_token())
    else {
        return;
    };
    let indent = match prev.kind() {
        SyntaxKind::NEWLINE => None,
        SyntaxKind::INDENT => Some(prev),
        _ => return,
    };
    let column = crate::column::token_column(&question);
    if indent
        .as_ref()
        .is_some_and(|t| t.text().chars().count() > column)
    {
        return;
    }
    let new_indent = fresh_token(SyntaxKind::INDENT, &" ".repeat(column + 2));
    let at = collection.index();
    match indent {
        Some(t) => replace_children(&host, t.index()..at, vec![new_indent.into()]),
        None => insert_children(&host, at, vec![new_indent.into()]),
    }
}

/// The node that carries `collection`'s line: the collection itself, the
/// TAGGED_NODE around it, or for an explicit key the KEY, or the entry if
/// that has no `:`.
fn line_node(collection: &SyntaxNode) -> SyntaxNode {
    let node = collection
        .parent()
        .filter(|p| p.kind() == SyntaxKind::TAGGED_NODE)
        .unwrap_or_else(|| collection.clone());
    let Some(key) = node.parent().filter(|p| p.kind() == SyntaxKind::KEY) else {
        return node;
    };
    let entry = key.parent().expect("a KEY is in an entry");
    if has_child_token(&entry, |k| k == SyntaxKind::COLON) {
        key
    } else {
        entry
    }
}

/// Detach the line break that ended the line of `empty_collection`, now
/// that `new_entry` is its first entry: the enclosing MAPPING_ENTRY's
/// trailing NEWLINE for a mapping value (a `key:\n    \n` or `key: {}`
/// line, see issue #18), or the NEWLINE after a collection that was a
/// pending-block `{}` or `[]` elsewhere. A root collection takes back the
/// lines after it, which were the document's while it was `{}` or `[]`.
///
/// The freshly built `new_entry` must be checked because it only supplies
/// its own trailing NEWLINE for some shapes; when it does not, the
/// placeholder is still needed to terminate the line.
pub(crate) fn detach_empty_collection_placeholder_newline(
    empty_collection: &SyntaxNode,
    new_entry: &SyntaxNode,
) {
    if !trivia::line_end(new_entry).is_terminated() {
        return;
    }
    let node = line_node(empty_collection);
    let Some(parent) = node.parent() else {
        return;
    };
    let line_break = if parent.kind() == SyntaxKind::VALUE {
        let entry = parent
            .parent()
            .filter(|e| e.kind() == SyntaxKind::MAPPING_ENTRY)
            .expect("a VALUE is in a MAPPING_ENTRY");
        entry.last_child_or_token()
    } else if node.kind() == SyntaxKind::MAPPING_ENTRY {
        node.last_child_or_token()
    } else {
        node.next_sibling_or_token()
    };
    if let Some(line_break) = line_break.filter(|t| t.kind() == SyntaxKind::NEWLINE) {
        line_break.detach();
    }
    // The lines after a root block collection up to `...` are its own.
    if parent.kind() == SyntaxKind::DOCUMENT {
        let lines: Vec<_> =
            std::iter::successors(node.next_sibling_or_token(), |c| c.next_sibling_or_token())
                .take_while(|c| c.kind() != SyntaxKind::DOC_END)
                .collect();
        for c in &lines {
            c.detach();
        }
        super::append_children(empty_collection, lines);
    }
}

/// The zero-width token inside `{}` or `[]` that marks an empty collection
/// as headed for block style; see
/// [`Mapping::new_pending_block`](crate::Mapping::new_pending_block).
fn pending_block_marker() -> SyntaxToken {
    fresh_token(SyntaxKind::WHITESPACE, "")
}

/// Is `collection` an empty mapping or sequence awaiting its first entry?
/// See [`Mapping::new_pending_block`](crate::Mapping::new_pending_block).
pub(crate) fn is_block_placeholder(collection: &SyntaxNode) -> bool {
    !collection.children().any(|c| {
        matches!(
            c.kind(),
            SyntaxKind::MAPPING_ENTRY | SyntaxKind::SEQUENCE_ENTRY
        )
    }) && collection
        .children_with_tokens()
        .filter_map(|c| c.into_token())
        .any(|t| t.kind() == SyntaxKind::WHITESPACE && t.text().is_empty())
}

/// Turn `collection` into a block collection if it is an empty placeholder
/// and nothing forces it to stay flow, before its first entry is added:
/// drop its flow delimiters. As a mapping value, it moves to the line
/// after the key, which keeps its node properties and comment:
/// `key: !t {}  # c` becomes `key: !t  # c\n  `, ready for the entry.
/// Elsewhere the entry takes the place of the brackets.
pub(crate) fn convert_placeholder_to_block(collection: &SyntaxNode) {
    if !is_block_placeholder(collection) || super::sequence::must_render_flow(collection) {
        return;
    }
    let children: Vec<_> = collection.children_with_tokens().collect();
    for child in children {
        child.detach();
    }

    let Some(host) = collection.parent() else {
        return;
    };
    let node = if host.kind() == SyntaxKind::TAGGED_NODE {
        host.clone()
    } else {
        collection.clone()
    };
    let Some(value) = node.parent().filter(|p| p.kind() == SyntaxKind::VALUE) else {
        return;
    };
    let pos = collection.index();
    let already_scaffolded = pos >= 1
        && host
            .children_with_tokens()
            .nth(pos - 1)
            .is_some_and(|c| matches!(c.kind(), SyntaxKind::INDENT | SyntaxKind::NEWLINE));
    if already_scaffolded {
        return;
    }
    let entry = value
        .parent()
        .filter(|e| e.kind() == SyntaxKind::MAPPING_ENTRY)
        .expect("a VALUE is in a MAPPING_ENTRY");
    let indent_width = entry
        .parent()
        .filter(|m| m.kind() == SyntaxKind::MAPPING)
        .and_then(crate::nodes::Mapping::cast)
        .map_or(2, |m| m.detect_indentation_level() + 2);

    // The comment ending the line moves before the block, where the parser
    // puts it in `key: # c\n  a: 1`.
    let after: Vec<_> = value
        .children_with_tokens()
        .skip(node.index() + 1)
        .collect();
    let comment = after
        .iter()
        .filter_map(|el| el.as_token())
        .find(|t| t.kind() == SyntaxKind::COMMENT)
        .map(|t| fresh_token(SyntaxKind::COMMENT, t.text()));
    for el in after {
        el.detach();
    }
    let space_before = collection
        .prev_sibling_or_token()
        .filter(|el| el.kind() == SyntaxKind::WHITESPACE);
    let has_properties = host
        .children_with_tokens()
        .any(|el| matches!(el.kind(), SyntaxKind::TAG | SyntaxKind::ANCHOR));
    if let Some(space) = space_before {
        space.detach();
    }

    let mut scaffold: Vec<Element> = Vec::new();
    if let Some(comment) = comment {
        if has_properties {
            scaffold.push(fresh_token(SyntaxKind::WHITESPACE, " ").into());
        }
        scaffold.push(comment.into());
    } else if !has_properties {
        // Nothing is left on the key's line after the colon, so the space
        // separating it from the value goes too.
        let index = value.index();
        let separator = entry
            .children_with_tokens()
            .nth(index.wrapping_sub(1))
            .filter(|el| index > 0 && el.kind() == SyntaxKind::WHITESPACE);
        if let Some(separator) = separator {
            separator.detach();
        }
    }
    scaffold.push(fresh_token(SyntaxKind::NEWLINE, "\n").into());
    scaffold.push(fresh_token(SyntaxKind::INDENT, &" ".repeat(indent_width)).into());
    let pos = collection.index();
    insert_children(&host, pos, scaffold);
}
