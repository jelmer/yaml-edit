//! AST node types for YAML.
//!
//! # CST invariants
//!
//! yaml-edit is a lossless editor: every mutation produces text that
//! re-parses into the same tree shape (see [`crate::debug::roundtrip_ok`]
//! and [`crate::debug::validate_tree`]). To keep that property, all
//! mutation helpers in this module must uphold the following invariants
//! on the concrete syntax tree.
//!
//! ## Where whitespace lives
//!
//! For a block-style mapping entry `key:\n  - a`, the NEWLINE after
//! `key:` and the INDENT before the SEQUENCE both live inside the parent
//! VALUE, *not* inside the child SEQUENCE. The SEQUENCE_ENTRYs
//! themselves carry only the DASH, WHITESPACE, and content:
//!
//! ```text
//! MAPPING_ENTRY
//!   KEY ...
//!   COLON ":"
//!   VALUE
//!     NEWLINE "\n"
//!     INDENT "  "
//!     SEQUENCE
//!       SEQUENCE_ENTRY
//!         DASH "-"
//!         WHITESPACE " "
//!         SCALAR ...
//! ```
//!
//! Consequence: when constructing a fresh block SEQUENCE or MAPPING to
//! splice under a key, do **not** prepend its own leading INDENT for the
//! first entry - the parent VALUE already carries one. Duplicating it
//! renders as doubled indentation and violates the no-stacked-INDENT
//! check in [`crate::debug::validate_tree`].
//!
//! For flow-style values (`{a: 1}`, `[1, 2]`) all separators live
//! *inside* the flow SEQUENCE / MAPPING: the parent VALUE has no direct
//! NEWLINE child. `value_is_block` in `mapping.rs` distinguishes the two
//! by looking for a NEWLINE token as a direct child of VALUE.
//!
//! The space after the colon belongs to MAPPING_ENTRY, before VALUE,
//! including for block values that start on the key line with an anchor,
//! tag or block scalar indicator (`key: &a\n  - a`, `key: |\n  text`).
//!
//! ## Entry termination
//!
//! Block-style MAPPING_ENTRY and SEQUENCE_ENTRY nodes are terminated by
//! a NEWLINE. That terminator normally lives as the entry's own last
//! token, but the parser sometimes lifts it out when a trailing comment
//! separates it from the next sibling:
//!
//! ```text
//! # SEQUENCE_ENTRY ends with STRING; the NEWLINE lives at the SEQUENCE
//! # level after the following COMMENT.
//! SEQUENCE
//!   SEQUENCE_ENTRY
//!     DASH "-"
//!     SCALAR ("a")
//!   WHITESPACE "  "
//!   COMMENT "# note"
//!   NEWLINE "\n"
//!   SEQUENCE_ENTRY ...
//! ```
//!
//! An explicit-key entry (`? key\n: value\n`) never owns the NEWLINE
//! ending its value line: the parser leaves it to the MAPPING, right
//! after the entry. See [`hand_line_break_to_mapping`].
//!
//! Two carve-outs:
//! - The **last** entry of a block collection may lack a terminator
//!   (unterminated sources like `a: 1\nb: 2` are legitimate YAML and
//!   must roundtrip).
//! - An entry ending in an **implicit-null** scalar (see below) has a
//!   zero-width `SCALAR { NULL "" }` as its last leaf, not a NEWLINE.
//!   That's a well-formed terminated entry; the NEWLINE lives elsewhere
//!   in the tree (inside the KEY subtree for `? key\n`, inside the VALUE
//!   between COLON and SCALAR for `key:\n`).
//!
//! When a comment line is indented deeper than the collection it
//! follows, the parser lifts the COMMENT and its NEWLINE into the parent
//! collection but leaves the comment line's INDENT as the last child of
//! the inner collection:
//!
//! ```text
//! # m:\n  a: 1\n    # c\nz: 1\n
//! MAPPING
//!   MAPPING_ENTRY ("m")
//!     VALUE
//!       ...
//!       MAPPING
//!         MAPPING_ENTRY ("a: 1\n")
//!         INDENT "    "
//!   COMMENT "# c"
//!   NEWLINE "\n"
//!   MAPPING_ENTRY ("z") ...
//! ```
//!
//! So a trailing INDENT in a collection is not necessarily at that
//! collection's indentation; don't reuse it for a new entry without
//! checking its width.
//!
//! When inserting a new sibling entry after an existing one, mutation
//! helpers must first ensure the predecessor is terminated
//! (`ensure_trailing_newline` in this module).
//!
//! ## Document-level trivia
//!
//! Trivia outside the root node lives in DOCUMENT: a leading BOM and
//! blank lines before the root, and the NEWLINE and blank lines after a
//! scalar or flow-collection root. When a comment or directive comes
//! first in the stream, the trivia before it is a child of the file root
//! instead, ahead of the first DOCUMENT. A BOM must stay first in the
//! stream, so `---` and directives are inserted after it.
//!
//! ## Implicit-null values
//!
//! Every `MAPPING_ENTRY` and `SEQUENCE_ENTRY` holds a value node, even
//! when the source omits it. The parser emits a zero-width
//! `SCALAR { NULL "" }` at every "missing value" position:
//!
//! ```text
//! Source form                     CST shape
//! ------------------------------- ----------------------------------------
//! key:\n     (block mapping)      VALUE { NEWLINE, SCALAR { NULL "" } }
//! key:       (block mapping, EOF) VALUE { SCALAR { NULL "" } }
//! {key}      (flow mapping)       MAPPING_ENTRY { KEY { SCALAR "key" },
//!                                                 VALUE { SCALAR { NULL "" } } }
//! {key:}     (flow mapping)       MAPPING_ENTRY { KEY { SCALAR "key" },
//!                                                 COLON,
//!                                                 VALUE { SCALAR { NULL "" } } }
//! {,}        (flow mapping)       MAPPING_ENTRY { KEY   { SCALAR { NULL "" } },
//!                                                 VALUE { SCALAR { NULL "" } },
//!                                                 COMMA }
//! {: v}      (flow mapping)       KEY { SCALAR { NULL "" } }, COLON, VALUE ...
//! ? key\n    (explicit key)       MAPPING_ENTRY { QUESTION, KEY ..., NEWLINE,
//!                                                 VALUE { SCALAR { NULL "" } } }
//! - \n       (block sequence)     SEQUENCE_ENTRY { DASH, WHITESPACE,
//!                                                  SCALAR { NULL "" }, NEWLINE }
//! [a, , c]   (flow sequence)      SEQUENCE_ENTRY { SCALAR { NULL "" }, COMMA, ... }
//! - !e\n     (tag, no content)    TAGGED_NODE { TAG, SCALAR { NULL "" } }
//! ```
//!
//! The invariant: **every KEY, every VALUE, every SEQUENCE_ENTRY holds
//! exactly one scalar or collection node.** Mappings wrap the value in a
//! `VALUE` node; sequences hold the scalar directly as a
//! `SEQUENCE_ENTRY` child (matching how each shape wraps its non-null
//! values -- sequences never had a wrapper, mappings always did).
//!
//! The zero-width `NULL ""` token renders as nothing, so round-trips are
//! lossless. This is distinct from a programmatically-written null,
//! which uses `SCALAR { NULL "null" }` (textual, four characters) so
//! the value is visible in the output.
//!
//! Consequences for mutation helpers:
//!
//! - Iterating `SEQUENCE_ENTRY` children with `matches!(kind, SCALAR |
//!   MAPPING | SEQUENCE | ALIAS | TAGGED_NODE)` finds the value in
//!   every entry, including implicit-null ones. `len` / `get` /
//!   `set` / `remove` therefore share the same set of indexes.
//! - `Scalar::is_null()` returns `true` for the zero-width form.
//! - `set(i, real_value)` on an implicit-null entry may need to insert
//!   a separating WHITESPACE that the original didn't have (the value
//!   was zero-width so no separator was needed). See `Sequence::set`
//!   for the `after_dash` pattern used in block sequences.
//!
//! ## No stacked INDENTs, no double trailing NEWLINEs
//!
//! Two adjacent INDENT tokens as direct children of the same node
//! concatenate at render time and produce visibly-wrong indentation
//! (this was the shape of issue #38). Two adjacent NEWLINE tokens at the
//! tail of a MAPPING_ENTRY or SEQUENCE_ENTRY render as a stray blank
//! line (issue #18).
//!
//! Both are checked by [`crate::debug::validate_tree`]; mutation helpers
//! must not produce either. Note the check for double NEWLINE is scoped
//! to *entries*, not their parent MAPPING / SEQUENCE containers: bare
//! NEWLINEs between entries (blank-line separators) are valid formatting
//! and appear at the container level.
//!
//! ## Flow separators live on the previous entry
//!
//! Inside a flow mapping or sequence, the COMMA (and any following
//! WHITESPACE / NEWLINE / INDENT) between two entries is stored as
//! trailing siblings of the *previous* entry's KEY / VALUE / SCALAR,
//! not inside the entry it precedes. Inserters that append a new entry
//! must add a `, ` suffix to the entry they follow, not a `,` prefix on
//! themselves.
//!
//! ## Prefer targeted splices over whole-node rebuilds
//!
//! Reconstructing an entire MAPPING or SEQUENCE from a builder discards
//! anchors, tags, comments, quoting styles, and any tokens the fix
//! didn't know about. Mutation helpers should locate the specific
//! `NodeOrToken` range they need to change and use `splice_children`,
//! preserving the surrounding tokens.

use crate::lex::SyntaxKind;

/// YAML language type for rowan.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Lang {}

impl rowan::Language for Lang {
    type Kind = SyntaxKind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        // SAFETY: SyntaxKind is #[repr(u16)] with contiguous discriminants
        // from ROOT = 0 through EOF, so every value in that range is a valid
        // variant. The bound is checked in every build, not just debug: a
        // stray value here would otherwise be undefined behaviour rather
        // than a panic.
        assert!(
            raw.0 <= SyntaxKind::EOF as u16,
            "raw SyntaxKind value {} is out of range (max {})",
            raw.0,
            SyntaxKind::EOF as u16,
        );
        unsafe { std::mem::transmute::<u16, SyntaxKind>(raw.0) }
    }

    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        kind.into()
    }
}

pub type SyntaxNode = rowan::SyntaxNode<Lang>;
pub type SyntaxToken = rowan::SyntaxToken<Lang>;

/// Detach `anchor` with the whitespace separating it from what follows it
/// on the same line, or else the space before it, so `k: &a 1` becomes
/// `k: 1` and `k: &a` followed by a block becomes `k:`.
pub(crate) fn detach_anchor(anchor: &SyntaxToken) {
    let separator = anchor
        .next_token()
        .filter(|t| t.kind() == SyntaxKind::WHITESPACE)
        .or_else(|| {
            anchor
                .prev_token()
                .filter(|t| t.kind() == SyntaxKind::WHITESPACE)
        });
    if let Some(separator) = separator {
        separator.detach();
    }
    anchor.detach();
}

/// Build a standalone `SyntaxToken` of `kind` with `text`, ready to
/// splice into a parent's child list via `splice_children`.
pub(crate) fn fresh_token(kind: SyntaxKind, text: &str) -> SyntaxToken {
    let mut builder = rowan::GreenNodeBuilder::new();
    builder.start_node(SyntaxKind::ROOT.into());
    builder.token(kind.into(), text);
    builder.finish_node();
    SyntaxNode::new_root_mut(builder.finish())
        .first_token()
        .expect("just built a token")
}

/// The first direct child node of `parent` with the given `kind`, if any.
pub(crate) fn child_of_kind(parent: &SyntaxNode, kind: SyntaxKind) -> Option<SyntaxNode> {
    parent.children().find(|n| n.kind() == kind)
}

/// Does `parent` have a direct child token whose kind satisfies `pred`?
///
/// Flow/block style and several syntactic properties are decided by the
/// presence of a marker token (`{`, `[`, `|`, `:`) directly under a node,
/// never a nested one, so the search deliberately stays one level deep.
pub(crate) fn has_child_token(parent: &SyntaxNode, pred: impl Fn(SyntaxKind) -> bool) -> bool {
    parent
        .children_with_tokens()
        .any(|el| el.as_token().is_some_and(|t| pred(t.kind())))
}

/// Drop the `,` separator at the end of a flow collection entry that has
/// become the last one. Whitespace and line breaks after the comma go too,
/// unless a comment follows it.
pub(crate) fn strip_flow_separator(entry: &SyntaxNode) {
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

/// Replace the children of `parent` in `range` with `children`.
///
/// rowan 0.16's `SyntaxNode::splice_children` detaches the range while
/// iterating over it, so it stops after the first element and leaves the
/// rest in place. Use this whenever the range can hold more than one child.
pub(crate) fn replace_children(
    parent: &SyntaxNode,
    range: std::ops::Range<usize>,
    children: Vec<rowan::NodeOrToken<SyntaxNode, SyntaxToken>>,
) {
    for i in range.clone().rev() {
        match parent
            .children_with_tokens()
            .nth(i)
            .expect("range within the parent's children")
        {
            rowan::NodeOrToken::Node(n) => n.detach(),
            rowan::NodeOrToken::Token(t) => t.detach(),
        }
    }
    parent.splice_children(range.start..range.start, children);
}

/// Append `children` at the end of `parent`'s child list.
pub(crate) fn append_children(
    parent: &SyntaxNode,
    children: Vec<rowan::NodeOrToken<SyntaxNode, SyntaxToken>>,
) {
    let end = parent.children_with_tokens().count();
    parent.splice_children(end..end, children);
}

/// Does this subtree end with a NEWLINE leaf, or with any run of
/// zero-width tokens (like an implicit-null NULL "") whose last
/// non-empty predecessor is a NEWLINE?
///
/// `last_token()` alone can't answer this: it returns the deepest tail
/// leaf, which may be a zero-width NULL sitting after a NEWLINE inside
/// the same KEY subtree (`? b\n` under a tagged mapping). Walk `node`'s
/// own token stream backwards, skipping empty tokens, and check what's
/// there. The walk is strictly bounded to `node`'s subtree; callers
/// don't need to worry about escaping into siblings.
pub(crate) fn trailing_newline_reachable(node: &SyntaxNode) -> bool {
    // rowan's descendants_with_tokens isn't DoubleEndedIterator, so
    // materialise the (usually short) tail slice; the walk is still
    // bounded to `node`'s subtree, avoiding prev_token()'s tree-wide
    // reach.
    let mut last_non_empty = None;
    for el in node.descendants_with_tokens() {
        if let Some(t) = el.into_token() {
            if !t.text().is_empty() {
                last_non_empty = Some(t);
            }
        }
    }
    last_non_empty.is_some_and(|t| t.kind() == SyntaxKind::NEWLINE)
}

/// Does `entry`'s line already end in a NEWLINE, counting the sibling one
/// that explicit-key entries carry instead of owning it themselves?
pub(crate) fn entry_line_terminated(entry: &SyntaxNode) -> bool {
    trailing_newline_reachable(entry)
        || entry
            .next_sibling_or_token()
            .is_some_and(|s| s.kind() == SyntaxKind::NEWLINE)
}

/// Append a trailing NEWLINE token to `entry` if its line doesn't already end
/// with one. Used when a block-style entry is about to have a new sibling
/// appended after it (its trailing newline separates the two entries
/// visually).
///
/// The new sibling must be going in immediately after `entry`: an explicit-key
/// entry is treated as terminated by a NEWLINE that follows it as a sibling,
/// which only separates the two if nothing is spliced in between.
pub(crate) fn ensure_trailing_newline(entry: &SyntaxNode) {
    if entry_line_terminated(entry) {
        return;
    }
    append_children(entry, vec![fresh_token(SyntaxKind::NEWLINE, "\n").into()]);
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

/// Move the blank lines straight after `entry`, a block collection entry
/// just inserted, to in front of its line, if its value ends in a kept
/// (`|+`, `>+`) block scalar.
///
/// Such a scalar takes every blank line after it into its value, so the
/// new entry goes after the blank lines rather than before them, leaving
/// the text around it as it was.
pub(crate) fn move_blank_lines_ahead_of_kept_scalar(entry: &SyntaxNode) {
    if !ends_with_kept_block_scalar(entry) {
        return;
    }
    // An explicit-key entry leaves the break ending its line to the mapping.
    let Some(line_end) = entry
        .last_token()
        .filter(|t| t.kind() == SyntaxKind::NEWLINE)
        .or_else(|| {
            entry
                .next_sibling_or_token()
                .and_then(|c| c.into_token())
                .filter(|t| t.kind() == SyntaxKind::NEWLINE)
        })
    else {
        return;
    };
    let mut lines = Vec::new();
    let mut next = line_end.next_token();
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
        lines.extend(space);
        lines.push(line_break);
    }
    if lines.is_empty() {
        return;
    }
    let moved: Vec<_> = lines
        .into_iter()
        .map(|t| {
            t.detach();
            fresh_token(t.kind(), t.text()).into()
        })
        .collect();

    // The lines go in front of the outermost element that starts the
    // entry's line, within the document.
    let first = entry.first_token().expect("an inserted entry has tokens");
    let line_start = std::iter::successors(Some(first), |t| {
        t.prev_token()
            .filter(|p| !matches!(p.kind(), SyntaxKind::NEWLINE | SyntaxKind::BOM))
    })
    .last()
    .expect("successors yields its first item");
    let mut at = rowan::NodeOrToken::Token(line_start.clone());
    while let Some(parent) = at
        .parent()
        .filter(|p| p.kind() != SyntaxKind::DOCUMENT && p.first_token() == Some(line_start.clone()))
    {
        at = rowan::NodeOrToken::Node(parent);
    }
    let parent = at.parent().expect("an inserted entry is in a document");
    let index = at.index();
    parent.splice_children(index..index, moved);
}

/// Whether the last node `entry` holds is a kept (`|+`, `>+`) block scalar.
fn ends_with_kept_block_scalar(entry: &SyntaxNode) -> bool {
    let tokens: Vec<_> = entry
        .descendants_with_tokens()
        .filter_map(|c| c.into_token())
        .collect();
    let Some(scalar) = tokens
        .iter()
        .rev()
        .filter(|t| !t.text().is_empty())
        .find_map(|t| {
            let parent = t.parent()?;
            let layout = matches!(
                t.kind(),
                SyntaxKind::NEWLINE | SyntaxKind::WHITESPACE | SyntaxKind::INDENT
            );
            match parent.kind() {
                SyntaxKind::SCALAR => Some(Some(parent)),
                _ if layout => None,
                _ => Some(None),
            }
        })
        .flatten()
    else {
        return false;
    };
    // The indicators follow `|` or `>` straight away. A scalar built from a
    // value is a single token, so stop at the end of the header line.
    let header: String = scalar
        .children_with_tokens()
        .filter_map(|c| c.into_token())
        .take_while(|t| t.kind() != SyntaxKind::NEWLINE)
        .map(|t| t.text().to_string())
        .collect();
    let mut chars = header.chars();
    matches!(chars.next(), Some('|' | '>'))
        && chars
            .take_while(|c| c.is_ascii_digit() || matches!(c, '+' | '-'))
            .any(|c| c == '+')
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
        SyntaxKind::SCALAR => has_child_token(node, |k| {
            matches!(k, SyntaxKind::PIPE | SyntaxKind::GREATER)
        }),
        SyntaxKind::VALUE => node
            .last_child()
            .is_some_and(|last| last.text_range().is_empty() || holds_line_end(&last)),
        SyntaxKind::TAGGED_NODE => node.last_child().is_some_and(|last| holds_line_end(&last)),
        _ => false,
    }
}

/// The `children_with_tokens()` indices of `parent`'s direct child nodes of
/// the given kind, in order.
///
/// `splice_children` indexes by `children_with_tokens()`, so mutation helpers
/// need entry positions in that numbering rather than `children()`'s.
pub(crate) fn entry_indices(parent: &SyntaxNode, kind: SyntaxKind) -> Vec<usize> {
    parent
        .children_with_tokens()
        .enumerate()
        .filter(|(_, c)| c.as_node().is_some_and(|n| n.kind() == kind))
        .map(|(i, _)| i)
        .collect()
}

/// The `children_with_tokens()` index of `parent`'s `index`-th child node of
/// the given kind.
pub(crate) fn nth_entry_index(
    parent: &SyntaxNode,
    kind: SyntaxKind,
    index: usize,
) -> Option<usize> {
    parent
        .children_with_tokens()
        .enumerate()
        .filter(|(_, c)| c.as_node().is_some_and(|n| n.kind() == kind))
        .map(|(i, _)| i)
        .nth(index)
}

/// How strings written into the flow collection `collection` are quoted.
///
/// If the string scalars among its keys and items are all double-quoted,
/// as in JSON, new strings are too; if any is plain or single-quoted, they
/// are quoted only where needed. A collection without string scalars of its
/// own goes by the nearest enclosing flow collection that has some, and
/// failing that quotes only where needed.
pub(crate) fn flow_quoting(collection: &SyntaxNode) -> crate::as_yaml::FlowQuoting {
    use crate::as_yaml::FlowQuoting;
    let is_flow = |n: &SyntaxNode| {
        n.children_with_tokens()
            .any(|c| matches!(c.kind(), SyntaxKind::LEFT_BRACE | SyntaxKind::LEFT_BRACKET))
    };
    let own_quoting = |n: SyntaxNode| {
        let mut strings = n
            .children()
            .flat_map(|entry| entry.children())
            .flat_map(|c| match c.kind() {
                SyntaxKind::KEY | SyntaxKind::VALUE => c.children().collect(),
                _ => vec![c],
            })
            .filter(|c| c.kind() == SyntaxKind::SCALAR)
            .filter_map(|c| c.first_token())
            .filter(|t| t.kind() == SyntaxKind::STRING)
            .peekable();
        strings.peek()?;
        Some(if strings.all(|t| t.text().starts_with('"')) {
            FlowQuoting::Double
        } else {
            FlowQuoting::Minimal
        })
    };
    collection
        .ancestors()
        .filter(|n| matches!(n.kind(), SyntaxKind::MAPPING | SyntaxKind::SEQUENCE))
        .take_while(is_flow)
        .find_map(own_quoting)
        .unwrap_or(FlowQuoting::Minimal)
}

/// Emit `key`'s content for use inside a `KEY` node, of a flow mapping
/// quoting strings as `flow` says if it is set.
///
/// A block scalar or block collection, tagged or not, is written in flow
/// form instead: a block collection cannot be an implicit key, and a block
/// scalar does not read back as the same key, as the entry's `:` lands
/// inside the block. `indent` is the column of the entry, which a
/// multi-line key's continuation lines are shifted to clear.
pub(crate) fn build_key_content(
    builder: &mut rowan::GreenNodeBuilder,
    key: &impl crate::AsYaml,
    indent: usize,
    flow: Option<crate::as_yaml::FlowQuoting>,
) {
    if let Some(quoting) = flow {
        // Flow context already rules out block scalars.
        key.build_flow_content(builder, indent, quoting);
        return;
    }
    if let Some(key) = mapping::flow_form(key, 2, crate::as_yaml::FlowQuoting::Minimal) {
        return build_key_content(builder, &key, indent, None);
    }
    // flow_form leaves raw values that claim to be inline alone, but a
    // string with line breaks still renders as a block scalar, and a set as
    // a block mapping below its tag. In flow context both fit on one line.
    let spans_lines = key.as_node().is_none() && {
        let mut probe = rowan::GreenNodeBuilder::new();
        probe.start_node(SyntaxKind::ROOT.into());
        key.build_content(&mut probe, 0, false);
        probe.finish_node();
        SyntaxNode::new_root(probe.finish())
            .text()
            .contains_char('\n')
    };
    key.build_content(builder, indent, spans_lines);
}

/// Whether `key`, as [`build_key_content`] writes it, spans several lines,
/// which in a block mapping only an explicit key may do.
pub(crate) fn key_spans_lines(key: &impl crate::AsYaml) -> bool {
    let mut builder = rowan::GreenNodeBuilder::new();
    builder.start_node(SyntaxKind::ROOT.into());
    build_key_content(&mut builder, key, 0, None);
    builder.finish_node();
    SyntaxNode::new_root(builder.finish())
        .text()
        .contains_char('\n')
}

/// The `KEY` child of a `MAPPING_ENTRY`.
pub(crate) fn entry_key(entry: &SyntaxNode) -> Option<SyntaxNode> {
    child_of_kind(entry, SyntaxKind::KEY)
}

/// The `VALUE` child of a `MAPPING_ENTRY` or `SEQUENCE_ENTRY`.
pub(crate) fn entry_value(entry: &SyntaxNode) -> Option<SyntaxNode> {
    child_of_kind(entry, SyntaxKind::VALUE)
}

/// A macro to create AST node wrappers.
macro_rules! ast_node {
    ($ast:ident, $kind:ident, $doc:expr) => {
        #[doc = $doc]
        #[doc = ""]
        #[doc = "**Note:** This type uses interior mutability through the rowan library."]
        #[doc = "Mutation methods work even when called through `&self`. See the crate-level"]
        #[doc = "documentation for details on the mutability model."]
        #[derive(Clone, PartialEq, Eq, Hash)]
        pub struct $ast(pub(crate) SyntaxNode);

        impl std::fmt::Debug for $ast {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_struct(stringify!($ast))
                    .field("syntax", &self.0)
                    .finish()
            }
        }

        impl AstNode for $ast {
            type Language = Lang;

            fn can_cast(kind: SyntaxKind) -> bool {
                kind == SyntaxKind::$kind
            }

            fn cast(syntax: SyntaxNode) -> Option<Self> {
                if Self::can_cast(syntax.kind()) {
                    Some(Self(syntax))
                } else {
                    None
                }
            }

            fn syntax(&self) -> &SyntaxNode {
                &self.0
            }
        }

        impl std::fmt::Display for $ast {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", self.0.text())
            }
        }
    };
}

pub(crate) use ast_node;
/// Emit the `byte_range` / `start_position` / `end_position` trio for a
/// wrapper whose `self.0` exposes `text_range()`. `$what` names the node
/// in the generated docs ("sequence", "comment", ...).
macro_rules! ast_node_spans {
    ($what:expr) => {
        #[doc = concat!("Get the byte offset range of this ", $what, " in the source text.")]
        #[doc = ""]
        #[doc = "Returns the start and end byte offsets as a `TextPosition`."]
        pub fn byte_range(&self) -> crate::TextPosition {
            self.0.text_range().into()
        }

        #[doc = concat!("Get the line and column where this ", $what, " starts.")]
        #[doc = ""]
        #[doc = "Requires the original source text to calculate line/column from byte"]
        #[doc = "offsets. Line and column numbers are 1-indexed."]
        #[doc = ""]
        #[doc = "# Arguments"]
        #[doc = ""]
        #[doc = "* `source_text` - The original YAML source text"]
        pub fn start_position(&self, source_text: &str) -> crate::LineColumn {
            crate::byte_offset_to_line_column(source_text, self.byte_range().start as usize)
        }

        #[doc = concat!("Get the line and column where this ", $what, " ends.")]
        #[doc = ""]
        #[doc = "Requires the original source text to calculate line/column from byte"]
        #[doc = "offsets. Line and column numbers are 1-indexed."]
        #[doc = ""]
        #[doc = "# Arguments"]
        #[doc = ""]
        #[doc = "* `source_text` - The original YAML source text"]
        pub fn end_position(&self, source_text: &str) -> crate::LineColumn {
            crate::byte_offset_to_line_column(source_text, self.byte_range().end as usize)
        }
    };
}

pub(crate) use ast_node_spans;

// Node modules
pub mod alias_node;
pub mod comment;
pub mod directive;
pub mod document;
pub mod entry;
pub mod mapping;
mod replaced_value;
pub mod scalar_node;
pub mod sequence;
pub mod tagged_node;

// Re-exports
pub use alias_node::Alias;
pub use comment::Comment;
pub use directive::Directive;
pub use document::Document;
pub use entry::{Entry, OccupiedEntry, VacantEntry};
pub use mapping::{Mapping, MappingEntry};
pub use scalar_node::{Scalar, ScalarConversionError};
pub use sequence::Sequence;
pub use tagged_node::TaggedNode;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_replace_children_removes_whole_range() {
        let mut builder = rowan::GreenNodeBuilder::new();
        builder.start_node(SyntaxKind::ROOT.into());
        for text in ["a", "b", "c", "d", "e"] {
            builder.token(SyntaxKind::STRING.into(), text);
        }
        builder.finish_node();
        let root = SyntaxNode::new_root_mut(builder.finish());

        replace_children(
            &root,
            1..4,
            vec![fresh_token(SyntaxKind::STRING, "X").into()],
        );
        assert_eq!(root.to_string(), "aXe");
    }
}
