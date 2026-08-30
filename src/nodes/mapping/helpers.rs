//! Module-private helpers used by MappingEntry, Mapping queries, and
//! Mapping mutations. Includes the [`FlowInsertPos`] enum, block-value
//! wrapper builders, decoration walkers, and tail/comment scanners.
//!
//! Split out of `nodes/mapping/mod.rs` unchanged.

use super::Mapping;
use crate::lex::SyntaxKind;
use crate::nodes::{fresh_token, Scalar, Sequence, SyntaxNode};
use crate::yaml::ValueNode;
use rowan::ast::AstNode;
use rowan::GreenNodeBuilder;

/// Where to place a new entry inside a flow-style mapping (`{...}`).
pub(super) enum FlowInsertPos {
    /// At the end (right before the closing `}`).
    End,
    /// Right after this existing MAPPING_ENTRY child.
    After(SyntaxNode),
    /// Right before this existing MAPPING_ENTRY child.
    Before(SyntaxNode),
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
pub(super) fn trailing_newline_reachable(node: &SyntaxNode) -> bool {
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

/// Is `value` laid out block-style (key on one line, value content on
/// subsequent lines)?
///
/// Block layout puts a NEWLINE as a *direct* child of VALUE, between
/// the COLON and the value's content. Inline flow -- even a multi-line
/// flow like `[\n  a,\n  b\n]` -- keeps its NEWLINEs nested inside the
/// flow SEQUENCE/MAPPING, so the VALUE has no direct-child NEWLINE.
///
/// A TAG annotation (`k: !!seq\n  - old`) wraps the content in a
/// TAGGED_NODE whose own children hold the NEWLINE; peel it before
/// looking. An ANCHOR annotation (`k: &x\n  - old`) sits as a plain
/// token alongside its NEWLINE inside VALUE, so no peeling is needed.
pub(super) fn value_is_block(value: &SyntaxNode) -> bool {
    let carrier = value
        .children()
        .find(|c| c.kind() == SyntaxKind::TAGGED_NODE)
        .unwrap_or_else(|| value.clone());
    carrier.children_with_tokens().any(|el| {
        el.as_token()
            .is_some_and(|t| t.kind() == SyntaxKind::NEWLINE)
    })
}

/// Inject a `{}` into a truly-orphan empty MAPPING (no entries, no
/// MAPPING_ENTRY parent, no lingering tokens) so it renders as `{}`
/// rather than empty text. The nested counterpart is
/// [`crate::yaml::collapse_empty_child_collection_in_parent`].
pub(super) fn ensure_top_level_empty_renders_as_flow(mapping: &SyntaxNode) {
    if mapping
        .children()
        .any(|c| c.kind() == SyntaxKind::MAPPING_ENTRY)
    {
        return;
    }
    let has_map_entry_parent = mapping
        .parent()
        .filter(|p| p.kind() == SyntaxKind::VALUE)
        .and_then(|v| v.parent())
        .is_some_and(|e| e.kind() == SyntaxKind::MAPPING_ENTRY);
    if has_map_entry_parent || mapping.children_with_tokens().next().is_some() {
        return;
    }
    let lbrace = fresh_token(SyntaxKind::LEFT_BRACE, "{");
    let rbrace = fresh_token(SyntaxKind::RIGHT_BRACE, "}");
    mapping.splice_children(0..0, vec![lbrace.into(), rbrace.into()]);
}

/// Append a trailing NEWLINE token to `entry` if it doesn't already end with
/// one. Used when a block-style entry is about to have a new sibling appended
/// after it (its trailing newline separates the two entries visually).
pub(super) fn ensure_trailing_newline(entry: &SyntaxNode) {
    let has_nl = entry
        .last_token()
        .is_some_and(|t| t.kind() == SyntaxKind::NEWLINE);
    if has_nl {
        return;
    }
    let end = entry.children_with_tokens().count();
    entry.splice_children(
        end..end,
        vec![fresh_token(SyntaxKind::NEWLINE, "\n").into()],
    );
}

/// Append a `, ` pair to the end of a MAPPING_ENTRY: the separator between
/// two entries inside a flow mapping. Idempotent: if the entry already ends
/// with a COMMA (possibly followed by whitespace), do nothing (avoids
/// stacking separators when inserting next to an entry that already had a
/// trailing comma).
///
/// The parser stores flow separators (COMMA and any following WHITESPACE /
/// NEWLINE / INDENT) as siblings of KEY / VALUE inside the *previous*
/// MAPPING_ENTRY, not inside its VALUE.
pub(super) fn append_comma_space_to_entry(entry: &SyntaxNode) {
    let ends_with_comma = entry
        .children_with_tokens()
        .filter_map(|c| c.into_token())
        .filter(|t| {
            !matches!(
                t.kind(),
                SyntaxKind::WHITESPACE | SyntaxKind::NEWLINE | SyntaxKind::INDENT
            )
        })
        .last()
        .is_some_and(|t| t.kind() == SyntaxKind::COMMA);
    if ends_with_comma {
        return;
    }
    let end = entry.children_with_tokens().count();
    entry.splice_children(
        end..end,
        vec![
            fresh_token(SyntaxKind::COMMA, ",").into(),
            fresh_token(SyntaxKind::WHITESPACE, " ").into(),
        ],
    );
}

/// Walk `entry`'s tokens in document order (descending into child nodes)
/// looking for a COMMENT on the same line as the key - that is, the first
/// COMMENT after the entry's COLON and before the first NEWLINE. Returns
/// the comment's text plus the WHITESPACE that precedes it (defaulting to
/// `"  "` for comments with no leading space); `None` if the key line has
/// no comment.
///
/// The comment may sit inside VALUE (`k: v  # note`, `k:  # note\n  block`)
/// or at MAPPING_ENTRY level between COLON and VALUE (`k:  # note\n  block`
/// with the parser choosing to store the comment as a sibling of VALUE).
pub(super) fn find_key_line_comment(entry: &SyntaxNode) -> Option<(String, String)> {
    let mut seen_colon = false;
    let mut last_ws: Option<String> = None;
    for tok in entry
        .descendants_with_tokens()
        .filter_map(|c| c.into_token())
    {
        match tok.kind() {
            SyntaxKind::COLON if !seen_colon => {
                seen_colon = true;
                last_ws = None;
            }
            _ if !seen_colon => {}
            SyntaxKind::WHITESPACE => last_ws = Some(tok.text().to_string()),
            SyntaxKind::COMMENT => {
                return Some((
                    last_ws.unwrap_or_else(|| "  ".to_string()),
                    tok.text().to_string(),
                ));
            }
            SyntaxKind::NEWLINE => return None,
            _ => last_ws = None,
        }
    }
    None
}

/// True if `source` is a block scalar (literal `|` or folded `>`) - its
/// first token is the block indicator that must sit on the same line as
/// the key.
pub(super) fn is_block_scalar(source: &SyntaxNode) -> bool {
    source.kind() == SyntaxKind::SCALAR
        && source
            .first_token()
            .is_some_and(|t| matches!(t.kind(), SyntaxKind::PIPE | SyntaxKind::GREATER))
}

/// Build a VALUE node wrapping `source` (a block sequence, mapping, scalar,
/// or tagged node from another CST) so it renders correctly under a key
/// whose content column is `target`. Block collections go on the line
/// after the key with content at `target`; block scalars keep their `|`/`>`
/// indicator on the key line and put content on the next line at `target`;
/// tagged nodes keep the `!tag` on the key line and place the wrapped
/// content on the next line. The source's interior indentation is shifted
/// so nested lines line up. If the source is preceded by an `&anchor`
/// inside its parent VALUE, the anchor is preserved on the key line so
/// aliases still resolve. Returns the new VALUE node plus whether it ends
/// with a NEWLINE.
pub(super) fn build_block_value_node(source: &SyntaxNode, target: usize) -> (SyntaxNode, bool) {
    if source.kind() == SyntaxKind::TAGGED_NODE {
        return build_tagged_value_node(source, target);
    }
    // Block scalars store their content column *inside* the SCALAR
    // (after `|`/`>` + NEWLINE). Block collections have it in the INDENT
    // that precedes them in their parent.
    let is_scalar = is_block_scalar(source);
    let source_base = if is_scalar {
        block_scalar_content_indent(source).unwrap_or(0)
    } else {
        crate::as_yaml::source_base_indent(source)
    };
    let delta = target as isize - source_base as isize;
    let anchor = source_anchor_text(source);

    let mut value_builder = GreenNodeBuilder::new();
    value_builder.start_node(SyntaxKind::VALUE.into());
    // "key: &anchor" - a leading space then the anchor sits on the key line.
    if let Some(a) = anchor.as_deref() {
        value_builder.token(SyntaxKind::WHITESPACE.into(), " ");
        value_builder.token(SyntaxKind::ANCHOR.into(), a);
    }
    // Block scalars keep the `|`/`>` indicator on the key line (one space
    // after the anchor or the colon); block collections drop to the next
    // line indented to `target`.
    if is_scalar {
        value_builder.token(SyntaxKind::WHITESPACE.into(), " ");
    } else {
        value_builder.token(SyntaxKind::NEWLINE.into(), "\n");
        value_builder.token(SyntaxKind::INDENT.into(), &" ".repeat(target));
    }
    value_builder.start_node(source.kind().into());
    let ends_with_newline =
        crate::as_yaml::copy_node_content_reindent(&mut value_builder, source, delta);
    value_builder.finish_node();
    value_builder.finish_node();
    (
        SyntaxNode::new_root_mut(value_builder.finish()),
        ends_with_newline,
    )
}

/// True if `node` (any yaml value node) renders inline (same line as its key).
pub(super) fn node_is_inline(node: &SyntaxNode) -> bool {
    match node.kind() {
        SyntaxKind::MAPPING => {
            Mapping::cast(node.clone()).is_some_and(|m| ValueNode::is_inline(&m))
        }
        SyntaxKind::SEQUENCE => {
            Sequence::cast(node.clone()).is_some_and(|s| ValueNode::is_inline(&s))
        }
        SyntaxKind::SCALAR => Scalar::cast(node.clone()).is_some_and(|s| ValueNode::is_inline(&s)),
        _ => true,
    }
}

/// Build a VALUE node wrapping a TAGGED_NODE source. The tag stays on the
/// key line; the wrapped inner value (mapping/sequence/scalar) goes on the
/// next line at `target` for block content, or stays inline with a leading
/// space for inline content, with interior indentation re-shifted so
/// nested lines line up.
pub(super) fn build_tagged_value_node(source: &SyntaxNode, target: usize) -> (SyntaxNode, bool) {
    // A TAGGED_NODE holds the TAG token followed by the wrapped value node.
    let tag_text = source
        .children_with_tokens()
        .filter_map(|c| c.into_token())
        .find(|t| t.kind() == SyntaxKind::TAG)
        .map(|t| t.text().to_string());
    let inner = source.first_child();
    let anchor = source_anchor_text(source);

    let mut value_builder = GreenNodeBuilder::new();
    value_builder.start_node(SyntaxKind::VALUE.into());
    value_builder.token(SyntaxKind::WHITESPACE.into(), " ");
    if let Some(a) = anchor.as_deref() {
        value_builder.token(SyntaxKind::ANCHOR.into(), a);
        value_builder.token(SyntaxKind::WHITESPACE.into(), " ");
    }
    value_builder.start_node(SyntaxKind::TAGGED_NODE.into());
    if let Some(tag) = tag_text.as_deref() {
        value_builder.token(SyntaxKind::TAG.into(), tag);
    }
    let ends_with_newline = match inner {
        None => false,
        Some(inner) if node_is_inline(&inner) => {
            // e.g. `!!str value` - content on same line, verbatim copy.
            value_builder.token(SyntaxKind::WHITESPACE.into(), " ");
            value_builder.start_node(inner.kind().into());
            let n = crate::as_yaml::copy_node_content_reindent(&mut value_builder, &inner, 0);
            value_builder.finish_node();
            n
        }
        Some(inner) => {
            // Block content: next line, re-indent by (target - source_base).
            value_builder.token(SyntaxKind::NEWLINE.into(), "\n");
            value_builder.token(SyntaxKind::INDENT.into(), &" ".repeat(target));
            let delta = target as isize - crate::as_yaml::source_base_indent(&inner) as isize;
            value_builder.start_node(inner.kind().into());
            let n = crate::as_yaml::copy_node_content_reindent(&mut value_builder, &inner, delta);
            value_builder.finish_node();
            n
        }
    };
    value_builder.finish_node(); // TAGGED_NODE
    value_builder.finish_node(); // VALUE
    (
        SyntaxNode::new_root_mut(value_builder.finish()),
        ends_with_newline,
    )
}

/// If `source` is preceded by an `&anchor` token in its parent VALUE, return
/// the anchor's text (including the leading `&`). Otherwise `None`.
pub(super) fn source_anchor_text(source: &SyntaxNode) -> Option<String> {
    let mut cursor = source.prev_sibling_or_token();
    while let Some(item) = cursor {
        let tok = item.as_token()?;
        match tok.kind() {
            SyntaxKind::ANCHOR => return Some(tok.text().to_string()),
            SyntaxKind::WHITESPACE | SyntaxKind::INDENT | SyntaxKind::NEWLINE => {}
            _ => return None,
        }
        cursor = item.prev_sibling_or_token();
    }
    None
}

/// Find the content indent of a block scalar - the INDENT token right after
/// the `|`/`>` indicator and its NEWLINE. Returns `None` if the scalar is
/// empty (no content lines).
pub(super) fn block_scalar_content_indent(scalar: &SyntaxNode) -> Option<usize> {
    let mut after_newline = false;
    for child in scalar.children_with_tokens() {
        if let Some(tok) = child.as_token() {
            match tok.kind() {
                SyntaxKind::NEWLINE => after_newline = true,
                SyntaxKind::WHITESPACE | SyntaxKind::INDENT if after_newline => {
                    return Some(tok.text().len());
                }
                _ => after_newline = false,
            }
        }
    }
    None
}
