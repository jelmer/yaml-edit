//! Module-private helpers used by MappingEntry, Mapping queries, and
//! Mapping mutations. Includes the [`FlowInsertPos`] enum, block-value
//! wrapper builders, decoration walkers, and tail/comment scanners.
//!
//! Split out of `nodes/mapping/mod.rs` unchanged.

use super::Mapping;
use crate::lex::SyntaxKind;
use crate::nodes::{append_children, fresh_token, has_child_token, Scalar, Sequence, SyntaxNode};
use crate::yaml::ValueNode;
use rowan::ast::AstNode;
use rowan::GreenNodeBuilder;

/// Where to place a new entry inside a flow-style mapping (`{...}`).
pub(in crate::nodes) enum FlowInsertPos {
    /// At the end (right before the closing `}`).
    End,
    /// Right after this existing MAPPING_ENTRY child.
    After(SyntaxNode),
    /// Right before this existing MAPPING_ENTRY child.
    Before(SyntaxNode),
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
    has_child_token(&carrier, |k| k == SyntaxKind::NEWLINE)
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
    if has_map_entry_parent {
        return;
    }
    // Entries normally take their trailing NEWLINE with them when removed.
    // An explicit-key entry's line break is a sibling, so it outlives the
    // entry and would otherwise leave the mapping rendering as bare
    // whitespace instead of `{}`.
    let leftovers: Vec<_> = mapping.children_with_tokens().collect();
    if !leftovers
        .iter()
        .all(|c| matches!(c.kind(), SyntaxKind::NEWLINE | SyntaxKind::INDENT))
    {
        return;
    }
    if !leftovers.is_empty() {
        crate::nodes::replace_children(mapping, 0..leftovers.len(), vec![]);
    }
    let lbrace = fresh_token(SyntaxKind::LEFT_BRACE, "{");
    let rbrace = fresh_token(SyntaxKind::RIGHT_BRACE, "}");
    mapping.splice_children(0..0, vec![lbrace.into(), rbrace.into()]);
}

/// Index just past `entry`'s line terminator among its siblings.
///
/// Entries normally own their trailing NEWLINE, so the slot after the entry
/// is the start of the next line. Explicit-key entries (`? k` / `: v`) are
/// parsed without one: theirs sits beside them as a sibling, and inserting
/// at the entry's own index would split it from its line break.
pub(super) fn index_after_entry_line(parent: &SyntaxNode, idx: usize) -> usize {
    let follows_newline = parent
        .children_with_tokens()
        .nth(idx + 1)
        .is_some_and(|c| c.kind() == SyntaxKind::NEWLINE);
    if follows_newline {
        idx + 2
    } else {
        idx + 1
    }
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
    append_children(
        entry,
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

/// A VALUE node built by [`build_block_value_node`].
pub(crate) struct BlockValue {
    pub(crate) node: SyntaxNode,
    /// The value starts on the key line (with an anchor, tag or block
    /// scalar indicator) and needs a space after the `:`. The parser keeps
    /// that space in MAPPING_ENTRY, before VALUE.
    pub(crate) on_key_line: bool,
    pub(crate) ends_with_newline: bool,
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
/// aliases still resolve.
pub(crate) fn build_block_value_node(source: &SyntaxNode, target: usize) -> BlockValue {
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
    if let Some(a) = anchor.as_deref() {
        value_builder.token(SyntaxKind::ANCHOR.into(), a);
    }
    // Block scalars keep the `|`/`>` indicator on the key line; block
    // collections drop to the next line indented to `target`.
    if !is_scalar {
        value_builder.token(SyntaxKind::NEWLINE.into(), "\n");
        value_builder.token(SyntaxKind::INDENT.into(), &" ".repeat(target));
    } else if anchor.is_some() {
        value_builder.token(SyntaxKind::WHITESPACE.into(), " ");
    }
    value_builder.start_node(source.kind().into());
    let ends_with_newline =
        crate::as_yaml::copy_node_content_reindent(&mut value_builder, source, delta);
    value_builder.finish_node();
    value_builder.finish_node();
    BlockValue {
        node: SyntaxNode::new_root_mut(value_builder.finish()),
        on_key_line: is_scalar || anchor.is_some(),
        ends_with_newline,
    }
}

/// Put `comment` (the whitespace before it, then its text) at the end of
/// the first line of `value`, a VALUE built by [`build_block_value_node`]
/// that starts on the key line. The parser keeps such a comment beside the
/// anchor, tag or block scalar header it follows.
// TODO: if that line already has a comment (a block scalar's header
// comment), the value's own is kept and `comment` is silently dropped, as
// a line holds only one.
pub(super) fn add_key_line_comment(value: &SyntaxNode, comment: (String, String)) {
    let first_line: Vec<_> = value
        .descendants_with_tokens()
        .filter_map(|c| c.into_token())
        .take_while(|t| t.kind() != SyntaxKind::NEWLINE)
        .collect();
    if first_line.iter().any(|t| t.kind() == SyntaxKind::COMMENT) {
        return;
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
}

/// `value` built as a standalone node, if it has no node of its own but
/// spans several lines (a non-empty `YamlValue` collection, or a tagged
/// one), so it can be laid out like a block node from another CST.
pub(super) fn built_block_node(value: &impl crate::AsYaml) -> Option<SyntaxNode> {
    if value.as_node().is_some() {
        return None;
    }
    let mut builder = GreenNodeBuilder::new();
    builder.start_node(SyntaxKind::ROOT.into());
    let ends_with_newline = value.build_content(&mut builder, 0, false);
    builder.finish_node();
    if value.is_inline() && !ends_with_newline {
        return None;
    }
    SyntaxNode::new_root(builder.finish()).first_child()
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
/// key line; the wrapped inner value (mapping/sequence) goes on the next
/// line at `target` for block content, or stays inline with a leading
/// space for inline content and block scalars, with interior indentation
/// re-shifted so nested lines line up.
pub(super) fn build_tagged_value_node(source: &SyntaxNode, target: usize) -> BlockValue {
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
        Some(inner) if node_is_inline(&inner) || is_block_scalar(&inner) => {
            // `!!str value` or `!!str |`: content on the tag line, with a
            // block scalar's content lines shifted to `target`.
            let delta = match block_scalar_content_indent(&inner) {
                Some(base) if is_block_scalar(&inner) => target as isize - base as isize,
                _ => 0,
            };
            value_builder.token(SyntaxKind::WHITESPACE.into(), " ");
            value_builder.start_node(inner.kind().into());
            let n = crate::as_yaml::copy_node_content_reindent(&mut value_builder, &inner, delta);
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
    BlockValue {
        node: SyntaxNode::new_root_mut(value_builder.finish()),
        on_key_line: true,
        ends_with_newline,
    }
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

/// `value` re-rendered in flow style, or `None` if it is already inline.
///
/// Block collections and block scalars can't appear inside a flow
/// collection or as an implicit key. Anchors, tags and aliases carry over.
/// Comments are kept: each one is moved to the next point where the flow
/// node can break its line (after an opening bracket, after a comma, or
/// before a closing bracket), with continuation lines at `indent`. A
/// result containing comments therefore spans several lines; see
/// [`has_comment`].
pub(crate) fn flow_form(
    value: &impl crate::AsYaml,
    indent: usize,
) -> Option<crate::as_yaml::YamlNode> {
    if value.is_inline() {
        return None;
    }
    let built;
    let node = match value.as_node() {
        Some(node) => node,
        None => {
            let mut builder = GreenNodeBuilder::new();
            builder.start_node(SyntaxKind::ROOT.into());
            value.build_content(&mut builder, 0, false);
            builder.finish_node();
            let root = SyntaxNode::new_root(builder.finish());
            built = root.first_child().expect("build_content emits a node");
            &built
        }
    };
    let mut writer = FlowWriter {
        builder: GreenNodeBuilder::new(),
        pending: Vec::new(),
    };
    writer.node(node, indent);
    // Comments only become pending inside a collection, which flushes them
    // before its closing bracket.
    assert!(writer.pending.is_empty(), "unplaced comments in flow form");
    crate::as_yaml::YamlNode::from_syntax(SyntaxNode::new_root_mut(writer.builder.finish()))
}

/// True if `node` contains a comment anywhere.
pub(super) fn has_comment(node: &SyntaxNode) -> bool {
    node.descendants_with_tokens()
        .any(|c| c.kind() == SyntaxKind::COMMENT)
}

/// Builds the flow-style CST for a block node, in the shape the parser
/// produces for the same text.
struct FlowWriter {
    builder: GreenNodeBuilder<'static>,
    /// Comments seen in the source but not yet written, each with the
    /// whitespace to put in front of it if it ends up after other content.
    pending: Vec<(String, String)>,
}

impl FlowWriter {
    fn node(&mut self, node: &SyntaxNode, indent: usize) {
        match node.kind() {
            SyntaxKind::MAPPING | SyntaxKind::SEQUENCE => self.collection(node, indent),
            SyntaxKind::SCALAR => self.scalar(node),
            SyntaxKind::TAGGED_NODE => {
                self.builder.start_node(SyntaxKind::TAGGED_NODE.into());
                self.decorated(node, indent, true);
                self.builder.finish_node();
            }
            _ => {
                self.builder.start_node(node.kind().into());
                crate::as_yaml::copy_node_content(&mut self.builder, node);
                self.builder.finish_node();
            }
        }
    }

    /// Write the anchor, tag and node held by a KEY, VALUE, SEQUENCE_ENTRY
    /// or TAGGED_NODE. An undecorated implicit null becomes a textual
    /// `null`, since an empty slot in a flow sequence would otherwise
    /// vanish or leave a stray comma. `nested` is set for a TAGGED_NODE,
    /// whose content sits at the same level as the tag.
    fn decorated(&mut self, wrapper: &SyntaxNode, indent: usize, nested: bool) {
        let mut decorated = false;
        for child in wrapper.children_with_tokens() {
            match child {
                rowan::NodeOrToken::Token(t) => match t.kind() {
                    SyntaxKind::ANCHOR | SyntaxKind::TAG => {
                        if decorated {
                            self.builder.token(SyntaxKind::WHITESPACE.into(), " ");
                        }
                        self.builder.token(t.kind().into(), t.text());
                        decorated = true;
                    }
                    SyntaxKind::COMMENT => self.push_comment(&t),
                    _ => {}
                },
                rowan::NodeOrToken::Node(n) => {
                    let empty = n.text_range().is_empty();
                    if empty && !decorated && !nested {
                        self.builder.start_node(SyntaxKind::SCALAR.into());
                        self.builder.token(SyntaxKind::NULL.into(), "null");
                        self.builder.finish_node();
                        continue;
                    }
                    if decorated && !empty {
                        self.builder.token(SyntaxKind::WHITESPACE.into(), " ");
                    }
                    let inner = indent + if nested { 0 } else { 2 };
                    self.node(&n, inner);
                }
            }
        }
    }

    fn collection(&mut self, node: &SyntaxNode, indent: usize) {
        let (entry_kind, open, close) = if node.kind() == SyntaxKind::MAPPING {
            (
                SyntaxKind::MAPPING_ENTRY,
                ("{", SyntaxKind::LEFT_BRACE),
                ("}", SyntaxKind::RIGHT_BRACE),
            )
        } else {
            (
                SyntaxKind::SEQUENCE_ENTRY,
                ("[", SyntaxKind::LEFT_BRACKET),
                ("]", SyntaxKind::RIGHT_BRACKET),
            )
        };
        self.builder.start_node(node.kind().into());
        self.builder.token(open.1.into(), open.0);
        let children: Vec<_> = node.children_with_tokens().collect();
        let entries: Vec<usize> = (0..children.len())
            .filter(|&i| children[i].kind() == entry_kind)
            .collect();
        // Comments between entries sit beside them in a block collection;
        // each goes with the entry before it.
        self.push_comments(&children[..entries.first().copied().unwrap_or(children.len())]);
        self.flush(indent);
        for (n, &i) in entries.iter().enumerate() {
            let entry = children[i].as_node().expect("entry is a node");
            self.builder.start_node(entry_kind.into());
            if entry_kind == SyntaxKind::MAPPING_ENTRY {
                self.mapping_entry(entry, indent);
            } else {
                self.decorated(entry, indent, false);
            }
            let next = entries.get(n + 1).copied();
            self.push_comments(&children[i + 1..next.unwrap_or(children.len())]);
            if next.is_some() {
                self.builder.token(SyntaxKind::COMMA.into(), ",");
                if self.pending.is_empty() {
                    self.builder.token(SyntaxKind::WHITESPACE.into(), " ");
                }
            }
            self.flush(indent);
            self.builder.finish_node();
        }
        self.builder.token(close.1.into(), close.0);
        self.builder.finish_node();
    }

    fn mapping_entry(&mut self, entry: &SyntaxNode, indent: usize) {
        let mut has_value = false;
        for child in entry.children_with_tokens() {
            match child {
                rowan::NodeOrToken::Token(t) if t.kind() == SyntaxKind::COMMENT => {
                    self.push_comment(&t)
                }
                rowan::NodeOrToken::Node(n) if n.kind() == SyntaxKind::KEY => self.key(&n, indent),
                rowan::NodeOrToken::Node(n) if n.kind() == SyntaxKind::VALUE => {
                    self.colon();
                    self.builder.start_node(SyntaxKind::VALUE.into());
                    self.decorated(&n, indent, false);
                    self.builder.finish_node();
                    has_value = true;
                }
                _ => {}
            }
        }
        if !has_value {
            self.colon();
            self.builder.start_node(SyntaxKind::VALUE.into());
            self.builder.start_node(SyntaxKind::SCALAR.into());
            self.builder.token(SyntaxKind::NULL.into(), "null");
            self.builder.finish_node();
            self.builder.finish_node();
        }
    }

    /// Write a KEY, as an explicit key if comments break it over several
    /// lines. YAML 1.2 lets an implicit key in a flow mapping span lines,
    /// but PyYAML and others only accept single-line implicit keys. The
    /// parser puts the `?` of a flow explicit key inside KEY.
    ///
    /// The key is written aside first, since only then is it known whether
    /// it breaks.
    fn key(&mut self, key: &SyntaxNode, indent: usize) {
        let mut sub = FlowWriter {
            builder: GreenNodeBuilder::new(),
            pending: std::mem::take(&mut self.pending),
        };
        sub.builder.start_node(SyntaxKind::KEY.into());
        sub.decorated(key, indent, false);
        sub.builder.finish_node();
        self.pending = sub.pending;
        let written = SyntaxNode::new_root(sub.builder.finish());
        self.builder.start_node(SyntaxKind::KEY.into());
        let multiline = written
            .descendants_with_tokens()
            .any(|c| c.kind() == SyntaxKind::NEWLINE);
        if multiline {
            self.builder.token(SyntaxKind::QUESTION.into(), "?");
            self.builder.token(SyntaxKind::WHITESPACE.into(), " ");
        }
        crate::yaml::add_node_children_to(&mut self.builder, &written);
        self.builder.finish_node();
    }

    fn colon(&mut self) {
        self.builder.token(SyntaxKind::COLON.into(), ":");
        self.builder.token(SyntaxKind::WHITESPACE.into(), " ");
    }

    fn scalar(&mut self, node: &SyntaxNode) {
        let tokens: Vec<_> = node
            .descendants_with_tokens()
            .filter_map(|c| c.into_token())
            .collect();
        for t in tokens.iter().filter(|t| t.kind() == SyntaxKind::COMMENT) {
            self.push_comment(t);
        }
        let text: String = tokens
            .iter()
            .filter(|t| t.kind() != SyntaxKind::COMMENT)
            .map(|t| t.text())
            .collect();
        let text = text.trim();
        let quoted = text.starts_with(['"', '\'']);
        let breaks_flow = is_block_scalar(node)
            || text.contains('\n')
            || (!quoted && text.contains([',', '[', ']', '{', '}']));
        if breaks_flow {
            let value = Scalar::cast(node.clone()).expect("SCALAR node").as_string();
            crate::AsYaml::build_content(
                &crate::scalar::ScalarValue::double_quoted(value),
                &mut self.builder,
                0,
                true,
            );
            return;
        }
        self.builder.start_node(SyntaxKind::SCALAR.into());
        let content = tokens.iter().filter(|t| {
            !matches!(
                t.kind(),
                SyntaxKind::COMMENT
                    | SyntaxKind::WHITESPACE
                    | SyntaxKind::NEWLINE
                    | SyntaxKind::INDENT
            )
        });
        for t in content {
            self.builder.token(t.kind().into(), t.text());
        }
        self.builder.finish_node();
    }

    fn push_comments(&mut self, elements: &[rowan::SyntaxElement<crate::nodes::Lang>]) {
        for element in elements {
            match element {
                rowan::NodeOrToken::Token(t) if t.kind() == SyntaxKind::COMMENT => {
                    self.push_comment(t)
                }
                rowan::NodeOrToken::Node(n) => {
                    for t in n.descendants_with_tokens().filter_map(|c| c.into_token()) {
                        if t.kind() == SyntaxKind::COMMENT {
                            self.push_comment(&t);
                        }
                    }
                }
                _ => {}
            }
        }
    }

    fn push_comment(&mut self, comment: &crate::nodes::SyntaxToken) {
        let ws = comment
            .prev_token()
            .filter(|t| t.kind() == SyntaxKind::WHITESPACE)
            .filter(|t| {
                t.prev_token()
                    .is_some_and(|p| p.kind() != SyntaxKind::NEWLINE)
            })
            .map_or_else(|| " ".to_string(), |t| t.text().to_string());
        self.pending.push((ws, comment.text().to_string()));
    }

    /// Write the pending comments, each ending its line, so that what
    /// follows starts a fresh line at `indent`.
    fn flush(&mut self, indent: usize) {
        for (i, (ws, comment)) in std::mem::take(&mut self.pending).into_iter().enumerate() {
            if i == 0 {
                self.builder.token(SyntaxKind::WHITESPACE.into(), &ws);
            }
            self.builder.token(SyntaxKind::COMMENT.into(), &comment);
            self.builder.token(SyntaxKind::NEWLINE.into(), "\n");
            self.builder
                .token(SyntaxKind::INDENT.into(), &" ".repeat(indent));
        }
    }
}
