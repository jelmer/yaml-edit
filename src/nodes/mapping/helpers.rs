//! Module-private helpers used by MappingEntry, Mapping queries, and
//! Mapping mutations. Includes the [`FlowInsertPos`] enum, block-value
//! wrapper builders, decoration walkers, and tail/comment scanners.
//!
//! Split out of `nodes/mapping/mod.rs` unchanged.

use crate::lex::SyntaxKind;
use crate::nodes::{
    append_children, fresh_token, has_child_token, trailing_newline_reachable, Scalar, SyntaxNode,
};
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
///
/// `terminated` says whether the mapping's last line ended in a line break
/// before it was drained; if so, the `{}` line gets one too.
pub(super) fn ensure_top_level_empty_renders_as_flow(mapping: &SyntaxNode, terminated: bool) {
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

    if !terminated {
        return;
    }
    // The line break follows the node, properties included, as a parsed
    // `{}` line has it.
    let node = mapping
        .parent()
        .filter(|p| p.kind() == SyntaxKind::TAGGED_NODE)
        .unwrap_or_else(|| mapping.clone());
    let Some(parent) = node
        .parent()
        .filter(|p| matches!(p.kind(), SyntaxKind::DOCUMENT | SyntaxKind::SEQUENCE_ENTRY))
    else {
        // TODO: keep the line break of a drained mapping elsewhere, such as
        // an explicit key.
        return;
    };
    let at = node.index() + 1;
    parent.splice_children(at..at, vec![fresh_token(SyntaxKind::NEWLINE, "\n").into()]);
}

/// Index just past `entry`'s line terminator among its siblings.
///
/// Entries normally own their trailing NEWLINE, so the slot after the entry
/// is the start of the next line. Explicit-key entries (`? k` / `: v`) are
/// parsed without one: theirs sits beside them as a sibling, and inserting
/// at the entry's own index would split it from its line break. A NEWLINE
/// beside an entry that owns its own is a blank line, and stays below.
pub(super) fn index_after_entry_line(parent: &SyntaxNode, idx: usize) -> usize {
    let entry = parent
        .children_with_tokens()
        .nth(idx)
        .and_then(|c| c.into_node())
        .expect("idx is the position of an entry");
    if trailing_newline_reachable(&entry) {
        return idx + 1;
    }
    let follows_newline = entry
        .next_sibling_or_token()
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
pub(crate) fn is_block_scalar(source: &SyntaxNode) -> bool {
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
/// aliases still resolve. `anchor` replaces any anchor the source has.
pub(crate) fn build_block_value_node(
    source: &SyntaxNode,
    target: usize,
    anchor: Option<&str>,
) -> BlockValue {
    if source.kind() == SyntaxKind::TAGGED_NODE {
        return build_tagged_value_node(source, target, anchor);
    }
    // Block scalars store their content column *inside* the SCALAR
    // (after `|`/`>` + NEWLINE). Block collections have it in the INDENT
    // that precedes them in their parent.
    let is_scalar = is_block_scalar(source);
    let delta = if is_scalar {
        block_scalar_body_shift(source, target)
    } else {
        target as isize - crate::as_yaml::source_base_indent(source) as isize
    };
    let anchor = anchor
        .map(str::to_string)
        .or_else(|| source_anchor_text(source));

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
    let node = SyntaxNode::new_root(builder.finish()).first_child()?;
    // A block scalar is laid out by build_content itself, given its key's
    // column, as for any other scalar.
    let scalar = match node.kind() {
        SyntaxKind::TAGGED_NODE => node.children().last(),
        _ => Some(node.clone()),
    };
    if scalar.is_some_and(|s| is_block_scalar(&s)) {
        return None;
    }
    Some(node)
}

/// Build a VALUE node wrapping a TAGGED_NODE source. Everything the
/// source holds (tag, anchor, comments, line breaks) is kept in place.
/// Content on lines after the tag is shifted to start at `target`; a block
/// scalar on the tag line has its content lines shifted to `target`.
/// `anchor` replaces any anchor the source has, before or after its tag.
pub(super) fn build_tagged_value_node(
    source: &SyntaxNode,
    target: usize,
    replacement_anchor: Option<&str>,
) -> BlockValue {
    let anchor = replacement_anchor
        .map(str::to_string)
        .or_else(|| source_anchor_text(source));
    let delta = source.first_child().map_or(0, |inner| {
        let content_on_tag_line = source
            .children_with_tokens()
            .take_while(|c| c.as_node().is_none())
            .all(|c| c.kind() != SyntaxKind::NEWLINE);
        if !content_on_tag_line {
            target as isize - crate::as_yaml::source_base_indent(&inner) as isize
        } else if is_block_scalar(&inner) {
            block_scalar_body_shift(&inner, target)
        } else {
            0
        }
    });

    let mut value_builder = GreenNodeBuilder::new();
    value_builder.start_node(SyntaxKind::VALUE.into());
    if let Some(a) = anchor.as_deref() {
        value_builder.token(SyntaxKind::ANCHOR.into(), a);
        value_builder.token(SyntaxKind::WHITESPACE.into(), " ");
    }
    value_builder.start_node(SyntaxKind::TAGGED_NODE.into());
    let ends_with_newline =
        crate::as_yaml::copy_node_content_reindent(&mut value_builder, source, delta);
    value_builder.finish_node(); // TAGGED_NODE
    value_builder.finish_node(); // VALUE
    let node = SyntaxNode::new_root_mut(value_builder.finish());
    if replacement_anchor.is_some() {
        drop_tagged_anchor(&node);
    }
    BlockValue {
        node,
        on_key_line: true,
        ends_with_newline,
    }
}

/// If `source` is preceded by an `&anchor` token in its parent VALUE, return
/// the anchor's text (including the leading `&`). Otherwise `None`.
pub(crate) fn source_anchor_text(source: &SyntaxNode) -> Option<String> {
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

/// The anchor among the properties of the node `holder` (a VALUE or
/// SEQUENCE_ENTRY) holds: before the node, or after the tag of a tagged
/// node.
pub(crate) fn held_anchor(holder: &SyntaxNode) -> Option<String> {
    held_anchor_token(holder).map(|t| t.text().to_string())
}

/// The token of [`held_anchor`].
pub(crate) fn held_anchor_token(holder: &SyntaxNode) -> Option<crate::nodes::SyntaxToken> {
    let tagged = holder
        .children()
        .next()
        .filter(|n| n.kind() == SyntaxKind::TAGGED_NODE);
    own_anchor(holder).or_else(|| tagged.and_then(|n| own_anchor(&n)))
}

/// The ANCHOR token among the children of `node` before its first node.
fn own_anchor(node: &SyntaxNode) -> Option<crate::nodes::SyntaxToken> {
    node.children_with_tokens()
        .map_while(|c| c.into_token())
        .find(|t| t.kind() == SyntaxKind::ANCHOR)
}

/// Remove the anchor after the tag of the tagged node `holder` holds, if
/// it has one, so another can go before the tag.
pub(crate) fn drop_tagged_anchor(holder: &SyntaxNode) {
    let anchor = holder
        .children()
        .find(|n| n.kind() == SyntaxKind::TAGGED_NODE)
        .and_then(|n| own_anchor(&n));
    if let Some(anchor) = anchor {
        crate::nodes::detach_anchor(&anchor);
    }
}

/// Find the content indent of a block scalar - the INDENT token right after
/// the `|`/`>` indicator and its NEWLINE. Returns `None` if the scalar is
/// empty (no content lines).
pub(crate) fn block_scalar_content_indent(scalar: &SyntaxNode) -> Option<usize> {
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

/// How far to shift the body of `scalar`, a block scalar from another CST,
/// to write it as the value of an entry whose content column is `target`,
/// two columns past the entry's own. The body's first line goes to
/// `target`, unless the scalar has an explicit indentation indicator
/// (`|2`): that counts from the entry's column, so the body keeps its
/// place relative to the entry instead, or its value would change.
pub(crate) fn block_scalar_body_shift(scalar: &SyntaxNode, target: usize) -> isize {
    if crate::nodes::scalar_node::block_scalar_indicators(scalar)
        .1
        .is_some()
    {
        let parent = crate::nodes::scalar_node::block_parent_column(scalar);
        return target.saturating_sub(2) as isize - parent as isize;
    }
    let body = block_scalar_content_indent(scalar).unwrap_or(target);
    target as isize - body as isize
}

/// `value` re-rendered in flow style, or `None` if it is already inline.
///
/// Block collections and block scalars can't appear inside a flow
/// collection or as an implicit key. Anchors, tags and aliases carry over.
/// Comments are kept: each one is moved to the next point where the flow
/// node can break its line (after an opening bracket, after a comma, or
/// before a closing bracket), with continuation lines at `indent`. A
/// result containing comments therefore spans several lines; see
/// [`has_comment`]. Building the result with `build_content` shifts those
/// lines by the indent it is given. The strings of a raw value are quoted
/// as `quoting` says; those of a node keep their style.
pub(crate) fn flow_form(
    value: &impl crate::AsYaml,
    indent: usize,
    quoting: crate::as_yaml::FlowQuoting,
) -> Option<crate::as_yaml::YamlNode> {
    if value.is_inline() {
        return None;
    }
    let Some(node) = value.as_node() else {
        // A raw value has no comments to place and can be built in flow
        // style directly.
        let mut builder = GreenNodeBuilder::new();
        value.build_flow_content(&mut builder, 0, quoting);
        return crate::as_yaml::YamlNode::from_syntax(SyntaxNode::new_root_mut(builder.finish()));
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
        if crate::nodes::ends_in_property_name(&written) {
            self.builder.token(SyntaxKind::WHITESPACE.into(), " ");
        }
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
