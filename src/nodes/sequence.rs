use super::replaced_value;
use super::{
    fresh_token, has_child_token, insert_children, nth_entry_index, replace_children, trivia, Lang,
    SyntaxNode,
};
use crate::as_yaml::{build_in, AsYaml, FlowQuoting, YamlKind};
use crate::lex::SyntaxKind;
use crate::nodes::empty_collection::{self, render_empty};
use crate::nodes::flow_quoting;
use crate::nodes::mapping::{drop_tagged_anchor, kept_anchor_token};
use crate::nodes::trivia::Slot;
use crate::yaml::ValueNode;
use rowan::ast::AstNode;
use rowan::GreenNodeBuilder;

ast_node!(Sequence, SEQUENCE, "A YAML sequence (list)");

/// True if `node` cannot be rendered as a block collection: any
/// flow ancestor forbids it.
pub(crate) fn must_render_flow(node: &SyntaxNode) -> bool {
    let mut cur = node.parent();
    while let Some(p) = cur {
        let opener = match p.kind() {
            SyntaxKind::MAPPING => Some(SyntaxKind::LEFT_BRACE),
            SyntaxKind::SEQUENCE => Some(SyntaxKind::LEFT_BRACKET),
            _ => None,
        };
        if let Some(open) = opener {
            // A pending-block placeholder is `{}` in the text but is headed
            // for block style, so it does not force flow on what nests inside
            // it (see `Mapping::new_pending_block`).
            if has_child_token(&p, |k| k == open) && !empty_collection::is_block_placeholder(&p) {
                return true;
            }
        }
        cur = p.parent();
    }
    false
}

/// Rebuild `entry` with `value` in place of the node it currently holds,
/// keeping everything else the entry carried.
fn rebuild_entry_with_value(
    entry: &SyntaxNode,
    value: &impl crate::AsYaml,
    flow: Option<FlowQuoting>,
    new_anchor: Option<&str>,
) -> SyntaxNode {
    let entry_children: Vec<_> = entry.children_with_tokens().collect();
    // Track the column while copying the entry, so the new value's later
    // lines line up under its first.
    let dash_column = crate::column::node_column(entry);
    let mut column = dash_column;

    // Build a new SEQUENCE_ENTRY with the new value using AsYaml
    let mut builder = GreenNodeBuilder::new();
    builder.start_node(SyntaxKind::SEQUENCE_ENTRY.into());

    // The old item's anchor goes on the new one, so aliases to it still
    // resolve; failing that `new_anchor`, or the anchor the new item had
    // where it was copied from. An alias cannot carry an anchor.
    let old_anchor = kept_anchor_token(entry, value).map(|t| t.text().to_string());
    let anchor = old_anchor
        .clone()
        .or_else(|| new_anchor.map(str::to_string))
        .or_else(|| {
            value
                .value_node()
                .filter(|_| value.kind() != YamlKind::Alias)
                .and_then(|n| crate::nodes::mapping::source_anchor_text(&n))
        });
    // An anchor before the old item is rewritten along with it, unless a
    // comment separates the two, in which case it stays where it is.
    let item = entry_item(entry);
    let item_at = entry_children
        .iter()
        .position(|c| c.as_node() == item.as_ref())
        .unwrap_or(entry_children.len());
    let outer_anchor_at = entry_children[..item_at]
        .iter()
        .position(|c| c.as_token().is_some_and(|t| t.kind() == SyntaxKind::ANCHOR));
    let rewrite_anchor = outer_anchor_at.map_or(true, |at| {
        entry_children[at + 1..item_at].iter().all(|c| {
            c.as_token().is_some_and(|t| {
                matches!(
                    t.kind(),
                    SyntaxKind::WHITESPACE | SyntaxKind::NEWLINE | SyntaxKind::INDENT
                )
            })
        })
    });
    let anchor = anchor.filter(|_| rewrite_anchor);
    // A block collection goes on the line after its anchor.
    let block_collection = flow.is_none()
        && !value.is_inline()
        && matches!(value.kind(), YamlKind::Mapping | YamlKind::Sequence);

    let mut value_inserted = false;
    let mut value_ends_with_newline = false;
    let mut old_ended_line = false;
    let mut after_dash = false;
    // A value ending in NEWLINE terminates the entry itself, so the old
    // terminator after it, up to this index, is dropped.
    let mut skip_until = 0;

    for (i, entry_child) in entry_children.iter().enumerate() {
        if i < skip_until {
            continue;
        }
        if rewrite_anchor && outer_anchor_at.is_some_and(|at| (at..item_at).contains(&i)) {
            continue;
        }
        match entry_child {
            rowan::NodeOrToken::Node(n)
                if matches!(
                    n.kind(),
                    SyntaxKind::SCALAR
                        | SyntaxKind::MAPPING
                        | SyntaxKind::SEQUENCE
                        | SyntaxKind::ALIAS
                        | SyntaxKind::TAGGED_NODE
                ) =>
            {
                // A multi-line value (a nested mapping, say) ends the
                // entry's line, which the new value has to as well.
                old_ended_line = crate::nodes::trivia::ends_line(n);
                if !value_inserted {
                    // A bare `-` item is DASH then a zero-width NULL
                    // scalar, so a written value needs the space that
                    // was never there: without it `set` gives `-x`.
                    if after_dash {
                        builder.token(SyntaxKind::WHITESPACE.into(), " ");
                        column += 1;
                    }
                    if let Some(anchor) = &anchor {
                        builder.token(SyntaxKind::ANCHOR.into(), anchor);
                        if block_collection {
                            column = dash_column + 2;
                            builder.token(SyntaxKind::NEWLINE.into(), "\n");
                            builder.token(SyntaxKind::INDENT.into(), &" ".repeat(column));
                        } else {
                            builder.token(SyntaxKind::WHITESPACE.into(), " ");
                            column += anchor.chars().count() + 1;
                        }
                    }
                    value_ends_with_newline =
                        build_entry_value(&mut builder, value, dash_column, column, flow);
                    value_inserted = true;
                    if value_ends_with_newline {
                        skip_until = terminator_end(&entry_children, i + 1);
                    }
                }
                after_dash = false;
            }
            // A SEQUENCE_ENTRY holds no other node; see the implicit-null
            // invariant in nodes/mod.rs.
            rowan::NodeOrToken::Node(n) => {
                panic!("unexpected {:?} in a SEQUENCE_ENTRY", n.kind())
            }
            rowan::NodeOrToken::Token(t) => {
                // A block collection on the dash's line is indented by the
                // white space after the dash, where a tab is not allowed.
                let text = if after_dash
                    && t.kind() == SyntaxKind::WHITESPACE
                    && block_collection
                    && anchor.is_none()
                {
                    t.text().replace('\t', " ")
                } else {
                    t.text().to_string()
                };
                builder.token(t.kind().into(), &text);
                column = crate::column::column_after(column, &text);
                after_dash = t.kind() == SyntaxKind::DASH;
            }
        }
    }

    if old_ended_line && !value_ends_with_newline {
        builder.token(SyntaxKind::NEWLINE.into(), "\n");
    }

    builder.finish_node();
    let new_entry = SyntaxNode::new_root_mut(builder.finish());
    if old_anchor.is_some() {
        drop_tagged_anchor(&new_entry);
    }
    new_entry
}

/// The item of a SEQUENCE_ENTRY.
fn entry_item(entry: &SyntaxNode) -> Option<SyntaxNode> {
    entry.children().find(|n| {
        matches!(
            n.kind(),
            SyntaxKind::SCALAR
                | SyntaxKind::MAPPING
                | SyntaxKind::SEQUENCE
                | SyntaxKind::ALIAS
                | SyntaxKind::TAGGED_NODE
        )
    })
}

/// A block sequence entry holding `value`, its dash at `column`, ending
/// in a line break.
fn block_entry(value: &impl crate::AsYaml, column: usize) -> SyntaxNode {
    let mut builder = GreenNodeBuilder::new();
    builder.start_node(SyntaxKind::SEQUENCE_ENTRY.into());
    builder.token(SyntaxKind::DASH.into(), "-");
    builder.token(SyntaxKind::WHITESPACE.into(), " ");
    if !build_entry_value(&mut builder, value, column, column + 2, None) {
        builder.token(SyntaxKind::NEWLINE.into(), "\n");
    }
    builder.finish_node();
    SyntaxNode::new_root_mut(builder.finish())
}

/// Emit `value` as the item of an entry whose dash is at `dash_column`,
/// the value itself starting at `column`. A flow sequence can't hold a
/// block collection, so one is written in flow form there. Returns
/// whether the content ends with a NEWLINE.
fn build_entry_value(
    builder: &mut GreenNodeBuilder,
    value: &impl crate::AsYaml,
    dash_column: usize,
    column: usize,
    flow: Option<FlowQuoting>,
) -> bool {
    // An inline value (a block scalar, say) takes the column of the entry
    // and indents its body past it; a block collection takes the column its
    // later lines align to, as in MappingEntry.
    let indent = |v: &dyn crate::AsYaml| if v.is_inline() { dash_column } else { column };
    // The flow form is inline, so it is shifted by dash_column when built;
    // its continuation lines are given relative to that.
    match flow.and_then(|q| crate::nodes::mapping::flow_form(value, 2, q)) {
        Some(flow_value) => build_in(&flow_value, builder, indent(&flow_value), flow),
        None => match value
            .value_node()
            .and_then(|n| Some((block_scalar_of(&n)?, n)))
        {
            Some((scalar, node)) => build_block_scalar(builder, &node, &scalar, dash_column + 2),
            None => build_in(value, builder, indent(value), flow),
        },
    }
}

/// The block scalar `node` is, or holds under its tag.
fn block_scalar_of(node: &SyntaxNode) -> Option<SyntaxNode> {
    let scalar = match node.kind() {
        SyntaxKind::TAGGED_NODE => node.first_child()?,
        _ => node.clone(),
    };
    crate::nodes::is_block_scalar(&scalar).then_some(scalar)
}

/// Copy `node`, the block scalar `scalar` or a tagged node holding it,
/// with the body shifted to `target`. Returns whether the copy ends with a
/// line break.
fn build_block_scalar(
    builder: &mut GreenNodeBuilder,
    node: &SyntaxNode,
    scalar: &SyntaxNode,
    target: usize,
) -> bool {
    let delta = crate::nodes::mapping::block_scalar_body_shift(scalar, target);
    builder.start_node(node.kind().into());
    let ends_with_newline = crate::as_yaml::copy_node_content_reindent(builder, node, delta);
    builder.finish_node();
    ends_with_newline
}

/// The index just past the NEWLINE that ends `children[start..]`, if only
/// whitespace precedes it, else `start`.
fn terminator_end(children: &[rowan::SyntaxElement<Lang>], start: usize) -> usize {
    let blank_or_newline = |c: &rowan::SyntaxElement<Lang>| {
        c.as_token()
            .map(|t| t.kind())
            .filter(|k| matches!(k, SyntaxKind::WHITESPACE | SyntaxKind::NEWLINE))
    };
    children[start..]
        .iter()
        .map_while(blank_or_newline)
        .position(|k| k == SyntaxKind::NEWLINE)
        .map_or(start, |offset| start + offset + 1)
}

impl Sequence {
    /// Iterate over items in this sequence as raw syntax nodes.
    ///
    /// For most use cases prefer [`values`](Self::values) which returns
    /// [`YamlNode`](crate::as_yaml::YamlNode)s.
    pub(crate) fn items(&self) -> impl Iterator<Item = SyntaxNode> + '_ {
        self.entries().filter_map(|e| entry_item(&e))
    }

    /// Iterate over items in this sequence as [`YamlNode`](crate::as_yaml::YamlNode)s.
    ///
    /// Items that cannot be wrapped as a `YamlNode` are silently skipped.
    pub fn values(&self) -> impl Iterator<Item = crate::as_yaml::YamlNode> + '_ {
        self.items()
            .filter_map(crate::as_yaml::YamlNode::from_syntax)
    }

    /// Returns the number of items in this sequence.
    pub fn len(&self) -> usize {
        self.items().count()
    }

    /// Returns `true` if this sequence contains no items.
    pub fn is_empty(&self) -> bool {
        self.items().next().is_none()
    }

    /// Get the item at `index` as a [`YamlNode`](crate::as_yaml::YamlNode).
    ///
    /// Returns `None` if `index` is out of bounds.
    pub fn get(&self, index: usize) -> Option<crate::as_yaml::YamlNode> {
        self.items()
            .nth(index)
            .and_then(crate::as_yaml::YamlNode::from_syntax)
    }

    /// Get the first item in this sequence, or `None` if empty.
    pub fn first(&self) -> Option<crate::as_yaml::YamlNode> {
        self.get(0)
    }

    /// Get the last item in this sequence, or `None` if empty.
    pub fn last(&self) -> Option<crate::as_yaml::YamlNode> {
        let len = self.len();
        if len == 0 {
            None
        } else {
            self.get(len - 1)
        }
    }
}

impl Sequence {
    /// Create a new empty sequence.
    pub fn new() -> Self {
        Self::new_pending_block()
    }

    /// An empty sequence that renders as `[]` and stays flow when filled.
    ///
    /// Use this for a sequence that is meant to be flow-style in the output,
    /// including one that is left empty.
    pub fn new_flow() -> Self {
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(SyntaxKind::SEQUENCE.into());
        builder.token(SyntaxKind::LEFT_BRACKET.into(), "[");
        builder.token(SyntaxKind::RIGHT_BRACKET.into(), "]");
        builder.finish_node();
        Sequence(SyntaxNode::new_root_mut(builder.finish()))
    }

    /// An empty sequence that becomes a block sequence once an item is added.
    ///
    /// YAML has no block syntax for an empty sequence: block collections are
    /// written as their entries, so with none left there is nothing to write,
    /// and a bare `key:` reads back as null. An empty sequence is therefore
    /// `[]` whatever its eventual style, and block-vs-flow only becomes a
    /// real choice once there is an item.
    ///
    /// This renders as exactly `[]`, plus a zero-width token that marks it as
    /// destined for block style. [`Sequence::push`] rewrites it into block
    /// form when the first item arrives; a `[]` that came from the source has
    /// no such marker and keeps its flow style.
    ///
    /// A block sequence that loses its last item to `remove` or `clear` is
    /// written the same way, marker included.
    ///
    /// The marker lives only in the tree. Serializing and re-parsing yields
    /// an ordinary `[]`, which stays flow, since YAML has nowhere to record
    /// the intent.
    pub fn new_pending_block() -> Self {
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(SyntaxKind::SEQUENCE.into());
        builder.token(SyntaxKind::LEFT_BRACKET.into(), "[");
        // Zero-width sentinel: renders as nothing, so this is still exactly
        // `[]` in the text, but marks the sequence as one that should become
        // block when it gets its first item.
        builder.token(SyntaxKind::WHITESPACE.into(), "");
        builder.token(SyntaxKind::RIGHT_BRACKET.into(), "]");
        builder.finish_node();
        Sequence(SyntaxNode::new_root_mut(builder.finish()))
    }

    /// Append `count` copies of `value` to the end of the sequence.
    ///
    /// Equivalent to calling [`push`](Self::push) `count` times, but splices
    /// the new entries in one edit rather than one each. Since a single edit
    /// is linear in the sequence's length (see `push`), doing them one at a
    /// time is quadratic: padding a sequence out to 1023 entries took 89ms
    /// that way and under 2ms this way.
    pub(crate) fn extend_with(&self, value: impl crate::AsYaml + Clone, count: usize) {
        if count == 0 {
            return;
        }
        // The first entry settles the questions push answers about style and
        // scaffolding -- flow vs block, the column, the placeholder newline.
        // Let it, then add the rest after it in one splice.
        self.push(value.clone());
        let Some(remaining) = count.checked_sub(1).filter(|n| *n > 0) else {
            return;
        };
        if self.is_flow_style() {
            for _ in 0..remaining {
                self.push(value.clone());
            }
            return;
        }

        let pushed = self
            .0
            .last_child()
            .filter(|n| n.kind() == SyntaxKind::SEQUENCE_ENTRY)
            .expect("push added an entry");
        let open = !trivia::ends_with_line_break(&pushed);
        trivia::terminate(&pushed);
        let column = trivia::entry_column(&self.0);
        let mut inserts: Vec<rowan::NodeOrToken<SyntaxNode, _>> = Vec::new();
        let mut last = None;
        for _ in 0..remaining {
            if column > 0 {
                inserts.push(fresh_token(SyntaxKind::INDENT, &" ".repeat(column)).into());
            }
            let entry = block_entry(&value, column);
            inserts.push(entry.clone().into());
            last = Some(entry);
        }
        if open {
            trivia::unterminate_last(&last.expect("at least one entry is added"));
        }
        let at = pushed.index() + 1;
        insert_children(&self.0, at, inserts);
    }

    /// Add an item to the end of the sequence.
    ///
    /// Mutates in place despite `&self` (see crate docs on interior mutability).
    ///
    /// # Performance
    ///
    /// Runs in time linear in the sequence's current length, so building a
    /// sequence one push at a time is quadratic overall. The cost is rowan
    /// rebuilding the green node for every edit, which an immutable tree has
    /// to do; this method already avoids the avoidable part by reading only
    /// the tail of the child list and holding no child handles across the
    /// edit. As a rough guide, 1000 pushes take about 70ms and 8000 about
    /// 4.5s (release build).
    ///
    /// For a large sequence, build it with
    /// [`SequenceBuilder`](crate::SequenceBuilder) or parse the YAML text in
    /// one go, rather than pushing in a loop: the builder is linear, and
    /// assembles those 8000 entries in under 2ms.
    /// [`insert`](Self::insert) and [`remove`](Self::remove) are likewise
    /// linear per call, and rather more costly than `push`.
    pub fn push(&self, value: impl crate::AsYaml) {
        self.attach(Slot::End, value);
    }

    /// Insert an item at a specific position.
    ///
    /// If `index` is out of bounds, the item is appended at the end.
    /// This method always succeeds; it never returns an error.
    ///
    /// Mutates in place despite `&self` (see crate docs on interior mutability).
    pub fn insert(&self, index: usize, value: impl crate::AsYaml) {
        let slot = self.entries().nth(index).map_or(Slot::End, Slot::Before);
        self.attach(slot, value);
    }

    /// Add an item holding `value` at `slot`.
    fn attach(&self, slot: Slot, value: impl crate::AsYaml) {
        // A placeholder from `Sequence::new_pending_block` turns into a real
        // block sequence now that it has an item. A `[]` from the source is
        // not marked and keeps its flow style; a placeholder nested inside a
        // flow container stays flow to avoid mixed output.
        empty_collection::convert_placeholder_to_block(&self.0);
        if self.is_flow_style() {
            trivia::attach_flow_entry(&self.0, slot, &self.flow_entry(value));
            return;
        }
        let entry = block_entry(&value, trivia::entry_column(&self.0));
        trivia::attach_entry(&self.0, slot, &entry);
    }

    /// A flow sequence entry holding `value`, for this sequence.
    fn flow_entry(&self, value: impl crate::AsYaml) -> SyntaxNode {
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(SyntaxKind::SEQUENCE_ENTRY.into());
        // Should the value need several lines (to keep comments), they are
        // indented past the entry holding this sequence, as for mappings.
        let base = self
            .0
            .ancestors()
            .find(|n| {
                matches!(
                    n.kind(),
                    SyntaxKind::MAPPING_ENTRY | SyntaxKind::SEQUENCE_ENTRY
                )
            })
            .map_or(0, |entry| crate::column::node_column(&entry));
        let flow = Some(flow_quoting(&self.0));
        build_entry_value(&mut builder, &value, base, base + 2, flow);
        builder.finish_node();
        SyntaxNode::new_root_mut(builder.finish())
    }

    /// Replace the item at `index` with a new value.
    ///
    /// Returns `true` if the index was in bounds and the item was replaced,
    /// `false` if `index >= len()`.
    ///
    /// An anchor on the replaced item stays, on the new item, so aliases to
    /// it still resolve. A node value that has an anchor of its own keeps
    /// it only if the old item had none. The first alias to an anchor
    /// deeper in the old item becomes a copy of the anchored node.
    ///
    /// Mutates in place despite `&self` (see crate docs on interior mutability).
    pub fn set(&self, index: usize, value: impl crate::AsYaml) -> bool {
        let Some(i) = nth_entry_index(&self.0, SyntaxKind::SEQUENCE_ENTRY, index) else {
            return false;
        };
        let node = self
            .0
            .children_with_tokens()
            .nth(i)
            .and_then(|c| c.into_node())
            .expect("entry index names a node");
        // An alias cannot carry an anchor; the first alias to the old one
        // gets a copy of the old item instead.
        let kept = crate::nodes::mapping::kept_anchor_token(&node, &value);
        crate::nodes::kept_aliases::keeping_aliases(&node, kept, || {
            Some(self.replace_entry_at(i, &value, None))
        });
        true
    }

    /// Replace the item at `index` as [`set`](Self::set) does, putting
    /// `anchor` on the new item, and return the new entry.
    pub(crate) fn replace_item_under(
        &self,
        index: usize,
        value: impl crate::AsYaml,
        anchor: Option<&str>,
    ) -> SyntaxNode {
        let i =
            nth_entry_index(&self.0, SyntaxKind::SEQUENCE_ENTRY, index).expect("index is in range");
        self.replace_entry_at(i, &value, anchor)
    }

    /// Replace the entry at child index `i` with one holding `value`, and
    /// return it. `anchor` goes on the new item if the old one had none.
    fn replace_entry_at(
        &self,
        i: usize,
        value: &impl crate::AsYaml,
        anchor: Option<&str>,
    ) -> SyntaxNode {
        let children: Vec<_> = self.0.children_with_tokens().collect();
        let node = children[i].as_node().expect("entry index names a node");
        let flow = self.is_flow_style().then(|| flow_quoting(&self.0));
        let dash_comment = match entry_item(node) {
            Some(old) if flow.is_none() => {
                replaced_value::remove_comment_after_item(&old);
                replaced_value::take_dash_line_comment(node)
            }
            _ => None,
        };
        let new_entry = rebuild_entry_with_value(node, value, flow, anchor);

        // Replace the old SEQUENCE_ENTRY with the new one
        replace_children(&self.0, i..i + 1, vec![new_entry.into()]);
        let new_entry = self
            .0
            .children_with_tokens()
            .nth(i)
            .and_then(|c| c.into_node());
        let new_entry = new_entry.expect("the new entry was just spliced in");
        replaced_value::settle_lines_after_block_scalar(&new_entry);
        if let Some(comment) = dash_comment {
            replaced_value::add_dash_line_comment(&new_entry, comment);
        }
        if let Some(item) = entry_item(&new_entry) {
            crate::nodes::trivia::drop_space_before_empty(&item);
        }
        new_entry
    }

    /// Remove the item at `index`, returning its value.
    ///
    /// Returns `Some(value)` if the index was in bounds, `None` otherwise.
    ///
    /// The first alias to an anchor in the removed item becomes a copy of
    /// the anchored node, so it and later aliases keep their value.
    ///
    /// Mutates in place despite `&self` (see crate docs on interior mutability).
    pub fn remove(&self, index: usize) -> Option<crate::as_yaml::YamlNode> {
        // Capture the value before removing so we can return it
        let removed_value = self.get(index);
        self.remove_entry(&self.entries().nth(index)?);
        removed_value
    }

    /// The SEQUENCE_ENTRY nodes of this sequence.
    fn entries(&self) -> impl Iterator<Item = SyntaxNode> {
        self.0
            .children()
            .filter(|n| n.kind() == SyntaxKind::SEQUENCE_ENTRY)
    }

    /// Remove `entry`, keeping aliases to anchors in it resolving.
    fn remove_entry(&self, entry: &SyntaxNode) {
        crate::nodes::kept_aliases::keeping_aliases(entry, None, || {
            trivia::detach_entry(&self.0, entry);
            None
        });
    }

    /// Check if this sequence is in flow style [item1, item2]
    pub fn is_flow_style(&self) -> bool {
        has_child_token(&self.0, |k| k == SyntaxKind::LEFT_BRACKET)
    }

    /// Remove and return the last item in this sequence.
    ///
    /// Returns `None` if the sequence is empty.
    ///
    /// Mutates in place despite `&self` (see crate docs on interior mutability).
    pub fn pop(&self) -> Option<crate::as_yaml::YamlNode> {
        self.remove(self.len().checked_sub(1)?)
    }

    /// Remove all items from this sequence.
    ///
    /// Mutates in place despite `&self` (see crate docs on interior mutability).
    pub fn clear(&self) {
        let line_break = self.entries().last().and_then(|e| trivia::line_break(&e));
        // Entries are removed one by one, as keeping aliases resolving may
        // rewrite a later item.
        while let Some(entry) = self.entries().next() {
            self.remove_entry(&entry);
        }
        render_empty(&self.0, line_break.as_deref());
    }

    crate::nodes::ast_node_spans!("sequence");
}

impl Default for Sequence {
    fn default() -> Self {
        Self::new()
    }
}

// Iterator trait implementations for Sequence

impl<'a> IntoIterator for &'a Sequence {
    type Item = crate::as_yaml::YamlNode;
    type IntoIter = Box<dyn Iterator<Item = crate::as_yaml::YamlNode> + 'a>;

    fn into_iter(self) -> Self::IntoIter {
        Box::new(self.values())
    }
}

impl AsYaml for Sequence {
    fn as_node(&self) -> Option<&SyntaxNode> {
        Some(&self.0)
    }

    fn kind(&self) -> YamlKind {
        YamlKind::Sequence
    }

    fn build_content(
        &self,
        builder: &mut rowan::GreenNodeBuilder,
        indent: usize,
        _flow_context: bool,
    ) -> bool {
        let is_flow = self.is_flow_style();
        let mut delta =
            indent as isize - crate::as_yaml::source_reindent_base(&self.0, is_flow) as isize;
        if is_flow {
            delta = crate::as_yaml::clear_parent_column(&self.0, indent, delta);
        }
        builder.start_node(SyntaxKind::SEQUENCE.into());
        crate::as_yaml::copy_node_content_reindent(builder, &self.0, delta);
        builder.finish_node();
        self.0
            .last_token()
            .is_some_and(|t| t.kind() == SyntaxKind::NEWLINE)
    }

    fn is_inline(&self) -> bool {
        ValueNode::is_inline(self)
    }
}
#[cfg(test)]
mod tests {
    use crate::debug::assert_cst_ok;
    use crate::yaml::YamlFile;
    use std::str::FromStr;

    /// `Sequence::new_pending_block` serializes as `[]` so an empty sequence
    /// survives a round-trip, and turns into a block sequence once it has an
    /// item. Before this it rendered as `key:` followed by blank space, which
    /// reparses as null: the empty sequence was silently lost.
    #[test]
    fn pending_block_sequence_round_trips_and_becomes_block() {
        use crate::yaml::Sequence;
        use crate::Document;
        use std::str::FromStr;

        let doc = Document::from_str("a: 1\n").unwrap();
        doc.as_mapping()
            .unwrap()
            .set("s", Sequence::new_pending_block());
        assert_cst_ok(&doc);
        let text = doc.to_string();
        assert_eq!(text, "a: 1\ns: []\n");

        let reparsed = Document::from_str(&text).unwrap();
        let value = reparsed.as_mapping().and_then(|m| m.get("s")).unwrap();
        assert!(
            value.as_sequence().is_some(),
            "empty sequence must survive as a sequence"
        );

        let doc = Document::from_str("a: 1\n").unwrap();
        let root = doc.as_mapping().unwrap();
        root.set("s", Sequence::new_pending_block());
        assert_cst_ok(&doc);
        let nested = root.get("s").unwrap();
        nested.as_sequence().unwrap().push("x");
        assert_cst_ok(&doc);
        assert_eq!(doc.to_string(), "a: 1\ns:\n  - x\n");
    }

    /// A `[]` that was in the source keeps its flow style when an item is
    /// added; only a pending-block placeholder is rewritten.
    #[test]
    fn parsed_flow_sequence_stays_flow_when_filled() {
        use crate::yaml::Sequence;
        use crate::Document;
        use std::str::FromStr;

        let doc = Document::from_str("a: 1\n").unwrap();
        let root = doc.as_mapping().unwrap();
        root.set("s", Sequence::new_flow());
        assert_cst_ok(&doc);
        let nested = root.get("s").unwrap();
        nested.as_sequence().unwrap().push("x");
        assert_cst_ok(&doc);
        assert_eq!(doc.to_string(), "a: 1\ns: [x]\n");
    }

    /// `set_path` builds sequence intermediates as pending-block sequences,
    /// so an indexed path produces readable block YAML.
    #[test]
    fn set_path_builds_block_sequence_intermediates() {
        use crate::path::YamlPath;
        use crate::Document;
        use std::str::FromStr;

        let doc = Document::from_str("name: test\n").unwrap();
        doc.try_set_path("a.b[0]", "v").unwrap();
        assert_cst_ok(&doc);
        assert_eq!(doc.to_string(), "name: test\na:\n  b:\n    - v\n");
    }

    /// `extend_with` must produce exactly what the same number of `push`
    /// calls would, for every sequence shape the padding path can meet.
    #[test]
    fn extend_with_matches_a_push_loop() {
        use crate::yaml::Document;
        use std::str::FromStr;

        for src in [
            "s:\n  - a\n",
            "s: []\n",
            "s: [1]\n",
            "s:\n  - x\n  - y\n",
            "s:\n    - deep\n",
            "s:\n  - a\nafter: kept\n",
            "s:\n  - a\n\nafter: kept\n",
            "s:\n  - a",
        ] {
            for count in 0..6usize {
                let pushed = {
                    let doc = Document::from_str(src).unwrap();
                    let seq = doc.as_mapping().unwrap().get_sequence("s").unwrap();
                    for _ in 0..count {
                        seq.push(crate::scalar::ScalarValue::null());
                        assert_cst_ok(&doc);
                    }
                    doc.to_string()
                };
                let extended = {
                    let doc = Document::from_str(src).unwrap();
                    let seq = doc.as_mapping().unwrap().get_sequence("s").unwrap();
                    seq.extend_with(crate::scalar::ScalarValue::null(), count);
                    assert_cst_ok(&doc);
                    doc.to_string()
                };
                assert_eq!(extended, pushed, "src={src:?} count={count}");
            }
        }
    }

    #[test]
    fn test_push_into_empty_flow_sequence_reshapes_to_block() {
        // Regression: `push` on an empty flow sequence `[]` used to append
        // the block entry after the `]`, producing `seq: []  - item1\n`.
        // A `[]` the source contained keeps its flow style, so the item goes
        // inside the brackets.
        use crate::path::YamlPath;
        use crate::Document;
        let doc = Document::from_str("seq: []").unwrap();
        let seq = doc
            .try_get_path("seq")
            .unwrap()
            .as_sequence()
            .unwrap()
            .clone();
        seq.push("item1");
        assert_cst_ok(&doc);
        assert_eq!(doc.to_string(), "seq: [item1]");
    }

    #[test]
    fn test_push_into_empty_flow_sequence_nested_indent() {
        use crate::path::YamlPath;
        use crate::Document;
        let doc = Document::from_str("a:\n  seq: []\n").unwrap();
        let seq = doc
            .try_get_path("a.seq")
            .unwrap()
            .as_sequence()
            .unwrap()
            .clone();
        seq.push("item1");
        assert_cst_ok(&doc);
        assert_eq!(doc.to_string(), "a:\n  seq: [item1]\n");
    }

    #[test]
    fn test_push_commented_value_into_nested_flow_sequence() {
        // Keeping the comment breaks the flow mapping across lines; the
        // continuation has to stay inside `a`.
        use crate::path::YamlPath;
        use crate::Document;
        use rowan::ast::AstNode;
        let doc = Document::from_str("a:\n  seq: [1]\n").unwrap();
        let seq = doc
            .try_get_path("a.seq")
            .unwrap()
            .as_sequence()
            .unwrap()
            .clone();
        let item = Document::from_str("x: 1 # c\ny: 2\n").unwrap();
        seq.push(item.as_mapping().unwrap());
        crate::debug::validate_tree(doc.syntax()).unwrap();
        crate::debug::roundtrip_ok(doc.syntax()).unwrap();
        assert_eq!(doc.to_string(), "a:\n  seq: [1, {x: 1, # c\n    y: 2}]\n");
    }

    #[test]
    fn test_push_deeply_nested_block_sequence_inherits_indent() {
        // Regression: `push` fell back to a 2-space INDENT for the new entry
        // because the sequence has no top-level INDENT (single entry) and its
        // one entry starts with DASH, not WHITESPACE. The correct column lives
        // on the parent VALUE's INDENT (the one before the SEQUENCE node).
        use crate::path::YamlPath;
        use crate::Document;
        let doc = Document::from_str("a:\n  b:\n    c:\n      - existing\n").unwrap();
        let seq = doc
            .try_get_path("a.b.c")
            .unwrap()
            .as_sequence()
            .unwrap()
            .clone();
        seq.push("new_item");
        assert_cst_ok(&doc);
        assert_eq!(
            doc.to_string(),
            "a:\n  b:\n    c:\n      - existing\n      - new_item\n"
        );
    }

    #[test]
    fn test_push_into_empty_sequence_under_mapping_placeholder() {
        // Regression: `mapping.set(k, Sequence::new())` produces the
        // placeholder shape `k:\n  \n` with an INDENT hint in the parent
        // VALUE. A follow-up `push` used to double the indent (4 spaces)
        // and leave a stray trailing newline.
        use crate::{Document, Sequence};
        let doc = Document::from_str("existing: value\n").unwrap();
        let mapping = doc.as_mapping().unwrap();
        mapping.set("items", Sequence::new());
        assert_cst_ok(&doc);
        let items = mapping.get_sequence("items").unwrap();
        items.push("apple");
        assert_cst_ok(&doc);
        items.push("banana");
        assert_cst_ok(&doc);
        assert_eq!(
            doc.to_string(),
            "existing: value\nitems:\n  - apple\n  - banana\n"
        );
    }

    #[test]
    fn test_implicit_null_item_shares_indexes_with_set_and_remove() {
        use crate::Document;
        let doc = Document::from_str("- a\n- \n- c\n").unwrap();
        let seq = doc.as_sequence().unwrap();
        assert_eq!(seq.len(), 3);
        assert_eq!(seq.get(0).unwrap().as_scalar().unwrap().as_string(), "a");
        assert_eq!(seq.get(1).unwrap().as_scalar().unwrap().as_string(), "");
        assert_eq!(seq.get(2).unwrap().as_scalar().unwrap().as_string(), "c");

        assert!(seq.set(1, "x"));
        assert_cst_ok(&doc);
        assert_eq!(doc.to_string(), "- a\n- x\n- c\n");

        let doc = Document::from_str("- a\n- \n- c\n").unwrap();
        let seq = doc.as_sequence().unwrap();
        let removed = seq.remove(1);
        assert_cst_ok(&doc);
        assert_eq!(
            removed.and_then(|n| n.as_scalar().map(|s| s.as_string())),
            Some(String::new())
        );
        assert_eq!(seq.len(), 2);
        assert_eq!(seq.get(1).unwrap().as_scalar().unwrap().as_string(), "c");

        let doc = Document::from_str("- a\n-\n- c\n").unwrap();
        let seq = doc.as_sequence().unwrap();
        assert_eq!(seq.len(), 3);
        assert!(seq.set(1, "x"));
        assert_cst_ok(&doc);
        assert_eq!(doc.to_string(), "- a\n- x\n- c\n");
    }

    #[test]
    fn test_flow_sequence_implicit_null_shares_indexes() {
        use crate::debug::{roundtrip_ok, validate_tree};
        use crate::{AsYaml, Document};

        // `[a, , c]` is three entries per YAML spec; the middle one is an
        // implicit null. Accessors must agree with mutators on the index
        // set, matching block-sequence behavior.
        let doc = Document::from_str("[a, , c]").unwrap();
        let seq = doc.as_sequence().unwrap();
        assert_eq!(seq.len(), 3);
        assert_eq!(seq.get(0).unwrap().as_scalar().unwrap().as_string(), "a");
        assert_eq!(seq.get(1).unwrap().as_scalar().unwrap().as_string(), "");
        assert_eq!(seq.get(2).unwrap().as_scalar().unwrap().as_string(), "c");
        validate_tree(doc.as_node().unwrap()).unwrap();
        roundtrip_ok(doc.as_node().unwrap()).unwrap();

        assert!(seq.set(1, "x"));
        assert_cst_ok(&doc);
        assert_eq!(doc.to_string(), "[a, x, c]");

        let doc = Document::from_str("[a, , c]").unwrap();
        let seq = doc.as_sequence().unwrap();
        let removed = seq.remove(1);
        assert_cst_ok(&doc);
        assert_eq!(
            removed.and_then(|n| n.as_scalar().map(|s| s.as_string())),
            Some(String::new())
        );
        assert_eq!(seq.len(), 2);
        assert_eq!(doc.to_string(), "[a, c]");
    }

    #[test]
    fn test_flow_sequence_trailing_comma_is_not_extra_entry() {
        use crate::Document;

        // `[a, b,]` has two entries: the trailing comma is a terminator,
        // not a separator introducing a null entry.
        let doc = Document::from_str("[a, b,]").unwrap();
        let seq = doc.as_sequence().unwrap();
        assert_eq!(seq.len(), 2);
        assert_eq!(seq.get(0).unwrap().as_scalar().unwrap().as_string(), "a");
        assert_eq!(seq.get(1).unwrap().as_scalar().unwrap().as_string(), "b");
        assert_eq!(doc.to_string(), "[a, b,]");
    }

    #[test]
    fn test_flow_sequence_only_nulls() {
        use crate::debug::{roundtrip_ok, validate_tree};
        use crate::{AsYaml, Document};

        // `[,]` is one null entry. `[,,]` is two.
        let doc = Document::from_str("[,]").unwrap();
        assert_eq!(doc.as_sequence().unwrap().len(), 1);
        validate_tree(doc.as_node().unwrap()).unwrap();
        roundtrip_ok(doc.as_node().unwrap()).unwrap();

        let doc = Document::from_str("[,,]").unwrap();
        assert_eq!(doc.as_sequence().unwrap().len(), 2);
        validate_tree(doc.as_node().unwrap()).unwrap();
        roundtrip_ok(doc.as_node().unwrap()).unwrap();
    }

    #[test]
    fn test_sequence_items_tagged_node() {
        // Tagged scalars inside sequences were previously skipped by items() because
        // TAGGED_NODE was not listed in the kind filter.
        let yaml = "- !custom foo\n- !custom bar\n- plain\n";
        let parsed = YamlFile::from_str(yaml).unwrap();

        let doc = parsed.document().unwrap();
        let seq = doc.as_sequence().unwrap();
        assert_eq!(
            seq.items().count(),
            3,
            "Tagged scalars should be included in items()"
        );
        // values() should also return tagged scalars (cast as Scalar YamlValues)
        assert_eq!(
            seq.values().count(),
            3,
            "Tagged scalars should be included in values()"
        );
    }

    #[test]
    fn test_sequence_set_tagged_node() {
        // Sequence::set() was missing TAGGED_NODE from its kind filter, so
        // replacing a tagged-scalar item would leave the original tag+value in place
        // and insert the new value alongside it.
        let yaml = "- !custom foo\n- bar\n";
        let parsed = YamlFile::from_str(yaml).unwrap();
        let doc = parsed.document().unwrap();
        let seq = doc.as_sequence().unwrap();

        seq.set(0, "replaced");
        assert_cst_ok(&parsed);

        let values: Vec<_> = seq.values().collect();
        assert_eq!(values.len(), 2);
        assert_eq!(
            values[0].as_scalar().map(|s| s.as_string()),
            Some("replaced".to_string())
        );
        assert_eq!(
            values[1].as_scalar().map(|s| s.as_string()),
            Some("bar".to_string())
        );
    }

    #[test]
    fn test_sequence_set_scalar_over_commented_block() {
        use crate::debug::{roundtrip_ok, shape_to_string, validate_tree};
        use crate::{AsYaml, Document};

        for (src, expected) in [
            ("-  # c\n  x: 1\n- 2\n", "- 5  # c\n- 2\n"),
            ("- # c\n  - 1\n", "- 5 # c\n"),
        ] {
            let doc = Document::from_str(src).unwrap();
            assert!(doc.as_sequence().unwrap().set(0, 5));
            let node = doc.as_node().unwrap();
            validate_tree(node).unwrap();
            roundtrip_ok(node).unwrap();
            assert_eq!(doc.to_string(), expected, "{src:?}");
            let reparsed = Document::from_str(expected).unwrap();
            assert_eq!(
                shape_to_string(node),
                shape_to_string(reparsed.as_node().unwrap()),
                "{src:?}"
            );
        }
    }

    #[test]
    fn test_sequence_set_block_mapping_adds_no_blank_line() {
        use crate::debug::{roundtrip_ok, validate_tree};
        use crate::{AsYaml, Document};

        let value = Document::from_str("k: 1\n").unwrap();
        let value = value.as_mapping().unwrap();
        for (src, index, expected) in [
            ("- a\n- b\n", 1, "- a\n- k: 1\n"),
            ("- a\n- b\n- c\n", 1, "- a\n- k: 1\n- c\n"),
            ("- a\n- b  \n- c\n", 1, "- a\n- k: 1\n- c\n"),
            ("- a\n- b", 1, "- a\n- k: 1\n"),
            ("- j: 2\n- b\n", 0, "- k: 1\n- b\n"),
            ("s:\n  - a\n  - b\nt: 1\n", 0, "s:\n  - k: 1\n  - b\nt: 1\n"),
        ] {
            let doc = Document::from_str(src).unwrap();
            let seq = doc
                .as_sequence()
                .or_else(|| doc.as_mapping()?.get_sequence("s"))
                .unwrap();
            assert!(seq.set(index, value.clone()));
            validate_tree(doc.as_node().unwrap()).unwrap();
            roundtrip_ok(doc.as_node().unwrap()).unwrap();
            assert_eq!(doc.to_string(), expected, "{src:?}");
        }
    }

    #[test]
    fn test_block_collection_values_take_the_entry_indent() {
        use crate::debug::{roundtrip_ok, shape_to_string, validate_tree};
        use crate::{AsYaml, Document, Sequence, YamlNode};

        let map_doc = Document::from_str("k: 1\nj: 2\n").unwrap();
        let map = YamlNode::Mapping(map_doc.as_mapping().unwrap());
        let nested_doc = Document::from_str("m:\n    k: 1\n    j: 2\n").unwrap();
        let nested = YamlNode::Mapping(nested_doc.as_mapping().unwrap().get_mapping("m").unwrap());
        let seq_doc = Document::from_str("- x\n- y\n").unwrap();
        let seq = YamlNode::Sequence(seq_doc.as_sequence().unwrap());

        let push = |s: &Sequence, v: &dyn AsYaml| s.push(v);
        let insert0 = |s: &Sequence, v: &dyn AsYaml| s.insert(0, v);
        let set0 = |s: &Sequence, v: &dyn AsYaml| assert!(s.set(0, v));
        type Op<'a> = &'a dyn Fn(&Sequence, &dyn AsYaml);
        let cases: Vec<(&str, Op, &dyn AsYaml, &str)> = vec![
            (
                "s:\n  - a\n",
                &push,
                &map,
                "s:\n  - a\n  - k: 1\n    j: 2\n",
            ),
            ("- a\n", &push, &map, "- a\n- k: 1\n  j: 2\n"),
            ("- a\n", &push, &nested, "- a\n- k: 1\n  j: 2\n"),
            (
                "a:\n  s:\n    - a\n",
                &push,
                &map,
                "a:\n  s:\n    - a\n    - k: 1\n      j: 2\n",
            ),
            ("s: [a]\n", &push, &map, "s: [a, {k: 1, j: 2}]\n"),
            ("s:\n  - a\n", &push, &seq, "s:\n  - a\n  - - x\n    - y\n"),
            ("- a\n", &push, &seq, "- a\n- - x\n  - y\n"),
            ("s: [a]\n", &push, &seq, "s: [a, [x, y]]\n"),
            (
                "s:\n  - a\n",
                &insert0,
                &map,
                "s:\n  - k: 1\n    j: 2\n  - a\n",
            ),
            ("- a\n", &insert0, &map, "- k: 1\n  j: 2\n- a\n"),
            ("s: [a]\n", &insert0, &map, "s: [{k: 1, j: 2}, a]\n"),
            (
                "s:\n  - a\n  - b\n",
                &set0,
                &map,
                "s:\n  - k: 1\n    j: 2\n  - b\n",
            ),
            ("- a\n", &set0, &map, "- k: 1\n  j: 2\n"),
            (
                "a:\n  s:\n    - a\n",
                &set0,
                &map,
                "a:\n  s:\n    - k: 1\n      j: 2\n",
            ),
            (
                "s:\n  - &x\n    k: 0\n",
                &set0,
                &map,
                "s:\n  - &x\n    k: 1\n    j: 2\n",
            ),
            ("s: [a, b]\n", &set0, &map, "s: [{k: 1, j: 2}, b]\n"),
        ];
        for (src, op, value, expected) in cases {
            let doc = Document::from_str(src).unwrap();
            let target = doc
                .as_sequence()
                .or_else(|| doc.as_mapping()?.get_sequence("s"))
                .or_else(|| doc.as_mapping()?.get_mapping("a")?.get_sequence("s"))
                .unwrap();
            op(&target, value);
            let node = doc.as_node().unwrap();
            assert_eq!(doc.to_string(), expected, "{src:?}");
            validate_tree(node).unwrap();
            roundtrip_ok(node).unwrap();
            let reparsed = Document::from_str(&doc.to_string()).unwrap();
            assert_eq!(
                shape_to_string(node),
                shape_to_string(reparsed.as_node().unwrap()),
                "{src:?}"
            );
        }
    }

    #[test]
    fn test_block_scalar_items_clear_the_dash() {
        use crate::debug::{assert_parser_shaped, roundtrip_ok, validate_tree};
        use crate::{AsYaml, Document};

        let literal = crate::ScalarValue::literal("x\ny\n");
        for (src, set, expected) in [
            ("- a\n", false, "- a\n- |\n  x\n  y\n"),
            ("s:\n  - a\n", false, "s:\n  - a\n  - |\n    x\n    y\n"),
            ("s:\n  - a\n", true, "s:\n  - |\n    x\n    y\n"),
            (
                "s:\n  - a  # c\n  - b\n",
                true,
                "s:\n  - |  # c\n    x\n    y\n  - b\n",
            ),
            ("- a # c\n", true, "- | # c\n  x\n  y\n"),
        ] {
            let doc = Document::from_str(src).unwrap();
            let seq = doc
                .as_sequence()
                .or_else(|| doc.as_mapping()?.get_sequence("s"))
                .unwrap();
            if set {
                assert!(seq.set(0, literal.clone()));
            } else {
                seq.push(literal.clone());
            }
            assert_eq!(doc.to_string(), expected, "{src:?}");
            validate_tree(doc.as_node().unwrap()).unwrap();
            roundtrip_ok(doc.as_node().unwrap()).unwrap();
            assert_parser_shaped(&doc);
        }
    }

    #[test]
    fn test_sequence_set_alias() {
        let yaml = "colors:\n  - *red\n  - keep\n";
        let parsed = YamlFile::from_str(yaml).unwrap();
        let doc = parsed.document().unwrap();
        let mapping = doc.as_mapping().unwrap();
        let colors_node = mapping.get("colors").unwrap();
        let seq = colors_node.as_sequence().unwrap();

        assert!(seq.set(0, "blue"));
        assert_cst_ok(&parsed);
        assert_eq!(parsed.to_string(), "colors:\n  - blue\n  - keep\n");

        let first = seq.get(0).unwrap();
        assert_eq!(
            first.as_scalar().map(|s| s.as_string()),
            Some("blue".to_string())
        );
    }

    #[test]
    fn test_sequence_operations() {
        let yaml = "- item1\n- item2";
        let parsed = YamlFile::from_str(yaml).unwrap();

        let doc = parsed.document().expect("expected a document");
        let seq = doc.as_sequence().expect("expected a sequence");

        // Test push
        seq.push("item3");
        assert_cst_ok(&parsed);
        let values: Vec<_> = seq.values().collect();
        assert_eq!(values.len(), 3);
        assert_eq!(
            values[2].as_scalar().map(|s| s.as_string()),
            Some("item3".to_string())
        );

        // Test insert
        seq.insert(0, "item0");
        assert_cst_ok(&parsed);
        let values: Vec<_> = seq.values().collect();
        assert_eq!(values.len(), 4);
        assert_eq!(
            values[0].as_scalar().map(|s| s.as_string()),
            Some("item0".to_string())
        );
    }

    // Iterator tests

    #[test]
    fn test_sequence_into_iterator() {
        use crate::Document;
        let text = "items:\n  - apple\n  - banana\n  - cherry";
        let doc = Document::from_str(text).unwrap();
        let mapping = doc.as_mapping().unwrap();
        let sequence = mapping.get_sequence("items").unwrap();

        // Test that we can use for loops directly
        let mut items = Vec::new();
        for value in &sequence {
            if let Some(scalar) = value.as_scalar() {
                items.push(scalar.to_string());
            }
        }

        assert_eq!(items.len(), 3);
        assert_eq!(items[0], "apple");
        assert_eq!(items[1], "banana");
        assert_eq!(items[2], "cherry");
    }

    #[test]
    fn test_sequence_into_iterator_count() {
        use crate::Document;
        let text = "[1, 2, 3, 4, 5]";
        let doc = Document::from_str(text).unwrap();
        let sequence = doc.as_sequence().unwrap();

        let count = (&sequence).into_iter().count();
        assert_eq!(count, 5);
    }

    #[test]
    fn test_sequence_iterator_map() {
        use crate::Document;
        let text = "numbers: [1, 2, 3]";
        let doc = Document::from_str(text).unwrap();
        let mapping = doc.as_mapping().unwrap();
        let sequence = mapping.get_sequence("numbers").unwrap();

        // Map to strings
        let strings: Vec<_> = (&sequence)
            .into_iter()
            .filter_map(|v| v.as_scalar().map(|s| s.to_string()))
            .collect();

        assert_eq!(strings, vec!["1", "2", "3"]);
    }

    #[test]
    fn test_empty_sequence_iterator() {
        use crate::Document;
        let text = "items: []";
        let doc = Document::from_str(text).unwrap();
        let mapping = doc.as_mapping().unwrap();
        let sequence = mapping.get_sequence("items").unwrap();

        let count = (&sequence).into_iter().count();
        assert_eq!(count, 0);
    }

    // Tests from sequence_operations_test.rs

    #[test]
    fn test_sequence_push_single() {
        use crate::Document;
        let original = r#"team:
  - Alice
  - Bob"#;

        let doc = Document::from_str(original).unwrap();
        let mapping = doc.as_mapping().unwrap();
        let team = mapping.get_sequence("team").unwrap();
        team.push("Charlie");
        assert_cst_ok(&doc);

        let expected = r#"team:
  - Alice
  - Bob
  - Charlie"#;
        assert_eq!(doc.to_string(), expected);
    }

    #[test]
    fn test_sequence_push_multiple() {
        use crate::Document;
        let original = r#"team:
  - Alice
  - Bob"#;

        let doc = Document::from_str(original).unwrap();
        let mapping = doc.as_mapping().unwrap();
        let team = mapping.get_sequence("team").unwrap();
        team.push("Charlie");
        assert_cst_ok(&doc);
        team.push("Diana");
        assert_cst_ok(&doc);

        let expected = r#"team:
  - Alice
  - Bob
  - Charlie
  - Diana"#;
        assert_eq!(doc.to_string(), expected);
    }

    #[test]
    fn test_sequence_set_item() {
        use crate::Document;
        let original = r#"team:
  - Alice
  - Bob
  - Charlie"#;

        let doc = Document::from_str(original).unwrap();
        let mapping = doc.as_mapping().unwrap();
        let team = mapping.get_sequence("team").unwrap();
        team.set(1, "Robert");
        assert_cst_ok(&doc);

        let expected = r#"team:
  - Alice
  - Robert
  - Charlie"#;
        assert_eq!(doc.to_string(), expected);
    }

    #[test]
    fn test_multiple_sequences() {
        use crate::Document;
        let original = r#"team:
  - Alice
  - Bob

scores:
  - 95
  - 87"#;

        let doc = Document::from_str(original).unwrap();
        let mapping = doc.as_mapping().unwrap();
        let team = mapping.get_sequence("team").unwrap();
        team.push("Charlie");
        assert_cst_ok(&doc);
        let scores = mapping.get_sequence("scores").unwrap();
        scores.push(92);
        assert_cst_ok(&doc);
        scores.set(0, 100);
        assert_cst_ok(&doc);

        let expected = r#"team:
  - Alice
  - Bob
  - Charlie

scores:
  - 100
  - 87
  - 92"#;
        assert_eq!(doc.to_string(), expected);
    }

    #[test]
    fn test_nested_structure_with_sequences() {
        use crate::Document;
        let original = r#"config:
  enabled: true
  retries: 3
  servers:
    - host1
    - host2"#;

        let doc = Document::from_str(original).unwrap();
        let mapping = doc.as_mapping().unwrap();
        let config = mapping.get_mapping("config").unwrap();
        config.set("enabled", false);
        assert_cst_ok(&doc);
        config.set("retries", 5);
        assert_cst_ok(&doc);

        let servers = config.get_sequence("servers").unwrap();
        servers.push("host3");
        assert_cst_ok(&doc);
        servers.set(0, "primary-host");
        assert_cst_ok(&doc);

        let expected = r#"config:
  enabled: false
  retries: 5
  servers:
    - primary-host
    - host2
    - host3"#;
        assert_eq!(doc.to_string(), expected);
    }

    #[test]
    fn test_sequence_len_and_is_empty() {
        use crate::Document;
        let doc = Document::from_str("items:\n  - a\n  - b\n  - c").unwrap();
        let mapping = doc.as_mapping().unwrap();
        let seq = mapping.get_sequence("items").unwrap();

        assert_eq!(seq.len(), 3);
        assert!(!seq.is_empty());

        let empty_doc = Document::from_str("items: []").unwrap();
        let empty_mapping = empty_doc.as_mapping().unwrap();
        let empty_seq = empty_mapping.get_sequence("items").unwrap();

        assert_eq!(empty_seq.len(), 0);
        assert!(empty_seq.is_empty());
    }

    #[test]
    fn test_sequence_get() {
        use crate::Document;
        let doc = Document::from_str("items:\n  - first\n  - second\n  - third").unwrap();
        let mapping = doc.as_mapping().unwrap();
        let seq = mapping.get_sequence("items").unwrap();

        assert_eq!(seq.get(0).unwrap().to_string(), "first");
        assert_eq!(seq.get(1).unwrap().to_string(), "second");
        assert_eq!(seq.get(2).unwrap().to_string(), "third");
        assert!(seq.get(3).is_none());
    }

    #[test]
    fn test_sequence_first_and_last() {
        use crate::Document;
        let doc = Document::from_str("items:\n  - first\n  - middle\n  - last").unwrap();
        let mapping = doc.as_mapping().unwrap();
        let seq = mapping.get_sequence("items").unwrap();

        assert_eq!(seq.first().unwrap().to_string(), "first");
        assert_eq!(seq.last().unwrap().to_string(), "last");

        let empty_doc = Document::from_str("items: []").unwrap();
        let empty_mapping = empty_doc.as_mapping().unwrap();
        let empty_seq = empty_mapping.get_sequence("items").unwrap();

        assert!(empty_seq.first().is_none());
        assert!(empty_seq.last().is_none());
    }

    #[test]
    fn test_sequence_values_iterator() {
        use crate::Document;
        let doc = Document::from_str("items:\n  - a\n  - b\n  - c").unwrap();
        let mapping = doc.as_mapping().unwrap();
        let seq = mapping.get_sequence("items").unwrap();

        let values: Vec<String> = seq.values().map(|v| v.to_string()).collect();
        assert_eq!(values, vec!["a", "b", "c"]);
    }

    #[test]
    fn test_sequence_pop() {
        use crate::Document;
        let doc = Document::from_str("items:\n  - a\n  - b\n  - c").unwrap();
        let mapping = doc.as_mapping().unwrap();
        let seq = mapping.get_sequence("items").unwrap();

        assert_eq!(seq.len(), 3);
        let popped = seq.pop().unwrap();
        assert_cst_ok(&doc);
        assert_eq!(popped.to_string(), "c");
        assert_eq!(seq.len(), 2);

        let popped = seq.pop().unwrap();
        assert_cst_ok(&doc);
        assert_eq!(popped.to_string(), "b");
        assert_eq!(seq.len(), 1);

        let expected = "items:\n  - a";
        assert_eq!(doc.to_string().trim_end(), expected);

        let popped = seq.pop().unwrap();
        assert_cst_ok(&doc);
        assert_eq!(popped.to_string(), "a");
        assert_eq!(seq.len(), 0);
        assert!(seq.pop().is_none());
        assert_cst_ok(&doc);
    }

    #[test]
    fn test_sequence_clear() {
        use crate::Document;
        let doc = Document::from_str("items:\n  - a\n  - b\n  - c").unwrap();
        let mapping = doc.as_mapping().unwrap();
        let seq = mapping.get_sequence("items").unwrap();

        assert_eq!(seq.len(), 3);
        seq.clear();
        assert_cst_ok(&doc);
        assert_eq!(seq.len(), 0);
        assert!(seq.is_empty());
    }

    #[test]
    fn test_sequence_get_with_nested_values() {
        use crate::Document;
        let doc = Document::from_str(
            r#"items:
  - simple
  - {key: value}
  - [nested, list]"#,
        )
        .unwrap();
        let mapping = doc.as_mapping().unwrap();
        let seq = mapping.get_sequence("items").unwrap();

        assert_eq!(seq.len(), 3);
        assert!(seq.get(0).unwrap().is_scalar());
        assert!(seq.get(1).unwrap().is_mapping());
        assert!(seq.get(2).unwrap().is_sequence());
    }

    #[test]
    fn test_flow_sequence_len_and_is_empty() {
        use crate::Document;
        let doc = Document::from_str("items: [a, b, c]").unwrap();
        let mapping = doc.as_mapping().unwrap();
        let seq = mapping.get_sequence("items").unwrap();

        assert_eq!(seq.len(), 3);
        assert!(!seq.is_empty());

        let empty_doc = Document::from_str("items: []").unwrap();
        let empty_mapping = empty_doc.as_mapping().unwrap();
        let empty_seq = empty_mapping.get_sequence("items").unwrap();

        assert_eq!(empty_seq.len(), 0);
        assert!(empty_seq.is_empty());
    }

    #[test]
    fn test_flow_sequence_get() {
        use crate::Document;
        let doc = Document::from_str("items: [first, second, third]").unwrap();
        let mapping = doc.as_mapping().unwrap();
        let seq = mapping.get_sequence("items").unwrap();

        assert_eq!(seq.get(0).unwrap().to_string(), "first");
        assert_eq!(seq.get(1).unwrap().to_string(), "second");
        assert_eq!(seq.get(2).unwrap().to_string(), "third");
        assert!(seq.get(3).is_none());
    }

    #[test]
    fn test_flow_sequence_first_and_last() {
        use crate::Document;
        let doc = Document::from_str("items: [first, middle, last]").unwrap();
        let mapping = doc.as_mapping().unwrap();
        let seq = mapping.get_sequence("items").unwrap();

        assert_eq!(seq.first().unwrap().to_string(), "first");
        assert_eq!(seq.last().unwrap().to_string(), "last");
    }

    #[test]
    fn test_flow_sequence_values_iterator() {
        use crate::Document;
        let doc = Document::from_str("items: [a, b, c]").unwrap();
        let mapping = doc.as_mapping().unwrap();
        let seq = mapping.get_sequence("items").unwrap();

        let values: Vec<String> = seq.values().map(|v| v.to_string()).collect();
        assert_eq!(values, vec!["a", "b", "c"]);
    }

    #[test]
    fn test_flow_sequence_remove_middle() {
        use crate::Document;
        let doc = Document::from_str("items: [a, b, c]").unwrap();
        let mapping = doc.as_mapping().unwrap();
        let seq = mapping.get_sequence("items").unwrap();

        assert_eq!(seq.len(), 3);
        let removed = seq.remove(1);
        assert_cst_ok(&doc);
        assert_eq!(removed.map(|v| v.to_string()), Some("b".to_string()));
        assert_eq!(seq.len(), 2);

        let values: Vec<String> = seq.values().map(|v| v.to_string()).collect();
        assert_eq!(values, vec!["a", "c"]);
    }

    #[test]
    fn test_flow_sequence_remove_first() {
        use crate::Document;
        let doc = Document::from_str("items: [a, b, c]").unwrap();
        let mapping = doc.as_mapping().unwrap();
        let seq = mapping.get_sequence("items").unwrap();

        assert_eq!(seq.len(), 3);
        let removed = seq.remove(0);
        assert_cst_ok(&doc);
        assert_eq!(removed.map(|v| v.to_string()), Some("a".to_string()));
        assert_eq!(seq.len(), 2);

        let values: Vec<String> = seq.values().map(|v| v.to_string()).collect();
        assert_eq!(values, vec!["b", "c"]);
    }

    #[test]
    fn test_flow_sequence_remove_last() {
        use crate::Document;
        let doc = Document::from_str("items: [a, b, c]").unwrap();
        let mapping = doc.as_mapping().unwrap();
        let seq = mapping.get_sequence("items").unwrap();

        assert_eq!(seq.len(), 3);
        let removed = seq.remove(2);
        assert_cst_ok(&doc);
        assert_eq!(removed.map(|v| v.to_string()), Some("c".to_string()));
        assert_eq!(seq.len(), 2);

        let values: Vec<String> = seq.values().map(|v| v.to_string()).collect();
        assert_eq!(values, vec!["a", "b"]);
    }

    #[test]
    fn test_flow_sequence_pop() {
        use crate::Document;
        let doc = Document::from_str("items: [a, b, c]").unwrap();
        let mapping = doc.as_mapping().unwrap();
        let seq = mapping.get_sequence("items").unwrap();

        assert_eq!(seq.len(), 3);
        let popped = seq.pop().unwrap();
        assert_cst_ok(&doc);
        assert_eq!(popped.to_string(), "c");
        assert_eq!(seq.len(), 2);

        let popped = seq.pop().unwrap();
        assert_cst_ok(&doc);
        assert_eq!(popped.to_string(), "b");
        assert_eq!(seq.len(), 1);

        let popped = seq.pop().unwrap();
        assert_cst_ok(&doc);
        assert_eq!(popped.to_string(), "a");
        assert_eq!(seq.len(), 0);
        assert!(seq.pop().is_none());
        assert_cst_ok(&doc);
    }

    #[test]
    fn test_flow_sequence_clear() {
        use crate::Document;
        let doc = Document::from_str("items: [a, b, c]").unwrap();
        let mapping = doc.as_mapping().unwrap();
        let seq = mapping.get_sequence("items").unwrap();

        assert_eq!(seq.len(), 3);
        seq.clear();
        assert_cst_ok(&doc);
        assert_eq!(seq.len(), 0);
        assert!(seq.is_empty());
    }

    #[test]
    fn test_flow_sequence_with_whitespace() {
        use crate::Document;
        let doc = Document::from_str("items: [ a , b , c ]").unwrap();
        let mapping = doc.as_mapping().unwrap();
        let seq = mapping.get_sequence("items").unwrap();

        assert_eq!(seq.len(), 3);
        let values: Vec<String> = seq.values().map(|v| v.to_string()).collect();
        assert_eq!(values, vec!["a", "b", "c"]);
    }

    #[test]
    fn test_block_sequence_remove_middle() {
        use crate::Document;
        let doc = Document::from_str("items:\n  - a\n  - b\n  - c").unwrap();
        let mapping = doc.as_mapping().unwrap();
        let seq = mapping.get_sequence("items").unwrap();

        assert_eq!(seq.len(), 3);
        let removed = seq.remove(1);
        assert_cst_ok(&doc);
        assert_eq!(removed.map(|v| v.to_string()), Some("b".to_string()));
        assert_eq!(seq.len(), 2);

        let values: Vec<String> = seq.values().map(|v| v.to_string()).collect();
        assert_eq!(values, vec!["a", "c"]);
    }

    #[test]
    fn test_block_sequence_remove_first() {
        use crate::Document;
        let doc = Document::from_str("items:\n  - a\n  - b\n  - c").unwrap();
        let mapping = doc.as_mapping().unwrap();
        let seq = mapping.get_sequence("items").unwrap();

        assert_eq!(seq.len(), 3);
        let removed = seq.remove(0);
        assert_cst_ok(&doc);
        assert_eq!(removed.map(|v| v.to_string()), Some("a".to_string()));
        assert_eq!(seq.len(), 2);

        let values: Vec<String> = seq.values().map(|v| v.to_string()).collect();
        assert_eq!(values, vec!["b", "c"]);
    }

    #[test]
    fn test_block_sequence_remove_last() {
        use crate::Document;
        let doc = Document::from_str("items:\n  - a\n  - b\n  - c").unwrap();
        let mapping = doc.as_mapping().unwrap();
        let seq = mapping.get_sequence("items").unwrap();

        assert_eq!(seq.len(), 3);
        let removed = seq.remove(2);
        assert_cst_ok(&doc);
        assert_eq!(removed.map(|v| v.to_string()), Some("c".to_string()));
        assert_eq!(seq.len(), 2);

        let values: Vec<String> = seq.values().map(|v| v.to_string()).collect();
        assert_eq!(values, vec!["a", "b"]);
    }

    #[test]
    fn test_single_item_block_sequence_remove() {
        use crate::Document;
        let doc = Document::from_str("items:\n  - only").unwrap();
        let mapping = doc.as_mapping().unwrap();
        let seq = mapping.get_sequence("items").unwrap();

        assert_eq!(seq.len(), 1);
        let removed = seq.remove(0);
        assert_cst_ok(&doc);
        assert_eq!(removed.map(|v| v.to_string()), Some("only".to_string()));
        assert_eq!(seq.len(), 0);
    }

    #[test]
    fn test_single_item_flow_sequence_remove() {
        use crate::Document;
        let doc = Document::from_str("items: [only]").unwrap();
        let mapping = doc.as_mapping().unwrap();
        let seq = mapping.get_sequence("items").unwrap();

        assert_eq!(seq.len(), 1);
        let removed = seq.remove(0);
        assert_cst_ok(&doc);
        assert_eq!(removed.map(|v| v.to_string()), Some("only".to_string()));
        assert_eq!(seq.len(), 0);
    }

    #[test]
    fn test_flow_sequence_push() {
        use crate::Document;
        let doc = Document::from_str("items: [a, b]").unwrap();
        let mapping = doc.as_mapping().unwrap();
        let seq = mapping.get_sequence("items").unwrap();

        assert_eq!(seq.len(), 2);
        seq.push("c");
        assert_cst_ok(&doc);
        assert_eq!(seq.len(), 3);

        let values: Vec<String> = seq.values().map(|v| v.to_string()).collect();
        assert_eq!(values, vec!["a", "b", "c"]);
    }

    #[test]
    fn test_flow_sequence_push_multiple() {
        use crate::Document;
        let doc = Document::from_str("items: [a]").unwrap();
        let mapping = doc.as_mapping().unwrap();
        let seq = mapping.get_sequence("items").unwrap();

        seq.push("b");
        assert_cst_ok(&doc);
        seq.push("c");
        assert_cst_ok(&doc);
        seq.push("d");
        assert_cst_ok(&doc);

        let values: Vec<String> = seq.values().map(|v| v.to_string()).collect();
        assert_eq!(values, vec!["a", "b", "c", "d"]);
    }

    #[test]
    fn test_flow_sequence_set_item() {
        use crate::Document;
        let doc = Document::from_str("items: [a, b, c]").unwrap();
        let mapping = doc.as_mapping().unwrap();
        let seq = mapping.get_sequence("items").unwrap();

        seq.set(1, "modified");
        assert_cst_ok(&doc);

        let values: Vec<String> = seq.values().map(|v| v.to_string()).collect();
        assert_eq!(values, vec!["a", "modified", "c"]);
    }

    #[test]
    fn test_flow_sequence_insert_beginning() {
        use crate::Document;
        let doc = Document::from_str("items: [b, c]").unwrap();
        let mapping = doc.as_mapping().unwrap();
        let seq = mapping.get_sequence("items").unwrap();

        seq.insert(0, "a");
        assert_cst_ok(&doc);

        let values: Vec<String> = seq.values().map(|v| v.to_string()).collect();
        assert_eq!(values, vec!["a", "b", "c"]);
    }

    #[test]
    fn test_flow_sequence_insert_middle() {
        use crate::Document;
        let doc = Document::from_str("items: [a, c]").unwrap();
        let mapping = doc.as_mapping().unwrap();
        let seq = mapping.get_sequence("items").unwrap();

        seq.insert(1, "b");
        assert_cst_ok(&doc);

        let values: Vec<String> = seq.values().map(|v| v.to_string()).collect();
        assert_eq!(values, vec!["a", "b", "c"]);
    }

    #[test]
    fn test_flow_sequence_insert_end() {
        use crate::Document;
        let doc = Document::from_str("items: [a, b]").unwrap();
        let mapping = doc.as_mapping().unwrap();
        let seq = mapping.get_sequence("items").unwrap();

        seq.insert(2, "c");
        assert_cst_ok(&doc);

        let values: Vec<String> = seq.values().map(|v| v.to_string()).collect();
        assert_eq!(values, vec!["a", "b", "c"]);
    }

    #[test]
    fn test_block_sequence_push() {
        use crate::Document;
        let doc = Document::from_str("items:\n  - a\n  - b").unwrap();
        let mapping = doc.as_mapping().unwrap();
        let seq = mapping.get_sequence("items").unwrap();

        assert_eq!(seq.len(), 2);
        seq.push("c");
        assert_cst_ok(&doc);
        assert_eq!(seq.len(), 3);

        let values: Vec<String> = seq.values().map(|v| v.to_string()).collect();
        assert_eq!(values, vec!["a", "b", "c"]);
    }

    #[test]
    fn test_block_sequence_set_item() {
        use crate::Document;
        let doc = Document::from_str("items:\n  - a\n  - b\n  - c").unwrap();
        let mapping = doc.as_mapping().unwrap();
        let seq = mapping.get_sequence("items").unwrap();

        seq.set(1, "modified");
        assert_cst_ok(&doc);

        let values: Vec<String> = seq.values().map(|v| v.to_string()).collect();
        assert_eq!(values, vec!["a", "modified", "c"]);
    }

    #[test]
    fn test_block_sequence_insert_beginning() {
        use crate::Document;
        let doc = Document::from_str("items:\n  - b\n  - c").unwrap();
        let mapping = doc.as_mapping().unwrap();
        let seq = mapping.get_sequence("items").unwrap();

        seq.insert(0, "a");
        assert_cst_ok(&doc);

        let values: Vec<String> = seq.values().map(|v| v.to_string()).collect();
        assert_eq!(values, vec!["a", "b", "c"]);
    }

    #[test]
    fn test_block_sequence_insert_middle() {
        use crate::Document;
        let doc = Document::from_str("items:\n  - a\n  - c").unwrap();
        let mapping = doc.as_mapping().unwrap();
        let seq = mapping.get_sequence("items").unwrap();

        seq.insert(1, "b");
        assert_cst_ok(&doc);

        let values: Vec<String> = seq.values().map(|v| v.to_string()).collect();
        assert_eq!(values, vec!["a", "b", "c"]);
    }

    #[test]
    fn test_block_sequence_insert_end() {
        use crate::Document;
        let doc = Document::from_str("items:\n  - a\n  - b").unwrap();
        let mapping = doc.as_mapping().unwrap();
        let seq = mapping.get_sequence("items").unwrap();

        seq.insert(2, "c");
        assert_cst_ok(&doc);

        let values: Vec<String> = seq.values().map(|v| v.to_string()).collect();
        assert_eq!(values, vec!["a", "b", "c"]);
    }

    #[test]
    fn test_sequence_get_node() {
        let doc = YamlFile::from_str("items:\n  - alpha\n  - beta\n  - gamma")
            .unwrap()
            .document()
            .unwrap();
        let seq = doc.as_mapping().unwrap().get_sequence("items").unwrap();

        assert_eq!(seq.len(), 3);
        assert!(seq.get(0).is_some());
        assert!(seq.get(1).is_some());
        assert!(seq.get(2).is_some());
        assert!(seq.get(3).is_none());

        assert_eq!(
            seq.get(0).unwrap().as_scalar().unwrap().as_string(),
            "alpha"
        );
        assert_eq!(seq.get(1).unwrap().as_scalar().unwrap().as_string(), "beta");
        assert_eq!(
            seq.get(2).unwrap().as_scalar().unwrap().as_string(),
            "gamma"
        );
    }

    #[test]
    fn test_sequence_set_with_nested_mapping() {
        use crate::path::YamlPath;
        use crate::Document;

        let yaml_str = "items:\n  - name: first\n    value: 1\n  - name: second\n    value: 2\n";
        let doc = Document::from_str(yaml_str).unwrap();

        let items_node = doc.try_get_path("items").unwrap();
        let items = items_node.as_sequence().unwrap();

        assert_eq!(items.len(), 2);
        let first = items.get(0).unwrap();
        assert!(first.is_mapping());

        items.set(0, "replaced");
        assert_cst_ok(&doc);
        assert_eq!(
            doc.to_string(),
            "items:\n  - replaced\n  - name: second\n    value: 2\n"
        );
    }

    /// A block sequence may sit at its key's column (`a:\n- 1`); new items
    /// go in that column too, not two spaces in.
    #[test]
    fn test_add_to_sequence_at_key_column() {
        let cases: &[(&str, &dyn Fn(&crate::Sequence), &str)] = &[
            ("a:\n- 1\n", &|s| s.push("x"), "a:\n- 1\n- x\n"),
            ("a:\n- 1", &|s| s.push("x"), "a:\n- 1\n- x"),
            ("a:\n- 1\n", &|s| s.insert(0, "x"), "a:\n- x\n- 1\n"),
            ("a:\n- 1\n", &|s| s.insert(1, "x"), "a:\n- 1\n- x\n"),
            (
                "a:\n- 1\n- 2\n",
                &|s| s.insert(0, "x"),
                "a:\n- x\n- 1\n- 2\n",
            ),
            (
                "a:\n- 1\n- 2\n",
                &|s| s.insert(1, "x"),
                "a:\n- 1\n- x\n- 2\n",
            ),
            ("a:\n- 1\nb: 2\n", &|s| s.push("x"), "a:\n- 1\n- x\nb: 2\n"),
            (
                "b:\n  a:\n  - 1\n",
                &|s| s.push("x"),
                "b:\n  a:\n  - 1\n  - x\n",
            ),
        ];
        for (source, edit, expected) in cases {
            let yaml = YamlFile::from_str(source).unwrap();
            let doc = yaml.document().unwrap();
            let root = doc.as_mapping().unwrap();
            let seq = match root.get_sequence("a") {
                Some(seq) => seq,
                None => root.get_mapping("b").unwrap().get_sequence("a").unwrap(),
            };
            edit(&seq);
            crate::debug::assert_edited(&yaml, expected);
        }
    }

    /// Blank, whitespace-only and comment lines between items may be
    /// indented anyhow; new items still go at the items' column.
    #[test]
    fn test_add_to_sequence_past_other_lines() {
        let cases: &[(&str, &dyn Fn(&crate::Sequence), &str)] = &[
            (
                "- 1\n     \n- 2\n",
                &|s| s.insert(1, "x"),
                "- 1\n     \n- x\n- 2\n",
            ),
            (
                "- 1\n    # c\n- 2\n",
                &|s| s.push("x"),
                "- 1\n    # c\n- 2\n- x\n",
            ),
            (
                "a:\n  - 1\n      # c\n  - 2\n",
                &|s| s.push("x"),
                "a:\n  - 1\n      # c\n  - 2\n  - x\n",
            ),
            (
                "a:\n  - 1\n\n      \n  - 2\n",
                &|s| s.insert(1, "x"),
                "a:\n  - 1\n\n      \n  - x\n  - 2\n",
            ),
            (
                "a:\n- 1\n    # c\n- 2\n",
                &|s| s.push("x"),
                "a:\n- 1\n    # c\n- 2\n- x\n",
            ),
            (
                "a:\n- 1\n    # c\n- 2\n",
                &|s| s.insert(0, "x"),
                "a:\n- x\n- 1\n    # c\n- 2\n",
            ),
            (
                "a:\n      # c\n  - 1\n",
                &|s| s.push("x"),
                "a:\n      # c\n  - 1\n  - x\n",
            ),
        ];
        for (source, edit, expected) in cases {
            let yaml = YamlFile::from_str(source).unwrap();
            let doc = yaml.document().unwrap();
            let seq = doc
                .as_sequence()
                .or_else(|| doc.as_mapping()?.get_sequence("a"))
                .unwrap();
            edit(&seq);
            crate::debug::assert_edited(&yaml, expected);
        }
    }

    /// A sequence nested on its parent's dash line (`- - 1`) has its items
    /// at the inner dash's column.
    #[test]
    fn test_add_to_compact_nested_sequence() {
        let cases: &[(&str, &dyn Fn(&crate::Sequence), &str)] = &[
            ("-   - 1\n", &|s| s.push("x"), "-   - 1\n    - x\n"),
            ("-   - 1\n", &|s| s.insert(0, "x"), "-   - x\n    - 1\n"),
            (
                "- - 1\n      # c\n  - 2\n",
                &|s| s.push("x"),
                "- - 1\n      # c\n  - 2\n  - x\n",
            ),
            (
                "-   - 1\n      # c\n    - 2\n",
                &|s| s.insert(1, "x"),
                "-   - 1\n      # c\n    - x\n    - 2\n",
            ),
        ];
        for (source, edit, expected) in cases {
            let yaml = YamlFile::from_str(source).unwrap();
            let outer = yaml.document().unwrap().as_sequence().unwrap();
            let seq = outer.get(0).unwrap().as_sequence().cloned().unwrap();
            edit(&seq);
            crate::debug::assert_edited(&yaml, expected);
        }
    }

    /// The last item's line break may sit inside a nested collection or
    /// block scalar rather than directly in its entry.
    #[test]
    fn test_append_after_nested_last_item() {
        let cases = [
            ("- - 1\n  - 2\n", "- - 1\n  - 2\n- x\n"),
            ("- - 1", "- - 1\n- x"),
            ("- a: 1\n  b: 2\n", "- a: 1\n  b: 2\n- x\n"),
            ("- a: 1", "- a: 1\n- x"),
            ("- a:\n", "- a:\n- x\n"),
            ("- ? a\n", "- ? a\n- x\n"),
            ("- |\n  x\n", "- |\n  x\n- x\n"),
            ("- - |\n    x", "- - |\n    x\n- x"),
            ("k:\n  - - 1\n", "k:\n  - - 1\n  - x\n"),
            ("k:\n  - - 1\nj: 2\n", "k:\n  - - 1\n  - x\nj: 2\n"),
        ];
        for (source, expected) in cases {
            let yaml = YamlFile::from_str(source).unwrap();
            let doc = yaml.document().unwrap();
            let seq = doc
                .as_sequence()
                .or_else(|| doc.as_mapping()?.get_sequence("k"))
                .unwrap();
            seq.push("x");
            crate::debug::assert_edited(&yaml, expected);
        }
        let cases = [
            ("- - 1", "- - 1\n- x"),
            ("- a: 1", "- a: 1\n- x"),
            ("- - |\n    x", "- - |\n    x\n- x"),
        ];
        for (source, expected) in cases {
            let yaml = YamlFile::from_str(source).unwrap();
            let seq = yaml.document().unwrap().as_sequence().unwrap();
            seq.insert(1, "x");
            crate::debug::assert_edited(&yaml, expected);
        }
    }

    /// A comment line ending the root sequence stays before the new last
    /// item; a comment on the last item's own line stays with that item.
    #[test]
    fn test_append_after_trailing_comment() {
        let cases: &[(&str, &str, &str)] = &[
            ("- 1\n  # c\n", "- 1\n  # c\n- x\n", "- 1\n  # c\n- x\n"),
            ("- 1\n  # c", "- 1\n  # c\n- x", "- 1\n  # c\n- x"),
            ("- 1\n# c\n", "- 1\n# c\n- x\n", "- 1\n# c\n- x\n"),
            (
                "- 1\n  # c\n  # d\n",
                "- 1\n  # c\n  # d\n- x\n",
                "- 1\n  # c\n  # d\n- x\n",
            ),
            ("- 1 # c\n", "- 1 # c\n- x\n", "- 1 # c\n- x\n"),
            ("- 1 # c", "- 1 # c\n- x", "- 1 # c\n- x"),
            (
                "- 1 # c\n  # d\n",
                "- 1 # c\n  # d\n- x\n",
                "- 1 # c\n  # d\n- x\n",
            ),
            // A comment line ending a nested sequence at the end of the
            // input is the root collection's, so it stays last.
            (
                "a:\n  - 1\n    # c\n",
                "a:\n  - 1\n  - x\n    # c\n",
                "a:\n  - 1\n  - x\n    # c\n",
            ),
            (
                "a:\n  - 1\n    # c",
                "a:\n  - 1\n  - x\n    # c",
                "a:\n  - 1\n  - x\n    # c",
            ),
            (
                "a:\n- 1\n  # c\n",
                "a:\n- 1\n- x\n  # c\n",
                "a:\n- 1\n- x\n  # c\n",
            ),
            (
                "a:\n  - 1 # c\n",
                "a:\n  - 1 # c\n  - x\n",
                "a:\n  - 1 # c\n  - x\n",
            ),
            (
                "a:\n  - 1 # c",
                "a:\n  - 1 # c\n  - x",
                "a:\n  - 1 # c\n  - x",
            ),
            (
                "a:\n  - 1 # c\nb: 1\n",
                "a:\n  - 1 # c\n  - x\nb: 1\n",
                "a:\n  - 1 # c\n  - x\nb: 1\n",
            ),
            ("a:\n- 1 # c\n", "a:\n- 1 # c\n- x\n", "a:\n- 1 # c\n- x\n"),
            // A comment line the parser lifted out of the sequence goes with
            // what follows it, as for mappings.
            (
                "a:\n  - 1\n    # c\nb: 1\n",
                "a:\n  - 1\n  - x\n    # c\nb: 1\n",
                "a:\n  - 1\n  - x\n    # c\nb: 1\n",
            ),
        ];
        for (source, pushed, inserted) in cases {
            for (append, expected) in [
                (
                    &(|s: &crate::Sequence| s.push("x")) as &dyn Fn(&crate::Sequence),
                    pushed,
                ),
                (&|s: &crate::Sequence| s.insert(s.len(), "x"), inserted),
            ] {
                let yaml = YamlFile::from_str(source).unwrap();
                let doc = yaml.document().unwrap();
                let seq = doc
                    .as_sequence()
                    .or_else(|| doc.as_mapping()?.get_sequence("a"))
                    .unwrap();
                append(&seq);
                crate::debug::assert_edited(&yaml, expected);
            }
        }
    }

    /// The same for a sequence nested in another one.
    #[test]
    fn test_append_to_nested_sequence_after_comment() {
        let cases: &[(&str, &str, &str)] = &[
            (
                "- - 1\n    # c\n",
                "- - 1\n  - x\n    # c\n",
                "- - 1\n  - x\n    # c\n",
            ),
            ("- - 1 # c\n", "- - 1 # c\n  - x\n", "- - 1 # c\n  - x\n"),
            (
                "- - 1 # c\n- 2\n",
                "- - 1 # c\n  - x\n- 2\n",
                "- - 1 # c\n  - x\n- 2\n",
            ),
        ];
        for (source, pushed, inserted) in cases {
            for (append, expected) in [
                (
                    &(|s: &crate::Sequence| s.push("x")) as &dyn Fn(&crate::Sequence),
                    pushed,
                ),
                (&|s: &crate::Sequence| s.insert(s.len(), "x"), inserted),
            ] {
                let yaml = YamlFile::from_str(source).unwrap();
                let outer = yaml.document().unwrap().as_sequence().unwrap();
                let seq = outer.get(0).unwrap().as_sequence().cloned().unwrap();
                append(&seq);
                crate::debug::assert_edited(&yaml, expected);
            }
        }
    }

    /// New items go at the column of the existing ones, wherever the
    /// parser put that column.
    #[test]
    fn test_add_item_at_column_of_items() {
        use rowan::ast::AstNode;
        let cases = [
            (
                "k: !!seq\n- a\n",
                "k: !!seq\n- a\n- x\n",
                "k: !!seq\n- x\n- a\n",
            ),
            (
                "k: !!seq\n  - a\n  - b\n",
                "k: !!seq\n  - a\n  - b\n  - x\n",
                "k: !!seq\n  - x\n  - a\n  - b\n",
            ),
            (
                "--- !!seq\n- a\n",
                "--- !!seq\n- a\n- x\n",
                "--- !!seq\n- x\n- a\n",
            ),
            (" - a\n - b\n", " - a\n - b\n - x\n", " - x\n - a\n - b\n"),
            ("? - a\n: b\n", "? - a\n  - x\n: b\n", "? - x\n  - a\n: b\n"),
            (
                "k:\n  ? - a\n  : b\n",
                "k:\n  ? - a\n    - x\n  : b\n",
                "k:\n  ? - x\n    - a\n  : b\n",
            ),
            ("? a\n: - b\n", "? a\n: - b\n  - x\n", "? a\n: - x\n  - b\n"),
            (
                "? |\n  k\n: - a\n",
                "? |\n  k\n: - a\n  - x\n",
                "? |\n  k\n: - x\n  - a\n",
            ),
        ];
        for (source, pushed, inserted) in cases {
            for (push, expected) in [(true, pushed), (false, inserted)] {
                let yaml = YamlFile::from_str(source).unwrap();
                let seq = yaml
                    .syntax()
                    .descendants()
                    .find_map(crate::Sequence::cast)
                    .unwrap();
                if push {
                    seq.push("x");
                } else {
                    seq.insert(0, "x");
                }
                crate::debug::assert_edited(&yaml, expected);
            }
        }
    }

    /// Sequence edits write the document's own line break, whichever it is.
    #[test]
    fn test_edits_write_document_line_breaks() {
        type Edit = fn(&crate::Sequence);
        let edits: [(&str, Edit, &str); 10] = [
            ("- a\n", |s| s.push("b"), "- a\n- b\n"),
            ("- a  # c\n", |s| s.push("b"), "- a  # c\n- b\n"),
            ("- a\n", |s| s.insert(0, "b"), "- b\n- a\n"),
            ("# h\n- a\n", |s| s.insert(0, "b"), "# h\n- b\n- a\n"),
            (
                "- a  # c\n- d\n",
                |s| s.insert(1, "b"),
                "- a  # c\n- b\n- d\n",
            ),
            ("- a\n", |s| s.extend_with("x", 3), "- a\n- x\n- x\n- x\n"),
            (
                "- a: 1\n  b: 2\n- c\n",
                |s| assert!(s.set(0, 5)),
                "- 5\n- c\n",
            ),
            (
                "- a\n- c\n",
                |s| {
                    assert!(s.set(
                        0,
                        crate::Document::from_str("x: 1\ny: 2\n")
                            .unwrap()
                            .as_mapping()
                            .unwrap()
                    ))
                },
                "- x: 1\n  y: 2\n- c\n",
            ),
            (
                "- &a\n  - x\n- c\n",
                |s| {
                    assert!(s.set(
                        0,
                        crate::Document::from_str("x: 1\ny: 2\n")
                            .unwrap()
                            .as_mapping()
                            .unwrap()
                    ))
                },
                "- &a\n  x: 1\n  y: 2\n- c\n",
            ),
            (
                "-\n  # n\n  a\n- c\n",
                |s| {
                    assert!(s.set(
                        0,
                        crate::Document::from_str("x: 1\ny: 2\n")
                            .unwrap()
                            .as_mapping()
                            .unwrap()
                    ))
                },
                "-\n  # n\n  x: 1\n  y: 2\n- c\n",
            ),
        ];
        for line_break in ["\n", "\r\n", "\r"] {
            for (source, edit, expected) in &edits {
                let source = source.replace('\n', line_break);
                let yaml = YamlFile::from_str(&source).unwrap();
                edit(&yaml.document().unwrap().as_sequence().unwrap());
                crate::debug::assert_edited(&yaml, &expected.replace('\n', line_break));
            }
        }
    }
}
