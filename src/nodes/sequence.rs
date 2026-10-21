use super::replaced_value;
use super::{
    fresh_token, insert_children, nth_entry_index, replace_children, starts_a_flow_collection,
    trivia, Lang, SyntaxNode,
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
    // A pending-block placeholder is `{}` in the text but is headed for
    // block style, so it does not force flow on what nests inside it (see
    // `Mapping::new_pending_block`).
    node.ancestors().skip(1).any(|p| {
        matches!(p.kind(), SyntaxKind::MAPPING | SyntaxKind::SEQUENCE)
            && starts_a_flow_collection(&p)
            && !empty_collection::is_block_placeholder(&p)
    })
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
                            // The value's later lines stay where they would
                            // be without the anchor.
                            builder.token(SyntaxKind::WHITESPACE.into(), " ");
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
    /// sequence one push at a time is quadratic overall. Almost all of the
    /// cost is in rowan's `splice_children`, which walks every child of the
    /// sequence and rebuilds its green node on each edit (see `rowan.md`);
    /// this method reads only the tail of the child list and holds no child
    /// handles across the edit.
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
        starts_a_flow_collection(&self.0)
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

    // Iterator tests

    // Tests from sequence_operations_test.rs

    /// Sequence edits write the document's own line break, whichever it is.
    #[test]
    fn test_edits_write_document_line_breaks() {
        type Edit = fn(&crate::Sequence);
        let edits: [(&str, Edit, &str); 11] = [
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
                "-\n  name: A\n  hr: 65\n-\n  name: B\n",
                |s| assert!(s.set(0, "xval")),
                "-\n  xval\n-\n  name: B\n",
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
