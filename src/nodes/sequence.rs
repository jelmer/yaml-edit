use super::{
    append_children, ensure_trailing_newline, entry_indices, fresh_token, has_child_token,
    nth_entry_index, Lang, SyntaxNode,
};
use crate::as_yaml::{AsYaml, YamlKind};
use crate::lex::SyntaxKind;
use crate::yaml::ValueNode;
use rowan::ast::AstNode;
use rowan::GreenNodeBuilder;

ast_node!(Sequence, SEQUENCE, "A YAML sequence (list)");

/// True if `node` cannot be rendered as a block collection: any
/// flow ancestor forbids it, and so does sitting inline after a `- `
/// (no NEWLINE + INDENT scaffold to hang a block entry off).
pub(crate) fn must_render_flow(node: &SyntaxNode) -> bool {
    if node
        .parent()
        .is_some_and(|p| p.kind() == SyntaxKind::SEQUENCE_ENTRY)
    {
        return true;
    }
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
            let pending =
                crate::nodes::Mapping::cast(p.clone()).is_some_and(|m| m.is_block_placeholder());
            if has_child_token(&p, |k| k == open) && !pending {
                return true;
            }
        }
        cur = p.parent();
    }
    false
}

/// Does the SEQUENCE's parent VALUE carry the leading INDENT for this
/// sequence? True for the block-under-key shape (`key:\n  - a`): the
/// first entry's indentation lives in VALUE, not inside SEQUENCE, so
/// mutation helpers must not emit their own leading INDENT for it.
fn parent_value_has_leading_indent(seq: &SyntaxNode) -> bool {
    let Some(parent) = seq.parent() else {
        return false;
    };
    if parent.kind() != SyntaxKind::VALUE {
        return false;
    }
    let mut saw_indent = false;
    for child in parent.children_with_tokens() {
        match &child {
            rowan::NodeOrToken::Node(n) if n == seq => return saw_indent,
            rowan::NodeOrToken::Token(t) if t.kind() == SyntaxKind::INDENT => saw_indent = true,
            rowan::NodeOrToken::Token(t) if t.kind() == SyntaxKind::NEWLINE => {}
            _ => saw_indent = false,
        }
    }
    false
}

/// If `node`'s tail token is a NEWLINE (optionally followed by an
/// INDENT), return the concatenated text as `Some("\n" | "\n<indent>")`.
///
/// Used by [`Sequence::set`] to preserve the trailing separator of a
/// multi-line value when swapping it out. Walking the CST directly is
/// safer than `node.text().rfind('\n')` because a NEWLINE nested inside
/// a block scalar's content would mislead a text-level search.
fn trailing_newline_indent(node: &SyntaxNode) -> Option<String> {
    let tokens: Vec<_> = node
        .descendants_with_tokens()
        .filter_map(|el| el.into_token())
        .collect();
    let mut result = String::new();
    // Look at the last two tail tokens: [NEWLINE], [NEWLINE, INDENT], or
    // [.., NEWLINE], [.., NEWLINE, INDENT].
    let last = tokens.last()?;
    match last.kind() {
        SyntaxKind::NEWLINE => {
            result.push_str(last.text());
        }
        SyntaxKind::INDENT => {
            let prev = tokens.iter().rev().nth(1)?;
            if prev.kind() != SyntaxKind::NEWLINE {
                return None;
            }
            result.push_str(prev.text());
            result.push_str(last.text());
        }
        _ => return None,
    }
    Some(result)
}

/// True if `sequence`'s enclosing MAPPING_ENTRY is the last entry of
/// its MAPPING. Callers use this to know whether trimming trailing
/// whitespace off the sequence would strand a following mapping entry
/// that relied on the separator NEWLINE. Conservative `false` when
/// the sequence isn't under a MAPPING_ENTRY at all.
/// Rebuild `entry` with `value` in place of the node it currently holds,
/// keeping everything else the entry carried.
fn rebuild_entry_with_value(entry: &SyntaxNode, value: &impl crate::AsYaml) -> SyntaxNode {
    let entry_children: Vec<_> = entry.children_with_tokens().collect();

    // Build a new SEQUENCE_ENTRY with the new value using AsYaml
    let mut builder = GreenNodeBuilder::new();
    builder.start_node(SyntaxKind::SEQUENCE_ENTRY.into());

    let mut value_inserted = false;
    let mut trailing_text: Option<String> = None;
    let mut after_dash = false;

    for entry_child in entry_children {
        match &entry_child {
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
                // A multi-line value (a nested mapping, say) ends with a
                // NEWLINE and often an INDENT, which separate the entry
                // from whatever follows and outlive the value itself.
                trailing_text = trailing_newline_indent(n);
                if !value_inserted {
                    // A bare `-` item is DASH then a zero-width NULL
                    // scalar, so a written value needs the space that
                    // was never there: without it `set` gives `-x`.
                    if after_dash {
                        builder.token(SyntaxKind::WHITESPACE.into(), " ");
                    }
                    value.build_content(&mut builder, 0, false);
                    value_inserted = true;
                }
                after_dash = false;
            }
            rowan::NodeOrToken::Node(n) => {
                // Copy other nodes as-is (like VALUE wrappers, etc.)
                crate::yaml::copy_node_to_builder(&mut builder, n);
                after_dash = false;
            }
            rowan::NodeOrToken::Token(t) => {
                // Copy tokens as-is
                builder.token(t.kind().into(), t.text());
                after_dash = t.kind() == SyntaxKind::DASH;
            }
        }
    }

    // Restore trailing whitespace extracted from the old value
    if let Some(trailing) = trailing_text {
        if let Some(indent_part) = trailing.strip_prefix('\n') {
            builder.token(SyntaxKind::NEWLINE.into(), "\n");
            if !indent_part.is_empty() {
                builder.token(SyntaxKind::INDENT.into(), indent_part);
            }
        }
    }

    builder.finish_node();
    SyntaxNode::new_root_mut(builder.finish())
}

/// Drop the run of blank tokens (newline, indent, whitespace) that ends
/// `entry`, so removing the entry after it leaves no stray blank line.
fn strip_trailing_blank_tokens(entry: &SyntaxNode) {
    let children: Vec<_> = entry.children_with_tokens().collect();
    let blanks = children
        .iter()
        .rev()
        .take_while(|c| {
            c.as_token().is_some_and(|t| {
                matches!(
                    t.kind(),
                    SyntaxKind::NEWLINE | SyntaxKind::INDENT | SyntaxKind::WHITESPACE
                )
            })
        })
        .count();
    if blanks > 0 {
        let total = children.len();
        entry.splice_children((total - blanks)..total, vec![]);
    }
}

fn mapping_entry_is_last_in_mapping(sequence: &SyntaxNode) -> bool {
    let Some(value) = sequence.parent() else {
        return false;
    };
    if value.kind() != SyntaxKind::VALUE {
        return false;
    }
    let Some(entry) = value.parent() else {
        return false;
    };
    if entry.kind() != SyntaxKind::MAPPING_ENTRY {
        return false;
    }
    let Some(parent_mapping) = entry.parent() else {
        return false;
    };
    if parent_mapping.kind() != SyntaxKind::MAPPING {
        return false;
    }
    // Any MAPPING_ENTRY sibling *after* this one means we must keep
    // whatever separator is currently in place.
    let mut seen_self = false;
    for child in parent_mapping.children() {
        if child == entry {
            seen_self = true;
            continue;
        }
        if seen_self && child.kind() == SyntaxKind::MAPPING_ENTRY {
            return false;
        }
    }
    true
}

// The collapse helper is shared with Mapping; it lives in yaml.rs
// as `collapse_empty_child_collection_in_parent`.
use crate::yaml::collapse_empty_child_collection_in_parent as collapse_empty_child_sequence_in_parent;

impl Sequence {
    /// Iterate over items in this sequence as raw syntax nodes.
    ///
    /// For most use cases prefer [`values`](Self::values) which returns
    /// [`YamlNode`](crate::as_yaml::YamlNode)s.
    pub(crate) fn items(&self) -> impl Iterator<Item = SyntaxNode> + '_ {
        self.0.children().filter_map(|child| {
            if child.kind() == SyntaxKind::SEQUENCE_ENTRY {
                // Look for the actual item within the SEQUENCE_ENTRY
                // Skip DASH and WHITESPACE tokens, find the actual value node
                child.children().find(|n| {
                    matches!(
                        n.kind(),
                        SyntaxKind::SCALAR
                            | SyntaxKind::MAPPING
                            | SyntaxKind::SEQUENCE
                            | SyntaxKind::ALIAS
                            | SyntaxKind::TAGGED_NODE
                    )
                })
            } else {
                None
            }
        })
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

    /// Is this an empty sequence awaiting its first item? See
    /// [`Sequence::new_pending_block`].
    pub(crate) fn is_block_placeholder(&self) -> bool {
        !self
            .0
            .children()
            .any(|c| c.kind() == SyntaxKind::SEQUENCE_ENTRY)
            && self
                .0
                .children_with_tokens()
                .filter_map(|c| c.into_token())
                .any(|t| t.kind() == SyntaxKind::WHITESPACE && t.text().is_empty())
    }

    /// Indentation string used by entries in this sequence: a
    /// top-level INDENT if present, else WHITESPACE before DASH inside
    /// an entry, else the parent VALUE's INDENT (the parser's storage
    /// for single-entry block sequences under a key). Defaults to two
    /// spaces.
    fn detect_indentation(&self) -> String {
        // First try top-level INDENT tokens
        if let Some(ind) = self.0.children_with_tokens().find_map(|child| {
            child
                .into_token()
                .filter(|t| t.kind() == SyntaxKind::INDENT)
                .map(|t| t.text().to_string())
        }) {
            return ind;
        }

        // Fall back: look for WHITESPACE before DASH inside entry nodes
        if let Some(indent) = self
            .0
            .children()
            .filter(|c| c.kind() == SyntaxKind::SEQUENCE_ENTRY)
            .find_map(|entry| {
                let tokens: Vec<_> = entry.children_with_tokens().collect();
                tokens.windows(2).find_map(|pair| {
                    let ws = pair[0].as_token()?;
                    let dash = pair[1].as_token()?;
                    if ws.kind() == SyntaxKind::WHITESPACE && dash.kind() == SyntaxKind::DASH {
                        Some(ws.text().to_string())
                    } else {
                        None
                    }
                })
            })
        {
            return indent;
        }

        // For a block sequence under a key, the parser stores the entry
        // column as an INDENT in the parent VALUE (right before the SEQUENCE).
        if let Some(parent) = self.0.parent().filter(|p| p.kind() == SyntaxKind::VALUE) {
            for child in parent.children_with_tokens() {
                match &child {
                    rowan::NodeOrToken::Node(n) if n == &self.0 => break,
                    rowan::NodeOrToken::Token(t) if t.kind() == SyntaxKind::INDENT => {
                        return t.text().to_string();
                    }
                    _ => {}
                }
            }
        }

        // A sequence that is the document's own node starts at column 0, so
        // there is nothing to indent by. Guessing two spaces here made
        // `push` write the new entry as a nested sequence inside the last
        // one, where as_sequence could no longer reach it.
        match self.0.parent() {
            None => return String::new(),
            Some(parent) if parent.kind() == SyntaxKind::DOCUMENT => return String::new(),
            Some(_) => {}
        }

        "  ".to_string()
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
        // scaffolding -- flow vs block, the indent string, whether the parent
        // supplies the indent, the placeholder newline. Let it, then append
        // the rest alongside it in one splice.
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

        // Mirror the entry push just made: same indent, and a trailing
        // NEWLINE since it is no longer last.
        let indentation = self.detect_indentation();
        let mut inserts: Vec<rowan::NodeOrToken<SyntaxNode, _>> = Vec::new();
        for _ in 0..remaining {
            let mut builder = GreenNodeBuilder::new();
            builder.start_node(SyntaxKind::SEQUENCE_ENTRY.into());
            builder.token(SyntaxKind::DASH.into(), "-");
            builder.token(SyntaxKind::WHITESPACE.into(), " ");
            let ends_with_newline = value.clone().build_content(&mut builder, 0, false);
            if !ends_with_newline {
                builder.token(SyntaxKind::NEWLINE.into(), "\n");
            }
            builder.finish_node();
            inserts.push(fresh_token(SyntaxKind::INDENT, &indentation).into());
            inserts.push(SyntaxNode::new_root_mut(builder.finish()).into());
        }

        let end = self.0.green().children().len();
        self.0.splice_children(end..end, inserts);
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
        // A placeholder from `Sequence::new_pending_block` turns into a real
        // block sequence now that it has an item. A `[]` from the source is
        // not marked and keeps its flow style; a placeholder nested inside a
        // flow container stays flow to avoid mixed output.
        if self.is_block_placeholder() && !must_render_flow(&self.0) {
            crate::yaml::convert_placeholder_to_block(&self.0);
        }
        if self.is_flow_style() {
            self.insert_flow(usize::MAX, value);
            return;
        }

        let indentation = self.detect_indentation();

        // Build the INDENT token (separate from the SEQUENCE_ENTRY)
        let indent_token = fresh_token(SyntaxKind::INDENT, &indentation);

        // Locate the last SEQUENCE_ENTRY and the insert position by walking
        // back from the end of the child list.
        //
        // Both this scan and `splice_children` cost O(len) if done naively,
        // which makes building a sequence O(len^2). The green child list is
        // a DoubleEndedIterator, so walking back from the end reads only the
        // few trailing elements; and since `splice_children` has to fix up
        // every *live* SyntaxNode handle, take the measurements and fix the
        // previous entry before creating the handles the splice needs.
        let green = self.0.green();
        let mut tail = green.children();
        let child_count = tail.len();

        // Walk back over the trailing tokens the parser leaves after the
        // last entry. A NEWLINE there is a blank line belonging to the
        // enclosing mapping, so the new entry goes before it; an INDENT is a
        // separator, so the new entry goes after it.
        let mut last_entry_index = None;
        let mut last_entry_has_newline = true; // Default for an empty sequence
        let mut insert_pos = child_count;
        let mut idx = child_count;
        while idx > 0 {
            idx -= 1;
            match tail.next_back() {
                Some(rowan::NodeOrToken::Node(n))
                    if n.kind() == SyntaxKind::SEQUENCE_ENTRY.into() =>
                {
                    last_entry_has_newline = n
                        .children()
                        .next_back()
                        .and_then(|c| c.into_token())
                        .is_some_and(|t| t.kind() == SyntaxKind::NEWLINE.into());
                    last_entry_index = Some(idx);
                    if insert_pos == child_count {
                        insert_pos = idx + 1;
                    }
                    break;
                }
                Some(rowan::NodeOrToken::Token(tok)) if tok.kind() == SyntaxKind::INDENT.into() => {
                    insert_pos = idx + 1;
                }
                Some(rowan::NodeOrToken::Token(tok))
                    if tok.kind() == SyntaxKind::NEWLINE.into() =>
                {
                    insert_pos = idx;
                }
                _ => break,
            }
        }

        // Build the SEQUENCE_ENTRY node using AsYaml trait
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(SyntaxKind::SEQUENCE_ENTRY.into());
        builder.token(SyntaxKind::DASH.into(), "-");
        builder.token(SyntaxKind::WHITESPACE.into(), " ");

        // Build the value content directly using AsYaml
        let value_ends_with_newline = value.build_content(&mut builder, 0, false);

        // Add trailing newline only if the value doesn't already end with one
        // and if the last entry had one (preserves document style)
        if last_entry_has_newline && !value_ends_with_newline {
            builder.token(SyntaxKind::NEWLINE.into(), "\n");
        }
        builder.finish_node(); // SEQUENCE_ENTRY
        let new_entry = SyntaxNode::new_root_mut(builder.finish());

        // Ensure the previous last entry has a trailing newline (it won't be
        // last anymore). `last_child` reaches it without walking the list.
        if last_entry_index.is_some() && !last_entry_has_newline {
            if let Some(node) = self
                .0
                .last_child()
                .filter(|n| n.kind() == SyntaxKind::SEQUENCE_ENTRY)
            {
                let entry_children_count = node.green().children().len();
                let nl = fresh_token(SyntaxKind::NEWLINE, "\n");
                node.splice_children(entry_children_count..entry_children_count, vec![nl.into()]);
            }
        }

        // Parser convention: the first entry in a SEQUENCE has no leading
        // INDENT inside the SEQUENCE; its indentation comes from the parent
        // VALUE (`NEWLINE INDENT` right before the SEQUENCE). Only later
        // entries carry an INDENT as a separator after the previous entry's
        // NEWLINE. Match that when pushing.
        let entry_is_first = last_entry_index.is_none();
        let parent_supplies_indent = entry_is_first && parent_value_has_leading_indent(&self.0);

        let mut inserts: Vec<rowan::NodeOrToken<SyntaxNode, _>> = Vec::new();
        if !parent_supplies_indent {
            inserts.push(indent_token.into());
        }
        inserts.push(new_entry.clone().into());

        self.0.splice_children(insert_pos..insert_pos, inserts);

        // Strip the outer MAPPING_ENTRY's placeholder NEWLINE if this was
        // the first entry (see issue #18).
        if entry_is_first {
            crate::yaml::detach_empty_collection_placeholder_newline(&self.0, &new_entry);
        }
    }

    /// Splice a new flow entry at `index` (or append when
    /// `index >= len`), wiring up `, ` separators. The flow separator
    /// convention is that every entry except the last carries a
    /// trailing `, ` as its own tail (see the flow-separator note in
    /// [`crate::nodes`]).
    fn insert_flow(&self, index: usize, value: impl crate::AsYaml) {
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(SyntaxKind::SEQUENCE_ENTRY.into());
        // flow_context=false: YAML flow permits plain scalars; only
        // JSON-flavored callers want the aggressive quoting.
        value.build_content(&mut builder, 0, false);
        builder.finish_node();
        let new_entry = SyntaxNode::new_root_mut(builder.finish());

        let children: Vec<_> = self.0.children_with_tokens().collect();
        let entry_positions = entry_indices(&self.0, SyntaxKind::SEQUENCE_ENTRY);
        let Some(right_bracket_pos) = children.iter().position(|c| {
            c.as_token()
                .is_some_and(|t| t.kind() == SyntaxKind::RIGHT_BRACKET)
        }) else {
            // Callers check `is_flow_style()` first, which requires
            // a LEFT_BRACKET; the matching RIGHT_BRACKET is a parser
            // invariant. Bail defensively on malformed CSTs rather
            // than corrupting the tree further.
            debug_assert!(false, "flow SEQUENCE missing RIGHT_BRACKET");
            return;
        };

        let comma = fresh_token(SyntaxKind::COMMA, ",");
        let sep_ws = fresh_token(SyntaxKind::WHITESPACE, " ");

        if index >= entry_positions.len() {
            // Append: add `, ` to the previous last entry, then
            // splice before the `]`.
            if let Some(&last_pos) = entry_positions.last() {
                let last_entry = children[last_pos].as_node().expect("SEQUENCE_ENTRY");
                let ends_with_comma = last_entry
                    .last_token()
                    .is_some_and(|t| t.kind() == SyntaxKind::COMMA);
                if !ends_with_comma {
                    append_children(last_entry, vec![comma.into(), sep_ws.into()]);
                }
            }
            self.0
                .splice_children(right_bracket_pos..right_bracket_pos, vec![new_entry.into()]);
            return;
        }

        // Insert before the entry at `index`; the new entry carries
        // its own `, ` tail to keep the displaced entry separated.
        let target_pos = entry_positions[index];
        append_children(&new_entry, vec![comma.into(), sep_ws.into()]);
        self.0
            .splice_children(target_pos..target_pos, vec![new_entry.into()]);
    }

    /// Insert an item at a specific position.
    ///
    /// If `index` is out of bounds, the item is appended at the end.
    /// This method always succeeds; it never returns an error.
    ///
    /// Mutates in place despite `&self` (see crate docs on interior mutability).
    pub fn insert(&self, index: usize, value: impl crate::AsYaml) {
        // Same rule as `push`; see there for the "why".
        if self.is_block_placeholder() && !must_render_flow(&self.0) {
            crate::yaml::convert_placeholder_to_block(&self.0);
        }
        if self.is_flow_style() {
            self.insert_flow(index, value);
            return;
        }

        let indentation = self.detect_indentation();

        // Build the new SEQUENCE_ENTRY, terminated with its own NEWLINE.
        // Parser convention: the INDENT sits at SEQUENCE level as a
        // separator before the entry, not inside it.
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(SyntaxKind::SEQUENCE_ENTRY.into());
        builder.token(SyntaxKind::DASH.into(), "-");
        builder.token(SyntaxKind::WHITESPACE.into(), " ");
        let value_ends_with_newline = value.build_content(&mut builder, 0, false);
        if !value_ends_with_newline {
            builder.token(SyntaxKind::NEWLINE.into(), "\n");
        }
        builder.finish_node();
        let new_entry = SyntaxNode::new_root_mut(builder.finish());

        // Build a standalone INDENT to sit before the new entry.
        let indent_token = fresh_token(SyntaxKind::INDENT, &indentation);

        // Locate the target position. If we're inserting before an
        // existing entry, we want to land right at the INDENT that
        // precedes it (so we don't split the existing NEWLINE-INDENT
        // pairing that separates entries).
        let children: Vec<_> = self.0.children_with_tokens().collect();
        let mut item_count = 0;
        let mut target_entry_pos = children.len();
        for (i, child) in children.iter().enumerate() {
            let Some(node) = child.as_node() else {
                continue;
            };
            if node.kind() != SyntaxKind::SEQUENCE_ENTRY {
                continue;
            }
            if item_count == index {
                target_entry_pos = i;
                break;
            }
            item_count += 1;
        }

        // The INDENT (if any) that separates the target entry from the
        // previous one is the child immediately before target_entry_pos.
        // Insert our new entry+INDENT-separator *before* that INDENT, so
        // the layout stays `NEWLINE INDENT ENTRY INDENT NEW_ENTRY`.
        let insert_at = if target_entry_pos > 0
            && children
                .get(target_entry_pos - 1)
                .and_then(|c| c.as_token())
                .is_some_and(|t| t.kind() == SyntaxKind::INDENT)
        {
            target_entry_pos - 1
        } else {
            target_entry_pos
        };

        // Appending past the last entry (source doc had no trailing
        // newline)? Ensure the previous entry has one, otherwise our
        // new INDENT+ENTRY gets glued onto its tail as `- b  - c`.
        if target_entry_pos == children.len() {
            if let Some(prev_entry) = children.iter().rev().find_map(|c| {
                c.as_node()
                    .filter(|n| n.kind() == SyntaxKind::SEQUENCE_ENTRY)
            }) {
                ensure_trailing_newline(prev_entry);
            }
        }

        // Inserting at the head of a SEQUENCE whose parent VALUE
        // supplies the leading INDENT? Don't emit our own leading
        // INDENT for the new entry (it would stack), but do give the
        // displaced old-first entry an INDENT of its own so it stays
        // at the right column.
        let inserting_at_head = target_entry_pos < children.len()
            && children[..target_entry_pos].iter().all(|c| {
                c.as_node()
                    .map_or(true, |n| n.kind() != SyntaxKind::SEQUENCE_ENTRY)
            });
        if inserting_at_head && parent_value_has_leading_indent(&self.0) {
            let old_first_indent = fresh_token(SyntaxKind::INDENT, &indentation);
            self.0.splice_children(
                insert_at..insert_at,
                vec![new_entry.into(), old_first_indent.into()],
            );
            return;
        }

        self.0.splice_children(
            insert_at..insert_at,
            vec![indent_token.into(), new_entry.into()],
        );
    }

    /// Replace the item at `index` with a new value.
    ///
    /// Returns `true` if the index was in bounds and the item was replaced,
    /// `false` if `index >= len()`.
    ///
    /// Mutates in place despite `&self` (see crate docs on interior mutability).
    pub fn set(&self, index: usize, value: impl crate::AsYaml) -> bool {
        let children: Vec<_> = self.0.children_with_tokens().collect();

        let Some(i) = nth_entry_index(&self.0, SyntaxKind::SEQUENCE_ENTRY, index) else {
            return false;
        };
        let node = children[i].as_node().expect("entry index names a node");
        let new_entry = rebuild_entry_with_value(node, &value);

        // Replace the old SEQUENCE_ENTRY with the new one
        self.0.splice_children(i..i + 1, vec![new_entry.into()]);
        true
    }

    /// Drop the INDENT that separated the entry just removed at child
    /// position `i` from its sibling.
    ///
    /// For a non-first entry that INDENT sits before it, left over from the
    /// previous entry's NEWLINE. For the first, the one *after* it would
    /// become a leading INDENT inside the SEQUENCE and stack with the parent
    /// VALUE's (`  ` + `  ` -> `    `), shifting the new first entry a level
    /// in. A flow sequence has no such separators.
    fn detach_orphaned_indent(
        &self,
        children: &[rowan::NodeOrToken<SyntaxNode, rowan::SyntaxToken<Lang>>],
        i: usize,
    ) {
        if self.is_flow_style() {
            return;
        }
        let (neighbour, range) = if i > 0 {
            (children.get(i - 1), (i - 1)..i)
        } else {
            (children.get(i + 1), i..(i + 1))
        };
        let is_indent =
            neighbour.is_some_and(|c| c.as_token().is_some_and(|t| t.kind() == SyntaxKind::INDENT));
        if is_indent {
            self.0.splice_children(range, vec![]);
        }
    }

    /// Remove the item at `index`, returning its value.
    ///
    /// Returns `Some(value)` if the index was in bounds, `None` otherwise.
    ///
    /// Mutates in place despite `&self` (see crate docs on interior mutability).
    pub fn remove(&self, index: usize) -> Option<crate::as_yaml::YamlNode> {
        // Capture the value before removing so we can return it
        let removed_value = self.get(index);

        // Use children_with_tokens() since splice_children() expects those indices
        let children: Vec<_> = self.0.children_with_tokens().collect();

        // The child position of the entry at `index`, which is not the index
        // itself: INDENT tokens sit between entries.
        let i = children
            .iter()
            .enumerate()
            .filter(|(_, c)| {
                c.as_node()
                    .is_some_and(|n| n.kind() == SyntaxKind::SEQUENCE_ENTRY)
            })
            .map(|(i, _)| i)
            .nth(index)?;
        let is_last = !children.iter().skip(i + 1).any(|c| {
            c.as_node()
                .is_some_and(|n| n.kind() == SyntaxKind::SEQUENCE_ENTRY)
        });

        // Remove the entry first, then the INDENT that
        // separated it from a sibling. Doing them as two
        // separate single-child splices sidesteps a
        // rowan iteration quirk where a multi-child
        // splice can skip elements mid-iteration.
        //
        // For non-first entries the INDENT sits right
        // before this entry (the separator after the
        // previous entry's NEWLINE); for the first
        // entry any INDENT is a top-level formatting
        // one we leave alone.
        self.0.splice_children(i..(i + 1), vec![]);
        self.detach_orphaned_indent(&children, i);

        if !self.is_flow_style() && is_last && i > 0 && mapping_entry_is_last_in_mapping(&self.0) {
            // Removed the last entry of a block sequence
            // that itself terminates its enclosing mapping.
            // Strip trailing whitespace/newline off the new
            // last entry so we don't emit a stray blank
            // line at the end of the document.
            //
            // When the enclosing MAPPING_ENTRY has a
            // following sibling, the new-last-entry's
            // NEWLINE is still needed as the separator
            // between mapping entries -- do not touch it.
            if let Some(prev_entry) = children[..i].iter().rev().find_map(|c| {
                c.as_node()
                    .filter(|n| n.kind() == SyntaxKind::SEQUENCE_ENTRY)
            }) {
                strip_trailing_blank_tokens(prev_entry);
            }
        }
        // If we just drained the last entry from a block
        // sequence under a key, collapse the placeholder
        // scaffold to `key: []` so re-parse still finds
        // the (now-empty) sequence at that key.
        if self.is_empty() {
            collapse_empty_child_sequence_in_parent(&self.0);
        }
        removed_value
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
        let len = self.len();
        if len == 0 {
            return None;
        }
        let removed = self.remove(len - 1);

        debug_assert_eq!(
            self.len(),
            len - 1,
            "pop() invariant: remove() did not reduce length"
        );

        removed
    }

    /// Remove all items from this sequence.
    ///
    /// Mutates in place despite `&self` (see crate docs on interior mutability).
    pub fn clear(&self) {
        // Remove items from the beginning to avoid recalculating indices
        // Use a safety counter to prevent infinite loops
        let initial_len = self.len();
        for _ in 0..initial_len {
            let current_len = self.len();
            if current_len == 0 {
                break;
            }
            // Always remove the first item
            let removed = self.remove(0);
            debug_assert!(
                removed.is_some(),
                "clear() invariant: remove(0) returned None"
            );
            debug_assert_eq!(
                self.len(),
                current_len - 1,
                "clear() invariant: remove(0) did not reduce length"
            );
        }
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
        builder.start_node(SyntaxKind::SEQUENCE.into());
        crate::as_yaml::copy_node_content_with_indent(builder, &self.0, indent);
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
        let nested = root.get("s").unwrap();
        nested.as_sequence().unwrap().push("x");
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
        let nested = root.get("s").unwrap();
        nested.as_sequence().unwrap().push("x");
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
        ] {
            for count in 0..6usize {
                let pushed = {
                    let doc = Document::from_str(src).unwrap();
                    let seq = doc.as_mapping().unwrap().get_sequence("s").unwrap();
                    for _ in 0..count {
                        seq.push(crate::scalar::ScalarValue::null());
                    }
                    doc.to_string()
                };
                let extended = {
                    let doc = Document::from_str(src).unwrap();
                    let seq = doc.as_mapping().unwrap().get_sequence("s").unwrap();
                    seq.extend_with(crate::scalar::ScalarValue::null(), count);
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
        assert_eq!(doc.to_string(), "a:\n  seq: [item1]\n");
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
        let items = mapping.get_sequence("items").unwrap();
        items.push("apple");
        items.push("banana");
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
        assert_eq!(doc.to_string(), "- a\n- x\n- c\n");

        let doc = Document::from_str("- a\n- \n- c\n").unwrap();
        let seq = doc.as_sequence().unwrap();
        let removed = seq.remove(1);
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
        assert_eq!(doc.to_string(), "[a, x, c]");

        let doc = Document::from_str("[a, , c]").unwrap();
        let seq = doc.as_sequence().unwrap();
        let removed = seq.remove(1);
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
    fn test_sequence_set_alias() {
        let yaml = "colors:\n  - *red\n  - keep\n";
        let parsed = YamlFile::from_str(yaml).unwrap();
        let doc = parsed.document().unwrap();
        let mapping = doc.as_mapping().unwrap();
        let colors_node = mapping.get("colors").unwrap();
        let seq = colors_node.as_sequence().unwrap();

        assert!(seq.set(0, "blue"));
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
        let values: Vec<_> = seq.values().collect();
        assert_eq!(values.len(), 3);
        assert_eq!(
            values[2].as_scalar().map(|s| s.as_string()),
            Some("item3".to_string())
        );

        // Test insert
        seq.insert(0, "item0");
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
        team.push("Diana");

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
        let scores = mapping.get_sequence("scores").unwrap();
        scores.push(92);
        scores.set(0, 100);

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
        config.set("retries", 5);

        let servers = config.get_sequence("servers").unwrap();
        servers.push("host3");
        servers.set(0, "primary-host");

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
        assert_eq!(popped.to_string(), "c");
        assert_eq!(seq.len(), 2);

        let popped = seq.pop().unwrap();
        assert_eq!(popped.to_string(), "b");
        assert_eq!(seq.len(), 1);

        let expected = "items:\n  - a";
        assert_eq!(doc.to_string().trim_end(), expected);

        let popped = seq.pop().unwrap();
        assert_eq!(popped.to_string(), "a");
        assert_eq!(seq.len(), 0);
        assert!(seq.pop().is_none());
    }

    #[test]
    fn test_sequence_clear() {
        use crate::Document;
        let doc = Document::from_str("items:\n  - a\n  - b\n  - c").unwrap();
        let mapping = doc.as_mapping().unwrap();
        let seq = mapping.get_sequence("items").unwrap();

        assert_eq!(seq.len(), 3);
        seq.clear();
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
        assert_eq!(popped.to_string(), "c");
        assert_eq!(seq.len(), 2);

        let popped = seq.pop().unwrap();
        assert_eq!(popped.to_string(), "b");
        assert_eq!(seq.len(), 1);

        let popped = seq.pop().unwrap();
        assert_eq!(popped.to_string(), "a");
        assert_eq!(seq.len(), 0);
        assert!(seq.pop().is_none());
    }

    #[test]
    fn test_flow_sequence_clear() {
        use crate::Document;
        let doc = Document::from_str("items: [a, b, c]").unwrap();
        let mapping = doc.as_mapping().unwrap();
        let seq = mapping.get_sequence("items").unwrap();

        assert_eq!(seq.len(), 3);
        seq.clear();
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
        seq.push("c");
        seq.push("d");

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
        assert_eq!(
            doc.to_string(),
            "items:\n  - replaced\n  - name: second\n    value: 2\n"
        );
    }
}
