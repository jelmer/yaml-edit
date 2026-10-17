mod entry;
mod helpers;
mod movements;

pub(crate) use helpers::build_block_value_node;
pub(crate) use helpers::flow_form;
use helpers::{
    append_comma_space_to_entry, ensure_top_level_empty_renders_as_flow, index_after_entry_line,
    FlowInsertPos,
};
pub(crate) use helpers::{
    block_scalar_body_shift, block_scalar_content_indent, drop_tagged_anchor, held_anchor,
    is_block_scalar, source_anchor_text,
};

use super::{
    ensure_trailing_newline, entry_key, entry_line_terminated, entry_value, fresh_token,
    hand_line_break_to_mapping, has_child_token, move_blank_lines_ahead_of_kept_scalar,
    strip_flow_separator, trailing_newline_reachable, Lang, Sequence, SyntaxNode,
};
use crate::as_yaml::{AsYaml, YamlKind};
use crate::lex::SyntaxKind;
use crate::yaml::{
    collapse_empty_child_collection_in_parent as collapse_empty_child_mapping_in_parent,
    dump_cst_to_string, ValueNode,
};
use rowan::ast::AstNode;
use rowan::GreenNodeBuilder;

ast_node!(
    MappingEntry,
    MAPPING_ENTRY,
    "A key-value pair in a YAML mapping"
);

ast_node!(Mapping, MAPPING, "A YAML mapping (key-value pairs)");

impl Mapping {
    /// Dump the CST (Concrete Syntax Tree) structure to a human-readable string.
    ///
    /// This is intended for debugging and testing. The output shows the full
    /// node hierarchy with indentation.
    pub fn dump_cst(&self) -> String {
        dump_cst_to_string(&self.0, 0)
    }

    /// Iterate over all keys in this mapping as [`YamlNode`](crate::as_yaml::YamlNode)s.
    ///
    /// Each key is returned as a [`YamlNode`](crate::as_yaml::YamlNode) wrapping
    /// the underlying CST node, preserving quoting style and other formatting.
    /// The nodes implement [`AsYaml`](crate::AsYaml), so they can be passed
    /// back to [`get`](Self::get), [`contains_key`](Self::contains_key), etc.,
    /// and compared semantically with [`yaml_eq`](crate::yaml_eq).
    ///
    /// Prefer [`entries`](Self::entries) when you also need the values, or
    /// [`iter`](Self::iter) for `(key, value)` pairs as `(YamlNode, YamlNode)`.
    /// For raw CST nodes, use `pairs()`.
    pub fn keys(&self) -> impl Iterator<Item = crate::as_yaml::YamlNode> + '_ {
        self.pairs().filter_map(|(k, _)| {
            k.children()
                .next()
                .and_then(crate::as_yaml::YamlNode::from_syntax)
        })
    }

    /// Iterate over raw KEY/VALUE syntax nodes for each mapping entry.
    ///
    /// Each item is `(key_node, value_node)`, both raw CST wrapper nodes
    /// (`KEY`/`VALUE`), not the content nodes inside them. Entries with a
    /// missing key or value (which indicate a parse error in the source) are
    /// silently skipped.
    ///
    /// For most use cases prefer [`iter`](Self::iter) (which yields
    /// `(YamlNode, YamlNode)` pairs) or [`entries`](Self::entries) (which
    /// yields typed [`MappingEntry`] handles that give access to the full
    /// entry including key, value, and mutation methods).
    pub(crate) fn pairs(&self) -> impl Iterator<Item = (SyntaxNode, SyntaxNode)> + '_ {
        self.0
            .children()
            .filter(|n| n.kind() == SyntaxKind::MAPPING_ENTRY)
            .filter_map(|entry| Some((entry_key(&entry)?, entry_value(&entry)?)))
    }

    /// Get the value associated with `key` as a [`YamlNode`](crate::as_yaml::YamlNode).
    ///
    /// Returns `None` if the key does not exist.
    ///
    /// Matching is semantic but type-strict, so a key's type has to match as
    /// well as its text: `1` does not find the string key `"1"`, and the
    /// null key of `: v` is found with
    /// [`ScalarValue::null`](crate::ScalarValue::null) rather than `""`,
    /// which finds the distinct empty-string key of `"": v`.
    ///
    /// ```rust
    /// # use std::str::FromStr;
    /// # use yaml_edit::{Document, ScalarValue};
    /// let doc = Document::from_str(": a\n\"\": b\n").unwrap();
    /// let mapping = doc.as_mapping().unwrap();
    /// assert!(mapping.get(ScalarValue::null()).is_some());
    /// assert!(mapping.get("").is_some());
    /// ```
    pub fn get(&self, key: impl crate::AsYaml) -> Option<crate::as_yaml::YamlNode> {
        self.get_node(key)
            .and_then(crate::as_yaml::YamlNode::from_syntax)
    }

    /// Get the raw content syntax node for `key` (for advanced CST access).
    ///
    /// Returns the content node inside the `VALUE` wrapper - i.e. the actual
    /// `SCALAR`, `MAPPING`, or `SEQUENCE` node. Returns `None` if the key does
    /// not exist. For most use cases prefer [`get`](Self::get).
    pub(crate) fn get_node(&self, key: impl crate::AsYaml) -> Option<SyntaxNode> {
        self.find_entry_by_key(key)
            .and_then(|entry| entry.value())
            .and_then(|value_node| {
                // VALUE nodes wrap the actual content, return the content instead
                value_node.children().next()
            })
    }

    /// Get the value for `key` as a nested [`Mapping`].
    ///
    /// Returns `None` if the key does not exist or its value is not a mapping.
    /// A tag on the value is looked through: `key: !custom` over a block
    /// mapping still gives that mapping.
    pub fn get_mapping(&self, key: impl crate::AsYaml) -> Option<Mapping> {
        self.get(key)?.mapping_content()
    }

    /// Modify a nested mapping in place by applying a closure to it.
    ///
    /// Returns `true` if `key` exists and its value is a mapping (and `f` was
    /// called); returns `false` if the key is missing or its value is not a
    /// mapping.
    ///
    /// Because [`Mapping`] uses interior mutability via rowan's `SyntaxNode`,
    /// the closure receives a shared reference - mutations are still possible
    /// through the node's `set`, `remove`, and other `&self` methods.
    pub fn modify_mapping<F>(&self, key: impl crate::AsYaml, f: F) -> bool
    where
        F: FnOnce(&Mapping),
    {
        // The closure mutates through rowan's interior mutability, so the
        // node found here is the one in the tree - nothing to splice back.
        let Some(nested) = self.get_mapping(key) else {
            return false;
        };
        f(&nested);
        true
    }

    /// Get the value for `key` as a nested [`Sequence`].
    ///
    /// Returns `None` if the key does not exist or its value is not a sequence.
    /// A tag on the value is looked through, as for
    /// [`get_mapping`](Self::get_mapping).
    pub fn get_sequence(&self, key: impl crate::AsYaml) -> Option<Sequence> {
        self.get(key)?.sequence_content()
    }

    /// Returns `true` if this mapping contains an entry with the given key.
    pub fn contains_key(&self, key: impl crate::AsYaml) -> bool {
        self.find_entry_by_key(key).is_some()
    }

    /// Iterate over the raw `KEY` wrapper nodes for all entries.
    ///
    /// For most use cases prefer [`keys`](Self::keys) which yields
    /// [`YamlNode`](crate::as_yaml::YamlNode) keys (formatting-preserving but
    /// comparable via `yaml_eq`), or [`entries`](Self::entries) which yields
    /// full [`MappingEntry`] handles.
    pub(crate) fn key_nodes(&self) -> impl Iterator<Item = SyntaxNode> + '_ {
        self.pairs().map(|(k, _)| k)
    }

    /// Check if the mapping is empty
    pub fn is_empty(&self) -> bool {
        self.pairs().next().is_none()
    }

    /// Get the number of key-value pairs in this mapping
    pub fn len(&self) -> usize {
        self.pairs().count()
    }

    /// Iterate over the values in this mapping as [`YamlNode`](crate::as_yaml::YamlNode)s.
    ///
    /// Only entries whose value can be successfully wrapped in a `YamlNode` are
    /// yielded; malformed or unrecognised value nodes are silently skipped.
    /// Use [`iter`](Self::iter) to get both the key and value simultaneously, or
    /// `pairs()` for the raw `SyntaxNode` pairs.
    pub fn values(&self) -> impl Iterator<Item = crate::as_yaml::YamlNode> + '_ {
        self.pairs().filter_map(|(_, value_node)| {
            // VALUE node contains the actual content as children
            value_node
                .children()
                .next()
                .and_then(crate::as_yaml::YamlNode::from_syntax)
        })
    }

    /// Iterate over `(key, value)` pairs, both as [`YamlNode`](crate::as_yaml::YamlNode)s.
    ///
    /// Entries that cannot be fully wrapped (malformed key or value nodes) are
    /// silently skipped. For raw CST nodes, use `pairs()`; for
    /// typed entry handles, prefer [`entries`](Self::entries).
    pub fn iter(
        &self,
    ) -> impl Iterator<Item = (crate::as_yaml::YamlNode, crate::as_yaml::YamlNode)> + '_ {
        self.pairs().filter_map(|(key_node, value_node)| {
            // KEY and VALUE nodes wrap the actual content - extract children
            let key = key_node
                .children()
                .next()
                .and_then(crate::as_yaml::YamlNode::from_syntax)?;
            let value = value_node
                .children()
                .next()
                .and_then(crate::as_yaml::YamlNode::from_syntax)?;
            Some((key, value))
        })
    }

    /// Create a new empty mapping
    pub fn new() -> Self {
        Self::new_pending_block()
    }

    /// An empty mapping that renders as `{}` and stays flow when filled.
    ///
    /// Use this for a mapping that is meant to be flow-style in the output,
    /// including one that is left empty.
    pub fn new_flow() -> Self {
        Self::empty_with_brace_text("{", "}")
    }

    /// An empty mapping that becomes a block mapping once an entry is added.
    ///
    /// YAML has no block syntax for an empty mapping: block collections are
    /// written as their entries, so with none left there is nothing to write,
    /// and a bare `key:` reads back as null. An empty mapping is therefore
    /// `{}` whatever its eventual style, and block-vs-flow only becomes a
    /// real choice once there is an entry.
    ///
    /// This renders as exactly `{}`, plus a zero-width token that marks it as
    /// destined for block style. [`Mapping::set`] rewrites it into block form
    /// when the first entry arrives; a `{}` that came from the source has no
    /// such marker and keeps its flow style.
    ///
    /// The marker lives only in the tree. Serializing and re-parsing yields
    /// an ordinary `{}`, which stays flow, since YAML has nowhere to record
    /// the intent.
    pub fn new_pending_block() -> Self {
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(SyntaxKind::MAPPING.into());
        builder.token(SyntaxKind::LEFT_BRACE.into(), "{");
        // Zero-width sentinel: renders as nothing, so this is still exactly
        // `{}` in the text, but marks the mapping as one that should become
        // block when it gets its first entry.
        builder.token(SyntaxKind::WHITESPACE.into(), "");
        builder.token(SyntaxKind::RIGHT_BRACE.into(), "}");
        builder.finish_node();
        Mapping(SyntaxNode::new_root_mut(builder.finish()))
    }

    fn empty_with_brace_text(open: &str, close: &str) -> Self {
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(SyntaxKind::MAPPING.into());
        builder.token(SyntaxKind::LEFT_BRACE.into(), open);
        builder.token(SyntaxKind::RIGHT_BRACE.into(), close);
        builder.finish_node();
        Mapping(SyntaxNode::new_root_mut(builder.finish()))
    }

    /// Is this an empty mapping awaiting its first entry? See
    /// [`Mapping::new_pending_block`].
    pub(crate) fn is_block_placeholder(&self) -> bool {
        !self
            .0
            .children()
            .any(|c| c.kind() == SyntaxKind::MAPPING_ENTRY)
            && self
                .0
                .children_with_tokens()
                .filter_map(|c| c.into_token())
                .any(|t| t.kind() == SyntaxKind::WHITESPACE && t.text().is_empty())
    }

    /// Slice this mapping's children into leading decoration, one
    /// `(entry, postscript)` group per entry, and trailing decoration.
    ///
    /// A group carries the tokens between its entry and the next, so
    /// reshuffling groups keeps a between-entry comment glued to the entry it
    /// visually followed.
    #[allow(clippy::type_complexity)]
    fn slice_into_entry_groups(
        &self,
        all: &[rowan::NodeOrToken<SyntaxNode, rowan::SyntaxToken<Lang>>],
    ) -> (
        Vec<rowan::NodeOrToken<SyntaxNode, rowan::SyntaxToken<Lang>>>,
        Vec<(
            SyntaxNode,
            Vec<rowan::NodeOrToken<SyntaxNode, rowan::SyntaxToken<Lang>>>,
        )>,
        Vec<rowan::NodeOrToken<SyntaxNode, rowan::SyntaxToken<Lang>>>,
    ) {
        let entry_indices: Vec<usize> = all
            .iter()
            .enumerate()
            .filter_map(|(i, c)| {
                c.as_node()
                    .filter(|n| n.kind() == SyntaxKind::MAPPING_ENTRY)
                    .map(|_| i)
            })
            .collect();

        let leading = all[..*entry_indices.first().unwrap_or(&all.len())].to_vec();
        let mut trailing = entry_indices
            .last()
            .map(|&i| all[i + 1..].to_vec())
            .unwrap_or_default();

        // An explicit-key entry (`? k` / `: v`) doesn't own its line break;
        // the NEWLINE sits beside it. For the last entry that token lands in
        // `trailing`, which stays put -- so reordering it to a non-final
        // position would run it into the entry that follows. Hand it back so
        // it travels with the entry.
        let last_needs_terminator = entry_indices.last().is_some_and(|&i| {
            all[i]
                .as_node()
                .is_some_and(|n| !trailing_newline_reachable(n))
        });
        let last_entry_terminator = (last_needs_terminator
            && trailing
                .first()
                .is_some_and(|c| c.kind() == SyntaxKind::NEWLINE))
        .then(|| trailing.remove(0));

        let mut groups: Vec<(SyntaxNode, Vec<_>)> = entry_indices
            .windows(2)
            .map(|w| {
                let entry = all[w[0]].as_node().cloned().unwrap();
                let postscript = all[w[0] + 1..w[1]].to_vec();
                (entry, postscript)
            })
            .collect();
        if let Some(&last) = entry_indices.last() {
            let postscript = last_entry_terminator.into_iter().collect();
            groups.push((all[last].as_node().cloned().unwrap(), postscript));
        }
        (leading, groups, trailing)
    }

    /// Reorder fields according to the specified order.
    ///
    /// Fields not in the order list will appear after the ordered fields,
    /// in their original relative order.
    pub fn reorder_fields<I, K>(&self, order: I)
    where
        I: IntoIterator<Item = K>,
        K: crate::AsYaml,
    {
        let order_keys: Vec<K> = order.into_iter().collect();

        // Slice the children into leading decoration, (entry, tokens
        // that follow it up to the next entry), and trailing
        // decoration. Reshuffling (entry, postscript) pairs keeps a
        // between-entry comment glued to the entry it visually
        // followed.
        let all: Vec<_> = self.0.children_with_tokens().collect();
        let (leading, mut groups, trailing) = self.slice_into_entry_groups(&all);

        // Take each named key's group in the order given; anything the
        // caller did not name keeps its relative position at the end.
        let entry_has_key = |entry: &SyntaxNode, key: &K| {
            entry
                .children()
                .find(|n| n.kind() == SyntaxKind::KEY)
                .is_some_and(|k| key_content_matches(&k, key))
        };
        let mut ordered: Vec<_> = order_keys
            .iter()
            .filter_map(|key| {
                let pos = groups
                    .iter()
                    .position(|(entry, _)| entry_has_key(entry, key))?;
                Some(groups.remove(pos))
            })
            .collect();
        ordered.extend(groups);

        let mut new_children = leading;
        for (entry, postscript) in ordered {
            new_children.push(entry.into());
            new_children.extend(postscript);
        }
        new_children.extend(trailing);

        crate::nodes::replace_children(&self.0, 0..all.len(), new_children);
    }
}

/// Check whether the CST key node matches `key` using semantic equality.
///
/// `key_node` may be a raw KEY wrapper; `from_syntax_peeled` unwraps it.
pub(super) fn key_content_matches(key_node: &SyntaxNode, key: impl crate::AsYaml) -> bool {
    match crate::as_yaml::YamlNode::from_syntax_peeled(key_node.clone()) {
        Some(node) => crate::as_yaml::yaml_eq(&node, &key),
        None => false,
    }
}

impl Mapping {
    /// Check if this mapping is in flow style (JSON/inline format with `{}`).
    ///
    /// Returns `true` if the mapping uses flow style (e.g., `{key: value}`),
    /// `false` if it uses block style (e.g., `key: value`).
    pub fn is_flow_style(&self) -> bool {
        // Flow-style mappings start with LEFT_BRACE token. A block
        // placeholder is `{}` in the text but is destined to become block, so
        // it does not count as flow.
        has_child_token(&self.0, |k| k == SyntaxKind::LEFT_BRACE) && !self.is_block_placeholder()
    }

    /// Find the [`MappingEntry`] whose key matches `key`, or `None` if not found.
    ///
    /// Matching is semantic (quoting style is ignored), so `"foo"`, `'foo'`,
    /// and `foo` all match the scalar `"foo"`.
    pub fn find_entry_by_key(&self, key: impl crate::AsYaml) -> Option<MappingEntry> {
        self.0
            .children()
            .filter_map(MappingEntry::cast)
            .find(|entry| entry.key().is_some_and(|k| key_content_matches(&k, &key)))
    }

    /// Find all entries with a given key.
    ///
    /// Returns an iterator over all [`MappingEntry`] instances that match the
    /// given key. This is useful for handling duplicate keys in YAML (which
    /// are allowed by the spec but semantically ambiguous).
    ///
    /// Matching is semantic (quoting style is ignored), so `"foo"`, `'foo'`,
    /// and `foo` all match the scalar `"foo"`.
    ///
    /// # Example
    ///
    /// ```rust
    /// # use std::str::FromStr;
    /// # use yaml_edit::Document;
    /// let yaml = r#"
    /// Reference: First
    /// Reference: Second
    /// Reference: Third
    /// "#;
    ///
    /// let doc = Document::from_str(yaml).unwrap();
    /// let mapping = doc.as_mapping().unwrap();
    ///
    /// // Collect all Reference entries
    /// let refs: Vec<_> = mapping.find_all_entries_by_key("Reference").collect();
    /// assert_eq!(refs.len(), 3);
    ///
    /// // Remove all but the first occurrence
    /// let _: Vec<()> = refs.into_iter().skip(1).map(|entry| entry.remove()).collect();
    /// ```
    pub fn find_all_entries_by_key<'a>(
        &'a self,
        key: impl crate::AsYaml + 'a,
    ) -> impl Iterator<Item = MappingEntry> + 'a {
        self.0
            .children()
            .filter_map(MappingEntry::cast)
            .filter(move |entry| entry.key().is_some_and(|k| key_content_matches(&k, &key)))
    }

    /// Iterate over all entries in this mapping as typed [`MappingEntry`] handles.
    ///
    /// Each [`MappingEntry`] gives access to the key, value, and mutation
    /// methods for that entry. For decoded `(key, value)` pairs, prefer
    /// [`iter`](Self::iter); for raw CST nodes, use `pairs()`.
    pub fn entries(&self) -> impl Iterator<Item = MappingEntry> {
        self.0.children().filter_map(MappingEntry::cast)
    }

    /// Return an [`Entry`](crate::Entry) view into the entry for `key`.
    ///
    /// Mirrors [`std::collections::BTreeMap::entry`], enabling
    /// insert-or-update patterns while preserving formatting.
    ///
    /// ```rust
    /// use std::str::FromStr;
    /// use yaml_edit::Document;
    ///
    /// let doc = Document::from_str("name: Alice\n").unwrap();
    /// let mapping = doc.as_mapping().unwrap();
    ///
    /// mapping.entry("age").or_insert(30);
    /// mapping.entry("name").or_insert("Bob");
    ///
    /// assert_eq!(doc.to_string(), "name: Alice\nage: 30\n");
    /// ```
    pub fn entry<K: crate::AsYaml>(&self, key: K) -> crate::Entry<'_, K> {
        match self.find_entry_by_key(&key) {
            Some(entry) => crate::Entry::Occupied(crate::OccupiedEntry {
                mapping: self,
                entry,
            }),
            None => crate::Entry::Vacant(crate::VacantEntry { mapping: self, key }),
        }
    }

    /// Find the child index of a mapping entry by its key.
    ///
    /// Returns the index within the mapping's children (including non-entry
    /// tokens like whitespace), or `None` if no entry with the given key exists.
    /// This index is suitable for use with `splice_children`.
    pub fn find_entry_index_by_key(&self, key: impl crate::AsYaml) -> Option<usize> {
        // Look through all children (not just entries, to get accurate index)
        self.0
            .children_with_tokens()
            .enumerate()
            .find_map(|(i, child)| {
                let node = child.as_node()?;
                if node.kind() != SyntaxKind::MAPPING_ENTRY {
                    return None;
                }
                let entry = MappingEntry::cast(node.clone())?;
                if entry.key().is_some_and(|k| key_content_matches(&k, &key)) {
                    Some(i)
                } else {
                    None
                }
            })
    }

    /// Set a key-value pair, replacing the existing value if the key exists or
    /// appending a new entry if it does not. Accepts any value that implements
    /// [`AsYaml`](crate::AsYaml) - scalars, mappings, sequences, etc.
    ///
    /// This method always succeeds; it never silently ignores input. See also
    /// [`insert_after`](Self::insert_after) and [`insert_before`](Self::insert_before)
    /// which return `bool` to indicate whether the anchor key was found.
    ///
    /// Mutates in place despite `&self` (see crate docs on interior mutability).
    ///
    /// `set` replaces the value with the node you pass. It does not infer
    /// YAML 1.1 merge keys from a resolved map. To keep identity, pass an
    /// alias node (`*name`) or a mapping that already contains `<<: *name`.
    /// Merge **reads** (`get_resolved()`, `merged()`) stay separate from
    /// this CST replace.
    ///
    /// An anchor on the replaced value stays, on the new value, so aliases
    /// to it still resolve: `b: &x 2` set to `6` gives `b: &x 6`. A node
    /// value that has an anchor of its own keeps it only if the old value
    /// had none.
    ///
    /// ```
    /// use std::str::FromStr;
    /// use yaml_edit::Document;
    ///
    /// let doc = Document::from_str(
    ///     "shared: &shared\n  timeout: 30\n  retries: 3\nservice_a: old\n",
    /// )
    /// .unwrap();
    /// let mapping = doc.as_mapping().unwrap();
    /// let merge = Document::from_str("<<: *shared\ntimeout: 60\n").unwrap();
    /// mapping.set("service_a", merge.as_mapping().unwrap());
    /// assert_eq!(
    ///     doc.to_string(),
    ///     "shared: &shared\n  timeout: 30\n  retries: 3\nservice_a:\n  <<: *shared\n  timeout: 60\n"
    /// );
    /// ```
    pub fn set(&self, key: impl crate::AsYaml, value: impl crate::AsYaml) {
        self.set_as_yaml(key, value);
    }

    /// Detect if this mapping uses explicit key indicators (?)
    pub(super) fn uses_explicit_keys(&self) -> bool {
        // Check if any existing entries use explicit key format
        // The QUESTION token is a child of MAPPING_ENTRY (sibling to KEY), not inside KEY
        for child in self.0.children() {
            if child.kind() == SyntaxKind::MAPPING_ENTRY {
                // Check if this entry has a QUESTION token as a child
                if has_child_token(&child, |k| k == SyntaxKind::QUESTION) {
                    return true;
                }
            }
        }
        false
    }

    /// Internal unified method to set any YAML value type
    fn set_as_yaml<K: crate::AsYaml, V: crate::AsYaml>(&self, key: K, value: V) {
        // A placeholder from `Mapping::new_pending_block` turns into a real block
        // mapping now that it has an entry to hold. This runs before the
        // flow_context check below so the entry is laid out as block.
        if self.is_block_placeholder() && !crate::nodes::sequence::must_render_flow(&self.0) {
            crate::yaml::convert_placeholder_to_block(&self.0);
        }

        // Detect if this mapping is in flow style (JSON format)
        let flow_context = self.is_flow_style();

        // Detect if existing entries use explicit keys
        let use_explicit_keys = self.uses_explicit_keys();

        // An existing entry with this key takes the value in place.
        let existing = self
            .0
            .children()
            .filter_map(MappingEntry::cast)
            .find(|entry| entry.key().is_some_and(|k| key_content_matches(&k, &key)));
        if let Some(entry) = existing {
            // set_value shifts the flow form to the entry's line, so its
            // continuation lines are given relative to that.
            let flow = flow_context.then(|| crate::nodes::flow_quoting(&self.0));
            match flow.and_then(|q| flow_form(&value, 2, q)) {
                Some(flow) => entry.set_value(flow, flow_context),
                None => entry.set_value(value, flow_context),
            }
            return;
        }

        // Entry doesn't exist, create a new one. Tell the constructor at
        // what column its key will sit so a node-backed block value has its
        // interior indentation shifted to line up under the new entry.
        let key_indent = self.detect_indentation_level();
        let flow = flow_context.then(|| crate::nodes::flow_quoting(&self.0));
        let new_entry = MappingEntry::new_in(key, value, flow, use_explicit_keys, key_indent);
        self.insert_entry_cst(&new_entry.0);
    }

    /// Insert `new_entry` into a flow-style mapping (`{...}`) at the position
    /// dictated by `where_at`, wiring up `, ` separators so the entries stay
    /// syntactically valid inside the braces.
    pub(super) fn insert_flow_entry_cst_at(&self, new_entry: &SyntaxNode, where_at: FlowInsertPos) {
        let brace_pos = self.0.children_with_tokens().position(|c| {
            c.as_token()
                .is_some_and(|t| t.kind() == SyntaxKind::RIGHT_BRACE)
        });

        // Resolve the target index. Anchor by node identity so a subsequent
        // splice targets the right slot even if we've added separators.
        let target_idx = match where_at {
            FlowInsertPos::End => {
                brace_pos.unwrap_or_else(|| self.0.children_with_tokens().count())
            }
            FlowInsertPos::After(ref n) => self
                .0
                .children_with_tokens()
                .position(|c| c.as_node() == Some(n))
                .map_or_else(|| brace_pos.unwrap_or(0), |i| i + 1),
            FlowInsertPos::Before(ref n) => self
                .0
                .children_with_tokens()
                .position(|c| c.as_node() == Some(n))
                .unwrap_or_else(|| brace_pos.unwrap_or(0)),
        };

        // If a MAPPING_ENTRY precedes the target position, append `, ` to
        // it so the two entries are properly delimited.
        let prev_entry = self
            .0
            .children_with_tokens()
            .take(target_idx)
            .filter_map(|c| c.into_node())
            .filter(|n| n.kind() == SyntaxKind::MAPPING_ENTRY)
            .last();
        if let Some(prev) = prev_entry {
            append_comma_space_to_entry(&prev);
        }
        // If a MAPPING_ENTRY follows the target position, append `, ` to
        // the *new* entry so it separates from the next one.
        let has_following_entry = self.0.children_with_tokens().skip(target_idx).any(|c| {
            c.as_node()
                .is_some_and(|n| n.kind() == SyntaxKind::MAPPING_ENTRY)
        });
        if has_following_entry {
            append_comma_space_to_entry(new_entry);
        }

        self.0
            .splice_children(target_idx..target_idx, vec![new_entry.clone().into()]);
    }

    /// Internal method to insert a new entry at the end (does not check for duplicates)
    fn insert_entry_cst(&self, new_entry: &SyntaxNode) {
        if self.is_flow_style() {
            self.insert_flow_entry_cst_at(new_entry, FlowInsertPos::End);
            return;
        }

        // Count children and check if last entry has trailing newline
        let mut count = 0;
        let mut last_mapping_entry: Option<SyntaxNode> = None;

        for child in self.0.children_with_tokens() {
            count += 1;
            if let Some(node) = child
                .as_node()
                .filter(|n| n.kind() == SyntaxKind::MAPPING_ENTRY)
            {
                last_mapping_entry = Some(node.clone());
            }
        }

        // Does the last entry end with a NEWLINE leaf? Checking
        // last_token().kind() == NEWLINE alone misses the case where
        // the tail token is a zero-width implicit-null (e.g. `? b\n`
        // under a tagged mapping) but a real NEWLINE sits just before
        // it in the token stream. Walk the token tail instead.
        let has_trailing_newline = if let Some(entry) = &last_mapping_entry {
            entry_line_terminated(entry)
        } else {
            trailing_newline_reachable(&self.0)
        };

        let mut new_elements = Vec::new();
        let insert_pos = if self.ends_with_lifted_comment_indent() {
            count - 1
        } else {
            count
        };

        // Give the previous entry its own trailing NEWLINE (see
        // insert_at_index for why standalone separators are fragile).
        if count > 0 && !has_trailing_newline {
            if let Some(prev_entry) = &last_mapping_entry {
                ensure_trailing_newline(prev_entry);
            }
        }

        let indent_level = self.detect_indentation_level();
        if indent_level > 0 && count > 0 {
            new_elements.push(fresh_token(SyntaxKind::INDENT, &" ".repeat(indent_level)).into());
        }

        new_elements.push(new_entry.clone().into());

        // Note: We don't add a trailing newline here because MappingEntry::new()
        // already adds one as part of the newline ownership model (entries own their trailing newlines)

        self.0.splice_children(insert_pos..insert_pos, new_elements);

        // If the mapping had been empty and sits inside a `key:\n    \n`
        // placeholder, strip the outer MAPPING_ENTRY's trailing NEWLINE
        // that acted as the placeholder marker (see issue #18).
        if count == 0 {
            crate::yaml::detach_empty_collection_placeholder_newline(&self.0, new_entry);
        }
        last_mapping_entry
            .iter()
            .chain([new_entry])
            .for_each(hand_line_break_to_mapping);
        move_blank_lines_ahead_of_kept_scalar(new_entry);
    }

    /// Set a key-value pair with field ordering support
    /// If the key exists, updates its value. If the key doesn't exist, inserts it
    /// at the correct position based on the provided field order.
    /// Fields not in the order list are placed at the end.
    pub fn set_with_field_order<I, K>(
        &self,
        key: impl crate::AsYaml,
        value: impl crate::AsYaml,
        field_order: I,
    ) where
        I: IntoIterator<Item = K>,
        K: crate::AsYaml,
    {
        // Collect field_order so we can iterate it multiple times.
        let field_order: Vec<K> = field_order.into_iter().collect();

        // An existing key just takes the new value, wherever it sits.
        if self.find_entry_by_key(&key).is_some() {
            self.set_as_yaml(&key, &value);
            return;
        }

        let children: Vec<_> = self.0.children_with_tokens().collect();

        // Key doesn't exist, need to find the correct insertion position based on field order.
        // Find position of this key in the field order (if it matches any)
        let key_position_in_order = field_order
            .iter()
            .position(|field| crate::as_yaml::yaml_eq(&key, field));

        if let Some(key_index) = key_position_in_order {
            // Key is in the field order, find the right position to insert
            let mut insert_after_node: Option<SyntaxNode> = None;
            let mut insert_before_node: Option<SyntaxNode> = None;

            // The nearest preceding field that is actually present decides
            // where the new entry goes.
            for field in field_order.iter().take(key_index).rev() {
                if let Some(entry) = self.find_entry_by_key(field) {
                    insert_after_node = Some(entry.0);
                    break;
                }
            }

            // If no predecessor found, look for the first existing key in document order
            // that comes after this one in field_order
            if insert_after_node.is_none() {
                // Nothing precedes it, so go before the first present field
                // that the order puts after it.
                insert_before_node = self.entries().find_map(|entry| {
                    let key_node = entry.key()?;
                    let pos = field_order
                        .iter()
                        .position(|field| key_content_matches(&key_node, field))?;
                    (pos > key_index).then_some(entry.0)
                });
            }

            // Build the new entry with proper newline ownership
            let flow_context = self.is_flow_style();
            let use_explicit_keys = self.uses_explicit_keys();
            let key_indent = self.detect_indentation_level();
            let flow = flow_context.then(|| crate::nodes::flow_quoting(&self.0));
            let new_entry =
                MappingEntry::new_in(&key, &value, flow, use_explicit_keys, key_indent).0;

            match (insert_after_node, insert_before_node) {
                (Some(after), _) if flow_context => {
                    self.insert_flow_entry_cst_at(&new_entry, FlowInsertPos::After(after));
                }
                (Some(after), _) => self.insert_block_entry_after(&children, &new_entry, &after),
                (None, Some(before)) if flow_context => {
                    self.insert_flow_entry_cst_at(&new_entry, FlowInsertPos::Before(before));
                }
                (None, Some(before)) => {
                    self.insert_block_entry_before(&children, &new_entry, &before)
                }
                // No ordered key is present yet, so order says nothing about
                // where this one goes.
                (None, None) => self.set_as_yaml(&key, &value),
            }
        } else {
            // Key is not in field order, append at the end using CST
            self.set_as_yaml(&key, &value);
        }
    }

    /// The INDENT token entries of this mapping are preceded by, if any.
    fn entry_indent_token(&self) -> Option<rowan::SyntaxToken<Lang>> {
        let level = self.detect_indentation_level();
        (level > 0).then(|| super::fresh_token(SyntaxKind::INDENT, &" ".repeat(level)))
    }

    /// Splice `new_entry` into this block mapping on the line after `after`.
    fn insert_block_entry_after(
        &self,
        children: &[rowan::NodeOrToken<SyntaxNode, rowan::SyntaxToken<Lang>>],
        new_entry: &SyntaxNode,
        after: &SyntaxNode,
    ) {
        ensure_trailing_newline(after);
        let idx = children
            .iter()
            .position(|c| c.as_node() == Some(after))
            .expect("after was found in children earlier");
        let at = index_after_entry_line(&self.0, idx);
        let mut elements: Vec<rowan::NodeOrToken<SyntaxNode, rowan::SyntaxToken<Lang>>> = self
            .entry_indent_token()
            .into_iter()
            .map(Into::into)
            .collect();
        elements.push(new_entry.clone().into());
        self.0.splice_children(at..at, elements);
        hand_line_break_to_mapping(after);
        hand_line_break_to_mapping(new_entry);
        move_blank_lines_ahead_of_kept_scalar(new_entry);
    }

    /// Splice `new_entry` into this block mapping ahead of `before`.
    fn insert_block_entry_before(
        &self,
        children: &[rowan::NodeOrToken<SyntaxNode, rowan::SyntaxToken<Lang>>],
        new_entry: &SyntaxNode,
        before: &SyntaxNode,
    ) {
        let idx = children
            .iter()
            .position(|c| c.as_node() == Some(before))
            .expect("before was found in children earlier");
        let prev = children[..idx].iter().rev().find_map(|c| {
            c.as_node()
                .filter(|n| n.kind() == SyntaxKind::MAPPING_ENTRY)
        });
        if let Some(prev) = prev {
            ensure_trailing_newline(prev);
        }
        // The indent preceding `before` (the parent VALUE's for the first
        // child, a sibling INDENT token otherwise) now serves the new entry,
        // so the displaced `before` needs its own sibling INDENT.
        let mut elements: Vec<rowan::NodeOrToken<SyntaxNode, rowan::SyntaxToken<Lang>>> =
            vec![new_entry.clone().into()];
        elements.extend(self.entry_indent_token().map(Into::into));
        self.0.splice_children(idx..idx, elements);
        prev.into_iter()
            .chain([new_entry])
            .for_each(hand_line_break_to_mapping);
        move_blank_lines_ahead_of_kept_scalar(new_entry);
    }

    /// Detect the indentation level (in spaces) used by entries in this mapping.
    ///
    /// Returns `0` for root-level mappings where entries have no leading indentation.
    pub fn detect_indentation_level(&self) -> usize {
        // Look for an INDENT token that precedes an entry inside this mapping.
        // For a multi-entry mapping the parser stores that column here. INDENTs
        // of comment or blank lines say nothing about where keys sit: those
        // lines may be indented arbitrarily.
        // For a single-entry (or empty) mapping the column isn't stored here,
        // it's the INDENT that precedes the MAPPING node in its parent's
        // VALUE. Fall back to that.
        self.0
            .children_with_tokens()
            .filter_map(|c| c.into_token())
            .find(|t| {
                t.kind() == SyntaxKind::INDENT
                    && t.next_sibling_or_token()
                        .is_some_and(|n| n.kind() == SyntaxKind::MAPPING_ENTRY)
            })
            .map_or_else(
                || crate::as_yaml::source_base_indent(&self.0),
                |t| t.text().len(),
            )
    }

    /// Whether this mapping ends in the INDENT of a comment line.
    ///
    /// When a block mapping's last line is a comment, the parser leaves that
    /// line's INDENT here and lifts the comment itself to an ancestor. The
    /// INDENT may be wider than the keys' column. New entries go in front of
    /// it, leaving it to the comment.
    fn ends_with_lifted_comment_indent(&self) -> bool {
        self.0
            .last_child_or_token()
            .is_some_and(|c| c.kind() == SyntaxKind::INDENT)
    }

    /// Insert a key-value pair at a specific index (0-based), preserving formatting.
    ///
    /// If `new_key` already exists in the mapping, the existing entry is replaced
    /// with a newly built entry at the **same position** (the `index` argument is
    /// ignored). Surrounding whitespace in the file is preserved, but the entry
    /// node itself is rebuilt (comments attached to the old entry may be lost).
    /// If `index` is out of bounds, the entry is appended at the end.
    pub fn insert_at_index_preserving(
        &self,
        index: usize,
        new_key: impl crate::AsYaml,
        new_value: impl crate::AsYaml,
    ) {
        match self.find_entry_by_key(&new_key) {
            Some(old) => self.replace_entry(&old.0, new_key, new_value),
            None => self.insert_new_entry_at(index, new_key, new_value),
        }
    }

    /// Replace `old`, an entry of this mapping, with a new entry built the
    /// way `set` builds one, in the same place.
    fn replace_entry(&self, old: &SyntaxNode, key: impl crate::AsYaml, value: impl crate::AsYaml) {
        let flow_context = self.is_flow_style();
        let new_entry = MappingEntry::new_at_indent(
            key,
            value,
            flow_context,
            self.uses_explicit_keys(),
            self.detect_indentation_level(),
        )
        .0;
        if flow_context {
            // The separator to the next entry lives in the entry before it.
            let separator: Vec<_> = old
                .children_with_tokens()
                .skip_while(|c| c.kind() != SyntaxKind::COMMA)
                .collect();
            for c in &separator {
                c.detach();
            }
            crate::nodes::append_children(&new_entry, separator);
        } else if !trailing_newline_reachable(old) {
            // The old line break goes, whether beside the entry or missing
            // at the end of the input, and the new entry brings its own.
            match old.next_sibling_or_token() {
                Some(newline) if newline.kind() == SyntaxKind::NEWLINE => newline.detach(),
                _ => {
                    let last = new_entry
                        .last_child_or_token()
                        .filter(|c| c.kind() == SyntaxKind::NEWLINE);
                    if let Some(last) = last {
                        last.detach();
                    }
                }
            }
        }
        let i = old.index();
        self.0
            .splice_children(i..i + 1, vec![new_entry.clone().into()]);
        hand_line_break_to_mapping(&new_entry);
    }

    /// Remove a key-value pair, returning the removed entry.
    ///
    /// Returns `Some(entry)` if the key existed and was removed, or `None` if
    /// the key was not found. The returned [`MappingEntry`] is detached from
    /// the tree; callers can inspect its key and value or re-insert it
    /// elsewhere.
    ///
    /// Mutates in place despite `&self` (see crate docs on interior mutability).
    pub fn remove(&self, key: impl crate::AsYaml) -> Option<MappingEntry> {
        let children: Vec<_> = self.0.children_with_tokens().collect();

        for (i, child) in children.iter().enumerate() {
            let Some(node) = child.as_node() else {
                continue;
            };
            if node.kind() != SyntaxKind::MAPPING_ENTRY {
                continue;
            }
            let Some(entry) = MappingEntry::cast(node.clone()) else {
                continue;
            };
            if !entry.key_matches(&key) {
                continue;
            }
            self.remove_entry_at(&children, i, node);
            return Some(entry);
        }
        None
    }

    /// Splice the entry at child index `i` out of this mapping, and
    /// restore the tail-newline convention on the new last entry.
    ///
    /// Shared by [`Mapping::remove`] and [`Mapping::remove_nth_occurrence`].
    /// The rule is: if we're removing the last MAPPING_ENTRY and it had
    /// no trailing NEWLINE (an unterminated source), strip the trailing
    /// NEWLINE from the new last entry too so the doc's terminator
    /// character is preserved. Otherwise leave the previous entry's
    /// NEWLINE in place.
    fn remove_entry_at(
        &self,
        children: &[rowan::NodeOrToken<SyntaxNode, rowan::SyntaxToken<Lang>>],
        i: usize,
        removed: &SyntaxNode,
    ) {
        let is_last = !children.iter().skip(i + 1).any(|c| {
            c.as_node()
                .is_some_and(|n| n.kind() == SyntaxKind::MAPPING_ENTRY)
        });
        let removed_had_newline = removed
            .last_token()
            .is_some_and(|t| t.kind() == SyntaxKind::NEWLINE);

        // In a block mapping every entry but the first is preceded by an
        // INDENT token that puts it at the right column; the first
        // entry's indent sits outside, on the parent VALUE. That token
        // belongs to the entry, so it goes with it -- otherwise it
        // orphans onto the following entry, over-indenting it, or is
        // left dangling as trailing whitespace when the entry was last.
        let indent_before = i.checked_sub(1).and_then(|p| {
            children[p]
                .as_token()
                .filter(|t| t.kind() == SyntaxKind::INDENT)
                .cloned()
        });

        // Blank lines on both sides of the entry would merge into a double
        // gap; keep only the ones after it. An explicit-key entry's line
        // break sits beside it rather than inside, and is not a blank line.
        let mut before = std::iter::successors(removed.prev_sibling_or_token(), |c| {
            c.prev_sibling_or_token()
        })
        .skip_while(|c| c.kind() == SyntaxKind::INDENT)
        .peekable();
        let mut blank_lines_before = Vec::new();
        while let Some(newline) = before.next_if(|c| c.kind() == SyntaxKind::NEWLINE) {
            blank_lines_before.push(newline);
        }
        if before
            .peek()
            .and_then(|c| c.as_node())
            .is_some_and(|n| !trailing_newline_reachable(n))
        {
            blank_lines_before.pop();
        }
        let mut after = std::iter::successors(removed.next_sibling_or_token(), |c| {
            c.next_sibling_or_token()
        })
        .take_while(|c| c.kind() == SyntaxKind::NEWLINE);
        let own_line_break = if trailing_newline_reachable(removed) {
            None
        } else {
            after.next()
        };
        let blank_line_after = after.next().is_some();

        self.0.splice_children(i..(i + 1), vec![]);
        if let Some(newline) = own_line_break {
            newline.detach();
        }
        if blank_line_after {
            for newline in blank_lines_before {
                newline.detach();
            }
        }
        // In a flow mapping the new last entry still carries the separator
        // that led to the removed one, unless that one had its own.
        if is_last && self.is_flow_style() && !has_child_token(removed, |k| k == SyntaxKind::COMMA)
        {
            if let Some(prev) = self.entries().last() {
                strip_flow_separator(prev.syntax());
            }
        }
        // Removing the first entry promotes the next one, but the parent
        // VALUE already supplies its indent, so the INDENT now leading
        // this mapping would double it. Drop whichever applies.
        let orphaned_indent = indent_before.or_else(|| {
            self.0
                .children_with_tokens()
                .next()
                .and_then(|c| c.into_token())
                .filter(|t| t.kind() == SyntaxKind::INDENT)
        });
        if let Some(indent) = orphaned_indent {
            indent.detach();
        }

        // Nested mapping drained by remove: collapse to `key: {}` so
        // `set_path("a.b.c", v) + remove_path(...)` doesn't leave a
        // dangling indent scaffold. Top-level drain is left alone here
        // -- callers may add more entries and injecting `{}` would
        // poison subsequent block-style `set`s; `Mapping::clear`
        // handles the explicit-empty case.
        if !self
            .0
            .children()
            .any(|c| c.kind() == SyntaxKind::MAPPING_ENTRY)
        {
            collapse_empty_child_mapping_in_parent(&self.0);
        }

        if !(is_last && i > 0 && !removed_had_newline) {
            return;
        }
        let Some(prev_entry_node) = children[..i].iter().rev().find_map(|c| {
            c.as_node()
                .filter(|n| n.kind() == SyntaxKind::MAPPING_ENTRY)
        }) else {
            return;
        };
        if !prev_entry_node
            .last_token()
            .is_some_and(|t| t.kind() == SyntaxKind::NEWLINE)
        {
            return;
        }
        let count = prev_entry_node.children_with_tokens().count();
        prev_entry_node.splice_children((count - 1)..count, vec![]);
    }

    /// Remove the nth occurrence of a key, returning the removed entry.
    ///
    /// Returns `Some(entry)` if the nth occurrence exists and was removed,
    /// or `None` if there are fewer than `n+1` occurrences of the key.
    /// The index `n` is 0-based (n=0 removes the first occurrence, n=1 removes
    /// the second, etc.).
    ///
    /// This is useful for handling duplicate keys in YAML. While duplicate keys
    /// are semantically ambiguous, they are allowed by the YAML spec, and this
    /// method provides fine-grained control over which occurrence to remove.
    ///
    /// # Example
    ///
    /// ```rust
    /// # use std::str::FromStr;
    /// # use yaml_edit::Document;
    /// let yaml = r#"
    /// Reference: First
    /// Reference: Second
    /// Reference: Third
    /// "#;
    ///
    /// let doc = Document::from_str(yaml).unwrap();
    /// let mapping = doc.as_mapping().unwrap();
    ///
    /// // Remove the second occurrence (index 1)
    /// let removed = mapping.remove_nth_occurrence("Reference", 1);
    /// assert!(removed.is_some());
    ///
    /// // Now only two Reference entries remain
    /// let count = mapping.find_all_entries_by_key("Reference").count();
    /// assert_eq!(count, 2);
    /// ```
    ///
    /// Mutates in place despite `&self` (see crate docs on interior mutability).
    pub fn remove_nth_occurrence(&self, key: impl crate::AsYaml, n: usize) -> Option<MappingEntry> {
        let children: Vec<_> = self.0.children_with_tokens().collect();

        let mut occurrence_count = 0;
        for (i, child) in children.iter().enumerate() {
            let Some(node) = child.as_node() else {
                continue;
            };
            if node.kind() != SyntaxKind::MAPPING_ENTRY {
                continue;
            }
            let Some(entry) = MappingEntry::cast(node.clone()) else {
                continue;
            };
            if !entry.key_matches(&key) {
                continue;
            }
            if occurrence_count != n {
                occurrence_count += 1;
                continue;
            }
            self.remove_entry_at(&children, i, node);
            return Some(entry);
        }
        None
    }

    /// Remove all key-value pairs from this mapping.
    ///
    /// Mutates in place despite `&self` (see crate docs on interior mutability).
    pub fn clear(&self) {
        let keys: Vec<crate::as_yaml::YamlNode> = self.keys().collect();
        for key in keys {
            self.remove(key);
        }
        ensure_top_level_empty_renders_as_flow(&self.0);
    }

    /// Rename a key while preserving its value and formatting.
    ///
    /// The new key is built using the same `AsYaml` infrastructure as other
    /// write methods, so quoting and escaping are handled automatically.
    /// Returns `true` if the key was found and renamed, `false` if `old_key`
    /// does not exist.
    ///
    /// If `new_key` differs from `old_key` and is already present, the
    /// existing `new_key` entry is removed, as with [`set`](Self::set) and
    /// [`move_after`](Self::move_after), so the rename does not create a
    /// duplicate key. The renamed entry keeps its position, value and
    /// comments.
    ///
    /// ```
    /// # use std::str::FromStr;
    /// # use yaml_edit::Document;
    /// let doc = Document::from_str("a: 1\nb: 2\nc: 3\n").unwrap();
    /// assert!(doc.as_mapping().unwrap().rename_key("a", "b"));
    /// assert_eq!(doc.to_string(), "b: 1\nc: 3\n");
    /// ```
    ///
    /// Mutates in place despite `&self` (see crate docs on interior mutability).
    pub fn rename_key(&self, old_key: impl crate::AsYaml, new_key: impl crate::AsYaml) -> bool {
        let indent = self.detect_indentation_level();
        if let Some(key) = flow_form(&new_key, 2, crate::nodes::flow_quoting(&self.0)) {
            return self.rename_key(old_key, key);
        }
        let Some(node) = self.find_entry_by_key(&old_key).map(|e| e.0) else {
            return false;
        };

        let renames_to_itself = entry_key(&node).is_some_and(|k| key_content_matches(&k, &new_key));
        if !renames_to_itself {
            self.remove_other_entries(&node, &new_key);
        }

        // A key spanning lines has to be explicit, with the `:` on a line
        // of its own.
        let make_explicit = !self.is_flow_style()
            && !has_child_token(&node, |k| k == SyntaxKind::QUESTION)
            && super::key_spans_lines(&new_key);
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(SyntaxKind::MAPPING_ENTRY.into());
        let mut after_key = false;
        for entry_child in node.children_with_tokens() {
            match entry_child {
                rowan::NodeOrToken::Node(n) if n.kind() == SyntaxKind::KEY => {
                    if make_explicit {
                        builder.token(SyntaxKind::QUESTION.into(), "?");
                        builder.token(SyntaxKind::WHITESPACE.into(), " ");
                    }
                    let flow = self
                        .is_flow_style()
                        .then(|| crate::nodes::flow_quoting(&self.0));
                    Self::rebuild_key(&mut builder, &n, &new_key, flow);
                    if make_explicit {
                        builder.token(SyntaxKind::NEWLINE.into(), "\n");
                        if indent > 0 {
                            builder.token(SyntaxKind::INDENT.into(), &" ".repeat(indent));
                        }
                    }
                    after_key = true;
                }
                // Whitespace between the key and the `:` would now start
                // the `:` line.
                rowan::NodeOrToken::Token(t)
                    if make_explicit && after_key && t.kind() == SyntaxKind::WHITESPACE => {}
                rowan::NodeOrToken::Node(n) => {
                    crate::yaml::copy_node_to_builder(&mut builder, &n);
                }
                rowan::NodeOrToken::Token(t) => {
                    after_key &= t.kind() != SyntaxKind::COLON;
                    builder.token(t.kind().into(), t.text());
                }
            }
        }
        builder.finish_node();
        let new_entry = SyntaxNode::new_root_mut(builder.finish());
        let i = node.index();
        self.0
            .splice_children(i..i + 1, vec![new_entry.clone().into()]);
        if make_explicit {
            hand_line_break_to_mapping(&new_entry);
        }
        true
    }

    /// Remove every entry other than `keep` whose key matches `key`.
    fn remove_other_entries(&self, keep: &SyntaxNode, key: &impl crate::AsYaml) {
        while let Some((i, other)) = self
            .0
            .children_with_tokens()
            .enumerate()
            .find_map(|(i, c)| {
                let entry = MappingEntry::cast(c.into_node()?)?;
                (entry.0 != *keep && entry.key_matches(key)).then_some((i, entry.0))
            })
        {
            let children: Vec<_> = self.0.children_with_tokens().collect();
            self.remove_entry_at(&children, i, &other);
        }
    }

    /// Rebuild an entry's KEY node around `new_key`, keeping the layout
    /// tokens the old one carried.
    ///
    /// An explicit key (`? k\n  : v`) puts the `:` on its own line, and the
    /// parser keeps that line break and indent inside KEY. They describe the
    /// entry's layout rather than the key, so they carry over; dropped, the
    /// entry collapses onto one line and reparses as one node.
    fn rebuild_key(
        builder: &mut GreenNodeBuilder,
        old: &SyntaxNode,
        new_key: &impl crate::AsYaml,
        flow: Option<crate::as_yaml::FlowQuoting>,
    ) {
        builder.start_node(SyntaxKind::KEY.into());
        let indent = old
            .parent()
            .map_or(0, |entry| crate::as_yaml::source_base_indent(&entry));
        super::build_key_content(builder, new_key, indent, flow);
        for trailing in old
            .children_with_tokens()
            .skip_while(|c| !matches!(c.as_token().map(|t| t.kind()), Some(SyntaxKind::NEWLINE)))
            .filter_map(|c| c.into_token())
        {
            builder.token(trailing.kind().into(), trailing.text());
        }
        builder.finish_node();
    }

    /// Insert a key-value pair immediately after an existing key.
    ///
    /// If `key` already exists in the mapping, its value is updated in-place and
    /// it remains at its current position (it is **not** moved to after `after_key`).
    /// Returns `true` in both the update and the insert cases.
    /// Returns `false` only if `after_key` is not found.
    ///
    /// Use [`move_after`](Self::move_after) if you want
    /// an existing entry to be moved to the new position.
    ///
    /// Mutates in place despite `&self` (see crate docs on interior mutability).
    pub fn insert_after(
        &self,
        after_key: impl crate::AsYaml,
        key: impl crate::AsYaml,
        value: impl crate::AsYaml,
    ) -> bool {
        // Check if the new key already exists - if so, just update it
        if self.find_entry_by_key(&key).is_some() {
            self.set_as_yaml(&key, &value);
            return self.find_entry_by_key(&after_key).is_some();
        }

        // Key doesn't exist yet - delegate to move_after, which already contains
        // the correct insertion logic (including newline handling for entries
        // that lack a trailing newline). The two methods differ only in what
        // they do when the key *already* exists.
        self.move_after(after_key, key, value)
    }

    /// Insert a key-value pair immediately before an existing key.
    ///
    /// If `key` already exists in the mapping, its value is updated in-place and
    /// it remains at its current position (it is **not** moved to before `before_key`).
    /// Returns `true` in both the update and the insert cases.
    /// Returns `false` only if `before_key` is not found.
    ///
    /// Use [`move_before`](Self::move_before) if you want
    /// an existing entry to be moved to the new position.
    ///
    /// Mutates in place despite `&self` (see crate docs on interior mutability).
    pub fn insert_before(
        &self,
        before_key: impl crate::AsYaml,
        key: impl crate::AsYaml,
        value: impl crate::AsYaml,
    ) -> bool {
        // Key exists → update in-place (don't move).
        if self.find_entry_by_key(&key).is_some() {
            self.set_as_yaml(&key, &value);
            // Only return true if before_key also exists (contract: false when
            // reference key is not found).
            return self.find_entry_by_key(&before_key).is_some();
        }

        // Key doesn't exist yet - delegate to move_before, which already contains
        // the correct insertion logic. The two methods differ only in what they do
        // when the key *already* exists.
        self.move_before(before_key, key, value)
    }

    /// Insert a key-value pair at a specific index (0-based).
    ///
    /// If `key` already exists in the mapping, its value is updated in-place and
    /// it remains at its current position (the `index` argument is ignored).
    /// If `index` is out of bounds, the entry is appended at the end.
    /// This method always succeeds; it never returns an error.
    ///
    /// Mutates in place despite `&self` (see crate docs on interior mutability).
    pub fn insert_at_index(
        &self,
        index: usize,
        key: impl crate::AsYaml,
        value: impl crate::AsYaml,
    ) {
        // Check if the key already exists - if so, just update it
        if self.find_entry_by_key(&key).is_some() {
            self.set_as_yaml(&key, &value);
            return;
        }
        self.insert_new_entry_at(index, key, value);
    }

    /// Insert an entry for a key that is not in the mapping yet before the
    /// `index`th entry, or at the end if there are fewer.
    fn insert_new_entry_at(
        &self,
        index: usize,
        key: impl crate::AsYaml,
        value: impl crate::AsYaml,
    ) {
        // As in `set_as_yaml`: a pending-block placeholder becomes a real
        // block mapping before its first entry is laid out.
        if self.is_block_placeholder() && !crate::nodes::sequence::must_render_flow(&self.0) {
            crate::yaml::convert_placeholder_to_block(&self.0);
        }

        // Create the new mapping entry
        let flow_context = self.is_flow_style();
        let use_explicit_keys = self.uses_explicit_keys();
        let key_indent = self.detect_indentation_level();
        let flow = flow_context.then(|| crate::nodes::flow_quoting(&self.0));
        let new_entry = MappingEntry::new_in(&key, &value, flow, use_explicit_keys, key_indent);

        // Flow needs bracket-aware placement (see insert_flow_entry_cst_at);
        // the block splice below would drop after `}`.
        if flow_context {
            let actual_index = index.min(self.entries().count());
            let where_at = self
                .entries()
                .nth(actual_index)
                .map_or(FlowInsertPos::End, |e| {
                    FlowInsertPos::Before(e.syntax().clone())
                });
            self.insert_flow_entry_cst_at(&new_entry.0, where_at);
            return;
        }

        // Count existing entries to determine actual insertion position
        let entry_count = self.entries().count();
        let actual_index = index.min(entry_count);

        // Find the position in children_with_tokens corresponding to the nth entry
        let mut entry_positions = Vec::new();
        for (i, child) in self.0.children_with_tokens().enumerate() {
            if child
                .as_node()
                .is_some_and(|n| n.kind() == SyntaxKind::MAPPING_ENTRY)
            {
                entry_positions.push(i);
            }
        }

        // Determine where to insert
        let child_count = self.0.children_with_tokens().count();
        let insert_pos = if actual_index < entry_positions.len() {
            entry_positions[actual_index]
        } else if self.ends_with_lifted_comment_indent() {
            child_count - 1
        } else {
            child_count
        };

        let mut new_elements = Vec::new();
        let mut prev_entry = None;

        if insert_pos > 0 {
            // Give the previous entry its own trailing NEWLINE so
            // the "each block MAPPING_ENTRY owns its terminator"
            // invariant holds (see src/nodes/mod.rs); a standalone
            // NEWLINE between entries would render fine now but be
            // fragile against later reshuffles.
            // An INDENT directly before the insertion point belongs to the
            // entry being displaced, not to the line above; look past it for
            // whatever actually terminates the previous line.
            let preceding: Vec<_> = self
                .0
                .children_with_tokens()
                .take(insert_pos)
                .filter(|c| c.kind() != SyntaxKind::INDENT)
                .collect();
            if let Some(prev_node) = preceding.last() {
                if let rowan::NodeOrToken::Node(prev) = prev_node {
                    if prev.kind() == SyntaxKind::MAPPING_ENTRY {
                        ensure_trailing_newline(prev);
                        prev_entry = Some(prev.clone());
                    }
                } else if let rowan::NodeOrToken::Token(t) = prev_node {
                    if t.kind() != SyntaxKind::NEWLINE {
                        new_elements.push(fresh_token(SyntaxKind::NEWLINE, "\n").into());
                    }
                }
            }
        }

        // Add the new entry
        new_elements.push(new_entry.0.clone().into());

        // Inserting before an existing entry hands the new entry the indent
        // that already precedes it, so the displaced entry needs a fresh one.
        // Appending at the end has no such donor: the indent goes in front.
        let displaces_entry = actual_index < entry_positions.len();
        let indent_level = self.detect_indentation_level();
        if indent_level > 0 {
            let token = fresh_token(SyntaxKind::INDENT, &" ".repeat(indent_level));
            if displaces_entry {
                new_elements.push(token.into());
            } else {
                new_elements.insert(0, token.into());
            }
        }

        // Insert at the calculated position
        self.0.splice_children(insert_pos..insert_pos, new_elements);
        prev_entry
            .iter()
            .chain([&new_entry.0])
            .for_each(hand_line_break_to_mapping);
        move_blank_lines_ahead_of_kept_scalar(&new_entry.0);
    }

    crate::nodes::ast_node_spans!("mapping");
}

impl Default for Mapping {
    fn default() -> Self {
        Self::new()
    }
}

// Iterator trait implementations for Mapping

impl<'a> IntoIterator for &'a Mapping {
    type Item = (crate::as_yaml::YamlNode, crate::as_yaml::YamlNode);
    type IntoIter =
        Box<dyn Iterator<Item = (crate::as_yaml::YamlNode, crate::as_yaml::YamlNode)> + 'a>;

    fn into_iter(self) -> Self::IntoIter {
        Box::new(self.iter())
    }
}

impl AsYaml for Mapping {
    fn as_node(&self) -> Option<&SyntaxNode> {
        Some(&self.0)
    }

    fn kind(&self) -> YamlKind {
        YamlKind::Mapping
    }

    fn build_content(
        &self,
        builder: &mut rowan::GreenNodeBuilder,
        indent: usize,
        _flow_context: bool,
    ) -> bool {
        let delta = indent as isize
            - crate::as_yaml::source_reindent_base(&self.0, self.is_flow_style()) as isize;
        builder.start_node(SyntaxKind::MAPPING.into());
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
mod tests;
