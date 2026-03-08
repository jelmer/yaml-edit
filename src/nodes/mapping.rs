use super::{Lang, Scalar, Sequence, SyntaxNode};
use crate::as_yaml::{AsYaml, YamlKind};
use crate::lex::SyntaxKind;
use crate::yaml::{
    add_newline_token, add_node_children_to, dump_cst_to_string, ends_with_newline, Document,
    ValueNode,
};
use rowan::ast::AstNode;
use rowan::GreenNodeBuilder;

ast_node!(
    MappingEntry,
    MAPPING_ENTRY,
    "A key-value pair in a YAML mapping"
);

impl MappingEntry {
    /// Get the underlying syntax node (for debugging/testing)
    #[cfg(test)]
    pub(crate) fn syntax(&self) -> &SyntaxNode {
        &self.0
    }

    /// Return the raw `KEY` wrapper node of this entry.
    ///
    /// The returned node has kind `KEY` and wraps the actual key content
    /// (a scalar, mapping, or sequence node). Returns `None` for malformed
    /// entries that have no key node.
    ///
    /// To compare the key against a value, prefer [`key_matches`](Self::key_matches).
    pub(crate) fn key(&self) -> Option<SyntaxNode> {
        self.0.children().find(|n| n.kind() == SyntaxKind::KEY)
    }

    /// Return `true` if the key of this entry matches `key`.
    ///
    /// Uses semantic YAML equality, so quoting style differences are ignored:
    /// `"foo"`, `'foo'`, and `foo` all match the scalar `"foo"`. Returns
    /// `false` if this entry has no key node.
    pub fn key_matches(&self, key: impl crate::AsYaml) -> bool {
        self.key().is_some_and(|k| key_content_matches(&k, key))
    }

    /// Return the raw `VALUE` wrapper node of this entry.
    ///
    /// The returned node has kind `VALUE` and wraps the actual value content
    /// (a scalar, mapping, or sequence node). Returns `None` for malformed
    /// entries that have no value node.
    pub(crate) fn value(&self) -> Option<SyntaxNode> {
        self.0.children().find(|n| n.kind() == SyntaxKind::VALUE)
    }

    /// Get the key of this entry as a [`YamlNode`](crate::as_yaml::YamlNode).
    ///
    /// Returns `None` for malformed entries that have no key.
    pub fn key_node(&self) -> Option<crate::as_yaml::YamlNode> {
        self.key()
            .and_then(|k| k.children().next())
            .and_then(crate::as_yaml::YamlNode::from_syntax)
    }

    /// Get the value of this entry as a [`YamlNode`](crate::as_yaml::YamlNode).
    ///
    /// Returns `None` for malformed entries that have no value.
    pub fn value_node(&self) -> Option<crate::as_yaml::YamlNode> {
        self.value()
            .and_then(|v| v.children().next())
            .and_then(crate::as_yaml::YamlNode::from_syntax)
    }

    /// Create a new mapping entry (key-value pair) not yet attached to any mapping.
    ///
    /// The entry is built as a standalone CST node; attach it to a mapping with
    /// one of the `insert_*` methods. Block-style values (mappings, sequences)
    /// are indented with 2 spaces relative to the key.
    pub fn new(
        key: impl crate::AsYaml,
        value: impl crate::AsYaml,
        flow_context: bool,
        use_explicit_key: bool,
        indent: usize,
    ) -> Self {
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(SyntaxKind::MAPPING_ENTRY.into());

        if use_explicit_key {
            // Add explicit key indicator as child of MAPPING_ENTRY
            builder.token(SyntaxKind::QUESTION.into(), "?");
            builder.token(SyntaxKind::WHITESPACE.into(), " ");
        }

        // Build KEY
        builder.start_node(SyntaxKind::KEY.into());
        let key_has_newline = key.build_content(&mut builder, 0, false);
        debug_assert!(!key_has_newline, "Keys should not end with newlines");
        builder.finish_node();

        if use_explicit_key {
            // Add newline after key for explicit format
            builder.token(SyntaxKind::NEWLINE.into(), "\n");
        }

        builder.token(SyntaxKind::COLON.into(), ":");

        // Build VALUE
        // Note: For explicit keys, we don't add a space here because
        // the VALUE building logic below will add it for inline values
        builder.start_node(SyntaxKind::VALUE.into());
        let value_ends_with_newline = match (value.is_inline(), value.kind()) {
            // Inline values (scalars, flow collections) go on same line with space
            (true, _) => {
                builder.token(SyntaxKind::WHITESPACE.into(), " ");
                // Note: TAGGED_NODE values (!!set, !!omap, !!pairs) are inline but may
                // end with newlines from their block-style content
                value.build_content(&mut builder, indent, flow_context)
            }
            // Block mappings and sequences start on new line but don't get pre-indented
            // They handle their own indentation via copy_node_content_with_indent
            (false, crate::as_yaml::YamlKind::Mapping)
            | (false, crate::as_yaml::YamlKind::Sequence) => {
                builder.token(SyntaxKind::NEWLINE.into(), "\n");
                value.build_content(&mut builder, indent, flow_context)
            }
            // Block scalars (literal/folded) get newline and indent
            (false, _) => {
                builder.token(SyntaxKind::NEWLINE.into(), "\n");
                builder.token(SyntaxKind::INDENT.into(), "  ");
                value.build_content(&mut builder, 2, flow_context)
            }
        };
        builder.finish_node(); // VALUE

        // Every block-style MAPPING_ENTRY ends with NEWLINE (newline ownership model)
        if !value_ends_with_newline {
            builder.token(SyntaxKind::NEWLINE.into(), "\n");
        }

        builder.finish_node(); // MAPPING_ENTRY
        MappingEntry(SyntaxNode::new_root_mut(builder.finish()))
    }

    /// Replace the value of this entry in place, preserving the key and surrounding whitespace.
    pub fn set_value(&self, new_value: impl crate::AsYaml, flow_context: bool) {
        // Build new VALUE node, preserving any inline comment from the old value
        let mut value_builder = GreenNodeBuilder::new();
        value_builder.start_node(SyntaxKind::VALUE.into());
        
        // Detect indentation from parent mapping
        let mut indent = 0;
        if let Some(parent) = self.0.parent() {
            if let Some(mapping) = Mapping::cast(parent) {
                indent = mapping.detect_indentation_level();
            }
        }
        if indent == 0 {
            indent = 2; // Default fallback
        }

        new_value.build_content(&mut value_builder, indent, flow_context);

        // Find the old VALUE node and extract trailing whitespace + comment
        for child in self.0.children_with_tokens() {
            if let Some(node) = child.as_node() {
                if node.kind() == SyntaxKind::VALUE {
                    // Look for trailing WHITESPACE + COMMENT after the value content.
                    // The VALUE node may contain SCALAR, MAPPING, SEQUENCE, ALIAS, etc.
                    // followed by optional inline WHITESPACE + COMMENT. We preserve
                    // the trailing tokens regardless of what the content node type is.
                    let mut found_content = false;
                    for val_child in node.children_with_tokens() {
                        match val_child.kind() {
                            SyntaxKind::WHITESPACE | SyntaxKind::COMMENT if found_content => {
                                // Preserve inline whitespace and comment
                                if let Some(tok) = val_child.as_token() {
                                    value_builder.token(tok.kind().into(), tok.text());
                                }
                            }
                            SyntaxKind::WHITESPACE | SyntaxKind::COMMENT => {
                                // Whitespace/comment before content - skip
                            }
                            _ => {
                                found_content = true;
                            }
                        }
                    }
                    break;
                }
            }
        }

        value_builder.finish_node();
        let new_value_node = SyntaxNode::new_root_mut(value_builder.finish());

        // Find and replace the VALUE child using splice_children
        for (i, child) in self.0.children_with_tokens().enumerate() {
            if let Some(node) = child.as_node() {
                if node.kind() == SyntaxKind::VALUE {
                    self.0
                        .splice_children(i..i + 1, vec![new_value_node.into()]);
                    return;
                }
            }
        }

        // If no VALUE node was found, we need to rebuild the entire entry
        // because we need to insert tokens (whitespace) which requires working
        // at the green tree level
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(SyntaxKind::MAPPING_ENTRY.into());

        let mut value_inserted = false;
        for child in self.0.children_with_tokens() {
            match child {
                rowan::NodeOrToken::Node(n) => {
                    // Preserve nodes
                    builder.start_node(n.kind().into());
                    add_node_children_to(&mut builder, &n);
                    builder.finish_node();
                }
                rowan::NodeOrToken::Token(t) => {
                    // Preserve tokens
                    builder.token(t.kind().into(), t.text());
                    // If this is a colon and we haven't inserted the value yet, insert it now
                    if t.kind() == SyntaxKind::COLON && !value_inserted {
                        builder.token(SyntaxKind::WHITESPACE.into(), " ");
                        add_node_children_to(&mut builder, &new_value_node);
                        value_inserted = true;
                    }
                }
            }
        }

        builder.finish_node();
        let new_green = builder.finish();
        let new_entry = SyntaxNode::new_root_mut(new_green);

        // Replace self's entire content with the new entry's content
        // Collect into Vec first to avoid borrow issues
        let new_children: Vec<_> = new_entry.children_with_tokens().collect();
        let child_count = self.0.children_with_tokens().count();
        self.0.splice_children(0..child_count, new_children);
    }

    /// Detach this entry from its parent mapping, effectively removing it.
    ///
    /// The entry node is detached from the tree; the `MappingEntry` value is
    /// consumed. To retrieve the removed entry from a mapping (and get back a
    /// `MappingEntry` you can inspect), use [`Mapping::remove`] instead.
    pub fn discard(self) {
        self.0.detach();
    }

    /// Remove this entry from its parent mapping.
    ///
    /// This is a convenience method that calls [`discard`](Self::discard)
    /// internally. It's useful when you have a [`MappingEntry`] (e.g., from
    /// [`find_all_entries_by_key`](Mapping::find_all_entries_by_key)) and want
    /// to remove it without retrieving it from the mapping again.
    ///
    /// Consumes `self` and detaches the entry from the parent mapping.
    pub fn remove(self) {
        self.discard();
    }
}

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
            .filter_map(|entry| {
                let key = entry.children().find(|n| n.kind() == SyntaxKind::KEY)?;
                let value = entry.children().find(|n| n.kind() == SyntaxKind::VALUE)?;
                Some((key, value))
            })
    }

    /// Get the value associated with `key` as a [`YamlNode`](crate::as_yaml::YamlNode).
    ///
    /// Returns `None` if the key does not exist.
    pub fn get(&self, key: impl crate::AsYaml) -> Option<crate::as_yaml::YamlNode> {
        self.get_node(key)
            .and_then(crate::as_yaml::YamlNode::from_syntax)
    }

    /// Get the raw content syntax node for `key` (for advanced CST access).
    ///
    /// Returns the content node inside the `VALUE` wrapper — i.e. the actual
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
    pub fn get_mapping(&self, key: impl crate::AsYaml) -> Option<Mapping> {
        self.get(key).and_then(|n| n.as_mapping().cloned())
    }

    /// Modify a nested mapping in place by applying a closure to it.
    ///
    /// Returns `true` if `key` exists and its value is a mapping (and `f` was
    /// called); returns `false` if the key is missing or its value is not a
    /// mapping.
    ///
    /// Because [`Mapping`] uses interior mutability via rowan's `SyntaxNode`,
    /// the closure receives a shared reference — mutations are still possible
    /// through the node's `set`, `remove`, and other `&self` methods.
    pub fn modify_mapping<F>(&self, key: impl crate::AsYaml, f: F) -> bool
    where
        F: FnOnce(&Mapping),
    {
        // Find the MAPPING_ENTRY for this key
        let children: Vec<_> = self.0.children_with_tokens().collect();
        for (i, child) in children.iter().enumerate() {
            if let Some(node) = child.as_node() {
                if node.kind() == SyntaxKind::MAPPING_ENTRY {
                    if let Some(key_node) = node.children().find(|n| n.kind() == SyntaxKind::KEY) {
                        if key_content_matches(&key_node, &key) {
                            // Found the entry, now find the VALUE node
                            if let Some(value_node) =
                                node.children().find(|n| n.kind() == SyntaxKind::VALUE)
                            {
                                // Check if the value is a mapping
                                if let Some(mapping_node) = value_node
                                    .children()
                                    .find(|n| n.kind() == SyntaxKind::MAPPING)
                                {
                                    // Create a Mapping and apply the function
                                    let mapping = Mapping(mapping_node);
                                    f(&mapping);

                                    // Replace the old MAPPING_ENTRY with updated one
                                    let entry_children: Vec<_> =
                                        node.children_with_tokens().collect();
                                    let mut builder = GreenNodeBuilder::new();
                                    builder.start_node(SyntaxKind::MAPPING_ENTRY.into());

                                    for entry_child in entry_children {
                                        match entry_child {
                                            rowan::NodeOrToken::Node(n)
                                                if n.kind() == SyntaxKind::VALUE =>
                                            {
                                                // Replace the VALUE node
                                                builder.start_node(SyntaxKind::VALUE.into());

                                                // First copy all non-MAPPING children from the original VALUE node (preserving structure)
                                                for value_child in n.children_with_tokens() {
                                                    match value_child {
                                                        rowan::NodeOrToken::Node(child_node)
                                                            if child_node.kind()
                                                                == SyntaxKind::MAPPING =>
                                                        {
                                                            // Replace the MAPPING node with our updated mapping
                                                            crate::yaml::copy_node_to_builder(
                                                                &mut builder,
                                                                &mapping.0,
                                                            );
                                                        }
                                                        rowan::NodeOrToken::Node(child_node) => {
                                                            // Copy other nodes as-is (preserving formatting)
                                                            crate::yaml::copy_node_to_builder(
                                                                &mut builder,
                                                                &child_node,
                                                            );
                                                        }
                                                        rowan::NodeOrToken::Token(token) => {
                                                            // Copy tokens as-is (preserving newlines, indents, etc)
                                                            builder.token(
                                                                token.kind().into(),
                                                                token.text(),
                                                            );
                                                        }
                                                    }
                                                }

                                                builder.finish_node(); // VALUE
                                            }
                                            rowan::NodeOrToken::Node(n) => {
                                                crate::yaml::copy_node_to_builder(&mut builder, &n);
                                            }
                                            rowan::NodeOrToken::Token(t) => {
                                                builder.token(t.kind().into(), t.text());
                                            }
                                        }
                                    }

                                    builder.finish_node(); // MAPPING_ENTRY
                                    let new_entry = SyntaxNode::new_root_mut(builder.finish());
                                    self.0.splice_children(i..i + 1, vec![new_entry.into()]);
                                    return true;
                                }
                            }
                        }
                    }
                }
            }
        }
        false
    }

    /// Get the value for `key` as a nested [`Sequence`].
    ///
    /// Returns `None` if the key does not exist or its value is not a sequence.
    pub fn get_sequence(&self, key: impl crate::AsYaml) -> Option<Sequence> {
        self.get(key).and_then(|n| n.as_sequence().cloned())
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
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(SyntaxKind::MAPPING.into());
        builder.finish_node();
        Mapping(SyntaxNode::new_root_mut(builder.finish()))
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

        // Collect all MAPPING_ENTRY nodes
        let entry_nodes: Vec<SyntaxNode> = self
            .0
            .children()
            .filter(|child| child.kind() == SyntaxKind::MAPPING_ENTRY)
            .collect();

        // Build ordered list: entries in specified order first, then unordered entries.
        let mut ordered_entries: Vec<SyntaxNode> = Vec::new();
        let mut remaining_entries: Vec<SyntaxNode> = entry_nodes;

        for order_key in &order_keys {
            if let Some(pos) = remaining_entries.iter().position(|entry| {
                entry
                    .children()
                    .find(|n| n.kind() == SyntaxKind::KEY)
                    .map(|k| key_content_matches(&k, order_key))
                    .unwrap_or(false)
            }) {
                ordered_entries.push(remaining_entries.remove(pos));
            }
        }

        let new_children: Vec<_> = ordered_entries
            .into_iter()
            .chain(remaining_entries)
            .map(|node| node.into())
            .collect();

        // Replace all children
        let children_count = self.0.children_with_tokens().count();
        self.0.splice_children(0..children_count, new_children);
    }
}

/// Check whether the CST key node matches `key` using semantic equality.
///
/// `key_node` may be a raw KEY wrapper; `from_syntax_peeled` unwraps it.
fn key_content_matches(key_node: &SyntaxNode, key: impl crate::AsYaml) -> bool {
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
        // Flow-style mappings start with LEFT_BRACE token
        self.0.children_with_tokens().any(|child| {
            child
                .as_token()
                .map(|token| token.kind() == SyntaxKind::LEFT_BRACE)
                .unwrap_or(false)
        })
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
    /// [`AsYaml`](crate::AsYaml) — scalars, mappings, sequences, etc.
    ///
    /// This method always succeeds; it never silently ignores input. See also
    /// [`insert_after`](Self::insert_after) and [`insert_before`](Self::insert_before)
    /// which return `bool` to indicate whether the anchor key was found.
    ///
    /// Mutates in place despite `&self` (see crate docs on interior mutability).
    pub fn set(&self, key: impl crate::AsYaml, value: impl crate::AsYaml) {
        self.set_as_yaml(key, value);
    }

    /// Detect if this mapping uses explicit key indicators (?)
    fn uses_explicit_keys(&self) -> bool {
        // Check if any existing entries use explicit key format
        // The QUESTION token is a child of MAPPING_ENTRY (sibling to KEY), not inside KEY
        for child in self.0.children() {
            if child.kind() == SyntaxKind::MAPPING_ENTRY {
                // Check if this entry has a QUESTION token as a child
                if child.children_with_tokens().any(|t| {
                    t.as_token()
                        .is_some_and(|tok| tok.kind() == SyntaxKind::QUESTION)
                }) {
                    return true;
                }
            }
        }
        false
    }

    /// Internal unified method to set any YAML value type
    fn set_as_yaml<K: crate::AsYaml, V: crate::AsYaml>(&self, key: K, value: V) {
        // Detect if this mapping is in flow style (JSON format)
        let flow_context = self.is_flow_style();

        // Detect if existing entries use explicit keys
        let use_explicit_keys = self.uses_explicit_keys();

        // First, look for an existing entry with this key
        for (i, child) in self.0.children_with_tokens().enumerate() {
            if let Some(node) = child.as_node() {
                if node.kind() == SyntaxKind::MAPPING_ENTRY {
                    if let Some(entry) = MappingEntry::cast(node.clone()) {
                        // Check if this entry matches our key by comparing using yaml_eq
                        if let Some(entry_key_node) = entry.key() {
                            if key_content_matches(&entry_key_node, &key) {
                                // Found it! Update the value in place
                                entry.set_value(value, flow_context);

                                self.0.splice_children(i..i + 1, vec![entry.0.into()]);
                                return;
                            }
                        }
                    }
                }
            }
        }

        // Entry doesn't exist, create a new one
        let indent = self.detect_indentation_level();
        let new_entry = MappingEntry::new(key, value, flow_context, use_explicit_keys, if indent > 0 { indent } else { 2 });
        self.insert_entry_cst(&new_entry.0);
    }

    /// Internal method to insert a new entry at the end (does not check for duplicates)
    fn insert_entry_cst(&self, new_entry: &SyntaxNode) {
        // Count children and check if last entry has trailing newline
        let mut count = 0;
        let mut last_mapping_entry: Option<SyntaxNode> = None;

        for child in self.0.children_with_tokens() {
            count += 1;
            if let Some(node) = child.as_node() {
                if node.kind() == SyntaxKind::MAPPING_ENTRY {
                    last_mapping_entry = Some(node.clone());
                }
            }
        }

        // Check if the last entry ends with a newline, OR if the mapping itself has a trailing newline
        // Note: The newline is inside the entry, not a direct child of the mapping
        let has_trailing_newline = if let Some(entry) = &last_mapping_entry {
            entry
                .last_token()
                .map(|t| t.kind() == SyntaxKind::NEWLINE)
                .unwrap_or(false)
        } else {
            // No mapping entries yet - check if mapping itself has a trailing newline token
            self.0
                .last_token()
                .map(|t| t.kind() == SyntaxKind::NEWLINE)
                .unwrap_or(false)
        };

        let mut new_elements = Vec::new();

        // Always insert at the end - the newline is inside the last entry, not a separate child
        let insert_pos = count;

        // Add indentation if needed
        let mut indent_level = self.detect_indentation_level();
        if indent_level == 0 {
            // Try to detect from parent or default to 2
            indent_level = 2; 
        }
        if count > 0 {
            let mut builder = rowan::GreenNodeBuilder::new();
            builder.start_node(SyntaxKind::ROOT.into());
            // Only add NEWLINE if we're NOT inserting before an existing trailing newline
            if !has_trailing_newline {
                builder.token(SyntaxKind::NEWLINE.into(), "\n");
            }
            builder.token(SyntaxKind::INDENT.into(), &" ".repeat(indent_level));
            builder.finish_node();
            let node = SyntaxNode::new_root_mut(builder.finish());
            // Get ALL tokens, not just the first one
            for child in node.children_with_tokens() {
                if let rowan::NodeOrToken::Token(token) = child {
                    new_elements.push(token.into());
                }
            }
        } else if count > 0 && !has_trailing_newline {
            // Only add newline if there isn't already one
            let mut builder = rowan::GreenNodeBuilder::new();
            builder.start_node(SyntaxKind::ROOT.into());
            builder.token(SyntaxKind::NEWLINE.into(), "\n");
            builder.finish_node();
            let node = SyntaxNode::new_root_mut(builder.finish());
            if let Some(token) = node.first_token() {
                new_elements.push(token.into());
            }
        }

        new_elements.push(new_entry.clone().into());

        // Note: We don't add a trailing newline here because MappingEntry::new()
        // already adds one as part of the newline ownership model (entries own their trailing newlines)

        self.0.splice_children(insert_pos..insert_pos, new_elements);
    }

    /// Compare two key nodes structurally
    pub(crate) fn compare_key_nodes(&self, actual: &SyntaxNode, expected: &SyntaxNode) -> bool {
        // Both must be KEY nodes
        if actual.kind() != SyntaxKind::KEY || expected.kind() != SyntaxKind::KEY {
            return actual.kind() == expected.kind()
                && self.compare_nodes_structurally(actual, expected);
        }

        // Get the actual content nodes (skipping whitespace)
        let actual_content = self.get_key_content_nodes(actual);
        let expected_content = self.get_key_content_nodes(expected);

        if actual_content.len() != expected_content.len() {
            return false;
        }

        for (a, e) in actual_content.iter().zip(expected_content.iter()) {
            if !self.compare_nodes_structurally(a, e) {
                return false;
            }
        }

        true
    }

    /// Get the content nodes of a KEY, skipping whitespace and formatting
    fn get_key_content_nodes(&self, key_node: &SyntaxNode) -> Vec<SyntaxNode> {
        let mut nodes = Vec::new();
        for child in key_node.children_with_tokens() {
            match child {
                rowan::NodeOrToken::Node(n) => {
                    // Include all child nodes (sequences, mappings, etc.)
                    nodes.push(n);
                }
                rowan::NodeOrToken::Token(t) => {
                    // Include significant tokens as synthetic nodes
                    if t.kind() != SyntaxKind::WHITESPACE
                        && t.kind() != SyntaxKind::INDENT
                        && t.kind() != SyntaxKind::QUESTION
                    {
                        // Create a synthetic node for the token to enable comparison
                        let mut token_builder = GreenNodeBuilder::new();
                        token_builder.start_node(t.kind().into());
                        token_builder.token(t.kind().into(), t.text());
                        token_builder.finish_node();
                        nodes.push(SyntaxNode::new_root_mut(token_builder.finish()));
                    }
                }
            }
        }
        nodes
    }

    /// Compare nodes structurally (for complex keys like sequences and mappings)
    fn compare_nodes_structurally(&self, node1: &SyntaxNode, node2: &SyntaxNode) -> bool {
        if node1.kind() != node2.kind() {
            return false;
        }

        match node1.kind() {
            SyntaxKind::SCALAR => {
                // For SCALAR nodes, compare the semantic content (unquoted strings).
                // Kind is already confirmed so cast should not fail; use map_or(false, …)
                // as a safe fallback rather than unwrap.
                let s1 = Scalar::cast(node1.clone()).map(|s| s.as_string());
                let s2 = Scalar::cast(node2.clone()).map(|s| s.as_string());
                s1 == s2 && s1.is_some()
            }
            SyntaxKind::STRING => {
                // For string tokens, compare the actual content
                let mut iter1 = node1
                    .children_with_tokens()
                    .filter_map(|c| c.into_token())
                    .filter(|t| t.kind() == SyntaxKind::STRING);
                let mut iter2 = node2
                    .children_with_tokens()
                    .filter_map(|c| c.into_token())
                    .filter(|t| t.kind() == SyntaxKind::STRING);
                loop {
                    match (iter1.next(), iter2.next()) {
                        (Some(a), Some(b)) if a.text() == b.text() => continue,
                        (None, None) => return true,
                        _ => return false,
                    }
                }
            }
            SyntaxKind::SEQUENCE => {
                // Compare sequence entries
                let mut entries1 = node1
                    .children()
                    .filter(|n| n.kind() == SyntaxKind::SEQUENCE_ENTRY);
                let mut entries2 = node2
                    .children()
                    .filter(|n| n.kind() == SyntaxKind::SEQUENCE_ENTRY);
                loop {
                    match (entries1.next(), entries2.next()) {
                        (Some(e1), Some(e2)) => {
                            if !self.compare_sequence_entries(&e1, &e2) {
                                return false;
                            }
                        }
                        (None, None) => return true,
                        _ => return false,
                    }
                }
            }
            SyntaxKind::MAPPING => {
                // Compare mapping entries (order matters for keys)
                let mut entries1 = node1
                    .children()
                    .filter(|n| n.kind() == SyntaxKind::MAPPING_ENTRY);
                let mut entries2 = node2
                    .children()
                    .filter(|n| n.kind() == SyntaxKind::MAPPING_ENTRY);
                loop {
                    match (entries1.next(), entries2.next()) {
                        (Some(e1), Some(e2)) => {
                            if !self.compare_mapping_entries(&e1, &e2) {
                                return false;
                            }
                        }
                        (None, None) => return true,
                        _ => return false,
                    }
                }
            }
            _ => {
                // For other node types, compare token content
                let filter_tokens = |node: &SyntaxNode| {
                    node.children_with_tokens()
                        .filter_map(|c| c.into_token())
                        .filter(|t| {
                            t.kind() != SyntaxKind::WHITESPACE && t.kind() != SyntaxKind::INDENT
                        })
                };
                let mut iter1 = filter_tokens(node1);
                let mut iter2 = filter_tokens(node2);
                loop {
                    match (iter1.next(), iter2.next()) {
                        (Some(a), Some(b)) if a.kind() == b.kind() && a.text() == b.text() => {
                            continue
                        }
                        (None, None) => return true,
                        _ => return false,
                    }
                }
            }
        }
    }

    /// Compare sequence entries
    fn compare_sequence_entries(&self, entry1: &SyntaxNode, entry2: &SyntaxNode) -> bool {
        let value1 = entry1.children().find(|n| n.kind() == SyntaxKind::VALUE);
        let value2 = entry2.children().find(|n| n.kind() == SyntaxKind::VALUE);

        match (value1, value2) {
            (Some(v1), Some(v2)) => self.compare_nodes_structurally(&v1, &v2),
            (None, None) => true,
            _ => false,
        }
    }

    /// Compare mapping entries
    fn compare_mapping_entries(&self, entry1: &SyntaxNode, entry2: &SyntaxNode) -> bool {
        let key1 = entry1.children().find(|n| n.kind() == SyntaxKind::KEY);
        let key2 = entry2.children().find(|n| n.kind() == SyntaxKind::KEY);
        let value1 = entry1.children().find(|n| n.kind() == SyntaxKind::VALUE);
        let value2 = entry2.children().find(|n| n.kind() == SyntaxKind::VALUE);

        match ((key1, value1), (key2, value2)) {
            ((Some(k1), Some(v1)), (Some(k2), Some(v2))) => {
                self.compare_key_nodes(&k1, &k2) && self.compare_nodes_structurally(&v1, &v2)
            }
            ((Some(k1), None), (Some(k2), None)) => self.compare_key_nodes(&k1, &k2),
            ((None, Some(v1)), (None, Some(v2))) => self.compare_nodes_structurally(&v1, &v2),
            ((None, None), (None, None)) => true,
            _ => false,
        }
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

        // First check if the key already exists - if so, just update it
        let children: Vec<_> = self.0.children_with_tokens().collect();
        for child in children.iter() {
            if let Some(node) = child.as_node() {
                if node.kind() == SyntaxKind::MAPPING_ENTRY {
                    if let Some(key_node) = node.children().find(|n| n.kind() == SyntaxKind::KEY) {
                        if key_content_matches(&key_node, &key) {
                            // Key exists, update its value using unified method
                            self.set_as_yaml(&key, &value);
                            return;
                        }
                    }
                }
            }
        }

        // Key doesn't exist, need to find the correct insertion position based on field order.
        // Find position of this key in the field order (if it matches any)
        let key_position_in_order = field_order
            .iter()
            .position(|field| crate::as_yaml::yaml_eq(&key, field));

        if let Some(key_index) = key_position_in_order {
            // Key is in the field order, find the right position to insert
            let mut insert_after_node: Option<SyntaxNode> = None;
            let mut insert_before_node: Option<SyntaxNode> = None;

            // Look backwards in field_order to find the last existing key before this one
            for field in field_order.iter().take(key_index).rev() {
                for child in children.iter() {
                    if let Some(node) = child.as_node() {
                        if node.kind() == SyntaxKind::MAPPING_ENTRY {
                            if let Some(key_node) =
                                node.children().find(|n| n.kind() == SyntaxKind::KEY)
                            {
                                if key_content_matches(&key_node, field) {
                                    insert_after_node = Some(node.clone());
                                    break;
                                }
                            }
                        }
                    }
                }
                if insert_after_node.is_some() {
                    break;
                }
            }

            // If no predecessor found, look for the first existing key in document order
            // that comes after this one in field_order
            if insert_after_node.is_none() {
                for child in children.iter() {
                    if let Some(node) = child.as_node() {
                        if node.kind() == SyntaxKind::MAPPING_ENTRY {
                            if let Some(existing_key_node) =
                                node.children().find(|n| n.kind() == SyntaxKind::KEY)
                            {
                                // Find this existing key's position in field_order
                                let existing_key_position = field_order.iter().position(|field| {
                                    key_content_matches(&existing_key_node, field)
                                });

                                // If this existing key comes after our new key in field_order, insert before it
                                if let Some(existing_pos) = existing_key_position {
                                    if existing_pos > key_index {
                                        insert_before_node = Some(node.clone());
                                        break;
                                    }
                                }
                            }
                        }
                    }
                }
            }

            // Build the new entry with proper newline ownership
            let flow_context = self.is_flow_style();
            let use_explicit_keys = self.uses_explicit_keys();
            let new_entry = MappingEntry::new(&key, &value, flow_context, use_explicit_keys, 0).0;

            // Insert after the target entry, ensuring it has a trailing newline
            if let Some(after_node) = insert_after_node {
                // Insert after after_node
                let idx = children
                    .iter()
                    .position(|c| c.as_node() == Some(&after_node))
                    .expect("after_node was found in children earlier");

                // Ensure after_node has a trailing newline
                let has_trailing_newline = after_node
                    .last_token()
                    .map(|t| t.kind() == SyntaxKind::NEWLINE)
                    .unwrap_or(false);

                if !has_trailing_newline {
                    // Add trailing newline to after_node
                    let entry_children_count = after_node.children_with_tokens().count();
                    let mut nl_builder = GreenNodeBuilder::new();
                    nl_builder.start_node(SyntaxKind::ROOT.into());
                    nl_builder.token(SyntaxKind::NEWLINE.into(), "\n");
                    nl_builder.finish_node();
                    let nl_node = SyntaxNode::new_root_mut(nl_builder.finish());
                    if let Some(token) = nl_node.first_token() {
                        after_node.splice_children(
                            entry_children_count..entry_children_count,
                            vec![token.into()],
                        );
                    }
                }

                // Insert new entry after after_node
                self.0
                    .splice_children(idx + 1..idx + 1, vec![new_entry.into()]);
            } else if let Some(before_node) = insert_before_node {
                // Insert before before_node
                let idx = children
                    .iter()
                    .position(|c| c.as_node() == Some(&before_node))
                    .expect("before_node was found in children earlier");

                // If there's a previous entry, ensure it has a trailing newline
                if idx > 0 {
                    if let Some(prev_entry) = children[..idx].iter().rev().find_map(|c| {
                        c.as_node()
                            .filter(|n| n.kind() == SyntaxKind::MAPPING_ENTRY)
                    }) {
                        let has_trailing_newline = prev_entry
                            .last_token()
                            .map(|t| t.kind() == SyntaxKind::NEWLINE)
                            .unwrap_or(false);

                        if !has_trailing_newline {
                            let entry_children_count = prev_entry.children_with_tokens().count();
                            let mut nl_builder = GreenNodeBuilder::new();
                            nl_builder.start_node(SyntaxKind::ROOT.into());
                            nl_builder.token(SyntaxKind::NEWLINE.into(), "\n");
                            nl_builder.finish_node();
                            let nl_node = SyntaxNode::new_root_mut(nl_builder.finish());
                            if let Some(token) = nl_node.first_token() {
                                prev_entry.splice_children(
                                    entry_children_count..entry_children_count,
                                    vec![token.into()],
                                );
                            }
                        }
                    }
                }

                // Insert new entry before before_node
                self.0.splice_children(idx..idx, vec![new_entry.into()]);
            } else {
                // No existing ordered keys, just append using CST
                self.set_as_yaml(&key, &value);
            }
        } else {
            // Key is not in field order, append at the end using CST
            self.set_as_yaml(&key, &value);
        }
    }

    /// Detect the indentation level (in spaces) used by entries in this mapping.
    ///
    /// Returns `0` for root-level mappings where entries have no leading indentation.
    pub fn detect_indentation_level(&self) -> usize {
        // Look for an INDENT token that precedes a KEY
        for child in self.0.children_with_tokens() {
            if let Some(token) = child.as_token() {
                if token.kind() == SyntaxKind::INDENT {
                    return token.text().len();
                }
            }
        }
        0 // No indentation found, must be root level
    }

    /// Move a key-value pair to immediately after an existing key.
    ///
    /// If `new_key` already exists in the mapping, it is first **removed** from its
    /// current position and then re-inserted after `after_key` with the new value —
    /// so the key ends up at the requested position regardless of where it was before.
    ///
    /// If `after_key` is not found, returns `false` and leaves the mapping unchanged.
    /// Returns `true` on success.
    ///
    /// Use [`insert_after`](Self::insert_after) if you want existing entries to be
    /// updated in-place rather than moved.
    pub fn move_after(
        &self,
        after_key: impl crate::AsYaml,
        new_key: impl crate::AsYaml,
        new_value: impl crate::AsYaml,
    ) -> bool {
        self.move_after_impl(after_key, new_key, new_value)
    }

    /// Internal implementation for move_after
    fn move_after_impl(
        &self,
        after_key: impl crate::AsYaml,
        new_key: impl crate::AsYaml,
        new_value: impl crate::AsYaml,
    ) -> bool {
        let children: Vec<_> = self.0.children_with_tokens().collect();
        let mut insert_position = None;
        let mut found_key = false;
        let mut last_value_end = 0;

        // First, check if the new key already exists and remove it
        let mut i = 0;
        let mut removed_existing = false;
        while i < children.len() {
            if let Some(node) = children[i].as_node() {
                if node.kind() == SyntaxKind::MAPPING_ENTRY {
                    // Look inside the MAPPING_ENTRY for the KEY
                    for key_child in node.children() {
                        if key_child.kind() == SyntaxKind::KEY
                            && key_content_matches(&key_child, &new_key)
                        {
                            // Found existing key, remove this entire MAPPING_ENTRY
                            let mut remove_range = i..i + 1;

                            // Also remove any trailing newline
                            if i + 1 < children.len() {
                                if let Some(token) = children[i + 1].as_token() {
                                    if token.kind() == SyntaxKind::NEWLINE {
                                        remove_range = i..i + 2;
                                    }
                                }
                            }

                            self.0.splice_children(remove_range, vec![]);
                            removed_existing = true;
                            break;
                        }
                    }
                    if removed_existing {
                        // Need to refresh children list after removal
                        break;
                    }
                }
            }
            if !removed_existing {
                i += 1;
            }
        }

        // If we removed an existing key, refresh the children list
        let children = if removed_existing {
            self.0.children_with_tokens().collect()
        } else {
            children
        };

        // Find the position after the specified key's value
        for child in children.iter() {
            if let Some(_node) = child.as_node() {
            } else if let Some(token) = child.as_token() {
                let _preview = token
                    .text()
                    .replace("\n", "\\n")
                    .chars()
                    .take(40)
                    .collect::<String>();
            }
        }
        for (i, child) in children.iter().enumerate() {
            if let Some(node) = child.as_node() {
                if node.kind() == SyntaxKind::MAPPING_ENTRY {
                    if found_key {
                        // Check if this MAPPING_ENTRY is at the root level
                        // Root level means it's not preceded by INDENT
                        let is_root_level = if i > 0 {
                            children
                                .get(i - 1)
                                .and_then(|c| c.as_token())
                                .map(|t| t.kind() != SyntaxKind::INDENT)
                                .unwrap_or(true)
                        } else {
                            true
                        };

                        if is_root_level {
                            insert_position = Some(i);
                            break;
                        }
                    }
                    // Look inside the MAPPING_ENTRY for the KEY
                    for key_child in node.children() {
                        if key_child.kind() == SyntaxKind::KEY
                            && key_content_matches(&key_child, &after_key)
                        {
                            found_key = true;
                            last_value_end = i + 1; // After this entire MAPPING_ENTRY
                            break;
                        }
                    }
                } else if node.kind() == SyntaxKind::KEY {
                    if key_content_matches(node, &after_key) {
                        found_key = true;
                    }
                } else if node.kind() == SyntaxKind::SCALAR {
                    // For SCALAR nodes that might be keys
                    if key_content_matches(node, &after_key) && !found_key {
                        // This is likely the key we're looking for
                        found_key = true;
                        // Look ahead for the value
                        for (j, child_j) in children[(i + 1)..].iter().enumerate() {
                            if let Some(n) = child_j.as_node() {
                                if n.kind() == SyntaxKind::VALUE || n.kind() == SyntaxKind::SCALAR {
                                    last_value_end = i + 1 + j + 1;
                                    break;
                                }
                            }
                        }
                    }
                } else if node.kind() == SyntaxKind::VALUE && found_key {
                    // We're at the value of the found key
                    last_value_end = i + 1;
                }
            } else if let Some(token) = child.as_token() {
                if found_key && token.kind() == SyntaxKind::COMMENT {
                    // Check if this comment is at the top level (not indented)
                    // Top-level comments can be preceded by:
                    // 1. NEWLINE token (traditional case)
                    // 2. MAPPING_ENTRY node (when all newlines are inside the entry)
                    if i > 0 {
                        if let Some(prev) = children.get(i - 1) {
                            let is_top_level = if let Some(prev_token) = prev.as_token() {
                                // Preceded by token - check if it's NEWLINE (not INDENT)
                                prev_token.kind() == SyntaxKind::NEWLINE
                            } else if let Some(prev_node) = prev.as_node() {
                                // Preceded by node - check if it's a MAPPING_ENTRY
                                // (means all newlines were inside the entry)
                                prev_node.kind() == SyntaxKind::MAPPING_ENTRY
                            } else {
                                false
                            };

                            if is_top_level {
                                // Top-level comment - insert before it
                                insert_position = Some(i);
                                break;
                            }
                        }
                    }
                } else if found_key && token.kind() == SyntaxKind::NEWLINE {
                    // Check if this is a root-level newline (not inside nested content)
                    // Root-level means not preceded by INDENT
                    let is_root_level = if i > 0 {
                        children
                            .get(i - 1)
                            .and_then(|c| c.as_token())
                            .map(|t| t.kind() != SyntaxKind::INDENT)
                            .unwrap_or(true)
                    } else {
                        true
                    };

                    if is_root_level && i + 1 < children.len() {
                        if let Some(next) = children.get(i + 1) {
                            if let Some(next_token) = next.as_token() {
                                if next_token.kind() == SyntaxKind::NEWLINE
                                    || next_token.kind() == SyntaxKind::COMMENT
                                {
                                    // Blank line or comment follows - insert before
                                    // them so the new entry is right after the target
                                    // key and blank lines are preserved before the
                                    // next key
                                    insert_position = Some(i);
                                    break;
                                }
                            } else if next.as_node().is_some() {
                                // Node follows (likely MAPPING_ENTRY) - insert before
                                // this separator newline so the new entry is right
                                // after the target key
                                insert_position = Some(i);
                                break;
                            }
                        }
                    }
                }
            }
        }

        // If we didn't find a newline but found the key, insert after the value
        if insert_position.is_none() && found_key && last_value_end > 0 {
            insert_position = Some(last_value_end);
        }

        if let Some(pos) = insert_position {
            // Create new elements for the key-value pair
            let mut new_elements = Vec::new();

            // Check if the previous entry has a trailing newline and add one if needed
            if pos > 0 {
                // Look backwards for the last MAPPING_ENTRY
                if let Some(prev_entry) = children[..pos].iter().rev().find_map(|child| {
                    child
                        .as_node()
                        .filter(|n| n.kind() == SyntaxKind::MAPPING_ENTRY)
                }) {
                    // Check if it ends with NEWLINE
                    let has_newline = prev_entry
                        .last_token()
                        .map(|t| t.kind() == SyntaxKind::NEWLINE)
                        .unwrap_or(false);

                    // If not, add one to the previous entry (not to the mapping)
                    if !has_newline {
                        let entry_children_count = prev_entry.children_with_tokens().count();
                        let mut nl_builder = GreenNodeBuilder::new();
                        nl_builder.start_node(SyntaxKind::ROOT.into());
                        nl_builder.token(SyntaxKind::NEWLINE.into(), "\n");
                        nl_builder.finish_node();
                        let nl_node = SyntaxNode::new_root_mut(nl_builder.finish());
                        if let Some(token) = nl_node.first_token() {
                            prev_entry.splice_children(
                                entry_children_count..entry_children_count,
                                vec![token.into()],
                            );
                        }
                    }
                }
            }

            // Add indentation if needed
            // Check if we're inserting at root level by looking at the previous element
            let needs_indent = if pos > 0 {
                children
                    .get(pos - 1)
                    .and_then(|c| c.as_token())
                    .map(|t| t.kind() == SyntaxKind::INDENT)
                    .unwrap_or(false)
            } else {
                false
            };

            if needs_indent {
                let indent_level = self.detect_indentation_level();
                if indent_level > 0 {
                    let mut indent_builder = GreenNodeBuilder::new();
                    indent_builder.start_node(SyntaxKind::ROOT.into());
                    indent_builder.token(SyntaxKind::INDENT.into(), &" ".repeat(indent_level));
                    indent_builder.finish_node();
                    let indent_node = SyntaxNode::new_root_mut(indent_builder.finish());
                    if let Some(token) = indent_node.first_token() {
                        new_elements.push(token.into());
                    }
                }
            }

            // Create the MAPPING_ENTRY node
            let (entry, _has_trailing_newline) = self.create_mapping_entry(&new_key, &new_value);

            // Add the new entry (which already has its own trailing newline)
            new_elements.push(entry.into());

            // Splice in the new elements
            self.0.splice_children(pos..pos, new_elements);
            true
        } else {
            false
        }
    }

    /// Move a key-value pair to immediately before an existing key.
    ///
    /// If `new_key` already exists in the mapping, it is first **removed** from its
    /// current position and then re-inserted before `before_key` with the new value.
    ///
    /// If `before_key` is not found, returns `false` and leaves the mapping unchanged.
    /// Returns `true` on success.
    ///
    /// Use [`insert_before`](Self::insert_before) if you want existing entries to be
    /// updated in-place rather than moved.
    pub fn move_before(
        &self,
        before_key: impl crate::AsYaml,
        new_key: impl crate::AsYaml,
        new_value: impl crate::AsYaml,
    ) -> bool {
        self.move_before_impl(before_key, new_key, new_value)
    }

    /// Internal implementation for move_before
    fn move_before_impl(
        &self,
        before_key: impl crate::AsYaml,
        new_key: impl crate::AsYaml,
        new_value: impl crate::AsYaml,
    ) -> bool {
        let children: Vec<_> = self.0.children_with_tokens().collect();
        let mut insert_position = None;

        // First, check if the new key already exists and remove it
        let mut i = 0;
        let mut removed_existing = false;
        while i < children.len() {
            if let Some(node) = children[i].as_node() {
                if node.kind() == SyntaxKind::MAPPING_ENTRY {
                    // Look inside the MAPPING_ENTRY for the KEY
                    for key_child in node.children() {
                        if key_child.kind() == SyntaxKind::KEY
                            && key_content_matches(&key_child, &new_key)
                        {
                            // Found existing key, remove this entire MAPPING_ENTRY
                            let mut remove_range = i..i + 1;

                            // Also remove any trailing newline
                            if i + 1 < children.len() {
                                if let Some(token) = children[i + 1].as_token() {
                                    if token.kind() == SyntaxKind::NEWLINE {
                                        remove_range = i..i + 2;
                                    }
                                }
                            }

                            self.0.splice_children(remove_range, vec![]);
                            removed_existing = true;
                            break;
                        }
                    }
                    if removed_existing {
                        // Need to refresh children list after removal
                        break;
                    }
                } else if (node.kind() == SyntaxKind::KEY || node.kind() == SyntaxKind::SCALAR)
                    && key_content_matches(node, &new_key)
                {
                    // Found existing key, find its VALUE node and replace just that
                    // Look for colon, then VALUE node
                    for (offset, child_j) in children[(i + 1)..].iter().enumerate() {
                        if let Some(node) = child_j.as_node() {
                            if node.kind() == SyntaxKind::VALUE {
                                // Found the VALUE node to replace
                                // Build new VALUE node using the helper
                                let mut value_builder = GreenNodeBuilder::new();
                                Document::build_value_content(&mut value_builder, &new_value, 2);
                                let new_value_node =
                                    SyntaxNode::new_root_mut(value_builder.finish());

                                // Replace just the VALUE node
                                let j = i + 1 + offset;
                                self.0
                                    .splice_children(j..j + 1, vec![new_value_node.into()]);
                                return true;
                            }
                        }
                    }
                    // If no VALUE node found, something's wrong with the structure
                    return false;
                }
            }
            if !removed_existing {
                i += 1;
            }
        }

        // If we removed an existing key, refresh the children list
        let children = if removed_existing {
            self.0.children_with_tokens().collect()
        } else {
            children
        };

        // Find the position before the specified key
        for (i, child) in children.iter().enumerate() {
            if let Some(node) = child.as_node() {
                if node.kind() == SyntaxKind::MAPPING_ENTRY {
                    // Look inside the MAPPING_ENTRY for the KEY
                    for key_child in node.children() {
                        if key_child.kind() == SyntaxKind::KEY
                            && key_content_matches(&key_child, &before_key)
                        {
                            // Found the key, insert before this MAPPING_ENTRY
                            let mut line_start = i;
                            for j in (0..i).rev() {
                                if let Some(token) = children[j].as_token() {
                                    if token.kind() == SyntaxKind::NEWLINE {
                                        line_start = j + 1;
                                        break;
                                    }
                                }
                            }
                            insert_position = Some(line_start);
                            break;
                        }
                    }
                } else if (node.kind() == SyntaxKind::KEY || node.kind() == SyntaxKind::SCALAR)
                    && key_content_matches(node, &before_key)
                {
                    // Found the key, insert before it
                    // Look back to find the start of this line
                    let mut line_start = i;
                    for j in (0..i).rev() {
                        if let Some(token) = children[j].as_token() {
                            if token.kind() == SyntaxKind::NEWLINE {
                                line_start = j + 1;
                                break;
                            }
                        }
                    }
                    insert_position = Some(line_start);
                    break;
                }
            }
        }

        if let Some(pos) = insert_position {
            // Create new AST elements for the key-value pair
            // Build the complete key-value entry as separate nodes/tokens

            // Build each element as a SyntaxNode/Token
            let mut new_elements = Vec::new();

            // Create the MAPPING_ENTRY node
            let (entry, _has_trailing_newline) = self.create_mapping_entry(&new_key, &new_value);
            new_elements.push(entry.into());

            // Note: create_mapping_entry already adds a trailing newline to the MAPPING_ENTRY
            // (newline ownership model), so we don't add an extra one here

            // Splice in the new elements
            self.0.splice_children(pos..pos, new_elements);
            true
        } else {
            false
        }
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
        // Create the new entry using create_mapping_entry
        let (new_entry, _has_trailing_newline) = self.create_mapping_entry(new_key, new_value);

        // Check if key already exists in newly created entry for update detection
        if let Some(created_entry) = MappingEntry::cast(new_entry.clone()) {
            if let Some(created_key_node) = created_entry.key() {
                // Find existing entry with matching key by comparing nodes directly
                let existing_entry_opt = self.entries().find(|entry| {
                    if let Some(entry_key) = entry.key() {
                        self.compare_key_nodes(&entry_key, &created_key_node)
                    } else {
                        false
                    }
                });

                if let Some(existing_entry) = existing_entry_opt {
                    // Replace the existing entry + its trailing newline if present
                    let children: Vec<_> = self.0.children_with_tokens().collect();
                    for (i, child) in children.iter().enumerate() {
                        let Some(node) = child.as_node() else {
                            continue;
                        };
                        if node != existing_entry.syntax() {
                            continue;
                        };

                        // Simple: new entries always end with newline (DESIGN.md rule)
                        // Just replace the old entry node with the new one
                        self.0.splice_children(i..i + 1, vec![new_entry.into()]);

                        return;
                    }
                }
            }
        }

        // Key doesn't exist, insert at the specified index
        let children: Vec<_> = self.0.children_with_tokens().collect();
        // Count existing MAPPING_ENTRY nodes to find insertion point
        let mut entry_indices = Vec::new();
        for (i, child) in children.iter().enumerate() {
            if let Some(node) = child.as_node() {
                if node.kind() == SyntaxKind::MAPPING_ENTRY {
                    entry_indices.push(i);
                }
            }
        }

        let mut new_elements = Vec::new();

        // Determine insertion position in children_with_tokens space
        let insert_pos = if entry_indices.is_empty() {
            // Empty mapping - insert at beginning
            0
        } else if index >= entry_indices.len() {
            // Index beyond end - insert after last entry
            let last_entry_idx = entry_indices[entry_indices.len() - 1];
            // Find the end of the last entry (including its newline if present)
            let mut pos = last_entry_idx + 1;
            while pos < children.len() {
                if let Some(token) = children[pos].as_token() {
                    if token.kind() == SyntaxKind::NEWLINE {
                        pos += 1;
                        break;
                    }
                }
                pos += 1;
            }
            pos
        } else {
            // Insert before the entry at the specified index
            entry_indices[index]
        };

        // Add newline before entry if inserting after existing content
        // Check if previous element ends with newline (either as token or inside node)
        if insert_pos > 0 && !entry_indices.is_empty() {
            let has_newline_before = if let Some(child) = children.get(insert_pos - 1) {
                match child {
                    rowan::NodeOrToken::Token(t) => t.kind() == SyntaxKind::NEWLINE,
                    rowan::NodeOrToken::Node(n) => ends_with_newline(n),
                }
            } else {
                false
            };

            if !has_newline_before {
                add_newline_token(&mut new_elements);
            }
        }

        // Use the already-created MAPPING_ENTRY node (already ends with newline)
        new_elements.push(new_entry.into());

        // Insert at the calculated position
        self.0.splice_children(insert_pos..insert_pos, new_elements);
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

        // Find the entry to remove
        for (i, child) in children.iter().enumerate() {
            if let Some(node) = child.as_node() {
                if node.kind() == SyntaxKind::MAPPING_ENTRY {
                    if let Some(entry) = MappingEntry::cast(node.clone()) {
                        if entry.key_matches(&key) {
                            // Check if this is the last MAPPING_ENTRY
                            let is_last = !children.iter().skip(i + 1).any(|c| {
                                c.as_node()
                                    .is_some_and(|n| n.kind() == SyntaxKind::MAPPING_ENTRY)
                            });

                            // Remove the entry (detaches its SyntaxNode from the tree)
                            self.0.splice_children(i..(i + 1), vec![]);

                            if is_last && i > 0 {
                                // Removed the last entry - remove trailing newline from new last entry
                                // Find the previous MAPPING_ENTRY
                                if let Some(prev_entry_node) =
                                    children[..i].iter().rev().find_map(|c| {
                                        c.as_node()
                                            .filter(|n| n.kind() == SyntaxKind::MAPPING_ENTRY)
                                    })
                                {
                                    // Check if it ends with NEWLINE and remove it
                                    if let Some(last_token) = prev_entry_node.last_token() {
                                        if last_token.kind() == SyntaxKind::NEWLINE {
                                            let entry_children_count =
                                                prev_entry_node.children_with_tokens().count();
                                            prev_entry_node.splice_children(
                                                (entry_children_count - 1)..entry_children_count,
                                                vec![],
                                            );
                                        }
                                    }
                                }
                            }
                            return Some(entry);
                        }
                    }
                }
            }
        }
        None
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

        // Find the nth entry matching the key
        let mut occurrence_count = 0;
        for (i, child) in children.iter().enumerate() {
            let node = child.as_node()?;
            if node.kind() != SyntaxKind::MAPPING_ENTRY {
                continue;
            }

            let entry = MappingEntry::cast(node.clone())?;
            if !entry.key_matches(&key) {
                continue;
            }

            if occurrence_count != n {
                occurrence_count += 1;
                continue;
            }

            // Found the nth occurrence - remove it
            let is_last = !children.iter().skip(i + 1).any(|c| {
                c.as_node()
                    .is_some_and(|n| n.kind() == SyntaxKind::MAPPING_ENTRY)
            });

            self.0.splice_children(i..(i + 1), vec![]);

            if is_last && i > 0 {
                // Removed the last entry - remove trailing newline from new last entry
                let prev_entry_node = children[..i].iter().rev().find_map(|c| {
                    c.as_node()
                        .filter(|n| n.kind() == SyntaxKind::MAPPING_ENTRY)
                })?;

                if let Some(last_token) = prev_entry_node.last_token() {
                    if last_token.kind() == SyntaxKind::NEWLINE {
                        let entry_children_count = prev_entry_node.children_with_tokens().count();
                        prev_entry_node.splice_children(
                            (entry_children_count - 1)..entry_children_count,
                            vec![],
                        );
                    }
                }
            }
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
    }

    /// Rename a key while preserving its value and formatting.
    ///
    /// The new key is built using the same `AsYaml` infrastructure as other
    /// write methods, so quoting and escaping are handled automatically.
    /// Returns `true` if the key was found and renamed, `false` if `old_key`
    /// does not exist.
    ///
    /// Mutates in place despite `&self` (see crate docs on interior mutability).
    pub fn rename_key(&self, old_key: impl crate::AsYaml, new_key: impl crate::AsYaml) -> bool {
        let children: Vec<_> = self.0.children_with_tokens().collect();

        for (i, child) in children.iter().enumerate() {
            if let Some(node) = child.as_node() {
                if node.kind() == SyntaxKind::MAPPING_ENTRY {
                    if let Some(key_node) = node.children().find(|n| n.kind() == SyntaxKind::KEY) {
                        if key_content_matches(&key_node, &old_key) {
                            let entry_children: Vec<_> = node.children_with_tokens().collect();

                            let mut builder = GreenNodeBuilder::new();
                            builder.start_node(SyntaxKind::MAPPING_ENTRY.into());

                            for entry_child in entry_children {
                                match entry_child {
                                    rowan::NodeOrToken::Node(n) if n.kind() == SyntaxKind::KEY => {
                                        // Replace the KEY node using AsYaml::build_content
                                        builder.start_node(SyntaxKind::KEY.into());
                                        new_key.build_content(&mut builder, 0, false);
                                        builder.finish_node(); // KEY
                                    }
                                    rowan::NodeOrToken::Node(n) => {
                                        crate::yaml::copy_node_to_builder(&mut builder, &n);
                                    }
                                    rowan::NodeOrToken::Token(t) => {
                                        builder.token(t.kind().into(), t.text());
                                    }
                                }
                            }

                            builder.finish_node();
                            let new_entry = SyntaxNode::new_root_mut(builder.finish());
                            self.0.splice_children(i..i + 1, vec![new_entry.into()]);
                            return true;
                        }
                    }
                }
            }
        }
        false
    }

    /// Helper to create a MAPPING_ENTRY node from key and value strings
    fn create_mapping_entry(
        &self,
        key: impl crate::AsYaml,
        value: impl crate::AsYaml,
    ) -> (SyntaxNode, bool) {
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(SyntaxKind::MAPPING_ENTRY.into());

        // Add KEY node
        builder.start_node(SyntaxKind::KEY.into());
        key.build_content(&mut builder, 0, false);
        builder.finish_node(); // KEY

        // Add colon
        builder.token(SyntaxKind::COLON.into(), ":");

        // Check if value is inline - if not, add newline + indent before VALUE
        if !value.is_inline() {
            builder.token(SyntaxKind::NEWLINE.into(), "\n");
            builder.token(SyntaxKind::INDENT.into(), "  "); // 2-space indent
        } else {
            builder.token(SyntaxKind::WHITESPACE.into(), " ");
        }

        // Add VALUE node
        builder.start_node(SyntaxKind::VALUE.into());
        let value_ends_with_newline = value.build_content(&mut builder, 2, false);
        builder.finish_node(); // VALUE

        // Every block-style MAPPING_ENTRY ends with NEWLINE
        // Only add if the value content didn't already end with one
        let added_newline = if !value_ends_with_newline {
            builder.token(SyntaxKind::NEWLINE.into(), "\n");
            true
        } else {
            false
        };

        builder.finish_node(); // MAPPING_ENTRY
        (
            SyntaxNode::new_root_mut(builder.finish()),
            added_newline || value_ends_with_newline,
        )
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
            return true;
        }

        // Key doesn't exist yet — delegate to move_after, which already contains
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

        // Key doesn't exist yet — delegate to move_before, which already contains
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

        // Create the new mapping entry
        let flow_context = self.is_flow_style();
        let use_explicit_keys = self.uses_explicit_keys();
        let new_entry = MappingEntry::new(&key, &value, flow_context, use_explicit_keys, 0);

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
        let insert_pos = if actual_index < entry_positions.len() {
            entry_positions[actual_index]
        } else {
            self.0.children_with_tokens().count()
        };

        // Build the elements to insert
        let mut new_elements = Vec::new();

        // Add spacing before the new entry if not at position 0
        if insert_pos > 0 {
            // Check if previous element ends with newline
            // (normally entries own their newlines, but the last entry might not have one
            // if the file didn't end with a newline)
            let needs_newline =
                if let Some(prev) = self.0.children_with_tokens().nth(insert_pos - 1) {
                    match prev {
                        rowan::NodeOrToken::Token(t) => t.kind() != SyntaxKind::NEWLINE,
                        rowan::NodeOrToken::Node(n) => !ends_with_newline(&n),
                    }
                } else {
                    false
                };

            if needs_newline {
                add_newline_token(&mut new_elements);
            }

            // Add indentation
            let indent_level = self.detect_indentation_level();
            if indent_level > 0 {
                let mut indent_builder = GreenNodeBuilder::new();
                indent_builder.start_node(SyntaxKind::ROOT.into());
                indent_builder.token(SyntaxKind::INDENT.into(), &" ".repeat(indent_level));
                indent_builder.finish_node();
                let indent_node = SyntaxNode::new_root_mut(indent_builder.finish());
                if let Some(token) = indent_node.first_token() {
                    new_elements.push(token.into());
                }
            }
        }

        // Add the new entry
        new_elements.push(new_entry.0.into());

        // Insert at the calculated position
        self.0.splice_children(insert_pos..insert_pos, new_elements);
    }

    /// Get the byte offset range of this mapping in the source text.
    ///
    /// Returns the start and end byte offsets as a `TextPosition`.
    pub fn byte_range(&self) -> crate::TextPosition {
        self.0.text_range().into()
    }

    /// Get the line and column where this mapping starts.
    ///
    /// Requires the original source text to calculate line/column from byte offsets.
    /// Line and column numbers are 1-indexed.
    ///
    /// # Arguments
    ///
    /// * `source_text` - The original YAML source text
    pub fn start_position(&self, source_text: &str) -> crate::LineColumn {
        let range = self.byte_range();
        crate::byte_offset_to_line_column(source_text, range.start as usize)
    }

    /// Get the line and column where this mapping ends.
    ///
    /// Requires the original source text to calculate line/column from byte offsets.
    /// Line and column numbers are 1-indexed.
    ///
    /// # Arguments
    ///
    /// * `source_text` - The original YAML source text
    pub fn end_position(&self, source_text: &str) -> crate::LineColumn {
        let range = self.byte_range();
        crate::byte_offset_to_line_column(source_text, range.end as usize)
    }
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
        builder.start_node(SyntaxKind::MAPPING.into());
        crate::as_yaml::copy_node_content_with_indent(builder, &self.0, indent);
        builder.finish_node();
        self.0
            .last_token()
            .map(|t| t.kind() == SyntaxKind::NEWLINE)
            .unwrap_or(false)
    }

    fn is_inline(&self) -> bool {
        ValueNode::is_inline(self)
    }
}
#[cfg(test)]
mod tests {
    use crate::scalar::ScalarValue;
    use crate::yaml::YamlFile;
    use std::str::FromStr;

    #[test]
    fn test_mapping_set_new_key() {
        let yaml = "existing: value";
        let parsed = YamlFile::from_str(yaml).unwrap();

        // Get the document and set on it
        let doc = parsed.document().expect("Should have a document");
        doc.set("new_key", "new_value");

        let output = doc.to_string();

        let expected = r#"existing: value
new_key: new_value"#;
        assert_eq!(output.trim(), expected);
    }
    #[test]
    fn test_mapping_rename_key() {
        let yaml = "old_name: value";
        let parsed = YamlFile::from_str(yaml).unwrap();

        let doc = parsed.document().expect("expected a document");
        let mapping = doc.as_mapping().expect("expected a mapping");
        let renamed = mapping.rename_key("old_name", "new_name");
        assert!(renamed);
        assert!(doc.contains_key("new_name"));
        assert!(!doc.contains_key("old_name"));
    }

    #[test]
    fn test_mapping_remove_key() {
        let yaml = "key1: value1\nkey2: value2";
        let parsed = YamlFile::from_str(yaml).unwrap();

        let doc = parsed.document().expect("expected a document");
        let mapping = doc.as_mapping().expect("expected a mapping");
        let removed = mapping.remove("key1");
        assert!(removed.is_some());
        assert!(!doc.contains_key("key1"));
        assert!(doc.contains_key("key2"));
    }
    #[test]
    fn test_mapping_simple_set() {
        let yaml = "key1: value1";
        let parsed = YamlFile::from_str(yaml).unwrap();

        // Get document and add a new key
        let doc = parsed.document().expect("Should have a document");
        doc.set("key2", "value2");

        let output = doc.to_string();

        let expected = r#"key1: value1
key2: value2"#;
        assert_eq!(output.trim(), expected);
    }
    #[test]
    fn test_mapping_set_preserves_position() {
        // Test that set() preserves the position of existing fields when updating
        let yaml = r#"Name: original_name
Contact: original_contact
Repository: https://github.com/example/repo.git
"#;
        let parsed = YamlFile::from_str(yaml).unwrap();
        let doc = parsed.document().expect("Should have a document");

        // Update Contact - it should stay in position 2, not move to the end
        doc.set("Contact", "updated_contact");

        let output = doc.to_string();
        let expected = r#"Name: original_name
Contact: updated_contact
Repository: https://github.com/example/repo.git
"#;
        assert_eq!(output, expected);
    }
    #[test]
    fn test_mapping_set_preserves_multiple_fields() {
        // Test updating multiple existing fields preserves all positions
        let yaml = r#"Name: tsne
Contact: Justin Donaldson <jdonaldson@gmail.com>
Archive: CRAN
Repository: https://github.com/jdonaldson/rtsne.git
"#;
        let parsed = YamlFile::from_str(yaml).unwrap();
        let doc = parsed.document().expect("Should have a document");

        if let Some(mapping) = doc.as_mapping() {
            // Update Contact - should stay in position 2
            mapping.set("Contact", "New Contact <new@example.com>");
            // Update Archive - should stay in position 3
            mapping.set("Archive", "PyPI");
        }

        let output = doc.to_string();
        let expected = r#"Name: tsne
Contact: New Contact <new@example.com>
Archive: PyPI
Repository: https://github.com/jdonaldson/rtsne.git
"#;
        assert_eq!(output, expected);
    }
    #[test]
    fn test_mapping_insert_after() {
        let yaml = r#"first: 1
second: 2
fourth: 4"#;

        let parsed = YamlFile::from_str(yaml).unwrap();

        let doc = parsed.document().expect("Should have a document");

        // Insert after "second"
        let success = doc.insert_after("second", "third", 3);
        assert!(
            success,
            "insert_after should succeed when reference key exists"
        );

        let output = doc.to_string();

        // Check exact output - should preserve original structure and insert correctly
        let expected = r#"first: 1
second: 2
third: 3
fourth: 4"#;
        assert_eq!(output.trim(), expected);

        // Test inserting after non-existent key
        let failed = doc.insert_after("nonexistent", "new_key", "new_value");
        assert!(
            !failed,
            "insert_after should fail when reference key doesn't exist"
        );

        // Test updating existing key through insert_after
        let updated = doc.insert_after("first", "second", "2_updated");
        assert!(updated, "insert_after should update existing key");
        let updated_output = doc.to_string();
        let expected_updated = r#"first: 1
second: 2_updated
third: 3
fourth: 4"#;
        assert_eq!(updated_output.trim(), expected_updated);
    }
    #[test]
    fn test_mapping_insert_before() {
        let yaml = r#"first: 1
third: 3
fourth: 4"#;

        let parsed = YamlFile::from_str(yaml).unwrap();
        let doc = parsed.document().expect("Should have a document");

        // Insert before "third"
        let success = doc.insert_before("third", "second", 2);
        assert!(
            success,
            "insert_before should succeed when reference key exists"
        );

        let output = doc.to_string();

        // Check exact output - should preserve original structure and insert correctly
        let expected = r#"first: 1
second: 2
third: 3
fourth: 4"#;
        assert_eq!(output.trim(), expected);

        // Test inserting before non-existent key
        let failed = doc.insert_before("nonexistent", "new_key", "new_value");
        assert!(
            !failed,
            "insert_before should fail when reference key doesn't exist"
        );

        // Test updating existing key through insert_before
        let updated = doc.insert_before("fourth", "third", "3_updated");
        assert!(updated, "insert_before should update existing key");
        let output = doc.to_string();
        let expected_updated = r#"first: 1
second: 2
third: 3_updated
fourth: 4"#;
        assert_eq!(output.trim(), expected_updated);
    }
    #[test]
    fn test_mapping_insert_at_index() {
        let yaml = r#"first: 1
third: 3"#;

        let parsed = YamlFile::from_str(yaml).unwrap();
        let doc = parsed.document().expect("Should have a document");

        // Insert at index 1 (between first and third)
        doc.insert_at_index(1, "second", 2);

        let output = doc.to_string();

        // Check exact output - should preserve original structure and insert correctly
        let expected = r#"first: 1
second: 2
third: 3"#;
        assert_eq!(output.trim(), expected);

        // Insert at index 0 (beginning)
        doc.insert_at_index(0, "zero", 0);
        let output2 = doc.to_string();
        let expected2 = r#"zero: 0
first: 1
second: 2
third: 3"#;
        assert_eq!(output2.trim(), expected2);

        // Insert at out-of-bounds index (should append at end)
        doc.insert_at_index(100, "last", "999");
        let output3 = doc.to_string();
        let expected3 = r#"zero: 0
first: 1
second: 2
third: 3
last: '999'"#;
        assert_eq!(output3.trim(), expected3);

        // Test updating existing key through insert_at_index
        doc.insert_at_index(2, "first", "1_updated");
        let final_output = doc.to_string();
        let expected_final = r#"zero: 0
first: 1_updated
second: 2
third: 3
last: '999'"#;
        assert_eq!(final_output.trim(), expected_final);
    }
    #[test]
    fn test_mapping_insert_special_characters() {
        let yaml = "key1: value1";

        let parsed = YamlFile::from_str(yaml).unwrap();
        let doc = parsed.document().expect("Should have a document");

        // Test with special characters that need escaping
        doc.insert_after("key1", "special:key", "value:with:colons");
        doc.insert_before("key1", "key with spaces", "value with spaces");
        doc.insert_at_index(1, "key@symbol", "value#hash");

        // Verify all keys are present
        assert!(doc.contains_key("special:key"));
        assert!(doc.contains_key("key with spaces"));
        assert!(doc.contains_key("key@symbol"));

        // Parse the output to verify it's valid YAML
        let output = doc.to_string();
        let reparsed = YamlFile::from_str(&output);
        assert!(reparsed.is_ok(), "Output should be valid YAML");
    }
    #[test]
    fn test_mapping_insert_empty_values() {
        let yaml = "key1: value1";

        let parsed = YamlFile::from_str(yaml).unwrap();
        let doc = parsed.document().expect("Should have a document");

        // Test with empty values
        doc.insert_after("key1", "empty", "");
        doc.insert_before("key1", "null_key", ScalarValue::null());

        assert!(doc.contains_key("empty"));
        assert!(doc.contains_key("null_key"));

        // Verify the output is valid YAML
        let output = parsed.to_string();
        let reparsed = YamlFile::from_str(&output);
        assert!(
            reparsed.is_ok(),
            "Output with empty values should be valid YAML"
        );
    }

    // Iterator tests

    #[test]
    fn test_mapping_into_iterator() {
        use crate::Document;
        let text = "name: Alice\nage: 30\ncity: Boston";
        let doc = Document::from_str(text).unwrap();
        let mapping = doc.as_mapping().unwrap();

        // Test that we can use for loops directly
        let mut count = 0;
        for (key, value) in &mapping {
            count += 1;

            // Check that we get scalar nodes
            assert!(key.is_scalar());
            assert!(value.is_scalar());
        }

        assert_eq!(count, 3);
    }

    #[test]
    fn test_mapping_into_iterator_collect() {
        use crate::Document;
        let text = "a: 1\nb: 2\nc: 3";
        let doc = Document::from_str(text).unwrap();
        let mapping = doc.as_mapping().unwrap();

        // Collect into a Vec
        let pairs: Vec<_> = (&mapping).into_iter().collect();
        assert_eq!(pairs.len(), 3);

        // Check we can get scalars
        for (key, value) in pairs {
            assert!(key.as_scalar().is_some());
            assert!(value.as_scalar().is_some());
        }
    }

    #[test]
    fn test_mapping_iterator_filter() {
        use crate::Document;
        let text = "a: 1\nb: 2\nc: 3\nd: 4";
        let doc = Document::from_str(text).unwrap();
        let mapping = doc.as_mapping().unwrap();

        // Filter for even values
        let even_count = (&mapping)
            .into_iter()
            .filter(|(_, value)| {
                value
                    .as_scalar()
                    .and_then(|s| s.to_string().parse::<i32>().ok())
                    .map(|n| n % 2 == 0)
                    .unwrap_or(false)
            })
            .count();

        assert_eq!(even_count, 2); // b: 2 and d: 4
    }

    #[test]
    fn test_empty_mapping_iterator() {
        let empty = crate::Mapping::new();

        let count = (&empty).into_iter().count();
        assert_eq!(count, 0);
    }

    #[test]
    fn test_nested_mapping_iteration() {
        use crate::Document;
        let text = "server:\n  host: localhost\n  port: 8080";
        let doc = Document::from_str(text).unwrap();
        let mapping = doc.as_mapping().unwrap();

        // Iterate outer mapping
        for (key, _value) in &mapping {
            if let Some(key_scalar) = key.as_scalar() {
                if key_scalar.to_string() == "server" {
                    // Get nested mapping
                    if let Some(nested_mapping) = mapping.get_mapping("server") {
                        let nested_count = (&nested_mapping).into_iter().count();
                        assert_eq!(nested_count, 2); // host and port
                    }
                }
            }
        }
    }

    // Tests from mapping_operations_test.rs

    // ===== Basic accessor tests =====

    #[test]
    fn test_mapping_keys() {
        let yaml = YamlFile::from_str("name: Alice\nage: 30\ncity: NYC").unwrap();
        let doc = yaml.document().unwrap();
        let mapping = doc.as_mapping().unwrap();

        let keys: Vec<String> = mapping.keys().map(|k| k.to_string()).collect();
        assert_eq!(keys, vec!["name", "age", "city"]);
    }

    #[test]
    fn test_mapping_is_empty() {
        let yaml = YamlFile::from_str("{}").unwrap();
        let doc = yaml.document().unwrap();
        let mapping = doc.as_mapping().unwrap();
        assert!(mapping.is_empty());

        let yaml2 = YamlFile::from_str("key: value").unwrap();
        let doc2 = yaml2.document().unwrap();
        let mapping2 = doc2.as_mapping().unwrap();
        assert!(!mapping2.is_empty());
    }

    #[test]
    fn test_mapping_contains_key() {
        let yaml = YamlFile::from_str("name: Alice\nage: 30").unwrap();
        let doc = yaml.document().unwrap();
        let mapping = doc.as_mapping().unwrap();

        assert!(mapping.contains_key("name"));
        assert!(mapping.contains_key("age"));
        assert!(!mapping.contains_key("city"));
    }

    #[test]
    fn test_mapping_get() {
        let yaml = YamlFile::from_str("name: Alice\nage: 30").unwrap();
        let doc = yaml.document().unwrap();
        let mapping = doc.as_mapping().unwrap();

        assert_eq!(
            mapping
                .get("name")
                .and_then(|v| v.as_scalar().map(|s| s.as_string())),
            Some("Alice".to_string())
        );
        assert_eq!(mapping.get("age").and_then(|v| v.to_i64()), Some(30));
        assert!(mapping.get("city").is_none());
    }

    #[test]
    fn test_mapping_single_entry() {
        let yaml = YamlFile::from_str("key: value").unwrap();
        let doc = yaml.document().unwrap();
        let mapping = doc.as_mapping().unwrap();

        let keys: Vec<String> = mapping.keys().map(|k| k.to_string()).collect();
        assert_eq!(keys, vec!["key"]);
        assert!(!mapping.is_empty());
        assert!(mapping.contains_key("key"));
    }

    // ===== Mutation tests =====

    #[test]
    fn test_mapping_ops_set_new_key() {
        let yaml = YamlFile::from_str("name: Alice").unwrap();
        let doc = yaml.document().unwrap();
        let mapping = doc.as_mapping().unwrap();

        mapping.set("age", 30);

        let keys: Vec<String> = mapping.keys().map(|k| k.to_string()).collect();
        assert_eq!(keys, vec!["name", "age"]);
        assert_eq!(mapping.get("age").and_then(|v| v.to_i64()), Some(30));
    }

    #[test]
    fn test_mapping_set_existing_key() {
        let yaml = YamlFile::from_str("name: Alice\nage: 30").unwrap();
        let doc = yaml.document().unwrap();
        let mapping = doc.as_mapping().unwrap();

        mapping.set("age", 31);

        assert_eq!(mapping.get("age").and_then(|v| v.to_i64()), Some(31));
        let keys: Vec<String> = mapping.keys().map(|k| k.to_string()).collect();
        assert_eq!(keys, vec!["name", "age"]);
    }

    #[test]
    fn test_mapping_remove_existing_key() {
        let yaml = YamlFile::from_str("name: Alice\nage: 30\ncity: NYC").unwrap();
        let doc = yaml.document().unwrap();
        let mapping = doc.as_mapping().unwrap();

        let removed = mapping.remove("age");
        assert!(removed.is_some());

        let keys: Vec<String> = mapping.keys().map(|k| k.to_string()).collect();
        assert_eq!(keys, vec!["name", "city"]);
        assert!(!mapping.contains_key("age"));
    }

    #[test]
    fn test_mapping_remove_nonexistent_key() {
        let yaml = YamlFile::from_str("name: Alice").unwrap();
        let doc = yaml.document().unwrap();
        let mapping = doc.as_mapping().unwrap();

        let removed = mapping.remove("age");
        assert!(removed.is_none());

        let keys: Vec<String> = mapping.keys().map(|k| k.to_string()).collect();
        assert_eq!(keys, vec!["name"]);
    }

    #[test]
    fn test_mapping_remove_all_keys() {
        let yaml = YamlFile::from_str("a: 1\nb: 2").unwrap();
        let doc = yaml.document().unwrap();
        let mapping = doc.as_mapping().unwrap();

        assert!(mapping.remove("a").is_some());
        assert!(mapping.remove("b").is_some());
        assert!(mapping.is_empty());
    }

    #[test]
    fn test_rename_key_basic() {
        let original = r#"name: my-app
version: 1.0
author: Alice"#;

        let yaml = YamlFile::from_str(original).unwrap();

        if let Some(doc) = yaml.document() {
            if let Some(mapping) = doc.as_mapping() {
                let success = mapping.rename_key("version", "app_version");
                assert!(success);
            }
        }

        let expected = r#"name: my-app
app_version: 1.0
author: Alice"#;
        assert_eq!(yaml.to_string(), expected);
    }

    #[test]
    fn test_rename_key_preserves_value() {
        let original = r#"count: 42
enabled: true"#;

        let yaml = YamlFile::from_str(original).unwrap();

        if let Some(doc) = yaml.document() {
            if let Some(mapping) = doc.as_mapping() {
                mapping.rename_key("count", "total");
            }
        }

        let expected = r#"total: 42
enabled: true"#;
        assert_eq!(yaml.to_string(), expected);
    }

    #[test]
    fn test_remove_field() {
        let original = r#"name: my-app
version: 1.0
author: Alice"#;

        let yaml = YamlFile::from_str(original).unwrap();

        if let Some(doc) = yaml.document() {
            if let Some(mapping) = doc.as_mapping() {
                let removed = mapping.remove("author");
                assert!(removed.is_some());
            }
        }

        let expected = r#"name: my-app
version: 1.0"#;
        assert_eq!(yaml.to_string(), expected);
    }

    #[test]
    fn test_complex_operations_combined() {
        let original = r#"name: my-app
version: 1.0
author: Alice
year: 2023

features:
  - logging
  - auth"#;

        let yaml = YamlFile::from_str(original).unwrap();

        if let Some(doc) = yaml.document() {
            if let Some(mapping) = doc.as_mapping() {
                // Add new fields
                mapping.set("license", "MIT");
                mapping.set("published", true);
                mapping.set("downloads", 1000);

                // Remove a field
                mapping.remove("author");

                // Rename a field
                mapping.rename_key("version", "app_version");

                // Update existing field
                mapping.set("year", 2024);
            }
        }

        let expected = r#"name: my-app
app_version: 1.0
year: 2024

features:
  - logging
  - auth
license: MIT
published: true
downloads: 1000
"#;
        assert_eq!(yaml.to_string(), expected);
    }

    // ===== Nested structure tests =====

    #[test]
    fn test_mapping_get_nested_mapping() {
        let yaml = YamlFile::from_str("user:\n  name: Alice\n  age: 30").unwrap();
        let doc = yaml.document().unwrap();
        let mapping = doc.as_mapping().unwrap();

        let nested = mapping.get_mapping("user");
        assert!(nested.is_some());

        let nested = nested.unwrap();
        assert_eq!(
            nested
                .get("name")
                .and_then(|v| v.as_scalar().map(|s| s.as_string())),
            Some("Alice".to_string())
        );
        assert_eq!(nested.get("age").and_then(|v| v.to_i64()), Some(30));
    }

    #[test]
    fn test_mapping_get_nested_sequence() {
        let yaml = YamlFile::from_str("items:\n  - a\n  - b\n  - c").unwrap();
        let doc = yaml.document().unwrap();
        let mapping = doc.as_mapping().unwrap();

        let seq = mapping.get_sequence("items");
        assert!(seq.is_some());

        let seq = seq.unwrap();
        assert_eq!(seq.len(), 3);
        let values: Vec<String> = seq.values().map(|v| v.to_string()).collect();
        assert_eq!(values, vec!["a", "b", "c"]);
    }

    #[test]
    fn test_mapping_get_nonexistent_nested() {
        let yaml = YamlFile::from_str("name: Alice").unwrap();
        let doc = yaml.document().unwrap();
        let mapping = doc.as_mapping().unwrap();

        assert_eq!(mapping.get_mapping("user"), None);
        assert_eq!(mapping.get_sequence("items"), None);
    }

    // ===== Rename key tests =====

    #[test]
    fn test_rename_key_nonexistent() {
        let yaml = YamlFile::from_str("name: Alice").unwrap();
        let doc = yaml.document().unwrap();
        let mapping = doc.as_mapping().unwrap();

        let success = mapping.rename_key("age", "years");
        assert!(!success);

        let keys: Vec<String> = mapping.keys().map(|k| k.to_string()).collect();
        assert_eq!(keys, vec!["name"]);
    }

    #[test]
    fn test_rename_key_first_entry() {
        let yaml = YamlFile::from_str("name: Alice\nage: 30\ncity: NYC").unwrap();
        let doc = yaml.document().unwrap();
        let mapping = doc.as_mapping().unwrap();

        let success = mapping.rename_key("name", "username");
        assert!(success);

        let keys: Vec<String> = mapping.keys().map(|k| k.to_string()).collect();
        assert_eq!(keys, vec!["username", "age", "city"]);
    }

    #[test]
    fn test_rename_key_middle_entry() {
        let yaml = YamlFile::from_str("name: Alice\nage: 30\ncity: NYC").unwrap();
        let doc = yaml.document().unwrap();
        let mapping = doc.as_mapping().unwrap();

        let success = mapping.rename_key("age", "years");
        assert!(success);

        let keys: Vec<String> = mapping.keys().map(|k| k.to_string()).collect();
        assert_eq!(keys, vec!["name", "years", "city"]);
    }

    #[test]
    fn test_rename_key_last_entry() {
        let yaml = YamlFile::from_str("name: Alice\nage: 30\ncity: NYC").unwrap();
        let doc = yaml.document().unwrap();
        let mapping = doc.as_mapping().unwrap();

        let success = mapping.rename_key("city", "location");
        assert!(success);

        let keys: Vec<String> = mapping.keys().map(|k| k.to_string()).collect();
        assert_eq!(keys, vec!["name", "age", "location"]);
    }

    // ===== Multiple value type tests =====

    #[test]
    fn test_mapping_with_different_value_types() {
        let yaml = YamlFile::from_str("string: hello\nnumber: 42\nbool: true").unwrap();
        let doc = yaml.document().unwrap();
        let mapping = doc.as_mapping().unwrap();

        assert_eq!(
            mapping
                .get("string")
                .and_then(|v| v.as_scalar().map(|s| s.as_string())),
            Some("hello".to_string())
        );
        assert_eq!(mapping.get("number").and_then(|v| v.to_i64()), Some(42));
        assert_eq!(mapping.get("bool").and_then(|v| v.to_bool()), Some(true));
    }

    #[test]
    fn test_mapping_set_different_value_types() {
        let yaml = YamlFile::from_str("key: value").unwrap();
        let doc = yaml.document().unwrap();
        let mapping = doc.as_mapping().unwrap();

        mapping.set("number", 123);
        mapping.set("bool", false);
        mapping.set("text", "hello");

        assert_eq!(mapping.get("number").and_then(|v| v.to_i64()), Some(123));
        assert_eq!(mapping.get("bool").and_then(|v| v.to_bool()), Some(false));
        assert_eq!(
            mapping
                .get("text")
                .and_then(|v| v.as_scalar().map(|s| s.as_string())),
            Some("hello".to_string())
        );
    }

    // ===== Edge case tests =====

    #[test]
    fn test_empty_mapping_operations() {
        let yaml = YamlFile::from_str("{}").unwrap();
        let doc = yaml.document().unwrap();
        let mapping = doc.as_mapping().unwrap();

        assert!(mapping.is_empty());
        assert!(!mapping.contains_key("any"));
        assert_eq!(mapping.get("any"), None);
        assert!(mapping.remove("any").is_none());
        assert!(!mapping.rename_key("old", "new"));

        // Can still add to empty mapping
        mapping.set("first", "value");
        assert!(!mapping.is_empty());
        // In flow-style (JSON) context, strings are quoted
        assert_eq!(
            mapping.get("first").map(|v| v.to_string()),
            Some("\"value\"".to_string())
        );
    }

    #[test]
    fn test_mapping_remove_first_of_three() {
        let yaml = YamlFile::from_str("a: 1\nb: 2\nc: 3").unwrap();
        let doc = yaml.document().unwrap();
        let mapping = doc.as_mapping().unwrap();

        assert!(mapping.remove("a").is_some());

        let keys: Vec<String> = mapping.keys().map(|k| k.to_string()).collect();
        assert_eq!(keys, vec!["b", "c"]);
    }

    #[test]
    fn test_mapping_remove_middle_of_three() {
        let yaml = YamlFile::from_str("a: 1\nb: 2\nc: 3").unwrap();
        let doc = yaml.document().unwrap();
        let mapping = doc.as_mapping().unwrap();

        assert!(mapping.remove("b").is_some());

        let keys: Vec<String> = mapping.keys().map(|k| k.to_string()).collect();
        assert_eq!(keys, vec!["a", "c"]);
    }

    #[test]
    fn test_mapping_remove_last_of_three() {
        let yaml = YamlFile::from_str("a: 1\nb: 2\nc: 3").unwrap();
        let doc = yaml.document().unwrap();
        let mapping = doc.as_mapping().unwrap();

        assert!(mapping.remove("c").is_some());

        let keys: Vec<String> = mapping.keys().map(|k| k.to_string()).collect();
        assert_eq!(keys, vec!["a", "b"]);
    }

    // ===== Collection method tests =====

    #[test]
    fn test_mapping_len_empty() {
        let yaml = YamlFile::from_str("{}").unwrap();
        let doc = yaml.document().unwrap();
        let mapping = doc.as_mapping().unwrap();

        assert_eq!(mapping.len(), 0);
        assert!(mapping.is_empty());
    }

    #[test]
    fn test_mapping_len_single() {
        let yaml = YamlFile::from_str("name: Alice").unwrap();
        let doc = yaml.document().unwrap();
        let mapping = doc.as_mapping().unwrap();

        assert_eq!(mapping.len(), 1);
        assert!(!mapping.is_empty());
    }

    #[test]
    fn test_mapping_len_multiple() {
        let yaml = YamlFile::from_str("name: Alice\nage: 30\ncity: NYC").unwrap();
        let doc = yaml.document().unwrap();
        let mapping = doc.as_mapping().unwrap();

        assert_eq!(mapping.len(), 3);
        assert!(!mapping.is_empty());
    }

    #[test]
    fn test_mapping_len_after_adding() {
        let yaml = YamlFile::from_str("name: Alice").unwrap();
        let doc = yaml.document().unwrap();
        let mapping = doc.as_mapping().unwrap();

        assert_eq!(mapping.len(), 1);

        mapping.set("age", 30);
        assert_eq!(mapping.len(), 2);

        mapping.set("city", "NYC");
        assert_eq!(mapping.len(), 3);
    }

    #[test]
    fn test_mapping_len_after_removing() {
        let yaml = YamlFile::from_str("name: Alice\nage: 30\ncity: NYC").unwrap();
        let doc = yaml.document().unwrap();
        let mapping = doc.as_mapping().unwrap();

        assert_eq!(mapping.len(), 3);

        mapping.remove("age");
        assert_eq!(mapping.len(), 2);

        mapping.remove("city");
        assert_eq!(mapping.len(), 1);

        mapping.remove("name");
        assert_eq!(mapping.len(), 0);
        assert!(mapping.is_empty());
    }

    #[test]
    fn test_mapping_values_empty() {
        let yaml = YamlFile::from_str("{}").unwrap();
        let doc = yaml.document().unwrap();
        let mapping = doc.as_mapping().unwrap();

        let values: Vec<_> = mapping.values().collect();
        assert_eq!(values.len(), 0);
    }

    #[test]
    fn test_mapping_values_single() {
        let yaml = YamlFile::from_str("name: Alice").unwrap();
        let doc = yaml.document().unwrap();
        let mapping = doc.as_mapping().unwrap();

        let values: Vec<_> = mapping.values().collect();
        assert_eq!(values.len(), 1);
        assert_eq!(
            values[0].as_scalar().map(|s| s.as_string()),
            Some("Alice".to_string())
        );
    }

    #[test]
    fn test_mapping_values_multiple() {
        let yaml = YamlFile::from_str("name: Alice\nage: 30\nactive: true").unwrap();
        let doc = yaml.document().unwrap();
        let mapping = doc.as_mapping().unwrap();

        let values: Vec<_> = mapping.values().collect();
        assert_eq!(values.len(), 3);
        assert_eq!(
            values[0].as_scalar().map(|s| s.as_string()),
            Some("Alice".to_string())
        );
        assert_eq!(values[1].to_i64(), Some(30));
        assert_eq!(values[2].to_bool(), Some(true));
    }

    #[test]
    fn test_mapping_values_different_types() {
        let yaml = YamlFile::from_str("string: hello\nnumber: 42\nbool: false").unwrap();
        let doc = yaml.document().unwrap();
        let mapping = doc.as_mapping().unwrap();

        // Collect values and check types
        let values: Vec<_> = mapping.values().collect();
        assert_eq!(values.len(), 3);

        assert_eq!(
            values[0].as_scalar().map(|s| s.as_string()),
            Some("hello".to_string())
        );
        assert_eq!(values[1].to_i64(), Some(42));
        assert_eq!(values[2].to_bool(), Some(false));
    }

    #[test]
    fn test_mapping_iter_empty() {
        let yaml = YamlFile::from_str("{}").unwrap();
        let doc = yaml.document().unwrap();
        let mapping = doc.as_mapping().unwrap();

        let pairs: Vec<_> = mapping.iter().collect();
        assert_eq!(pairs.len(), 0);
    }

    #[test]
    fn test_mapping_iter_single() {
        let yaml = YamlFile::from_str("name: Alice").unwrap();
        let doc = yaml.document().unwrap();
        let mapping = doc.as_mapping().unwrap();

        let pairs: Vec<_> = mapping.iter().collect();
        assert_eq!(pairs.len(), 1);
        assert_eq!(
            pairs[0].0.as_scalar().map(|s| s.as_string()),
            Some("name".to_string())
        );
        assert_eq!(
            pairs[0].1.as_scalar().map(|s| s.as_string()),
            Some("Alice".to_string())
        );
    }

    #[test]
    fn test_mapping_iter_multiple() {
        let yaml = YamlFile::from_str("name: Alice\nage: 30\nactive: true").unwrap();
        let doc = yaml.document().unwrap();
        let mapping = doc.as_mapping().unwrap();

        let pairs: Vec<_> = mapping.iter().collect();

        assert_eq!(pairs.len(), 3);
        assert_eq!(
            pairs[0].0.as_scalar().map(|s| s.as_string()),
            Some("name".to_string())
        );
        assert_eq!(
            pairs[0].1.as_scalar().map(|s| s.as_string()),
            Some("Alice".to_string())
        );
        assert_eq!(
            pairs[1].0.as_scalar().map(|s| s.as_string()),
            Some("age".to_string())
        );
        assert_eq!(pairs[1].1.to_i64(), Some(30));
        assert_eq!(
            pairs[2].0.as_scalar().map(|s| s.as_string()),
            Some("active".to_string())
        );
        assert_eq!(pairs[2].1.to_bool(), Some(true));
    }

    #[test]
    fn test_mapping_iter_different_types() {
        let yaml = YamlFile::from_str("string: hello\nnumber: 42\nbool: false").unwrap();
        let doc = yaml.document().unwrap();
        let mapping = doc.as_mapping().unwrap();

        let pairs: Vec<_> = mapping.iter().collect();
        assert_eq!(pairs.len(), 3);

        // Check first pair (string: hello)
        assert_eq!(
            pairs[0].0.as_scalar().map(|s| s.as_string()),
            Some("string".to_string())
        );
        assert_eq!(
            pairs[0].1.as_scalar().map(|s| s.as_string()),
            Some("hello".to_string())
        );

        // Check second pair (number: 42)
        assert_eq!(
            pairs[1].0.as_scalar().map(|s| s.as_string()),
            Some("number".to_string())
        );
        assert_eq!(pairs[1].1.to_i64(), Some(42));

        // Check third pair (bool: false)
        assert_eq!(
            pairs[2].0.as_scalar().map(|s| s.as_string()),
            Some("bool".to_string())
        );
        assert_eq!(pairs[2].1.to_bool(), Some(false));
    }

    #[test]
    fn test_mapping_iter_preserves_order() {
        let yaml = YamlFile::from_str("z: 1\na: 2\nm: 3").unwrap();
        let doc = yaml.document().unwrap();
        let mapping = doc.as_mapping().unwrap();

        let pairs: Vec<_> = mapping.iter().collect();
        assert_eq!(pairs.len(), 3);
        assert_eq!(
            pairs[0].0.as_scalar().map(|s| s.as_string()),
            Some("z".to_string())
        );
        assert_eq!(
            pairs[1].0.as_scalar().map(|s| s.as_string()),
            Some("a".to_string())
        );
        assert_eq!(
            pairs[2].0.as_scalar().map(|s| s.as_string()),
            Some("m".to_string())
        );
    }

    #[test]
    fn test_mapping_clear_empty() {
        let yaml = YamlFile::from_str("{}").unwrap();
        let doc = yaml.document().unwrap();
        let mapping = doc.as_mapping().unwrap();

        assert_eq!(mapping.len(), 0);
        mapping.clear();
        assert_eq!(mapping.len(), 0);
    }

    #[test]
    fn test_mapping_clear_single() {
        let yaml = YamlFile::from_str("name: Alice").unwrap();
        let doc = yaml.document().unwrap();
        let mapping = doc.as_mapping().unwrap();

        assert_eq!(mapping.len(), 1);
        mapping.clear();
        assert_eq!(mapping.len(), 0);
        assert!(mapping.is_empty());

        let keys: Vec<String> = mapping.keys().map(|k| k.to_string()).collect();
        assert_eq!(keys, Vec::<String>::new());
    }

    #[test]
    fn test_mapping_clear_multiple() {
        let yaml = YamlFile::from_str("name: Alice\nage: 30\ncity: NYC").unwrap();
        let doc = yaml.document().unwrap();
        let mapping = doc.as_mapping().unwrap();

        assert_eq!(mapping.len(), 3);
        mapping.clear();
        assert_eq!(mapping.len(), 0);
        assert!(mapping.is_empty());

        let keys: Vec<String> = mapping.keys().map(|k| k.to_string()).collect();
        assert_eq!(keys, Vec::<String>::new());
    }

    #[test]
    fn test_mapping_clear_and_add() {
        let yaml = YamlFile::from_str("name: Alice\nage: 30").unwrap();
        let doc = yaml.document().unwrap();
        let mapping = doc.as_mapping().unwrap();

        assert_eq!(mapping.len(), 2);
        mapping.clear();
        assert_eq!(mapping.len(), 0);

        // Add new entries after clearing
        mapping.set("new_key", "new_value");
        assert_eq!(mapping.len(), 1);
        let value = mapping.get("new_key").unwrap();
        assert_eq!(
            value.as_scalar().map(|s| s.as_string()),
            Some("new_value".to_string())
        );
    }

    #[test]
    fn test_mapping_clear_large() {
        // Build a large mapping
        let yaml_str = (0..100)
            .map(|i| format!("key{}: value{}", i, i))
            .collect::<Vec<_>>()
            .join("\n");
        let yaml = YamlFile::from_str(&yaml_str).unwrap();
        let doc = yaml.document().unwrap();
        let mapping = doc.as_mapping().unwrap();

        assert_eq!(mapping.len(), 100);
        mapping.clear();
        assert_eq!(mapping.len(), 0);
        assert!(mapping.is_empty());
    }

    #[test]
    fn test_mapping_newline_handling_block_style() {
        // Block-style mappings should end with newline
        let yaml_with_newline = "key1: value1\nkey2: value2\n";
        let yaml = YamlFile::from_str(yaml_with_newline).unwrap();

        // Convert back to string - should preserve the newline
        let output = yaml.to_string();
        assert!(
            output.ends_with('\n'),
            "Block-style mapping should preserve trailing newline"
        );
        assert_eq!(output, yaml_with_newline);
    }

    #[test]
    fn test_mapping_newline_handling_no_trailing() {
        // Mapping without trailing newline
        let yaml_no_newline = "key: value";
        let yaml = YamlFile::from_str(yaml_no_newline).unwrap();

        // Convert back to string - should not add newline
        let output = yaml.to_string();
        assert!(
            !output.ends_with('\n'),
            "Mapping without trailing newline should not add one"
        );
        assert_eq!(output, yaml_no_newline);
    }

    #[test]
    fn test_mapping_newline_handling_flow_style() {
        // Flow-style mappings typically don't have trailing newlines
        let yaml_flow = "data: {key1: value1, key2: value2}";
        let yaml = YamlFile::from_str(yaml_flow).unwrap();

        // The flow mapping should serialize exactly as parsed
        let output = yaml.to_string();
        assert_eq!(output, yaml_flow);
    }

    #[test]
    fn test_mapping_set_preserves_newline_context() {
        // When setting values in a mapping, newline context should be preserved
        let yaml_str = "key1: value1\nkey2: value2\n";
        let yaml = YamlFile::from_str(yaml_str).unwrap();
        let doc = yaml.document().unwrap();
        let mapping = doc.as_mapping().unwrap();

        // Modify a value
        mapping.set("key1", "new_value");

        // Should still end with newline
        let output = yaml.to_string();
        assert!(
            output.ends_with('\n'),
            "Newline should be preserved after modification"
        );
    }
}
