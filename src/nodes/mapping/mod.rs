mod entry;
mod helpers;
mod movements;

pub(crate) use helpers::build_block_value_node;
pub(crate) use helpers::flow_form;
pub(crate) use helpers::{
    block_scalar_body_shift, drop_tagged_anchor, held_anchor, held_anchor_token, source_anchor_text,
};

use super::empty_collection::{self, render_empty};
use super::trivia::{self, Slot};
use super::{
    entry_key, entry_value, fresh_token, has_child_token, insert_children, replace_children, Lang,
    Sequence, SyntaxNode,
};
use crate::as_yaml::{AsYaml, YamlKind};
use crate::lex::SyntaxKind;
use crate::yaml::{dump_cst_to_string, ValueNode};
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
            // VALUE nodes wrap the actual content, return the content instead
            .and_then(|entry| entry.value().children().next())
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
    /// A block mapping that loses its last entry to `remove` or `clear` is
    /// written the same way, marker included.
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

    /// Wrap a flow pair in braces, so that it reads as one mapping however
    /// many entries it ends up with: `[a: 1, b: 2]` would be two.
    fn brace_flow_pair(&self) {
        if !is_flow_pair(&self.0) {
            return;
        }
        let end = self.0.children_with_tokens().count();
        insert_children(
            &self.0,
            end,
            vec![fresh_token(SyntaxKind::RIGHT_BRACE, "}").into()],
        );
        insert_children(
            &self.0,
            0,
            vec![fresh_token(SyntaxKind::LEFT_BRACE, "{").into()],
        );
    }

    /// Reorder fields according to the specified order.
    ///
    /// Fields not in the order list will appear after the ordered fields,
    /// in their original relative order.
    ///
    /// An alias that ends up before the node it refers to becomes a copy of
    /// it, anchor included, and the node an alias where it can.
    pub fn reorder_fields<I, K>(&self, order: I)
    where
        I: IntoIterator<Item = K>,
        K: crate::AsYaml,
    {
        let order_keys: Vec<K> = order.into_iter().collect();
        let entry_has_key = |entry: &SyntaxNode, key: &K| {
            entry
                .children()
                .find(|n| n.kind() == SyntaxKind::KEY)
                .is_some_and(|k| key_content_matches(&k, key))
        };
        // Take each named key's entry in the order given; anything the
        // caller did not name keeps its relative position at the end.
        let mut rest: Vec<SyntaxNode> = self.entries().map(|e| e.0).collect();
        let mut ordered: Vec<SyntaxNode> = order_keys
            .iter()
            .filter_map(|key| {
                let pos = rest.iter().position(|entry| entry_has_key(entry, key))?;
                Some(rest.remove(pos))
            })
            .collect();
        ordered.extend(rest);
        self.arrange_entries(&ordered);
    }
}

/// Is `node` a single pair in a flow sequence (`[a: 1]`), a flow mapping
/// written without braces?
pub(crate) fn is_flow_pair(node: &SyntaxNode) -> bool {
    node.kind() == SyntaxKind::MAPPING
        && !has_child_token(node, |k| k == SyntaxKind::LEFT_BRACE)
        && node
            .parent()
            .filter(|p| p.kind() == SyntaxKind::SEQUENCE_ENTRY)
            .and_then(|p| p.parent())
            .is_some_and(|seq| has_child_token(&seq, |k| k == SyntaxKind::LEFT_BRACKET))
}

/// Check whether the CST key node matches `key` using semantic equality.
///
/// An alias equals no node under `yaml_eq`, so an alias in a key is matched
/// by an alias of the same name instead, not by the node it refers to:
/// `*a` by `*a`, and `[*a]` by `[*a]`.
///
/// `key_node` may be a raw KEY wrapper.
pub(super) fn key_content_matches(key_node: &SyntaxNode, key: impl crate::AsYaml) -> bool {
    use crate::as_yaml::lookup_identity;
    let Some(own) = lookup_identity(key_node) else {
        return false;
    };
    let given = match key.value_node() {
        Some(node) => lookup_identity(&node),
        None => crate::as_yaml::identity_of(&key, None).ok().flatten(),
    };
    given.is_some_and(|given| given == own)
}

/// The anchor and tag on a KEY node, which sit in KEY itself or in the
/// TAGGED_NODE it holds.
fn key_annotations(key: &SyntaxNode) -> (Option<String>, Option<String>) {
    let tagged = key
        .children()
        .next()
        .filter(|n| n.kind() == SyntaxKind::TAGGED_NODE);
    let tokens = key
        .children_with_tokens()
        .chain(tagged.iter().flat_map(|n| n.children_with_tokens()))
        .filter_map(|c| c.into_token());
    let (mut anchor, mut tag) = (None, None);
    for token in tokens {
        match token.kind() {
            SyntaxKind::ANCHOR => anchor = anchor.or(Some(token.text().to_string())),
            SyntaxKind::TAG => tag = tag.or(Some(token.text().to_string())),
            _ => {}
        }
    }
    (anchor, tag)
}

/// `key` under `tag`, if it has no tag of its own and is still the same key
/// with it: `!!str` suits a string but not `1`, and a custom tag suits
/// nothing given untagged.
fn retagged_key(
    tag: &str,
    key: &impl crate::AsYaml,
    flow: Option<crate::as_yaml::FlowQuoting>,
) -> Option<crate::as_yaml::YamlNode> {
    if matches!(key.kind(), YamlKind::Tagged(_)) {
        return None;
    }
    let mut builder = GreenNodeBuilder::new();
    builder.start_node(SyntaxKind::TAGGED_NODE.into());
    builder.token(SyntaxKind::TAG.into(), tag);
    builder.token(SyntaxKind::WHITESPACE.into(), " ");
    super::build_key_content(&mut builder, key, 0, flow);
    builder.finish_node();
    let tagged = crate::as_yaml::YamlNode::from_syntax(SyntaxNode::new_root_mut(builder.finish()))?;
    crate::as_yaml::yaml_eq(&tagged, key).then_some(tagged)
}

impl Mapping {
    /// Check if this mapping is in flow style (JSON/inline format with `{}`).
    ///
    /// Returns `true` if the mapping uses flow style (e.g., `{key: value}`,
    /// or a single pair in a flow sequence such as `[key: value]`), `false`
    /// if it uses block style (e.g., `key: value`).
    pub fn is_flow_style(&self) -> bool {
        // Flow-style mappings start with LEFT_BRACE token. A block
        // placeholder is `{}` in the text but is destined to become block, so
        // it does not count as flow.
        (has_child_token(&self.0, |k| k == SyntaxKind::LEFT_BRACE)
            && !empty_collection::is_block_placeholder(&self.0))
            || is_flow_pair(&self.0)
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
    /// had none. The first alias to an anchor deeper in the old value
    /// becomes a copy of the anchored node: `b: {c: &y 1}` and `d: *y` give
    /// `b: 6` and `d: &y 1`.
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
        // Only block explicit keys count, whose `?` is a child of
        // MAPPING_ENTRY. In flow context the parser puts it inside KEY, and
        // new entries there are written implicit.
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
        // An existing entry with this key takes the value in place.
        let existing = self
            .0
            .children()
            .filter_map(MappingEntry::cast)
            .find(|entry| entry.key().is_some_and(|k| key_content_matches(&k, &key)));
        let Some(entry) = existing else {
            self.insert_new(Slot::End, key, value);
            return;
        };
        // set_value shifts the flow form to the entry's line, so its
        // continuation lines are given relative to that.
        let flow_context = self.is_flow_style();
        let flow = flow_context.then(|| crate::nodes::flow_quoting(&self.0));
        match flow.and_then(|q| flow_form(&value, 2, q)) {
            Some(flow) => entry.set_value(flow, flow_context),
            None => entry.set_value(value, flow_context),
        }
    }

    /// Add an entry for `key`, which is not in this mapping yet, at `slot`,
    /// written in the mapping's style.
    fn insert_new(&self, slot: Slot, key: impl crate::AsYaml, value: impl crate::AsYaml) {
        // A placeholder from `Mapping::new_pending_block` turns into a real
        // block mapping now that it has an entry to hold, before the entry
        // is laid out.
        empty_collection::convert_placeholder_to_block(&self.0);
        let flow = self
            .is_flow_style()
            .then(|| crate::nodes::flow_quoting(&self.0));
        // The column of the key tells the constructor how far to shift a
        // node-backed block value.
        let new_entry = MappingEntry::new_in(
            key,
            value,
            flow,
            self.uses_explicit_keys(),
            self.detect_indentation_level(),
        );
        self.attach(&new_entry.0, slot);
    }

    /// Splice `new_entry` into this mapping at `slot`.
    pub(super) fn attach(&self, new_entry: &SyntaxNode, slot: Slot) {
        if self.is_flow_style() {
            self.brace_flow_pair();
            trivia::attach_flow_entry(&self.0, slot, new_entry);
        } else {
            trivia::attach_entry(&self.0, slot, new_entry);
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

        // An existing key just takes the new value, wherever it sits.
        if self.find_entry_by_key(&key).is_some() {
            self.set_as_yaml(&key, &value);
            return;
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

            match (insert_after_node, insert_before_node) {
                (Some(after), _) => self.insert_new(Slot::After(after), &key, &value),
                (None, Some(before)) => self.insert_new(Slot::Before(before), &key, &value),
                // No ordered key is present yet, so order says nothing about
                // where this one goes.
                (None, None) => self.set_as_yaml(&key, &value),
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
        trivia::entry_column(&self.0)
    }

    /// Insert a key-value pair at a specific index (0-based), preserving formatting.
    ///
    /// If `new_key` already exists in the mapping, the existing entry is replaced
    /// with a newly built entry at the **same position** (the `index` argument is
    /// ignored). Surrounding whitespace in the file is preserved, but the entry
    /// node itself is rebuilt (comments attached to the old entry may be lost).
    /// An anchor on the old value stays on the new one, as with
    /// [`set`](Self::set).
    /// If `index` is out of bounds, the entry is appended at the end.
    pub fn insert_at_index_preserving(
        &self,
        index: usize,
        new_key: impl crate::AsYaml,
        new_value: impl crate::AsYaml,
    ) {
        match self.find_entry_by_key(&new_key) {
            Some(old) => {
                self.replace_entry(&old.0, new_key, new_value, true);
            }
            None => self.insert_new_entry_at(index, new_key, new_value),
        }
    }

    /// Replace `old`, an entry of this mapping, with a new entry built the
    /// way `set` builds one, in the same place, and return it. If
    /// `keep_anchor` is set, an anchor on the old value goes on the new one
    /// unless that is an alias, as with `set`; otherwise the old value goes
    /// as with `remove`.
    pub(super) fn replace_entry(
        &self,
        old: &SyntaxNode,
        key: impl crate::AsYaml,
        value: impl crate::AsYaml,
        keep_anchor: bool,
    ) -> SyntaxNode {
        let kept = MappingEntry::cast(old.clone())
            .map(|e| e.value())
            .filter(|_| keep_anchor && value.kind() != YamlKind::Alias)
            .and_then(|v| held_anchor_token(&v));
        let anchor = kept.as_ref().map(|t| t.text().to_string());
        let mut new = None;
        crate::nodes::kept_aliases::keeping_aliases(old, kept, || {
            // The entry is built without the value's anchor; putting the
            // value in again settles which anchor it gets, as in set_value.
            let entry = MappingEntry(self.splice_in_entry(old, key, &value));
            self.put_value(&entry, &value, anchor.as_deref());
            new = Some(entry.0.clone());
            Some(entry.value())
        });
        new.expect("the edit ran")
    }

    /// Replace the value of `entry`, an entry of this mapping, with `value`
    /// under `anchor`, as `set` does.
    pub(super) fn put_value(
        &self,
        entry: &MappingEntry,
        value: impl crate::AsYaml,
        anchor: Option<&str>,
    ) {
        let flow_context = self.is_flow_style();
        let flow = flow_context.then(|| crate::nodes::flow_quoting(&self.0));
        match flow.and_then(|q| flow_form(&value, 2, q)) {
            Some(flow) => entry.replace_value_under(flow, flow_context, anchor),
            None => entry.replace_value_under(&value, flow_context, anchor),
        }
    }

    /// The edit of [`replace_entry`](Self::replace_entry), without the
    /// kept anchor. Returns the new entry.
    fn splice_in_entry(
        &self,
        old: &SyntaxNode,
        key: impl crate::AsYaml,
        value: impl crate::AsYaml,
    ) -> SyntaxNode {
        let flow_context = self.is_flow_style();
        let new_entry = MappingEntry::new_in(
            key,
            value,
            flow_context.then(|| crate::nodes::flow_quoting(&self.0)),
            self.uses_explicit_keys(),
            self.detect_indentation_level(),
        )
        .0;
        if flow_context {
            // The separator to the next entry lives in the entry before it,
            // as does the line break ending its line.
            let mut separator: Vec<_> = old
                .children_with_tokens()
                .skip_while(|c| c.kind() != SyntaxKind::COMMA)
                .collect();
            if separator.is_empty() {
                separator.extend(
                    old.last_child_or_token()
                        .filter(|c| c.kind() == SyntaxKind::NEWLINE),
                );
            }
            for c in &separator {
                c.detach();
            }
            crate::nodes::append_children(&new_entry, separator);
        } else {
            // The new entry brings its own line break, unless the old one
            // ended the input without one.
            if !trivia::ends_with_line_break(old) {
                let last = new_entry
                    .last_child_or_token()
                    .filter(|c| c.kind() == SyntaxKind::NEWLINE);
                if let Some(last) = last {
                    last.detach();
                }
            }
        }
        let i = old.index();
        replace_children(&self.0, i..i + 1, vec![new_entry.clone().into()]);
        new_entry
    }

    /// Remove a key-value pair, returning the removed entry.
    ///
    /// Returns `Some(entry)` if the key existed and was removed, or `None` if
    /// the key was not found. The returned [`MappingEntry`] is detached from
    /// the tree; callers can inspect its key and value or re-insert it
    /// elsewhere.
    ///
    /// The first alias to an anchor in the removed entry becomes a copy of
    /// the anchored node, so it and later aliases keep their value.
    ///
    /// Mutates in place despite `&self` (see crate docs on interior mutability).
    pub fn remove(&self, key: impl crate::AsYaml) -> Option<MappingEntry> {
        let entry = self.entries().find(|e| e.key_matches(&key))?;
        self.remove_entry(entry.syntax());
        Some(entry)
    }

    /// Splice `removed`, an entry of this mapping, out of it with the
    /// trivia that belongs to it.
    fn remove_entry(&self, removed: &SyntaxNode) {
        // Without its pair, a flow pair would leave `[]`, an empty sequence.
        if is_flow_pair(&self.0) {
            self.brace_flow_pair();
        }
        crate::nodes::kept_aliases::keeping_aliases(removed, None, || {
            trivia::detach_entry(&self.0, removed);
            None
        });
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
        let entry = self.entries().filter(|e| e.key_matches(&key)).nth(n)?;
        self.remove_entry(entry.syntax());
        Some(entry)
    }

    /// Remove all key-value pairs from this mapping.
    ///
    /// Mutates in place despite `&self` (see crate docs on interior mutability).
    pub fn clear(&self) {
        let line_break = self
            .entries()
            .last()
            .and_then(|e| trivia::line_break(e.syntax()));
        // Entries are removed one by one, as keeping aliases resolving may
        // rewrite a later key.
        while let Some(entry) = self.entries().next() {
            self.remove_entry(entry.syntax());
        }
        render_empty(&self.0, line_break.as_deref());
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
    /// The key keeps its anchor, so aliases to it still resolve; an anchor
    /// on a `new_key` node is used only if the old key had none. The key's
    /// tag is kept if `new_key` has no tag of its own and still means the
    /// same with it: `!!str a` renamed to `"b"` gives `!!str b`, but renamed
    /// to `1` gives `1`, as `!!str 1` would be a string. The first alias to
    /// an anchor deeper in the old key becomes a copy of the anchored node.
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
        let Some(node) = self.find_entry_by_key(&old_key).map(|e| e.0) else {
            return false;
        };

        let renames_to_itself = entry_key(&node).is_some_and(|k| key_content_matches(&k, &new_key));
        if !renames_to_itself {
            self.remove_other_entries(&node, &new_key);
        }
        let old = entry_key(&node).expect("a mapping entry has a key");
        let kept = held_anchor_token(&old);
        crate::nodes::kept_aliases::keeping_aliases(&old, kept, || {
            Some(self.replace_key(&node, new_key, None))
        });
        true
    }

    /// Replace the key of `node`, an entry of this mapping, with `new_key`
    /// under `anchor`, and return the new KEY.
    pub(crate) fn replace_key(
        &self,
        node: &SyntaxNode,
        new_key: impl crate::AsYaml,
        anchor: Option<&str>,
    ) -> SyntaxNode {
        if let Some(key) = flow_form(&new_key, 2, crate::nodes::flow_quoting(&self.0)) {
            return self.replace_key(node, key, anchor);
        }
        let indent = self.detect_indentation_level();
        // A key spanning lines has to be explicit, with the `:` on a line
        // of its own.
        let make_explicit = !self.is_flow_style()
            && !has_child_token(node, |k| k == SyntaxKind::QUESTION)
            && super::key_spans_lines(&new_key);
        let old_key = entry_key(node).expect("a mapping entry has a key");
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(SyntaxKind::ROOT.into());
        if make_explicit {
            builder.token(SyntaxKind::QUESTION.into(), "?");
            builder.token(SyntaxKind::WHITESPACE.into(), " ");
        }
        let flow = self
            .is_flow_style()
            .then(|| crate::nodes::flow_quoting(&self.0));
        Self::rebuild_key(&mut builder, &old_key, &new_key, anchor, flow);
        if make_explicit {
            builder.token(SyntaxKind::NEWLINE.into(), "\n");
            if indent > 0 {
                builder.token(SyntaxKind::INDENT.into(), &" ".repeat(indent));
            }
        }
        builder.finish_node();
        let built = SyntaxNode::new_root_mut(builder.finish());
        let new_key = built
            .children()
            .find(|n| n.kind() == SyntaxKind::KEY)
            .expect("a KEY was just built");
        if anchor.is_some() {
            drop_tagged_anchor(&new_key);
        }

        // Whitespace between the key and the `:` would now start the `:`
        // line. Detached one by one, as a multi-child splice can skip some.
        if make_explicit {
            let stale: Vec<_> = old_key
                .siblings_with_tokens(rowan::Direction::Next)
                .skip(1)
                .map_while(|c| {
                    c.into_token()
                        .filter(|t| t.kind() == SyntaxKind::WHITESPACE)
                })
                .collect();
            for token in stale {
                token.detach();
            }
        }
        let old_ended_line = trivia::ends_with_line_break(&old_key);
        let i = old_key.index();
        replace_children(
            node,
            i..i + 1,
            built.children_with_tokens().collect::<Vec<_>>(),
        );
        if has_child_token(node, |k| k == SyntaxKind::QUESTION) {
            Self::settle_explicit_key_line(&new_key, old_ended_line);
        }
        if self.is_flow_style() {
            Self::separate_adjacent_value(&new_key);
        }
        crate::nodes::separate_key_from_colon(node);
        new_key
    }

    /// Put a space after the `:` following `key` if the value starts right
    /// after it and `key` is not JSON-like: only after a quoted scalar or a
    /// flow collection does `"a":b` read as a key and a value, while `a:b`
    /// is one plain scalar.
    fn separate_adjacent_value(key: &SyntaxNode) {
        let trivia = |k: SyntaxKind| {
            matches!(
                k,
                SyntaxKind::WHITESPACE
                    | SyntaxKind::INDENT
                    | SyntaxKind::NEWLINE
                    | SyntaxKind::COMMENT
            )
        };
        // The `:` may be on a later line, after a comment.
        let Some(colon) = key
            .siblings_with_tokens(rowan::Direction::Next)
            .skip(1)
            .find(|c| !trivia(c.kind()))
            .and_then(|c| c.into_token())
            .filter(|t| t.kind() == SyntaxKind::COLON)
        else {
            return;
        };
        let json_like = key
            .descendants_with_tokens()
            .filter_map(|c| c.into_token())
            .filter(|t| !t.text().is_empty() && !trivia(t.kind()))
            .last()
            .is_some_and(|t| {
                matches!(
                    t.kind(),
                    SyntaxKind::RIGHT_BRACKET | SyntaxKind::RIGHT_BRACE
                ) || (t.kind() == SyntaxKind::STRING && t.text().starts_with(['"', '\'']))
            });
        let value_adjacent = std::iter::successors(colon.next_token(), |t| t.next_token())
            .find_map(|t| t.text().chars().next())
            .is_some_and(|c| !c.is_whitespace() && !matches!(c, ',' | ']' | '}'));
        if !json_like && value_adjacent {
            let i = colon.index() + 1;
            insert_children(
                &colon.parent().expect("a COLON is in an entry"),
                i,
                vec![fresh_token(SyntaxKind::WHITESPACE, " ").into()],
            );
        }
    }

    /// Fix up the line of a block explicit key whose content was replaced.
    ///
    /// A block key (`? - a\n`) ends its own line, while the entry holds the
    /// line break after an inline one (`? a` NEWLINE). An empty key may
    /// follow the `?` directly, a non-empty one needs a space.
    fn settle_explicit_key_line(key: &SyntaxNode, old_ended_line: bool) {
        let entry = key.parent().expect("a KEY is in an entry");
        let before = key.prev_sibling_or_token();
        if before
            .as_ref()
            .is_some_and(|c| c.kind() == SyntaxKind::QUESTION)
            && !key.text().is_empty()
        {
            let i = key.index();
            insert_children(
                &entry,
                i,
                vec![fresh_token(SyntaxKind::WHITESPACE, " ").into()],
            );
        }
        // Without a `:` line, the key's line is the entry's.
        let has_colon = has_child_token(&entry, |k| k == SyntaxKind::COLON);
        let ends_line = trivia::ends_with_line_break(key);
        let newline_after = if has_colon {
            key.next_sibling_or_token()
        } else {
            entry.last_child_or_token()
        }
        .filter(|c| c.kind() == SyntaxKind::NEWLINE);
        if ends_line && !old_ended_line {
            if let Some(newline) = newline_after {
                newline.detach();
            }
        } else if old_ended_line && !ends_line {
            if has_colon {
                insert_children(
                    &entry,
                    key.index() + 1,
                    vec![fresh_token(SyntaxKind::NEWLINE, "\n").into()],
                );
            } else {
                trivia::terminate(&entry);
            }
        }
    }

    /// Remove every entry other than `keep` whose key matches `key`.
    fn remove_other_entries(&self, keep: &SyntaxNode, key: &impl crate::AsYaml) {
        while let Some(other) = self.entries().find(|e| e.0 != *keep && e.key_matches(key)) {
            self.remove_entry(other.syntax());
        }
    }

    /// Rebuild an entry's KEY node around `new_key`, keeping the layout
    /// tokens the old one carried.
    ///
    /// In a flow mapping the `?` sits inside KEY, so it carries over, as do
    /// any tokens after the key's content. The line break before block
    /// content (`?\n- a`) goes, as the new key is written inline, and the
    /// one after an empty key (`?\n}`) follows the new key.
    ///
    /// A comment before the old content stays on its line, or follows the
    /// new KEY if the content was on that line too.
    ///
    /// The old key's anchor stays; failing that the key takes `anchor`, or
    /// the anchor before a `new_key` node where it was copied from.
    fn rebuild_key(
        builder: &mut GreenNodeBuilder,
        old: &SyntaxNode,
        new_key: &impl crate::AsYaml,
        anchor: Option<&str>,
        flow: Option<crate::as_yaml::FlowQuoting>,
    ) {
        builder.start_node(SyntaxKind::KEY.into());
        let indent = old
            .parent()
            .map_or(0, |entry| crate::column::node_column(&entry));
        let leading: Vec<_> = old
            .children_with_tokens()
            .map_while(|c| c.into_token())
            .collect();
        let mut rest = &leading[..];
        if let Some((question, after)) = rest
            .split_first()
            .filter(|(t, _)| t.kind() == SyntaxKind::QUESTION)
        {
            builder.token(question.kind().into(), question.text());
            let ws = after
                .iter()
                .take_while(|t| t.kind() == SyntaxKind::WHITESPACE)
                .count();
            for t in &after[..ws] {
                builder.token(t.kind().into(), t.text());
            }
            if ws == 0 {
                builder.token(SyntaxKind::WHITESPACE.into(), " ");
            }
            rest = &after[ws..];
        }
        // A comment on the `?` line stays there if the content starts on a
        // later line, and otherwise goes after the new key.
        let comment = rest.iter().position(|t| t.kind() == SyntaxKind::COMMENT);
        let comment_lines = comment
            .filter(|&i| rest[i..].iter().any(|t| t.kind() == SyntaxKind::NEWLINE))
            .map(|i| &rest[i..]);
        let comment_after = comment
            .filter(|_| comment_lines.is_none())
            .map(|i| rest[i].text().to_string());
        let (old_anchor, tag) = key_annotations(old);
        let anchor = old_anchor
            .or_else(|| anchor.map(str::to_string))
            .or_else(|| new_key.value_node().and_then(|n| source_anchor_text(&n)));
        if let Some(anchor) = anchor {
            builder.token(SyntaxKind::ANCHOR.into(), &anchor);
            builder.token(SyntaxKind::WHITESPACE.into(), " ");
        }
        // On a line of its own, only a block sequence may start at the
        // column of the `?`; anything else has to be indented past it.
        let mut lines = comment_lines.unwrap_or_default();
        let line_indent = match lines.last() {
            Some(t) if t.kind() == SyntaxKind::INDENT => {
                lines = &lines[..lines.len() - 1];
                Some(t.text())
            }
            _ => None,
        };
        let block_sequence =
            new_key.kind() == crate::as_yaml::YamlKind::Sequence && !new_key.is_inline();
        let deeper = " ".repeat(indent + 2);
        let line_indent = match line_indent {
            Some(text) if block_sequence || text.len() > indent => Some(text),
            _ if comment_lines.is_some() && !block_sequence => Some(deeper.as_str()),
            other => other,
        };
        for t in lines {
            builder.token(t.kind().into(), t.text());
        }
        if let Some(text) = line_indent {
            builder.token(SyntaxKind::INDENT.into(), text);
        }
        match tag.and_then(|tag| retagged_key(&tag, new_key, flow)) {
            Some(tagged) => super::build_key_content(builder, &tagged, indent, flow),
            None => super::build_key_content(builder, new_key, indent, flow),
        }
        for trailing in old
            .children_with_tokens()
            .skip_while(|c| c.as_token().is_some())
            .skip(1)
            .filter_map(|c| c.into_token())
        {
            builder.token(trailing.kind().into(), trailing.text());
        }
        builder.finish_node();
        if let Some(comment) = comment_after {
            builder.token(SyntaxKind::WHITESPACE.into(), "  ");
            builder.token(SyntaxKind::COMMENT.into(), &comment);
        }
        // The line break after an empty key now follows the new one.
        let old_empty = old.children().next().is_some_and(|n| n.text().is_empty());
        if old_empty && comment.is_none() {
            for t in rest
                .iter()
                .filter(|t| matches!(t.kind(), SyntaxKind::NEWLINE | SyntaxKind::INDENT))
            {
                builder.token(t.kind().into(), t.text());
            }
        }
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
        self.insert_around(after_key, key, value, true)
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
        self.insert_around(before_key, key, value, false)
    }

    /// Shared body of [`insert_after`](Self::insert_after) and
    /// [`insert_before`](Self::insert_before): an existing `key` is updated
    /// where it is, a new one goes next to `ref_key`.
    fn insert_around(
        &self,
        ref_key: impl crate::AsYaml,
        key: impl crate::AsYaml,
        value: impl crate::AsYaml,
        after: bool,
    ) -> bool {
        if self.find_entry_by_key(&key).is_some() {
            self.set_as_yaml(&key, &value);
            return self.find_entry_by_key(&ref_key).is_some();
        }
        self.move_around(&ref_key, &key, &value, after)
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
        let slot = self
            .entries()
            .nth(index)
            .map_or(Slot::End, |e| Slot::Before(e.0));
        self.insert_new(slot, key, value);
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
        let is_flow = self.is_flow_style();
        let mut delta =
            indent as isize - crate::as_yaml::source_reindent_base(&self.0, is_flow) as isize;
        if is_flow {
            delta = crate::as_yaml::clear_parent_column(&self.0, indent, delta);
        }
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
