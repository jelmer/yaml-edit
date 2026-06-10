//! Anchor and alias resolution for semantic YAML operations
//!
//! This module provides functionality to resolve YAML anchors and aliases,
//! enabling semantic lookups while preserving the lossless nature of the
//! syntax tree.
//!
//! # Design Philosophy
//!
//! - **Opt-in**: Resolution is explicit via `get_resolved()` methods
//! - **Preserves structure**: Original syntax tree remains unchanged
//! - **Lazy evaluation**: Anchors are resolved on-demand, not eagerly
//! - **Error handling**: Undefined aliases return None instead of panicking
//!
//! # Examples
//!
//! ```
//! use yaml_edit::Document;
//! use yaml_edit::anchor_resolution::DocumentResolvedExt;
//! use std::str::FromStr;
//!
//! let yaml = r#"
//! defaults: &defaults
//!   timeout: 30
//!   retries: 3
//!
//! production:
//!   <<: *defaults
//!   host: prod.example.com
//! "#;
//!
//! let doc = Document::from_str(yaml).unwrap();
//!
//! // Regular get() doesn't resolve - returns the alias syntax
//! // get_resolved() expands aliases and merge keys
//! if let Some(resolved_value) = doc.get_resolved("production") {
//!     if let Some(prod) = resolved_value.as_mapping() {
//!         // This will find 'timeout: 30' from the merged defaults
//!         if let Some(timeout) = prod.get("timeout") {
//!             assert_eq!(timeout.to_i64(), Some(30));
//!         }
//!         // This will find the overridden value
//!         if let Some(host) = prod.get("host") {
//!             assert_eq!(host.as_scalar().map(|s| s.to_string()).as_deref(), Some("prod.example.com"));
//!         }
//!     }
//! }
//! ```

use crate::as_yaml::AsYaml;
use crate::lex::SyntaxKind;
use crate::value::YamlValue;
use crate::yaml::SyntaxNode;
use std::collections::HashMap;

/// A registry of anchors defined in a YAML document
#[derive(Debug, Clone)]
pub struct AnchorRegistry {
    /// Map from anchor name to the syntax node it refers to
    anchors: HashMap<String, SyntaxNode>,
}

impl AnchorRegistry {
    /// Create a new empty anchor registry
    pub fn new() -> Self {
        Self {
            anchors: HashMap::new(),
        }
    }

    /// Build a registry from a Document
    pub fn from_document(doc: &crate::yaml::Document) -> Self {
        if let Some(node) = doc.as_node() {
            Self::from_tree(node)
        } else {
            Self::new()
        }
    }

    /// Build a registry by scanning a syntax tree
    pub fn from_tree(root: &SyntaxNode) -> Self {
        let mut registry = Self::new();
        registry.collect_anchors_from_tree(root);
        registry
    }

    /// Recursively collect all anchors from a syntax tree
    fn collect_anchors_from_tree(&mut self, node: &SyntaxNode) {
        // Look for ANCHOR tokens in this node
        for child in node.children_with_tokens() {
            if let Some(token) = child.as_token() {
                if token.kind() == SyntaxKind::ANCHOR {
                    // Extract anchor name (remove '&' prefix)
                    let text = token.text();
                    if let Some(name) = text.strip_prefix('&') {
                        // Find the associated value node
                        if let Some(value_node) = self.find_anchored_value(node) {
                            self.anchors.insert(name.to_string(), value_node);
                        }
                    }
                }
            } else if let Some(child_node) = child.as_node() {
                // Recurse into child nodes
                self.collect_anchors_from_tree(child_node);
            }
        }
    }

    /// Find the value node that an anchor refers to (low-level helper)
    fn find_anchored_value(&self, node: &SyntaxNode) -> Option<SyntaxNode> {
        // The anchored value is typically a sibling MAPPING/SEQUENCE/SCALAR node
        for child in node.children() {
            if matches!(
                child.kind(),
                SyntaxKind::VALUE
                    | SyntaxKind::SCALAR
                    | SyntaxKind::MAPPING
                    | SyntaxKind::SEQUENCE
                    | SyntaxKind::TAGGED_NODE
            ) {
                return Some(child);
            }
        }
        None
    }

    /// Look up an anchor by name
    pub fn resolve(&self, name: &str) -> Option<&SyntaxNode> {
        self.anchors.get(name)
    }

    /// Check if an anchor is defined
    pub fn contains(&self, name: &str) -> bool {
        self.anchors.contains_key(name)
    }

    /// Get all anchor names
    pub fn anchor_names(&self) -> impl Iterator<Item = &str> {
        self.anchors.keys().map(|s| s.as_str())
    }
}

impl Default for AnchorRegistry {
    fn default() -> Self {
        Self::new()
    }
}

/// Extension trait for Document to support resolved lookups
pub trait DocumentResolvedExt {
    /// Get a value with anchor/alias resolution
    ///
    /// This method resolves aliases (*alias) to their anchored values (&anchor),
    /// and supports merge keys (<<) for combining mappings.
    ///
    /// # Examples
    ///
    /// ```
    /// use yaml_edit::Document;
    /// use yaml_edit::anchor_resolution::DocumentResolvedExt;
    /// use std::str::FromStr;
    ///
    /// let yaml = r#"
    /// config: &cfg
    ///   port: 8080
    /// server: *cfg
    /// "#;
    /// let doc = Document::from_str(yaml).unwrap();
    ///
    /// // Get with resolution - expands the *cfg alias
    /// if let Some(resolved_value) = doc.get_resolved("server") {
    ///     if let Some(server) = resolved_value.as_mapping() {
    ///         if let Some(port) = server.get("port") {
    ///             assert_eq!(port.to_i64(), Some(8080));
    ///         }
    ///     }
    /// }
    /// ```
    fn get_resolved(&self, key: impl crate::AsYaml) -> Option<YamlValue>;

    /// Build the anchor registry for this document
    fn build_anchor_registry(&self) -> AnchorRegistry;
}

impl DocumentResolvedExt for crate::yaml::Document {
    fn get_resolved(&self, key: impl crate::AsYaml) -> Option<YamlValue> {
        use rowan::ast::AstNode;

        // Build the anchor registry from this document
        let registry = self.build_anchor_registry();

        // Get the value from the document (assuming it's a mapping)
        let mapping = self.as_mapping()?;
        let value = mapping
            .get_node(&key)
            .and_then(crate::value::YamlValue::cast)?;

        // Check if we need to resolve an alias
        // Get the syntax node to check for REFERENCE tokens
        if let Some(node) = mapping.get_node(&key) {
            if let Some(alias_name) = find_alias_reference(&node) {
                // Resolve the alias
                if let Some(resolved_node) = registry.resolve(&alias_name) {
                    // Convert the resolved node back to YamlValue
                    return YamlValue::cast(resolved_node.clone());
                }
            }
        }

        // Check for merge keys — must use CST Mapping, not YamlValue::Mapping (BTreeMap)
        if let Some(node) = mapping.get_node(&key) {
            if let Some(result_mapping) = crate::yaml::Mapping::cast(node) {
                if has_merge_keys(&result_mapping) {
                    return Some(YamlValue::Mapping(apply_merge_keys(
                        &result_mapping,
                        &registry,
                    )));
                }
            }
        }

        Some(value)
    }

    fn build_anchor_registry(&self) -> AnchorRegistry {
        AnchorRegistry::from_document(self)
    }
}

/// Find if a node is an alias reference (contains a REFERENCE token)
fn find_alias_reference(node: &SyntaxNode) -> Option<String> {
    // Check this node and its children for REFERENCE tokens
    for child in node.children_with_tokens() {
        if let Some(token) = child.as_token() {
            if token.kind() == SyntaxKind::REFERENCE {
                let text = token.text();
                // Remove the '*' prefix
                return text.strip_prefix('*').map(|s| s.to_string());
            }
        }
    }

    // Check parent nodes too
    if let Some(parent) = node.parent() {
        for child in parent.children_with_tokens() {
            if let Some(token) = child.as_token() {
                if token.kind() == SyntaxKind::REFERENCE {
                    let text = token.text();
                    return text.strip_prefix('*').map(|s| s.to_string());
                }
            }
        }
    }

    None
}

/// Extract a string value from a YamlNode scalar.
fn node_as_string(node: &crate::as_yaml::YamlNode) -> Option<String> {
    node.as_scalar().map(|s| s.as_string())
}

/// Check if a mapping contains merge keys (<<)
fn has_merge_keys(mapping: &crate::yaml::Mapping) -> bool {
    for (key, _) in mapping.iter() {
        if node_as_string(&key).as_deref() == Some("<<") {
            return true;
        }
    }
    false
}

/// Apply merge keys to create a new merged mapping (returns BTreeMap for YamlValue::Mapping)
fn apply_merge_keys(
    mapping: &crate::yaml::Mapping,
    registry: &AnchorRegistry,
) -> std::collections::BTreeMap<String, YamlValue> {
    use crate::as_yaml::YamlNode;
    use std::collections::BTreeMap;

    let mut merged_pairs: BTreeMap<String, YamlValue> = BTreeMap::new();

    // First pass: process merge keys and collect merged values
    for (key, value) in mapping.iter() {
        let Some(key_str) = node_as_string(&key) else {
            continue;
        };
        if key_str != "<<" {
            continue;
        }

        // Handle both single alias and sequence of aliases
        match &value {
            // Single alias: <<: *alias
            YamlNode::Scalar(_) => {
                let Some(alias_text) = node_as_string(&value) else {
                    continue;
                };
                let Some(alias_name) = alias_text.strip_prefix('*') else {
                    continue;
                };

                merge_from_alias(&mut merged_pairs, alias_name, registry);
            }
            // Multiple aliases: <<: [*alias1, *alias2]
            YamlNode::Sequence(seq) => {
                // Process aliases in order - later aliases override earlier ones
                for alias_node in seq.values() {
                    let Some(alias_text) = node_as_string(&alias_node) else {
                        continue;
                    };
                    let Some(alias_name) = alias_text.strip_prefix('*') else {
                        continue;
                    };

                    merge_from_alias(&mut merged_pairs, alias_name, registry);
                }
            }
            _ => continue,
        }
    }

    // Second pass: add direct keys (override merged keys)
    for (key, value) in mapping.iter() {
        let Some(key_str) = node_as_string(&key) else {
            continue;
        };
        if key_str == "<<" {
            continue;
        }
        // Convert YamlNode back to YamlValue for storage
        if let Some(yaml_value) = YamlValue::cast(value.syntax().clone()) {
            merged_pairs.insert(key_str, yaml_value);
        }
    }

    merged_pairs
}

/// Helper function to merge keys from a single alias
fn merge_from_alias(
    merged_pairs: &mut std::collections::BTreeMap<String, YamlValue>,
    alias_name: &str,
    registry: &AnchorRegistry,
) {
    use rowan::ast::AstNode;

    let Some(resolved_node) = registry.resolve(alias_name) else {
        return;
    };

    // Get the resolved mapping and merge its keys
    if let Some(resolved_mapping) = crate::yaml::Mapping::cast(resolved_node.clone()) {
        for (src_key, src_value) in resolved_mapping.iter() {
            let Some(k_str) = node_as_string(&src_key) else {
                continue;
            };
            // Convert YamlNode back to YamlValue for storage; insert or override
            if let Some(yaml_value) = YamlValue::cast(src_value.syntax().clone()) {
                merged_pairs.insert(k_str, yaml_value);
            }
        }
    }
}

/// A view over a [`Mapping`](crate::yaml::Mapping) that transparently expands
/// alias references (`*name`) and merge keys (`<<:`).
///
/// `MergedMapping` is a lightweight, zero-copy view: it borrows the underlying
/// mapping and an [`AnchorRegistry`]. The underlying syntax tree is not
/// modified — every lookup walks the CST on demand and resolves aliases via
/// the registry.
///
/// Direct keys in the base mapping always shadow keys contributed by merge
/// sources, matching the YAML 1.1 merge-key semantics. When a merge value is
/// a sequence of aliases (`<<: [*a, *b]`), earlier aliases take precedence
/// over later ones, again per the YAML 1.1 spec.
///
/// Returned values are [`YamlNode`](crate::as_yaml::YamlNode)s backed by real
/// CST nodes, so they preserve the original formatting and quoting style.
///
/// # Examples
///
/// ```
/// use yaml_edit::{Document, anchor_resolution::{DocumentResolvedExt, MappingMergedExt}};
/// use std::str::FromStr;
///
/// let yaml = r#"
/// defaults: &defaults
///   timeout: 30
///   retries: 3
///
/// production:
///   <<: *defaults
///   host: prod.example.com
///   timeout: 60
/// "#;
///
/// let doc = Document::from_str(yaml).unwrap();
/// let root = doc.as_mapping().unwrap();
/// let registry = doc.build_anchor_registry();
///
/// let prod = root.get_mapping("production").unwrap();
/// let merged = prod.merged(&registry);
///
/// // Direct key wins over merged value.
/// assert_eq!(merged.get("timeout").unwrap().to_i64(), Some(60));
/// // Merged key from defaults is visible.
/// assert_eq!(merged.get("retries").unwrap().to_i64(), Some(3));
/// // Direct-only key is visible.
/// assert!(merged.get("host").is_some());
/// // `<<` itself is hidden.
/// assert!(merged.get("<<").is_none());
/// ```
#[derive(Clone)]
pub struct MergedMapping<'a> {
    base: crate::yaml::Mapping,
    registry: &'a AnchorRegistry,
}

impl<'a> MergedMapping<'a> {
    /// Create a new merged view over `base`, resolving aliases against `registry`.
    pub fn new(base: crate::yaml::Mapping, registry: &'a AnchorRegistry) -> Self {
        Self { base, registry }
    }

    /// Return the underlying (un-merged) [`Mapping`](crate::yaml::Mapping).
    ///
    /// Useful when you want to mutate the original mapping or read its raw
    /// (non-resolved) contents.
    pub fn base(&self) -> &crate::yaml::Mapping {
        &self.base
    }

    /// Return the [`AnchorRegistry`] this view resolves against.
    pub fn registry(&self) -> &AnchorRegistry {
        self.registry
    }

    /// Get the value associated with `key`, resolving aliases and merge keys.
    ///
    /// Direct entries in the base mapping take precedence over keys
    /// contributed by `<<:` merge sources. The synthetic `<<` key itself is
    /// hidden — looking it up returns `None`.
    ///
    /// If the matched value is an alias (`*name`), the resolved target node
    /// is returned. If no such anchor is defined, the alias is left
    /// unresolved and returned as-is.
    pub fn get(&self, key: impl crate::AsYaml) -> Option<crate::as_yaml::YamlNode> {
        if is_merge_key(&key) {
            return None;
        }

        if let Some(node) = self.base.get(&key) {
            return Some(resolve_alias_node(node, self.registry));
        }

        for source in merge_sources(&self.base, self.registry) {
            if let Some(node) = source.get(&key) {
                return Some(resolve_alias_node(node, self.registry));
            }
        }
        None
    }

    /// Returns `true` if a value would be returned by [`get`](Self::get) for
    /// `key`.
    pub fn contains_key(&self, key: impl crate::AsYaml) -> bool {
        self.get(key).is_some()
    }

    /// Iterate over `(key, value)` pairs in the merged view.
    ///
    /// Direct entries appear first, in their original document order.
    /// Merged-in entries follow, with duplicates (and any direct-key matches
    /// already yielded) filtered out. The `<<` merge key itself is never
    /// yielded.
    pub fn iter(
        &self,
    ) -> impl Iterator<Item = (crate::as_yaml::YamlNode, crate::as_yaml::YamlNode)> + '_ {
        let mut seen: Vec<String> = Vec::new();
        let mut out: Vec<(crate::as_yaml::YamlNode, crate::as_yaml::YamlNode)> = Vec::new();

        for (key, value) in self.base.iter() {
            let Some(key_str) = node_as_string(&key) else {
                continue;
            };
            if key_str == "<<" {
                continue;
            }
            seen.push(key_str);
            out.push((key, resolve_alias_node(value, self.registry)));
        }

        for source in merge_sources(&self.base, self.registry) {
            for (key, value) in source.iter() {
                let Some(key_str) = node_as_string(&key) else {
                    continue;
                };
                if key_str == "<<" || seen.contains(&key_str) {
                    continue;
                }
                seen.push(key_str);
                out.push((key, resolve_alias_node(value, self.registry)));
            }
        }

        out.into_iter()
    }

    /// Iterate over the keys of the merged view.
    pub fn keys(&self) -> impl Iterator<Item = crate::as_yaml::YamlNode> + '_ {
        self.iter().map(|(k, _)| k)
    }

    /// Iterate over the values of the merged view.
    pub fn values(&self) -> impl Iterator<Item = crate::as_yaml::YamlNode> + '_ {
        self.iter().map(|(_, v)| v)
    }

    /// Number of distinct keys visible through the merged view.
    pub fn len(&self) -> usize {
        self.iter().count()
    }

    /// Returns `true` if the merged view has no entries.
    pub fn is_empty(&self) -> bool {
        self.iter().next().is_none()
    }

    /// Get a nested mapping by `key` and return it as another `MergedMapping`
    /// that shares the same registry.
    ///
    /// Returns `None` if the key does not exist or the value is not a
    /// mapping.
    pub fn get_merged(&self, key: impl crate::AsYaml) -> Option<MergedMapping<'a>> {
        let node = self.get(key)?;
        let mapping = node.as_mapping().cloned()?;
        Some(MergedMapping::new(mapping, self.registry))
    }
}

impl std::fmt::Debug for MergedMapping<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("MergedMapping")
            .field("len", &self.len())
            .finish()
    }
}

/// Returns `true` if `key` is the YAML merge key `<<`.
fn is_merge_key(key: &impl crate::AsYaml) -> bool {
    use crate::as_yaml::YamlNode;

    if let Some(node) = key.as_node() {
        if let Some(yaml) = YamlNode::from_syntax_peeled(node.clone()) {
            return node_as_string(&yaml).as_deref() == Some("<<");
        }
    }

    // Fallback: build a green node and stringify it. This handles raw `&str`
    // and similar input types.
    let mut builder = rowan::GreenNodeBuilder::new();
    builder.start_node(crate::lex::SyntaxKind::ROOT.into());
    key.build_content(&mut builder, 0, true);
    builder.finish_node();
    let root = crate::yaml::SyntaxNode::new_root(builder.finish());
    root.text().to_string().trim() == "<<"
}

/// Walk the `<<:` entries of `base` and return the resolved source mappings,
/// in precedence order (highest first).
fn merge_sources(
    base: &crate::yaml::Mapping,
    registry: &AnchorRegistry,
) -> Vec<crate::yaml::Mapping> {
    use crate::as_yaml::YamlNode;
    use rowan::ast::AstNode;

    let mut sources = Vec::new();

    for (key, value) in base.iter() {
        if node_as_string(&key).as_deref() != Some("<<") {
            continue;
        }

        match &value {
            YamlNode::Scalar(_) => {
                if let Some(alias_text) = node_as_string(&value) {
                    if let Some(alias_name) = alias_text.strip_prefix('*') {
                        if let Some(target) = registry.resolve(alias_name) {
                            if let Some(m) = crate::yaml::Mapping::cast(target.clone()) {
                                sources.push(m);
                            }
                        }
                    }
                }
            }
            YamlNode::Sequence(seq) => {
                for item in seq.values() {
                    let alias_name = match &item {
                        YamlNode::Alias(a) => a.name(),
                        YamlNode::Scalar(_) => {
                            let Some(text) = node_as_string(&item) else {
                                continue;
                            };
                            let Some(name) = text.strip_prefix('*') else {
                                continue;
                            };
                            name.to_string()
                        }
                        _ => continue,
                    };
                    let Some(target) = registry.resolve(&alias_name) else {
                        continue;
                    };
                    if let Some(m) = crate::yaml::Mapping::cast(target.clone()) {
                        sources.push(m);
                    }
                }
            }
            YamlNode::Alias(alias) => {
                if let Some(target) = registry.resolve(&alias.name()) {
                    if let Some(m) = crate::yaml::Mapping::cast(target.clone()) {
                        sources.push(m);
                    }
                }
            }
            _ => continue,
        }
    }

    sources
}

/// If `node` is an alias, resolve it via `registry`; otherwise return it as-is.
fn resolve_alias_node(
    node: crate::as_yaml::YamlNode,
    registry: &AnchorRegistry,
) -> crate::as_yaml::YamlNode {
    use crate::as_yaml::YamlNode;

    if let YamlNode::Alias(alias) = &node {
        if let Some(target) = registry.resolve(&alias.name()) {
            if let Some(resolved) = YamlNode::from_syntax_peeled(target.clone()) {
                return resolved;
            }
        }
    }
    node
}

/// Extension trait that adds [`merged`](Self::merged) to [`Mapping`](crate::yaml::Mapping).
pub trait MappingMergedExt {
    /// Return a [`MergedMapping`] view over this mapping that resolves
    /// aliases and merge keys against `registry`.
    ///
    /// The view is read-only and does not modify the underlying CST.
    fn merged<'a>(&self, registry: &'a AnchorRegistry) -> MergedMapping<'a>;
}

impl MappingMergedExt for crate::yaml::Mapping {
    fn merged<'a>(&self, registry: &'a AnchorRegistry) -> MergedMapping<'a> {
        MergedMapping::new(self.clone(), registry)
    }
}

/// Extension trait that adds [`merged`](Self::merged) to [`Document`](crate::yaml::Document).
pub trait DocumentMergedExt {
    /// Return a [`MergedMapping`] view over the document's root mapping.
    ///
    /// The anchor registry is built from this document automatically.
    /// Returns `None` if the document's root is not a mapping.
    ///
    /// Because the registry is owned by the returned view, this method
    /// returns an owned [`MergedView`] container rather than a borrowing
    /// `MergedMapping<'_>`. Use [`MergedView::as_mapping`] to obtain the
    /// borrowing view for lookups.
    fn merged(&self) -> Option<MergedView>;
}

impl DocumentMergedExt for crate::yaml::Document {
    fn merged(&self) -> Option<MergedView> {
        let mapping = self.as_mapping()?;
        let registry = self.build_anchor_registry();
        Some(MergedView { mapping, registry })
    }
}

/// An owning container for a [`MergedMapping`] view of a document's root.
///
/// Holds both the root [`Mapping`](crate::yaml::Mapping) and the
/// [`AnchorRegistry`] built from the document, so the view can be returned
/// from a function without lifetime gymnastics. Call
/// [`as_mapping`](Self::as_mapping) (or use the convenience methods
/// directly) to query the view.
pub struct MergedView {
    mapping: crate::yaml::Mapping,
    registry: AnchorRegistry,
}

impl MergedView {
    /// Borrow this view as a [`MergedMapping`] for lookups.
    pub fn as_mapping(&self) -> MergedMapping<'_> {
        MergedMapping::new(self.mapping.clone(), &self.registry)
    }

    /// Get a value through the merged view.
    pub fn get(&self, key: impl crate::AsYaml) -> Option<crate::as_yaml::YamlNode> {
        self.as_mapping().get(key)
    }

    /// Check whether `key` is reachable through the merged view.
    pub fn contains_key(&self, key: impl crate::AsYaml) -> bool {
        self.as_mapping().contains_key(key)
    }

    /// Return the registry built from the document.
    pub fn registry(&self) -> &AnchorRegistry {
        &self.registry
    }
}

impl std::fmt::Debug for MergedView {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("MergedView").finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Document;
    use std::str::FromStr;

    fn doc(text: &str) -> Document {
        Document::from_str(text).expect("parse")
    }

    #[test]
    fn merged_basic_merge_key() {
        let yaml = "\
defaults: &d
  timeout: 30
  retries: 3

prod:
  <<: *d
  host: prod.example.com
";
        let d = doc(yaml);
        let root = d.as_mapping().unwrap();
        let reg = d.build_anchor_registry();
        let prod = root.get_mapping("prod").unwrap();
        let m = prod.merged(&reg);

        assert_eq!(m.get("timeout").unwrap().to_i64(), Some(30));
        assert_eq!(m.get("retries").unwrap().to_i64(), Some(3));
        assert_eq!(
            m.get("host").unwrap().as_scalar().unwrap().as_string(),
            "prod.example.com"
        );
    }

    #[test]
    fn merged_direct_key_wins() {
        let yaml = "\
defaults: &d
  timeout: 30

prod:
  <<: *d
  timeout: 60
";
        let d = doc(yaml);
        let reg = d.build_anchor_registry();
        let prod = d.as_mapping().unwrap().get_mapping("prod").unwrap();
        let m = prod.merged(&reg);

        assert_eq!(m.get("timeout").unwrap().to_i64(), Some(60));
    }

    #[test]
    fn merged_merge_key_itself_is_hidden() {
        let yaml = "\
d: &d
  a: 1
m:
  <<: *d
";
        let d = doc(yaml);
        let reg = d.build_anchor_registry();
        let inner = d.as_mapping().unwrap().get_mapping("m").unwrap();
        let m = inner.merged(&reg);

        assert!(m.get("<<").is_none());
        let keys: Vec<String> = m
            .keys()
            .map(|k| k.as_scalar().unwrap().as_string())
            .collect();
        assert_eq!(keys, vec!["a"]);
    }

    #[test]
    fn merged_sequence_of_aliases() {
        // Per YAML 1.1: earlier aliases take precedence over later ones.
        let yaml = "\
a: &a
  x: 1
  y: 1
b: &b
  y: 2
  z: 2
m:
  <<: [*a, *b]
";
        let d = doc(yaml);
        let reg = d.build_anchor_registry();
        let inner = d.as_mapping().unwrap().get_mapping("m").unwrap();
        let m = inner.merged(&reg);

        // x only in a
        assert_eq!(m.get("x").unwrap().to_i64(), Some(1));
        // y in both — a wins (first listed)
        assert_eq!(m.get("y").unwrap().to_i64(), Some(1));
        // z only in b
        assert_eq!(m.get("z").unwrap().to_i64(), Some(2));
    }

    #[test]
    fn merged_missing_alias_returns_none() {
        let yaml = "\
m:
  <<: *nope
  a: 1
";
        let d = doc(yaml);
        let reg = d.build_anchor_registry();
        let inner = d.as_mapping().unwrap().get_mapping("m").unwrap();
        let m = inner.merged(&reg);

        // Direct key still works
        assert_eq!(m.get("a").unwrap().to_i64(), Some(1));
        // Missing alias contributes nothing
        assert!(m.get("b").is_none());
    }

    #[test]
    fn merged_iter_order_direct_then_merged() {
        let yaml = "\
d: &d
  shared: from_d
  only_in_d: 1
m:
  direct: hi
  <<: *d
";
        let d = doc(yaml);
        let reg = d.build_anchor_registry();
        let inner = d.as_mapping().unwrap().get_mapping("m").unwrap();
        let m = inner.merged(&reg);

        let keys: Vec<String> = m
            .iter()
            .map(|(k, _)| k.as_scalar().unwrap().as_string())
            .collect();
        assert_eq!(keys, vec!["direct", "shared", "only_in_d"]);
    }

    #[test]
    fn merged_len_and_empty() {
        let yaml = "\
d: &d
  a: 1
  b: 2
m:
  <<: *d
  c: 3
";
        let d = doc(yaml);
        let reg = d.build_anchor_registry();
        let inner = d.as_mapping().unwrap().get_mapping("m").unwrap();
        let m = inner.merged(&reg);

        assert_eq!(m.len(), 3);
        assert!(!m.is_empty());
    }

    #[test]
    fn merged_alias_value_is_resolved() {
        let yaml = "\
target: &t
  k: 42
m:
  ref: *t
";
        let d = doc(yaml);
        let reg = d.build_anchor_registry();
        let inner = d.as_mapping().unwrap().get_mapping("m").unwrap();
        let m = inner.merged(&reg);

        let r = m.get("ref").unwrap();
        let nested = r.as_mapping().unwrap();
        assert_eq!(nested.get("k").unwrap().to_i64(), Some(42));
    }

    #[test]
    fn merged_get_merged_nested() {
        let yaml = "\
defaults: &d
  port: 80
prod:
  <<: *d
  inner:
    a: 1
";
        let d = doc(yaml);
        let reg = d.build_anchor_registry();
        let prod = d.as_mapping().unwrap().get_mapping("prod").unwrap();
        let m = prod.merged(&reg);

        let inner = m.get_merged("inner").unwrap();
        assert_eq!(inner.get("a").unwrap().to_i64(), Some(1));
    }

    #[test]
    fn document_merged_view() {
        let yaml = "\
defaults: &d
  timeout: 30
shared:
  <<: *d
  retries: 5
";
        let d = doc(yaml);
        let view = d.merged().unwrap();

        // Top-level keys visible
        assert!(view.contains_key("defaults"));
        assert!(view.contains_key("shared"));

        // Drill into merged child via as_mapping()
        let m = view.as_mapping();
        let shared = m.get_merged("shared").unwrap();
        assert_eq!(shared.get("timeout").unwrap().to_i64(), Some(30));
        assert_eq!(shared.get("retries").unwrap().to_i64(), Some(5));
    }

    #[test]
    fn merged_does_not_mutate_underlying_cst() {
        let yaml = "\
d: &d
  a: 1
m:
  <<: *d
  b: 2
";
        let d = doc(yaml);
        let reg = d.build_anchor_registry();
        let inner = d.as_mapping().unwrap().get_mapping("m").unwrap();
        let _ = inner.merged(&reg).get("a");
        // Original document text should round-trip unchanged.
        assert_eq!(d.to_string(), yaml);
    }
}
