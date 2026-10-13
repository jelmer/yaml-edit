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
use rowan::ast::AstNode;
use std::collections::HashMap;

/// A registry of anchors defined in a YAML document
#[derive(Debug, Clone)]
pub struct AnchorRegistry {
    /// Map from anchor name to every definition of it, in document order,
    /// as (offset of the `&name` token, anchored node)
    anchors: HashMap<String, Vec<(rowan::TextSize, SyntaxNode)>>,
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
                            self.anchors
                                .entry(name.to_string())
                                .or_default()
                                .push((token.text_range().start(), value_node));
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
    ///
    /// If the anchor is defined more than once, this returns the last
    /// definition in the document.
    pub fn resolve(&self, name: &str) -> Option<&SyntaxNode> {
        self.anchors.get(name)?.last().map(|(_, node)| node)
    }

    /// Look up the node that an alias at `alias` refers to.
    ///
    /// An alias refers to the most recent definition of its anchor that
    /// precedes it in the document, so a redefined anchor only affects the
    /// aliases that follow the redefinition. `alias` must belong to the same
    /// tree this registry was built from.
    pub(crate) fn resolve_alias(&self, name: &str, alias: &SyntaxNode) -> Option<&SyntaxNode> {
        let position = alias.text_range().start();
        self.anchors
            .get(name)?
            .iter()
            .rev()
            .find(|(anchor, _)| *anchor < position)
            .map(|(_, node)| node)
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
    /// Resolution is read-only. To write a merge or alias back, pass a mapping
    /// that already contains `<<: *name` (or an alias node) to
    /// [`Mapping::set`](crate::Mapping::set). `set` does not infer a merge
    /// from a resolved object.
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
        let registry = self.build_anchor_registry();
        let node = self.as_mapping()?.get(&key)?;
        if let crate::as_yaml::YamlNode::Alias(alias) = &node {
            registry.resolve_alias(&alias.name(), alias.syntax())?;
        }
        resolved_value(node, &registry, &mut Vec::new())
    }

    fn build_anchor_registry(&self) -> AnchorRegistry {
        AnchorRegistry::from_document(self)
    }
}

/// Whether `key` is the merge key. Only a plain `<<` is one; a quoted
/// `'<<'` is an ordinary string.
fn is_merge_key(key: &crate::as_yaml::YamlNode) -> bool {
    key.as_scalar().is_some_and(|s| s.value() == "<<")
}

/// Extract a string value from a YamlNode scalar.
fn node_as_string(node: &crate::as_yaml::YamlNode) -> Option<String> {
    node.as_scalar().map(|s| s.as_string())
}

/// Apply merge keys to create a new merged mapping (returns BTreeMap for YamlValue::Mapping)
///
/// Direct keys win over merged ones, and merge sources are taken in the
/// precedence order [`merge_sources`] returns them.
fn apply_merge_keys(
    mapping: &crate::yaml::Mapping,
    registry: &AnchorRegistry,
    expanding: &mut Vec<String>,
) -> std::collections::BTreeMap<String, YamlValue> {
    let mut merged_pairs = std::collections::BTreeMap::new();
    let sources = merge_sources(mapping, registry);
    for source in std::iter::once(mapping).chain(&sources) {
        for (key, value) in source.iter() {
            if is_merge_key(&key) {
                continue;
            }
            // A non-scalar key has no string form, so use its text as
            // YamlValue::cast does.
            let key_str = node_as_string(&key).unwrap_or_else(|| key.to_string());
            if merged_pairs.contains_key(&key_str) {
                continue;
            }
            if let Some(yaml_value) = resolved_value(value, registry, expanding) {
                merged_pairs.insert(key_str, yaml_value);
            }
        }
    }
    merged_pairs
}

/// Turn `node` into a [`YamlValue`], following aliases and applying merge
/// keys at every level.
///
/// An alias that resolves to nothing (the anchor was edited away, say), or
/// that refers back to an anchor it is nested in, is kept as its literal
/// `*name` text rather than dropped. `expanding` holds the anchors currently
/// being expanded.
fn resolved_value(
    node: crate::as_yaml::YamlNode,
    registry: &AnchorRegistry,
    expanding: &mut Vec<String>,
) -> Option<YamlValue> {
    use crate::as_yaml::YamlNode;

    match node {
        YamlNode::Alias(alias) => {
            let name = alias.name();
            let target = registry
                .resolve_alias(&name, alias.syntax())
                .filter(|_| !expanding.contains(&name))
                .and_then(|target| YamlNode::from_syntax_peeled(target.clone()));
            let Some(target) = target else {
                return Some(YamlValue::Scalar(crate::ScalarValue::string(alias.value())));
            };
            expanding.push(name);
            let value = resolved_value(target, registry, expanding);
            expanding.pop();
            value
        }
        YamlNode::Mapping(mapping) => Some(YamlValue::Mapping(apply_merge_keys(
            &mapping, registry, expanding,
        ))),
        YamlNode::Sequence(seq) => Some(YamlValue::Sequence(
            seq.values()
                .filter_map(|item| resolved_value(item, registry, expanding))
                .collect(),
        )),
        other => YamlValue::cast(other.syntax().clone()),
    }
}

/// The anchor a merge-key operand refers to.
///
/// `*name` parses as an ALIAS node, but a merge value can also reach here as
/// a plain scalar whose text starts with `*`, so accept both.
fn alias_target(node: &crate::as_yaml::YamlNode) -> Option<String> {
    match node {
        crate::as_yaml::YamlNode::Alias(alias) => Some(alias.name()),
        _ => node_as_string(node)?.strip_prefix('*').map(str::to_string),
    }
}

/// A view over a [`Mapping`](crate::yaml::Mapping) that transparently expands
/// alias references (`*name`) and merge keys (`<<:`).
///
/// `MergedMapping` is a lightweight, zero-copy view: it borrows the underlying
/// mapping and an [`AnchorRegistry`]. The underlying syntax tree is not
/// modified - every lookup walks the CST on demand and resolves aliases via
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
/// This view is read-only. [`Mapping::set`](crate::Mapping::set) replaces the
/// value node you pass; it does not turn a resolved map back into `<<: *name`.
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
    /// contributed by `<<:` merge sources. The merge key itself is hidden:
    /// looking up `"<<"` only finds a quoted `'<<'` key, which is an ordinary
    /// string.
    ///
    /// If the matched value is an alias (`*name`), the resolved target node
    /// is returned. If no such anchor is defined, the alias is left
    /// unresolved and returned as-is.
    pub fn get(&self, key: impl crate::AsYaml) -> Option<crate::as_yaml::YamlNode> {
        let find = |mapping: &crate::yaml::Mapping| {
            mapping
                .entries()
                .find(|e| e.key_matches(&key) && !e.key_node().is_some_and(|k| is_merge_key(&k)))
                .and_then(|e| e.value_node())
        };
        let node = find(&self.base).or_else(|| {
            merge_sources(&self.base, self.registry)
                .iter()
                .find_map(find)
        })?;
        Some(resolve_alias_node(node, self.registry))
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
    /// already yielded) filtered out. Keys are compared as YAML values, so
    /// the integer `1` and the string `'1'` are distinct. The `<<` merge key
    /// itself is never yielded.
    pub fn iter(
        &self,
    ) -> impl Iterator<Item = (crate::as_yaml::YamlNode, crate::as_yaml::YamlNode)> + '_ {
        let mut out: Vec<_> = self.base.iter().filter(|(k, _)| !is_merge_key(k)).collect();
        for source in merge_sources(&self.base, self.registry) {
            for (key, value) in source.iter() {
                if is_merge_key(&key) || out.iter().any(|(k, _)| k.yaml_eq(&key)) {
                    continue;
                }
                out.push((key, value));
            }
        }
        out.into_iter()
            .map(|(k, v)| (k, resolve_alias_node(v, self.registry)))
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

/// Walk the `<<:` entries of `base` and return the resolved source mappings,
/// in precedence order (highest first).
///
/// Each source is followed by the sources it merges in itself, since its own
/// keys shadow those it inherits. A mapping is only collected once, which
/// also stops a merge that refers back to an enclosing anchor.
fn merge_sources(
    base: &crate::yaml::Mapping,
    registry: &AnchorRegistry,
) -> Vec<crate::yaml::Mapping> {
    let mut sources = Vec::new();
    collect_merge_sources(base, registry, &mut sources);
    sources
}

fn collect_merge_sources(
    base: &crate::yaml::Mapping,
    registry: &AnchorRegistry,
    sources: &mut Vec<crate::yaml::Mapping>,
) {
    use crate::as_yaml::YamlNode;

    for (key, value) in base.iter() {
        if !is_merge_key(&key) {
            continue;
        }
        let operands = match value {
            YamlNode::Sequence(seq) => seq.values().collect(),
            other => vec![other],
        };
        for operand in operands {
            let source = match operand {
                YamlNode::Mapping(m) => m,
                other => {
                    let Some(target) = alias_target(&other)
                        .and_then(|name| registry.resolve_alias(&name, other.syntax()))
                    else {
                        continue;
                    };
                    let Some(m) = crate::yaml::Mapping::cast(target.clone()) else {
                        continue;
                    };
                    m
                }
            };
            if sources.iter().any(|s| s.syntax() == source.syntax()) {
                continue;
            }
            sources.push(source.clone());
            collect_merge_sources(&source, registry, sources);
        }
    }
}

/// If `node` is an alias, resolve it via `registry`; otherwise return it as-is.
fn resolve_alias_node(
    node: crate::as_yaml::YamlNode,
    registry: &AnchorRegistry,
) -> crate::as_yaml::YamlNode {
    use crate::as_yaml::YamlNode;

    if let YamlNode::Alias(alias) = &node {
        if let Some(target) = registry.resolve_alias(&alias.name(), node.syntax()) {
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

    /// `get_resolved(key)` on `yaml` as `(key, value)` pairs, with each
    /// value rendered through `Display`.
    fn resolved_pairs(yaml: &str, key: &str) -> Vec<(String, String)> {
        let resolved = doc(yaml).get_resolved(key).expect("key resolves");
        let mapping = resolved.as_mapping().expect("value is a mapping");
        mapping
            .iter()
            .map(|(k, v)| (k.clone(), v.to_string()))
            .collect()
    }

    fn pairs(expected: &[(&str, &str)]) -> Vec<(String, String)> {
        expected
            .iter()
            .map(|&(k, v)| (k.to_string(), v.to_string()))
            .collect()
    }

    #[test]
    fn get_resolved_merge_sequence_earlier_wins() {
        let yaml = "a: &a {k: from_a}\nb: &b {k: from_b, j: 2}\nm:\n  <<: [*a, *b]\n";
        assert_eq!(
            resolved_pairs(yaml, "m"),
            pairs(&[("j", "2"), ("k", "from_a")])
        );
    }

    fn merged_pairs(yaml: &str, key: &str) -> Vec<(String, String)> {
        let d = doc(yaml);
        let reg = d.build_anchor_registry();
        let m = d.as_mapping().unwrap().get_mapping(key).unwrap();
        let merged = m.merged(&reg);
        merged
            .iter()
            .map(|(k, v)| (k.to_string(), v.to_string()))
            .collect()
    }

    #[test]
    fn nested_merge_keys_are_followed() {
        use crate::path::YamlPath;
        let yaml = "b: &b {x: 1, y: 0}\nmid: &m\n  <<: *b\n  y: 2\ntop:\n  <<: *m\n";
        assert_eq!(
            resolved_pairs(yaml, "top"),
            pairs(&[("x", "1"), ("y", "2")])
        );
        assert_eq!(merged_pairs(yaml, "top"), pairs(&[("y", "2"), ("x", "1")]));
        let d = doc(yaml);
        assert_eq!(d.try_get_path("top.x").unwrap().to_string(), "1");
        assert_eq!(d.try_get_path("top.y").unwrap().to_string(), "2");
    }

    #[test]
    fn self_referencing_merge_terminates() {
        let yaml = "a: &a\n  k: 1\n  <<: *a\n";
        assert_eq!(resolved_pairs(yaml, "a"), pairs(&[("k", "1")]));
        assert_eq!(merged_pairs(yaml, "a"), pairs(&[("k", "1")]));
    }

    #[test]
    fn inline_mapping_merge_value() {
        use crate::path::YamlPath;
        for (yaml, expected) in [
            ("m:\n  <<: {z: 1}\n  y: 2\n", vec![("y", "2"), ("z", "1")]),
            (
                "a: &a {w: 0}\nm:\n  <<: [{z: 1}, *a]\n  y: 2\n",
                vec![("w", "0"), ("y", "2"), ("z", "1")],
            ),
        ] {
            assert_eq!(resolved_pairs(yaml, "m"), pairs(&expected), "{yaml:?}");
            assert_eq!(doc(yaml).try_get_path("m.z").unwrap().to_string(), "1");
        }
        assert_eq!(
            merged_pairs("m:\n  <<: {z: 1}\n  y: 2\n", "m"),
            pairs(&[("y", "2"), ("z", "1")])
        );
    }

    #[test]
    fn get_resolved_follows_aliases() {
        for (yaml, key, expected) in [
            ("a: &x 1\nb: *x\n", "b", "b: 1\n"),
            ("x: &x 1\nm: {a: *x, c: 2}\n", "m", "m: {a: 1, c: 2}\n"),
            ("x: &x 1\ns: [*x, 2]\n", "s", "s: [1, 2]\n"),
            ("x: &x {k: 1}\nm:\n  a: [*x]\n", "m", "m:\n  a: [{k: 1}]\n"),
            (
                "b: &b {k: 1}\nx: &x {<<: *b}\nm: {a: *x}\n",
                "m",
                "m: {a: {k: 1}}\n",
            ),
        ] {
            assert_eq!(
                doc(yaml).get_resolved(key),
                doc(expected).get_resolved(key),
                "{yaml:?}"
            );
        }
    }

    #[test]
    fn get_resolved_keeps_complex_keys() {
        assert_eq!(
            resolved_pairs("m:\n  ? [p, q]\n  : complex\n  j: 1\n", "m"),
            pairs(&[("[p, q]", "complex"), ("j", "1")])
        );
    }

    #[test]
    fn get_resolved_undefined_alias() {
        assert_eq!(doc("b: *nope\n").get_resolved("b"), None);
    }

    #[test]
    fn get_resolved_recursive_alias_terminates() {
        let alias = YamlValue::Scalar(crate::ScalarValue::string("*a"));
        assert_eq!(
            doc("a: &a [*a]\n").get_resolved("a"),
            Some(YamlValue::Sequence(vec![YamlValue::Sequence(vec![alias])]))
        );
    }

    #[test]
    fn quoted_merge_key_is_an_ordinary_key() {
        use crate::path::YamlPath;
        for yaml in [
            "a: &a {k: 1}\nm:\n  '<<': *a\n  j: 2\n",
            "a: &a {k: 1}\nm:\n  \"<<\": *a\n  j: 2\n",
        ] {
            let d = doc(yaml);
            let resolved = d.get_resolved("m").unwrap();
            let keys: Vec<&str> = resolved
                .as_mapping()
                .unwrap()
                .keys()
                .map(String::as_str)
                .collect();
            assert_eq!(keys, vec!["<<", "j"], "{yaml:?}");

            let reg = d.build_anchor_registry();
            let m = d.as_mapping().unwrap().get_mapping("m").unwrap();
            let merged = m.merged(&reg);
            let keys: Vec<String> = merged
                .keys()
                .map(|k| k.as_scalar().unwrap().as_string())
                .collect();
            assert_eq!(keys, vec!["<<", "j"], "{yaml:?}");
            assert_eq!(merged.get("k"), None, "{yaml:?}");
            assert_eq!(
                merged.get("<<").map(|v| v.to_string()).as_deref(),
                Some("{k: 1}"),
                "{yaml:?}"
            );
            assert_eq!(
                d.try_get_path("m.k"),
                Err(crate::path::PathError::NotFound { at: "k".into() }),
                "{yaml:?}"
            );
        }
    }

    #[test]
    fn merged_iter_keeps_non_string_keys() {
        assert_eq!(
            merged_pairs("a: &a {x: 1}\nm:\n  <<: *a\n  ? [p, q]\n  : complex\n", "m"),
            pairs(&[("[p, q]", "complex"), ("x", "1")])
        );
        // The int 1 and the string '1' are different keys.
        let yaml = "s: &s {1: from_src, x: 1}\nm:\n  <<: *s\n  '1': base\n  x: 2\n";
        assert_eq!(
            merged_pairs(yaml, "m"),
            pairs(&[("'1'", "base"), ("x", "2"), ("1", "from_src")])
        );
        let d = doc(yaml);
        let reg = d.build_anchor_registry();
        let m = d.as_mapping().unwrap().get_mapping("m").unwrap();
        assert_eq!(m.merged(&reg).len(), 3);
        assert_eq!(m.merged(&reg).get(1).unwrap().to_string(), "from_src");
        assert_eq!(m.merged(&reg).get("1").unwrap().to_string(), "base");
    }

    #[test]
    fn get_resolved_keys_are_decoded() {
        // Mapping::pairs() yields the KEY wrapper, so casting it straight to
        // Scalar failed and the key fell back to raw text. A quoted key kept
        // its quotes and became unreachable by its own name -- but only
        // without a merge key, since apply_merge_keys decodes separately.
        for (yaml, expected) in [
            ("a:\n  '0': z\n  x: 1\n", vec!["0", "x"]),
            ("a:\n  \"k\": z\n", vec!["k"]),
            ("a:\n  plain: z\n", vec!["plain"]),
            // With a merge key, which takes the other code path.
            ("s: &s\n  q: r\na:\n  <<: *s\n  '0': z\n", vec!["0", "q"]),
        ] {
            let doc = Document::from_str(yaml).unwrap();
            let resolved = doc.get_resolved("a").expect("a resolves");
            let mapping = resolved.as_mapping().expect("a is a mapping");
            let keys: Vec<&str> = mapping.keys().map(String::as_str).collect();
            assert_eq!(keys, expected, "{yaml:?}");
            for key in expected {
                assert!(
                    mapping.contains_key(key),
                    "{yaml:?}: {key:?} not reachable by name"
                );
            }
        }
    }

    #[test]
    fn get_resolved_expands_merge_keys() {
        // `*a` reaches apply_merge_keys as an ALIAS node, not a scalar, so
        // the merge used to fall through and drop every inherited key. Check
        // get_resolved against the MergedMapping view, which is the same
        // question asked of the newer code path.
        for (yaml, present, absent) in [
            (
                "a: &a\n  x: 1\nb: &b\n  y: 2\nm:\n  <<: [*a, *b]\n  z: 3\n",
                vec!["x", "y", "z"],
                vec!["w"],
            ),
            (
                "a: &a\n  x: 1\nm:\n  <<: *a\n  z: 3\n",
                vec!["x", "z"],
                vec!["y"],
            ),
        ] {
            let doc = Document::from_str(yaml).unwrap();
            let registry = doc.build_anchor_registry();
            let root = doc.as_mapping().unwrap();
            let m_node = root.get("m").unwrap();
            let m = m_node.as_mapping().unwrap();
            let merged = m.merged(&registry);
            let resolved = doc.get_resolved("m").expect("m resolves");

            for key in present {
                assert!(merged.get(key).is_some(), "MergedMapping lost {key:?}");
                assert!(
                    resolved.as_mapping().and_then(|mm| mm.get(key)).is_some(),
                    "get_resolved lost {key:?} in {yaml:?}"
                );
            }
            for key in absent {
                assert!(merged.get(key).is_none(), "MergedMapping invented {key:?}");
                assert!(
                    resolved.as_mapping().and_then(|mm| mm.get(key)).is_none(),
                    "get_resolved invented {key:?} in {yaml:?}"
                );
            }
        }
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
        // y in both - a wins (first listed)
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

    #[test]
    fn merged_alias_uses_preceding_anchor_definition() {
        let d = doc("a: &x 1\nb: *x\nc: &x 2\nd: *x\n");
        let view = d.merged().unwrap();
        assert_eq!(view.get("b").unwrap().to_i64(), Some(1));
        assert_eq!(view.get("d").unwrap().to_i64(), Some(2));
    }

    #[test]
    fn merged_merge_key_uses_preceding_anchor_definition() {
        let d = doc("a: &x {k: 1}\nm:\n  <<: *x\nc: &x {k: 2}\n");
        let view = d.merged().unwrap();
        let m = view.as_mapping().get_merged("m").unwrap();
        assert_eq!(m.get("k").unwrap().to_i64(), Some(1));
    }

    #[test]
    fn merged_alias_ignores_later_anchor() {
        let d = doc("b: *x\na: &x 1\n");
        let view = d.merged().unwrap();
        assert!(view.get("b").unwrap().is_alias());
    }

    #[test]
    fn registry_resolve_returns_last_definition() {
        let d = doc("a: &x 1\nc: &x 2\n");
        let reg = d.build_anchor_registry();
        assert_eq!(reg.resolve("x").unwrap().text().to_string(), "2");
    }
}
