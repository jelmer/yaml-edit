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
//! - **Error handling**: An undefined or recursive alias, a merge key
//!   whose operand is not a mapping, a key repeated within a mapping,
//!   distinct keys that would collide in a resolved mapping, or `!!set`, `!!omap` and `!!pairs` content
//!   that [`YamlValue`] cannot hold are reported as a [`ResolveError`]
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
//! if let Some(resolved_value) = doc.get_resolved("production").unwrap() {
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

use crate::as_yaml::{key_identity, AsYaml, NodeIdentity};
use crate::lex::SyntaxKind;
use crate::value::YamlValue;
use crate::yaml::SyntaxNode;
use rowan::ast::AstNode;
use std::collections::HashMap;

/// An error resolving aliases or merge keys, or converting a node to a
/// [`YamlValue`] that cannot hold it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ResolveError {
    /// An alias refers to an anchor that is not defined before it in the
    /// document.
    UndefinedAlias {
        /// The anchor name the alias references, without the `*`.
        alias: String,
    },
    /// An alias refers to an anchor it is nested in, so expanding it
    /// would not terminate.
    RecursiveAlias {
        /// The anchor name the alias references, without the `*`.
        alias: String,
    },
    /// A merge key (`<<`) operand is neither a mapping nor an alias to
    /// one.
    InvalidMergeOperand {
        /// The source text of the offending operand.
        operand: String,
    },
    /// A mapping has two equal keys, such as `a` and `'a'`, or a key and
    /// an alias to an equal node, so it cannot keep both values. A direct
    /// key shadowing a key from a merge key (`<<`) is not a duplicate. The
    /// members of a `!!set` and the keys of a `!!omap` have to be distinct
    /// too.
    DuplicateKey {
        /// The source text of the key that was seen first.
        first: String,
        /// The source text of the key that repeats it.
        second: String,
    },
    /// Two distinct keys of a mapping have the same string form, such as
    /// the int `1` and the string `'1'`, so they cannot both be kept in a
    /// [`YamlValue::Mapping`]. The same goes for the members of a
    /// [`YamlValue::Set`] and the keys of a [`YamlValue::OrderedMapping`].
    KeyCollision {
        /// The source text of the key that was seen first.
        first: String,
        /// The source text of the key that collides with it.
        second: String,
    },
    /// The content of a `!!set` is not a mapping, so it has no members.
    InvalidSetContent {
        /// The source text of the content.
        content: String,
    },
    /// A `!!set` member is not a scalar, so it cannot be kept in a
    /// [`YamlValue::Set`].
    InvalidSetMember {
        /// The source text of the member.
        member: String,
    },
    /// A `!!set` member has a value other than null, which a
    /// [`YamlValue::Set`] cannot keep.
    NonNullSetValue {
        /// The source text of the member.
        member: String,
        /// The source text of its value.
        value: String,
    },
    /// An item of a `!!omap` or `!!pairs` sequence is not a mapping with a
    /// single entry.
    InvalidPairsItem {
        /// The tag of the sequence, `!!omap` or `!!pairs`.
        tag: String,
        /// The source text of the item.
        item: String,
    },
    /// The content of a `!!omap` or `!!pairs` is not a sequence, so it has
    /// no entries.
    InvalidPairsContent {
        /// The tag, `!!omap` or `!!pairs`.
        tag: String,
        /// The source text of the content.
        content: String,
    },
    /// [`YamlValue::cast`] met an alias, which it cannot resolve without
    /// the document's anchors. [`YamlValue::from_document`] and
    /// [`DocumentResolvedExt::get_resolved`] resolve aliases.
    UnresolvedAlias {
        /// The anchor name the alias references, without the `*`.
        alias: String,
    },
    /// A document, key, value, sequence entry or tagged node holds more
    /// than one node, as in `k: [q] x`, which YAML does not allow but the
    /// parser accepts. A [`YamlValue`] can only keep one of them.
    ExtraNode {
        /// The source text of the node after the first.
        node: String,
    },
}

impl std::fmt::Display for ResolveError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ResolveError::UndefinedAlias { alias } => {
                write!(f, "alias to undefined anchor {alias:?}")
            }
            ResolveError::RecursiveAlias { alias } => {
                write!(f, "alias {alias:?} refers to an anchor it is nested in")
            }
            ResolveError::InvalidMergeOperand { operand } => {
                write!(f, "merge key operand {operand:?} is not a mapping")
            }
            ResolveError::KeyCollision { first, second } => {
                write!(
                    f,
                    "distinct keys {first:?} and {second:?} have the same string form"
                )
            }
            ResolveError::DuplicateKey { first, second } => {
                write!(f, "duplicate mapping key: {second:?} equals {first:?}")
            }
            ResolveError::InvalidSetContent { content } => {
                write!(f, "!!set content {content:?} is not a mapping")
            }
            ResolveError::InvalidSetMember { member } => {
                write!(f, "!!set member {member:?} is not a scalar")
            }
            ResolveError::NonNullSetValue { member, value } => {
                write!(f, "!!set member {member:?} has non-null value {value:?}")
            }
            ResolveError::InvalidPairsItem { tag, item } => {
                write!(
                    f,
                    "{tag} item {item:?} is not a mapping with a single entry"
                )
            }
            ResolveError::InvalidPairsContent { tag, content } => {
                write!(f, "{tag} content {content:?} is not a sequence")
            }
            ResolveError::UnresolvedAlias { alias } => {
                write!(
                    f,
                    "alias {alias:?} cannot be converted without resolving it; \
                     use get_resolved to resolve aliases"
                )
            }
            ResolveError::ExtraNode { node } => {
                write!(f, "node {node:?} follows another in the same place")
            }
        }
    }
}

impl std::error::Error for ResolveError {}

/// An alias that does not refer to a node.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum AliasError {
    /// No anchor of the alias's name is defined before it.
    Undefined { alias: String },
    /// The alias refers to an anchor it is nested in.
    Recursive { alias: String },
}

impl From<AliasError> for ResolveError {
    fn from(err: AliasError) -> Self {
        match err {
            AliasError::Undefined { alias } => ResolveError::UndefinedAlias { alias },
            AliasError::Recursive { alias } => ResolveError::RecursiveAlias { alias },
        }
    }
}

/// An error collecting the entries of a [`MergedMapping`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum MergeError {
    Alias(AliasError),
    /// A merge key operand is not a mapping; holds its source text.
    InvalidOperand {
        operand: String,
    },
}

impl From<AliasError> for MergeError {
    fn from(err: AliasError) -> Self {
        MergeError::Alias(err)
    }
}

impl From<MergeError> for ResolveError {
    fn from(err: MergeError) -> Self {
        match err {
            MergeError::Alias(err) => err.into(),
            MergeError::InvalidOperand { operand } => ResolveError::InvalidMergeOperand { operand },
        }
    }
}

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
        // Node properties come in either order. In `!!map &x` the anchor
        // sits inside the TAGGED_NODE and anchors it, tag included.
        if node.kind() == SyntaxKind::TAGGED_NODE {
            return Some(node.clone());
        }
        // The anchored value is typically a sibling MAPPING/SEQUENCE/SCALAR node
        for child in node.children() {
            if matches!(
                child.kind(),
                SyntaxKind::VALUE
                    | SyntaxKind::SCALAR
                    | SyntaxKind::MAPPING
                    | SyntaxKind::SEQUENCE
                    | SyntaxKind::TAGGED_NODE
                    | SyntaxKind::ALIAS
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

    /// Look up the node that `alias` refers to like
    /// [`resolve_alias`](Self::resolve_alias), following anchors that are
    /// on aliases themselves, as in `&b *a`, to the node they end at.
    pub(crate) fn resolve_alias_through(
        &self,
        alias: &crate::yaml::Alias,
    ) -> Result<&SyntaxNode, AliasError> {
        let mut alias = alias.clone();
        let mut seen = Vec::new();
        loop {
            let name = alias.name();
            let target =
                self.resolve_alias(&name, alias.syntax())
                    .ok_or_else(|| AliasError::Undefined {
                        alias: name.clone(),
                    })?;
            let content = if target.kind() == SyntaxKind::VALUE {
                target.children().next()
            } else {
                Some(target.clone())
            };
            let Some(next) = content.and_then(crate::yaml::Alias::cast) else {
                return Ok(target);
            };
            if seen.contains(target) {
                return Err(AliasError::Recursive { alias: name });
            }
            seen.push(target.clone());
            alias = next;
        }
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
    /// Returns `Ok(None)` if the document root is not a mapping or has no
    /// such key, and an error if the value contains an undefined or
    /// recursive alias, an invalid merge key, a mapping with the same key
    /// twice, or distinct keys that have the same string form, such as `1`
    /// and `'1'`.
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
    /// if let Some(resolved_value) = doc.get_resolved("server").unwrap() {
    ///     if let Some(server) = resolved_value.as_mapping() {
    ///         if let Some(port) = server.get("port") {
    ///             assert_eq!(port.to_i64(), Some(8080));
    ///         }
    ///     }
    /// }
    /// ```
    fn get_resolved(&self, key: impl crate::AsYaml) -> Result<Option<YamlValue>, ResolveError>;

    /// Build the anchor registry for this document
    fn build_anchor_registry(&self) -> AnchorRegistry;
}

impl DocumentResolvedExt for crate::yaml::Document {
    fn get_resolved(&self, key: impl crate::AsYaml) -> Result<Option<YamlValue>, ResolveError> {
        let Some(node) = self.as_mapping().and_then(|m| m.get(&key)) else {
            return Ok(None);
        };
        let registry = self.build_anchor_registry();
        crate::value::convert::resolved_value(node, Some(&registry), &mut Vec::new())
    }

    fn build_anchor_registry(&self) -> AnchorRegistry {
        AnchorRegistry::from_document(self)
    }
}

/// Whether `key` is the merge key. Only a plain `<<` is one; a quoted
/// `'<<'` is an ordinary string.
pub(crate) fn is_merge_key(key: &crate::as_yaml::YamlNode) -> bool {
    key.as_scalar().is_some_and(|s| s.value() == "<<")
}

/// A view over a [`Mapping`](crate::yaml::Mapping) that transparently expands
/// alias references (`*name`) and merge keys (`<<:`).
///
/// `MergedMapping` borrows an [`AnchorRegistry`] and collects its entries
/// when it is created: the merge sources are followed and alias values are
/// resolved then, so an undefined alias or invalid merge key is reported
/// by [`new`](Self::new). The underlying syntax tree is not modified and
/// the returned nodes are live CST nodes, but entries added, removed or
/// replaced after the view was created are not seen.
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
/// let merged = prod.merged(&registry).unwrap();
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
    entries: Vec<MergedEntry>,
}

impl<'a> MergedMapping<'a> {
    /// Create a new merged view over `base`, resolving aliases against `registry`.
    ///
    /// Fails if a `<<` operand in `base`, or in a mapping merged into it,
    /// is an undefined alias or not a mapping, if a value visible
    /// through the view is an undefined alias, or if a key is or holds an
    /// undefined or recursive alias.
    pub fn new(
        base: crate::yaml::Mapping,
        registry: &'a AnchorRegistry,
    ) -> Result<Self, ResolveError> {
        Ok(Self::build(base, registry)?)
    }

    /// Like [`new`](Self::new), failing with the errors only a merged view
    /// can meet.
    pub(crate) fn build(
        base: crate::yaml::Mapping,
        registry: &'a AnchorRegistry,
    ) -> Result<Self, MergeError> {
        let entries = merged_entries(&base, registry)?;
        Ok(Self {
            base,
            registry,
            entries,
        })
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
    /// is returned.
    pub fn get(&self, key: impl crate::AsYaml) -> Option<crate::as_yaml::YamlNode> {
        // A key that cannot be resolved, such as an undefined alias, is in
        // no mapping.
        let identity = crate::as_yaml::identity_of(&key, Some(self.registry))
            .ok()
            .flatten()?;
        self.entries
            .iter()
            .find(|(k, _, _)| *k == identity)
            .map(|(_, _, v)| v.clone())
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
        self.entries.iter().map(|(_, k, v)| (k.clone(), v.clone()))
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
    /// Returns `Ok(None)` if the key does not exist or the value is not a
    /// mapping, and fails as [`new`](Self::new) does on the nested mapping.
    pub fn get_merged(
        &self,
        key: impl crate::AsYaml,
    ) -> Result<Option<MergedMapping<'a>>, ResolveError> {
        let Some(mapping) = self.get(key).and_then(|node| node.as_mapping().cloned()) else {
            return Ok(None);
        };
        MergedMapping::new(mapping, self.registry).map(Some)
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
pub(crate) fn merge_sources(
    base: &crate::yaml::Mapping,
    registry: &AnchorRegistry,
) -> Result<Vec<crate::yaml::Mapping>, MergeError> {
    let mut sources = Vec::new();
    collect_merge_sources(base, registry, &mut sources)?;
    Ok(sources)
}

fn collect_merge_sources(
    base: &crate::yaml::Mapping,
    registry: &AnchorRegistry,
    sources: &mut Vec<crate::yaml::Mapping>,
) -> Result<(), MergeError> {
    use crate::as_yaml::YamlNode;

    for (key, value) in base.iter() {
        if !is_merge_key(&key) {
            continue;
        }
        // An alias stands for its node, so it can name a sequence of
        // mappings too.
        let sequence = match resolve_alias_node(value.clone(), registry)? {
            YamlNode::Sequence(seq) => Some(seq),
            YamlNode::TaggedNode(tagged) => tagged.as_sequence(),
            _ => None,
        };
        let operands = match sequence {
            Some(seq) => seq.values().collect(),
            None => vec![value],
        };
        for operand in operands {
            let source = match resolve_alias_node(operand.clone(), registry)? {
                YamlNode::Mapping(m) => Some(m),
                YamlNode::TaggedNode(tagged) => tagged.as_mapping(),
                _ => None,
            }
            .ok_or_else(|| MergeError::InvalidOperand {
                operand: operand.to_string(),
            })?;
            if sources.iter().any(|s| s.syntax() == source.syntax()) {
                continue;
            }
            sources.push(source.clone());
            collect_merge_sources(&source, registry, sources)?;
        }
    }
    Ok(())
}

/// The entries visible through a merged view of `base`: its own entries,
/// then those of its merge sources that are not shadowed, with alias
/// values resolved.
fn merged_entries(
    base: &crate::yaml::Mapping,
    registry: &AnchorRegistry,
) -> Result<Vec<MergedEntry>, MergeError> {
    let mut entries: Vec<MergedEntry> = Vec::new();
    for (key, value) in base.iter().filter(|(k, _)| !is_merge_key(k)) {
        let identity = key_identity(&key, Some(registry))?;
        entries.push((identity, key, resolve_alias_node(value, registry)?));
    }
    for source in merge_sources(base, registry)? {
        for (key, value) in source.iter() {
            if is_merge_key(&key) {
                continue;
            }
            let identity = key_identity(&key, Some(registry))?;
            if entries.iter().any(|(k, _, _)| *k == identity) {
                continue;
            }
            entries.push((identity, key, resolve_alias_node(value, registry)?));
        }
    }
    Ok(entries)
}

/// An entry of a merged view: the key's identity, the key as written and
/// the value, with an alias resolved.
type MergedEntry = (
    NodeIdentity,
    crate::as_yaml::YamlNode,
    crate::as_yaml::YamlNode,
);

/// The document `node` is in, or the root of its tree outside one.
/// Anchors are scoped to a single document.
pub(crate) fn document_scope(node: &SyntaxNode) -> SyntaxNode {
    node.ancestors()
        .find(|n| n.kind() == SyntaxKind::DOCUMENT)
        .unwrap_or_else(|| node.ancestors().last().expect("ancestors has self"))
}

/// If `node` is an alias, resolve it via `registry`; otherwise return it as-is.
pub(crate) fn resolve_alias_node(
    node: crate::as_yaml::YamlNode,
    registry: &AnchorRegistry,
) -> Result<crate::as_yaml::YamlNode, AliasError> {
    use crate::as_yaml::YamlNode;

    let YamlNode::Alias(alias) = &node else {
        return Ok(node);
    };
    resolve_alias_with(alias, registry, SyntaxNode::clone)
}

/// Resolve `alias` via `registry` to the node built from `adjust` applied
/// to the anchored node.
pub(crate) fn resolve_alias_with(
    alias: &crate::yaml::Alias,
    registry: &AnchorRegistry,
    adjust: impl FnOnce(&SyntaxNode) -> SyntaxNode,
) -> Result<crate::as_yaml::YamlNode, AliasError> {
    let target = adjust(registry.resolve_alias_through(alias)?);
    crate::as_yaml::YamlNode::from_syntax_peeled(target).ok_or(AliasError::Undefined {
        alias: alias.name(),
    })
}

/// Extension trait that adds [`merged`](Self::merged) to [`Mapping`](crate::yaml::Mapping).
pub trait MappingMergedExt {
    /// Return a [`MergedMapping`] view over this mapping that resolves
    /// aliases and merge keys against `registry`.
    ///
    /// The view is read-only and does not modify the underlying CST.
    ///
    /// Fails as [`MergedMapping::new`] does.
    fn merged<'a>(&self, registry: &'a AnchorRegistry) -> Result<MergedMapping<'a>, ResolveError>;
}

impl MappingMergedExt for crate::yaml::Mapping {
    fn merged<'a>(&self, registry: &'a AnchorRegistry) -> Result<MergedMapping<'a>, ResolveError> {
        MergedMapping::new(self.clone(), registry)
    }
}

/// Extension trait that adds [`merged`](Self::merged) to [`Document`](crate::yaml::Document).
pub trait DocumentMergedExt {
    /// Return a [`MergedMapping`] view over the document's root mapping.
    ///
    /// The anchor registry is built from this document automatically.
    /// Returns `Ok(None)` if the document's root is not a mapping, and fails
    /// as [`MergedMapping::new`] does.
    ///
    /// Because the registry is owned by the returned view, this method
    /// returns an owned [`MergedView`] container rather than a borrowing
    /// `MergedMapping<'_>`. Use [`MergedView::as_mapping`] to obtain the
    /// borrowing view for lookups.
    fn merged(&self) -> Result<Option<MergedView>, ResolveError>;
}

impl DocumentMergedExt for crate::yaml::Document {
    fn merged(&self) -> Result<Option<MergedView>, ResolveError> {
        let Some(mapping) = self.as_mapping() else {
            return Ok(None);
        };
        let registry = self.build_anchor_registry();
        let entries = merged_entries(&mapping, &registry).map_err(ResolveError::from)?;
        Ok(Some(MergedView {
            mapping,
            registry,
            entries,
        }))
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
    entries: Vec<MergedEntry>,
}

impl MergedView {
    /// Borrow this view as a [`MergedMapping`] for lookups.
    pub fn as_mapping(&self) -> MergedMapping<'_> {
        MergedMapping {
            base: self.mapping.clone(),
            registry: &self.registry,
            entries: self.entries.clone(),
        }
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
    fn get_resolved_tagged_alias() {
        // The alias's own tag applies to what it refers to, as when an
        // edit replaces it with a copy.
        for (yaml, expected) in [
            ("x: &x 1\na: !t *x\n", "a: !t 1\n"),
            ("x: &x '1'\na: !!int *x\n", "a: !!int '1'\n"),
            ("x: &x [1]\na: !t *x\n", "a: !t [1]\n"),
            ("x: &x {k: 1}\na: !t *x\n", "a: !t {k: 1}\n"),
            ("x: &x {k: }\na: !!set *x\n", "a: !!set {k: }\n"),
            ("x: &x [{k: 1}]\na: !!omap *x\n", "a: !!omap [{k: 1}]\n"),
            ("y: &y 2\nx: &x [*y]\na: !t *x\n", "a: !t [2]\n"),
            ("x: &x !u 1\na: !t *x\n", "a: !t 1\n"),
            ("x: !u &x [1]\na: !t *x\n", "a: !t [1]\n"),
            ("x: &x !u {k: }\na: !!set *x\n", "a: !!set {k: }\n"),
        ] {
            assert_eq!(
                doc(yaml).get_resolved("a"),
                doc(expected).get_resolved("a"),
                "{yaml:?}"
            );
        }
        assert_eq!(
            doc("x: &x 1\na: !t *x\n").get_resolved("a"),
            Ok(Some(YamlValue::Scalar(
                crate::ScalarValue::parse("1").with_resolved_tag("!t".to_string())
            )))
        );
    }
}
