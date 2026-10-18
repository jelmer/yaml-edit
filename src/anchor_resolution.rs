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

use crate::as_yaml::{AsYaml, NodeIdentity};
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
    ) -> Result<&SyntaxNode, ResolveError> {
        let mut alias = alias.clone();
        let mut seen = Vec::new();
        loop {
            let name = alias.name();
            let target = self.resolve_alias(&name, alias.syntax()).ok_or_else(|| {
                ResolveError::UndefinedAlias {
                    alias: name.clone(),
                }
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
                return Err(ResolveError::RecursiveAlias { alias: name });
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
        resolved_value(node, Some(&registry), &mut Vec::new())
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

/// The identity of `key` as a mapping key, following aliases through
/// `registry`; see [`NodeIdentity`].
pub(crate) fn key_identity(
    key: &crate::as_yaml::YamlNode,
    registry: Option<&AnchorRegistry>,
) -> Result<NodeIdentity, ResolveError> {
    Ok(crate::as_yaml::node_identity(key.syntax(), registry)?.expect("a YamlNode is a value node"))
}

/// Apply merge keys to create a new merged mapping (returns BTreeMap for YamlValue::Mapping)
///
/// Direct keys win over merged ones, and merge sources are taken in the
/// precedence order [`merge_sources`] returns them. Keys are compared by
/// [`key_identity`], so a direct `16` shadows a merged `0x10`; within one
/// mapping, base or merged, an equal key is a
/// [`ResolveError::DuplicateKey`]. Each key is stored under its
/// [`NodeIdentity::key_string`], and distinct keys with the same string
/// form, such as `1` and `'1'`, are a [`ResolveError::KeyCollision`].
/// Without a registry, `<<` is an ordinary key and an alias key is a
/// [`ResolveError::UnresolvedAlias`].
fn apply_merge_keys(
    mapping: &crate::yaml::Mapping,
    registry: Option<&AnchorRegistry>,
    expanding: &mut Vec<SyntaxNode>,
) -> Result<std::collections::BTreeMap<String, YamlValue>, ResolveError> {
    use crate::as_yaml::YamlNode;

    let mut merged_pairs = std::collections::BTreeMap::new();
    // The identity of each key, its string form and the key as written.
    let mut seen: Vec<(NodeIdentity, String, YamlNode)> = Vec::new();
    let sources = match registry {
        Some(registry) => merge_sources(mapping, registry)?,
        None => Vec::new(),
    };
    for source in std::iter::once(mapping).chain(&sources) {
        // The identities of the keys of this mapping, and the keys as
        // written.
        let mut own_keys: Vec<(NodeIdentity, YamlNode)> = Vec::new();
        for (key, value) in source.iter() {
            check_single_node_in_parent(key.syntax())?;
            if registry.is_some() && is_merge_key(&key) {
                continue;
            }
            let identity = key_identity(&key, registry)?;
            if let Some((_, first)) = own_keys.iter().find(|(k, _)| *k == identity) {
                return Err(ResolveError::DuplicateKey {
                    first: first.to_string(),
                    second: key.to_string(),
                });
            }
            own_keys.push((identity.clone(), key.clone()));
            // An earlier mapping, the base or a merged one, shadows this key.
            if seen.iter().any(|(k, _, _)| *k == identity) {
                continue;
            }
            let key_str = record_unique_key(&mut seen, identity, key)?;
            if let Some(yaml_value) = resolved_value(value, registry, expanding)? {
                merged_pairs.insert(key_str, yaml_value);
            }
        }
    }
    Ok(merged_pairs)
}

/// Fail if `slot`, a node that holds a single node such as a VALUE, holds
/// more than one, so converting it would drop the rest.
fn check_single_node(slot: &SyntaxNode) -> Result<(), ResolveError> {
    let extra = slot
        .children()
        .filter(|c| {
            matches!(
                c.kind(),
                SyntaxKind::SCALAR
                    | SyntaxKind::MAPPING
                    | SyntaxKind::SEQUENCE
                    | SyntaxKind::TAGGED_NODE
                    | SyntaxKind::ALIAS
            )
        })
        .nth(1);
    match extra {
        Some(extra) => Err(ResolveError::ExtraNode {
            node: extra.text().to_string(),
        }),
        None => Ok(()),
    }
}

/// Like [`check_single_node`], for the slot `node` sits in.
fn check_single_node_in_parent(node: &SyntaxNode) -> Result<(), ResolveError> {
    match node.parent() {
        Some(parent)
            if matches!(
                parent.kind(),
                SyntaxKind::DOCUMENT
                    | SyntaxKind::KEY
                    | SyntaxKind::VALUE
                    | SyntaxKind::SEQUENCE_ENTRY
                    | SyntaxKind::TAGGED_NODE
            ) =>
        {
            check_single_node(&parent)
        }
        _ => Ok(()),
    }
}

/// Turn `node` into a [`YamlValue`], following aliases and applying merge
/// keys at every level.
///
/// Without a registry, as for [`YamlValue::cast`], an alias is a
/// [`ResolveError::UnresolvedAlias`] and merge keys are not applied.
///
/// `expanding` holds the anchored nodes currently being expanded, so an alias
/// that refers back to one of them is reported as recursive.
pub(crate) fn resolved_value(
    node: crate::as_yaml::YamlNode,
    registry: Option<&AnchorRegistry>,
    expanding: &mut Vec<SyntaxNode>,
) -> Result<Option<YamlValue>, ResolveError> {
    use crate::as_yaml::YamlNode;

    check_single_node_in_parent(node.syntax())?;
    match node {
        YamlNode::Alias(alias) => {
            let name = alias.name();
            let Some(registry) = registry else {
                return Err(ResolveError::UnresolvedAlias { alias: name });
            };
            let target = registry
                .resolve_alias(&name, alias.syntax())
                .ok_or_else(|| ResolveError::UndefinedAlias {
                    alias: name.clone(),
                })?;
            if expanding.contains(target) {
                return Err(ResolveError::RecursiveAlias { alias: name });
            }
            let Some(target_node) = YamlNode::from_syntax_peeled(target.clone()) else {
                return Err(ResolveError::UndefinedAlias { alias: name });
            };
            expanding.push(target.clone());
            let value = resolved_value(target_node, Some(registry), expanding);
            expanding.pop();
            value
        }
        YamlNode::Mapping(mapping) => Ok(Some(YamlValue::Mapping(apply_merge_keys(
            &mapping, registry, expanding,
        )?))),
        YamlNode::Sequence(seq) => {
            let mut items = Vec::new();
            for item in seq.values() {
                items.extend(resolved_value(item, registry, expanding)?);
            }
            Ok(Some(YamlValue::Sequence(items)))
        }
        YamlNode::TaggedNode(tagged) => resolved_tagged(&tagged, registry, expanding),
        YamlNode::Scalar(scalar) => Ok(Some(YamlValue::Scalar(
            crate::scalar::ScalarValue::from_scalar(&scalar),
        ))),
    }
}

/// Resolve a tagged node like [`resolved_value`]: `!!set`, `!!omap` and
/// `!!pairs` get their own [`YamlValue`] variants, other tagged
/// collections are [`YamlValue::Tagged`] and a tagged scalar keeps its tag
/// on the [`ScalarValue`](crate::ScalarValue).
///
/// A tag on an alias, which YAML does not allow but the parser accepts,
/// applies to the node the alias refers to, as if the alias were a copy of
/// it.
fn resolved_tagged(
    tagged: &crate::yaml::TaggedNode,
    registry: Option<&AnchorRegistry>,
    expanding: &mut Vec<SyntaxNode>,
) -> Result<Option<YamlValue>, ResolveError> {
    use crate::as_yaml::YamlNode;

    check_single_node(tagged.syntax())?;
    let content = tagged.syntax().children().find_map(YamlNode::from_syntax);
    let (Some(tag), Some(content)) = (tagged.tag(), content) else {
        return Ok(crate::scalar::ScalarValue::from_tagged(tagged).map(YamlValue::Scalar));
    };
    match content {
        YamlNode::Alias(alias) => resolved_tagged_alias(&tag, &alias, registry, expanding),
        content => resolved_with_tag(&tag, content, registry, expanding),
    }
}

/// Resolve `alias`, which has tag `tag`, to the node it refers to under
/// that tag.
fn resolved_tagged_alias(
    tag: &str,
    alias: &crate::yaml::Alias,
    registry: Option<&AnchorRegistry>,
    expanding: &mut Vec<SyntaxNode>,
) -> Result<Option<YamlValue>, ResolveError> {
    use crate::as_yaml::YamlNode;

    let name = alias.name();
    let Some(registry) = registry else {
        return Err(ResolveError::UnresolvedAlias { alias: name });
    };
    let target = registry
        .resolve_alias(&name, alias.syntax())
        .ok_or_else(|| ResolveError::UndefinedAlias {
            alias: name.clone(),
        })?;
    if expanding.contains(target) {
        return Err(ResolveError::RecursiveAlias { alias: name });
    }
    let Some(target_node) = YamlNode::from_syntax_peeled(target.clone()) else {
        return Err(ResolveError::UndefinedAlias { alias: name });
    };
    expanding.push(target.clone());
    let value = match target_node {
        YamlNode::Alias(inner) => resolved_tagged_alias(tag, &inner, Some(registry), expanding),
        // The tag of the alias takes the place of that of the node.
        YamlNode::TaggedNode(tagged) => {
            match tagged.syntax().children().find_map(YamlNode::from_syntax) {
                Some(YamlNode::Alias(inner)) => {
                    resolved_tagged_alias(tag, &inner, Some(registry), expanding)
                }
                Some(content) => resolved_with_tag(tag, content, Some(registry), expanding),
                None => Ok(crate::scalar::ScalarValue::from_tagged(&tagged)
                    .map(|s| YamlValue::Scalar(s.with_resolved_tag(tag.to_string())))),
            }
        }
        content => resolved_with_tag(tag, content, Some(registry), expanding),
    };
    expanding.pop();
    value
}

/// Resolve `content`, an untagged scalar, mapping or sequence, as if it
/// had tag `tag`.
fn resolved_with_tag(
    tag: &str,
    content: crate::as_yaml::YamlNode,
    registry: Option<&AnchorRegistry>,
    expanding: &mut Vec<SyntaxNode>,
) -> Result<Option<YamlValue>, ResolveError> {
    use crate::as_yaml::YamlNode;

    match (tag, content) {
        ("!!set", YamlNode::Mapping(mapping)) => {
            resolved_set(&mapping, registry, expanding).map(|set| Some(YamlValue::Set(set)))
        }
        ("!!set", content) => Err(ResolveError::InvalidSetContent {
            content: content.syntax().text().to_string(),
        }),
        ("!!omap", YamlNode::Sequence(seq)) => Ok(Some(YamlValue::OrderedMapping(resolved_pairs(
            tag, &seq, registry, expanding,
        )?))),
        ("!!pairs", YamlNode::Sequence(seq)) => Ok(Some(YamlValue::Pairs(resolved_pairs(
            tag, &seq, registry, expanding,
        )?))),
        ("!!omap" | "!!pairs", content) => Err(ResolveError::InvalidPairsContent {
            tag: tag.to_string(),
            content: content.syntax().text().to_string(),
        }),
        (_, YamlNode::Scalar(scalar)) => Ok(Some(YamlValue::Scalar(
            crate::scalar::ScalarValue::from_scalar(&scalar).with_resolved_tag(tag.to_string()),
        ))),
        (_, content) => Ok(resolved_value(content, registry, expanding)?
            .map(|value| YamlValue::Tagged(tag.to_string(), Box::new(value)))),
    }
}

/// The members of a `!!set` whose content is `mapping`.
fn resolved_set(
    mapping: &crate::yaml::Mapping,
    registry: Option<&AnchorRegistry>,
    expanding: &mut Vec<SyntaxNode>,
) -> Result<std::collections::BTreeSet<String>, ResolveError> {
    for entry in mapping.entries() {
        let Some(value) = entry.value_node() else {
            continue;
        };
        let is_null = matches!(
            resolved_value(value.clone(), registry, expanding)?,
            Some(YamlValue::Scalar(s)) if s.scalar_type() == crate::scalar::ScalarType::Null
        );
        if !is_null {
            return Err(ResolveError::NonNullSetValue {
                member: entry
                    .key_node()
                    .map_or_else(String::new, |key| key.to_string()),
                value: value.to_string(),
            });
        }
    }
    let mut members = std::collections::BTreeSet::new();
    let mut seen = Vec::new();
    for member in mapping.keys() {
        let Some(YamlValue::Scalar(_)) = resolved_value(member.clone(), registry, expanding)?
        else {
            return Err(ResolveError::InvalidSetMember {
                member: member.to_string(),
            });
        };
        let identity = key_identity(&member, registry)?;
        members.insert(record_unique_key(&mut seen, identity, member)?);
    }
    Ok(members)
}

/// The entries of a `!!omap` or `!!pairs` sequence tagged `tag`, in order.
///
/// Each item has to be a mapping, untagged or tagged `!!map`, with a single
/// entry.
fn resolved_pairs(
    tag: &str,
    seq: &crate::yaml::Sequence,
    registry: Option<&AnchorRegistry>,
    expanding: &mut Vec<SyntaxNode>,
) -> Result<Vec<(String, YamlValue)>, ResolveError> {
    let mut pairs = Vec::new();
    let mut seen = Vec::new();
    for item in seq.values() {
        let entries = match resolved_value(item.clone(), registry, expanding)? {
            Some(YamlValue::Mapping(entries)) => Some(entries),
            Some(YamlValue::Tagged(t, inner)) if t == "!!map" => match *inner {
                YamlValue::Mapping(entries) => Some(entries),
                _ => None,
            },
            _ => None,
        }
        .filter(|entries| entries.len() == 1)
        .ok_or_else(|| ResolveError::InvalidPairsItem {
            tag: tag.to_string(),
            item: item.to_string(),
        })?;
        if tag == "!!omap" {
            let key = pair_key(tag, &item, registry)?;
            record_unique_key(&mut seen, key_identity(&key, registry)?, key)?;
        }
        pairs.extend(entries);
    }
    Ok(pairs)
}

/// The key of `item`, a `!!omap` or `!!pairs` item that converts to a
/// mapping with a single entry, as written.
fn pair_key(
    tag: &str,
    item: &crate::as_yaml::YamlNode,
    registry: Option<&AnchorRegistry>,
) -> Result<crate::as_yaml::YamlNode, ResolveError> {
    use crate::as_yaml::YamlNode;

    let invalid = || ResolveError::InvalidPairsItem {
        tag: tag.to_string(),
        item: item.to_string(),
    };
    let resolved = match registry {
        Some(registry) => resolve_alias_node(item.clone(), registry)?,
        None => item.clone(),
    };
    let mapping = match resolved {
        YamlNode::Mapping(mapping) => mapping,
        YamlNode::TaggedNode(tagged) => tagged.as_mapping().ok_or_else(invalid)?,
        _ => return Err(invalid()),
    };
    let key = match registry {
        Some(registry) => MergedMapping::new(mapping, registry)?.keys().next(),
        None => mapping.keys().next(),
    };
    key.ok_or_else(invalid)
}

/// Add `key`, as written, with identity `identity`, to the `seen` keys of
/// one mapping, `!!set` or `!!omap`, as (identity, string form, key as
/// written), and return its string form.
///
/// A key equal to one seen before is a [`ResolveError::DuplicateKey`], and
/// a distinct key with the same string form a
/// [`ResolveError::KeyCollision`].
fn record_unique_key(
    seen: &mut Vec<(NodeIdentity, String, crate::as_yaml::YamlNode)>,
    identity: NodeIdentity,
    key: crate::as_yaml::YamlNode,
) -> Result<String, ResolveError> {
    if let Some((_, _, first)) = seen.iter().find(|(k, _, _)| *k == identity) {
        return Err(ResolveError::DuplicateKey {
            first: first.to_string(),
            second: key.to_string(),
        });
    }
    let key_str = identity.key_string();
    if let Some((_, _, first)) = seen.iter().find(|(_, s, _)| *s == key_str) {
        return Err(ResolveError::KeyCollision {
            first: first.to_string(),
            second: key.to_string(),
        });
    }
    seen.push((identity, key_str.clone(), key));
    Ok(key_str)
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
fn merge_sources(
    base: &crate::yaml::Mapping,
    registry: &AnchorRegistry,
) -> Result<Vec<crate::yaml::Mapping>, ResolveError> {
    let mut sources = Vec::new();
    collect_merge_sources(base, registry, &mut sources)?;
    Ok(sources)
}

fn collect_merge_sources(
    base: &crate::yaml::Mapping,
    registry: &AnchorRegistry,
    sources: &mut Vec<crate::yaml::Mapping>,
) -> Result<(), ResolveError> {
    use crate::as_yaml::YamlNode;

    for (key, value) in base.iter() {
        if !is_merge_key(&key) {
            continue;
        }
        let sequence = match &value {
            YamlNode::Sequence(seq) => Some(seq.clone()),
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
            .ok_or_else(|| ResolveError::InvalidMergeOperand {
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
) -> Result<Vec<MergedEntry>, ResolveError> {
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

/// If `node` is an alias, resolve it via `registry`; otherwise return it as-is.
fn resolve_alias_node(
    node: crate::as_yaml::YamlNode,
    registry: &AnchorRegistry,
) -> Result<crate::as_yaml::YamlNode, ResolveError> {
    use crate::as_yaml::YamlNode;

    let YamlNode::Alias(alias) = &node else {
        return Ok(node);
    };
    let target = registry.resolve_alias_through(alias)?;
    YamlNode::from_syntax_peeled(target.clone()).ok_or(ResolveError::UndefinedAlias {
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
        let entries = merged_entries(&mapping, &registry)?;
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

    /// `get_resolved(key)` on `yaml` as `(key, value)` pairs, with each
    /// value rendered through `Display`.
    fn resolved_pairs(yaml: &str, key: &str) -> Vec<(String, String)> {
        let resolved = doc(yaml).get_resolved(key).unwrap().expect("key resolves");
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
        let merged = m.merged(&reg).unwrap();
        merged
            .iter()
            .map(|(k, v)| (k.to_string(), v.to_string()))
            .collect()
    }

    #[test]
    fn get_resolved_merge_shadows_by_key_value() {
        // 16 and 0x10 are the same int, ~ and null the same null.
        let yaml = "a: &a {0x10: merged, ~: merged}\nm: {<<: *a, 16: direct, null: direct}\n";
        assert_eq!(
            resolved_pairs(yaml, "m"),
            pairs(&[("16", "direct"), ("null", "direct")])
        );
        assert_eq!(
            merged_pairs(yaml, "m"),
            pairs(&[("16", "direct"), ("null", "direct")])
        );
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
                doc(yaml).get_resolved(key).unwrap(),
                doc(expected).get_resolved(key).unwrap(),
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
    fn get_resolved_key_string_is_canonical() {
        // Equal keys have the same string form, however they are written.
        for (yaml, expected) in [
            ("m:\n  ? [a,b]\n  : 1\n", "[a, b]"),
            ("m:\n  ? - a\n    - b\n  : 1\n", "[a, b]"),
            ("m: {[a, b]: 1}\n", "[a, b]"),
            ("m:\n  ? - a\n    # c\n  : 1\n", "[a]"),
            ("m:\n  ? {y: 2, x: 1}\n  : 1\n", "{x: 1, y: 2}"),
            ("m:\n  ? y: 2\n    x: 1\n  : 1\n", "{x: 1, y: 2}"),
            ("m: {['a', \"b\"]: 1}\n", "[a, b]"),
            (
                "m: {[0x1, !!str a, ~, True, 1.50]: 1}\n",
                "[1, a, null, true, 1.5]",
            ),
            ("m: {['1', 'a,b', '']: 1}\n", "['1', \"a,b\", '']"),
            ("m: {[!!int '1', !t a, !!int x]: 1}\n", "[1, !t a, !!int x]"),
            ("m: {!t [a]: 1}\n", "!t [a]"),
            ("m: {!!seq [a]: 1}\n", "[a]"),
            ("m: {[{b: 1, a: 2}, []]: 1}\n", "[{a: 2, b: 1}, []]"),
            ("x: &x a\nm: {[*x]: 1}\n", "[a]"),
            ("m: {'a': 1}\n", "a"),
            ("m: {\"a\": 1}\n", "a"),
            ("m: {0x1: 1}\n", "1"),
            ("m: {!!int 1: 1}\n", "1"),
            ("m: {!!str 1: 1}\n", "1"),
            ("m: {~: 1}\n", "null"),
            ("m: {True: 1}\n", "true"),
            ("m: {.INF: 1}\n", ".inf"),
            ("m: {!t a: 1}\n", "a"),
        ] {
            assert_eq!(
                resolved_pairs(yaml, "m"),
                pairs(&[(expected, "1")]),
                "{yaml:?}"
            );
        }
    }

    #[test]
    fn get_resolved_equal_collection_keys() {
        for (yaml, first, second) in [
            (
                "m:\n  ? [a, b]\n  : 1\n  ? [a,b]\n  : 2\n",
                "[a, b]",
                "[a,b]",
            ),
            ("m: {[0x1]: 1, [1]: 2}\n", "[0x1]", "[1]"),
            ("x: &x a\nm: {[*x]: 1, [a]: 2}\n", "[*x]", "[a]"),
            ("x: &x a\nm: {[*x]: 1, [*x]: 2}\n", "[*x]", "[*x]"),
        ] {
            assert_eq!(
                doc(yaml).get_resolved("m"),
                Err(ResolveError::DuplicateKey {
                    first: first.to_string(),
                    second: second.to_string(),
                }),
                "{yaml:?}"
            );
        }
        // A direct key shadows an equal merged one, however it is written.
        for yaml in [
            "s: &s {[a, b]: 1}\nm: {<<: *s, [a,b]: 2}\n",
            "s: &s {[a, b]: 1}\nm:\n  <<: *s\n  ? - a\n    - b\n  : 2\n",
        ] {
            assert_eq!(
                resolved_pairs(yaml, "m"),
                pairs(&[("[a, b]", "2")]),
                "{yaml:?}"
            );
        }
        // A string spelled like a collection is a different key.
        assert_eq!(
            doc("m: {[a, b]: 1, '[a, b]': 2}\n").get_resolved("m"),
            Err(ResolveError::KeyCollision {
                first: "[a, b]".to_string(),
                second: "'[a, b]'".to_string(),
            })
        );
        assert_eq!(
            doc("m: &a {[*a]: 1}\n").get_resolved("m"),
            Err(ResolveError::RecursiveAlias {
                alias: "a".to_string()
            })
        );
    }

    #[test]
    fn merged_alias_key_is_the_node_it_refers_to() {
        let d = doc("x: &a k\ns: &s {k: 1, j: 3}\nm: {<<: *s, *a : 2}\n");
        let reg = d.build_anchor_registry();
        let m = d.as_mapping().unwrap().get_mapping("m").unwrap();
        let merged = m.merged(&reg).unwrap();
        assert_eq!(merged.len(), 2);
        assert_eq!(merged.get("k").unwrap().to_i64(), Some(2));
        assert_eq!(merged.get("j").unwrap().to_i64(), Some(3));
        let keys: Vec<_> = merged.keys().map(|k| k.to_string()).collect();
        assert_eq!(keys, vec!["*a", "j"]);
    }

    #[test]
    fn get_resolved_missing_key() {
        assert_eq!(doc("a: 1\n").get_resolved("b"), Ok(None));
    }

    #[test]
    fn get_resolved_undefined_alias() {
        let undefined = Err(ResolveError::UndefinedAlias {
            alias: "nope".to_string(),
        });
        assert_eq!(doc("b: *nope\n").get_resolved("b"), undefined);
        assert_eq!(doc("m: {a: *nope}\n").get_resolved("m"), undefined);
        assert_eq!(doc("m: [1, *nope]\n").get_resolved("m"), undefined);
    }

    #[test]
    fn get_resolved_recursive_alias() {
        assert_eq!(
            doc("a: &a [*a]\n").get_resolved("a"),
            Err(ResolveError::RecursiveAlias {
                alias: "a".to_string()
            })
        );
    }

    #[test]
    fn get_resolved_redefined_anchor_is_not_recursive() {
        assert_eq!(
            doc("a: &x [&x 1, *x]\nb: *x\n").get_resolved("a"),
            Ok(Some(YamlValue::Sequence(vec![
                YamlValue::from(1),
                YamlValue::from(1)
            ])))
        );
    }

    #[test]
    fn get_resolved_invalid_merge() {
        assert_eq!(
            doc("m: {<<: *nope}\n").get_resolved("m"),
            Err(ResolveError::UndefinedAlias {
                alias: "nope".to_string()
            })
        );
        assert_eq!(
            doc("a: &a 5\nm: {<<: *a}\n").get_resolved("m"),
            Err(ResolveError::InvalidMergeOperand {
                operand: "*a".to_string()
            })
        );
        assert_eq!(
            doc("m: {<<: [3]}\n").get_resolved("m"),
            Err(ResolveError::InvalidMergeOperand {
                operand: "3".to_string()
            })
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
            let resolved = d.get_resolved("m").unwrap().unwrap();
            let keys: Vec<&str> = resolved
                .as_mapping()
                .unwrap()
                .keys()
                .map(String::as_str)
                .collect();
            assert_eq!(keys, vec!["<<", "j"], "{yaml:?}");

            let reg = d.build_anchor_registry();
            let m = d.as_mapping().unwrap().get_mapping("m").unwrap();
            let merged = m.merged(&reg).unwrap();
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
        assert_eq!(m.merged(&reg).unwrap().len(), 3);
        assert_eq!(
            m.merged(&reg).unwrap().get(1).unwrap().to_string(),
            "from_src"
        );
        assert_eq!(
            m.merged(&reg).unwrap().get("1").unwrap().to_string(),
            "base"
        );
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
            let resolved = doc.get_resolved("a").unwrap().expect("a resolves");
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
            let merged = m.merged(&registry).unwrap();
            let resolved = doc.get_resolved("m").unwrap().expect("m resolves");

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
        let m = prod.merged(&reg).unwrap();

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
        let m = prod.merged(&reg).unwrap();

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
        let m = inner.merged(&reg).unwrap();

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
        let m = inner.merged(&reg).unwrap();

        // x only in a
        assert_eq!(m.get("x").unwrap().to_i64(), Some(1));
        // y in both - a wins (first listed)
        assert_eq!(m.get("y").unwrap().to_i64(), Some(1));
        // z only in b
        assert_eq!(m.get("z").unwrap().to_i64(), Some(2));
    }

    #[test]
    fn tagged_merge_operands_are_mappings() {
        use crate::path::YamlPath;
        for yaml in [
            "a: &a !!map {k: 1}\nm: {<<: *a}\n",
            "m: {<<: !!map {k: 1}}\n",
            "a: &a !!map {k: 1}\nm: {<<: [*a]}\n",
            "m: {<<: [!!map {k: 1}]}\n",
        ] {
            assert_eq!(resolved_pairs(yaml, "m"), pairs(&[("k", "1")]), "{yaml:?}");
            assert_eq!(merged_pairs(yaml, "m"), pairs(&[("k", "1")]), "{yaml:?}");
            assert_eq!(
                doc(yaml).try_get_path("m.k").unwrap().to_string(),
                "1",
                "{yaml:?}"
            );
        }
    }

    #[test]
    fn merged_undefined_alias_value_is_error() {
        let undefined = ResolveError::UndefinedAlias {
            alias: "nope".to_string(),
        };
        for yaml in ["m: {a: *nope}\n", "s: &s {a: *nope}\nm: {<<: *s}\n"] {
            let d = doc(yaml);
            let reg = d.build_anchor_registry();
            let m = d.as_mapping().unwrap().get_mapping("m").unwrap();
            assert_eq!(m.merged(&reg).unwrap_err(), undefined, "{yaml:?}");
        }
        // A shadowed alias is never looked at.
        let d = doc("s: &s {a: *nope}\nm: {<<: *s, a: 1}\n");
        let reg = d.build_anchor_registry();
        let m = d.as_mapping().unwrap().get_mapping("m").unwrap();
        assert_eq!(m.merged(&reg).unwrap().get("a").unwrap().to_i64(), Some(1));
    }

    #[test]
    fn merged_invalid_merge_operand_is_error() {
        let d = doc("a: &a 5\nm:\n  <<: *nope\n  a: 1\nn:\n  <<: [*a]\n");
        let reg = d.build_anchor_registry();
        let root = d.as_mapping().unwrap();
        assert_eq!(
            root.get_mapping("m").unwrap().merged(&reg).unwrap_err(),
            ResolveError::UndefinedAlias {
                alias: "nope".to_string()
            }
        );
        assert_eq!(
            root.get_mapping("n").unwrap().merged(&reg).unwrap_err(),
            ResolveError::InvalidMergeOperand {
                operand: "*a".to_string()
            }
        );
        assert_eq!(
            root.merged(&reg).unwrap().get_merged("m").unwrap_err(),
            ResolveError::UndefinedAlias {
                alias: "nope".to_string()
            }
        );
        assert_eq!(
            doc("<<: *nope\n").merged().unwrap_err(),
            ResolveError::UndefinedAlias {
                alias: "nope".to_string()
            }
        );
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
        let m = inner.merged(&reg).unwrap();

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
        let m = inner.merged(&reg).unwrap();

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
        let m = inner.merged(&reg).unwrap();

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
        let m = prod.merged(&reg).unwrap();

        let inner = m.get_merged("inner").unwrap().unwrap();
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
        let view = d.merged().unwrap().unwrap();

        // Top-level keys visible
        assert!(view.contains_key("defaults"));
        assert!(view.contains_key("shared"));

        // Drill into merged child via as_mapping()
        let m = view.as_mapping();
        let shared = m.get_merged("shared").unwrap().unwrap();
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
        let _ = inner.merged(&reg).unwrap().get("a");
        // Original document text should round-trip unchanged.
        assert_eq!(d.to_string(), yaml);
    }

    #[test]
    fn merged_alias_uses_preceding_anchor_definition() {
        let d = doc("a: &x 1\nb: *x\nc: &x 2\nd: *x\n");
        let view = d.merged().unwrap().unwrap();
        assert_eq!(view.get("b").unwrap().to_i64(), Some(1));
        assert_eq!(view.get("d").unwrap().to_i64(), Some(2));
    }

    #[test]
    fn merged_merge_key_uses_preceding_anchor_definition() {
        let d = doc("a: &x {k: 1}\nm:\n  <<: *x\nc: &x {k: 2}\n");
        let view = d.merged().unwrap().unwrap();
        let m = view.as_mapping().get_merged("m").unwrap().unwrap();
        assert_eq!(m.get("k").unwrap().to_i64(), Some(1));
    }

    #[test]
    fn merged_alias_ignores_later_anchor() {
        assert_eq!(
            doc("b: *x\na: &x 1\n").merged().unwrap_err(),
            ResolveError::UndefinedAlias {
                alias: "x".to_string()
            }
        );
    }

    #[test]
    fn registry_resolve_returns_last_definition() {
        let d = doc("a: &x 1\nc: &x 2\n");
        let reg = d.build_anchor_registry();
        assert_eq!(reg.resolve("x").unwrap().text().to_string(), "2");
    }

    #[test]
    fn anchor_after_tag_covers_the_tag() {
        // `!!str &x` and `&x !!str` are the same node properties.
        for src in ["a: !!str &x 1\nb: *x\n", "a: &x !!str 1\nb: *x\n"] {
            let d = doc(src);
            assert_eq!(
                d.get_resolved("b"),
                doc("b: !!str 1\n").get_resolved("b"),
                "{src:?}"
            );
        }
    }

    #[test]
    fn merge_key_from_tagged_mapping() {
        for src in [
            "a: !!map &x\n  k: 1\nb:\n  <<: *x\n",
            "a: &x !!map\n  k: 1\nb:\n  <<: *x\n",
        ] {
            let d = doc(src);
            let view = d.merged().unwrap().unwrap();
            let b = view.as_mapping().get_merged("b").unwrap().unwrap();
            assert_eq!(b.get("k").unwrap().to_i64(), Some(1), "{src:?}");
        }
    }

    #[test]
    fn get_resolved_key_collision_is_error() {
        let cases = [
            ("m: {1: int, '1': str}\n", "1", "'1'"),
            ("m: {a: {'true': str, true: bool}}\n", "'true'", "true"),
            (
                "s: &s {1: from_src}\nm:\n  <<: *s\n  '1': base\n",
                "'1'",
                "1",
            ),
            (
                "s: &s {1: a}\nt: &t {'1': b}\nm: {<<: [*s, *t]}\n",
                "1",
                "'1'",
            ),
            ("s: &s {1: a}\nm: [{<<: *s, '1': b}]\n", "'1'", "1"),
        ];
        for (yaml, first, second) in cases {
            assert_eq!(
                doc(yaml).get_resolved("m"),
                Err(ResolveError::KeyCollision {
                    first: first.to_string(),
                    second: second.to_string(),
                }),
                "{yaml:?}"
            );
        }
    }

    #[test]
    fn get_resolved_duplicate_key_is_error() {
        for (yaml, first, second) in [
            ("m: {a: 1, a: 2}\n", "a", "a"),
            ("m:\n  a: 1\n  b: 2\n  'a': 3\n", "a", "'a'"),
            ("m: {16: a, 0x10: b}\n", "16", "0x10"),
            ("m: {a: {~: 1, null: 2}}\n", "~", "null"),
            ("x: &k a\nm: {a: 1, *k : 2}\n", "a", "*k"),
            ("x: &k a\nm: {*k : 1, *k : 2}\n", "*k", "*k"),
            // Within a merged mapping, too.
            ("s: &s {a: 1, a: 2}\nm: {<<: *s, b: 3}\n", "a", "a"),
            ("s: &s {a: 1, a: 2}\nm: {<<: *s, a: 3}\n", "a", "a"),
            ("m: [{a: 1, a: 2}]\n", "a", "a"),
        ] {
            assert_eq!(
                doc(yaml).get_resolved("m"),
                Err(ResolveError::DuplicateKey {
                    first: first.to_string(),
                    second: second.to_string(),
                }),
                "{yaml:?}"
            );
        }
        // A direct key shadowing a merged one is not a duplicate, nor are
        // keys shared between merged mappings.
        assert_eq!(
            resolved_pairs(
                "s: &s {a: 1, b: 1}\nt: &t {a: 2, c: 2}\nm: {b: 3, <<: [*s, *t], c: 3}\n",
                "m"
            ),
            pairs(&[("a", "1"), ("b", "3"), ("c", "3")])
        );
        assert_eq!(
            ResolveError::DuplicateKey {
                first: "a".to_string(),
                second: "'a'".to_string()
            }
            .to_string(),
            "duplicate mapping key: \"'a'\" equals \"a\""
        );
    }

    #[test]
    fn get_resolved_equal_keys_are_not_a_collision() {
        // A key overriding an equal merged key, however it is spelled, is
        // the normal merge semantics.
        let yaml = "s: &s {1: a, 0x10: b, x: c}\nm: {<<: *s, 1: d, 16: e, 'x': f}\n";
        assert_eq!(
            resolved_pairs(yaml, "m"),
            pairs(&[("1", "d"), ("16", "e"), ("x", "f")])
        );
    }

    #[test]
    fn key_collision_display() {
        let e = ResolveError::KeyCollision {
            first: "1".to_string(),
            second: "'1'".to_string(),
        };
        assert_eq!(
            e.to_string(),
            "distinct keys \"1\" and \"'1'\" have the same string form"
        );
    }

    fn mapping(entries: Vec<(&str, YamlValue)>) -> YamlValue {
        YamlValue::Mapping(
            entries
                .into_iter()
                .map(|(k, v)| (k.to_string(), v))
                .collect(),
        )
    }

    fn pair_list(entries: Vec<(&str, YamlValue)>) -> Vec<(String, YamlValue)> {
        entries
            .into_iter()
            .map(|(k, v)| (k.to_string(), v))
            .collect()
    }

    #[test]
    fn get_resolved_tagged_collections() {
        let k1 = || mapping(vec![("k", YamlValue::from(1))]);
        for (yaml, expected) in [
            (
                "s: &s {k: 1}\nm: !!map {<<: *s, j: *s}\n",
                YamlValue::tagged(
                    "!!map",
                    mapping(vec![("j", k1()), ("k", YamlValue::from(1))]),
                ),
            ),
            (
                "s: &s {k: 1}\nm: !!seq [*s, 2]\n",
                YamlValue::tagged("!!seq", YamlValue::Sequence(vec![k1(), YamlValue::from(2)])),
            ),
            (
                "s: &s {k: 1}\nm: {a: !local [*s]}\n",
                mapping(vec![(
                    "a",
                    YamlValue::tagged("!local", YamlValue::Sequence(vec![k1()])),
                )]),
            ),
            (
                "s: &s {k: 1}\nt: &t !!map {a: *s}\nm: [*t]\n",
                YamlValue::Sequence(vec![YamlValue::tagged("!!map", mapping(vec![("a", k1())]))]),
            ),
            (
                "x: &x a\nm: !!set {? *x, ? b}\n",
                YamlValue::Set(["a", "b"].map(String::from).into()),
            ),
            (
                "s: &s {k: 1}\nm: !!omap [z: *s, a: 2]\n",
                YamlValue::OrderedMapping(pair_list(vec![("z", k1()), ("a", YamlValue::from(2))])),
            ),
            (
                "s: &s {k: 1}\nm: !!pairs [z: *s, z: 2]\n",
                YamlValue::Pairs(pair_list(vec![("z", k1()), ("z", YamlValue::from(2))])),
            ),
            (
                "e: &e {z: 1}\nm: !!omap [*e, a: 2]\n",
                YamlValue::OrderedMapping(pair_list(vec![
                    ("z", YamlValue::from(1)),
                    ("a", YamlValue::from(2)),
                ])),
            ),
        ] {
            assert_eq!(doc(yaml).get_resolved("m"), Ok(Some(expected)), "{yaml:?}");
        }
    }

    #[test]
    fn get_resolved_tagged_collection_errors() {
        let undefined = Err(ResolveError::UndefinedAlias {
            alias: "nope".to_string(),
        });
        for yaml in [
            "m: !!map {a: *nope}\n",
            "m: !!seq [*nope]\n",
            "m: !!set {? *nope}\n",
            "m: !!omap [a: *nope]\n",
            "m: !!pairs [a: *nope]\n",
            "m: !!map {<<: *nope}\n",
        ] {
            assert_eq!(doc(yaml).get_resolved("m"), undefined, "{yaml:?}");
        }
        assert_eq!(
            doc("m: &a !!seq [*a]\n").get_resolved("m"),
            Err(ResolveError::RecursiveAlias {
                alias: "a".to_string()
            })
        );
        assert_eq!(
            doc("m: !!map {<<: 3}\n").get_resolved("m"),
            Err(ResolveError::InvalidMergeOperand {
                operand: "3".to_string()
            })
        );
    }

    #[test]
    fn get_resolved_invalid_set_member() {
        for (yaml, member) in [
            ("m: !!set {? [a], ? b}\n", "[a]"),
            ("m: !!set {? {a: 1}}\n", "{a: 1}"),
            ("x: &x [a]\nm: !!set {? *x}\n", "*x"),
        ] {
            assert_eq!(
                doc(yaml).get_resolved("m"),
                Err(ResolveError::InvalidSetMember {
                    member: member.to_string()
                }),
                "{yaml:?}"
            );
        }
    }

    #[test]
    fn get_resolved_set_content_must_be_mapping() {
        for (yaml, content) in [
            ("m: !!set [a, b]\n", "[a, b]"),
            ("m: !!set &s [a]\n", "[a]"),
            ("m: !!set\n- a\n", "- a\n"),
            ("m: !!set\nn: 1\n", ""),
        ] {
            assert_eq!(
                doc(yaml).get_resolved("m"),
                Err(ResolveError::InvalidSetContent {
                    content: content.to_string()
                }),
                "{yaml:?}"
            );
        }
        assert_eq!(
            ResolveError::InvalidSetContent {
                content: "[a]".to_string()
            }
            .to_string(),
            "!!set content \"[a]\" is not a mapping"
        );
    }

    #[test]
    fn get_resolved_set_value_must_be_null() {
        for (yaml, member, value) in [
            ("m: !!set {a: 1}\n", "a", "1"),
            ("m: !!set\n  ? a\n  b: x\n", "b", "x"),
            ("m: !!set\n  ? b\n  ? a\n  : x\n", "a", "x"),
            ("m: !!set {a: ~, b: 'null'}\n", "b", "'null'"),
            ("m: !!set {a: [~]}\n", "a", "[~]"),
            ("x: &x 1\nm: !!set {a: *x}\n", "a", "*x"),
        ] {
            assert_eq!(
                doc(yaml).get_resolved("m"),
                Err(ResolveError::NonNullSetValue {
                    member: member.to_string(),
                    value: value.to_string()
                }),
                "{yaml:?}"
            );
        }
        for yaml in [
            "m: !!set {a: ~, b: null, c: , ? d, e: !!null ''}\n",
            "m: !!set\n  ? a\n  b:\n  c: ~\n  ? d\n  e: !!null ''\n",
            "n: &n ~\nm: !!set {a: *n, b: , c: *n, ? d, e: *n}\n",
        ] {
            assert_eq!(
                doc(yaml).get_resolved("m"),
                Ok(Some(YamlValue::Set(
                    ["a", "b", "c", "d", "e"].map(String::from).into()
                ))),
                "{yaml:?}"
            );
        }
        assert_eq!(
            ResolveError::NonNullSetValue {
                member: "a".to_string(),
                value: "1".to_string()
            }
            .to_string(),
            "!!set member \"a\" has non-null value \"1\""
        );
    }

    #[test]
    fn get_resolved_invalid_pairs_item() {
        for (yaml, tag, item) in [
            ("m: !!omap [a]\n", "!!omap", "a"),
            ("m: !!omap [[a]]\n", "!!omap", "[a]"),
            ("m: !!omap [{}]\n", "!!omap", "{}"),
            ("m: !!pairs [{b: 1, c: 2}]\n", "!!pairs", "{b: 1, c: 2}"),
            ("m: !!pairs [!local {a: 1}]\n", "!!pairs", "!local {a: 1}"),
            ("e: &e {a: 1, b: 2}\nm: !!omap [*e]\n", "!!omap", "*e"),
            (
                "e: &e {a: 1}\nm: !!omap [{<<: *e, b: 2}]\n",
                "!!omap",
                "{<<: *e, b: 2}",
            ),
        ] {
            assert_eq!(
                doc(yaml).get_resolved("m"),
                Err(ResolveError::InvalidPairsItem {
                    tag: tag.to_string(),
                    item: item.to_string()
                }),
                "{yaml:?}"
            );
        }
    }

    #[test]
    fn get_resolved_pairs_content_must_be_sequence() {
        for (yaml, tag, content) in [
            ("m: !!omap {a: 1}\n", "!!omap", "{a: 1}"),
            ("m: !!pairs &p {a: 1}\n", "!!pairs", "{a: 1}"),
            ("m: !!omap\n  a: 1\n", "!!omap", "a: 1\n"),
            ("m: !!pairs\n  3\n", "!!pairs", "3"),
        ] {
            assert_eq!(
                doc(yaml).get_resolved("m"),
                Err(ResolveError::InvalidPairsContent {
                    tag: tag.to_string(),
                    content: content.to_string()
                }),
                "{yaml:?}"
            );
        }
        assert_eq!(
            ResolveError::InvalidPairsContent {
                tag: "!!omap".to_string(),
                content: "{a: 1}".to_string()
            }
            .to_string(),
            "!!omap content \"{a: 1}\" is not a sequence"
        );
    }

    #[test]
    fn get_resolved_repeated_set_member_or_omap_key() {
        for (yaml, first, second) in [
            ("m: !!set {? a, ? a}\n", "a", "a"),
            ("m: !!set\n  ? a\n  ? b\n  ? 'a'\n", "a", "'a'"),
            ("m: !!set {? 16, ? 0x10}\n", "16", "0x10"),
            ("x: &k a\nm: !!set {? a, ? *k}\n", "a", "*k"),
            ("m: !!omap [a: 1, a: 2]\n", "a", "a"),
            ("m: !!omap\n  - a: 1\n  - b: 2\n  - 'a': 3\n", "a", "'a'"),
            ("m: !!omap [16: a, 0x10: b]\n", "16", "0x10"),
            ("x: &k a\nm: !!omap [a: 1, *k : 2]\n", "a", "*k"),
            ("e: &e {a: 1}\nm: !!omap [*e, a: 2]\n", "a", "a"),
            ("e: &e {a: 1}\nm: !!omap [{<<: *e}, a: 2]\n", "a", "a"),
        ] {
            assert_eq!(
                doc(yaml).get_resolved("m"),
                Err(ResolveError::DuplicateKey {
                    first: first.to_string(),
                    second: second.to_string()
                }),
                "{yaml:?}"
            );
        }
        for (yaml, first, second) in [
            ("m: !!set {? 1, ? '1'}\n", "1", "'1'"),
            ("m: !!omap [1: a, '1': b]\n", "1", "'1'"),
        ] {
            assert_eq!(
                doc(yaml).get_resolved("m"),
                Err(ResolveError::KeyCollision {
                    first: first.to_string(),
                    second: second.to_string()
                }),
                "{yaml:?}"
            );
        }
        // !!pairs allows a key to repeat.
        assert_eq!(
            doc("m: !!pairs [a: 1, a: 2]\n").get_resolved("m"),
            Ok(Some(YamlValue::Pairs(pair_list(vec![
                ("a", YamlValue::from(1)),
                ("a", YamlValue::from(2))
            ]))))
        );
    }

    #[test]
    fn get_resolved_alias_key() {
        for (yaml, expected) in [
            ("x: &a k\nm: {*a : 1}\n", vec![("k", "1")]),
            ("x: &a !!int 5\nm:\n  *a : 1\n", vec![("5", "1")]),
            (
                "x: &a k\nm: {*a : 1, j: &a i, *a : 3}\n",
                vec![("i", "3"), ("j", "i"), ("k", "1")],
            ),
            (
                "x: &a k\ns: &s {*a : 1}\nm: {<<: *s, j: 2}\n",
                vec![("j", "2"), ("k", "1")],
            ),
            // An alias to a collection is a collection key.
            ("x: &a [p,q]\nm: {*a : 1}\n", vec![("[p, q]", "1")]),
        ] {
            assert_eq!(resolved_pairs(yaml, "m"), pairs(&expected), "{yaml:?}");
        }
        assert_eq!(
            doc("x: &a 1\nm: {*a : a, '1': b}\n").get_resolved("m"),
            Err(ResolveError::KeyCollision {
                first: "*a".to_string(),
                second: "'1'".to_string()
            })
        );
        assert_eq!(
            doc("m: {*nope : 1}\n").get_resolved("m"),
            Err(ResolveError::UndefinedAlias {
                alias: "nope".to_string()
            })
        );
    }

    #[test]
    fn get_resolved_pairs_item_tagged_map() {
        assert_eq!(
            doc("m: !!omap [!!map {a: 1}]\n").get_resolved("m"),
            Ok(Some(YamlValue::OrderedMapping(pair_list(vec![(
                "a",
                YamlValue::from(1)
            )]))))
        );
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

    #[test]
    fn get_resolved_tagged_alias_errors() {
        for (yaml, expected) in [
            (
                "a: !t *nope\n",
                ResolveError::UndefinedAlias {
                    alias: "nope".to_string(),
                },
            ),
            (
                "a: &a [!t *a]\n",
                ResolveError::RecursiveAlias {
                    alias: "a".to_string(),
                },
            ),
            (
                "x: &x 1\na: !!set *x\n",
                ResolveError::InvalidSetContent {
                    content: "1".to_string(),
                },
            ),
        ] {
            assert_eq!(doc(yaml).get_resolved("a"), Err(expected), "{yaml:?}");
        }
    }

    #[test]
    fn get_resolved_anchored_alias() {
        // An anchor on an alias, which YAML does not allow, names the node
        // the alias refers to.
        for (yaml, key, expected) in [
            ("x: &x 1\nb: &b *x\nc: *b\n", "c", "c: 1\n"),
            ("x: &x 1\ns: [&b *x, *b]\n", "s", "s: [1, 1]\n"),
            ("x: &x 1\nb: &b *x\nc: !t *b\n", "c", "c: !t 1\n"),
            ("x: &x 1\nb: !t &b *x\nc: *b\n", "c", "c: !t 1\n"),
            (
                "x: &x {k: 1}\nb: &b *x\nm: {<<: *b, j: 2}\n",
                "m",
                "m: {k: 1, j: 2}\n",
            ),
        ] {
            assert_eq!(
                doc(yaml).get_resolved(key),
                doc(expected).get_resolved(key),
                "{yaml:?}"
            );
        }
        assert_eq!(
            doc("x: &x 1\nb: &b *x\nc: *b\n").get_resolved("c"),
            Ok(Some(YamlValue::from(1)))
        );
        assert_eq!(
            merged_pairs("x: &x {k: 1}\nb: &b *x\nm: {<<: *b, j: *b}\n", "m"),
            pairs(&[("j", "{k: 1}"), ("k", "1")])
        );
    }

    #[test]
    fn get_resolved_anchored_alias_errors() {
        for (yaml, expected) in [
            (
                "c: &c *c\n",
                ResolveError::RecursiveAlias {
                    alias: "c".to_string(),
                },
            ),
            (
                "b: &b *nope\nc: *b\n",
                ResolveError::UndefinedAlias {
                    alias: "nope".to_string(),
                },
            ),
        ] {
            assert_eq!(doc(yaml).get_resolved("c"), Err(expected), "{yaml:?}");
        }
        let d = doc("b: &b *b\nm: {<<: *b}\n");
        let registry = d.build_anchor_registry();
        let m = d.as_mapping().unwrap().get_mapping("m").unwrap();
        assert_eq!(
            m.merged(&registry).err(),
            Some(ResolveError::RecursiveAlias {
                alias: "b".to_string()
            })
        );
    }

    #[test]
    fn resolve_error_display() {
        assert_eq!(
            ResolveError::InvalidSetMember {
                member: "[a]".to_string()
            }
            .to_string(),
            "!!set member \"[a]\" is not a scalar"
        );
        assert_eq!(
            ResolveError::InvalidPairsItem {
                tag: "!!omap".to_string(),
                item: "a".to_string()
            }
            .to_string(),
            "!!omap item \"a\" is not a mapping with a single entry"
        );
    }
}
