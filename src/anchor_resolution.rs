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
