//! Visitor pattern for traversing YAML AST nodes.
//!
//! Allows traversing and processing YAML structures without modifying node types.
//! Useful for validation, transformation, collection, or analysis.
//!
//! # Example
//!
//! ```rust
//! use yaml_edit::visitor::{YamlVisitor, YamlAccept};
//! use yaml_edit::{Document, Scalar, Mapping, Sequence};
//! use std::str::FromStr;
//!
//! struct ScalarCounter {
//!     count: usize,
//! }
//!
//! impl YamlVisitor for ScalarCounter {
//!     fn visit_scalar(&mut self, _scalar: &Scalar) {
//!         self.count += 1;
//!     }
//! }
//!
//! let doc = Document::from_str("key: value\nlist:\n  - item1\n  - item2").unwrap();
//! let mut counter = ScalarCounter { count: 0 };
//! doc.accept(&mut counter);
//! assert_eq!(counter.count, 5); // "key", "value", "list", "item1", "item2"
//! ```
//!
//! Default traversal implementations automatically visit child nodes. Override
//! `visit_mapping`, `visit_sequence` or `visit_tagged_node` for custom
//! traversal logic.
//!
//! A tagged node (`!!str x`, `!custom {a: 1}`) is handed to
//! `visit_tagged_node`, which by default descends into the tagged content, so
//! a visitor that only implements `visit_scalar` still sees the scalar
//! inside `!!str x`.

use crate::yaml::{Document, Mapping, Scalar, Sequence, TaggedNode, YamlFile};

/// Trait for implementing the visitor pattern on YAML nodes.
pub trait YamlVisitor {
    /// Visit a YAML root node
    fn visit_yaml(&mut self, yaml: &YamlFile) {
        // Default implementation visits all documents
        for doc in yaml.documents() {
            self.visit_document(&doc);
        }
    }

    /// Visit a document node
    ///
    /// The default implementation visits the document's root node.
    fn visit_document(&mut self, document: &Document) {
        self.walk_document(document);
    }

    /// Visit the root node of a document (helper for default traversal).
    fn walk_document(&mut self, document: &Document) {
        if let Some(tagged) = document.as_tagged() {
            self.visit_tagged_node(&tagged);
        } else if let Some(mapping) = document.as_mapping() {
            self.visit_mapping(&mapping);
        } else if let Some(sequence) = document.as_sequence() {
            self.visit_sequence(&sequence);
        } else if let Some(scalar) = document.as_scalar() {
            self.visit_scalar(&scalar);
        }
    }

    /// Visit a scalar node
    fn visit_scalar(&mut self, _scalar: &Scalar) {}

    /// Visit a mapping node
    ///
    /// The default implementation traverses all key-value pairs in the mapping,
    /// visiting both keys and values recursively. Override this method if you need
    /// custom logic when encountering mappings.
    fn visit_mapping(&mut self, mapping: &Mapping) {
        self.walk_mapping(mapping);
    }

    /// Visit a sequence node
    ///
    /// The default implementation traverses all items in the sequence recursively.
    /// Override this method if you need custom logic when encountering sequences.
    fn visit_sequence(&mut self, sequence: &Sequence) {
        self.walk_sequence(sequence);
    }

    /// Visit a node carrying an explicit tag (`!!str x`, `!custom {a: 1}`).
    ///
    /// The default implementation descends into the tagged content, so a
    /// visitor that only cares about scalars still sees `!!str x`. Override
    /// this method to inspect the tag itself.
    fn visit_tagged_node(&mut self, tagged: &TaggedNode) {
        self.walk_tagged_node(tagged);
    }

    /// Traverse the content a tag is attached to (helper for default
    /// traversal).
    fn walk_tagged_node(&mut self, tagged: &TaggedNode) {
        if let Some(mapping) = tagged.as_mapping() {
            self.visit_mapping(&mapping);
        } else if let Some(sequence) = tagged.as_sequence() {
            self.visit_sequence(&sequence);
        } else if let Some(scalar) = tagged.value() {
            self.visit_scalar(&scalar);
        }
    }

    /// Traverse all key-value pairs in a mapping (helper for default traversal).
    ///
    /// This method is called by the default `visit_mapping` implementation.
    /// You can call it explicitly if you override `visit_mapping` and want to
    /// preserve the default traversal behavior.
    fn walk_mapping(&mut self, mapping: &Mapping) {
        for (key_node, value_node) in mapping.pairs() {
            visit_node(self, &key_node);
            visit_node(self, &value_node);
        }
    }

    /// Traverse all items in a sequence (helper for default traversal).
    ///
    /// This method is called by the default `visit_sequence` implementation.
    /// You can call it explicitly if you override `visit_sequence` and want to
    /// preserve the default traversal behavior.
    fn walk_sequence(&mut self, sequence: &Sequence) {
        for item in sequence.items() {
            visit_node(self, &item);
        }
    }
}

/// Visit the scalar, tagged node, mapping or sequence `node` is or wraps.
fn visit_node<V: YamlVisitor + ?Sized>(visitor: &mut V, node: &crate::yaml::SyntaxNode) {
    use crate::yaml::extract;

    if let Some(scalar) = extract(node) {
        visitor.visit_scalar(&scalar);
    } else if let Some(tagged) = extract(node) {
        visitor.visit_tagged_node(&tagged);
    } else if let Some(mapping) = extract(node) {
        visitor.visit_mapping(&mapping);
    } else if let Some(sequence) = extract(node) {
        visitor.visit_sequence(&sequence);
    }
}

/// Trait for nodes that can accept a visitor
pub trait YamlAccept {
    /// Accept a visitor for traversal
    fn accept<V: YamlVisitor>(&self, visitor: &mut V);
}

impl YamlAccept for YamlFile {
    fn accept<V: YamlVisitor>(&self, visitor: &mut V) {
        visitor.visit_yaml(self);
    }
}

impl YamlAccept for Document {
    fn accept<V: YamlVisitor>(&self, visitor: &mut V) {
        visitor.visit_document(self);
    }
}

impl YamlAccept for Scalar {
    fn accept<V: YamlVisitor>(&self, visitor: &mut V) {
        visitor.visit_scalar(self);
    }
}

impl YamlAccept for Mapping {
    fn accept<V: YamlVisitor>(&self, visitor: &mut V) {
        visitor.visit_mapping(self);
    }
}

impl YamlAccept for Sequence {
    fn accept<V: YamlVisitor>(&self, visitor: &mut V) {
        visitor.visit_sequence(self);
    }
}

/// A visitor that collects all scalar values from a YAML document
#[derive(Debug, Clone)]
pub struct ScalarCollector {
    /// The collected scalar values
    pub scalars: Vec<String>,
}

impl ScalarCollector {
    /// Create a new scalar collector
    pub fn new() -> Self {
        Self {
            scalars: Vec::new(),
        }
    }
}

impl Default for ScalarCollector {
    fn default() -> Self {
        Self::new()
    }
}

impl YamlVisitor for ScalarCollector {
    fn visit_scalar(&mut self, scalar: &Scalar) {
        self.scalars.push(scalar.to_string());
    }
    // Default traversal methods handle mapping and sequence traversal automatically
}

/// A visitor that counts different types of nodes
#[derive(Debug, Clone)]
pub struct NodeCounter {
    /// Number of document nodes encountered
    pub document_count: usize,
    /// Number of scalar nodes encountered
    pub scalar_count: usize,
    /// Number of mapping nodes encountered
    pub mapping_count: usize,
    /// Number of sequence nodes encountered
    pub sequence_count: usize,
}

impl NodeCounter {
    /// Create a new node counter
    pub fn new() -> Self {
        Self {
            document_count: 0,
            scalar_count: 0,
            mapping_count: 0,
            sequence_count: 0,
        }
    }

    /// Get total node count
    pub fn total(&self) -> usize {
        self.document_count + self.scalar_count + self.mapping_count + self.sequence_count
    }
}

impl Default for NodeCounter {
    fn default() -> Self {
        Self::new()
    }
}

impl YamlVisitor for NodeCounter {
    fn visit_document(&mut self, document: &Document) {
        self.document_count += 1;
        self.walk_document(document);
    }

    fn visit_scalar(&mut self, _scalar: &Scalar) {
        self.scalar_count += 1;
    }

    fn visit_mapping(&mut self, mapping: &Mapping) {
        self.mapping_count += 1;
        // Call default traversal to visit children
        self.walk_mapping(mapping);
    }

    fn visit_sequence(&mut self, sequence: &Sequence) {
        self.sequence_count += 1;
        // Call default traversal to visit children
        self.walk_sequence(sequence);
    }
}

/// A visitor that transforms scalar values
pub struct ScalarTransformer<F>
where
    F: FnMut(&str) -> String,
{
    transform: F,
    transformed: Vec<(String, String)>, // (original, transformed) pairs
}

impl<F> ScalarTransformer<F>
where
    F: FnMut(&str) -> String,
{
    /// Create a new scalar transformer with the given transformation function
    pub fn new(transform: F) -> Self {
        Self {
            transform,
            transformed: Vec::new(),
        }
    }

    /// Get the transformed pairs
    pub fn results(&self) -> &[(String, String)] {
        &self.transformed
    }
}

impl<F> YamlVisitor for ScalarTransformer<F>
where
    F: FnMut(&str) -> String,
{
    fn visit_scalar(&mut self, scalar: &Scalar) {
        let original = scalar.to_string();
        let transformed = (self.transform)(&original);
        self.transformed.push((original, transformed));
    }

    // Default traversal methods handle mapping and sequence traversal automatically
}
