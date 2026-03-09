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
//! `visit_mapping` or `visit_sequence` for custom traversal logic.

use crate::yaml::{Document, Mapping, Scalar, Sequence, YamlFile};
use rowan::ast::AstNode;

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
    fn visit_document(&mut self, document: &Document) {
        // Default implementation visits the document's content
        if let Some(mapping) = document.as_mapping() {
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

    /// Traverse all key-value pairs in a mapping (helper for default traversal).
    ///
    /// This method is called by the default `visit_mapping` implementation.
    /// You can call it explicitly if you override `visit_mapping` and want to
    /// preserve the default traversal behavior.
    fn walk_mapping(&mut self, mapping: &Mapping) {
        use crate::yaml::{extract_mapping, extract_scalar, extract_sequence};

        for (key_node, value_node) in mapping.pairs() {
            // Visit key
            if let Some(scalar) = extract_scalar(&key_node) {
                self.visit_scalar(&scalar);
            } else if let Some(sequence) = extract_sequence(&key_node) {
                self.visit_sequence(&sequence);
            } else if let Some(mapping) = extract_mapping(&key_node) {
                self.visit_mapping(&mapping);
            }

            // Visit value
            if let Some(scalar) = extract_scalar(&value_node) {
                self.visit_scalar(&scalar);
            } else if let Some(nested_mapping) = extract_mapping(&value_node) {
                self.visit_mapping(&nested_mapping);
            } else if let Some(nested_sequence) = extract_sequence(&value_node) {
                self.visit_sequence(&nested_sequence);
            }
        }
    }

    /// Traverse all items in a sequence (helper for default traversal).
    ///
    /// This method is called by the default `visit_sequence` implementation.
    /// You can call it explicitly if you override `visit_sequence` and want to
    /// preserve the default traversal behavior.
    fn walk_sequence(&mut self, sequence: &Sequence) {
        for item in sequence.items() {
            if let Some(scalar) = Scalar::cast(item.clone()) {
                self.visit_scalar(&scalar);
            } else if let Some(nested_mapping) = Mapping::cast(item.clone()) {
                self.visit_mapping(&nested_mapping);
            } else if let Some(nested_sequence) = Sequence::cast(item.clone()) {
                self.visit_sequence(&nested_sequence);
            }
        }
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
        // Continue visiting children
        if let Some(mapping) = document.as_mapping() {
            self.visit_mapping(&mapping);
        } else if let Some(sequence) = document.as_sequence() {
            self.visit_sequence(&sequence);
        } else if let Some(scalar) = document.as_scalar() {
            self.visit_scalar(&scalar);
        }
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
#[cfg(test)]
mod tests {
    use super::*;
    use crate::YamlFile;

    #[test]
    fn test_scalar_collector() {
        let yaml_text = r#"
name: John Doe
age: 30
address:
  street: 123 Main St
  city: New York
  country: USA
hobbies:
  - reading
  - coding
  - hiking
"#;

        let parsed = YamlFile::parse(yaml_text);
        let yaml = parsed.tree();

        let mut collector = ScalarCollector::new();
        yaml.accept(&mut collector);

        // Should collect all scalar values from the document
        assert_eq!(
            collector.scalars,
            vec![
                "name",
                "John Doe",
                "age",
                "30",
                "address",
                "street",
                "123 Main St",
                "city",
                "New York",
                "country",
                "USA",
                "hobbies",
                "reading",
                "coding",
                "hiking",
            ]
        );
    }

    #[test]
    fn test_node_counter() {
        let yaml_text = r#"
name: John Doe
age: 30
address:
  street: 123 Main St
  city: New York
  country: USA
hobbies:
  - reading
  - coding
  - hiking
"#;

        let parsed = YamlFile::parse(yaml_text);
        let yaml = parsed.tree();

        let mut counter = NodeCounter::new();
        yaml.accept(&mut counter);

        // Should count all node types
        assert_eq!(counter.document_count, 1);
        assert_eq!(counter.scalar_count, 15);
        assert_eq!(counter.mapping_count, 2); // Root mapping and address mapping
        assert_eq!(counter.sequence_count, 1); // hobbies sequence

        // Total should be sum of all counts
        assert_eq!(
            counter.total(),
            counter.document_count
                + counter.scalar_count
                + counter.mapping_count
                + counter.sequence_count
        );
    }

    #[test]
    fn test_scalar_transformer() {
        let yaml_text = r#"
name: john
city: new york
country: usa
"#;

        let parsed = YamlFile::parse(yaml_text);
        let yaml = parsed.tree();

        // Transform all scalars to uppercase
        let mut transformer = ScalarTransformer::new(|s: &str| s.to_uppercase());
        yaml.accept(&mut transformer);

        let results = transformer.results();

        // Check that transformations were applied
        assert_eq!(
            results,
            &[
                ("name".to_string(), "NAME".to_string()),
                ("john".to_string(), "JOHN".to_string()),
                ("city".to_string(), "CITY".to_string()),
                ("new york".to_string(), "NEW YORK".to_string()),
                ("country".to_string(), "COUNTRY".to_string()),
                ("usa".to_string(), "USA".to_string()),
            ]
        );
    }

    #[test]
    fn test_visitor_on_sequence() {
        let yaml_text = r#"
- item1
- item2
- nested:
    - subitem1
    - subitem2
"#;

        let parsed = YamlFile::parse(yaml_text);
        let yaml = parsed.tree();

        let mut collector = ScalarCollector::new();
        yaml.accept(&mut collector);

        // Should collect all scalars including nested ones
        assert_eq!(
            collector.scalars,
            vec!["item1", "item2", "nested", "subitem1", "subitem2"]
        );
    }

    #[test]
    fn test_visitor_on_empty_document() {
        let yaml_text = "";

        let parsed = YamlFile::parse(yaml_text);
        let yaml = parsed.tree();

        let mut counter = NodeCounter::new();
        yaml.accept(&mut counter);

        // Empty document should have no nodes (or minimal structure)
        assert_eq!(counter.total(), 0);
    }

    #[test]
    fn test_custom_visitor() {
        // Define a custom visitor that counts only keys in mappings
        struct KeyCounter {
            key_count: usize,
        }

        impl YamlVisitor for KeyCounter {
            fn visit_scalar(&mut self, _scalar: &crate::Scalar) {
                // Don't count scalars that are not keys
            }

            fn visit_mapping(&mut self, mapping: &crate::Mapping) {
                // Count keys in this mapping
                for (_key, value) in mapping.iter() {
                    self.key_count += 1;
                    // Recursively visit nested structures
                    if let Some(nested_mapping) = value.as_mapping() {
                        nested_mapping.accept(self);
                    } else if let Some(nested_sequence) = value.as_sequence() {
                        nested_sequence.accept(self);
                    }
                }
            }

            fn visit_sequence(&mut self, sequence: &crate::Sequence) {
                // Visit items in sequence to find nested mappings
                for value in sequence.values() {
                    if let Some(nested_mapping) = value.as_mapping() {
                        nested_mapping.accept(self);
                    } else if let Some(nested_sequence) = value.as_sequence() {
                        nested_sequence.accept(self);
                    }
                }
            }
        }

        let yaml_text = r#"
name: John
age: 30
address:
  street: 123 Main St
  city: New York
metadata:
  created: 2024-01-01
  updated: 2024-01-02
"#;

        let parsed = YamlFile::parse(yaml_text);
        let yaml = parsed.tree();

        let mut key_counter = KeyCounter { key_count: 0 };
        yaml.accept(&mut key_counter);

        // Should count: name, age, address, street, city, metadata, created, updated = 8 keys
        assert_eq!(key_counter.key_count, 8);
    }

    #[test]
    fn test_visitor_with_multiple_documents() {
        let yaml_text = r#"
---
doc1: value1
---
doc2: value2
---
doc3: value3
"#;

        let parsed = YamlFile::parse(yaml_text);
        let yaml = parsed.tree();

        let mut counter = NodeCounter::new();
        yaml.accept(&mut counter);

        // Should count 3 documents
        assert_eq!(counter.document_count, 3);
        assert!(counter.scalar_count >= 6); // At least 3 keys and 3 values
    }
}
