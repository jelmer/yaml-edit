//! Visitor pattern implementation for traversing YAML AST nodes.

use crate::value::YamlValue;
use crate::yaml::{Document, Mapping, Scalar, Sequence, Yaml};
use rowan::ast::AstNode;

/// Trait for implementing the visitor pattern on YAML nodes.
///
/// The visitor pattern allows you to traverse and process YAML structures
/// without modifying the existing node types. This is useful for operations
/// like validation, transformation, collection, or analysis of YAML data.
///
/// # Example
///
/// ```rust
/// use yaml_edit::visitor::{YamlVisitor, YamlAccept};
/// use yaml_edit::{Yaml, Scalar, Mapping, Sequence};
/// use rowan::ast::AstNode;
///
/// struct CountingVisitor {
///     scalar_count: usize,
///     mapping_count: usize,
///     sequence_count: usize,
/// }
///
/// impl YamlVisitor for CountingVisitor {
///     fn visit_scalar(&mut self, _scalar: &Scalar) {
///         self.scalar_count += 1;
///     }
///
///     fn visit_mapping(&mut self, mapping: &Mapping) {
///         self.mapping_count += 1;
///         // Visit children
///         for (key, value) in mapping.pairs() {
///             if let Some(key_node) = key {
///                 // Use smart extraction to handle wrapper nodes automatically
///                 if let Some(scalar) = yaml_edit::extract_scalar(&key_node) {
///                     scalar.accept(self);
///                 } else if let Some(sequence) = yaml_edit::extract_sequence(&key_node) {
///                     sequence.accept(self);
///                 } else if let Some(mapping) = yaml_edit::extract_mapping(&key_node) {
///                     mapping.accept(self);
///                 }
///             }
///             if let Some(value_node) = value {
///                 // Use smart extraction to handle wrapper nodes automatically
///                 if let Some(scalar) = yaml_edit::extract_scalar(&value_node) {
///                     scalar.accept(self);
///                 } else if let Some(nested_mapping) = yaml_edit::extract_mapping(&value_node) {
///                     nested_mapping.accept(self);
///                 } else if let Some(nested_sequence) = yaml_edit::extract_sequence(&value_node) {
///                     nested_sequence.accept(self);
///                 }
///             }
///         }
///     }
///
///     fn visit_sequence(&mut self, sequence: &Sequence) {
///         self.sequence_count += 1;
///         // Visit children
///         for item in sequence.items() {
///             if let Some(scalar) = Scalar::cast(item.clone()) {
///                 scalar.accept(self);
///             } else if let Some(nested_mapping) = Mapping::cast(item.clone()) {
///                 nested_mapping.accept(self);
///             } else if let Some(nested_sequence) = Sequence::cast(item.clone()) {
///                 nested_sequence.accept(self);
///             }
///         }
///     }
/// }
/// ```
pub trait YamlVisitor {
    /// Visit a YAML root node
    fn visit_yaml(&mut self, yaml: &Yaml) {
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
    fn visit_scalar(&mut self, scalar: &Scalar);

    /// Visit a mapping node
    fn visit_mapping(&mut self, mapping: &Mapping);

    /// Visit a sequence node
    fn visit_sequence(&mut self, sequence: &Sequence);

    /// Visit a YamlValue (high-level representation)
    fn visit_value(&mut self, value: &YamlValue) {
        match value {
            YamlValue::Scalar(s) => self.visit_scalar_value(s),
            YamlValue::Sequence(seq) => {
                for item in seq {
                    self.visit_value(item);
                }
            }
            YamlValue::Mapping(map) => {
                for val in map.values() {
                    self.visit_value(val);
                }
            }
            YamlValue::Set(_set) => {
                // Handle set if needed
            }
            YamlValue::OrderedMapping(pairs) => {
                for (_key, val) in pairs {
                    self.visit_value(val);
                }
            }
            YamlValue::Pairs(pairs) => {
                for (_key, val) in pairs {
                    self.visit_value(val);
                }
            }
        }
    }

    /// Visit a scalar value (high-level)
    fn visit_scalar_value(&mut self, _scalar: &crate::scalar::ScalarValue) {
        // Default empty implementation
    }
}

/// Trait for nodes that can accept a visitor
pub trait YamlAccept {
    /// Accept a visitor for traversal
    fn accept<V: YamlVisitor>(&self, visitor: &mut V);
}

impl YamlAccept for Yaml {
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

impl YamlAccept for YamlValue {
    fn accept<V: YamlVisitor>(&self, visitor: &mut V) {
        visitor.visit_value(self);
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

    fn visit_mapping(&mut self, mapping: &Mapping) {
        use crate::yaml::{extract_scalar, extract_mapping, extract_sequence};
        
        // Visit all key-value pairs in the mapping
        for (key, value) in mapping.pairs() {
            if let Some(key_node) = key {
                // Use smart extraction to handle wrapper nodes automatically
                if let Some(scalar) = extract_scalar(&key_node) {
                    scalar.accept(self);
                } else if let Some(sequence) = extract_sequence(&key_node) {
                    sequence.accept(self);
                } else if let Some(mapping) = extract_mapping(&key_node) {
                    mapping.accept(self);
                }
            }
            if let Some(value_node) = value {
                // Use smart extraction to handle wrapper nodes automatically
                if let Some(scalar) = extract_scalar(&value_node) {
                    scalar.accept(self);
                } else if let Some(nested_mapping) = extract_mapping(&value_node) {
                    nested_mapping.accept(self);
                } else if let Some(nested_sequence) = extract_sequence(&value_node) {
                    nested_sequence.accept(self);
                }
            }
        }
    }

    fn visit_sequence(&mut self, sequence: &Sequence) {
        // Visit all items in the sequence
        for item in sequence.items() {
            // Try to cast item to different types
            if let Some(scalar) = Scalar::cast(item.clone()) {
                scalar.accept(self);
            } else if let Some(nested_mapping) = Mapping::cast(item.clone()) {
                nested_mapping.accept(self);
            } else if let Some(nested_sequence) = Sequence::cast(item.clone()) {
                nested_sequence.accept(self);
            }
        }
    }
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
        use crate::yaml::{extract_scalar, extract_mapping, extract_sequence};
        
        self.mapping_count += 1;
        // Visit children
        for (key, value) in mapping.pairs() {
            if let Some(key_node) = key {
                // Use smart extraction to handle wrapper nodes automatically
                if let Some(scalar) = extract_scalar(&key_node) {
                    scalar.accept(self);
                } else if let Some(sequence) = extract_sequence(&key_node) {
                    sequence.accept(self);
                } else if let Some(mapping) = extract_mapping(&key_node) {
                    mapping.accept(self);
                }
            }
            if let Some(value_node) = value {
                // Use smart extraction to handle wrapper nodes automatically
                if let Some(scalar) = extract_scalar(&value_node) {
                    scalar.accept(self);
                } else if let Some(nested_mapping) = extract_mapping(&value_node) {
                    nested_mapping.accept(self);
                } else if let Some(nested_sequence) = extract_sequence(&value_node) {
                    nested_sequence.accept(self);
                }
            }
        }
    }

    fn visit_sequence(&mut self, sequence: &Sequence) {
        self.sequence_count += 1;
        // Visit children
        for item in sequence.items() {
            // Try to cast item to different types
            if let Some(scalar) = Scalar::cast(item.clone()) {
                scalar.accept(self);
            } else if let Some(nested_mapping) = Mapping::cast(item.clone()) {
                nested_mapping.accept(self);
            } else if let Some(nested_sequence) = Sequence::cast(item.clone()) {
                nested_sequence.accept(self);
            }
        }
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

    fn visit_mapping(&mut self, mapping: &Mapping) {
        use crate::yaml::{extract_scalar, extract_mapping, extract_sequence};
        
        for (key, value) in mapping.pairs() {
            if let Some(key_node) = key {
                // Use smart extraction to handle wrapper nodes automatically
                if let Some(scalar) = extract_scalar(&key_node) {
                    scalar.accept(self);
                } else if let Some(sequence) = extract_sequence(&key_node) {
                    sequence.accept(self);
                } else if let Some(mapping) = extract_mapping(&key_node) {
                    mapping.accept(self);
                }
            }
            if let Some(value_node) = value {
                // Use smart extraction to handle wrapper nodes automatically
                if let Some(scalar) = extract_scalar(&value_node) {
                    scalar.accept(self);
                } else if let Some(nested_mapping) = extract_mapping(&value_node) {
                    nested_mapping.accept(self);
                } else if let Some(nested_sequence) = extract_sequence(&value_node) {
                    nested_sequence.accept(self);
                }
            }
        }
    }

    fn visit_sequence(&mut self, sequence: &Sequence) {
        for item in sequence.items() {
            // Try to cast item to different types
            if let Some(scalar) = Scalar::cast(item.clone()) {
                scalar.accept(self);
            } else if let Some(nested_mapping) = Mapping::cast(item.clone()) {
                nested_mapping.accept(self);
            } else if let Some(nested_sequence) = Sequence::cast(item.clone()) {
                nested_sequence.accept(self);
            }
        }
    }
}
