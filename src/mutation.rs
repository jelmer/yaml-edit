//! Unified mutation API with builder pattern for YAML editing operations.
//!
//! This module provides a clean, fluent API for complex YAML mutations using the builder pattern.
//! The existing convenience methods (.set(), .remove(), etc.) are still available and preferred
//! for simple operations. This builder is for complex operations that need positioning,
//! field ordering, or other advanced options.

use crate::scalar::ScalarValue;
use crate::value::YamlValue;
use crate::yaml::{Document, Mapping, Sequence};

/// Position specification for insertions
#[derive(Debug, Clone, PartialEq)]
pub enum InsertPosition {
    /// Insert at the end
    End,
    /// Insert at the beginning  
    Start,
    /// Insert at a specific index
    Index(usize),
    /// Insert after a specific key (for mappings)
    AfterKey(String),
    /// Insert before a specific key (for mappings)
    BeforeKey(String),
    /// Insert according to field order
    Ordered(Vec<String>),
}

/// Options for mutation operations
#[derive(Debug, Clone)]
pub struct MutationOptions {
    /// Where to insert new items
    pub position: InsertPosition,
    /// Whether to preserve existing formatting
    pub preserve_formatting: bool,
    /// Field order for ordered insertions (mappings only)
    pub field_order: Option<Vec<String>>,
    /// Whether to replace existing keys/values
    pub replace_existing: bool,
}

impl Default for MutationOptions {
    fn default() -> Self {
        Self {
            position: InsertPosition::End,
            preserve_formatting: true,
            field_order: None,
            replace_existing: true,
        }
    }
}

/// Unified builder for all YAML mutation operations
pub struct MutationBuilder<'a> {
    target: MutationTarget<'a>,
    options: MutationOptions,
}

/// What we're mutating
enum MutationTarget<'a> {
    Document(&'a mut Document),
    Mapping(&'a mut Mapping),
    Sequence(&'a mut Sequence),
}

impl<'a> MutationBuilder<'a> {
    /// Create a builder for Document mutations
    pub fn document(doc: &'a mut Document) -> Self {
        Self {
            target: MutationTarget::Document(doc),
            options: MutationOptions::default(),
        }
    }

    /// Create a builder for Mapping mutations
    pub fn mapping(mapping: &'a mut Mapping) -> Self {
        Self {
            target: MutationTarget::Mapping(mapping),
            options: MutationOptions::default(),
        }
    }

    /// Create a builder for Sequence mutations
    pub fn sequence(sequence: &'a mut Sequence) -> Self {
        Self {
            target: MutationTarget::Sequence(sequence),
            options: MutationOptions::default(),
        }
    }

    /// Set the insertion position
    pub fn position(mut self, pos: InsertPosition) -> Self {
        self.options.position = pos;
        self
    }

    /// Insert at the end (default)
    pub fn at_end(self) -> Self {
        self.position(InsertPosition::End)
    }

    /// Insert at the beginning
    pub fn at_start(self) -> Self {
        self.position(InsertPosition::Start)
    }

    /// Insert at a specific index
    pub fn at_index(self, index: usize) -> Self {
        self.position(InsertPosition::Index(index))
    }

    /// Insert after a specific key (mappings only)
    pub fn after_key(self, key: impl Into<String>) -> Self {
        self.position(InsertPosition::AfterKey(key.into()))
    }

    /// Insert before a specific key (mappings only)
    pub fn before_key(self, key: impl Into<String>) -> Self {
        self.position(InsertPosition::BeforeKey(key.into()))
    }

    /// Use field ordering for insertion
    pub fn with_field_order(mut self, order: Vec<String>) -> Self {
        self.options.field_order = Some(order.clone());
        self.options.position = InsertPosition::Ordered(order);
        self
    }

    /// Enable/disable preserving existing formatting
    pub fn preserve_formatting(mut self, preserve: bool) -> Self {
        self.options.preserve_formatting = preserve;
        self
    }

    /// Enable/disable replacing existing entries
    pub fn replace_existing(mut self, replace: bool) -> Self {
        self.options.replace_existing = replace;
        self
    }

    /// Set a key-value pair (mappings and documents)
    pub fn set(
        mut self,
        key: impl Into<ScalarValue>,
        value: impl Into<YamlValue>,
    ) -> MutationResult {
        let key_val = YamlValue::Scalar(key.into());
        let value_val = value.into();
        let options = self.options.clone();

        match self.target {
            MutationTarget::Document(ref mut doc) => {
                Self::set_in_document_impl(doc, key_val, value_val, &options)
            }
            MutationTarget::Mapping(ref mut mapping) => {
                Self::set_in_mapping_impl(mapping, key_val, value_val, &options)
            }
            MutationTarget::Sequence(_) => Err(MutationError::InvalidOperation(
                "Cannot set key-value pairs in sequences".to_string(),
            )),
        }
    }

    /// Set raw YAML strings (parses them first)
    pub fn set_raw(mut self, key: &str, value: &str) -> MutationResult {
        let key_val = YamlValue::parse_raw(key);
        let value_val = YamlValue::parse_raw(value);
        let options = self.options.clone();

        match self.target {
            MutationTarget::Document(ref mut doc) => {
                Self::set_in_document_impl(doc, key_val, value_val, &options)
            }
            MutationTarget::Mapping(ref mut mapping) => {
                Self::set_in_mapping_impl(mapping, key_val, value_val, &options)
            }
            MutationTarget::Sequence(_) => Err(MutationError::InvalidOperation(
                "Cannot set key-value pairs in sequences".to_string(),
            )),
        }
    }

    /// Add an item to a sequence
    pub fn push(mut self, value: impl Into<YamlValue>) -> MutationResult {
        match self.target {
            MutationTarget::Sequence(ref mut seq) => {
                seq.push_yaml(&value.into());
                Ok(())
            }
            _ => Err(MutationError::InvalidOperation(
                "Can only push to sequences".to_string(),
            )),
        }
    }

    /// Insert an item at the specified position
    pub fn insert(mut self, value: impl Into<YamlValue>) -> MutationResult {
        let value_val = value.into();
        let options = self.options.clone();
        match self.target {
            MutationTarget::Sequence(ref mut seq) => {
                Self::insert_in_sequence_impl(seq, value_val, &options)
            }
            _ => Err(MutationError::InvalidOperation(
                "Insert with position only supported for sequences".to_string(),
            )),
        }
    }

    /// Remove a key or item
    pub fn remove(mut self, key: impl Into<YamlValue>) -> MutationResult {
        let key_val = key.into();

        match self.target {
            MutationTarget::Document(ref mut doc) => {
                if let YamlValue::Scalar(scalar_key) = key_val {
                    if doc.remove(&YamlValue::Scalar(scalar_key.clone())) {
                        Ok(())
                    } else {
                        Err(MutationError::KeyNotFound(scalar_key.value().to_string()))
                    }
                } else {
                    Err(MutationError::InvalidKeyType(
                        "Document remove requires scalar key".to_string(),
                    ))
                }
            }
            MutationTarget::Mapping(ref mut mapping) => {
                if let YamlValue::Scalar(scalar_key) = key_val {
                    if mapping.remove(&YamlValue::Scalar(scalar_key.clone())) {
                        Ok(())
                    } else {
                        Err(MutationError::KeyNotFound(scalar_key.value().to_string()))
                    }
                } else {
                    Err(MutationError::InvalidKeyType(
                        "Mapping remove requires scalar key".to_string(),
                    ))
                }
            }
            MutationTarget::Sequence(ref mut seq) => {
                // For sequences, treat key as index
                if let YamlValue::Scalar(scalar) = key_val {
                    if let Ok(index) = scalar.value().parse::<usize>() {
                        seq.remove(index);
                        Ok(())
                    } else {
                        Err(MutationError::InvalidKeyType(
                            "Invalid index for sequence".to_string(),
                        ))
                    }
                } else {
                    Err(MutationError::InvalidKeyType(
                        "Sequence remove requires numeric index".to_string(),
                    ))
                }
            }
        }
    }

    /// Rename a key (mappings only)
    pub fn rename(mut self, old_key: &str, new_key: impl Into<ScalarValue>) -> MutationResult {
        match self.target {
            MutationTarget::Mapping(ref mut mapping) => {
                if mapping.rename_key(old_key, new_key) {
                    Ok(())
                } else {
                    Err(MutationError::KeyNotFound(old_key.to_string()))
                }
            }
            _ => Err(MutationError::InvalidOperation(
                "Rename only supported for mappings".to_string(),
            )),
        }
    }

    // Internal implementation methods

    fn set_in_document_impl(
        doc: &mut Document,
        key: YamlValue,
        value: YamlValue,
        options: &MutationOptions,
    ) -> MutationResult {
        // Delegate to existing document logic but with options
        if let Some(mut mapping) = doc.as_mapping_mut() {
            Self::set_in_mapping_impl(&mut mapping, key, value, options)
        } else {
            // Use the Document's built-in set method for non-mapping documents
            match key {
                YamlValue::Scalar(scalar_key) => {
                    doc.set(&YamlValue::Scalar(scalar_key), &value);
                    Ok(())
                }
                _ => Err(MutationError::InvalidKeyType(
                    "Document set requires scalar key".to_string(),
                )),
            }
        }
    }

    fn set_in_mapping_impl(
        mapping: &mut Mapping,
        key: YamlValue,
        value: YamlValue,
        options: &MutationOptions,
    ) -> MutationResult {
        match options.position {
            InsertPosition::Ordered(ref order) => {
                // Use field order - delegate to existing method
                let field_order: Vec<&str> = order.iter().map(|s| s.as_str()).collect();
                mapping.set_with_field_order(key, value, &field_order);
                Ok(())
            }
            InsertPosition::AfterKey(ref after_key) => {
                // Delegate to existing method
                if mapping.insert_after_preserving(after_key, key, value) {
                    Ok(())
                } else {
                    Err(MutationError::KeyNotFound(after_key.clone()))
                }
            }
            InsertPosition::BeforeKey(ref before_key) => {
                // Delegate to existing method
                if mapping.insert_before_preserving(before_key, key, value) {
                    Ok(())
                } else {
                    Err(MutationError::KeyNotFound(before_key.clone()))
                }
            }
            InsertPosition::Index(index) => {
                // Delegate to existing method
                mapping.insert_at_index_preserving(index, &key, &value);
                Ok(())
            }
            _ => {
                // Default set operation - delegate to existing method
                mapping.set(&key, &value);
                Ok(())
            }
        }
    }

    fn insert_in_sequence_impl(
        seq: &mut Sequence,
        value: YamlValue,
        options: &MutationOptions,
    ) -> MutationResult {
        match options.position {
            InsertPosition::Index(index) => {
                seq.insert(index, &value);
                Ok(())
            }
            InsertPosition::Start => {
                seq.insert(0, &value);
                Ok(())
            }
            _ => {
                // Default to push (end insertion)
                seq.push_yaml(&value);
                Ok(())
            }
        }
    }
}

/// Error types for mutation operations
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MutationError {
    /// Key was not found in the target structure
    KeyNotFound(String),
    /// Invalid operation for the target type
    InvalidOperation(String),
    /// Index is out of bounds
    IndexOutOfBounds(usize),
    /// Invalid key type (e.g., non-scalar key where scalar required)
    InvalidKeyType(String),
    /// Invalid value type for the operation
    InvalidValueType(String),
    /// General error with custom message
    Custom(String),
}

impl std::fmt::Display for MutationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MutationError::KeyNotFound(key) => write!(f, "Key '{}' not found", key),
            MutationError::InvalidOperation(msg) => write!(f, "Invalid operation: {}", msg),
            MutationError::IndexOutOfBounds(index) => write!(f, "Index {} out of bounds", index),
            MutationError::InvalidKeyType(msg) => write!(f, "Invalid key type: {}", msg),
            MutationError::InvalidValueType(msg) => write!(f, "Invalid value type: {}", msg),
            MutationError::Custom(msg) => write!(f, "{}", msg),
        }
    }
}

impl std::error::Error for MutationError {}

/// Result type for mutation operations
pub type MutationResult = Result<(), MutationError>;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::yaml::Yaml;
    use std::str::FromStr;

    #[test]
    fn test_builder_basic_set() {
        let mut yaml = Yaml::from_str("name: old_value").unwrap();
        let mut doc = yaml.document().unwrap();

        let result = MutationBuilder::document(&mut doc).set("name", "new_value");

        assert!(result.is_ok());
        assert_eq!(
            doc.get_string(&YamlValue::scalar("name")).unwrap(),
            "new_value"
        );
    }

    #[test]
    fn test_builder_with_field_order() {
        let mut yaml = Yaml::from_str("name: test\nversion: 1.0").unwrap();
        let mut doc = yaml.document().unwrap();

        let result = MutationBuilder::document(&mut doc)
            .with_field_order(vec!["version".to_string(), "name".to_string()])
            .set("description", "A test project");

        assert!(result.is_ok());

        // Check that the field was added
        assert!(doc.get_string(&YamlValue::scalar("description")).is_some());
    }

    #[test]
    fn test_builder_sequence_operations() {
        let mut yaml = Yaml::from_str("items: [1, 2]").unwrap();
        let mut doc = yaml.document().unwrap();

        if let Some(mut mapping) = doc.as_mapping_mut() {
            if let Some(items_node) = mapping.get(&YamlValue::scalar("items")) {
                if let Some(mut seq) = crate::yaml::extract_sequence(&items_node) {
                    let result = MutationBuilder::sequence(&mut seq).push("3");

                    assert!(result.is_ok());
                }
            }
        }
    }
}
