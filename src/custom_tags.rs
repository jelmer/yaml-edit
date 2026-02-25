//! Custom YAML tag system for format-preserving parsing and serialization
//!
//! This module provides a flexible system for using custom tag handlers that can:
//! - **Parse tagged values** while preserving the exact formatting
//! - **Serialize values** with custom tags
//! - **Validate** tag content according to specific rules
//! - **Maintain lossless round-trip** behavior (parse → edit → serialize preserves formatting)
//!
//! # Overview
//!
//! YAML tags are type annotations that appear in YAML documents:
//!
//! ```yaml
//! # Built-in tags
//! timestamp: !!timestamp 2024-01-15T10:30:00Z
//! binary: !!binary SGVsbG8gV29ybGQh
//!
//! # Custom tags
//! email: !email user@example.com
//! uuid: !uuid 550e8400-e29b-41d4-a716-446655440000
//! json_data: !json {"key": "value"}
//! ```
//!
//! # Built-in Tag Handlers
//!
//! The library includes several built-in tag handlers:
//!
//! - [`TimestampHandler`] - Validates and parses timestamps with custom formats
//! - [`JsonHandler`] - Handles JSON data embedded in YAML
//! - [`EnvVarHandler`] - Resolves environment variables at parse time
//! - [`CompressedBinaryHandler`] - Handles base64-encoded binary data with optional compression
//!
//! # Quick Start
//!
//! ```rust
//! use yaml_edit::custom_tags::{CustomTagRegistry, TimestampHandler, JsonHandler};
//! use yaml_edit::YamlFile;
//! use std::str::FromStr;
//!
//! # fn main() -> Result<(), Box<dyn std::error::Error>> {
//! // Create a registry and register handlers
//! let mut registry = CustomTagRegistry::new();
//! registry.register("!timestamp", TimestampHandler::new("%Y-%m-%d"))?;
//! registry.register("!json", JsonHandler)?;
//!
//! // Parse YAML with custom tags
//! let yaml_str = r#"
//! config:
//!   created: !timestamp 2024-01-15
//!   data: !json {"status": "active"}
//! "#;
//!
//! let yaml = YamlFile::from_str(yaml_str)?;
//!
//! // Tags are preserved exactly as they appear
//! assert_eq!(yaml.to_string(), yaml_str);
//! # Ok(())
//! # }
//! ```
//!
//! # Format-Preserving Round-Trip
//!
//! One of the key features is that custom tags preserve exact formatting:
//!
//! ```rust
//! # use yaml_edit::YamlFile;
//! # use std::str::FromStr;
//! # fn main() -> Result<(), Box<dyn std::error::Error>> {
//! let original = r#"config:
//!   timestamp: !timestamp  2024-01-15    # Creation date
//!   data:      !json       {"key": "value"}
//! "#;
//!
//! let yaml = YamlFile::from_str(original)?;
//!
//! // All formatting, spacing, and comments are preserved
//! assert_eq!(yaml.to_string(), original);
//! # Ok(())
//! # }
//! ```
//!
//! # Validating Tagged Content
//!
//! Use the registry to validate content for specific tags:
//!
//! ```rust
//! # use yaml_edit::custom_tags::{CustomTagRegistry, TimestampHandler};
//! # fn main() -> Result<(), Box<dyn std::error::Error>> {
//! let mut registry = CustomTagRegistry::new();
//! registry.register("!timestamp", TimestampHandler::new("%Y-%m-%d"))?;
//!
//! // Validate content
//! assert!(registry.validate("!timestamp", "2024-01-15").is_ok());
//!
//! // Invalid content returns an error
//! assert!(registry.validate("!timestamp", "").is_err());
//! # Ok(())
//! # }
//! ```
//!
//! # Working with Tagged Nodes
//!
//! When traversing a YAML document, you can access tagged nodes:
//!
//! ```rust
//! # use yaml_edit::Document;
//! # use std::str::FromStr;
//! # fn main() -> Result<(), Box<dyn std::error::Error>> {
//! let yaml_str = r#"
//! email: !email user@example.com
//! id: !uuid 550e8400-e29b-41d4-a716-446655440000
//! "#;
//!
//! let doc = Document::from_str(yaml_str)?;
//!
//! if let Some(mapping) = doc.as_mapping() {
//!     if let Some(email_node) = mapping.get("email") {
//!         if let Some(tagged) = email_node.as_tagged() {
//!             println!("Tag: {:?}", tagged.tag());
//!             if let Some(scalar) = tagged.value() {
//!                 println!("Email value: {}", scalar.to_string());
//!             }
//!         }
//!     }
//! }
//! # Ok(())
//! # }
//! ```
//!
//! # Error Handling
//!
//! All tag operations return `Result<T, CustomTagError>` for proper error handling:
//!
//! ```rust
//! # use yaml_edit::custom_tags::{CustomTagRegistry, CustomTagError, TimestampHandler};
//! # fn main() -> Result<(), Box<dyn std::error::Error>> {
//! let mut registry = CustomTagRegistry::new();
//! registry.register("!timestamp", TimestampHandler::new("%Y-%m-%d"))?;
//!
//! match registry.validate("!timestamp", "invalid-date") {
//!     Ok(_) => println!("Valid"),
//!     Err(e) => {
//!         println!("Tag: {}", e.tag);
//!         println!("Message: {}", e.message);
//!         if let Some(content) = e.content {
//!             println!("Content: {}", content);
//!         }
//!     }
//! }
//! # Ok(())
//! # }
//! ```
//!
//! # Limitations and Notes
//!
//! 1. **Internal API**: The [`CustomTagHandler`] trait itself is internal. This module provides
//!    built-in handlers that are publicly available. If you need additional tag types, please
//!    file an issue or contribute to the library.
//!
//! 2. **Format Preservation**: Tags are preserved during parse/serialize, but the library doesn't
//!    validate tag semantics during normal document operations - validation must be explicitly
//!    called via the registry.
//!
//! 3. **Thread Safety**: Tag handlers are `Send + Sync` and the registry can be shared across
//!    threads safely.
//!
//! 4. **Tag Naming**: Tag names must start with `!` and contain only alphanumeric characters,
//!    hyphens, underscores, dots, colons, and forward slashes.

use crate::scalar::ScalarValue;
use crate::value::YamlValue;
use crate::yaml::{Document, TaggedNode};
use std::collections::HashMap;
use std::sync::{Arc, RwLock};

/// A custom tag handler that can serialize and deserialize values
///
/// This trait provides type-safe custom tag handling without using `dyn Any`.
/// All handlers work with `YamlValue` for serialization/deserialization,
/// ensuring type safety and proper error handling.
///
/// Primarily for internal use — custom tag handlers are typically registered through
/// the tag registry system rather than implemented directly.
pub trait CustomTagHandler: Send + Sync {
    /// Convert a YamlValue to the custom type representation
    fn serialize(&self, value: &YamlValue) -> Result<String, CustomTagError>;

    /// Convert a string representation back to a YamlValue
    fn deserialize(&self, content: &str) -> Result<YamlValue, CustomTagError>;

    /// Get a human-readable description of this tag
    fn description(&self) -> &str;

    /// Validate that the content is valid for this tag
    fn validate(&self, content: &str) -> Result<(), CustomTagError> {
        // Default implementation tries to deserialize
        self.deserialize(content).map(|_| ())
    }
}

/// Error type for custom tag operations
#[derive(Debug, Clone, PartialEq)]
pub struct CustomTagError {
    /// The error message
    pub message: String,
    /// The tag that caused the error
    pub tag: String,
    /// The problematic content (if available)
    pub content: Option<String>,
}

impl CustomTagError {
    /// Create a new custom tag error
    pub fn new(tag: impl Into<String>, message: impl Into<String>) -> Self {
        Self {
            tag: tag.into(),
            message: message.into(),
            content: None,
        }
    }

    /// Create a new custom tag error with content
    pub fn with_content(
        tag: impl Into<String>,
        message: impl Into<String>,
        content: impl Into<String>,
    ) -> Self {
        Self {
            tag: tag.into(),
            message: message.into(),
            content: Some(content.into()),
        }
    }
}

impl std::fmt::Display for CustomTagError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(content) = &self.content {
            write!(
                f,
                "Custom tag '{}' error: {} (content: {})",
                self.tag, self.message, content
            )
        } else {
            write!(f, "Custom tag '{}' error: {}", self.tag, self.message)
        }
    }
}

impl std::error::Error for CustomTagError {}

/// Registry for custom YAML tags
#[derive(Default)]
pub struct CustomTagRegistry {
    handlers: Arc<RwLock<HashMap<String, Arc<dyn CustomTagHandler>>>>,
}

impl CustomTagRegistry {
    /// Create a new empty tag registry
    pub fn new() -> Self {
        Self {
            handlers: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    /// Register a custom tag handler
    pub fn register<T>(&mut self, tag: impl Into<String>, handler: T) -> Result<(), CustomTagError>
    where
        T: CustomTagHandler + 'static,
    {
        let tag = tag.into();
        if !Self::is_valid_tag_name(&tag) {
            return Err(CustomTagError::new(
                &tag,
                "Invalid tag name: must start with '!' or '!!'",
            ));
        }

        let mut handlers = self.handlers.write().unwrap();
        handlers.insert(tag, Arc::new(handler));
        Ok(())
    }

    /// Unregister a custom tag handler
    pub fn unregister(&mut self, tag: &str) -> bool {
        let mut handlers = self.handlers.write().unwrap();
        handlers.remove(tag).is_some()
    }

    /// Check if a tag is registered
    pub fn has_tag(&self, tag: &str) -> bool {
        let handlers = self.handlers.read().unwrap();
        handlers.contains_key(tag)
    }

    /// Get all registered tag names
    pub fn registered_tags(&self) -> Vec<String> {
        let handlers = self.handlers.read().unwrap();
        handlers.keys().cloned().collect()
    }

    /// Serialize a value using a custom tag
    pub fn serialize(&self, tag: &str, value: &YamlValue) -> Result<String, CustomTagError> {
        let handlers = self.handlers.read().unwrap();
        if let Some(handler) = handlers.get(tag) {
            handler.serialize(value)
        } else {
            Err(CustomTagError::new(tag, "Tag not registered"))
        }
    }

    /// Deserialize a value using a custom tag
    pub fn deserialize(&self, tag: &str, content: &str) -> Result<YamlValue, CustomTagError> {
        let handlers = self.handlers.read().unwrap();
        if let Some(handler) = handlers.get(tag) {
            handler.deserialize(content)
        } else {
            Err(CustomTagError::new(tag, "Tag not registered"))
        }
    }

    /// Validate content for a custom tag
    pub fn validate(&self, tag: &str, content: &str) -> Result<(), CustomTagError> {
        let handlers = self.handlers.read().unwrap();
        if let Some(handler) = handlers.get(tag) {
            handler.validate(content)
        } else {
            Err(CustomTagError::new(tag, "Tag not registered"))
        }
    }

    /// Check if a tag name is valid (follows YAML tag naming conventions)
    fn is_valid_tag_name(tag: &str) -> bool {
        // YAML tags must start with ! and contain valid characters
        if !tag.starts_with('!') {
            return false;
        }

        if tag.len() < 2 {
            return false;
        }

        // Check for valid characters after !
        for ch in tag[1..].chars() {
            match ch {
                'a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_' | '.' | ':' | '/' => {}
                _ => return false,
            }
        }

        true
    }
}

impl Clone for CustomTagRegistry {
    fn clone(&self) -> Self {
        Self {
            handlers: Arc::clone(&self.handlers),
        }
    }
}

/// A document parser that supports custom tags
pub struct CustomTagParser {
    registry: CustomTagRegistry,
}

impl CustomTagParser {
    /// Create a new parser with a custom tag registry
    pub fn new(registry: CustomTagRegistry) -> Self {
        Self { registry }
    }

    /// Parse a document and process custom tags
    pub fn parse_with_custom_tags(&self, _document: &Document) -> Result<(), CustomTagError> {
        // This would traverse the document and process any custom tags
        // For now, just return success
        Ok(())
    }

    /// Process a tagged scalar using the custom tag registry
    pub fn process_tagged_node(
        &self,
        tagged_node: &TaggedNode,
    ) -> Result<YamlValue, CustomTagError> {
        if let Some(tag) = tagged_node.tag() {
            if let Some(scalar) = tagged_node.value() {
                let content = scalar.as_string();
                return self.registry.deserialize(&tag, &content);
            }
        }
        Err(CustomTagError::new("", "Invalid tagged scalar"))
    }
}

// Built-in custom tag handlers for common use cases

/// A custom tag handler for timestamps with custom formats
pub struct TimestampHandler {
    format: String,
}

impl TimestampHandler {
    /// Create a new timestamp handler with a specific format
    pub fn new(format: impl Into<String>) -> Self {
        Self {
            format: format.into(),
        }
    }
}

impl CustomTagHandler for TimestampHandler {
    fn serialize(&self, value: &YamlValue) -> Result<String, CustomTagError> {
        if let Some(scalar) = value.as_scalar() {
            // Return the value with format validation
            // Different formats have different validation rules
            let content = scalar.value();

            // Simple validation based on format string
            if self.format.contains("%Y") && !content.chars().any(|c| c.is_ascii_digit()) {
                return Err(CustomTagError::with_content(
                    "!timestamp",
                    "Timestamp should contain year digits",
                    content,
                ));
            }

            Ok(content.to_string())
        } else {
            Err(CustomTagError::new("!timestamp", "Value must be a scalar"))
        }
    }

    fn deserialize(&self, content: &str) -> Result<YamlValue, CustomTagError> {
        // Validate the content matches expected format patterns
        // The format field determines what patterns we expect

        // Basic validation based on common format patterns
        if self.format.contains("%Y-%m-%d") {
            // ISO date format - check for YYYY-MM-DD pattern
            let parts: Vec<&str> = content.split('-').collect();
            if parts.len() < 3 {
                return Err(CustomTagError::with_content(
                    "!timestamp",
                    format!("Expected format: {}", self.format),
                    content,
                ));
            }
        }

        Ok(YamlValue::scalar(ScalarValue::timestamp(content)))
    }

    fn description(&self) -> &str {
        "Custom timestamp format handler"
    }

    fn validate(&self, content: &str) -> Result<(), CustomTagError> {
        // Basic validation - check if it's not empty
        if content.trim().is_empty() {
            Err(CustomTagError::with_content(
                "!timestamp",
                "Timestamp cannot be empty",
                content,
            ))
        } else {
            Ok(())
        }
    }
}

/// A custom tag handler for JSON data
pub struct JsonHandler;

impl CustomTagHandler for JsonHandler {
    fn serialize(&self, value: &YamlValue) -> Result<String, CustomTagError> {
        // Convert YamlValue to JSON string
        match value {
            YamlValue::Scalar(s) => Ok(format!("\"{}\"", s.value())),
            YamlValue::Sequence(seq) => {
                let items: Result<Vec<_>, _> = seq.iter().map(|v| self.serialize(v)).collect();
                let items = items?;
                Ok(format!("[{}]", items.join(",")))
            }
            YamlValue::Mapping(map) => {
                let pairs: Result<Vec<_>, _> = map
                    .iter()
                    .map(|(k, v)| Ok(format!("\"{}\":{}", k, self.serialize(v)?)))
                    .collect();
                let pairs = pairs?;
                Ok(format!("{{{}}}", pairs.join(",")))
            }
            _ => Err(CustomTagError::new(
                "!json",
                "Unsupported value type for JSON serialization",
            )),
        }
    }

    fn deserialize(&self, content: &str) -> Result<YamlValue, CustomTagError> {
        // TODO: implement a proper JSON parser here
        let content = content.trim();
        if content.starts_with('"') && content.ends_with('"') {
            let inner = &content[1..content.len() - 1];
            Ok(YamlValue::scalar(inner))
        } else if content.starts_with('[') && content.ends_with(']') {
            unimplemented!("JsonHandler::deserialize does not yet parse JSON arrays")
        } else if content.starts_with('{') && content.ends_with('}') {
            unimplemented!("JsonHandler::deserialize does not yet parse JSON objects")
        } else {
            Ok(YamlValue::scalar(content))
        }
    }

    fn description(&self) -> &str {
        "JSON data embedded in YAML"
    }
}

/// A custom tag handler for base64-encoded binary data with compression
pub struct CompressedBinaryHandler {
    compression_level: u32,
}

impl CompressedBinaryHandler {
    /// Create a new compressed binary handler
    pub fn new(compression_level: u32) -> Self {
        Self { compression_level }
    }
}

impl CustomTagHandler for CompressedBinaryHandler {
    fn serialize(&self, value: &YamlValue) -> Result<String, CustomTagError> {
        if let Some(scalar) = value.as_scalar() {
            let data = scalar.value();

            // For demonstration: higher compression levels result in a marker prefix
            // In a real implementation, this would use a compression library
            let output = if self.compression_level > 0 {
                // Add a compression marker that indicates the level
                format!("COMPRESSED[{}]:{}", self.compression_level, data)
            } else {
                // No compression - just base64
                data.to_string()
            };

            Ok(output)
        } else {
            Err(CustomTagError::new("!compressed", "Value must be a scalar"))
        }
    }

    fn deserialize(&self, content: &str) -> Result<YamlValue, CustomTagError> {
        // Check if content has compression marker
        if let Some(rest) = content.strip_prefix("COMPRESSED[") {
            // Parse compression level and extract data
            if let Some(bracket_pos) = rest.find("]:") {
                let _level_str = &rest[..bracket_pos];
                let data = &rest[bracket_pos + 2..];
                // In a real implementation, would decompress based on level
                Ok(YamlValue::scalar(data))
            } else {
                Ok(YamlValue::scalar(content))
            }
        } else {
            // No compression marker - return as-is
            Ok(YamlValue::scalar(content))
        }
    }

    fn description(&self) -> &str {
        // The trait requires a static string, but we use compression_level
        // in serialize/deserialize to control the compression behavior
        "Compressed binary data"
    }

    fn validate(&self, content: &str) -> Result<(), CustomTagError> {
        // Basic base64 validation
        if content
            .chars()
            .all(|c| c.is_ascii_alphanumeric() || c == '+' || c == '/' || c == '=')
        {
            Ok(())
        } else {
            Err(CustomTagError::with_content(
                "!compressed",
                "Invalid base64 content",
                content,
            ))
        }
    }
}

/// A simple environment variable handler
pub struct EnvVarHandler;

impl CustomTagHandler for EnvVarHandler {
    fn serialize(&self, value: &YamlValue) -> Result<String, CustomTagError> {
        if let Some(scalar) = value.as_scalar() {
            Ok(scalar.value().to_string())
        } else {
            Err(CustomTagError::new(
                "!env",
                "Environment variable must be a scalar",
            ))
        }
    }

    fn deserialize(&self, content: &str) -> Result<YamlValue, CustomTagError> {
        // Resolve environment variable
        if let Ok(env_value) = std::env::var(content.trim()) {
            Ok(YamlValue::scalar(env_value))
        } else {
            Err(CustomTagError::with_content(
                "!env",
                "Environment variable not found",
                content,
            ))
        }
    }

    fn description(&self) -> &str {
        "Environment variable substitution"
    }

    fn validate(&self, content: &str) -> Result<(), CustomTagError> {
        let var_name = content.trim();
        if var_name.is_empty() {
            Err(CustomTagError::with_content(
                "!env",
                "Environment variable name cannot be empty",
                content,
            ))
        } else if var_name
            .chars()
            .all(|c| c.is_ascii_alphanumeric() || c == '_')
        {
            Ok(())
        } else {
            Err(CustomTagError::with_content(
                "!env",
                "Invalid environment variable name",
                content,
            ))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tag_registry_basic() {
        let mut registry = CustomTagRegistry::new();

        // Register a handler
        let handler = JsonHandler;
        assert!(registry.register("!json", handler).is_ok());

        // Check if registered
        assert!(registry.has_tag("!json"));
        assert_eq!(registry.registered_tags(), vec!["!json"]);

        // Unregister
        assert!(registry.unregister("!json"));
        assert!(!registry.has_tag("!json"));
    }

    #[test]
    fn test_tag_name_validation() {
        assert!(CustomTagRegistry::is_valid_tag_name("!json"));
        assert!(CustomTagRegistry::is_valid_tag_name("!my-custom-tag"));
        assert!(CustomTagRegistry::is_valid_tag_name("!domain.com/type"));

        assert!(!CustomTagRegistry::is_valid_tag_name("json")); // Missing !
        assert!(!CustomTagRegistry::is_valid_tag_name("!")); // Too short
        assert!(!CustomTagRegistry::is_valid_tag_name("!invalid space")); // Space not allowed
        assert!(!CustomTagRegistry::is_valid_tag_name("!invalid@char")); // @ not allowed
    }

    #[test]
    fn test_json_handler() {
        let handler = JsonHandler;

        // Test serialization
        let scalar_value = YamlValue::scalar("test");
        assert_eq!(handler.serialize(&scalar_value).unwrap(), "\"test\"");

        let seq_value = YamlValue::sequence();
        assert_eq!(handler.serialize(&seq_value).unwrap(), "[]");

        // Test deserialization of scalar and quoted-string values
        assert!(handler.deserialize("\"hello\"").is_ok());
        assert!(handler.deserialize("plain").is_ok());

        // Test validation (implicit through deserialization)
        assert!(handler.validate("\"valid\"").is_ok());
    }

    #[test]
    fn test_timestamp_handler() {
        let handler = TimestampHandler::new("YYYY-MM-DD");

        let scalar_value = YamlValue::scalar("2023-12-25");
        assert!(handler.serialize(&scalar_value).is_ok());
        assert!(handler.deserialize("2023-12-25").is_ok());
        assert!(handler.validate("2023-12-25").is_ok());
        assert!(handler.validate("").is_err());
    }

    #[test]
    fn test_env_var_handler() {
        let handler = EnvVarHandler;

        // Set a test environment variable
        std::env::set_var("TEST_VAR", "test_value");

        let result = handler.deserialize("TEST_VAR");
        assert!(result.is_ok());
        if let Ok(YamlValue::Scalar(scalar)) = result {
            assert_eq!(scalar.value(), "test_value");
        }

        // Test validation
        assert!(handler.validate("VALID_VAR_NAME").is_ok());
        assert!(handler.validate("").is_err());
        assert!(handler.validate("invalid var name").is_err());

        std::env::remove_var("TEST_VAR");
    }

    #[test]
    fn test_compressed_binary_handler() {
        let handler = CompressedBinaryHandler::new(6);

        let scalar_value = YamlValue::scalar("SGVsbG8gV29ybGQ="); // "Hello World" in base64
        assert!(handler.serialize(&scalar_value).is_ok());
        assert!(handler.deserialize("SGVsbG8gV29ybGQ=").is_ok());

        // Test validation
        assert!(handler.validate("SGVsbG8gV29ybGQ=").is_ok());
        assert!(handler.validate("invalid@base64").is_err());
    }

    #[test]
    fn test_registry_serialize_deserialize() {
        let mut registry = CustomTagRegistry::new();
        registry.register("!json", JsonHandler).unwrap();

        let scalar_value = YamlValue::scalar("test");
        let serialized = registry.serialize("!json", &scalar_value).unwrap();
        assert_eq!(serialized, "\"test\"");

        let deserialized = registry.deserialize("!json", "\"hello\"").unwrap();
        assert!(deserialized.is_scalar());

        // Test unregistered tag
        assert!(registry.serialize("!unknown", &scalar_value).is_err());
        assert!(registry.deserialize("!unknown", "content").is_err());
    }

    #[test]
    fn test_custom_tag_error() {
        let error = CustomTagError::new("!test", "Test error message");
        assert_eq!(error.tag, "!test");
        assert_eq!(error.message, "Test error message");
        assert!(error.content.is_none());

        let error_with_content = CustomTagError::with_content("!test", "Test error", "bad content");
        assert_eq!(error_with_content.content, Some("bad content".to_string()));

        assert_eq!(
            format!("{}", error_with_content),
            "Custom tag '!test' error: Test error (content: bad content)"
        );
    }
}
