#![deny(missing_docs)]
#![allow(clippy::type_complexity)]
#![warn(clippy::unnecessary_to_owned)]
#![warn(clippy::redundant_clone)]
#![warn(clippy::inefficient_to_string)]
#![warn(clippy::manual_string_new)]
#![doc = include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/README.md"))]

//! A lossless YAML parser and editor.
//!
//! This library provides a lossless parser for YAML files, preserving
//! all whitespace, comments, and formatting. It is based on the [rowan] library.
//!
//! # Mutability Model
//!
//! **Important:** This library uses interior mutability through the rowan library.
//! This means methods taking `&self` can still modify the underlying syntax tree.
//!
//! ## What This Means
//!
//! - Types like [`Mapping`], [`Sequence`], and [`Document`] can mutate even from `&self`
//! - Changes are immediately visible to all holders of the syntax tree
//! - You don't need to mark variables as `mut` to call mutation methods
//!
//! ## Example
//!
//! ```rust
//! use yaml_edit::Document;
//! use std::str::FromStr;
//!
//! let doc = Document::from_str("name: Alice").unwrap();  // Note: not `mut`
//! let mapping = doc.as_mapping().unwrap();  // Note: not `mut`
//!
//! // Yet we can still mutate!
//! mapping.set("age", 30);  // This works despite `mapping` not being `mut`
//!
//! assert_eq!(doc.to_string(), "name: Alice\nage: 30\n");
//! ```
//!
//! ## Why This Design?
//!
//! This design enables:
//! - **Efficient in-place mutations** without cloning the entire tree
//! - **Sharing references** while still allowing modifications
//! - **Lossless preservation** of formatting and comments during edits
//!
//! If you're familiar with `RefCell` or `Rc`, this is similar - the tree uses
//! internal synchronization to allow shared mutable access.
//!
//! ## Migration Note
//!
//! If you're coming from other YAML libraries, this might seem unusual. In most
//! libraries, you need `&mut` to modify data. Here, you don't. This is intentional
//! and allows for a more flexible API while maintaining the guarantees of Rust's
//! borrow checker.
//!
//! # Getting Started
//!
//! ## Parsing YAML
//!
//! ```rust
//! use yaml_edit::Document;
//! use std::str::FromStr;
//!
//! let yaml = Document::from_str("name: Alice\nage: 30").unwrap();
//! let mapping = yaml.as_mapping().unwrap();
//!
//! // Get values
//! let name = mapping.get("name").unwrap();
//! assert_eq!(name.as_scalar().unwrap().to_string(), "Alice");
//! ```
//!
//! ## Modifying YAML
//!
//! ```rust
//! use yaml_edit::Document;
//! use std::str::FromStr;
//!
//! let yaml = Document::from_str("name: Alice").unwrap();
//! let mapping = yaml.as_mapping().unwrap();
//!
//! // Add a new field
//! mapping.set("age", 30);
//!
//! // Update an existing field
//! mapping.set("name", "Bob");
//!
//! // Remove a field
//! mapping.remove("age");
//! ```
//!
//! ## Path-based Access
//!
//! ```rust
//! use yaml_edit::{Document, path::YamlPath};
//! use std::str::FromStr;
//!
//! let yaml = Document::from_str("server:\n  host: localhost").unwrap();
//!
//! // Get nested values
//! let host = yaml.get_path("server.host");
//! assert!(host.is_some());
//!
//! // Set nested values (creates intermediate mappings)
//! yaml.set_path("server.port", 8080);
//! yaml.set_path("database.host", "db.example.com");
//! ```
//!
//! ## Iterating Over Collections
//!
//! ```rust
//! use yaml_edit::Document;
//! use std::str::FromStr;
//!
//! let yaml = Document::from_str("a: 1\nb: 2\nc: 3").unwrap();
//! let mapping = yaml.as_mapping().unwrap();
//!
//! // Iterate over key-value pairs
//! for (key, value) in &mapping {
//!     println!("{:?}: {:?}", key, value);
//! }
//!
//! // Use iterator methods
//! let count = (&mapping).into_iter().count();
//! assert_eq!(count, 3);
//! ```
//!
//! ## Working with Sequences
//!
//! ```rust
//! use yaml_edit::Document;
//! use std::str::FromStr;
//!
//! let yaml = Document::from_str("items:\n  - apple\n  - banana").unwrap();
//! let mapping = yaml.as_mapping().unwrap();
//! let sequence = mapping.get_sequence("items").unwrap();
//!
//! // Iterate over items
//! for item in &sequence {
//!     println!("{:?}", item);
//! }
//!
//! // Get specific item
//! let first = sequence.get(0);
//! assert!(first.is_some());
//! ```
//!
//! ## Schema Validation
//!
//! ```rust
//! use yaml_edit::{Document, SchemaValidator};
//! use std::str::FromStr;
//!
//! let yaml = Document::from_str("name: Alice\nage: 30").unwrap();
//!
//! // Validate against JSON schema (no custom types)
//! let result = SchemaValidator::json().validate(&yaml);
//! assert!(result.is_ok());
//! ```
//!
//! ## Position Tracking
//!
//! ```rust
//! use yaml_edit::Document;
//! use std::str::FromStr;
//!
//! let text = "name: Alice\nage: 30";
//! let doc = Document::from_str(text).unwrap();
//!
//! // Get line/column positions
//! let start = doc.start_position(text);
//! assert_eq!(start.line, 1);
//! assert_eq!(start.column, 1);
//! ```

pub mod anchor_resolution;
mod as_yaml;
mod builder;
pub mod custom_tags;
pub mod debug;
mod error;
pub mod error_recovery;
mod lex;
mod nodes;
mod parse;
pub mod path;
mod scalar;
mod schema;
pub mod validator;
mod value;
pub mod visitor;
mod yaml;

pub use as_yaml::{yaml_eq, AsYaml, YamlKind, YamlNode};
pub use builder::{MappingBuilder, SequenceBuilder, YamlBuilder};
pub use error::{YamlError, YamlResult};
pub use lex::{
    lex, lex_with_validation, lex_with_validation_config, SyntaxKind, ValidationConfig,
    WhitespaceError, WhitespaceErrorCategory,
};
pub use parse::Parse;
pub use scalar::{ScalarStyle, ScalarType, ScalarValue};
pub use schema::{
    CustomSchema, CustomValidationResult, Schema, SchemaValidator, ValidationError,
    ValidationErrorKind, ValidationResult,
};
pub use yaml::{
    Alias, Directive, Document, Lang, Mapping, MappingEntry, Scalar, ScalarConversionError,
    Sequence, Set, TaggedNode, YamlFile,
};

/// Advanced API for power users who need direct access to the underlying syntax tree.
///
/// This module provides low-level access to the rowan syntax tree implementation.
/// Most users should not need this module - the main API provides high-level
/// wrappers that are easier to use and don't expose implementation details.
///
/// # Example
///
/// ```rust
/// use yaml_edit::{Document, advanced};
/// use std::str::FromStr;
///
/// let doc = Document::from_str("key: value").unwrap();
/// let mapping = doc.as_mapping().unwrap();
///
/// // Get a value node
/// if let Some(value) = mapping.get("key") {
///     // YamlNode provides access to the underlying structure
///     println!("Found value: {}", value.to_string());
/// }
/// ```
pub mod advanced {
    pub use rowan::TextRange;

    use crate::yaml::SyntaxNode;
    use crate::TextPosition;

    /// Get the text range of a syntax node
    pub fn syntax_node_range(node: &SyntaxNode) -> TextRange {
        node.text_range()
    }

    /// Convert a TextPosition to rowan's TextRange
    pub fn text_position_to_range(pos: TextPosition) -> TextRange {
        pos.into()
    }

    /// Convert rowan's TextRange to TextPosition
    pub fn text_range_to_position(range: TextRange) -> TextPosition {
        range.into()
    }
}

// Re-export custom tags API
pub use custom_tags::{
    // Built-in handlers
    CompressedBinaryHandler,
    CustomTagError,
    CustomTagHandler,
    CustomTagParser,
    CustomTagRegistry,
    EnvVarHandler,
    JsonHandler,
    TimestampHandler,
};

/// A text position in a YAML document, represented as byte offsets.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TextPosition {
    /// The start byte offset
    pub start: u32,
    /// The end byte offset (exclusive)
    pub end: u32,
}

impl TextPosition {
    /// Create a new text position
    pub fn new(start: u32, end: u32) -> Self {
        Self { start, end }
    }

    /// Get the length of this text range
    pub fn len(&self) -> u32 {
        self.end - self.start
    }

    /// Check if this range is empty
    pub fn is_empty(&self) -> bool {
        self.start == self.end
    }
}

/// A line and column position in a YAML document (1-indexed).
///
/// Line and column numbers are both 1-indexed (first line is line 1, first column is column 1).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LineColumn {
    /// Line number (1-indexed)
    pub line: usize,
    /// Column number (1-indexed, counts Unicode scalar values)
    pub column: usize,
}

impl LineColumn {
    /// Create a new line/column position
    pub fn new(line: usize, column: usize) -> Self {
        Self { line, column }
    }
}

impl std::fmt::Display for LineColumn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

/// Convert a byte offset to line and column numbers in the given text.
///
/// Line and column numbers are 1-indexed. Column numbers count Unicode scalar values,
/// not bytes or grapheme clusters.
///
/// # Arguments
///
/// * `text` - The full source text
/// * `byte_offset` - Byte offset into the text
///
/// # Returns
///
/// `LineColumn` with 1-indexed line and column numbers, or line 1, column 1 if offset is out of bounds.
///
/// # Examples
///
/// ```
/// use yaml_edit::byte_offset_to_line_column;
///
/// let text = "line 1\nline 2\nline 3";
/// let pos = byte_offset_to_line_column(text, 7); // Start of "line 2"
/// assert_eq!(pos.line, 2);
/// assert_eq!(pos.column, 1);
/// ```
pub fn byte_offset_to_line_column(text: &str, byte_offset: usize) -> LineColumn {
    let mut line = 1;
    let mut column = 1;

    for (i, ch) in text.char_indices() {
        if i >= byte_offset {
            break;
        }

        if ch == '\n' {
            line += 1;
            column = 1;
        } else {
            column += 1;
        }
    }

    LineColumn { line, column }
}

impl From<rowan::TextRange> for TextPosition {
    fn from(range: rowan::TextRange) -> Self {
        Self {
            start: u32::from(range.start()),
            end: u32::from(range.end()),
        }
    }
}

impl From<TextPosition> for rowan::TextRange {
    fn from(pos: TextPosition) -> Self {
        rowan::TextRange::new(pos.start.into(), pos.end.into())
    }
}

/// The kind of parse error, enabling structured matching without string parsing.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ParseErrorKind {
    /// An unclosed flow sequence (missing `]`)
    UnclosedFlowSequence,
    /// An unclosed flow mapping (missing `}`)
    UnclosedFlowMapping,
    /// An unterminated quoted string (missing closing quote)
    UnterminatedString,
    /// Any other parse error
    Other,
}

/// A positioned parse error containing location information.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PositionedParseError {
    /// The error message
    pub message: String,
    /// The text range where the error occurred
    pub range: TextPosition,
    /// Optional error code for categorization
    pub code: Option<String>,
    /// Structured error kind
    pub kind: ParseErrorKind,
}

impl PositionedParseError {
    /// Get the line and column where this error starts (if source text is available).
    ///
    /// # Arguments
    ///
    /// * `source_text` - The original YAML source text
    ///
    /// # Returns
    ///
    /// `LineColumn` with 1-indexed line and column numbers.
    ///
    /// # Examples
    ///
    /// ```
    /// use yaml_edit::{YamlFile, Parse};
    /// use std::str::FromStr;
    ///
    /// let text = "invalid:\n  - [unclosed";
    /// let parse = Parse::parse_yaml(text);
    ///
    /// if let Some(err) = parse.positioned_errors().first() {
    ///     let pos = err.start_position(text);
    ///     assert_eq!(pos.line, 2);
    /// }
    /// ```
    pub fn start_position(&self, source_text: &str) -> LineColumn {
        byte_offset_to_line_column(source_text, self.range.start as usize)
    }

    /// Get the line and column where this error ends (if source text is available).
    ///
    /// # Arguments
    ///
    /// * `source_text` - The original YAML source text
    ///
    /// # Returns
    ///
    /// `LineColumn` with 1-indexed line and column numbers.
    pub fn end_position(&self, source_text: &str) -> LineColumn {
        byte_offset_to_line_column(source_text, self.range.end as usize)
    }
}

impl std::fmt::Display for PositionedParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for PositionedParseError {}

/// The indentation to use when writing a YAML file.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Indentation {
    /// Use the same indentation as the original line for the value.
    FieldNameLength,

    /// The number of spaces to use for indentation.
    Spaces(u32),
}

impl Default for Indentation {
    fn default() -> Self {
        Indentation::Spaces(2)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::str::FromStr;

    #[test]
    fn test_byte_offset_to_line_column_basic() {
        let text = "line 1\nline 2\nline 3";

        let pos = byte_offset_to_line_column(text, 0);
        assert_eq!(pos.line, 1);
        assert_eq!(pos.column, 1);

        let pos = byte_offset_to_line_column(text, 7);
        assert_eq!(pos.line, 2);
        assert_eq!(pos.column, 1);

        let pos = byte_offset_to_line_column(text, 10);
        assert_eq!(pos.line, 2);
        assert_eq!(pos.column, 4);

        let pos = byte_offset_to_line_column(text, 14);
        assert_eq!(pos.line, 3);
        assert_eq!(pos.column, 1);
    }

    #[test]
    fn test_byte_offset_to_line_column_unicode() {
        let text = "hello\n世界\nworld";

        let pos = byte_offset_to_line_column(text, 0);
        assert_eq!(pos.line, 1);
        assert_eq!(pos.column, 1);

        let pos = byte_offset_to_line_column(text, 6);
        assert_eq!(pos.line, 2);
        assert_eq!(pos.column, 1);

        // After first Chinese character "世" (3 bytes)
        let pos = byte_offset_to_line_column(text, 9);
        assert_eq!(pos.line, 2);
        assert_eq!(pos.column, 2);
    }

    #[test]
    fn test_line_column_display() {
        let pos = LineColumn::new(42, 17);
        assert_eq!(format!("{}", pos), "42:17");

        let pos2 = LineColumn::new(1, 1);
        assert_eq!(format!("{}", pos2), "1:1");
    }

    #[test]
    fn test_document_position() {
        let text = "name: Alice\nage: 30";
        let doc = Document::from_str(text).unwrap();

        let start = doc.start_position(text);
        assert_eq!(start.line, 1);
        assert_eq!(start.column, 1);

        let range = doc.byte_range();
        assert_eq!(range.start, 0);
        assert!(range.end > 0);
    }

    #[test]
    fn test_mapping_position() {
        let text = "server:\n  host: localhost\n  port: 8080";
        let doc = Document::from_str(text).unwrap();
        let mapping = doc.as_mapping().unwrap();

        let start = mapping.start_position(text);
        assert_eq!(start.line, 1);
        assert_eq!(start.column, 1);

        let server_mapping = mapping.get_mapping("server").unwrap();
        let server_start = server_mapping.start_position(text);
        assert_eq!(server_start.line, 2);
    }

    #[test]
    fn test_scalar_position_via_nodes() {
        let text = "name: Alice\nage: 30";
        let doc = Document::from_str(text).unwrap();
        let mapping = doc.as_mapping().unwrap();

        let entries: Vec<_> = mapping.entries().collect();
        assert!(entries.len() >= 2);

        let first_entry = &entries[0];
        let key_node = first_entry.key_node().unwrap();
        assert_eq!(key_node.to_string().trim(), "name");

        let value_node = first_entry.value_node().unwrap();
        assert_eq!(value_node.to_string().trim(), "Alice");
    }

    #[test]
    fn test_sequence_position() {
        let text = "items:\n  - apple\n  - banana";
        let doc = Document::from_str(text).unwrap();
        let mapping = doc.as_mapping().unwrap();

        let items_node = mapping.get("items").unwrap();
        assert!(items_node.as_sequence().is_some());
    }

    #[test]
    fn test_positioned_parse_error() {
        let text = "invalid:\n  - [unclosed";
        let parse = Parse::parse_yaml(text);

        let errors = parse.positioned_errors();
        if errors.is_empty() {
            return;
        }

        let err = &errors[0];
        let start = err.start_position(text);
        assert_eq!(start.line, 2);
    }

    #[test]
    fn test_multiline_document_byte_offsets() {
        let text = "# Comment\nname: Alice\n\nage: 30";
        let doc = Document::from_str(text).unwrap();

        let range = doc.byte_range();
        assert_eq!(range.start, 10);
        assert_eq!(range.end, 30);

        let start = doc.start_position(text);
        assert_eq!(start.line, 2);
        assert_eq!(start.column, 1);
    }

    #[test]
    fn test_nested_mapping_byte_ranges() {
        let text = "server:\n  database:\n    host: localhost";
        let doc = Document::from_str(text).unwrap();
        let mapping = doc.as_mapping().unwrap();

        let server_mapping = mapping.get_mapping("server").unwrap();
        let server_range = server_mapping.byte_range();

        assert!(server_range.end > server_range.start);

        let server_pos = server_mapping.start_position(text);
        assert!(server_pos.line > 0);
    }

    #[test]
    fn test_empty_lines_positions() {
        let text = "a: 1\n\n\nb: 2";

        let pos1 = byte_offset_to_line_column(text, 0);
        assert_eq!(pos1.line, 1);

        let pos2 = byte_offset_to_line_column(text, 7);
        assert_eq!(pos2.line, 4);
    }

    #[test]
    fn test_document_end_position() {
        let text = "key: value";
        let doc = Document::from_str(text).unwrap();

        let start = doc.start_position(text);
        let end = doc.end_position(text);

        assert_eq!(start.line, 1);
        assert!(end.column >= start.column);
    }

    #[test]
    fn test_mapping_end_position() {
        let text = "a: 1\nb: 2";
        let doc = Document::from_str(text).unwrap();
        let mapping = doc.as_mapping().unwrap();

        let start = mapping.start_position(text);
        let end = mapping.end_position(text);

        assert_eq!(start.line, 1);
        assert_eq!(end.line, 2);
    }
}
