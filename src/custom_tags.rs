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
//! let e = registry.validate("!timestamp", "invalid-date").unwrap_err();
//! assert_eq!(e.tag, "!timestamp");
//! assert_eq!(e.message, "Expected format: %Y-%m-%d");
//! assert_eq!(e.content.as_deref(), Some("invalid-date"));
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
//!    called via the registry, or for a whole document via
//!    [`CustomTagParser::parse_with_custom_tags`].
//!
//! 3. **Thread Safety**: Tag handlers are `Send + Sync` and the registry can be shared across
//!    threads safely.
//!
//! 4. **Tag Naming**: Tag names must start with `!` and contain only alphanumeric characters,
//!    hyphens, underscores, dots, colons, and forward slashes.

use crate::lex::SyntaxKind;
use crate::nodes::SyntaxToken;
use crate::scalar::{ScalarType, ScalarValue};
use crate::value::YamlValue;
use crate::yaml::{Document, SyntaxNode, TaggedNode};
use rowan::ast::AstNode;
use rowan::NodeOrToken;
use std::collections::HashMap;
use std::sync::{Arc, RwLock};

/// A custom tag handler that can serialize and deserialize values
///
/// This trait provides type-safe custom tag handling without using `dyn Any`.
/// All handlers work with `YamlValue` for serialization/deserialization,
/// ensuring type safety and proper error handling.
///
/// Primarily for internal use - custom tag handlers are typically registered through
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

impl std::fmt::Debug for CustomTagRegistry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Handlers are trait objects with no Debug bound; list the tags they
        // are registered under instead.
        let mut dbg = f.debug_struct("CustomTagRegistry");
        match self.handlers.read() {
            Ok(handlers) => {
                let mut tags: Vec<&str> = handlers.keys().map(String::as_str).collect();
                tags.sort_unstable();
                dbg.field("tags", &tags).finish()
            }
            Err(_) => dbg.finish_non_exhaustive(),
        }
    }
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
                "Invalid tag name: must be a YAML tag such as '!name', '!!name' or '!<uri>'",
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

    /// Run `f` against the handler registered for `tag`. Its errors name
    /// `tag`, since a handler cannot know what it was registered under.
    fn with_handler<T>(
        &self,
        tag: &str,
        f: impl FnOnce(&dyn CustomTagHandler) -> Result<T, CustomTagError>,
    ) -> Result<T, CustomTagError> {
        let handlers = self.handlers.read().unwrap();
        let handler = handlers
            .get(tag)
            .ok_or_else(|| CustomTagError::new(tag, "Tag not registered"))?;
        f(handler.as_ref()).map_err(|error| CustomTagError {
            tag: tag.to_string(),
            ..error
        })
    }

    /// Serialize a value using a custom tag
    pub fn serialize(&self, tag: &str, value: &YamlValue) -> Result<String, CustomTagError> {
        self.with_handler(tag, |h| h.serialize(value))
    }

    /// Deserialize a value using a custom tag
    pub fn deserialize(&self, tag: &str, content: &str) -> Result<YamlValue, CustomTagError> {
        self.with_handler(tag, |h| h.deserialize(content))
    }

    /// Validate content for a custom tag
    pub fn validate(&self, tag: &str, content: &str) -> Result<(), CustomTagError> {
        self.with_handler(tag, |h| h.validate(content))
    }

    /// Check whether `tag` is a tag property as YAML 1.2 spells it
    /// (`c-ns-tag-property`): a verbatim tag (`!<...>`) or a shorthand
    /// (`!local`, `!!str`, `!e!foo`). The non-specific `!` names no type.
    fn is_valid_tag_name(tag: &str) -> bool {
        let Some(rest) = tag.strip_prefix('!') else {
            return false;
        };
        if let Some(uri) = rest.strip_prefix('<') {
            return uri
                .strip_suffix('>')
                .is_some_and(|uri| is_uri_run(uri, |_| true));
        }
        // c-named-tag-handle is `!` ns-word-char+ `!`, and `!!` the
        // secondary handle; anything else uses the primary `!`.
        let suffix = match rest.split_once('!') {
            Some((name, suffix)) if name.chars().all(is_word_char) => suffix,
            Some(_) => return false,
            None => rest,
        };
        is_uri_run(suffix, |c| !matches!(c, '!' | ',' | '[' | ']' | '{' | '}'))
    }
}

/// `ns-word-char`: an ASCII letter, digit or `-`.
fn is_word_char(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '-'
}

/// Whether `s` is one or more `ns-uri-char`s accepted by `allowed`: `%`
/// escapes, word characters and URI punctuation.
fn is_uri_run(s: &str, allowed: impl Fn(char) -> bool) -> bool {
    let mut chars = s.chars();
    let mut empty = true;
    while let Some(c) = chars.next() {
        empty = false;
        let ok = if c == '%' {
            chars.next().is_some_and(|c| c.is_ascii_hexdigit())
                && chars.next().is_some_and(|c| c.is_ascii_hexdigit())
        } else {
            (is_word_char(c) || "#;/?:@&=+$,_.!~*'()[]".contains(c)) && allowed(c)
        };
        if !ok {
            return false;
        }
    }
    !empty
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

    /// Run every tagged node in `document` through the registry, as
    /// [`process_tagged_node`](Self::process_tagged_node) does, stopping at
    /// the first error.
    pub fn parse_with_custom_tags(&self, document: &Document) -> Result<(), CustomTagError> {
        document
            .syntax()
            .descendants()
            .filter_map(TaggedNode::cast)
            .try_for_each(|tagged| self.process_tagged_node(&tagged).map(|_| ()))
    }

    /// Deserialize a tagged node with the handler registered for its tag.
    ///
    /// A scalar is passed to the handler as its string value, with quotes
    /// and escapes resolved; a flow collection as its source text. A block
    /// collection is passed as its source text with its indentation in the
    /// document removed, as it would read at the top level, so positions in
    /// a handler's errors are relative to that text.
    pub fn process_tagged_node(
        &self,
        tagged_node: &TaggedNode,
    ) -> Result<YamlValue, CustomTagError> {
        let tag = tagged_node
            .tag()
            .ok_or_else(|| CustomTagError::new("", "Invalid tagged scalar"))?;
        let content = if let Some(scalar) = tagged_node.value() {
            scalar.as_string()
        } else {
            let (collection, is_flow) = tagged_node
                .as_mapping()
                .map(|m| (m.syntax().clone(), m.is_flow_style()))
                .or_else(|| {
                    let s = tagged_node.as_sequence()?;
                    Some((s.syntax().clone(), s.is_flow_style()))
                })
                .ok_or_else(|| CustomTagError::new(&tag, "Invalid tagged node"))?;
            if is_flow {
                collection.text().to_string()
            } else {
                dedented_block_text(&collection)
            }
        };
        self.registry.deserialize(&tag, &content)
    }
}

/// The text of block collection `collection` with its indentation in the
/// document removed from every line after the first, which starts at the
/// collection's column.
///
/// A line indented less than that (a blank line, a comment, a quoted
/// scalar's continuation) loses only the indentation it has, which in those
/// places carries no meaning.
fn dedented_block_text(collection: &SyntaxNode) -> String {
    let column = crate::column::node_column(collection);
    let text = collection.text().to_string();
    let mut lines = text.split_inclusive('\n');
    let mut dedented = lines.next().unwrap_or_default().to_string();
    for line in lines {
        let indent = crate::column::space_indent(line);
        dedented.push_str(&line[indent.min(column)..]);
    }
    dedented
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
        if content.trim().is_empty() {
            return Err(CustomTagError::with_content(
                "!timestamp",
                "Timestamp cannot be empty",
                content,
            ));
        }
        self.deserialize(content).map(|_| ())
    }
}

/// A custom tag handler for JSON data
pub struct JsonHandler;

impl CustomTagHandler for JsonHandler {
    fn serialize(&self, value: &YamlValue) -> Result<String, CustomTagError> {
        // Convert YamlValue to JSON string
        match value {
            YamlValue::Scalar(s) => json_scalar(s),
            YamlValue::Sequence(seq) => {
                let items: Result<Vec<_>, _> = seq.iter().map(|v| self.serialize(v)).collect();
                let items = items?;
                Ok(format!("[{}]", items.join(",")))
            }
            YamlValue::Mapping(map) => {
                let pairs: Result<Vec<_>, _> = map
                    .iter()
                    .map(|(k, v)| Ok(format!("{}:{}", json_quote(k), self.serialize(v)?)))
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

    /// Content that is not strictly valid JSON is accepted as the raw
    /// string, as with any other scalar; [`validate`](Self::validate)
    /// reports it instead.
    fn deserialize(&self, content: &str) -> Result<YamlValue, CustomTagError> {
        parse_json(content, false)
    }

    fn validate(&self, content: &str) -> Result<(), CustomTagError> {
        parse_json(content, true).map(|_| ())
    }

    fn description(&self) -> &str {
        "JSON data embedded in YAML"
    }
}

/// Parse `content` as JSON. Unless `strict`, content that is not valid JSON
/// is returned as the raw string.
fn parse_json(content: &str, strict: bool) -> Result<YamlValue, CustomTagError> {
    let content = content.trim();
    if content == "\"" || content == "'" {
        return Err(CustomTagError::with_content(
            "!json",
            "Unterminated quoted string",
            content,
        ));
    }
    let message = match json_document(content) {
        Ok(value) => return Ok(value),
        Err(message) => message,
    };
    if strict {
        return Err(CustomTagError::with_content("!json", message, content));
    }
    let inner = content.strip_prefix('"').and_then(|c| c.strip_suffix('"'));
    Ok(YamlValue::scalar(inner.unwrap_or(content)))
}

const INVALID_JSON: &str = "Invalid JSON value";

/// Parse `content` with the YAML parser, accepting only the JSON subset of
/// flow syntax.
fn json_document(content: &str) -> Result<YamlValue, String> {
    let parsed = crate::YamlFile::parse(content);
    if let Some(error) = parsed.errors().into_iter().next() {
        return Err(format!("Invalid JSON: {error}"));
    }
    let root = parsed.tree();
    let document = json_only_node(root.syntax())?;
    if document.kind() != SyntaxKind::DOCUMENT {
        return Err(INVALID_JSON.to_string());
    }
    let value = json_value(&json_only_node(&document)?)?;
    if let Some(violation) = crate::validator::Validator::new()
        .validate_syntax(root.syntax())
        .into_iter()
        .find(|v| v.severity == crate::validator::Severity::Error)
    {
        return Err(format!("Invalid JSON: {}", violation.message));
    }
    Ok(value)
}

/// The children of `node` other than whitespace, rejecting YAML-only
/// trivia and node properties.
fn json_significant(
    node: &SyntaxNode,
) -> Result<Vec<NodeOrToken<SyntaxNode, SyntaxToken>>, String> {
    let mut significant = Vec::new();
    for child in node.children_with_tokens() {
        match child.kind() {
            SyntaxKind::WHITESPACE | SyntaxKind::NEWLINE | SyntaxKind::INDENT => {}
            SyntaxKind::COMMENT => return Err("Comments are not allowed in JSON".to_string()),
            SyntaxKind::TAG | SyntaxKind::TAGGED_NODE => {
                return Err("Tags are not allowed in JSON".to_string())
            }
            SyntaxKind::ANCHOR => return Err("Anchors are not allowed in JSON".to_string()),
            SyntaxKind::ALIAS | SyntaxKind::REFERENCE => {
                return Err("Aliases are not allowed in JSON".to_string())
            }
            _ => significant.push(child),
        }
    }
    Ok(significant)
}

fn json_value(node: &SyntaxNode) -> Result<YamlValue, String> {
    match node.kind() {
        SyntaxKind::SCALAR => {
            let text = json_scalar_text(node)?;
            match json_string(&text) {
                Some(string) => string.map(YamlValue::scalar),
                None => json_literal(&text)
                    .map(YamlValue::Scalar)
                    .ok_or_else(|| INVALID_JSON.to_string()),
            }
        }
        SyntaxKind::SEQUENCE => {
            let entries = json_entries(node, SyntaxKind::LEFT_BRACKET, SyntaxKind::RIGHT_BRACKET)?;
            let items = entries.iter().map(|entry| match entry.as_slice() {
                [NodeOrToken::Node(item)] => json_value(item),
                _ => Err(INVALID_JSON.to_string()),
            });
            items.collect::<Result<_, _>>().map(YamlValue::Sequence)
        }
        SyntaxKind::MAPPING => {
            let entries = json_entries(node, SyntaxKind::LEFT_BRACE, SyntaxKind::RIGHT_BRACE)?;
            let mut map = std::collections::BTreeMap::new();
            for entry in entries {
                let [NodeOrToken::Node(key), NodeOrToken::Token(colon), NodeOrToken::Node(value)] =
                    entry.as_slice()
                else {
                    return Err(INVALID_JSON.to_string());
                };
                if colon.kind() != SyntaxKind::COLON {
                    return Err(INVALID_JSON.to_string());
                }
                let key = json_key(key)?;
                let value = json_value(&json_only_node(value)?)?;
                if map.insert(key, value).is_some() {
                    return Err("Duplicate key".to_string());
                }
            }
            Ok(YamlValue::Mapping(map))
        }
        _ => Err(INVALID_JSON.to_string()),
    }
}

/// The single node among the significant children of `node`.
fn json_only_node(node: &SyntaxNode) -> Result<SyntaxNode, String> {
    match json_significant(node)?.as_slice() {
        [NodeOrToken::Node(inner)] => Ok(inner.clone()),
        _ => Err(INVALID_JSON.to_string()),
    }
}

fn json_key(node: &SyntaxNode) -> Result<String, String> {
    let not_string = || "Object keys must be strings".to_string();
    let scalar = json_only_node(node)?;
    if scalar.kind() != SyntaxKind::SCALAR {
        return Err(not_string());
    }
    json_string(&json_scalar_text(&scalar)?).ok_or_else(not_string)?
}

/// The text of the single token making up a scalar.
fn json_scalar_text(node: &SyntaxNode) -> Result<String, String> {
    match json_significant(node)?.as_slice() {
        [NodeOrToken::Token(token)] => Ok(token.text().to_string()),
        _ => Err(INVALID_JSON.to_string()),
    }
}

/// Decode a JSON string literal, or `None` if `text` is not one.
fn json_string(text: &str) -> Option<Result<String, String>> {
    let inner = text.strip_prefix('"')?.strip_suffix('"')?;
    Some(json_unescape(inner).map_err(String::from))
}

/// The significant children of each entry of the flow collection `node`,
/// without the separating commas.
fn json_entries(
    node: &SyntaxNode,
    open: SyntaxKind,
    close: SyntaxKind,
) -> Result<Vec<Vec<NodeOrToken<SyntaxNode, SyntaxToken>>>, String> {
    let children = json_significant(node)?;
    let [first, entries @ .., last] = children.as_slice() else {
        return Err(INVALID_JSON.to_string());
    };
    if first.kind() != open || last.kind() != close {
        return Err(INVALID_JSON.to_string());
    }
    let mut result = Vec::with_capacity(entries.len());
    for (i, entry) in entries.iter().enumerate() {
        let NodeOrToken::Node(entry) = entry else {
            return Err(INVALID_JSON.to_string());
        };
        let mut children = json_significant(entry)?;
        let has_comma = children
            .last()
            .is_some_and(|c| c.kind() == SyntaxKind::COMMA);
        if has_comma {
            children.pop();
        }
        match (has_comma, i + 1 == entries.len()) {
            (true, true) => return Err("Trailing comma".to_string()),
            (false, false) => return Err(INVALID_JSON.to_string()),
            _ => result.push(children),
        }
    }
    Ok(result)
}

fn json_quote(s: &str) -> String {
    let mut out = String::with_capacity(s.len() + 2);
    out.push('"');
    for c in s.chars() {
        match c {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            '\u{8}' => out.push_str("\\b"),
            '\u{c}' => out.push_str("\\f"),
            c if c < '\u{20}' => out.push_str(&format!("\\u{:04x}", c as u32)),
            c => out.push(c),
        }
    }
    out.push('"');
    out
}

fn json_scalar(s: &ScalarValue) -> Result<String, CustomTagError> {
    let unrepresentable =
        || CustomTagError::with_content("!json", "Value cannot be represented in JSON", s.value());
    match s.scalar_type() {
        ScalarType::Null => Ok("null".to_string()),
        ScalarType::Boolean => s
            .to_bool()
            .map(|b| b.to_string())
            .ok_or_else(unrepresentable),
        // JSON integers have no size limit.
        ScalarType::Integer => {
            ScalarValue::canonical_integer(s.value().trim()).ok_or_else(unrepresentable)
        }
        ScalarType::Float => json_float(s.value().trim()).ok_or_else(unrepresentable),
        _ => Ok(json_quote(s.value())),
    }
}

/// Respell a YAML core schema float as a JSON number, keeping its digits,
/// or `None` for `.inf`, `.nan` and anything else JSON cannot spell.
///
/// The text is kept rather than going through `f64`, which would round it
/// and turn values beyond its range into infinity.
fn json_float(text: &str) -> Option<String> {
    if !ScalarValue::is_core_float(text) {
        return None;
    }
    let minus = if text.starts_with('-') { "-" } else { "" };
    let body = text.strip_prefix(['+', '-']).unwrap_or(text);
    let (mantissa, exponent) = body.split_at(body.find(['e', 'E']).unwrap_or(body.len()));
    let (int, frac) = match mantissa.split_once('.') {
        // `1.` keeps its point, so the value still reads as a float.
        Some((int, frac)) => (int, Some(if frac.is_empty() { "0" } else { frac })),
        None => (mantissa, None),
    };
    let int = match int.trim_start_matches('0') {
        "" => "0",
        int => int,
    };
    let frac = frac.map(|f| format!(".{f}")).unwrap_or_default();
    Some(format!("{minus}{int}{frac}{exponent}"))
}

/// Decode the body of a JSON string literal (without the surrounding quotes).
fn json_unescape(inner: &str) -> Result<String, &'static str> {
    fn hex4(chars: &mut std::str::Chars) -> Result<u32, &'static str> {
        let digits: String = chars.take(4).collect();
        if digits.len() != 4 {
            return Err("Truncated \\u escape");
        }
        u32::from_str_radix(&digits, 16).map_err(|_| "Invalid \\u escape")
    }

    let mut out = String::with_capacity(inner.len());
    let mut chars = inner.chars();
    while let Some(c) = chars.next() {
        match c {
            '"' => return Err("Unescaped quote in string"),
            c if c < '\u{20}' => return Err("Unescaped control character in string"),
            '\\' => {
                let decoded = match chars.next().ok_or("Unterminated escape sequence")? {
                    '"' => '"',
                    '\\' => '\\',
                    '/' => '/',
                    'b' => '\u{8}',
                    'f' => '\u{c}',
                    'n' => '\n',
                    'r' => '\r',
                    't' => '\t',
                    'u' => {
                        let hi = hex4(&mut chars)?;
                        let code = if (0xD800..0xDC00).contains(&hi) {
                            if chars.next() != Some('\\') || chars.next() != Some('u') {
                                return Err("Unpaired surrogate in \\u escape");
                            }
                            let lo = hex4(&mut chars)?;
                            if !(0xDC00..0xE000).contains(&lo) {
                                return Err("Unpaired surrogate in \\u escape");
                            }
                            0x10000 + ((hi - 0xD800) << 10) + (lo - 0xDC00)
                        } else {
                            hi
                        };
                        char::from_u32(code).ok_or("Unpaired surrogate in \\u escape")?
                    }
                    _ => return Err("Invalid escape sequence"),
                };
                out.push(decoded);
            }
            c => out.push(c),
        }
    }
    Ok(out)
}

/// Parse a JSON number, `true`, `false` or `null`.
fn json_literal(content: &str) -> Option<ScalarValue> {
    match content {
        "true" => return Some(ScalarValue::from(true)),
        "false" => return Some(ScalarValue::from(false)),
        "null" => return Some(ScalarValue::null()),
        _ => {}
    }
    let digits = |s: &str| s.len() - s.trim_start_matches(|c: char| c.is_ascii_digit()).len();
    let rest = content.strip_prefix('-').unwrap_or(content);
    let int_len = digits(rest);
    if int_len == 0 || (int_len > 1 && rest.starts_with('0')) {
        return None;
    }
    let mut rest = &rest[int_len..];
    if let Some(frac) = rest.strip_prefix('.') {
        let n = digits(frac);
        if n == 0 {
            return None;
        }
        rest = &frac[n..];
    }
    if let Some(exp) = rest.strip_prefix(['e', 'E']) {
        let exp = exp.strip_prefix(['+', '-']).unwrap_or(exp);
        let n = digits(exp);
        if n == 0 {
            return None;
        }
        rest = &exp[n..];
    }
    rest.is_empty().then(|| ScalarValue::parse(content))
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

            // TODO: compress the data; for now only the marker records the level
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
        // TODO: decompress according to the level in the marker
        Ok(YamlValue::scalar(compressed_payload(content)))
    }

    fn description(&self) -> &str {
        "Compressed binary data"
    }

    fn validate(&self, content: &str) -> Result<(), CustomTagError> {
        // Basic base64 validation
        if compressed_payload(content)
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

/// `content` without the `COMPRESSED[level]:` marker, if it has one.
fn compressed_payload(content: &str) -> &str {
    content
        .strip_prefix("COMPRESSED[")
        .and_then(|rest| rest.split_once("]:"))
        .map_or(content, |(_level, data)| data)
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

        for tag in [
            "!!binary",
            "!e!foo",
            "!at@char",
            "!a%21b",
            "!<tag:yaml.org,2002:str>",
            "!<!>",
            "!~*'()",
        ] {
            assert!(CustomTagRegistry::is_valid_tag_name(tag), "{tag:?}");
        }

        for tag in [
            "json",
            // The non-specific tag names no type.
            "!",
            "!!",
            "!invalid space",
            "!a,b",
            "!a{b}",
            "!a!",
            "!e!",
            "!e!f!g",
            "!a%2",
            "!a%zz",
            "!a\"b",
            "!<>",
            "!<a b>",
            "!<a",
            "!naïve",
        ] {
            assert!(!CustomTagRegistry::is_valid_tag_name(tag), "{tag:?}");
        }
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
    fn test_json_handler_deserialize_collections() {
        let handler = JsonHandler;
        let num = |s: &str| YamlValue::Scalar(ScalarValue::parse(s));
        let map = |pairs: Vec<(&str, YamlValue)>| {
            YamlValue::Mapping(pairs.into_iter().map(|(k, v)| (k.to_string(), v)).collect())
        };

        assert_eq!(
            handler.deserialize("[1,2]"),
            Ok(YamlValue::Sequence(vec![num("1"), num("2")]))
        );
        assert_eq!(handler.deserialize("[]"), Ok(YamlValue::Sequence(vec![])));
        assert_eq!(handler.deserialize(" { } "), Ok(map(vec![])));
        assert_eq!(
            handler.deserialize(
                r#"{"a": [1, "x", true, null], "b":{"c":-1.5e2, "d\n\u00e9": "\"y\""}}"#
            ),
            Ok(map(vec![
                (
                    "a",
                    YamlValue::Sequence(vec![
                        num("1"),
                        YamlValue::scalar("x"),
                        YamlValue::scalar(true),
                        YamlValue::Scalar(ScalarValue::null()),
                    ])
                ),
                (
                    "b",
                    map(vec![
                        ("c", num("-1.5e2")),
                        ("d\n\u{e9}", YamlValue::scalar("\"y\"")),
                    ])
                ),
            ]))
        );
        assert_eq!(
            handler.deserialize("{\r\n\t\"a\" : [\n  1 ,\n  [ ]\n ]\n}\n"),
            Ok(map(vec![(
                "a",
                YamlValue::Sequence(vec![num("1"), YamlValue::Sequence(vec![])])
            )]))
        );
        assert_eq!(handler.validate(r#"{"a": [{"b": []}]}"#), Ok(()));
    }

    #[test]
    fn test_json_handler_rejects_yaml_only_syntax() {
        let handler = JsonHandler;
        for (content, message) in [
            ("{a: 1}", "Object keys must be strings"),
            ("{1: 2}", "Object keys must be strings"),
            (r#"{"a": x}"#, "Invalid JSON value"),
            ("['a']", "Invalid JSON value"),
            ("[1, , 2]", "Invalid JSON value"),
            (r#"{"a"}"#, "Invalid JSON value"),
            (r#"["a": 1]"#, "Invalid JSON value"),
            ("- 1", "Invalid JSON value"),
            ("a: 1", "Invalid JSON value"),
            ("--- [1]", "Invalid JSON value"),
            (
                "[1] [2]",
                "Invalid JSON: 1:5: Content after the document could not be parsed: \"[\"",
            ),
            ("[1, 2,]", "Trailing comma"),
            (r#"{"a": 1,}"#, "Trailing comma"),
            (r#"{"a": 1, "a": 2}"#, "Duplicate key"),
            ("[1, # c\n 2]", "Comments are not allowed in JSON"),
            ("[!t 1]", "Tags are not allowed in JSON"),
            ("!t [1]", "Tags are not allowed in JSON"),
            ("[&a 1]", "Anchors are not allowed in JSON"),
            ("[*a]", "Aliases are not allowed in JSON"),
            (r#"["a\x41"]"#, "Invalid escape sequence"),
            ("[\"a\n b\"]", "Unescaped control character in string"),
        ] {
            assert_eq!(
                handler.validate(content),
                Err(CustomTagError::with_content("!json", message, content)),
                "{}",
                content
            );
            // Lenient deserialization reads it as the raw string.
            assert_eq!(
                handler.deserialize(content),
                Ok(YamlValue::scalar(content)),
                "{}",
                content
            );
        }
    }

    #[test]
    fn test_json_handler_reports_parse_errors() {
        let handler = JsonHandler;
        let err = handler.validate("[1, 2").unwrap_err();
        assert_eq!(
            err,
            CustomTagError::with_content(
                "!json",
                concat!(
                    "Invalid JSON: 1:6: Unclosed flow sequence. Expected: ']' to close sequence. ",
                    "Found: end of input. Context: in flow sequence. Suggestion: Add ']' to close ",
                    "the array, or check for missing commas between elements"
                ),
                "[1, 2"
            )
        );
        assert_eq!(handler.deserialize("[1, 2"), Ok(YamlValue::scalar("[1, 2")));
    }

    #[test]
    fn test_json_handler_deserialize_returns_err_not_panic() {
        let handler = JsonHandler;
        let quote = handler.deserialize("\"").unwrap_err();
        assert_eq!(quote.tag, "!json");
        assert_eq!(quote.message, "Unterminated quoted string");
        assert_eq!(
            handler
                .deserialize("\"hello\"")
                .unwrap()
                .as_scalar()
                .unwrap()
                .value(),
            "hello"
        );
    }

    #[test]
    fn test_json_handler_serialize_escapes_strings() {
        let handler = JsonHandler;
        assert_eq!(
            handler.serialize(&YamlValue::scalar("a\"b\\c")).unwrap(),
            r#""a\"b\\c""#
        );
        assert_eq!(
            handler
                .serialize(&YamlValue::scalar("l1\nl2\t\u{1}"))
                .unwrap(),
            r#""l1\nl2\t\u0001""#
        );
        assert_eq!(
            handler.serialize(&YamlValue::scalar("42")).unwrap(),
            "\"42\""
        );

        let mut map = std::collections::BTreeMap::new();
        map.insert("k\"".to_string(), YamlValue::scalar("v"));
        assert_eq!(
            handler.serialize(&YamlValue::Mapping(map)).unwrap(),
            r#"{"k\"":"v"}"#
        );
    }

    #[test]
    fn test_json_handler_serialize_non_strings() {
        let handler = JsonHandler;
        assert_eq!(handler.serialize(&YamlValue::scalar(42i64)).unwrap(), "42");
        assert_eq!(handler.serialize(&YamlValue::scalar(-7i32)).unwrap(), "-7");
        assert_eq!(
            handler.serialize(&YamlValue::scalar(1.5f64)).unwrap(),
            "1.5"
        );
        assert_eq!(handler.serialize(&YamlValue::scalar(true)).unwrap(), "true");
        assert_eq!(
            handler
                .serialize(&YamlValue::Scalar(ScalarValue::null()))
                .unwrap(),
            "null"
        );
        assert_eq!(
            handler
                .serialize(&YamlValue::Scalar(ScalarValue::parse("0x1F")))
                .unwrap(),
            "31"
        );
        let err = handler
            .serialize(&YamlValue::scalar(f64::INFINITY))
            .unwrap_err();
        assert_eq!(err.message, "Value cannot be represented in JSON");
    }

    #[test]
    fn test_json_handler_serialize_floats_as_written() {
        let handler = JsonHandler;
        let json = |text: &str| handler.serialize(&YamlValue::Scalar(ScalarValue::parse(text)));
        // JSON numbers have no range limit, so a float beyond f64 is kept.
        assert_eq!(json("1e400").unwrap(), "1e400");
        assert_eq!(json("-1.5E+400").unwrap(), "-1.5E+400");
        assert_eq!(json("1.0").unwrap(), "1.0");
        assert_eq!(
            json("0.10000000000000000000001").unwrap(),
            "0.10000000000000000000001"
        );
        // YAML spellings that JSON lacks.
        assert_eq!(json("+1.5").unwrap(), "1.5");
        assert_eq!(json(".5").unwrap(), "0.5");
        assert_eq!(json("-.5e3").unwrap(), "-0.5e3");
        assert_eq!(json("1.").unwrap(), "1.0");
        assert_eq!(json("1.e3").unwrap(), "1.0e3");
        assert_eq!(json("007.5").unwrap(), "7.5");
        assert_eq!(json("00.5").unwrap(), "0.5");
        assert_eq!(json("1e3").unwrap(), "1e3");

        for text in [".inf", "-.inf", "+.INF", ".nan", ".NaN"] {
            let err = json(text).unwrap_err();
            assert_eq!(err.message, "Value cannot be represented in JSON", "{text}");
        }
        let err = handler.serialize(&YamlValue::scalar(f64::NAN)).unwrap_err();
        assert_eq!(err.message, "Value cannot be represented in JSON");

        for text in ["1e400", "-2.5e-400", "1.0", "0.1"] {
            let value = handler.deserialize(text).unwrap();
            assert_eq!(handler.serialize(&value).unwrap(), text);
        }
    }

    #[test]
    fn test_json_handler_serialize_integers_of_any_size() {
        let handler = JsonHandler;
        let json = |text: &str| {
            handler
                .serialize(&YamlValue::Scalar(ScalarValue::parse(text)))
                .unwrap()
        };
        assert_eq!(
            json("123456789012345678901234567890"),
            "123456789012345678901234567890"
        );
        assert_eq!(
            json("-123456789012345678901234567890"),
            "-123456789012345678901234567890"
        );
        assert_eq!(
            json("+00123456789012345678901234567890"),
            "123456789012345678901234567890"
        );
        assert_eq!(
            json("0x100000000000000000000000000000000"),
            "340282366920938463463374607431768211456"
        );
        assert_eq!(json("0o2000000000000000000000"), "18446744073709551616");

        // A JSON integer outside i64 comes back as it went in.
        let big = "-98765432109876543210987654321";
        let value = handler.deserialize(big).unwrap();
        assert_eq!(handler.serialize(&value).unwrap(), big);
    }

    #[test]
    fn test_json_handler_deserialize_unescapes_strings() {
        let handler = JsonHandler;
        let value = |s: &str| handler.deserialize(s).unwrap().as_scalar().unwrap().clone();
        assert_eq!(value(r#""a\"b\u0041""#).value(), "a\"bA");
        assert_eq!(value(r#""\\\/\b\f\n\r\t""#).value(), "\\/\u{8}\u{c}\n\r\t");
        assert_eq!(value(r#""\ud83d\ude00""#).value(), "\u{1F600}");
        assert_eq!(
            value(r#""42""#).scalar_type(),
            crate::scalar::ScalarType::String
        );

        for bad in [
            r#""a"b""#,
            r#""a\x""#,
            r#""\u12""#,
            r#""\ud83d""#,
            "\"a\u{1}\"",
            r#""a\""#,
        ] {
            // Invalid JSON is read as the raw string, but does not validate.
            assert_eq!(value(bad).value(), &bad[1..bad.len() - 1], "{}", bad);
            let err = handler.validate(bad).unwrap_err();
            assert_eq!(err.tag, "!json", "{}", bad);
            assert_eq!(err.content.as_deref(), Some(bad), "{}", bad);
        }
    }

    #[test]
    fn test_json_handler_deserialize_non_strings() {
        let handler = JsonHandler;
        let value = |s: &str| handler.deserialize(s).unwrap().as_scalar().unwrap().clone();
        assert_eq!(value("42").to_i64(), Some(42));
        assert_eq!(value("-0").to_i64(), Some(0));
        assert_eq!(value("1.5e3").to_f64(), Some(1500.0));
        assert_eq!(value("true").to_bool(), Some(true));
        assert_eq!(value("null").scalar_type(), crate::scalar::ScalarType::Null);

        for bad in ["plain", "0x1F", "01", "1.", ".5", "+1", "True", "~"] {
            assert_eq!(value(bad).value(), bad);
            let err = handler.validate(bad).unwrap_err();
            assert_eq!(err.message, "Invalid JSON value", "{}", bad);
        }
        assert_eq!(handler.validate("42"), Ok(()));
        assert_eq!(handler.validate(r#""a\"b""#), Ok(()));
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

    /// Records the content of every node it is given and rejects "bad".
    struct RecordingHandler(Arc<std::sync::Mutex<Vec<String>>>);

    impl CustomTagHandler for RecordingHandler {
        fn serialize(&self, _value: &YamlValue) -> Result<String, CustomTagError> {
            Err(CustomTagError::new("!rec", "not used"))
        }

        fn deserialize(&self, content: &str) -> Result<YamlValue, CustomTagError> {
            self.0.lock().unwrap().push(content.to_string());
            if content == "bad" {
                return Err(CustomTagError::with_content("!rec", "rejected", content));
            }
            Ok(YamlValue::scalar(content))
        }

        fn description(&self) -> &str {
            "records its input"
        }
    }

    fn recording_parser() -> (CustomTagParser, Arc<std::sync::Mutex<Vec<String>>>) {
        let seen = Arc::new(std::sync::Mutex::new(Vec::new()));
        let mut registry = CustomTagRegistry::new();
        registry
            .register("!rec", RecordingHandler(Arc::clone(&seen)))
            .unwrap();
        (CustomTagParser::new(registry), seen)
    }

    fn doc(text: &str) -> Document {
        use std::str::FromStr;
        Document::from_str(text).unwrap()
    }

    #[test]
    fn test_parse_with_custom_tags_visits_every_tagged_node() {
        let (parser, seen) = recording_parser();
        let document = doc(concat!(
            "a: !rec one\n",
            "? !rec two\n",
            ": [!rec three, {!rec four: !rec '5'}]\n",
            "c: !rec {\"x\": !rec six}\n",
            "d:\n",
            "  - !rec \"seven\"\n",
        ));
        assert_eq!(parser.parse_with_custom_tags(&document), Ok(()));
        assert_eq!(
            *seen.lock().unwrap(),
            vec![
                "one",
                "two",
                "three",
                "four",
                "5",
                "{\"x\": !rec six}",
                "six",
                "seven"
            ]
        );
    }

    #[test]
    fn test_parse_with_custom_tags_returns_first_error() {
        let (parser, seen) = recording_parser();
        let document = doc("[!rec ok, !rec bad, !rec bad]\n");
        assert_eq!(
            parser.parse_with_custom_tags(&document),
            Err(CustomTagError::with_content("!rec", "rejected", "bad"))
        );
        assert_eq!(*seen.lock().unwrap(), vec!["ok", "bad"]);
    }

    #[test]
    fn test_parse_with_custom_tags_unregistered_tag() {
        let (parser, _) = recording_parser();
        let document = doc("a: !rec x\nb: !other y\n");
        assert_eq!(
            parser.parse_with_custom_tags(&document),
            Err(CustomTagError::new("!other", "Tag not registered"))
        );
    }

    #[test]
    fn test_parse_with_custom_tags_block_collection() {
        // A handler sees a block collection as it would read at the top
        // level, without the indentation it has in the document.
        let (parser, seen) = recording_parser();
        let document = doc(concat!(
            "a: !rec\n",
            "  b: !rec x\n",
            "  c:\n",
            "    - 1\n",
            "d:\n",
            "  - !rec\n",
            "    -  y\n",
            "    - z\n",
        ));
        assert_eq!(parser.parse_with_custom_tags(&document), Ok(()));
        assert_eq!(
            *seen.lock().unwrap(),
            vec!["b: !rec x\nc:\n  - 1\n", "x", "-  y\n- z\n"]
        );

        let (parser, seen) = recording_parser();
        let document = doc("--- !rec\na: 1\nb:\n  - 2\n");
        assert_eq!(parser.parse_with_custom_tags(&document), Ok(()));
        assert_eq!(*seen.lock().unwrap(), vec!["a: 1\nb:\n  - 2\n"]);
    }

    #[test]
    fn test_parse_with_custom_tags_block_collection_keeps_relative_indent() {
        // Blank lines, comments and block scalar bodies keep their
        // indentation relative to the collection.
        let (parser, seen) = recording_parser();
        let document = doc(concat!(
            "items:\n",
            "  - !rec\n",
            "    k: |\n",
            "      line\n",
            "        more\n",
            "\n",
            "    # note\n",
            "    j: \"a\n",
            "   b\"\n",
        ));
        assert_eq!(parser.parse_with_custom_tags(&document), Ok(()));
        assert_eq!(
            *seen.lock().unwrap(),
            vec!["k: |\n  line\n    more\n\n# note\nj: \"a\nb\"\n"]
        );
    }

    #[test]
    fn test_process_tagged_block_mapping() {
        let (parser, _) = recording_parser();
        let document = doc("a: !rec\n  b: 1\n");
        let tagged = document.as_mapping().unwrap().get("a").unwrap();
        assert_eq!(
            parser.process_tagged_node(tagged.as_tagged().unwrap()),
            Ok(YamlValue::scalar("b: 1\n"))
        );
    }

    #[test]
    fn test_process_tagged_json_object() {
        let mut registry = CustomTagRegistry::new();
        registry.register("!json", JsonHandler).unwrap();
        let parser = CustomTagParser::new(registry);
        let document = doc("data: !json {\"status\": \"active\"}\n");
        let mapping = document.as_mapping().unwrap();
        let data = mapping.get("data").unwrap();
        let tagged = data.as_tagged().unwrap();
        let mut expected = std::collections::BTreeMap::new();
        expected.insert("status".to_string(), YamlValue::scalar("active"));
        assert_eq!(
            parser.process_tagged_node(tagged),
            Ok(YamlValue::Mapping(expected))
        );
        assert_eq!(parser.parse_with_custom_tags(&document), Ok(()));
    }

    #[test]
    fn test_parse_with_custom_tags_untagged_document() {
        let (parser, seen) = recording_parser();
        assert_eq!(
            parser.parse_with_custom_tags(&doc("a: [1, {b: c}]\n")),
            Ok(())
        );
        assert_eq!(*seen.lock().unwrap(), Vec::<String>::new());
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

    #[test]
    fn test_timestamp_validate_checks_format() {
        let handler = TimestampHandler::new("%Y-%m-%d");
        let expected = Err(CustomTagError::with_content(
            "!timestamp",
            "Expected format: %Y-%m-%d",
            "invalid-date",
        ));
        assert_eq!(handler.deserialize("invalid-date").map(|_| ()), expected);
        assert_eq!(handler.validate("invalid-date"), expected);
        assert_eq!(handler.validate("2024-01-15"), Ok(()));
    }

    #[test]
    fn test_compressed_binary_validates_own_output() {
        let handler = CompressedBinaryHandler::new(6);
        let serialized = handler.serialize(&YamlValue::scalar("SGk=")).unwrap();
        assert_eq!(serialized, "COMPRESSED[6]:SGk=");
        assert_eq!(handler.validate(&serialized), Ok(()));
        assert_eq!(
            handler.validate("COMPRESSED[6]:no base64"),
            Err(CustomTagError::with_content(
                "!compressed",
                "Invalid base64 content",
                "COMPRESSED[6]:no base64",
            ))
        );
    }

    #[test]
    fn test_registry_errors_name_registered_tag() {
        let mut registry = CustomTagRegistry::new();
        registry
            .register("!date", TimestampHandler::new("%Y-%m-%d"))
            .unwrap();
        let expected = CustomTagError::with_content("!date", "Timestamp cannot be empty", "");
        assert_eq!(registry.validate("!date", ""), Err(expected));
        assert_eq!(
            registry.deserialize("!date", "x"),
            Err(CustomTagError::with_content(
                "!date",
                "Expected format: %Y-%m-%d",
                "x"
            ))
        );
        assert_eq!(
            registry.serialize("!date", &YamlValue::sequence()),
            Err(CustomTagError::new("!date", "Value must be a scalar"))
        );
    }

    #[test]
    fn test_json_handler_validate_messages() {
        let handler = JsonHandler;
        for (content, message) in [
            (r#"{"a" 1}"#, "Invalid JSON value"),
            (r#"{"a": 1, "b"}"#, "Invalid JSON value"),
            (r#"{"a": }"#, "Invalid JSON value"),
            (r#"{? "a": 1}"#, "Invalid JSON value"),
            (r#"["a": 1]"#, "Invalid JSON value"),
            ("{1: 2}", "Object keys must be strings"),
            ("{[1]: 2}", "Object keys must be strings"),
            ("[1,,2]", "Invalid JSON value"),
            ("[1 2]", "Invalid JSON value"),
            ("- 1", "Invalid JSON value"),
            ("a: 1", "Invalid JSON value"),
            ("--- 1", "Invalid JSON value"),
            ("1\n--- 2", "Invalid JSON value"),
            ("%YAML 1.2\n--- 1", "Invalid JSON value"),
            (r#"{"a":1}x"#, "Invalid JSON value"),
            ("", "Invalid JSON value"),
            ("-", "Invalid JSON value"),
            ("1e+", "Invalid JSON value"),
            (r#"{"a": 1, "a": 2}"#, "Duplicate key"),
            ("[1,]", "Trailing comma"),
            ("{,}", "Trailing comma"),
            ("[&x 1]", "Anchors are not allowed in JSON"),
            ("[*x]", "Aliases are not allowed in JSON"),
            ("[1] # c", "Comments are not allowed in JSON"),
            ("[!!int 1]", "Tags are not allowed in JSON"),
            (r#""\ud800A""#, "Unpaired surrogate in \\u escape"),
            (r#""\udc00""#, "Unpaired surrogate in \\u escape"),
            ("\"", "Unterminated quoted string"),
        ] {
            assert_eq!(
                handler.validate(content),
                Err(CustomTagError::with_content(
                    "!json",
                    message,
                    content.trim()
                )),
                "{content}"
            );
        }
        assert_eq!(
            handler.deserialize("'"),
            Err(CustomTagError::with_content(
                "!json",
                "Unterminated quoted string",
                "'"
            ))
        );
    }

    #[test]
    fn test_json_handler_nested_and_numbers() {
        let handler = JsonHandler;
        let value = handler
            .deserialize(r#"[1, [2.5e-3, {"a": [true, null], "b": "é"}]]"#)
            .unwrap();
        assert_eq!(handler.validate(r#"{"a":1,"b":2}"#), Ok(()));
        assert_eq!(
            handler.serialize(&value).unwrap(),
            r#"[1,[2.5e-3,{"a":[true,null],"b":"é"}]]"#
        );
        assert_eq!(handler.validate("1e5"), Ok(()));
        assert_eq!(handler.validate("-1.5E+3"), Ok(()));
    }

    #[test]
    fn test_json_handler_serialize_escapes_and_unsupported() {
        let handler = JsonHandler;
        assert_eq!(
            handler.serialize(&YamlValue::scalar("a\r\u{8}\u{c}\u{1}\"\\\n\t")),
            Ok(r#""a\r\b\f\u0001\"\\\n\t""#.to_string())
        );
        assert_eq!(
            handler.serialize(&YamlValue::set()),
            Err(CustomTagError::new(
                "!json",
                "Unsupported value type for JSON serialization"
            ))
        );
        assert_eq!(handler.description(), "JSON data embedded in YAML");
    }

    #[test]
    fn test_timestamp_handler_errors() {
        let handler = TimestampHandler::new("%Y-%m-%d");
        assert_eq!(
            handler.serialize(&YamlValue::scalar("2024-01-15")),
            Ok("2024-01-15".to_string())
        );
        assert_eq!(
            handler.serialize(&YamlValue::scalar("today")),
            Err(CustomTagError::with_content(
                "!timestamp",
                "Timestamp should contain year digits",
                "today"
            ))
        );
        assert_eq!(
            handler.serialize(&YamlValue::sequence()),
            Err(CustomTagError::new("!timestamp", "Value must be a scalar"))
        );
        assert_eq!(
            handler.deserialize("2024-01-15"),
            Ok(YamlValue::scalar(ScalarValue::timestamp("2024-01-15")))
        );
        assert_eq!(handler.description(), "Custom timestamp format handler");
        // A format without `%Y` does not check for digits.
        let handler = TimestampHandler::new("%H:%M");
        assert_eq!(
            handler.serialize(&YamlValue::scalar("noon")),
            Ok("noon".to_string())
        );
        assert_eq!(handler.validate("noon"), Ok(()));
    }

    #[test]
    fn test_compressed_binary_handler_round_trip() {
        let handler = CompressedBinaryHandler::new(0);
        assert_eq!(
            handler.serialize(&YamlValue::scalar("SGk=")),
            Ok("SGk=".to_string())
        );
        assert_eq!(
            handler.serialize(&YamlValue::sequence()),
            Err(CustomTagError::new("!compressed", "Value must be a scalar"))
        );
        assert_eq!(
            handler.deserialize("COMPRESSED[3]:SGk="),
            Ok(YamlValue::scalar("SGk="))
        );
        assert_eq!(
            handler.deserialize("COMPRESSED[3"),
            Ok(YamlValue::scalar("COMPRESSED[3"))
        );
        assert_eq!(handler.deserialize("SGk="), Ok(YamlValue::scalar("SGk=")));
        assert_eq!(handler.description(), "Compressed binary data");
    }

    #[test]
    fn test_env_var_handler_errors() {
        let handler = EnvVarHandler;
        assert_eq!(
            handler.serialize(&YamlValue::scalar("HOME")),
            Ok("HOME".to_string())
        );
        assert_eq!(
            handler.serialize(&YamlValue::sequence()),
            Err(CustomTagError::new(
                "!env",
                "Environment variable must be a scalar"
            ))
        );
        assert_eq!(
            handler.deserialize("YAML_EDIT_SURELY_UNSET_VARIABLE"),
            Err(CustomTagError::with_content(
                "!env",
                "Environment variable not found",
                "YAML_EDIT_SURELY_UNSET_VARIABLE"
            ))
        );
        assert_eq!(handler.description(), "Environment variable substitution");
    }

    #[test]
    fn test_default_validate_deserializes() {
        let (parser, seen) = recording_parser();
        assert_eq!(
            parser.registry.validate("!rec", "bad"),
            Err(CustomTagError::with_content("!rec", "rejected", "bad"))
        );
        assert_eq!(parser.registry.validate("!rec", "good"), Ok(()));
        assert_eq!(*seen.lock().unwrap(), vec!["bad", "good"]);
    }

    #[test]
    fn test_registry_debug_clone_unregister() {
        let mut registry = CustomTagRegistry::new();
        registry.register("!json", JsonHandler).unwrap();
        registry.register("!env", EnvVarHandler).unwrap();
        assert_eq!(
            format!("{registry:?}"),
            r#"CustomTagRegistry { tags: ["!env", "!json"] }"#
        );
        // Clones share their handlers.
        let mut clone = registry.clone();
        assert!(clone.unregister("!env"));
        assert!(!clone.unregister("!env"));
        assert!(!registry.has_tag("!env"));
        assert_eq!(registry.registered_tags(), vec!["!json".to_string()]);
    }

    #[test]
    fn test_custom_tag_error_display_without_content() {
        assert_eq!(
            CustomTagError::new("!t", "broken").to_string(),
            "Custom tag '!t' error: broken"
        );
    }

    #[test]
    fn test_tag_name_with_invalid_named_handle() {
        let mut registry = CustomTagRegistry::new();
        for tag in ["!a_b!c", "!a.b!c", "!!", "!e!"] {
            assert_eq!(
                registry.register(tag, JsonHandler),
                Err(CustomTagError::new(
                    tag,
                    "Invalid tag name: must be a YAML tag such as '!name', '!!name' or '!<uri>'"
                )),
                "{tag}"
            );
        }
        assert_eq!(registry.register("!a-1!c", JsonHandler), Ok(()));
    }

    #[test]
    fn test_json_handler_spaces_and_false() {
        let handler = JsonHandler;
        assert_eq!(
            handler.serialize(&YamlValue::scalar("a b")),
            Ok("\"a b\"".to_string())
        );
        assert_eq!(handler.validate("\"a b\""), Ok(()));
        assert_eq!(handler.validate("false"), Ok(()));
        assert_eq!(
            handler.deserialize("false"),
            Ok(YamlValue::Scalar(ScalarValue::from(false)))
        );
    }
}
