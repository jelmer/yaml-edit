//! Path-based access to YAML documents.
//!
//! Provides convenient dot-separated path syntax for accessing nested YAML values
//! like `"server.host"` or `"database.primary.port"`.
//!
//! Operations: [`get_path`](YamlPath::get_path), [`set_path`](YamlPath::set_path), [`remove_path`](YamlPath::remove_path)
//!
//! # Example
//!
//! ```
//! use yaml_edit::{Document, path::YamlPath};
//! use std::str::FromStr;
//!
//! let yaml = Document::from_str("server:\n  host: localhost\n  port: 8080\n").unwrap();
//!
//! // Get nested values
//! let host = yaml.get_path("server.host");
//!
//! // Set nested values (creates intermediate mappings)
//! yaml.set_path("database.primary.host", "db.example.com");
//!
//! // Remove nested values
//! yaml.remove_path("server.port");
//! ```
//!
//! All operations preserve formatting, comments, and whitespace.

use crate::builder::MappingBuilder;
use crate::yaml::Mapping;

/// Trait for YAML types that support path-based access.
///
/// Path syntax uses dots (`.`) as separators to navigate nested mappings.
/// For example, `"server.database.host"` accesses:
/// ```yaml
/// server:
///   database:
///     host: value
/// ```
pub trait YamlPath {
    /// Get a value at a nested path.
    ///
    /// # Arguments
    ///
    /// * `path` - Dot-separated path like `"server.host"` or `"db.primary.port"`
    ///
    /// # Returns
    ///
    /// `Some(YamlNode)` if the path exists, `None` otherwise.
    ///
    /// # Examples
    ///
    /// ```
    /// use yaml_edit::{Document, path::YamlPath};
    /// use std::str::FromStr;
    ///
    /// let yaml = Document::from_str("server:\n  host: localhost\n").unwrap();
    /// let host = yaml.get_path("server.host");
    /// assert!(host.is_some());
    /// ```
    fn get_path(&self, path: &str) -> Option<crate::as_yaml::YamlNode>;

    /// Set a value at a nested path, creating intermediate mappings if needed.
    ///
    /// # Arguments
    ///
    /// * `path` - Dot-separated path like `"server.host"`
    /// * `value` - Value to set (can be any type implementing `AsYaml`)
    ///
    /// # Examples
    ///
    /// ```
    /// use yaml_edit::{Document, path::YamlPath};
    /// use std::str::FromStr;
    ///
    /// let yaml = Document::from_str("name: test\n").unwrap();
    /// yaml.set_path("server.host", "localhost");
    /// yaml.set_path("server.port", 8080);
    /// ```
    fn set_path(&self, path: &str, value: impl crate::AsYaml);

    /// Remove a value at a nested path.
    ///
    /// # Arguments
    ///
    /// * `path` - Dot-separated path to the value to remove
    ///
    /// # Returns
    ///
    /// `true` if the value was found and removed, `false` otherwise.
    ///
    /// # Examples
    ///
    /// ```
    /// use yaml_edit::{Document, path::YamlPath};
    /// use std::str::FromStr;
    ///
    /// let yaml = Document::from_str("server:\n  host: localhost\n  port: 8080\n").unwrap();
    /// assert_eq!(yaml.remove_path("server.port"), true);
    /// assert_eq!(yaml.remove_path("server.missing"), false);
    /// ```
    fn remove_path(&self, path: &str) -> bool;
}

/// Represents a segment in a YAML path.
#[derive(Debug, Clone, PartialEq)]
pub enum PathSegment {
    /// A mapping key (e.g., "server" in "server.host")
    Key(String),
    /// An array index (e.g., `0` in "items\[0\]" or "items.0")
    Index(usize),
}

/// Error from [`try_parse_path`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PathParseError {
    /// A `[` was not closed by `]`.
    UnclosedIndex,
    /// The text between `[` and `]` is not a `usize` index.
    InvalidIndex(String),
}

impl std::fmt::Display for PathParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PathParseError::UnclosedIndex => {
                write!(f, "unclosed '[' in path")
            }
            PathParseError::InvalidIndex(text) => {
                write!(f, "invalid path index [{text}]")
            }
        }
    }
}

impl std::error::Error for PathParseError {}

/// Parse a path string into components.
///
/// Supports multiple syntaxes:
/// - Dot notation: `"server.host"` → `[Key("server"), Key("host")]`
/// - Array indices with brackets: `"items[0].name"` → `[Key("items"), Index(0), Key("name")]`
/// - Array indices with dots: `"items.0.name"` → `[Key("items"), Index(0), Key("name")]`
/// - Escaped dots: `"key\\.with\\.dots"` → `[Key("key.with.dots")]`
///
/// An empty path returns an empty list. A malformed `[...]` index is an
/// error (unclosed `]` or a non-`usize` index).
///
/// # Examples
///
/// ```
/// use yaml_edit::path::{try_parse_path, PathSegment};
///
/// let segments = try_parse_path("server.host").unwrap();
/// assert_eq!(segments, vec![
///     PathSegment::Key("server".to_string()),
///     PathSegment::Key("host".to_string())
/// ]);
///
/// let segments = try_parse_path("items[0].name").unwrap();
/// assert_eq!(segments, vec![
///     PathSegment::Key("items".to_string()),
///     PathSegment::Index(0),
///     PathSegment::Key("name".to_string())
/// ]);
///
/// let segments = try_parse_path("items.0").unwrap();
/// assert_eq!(segments, vec![
///     PathSegment::Key("items".to_string()),
///     PathSegment::Index(0)
/// ]);
/// ```
///
/// # Errors
///
/// Returns [`PathParseError::UnclosedIndex`] when a `[` has no matching `]`,
/// or [`PathParseError::InvalidIndex`] when the brackets do not contain a
/// `usize`.
pub fn try_parse_path(path: &str) -> Result<Vec<PathSegment>, PathParseError> {
    if path.is_empty() {
        return Ok(vec![]);
    }

    let mut segments = Vec::new();
    let mut current = String::new();
    let mut chars = path.chars().peekable();
    let mut escaped = false;

    while let Some(ch) = chars.next() {
        if escaped {
            // Previous character was backslash, add this character literally
            current.push(ch);
            escaped = false;
            continue;
        }

        match ch {
            '\\' => {
                // Escape next character
                escaped = true;
            }
            '.' => {
                // Segment separator
                if !current.is_empty() {
                    // Check if current segment is a number (for array index notation like "items.0")
                    if let Ok(index) = current.parse::<usize>() {
                        segments.push(PathSegment::Index(index));
                    } else {
                        segments.push(PathSegment::Key(current.clone()));
                    }
                    current.clear();
                }
            }
            '[' => {
                // Array index with bracket notation
                if !current.is_empty() {
                    segments.push(PathSegment::Key(current.clone()));
                    current.clear();
                }

                // Parse the index until we hit ']'
                let mut index_str = String::new();
                let mut closed = false;
                while let Some(&next_ch) = chars.peek() {
                    if next_ch == ']' {
                        chars.next(); // consume the ']'
                        closed = true;
                        break;
                    }
                    index_str.push(chars.next().unwrap());
                }

                if !closed {
                    return Err(PathParseError::UnclosedIndex);
                }
                match index_str.parse::<usize>() {
                    Ok(index) => segments.push(PathSegment::Index(index)),
                    Err(_) => return Err(PathParseError::InvalidIndex(index_str)),
                }
            }
            _ => {
                current.push(ch);
            }
        }
    }

    // Add the last segment
    if !current.is_empty() {
        if let Ok(index) = current.parse::<usize>() {
            segments.push(PathSegment::Index(index));
        } else {
            segments.push(PathSegment::Key(current));
        }
    }

    Ok(segments)
}

/// Parse a path string into components.
///
/// Invalid bracket indexes used to return an empty `Vec`, which callers
/// could not tell from a real empty path. Prefer [`try_parse_path`].
#[deprecated(note = "use try_parse_path; a bad [index] used to return an empty Vec")]
pub fn parse_path(path: &str) -> Vec<PathSegment> {
    try_parse_path(path).unwrap_or_default()
}

fn path_segments(path: &str) -> Option<Vec<PathSegment>> {
    let segments = try_parse_path(path).ok()?;
    if segments.is_empty() {
        None
    } else {
        Some(segments)
    }
}

/// Navigate through a YAML structure following path segments.
///
/// Handles both mapping keys and sequence indices.
fn navigate_path(
    mut current: crate::as_yaml::YamlNode,
    segments: &[PathSegment],
) -> Option<crate::as_yaml::YamlNode> {
    for segment in segments {
        match segment {
            PathSegment::Key(key) => {
                // Navigate through a mapping
                let mapping = current.as_mapping()?;
                current = mapping.get(key)?;
            }
            PathSegment::Index(index) => {
                // Navigate through a sequence
                let sequence = current.as_sequence()?;
                current = sequence.get(*index)?;
            }
        }
    }

    Some(current)
}

// Implementation for Document
impl YamlPath for crate::yaml::Document {
    fn get_path(&self, path: &str) -> Option<crate::as_yaml::YamlNode> {
        let segments = path_segments(path)?;

        // Start from the document's root content
        let root = if let Some(m) = self.as_mapping() {
            crate::as_yaml::YamlNode::Mapping(m)
        } else if let Some(s) = self.as_sequence() {
            crate::as_yaml::YamlNode::Sequence(s)
        } else {
            let sc = self.as_scalar()?;
            crate::as_yaml::YamlNode::Scalar(sc)
        };

        // Navigate through the path segments
        navigate_path(root, &segments)
    }

    fn set_path(&self, path: &str, value: impl crate::AsYaml) {
        let Some(segments) = path_segments(path) else {
            return;
        };

        // Get the root mapping (can only set paths on mappings at the root)
        let mapping = match self.as_mapping() {
            Some(m) => m,
            None => return,
        };

        set_path_impl(&mapping, &segments, value);
    }

    fn remove_path(&self, path: &str) -> bool {
        let Some(segments) = path_segments(path) else {
            return false;
        };

        // Start from document root
        let root = if let Some(m) = self.as_mapping() {
            crate::as_yaml::YamlNode::Mapping(m)
        } else if let Some(s) = self.as_sequence() {
            crate::as_yaml::YamlNode::Sequence(s)
        } else {
            return false;
        };

        remove_path_impl(root, &segments)
    }
}

/// Set a value at a path, creating intermediate mappings as needed.
///
/// This is used by Document::set_path() to handle the full path navigation.
fn set_path_impl<V: crate::AsYaml>(mapping: &Mapping, segments: &[PathSegment], value: V) {
    set_path_on_mapping(mapping, segments, value);
}

/// Remove a value at a nested path.
///
/// This is used by Document::remove_path() to handle the full path navigation.
fn remove_path_impl(root: crate::as_yaml::YamlNode, segments: &[PathSegment]) -> bool {
    if segments.is_empty() {
        return false;
    }

    if segments.len() == 1 {
        // Base case: remove from the current node
        match &segments[0] {
            PathSegment::Key(key) => {
                if let Some(mapping) = root.as_mapping() {
                    return mapping.remove(key.as_str()).is_some();
                }
            }
            PathSegment::Index(_) => {
                // Removing by index from a sequence is not supported
                // (would require shifting all subsequent elements)
                return false;
            }
        }
        return false;
    }

    // Navigate to the parent and recurse
    match &segments[0] {
        PathSegment::Key(key) => {
            if let Some(mapping) = root.as_mapping() {
                if let Some(nested) = mapping.get(key.as_str()) {
                    return remove_path_impl(nested, &segments[1..]);
                }
            }
        }
        PathSegment::Index(index) => {
            if let Some(sequence) = root.as_sequence() {
                if let Some(nested) = sequence.get(*index) {
                    return remove_path_impl(nested, &segments[1..]);
                }
            }
        }
    }

    false
}

// Implementation for Mapping
impl YamlPath for Mapping {
    fn get_path(&self, path: &str) -> Option<crate::as_yaml::YamlNode> {
        let segments = path_segments(path)?;

        // Start from the first segment (must be a key for mappings)
        let first_key = match &segments[0] {
            PathSegment::Key(key) => key.as_str(),
            PathSegment::Index(_) => return None, // Can't index into a mapping directly
        };

        if segments.len() == 1 {
            return self.get(first_key);
        }

        // Get the value at the first key and navigate the rest
        let current = self.get(first_key)?;
        navigate_path(current, &segments[1..])
    }

    fn set_path(&self, path: &str, value: impl crate::AsYaml) {
        let Some(segments) = path_segments(path) else {
            return;
        };

        set_path_on_mapping(self, &segments, value);
    }

    fn remove_path(&self, path: &str) -> bool {
        let Some(segments) = path_segments(path) else {
            return false;
        };

        remove_path_from_mapping(self, &segments)
    }
}

/// Set a value at a path on a mapping, creating intermediate mappings or
/// sequences as needed.
///
/// Uses only the public API (get_mapping, get_sequence, set) and does NOT
/// rebuild nodes. Recurses through `set_path_on_sequence` when a segment
/// dives into a sequence.
fn set_path_on_mapping<V: crate::AsYaml>(mapping: &Mapping, segments: &[PathSegment], value: V) {
    if segments.is_empty() {
        return;
    }

    // First segment must be a key for mappings
    let first_key = match &segments[0] {
        PathSegment::Key(key) => key.as_str(),
        PathSegment::Index(_) => return, // Can't set by index on a mapping
    };

    if segments.len() == 1 {
        // Base case: set directly
        mapping.set(first_key, value);
        return;
    }

    // What container does the next segment expect at `first_key`?
    let next_wants_sequence = matches!(segments[1], PathSegment::Index(_));

    if next_wants_sequence {
        if let Some(nested) = mapping.get_sequence(first_key) {
            set_path_on_sequence(&nested, &segments[1..], value);
            return;
        }
        // Match the parent's style: nested-under-flow keeps flow, so
        // the intermediate sequence is created via SequenceBuilder
        // (renders as `[]`). Nested-under-block gets a bare empty
        // SEQUENCE (renders as block after push).
        if mapping.is_flow_style() {
            let flow_empty = crate::builder::SequenceBuilder::new()
                .build_document()
                .as_sequence()
                .expect("SequenceBuilder always produces a sequence");
            mapping.set(first_key, flow_empty);
        } else {
            mapping.set(first_key, crate::yaml::Sequence::new());
        }
        if let Some(nested) = mapping.get_sequence(first_key) {
            set_path_on_sequence(&nested, &segments[1..], value);
        }
        return;
    }

    if let Some(nested) = mapping.get_mapping(first_key) {
        set_path_on_mapping(&nested, &segments[1..], value);
        return;
    }

    // Match the parent's style so we don't mix block content into a flow
    // container. `Mapping::new()` is a bare empty MAPPING (renders block);
    // `MappingBuilder::new()` produces the flow-empty `{}` form.
    if mapping.is_flow_style() {
        let flow_empty = MappingBuilder::new()
            .build_document()
            .as_mapping()
            .expect("MappingBuilder always produces a mapping");
        mapping.set(first_key, flow_empty);
    } else {
        mapping.set(first_key, Mapping::new());
    }

    if let Some(nested) = mapping.get_mapping(first_key) {
        set_path_on_mapping(&nested, &segments[1..], value);
    }
}

/// Set a value at a path on a sequence, growing it and creating intermediate
/// containers as needed.
///
/// The segments slice must start with an `Index`. Missing entries up to
/// `index` are pushed as null scalars; the target entry is replaced (single-
/// segment path) or descended into (multi-segment path).
fn set_path_on_sequence<V: crate::AsYaml>(
    sequence: &crate::yaml::Sequence,
    segments: &[PathSegment],
    value: V,
) {
    if segments.is_empty() {
        return;
    }
    let index = match &segments[0] {
        PathSegment::Index(i) => *i,
        PathSegment::Key(_) => return, // Can't key into a sequence
    };

    // Grow the sequence with null placeholders until `index` is in range.
    while sequence.len() <= index {
        sequence.push(crate::scalar::ScalarValue::null());
    }

    if segments.len() == 1 {
        sequence.set(index, value);
        return;
    }

    let next_wants_sequence = matches!(segments[1], PathSegment::Index(_));

    if next_wants_sequence {
        if let Some(nested) = sequence.get(index).and_then(|n| n.as_sequence().cloned()) {
            set_path_on_sequence(&nested, &segments[1..], value);
            return;
        }
        // Nested-sequence-under-sequence: use SequenceBuilder to
        // create a flow-empty `[]`. A block SEQUENCE nested inline
        // after `- ` renders as a compact-block shape that re-parses
        // as a plain scalar, so we need to keep the inner sequence
        // flow. push/insert see `must_render_flow` and preserve
        // that flow style through the subsequent set.
        let flow_empty = crate::builder::SequenceBuilder::new()
            .build_document()
            .as_sequence()
            .expect("SequenceBuilder always produces a sequence");
        sequence.set(index, flow_empty);
        if let Some(nested) = sequence.get(index).and_then(|n| n.as_sequence().cloned()) {
            set_path_on_sequence(&nested, &segments[1..], value);
        }
        return;
    }

    if let Some(nested) = sequence.get(index).and_then(|n| n.as_mapping().cloned()) {
        set_path_on_mapping(&nested, &segments[1..], value);
        return;
    }
    let flow_empty = crate::builder::MappingBuilder::new()
        .build_document()
        .as_mapping()
        .expect("MappingBuilder always produces a mapping");
    sequence.set(index, flow_empty);
    if let Some(nested) = sequence.get(index).and_then(|n| n.as_mapping().cloned()) {
        set_path_on_mapping(&nested, &segments[1..], value);
    }
}

/// Remove a value at a path from a mapping.
///
/// This function uses only the public API and does NOT rebuild nodes.
fn remove_path_from_mapping(mapping: &Mapping, segments: &[PathSegment]) -> bool {
    if segments.is_empty() {
        return false;
    }

    // First segment must be a key for mappings
    let first_key = match &segments[0] {
        PathSegment::Key(key) => key.as_str(),
        PathSegment::Index(_) => return false, // Can't index into a mapping
    };

    if segments.len() == 1 {
        // Base case: remove directly
        return mapping.remove(first_key).is_some();
    }

    // Navigate to the parent mapping and recurse
    if let Some(nested) = mapping.get_mapping(first_key) {
        remove_path_from_mapping(&nested, &segments[1..])
    } else {
        false // Path doesn't exist
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_path_basic() {
        assert_eq!(try_parse_path("").unwrap(), Vec::<PathSegment>::new());
        assert_eq!(
            try_parse_path("key").unwrap(),
            vec![PathSegment::Key("key".to_string())]
        );
        assert_eq!(
            try_parse_path("a.b").unwrap(),
            vec![
                PathSegment::Key("a".to_string()),
                PathSegment::Key("b".to_string())
            ]
        );
        assert_eq!(
            try_parse_path("a.b.c.d").unwrap(),
            vec![
                PathSegment::Key("a".to_string()),
                PathSegment::Key("b".to_string()),
                PathSegment::Key("c".to_string()),
                PathSegment::Key("d".to_string())
            ]
        );
    }

    #[test]
    fn test_try_parse_path_invalid_brackets() {
        assert_eq!(
            try_parse_path("items[abc].name"),
            Err(PathParseError::InvalidIndex("abc".to_string()))
        );
        assert_eq!(
            try_parse_path("items[].name"),
            Err(PathParseError::InvalidIndex(String::new()))
        );
        assert_eq!(
            try_parse_path("items[0"),
            Err(PathParseError::UnclosedIndex)
        );
    }

    #[test]
    #[allow(deprecated)]
    fn test_parse_path_deprecated_wrapper_empty_on_bad_index() {
        assert_eq!(parse_path("items[abc].name"), Vec::<PathSegment>::new());
        assert_eq!(parse_path("items[0"), Vec::<PathSegment>::new());
    }

    #[test]
    fn test_parse_path_with_array_indices() {
        assert_eq!(
            try_parse_path("items[0]").unwrap(),
            vec![PathSegment::Key("items".to_string()), PathSegment::Index(0)]
        );
        assert_eq!(
            try_parse_path("items[0].name").unwrap(),
            vec![
                PathSegment::Key("items".to_string()),
                PathSegment::Index(0),
                PathSegment::Key("name".to_string())
            ]
        );
        assert_eq!(
            try_parse_path("data.items[5].value").unwrap(),
            vec![
                PathSegment::Key("data".to_string()),
                PathSegment::Key("items".to_string()),
                PathSegment::Index(5),
                PathSegment::Key("value".to_string())
            ]
        );
    }

    #[test]
    fn test_parse_path_with_numeric_indices() {
        assert_eq!(
            try_parse_path("items.0").unwrap(),
            vec![PathSegment::Key("items".to_string()), PathSegment::Index(0)]
        );
        assert_eq!(
            try_parse_path("items.0.name").unwrap(),
            vec![
                PathSegment::Key("items".to_string()),
                PathSegment::Index(0),
                PathSegment::Key("name".to_string())
            ]
        );
    }

    #[test]
    fn test_parse_path_with_escaping() {
        assert_eq!(
            try_parse_path("key\\.with\\.dots").unwrap(),
            vec![PathSegment::Key("key.with.dots".to_string())]
        );
        assert_eq!(
            try_parse_path("a.key\\.with\\.dots.b").unwrap(),
            vec![
                PathSegment::Key("a".to_string()),
                PathSegment::Key("key.with.dots".to_string()),
                PathSegment::Key("b".to_string())
            ]
        );
    }

    #[test]
    fn test_get_path_with_array_index() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let yaml = r#"
items:
  - name: first
    value: 1
  - name: second
    value: 2
"#;
        let doc = Document::from_str(yaml).unwrap();

        // Test bracket notation
        let name = doc.get_path("items[0].name");
        assert_eq!(
            name.as_ref()
                .and_then(|v| v.as_scalar())
                .map(|s| s.as_string()),
            Some("first".to_string())
        );

        let value = doc.get_path("items[1].value");
        assert_eq!(
            value
                .as_ref()
                .and_then(|v| v.as_scalar())
                .map(|s| s.as_string()),
            Some("2".to_string())
        );
    }

    #[test]
    fn test_get_path_with_numeric_index() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let yaml = r#"
items:
  - name: first
    value: 1
  - name: second
    value: 2
"#;
        let doc = Document::from_str(yaml).unwrap();

        // Test numeric dot notation
        let name = doc.get_path("items.0.name");
        assert_eq!(
            name.as_ref()
                .and_then(|v| v.as_scalar())
                .map(|s| s.as_string()),
            Some("first".to_string())
        );

        let value = doc.get_path("items.1.value");
        assert_eq!(
            value
                .as_ref()
                .and_then(|v| v.as_scalar())
                .map(|s| s.as_string()),
            Some("2".to_string())
        );
    }

    #[test]
    fn test_get_path_with_escaping() {
        use crate::yaml::Document;

        let doc = Document::new();
        doc.set("key.with.dots", "test value");

        // Without escaping - should not find it (looking for nested keys)
        assert!(doc.get_path("key.with.dots").is_none());

        // With escaping - should find it
        let value = doc.get_path("key\\.with\\.dots");
        assert_eq!(
            value
                .as_ref()
                .and_then(|v| v.as_scalar())
                .map(|s| s.as_string()),
            Some("test value".to_string())
        );
    }

    #[test]
    fn test_get_path_array_only() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let yaml = r#"
- first
- second
- third
"#;
        let doc = Document::from_str(yaml).unwrap();

        // Get from root sequence
        let item = doc.get_path("0");
        assert_eq!(
            item.as_ref()
                .and_then(|v| v.as_scalar())
                .map(|s| s.as_string()),
            Some("first".to_string())
        );

        let item = doc.get_path("2");
        assert_eq!(
            item.as_ref()
                .and_then(|v| v.as_scalar())
                .map(|s| s.as_string()),
            Some("third".to_string())
        );
    }

    #[test]
    fn test_remove_path_with_array_index() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let yaml = r#"
items:
  - name: first
    nested:
      key: value
"#;
        let doc = Document::from_str(yaml).unwrap();

        // Remove nested key inside array element
        assert!(doc.remove_path("items[0].nested.key"));
        assert!(doc.get_path("items[0].nested.key").is_none());

        // The nested mapping should still exist but be empty
        assert!(doc.get_path("items[0].nested").is_some());
    }

    #[test]
    fn test_mapping_get_path_with_indices() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let yaml = r#"
config:
  servers:
    - host: server1.com
      port: 8080
    - host: server2.com
      port: 9090
"#;
        let doc = Document::from_str(yaml).unwrap();
        let mapping = doc.as_mapping().unwrap();

        // Access through mapping using indices
        let host = mapping.get_path("config.servers[0].host");
        assert_eq!(
            host.as_ref()
                .and_then(|v| v.as_scalar())
                .map(|s| s.as_string()),
            Some("server1.com".to_string())
        );

        let port = mapping.get_path("config.servers.1.port");
        assert_eq!(
            port.as_ref()
                .and_then(|v| v.as_scalar())
                .map(|s| s.as_string()),
            Some("9090".to_string())
        );
    }

    #[test]
    fn test_get_path_simple() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let yaml = Document::from_str("name: Alice\nage: 30\n").unwrap();

        let name = yaml.get_path("name");
        assert_eq!(
            name.as_ref()
                .and_then(|v| v.as_scalar())
                .map(|s| s.to_string()),
            Some("Alice".to_string())
        );

        let age = yaml.get_path("age");
        assert_eq!(
            age.as_ref()
                .and_then(|v| v.as_scalar())
                .map(|s| s.to_string()),
            Some("30".to_string())
        );
    }

    #[test]
    fn test_get_path_nested() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let yaml = Document::from_str("server:\n  host: localhost\n  port: 8080\n").unwrap();

        let host = yaml.get_path("server.host");
        assert_eq!(
            host.as_ref()
                .and_then(|v| v.as_scalar())
                .map(|s| s.to_string()),
            Some("localhost".to_string())
        );

        let port = yaml.get_path("server.port");
        assert_eq!(
            port.as_ref()
                .and_then(|v| v.as_scalar())
                .map(|s| s.to_string()),
            Some("8080".to_string())
        );
    }

    #[test]
    fn test_get_path_deeply_nested() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let yaml = Document::from_str(
            "app:\n  database:\n    primary:\n      host: db.example.com\n      port: 5432\n",
        )
        .unwrap();

        let host = yaml.get_path("app.database.primary.host");
        assert_eq!(
            host.as_ref()
                .and_then(|v| v.as_scalar())
                .map(|s| s.to_string()),
            Some("db.example.com".to_string())
        );

        let port = yaml.get_path("app.database.primary.port");
        assert_eq!(
            port.as_ref()
                .and_then(|v| v.as_scalar())
                .map(|s| s.to_string()),
            Some("5432".to_string())
        );
    }

    #[test]
    fn test_get_path_missing() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let yaml = Document::from_str("name: Alice\n").unwrap();

        assert_eq!(yaml.get_path("missing"), None);
        assert_eq!(yaml.get_path("name.nested"), None);
        assert_eq!(yaml.get_path(""), None);
    }

    #[test]
    fn test_set_path_existing_key() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let yaml = Document::from_str("name: Alice\nage: 30\n").unwrap();

        yaml.set_path("name", "Bob");

        assert_eq!(yaml.to_string(), "name: Bob\nage: 30\n");
    }

    #[test]
    fn test_set_path_new_key() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let yaml = Document::from_str("name: Alice\n").unwrap();

        yaml.set_path("age", 30);

        assert_eq!(yaml.to_string(), "name: Alice\nage: 30\n");
    }

    #[test]
    fn test_set_path_nested_existing() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let yaml = Document::from_str("server:\n  host: localhost\n  port: 8080\n").unwrap();

        yaml.set_path("server.port", 9000);

        assert_eq!(
            yaml.to_string(),
            "server:\n  host: localhost\n  port: 9000\n"
        );
    }

    #[test]
    fn test_set_path_nested_new() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let yaml = Document::from_str("server:\n  host: localhost\n").unwrap();

        yaml.set_path("server.port", 8080);

        assert_eq!(
            yaml.to_string(),
            "server:\n  host: localhost\n  port: 8080\n"
        );
    }

    #[test]
    fn test_set_path_create_intermediate() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let yaml = Document::from_str("name: test\n").unwrap();

        yaml.set_path("server.database.host", "localhost");

        assert_eq!(
            yaml.to_string(),
            "name: test\nserver:\n  database:\n    host: localhost\n"
        );

        // Verify we can retrieve it
        let host = yaml.get_path("server.database.host");
        assert_eq!(
            host.as_ref()
                .and_then(|v| v.as_scalar())
                .map(|s| s.to_string()),
            Some("localhost".to_string())
        );
    }

    #[test]
    fn test_set_path_deeply_nested_create() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let yaml = Document::from_str("app: {}\n").unwrap();

        yaml.set_path("app.database.primary.host", "db.example.com");
        yaml.set_path("app.database.primary.port", 5432);

        // Parent was flow-style, so the whole nested chain stays flow.
        assert_eq!(
            yaml.to_string().trim(),
            r#"app: {database: {primary: {host: "db.example.com", port: 5432}}}"#
        );

        let host = yaml.get_path("app.database.primary.host");
        assert_eq!(
            host.as_ref()
                .and_then(|v| v.as_scalar())
                .map(|s| s.as_string()),
            Some("db.example.com".to_string())
        );

        let port = yaml.get_path("app.database.primary.port");
        assert_eq!(port.as_ref().and_then(|v| v.to_i64()), Some(5432));
    }

    #[test]
    fn test_remove_path_simple() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let yaml = Document::from_str("name: Alice\nage: 30\n").unwrap();

        let result = yaml.remove_path("age");
        assert!(result);

        assert_eq!(yaml.to_string(), "name: Alice\n");
    }

    #[test]
    fn test_remove_path_nested() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let yaml = Document::from_str("server:\n  host: localhost\n  port: 8080\n").unwrap();

        let result = yaml.remove_path("server.port");
        assert!(result);

        assert_eq!(yaml.to_string(), "server:\n  host: localhost\n  ");
    }

    #[test]
    fn test_remove_path_missing() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let yaml = Document::from_str("name: Alice\n").unwrap();

        let result = yaml.remove_path("missing");
        assert!(!result);

        let result = yaml.remove_path("name.nested");
        assert!(!result);

        // Document should be unchanged
        assert_eq!(yaml.to_string(), "name: Alice\n");
    }

    #[test]
    fn test_remove_path_deeply_nested() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let yaml = Document::from_str(
            "app:\n  database:\n    primary:\n      host: db.example.com\n      port: 5432\n",
        )
        .unwrap();

        let result = yaml.remove_path("app.database.primary.port");
        assert!(result);

        assert_eq!(
            yaml.to_string(),
            "app:\n  database:\n    primary:\n      host: db.example.com\n      "
        );
    }

    #[test]
    fn test_path_on_mapping_directly() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let yaml = Document::from_str("server:\n  host: localhost\n").unwrap();
        let mapping = yaml.as_mapping().unwrap();

        // Get from mapping
        let host = mapping.get_path("server.host");
        assert_eq!(
            host.as_ref()
                .and_then(|v| v.as_scalar())
                .map(|s| s.to_string()),
            Some("localhost".to_string())
        );

        // Set on mapping
        mapping.set_path("server.port", 8080);
        assert_eq!(
            yaml.to_string(),
            "server:\n  host: localhost\n  port: 8080\n"
        );

        // Remove from mapping
        let result = mapping.remove_path("server.port");
        assert!(result);

        // Try to remove non-existent path from mapping
        let result_missing = mapping.remove_path("nonexistent.path");
        assert!(!result_missing);
    }

    #[test]
    fn test_mapping_remove_path_single_segment() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let yaml = Document::from_str("a: 1\nb: 2\n").unwrap();
        let mapping = yaml.as_mapping().unwrap();

        // A single-segment path removes the key directly from the mapping.
        assert!(mapping.remove_path("a"));
        assert!(mapping.get_path("a").is_none());
        assert!(mapping.get_path("b").is_some());

        // Removing a missing single-segment key returns false.
        assert!(!mapping.remove_path("missing"));
    }

    #[test]
    fn test_set_path_preserves_formatting() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let yaml = Document::from_str("server:\n  host: localhost  # production server\n").unwrap();

        yaml.set_path("server.host", "newhost");

        assert_eq!(
            yaml.to_string(),
            "server:\n  host: newhost  # production server\n"
        );
    }

    #[test]
    fn test_multiple_path_operations() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let yaml = Document::from_str("name: test\n").unwrap();

        // Create nested structure
        yaml.set_path("server.host", "localhost");
        yaml.set_path("server.port", 8080);
        yaml.set_path("database.host", "db.local");
        yaml.set_path("database.port", 5432);

        // Verify all values
        assert_eq!(
            yaml.get_path("server.host")
                .as_ref()
                .and_then(|v| v.as_scalar())
                .map(|s| s.to_string()),
            Some("localhost".to_string())
        );
        assert_eq!(
            yaml.get_path("server.port")
                .as_ref()
                .and_then(|v| v.as_scalar())
                .map(|s| s.to_string()),
            Some("8080".to_string())
        );
        assert_eq!(
            yaml.get_path("database.host")
                .as_ref()
                .and_then(|v| v.as_scalar())
                .map(|s| s.to_string()),
            Some("db.local".to_string())
        );
        assert_eq!(
            yaml.get_path("database.port")
                .as_ref()
                .and_then(|v| v.as_scalar())
                .map(|s| s.to_string()),
            Some("5432".to_string())
        );

        // Remove some values
        yaml.remove_path("server.port");
        yaml.remove_path("database.host");

        // Verify removals
        assert_eq!(yaml.get_path("server.port"), None);
        assert_eq!(yaml.get_path("database.host"), None);

        // Verify remaining values still exist
        assert_eq!(
            yaml.get_path("server.host")
                .as_ref()
                .and_then(|v| v.as_scalar())
                .map(|s| s.to_string()),
            Some("localhost".to_string())
        );
        assert_eq!(
            yaml.get_path("database.port")
                .as_ref()
                .and_then(|v| v.as_scalar())
                .map(|s| s.to_string()),
            Some("5432".to_string())
        );
    }

    #[test]
    fn test_set_path_creates_intermediate_sequence() {
        // Nested collections created inline after `- ` render in
        // flow style (`- {c: "value"}`) so re-parse can find the
        // key. A block mapping inline after the dash would render
        // as a compact-block shape that re-parses ambiguously.
        use crate::yaml::Document;
        use std::str::FromStr;
        let doc = Document::from_str("base: true\n").unwrap();
        doc.set_path("a.b[0].c", "value");
        assert_eq!(
            doc.to_string(),
            "base: true\na:\n  b:\n    - {c: \"value\"}\n"
        );
    }

    #[test]
    fn test_set_path_into_existing_sequence_by_index() {
        use crate::yaml::Document;
        use std::str::FromStr;
        let doc = Document::from_str("items:\n  - a\n  - b\n").unwrap();
        doc.set_path("items[1]", "B");
        assert_eq!(doc.to_string(), "items:\n  - a\n  - B\n");
    }

    #[test]
    fn test_set_path_grows_sequence_with_nulls() {
        use crate::yaml::Document;
        use std::str::FromStr;
        let doc = Document::from_str("items:\n  - a\n").unwrap();
        doc.set_path("items[3]", "z");
        assert_eq!(
            doc.to_string(),
            "items:\n  - a\n  - null\n  - null\n  - z\n"
        );
    }
}
