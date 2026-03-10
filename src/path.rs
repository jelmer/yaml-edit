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
//! yaml.set_path("database.primary.host", "db.example.com").unwrap();
//!
//! // Remove nested values
//! yaml.remove_path("server.port");
//! ```
//!
//! All operations preserve formatting, comments, and whitespace.

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
    /// yaml.set_path("server.host", "localhost").unwrap();
    /// yaml.set_path("server.port", 8080).unwrap();
    /// ```
    fn set_path(
        &self,
        path: &str,
        value: impl crate::AsYaml,
    ) -> Result<(), crate::error::YamlError>;

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

/// Parse a path string into components.
///
/// Supports multiple syntaxes:
/// - Dot notation: `"server.host"` → `[Key("server"), Key("host")]`
/// - Array indices with brackets: `"items[0].name"` → `[Key("items"), Index(0), Key("name")]`
/// - Array indices with dots: `"items.0.name"` → `[Key("items"), Index(0), Key("name")]`
/// - Escaped dots: `"key\\.with\\.dots"` → `[Key("key.with.dots")]`
///
/// # Examples
///
/// ```
/// use yaml_edit::path::{parse_path, PathSegment};
///
/// let segments = parse_path("server.host");
/// assert_eq!(segments, vec![
///     PathSegment::Key("server".to_string()),
///     PathSegment::Key("host".to_string())
/// ]);
///
/// let segments = parse_path("items[0].name");
/// assert_eq!(segments, vec![
///     PathSegment::Key("items".to_string()),
///     PathSegment::Index(0),
///     PathSegment::Key("name".to_string())
/// ]);
///
/// let segments = parse_path("items.0");
/// assert_eq!(segments, vec![
///     PathSegment::Key("items".to_string()),
///     PathSegment::Index(0)
/// ]);
/// ```
pub fn parse_path(path: &str) -> Vec<PathSegment> {
    if path.is_empty() {
        return vec![];
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
                while let Some(&next_ch) = chars.peek() {
                    if next_ch == ']' {
                        chars.next(); // consume the ']'
                        break;
                    }
                    index_str.push(chars.next().unwrap());
                }

                // Parse the index
                if let Ok(index) = index_str.parse::<usize>() {
                    segments.push(PathSegment::Index(index));
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

    segments
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
        let segments = parse_path(path);
        if segments.is_empty() {
            return None;
        }

        // Start from the document's root content
        let root = if let Some(m) = self.as_mapping() {
            crate::as_yaml::YamlNode::Mapping(m)
        } else if let Some(s) = self.as_sequence() {
            crate::as_yaml::YamlNode::Sequence(s)
        } else if let Some(sc) = self.as_scalar() {
            crate::as_yaml::YamlNode::Scalar(sc)
        } else {
            return None;
        };

        // Navigate through the path segments
        navigate_path(root, &segments)
    }

    fn set_path(
        &self,
        path: &str,
        value: impl crate::AsYaml,
    ) -> Result<(), crate::error::YamlError> {
        let segments = parse_path(path);
        if segments.is_empty() {
            return Ok(());
        }

        // Dispatch based on root type
        if let Some(mapping) = self.as_mapping() {
            set_path_on_mapping(&mapping, &segments, value)
        } else if let Some(sequence) = self.as_sequence() {
            set_path_on_sequence(&sequence, &segments, value)
        } else {
            Ok(())
        }
    }

    fn remove_path(&self, path: &str) -> bool {
        let segments = parse_path(path);
        if segments.is_empty() {
            return false;
        }

        // Dispatch based on root type
        if let Some(mapping) = self.as_mapping() {
            remove_path_from_mapping(&mapping, &segments)
        } else if let Some(sequence) = self.as_sequence() {
            remove_path_from_sequence(&sequence, &segments)
        } else {
            false
        }
    }
}

// Implementation for Mapping
impl YamlPath for Mapping {
    fn get_path(&self, path: &str) -> Option<crate::as_yaml::YamlNode> {
        let segments = parse_path(path);
        if segments.is_empty() {
            return None;
        }

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

    fn set_path(
        &self,
        path: &str,
        value: impl crate::AsYaml,
    ) -> Result<(), crate::error::YamlError> {
        let segments = parse_path(path);
        if segments.is_empty() {
            return Ok(());
        }

        set_path_on_mapping(self, &segments, value)
    }

    fn remove_path(&self, path: &str) -> bool {
        let segments = parse_path(path);
        if segments.is_empty() {
            return false;
        }

        remove_path_from_mapping(self, &segments)
    }
}

// Implementation for Sequence
impl YamlPath for crate::Sequence {
    fn get_path(&self, path: &str) -> Option<crate::as_yaml::YamlNode> {
        let segments = parse_path(path);
        if segments.is_empty() {
            return None;
        }

        let index = match &segments[0] {
            PathSegment::Index(idx) => *idx,
            PathSegment::Key(_) => return None, // Can't key into a sequence directly
        };

        let current = self.get(index)?;
        if segments.len() == 1 {
            return Some(current);
        }

        navigate_path(current, &segments[1..])
    }

    fn set_path(
        &self,
        path: &str,
        value: impl crate::AsYaml,
    ) -> Result<(), crate::error::YamlError> {
        let segments = parse_path(path);
        if segments.is_empty() {
            return Ok(());
        }

        set_path_on_sequence(self, &segments, value)
    }

    fn remove_path(&self, path: &str) -> bool {
        let segments = parse_path(path);
        if segments.is_empty() {
            return false;
        }

        remove_path_from_sequence(self, &segments)
    }
}

/// Set a value at a path on a mapping, creating intermediate structures as needed.
///
/// This function uses only the public API (get_mapping, set) and does NOT rebuild nodes.
/// Uses YamlValue for intermediate structures to avoid cross-document node insertion bugs.
fn set_path_on_mapping<V: crate::AsYaml>(
    mapping: &Mapping,
    segments: &[PathSegment],
    value: V,
) -> Result<(), crate::error::YamlError> {
    if segments.is_empty() {
        return Ok(());
    }

    let first_key = match &segments[0] {
        PathSegment::Key(key) => key.as_str(),
        PathSegment::Index(_) => return Ok(()), // Can't set by index on a mapping
    };

    if segments.len() == 1 {
        // Base case: set directly
        mapping.set(first_key, value)?;
        return Ok(());
    }

    let next_segment = &segments[1];
    match next_segment {
        PathSegment::Key(_) => {
            // Next segment is a key, so we need a mapping
            if let Some(nested) = mapping.get_mapping(first_key) {
                set_path_on_mapping(&nested, &segments[1..], value)?;
            } else {
                // Create empty mapping using YamlValue (avoids cross-document bugs)
                mapping.set(first_key, Mapping::new())?;
                if let Some(nested) = mapping.get_mapping(first_key) {
                    set_path_on_mapping(&nested, &segments[1..], value)?;
                }
            }
        }
        PathSegment::Index(_) => {
            // Next segment is an index, so we need a sequence
            if let Some(nested) = mapping.get_sequence(first_key) {
                set_path_on_sequence(&nested, &segments[1..], value)?;
            } else {
                // Create empty sequence using YamlValue (avoids cross-document bugs)
                mapping.set(
                    first_key,
                    crate::value::YamlValue::Sequence(Default::default()),
                )?;
                if let Some(nested) = mapping.get_sequence(first_key) {
                    set_path_on_sequence(&nested, &segments[1..], value)?;
                }
            }
        }
    }
    Ok(())
}

/// Set a value at a path on a sequence, creating intermediate structures as needed.
fn set_path_on_sequence<V: crate::AsYaml>(
    seq: &crate::Sequence,
    segments: &[PathSegment],
    value: V,
) -> Result<(), crate::error::YamlError> {
    if segments.is_empty() {
        return Ok(());
    }

    let index = match &segments[0] {
        PathSegment::Index(idx) => *idx,
        PathSegment::Key(_) => return Ok(()), // Can't set by key on a sequence
    };

    if segments.len() == 1 {
        seq.set(index, value);
        return Ok(());
    }

    // Ensure the item exists at `index`, filling with nulls if needed
    if seq.get(index).is_none() {
        while seq.len() <= index {
            seq.push(crate::scalar::ScalarValue::null())?;
        }
    }

    match &segments[1] {
        PathSegment::Key(_) => {
            // Next segment is a key — need a mapping at this index
            let has_mapping = seq
                .get(index)
                .is_some_and(|item| item.as_mapping().is_some());
            if !has_mapping {
                seq.set(index, Mapping::new());
            }
            if let Some(item) = seq.get(index) {
                if let Some(nested) = item.as_mapping() {
                    set_path_on_mapping(nested, &segments[1..], value)?;
                }
            }
        }
        PathSegment::Index(_) => {
            // Next segment is an index — need a sequence at this index
            let has_sequence = seq
                .get(index)
                .is_some_and(|item| item.as_sequence().is_some());
            if !has_sequence {
                seq.set(index, crate::value::YamlValue::Sequence(Default::default()));
            }
            if let Some(item) = seq.get(index) {
                if let Some(nested) = item.as_sequence() {
                    set_path_on_sequence(nested, &segments[1..], value)?;
                }
            }
        }
    }
    Ok(())
}

/// Remove a value at a path from a mapping.
///
/// This function uses only the public API and does NOT rebuild nodes.
fn remove_path_from_mapping(mapping: &Mapping, segments: &[PathSegment]) -> bool {
    if segments.is_empty() {
        return false;
    }

    let first_key = match &segments[0] {
        PathSegment::Key(key) => key.as_str(),
        PathSegment::Index(_) => return false, // Can't index into a mapping
    };

    if segments.len() == 1 {
        return mapping.remove(first_key).is_some();
    }

    // Navigate into the value at this key and recurse
    match &segments[1] {
        PathSegment::Key(_) => {
            if let Some(nested) = mapping.get_mapping(first_key) {
                remove_path_from_mapping(&nested, &segments[1..])
            } else {
                false
            }
        }
        PathSegment::Index(_) => {
            if let Some(nested) = mapping.get_sequence(first_key) {
                remove_path_from_sequence(&nested, &segments[1..])
            } else {
                false
            }
        }
    }
}

/// Remove a value at a path from a sequence.
fn remove_path_from_sequence(seq: &crate::Sequence, segments: &[PathSegment]) -> bool {
    if segments.is_empty() {
        return false;
    }

    let index = match &segments[0] {
        PathSegment::Index(idx) => *idx,
        PathSegment::Key(_) => return false, // Can't key into a sequence
    };

    if segments.len() == 1 {
        return seq.remove(index).is_some();
    }

    // Navigate into the item at this index and recurse
    let item = match seq.get(index) {
        Some(item) => item,
        None => return false,
    };

    match &segments[1] {
        PathSegment::Key(_) => {
            if let Some(nested) = item.as_mapping() {
                remove_path_from_mapping(nested, &segments[1..])
            } else {
                false
            }
        }
        PathSegment::Index(_) => {
            if let Some(nested) = item.as_sequence() {
                remove_path_from_sequence(nested, &segments[1..])
            } else {
                false
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_path_basic() {
        assert_eq!(parse_path(""), Vec::<PathSegment>::new());
        assert_eq!(parse_path("key"), vec![PathSegment::Key("key".to_string())]);
        assert_eq!(
            parse_path("a.b"),
            vec![
                PathSegment::Key("a".to_string()),
                PathSegment::Key("b".to_string())
            ]
        );
        assert_eq!(
            parse_path("a.b.c.d"),
            vec![
                PathSegment::Key("a".to_string()),
                PathSegment::Key("b".to_string()),
                PathSegment::Key("c".to_string()),
                PathSegment::Key("d".to_string())
            ]
        );
    }

    #[test]
    fn test_parse_path_with_array_indices() {
        assert_eq!(
            parse_path("items[0]"),
            vec![PathSegment::Key("items".to_string()), PathSegment::Index(0)]
        );
        assert_eq!(
            parse_path("items[0].name"),
            vec![
                PathSegment::Key("items".to_string()),
                PathSegment::Index(0),
                PathSegment::Key("name".to_string())
            ]
        );
        assert_eq!(
            parse_path("data.items[5].value"),
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
            parse_path("items.0"),
            vec![PathSegment::Key("items".to_string()), PathSegment::Index(0)]
        );
        assert_eq!(
            parse_path("items.0.name"),
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
            parse_path("key\\.with\\.dots"),
            vec![PathSegment::Key("key.with.dots".to_string())]
        );
        assert_eq!(
            parse_path("a.key\\.with\\.dots.b"),
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
        doc.set("key.with.dots", "test value").unwrap();

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

        yaml.set_path("name", "Bob").unwrap();

        assert_eq!(yaml.to_string(), "name: Bob\nage: 30\n");
    }

    #[test]
    fn test_set_path_new_key() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let yaml = Document::from_str("name: Alice\n").unwrap();

        yaml.set_path("age", 30).unwrap();

        assert_eq!(yaml.to_string(), "name: Alice\nage: 30\n");
    }

    #[test]
    fn test_set_path_nested_existing() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let yaml = Document::from_str("server:\n  host: localhost\n  port: 8080\n").unwrap();

        yaml.set_path("server.port", 9000).unwrap();

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

        yaml.set_path("server.port", 8080).unwrap();

        assert_eq!(yaml.to_string(), "server:\n  host: localhost\nport: 8080\n");
    }

    #[test]
    fn test_set_path_create_intermediate() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let yaml = Document::from_str("name: test\n").unwrap();

        yaml.set_path("server.database.host", "localhost").unwrap();

        assert_eq!(
            yaml.to_string(),
            "name: test\nserver:\ndatabase:\nhost: localhost\n\n\n"
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

        let yaml = Document::from_str("app:\n  name: test\n").unwrap();

        yaml.set_path("app.database.primary.host", "db.example.com")
            .unwrap();
        yaml.set_path("app.database.primary.port", 5432).unwrap();

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
    fn test_remove_path_simple() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let yaml = Document::from_str("name: Alice\nage: 30\n").unwrap();

        let result = yaml.remove_path("age");
        assert!(result);

        assert_eq!(yaml.to_string(), "name: Alice");
    }

    #[test]
    fn test_remove_path_nested() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let yaml = Document::from_str("server:\n  host: localhost\n  port: 8080\n").unwrap();

        let result = yaml.remove_path("server.port");
        assert!(result);

        assert_eq!(yaml.to_string(), "server:\n  host: localhost  ");
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
            "app:\n  database:\n    primary:\n      host: db.example.com      "
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
        mapping.set_path("server.port", 8080).unwrap();
        assert_eq!(yaml.to_string(), "server:\n  host: localhost\nport: 8080\n");

        // Remove from mapping
        let result = mapping.remove_path("server.port");
        assert!(result);

        // Try to remove non-existent path from mapping
        let result_missing = mapping.remove_path("nonexistent.path");
        assert!(!result_missing);
    }

    #[test]
    fn test_set_path_preserves_formatting() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let yaml = Document::from_str("server:\n  host: localhost  # production server\n").unwrap();

        yaml.set_path("server.host", "newhost").unwrap();

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
        yaml.set_path("server.host", "localhost").unwrap();
        yaml.set_path("server.port", 8080).unwrap();
        yaml.set_path("database.host", "db.local").unwrap();
        yaml.set_path("database.port", 5432).unwrap();

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
    fn test_set_path_creates_intermediate_nodes() {
        use std::str::FromStr;
        let doc = crate::Document::from_str("base: true\n").unwrap();

        // Setting a deeply nested path should create intermediate structures
        doc.set_path("a.b[0].c", "value").unwrap();

        // Verify the value is accessible via get_path
        assert_eq!(
            doc.get_path("a.b[0].c")
                .as_ref()
                .and_then(|v| v.as_scalar())
                .map(|s| s.to_string()),
            Some("value".to_string())
        );
    }

    #[test]
    fn test_set_path_into_sequence() {
        use std::str::FromStr;
        let doc = crate::Document::from_str("items:\n  - name: first\n").unwrap();

        // Setting a nested value in an existing sequence item
        doc.set_path("items[0].name", "updated").unwrap();

        assert_eq!(doc.to_string(), "items:\n  - name: updated\n");
    }

    #[test]
    fn test_remove_path_sequence_index() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let yaml = Document::from_str("items:\n  - first\n  - second\n  - third\n").unwrap();

        assert!(yaml.remove_path("items[1]"));

        let binding = yaml.get_path("items").unwrap();
        let seq = binding.as_sequence().unwrap();
        let values: Vec<String> = seq.values().map(|v| v.to_string()).collect();
        assert_eq!(values, vec!["first", "third"]);
    }

    #[test]
    fn test_remove_path_nested_in_sequence() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let yaml = Document::from_str("items:\n  - name: first\n    value: 1\n  - name: second\n")
            .unwrap();

        assert!(yaml.remove_path("items[0].value"));
        assert_eq!(
            yaml.get_path("items[0].name")
                .as_ref()
                .and_then(|v| v.as_scalar())
                .map(|s| s.to_string()),
            Some("first".to_string())
        );
        assert_eq!(yaml.get_path("items[0].value"), None);
    }

    #[test]
    fn test_remove_path_sequence_index_out_of_bounds() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let yaml = Document::from_str("items:\n  - first\n").unwrap();
        assert!(!yaml.remove_path("items[5]"));
    }

    #[test]
    fn test_mapping_remove_path_through_sequence() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let yaml = Document::from_str("items:\n  - name: first\n    value: 1\n").unwrap();
        let mapping = yaml.as_mapping().unwrap();

        assert!(mapping.remove_path("items[0].value"));
        assert_eq!(yaml.get_path("items[0].value"), None);
        assert!(yaml.get_path("items[0].name").is_some());
    }

    #[test]
    fn test_sequence_yaml_path() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let yaml = Document::from_str("- name: first\n  value: 1\n- name: second\n").unwrap();
        let seq = yaml.as_sequence().unwrap();

        // get_path
        assert_eq!(
            seq.get_path("[0].name")
                .as_ref()
                .and_then(|v| v.as_scalar())
                .map(|s| s.to_string()),
            Some("first".to_string())
        );

        // set_path
        seq.set_path("[0].value", 42).unwrap();
        assert_eq!(
            seq.get_path("[0].value")
                .as_ref()
                .and_then(|v| v.as_scalar())
                .map(|s| s.to_string()),
            Some("42".to_string())
        );

        // remove_path
        assert!(seq.remove_path("[1]"));
        assert_eq!(seq.get_path("[1]"), None);
    }

    #[test]
    fn test_document_set_path_sequence_root() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let yaml = Document::from_str("- name: first\n").unwrap();

        yaml.set_path("[0].name", "updated").unwrap();
        assert_eq!(yaml.to_string(), "- name: updated\n");
    }

    #[test]
    fn test_document_remove_path_sequence_root() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let yaml = Document::from_str("- first\n- second\n- third\n").unwrap();

        assert!(yaml.remove_path("[1]"));

        let seq = yaml.as_sequence().unwrap();
        let values: Vec<String> = seq.values().map(|v| v.to_string()).collect();
        assert_eq!(values, vec!["first", "third"]);
    }
}
