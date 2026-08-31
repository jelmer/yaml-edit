//! Path-based access to YAML documents.
//!
//! Provides convenient dot-separated path syntax for accessing nested YAML values
//! like `"server.host"` or `"database.primary.port"`.
//!
//! Operations:
//! [`try_get_path`](YamlPath::try_get_path),
//! [`try_set_path`](YamlPath::try_set_path),
//! [`try_remove_path`](YamlPath::try_remove_path).
//! The `try_` variants return [`Result<_, PathError>`](PathError) so
//! malformed paths, empty paths, missing keys, and container-type
//! mismatches are visible rather than silently swallowed.
//!
//! The legacy `get_path` / `set_path` / `remove_path` methods still
//! exist as thin wrappers that discard `PathError`; they are deprecated
//! and will emit warnings.
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
//! let host = yaml.try_get_path("server.host").ok();
//!
//! // Set nested values (creates intermediate mappings)
//! yaml.try_set_path("database.primary.host", "db.example.com").unwrap();
//!
//! // Remove nested values
//! yaml.try_remove_path("server.port").unwrap();
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
///
/// The `try_` methods ([`try_get_path`](Self::try_get_path),
/// [`try_set_path`](Self::try_set_path),
/// [`try_remove_path`](Self::try_remove_path)) return
/// `Result<_, PathError>` and are the recommended API. The older
/// [`get_path`](Self::get_path) / [`set_path`](Self::set_path) /
/// [`remove_path`](Self::remove_path) methods are deprecated wrappers
/// that discard the error and silently no-op.
pub trait YamlPath {
    /// Get a value at a nested path, returning a specific [`PathError`]
    /// on failure instead of `None`.
    ///
    /// # Errors
    ///
    /// - [`PathError::Parse`] for a malformed path.
    /// - [`PathError::EmptyPath`] when the path parses to zero segments.
    /// - [`PathError::NoRoot`] when the receiver has no value to descend into.
    /// - [`PathError::TypeMismatch`] when a segment tries to descend into a
    ///   value of the wrong container type.
    /// - [`PathError::NotFound`] when a segment addresses a key/index that
    ///   does not exist.
    fn try_get_path(&self, path: &str) -> Result<crate::as_yaml::YamlNode, PathError>;

    /// Set a value at a nested path, creating intermediate mappings /
    /// sequences as needed. Returns a specific [`PathError`] on failure
    /// instead of silently no-oping.
    ///
    /// # Errors
    ///
    /// - [`PathError::Parse`] for a malformed path.
    /// - [`PathError::EmptyPath`] when the path parses to zero segments.
    /// - [`PathError::NoRoot`] when the receiver has no root mapping to
    ///   write into (Document with no root, or with a scalar/sequence root).
    /// - [`PathError::TypeMismatch`] when an intermediate segment lands on
    ///   a scalar that cannot be turned into a container.
    fn try_set_path(&self, path: &str, value: impl crate::AsYaml) -> Result<(), PathError>;

    /// Remove a value at a nested path. Returns the removed
    /// [`YamlNode`](crate::as_yaml::YamlNode) on success, or a specific
    /// [`PathError`] describing why the removal did not happen.
    ///
    /// # Errors
    ///
    /// Same shape as [`try_get_path`](Self::try_get_path).
    fn try_remove_path(&self, path: &str) -> Result<crate::as_yaml::YamlNode, PathError>;

    /// Get a value at a nested path.
    ///
    /// This is a lossy wrapper around
    /// [`try_get_path`](Self::try_get_path): every error becomes `None`,
    /// so callers cannot distinguish "path parsed but nothing found"
    /// from "path was malformed" or "descended through the wrong
    /// container type."
    ///
    /// # Examples
    ///
    /// ```
    /// use yaml_edit::{Document, path::YamlPath};
    /// use std::str::FromStr;
    ///
    /// let yaml = Document::from_str("server:\n  host: localhost\n").unwrap();
    /// #[allow(deprecated)]
    /// let host = yaml.get_path("server.host");
    /// assert!(host.is_some());
    /// ```
    #[deprecated(
        since = "0.4.0",
        note = "use try_get_path; get_path swallows PathError as None"
    )]
    fn get_path(&self, path: &str) -> Option<crate::as_yaml::YamlNode> {
        self.try_get_path(path).ok()
    }

    /// Set a value at a nested path.
    ///
    /// Lossy wrapper around [`try_set_path`](Self::try_set_path): every
    /// error is silently ignored. Callers get no signal that the write
    /// failed (bad path, no root, descending through a scalar, ...).
    ///
    /// # Examples
    ///
    /// ```
    /// use yaml_edit::{Document, path::YamlPath};
    /// use std::str::FromStr;
    ///
    /// let yaml = Document::from_str("name: test\n").unwrap();
    /// #[allow(deprecated)]
    /// yaml.set_path("server.host", "localhost");
    /// #[allow(deprecated)]
    /// yaml.set_path("server.port", 8080);
    /// ```
    #[deprecated(
        since = "0.4.0",
        note = "use try_set_path; set_path silently ignores PathError"
    )]
    fn set_path(&self, path: &str, value: impl crate::AsYaml) {
        let _ = self.try_set_path(path, value);
    }

    /// Remove a value at a nested path. Returns `true` if a value was
    /// removed.
    ///
    /// Lossy wrapper around [`try_remove_path`](Self::try_remove_path):
    /// every error is reported as `false`, indistinguishable from
    /// "path was well-formed but key not present."
    ///
    /// # Examples
    ///
    /// ```
    /// use yaml_edit::{Document, path::YamlPath};
    /// use std::str::FromStr;
    ///
    /// let yaml = Document::from_str("server:\n  host: localhost\n  port: 8080\n").unwrap();
    /// #[allow(deprecated)]
    /// {
    ///     assert_eq!(yaml.remove_path("server.port"), true);
    ///     assert_eq!(yaml.remove_path("server.missing"), false);
    /// }
    /// ```
    #[deprecated(
        since = "0.4.0",
        note = "use try_remove_path; remove_path swallows PathError as false"
    )]
    fn remove_path(&self, path: &str) -> bool {
        self.try_remove_path(path).is_ok()
    }
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

/// Error returned by [`try_get_path`](YamlPath::try_get_path) /
/// [`try_set_path`](YamlPath::try_set_path) /
/// [`try_remove_path`](YamlPath::try_remove_path).
///
/// Distinguishes the several ways a path operation can fail so callers
/// can react appropriately, instead of getting the previous silent
/// no-op or ambiguous `None`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PathError {
    /// The path string is malformed (unclosed `[`, non-`usize` index).
    Parse(PathParseError),
    /// The path parsed to zero segments (typically the empty string or
    /// a run of stray dots).
    EmptyPath,
    /// The receiver has no root value to descend into: `Document` was
    /// empty, or the requested operation needed a root mapping and the
    /// document's root is a scalar / sequence.
    NoRoot,
    /// A segment tried to descend into a value of the wrong container
    /// type -- e.g. treating a scalar as a mapping in `foo.bar` when
    /// `foo` is a scalar. `at` is the segment index (`"foo"` in the
    /// example above) that led to the type mismatch.
    TypeMismatch {
        /// The segment whose value was not the expected container type.
        at: String,
    },
    /// The target segment does not exist. Applies to
    /// [`try_get_path`](YamlPath::try_get_path) and
    /// [`try_remove_path`](YamlPath::try_remove_path); `try_set_path`
    /// creates missing intermediates and never reports this. `at` is
    /// the missing segment.
    NotFound {
        /// The segment that could not be resolved.
        at: String,
    },
}

impl std::fmt::Display for PathError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PathError::Parse(e) => write!(f, "path parse error: {e}"),
            PathError::EmptyPath => write!(f, "path is empty (no segments)"),
            PathError::NoRoot => write!(f, "document has no root to descend into"),
            PathError::TypeMismatch { at } => {
                write!(f, "path segment {at:?} is not a container")
            }
            PathError::NotFound { at } => write!(f, "path segment {at:?} not found"),
        }
    }
}

impl std::error::Error for PathError {}

impl From<PathParseError> for PathError {
    fn from(e: PathParseError) -> Self {
        PathError::Parse(e)
    }
}

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

/// Parse `path` into a non-empty segment list, mapping the two
/// failure modes ("unparseable" and "empty") to distinct `PathError`
/// variants.
fn path_segments_required(path: &str) -> Result<Vec<PathSegment>, PathError> {
    let segments = try_parse_path(path)?;
    if segments.is_empty() {
        Err(PathError::EmptyPath)
    } else {
        Ok(segments)
    }
}

/// Format a segment for use in a `PathError::TypeMismatch` / `NotFound`
/// message. Mirrors the input syntax the user would type.
fn segment_display(segment: &PathSegment) -> String {
    match segment {
        PathSegment::Key(key) => key.clone(),
        PathSegment::Index(index) => format!("[{index}]"),
    }
}

/// Navigate through a YAML structure following path segments.
///
/// Handles both mapping keys and sequence indices. A numeric segment
/// like `997` in `foo.997` parses as `Index(997)` even though the user
/// may have meant it as a mapping key: when the current node is a
/// mapping, fall back to looking up the stringified form. Errors
/// distinguish "wrong container type" from "key/index not present".
fn navigate_path(
    mut current: crate::as_yaml::YamlNode,
    segments: &[PathSegment],
) -> Result<crate::as_yaml::YamlNode, PathError> {
    for segment in segments {
        current = descend_one(current, segment)?;
    }
    Ok(current)
}

fn descend_one(
    current: crate::as_yaml::YamlNode,
    segment: &PathSegment,
) -> Result<crate::as_yaml::YamlNode, PathError> {
    match segment {
        PathSegment::Key(key) => {
            let mapping = current
                .as_mapping()
                .ok_or_else(|| PathError::TypeMismatch {
                    at: segment_display(segment),
                })?;
            mapping.get(key).ok_or_else(|| PathError::NotFound {
                at: segment_display(segment),
            })
        }
        PathSegment::Index(index) => {
            if let Some(seq) = current.as_sequence() {
                seq.get(*index).ok_or_else(|| PathError::NotFound {
                    at: segment_display(segment),
                })
            } else if let Some(map) = current.as_mapping() {
                // Fallback: numeric segment used as a mapping key.
                map.get(index.to_string().as_str())
                    .ok_or_else(|| PathError::NotFound {
                        at: segment_display(segment),
                    })
            } else {
                Err(PathError::TypeMismatch {
                    at: segment_display(segment),
                })
            }
        }
    }
}

/// Interpret a segment as a mapping key. `Index(n)` is stringified so
/// paths like `foo.997` (parsed as `foo` + `Index(997)`) still address
/// a mapping key `"997"`.
fn segment_key(segment: &PathSegment) -> String {
    match segment {
        PathSegment::Key(key) => key.clone(),
        PathSegment::Index(index) => index.to_string(),
    }
}

// Implementation for Document
impl YamlPath for crate::yaml::Document {
    fn try_get_path(&self, path: &str) -> Result<crate::as_yaml::YamlNode, PathError> {
        let segments = path_segments_required(path)?;

        // Start from the document's root content
        let root = if let Some(m) = self.as_mapping() {
            crate::as_yaml::YamlNode::Mapping(m)
        } else if let Some(s) = self.as_sequence() {
            crate::as_yaml::YamlNode::Sequence(s)
        } else if let Some(sc) = self.as_scalar() {
            crate::as_yaml::YamlNode::Scalar(sc)
        } else {
            return Err(PathError::NoRoot);
        };

        navigate_path(root, &segments)
    }

    fn try_set_path(&self, path: &str, value: impl crate::AsYaml) -> Result<(), PathError> {
        let segments = path_segments_required(path)?;

        // Only a root mapping can hold new-key insertions; a scalar or
        // sequence root has no place to graft `foo.bar` under.
        let mapping = self.as_mapping().ok_or(PathError::NoRoot)?;

        set_path_on_mapping(&mapping, &segments, value)
    }

    fn try_remove_path(&self, path: &str) -> Result<crate::as_yaml::YamlNode, PathError> {
        let segments = path_segments_required(path)?;

        let root = if let Some(m) = self.as_mapping() {
            crate::as_yaml::YamlNode::Mapping(m)
        } else if let Some(s) = self.as_sequence() {
            crate::as_yaml::YamlNode::Sequence(s)
        } else {
            return Err(PathError::NoRoot);
        };

        remove_path_impl(root, &segments)
    }
}

/// Remove a value at a nested path. Returns the removed
/// [`YamlNode`](crate::as_yaml::YamlNode) on success. Errors carry the
/// specific reason (type mismatch vs. not-found).
///
/// Removing by index from a sequence is intentionally unsupported (it
/// would shift every subsequent element) and reported as a
/// `PathError::TypeMismatch` at that segment.
fn remove_path_impl(
    root: crate::as_yaml::YamlNode,
    segments: &[PathSegment],
) -> Result<crate::as_yaml::YamlNode, PathError> {
    debug_assert!(!segments.is_empty(), "caller must reject empty paths");

    if segments.len() == 1 {
        let seg = &segments[0];
        let key = match seg {
            PathSegment::Key(key) => key.clone(),
            PathSegment::Index(index) => {
                // Numeric segment on a mapping falls back to the
                // stringified key. On a sequence, index removal is
                // unsupported.
                if root.as_mapping().is_none() {
                    return Err(PathError::TypeMismatch {
                        at: segment_display(seg),
                    });
                }
                index.to_string()
            }
        };
        let mapping = root.as_mapping().ok_or_else(|| PathError::TypeMismatch {
            at: segment_display(seg),
        })?;
        // Grab the value before removal so we can return it. If the
        // entry has no VALUE child (unusual: implicit-null entry), fall
        // back to a null scalar.
        let value = mapping
            .get(key.as_str())
            .ok_or_else(|| PathError::NotFound {
                at: segment_display(seg),
            })?;
        mapping.remove(key.as_str());
        return Ok(value);
    }

    // Descend one level and recurse.
    let nested = descend_one(root, &segments[0])?;
    remove_path_impl(nested, &segments[1..])
}

// Implementation for Mapping
impl YamlPath for Mapping {
    fn try_get_path(&self, path: &str) -> Result<crate::as_yaml::YamlNode, PathError> {
        let segments = path_segments_required(path)?;
        navigate_path(crate::as_yaml::YamlNode::Mapping(self.clone()), &segments)
    }

    fn try_set_path(&self, path: &str, value: impl crate::AsYaml) -> Result<(), PathError> {
        let segments = path_segments_required(path)?;
        set_path_on_mapping(self, &segments, value)
    }

    fn try_remove_path(&self, path: &str) -> Result<crate::as_yaml::YamlNode, PathError> {
        let segments = path_segments_required(path)?;
        remove_path_impl(crate::as_yaml::YamlNode::Mapping(self.clone()), &segments)
    }
}

/// Set a value at a path on a mapping, creating intermediate mappings or
/// sequences as needed.
///
/// Uses only the public API (get_mapping, get_sequence, set) and does NOT
/// rebuild nodes. Recurses through `set_path_on_sequence` when a segment
/// dives into a sequence.
///
/// Errors when an intermediate segment lands on an existing scalar (we
/// won't overwrite user data implicitly).
fn set_path_on_mapping<V: crate::AsYaml>(
    mapping: &Mapping,
    segments: &[PathSegment],
    value: V,
) -> Result<(), PathError> {
    debug_assert!(!segments.is_empty(), "caller must reject empty paths");

    // First segment: numeric segments (from `foo.997`) are stringified
    // so they still address a mapping key.
    let first_key_owned = segment_key(&segments[0]);
    let first_key = first_key_owned.as_str();

    if segments.len() == 1 {
        // Base case: set directly
        mapping.set(first_key, value);
        return Ok(());
    }

    // What container does the next segment expect at `first_key`?
    let next_wants_sequence = matches!(segments[1], PathSegment::Index(_));

    // Reject descending through a scalar. `get(first_key)` returns the
    // existing value if any; if it's a non-null scalar we'd have to
    // overwrite user data to continue. Null placeholders are treated
    // as "vacant" and get replaced by the appropriate container.
    if let Some(existing) = mapping.get(first_key) {
        if let Some(sc) = existing.as_scalar() {
            let s = sc.as_string();
            let is_null_placeholder = s.is_empty() || s.eq_ignore_ascii_case("null") || s == "~";
            if !is_null_placeholder {
                return Err(PathError::TypeMismatch {
                    at: segment_display(&segments[0]),
                });
            }
        }
    }

    if next_wants_sequence {
        if let Some(nested) = mapping.get_sequence(first_key) {
            return set_path_on_sequence(&nested, &segments[1..], value);
        }
        // Index on an existing mapping is a key (`m.0` / `m[0]`), same
        // as get_path. Do not replace the mapping with a sequence.
        if let Some(nested) = mapping.get_mapping(first_key) {
            return set_path_on_mapping(&nested, &segments[1..], value);
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
        let nested = mapping
            .get_sequence(first_key)
            .expect("we just inserted this key as a sequence");
        return set_path_on_sequence(&nested, &segments[1..], value);
    }

    if let Some(nested) = mapping.get_mapping(first_key) {
        return set_path_on_mapping(&nested, &segments[1..], value);
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

    let nested = mapping
        .get_mapping(first_key)
        .expect("we just inserted this key as a mapping");
    set_path_on_mapping(&nested, &segments[1..], value)
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
) -> Result<(), PathError> {
    debug_assert!(!segments.is_empty(), "caller must reject empty paths");

    let index = match &segments[0] {
        PathSegment::Index(i) => *i,
        // A string segment on a sequence is a real type mismatch: we
        // have no reasonable coercion to try (unlike numeric-on-mapping,
        // which stringifies).
        PathSegment::Key(_) => {
            return Err(PathError::TypeMismatch {
                at: segment_display(&segments[0]),
            });
        }
    };

    // Grow the sequence with null placeholders until `index` is in range.
    while sequence.len() <= index {
        sequence.push(crate::scalar::ScalarValue::null());
    }

    if segments.len() == 1 {
        sequence.set(index, value);
        return Ok(());
    }

    // Descending through an existing scalar is fine when that scalar is
    // a null placeholder (either one we just pushed to grow the
    // sequence, or an existing `null` the user chose). Reject only when
    // we'd have to overwrite a non-null user value.
    if let Some(existing) = sequence.get(index) {
        if let Some(sc) = existing.as_scalar() {
            let s = sc.as_string();
            let is_null_placeholder = s.is_empty() || s.eq_ignore_ascii_case("null") || s == "~";
            if !is_null_placeholder {
                return Err(PathError::TypeMismatch {
                    at: segment_display(&segments[0]),
                });
            }
        }
    }

    let next_wants_sequence = matches!(segments[1], PathSegment::Index(_));

    if next_wants_sequence {
        if let Some(nested) = sequence.get(index).and_then(|n| n.as_sequence().cloned()) {
            return set_path_on_sequence(&nested, &segments[1..], value);
        }
        if let Some(nested) = sequence.get(index).and_then(|n| n.as_mapping().cloned()) {
            return set_path_on_mapping(&nested, &segments[1..], value);
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
        let nested = sequence
            .get(index)
            .and_then(|n| n.as_sequence().cloned())
            .expect("we just inserted a sequence at this index");
        return set_path_on_sequence(&nested, &segments[1..], value);
    }

    if let Some(nested) = sequence.get(index).and_then(|n| n.as_mapping().cloned()) {
        return set_path_on_mapping(&nested, &segments[1..], value);
    }
    let flow_empty = crate::builder::MappingBuilder::new()
        .build_document()
        .as_mapping()
        .expect("MappingBuilder always produces a mapping");
    sequence.set(index, flow_empty);
    let nested = sequence
        .get(index)
        .and_then(|n| n.as_mapping().cloned())
        .expect("we just inserted a mapping at this index");
    set_path_on_mapping(&nested, &segments[1..], value)
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
        let name = doc.try_get_path("items[0].name").ok();
        assert_eq!(
            name.as_ref()
                .and_then(|v| v.as_scalar())
                .map(|s| s.as_string()),
            Some("first".to_string())
        );

        let value = doc.try_get_path("items[1].value").ok();
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
        let name = doc.try_get_path("items.0.name").ok();
        assert_eq!(
            name.as_ref()
                .and_then(|v| v.as_scalar())
                .map(|s| s.as_string()),
            Some("first".to_string())
        );

        let value = doc.try_get_path("items.1.value").ok();
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
        assert!(doc.try_get_path("key.with.dots").is_err());

        // With escaping - should find it
        let value = doc.try_get_path("key\\.with\\.dots").ok();
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
        let item = doc.try_get_path("0").ok();
        assert_eq!(
            item.as_ref()
                .and_then(|v| v.as_scalar())
                .map(|s| s.as_string()),
            Some("first".to_string())
        );

        let item = doc.try_get_path("2").ok();
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
        assert!(doc.try_remove_path("items[0].nested.key").is_ok());
        assert!(doc.try_get_path("items[0].nested.key").is_err());

        // The nested mapping should still exist but be empty
        assert!(doc.try_get_path("items[0].nested").is_ok());
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
        let host = mapping.try_get_path("config.servers[0].host").ok();
        assert_eq!(
            host.as_ref()
                .and_then(|v| v.as_scalar())
                .map(|s| s.as_string()),
            Some("server1.com".to_string())
        );

        let port = mapping.try_get_path("config.servers.1.port").ok();
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

        let name = yaml.try_get_path("name").ok();
        assert_eq!(
            name.as_ref()
                .and_then(|v| v.as_scalar())
                .map(|s| s.to_string()),
            Some("Alice".to_string())
        );

        let age = yaml.try_get_path("age").ok();
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

        let host = yaml.try_get_path("server.host").ok();
        assert_eq!(
            host.as_ref()
                .and_then(|v| v.as_scalar())
                .map(|s| s.to_string()),
            Some("localhost".to_string())
        );

        let port = yaml.try_get_path("server.port").ok();
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

        let host = yaml.try_get_path("app.database.primary.host").ok();
        assert_eq!(
            host.as_ref()
                .and_then(|v| v.as_scalar())
                .map(|s| s.to_string()),
            Some("db.example.com".to_string())
        );

        let port = yaml.try_get_path("app.database.primary.port").ok();
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

        assert!(matches!(
            yaml.try_get_path("missing"),
            Err(PathError::NotFound { .. })
        ));
        // `name` is a scalar, `.nested` tries to descend into it.
        assert!(matches!(
            yaml.try_get_path("name.nested"),
            Err(PathError::TypeMismatch { .. })
        ));
        assert_eq!(yaml.try_get_path(""), Err(PathError::EmptyPath));
    }

    #[test]
    fn test_set_path_existing_key() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let yaml = Document::from_str("name: Alice\nage: 30\n").unwrap();

        yaml.try_set_path("name", "Bob").expect("set_path");

        assert_eq!(yaml.to_string(), "name: Bob\nage: 30\n");
    }

    #[test]
    fn test_set_path_new_key() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let yaml = Document::from_str("name: Alice\n").unwrap();

        yaml.try_set_path("age", 30).expect("set_path");

        assert_eq!(yaml.to_string(), "name: Alice\nage: 30\n");
    }

    #[test]
    fn test_set_path_nested_existing() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let yaml = Document::from_str("server:\n  host: localhost\n  port: 8080\n").unwrap();

        yaml.try_set_path("server.port", 9000).expect("set_path");

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

        yaml.try_set_path("server.port", 8080).expect("set_path");

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

        yaml.try_set_path("server.database.host", "localhost")
            .expect("set_path");

        assert_eq!(
            yaml.to_string(),
            "name: test\nserver:\n  database:\n    host: localhost\n"
        );

        // Verify we can retrieve it
        let host = yaml.try_get_path("server.database.host").ok();
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

        yaml.try_set_path("app.database.primary.host", "db.example.com")
            .expect("set_path");
        yaml.try_set_path("app.database.primary.port", 5432)
            .expect("set_path");

        // Parent was flow-style, so the whole nested chain stays flow.
        assert_eq!(
            yaml.to_string().trim(),
            r#"app: {database: {primary: {host: "db.example.com", port: 5432}}}"#
        );

        let host = yaml.try_get_path("app.database.primary.host").ok();
        assert_eq!(
            host.as_ref()
                .and_then(|v| v.as_scalar())
                .map(|s| s.as_string()),
            Some("db.example.com".to_string())
        );

        let port = yaml.try_get_path("app.database.primary.port").ok();
        assert_eq!(port.as_ref().and_then(|v| v.to_i64()), Some(5432));
    }

    #[test]
    fn test_remove_path_simple() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let yaml = Document::from_str("name: Alice\nage: 30\n").unwrap();

        let result = yaml.try_remove_path("age").is_ok();
        assert!(result);

        assert_eq!(yaml.to_string(), "name: Alice\n");
    }

    #[test]
    fn test_remove_path_nested() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let yaml = Document::from_str("server:\n  host: localhost\n  port: 8080\n").unwrap();

        let result = yaml.try_remove_path("server.port").is_ok();
        assert!(result);

        assert_eq!(yaml.to_string(), "server:\n  host: localhost\n  ");
    }

    #[test]
    fn test_remove_path_missing() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let yaml = Document::from_str("name: Alice\n").unwrap();

        let result = yaml.try_remove_path("missing").is_ok();
        assert!(!result);

        let result = yaml.try_remove_path("name.nested").is_ok();
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

        let result = yaml.try_remove_path("app.database.primary.port").is_ok();
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
        let host = mapping.try_get_path("server.host").ok();
        assert_eq!(
            host.as_ref()
                .and_then(|v| v.as_scalar())
                .map(|s| s.to_string()),
            Some("localhost".to_string())
        );

        // Set on mapping
        mapping.try_set_path("server.port", 8080).expect("set_path");
        assert_eq!(
            yaml.to_string(),
            "server:\n  host: localhost\n  port: 8080\n"
        );

        // Remove from mapping
        let result = mapping.try_remove_path("server.port").is_ok();
        assert!(result);

        // Try to remove non-existent path from mapping
        let result_missing = mapping.try_remove_path("nonexistent.path").is_ok();
        assert!(!result_missing);
    }

    #[test]
    fn test_mapping_remove_path_single_segment() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let yaml = Document::from_str("a: 1\nb: 2\n").unwrap();
        let mapping = yaml.as_mapping().unwrap();

        // A single-segment path removes the key directly from the mapping.
        assert!(mapping.try_remove_path("a").is_ok());
        assert!(mapping.try_get_path("a").is_err());
        assert!(mapping.try_get_path("b").is_ok());

        // Removing a missing single-segment key returns false.
        assert!(mapping.try_remove_path("missing").is_err());
    }

    #[test]
    fn test_set_path_preserves_formatting() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let yaml = Document::from_str("server:\n  host: localhost  # production server\n").unwrap();

        yaml.try_set_path("server.host", "newhost")
            .expect("set_path");

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
        yaml.try_set_path("server.host", "localhost")
            .expect("set_path");
        yaml.try_set_path("server.port", 8080).expect("set_path");
        yaml.try_set_path("database.host", "db.local")
            .expect("set_path");
        yaml.try_set_path("database.port", 5432).expect("set_path");

        // Verify all values
        assert_eq!(
            yaml.try_get_path("server.host")
                .ok()
                .as_ref()
                .and_then(|v| v.as_scalar())
                .map(|s| s.to_string()),
            Some("localhost".to_string())
        );
        assert_eq!(
            yaml.try_get_path("server.port")
                .ok()
                .as_ref()
                .and_then(|v| v.as_scalar())
                .map(|s| s.to_string()),
            Some("8080".to_string())
        );
        assert_eq!(
            yaml.try_get_path("database.host")
                .ok()
                .as_ref()
                .and_then(|v| v.as_scalar())
                .map(|s| s.to_string()),
            Some("db.local".to_string())
        );
        assert_eq!(
            yaml.try_get_path("database.port")
                .ok()
                .as_ref()
                .and_then(|v| v.as_scalar())
                .map(|s| s.to_string()),
            Some("5432".to_string())
        );

        // Remove some values
        yaml.try_remove_path("server.port").expect("remove_path");
        yaml.try_remove_path("database.host").expect("remove_path");

        // Verify removals
        assert!(yaml.try_get_path("server.port").is_err());
        assert!(yaml.try_get_path("database.host").is_err());

        // Verify remaining values still exist
        assert_eq!(
            yaml.try_get_path("server.host")
                .ok()
                .as_ref()
                .and_then(|v| v.as_scalar())
                .map(|s| s.to_string()),
            Some("localhost".to_string())
        );
        assert_eq!(
            yaml.try_get_path("database.port")
                .ok()
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
        doc.try_set_path("a.b[0].c", "value").expect("set_path");
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
        doc.try_set_path("items[1]", "B").expect("set_path");
        assert_eq!(doc.to_string(), "items:\n  - a\n  - B\n");
    }

    #[test]
    fn test_set_path_grows_sequence_with_nulls() {
        use crate::yaml::Document;
        use std::str::FromStr;
        let doc = Document::from_str("items:\n  - a\n").unwrap();
        doc.try_set_path("items[3]", "z").expect("set_path");
        assert_eq!(
            doc.to_string(),
            "items:\n  - a\n  - null\n  - null\n  - z\n"
        );
    }

    #[test]
    fn test_set_path_index_does_not_replace_existing_mapping() {
        use crate::yaml::Document;
        use std::str::FromStr;
        let doc = Document::from_str("m:\n  a: 1\n  b: 2\n").unwrap();
        doc.try_set_path("m[0]", "z").expect("set_path");
        assert_eq!(doc.to_string(), "m:\n  a: 1\n  b: 2\n  '0': z\n");

        let doc = Document::from_str("m:\n  \"0\":\n    x: 1\n").unwrap();
        assert_eq!(
            doc.try_get_path("m.0.x")
                .unwrap()
                .as_scalar()
                .unwrap()
                .as_string(),
            "1"
        );
        doc.try_set_path("m.0.x", "2").expect("set_path");
        assert_eq!(
            doc.try_get_path("m.0.x")
                .unwrap()
                .as_scalar()
                .unwrap()
                .as_string(),
            "2"
        );
        assert!(doc.get_mapping("m").is_some());

        let doc = Document::from_str("items:\n  - a: 1\n    b: 2\n").unwrap();
        doc.try_set_path("items[0][0]", "z").expect("set_path");
        assert_eq!(doc.to_string(), "items:\n  - a: 1\n    b: 2\n    '0': z\n");
    }
}
