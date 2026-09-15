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
//! # Aliases
//!
//! When a path segment lands on an alias (`*name`), reads resolve it to
//! the anchored value by default, matching
//! [`get_resolved`](crate::anchor_resolution::DocumentResolvedExt::get_resolved)
//! and [`merged`](crate::anchor_resolution::MappingMergedExt::merged):
//!
//! ```
//! use yaml_edit::{Document, path::YamlPath};
//! use std::str::FromStr;
//!
//! let doc = Document::from_str("shared: &shared\n  timeout: 30\nservice: *shared\n").unwrap();
//! assert_eq!(doc.try_get_path("service.timeout").unwrap().to_string(), "30");
//! ```
//!
//! Writes are different: writing through an alias changes the anchored
//! node, and so changes every other alias pointing at it. That is
//! refused by default. Pass an [`AliasPolicy`] to
//! [`try_set_path_with`](YamlPath::try_set_path_with) to choose
//! deliberately between updating the shared anchor
//! ([`Follow`](AliasPolicy::Follow)) and replacing the alias with an
//! independent copy ([`Expand`](AliasPolicy::Expand)).
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
    /// A tagged collection (`key: !custom` over a block mapping, say) is
    /// descended into as the collection it wraps; the tag stays put.
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
    fn try_get_path(&self, path: &str) -> Result<crate::as_yaml::YamlNode, PathError> {
        self.try_get_path_with(path, AliasPolicy::Follow)
    }

    /// Like [`try_get_path`](Self::try_get_path), but with an explicit
    /// [`AliasPolicy`] for segments that land on an alias.
    ///
    /// # Errors
    ///
    /// As [`try_get_path`](Self::try_get_path), plus
    /// [`PathError::AliasRefused`] under [`AliasPolicy::Refuse`] and
    /// [`PathError::UndefinedAlias`] when an alias names an anchor that
    /// is not defined in the document.
    fn try_get_path_with(
        &self,
        path: &str,
        policy: AliasPolicy,
    ) -> Result<crate::as_yaml::YamlNode, PathError>;

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
    fn try_set_path(&self, path: &str, value: impl crate::AsYaml) -> Result<(), PathError> {
        self.try_set_path_with(path, value, AliasPolicy::Refuse)
    }

    /// Like [`try_set_path`](Self::try_set_path), but with an explicit
    /// [`AliasPolicy`] for intermediate segments that land on an alias.
    ///
    /// Writing through an alias changes the anchored node, and therefore
    /// every other alias that references it; that is why the default is
    /// [`AliasPolicy::Refuse`] and this opt-in exists.
    ///
    /// # Errors
    ///
    /// As [`try_set_path`](Self::try_set_path), plus
    /// [`PathError::AliasRefused`] under [`AliasPolicy::Refuse`] and
    /// [`PathError::UndefinedAlias`] when an alias names an anchor that
    /// is not defined in the document.
    fn try_set_path_with(
        &self,
        path: &str,
        value: impl crate::AsYaml,
        policy: AliasPolicy,
    ) -> Result<(), PathError>;

    /// Remove a value at a nested path. Returns the removed
    /// [`YamlNode`](crate::as_yaml::YamlNode) on success, or a specific
    /// [`PathError`] describing why the removal did not happen.
    ///
    /// # Errors
    ///
    /// Same shape as [`try_get_path`](Self::try_get_path).
    fn try_remove_path(&self, path: &str) -> Result<crate::as_yaml::YamlNode, PathError> {
        self.try_remove_path_with(path, AliasPolicy::Refuse)
    }

    /// Like [`try_remove_path`](Self::try_remove_path), but with an
    /// explicit [`AliasPolicy`] for intermediate segments that land on
    /// an alias.
    ///
    /// Removal is a mutation, so it defaults to
    /// [`AliasPolicy::Refuse`] for the same reason as
    /// [`try_set_path`](Self::try_set_path).
    ///
    /// # Errors
    ///
    /// As [`try_remove_path`](Self::try_remove_path), plus
    /// [`PathError::AliasRefused`] and [`PathError::UndefinedAlias`].
    fn try_remove_path_with(
        &self,
        path: &str,
        policy: AliasPolicy,
    ) -> Result<crate::as_yaml::YamlNode, PathError>;

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
    /// A segment landed on an alias (`*name`) and the active
    /// [`AliasPolicy`] refused to go through it. `at` is the segment,
    /// `alias` the anchor name it referenced.
    AliasRefused {
        /// The segment whose value is an alias.
        at: String,
        /// The anchor name the alias references, without the `*`.
        alias: String,
    },
    /// A segment landed on an alias whose anchor is not defined anywhere
    /// in the document, so it could not be resolved.
    UndefinedAlias {
        /// The segment whose value is a dangling alias.
        at: String,
        /// The anchor name the alias references, without the `*`.
        alias: String,
    },
}

/// What path traversal should do when a segment lands on an alias
/// (`*name`) that it needs to descend through.
///
/// Reads and writes want different answers. Reading through an alias is
/// just a lookup, so [`try_get_path`](YamlPath::try_get_path) resolves by
/// default. Writing through one mutates the shared anchor and therefore
/// every other alias pointing at it, so
/// [`try_set_path`](YamlPath::try_set_path) refuses by default and the
/// caller has to pick a policy deliberately.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum AliasPolicy {
    /// Refuse to descend, reporting [`PathError::AliasRefused`].
    ///
    /// The default for writes.
    #[default]
    Refuse,
    /// Follow the alias to its anchored node.
    ///
    /// For reads this is plain resolution. For writes it mutates the
    /// anchored node in place, so the change is visible through the
    /// anchor and through every other alias that references it.
    Follow,
    /// Replace the alias with an independent copy of the anchored node,
    /// then operate on that copy.
    ///
    /// Only meaningful for writes; reads treat it like
    /// [`Follow`](Self::Follow), since a read has nothing to write back.
    /// The copy does not carry the original `&anchor`, so other aliases
    /// keep pointing at the untouched original.
    Expand,
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
            PathError::AliasRefused { at, alias } => write!(
                f,
                "path segment {at:?} is an alias to {alias:?}; refusing to descend through it"
            ),
            PathError::UndefinedAlias { at, alias } => write!(
                f,
                "path segment {at:?} is an alias to undefined anchor {alias:?}"
            ),
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

/// Resolve a node to the mapping it addresses, looking through a tag.
///
/// A tagged collection is a mapping or sequence wearing a tag, so a path
/// segment descends into the inner collection and leaves the tag in place.
fn node_as_mapping(node: &crate::as_yaml::YamlNode) -> Option<Mapping> {
    match node {
        crate::as_yaml::YamlNode::Mapping(m) => Some(m.clone()),
        crate::as_yaml::YamlNode::TaggedNode(t) => t.as_mapping(),
        _ => None,
    }
}

/// Resolve a node to the sequence it addresses, looking through a tag.
/// See [`node_as_mapping`].
fn node_as_sequence(node: &crate::as_yaml::YamlNode) -> Option<crate::yaml::Sequence> {
    match node {
        crate::as_yaml::YamlNode::Sequence(s) => Some(s.clone()),
        crate::as_yaml::YamlNode::TaggedNode(t) => t.as_sequence(),
        _ => None,
    }
}

/// Whether an existing value may be replaced by a container the setter
/// creates. Only a bare scalar may: a tagged scalar, an alias, or a
/// collection of the wrong shape is user data we refuse to overwrite.
fn is_replaceable_scalar(node: &crate::as_yaml::YamlNode) -> bool {
    matches!(node, crate::as_yaml::YamlNode::Scalar(_))
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
    policy: AliasPolicy,
) -> Result<crate::as_yaml::YamlNode, PathError> {
    for segment in segments {
        current = descend_one(current, segment, policy)?;
    }
    Ok(current)
}

/// Build an anchor registry covering the whole tree `node` belongs to,
/// so aliases anywhere in the document resolve even when traversal
/// started from a nested `Mapping`.
fn registry_for(node: &crate::as_yaml::YamlNode) -> crate::anchor_resolution::AnchorRegistry {
    let root = node
        .syntax()
        .ancestors()
        .last()
        .unwrap_or_else(|| node.syntax().clone());
    crate::anchor_resolution::AnchorRegistry::from_tree(&root)
}

/// Resolve `current` if it is an alias, according to `policy`.
///
/// `segment` is only used to label errors: it is the segment that
/// produced `current`, i.e. the one the caller is about to descend
/// through.
fn resolve_for_read(
    current: crate::as_yaml::YamlNode,
    segment: &PathSegment,
    policy: AliasPolicy,
) -> Result<crate::as_yaml::YamlNode, PathError> {
    let crate::as_yaml::YamlNode::Alias(alias) = &current else {
        return Ok(current);
    };
    let name = alias.name();

    if policy == AliasPolicy::Refuse {
        return Err(PathError::AliasRefused {
            at: segment_display(segment),
            alias: name,
        });
    }

    let registry = registry_for(&current);
    let target = registry
        .resolve(&name)
        .cloned()
        .ok_or_else(|| PathError::UndefinedAlias {
            at: segment_display(segment),
            alias: name.clone(),
        })?;

    crate::as_yaml::YamlNode::from_syntax_peeled(target).ok_or(PathError::UndefinedAlias {
        at: segment_display(segment),
        alias: name,
    })
}

/// Look up `key` in `mapping`, falling back to the keys it inherits
/// through `<<:` merge keys.
///
/// A direct entry always wins: YAML 1.1 says a mapping's own key
/// shadows one merged in from an anchor.
fn lookup_merged(mapping: &Mapping, key: &str) -> Option<crate::as_yaml::YamlNode> {
    if let Some(found) = mapping.get(key) {
        return Some(found);
    }
    use crate::anchor_resolution::MappingMergedExt;
    let registry = registry_for(&crate::as_yaml::YamlNode::Mapping(mapping.clone()));
    mapping.merged(&registry).get(key)
}

fn descend_one(
    current: crate::as_yaml::YamlNode,
    segment: &PathSegment,
    policy: AliasPolicy,
) -> Result<crate::as_yaml::YamlNode, PathError> {
    let current = resolve_for_read(current, segment, policy)?;
    match segment {
        PathSegment::Key(key) => {
            let mapping = node_as_mapping(&current).ok_or_else(|| PathError::TypeMismatch {
                at: segment_display(segment),
            })?;
            lookup_merged(&mapping, key).ok_or_else(|| PathError::NotFound {
                at: segment_display(segment),
            })
        }
        PathSegment::Index(index) => {
            if let Some(seq) = node_as_sequence(&current) {
                seq.get(*index).ok_or_else(|| PathError::NotFound {
                    at: segment_display(segment),
                })
            } else if let Some(map) = node_as_mapping(&current) {
                // Fallback: numeric segment used as a mapping key.
                lookup_merged(&map, &index.to_string()).ok_or_else(|| PathError::NotFound {
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
    fn try_get_path_with(
        &self,
        path: &str,
        policy: AliasPolicy,
    ) -> Result<crate::as_yaml::YamlNode, PathError> {
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

        navigate_path(root, &segments, policy)
    }

    fn try_set_path_with(
        &self,
        path: &str,
        value: impl crate::AsYaml,
        policy: AliasPolicy,
    ) -> Result<(), PathError> {
        let segments = path_segments_required(path)?;

        // Only a root mapping can hold new-key insertions; a scalar or
        // sequence root has no place to graft `foo.bar` under.
        let mapping = self.as_mapping().ok_or(PathError::NoRoot)?;

        set_path_on_mapping(&mapping, &segments, value, policy)
    }

    fn try_remove_path_with(
        &self,
        path: &str,
        policy: AliasPolicy,
    ) -> Result<crate::as_yaml::YamlNode, PathError> {
        let segments = path_segments_required(path)?;

        let root = if let Some(m) = self.as_mapping() {
            crate::as_yaml::YamlNode::Mapping(m)
        } else if let Some(s) = self.as_sequence() {
            crate::as_yaml::YamlNode::Sequence(s)
        } else {
            return Err(PathError::NoRoot);
        };

        remove_path_impl(root, &segments, policy)
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
    policy: AliasPolicy,
) -> Result<crate::as_yaml::YamlNode, PathError> {
    debug_assert!(!segments.is_empty(), "caller must reject empty paths");

    // The node we were handed may itself be an alias (e.g. `a: *shared`
    // then removing `a.timeout`). Resolving here means the final
    // container lookup below sees the anchored mapping.
    let root = resolve_for_read(root, &segments[0], policy)?;

    if segments.len() == 1 {
        let seg = &segments[0];
        let key = match seg {
            PathSegment::Key(key) => key.clone(),
            PathSegment::Index(index) => {
                // Numeric segment on a mapping falls back to the
                // stringified key. On a sequence, index removal is
                // unsupported.
                if node_as_mapping(&root).is_none() {
                    return Err(PathError::TypeMismatch {
                        at: segment_display(seg),
                    });
                }
                index.to_string()
            }
        };
        let mapping = node_as_mapping(&root).ok_or_else(|| PathError::TypeMismatch {
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
    let nested = descend_one(root, &segments[0], policy)?;
    remove_path_impl(nested, &segments[1..], policy)
}

// Implementation for Mapping
impl YamlPath for Mapping {
    fn try_get_path_with(
        &self,
        path: &str,
        policy: AliasPolicy,
    ) -> Result<crate::as_yaml::YamlNode, PathError> {
        let segments = path_segments_required(path)?;
        navigate_path(
            crate::as_yaml::YamlNode::Mapping(self.clone()),
            &segments,
            policy,
        )
    }

    fn try_set_path_with(
        &self,
        path: &str,
        value: impl crate::AsYaml,
        policy: AliasPolicy,
    ) -> Result<(), PathError> {
        let segments = path_segments_required(path)?;
        set_path_on_mapping(self, &segments, value, policy)
    }

    fn try_remove_path_with(
        &self,
        path: &str,
        policy: AliasPolicy,
    ) -> Result<crate::as_yaml::YamlNode, PathError> {
        let segments = path_segments_required(path)?;
        remove_path_impl(
            crate::as_yaml::YamlNode::Mapping(self.clone()),
            &segments,
            policy,
        )
    }
}

/// Resolve an alias encountered as an intermediate segment of a write.
///
/// Under [`AliasPolicy::Refuse`] this is an error. Under
/// [`AliasPolicy::Follow`] it returns the anchored node itself, so
/// writes land on the shared value. Under [`AliasPolicy::Expand`] it
/// returns a detached copy of the anchored node, which the caller
/// substitutes for the alias; the copy is rebuilt as its own tree root
/// so it does not carry the original `&anchor` along.
fn resolve_alias_for_write(
    alias: &crate::yaml::Alias,
    segment: &PathSegment,
    policy: AliasPolicy,
) -> Result<crate::as_yaml::YamlNode, PathError> {
    use rowan::ast::AstNode;

    let name = alias.name();
    if policy == AliasPolicy::Refuse {
        return Err(PathError::AliasRefused {
            at: segment_display(segment),
            alias: name,
        });
    }

    let undefined = || PathError::UndefinedAlias {
        at: segment_display(segment),
        alias: name.clone(),
    };

    let root = alias
        .syntax()
        .ancestors()
        .last()
        .unwrap_or_else(|| alias.syntax().clone());
    let target = crate::anchor_resolution::AnchorRegistry::from_tree(&root)
        .resolve(&name)
        .cloned()
        .ok_or_else(undefined)?;

    let target = if policy == AliasPolicy::Expand {
        // Detaching drops the `&anchor` that sits beside the original in
        // its parent VALUE, so the copy is a plain value.
        crate::yaml::SyntaxNode::new_root_mut(target.green().into_owned())
    } else {
        target
    };

    crate::as_yaml::YamlNode::from_syntax_peeled(target).ok_or_else(undefined)
}

/// Continue a write into `node`, dispatching on whether it is a mapping
/// or a sequence. `via` is the segment that produced `node`, used only
/// to label a type mismatch.
fn set_path_on_node<V: crate::AsYaml>(
    node: crate::as_yaml::YamlNode,
    segments: &[PathSegment],
    value: V,
    policy: AliasPolicy,
    via: &PathSegment,
) -> Result<(), PathError> {
    if let Some(m) = node.as_mapping() {
        set_path_on_mapping(m, segments, value, policy)
    } else if let Some(seq) = node.as_sequence() {
        set_path_on_sequence(seq, segments, value, policy)
    } else {
        Err(PathError::TypeMismatch {
            at: segment_display(via),
        })
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
    policy: AliasPolicy,
) -> Result<(), PathError> {
    debug_assert!(!segments.is_empty(), "caller must reject empty paths");

    // First segment: numeric segments (from `foo.997`) are stringified
    // so they still address a mapping key.
    let first_key_owned = segment_key(&segments[0]);
    let first_key = first_key_owned.as_str();

    if segments.len() == 1 {
        // Base case: set directly. Replacing an alias outright is not
        // writing *through* it -- no other alias is affected -- so it
        // needs no policy check.
        mapping.set(first_key, value);
        return Ok(());
    }

    // An intermediate alias: either refuse, follow it into the anchored
    // node, or replace it here with an anchor-free copy and continue in
    // that copy.
    if let Some(crate::as_yaml::YamlNode::Alias(alias)) = mapping.get(first_key) {
        let resolved = resolve_alias_for_write(&alias, &segments[0], policy)?;
        let target = if policy == AliasPolicy::Expand {
            mapping.set(first_key, resolved);
            // Re-read through the mapping so we descend into the copy
            // that now lives in the tree, not the detached one.
            mapping
                .get(first_key)
                .ok_or_else(|| PathError::TypeMismatch {
                    at: segment_display(&segments[0]),
                })?
        } else {
            resolved
        };
        return set_path_on_node(target, &segments[1..], value, policy, &segments[0]);
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

    let existing = mapping.get(first_key);

    if next_wants_sequence {
        if let Some(nested) = existing.as_ref().and_then(node_as_sequence) {
            return set_path_on_sequence(&nested, &segments[1..], value, policy);
        }
        // Index on an existing mapping is a key (`m.0` / `m[0]`), same
        // as get_path. Do not replace the mapping with a sequence.
        if let Some(nested) = existing.as_ref().and_then(node_as_mapping) {
            return set_path_on_mapping(&nested, &segments[1..], value, policy);
        }
        if existing.as_ref().is_some_and(|n| !is_replaceable_scalar(n)) {
            return Err(PathError::TypeMismatch {
                at: segment_display(&segments[0]),
            });
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
        return set_path_on_sequence(&nested, &segments[1..], value, policy);
    }

    if let Some(nested) = existing.as_ref().and_then(node_as_mapping) {
        return set_path_on_mapping(&nested, &segments[1..], value, policy);
    }
    if existing.as_ref().is_some_and(|n| !is_replaceable_scalar(n)) {
        return Err(PathError::TypeMismatch {
            at: segment_display(&segments[0]),
        });
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
    set_path_on_mapping(&nested, &segments[1..], value, policy)
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
    policy: AliasPolicy,
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
        // Replacing an alias entry outright affects no other alias, so
        // no policy check here (mirrors the mapping base case).
        sequence.set(index, value);
        return Ok(());
    }

    // Same alias handling as the mapping case: refuse, follow into the
    // anchored node, or swap in an anchor-free copy and descend there.
    if let Some(crate::as_yaml::YamlNode::Alias(alias)) = sequence.get(index) {
        let resolved = resolve_alias_for_write(&alias, &segments[0], policy)?;
        let target = if policy == AliasPolicy::Expand {
            sequence.set(index, resolved);
            sequence.get(index).ok_or_else(|| PathError::TypeMismatch {
                at: segment_display(&segments[0]),
            })?
        } else {
            resolved
        };
        return set_path_on_node(target, &segments[1..], value, policy, &segments[0]);
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
    let existing = sequence.get(index);

    if next_wants_sequence {
        if let Some(nested) = existing.as_ref().and_then(node_as_sequence) {
            return set_path_on_sequence(&nested, &segments[1..], value, policy);
        }
        if let Some(nested) = existing.as_ref().and_then(node_as_mapping) {
            return set_path_on_mapping(&nested, &segments[1..], value, policy);
        }
        if existing.as_ref().is_some_and(|n| !is_replaceable_scalar(n)) {
            return Err(PathError::TypeMismatch {
                at: segment_display(&segments[0]),
            });
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
        return set_path_on_sequence(&nested, &segments[1..], value, policy);
    }

    if let Some(nested) = existing.as_ref().and_then(node_as_mapping) {
        return set_path_on_mapping(&nested, &segments[1..], value, policy);
    }
    if existing.as_ref().is_some_and(|n| !is_replaceable_scalar(n)) {
        return Err(PathError::TypeMismatch {
            at: segment_display(&segments[0]),
        });
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
    set_path_on_mapping(&nested, &segments[1..], value, policy)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn merge_doc() -> crate::yaml::YamlFile {
        use std::str::FromStr;
        crate::yaml::YamlFile::from_str(
            "base: &b\n  a: 1\n  shared: x\nderived:\n  <<: *b\n  own: 2\n  a: 99\n",
        )
        .unwrap()
    }

    #[test]
    fn get_path_resolves_merged_keys() {
        let file = merge_doc();
        let doc = file.document().unwrap();

        // Inherited through `<<: *b`.
        assert_eq!(doc.try_get_path("derived.shared").unwrap().to_string(), "x");
        // A local entry still wins over the merged one.
        assert_eq!(doc.try_get_path("derived.a").unwrap().to_string(), "99");
        // Plain local key, and the anchor's own mapping, are unaffected.
        assert_eq!(doc.try_get_path("derived.own").unwrap().to_string(), "2");
        assert_eq!(doc.try_get_path("base.a").unwrap().to_string(), "1");
    }

    #[test]
    fn get_path_still_reports_genuinely_absent_keys() {
        let file = merge_doc();
        let doc = file.document().unwrap();
        assert!(doc.try_get_path("derived.nope").is_err());
    }

    #[test]
    fn remove_path_stays_literal_for_merged_keys() {
        // An inherited key lives in the anchor, not in the mapping being
        // walked, so it must not look removable from `derived`.
        let file = merge_doc();
        let before = file.to_string();
        let doc = file.document().unwrap();
        assert!(doc.try_remove_path("derived.shared").is_err());
        assert_eq!(file.to_string(), before);
    }

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

        assert_eq!(yaml.to_string(), "server:\n  host: localhost\n");
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
            "app:\n  database:\n    primary:\n      host: db.example.com\n"
        );
    }

    #[test]
    fn test_remove_path_keeps_sibling_indentation() {
        use crate::yaml::Document;
        use std::str::FromStr;

        // Each entry after the first owns the INDENT token before it.
        // Removing an entry without that token left it behind, either
        // over-indenting the next entry or dangling as trailing
        // whitespace when the removed entry was last.
        let doc = Document::from_str("config:\n  a: 1\n  b: 2\n  c: 3\n").unwrap();
        doc.try_remove_path("config.b").unwrap();
        assert_eq!(doc.to_string(), "config:\n  a: 1\n  c: 3\n");

        let doc = Document::from_str("config:\n  a: 1\n  b: 2\n  c: 3\n").unwrap();
        doc.try_remove_path("config.a").unwrap();
        assert_eq!(doc.to_string(), "config:\n  b: 2\n  c: 3\n");

        let doc = Document::from_str("config:\n  a: 1\n  b: 2\n  c: 3\n").unwrap();
        doc.try_remove_path("config.c").unwrap();
        assert_eq!(doc.to_string(), "config:\n  a: 1\n  b: 2\n");

        // Deeper nesting used to compound the over-indentation.
        let doc = Document::from_str("x:\n  y:\n    a: 1\n    b: 2\n").unwrap();
        doc.try_remove_path("x.y.a").unwrap();
        assert_eq!(doc.to_string(), "x:\n  y:\n    b: 2\n");
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

    #[test]
    fn test_set_path_key_does_not_replace_existing_sequence() {
        use crate::yaml::Document;
        use std::str::FromStr;
        let original = "items:\n- a\n- b\n";
        let doc = Document::from_str(original).unwrap();
        assert!(matches!(
            doc.try_get_path("items.foo"),
            Err(PathError::TypeMismatch { .. })
        ));
        assert!(matches!(
            doc.try_set_path("items.foo", "x"),
            Err(PathError::TypeMismatch { .. })
        ));
        assert_eq!(doc.to_string(), original);

        // An alias is refused rather than silently overwritten; the
        // dedicated error says why (see the alias policy tests below).
        let original = "other: &o\n  a: 1\nitems: *o\n";
        let doc = Document::from_str(original).unwrap();
        assert_eq!(
            doc.try_set_path("items.foo", "x"),
            Err(PathError::AliasRefused {
                at: "items".to_string(),
                alias: "o".to_string()
            })
        );
        assert_eq!(doc.to_string(), original);
    }

    #[test]
    fn test_set_path_index_does_not_replace_alias() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let original = "other: &o\n- a\n- b\nitems: *o\n";
        let doc = Document::from_str(original).unwrap();
        assert_eq!(
            doc.try_get_path("items[0]").unwrap().to_string(),
            "a".to_string()
        );
        assert_eq!(
            doc.try_set_path("items[0]", "x"),
            Err(PathError::AliasRefused {
                at: "items".to_string(),
                alias: "o".to_string()
            })
        );
        assert_eq!(doc.to_string(), original);

        // A tagged sequence is a sequence wearing a tag: descend into it
        // and leave the tag alone (#88).
        let doc = Document::from_str("items: !!seq [1, 2]\n").unwrap();
        doc.try_set_path("items[1]", "x").unwrap();
        assert_eq!(doc.to_string(), "items: !!seq [1, x]\n");

        let original = "other: &o\n  a: 1\ntop:\n  - *o\n";
        let doc = Document::from_str(original).unwrap();
        assert_eq!(
            doc.try_set_path("top[0][0]", "x"),
            Err(PathError::AliasRefused {
                at: "[0]".to_string(),
                alias: "o".to_string()
            })
        );
        assert_eq!(doc.to_string(), original);

        // A tagged mapping behaves like the mapping it wraps, so the
        // numeric-on-mapping fallback applies and `[0]` addresses key "0".
        let doc = Document::from_str("top:\n  - !custom\n    a: 1\n").unwrap();
        doc.try_set_path("top[0][0]", "x").unwrap();
        assert_eq!(doc.to_string(), "top:\n  - !custom\n    a: 1\n    '0': x\n");
    }

    #[test]
    fn test_path_descends_into_tagged_mapping() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let doc = Document::from_str("config: !custom\n  a: 1\n  b: 2\n").unwrap();
        assert_eq!(
            doc.try_get_path("config.a")
                .unwrap()
                .as_scalar()
                .unwrap()
                .as_string(),
            "1"
        );

        doc.try_set_path("config.a", 5).unwrap();
        doc.try_set_path("config.c", 7).unwrap();
        assert_eq!(doc.to_string(), "config: !custom\n  a: 5\n  b: 2\n  c: 7\n");
    }

    #[test]
    fn test_path_descends_into_tagged_sequence() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let doc = Document::from_str("items: !!seq [1, 2]\n").unwrap();
        assert_eq!(
            doc.try_get_path("items[0]")
                .unwrap()
                .as_scalar()
                .unwrap()
                .as_string(),
            "1"
        );

        doc.try_set_path("items[0]", 9).unwrap();
        doc.try_set_path("items[2]", 7).unwrap();
        assert_eq!(doc.to_string(), "items: !!seq [9, 2, 7]\n");
    }

    #[test]
    fn test_path_descends_through_nested_tags() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let doc = Document::from_str("a: !outer\n  b: !inner\n    c: 1\n").unwrap();
        doc.try_set_path("a.b.c", 2).unwrap();
        assert_eq!(doc.to_string(), "a: !outer\n  b: !inner\n    c: 2\n");

        let doc = Document::from_str("l: !t\n  - x: 1\n").unwrap();
        doc.try_set_path("l[0].x", 9).unwrap();
        assert_eq!(doc.to_string(), "l: !t\n  - x: 9\n");
    }

    #[test]
    fn test_remove_path_in_tagged_mapping_keeps_tag() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let doc = Document::from_str("config: !custom\n  a: 1\n  b: 2\n").unwrap();
        let removed = doc.try_remove_path("config.b").unwrap();
        assert_eq!(removed.as_scalar().unwrap().as_string(), "2");
        assert_eq!(doc.to_string(), "config: !custom\n  a: 1\n");

        // Draining the last entry collapses to the flow-empty form, as it
        // does for an untagged mapping, but keeps the tag.
        doc.try_remove_path("config.a").unwrap();
        assert_eq!(doc.to_string(), "config: !custom {}\n");
    }

    #[test]
    fn test_path_does_not_descend_into_tagged_scalar() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let original = "config: !custom hello\n";
        let doc = Document::from_str(original).unwrap();
        assert!(matches!(
            doc.try_get_path("config.a"),
            Err(PathError::TypeMismatch { .. })
        ));
        assert!(matches!(
            doc.try_set_path("config.a", 1),
            Err(PathError::TypeMismatch { .. })
        ));
        assert_eq!(doc.to_string(), original);
    }

    const ALIASED: &str = "shared: &shared\n  timeout: 30\nservice: *shared\n";

    #[test]
    fn test_get_path_resolves_alias_by_default() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let doc = Document::from_str(ALIASED).unwrap();
        assert_eq!(
            doc.try_get_path("service.timeout").unwrap().to_string(),
            "30".to_string()
        );
        assert_eq!(doc.to_string(), ALIASED.to_string());
    }

    #[test]
    fn test_get_path_refuse_policy_reports_alias() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let doc = Document::from_str(ALIASED).unwrap();
        assert_eq!(
            doc.try_get_path_with("service.timeout", AliasPolicy::Refuse),
            Err(PathError::AliasRefused {
                at: "timeout".to_string(),
                alias: "shared".to_string()
            })
        );
    }

    #[test]
    fn test_get_path_undefined_alias() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let doc = Document::from_str(
            "service: *missing
",
        )
        .unwrap();
        assert_eq!(
            doc.try_get_path("service.timeout"),
            Err(PathError::UndefinedAlias {
                at: "timeout".to_string(),
                alias: "missing".to_string()
            })
        );
    }

    #[test]
    fn test_set_path_refuses_alias_by_default() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let doc = Document::from_str(ALIASED).unwrap();
        assert_eq!(
            doc.try_set_path("service.timeout", 60),
            Err(PathError::AliasRefused {
                at: "service".to_string(),
                alias: "shared".to_string()
            })
        );
        assert_eq!(doc.to_string(), ALIASED.to_string());
    }

    #[test]
    fn test_set_path_follow_writes_through_to_anchor() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let doc = Document::from_str(ALIASED).unwrap();
        doc.try_set_path_with("service.timeout", 60, AliasPolicy::Follow)
            .unwrap();
        // The anchored value changed, so every alias to it sees 60.
        assert_eq!(
            doc.to_string(),
            "shared: &shared\n  timeout: 60\nservice: *shared\n".to_string()
        );
    }

    #[test]
    fn test_set_path_expand_copies_without_anchor() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let doc = Document::from_str(ALIASED).unwrap();
        doc.try_set_path_with("service.timeout", 60, AliasPolicy::Expand)
            .unwrap();
        // The alias is replaced by an independent copy; the anchor keeps
        // its original value and carries no duplicate `&shared`.
        assert_eq!(
            doc.to_string(),
            "shared: &shared\n  timeout: 30\nservice:\n  timeout: 60\n".to_string()
        );
    }

    #[test]
    fn test_set_path_expand_leaves_other_aliases_alone() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let src = "shared: &shared\n  timeout: 30\na: *shared\nb: *shared\n";
        let doc = Document::from_str(src).unwrap();
        doc.try_set_path_with("a.timeout", 60, AliasPolicy::Expand)
            .unwrap();
        assert_eq!(
            doc.to_string(),
            "shared: &shared\n  timeout: 30\na:\n  timeout: 60\nb: *shared\n".to_string()
        );
    }

    #[test]
    fn test_set_path_replacing_alias_outright_needs_no_policy() {
        use crate::yaml::Document;
        use std::str::FromStr;

        // A single-segment path overwrites the alias entry itself rather
        // than writing through it, so the default policy allows it.
        let doc = Document::from_str(ALIASED).unwrap();
        doc.try_set_path("service", 1).unwrap();
        assert_eq!(
            doc.to_string(),
            "shared: &shared\n  timeout: 30\nservice: 1\n".to_string()
        );
    }

    #[test]
    fn test_set_path_undefined_alias_is_reported() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let src = "service: *missing\n";
        let doc = Document::from_str(src).unwrap();
        assert_eq!(
            doc.try_set_path_with("service.timeout", 1, AliasPolicy::Follow),
            Err(PathError::UndefinedAlias {
                at: "service".to_string(),
                alias: "missing".to_string()
            })
        );
        assert_eq!(doc.to_string(), src.to_string());
    }

    #[test]
    fn test_remove_path_refuses_alias_by_default() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let doc = Document::from_str(ALIASED).unwrap();
        assert_eq!(
            doc.try_remove_path("service.timeout"),
            Err(PathError::AliasRefused {
                at: "timeout".to_string(),
                alias: "shared".to_string()
            })
        );
        assert_eq!(doc.to_string(), ALIASED.to_string());
    }

    #[test]
    fn test_remove_path_follow_removes_from_anchor() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let src = "shared: &shared\n  timeout: 30\n  retries: 3\nservice: *shared\n";
        let doc = Document::from_str(src).unwrap();
        let removed = doc
            .try_remove_path_with("service.timeout", AliasPolicy::Follow)
            .unwrap();
        assert_eq!(removed.to_string(), "30".to_string());
        // The key is gone from the anchored mapping, so the alias sees
        // the removal too.
        assert_eq!(
            doc.to_string(),
            "shared: &shared\n  retries: 3\nservice: *shared\n".to_string()
        );
    }

    #[test]
    fn test_alias_to_sequence_resolves_for_read() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let doc = Document::from_str("other: &o\n- a\n- b\nitems: *o\n").unwrap();
        assert_eq!(
            doc.try_get_path("items[1]").unwrap().to_string(),
            "b".to_string()
        );
    }

    #[test]
    fn test_get_path_on_mapping_resolves_document_anchors() {
        use crate::yaml::Document;
        use std::str::FromStr;

        // Traversal started from a nested Mapping, but the anchor is
        // defined at the document root: the registry is built from the
        // whole tree, so it still resolves.
        let doc =
            Document::from_str("shared: &shared\n  timeout: 30\nouter:\n  svc: *shared\n").unwrap();
        let outer = doc.as_mapping().unwrap().get_mapping("outer").unwrap();
        assert_eq!(
            outer.try_get_path("svc.timeout").unwrap().to_string(),
            "30".to_string()
        );
    }

    #[test]
    fn test_recursive_alias_terminates() {
        use crate::yaml::Document;
        use std::str::FromStr;

        // Each segment resolves exactly one alias hop, so a self
        // referential anchor consumes path segments instead of looping.
        let doc = Document::from_str("a: &x\n  b: *x\n").unwrap();
        assert!(doc.try_get_path("a.b.b.b").is_ok());
    }
}
