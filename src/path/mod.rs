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
//! let host = yaml.try_get_path("server.host").unwrap();
//! assert_eq!(host.to_string(), "localhost");
//!
//! // Set nested values (creates intermediate mappings)
//! yaml.try_set_path("database.primary.host", "db.example.com").unwrap();
//!
//! // Remove nested values
//! yaml.try_remove_path("server.port").unwrap();
//! assert_eq!(
//!     yaml.to_string(),
//!     "server:\n  host: localhost\ndatabase:\n  primary:\n    host: db.example.com\n"
//! );
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

mod alias;
mod parse;
mod read;
mod write;

#[allow(deprecated)]
pub use parse::parse_path;
pub use parse::{try_parse_path, PathParseError};

use crate::yaml::Mapping;
use parse::path_segments_required;
use read::navigate_path;
use write::{remove_path_impl, set_path_on_mapping};

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
/// A segment names a string key, or failing that the first scalar key
/// of another type written the same way: `m.1` reaches `1: one` and
/// `m.true` reaches `true: t`. Setting a key that does not exist yet
/// creates a string key.
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
    /// - [`PathError::EmptyPath`] when the path is empty.
    /// - [`PathError::NoRoot`] when the receiver has no value to descend into.
    /// - [`PathError::TypeMismatch`] when a segment tries to descend into a
    ///   value of the wrong container type.
    /// - [`PathError::NotFound`] when a segment addresses a key/index that
    ///   does not exist.
    /// - [`PathError::UndefinedAlias`] or [`PathError::InvalidMergeOperand`]
    ///   when a key is not found directly and a `<<` merge operand of the
    ///   mapping is an undefined alias or not a mapping.
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
    /// - [`PathError::EmptyPath`] when the path is empty.
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
    /// One entry is removed. Duplicate keys are legal YAML, so a path can
    /// still resolve afterwards; use
    /// [`Mapping::remove_nth_occurrence`](crate::Mapping::remove_nth_occurrence)
    /// to choose between them.
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
    /// assert_eq!(host.unwrap().to_string(), "localhost");
    /// #[allow(deprecated)]
    /// let missing = yaml.get_path("server..host");
    /// assert!(missing.is_none());
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
    /// assert_eq!(
    ///     yaml.to_string(),
    ///     "name: test\nserver:\n  host: localhost\n  port: 8080\n"
    /// );
    /// // A failed write is not reported.
    /// #[allow(deprecated)]
    /// yaml.set_path("name.first", "x");
    /// assert_eq!(yaml.try_get_path("name").unwrap().to_string(), "test");
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

/// The most null placeholders [`try_set_path`](YamlPath::try_set_path) will
/// insert to reach an out-of-range sequence index.
///
/// Writing to `s[5]` on an empty sequence is a reasonable way to build one;
/// writing to `s[900000]` is almost always a mistake or hostile input, and
/// filling the gap would cost time and memory that grows faster than the
/// index (each insert re-splices the sequence's children).
pub const MAX_INDEX_GROWTH: usize = 256;

/// Error returned by [`try_get_path`](YamlPath::try_get_path) /
/// [`try_set_path`](YamlPath::try_set_path) /
/// [`try_remove_path`](YamlPath::try_remove_path).
///
/// Distinguishes the several ways a path operation can fail so callers
/// can react appropriately, instead of getting the previous silent
/// no-op or ambiguous `None`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PathError {
    /// The path string is malformed; see [`PathParseError`].
    Parse(PathParseError),
    /// The path is the empty string. Stray dots are a
    /// [`PathParseError::EmptySegment`] instead.
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
    /// Setting the requested index would have grown a sequence by more
    /// than [`MAX_INDEX_GROWTH`] null placeholders.
    ///
    /// `try_set_path("s[900000]", v)` on a short sequence would otherwise
    /// pad it with that many entries, so a path from untrusted input could
    /// spend unbounded time and memory.
    IndexTooFar {
        /// The segment whose index was out of reach.
        at: String,
        /// The current length of the sequence.
        len: usize,
        /// The index that was asked for.
        index: usize,
    },
    /// A segment landed on an alias whose anchor is not defined anywhere
    /// in the document, so it could not be resolved. Also reported when a
    /// `<<` merge operand consulted for the segment is such an alias.
    UndefinedAlias {
        /// The segment whose value is a dangling alias.
        at: String,
        /// The anchor name the alias references, without the `*`.
        alias: String,
    },
    /// Looking up a segment needed the mapping's merge keys, and a `<<`
    /// operand was neither a mapping nor an alias to one.
    InvalidMergeOperand {
        /// The segment being looked up.
        at: String,
        /// The source text of the offending operand.
        operand: String,
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
            PathError::IndexTooFar { at, len, index } => write!(
                f,
                "path segment {at:?} would grow a sequence of {len} by {} null entries to reach index {index}",
                index + 1 - len
            ),
            PathError::UndefinedAlias { at, alias } => write!(
                f,
                "path segment {at:?} is an alias to undefined anchor {alias:?}"
            ),
            PathError::InvalidMergeOperand { at, operand } => write!(
                f,
                "looking up path segment {at:?}: merge key operand {operand:?} is not a mapping"
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

/// The document's root content, which a path descends from.
fn root_node(doc: &crate::yaml::Document) -> Result<crate::as_yaml::YamlNode, PathError> {
    doc.as_mapping()
        .map(crate::as_yaml::YamlNode::Mapping)
        .or_else(|| doc.as_sequence().map(crate::as_yaml::YamlNode::Sequence))
        .or_else(|| doc.as_scalar().map(crate::as_yaml::YamlNode::Scalar))
        .ok_or(PathError::NoRoot)
}

// Implementation for Document
impl YamlPath for crate::yaml::Document {
    fn try_get_path_with(
        &self,
        path: &str,
        policy: AliasPolicy,
    ) -> Result<crate::as_yaml::YamlNode, PathError> {
        let segments = path_segments_required(path)?;

        navigate_path(root_node(self)?, &segments, policy)
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

        remove_path_impl(root_node(self)?, None, &segments, policy)
    }
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
            None,
            &segments,
            policy,
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_path_error_display() {
        for (error, text) in [
            (
                PathError::Parse(PathParseError::UnclosedIndex),
                "path parse error: unclosed '[' in path",
            ),
            (
                PathError::Parse(PathParseError::InvalidIndex("x".to_string())),
                "path parse error: invalid path index [x]",
            ),
            (
                PathError::Parse(PathParseError::TrailingBackslash),
                "path parse error: path ends with an unescaped '\\'",
            ),
            (
                PathError::Parse(PathParseError::EmptySegment),
                "path parse error: empty segment in path",
            ),
            (
                PathError::Parse(PathParseError::MissingSeparator),
                "path parse error: expected '.' or '[' after ']' in path",
            ),
            (PathError::EmptyPath, "path is empty (no segments)"),
            (PathError::NoRoot, "document has no root to descend into"),
            (
                PathError::TypeMismatch { at: "a".into() },
                "path segment \"a\" is not a container",
            ),
            (
                PathError::NotFound { at: "a".into() },
                "path segment \"a\" not found",
            ),
            (
                PathError::AliasRefused {
                    at: "a".into(),
                    alias: "x".into(),
                },
                "path segment \"a\" is an alias to \"x\"; refusing to descend through it",
            ),
            (
                PathError::IndexTooFar {
                    at: "[300]".into(),
                    len: 2,
                    index: 300,
                },
                "path segment \"[300]\" would grow a sequence of 2 by 299 null entries to reach index 300",
            ),
            (
                PathError::UndefinedAlias {
                    at: "a".into(),
                    alias: "x".into(),
                },
                "path segment \"a\" is an alias to undefined anchor \"x\"",
            ),
            (
                PathError::InvalidMergeOperand {
                    at: "a".into(),
                    operand: "1".into(),
                },
                "looking up path segment \"a\": merge key operand \"1\" is not a mapping",
            ),
        ] {
            assert_eq!(error.to_string(), text);
        }
    }
}
