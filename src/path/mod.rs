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

use alias::resolve_alias;
use read::{descend_one, navigate_path, resolve_for_read, spelled_key};

#[allow(deprecated)]
pub use parse::parse_path;
use parse::{path_segments_required, segment_display};
pub use parse::{try_parse_path, PathParseError};

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

/// Whether an existing value may be replaced by a container the setter
/// creates. Only a bare scalar may: a tagged scalar, an alias, or a
/// collection of the wrong shape is user data we refuse to overwrite.
fn is_replaceable_scalar(node: &crate::as_yaml::YamlNode) -> bool {
    matches!(node, crate::as_yaml::YamlNode::Scalar(_))
}

/// Remove `key` from `mapping`, returning the value it held. The value is
/// read before removal, so it is the node that was in the tree.
fn take_value(
    mapping: &Mapping,
    key: impl crate::AsYaml + Copy,
) -> Option<crate::as_yaml::YamlNode> {
    let value = mapping.get(key)?;
    mapping.remove(key);
    Some(value)
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

/// Remove a value at a nested path. Returns the removed
/// [`YamlNode`](crate::as_yaml::YamlNode) on success. Errors carry the
/// specific reason (type mismatch vs. not-found).
///
/// Removing by index from a sequence is intentionally unsupported (it
/// would shift every subsequent element) and reported as a
/// `PathError::TypeMismatch` at that segment.
fn remove_path_impl(
    root: crate::as_yaml::YamlNode,
    holder: Option<&PathSegment>,
    segments: &[PathSegment],
    policy: AliasPolicy,
) -> Result<crate::as_yaml::YamlNode, PathError> {
    debug_assert!(!segments.is_empty(), "caller must reject empty paths");
    let seg = &segments[0];
    if segments.len() > 1 {
        let nested = descend_one(root, holder, seg, policy)?;
        return remove_path_impl(nested, Some(seg), &segments[1..], policy);
    }

    // The node we were handed may itself be an alias (e.g. `a: *shared`
    // then removing `a.timeout`). Resolving here means the final
    // container lookup below sees the anchored mapping.
    let holder = holder.unwrap_or(seg);
    let root = resolve_for_read(root, holder, policy)?;
    let mapping = root.mapping_content().ok_or_else(|| {
        // A numeric segment on a mapping is a stringified key; on a
        // sequence, index removal is unsupported.
        let index_on_sequence =
            matches!(seg, PathSegment::Index(_)) && root.sequence_content().is_some();
        PathError::TypeMismatch {
            at: segment_display(if index_on_sequence { seg } else { holder }),
        }
    })?;
    let key = segment_key(seg);
    take_value(&mapping, key.as_str())
        .or_else(|| take_value(&mapping, &spelled_key(mapping.keys(), &key)?))
        .ok_or_else(|| PathError::NotFound {
            at: segment_display(seg),
        })
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

/// Run `write`, which edits `container`'s child node at `child` (a
/// `children_with_tokens` index) without moving it, and put that child back
/// as it was if the write fails, e.g. when the rest of the path does not
/// fit the copy an expanded alias left behind.
fn undo_child_on_err(
    container: &crate::yaml::SyntaxNode,
    child: usize,
    write: impl FnOnce() -> Result<(), PathError>,
) -> Result<(), PathError> {
    let saved = container
        .children_with_tokens()
        .nth(child)
        .and_then(|c| c.into_node())
        .expect("child names a node")
        .green()
        .into_owned();
    let result = write();
    if result.is_err() {
        let restored = crate::yaml::SyntaxNode::new_root_mut(saved);
        crate::nodes::replace_children(container, child..child + 1, vec![restored.into()]);
    }
    result
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
    if let Some(m) = node.mapping_content() {
        set_path_on_mapping(&m, segments, value, policy)
    } else if let Some(seq) = node.sequence_content() {
        set_path_on_sequence(&seq, segments, value, policy)
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
    // so they still address a mapping key. Without a string key of that
    // name, write to an existing scalar key spelled the same.
    let key = segment_key(&segments[0]);
    if mapping.contains_key(key.as_str()) {
        return set_path_in_slot(Slot::Key(mapping, key.as_str()), segments, value, policy);
    }
    match spelled_key(mapping.keys(), &key) {
        Some(existing) => set_path_in_slot(Slot::Key(mapping, &existing), segments, value, policy),
        None => set_path_in_slot(Slot::Key(mapping, key.as_str()), segments, value, policy),
    }
}

/// The place a write continues from: the value of a mapping entry or a
/// sequence item.
enum Slot<'a, K> {
    Key(&'a Mapping, K),
    Index(&'a crate::yaml::Sequence, usize),
}

impl<K: crate::AsYaml + Copy> Slot<'_, K> {
    fn get(&self) -> Option<crate::as_yaml::YamlNode> {
        match *self {
            Slot::Key(mapping, key) => mapping.get(key),
            Slot::Index(sequence, index) => sequence.get(index),
        }
    }

    fn set(&self, value: impl crate::AsYaml) {
        match *self {
            Slot::Key(mapping, key) => mapping.set(key, value),
            Slot::Index(sequence, index) => {
                sequence.set(index, value);
            }
        }
    }

    /// The container node and the `children_with_tokens` index of the
    /// entry holding this slot, which must exist.
    fn entry(&self) -> (&crate::yaml::SyntaxNode, usize) {
        match *self {
            Slot::Key(mapping, key) => (
                &mapping.0,
                mapping
                    .find_entry_index_by_key(key)
                    .expect("get just found this key"),
            ),
            Slot::Index(sequence, index) => (
                &sequence.0,
                crate::nodes::nth_entry_index(
                    &sequence.0,
                    crate::SyntaxKind::SEQUENCE_ENTRY,
                    index,
                )
                .expect("get just found this index"),
            ),
        }
    }

    /// Put an empty sequence or mapping in the slot. Under a mapping the
    /// parent's style is matched so block content is not mixed into a flow
    /// container. Under a sequence it is a flow-empty `[]` or `{}`, which
    /// has no pending-block marker and so stays flow through the
    /// subsequent set.
    fn insert_container(&self, sequence: bool) {
        match *self {
            Slot::Key(mapping, key) => match (sequence, mapping.is_flow_style()) {
                (true, true) => mapping.set(key, crate::yaml::Sequence::new_flow()),
                (true, false) => mapping.set(key, crate::yaml::Sequence::new_pending_block()),
                (false, true) => mapping.set(key, Mapping::new_flow()),
                (false, false) => mapping.set(key, Mapping::new_pending_block()),
            },
            Slot::Index(..) if sequence => self.set(
                crate::builder::SequenceBuilder::new()
                    .build_document()
                    .as_sequence()
                    .expect("SequenceBuilder always produces a sequence"),
            ),
            Slot::Index(..) => self.set(
                crate::builder::MappingBuilder::new()
                    .build_document()
                    .as_mapping()
                    .expect("MappingBuilder always produces a mapping"),
            ),
        }
    }
}

/// Continue a write at `slot`, which `segments[0]` addressed: set the
/// value there if this is the last segment, otherwise descend into the
/// container held there, creating one as needed.
fn set_path_in_slot<K: crate::AsYaml + Copy, V: crate::AsYaml>(
    slot: Slot<'_, K>,
    segments: &[PathSegment],
    value: V,
    policy: AliasPolicy,
) -> Result<(), PathError> {
    if segments.len() == 1 {
        // Replacing an alias outright is not writing *through* it -- no
        // other alias is affected -- so it needs no policy check.
        slot.set(value);
        return Ok(());
    }
    let (at, rest) = (&segments[0], &segments[1..]);
    let mismatch = || PathError::TypeMismatch {
        at: segment_display(at),
    };
    let existing = slot.get();

    // An intermediate alias: either refuse, follow it into the anchored
    // node, or replace it here with an anchor-free copy and continue in
    // that copy.
    if let Some(crate::as_yaml::YamlNode::Alias(alias)) = &existing {
        let resolved = resolve_alias(alias, at, policy)?;
        if policy != AliasPolicy::Expand {
            return set_path_on_node(resolved, rest, value, policy, at);
        }
        let (container, child) = slot.entry();
        return undo_child_on_err(container, child, || {
            slot.set(resolved);
            // Re-read through the container so we descend into the copy
            // that now lives in the tree, not the detached one.
            let target = slot.get().expect("set keeps the entry");
            set_path_on_node(target, rest, value, policy, at)
        });
    }

    // Descending through a scalar would overwrite user data, unless it is
    // a null placeholder (one the user wrote, or one pushed to grow a
    // sequence), which is replaced by the appropriate container.
    if existing
        .as_ref()
        .and_then(|n| n.as_scalar())
        .is_some_and(|sc| !sc.is_null())
    {
        return Err(mismatch());
    }

    let wants_sequence = matches!(rest[0], PathSegment::Index(_));
    if wants_sequence {
        if let Some(nested) = existing.as_ref().and_then(|n| n.sequence_content()) {
            return set_path_on_sequence(&nested, rest, value, policy);
        }
    }
    // Index on an existing mapping is a key (`m.0` / `m[0]`), same as
    // get_path. Do not replace the mapping with a sequence.
    if let Some(nested) = existing.as_ref().and_then(|n| n.mapping_content()) {
        return set_path_on_mapping(&nested, rest, value, policy);
    }
    if existing.as_ref().is_some_and(|n| !is_replaceable_scalar(n)) {
        return Err(mismatch());
    }

    check_new_path(rest)?;
    slot.insert_container(wants_sequence);
    let created = slot.get();
    if wants_sequence {
        let nested = created
            .and_then(|n| n.sequence_content())
            .expect("we just inserted a sequence here");
        set_path_on_sequence(&nested, rest, value, policy)
    } else {
        let nested = created
            .and_then(|n| n.mapping_content())
            .expect("we just inserted a mapping here");
        set_path_on_mapping(&nested, rest, value, policy)
    }
}

/// Check that a write of `segments` into containers about to be created
/// can succeed, before anything is written.
///
/// Every container on the way starts empty, so the only way such a write
/// fails is an index past the growth bound. Writing first and failing
/// later would leave a half-built path behind.
fn check_new_path(segments: &[PathSegment]) -> Result<(), PathError> {
    segments.iter().try_for_each(|segment| match segment {
        PathSegment::Index(index) if *index >= MAX_INDEX_GROWTH => Err(PathError::IndexTooFar {
            at: segment_display(segment),
            len: 0,
            index: *index,
        }),
        _ => Ok(()),
    })
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

    // Grow the sequence with null placeholders until `index` is in range,
    // within reason: an index far past the end would otherwise let a single
    // path allocate unboundedly.
    let len = sequence.len();
    // A single sequence edit is linear in its length, so pad in one splice
    // rather than one push per placeholder.
    if index >= len && index - len >= MAX_INDEX_GROWTH {
        return Err(PathError::IndexTooFar {
            at: segment_display(&segments[0]),
            len,
            index,
        });
    }
    if let Some(growth) = (index + 1).checked_sub(len) {
        check_new_path(&segments[1..])?;
        sequence.extend_with(crate::scalar::ScalarValue::null(), growth);
    }

    set_path_in_slot(
        Slot::<&str>::Index(sequence, index),
        segments,
        value,
        policy,
    )
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
