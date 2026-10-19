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

/// Error from [`try_parse_path`].
#[derive(Debug, Clone, PartialEq, Eq)]
#[non_exhaustive]
pub enum PathParseError {
    /// A `[` was not closed by `]`.
    UnclosedIndex,
    /// The text between `[` and `]` is not a decimal `usize` index.
    InvalidIndex(String),
    /// The path ends with a `\` that has nothing to escape.
    TrailingBackslash,
    /// A segment is empty: the path starts or ends with `.`, contains
    /// `..`, or has a `.` directly before `[`.
    EmptySegment,
    /// A `[...]` index is followed by something other than `.`, `[` or
    /// the end of the path, as in `a[0]b`.
    MissingSeparator,
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
            PathParseError::TrailingBackslash => {
                write!(f, "path ends with an unescaped '\\'")
            }
            PathParseError::EmptySegment => write!(f, "empty segment in path"),
            PathParseError::MissingSeparator => {
                write!(f, "expected '.' or '[' after ']' in path")
            }
        }
    }
}

impl std::error::Error for PathParseError {}

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

/// Parse a path string into components.
///
/// Supports multiple syntaxes:
/// - Dot notation: `"server.host"` → `[Key("server"), Key("host")]`
/// - Array indices with brackets: `"items[0].name"` → `[Key("items"), Index(0), Key("name")]`
/// - Array indices with dots: `"items.0.name"` → `[Key("items"), Index(0), Key("name")]`
/// - Escaped dots: `"key\\.with\\.dots"` → `[Key("key.with.dots")]`
/// - Escaped digits are keys: `"items.\\0"` -> `[Key("items"), Key("0")]`
///
/// An index is ASCII digits only, so `"items.+1"` is the key `"+1"` and
/// `"items[+1]"` is an error. A `[...]` index may start the path and may
/// be followed only by `.`, another `[...]` or the end of the path.
///
/// An empty path returns an empty list; any other path yields at least
/// one segment or an error.
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
/// - [`PathParseError::UnclosedIndex`] when a `[` has no matching `]`.
/// - [`PathParseError::InvalidIndex`] when the brackets do not contain a
///   decimal `usize`.
/// - [`PathParseError::TrailingBackslash`] when the path ends in an
///   unescaped `\\`.
/// - [`PathParseError::EmptySegment`] for `a..b`, `.a`, `a.` or `a.[0]`.
/// - [`PathParseError::MissingSeparator`] for `a[0]b`.
pub fn try_parse_path(path: &str) -> Result<Vec<PathSegment>, PathParseError> {
    #[derive(PartialEq)]
    enum State {
        // At the start of the path, where `[0]` may appear without a key.
        Start,
        // Just after a `.`; a key or digits must follow.
        AfterDot,
        InSegment,
        AfterIndex,
    }

    let mut segments = Vec::new();
    let mut current = String::new();
    // A segment with an escaped character is always a key, so `\0`
    // names the key "0" rather than an index.
    let mut current_has_escape = false;
    let mut state = State::Start;
    let mut chars = path.chars();

    while let Some(ch) = chars.next() {
        match (ch, &state) {
            ('.', State::Start | State::AfterDot) | ('[', State::AfterDot) => {
                return Err(PathParseError::EmptySegment);
            }
            ('.', State::InSegment) => {
                segments.push(dotted_segment(&current, current_has_escape));
                current.clear();
                current_has_escape = false;
                state = State::AfterDot;
            }
            ('.', State::AfterIndex) => state = State::AfterDot,
            ('[', _) => {
                if state == State::InSegment {
                    segments.push(PathSegment::Key(std::mem::take(&mut current)));
                    current_has_escape = false;
                }
                let mut index_str = String::new();
                loop {
                    match chars.next() {
                        Some(']') => break,
                        Some(c) => index_str.push(c),
                        None => return Err(PathParseError::UnclosedIndex),
                    }
                }
                let index = parse_index(&index_str)
                    .ok_or_else(|| PathParseError::InvalidIndex(index_str.clone()))?;
                segments.push(PathSegment::Index(index));
                state = State::AfterIndex;
            }
            (_, State::AfterIndex) => return Err(PathParseError::MissingSeparator),
            ('\\', _) => {
                current.push(chars.next().ok_or(PathParseError::TrailingBackslash)?);
                current_has_escape = true;
                state = State::InSegment;
            }
            _ => {
                current.push(ch);
                state = State::InSegment;
            }
        }
    }

    match state {
        State::AfterDot => return Err(PathParseError::EmptySegment),
        State::InSegment => segments.push(dotted_segment(&current, current_has_escape)),
        State::Start | State::AfterIndex => {}
    }

    Ok(segments)
}

/// Parse an index made of ASCII digits only; `usize::from_str` would
/// also accept a leading `+`.
fn parse_index(text: &str) -> Option<usize> {
    if text.is_empty() || !text.bytes().all(|b| b.is_ascii_digit()) {
        return None;
    }
    text.parse().ok()
}

/// A dot-separated segment is an index when it is a bare number
/// (`items.0`), and a key otherwise.
fn dotted_segment(text: &str, has_escape: bool) -> PathSegment {
    match parse_index(text) {
        Some(index) if !has_escape => PathSegment::Index(index),
        _ => PathSegment::Key(text.to_string()),
    }
}

/// Parse a path string into components.
///
/// Invalid bracket indexes used to return an empty `Vec`, which callers
/// could not tell from a real empty path. Prefer [`try_parse_path`].
#[deprecated(note = "use try_parse_path; a bad `[index]` used to return an empty Vec")]
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
    let mut holder = None;
    for segment in segments {
        current = descend_one(current, holder, segment, policy)?;
        holder = Some(segment);
    }
    Ok(current)
}

/// Build an anchor registry covering the document `node` belongs to,
/// so aliases anywhere in the document resolve even when traversal
/// started from a nested `Mapping`.
///
/// Anchors are scoped to a single document, so the scan stops at the
/// enclosing DOCUMENT rather than the whole stream. A node outside any
/// document (e.g. a detached `Mapping`) uses its own tree root.
fn registry_for(node: &crate::yaml::SyntaxNode) -> crate::anchor_resolution::AnchorRegistry {
    let scope = node
        .ancestors()
        .find(|n| n.kind() == crate::SyntaxKind::DOCUMENT || n.parent().is_none())
        .unwrap_or_else(|| node.clone());
    crate::anchor_resolution::AnchorRegistry::from_tree(&scope)
}

/// Resolve `current` if it is an alias, according to `policy`.
///
/// `segment` is only used to label errors: it is the segment that
/// produced `current`.
fn resolve_for_read(
    current: crate::as_yaml::YamlNode,
    segment: &PathSegment,
    policy: AliasPolicy,
) -> Result<crate::as_yaml::YamlNode, PathError> {
    let crate::as_yaml::YamlNode::Alias(alias) = &current else {
        return Ok(current);
    };
    // A read leaves the tree alone, so there is nothing to expand into.
    let policy = match policy {
        AliasPolicy::Expand => AliasPolicy::Follow,
        policy => policy,
    };
    resolve_alias(alias, segment, policy)
}

/// The [`PathError`] for an alias at `segment` that
/// [`AnchorRegistry::resolve_alias_through`] could not resolve. An alias
/// that ends up referring to itself, as in `&a *a`, refers to no value
/// and is undefined as well.
///
/// [`AnchorRegistry::resolve_alias_through`]: crate::anchor_resolution::AnchorRegistry::resolve_alias_through
fn undefined_alias(
    error: crate::anchor_resolution::AliasError,
    segment: &PathSegment,
) -> PathError {
    use crate::anchor_resolution::AliasError;
    let (AliasError::Undefined { alias } | AliasError::Recursive { alias }) = error;
    PathError::UndefinedAlias {
        at: segment_display(segment),
        alias,
    }
}

/// Find the first scalar key spelled `segment`, whatever type it
/// resolves to, so a path can name keys such as `1`, `true` or `0x10`.
/// The match is on the key text: `m.16` does not reach a `0x10` key.
fn spelled_key(
    mut keys: impl Iterator<Item = crate::as_yaml::YamlNode>,
    segment: &str,
) -> Option<crate::as_yaml::YamlNode> {
    keys.find(|k| k.as_scalar().is_some_and(|s| s.as_string() == segment))
}

/// Look up the key a path segment names in `mapping`'s own entries: a
/// string key equal to `key` if there is one, otherwise the first scalar
/// key spelled `key`.
fn lookup_own(mapping: &Mapping, key: &str) -> Option<crate::as_yaml::YamlNode> {
    mapping
        .get(key)
        .or_else(|| mapping.get(spelled_key(mapping.keys(), key)?))
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

/// Look up `key` in `mapping`, falling back to the keys it inherits
/// through `<<:` merge keys.
///
/// A direct entry always wins: YAML 1.1 says a mapping's own key
/// shadows one merged in from an anchor, so the merge keys are only
/// checked when there is no direct entry.
fn lookup_merged(
    mapping: &Mapping,
    key: &str,
    segment: &PathSegment,
) -> Result<crate::as_yaml::YamlNode, PathError> {
    use crate::anchor_resolution::{MergeError, MergedMapping};
    if let Some(found) = lookup_own(mapping, key) {
        return Ok(found);
    }
    let at = segment_display(segment);
    let registry = registry_for(rowan::ast::AstNode::syntax(mapping));
    let merged = MergedMapping::build(mapping.clone(), &registry).map_err(|e| match e {
        // A key holding an alias within its own anchored node is recursive.
        MergeError::Alias(e) => undefined_alias(e, segment),
        MergeError::InvalidOperand { operand } => PathError::InvalidMergeOperand {
            at: at.clone(),
            operand,
        },
    })?;
    merged
        .get(key)
        .or_else(|| merged.get(spelled_key(merged.keys(), key)?))
        .ok_or(PathError::NotFound { at })
}

/// Apply `segment` to `current`, the value of segment `holder` (`None`
/// for the root, whose errors name `segment` instead).
fn descend_one(
    current: crate::as_yaml::YamlNode,
    holder: Option<&PathSegment>,
    segment: &PathSegment,
    policy: AliasPolicy,
) -> Result<crate::as_yaml::YamlNode, PathError> {
    let holder = holder.unwrap_or(segment);
    let current = resolve_for_read(current, holder, policy)?;
    let mismatch = || PathError::TypeMismatch {
        at: segment_display(holder),
    };
    match segment {
        PathSegment::Key(key) => {
            let mapping = current.mapping_content().ok_or_else(mismatch)?;
            lookup_merged(&mapping, key, segment)
        }
        PathSegment::Index(index) => {
            if let Some(seq) = current.sequence_content() {
                seq.get(*index).ok_or_else(|| PathError::NotFound {
                    at: segment_display(segment),
                })
            } else if let Some(map) = current.mapping_content() {
                // Fallback: numeric segment used as a mapping key.
                lookup_merged(&map, &index.to_string(), segment)
            } else {
                Err(mismatch())
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

/// The document's root content if it is a mapping or a sequence.
fn collection_root(doc: &crate::yaml::Document) -> Option<crate::as_yaml::YamlNode> {
    doc.as_mapping()
        .map(crate::as_yaml::YamlNode::Mapping)
        .or_else(|| doc.as_sequence().map(crate::as_yaml::YamlNode::Sequence))
}

// Implementation for Document
impl YamlPath for crate::yaml::Document {
    fn try_get_path_with(
        &self,
        path: &str,
        policy: AliasPolicy,
    ) -> Result<crate::as_yaml::YamlNode, PathError> {
        let segments = path_segments_required(path)?;

        let root = collection_root(self)
            .or_else(|| self.as_scalar().map(crate::as_yaml::YamlNode::Scalar))
            .ok_or(PathError::NoRoot)?;
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

        let root = collection_root(self).ok_or(PathError::NoRoot)?;
        remove_path_impl(root, None, &segments, policy)
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

/// Resolve an alias that `segment` descends through.
///
/// Under [`AliasPolicy::Refuse`] this is an error. Under
/// [`AliasPolicy::Follow`] it returns the anchored node itself, so
/// writes land on the shared value. Under [`AliasPolicy::Expand`] it
/// returns a detached, anchor-free copy of the anchored node (see
/// [`expanded_copy`]), which the caller substitutes for the alias.
fn resolve_alias(
    alias: &crate::yaml::Alias,
    segment: &PathSegment,
    policy: AliasPolicy,
) -> Result<crate::as_yaml::YamlNode, PathError> {
    use rowan::ast::AstNode;

    if policy == AliasPolicy::Refuse {
        return Err(PathError::AliasRefused {
            at: segment_display(segment),
            alias: alias.name(),
        });
    }
    let registry = registry_for(alias.syntax());
    crate::anchor_resolution::resolve_alias_with(alias, &registry, |target| {
        if policy == AliasPolicy::Expand {
            expanded_copy(target)
        } else {
            target.clone()
        }
    })
    .map_err(|e| undefined_alias(e, segment))
}

/// Build a detached copy of an anchored node to substitute for an alias.
///
/// The copy is dedented to column 0, as a detached root has no context to
/// measure its indentation from. It carries no anchors: the `&anchor` beside
/// the original stays behind in its parent VALUE, and anchors nested inside
/// are dropped so that later aliases keep referring to the original nodes.
fn expanded_copy(target: &crate::yaml::SyntaxNode) -> crate::yaml::SyntaxNode {
    use crate::lex::SyntaxKind;

    let mut builder = rowan::GreenNodeBuilder::new();
    builder.start_node(target.kind().into());
    let base = crate::column::node_column(target) as isize;
    crate::as_yaml::copy_node_content_reindent(&mut builder, target, -base);
    builder.finish_node();
    let copy = crate::yaml::SyntaxNode::new_root_mut(builder.finish());

    let anchors: Vec<_> = copy
        .descendants_with_tokens()
        .filter_map(|e| e.into_token())
        .filter(|t| t.kind() == SyntaxKind::ANCHOR)
        .collect();
    for anchor in anchors {
        crate::nodes::detach_anchor(&anchor);
    }
    copy
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

    // A sequence created under a key starts empty, so an index past the
    // growth bound is known to fail. Say so before writing anything:
    // creating the key first left `k:\n  \n` behind, a half-built entry
    // that no longer reparses as the sequence it claimed.
    if let (Slot::Key(..), PathSegment::Index(index)) = (&slot, &rest[0]) {
        if *index >= MAX_INDEX_GROWTH {
            return Err(PathError::IndexTooFar {
                at: segment_display(&rest[0]),
                len: 0,
                index: *index,
            });
        }
    }
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
    use crate::debug::assert_cst_ok;

    #[test]
    fn set_path_bounds_sequence_growth() {
        use crate::Document;
        use std::str::FromStr;

        // Filling the gap to an out-of-range index used to be unbounded, so
        // a single path could build an enormous document: the fuzz-found
        // "999.hh.hh.999.9aa" spent ~2s producing 13KB from a 6-byte seed.
        let doc = Document::from_str("m: {}\n").unwrap();
        let err = doc
            .try_set_path("999.hh.hh.999.9aa", "")
            .expect_err("a far index must be refused");
        assert_cst_ok(&doc);
        assert!(
            matches!(err, PathError::IndexTooFar { .. }),
            "unexpected error: {err}"
        );
        // Nothing was written on the way to refusing.
        assert!(doc.to_string().len() < 64, "{:?}", doc.to_string());

        // Growth within the limit still works: writing s[5] on an empty
        // sequence is a normal way to build one.
        let doc = Document::from_str("m: {}\n").unwrap();
        doc.try_set_path("s[5]", "v").expect("small index is fine");
        assert_cst_ok(&doc);
        assert_eq!(doc.try_get_path("s[5]").unwrap().to_string(), "v");

        let doc = Document::from_str("m: {}\n").unwrap();
        assert!(matches!(
            doc.try_set_path(&format!("s[{MAX_INDEX_GROWTH}]"), "v"),
            Err(PathError::IndexTooFar { .. })
        ));
        assert_cst_ok(&doc);
    }

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
        assert_cst_ok(&doc);
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
    fn test_try_parse_path_trailing_backslash() {
        assert_eq!(
            try_parse_path("a\\"),
            Err(PathParseError::TrailingBackslash)
        );
        assert_eq!(
            try_parse_path("a.\\"),
            Err(PathParseError::TrailingBackslash)
        );
    }

    #[test]
    fn test_try_parse_path_empty_segment() {
        for path in [".", "..", ".a", "a.", "a..b", "a.[0]", "a[0].", "a[0]..b"] {
            assert_eq!(
                try_parse_path(path),
                Err(PathParseError::EmptySegment),
                "{path:?}"
            );
        }
    }

    #[test]
    fn test_try_parse_path_missing_separator_after_index() {
        assert_eq!(
            try_parse_path("a[0]b"),
            Err(PathParseError::MissingSeparator)
        );
        assert_eq!(
            try_parse_path("a[0]\\.b"),
            Err(PathParseError::MissingSeparator)
        );
    }

    #[test]
    fn test_try_parse_path_index_must_be_digits() {
        assert_eq!(
            try_parse_path("a[+1]"),
            Err(PathParseError::InvalidIndex("+1".to_string()))
        );
        assert_eq!(
            try_parse_path("a.+1").unwrap(),
            vec![
                PathSegment::Key("a".to_string()),
                PathSegment::Key("+1".to_string())
            ]
        );
    }

    #[test]
    fn test_try_parse_path_index_chains() {
        assert_eq!(
            try_parse_path("[0][1].a").unwrap(),
            vec![
                PathSegment::Index(0),
                PathSegment::Index(1),
                PathSegment::Key("a".to_string())
            ]
        );
        assert_eq!(
            try_parse_path("a[0].1").unwrap(),
            vec![
                PathSegment::Key("a".to_string()),
                PathSegment::Index(0),
                PathSegment::Index(1)
            ]
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
        // An escaped digit is part of a key, not an index.
        assert_eq!(
            try_parse_path("a.\\0").unwrap(),
            vec![
                PathSegment::Key("a".to_string()),
                PathSegment::Key("0".to_string())
            ]
        );
        assert_eq!(
            try_parse_path("\\1.b").unwrap(),
            vec![
                PathSegment::Key("1".to_string()),
                PathSegment::Key("b".to_string())
            ]
        );
    }

    #[test]
    fn test_set_path_escaped_digit_creates_mapping_key() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let doc = Document::from_str("x: 1\n").unwrap();
        doc.try_set_path("a.\\0", "v").unwrap();
        assert_cst_ok(&doc);
        assert_eq!(doc.to_string(), "x: 1\na:\n  '0': v\n");
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
        assert_cst_ok(&doc);

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
        assert_cst_ok(&doc);
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
        assert_cst_ok(&yaml);

        assert_eq!(yaml.to_string(), "name: Bob\nage: 30\n");
    }

    #[test]
    fn test_set_path_new_key() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let yaml = Document::from_str("name: Alice\n").unwrap();

        yaml.try_set_path("age", 30).expect("set_path");
        assert_cst_ok(&yaml);

        assert_eq!(yaml.to_string(), "name: Alice\nage: 30\n");
    }

    #[test]
    fn test_set_path_nested_existing() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let yaml = Document::from_str("server:\n  host: localhost\n  port: 8080\n").unwrap();

        yaml.try_set_path("server.port", 9000).expect("set_path");
        assert_cst_ok(&yaml);

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
        assert_cst_ok(&yaml);

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
        assert_cst_ok(&yaml);

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
        assert_cst_ok(&yaml);
        yaml.try_set_path("app.database.primary.port", 5432)
            .expect("set_path");
        assert_cst_ok(&yaml);

        // Parent was flow-style, so the whole nested chain stays flow.
        assert_eq!(
            yaml.to_string().trim(),
            "app: {database: {primary: {host: db.example.com, port: 5432}}}"
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
        assert_cst_ok(&yaml);
        assert!(result);

        assert_eq!(yaml.to_string(), "name: Alice\n");
    }

    #[test]
    fn test_remove_path_nested() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let yaml = Document::from_str("server:\n  host: localhost\n  port: 8080\n").unwrap();

        let result = yaml.try_remove_path("server.port").is_ok();
        assert_cst_ok(&yaml);
        assert!(result);

        assert_eq!(yaml.to_string(), "server:\n  host: localhost\n");
    }

    #[test]
    fn test_remove_path_missing() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let yaml = Document::from_str("name: Alice\n").unwrap();

        let result = yaml.try_remove_path("missing").is_ok();
        assert_cst_ok(&yaml);
        assert!(!result);

        let result = yaml.try_remove_path("name.nested").is_ok();
        assert_cst_ok(&yaml);
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
        assert_cst_ok(&yaml);
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
        assert_cst_ok(&doc);
        assert_eq!(doc.to_string(), "config:\n  a: 1\n  c: 3\n");

        let doc = Document::from_str("config:\n  a: 1\n  b: 2\n  c: 3\n").unwrap();
        doc.try_remove_path("config.a").unwrap();
        assert_cst_ok(&doc);
        assert_eq!(doc.to_string(), "config:\n  b: 2\n  c: 3\n");

        let doc = Document::from_str("config:\n  a: 1\n  b: 2\n  c: 3\n").unwrap();
        doc.try_remove_path("config.c").unwrap();
        assert_cst_ok(&doc);
        assert_eq!(doc.to_string(), "config:\n  a: 1\n  b: 2\n");

        // Deeper nesting used to compound the over-indentation.
        let doc = Document::from_str("x:\n  y:\n    a: 1\n    b: 2\n").unwrap();
        doc.try_remove_path("x.y.a").unwrap();
        assert_cst_ok(&doc);
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
        assert_cst_ok(&yaml);
        assert_eq!(
            yaml.to_string(),
            "server:\n  host: localhost\n  port: 8080\n"
        );

        // Remove from mapping
        let result = mapping.try_remove_path("server.port").is_ok();
        assert_cst_ok(&yaml);
        assert!(result);

        // Try to remove non-existent path from mapping
        let result_missing = mapping.try_remove_path("nonexistent.path").is_ok();
        assert_cst_ok(&yaml);
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
        assert_cst_ok(&yaml);
        assert!(mapping.try_get_path("a").is_err());
        assert!(mapping.try_get_path("b").is_ok());

        // Removing a missing single-segment key returns false.
        assert!(mapping.try_remove_path("missing").is_err());
        assert_cst_ok(&yaml);
    }

    #[test]
    fn test_set_path_preserves_formatting() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let yaml = Document::from_str("server:\n  host: localhost  # production server\n").unwrap();

        yaml.try_set_path("server.host", "newhost")
            .expect("set_path");
        assert_cst_ok(&yaml);

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
        assert_cst_ok(&yaml);
        yaml.try_set_path("server.port", 8080).expect("set_path");
        assert_cst_ok(&yaml);
        yaml.try_set_path("database.host", "db.local")
            .expect("set_path");
        assert_cst_ok(&yaml);
        yaml.try_set_path("database.port", 5432).expect("set_path");
        assert_cst_ok(&yaml);

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
        assert_cst_ok(&yaml);
        yaml.try_remove_path("database.host").expect("remove_path");
        assert_cst_ok(&yaml);

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
        assert_cst_ok(&doc);
        assert_eq!(doc.to_string(), "base: true\na:\n  b:\n    - {c: value}\n");
    }

    #[test]
    fn test_set_path_into_existing_sequence_by_index() {
        use crate::yaml::Document;
        use std::str::FromStr;
        let doc = Document::from_str("items:\n  - a\n  - b\n").unwrap();
        doc.try_set_path("items[1]", "B").expect("set_path");
        assert_cst_ok(&doc);
        assert_eq!(doc.to_string(), "items:\n  - a\n  - B\n");
    }

    #[test]
    fn test_set_path_grows_sequence_with_nulls() {
        use crate::yaml::Document;
        use std::str::FromStr;
        let doc = Document::from_str("items:\n  - a\n").unwrap();
        doc.try_set_path("items[3]", "z").expect("set_path");
        assert_cst_ok(&doc);
        assert_eq!(
            doc.to_string(),
            "items:\n  - a\n  - null\n  - null\n  - z\n"
        );
    }

    #[test]
    fn test_set_path_does_not_replace_block_scalar() {
        use crate::yaml::Document;
        use std::str::FromStr;
        // Block scalars are strings, never null placeholders.
        for (yaml, path) in [
            ("a: |\n", "a.b"),
            ("a: >-\n  null\n", "a.b"),
            ("items:\n  - |-\n    ~\n", "items[0].b"),
        ] {
            let doc = Document::from_str(yaml).unwrap();
            let err = doc.try_set_path(path, "x").unwrap_err();
            assert_cst_ok(&doc);
            assert!(
                matches!(err, PathError::TypeMismatch { .. }),
                "{:?}: {:?}",
                yaml,
                err
            );
            assert_eq!(doc.to_string(), yaml);
        }
    }

    #[test]
    fn test_set_path_replaces_null_placeholder() {
        use crate::yaml::Document;
        use std::str::FromStr;
        for yaml in ["a:\n", "a: null\n", "a: ~\n"] {
            let doc = Document::from_str(yaml).unwrap();
            doc.try_set_path("a.b", "x").expect("set_path");
            assert_cst_ok(&doc);
            assert_eq!(
                doc.try_get_path("a.b")
                    .unwrap()
                    .as_scalar()
                    .unwrap()
                    .as_string(),
                "x"
            );
        }
    }

    #[test]
    fn test_set_path_index_does_not_replace_existing_mapping() {
        use crate::yaml::Document;
        use std::str::FromStr;
        let doc = Document::from_str("m:\n  a: 1\n  b: 2\n").unwrap();
        doc.try_set_path("m[0]", "z").expect("set_path");
        assert_cst_ok(&doc);
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
        assert_cst_ok(&doc);
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
        assert_cst_ok(&doc);
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
        assert_cst_ok(&doc);
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
        assert_cst_ok(&doc);
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
        assert_cst_ok(&doc);
        assert_eq!(doc.to_string(), original);

        // A tagged sequence is a sequence wearing a tag: descend into it
        // and leave the tag alone (#88).
        let doc = Document::from_str("items: !!seq [1, 2]\n").unwrap();
        doc.try_set_path("items[1]", "x").unwrap();
        assert_cst_ok(&doc);
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
        assert_cst_ok(&doc);
        assert_eq!(doc.to_string(), original);

        // A tagged mapping behaves like the mapping it wraps, so the
        // numeric-on-mapping fallback applies and `[0]` addresses key "0".
        let doc = Document::from_str("top:\n  - !custom\n    a: 1\n").unwrap();
        doc.try_set_path("top[0][0]", "x").unwrap();
        assert_cst_ok(&doc);
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
        assert_cst_ok(&doc);
        doc.try_set_path("config.c", 7).unwrap();
        assert_cst_ok(&doc);
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
        assert_cst_ok(&doc);
        doc.try_set_path("items[2]", 7).unwrap();
        assert_cst_ok(&doc);
        assert_eq!(doc.to_string(), "items: !!seq [9, 2, 7]\n");
    }

    #[test]
    fn test_path_descends_through_nested_tags() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let doc = Document::from_str("a: !outer\n  b: !inner\n    c: 1\n").unwrap();
        doc.try_set_path("a.b.c", 2).unwrap();
        assert_cst_ok(&doc);
        assert_eq!(doc.to_string(), "a: !outer\n  b: !inner\n    c: 2\n");

        let doc = Document::from_str("l: !t\n  - x: 1\n").unwrap();
        doc.try_set_path("l[0].x", 9).unwrap();
        assert_cst_ok(&doc);
        assert_eq!(doc.to_string(), "l: !t\n  - x: 9\n");
    }

    #[test]
    fn test_remove_path_in_tagged_mapping_keeps_tag() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let doc = Document::from_str("config: !custom\n  a: 1\n  b: 2\n").unwrap();
        let removed = doc.try_remove_path("config.b").unwrap();
        assert_cst_ok(&doc);
        assert_eq!(removed.as_scalar().unwrap().as_string(), "2");
        assert_eq!(doc.to_string(), "config: !custom\n  a: 1\n");

        // Draining the last entry collapses to the flow-empty form, as it
        // does for an untagged mapping, but keeps the tag.
        doc.try_remove_path("config.a").unwrap();
        assert_cst_ok(&doc);
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
        assert_cst_ok(&doc);
        assert_eq!(doc.to_string(), original);
    }

    #[test]
    fn test_set_path_does_not_replace_null_lookalike_strings() {
        use crate::yaml::Document;
        use std::str::FromStr;

        for original in [
            "a: \"null\"\n",
            "a: 'null'\n",
            "a: ''\n",
            "a: \"~\"\n",
            "a: nUlL\n",
        ] {
            let doc = Document::from_str(original).unwrap();
            assert_eq!(
                doc.try_set_path("a.b", "v"),
                Err(PathError::TypeMismatch { at: "a".into() }),
                "{original:?}"
            );
            assert_eq!(doc.to_string(), original);
        }

        for original in ["items:\n  - 'null'\n", "items:\n  - nUlL\n"] {
            let doc = Document::from_str(original).unwrap();
            assert_eq!(
                doc.try_set_path("items[0].b", "v"),
                Err(PathError::TypeMismatch { at: "[0]".into() }),
                "{original:?}"
            );
            assert_eq!(doc.to_string(), original);
        }
    }

    #[test]
    fn test_set_path_replaces_null_placeholders() {
        use crate::yaml::Document;
        use std::str::FromStr;

        for null in ["null", "Null", "NULL", "~", ""] {
            let doc = Document::from_str(&format!("a: {null}\n")).unwrap();
            doc.try_set_path("a.b", "v").unwrap();
            assert_eq!(doc.to_string(), "a:\n  b: v\n", "{null:?}");
        }
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
                at: "service".to_string(),
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
                at: "service".to_string(),
                alias: "missing".to_string()
            })
        );
    }

    #[test]
    fn test_path_alias_uses_preceding_anchor_definition() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let original = "a: &x {k: 1}\nb: *x\nc: &x {k: 2}\n";
        let doc = Document::from_str(original).unwrap();
        assert_eq!(
            doc.try_get_path("b.k")
                .unwrap()
                .as_scalar()
                .map(|s| s.as_string()),
            Some("1".to_string())
        );
        doc.try_set_path_with("b.k", 9, AliasPolicy::Follow)
            .unwrap();
        assert_cst_ok(&doc);
        assert_eq!(doc.to_string(), "a: &x {k: 9}\nb: *x\nc: &x {k: 2}\n");
    }

    #[test]
    fn test_path_alias_before_anchor_is_undefined() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let doc = Document::from_str("b: *x\na: &x {k: 1}\n").unwrap();
        assert_eq!(
            doc.try_get_path("b.k"),
            Err(PathError::UndefinedAlias {
                at: "b".to_string(),
                alias: "x".to_string()
            })
        );
        assert_eq!(
            doc.try_set_path_with("b.k", 9, AliasPolicy::Follow),
            Err(PathError::UndefinedAlias {
                at: "b".to_string(),
                alias: "x".to_string()
            })
        );
        assert_cst_ok(&doc);
    }

    #[test]
    fn test_get_path_alias_to_anchor_in_earlier_document() {
        use crate::yaml::YamlFile;
        use std::str::FromStr;

        // Anchors are scoped to their document (YAML 1.2 section 3.2.2.2).
        let file = YamlFile::from_str("a: &x\n  k: 1\n---\nb: *x\n").unwrap();
        let second = file.documents().nth(1).unwrap();
        assert_eq!(
            second.try_get_path("b.k"),
            Err(PathError::UndefinedAlias {
                at: "b".to_string(),
                alias: "x".to_string()
            })
        );
    }

    #[test]
    fn test_set_path_alias_to_anchor_in_earlier_document() {
        use crate::yaml::YamlFile;
        use std::str::FromStr;

        let src = "a: &x\n  k: 1\n---\nb: *x\n";
        let file = YamlFile::from_str(src).unwrap();
        let second = file.documents().nth(1).unwrap();
        for policy in [AliasPolicy::Follow, AliasPolicy::Expand] {
            assert_eq!(
                second.try_set_path_with("b.k", 2, policy),
                Err(PathError::UndefinedAlias {
                    at: "b".to_string(),
                    alias: "x".to_string()
                })
            );
            assert_cst_ok(&file);
        }
        assert_eq!(file.to_string(), src.to_string());
    }

    #[test]
    fn test_get_path_merge_key_from_earlier_document() {
        use crate::yaml::YamlFile;
        use std::str::FromStr;

        let file = YamlFile::from_str("a: &x\n  k: 1\n---\nb:\n  <<: *x\n").unwrap();
        let second = file.documents().nth(1).unwrap();
        assert_eq!(
            second.try_get_path("b.k"),
            Err(PathError::UndefinedAlias {
                at: "k".to_string(),
                alias: "x".to_string()
            })
        );
    }

    #[test]
    fn test_get_path_merge_key_not_a_mapping() {
        use std::str::FromStr;

        let doc = crate::Document::from_str("a: &a 5\nm: {<<: *a, j: 1}\n").unwrap();
        assert_eq!(
            doc.try_get_path("m.k"),
            Err(PathError::InvalidMergeOperand {
                at: "k".to_string(),
                operand: "*a".to_string()
            })
        );
        // A direct key does not need the merge.
        assert_eq!(doc.try_get_path("m.j").unwrap().to_string(), "1");
    }

    #[test]
    fn test_get_path_alias_resolves_within_later_document() {
        use crate::yaml::YamlFile;
        use std::str::FromStr;

        // The same anchor name in two documents: each alias sees its own.
        let file = YamlFile::from_str("a: &x\n  k: 1\nb: *x\n---\na: &x\n  k: 2\nb: *x\n").unwrap();
        let docs: Vec<_> = file.documents().collect();
        assert_eq!(docs[0].try_get_path("b.k").unwrap().to_i64(), Some(1));
        assert_eq!(docs[1].try_get_path("b.k").unwrap().to_i64(), Some(2));
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
        assert_cst_ok(&doc);
        assert_eq!(doc.to_string(), ALIASED.to_string());
    }

    #[test]
    fn test_set_path_follow_writes_through_to_anchor() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let doc = Document::from_str(ALIASED).unwrap();
        doc.try_set_path_with("service.timeout", 60, AliasPolicy::Follow)
            .unwrap();
        assert_cst_ok(&doc);
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
        assert_cst_ok(&doc);
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
        assert_cst_ok(&doc);
        assert_eq!(
            doc.to_string(),
            "shared: &shared\n  timeout: 30\na:\n  timeout: 60\nb: *shared\n".to_string()
        );
    }

    #[test]
    fn test_set_path_expand_multi_entry_indentation() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let doc = Document::from_str("a: &x\n  k: 1\n  j: 2\nb: *x\n").unwrap();
        doc.try_set_path_with("b.j", 3, AliasPolicy::Expand)
            .unwrap();
        assert_cst_ok(&doc);
        assert_eq!(
            doc.to_string(),
            "a: &x\n  k: 1\n  j: 2\nb:\n  k: 1\n  j: 3\n"
        );

        let doc = Document::from_str("a: &x\n  - 1\n  - 2\nb: *x\n").unwrap();
        doc.try_set_path_with("b[1]", 3, AliasPolicy::Expand)
            .unwrap();
        assert_cst_ok(&doc);
        assert_eq!(doc.to_string(), "a: &x\n  - 1\n  - 2\nb:\n  - 1\n  - 3\n");
    }

    #[test]
    fn test_set_path_expand_drops_nested_anchors() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let doc =
            Document::from_str("a: &x\n  k: &y 1\n  m: &z\n    n: 2\nb: *x\nc: *y\n").unwrap();
        doc.try_set_path_with("b.j", 3, AliasPolicy::Expand)
            .unwrap();
        assert_cst_ok(&doc);
        assert_eq!(
            doc.to_string(),
            "a: &x\n  k: &y 1\n  m: &z\n    n: 2\nb:\n  k: 1\n  m:\n    n: 2\n  j: 3\nc: *y\n"
        );

        let doc = Document::from_str("a: &x\n  - &y [&z 1, 2]\n  - 3\nb: *x\n").unwrap();
        doc.try_set_path_with("b[1]", 4, AliasPolicy::Expand)
            .unwrap();
        assert_cst_ok(&doc);
        assert_eq!(
            doc.to_string(),
            "a: &x\n  - &y [&z 1, 2]\n  - 3\nb:\n  - [1, 2]\n  - 4\n"
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
        assert_cst_ok(&doc);
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
        assert_cst_ok(&doc);
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
                at: "service".to_string(),
                alias: "shared".to_string()
            })
        );
        assert_cst_ok(&doc);
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
        assert_cst_ok(&doc);
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
    fn test_path_alias_to_anchored_alias() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let src = "x: &x {k: 1}\nb: &b *x\nc: *b\n";
        let doc = Document::from_str(src).unwrap();
        assert_eq!(doc.try_get_path("c.k").unwrap().to_string(), "1");

        doc.try_set_path_with("c.k", 2, AliasPolicy::Follow)
            .unwrap();
        assert_cst_ok(&doc);
        assert_eq!(doc.to_string(), "x: &x {k: 2}\nb: &b *x\nc: *b\n");

        let doc = Document::from_str(src).unwrap();
        doc.try_set_path_with("c.k", 2, AliasPolicy::Expand)
            .unwrap();
        assert_cst_ok(&doc);
        assert_eq!(doc.to_string(), "x: &x {k: 1}\nb: &b *x\nc: {k: 2}\n");

        let doc = Document::from_str("b: &b *b\nc: *b\n").unwrap();
        assert_eq!(
            doc.try_get_path("c.k"),
            Err(PathError::UndefinedAlias {
                at: "c".to_string(),
                alias: "b".to_string()
            })
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

    #[test]
    fn test_path_reaches_non_string_scalar_keys() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let yaml = "m:\n  1: one\n  true: t\n  null: n\n  0x10: hex\n";
        let doc = Document::from_str(yaml).unwrap();
        let get = |path: &str| {
            doc.try_get_path(path)
                .map(|n| n.as_scalar().unwrap().as_string())
        };
        assert_eq!(get("m.1"), Ok("one".to_string()));
        assert_eq!(get("m[1]"), Ok("one".to_string()));
        assert_eq!(get("m.true"), Ok("t".to_string()));
        assert_eq!(get("m.null"), Ok("n".to_string()));
        assert_eq!(get("m.0x10"), Ok("hex".to_string()));
        // Matching is on the key as written, not its resolved value.
        assert_eq!(
            get("m.16"),
            Err(PathError::NotFound {
                at: "[16]".to_string()
            })
        );

        doc.try_set_path("m.1", "uno").unwrap();
        assert_cst_ok(&doc);
        assert_eq!(
            doc.try_remove_path("m.true").unwrap().to_string(),
            "t".to_string()
        );
        assert_cst_ok(&doc);
        assert_eq!(doc.to_string(), "m:\n  1: uno\n  null: n\n  0x10: hex\n");

        let doc = Document::from_str("m:\n  '1': s\n").unwrap();
        assert_eq!(
            doc.try_get_path("m.1").unwrap().to_string(),
            "s".to_string()
        );
    }

    #[test]
    fn test_path_reaches_non_string_key_through_merge() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let doc = Document::from_str("base: &b\n  1: one\nm:\n  <<: *b\n").unwrap();
        assert_eq!(doc.try_get_path("m.1").unwrap().to_string(), "one");
    }

    #[test]
    fn test_set_path_expand_block_mapping_in_sequence() {
        use crate::yaml::Document;
        use rowan::ast::AstNode;
        use std::str::FromStr;

        let doc = Document::from_str("s:\n  - &x\n    k: 1\n  - *x\n").unwrap();
        doc.try_set_path_with("s[1].k", 9, AliasPolicy::Expand)
            .unwrap();
        crate::debug::validate_tree(doc.syntax()).unwrap();
        crate::debug::roundtrip_ok(doc.syntax()).unwrap();
        assert_eq!(doc.to_string(), "s:\n  - &x\n    k: 1\n  - k: 9\n");
    }

    #[test]
    fn test_expand_copy_keeps_tag_after_anchor() {
        use crate::yaml::Document;
        use std::str::FromStr;

        for src in [
            "a: !!map &x\n  k: 1\nb: *x\n",
            "a: &x !!map\n  k: 1\nb: *x\n",
        ] {
            let doc = Document::from_str(src).unwrap();
            let Some(crate::as_yaml::YamlNode::Alias(alias)) = doc.as_mapping().unwrap().get("b")
            else {
                panic!("b is not an alias in {src:?}");
            };
            let segment = PathSegment::Key("b".to_string());
            let copy = resolve_alias(&alias, &segment, AliasPolicy::Expand).unwrap();
            assert_eq!(copy.to_string(), "!!map\nk: 1\n", "{src:?}");
        }
    }

    fn set_and_check(
        src: &str,
        path: &str,
        policy: AliasPolicy,
    ) -> (Result<(), PathError>, String) {
        use crate::yaml::Document;
        use rowan::ast::AstNode;
        use std::str::FromStr;

        let doc = Document::from_str(src).unwrap();
        let result = doc.try_set_path_with(path, 9, policy);
        crate::debug::validate_tree(doc.syntax()).unwrap();
        crate::debug::roundtrip_ok(doc.syntax()).unwrap();
        (result, doc.to_string())
    }

    #[test]
    fn test_set_path_through_alias_to_tagged_collection() {
        let src = "a: &x !!map {k: 1}\nb: *x\n";
        assert_eq!(
            set_and_check(src, "b.j", AliasPolicy::Expand),
            (
                Ok(()),
                "a: &x !!map {k: 1}\nb: !!map {k: 1, j: 9}\n".to_string()
            )
        );
        assert_eq!(
            set_and_check(src, "b.j", AliasPolicy::Follow),
            (Ok(()), "a: &x !!map {k: 1, j: 9}\nb: *x\n".to_string())
        );

        let src = "a: &x !!seq [1]\nb: *x\n";
        assert_eq!(
            set_and_check(src, "b[0]", AliasPolicy::Expand),
            (Ok(()), "a: &x !!seq [1]\nb: !!seq [9]\n".to_string())
        );

        let src = "s:\n  - &x !!map {k: 1}\n  - *x\n";
        assert_eq!(
            set_and_check(src, "s[1].j", AliasPolicy::Expand),
            (
                Ok(()),
                "s:\n  - &x !!map {k: 1}\n  - !!map {k: 1, j: 9}\n".to_string()
            )
        );
    }

    #[test]
    fn test_set_path_expand_failure_leaves_document_alone() {
        let mismatch = |at: &str| Err(PathError::TypeMismatch { at: at.to_string() });
        for (src, path, err) in [
            ("a: &x 1\nb: *x\n", "b.k", mismatch("b")),
            ("a: &x 1\nb: *x\n", "b[0]", mismatch("b")),
            ("s:\n  - &x 1\n  - *x\n", "s[1].k", mismatch("[1]")),
            ("s:\n  - &x 1\n  - *x\n", "s[1][0]", mismatch("[1]")),
            ("a: &x {k: 1}\nb: *x\n", "b.k.j", mismatch("k")),
            ("s:\n  - &x {k: 1}\n  - *x\n", "s[1].k.j", mismatch("k")),
            (
                "a: &x {k: 1, m: *y}\nb: *x\n",
                "b.m.j",
                Err(PathError::UndefinedAlias {
                    at: "m".to_string(),
                    alias: "y".to_string(),
                }),
            ),
        ] {
            assert_eq!(
                set_and_check(src, path, AliasPolicy::Expand),
                (err, src.to_string()),
                "{path} on {src:?}"
            );
        }
    }

    #[test]
    fn test_errors_name_the_segment_holding_the_value() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let doc = Document::from_str("x: &x {a: 1}\nfoo: 1\ns: [a]\nb: *x\nu: *nope\n").unwrap();
        let mismatch = |at: &str| PathError::TypeMismatch { at: at.to_string() };
        for (path, expected) in [
            ("foo.bar", mismatch("foo")),
            ("foo.bar.baz", mismatch("foo")),
            ("foo[0]", mismatch("foo")),
            ("s.k", mismatch("s")),
            ("s[0].k", mismatch("[0]")),
            ("s[0][0]", mismatch("[0]")),
            (
                "b.a",
                PathError::AliasRefused {
                    at: "b".to_string(),
                    alias: "x".to_string(),
                },
            ),
            (
                "u.a",
                PathError::AliasRefused {
                    at: "u".to_string(),
                    alias: "nope".to_string(),
                },
            ),
        ] {
            assert_eq!(
                doc.try_get_path_with(path, AliasPolicy::Refuse),
                Err(expected.clone()),
                "get {path}"
            );
            assert_eq!(
                doc.try_remove_path(path).map(|_| ()),
                Err(expected.clone()),
                "remove {path}"
            );
            assert_eq!(doc.try_set_path(path, 1), Err(expected), "set {path}");
        }
        let undefined = PathError::UndefinedAlias {
            at: "u".to_string(),
            alias: "nope".to_string(),
        };
        assert_eq!(
            doc.try_get_path_with("u.a", AliasPolicy::Follow),
            Err(undefined.clone())
        );
        assert_eq!(
            doc.try_remove_path_with("u.a", AliasPolicy::Follow)
                .map(|_| ()),
            Err(undefined.clone())
        );
        assert_eq!(
            doc.try_set_path_with("u.a", 1, AliasPolicy::Follow),
            Err(undefined)
        );
        // At the root no segment holds the value, so the one applied to it
        // is named.
        let doc = Document::from_str("5\n").unwrap();
        assert_eq!(doc.try_get_path("a"), Err(mismatch("a")));
    }

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

    #[test]
    fn test_document_root_sequence_and_scalar() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let doc = Document::from_str("- a\n- b\n").unwrap();
        assert_eq!(doc.try_get_path("[1]").unwrap().to_string(), "b");
        assert_eq!(
            doc.try_remove_path("[1]").map(|_| ()),
            Err(PathError::TypeMismatch { at: "[1]".into() })
        );
        assert_eq!(doc.try_set_path("[1]", "x"), Err(PathError::NoRoot));
        assert_eq!(doc.to_string(), "- a\n- b\n");

        let doc = Document::from_str("5\n").unwrap();
        assert_eq!(
            doc.try_get_path("a"),
            Err(PathError::TypeMismatch { at: "a".into() })
        );
        assert_eq!(doc.try_remove_path("a").map(|_| ()), Err(PathError::NoRoot));

        let doc = Document::new();
        assert_eq!(doc.try_get_path("a"), Err(PathError::NoRoot));
        assert_eq!(doc.try_remove_path("a").map(|_| ()), Err(PathError::NoRoot));
    }

    #[test]
    fn test_set_path_creates_sequences_and_mappings_in_sequences() {
        let refuse = AliasPolicy::Refuse;
        for (src, path, expected) in [
            ("{a: 1}", "b[0]", "{a: 1, b: [9]}"),
            ("{a: 1}", "b.c", "{a: 1, b: {c: 9}}"),
            ("s: [[a]]\n", "s[0][1]", "s: [[a, 9]]\n"),
            ("s: [{a: 1}]\n", "s[0][1]", "s: [{a: 1, '1': 9}]\n"),
            ("s:\n- a\n", "s[2].k", "s:\n- a\n- null\n- {k: 9}\n"),
            ("s:\n- a\n", "s[2][0]", "s:\n- a\n- null\n- [9]\n"),
            ("s:\n- a\n", "s[1][0][0]", "s:\n- a\n- [[9]]\n"),
            ("s:\n- a\n- ~\n", "s[1].k", "s:\n- a\n- {k: 9}\n"),
            (
                "s:\n- a\n- {b: 1}\n",
                "s[1][0]",
                "s:\n- a\n- {b: 1, '0': 9}\n",
            ),
            ("a: ~\n", "a[0]", "a:\n  - 9\n"),
            ("a:\n  b: 1\n", "a[0]", "a:\n  b: 1\n  '0': 9\n"),
        ] {
            assert_eq!(
                set_and_check(src, path, refuse),
                (Ok(()), expected.to_string()),
                "{path} on {src:?}"
            );
        }
    }

    #[test]
    fn test_set_path_refusals_leave_document_alone() {
        let mismatch = |at: &str| Err(PathError::TypeMismatch { at: at.to_string() });
        for (src, path, err) in [
            ("s: [a]\n", "s.k", mismatch("s")),
            ("s: [a]\n", "s[0].k", mismatch("[0]")),
            ("s: [!t x]\n", "s[0][0]", mismatch("[0]")),
            ("s: [!t x]\n", "s[0].k", mismatch("[0]")),
            ("s:\n- a\n- [b]\n", "s[1].k", mismatch("[1]")),
            ("a: [x]\n", "a[0][0]", mismatch("[0]")),
            ("a: !t x\n", "a.b", mismatch("a")),
            ("a: !t x\n", "a[0]", mismatch("a")),
            (
                "a: 1\n",
                "b[900000]",
                Err(PathError::IndexTooFar {
                    at: "[900000]".to_string(),
                    len: 0,
                    index: 900000,
                }),
            ),
            (
                "s: [a]\n",
                "s[257]",
                Err(PathError::IndexTooFar {
                    at: "[257]".to_string(),
                    len: 1,
                    index: 257,
                }),
            ),
        ] {
            for policy in [AliasPolicy::Refuse, AliasPolicy::Expand] {
                assert_eq!(
                    set_and_check(src, path, policy),
                    (err.clone(), src.to_string()),
                    "{path} on {src:?}"
                );
            }
        }
    }

    #[test]
    fn test_set_path_through_undefined_alias_in_sequence() {
        assert_eq!(
            set_and_check("s:\n- *nope\n", "s[0].a", AliasPolicy::Expand),
            (
                Err(PathError::UndefinedAlias {
                    at: "[0]".to_string(),
                    alias: "nope".to_string(),
                }),
                "s:\n- *nope\n".to_string()
            )
        );
    }

    #[test]
    fn test_set_path_through_alias_in_sequence() {
        let src = "x: &x {a: 1}\ns: [*x]\n";
        assert_eq!(
            set_and_check(src, "s[0].a", AliasPolicy::Refuse),
            (
                Err(PathError::AliasRefused {
                    at: "[0]".to_string(),
                    alias: "x".to_string()
                }),
                src.to_string()
            )
        );
        assert_eq!(
            set_and_check(src, "s[0].a", AliasPolicy::Follow),
            (Ok(()), "x: &x {a: 9}\ns: [*x]\n".to_string())
        );
        assert_eq!(
            set_and_check(src, "s[0].a", AliasPolicy::Expand),
            (Ok(()), "x: &x {a: 1}\ns: [{a: 9}]\n".to_string())
        );
        assert_eq!(
            set_and_check("x: &x [1]\ns:\n- *x\n", "s[0][1]", AliasPolicy::Expand),
            (Ok(()), "x: &x [1]\ns:\n- [1, 9]\n".to_string())
        );
    }

    #[test]
    #[allow(deprecated)]
    fn test_deprecated_wrappers() {
        use crate::yaml::Document;
        use std::str::FromStr;

        assert_eq!(
            parse_path("a[1].b"),
            vec![
                PathSegment::Key("a".to_string()),
                PathSegment::Index(1),
                PathSegment::Key("b".to_string()),
            ]
        );
        let doc = Document::from_str("a:\n  b: 1\n").unwrap();
        assert_eq!(doc.get_path("a.b").unwrap().to_string(), "1");
        assert!(doc.get_path("a.c").is_none());
        doc.set_path("a.c", 2);
        doc.set_path("a.b.x", 3);
        assert_eq!(doc.to_string(), "a:\n  b: 1\n  c: 2\n");
        assert!(doc.remove_path("a.b"));
        assert!(!doc.remove_path("a.b"));
        assert_eq!(doc.to_string(), "a:\n  c: 2\n");
    }

    #[test]
    fn test_set_path_growth_bound_is_exact() {
        use crate::yaml::Document;
        use std::str::FromStr;

        let doc = Document::from_str("s: [a, b]\n").unwrap();
        assert_eq!(
            doc.try_set_path("s[258]", 1),
            Err(PathError::IndexTooFar {
                at: "[258]".to_string(),
                len: 2,
                index: 258,
            })
        );
        assert_eq!(doc.try_set_path("s[257]", 1), Ok(()));
        let s = doc.as_mapping().unwrap().get_sequence("s").unwrap();
        assert_eq!(s.len(), 258);
        assert_eq!(s.get(257).unwrap().to_string(), "1");
    }
}
