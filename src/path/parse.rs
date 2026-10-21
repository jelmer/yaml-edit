//! Parsing path strings into segments.

use super::{PathError, PathSegment};

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
pub(super) fn path_segments_required(path: &str) -> Result<Vec<PathSegment>, PathError> {
    let segments = try_parse_path(path)?;
    if segments.is_empty() {
        Err(PathError::EmptyPath)
    } else {
        Ok(segments)
    }
}

/// Format a segment for use in a `PathError::TypeMismatch` / `NotFound`
/// message. Mirrors the input syntax the user would type.
pub(super) fn segment_display(segment: &PathSegment) -> String {
    match segment {
        PathSegment::Key(key) => key.clone(),
        PathSegment::Index(index) => format!("[{index}]"),
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
}
