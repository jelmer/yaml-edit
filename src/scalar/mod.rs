//! Scalar value wrapper with proper escaping and style support.

#[cfg(feature = "base64")]
mod binary;
mod decimal;
mod quoting;
mod render;
mod resolve;

pub(crate) use decimal::{digits_to_decimal, places_to_decimal, signed_digits_to_decimal};

use crate::as_yaml::FlowQuoting;
use std::fmt;

#[cfg(feature = "base64")]
use binary::base64_decode;

/// Style of scalar representation in YAML
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScalarStyle {
    /// Plain scalar (no quotes)
    Plain,
    /// Single-quoted scalar
    SingleQuoted,
    /// Double-quoted scalar
    DoubleQuoted,
    /// Literal scalar (|)
    Literal,
    /// Folded scalar (>)
    Folded,
}

/// The five scalar types recognised by the YAML 1.2 core schema.
///
/// Returned by [`ScalarValue::classify_plain`], which is the single
/// source of truth for plain-scalar tag resolution. This is a strict
/// subset of [`ScalarType`], which additionally covers yaml-edit's
/// out-of-band types (`Timestamp`, `Binary`, `Regex`) used when
/// constructing new values.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CoreScalarType {
    /// `!!null`
    Null,
    /// `!!bool`
    Boolean,
    /// `!!int`
    Integer,
    /// `!!float`
    Float,
    /// `!!str` (the fallback when no other tag matches)
    String,
}

impl From<CoreScalarType> for ScalarType {
    fn from(t: CoreScalarType) -> Self {
        match t {
            CoreScalarType::Null => ScalarType::Null,
            CoreScalarType::Boolean => ScalarType::Boolean,
            CoreScalarType::Integer => ScalarType::Integer,
            CoreScalarType::Float => ScalarType::Float,
            CoreScalarType::String => ScalarType::String,
        }
    }
}

/// The c-indicator characters (YAML 1.2 5.3), which cannot start a plain
/// scalar, except for `-`, `?` and `:` followed by a safe character.
pub(crate) const INDICATORS: &[char] = &[
    '-', '?', ':', ',', '[', ']', '{', '}', '#', '&', '*', '!', '|', '>', '\'', '"', '%', '@', '`',
];

/// Type of a scalar value
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ScalarType {
    /// String value
    String,
    /// Integer value
    Integer,
    /// Float value
    Float,
    /// Boolean value
    Boolean,
    /// Null value
    Null,
    /// Binary data (base64 encoded)
    #[cfg(feature = "base64")]
    Binary,
    /// Timestamp value
    Timestamp,
    /// Regular expression
    Regex,
}

/// A scalar value with metadata about its style and content
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ScalarValue {
    /// The actual value
    value: String,
    /// The style to use when rendering
    style: ScalarStyle,
    /// The type of the scalar
    scalar_type: ScalarType,
    /// The explicit tag written on the node, e.g. `!!float` or `!local`
    tag: Option<String>,
}

impl ScalarValue {
    /// Create a scalar value explicitly treating it as a string (no type auto-detection)
    ///
    /// This method always creates a `String` type scalar. The value will be properly
    /// quoted if needed when rendering to YAML, but no type detection is performed.
    ///
    /// # Examples
    ///
    /// ```
    /// use yaml_edit::ScalarValue;
    ///
    /// let scalar = ScalarValue::string("123");
    /// assert_eq!(scalar.scalar_type(), yaml_edit::ScalarType::String);
    /// // Renders with quotes to distinguish from integer
    /// assert_eq!(scalar.to_yaml_string(), "'123'");
    ///
    /// let scalar = ScalarValue::string("true");
    /// assert_eq!(scalar.scalar_type(), yaml_edit::ScalarType::String);
    /// // Renders with quotes to distinguish from boolean
    /// assert_eq!(scalar.to_yaml_string(), "'true'");
    ///
    /// let scalar = ScalarValue::string("hello");
    /// assert_eq!(scalar.scalar_type(), yaml_edit::ScalarType::String);
    /// // Plain strings don't need quotes
    /// assert_eq!(scalar.to_yaml_string(), "hello");
    /// ```
    ///
    /// For YAML-style type detection (parsing "123" as Integer, "true" as Boolean),
    /// use [`ScalarValue::parse()`] instead.
    pub fn string(value: impl Into<String>) -> Self {
        let value = value.into();
        let style = Self::detect_style(&value);
        // Detect the type - default to String for user-provided values
        let scalar_type = ScalarType::String;
        Self {
            value,
            style,
            scalar_type,
            tag: None,
        }
    }

    /// Create a new scalar with a specific style
    pub fn with_style(value: impl Into<String>, style: ScalarStyle) -> Self {
        Self {
            value: value.into(),
            style,
            scalar_type: ScalarType::String,
            tag: None,
        }
    }

    /// Create a plain scalar
    pub fn plain(value: impl Into<String>) -> Self {
        Self::with_style(value, ScalarStyle::Plain)
    }

    /// Create a single-quoted scalar
    pub fn single_quoted(value: impl Into<String>) -> Self {
        Self::with_style(value, ScalarStyle::SingleQuoted)
    }

    /// Create a double-quoted scalar
    pub fn double_quoted(value: impl Into<String>) -> Self {
        Self::with_style(value, ScalarStyle::DoubleQuoted)
    }

    /// Create a literal scalar
    pub fn literal(value: impl Into<String>) -> Self {
        Self::with_style(value, ScalarStyle::Literal)
    }

    /// Create a folded scalar
    pub fn folded(value: impl Into<String>) -> Self {
        Self::with_style(value, ScalarStyle::Folded)
    }

    /// Create a null scalar
    pub fn null() -> Self {
        Self {
            value: "null".to_string(),
            style: ScalarStyle::Plain,
            scalar_type: ScalarType::Null,
            tag: None,
        }
    }

    /// Create a timestamp scalar
    pub fn timestamp(value: impl Into<String>) -> Self {
        Self {
            value: value.into(),
            style: ScalarStyle::Plain,
            scalar_type: ScalarType::Timestamp,
            tag: None,
        }
    }

    /// Create a regex scalar
    pub fn regex(pattern: impl Into<String>) -> Self {
        Self {
            value: pattern.into(),
            style: ScalarStyle::Plain,
            scalar_type: ScalarType::Regex,
            tag: None,
        }
    }

    /// Return this scalar with an explicit tag, such as `!!str` or `!local`.
    ///
    /// The tag is rendered in front of the content. It does not change the
    /// scalar's [`ScalarType`].
    ///
    /// ```
    /// use yaml_edit::ScalarValue;
    ///
    /// let scalar = ScalarValue::from(1.0).with_tag("!!float");
    /// assert_eq!(scalar.tag(), Some("!!float"));
    /// assert_eq!(scalar.to_yaml_string(), "!!float 1.0");
    /// ```
    pub fn with_tag(mut self, tag: impl Into<String>) -> Self {
        self.tag = Some(tag.into());
        self
    }

    /// The explicit tag on this scalar, if any.
    pub fn tag(&self) -> Option<&str> {
        self.tag.as_deref()
    }

    /// Get the raw value
    pub fn value(&self) -> &str {
        &self.value
    }

    /// Get the style
    pub fn style(&self) -> ScalarStyle {
        self.style
    }

    /// Get the scalar type
    pub fn scalar_type(&self) -> ScalarType {
        self.scalar_type
    }

    /// Try to parse this scalar as an `i64`.
    ///
    /// Returns `None` if the scalar type is not `Integer` or the value does
    /// not fit in an `i64`. Integers are read as the YAML 1.2 core schema
    /// defines them, so `0755` is seven hundred and fifty five; only
    /// `0o755` is octal.
    pub fn to_i64(&self) -> Option<i64> {
        if self.scalar_type == ScalarType::Integer {
            Self::parse_integer(self.value.trim())
        } else {
            None
        }
    }

    /// Same as [`to_i64`](Self::to_i64), which now follows YAML 1.2.
    #[deprecated(since = "0.3.3", note = "to_i64 follows YAML 1.2; use it instead")]
    pub fn to_i64_yaml_1_2(&self) -> Option<i64> {
        self.to_i64()
    }

    /// Try to parse this scalar as an `f64`.
    ///
    /// Returns `None` if the scalar type is not `Float`.
    pub fn to_f64(&self) -> Option<f64> {
        if self.scalar_type == ScalarType::Float {
            Self::parse_float(self.value.trim())
        } else {
            None
        }
    }

    /// Try to parse this scalar as a `bool`.
    ///
    /// Returns `None` if the scalar type is not `Boolean`.
    /// Recognizes the YAML 1.2 core schema spellings: `true`, `True`,
    /// `TRUE`, `false`, `False` and `FALSE`.
    pub fn to_bool(&self) -> Option<bool> {
        if self.scalar_type == ScalarType::Boolean {
            Self::parse_bool(self.value.trim())
        } else {
            None
        }
    }

    /// Check if this is a timestamp scalar
    pub fn is_timestamp(&self) -> bool {
        self.scalar_type == ScalarType::Timestamp
    }

    /// Check if this is a regex scalar
    pub fn is_regex(&self) -> bool {
        self.scalar_type == ScalarType::Regex
    }

    /// Compile and return a Regex object if this is a regex scalar
    ///
    /// This method is only available when the `regex` feature is enabled.
    /// Returns None if this is not a regex scalar or if the pattern is invalid.
    ///
    /// # Example
    /// ```
    /// # #[cfg(feature = "regex")]
    /// # {
    /// use yaml_edit::ScalarValue;
    ///
    /// let scalar = ScalarValue::regex(r"\d{3}-\d{4}");
    /// let regex = scalar.as_regex().unwrap();
    /// assert!(regex.is_match("555-1234"));
    /// # }
    /// ```
    #[cfg(feature = "regex")]
    pub fn as_regex(&self) -> Option<regex::Regex> {
        if self.scalar_type == ScalarType::Regex {
            regex::Regex::new(&self.value).ok()
        } else {
            None
        }
    }

    /// Try to compile this scalar as a regex, regardless of its type
    ///
    /// This method is only available when the `regex` feature is enabled.
    /// This will attempt to compile the scalar value as a regex pattern,
    /// even if it's not marked with the !!regex tag.
    ///
    /// # Example
    /// ```
    /// # #[cfg(feature = "regex")]
    /// # {
    /// use yaml_edit::ScalarValue;
    ///
    /// let scalar = ScalarValue::string(r"\d+");  // Plain string scalar
    /// let regex = scalar.try_as_regex().unwrap();
    /// assert!(regex.is_match("123"));
    /// # }
    /// ```
    #[cfg(feature = "regex")]
    pub fn try_as_regex(&self) -> Result<regex::Regex, regex::Error> {
        regex::Regex::new(&self.value)
    }

    /// Try to coerce this scalar to the specified type
    pub fn coerce_to_type(&self, target_type: ScalarType) -> Option<ScalarValue> {
        if self.scalar_type == target_type {
            return Some(self.clone());
        }

        match target_type {
            ScalarType::String => Some(ScalarValue {
                value: self.value.clone(),
                style: ScalarStyle::Plain,
                scalar_type: ScalarType::String,
                tag: None,
            }),
            ScalarType::Integer => Self::parse_integer(self.value.trim()).map(ScalarValue::from),
            ScalarType::Float => Self::parse_float(self.value.trim()).map(ScalarValue::from),
            ScalarType::Boolean => match self.value.to_lowercase().as_str() {
                "true" | "yes" | "on" | "1" => Some(ScalarValue::from(true)),
                "false" | "no" | "off" | "0" => Some(ScalarValue::from(false)),
                _ => None,
            },
            ScalarType::Null => match self.value.to_lowercase().as_str() {
                "null" | "~" | "" => Some(ScalarValue::null()),
                _ => None,
            },
            #[cfg(feature = "base64")]
            ScalarType::Binary => {
                // Try to decode as base64 to verify it's valid binary data
                if base64_decode(&self.value).is_ok() {
                    Some(ScalarValue {
                        value: self.value.clone(),
                        style: ScalarStyle::Plain,
                        scalar_type: ScalarType::Binary,
                        tag: None,
                    })
                } else {
                    None
                }
            }
            ScalarType::Timestamp => {
                // Basic timestamp format validation
                if Self::is_valid_timestamp(&self.value) {
                    Some(ScalarValue::timestamp(&self.value))
                } else {
                    None
                }
            }
            ScalarType::Regex => {
                // For regex, just convert the value
                Some(ScalarValue::regex(&self.value))
            }
        }
    }

    /// Parse a YAML scalar value with automatic type detection
    ///
    /// This method automatically detects the YAML type based on the content:
    /// - "123" → Integer
    /// - "3.14" → Float
    /// - "true" / "false" → Boolean
    /// - "null" / "~" → Null
    /// - etc.
    ///
    /// # Examples
    ///
    /// ```
    /// use yaml_edit::{ScalarValue, ScalarType};
    ///
    /// let scalar = ScalarValue::parse("123");
    /// assert_eq!(scalar.scalar_type(), ScalarType::Integer);
    /// assert_eq!(scalar.to_yaml_string(), "123");
    ///
    /// let scalar = ScalarValue::parse("true");
    /// assert_eq!(scalar.scalar_type(), ScalarType::Boolean);
    /// assert_eq!(scalar.to_yaml_string(), "true");
    ///
    /// let scalar = ScalarValue::parse("hello");
    /// assert_eq!(scalar.scalar_type(), ScalarType::String);
    /// assert_eq!(scalar.to_yaml_string(), "hello");
    /// ```
    ///
    /// To create a String-type scalar without auto-detection (e.g., to represent
    /// the string "123" rather than the integer 123), use [`ScalarValue::string()`] instead.
    pub fn parse(value: impl Into<String>) -> Self {
        let value = value.into();
        let scalar_type = Self::auto_detect_type(&value);
        // For non-string types, use Plain style (no quotes)
        // For string types, detect appropriate style
        let style = match scalar_type {
            ScalarType::String => Self::detect_style(&value),
            // All other types use plain style
            _ => ScalarStyle::Plain,
        };

        Self {
            value,
            style,
            scalar_type,
            tag: None,
        }
    }

    /// Create a ScalarValue from a Scalar syntax node, preserving the type from the lexer
    ///
    /// This extracts type information directly from the token kind (INT, BOOL, FLOAT, etc.)
    /// rather than guessing based on heuristics. This is the correct way to convert
    /// parsed YAML into ScalarValue.
    pub fn from_scalar(scalar: &crate::yaml::Scalar) -> Self {
        let value = scalar.as_string();
        let raw_text = scalar.value();

        let style = if raw_text.starts_with('"') && raw_text.ends_with('"') {
            ScalarStyle::DoubleQuoted
        } else if raw_text.starts_with('\'') && raw_text.ends_with('\'') {
            ScalarStyle::SingleQuoted
        } else if scalar.is_block() && raw_text.starts_with('|') {
            ScalarStyle::Literal
        } else if scalar.is_block() {
            ScalarStyle::Folded
        } else {
            ScalarStyle::Plain
        };

        // Quoted and block scalars are always !!str per YAML 1.2 tag
        // resolution. Plain scalars go through the core-schema
        // classifier, which stays correct even when the plain-scalar
        // text spans multiple lexer tokens (e.g. multi-line plain
        // scalars, which yield STRING + NEWLINE + INDENT + STRING).
        // `trim_end` because the lexer may absorb trailing whitespace
        // into the scalar text; leading whitespace can't appear because
        // it would have been emitted as INDENT.
        let scalar_type = if style != ScalarStyle::Plain {
            ScalarType::String
        } else {
            Self::classify_plain(raw_text.trim_end()).into()
        };

        Self {
            value,
            style,
            scalar_type,
            tag: None,
        }
    }
}

impl fmt::Display for ScalarValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_yaml_string())
    }
}

impl From<String> for ScalarValue {
    fn from(value: String) -> Self {
        Self::string(value)
    }
}

impl From<&str> for ScalarValue {
    fn from(value: &str) -> Self {
        Self::string(value)
    }
}

/// `From<$t>` for the numeric primitives, all of which render through
/// `to_string()` and differ only in the YAML type they carry.
macro_rules! impl_from_number {
    ($($t:ty => $ty:expr),* $(,)?) => {
        $(
            impl From<$t> for ScalarValue {
                fn from(value: $t) -> Self {
                    Self {
                        value: value.to_string(),
                        style: ScalarStyle::Plain,
                        scalar_type: $ty,
                        tag: None,
                    }
                }
            }
        )*
    };
}

impl_from_number! {
    i32 => ScalarType::Integer,
    i64 => ScalarType::Integer,
}

macro_rules! impl_from_float {
    ($($t:ty),* $(,)?) => {
        $(
            impl From<$t> for ScalarValue {
                fn from(value: $t) -> Self {
                    Self {
                        value: Self::format_float(value.to_string()),
                        style: ScalarStyle::Plain,
                        scalar_type: ScalarType::Float,
                        tag: None,
                    }
                }
            }
        )*
    };
}

impl_from_float!(f32, f64);

impl From<bool> for ScalarValue {
    fn from(value: bool) -> Self {
        Self {
            value: if value { "true" } else { "false" }.to_string(),
            style: ScalarStyle::Plain,
            scalar_type: ScalarType::Boolean,
            tag: None,
        }
    }
}

/// Same as [`ScalarValue::from_scalar`].
impl From<crate::yaml::Scalar> for ScalarValue {
    fn from(scalar: crate::yaml::Scalar) -> Self {
        Self::from_scalar(&scalar)
    }
}

impl crate::AsYaml for ScalarValue {
    fn as_node(&self) -> Option<&crate::yaml::SyntaxNode> {
        None
    }

    fn kind(&self) -> crate::as_yaml::YamlKind {
        match self.rendered_tag() {
            Some(tag) => crate::as_yaml::YamlKind::Tagged(std::borrow::Cow::Owned(tag.to_string())),
            None => crate::as_yaml::YamlKind::Scalar,
        }
    }

    fn build_content(
        &self,
        builder: &mut rowan::GreenNodeBuilder,
        indent: usize,
        flow_context: bool,
    ) -> bool {
        self.build(
            builder,
            indent,
            flow_context.then_some(FlowQuoting::Minimal),
        )
    }

    fn build_flow_content(
        &self,
        builder: &mut rowan::GreenNodeBuilder,
        indent: usize,
        quoting: FlowQuoting,
    ) -> bool {
        self.build(builder, indent, Some(quoting))
    }

    fn is_inline(&self) -> bool {
        true
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_plain_scalars() {
        let scalar = ScalarValue::string("simple");
        assert_eq!(scalar.to_yaml_string(), "simple");

        let scalar = ScalarValue::string("hello world");
        assert_eq!(scalar.to_yaml_string(), "hello world");
    }

    #[test]
    fn test_values_needing_quotes() {
        // Boolean-like values
        let scalar = ScalarValue::string("true");
        assert_eq!(scalar.to_yaml_string(), "'true'");

        let scalar = ScalarValue::string("false");
        assert_eq!(scalar.to_yaml_string(), "'false'");

        let scalar = ScalarValue::string("yes");
        assert_eq!(scalar.to_yaml_string(), "'yes'");

        let scalar = ScalarValue::string("no");
        assert_eq!(scalar.to_yaml_string(), "'no'");

        // Null-like values
        let scalar = ScalarValue::string("null");
        assert_eq!(scalar.to_yaml_string(), "'null'");

        let scalar = ScalarValue::string("~");
        assert_eq!(scalar.to_yaml_string(), "'~'");

        // Numbers
        let scalar = ScalarValue::string("123");
        assert_eq!(scalar.to_yaml_string(), "'123'");

        let scalar = ScalarValue::string("3.14");
        assert_eq!(scalar.to_yaml_string(), "'3.14'");

        // Special characters
        let scalar = ScalarValue::string("value: something");
        assert_eq!(scalar.to_yaml_string(), "'value: something'");

        let scalar = ScalarValue::string("# comment");
        assert_eq!(scalar.to_yaml_string(), "'# comment'");

        // Leading/trailing whitespace
        let scalar = ScalarValue::string("  spaces  ");
        assert_eq!(scalar.to_yaml_string(), "'  spaces  '");
    }

    #[test]
    fn test_single_quoted() {
        let scalar = ScalarValue::single_quoted("value with 'quotes'");
        assert_eq!(scalar.to_yaml_string(), "'value with ''quotes'''");
    }

    #[test]
    fn test_double_quoted() {
        let scalar = ScalarValue::double_quoted("value with \"quotes\" and \\backslash");
        assert_eq!(
            scalar.to_yaml_string(),
            "\"value with \\\"quotes\\\" and \\\\backslash\""
        );

        let scalar = ScalarValue::double_quoted("line1\nline2\ttab");
        assert_eq!(scalar.to_yaml_string(), "\"line1\\nline2\\ttab\"");
    }

    #[test]
    fn test_carriage_return_is_escaped() {
        assert_eq!(ScalarValue::plain("a\rb").to_yaml_string(), "\"a\\rb\"");
        assert_eq!(
            ScalarValue::single_quoted("a\rb").to_yaml_string(),
            "\"a\\rb\""
        );
    }

    #[test]
    fn test_multiline() {
        let scalar = ScalarValue::string("line1\nline2\nline3");
        // Should auto-detect literal style for multiline
        assert_eq!(scalar.style(), ScalarStyle::Literal);
    }

    #[test]
    fn test_from_types() {
        let scalar = ScalarValue::from(42);
        assert_eq!(scalar.to_yaml_string(), "42");

        let scalar = ScalarValue::from(1.234);
        assert_eq!(scalar.to_yaml_string(), "1.234");

        let scalar = ScalarValue::from(true);
        assert_eq!(scalar.to_yaml_string(), "true");

        let scalar = ScalarValue::from(false);
        assert_eq!(scalar.to_yaml_string(), "false");
    }

    #[test]
    fn test_empty_string() {
        let scalar = ScalarValue::string("");
        assert_eq!(scalar.to_yaml_string(), "''");
    }

    #[test]
    fn test_special_start_chars() {
        let scalar = ScalarValue::string("-item");
        assert_eq!(scalar.to_yaml_string(), "'-item'");

        let scalar = ScalarValue::string("?key");
        assert_eq!(scalar.to_yaml_string(), "'?key'");

        let scalar = ScalarValue::string("[array]");
        assert_eq!(scalar.to_yaml_string(), "'[array]'");
    }

    #[test]
    fn test_null_scalar() {
        let scalar = ScalarValue::null();
        assert_eq!(scalar.to_yaml_string(), "null");
        assert_eq!(scalar.scalar_type, ScalarType::Null);
    }

    #[test]
    fn test_indentation_preservation() {
        // Test preserving exact indentation in block scalars
        let content_with_indent = "  Line 1\n    Line 2 more indented\n  Line 3";
        let scalar = ScalarValue::literal(content_with_indent);

        // The indicator keeps the leading spaces from being read as the
        // block's own indentation.
        let yaml_output = scalar.to_literal_with_indent(0, 2);
        assert_eq!(
            yaml_output,
            "|2-\n    Line 1\n      Line 2 more indented\n    Line 3"
        );
    }

    #[test]
    fn test_literal_with_custom_indent() {
        // Test applying custom indentation to unindented content
        let content = "Line 1\nLine 2\nLine 3";
        let scalar = ScalarValue::literal(content);

        let yaml_4_spaces = scalar.to_literal_with_indent(0, 4);
        assert_eq!(yaml_4_spaces, "|-\n    Line 1\n    Line 2\n    Line 3");

        let yaml_1_space = scalar.to_literal_with_indent(0, 1);
        assert_eq!(yaml_1_space, "|-\n Line 1\n Line 2\n Line 3");
    }

    #[test]
    fn test_block_indent_counts_from_parent_column() {
        let scalar = ScalarValue::literal("  x\n  y");
        assert_eq!(scalar.to_yaml_string(), "|2-\n    x\n    y");
        assert_eq!(
            scalar.to_yaml_string_with_indent(2, 2),
            "|2-\n      x\n      y"
        );
        assert_eq!(
            scalar.to_literal_with_indent(4, 3),
            "|3-\n         x\n         y"
        );
        assert_eq!(
            ScalarValue::folded("  x\n").to_folded_with_indent(2, 2),
            ">2\n      x"
        );
        assert_eq!(
            ScalarValue::literal("x\ny\n").to_yaml_string_with_indent(2, 2),
            "|\n    x\n    y"
        );
    }

    #[test]
    fn test_block_indicator_when_first_line_is_indented() {
        // Only the first line decides the indentation a reader detects, so
        // it alone decides whether an indicator is needed.
        assert_eq!(
            ScalarValue::literal(" a\nb").to_yaml_string(),
            "|2-\n   a\n  b"
        );
        assert_eq!(
            ScalarValue::literal("  L1\n    L2\n  L3").to_yaml_string_with_indent(2, 2),
            "|2-\n      L1\n        L2\n      L3"
        );
        assert_eq!(
            ScalarValue::literal("  \nx").to_yaml_string(),
            "|2-\n    \n  x"
        );
        assert_eq!(
            ScalarValue::literal("a\n  b").to_yaml_string(),
            "|-\n  a\n    b"
        );
    }

    #[test]
    fn test_block_keeps_whitespace_only_lines() {
        assert_eq!(
            ScalarValue::literal("a\n  \nb").to_yaml_string(),
            "|-\n  a\n    \n  b"
        );
    }

    #[test]
    #[should_panic(expected = "does not fit in one digit")]
    fn test_block_indent_indicator_too_large() {
        ScalarValue::literal("  x").to_literal_with_indent(0, 10);
    }

    #[test]
    #[should_panic(expected = "must be indented")]
    fn test_block_indent_zero() {
        ScalarValue::literal("x").to_literal_with_indent(2, 0);
    }

    #[test]
    fn test_folded_with_custom_indent() {
        // Test applying custom indentation to folded scalars
        let content = "Line 1\nLine 2\nLine 3";
        let scalar = ScalarValue::folded(content);

        let yaml_3_spaces = scalar.to_folded_with_indent(0, 3);
        assert_eq!(yaml_3_spaces, ">-\n   Line 1\n\n   Line 2\n\n   Line 3");
    }

    #[test]
    fn test_block_scalar_first_line_more_indented() {
        // The first line's extra spaces would be taken as the block's own
        // indentation, leaving the later line outside it.
        assert_eq!(
            ScalarValue::string("  a\nb").to_yaml_string(),
            "|2-\n    a\n  b"
        );
        assert_eq!(
            ScalarValue::string("  x\n y").to_yaml_string_with_indent(2, 2),
            "|2-\n      x\n     y"
        );
        assert_eq!(
            ScalarValue::string("\n x").to_yaml_string_with_indent(2, 2),
            "|2-\n\n     x"
        );
    }

    #[test]
    fn test_block_scalar_whitespace_only_lines() {
        assert_eq!(
            ScalarValue::literal("a\n ").to_yaml_string(),
            "|-\n  a\n   "
        );
        assert_eq!(
            ScalarValue::literal("a\n   \nb").to_yaml_string(),
            "|-\n  a\n     \n  b"
        );
        assert_eq!(ScalarValue::literal(" ").to_yaml_string(), "|2-\n   ");
        assert_eq!(ScalarValue::literal(" \n").to_yaml_string(), "|2\n   ");
        // Left to choose, such a value is double-quoted.
        assert_eq!(ScalarValue::string("a\n ").to_yaml_string(), "\"a\\n \"");
        assert_eq!(ScalarValue::string(" \n").to_yaml_string(), "\" \\n\"");
    }

    #[test]
    fn test_folded_preserves_line_breaks() {
        assert_eq!(
            ScalarValue::folded("a\nb").to_yaml_string(),
            ">-\n  a\n\n  b"
        );
        // Breaks next to a more-indented line are not folded, so they need
        // no extra empty line.
        assert_eq!(
            ScalarValue::folded("a\n\nb\n  c\nd\n").to_yaml_string(),
            ">\n  a\n\n\n  b\n    c\n  d"
        );
    }

    #[test]
    fn test_single_quoted_line_breaks() {
        // Continuation lines are indented to stay inside the parent block.
        assert_eq!(
            ScalarValue::single_quoted("a\nb").to_yaml_string(),
            "'a\n\n  b'"
        );
        assert_eq!(
            ScalarValue::single_quoted("a\n\nb").to_yaml_string(),
            "'a\n\n\n  b'"
        );
        assert_eq!(
            ScalarValue::single_quoted("\nb\n").to_yaml_string(),
            "'\n\n  b\n\n'"
        );
        assert_eq!(
            ScalarValue::single_quoted("a\nb").to_yaml_string_with_indent(2, 2),
            "'a\n\n    b'"
        );
        // Whitespace around a line break is trimmed when folding, so only
        // double quotes can carry it.
        assert_eq!(
            ScalarValue::single_quoted("a \nb").to_yaml_string(),
            "\"a \\nb\""
        );
        assert_eq!(
            ScalarValue::single_quoted("a\n b").to_yaml_string(),
            "\"a\\n b\""
        );
    }

    #[test]
    fn test_mixed_empty_lines_preservation() {
        // Test handling of empty lines in block scalars
        let content_with_empty_lines = "Line 1\n\nLine 3\n\n\nLine 6";
        let scalar = ScalarValue::literal(content_with_empty_lines);

        let yaml_output = scalar.to_literal_with_indent(0, 2);
        assert_eq!(yaml_output, "|-\n  Line 1\n\n  Line 3\n\n\n  Line 6");

        // Empty lines should remain empty (no indentation added)
        // Input has 3 empty lines; they should appear unchanged in the output
        let lines: Vec<&str> = yaml_output.lines().collect();
        let empty_line_count = lines.iter().filter(|line| line.is_empty()).count();
        assert_eq!(empty_line_count, 3);
    }

    #[test]
    fn test_double_quoted_with_escapes() {
        // Test that double-quoted scalars properly escape and unescape
        let original = "Hello\nWorld\t😃";
        let scalar = ScalarValue::double_quoted(original);
        let yaml_string = scalar.to_yaml_string();

        // Should contain escaped sequences
        assert_eq!(yaml_string, "\"Hello\\nWorld\\t\\U0001F603\"");

        // Parse it back
        let parsed = ScalarValue::parse_escape_sequences(&yaml_string[1..yaml_string.len() - 1]);
        assert_eq!(parsed, original);
    }

    #[test]
    fn test_unicode_output_formatting() {
        // Test that Unicode characters are properly formatted in output
        let scalar = ScalarValue::double_quoted("Hello 世界 🌍");
        let yaml_string = scalar.to_yaml_string();

        // Should escape non-ASCII characters
        assert_eq!(yaml_string, "\"Hello \\u4E16\\u754C \\U0001F30D\"");

        // But the internal value should remain unchanged
        assert_eq!(scalar.value(), "Hello 世界 🌍");
    }

    #[test]
    fn test_timestamp_creation_and_validation() {
        // Test various timestamp formats
        let valid_timestamps = [
            "2023-12-25",
            "2023-12-25T10:30:45",
            "2023-12-25 10:30:45",
            "2023-12-25T10:30:45Z",
            "2001-12-14 21:59:43.10 -5", // Space-separated with timezone
            "2001-12-15T02:59:43.1Z",    // ISO 8601
            "2001-12-14t21:59:43.10-05:00", // Lowercase t
        ];

        for ts in valid_timestamps {
            let scalar = ScalarValue::timestamp(ts);
            assert!(scalar.is_timestamp());
            assert_eq!(scalar.scalar_type(), ScalarType::Timestamp);
            assert_eq!(scalar.value(), ts);

            let yaml_output = scalar.to_yaml_string();
            assert_eq!(yaml_output, format!("!!timestamp {}", ts));

            // Test auto-detection recognizes it as timestamp
            let auto_scalar = ScalarValue::parse(ts);
            assert_eq!(
                auto_scalar.scalar_type(),
                ScalarType::Timestamp,
                "Failed to auto-detect '{}' as timestamp",
                ts
            );
        }

        // Test invalid timestamps are not recognized
        let invalid_timestamps = [
            "not-a-date",
            "2023-13-01", // Invalid month
            "2023-12-32", // Invalid day
            "12:34:56",   // Time only (should be String)
            "2023/12/25", // Wrong separator
        ];

        for ts in invalid_timestamps {
            let auto_scalar = ScalarValue::parse(ts);
            assert_ne!(
                auto_scalar.scalar_type(),
                ScalarType::Timestamp,
                "'{}' should not be detected as timestamp",
                ts
            );
        }
    }

    #[test]
    fn test_regex_creation() {
        let pattern = r"^\d{3}-\d{2}-\d{4}$";
        let scalar = ScalarValue::regex(pattern);

        assert!(scalar.is_regex());
        assert_eq!(scalar.scalar_type(), ScalarType::Regex);
        assert_eq!(scalar.value(), pattern);

        let yaml_output = scalar.to_yaml_string();
        assert_eq!(yaml_output, format!("!!regex {}", pattern));
    }

    #[test]
    fn test_regex_edge_cases() {
        // Test empty pattern
        let empty_regex = ScalarValue::regex("");
        assert!(empty_regex.is_regex());
        assert_eq!(empty_regex.value(), "");
        assert_eq!(empty_regex.to_yaml_string(), "!!regex");

        // Test pattern with special characters
        let special_chars = ScalarValue::regex(r"[.*+?^${}()|[\]\\]");
        assert!(special_chars.is_regex());
        assert_eq!(special_chars.value(), r"[.*+?^${}()|[\]\\]");

        // Test unicode patterns
        let unicode_regex = ScalarValue::regex(r"\p{L}+");
        assert!(unicode_regex.is_regex());
        assert_eq!(unicode_regex.value(), r"\p{L}+");

        // Test very long pattern
        let long_pattern = "a".repeat(1000);
        let long_regex = ScalarValue::regex(&long_pattern);
        assert!(long_regex.is_regex());
        assert_eq!(long_regex.value(), long_pattern);

        // Test pattern with quotes and escapes
        let quoted_regex = ScalarValue::regex(r#"'quoted' and "double quoted" with \\ backslash"#);
        assert!(quoted_regex.is_regex());
        assert_eq!(
            quoted_regex.value(),
            r#"'quoted' and "double quoted" with \\ backslash"#
        );
    }

    #[test]
    fn test_regex_type_coercion() {
        let regex_scalar = ScalarValue::regex(r"\d+");

        // Test coercing regex to string
        let string_scalar = regex_scalar.coerce_to_type(ScalarType::String).unwrap();
        assert_eq!(string_scalar.scalar_type(), ScalarType::String);
        assert_eq!(string_scalar.value(), r"\d+");
        assert!(!string_scalar.is_regex());

        // Test coercing string to regex
        let str_scalar = ScalarValue::string("test.*");
        let regex_from_string = str_scalar.coerce_to_type(ScalarType::Regex).unwrap();
        assert_eq!(regex_from_string.scalar_type(), ScalarType::Regex);
        assert_eq!(regex_from_string.value(), "test.*");
        assert!(regex_from_string.is_regex());

        // Test that regex cannot be coerced to number types
        assert!(regex_scalar.coerce_to_type(ScalarType::Integer).is_none());
        assert!(regex_scalar.coerce_to_type(ScalarType::Float).is_none());
        assert!(regex_scalar.coerce_to_type(ScalarType::Boolean).is_none());
    }

    #[test]
    #[cfg(feature = "regex")]
    fn test_regex_compilation() {
        // Test as_regex() with a regex scalar
        let regex_scalar = ScalarValue::regex(r"\d{3}-\d{4}");
        let compiled = regex_scalar.as_regex().unwrap();
        assert!(compiled.is_match("555-1234"));
        assert!(!compiled.is_match("not-a-phone"));

        // Test as_regex() with a non-regex scalar returns None
        let string_scalar = ScalarValue::string("not a regex");
        assert!(string_scalar.as_regex().is_none());

        // Test try_as_regex() with any scalar type
        let pattern_scalar = ScalarValue::string(r"^\w+@\w+\.\w+$");
        let email_regex = pattern_scalar.try_as_regex().unwrap();
        assert!(email_regex.is_match("test@example.com"));
        assert!(!email_regex.is_match("not-an-email"));

        // Test with invalid regex pattern
        let invalid_scalar = ScalarValue::regex(r"[invalid(");
        assert!(invalid_scalar.as_regex().is_none());

        // Test try_as_regex() with invalid pattern returns error
        let invalid_pattern = ScalarValue::string(r"[invalid(");
        assert!(invalid_pattern.try_as_regex().is_err());
    }

    #[test]
    #[cfg(feature = "regex")]
    fn test_regex_extraction_use_cases() {
        // Test extracting and using regex for validation
        let validation_rules = [
            ScalarValue::regex(r"^\d{5}$"),                 // ZIP code
            ScalarValue::regex(r"^[A-Z]{2}$"),              // State code
            ScalarValue::regex(r"^\(\d{3}\) \d{3}-\d{4}$"), // Phone number
        ];

        let test_values = ["12345", "CA", "(555) 123-4567"];

        for (rule, value) in validation_rules.iter().zip(test_values.iter()) {
            let regex = rule.as_regex().unwrap();
            assert!(regex.is_match(value), "Pattern should match {}", value);
        }

        // Test with complex regex patterns
        let email_regex = ScalarValue::regex(r"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$");
        let compiled = email_regex.as_regex().unwrap();
        assert!(compiled.is_match("user@example.com"));
        assert!(compiled.is_match("test.user+tag@sub.domain.org"));
        assert!(!compiled.is_match("invalid.email"));

        // Test extracting capture groups
        let version_regex = ScalarValue::regex(r"^v(\d+)\.(\d+)\.(\d+)$");
        let compiled = version_regex.as_regex().unwrap();
        if let Some(captures) = compiled.captures("v1.2.3") {
            assert_eq!(captures.get(1).unwrap().as_str(), "1");
            assert_eq!(captures.get(2).unwrap().as_str(), "2");
            assert_eq!(captures.get(3).unwrap().as_str(), "3");
        } else {
            panic!("Should have matched version string");
        }
    }

    #[test]
    fn test_type_coercion() {
        // Test coercing string to integer
        let str_scalar = ScalarValue::string("42");
        let int_scalar = str_scalar.coerce_to_type(ScalarType::Integer).unwrap();
        assert_eq!(int_scalar.scalar_type(), ScalarType::Integer);
        assert_eq!(int_scalar.value(), "42");

        // Test coercing string to boolean
        let bool_scalar = ScalarValue::string("true")
            .coerce_to_type(ScalarType::Boolean)
            .unwrap();
        assert_eq!(bool_scalar.scalar_type(), ScalarType::Boolean);
        assert_eq!(bool_scalar.value(), "true");

        // Test coercing boolean string variations
        let yes_scalar = ScalarValue::string("yes")
            .coerce_to_type(ScalarType::Boolean)
            .unwrap();
        assert_eq!(yes_scalar.value(), "true");

        let no_scalar = ScalarValue::string("no")
            .coerce_to_type(ScalarType::Boolean)
            .unwrap();
        assert_eq!(no_scalar.value(), "false");

        // Test failed coercion
        let str_scalar = ScalarValue::string("not_a_number");
        assert!(str_scalar.coerce_to_type(ScalarType::Integer).is_none());
    }

    #[test]
    fn test_auto_type_detection() {
        // Test various automatic type detections
        assert_eq!(ScalarValue::auto_detect_type("42"), ScalarType::Integer);
        assert_eq!(ScalarValue::auto_detect_type("3.14"), ScalarType::Float);
        assert_eq!(ScalarValue::auto_detect_type("true"), ScalarType::Boolean);
        assert_eq!(ScalarValue::auto_detect_type("false"), ScalarType::Boolean);
        assert_eq!(ScalarValue::auto_detect_type("yes"), ScalarType::String);
        assert_eq!(ScalarValue::auto_detect_type("NuLl"), ScalarType::String);
        assert_eq!(ScalarValue::auto_detect_type("null"), ScalarType::Null);
        assert_eq!(ScalarValue::auto_detect_type("~"), ScalarType::Null);
        assert_eq!(ScalarValue::auto_detect_type(""), ScalarType::Null);
        assert_eq!(
            ScalarValue::auto_detect_type("2023-12-25"),
            ScalarType::Timestamp
        );
        assert_eq!(
            ScalarValue::auto_detect_type("2023-12-25T10:30:45"),
            ScalarType::Timestamp
        );
        // Base64-looking text is still a string unless tagged !!binary.
        assert_eq!(
            ScalarValue::auto_detect_type("SGVsbG8gV29ybGQ="),
            ScalarType::String
        );
        assert_eq!(
            ScalarValue::auto_detect_type("hello world"),
            ScalarType::String
        );
    }

    #[test]
    fn test_from_yaml_scalar_creation() {
        let int_scalar = ScalarValue::parse("123");
        assert_eq!(int_scalar.scalar_type(), ScalarType::Integer);

        let bool_scalar = ScalarValue::parse("true");
        assert_eq!(bool_scalar.scalar_type(), ScalarType::Boolean);

        let timestamp_scalar = ScalarValue::parse("2023-12-25");
        assert_eq!(timestamp_scalar.scalar_type(), ScalarType::Timestamp);

        let string_scalar = ScalarValue::parse("hello world");
        assert_eq!(string_scalar.scalar_type(), ScalarType::String);
    }

    #[test]
    fn test_yaml_1_1_timestamp_examples() {
        // The examples of https://yaml.org/type/timestamp.html, and the
        // one-digit and white space forms its regular expression allows.
        for text in [
            "2001-12-15T02:59:43.1Z",
            "2001-12-14t21:59:43.10-05:00",
            "2001-12-14 21:59:43.10 -5",
            "2001-12-15 2:59:43.10",
            "2002-12-14",
            "2001-1-5T1:02:03Z",
            "2001-12-14  21:59:43",
            "2001-12-14\t21:59:43",
        ] {
            assert_eq!(
                ScalarValue::parse(text).scalar_type(),
                ScalarType::Timestamp,
                "{text:?}"
            );
        }
        for text in [
            "2001-1-5",
            "2001-12-5",
            "2001-1-15",
            "2001-12-14T",
            "2001-12-14 123:59:43",
            "2001-12-14T21:5:43",
            "2001-12-14T21:59:4",
            "2001-12-14x21:59:43",
            "2001-12-14T24:00:00",
            "2001-12-32",
        ] {
            assert_eq!(
                ScalarValue::parse(text).scalar_type(),
                ScalarType::String,
                "{text:?}"
            );
        }
    }

    #[test]
    #[cfg(feature = "base64")]
    fn test_binary_yaml_output_with_tags() {
        let data = b"Binary data here";
        let scalar = ScalarValue::binary(data);
        let yaml_output = scalar.to_yaml_string();

        assert!(yaml_output.starts_with("!!binary "));

        // Extract just the base64 part
        let base64_part = &yaml_output[9..]; // Remove "!!binary "
        let decoded = base64_decode(base64_part).expect("Should decode");
        assert_eq!(decoded, data);
    }

    #[test]
    #[cfg(feature = "base64")]
    fn test_special_data_types_with_different_styles() {
        // Binary with different styles should still include tag
        let data = b"test";
        let binary_scalar = ScalarValue::binary(data);

        // Even if we change style, binary type should maintain tag
        let mut styled_binary = binary_scalar;
        styled_binary.style = ScalarStyle::DoubleQuoted;

        // The to_yaml_string should still respect the scalar type for tagging
        assert_eq!(styled_binary.to_yaml_string(), "!!binary \"dGVzdA==\"");
    }

    #[test]
    fn test_type_checking_methods() {
        #[cfg(feature = "base64")]
        let binary_scalar = ScalarValue::binary(b"test");
        let timestamp_scalar = ScalarValue::timestamp("2023-12-25");
        let regex_scalar = ScalarValue::regex(r"\d+");
        let string_scalar = ScalarValue::string("hello");

        // Test type checking methods
        #[cfg(feature = "base64")]
        assert!(binary_scalar.is_binary());
        #[cfg(feature = "base64")]
        assert!(!binary_scalar.is_timestamp());
        #[cfg(feature = "base64")]
        assert!(!binary_scalar.is_regex());

        #[cfg(feature = "base64")]
        assert!(!timestamp_scalar.is_binary());
        assert!(timestamp_scalar.is_timestamp());
        assert!(!timestamp_scalar.is_regex());

        #[cfg(feature = "base64")]
        assert!(!regex_scalar.is_binary());
        assert!(!regex_scalar.is_timestamp());
        assert!(regex_scalar.is_regex());

        #[cfg(feature = "base64")]
        assert!(!string_scalar.is_binary());
        assert!(!string_scalar.is_timestamp());
        assert!(!string_scalar.is_regex());
    }

    #[test]
    fn test_binary_is_not_an_integer() {
        // YAML 1.2 has no binary integer form.
        for text in ["0b1010", "0B101", "-0b1010", "+0b101"] {
            assert_eq!(ScalarValue::parse_integer(text), None, "{text}");
            assert_eq!(ScalarValue::auto_detect_type(text), ScalarType::String);
        }
    }

    #[test]
    fn test_modern_octal_number_parsing() {
        // Test modern octal number parsing (0o prefix)
        assert_eq!(ScalarValue::parse_integer("0o755"), Some(493)); // 7*64 + 5*8 + 5
        assert_eq!(ScalarValue::parse_integer("0o644"), Some(420)); // 6*64 + 4*8 + 4
        assert_eq!(ScalarValue::auto_detect_type("0o755"), ScalarType::Integer);

        // The core schema allows neither a sign nor an uppercase prefix.
        for text in ["0O777", "-0o755", "+0o644"] {
            assert_eq!(ScalarValue::parse_integer(text), None, "{text}");
            assert_eq!(ScalarValue::auto_detect_type(text), ScalarType::String);
        }

        // Test invalid octal
        assert_eq!(ScalarValue::parse_integer("0o789"), None); // Contains invalid digit
        assert_eq!(ScalarValue::parse_integer("0o"), None); // Empty after prefix
    }

    #[test]
    fn test_leading_zero_is_decimal() {
        // YAML 1.2 dropped 1.1's bare-octal form.
        assert_eq!(ScalarValue::parse_integer("0755"), Some(755));
        assert_eq!(ScalarValue::parse_integer("0644"), Some(644));
        assert_eq!(ScalarValue::parse_integer("-0777"), Some(-777));

        // Test auto-detection
        assert_eq!(ScalarValue::auto_detect_type("0755"), ScalarType::Integer);
        assert_eq!(ScalarValue::auto_detect_type("0644"), ScalarType::Integer);

        // Test edge cases
        assert_eq!(ScalarValue::parse_integer("0"), Some(0)); // Single zero
        assert_eq!(ScalarValue::parse_integer("00"), Some(0)); // Double zero

        assert_eq!(ScalarValue::parse_integer("0789"), Some(789));
        assert_eq!(ScalarValue::parse_integer("08"), Some(8));
    }

    #[test]
    fn test_hexadecimal_number_parsing() {
        // Test hexadecimal number parsing (0x prefix) - should still work
        assert_eq!(ScalarValue::parse_integer("0xFF"), Some(255));
        assert_eq!(ScalarValue::parse_integer("0x1A"), Some(26));
        assert_eq!(ScalarValue::parse_integer("0xff"), Some(255));
        assert_eq!(ScalarValue::auto_detect_type("0xFF"), ScalarType::Integer);

        // The core schema allows neither a sign nor an uppercase prefix.
        for text in ["0XFF", "-0xFF", "+0x1A", "+0x1F", "-0o7", "0X1F", "0O7"] {
            assert_eq!(ScalarValue::parse_integer(text), None, "{text}");
            assert_eq!(ScalarValue::auto_detect_type(text), ScalarType::String);
            assert_eq!(
                ScalarValue::classify_plain(text),
                CoreScalarType::String,
                "{text}"
            );
        }
    }

    #[test]
    fn test_decimal_number_parsing() {
        // Test decimal number parsing (no prefix) - should still work
        assert_eq!(ScalarValue::parse_integer("42"), Some(42));
        assert_eq!(ScalarValue::parse_integer("123"), Some(123));
        assert_eq!(ScalarValue::parse_integer("-42"), Some(-42));
        assert_eq!(ScalarValue::parse_integer("+123"), Some(123));

        // Test auto-detection
        assert_eq!(ScalarValue::auto_detect_type("42"), ScalarType::Integer);
        assert_eq!(ScalarValue::auto_detect_type("-123"), ScalarType::Integer);
    }

    #[test]
    fn test_number_format_yaml_output() {
        // Test that different number formats are properly detected and output
        let binary_scalar = ScalarValue::parse("0b1010");
        assert_eq!(binary_scalar.scalar_type(), ScalarType::String);
        assert_eq!(binary_scalar.to_yaml_string(), "'0b1010'");

        let octal_scalar = ScalarValue::parse("0o755");
        assert_eq!(octal_scalar.scalar_type(), ScalarType::Integer);
        assert_eq!(octal_scalar.value(), "0o755");

        let hex_scalar = ScalarValue::parse("0xFF");
        assert_eq!(hex_scalar.scalar_type(), ScalarType::Integer);
        assert_eq!(hex_scalar.value(), "0xFF");

        let legacy_octal_scalar = ScalarValue::parse("0755");
        assert_eq!(legacy_octal_scalar.scalar_type(), ScalarType::Integer);
        assert_eq!(legacy_octal_scalar.value(), "0755");
        assert_eq!(legacy_octal_scalar.to_i64(), Some(755));
    }

    #[test]
    fn test_coerce_to_null() {
        // Only the recognized null spellings coerce to Null.
        let s = ScalarValue::string("null");
        assert_eq!(
            s.coerce_to_type(ScalarType::Null).map(|v| v.scalar_type()),
            Some(ScalarType::Null)
        );
        assert!(ScalarValue::string("~")
            .coerce_to_type(ScalarType::Null)
            .is_some());
        assert!(ScalarValue::string("")
            .coerce_to_type(ScalarType::Null)
            .is_some());
        assert!(ScalarValue::string("notnull")
            .coerce_to_type(ScalarType::Null)
            .is_none());
    }

    #[test]
    fn test_parse_preserves_string_style() {
        // The String arm of parse() runs detect_style; a plain word stays Plain.
        let plain = ScalarValue::parse("hello");
        assert_eq!(plain.scalar_type(), ScalarType::String);
        assert_eq!(plain.style, ScalarStyle::Plain);

        // A multi-line string is given Literal style by detect_style, which only
        // runs in the String arm of parse().
        let multiline = ScalarValue::parse("multi line\nvalue");
        assert_eq!(multiline.scalar_type(), ScalarType::String);
        assert_eq!(multiline.style, ScalarStyle::Literal);
    }

    #[test]
    fn test_from_scalar_type_and_style() {
        use crate::yaml::Document;
        use rowan::ast::AstNode;
        use std::str::FromStr;

        // NULL token maps to Null type.
        let doc = Document::from_str("k: null\n").unwrap();
        let mapping = doc.as_mapping().unwrap();
        let null_node = mapping.get("k").unwrap();
        let null_scalar = crate::yaml::Scalar::cast(null_node.syntax().clone()).unwrap();
        let sv = ScalarValue::from_scalar(&null_scalar);
        assert_eq!(sv.scalar_type(), ScalarType::Null);
        assert_eq!(sv.style, ScalarStyle::Plain);

        // STRING token in double quotes maps to String type, DoubleQuoted style.
        let doc = Document::from_str("k: \"hi\"\n").unwrap();
        let node = doc.as_mapping().unwrap().get("k").unwrap();
        let scalar = crate::yaml::Scalar::cast(node.syntax().clone()).unwrap();
        let sv = ScalarValue::from_scalar(&scalar);
        assert_eq!(sv.scalar_type(), ScalarType::String);
        assert_eq!(sv.style, ScalarStyle::DoubleQuoted);

        // Single quoted string keeps SingleQuoted style.
        let doc = Document::from_str("k: 'hi'\n").unwrap();
        let node = doc.as_mapping().unwrap().get("k").unwrap();
        let scalar = crate::yaml::Scalar::cast(node.syntax().clone()).unwrap();
        let sv = ScalarValue::from_scalar(&scalar);
        assert_eq!(sv.style, ScalarStyle::SingleQuoted);
    }

    #[test]
    fn test_from_scalar_ampersand_in_plain_is_string() {
        // `3.1&1` is a single plain scalar with literal content `3.1&1`.
        // Per YAML 1.2 core-schema tag resolution it must resolve to
        // !!str, not !!float.
        use crate::yaml::Document;
        use rowan::ast::AstNode;
        use std::str::FromStr;

        let doc = Document::from_str("k: 3.1&1\n").unwrap();
        let node = doc.as_mapping().unwrap().get("k").unwrap();
        let scalar = crate::yaml::Scalar::cast(node.syntax().clone()).unwrap();
        let sv = ScalarValue::from_scalar(&scalar);
        assert_eq!(sv.scalar_type(), ScalarType::String);
        assert_eq!(sv.style, ScalarStyle::Plain);
    }

    #[test]
    fn test_from_scalar_signed_yaml_infinity_is_float() {
        // `+.INF` is one !!float per YAML 1.2 even though Rust's
        // `f64::from_str` wouldn't accept the bare token on its own.
        use crate::yaml::Document;
        use rowan::ast::AstNode;
        use std::str::FromStr;

        let doc = Document::from_str("k: +.INF\n").unwrap();
        let node = doc.as_mapping().unwrap().get("k").unwrap();
        let scalar = crate::yaml::Scalar::cast(node.syntax().clone()).unwrap();
        let sv = ScalarValue::from_scalar(&scalar);
        assert_eq!(sv.scalar_type(), ScalarType::Float);
    }

    #[test]
    fn test_classify_plain_bare_inf_nan_is_string() {
        // Per YAML 1.2 only the dotted spellings are !!float; bare
        // `inf`, `nan`, `infinity` in any casing are strings even
        // though Rust's `f64::from_str` accepts them.
        for input in [
            "inf", "Inf", "InF", "INF", "iNf", "nan", "NaN", "NAN", "nAn", "infinity", "Infinity",
            "INFINITY", "+inf", "-Inf", "+nan",
        ] {
            assert_eq!(
                ScalarValue::classify_plain(input),
                CoreScalarType::String,
                "bare {input:?} should classify as String"
            );
        }
        // But the dotted spellings still resolve to Float.
        for input in [".inf", ".Inf", ".INF", "+.inf", "-.INF", ".nan"] {
            assert_eq!(
                ScalarValue::classify_plain(input),
                CoreScalarType::Float,
                "{input:?} should classify as Float"
            );
        }
    }

    #[test]
    fn test_canonical_integer() {
        for (text, expected) in [
            ("0", "0"),
            ("-0", "0"),
            ("+000", "0"),
            ("0x0", "0"),
            ("0o0", "0"),
            ("42", "42"),
            ("+0042", "42"),
            ("-42", "-42"),
            ("0x2A", "42"),
            ("0o52", "42"),
            ("-9223372036854775808", "-9223372036854775808"),
            ("9223372036854775808", "9223372036854775808"),
            ("+9223372036854775808", "9223372036854775808"),
            ("0x10000000000000000", "18446744073709551616"),
            ("0o2000000000000000000000", "18446744073709551616"),
            ("0x1000000000", "68719476736"),
            (
                "0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF",
                "340282366920938463463374607431768211455",
            ),
            (
                "0x100000000000000000000000000000000",
                "340282366920938463463374607431768211456",
            ),
            (
                "-000100000000000000000000000000",
                "-100000000000000000000000000",
            ),
        ] {
            assert_eq!(
                ScalarValue::canonical_integer(text).as_deref(),
                Some(expected),
                "{text}"
            );
        }
        for text in [
            "", "+", "-", "0x", "0o8", "-0x1", "+0o1", "1_000", "1.0", " 1",
        ] {
            assert_eq!(ScalarValue::canonical_integer(text), None, "{text:?}");
        }
    }

    #[test]
    fn test_from_scalar_block_style_is_string() {
        // Block scalars (| and >) always resolve to !!str regardless of
        // content, and their style must be reported as Literal or Folded.
        use crate::yaml::Document;
        use rowan::ast::AstNode;
        use std::str::FromStr;

        let doc = Document::from_str("k: |\n  1\n").unwrap();
        let node = doc.as_mapping().unwrap().get("k").unwrap();
        let scalar = crate::yaml::Scalar::cast(node.syntax().clone()).unwrap();
        let sv = ScalarValue::from_scalar(&scalar);
        assert_eq!(sv.style, ScalarStyle::Literal);
        assert_eq!(sv.scalar_type(), ScalarType::String);

        let doc = Document::from_str("k: >\n  x\n").unwrap();
        let node = doc.as_mapping().unwrap().get("k").unwrap();
        let scalar = crate::yaml::Scalar::cast(node.syntax().clone()).unwrap();
        let sv = ScalarValue::from_scalar(&scalar);
        assert_eq!(sv.style, ScalarStyle::Folded);
        assert_eq!(sv.scalar_type(), ScalarType::String);
    }

    #[test]
    fn test_needs_quoting_special_chars() {
        // Leading-character based quoting and special embedded characters, plus
        // each boolean/null keyword spelling (so dropping any one keyword arm is
        // detected). Single quotes are preferred unless the value already
        // contains one, in which case double quotes are used.
        let cases = [
            ("&anchor", "'&anchor'"),
            ("*alias", "'*alias'"),
            ("!tag", "'!tag'"),
            ("a|b", "'a|b'"),
            ("it's", "\"it's\""),
            ("a\"b", "'a\"b'"),
            ("50%", "'50%'"),
            ("on", "'on'"),
            ("off", "'off'"),
            ("On", "'On'"),
            ("OFF", "'OFF'"),
            ("yes", "'yes'"),
            ("no", "'no'"),
            ("true", "'true'"),
            ("false", "'false'"),
            ("null", "'null'"),
        ];
        for (input, expected) in cases {
            assert_eq!(ScalarValue::string(input).to_yaml_string(), expected);
        }
        // A plain word that needs no quoting round-trips unchanged.
        assert_eq!(ScalarValue::string("plain").to_yaml_string(), "plain");
    }

    #[test]
    fn test_plain_string_with_control_character_is_double_quoted() {
        assert_eq!(ScalarValue::plain("\u{7}").to_yaml_string(), "\"\\a\"");
        assert_eq!(ScalarValue::plain("a\rb").to_yaml_string(), "\"a\\rb\"");
        assert_eq!(
            ScalarValue::plain("a\rb").with_tag("!x").to_yaml_string(),
            "!x \"a\\rb\""
        );
    }

    #[test]
    fn test_literal_and_folded_nonempty() {
        // Literal and folded styles render the block indicator on its own line
        // followed by the content indented by two spaces. A folded block
        // needs an empty line to keep a line break.
        let s = ScalarValue::with_style("line one\nline two", ScalarStyle::Literal);
        assert_eq!(s.to_yaml_string(), "|-\n  line one\n  line two");

        let s = ScalarValue::with_style("line one\nline two", ScalarStyle::Folded);
        assert_eq!(s.to_yaml_string(), ">-\n  line one\n\n  line two");
    }

    #[test]
    fn test_display_matches_yaml_string() {
        let s = ScalarValue::string("true");
        assert_eq!(format!("{}", s), s.to_yaml_string());
        assert_eq!(format!("{}", s), "'true'");
    }

    #[test]
    fn test_as_yaml_token_kind_per_type() {
        use crate::lex::SyntaxKind;
        use rowan::ast::AstNode;

        let cases = [
            (ScalarValue::parse("42"), SyntaxKind::INT),
            (ScalarValue::parse("3.14"), SyntaxKind::FLOAT),
            (ScalarValue::parse("true"), SyntaxKind::BOOL),
            (ScalarValue::parse("null"), SyntaxKind::NULL),
            (ScalarValue::string("hello"), SyntaxKind::STRING),
        ];
        for (value, expected) in cases {
            let mut builder = rowan::GreenNodeBuilder::new();
            crate::AsYaml::build_content(&value, &mut builder, 0, false);
            let green = builder.finish();
            let node = rowan::SyntaxNode::<crate::yaml::Lang>::new_root(green);
            let scalar = crate::yaml::Scalar::cast(node).unwrap();
            let kind = scalar
                .syntax()
                .children_with_tokens()
                .filter_map(|c| c.into_token())
                .next()
                .unwrap()
                .kind();
            assert_eq!(kind, expected, "value {:?}", value.value());
        }
    }

    #[test]
    #[cfg(feature = "regex")]
    fn test_as_regex_type_gated() {
        // as_regex only compiles when the scalar is tagged as a regex.
        let re = ScalarValue::regex(r"\d+");
        assert!(re.as_regex().is_some());

        let not_re = ScalarValue::string(r"\d+");
        assert!(not_re.as_regex().is_none());

        // try_as_regex ignores the type and compiles the value directly.
        assert!(not_re.try_as_regex().is_ok());
        assert!(ScalarValue::string("(").try_as_regex().is_err());
    }

    #[test]
    fn test_from_f64_renders_yaml_float() {
        let cases: [(f64, &str); 6] = [
            (f64::NAN, ".nan"),
            (f64::INFINITY, ".inf"),
            (f64::NEG_INFINITY, "-.inf"),
            (1.0, "1.0"),
            (-0.0, "-0.0"),
            (2.5, "2.5"),
        ];
        for (value, expected) in cases {
            let sv = ScalarValue::from(value);
            assert_eq!(sv.to_yaml_string(), expected);
            assert_eq!(ScalarValue::classify_plain(expected), CoreScalarType::Float);
        }
        assert_eq!(ScalarValue::from(1.0f32).to_yaml_string(), "1.0");
        assert_eq!(ScalarValue::from(f32::NAN).to_yaml_string(), ".nan");
    }

    #[test]
    fn test_to_f64_special_values() {
        let float = |s: &str| ScalarValue {
            value: s.to_string(),
            style: ScalarStyle::Plain,
            scalar_type: ScalarType::Float,
            tag: None,
        };
        assert_eq!(float(".inf").to_f64(), Some(f64::INFINITY));
        assert_eq!(float("+.INF").to_f64(), Some(f64::INFINITY));
        assert_eq!(float("-.Inf").to_f64(), Some(f64::NEG_INFINITY));
        assert!(float(".NaN").to_f64().unwrap().is_nan());
        assert_eq!(float("inf").to_f64(), None);
        assert_eq!(float("1.5").to_f64(), Some(1.5));
    }

    #[test]
    fn test_parse_does_not_infer_binary() {
        for text in ["test", "abcd", "noon", "SGVsbG8="] {
            let scalar = ScalarValue::parse(text);
            assert_eq!(scalar.scalar_type(), ScalarType::String, "{text}");
            assert_eq!(scalar.to_yaml_string(), text);
        }
    }

    #[test]
    fn test_sign_after_radix_prefix_is_not_integer() {
        for text in ["0x-5", "0x+5", "0o-7", "0o+7", "0b-1", "+-5", "-+5"] {
            assert_eq!(ScalarValue::parse_integer(text), None, "{text}");
            assert_eq!(
                ScalarValue::classify_plain(text),
                CoreScalarType::String,
                "{text}"
            );
            assert_eq!(
                ScalarValue::auto_detect_type(text),
                ScalarType::String,
                "{text}"
            );
            let doc: crate::yaml::Document = format!("k: {text}\n").parse().unwrap();
            let node = doc.as_mapping().unwrap().get("k").unwrap();
            assert_eq!(node.as_scalar().unwrap().as_i64(), None, "{text}");
        }
    }

    #[test]
    fn test_integers_outside_i64() {
        assert_eq!(
            ScalarValue::parse_integer("-9223372036854775808"),
            Some(i64::MIN)
        );
        assert_eq!(ScalarValue::parse_integer("9223372036854775808"), None);
        for text in [
            "9223372036854775808",
            "-9223372036854775808",
            "0xFFFFFFFFFFFFFFFF",
            "123456789012345678901234567890",
        ] {
            assert_eq!(
                ScalarValue::classify_plain(text),
                CoreScalarType::Integer,
                "{text}"
            );
            assert_eq!(
                ScalarValue::auto_detect_type(text),
                ScalarType::Integer,
                "{text}"
            );
        }
        assert_eq!(
            ScalarValue::parse("-9223372036854775808").to_i64(),
            Some(i64::MIN)
        );
        assert_eq!(ScalarValue::parse("9223372036854775808").to_i64(), None);
    }

    #[test]
    fn test_coerce_to_special_types() {
        let coerce = |value: &str, target: ScalarType| {
            ScalarValue::string(value)
                .coerce_to_type(target)
                .map(|s| (s.scalar_type(), s.value().to_string()))
        };
        #[cfg(feature = "base64")]
        {
            assert_eq!(
                coerce("aGk=", ScalarType::Binary),
                Some((ScalarType::Binary, "aGk=".to_string()))
            );
            assert_eq!(coerce("!!", ScalarType::Binary), None);
            assert_eq!(ScalarValue::string("aGk=").as_binary(), None);
            assert_eq!(
                ScalarValue::binary(b"hi").as_binary(),
                Some(Ok(b"hi".to_vec()))
            );
        }
        assert_eq!(
            coerce("2001-12-14", ScalarType::Timestamp),
            Some((ScalarType::Timestamp, "2001-12-14".to_string()))
        );
        assert_eq!(coerce("x", ScalarType::Timestamp), None);
        assert_eq!(
            coerce("a+", ScalarType::Regex),
            Some((ScalarType::Regex, "a+".to_string()))
        );
        assert_eq!(
            coerce("~", ScalarType::Null),
            Some((ScalarType::Null, "null".to_string()))
        );
        assert_eq!(coerce("x", ScalarType::Null), None);
    }

    #[test]
    #[should_panic(expected = "does not fit in one digit")]
    fn test_block_indent_indicator_from_first_line_only() {
        ScalarValue::literal("\n  x\ny").to_literal_with_indent(0, 10);
    }
}
