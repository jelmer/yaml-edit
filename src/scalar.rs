//! Scalar value wrapper with proper escaping and style support.

use std::fmt;

#[cfg(feature = "base64")]
use base64::{engine::general_purpose, Engine as _};

/// Base64 encode bytes for binary data
#[cfg(feature = "base64")]
fn base64_encode(input: &[u8]) -> String {
    general_purpose::STANDARD.encode(input)
}

/// Base64 decode string back to bytes
#[cfg(feature = "base64")]
fn base64_decode(input: &str) -> Result<Vec<u8>, String> {
    general_purpose::STANDARD
        .decode(input.trim())
        .map_err(|e| format!("Base64 decode error: {e}"))
}

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

/// The non-string core schema type named by `tag`, if any.
fn core_tag_type(tag: &str) -> Option<CoreScalarType> {
    let name = tag
        .strip_prefix("!!")
        .or_else(|| tag.strip_prefix("!<tag:yaml.org,2002:")?.strip_suffix('>'))?;
    match name {
        "int" => Some(CoreScalarType::Integer),
        "float" => Some(CoreScalarType::Float),
        "bool" => Some(CoreScalarType::Boolean),
        "null" => Some(CoreScalarType::Null),
        _ => None,
    }
}

/// Whether `value` is a valid spelling of a `t` under the core schema.
fn core_tag_accepts(t: CoreScalarType, value: &str) -> bool {
    match (t, ScalarValue::classify_plain(value)) {
        (CoreScalarType::Null, _) if value.is_empty() => true,
        // The core schema's float pattern also matches integers.
        (CoreScalarType::Float, CoreScalarType::Integer) => true,
        (t, c) => t == c,
    }
}

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

/// Parse a `\xNN`, `\uNNNN` or `\UNNNNNNNN` escape from `chars` (with
/// the leading `\x`/`\u`/`\U` already consumed) and append the resulting
/// character to `out`, falling back to the literal `\<prefix><digits>`
/// when the digits are missing, non-hex, or outside the valid unicode
/// range.
fn push_unicode_escape(
    out: &mut String,
    chars: &mut std::iter::Peekable<std::str::Chars<'_>>,
    prefix: char,
    width: usize,
) {
    let hex_digits: String = chars.by_ref().take(width).collect();
    // from_str_radix would accept a leading '+'
    let decoded = if hex_digits.len() == width && hex_digits.chars().all(|c| c.is_ascii_hexdigit())
    {
        u32::from_str_radix(&hex_digits, 16)
            .ok()
            .and_then(char::from_u32)
    } else {
        None
    };
    match decoded {
        Some(c) => out.push(c),
        None => {
            out.push('\\');
            out.push(prefix);
            out.push_str(&hex_digits);
        }
    }
}

/// Skip the leading white space of the lines following a line break,
/// returning how many of those lines were empty.
fn skip_line_prefixes(chars: &mut std::iter::Peekable<std::str::Chars<'_>>) -> usize {
    let mut empty_lines = 0;
    loop {
        while chars.next_if(|c| *c == ' ' || *c == '\t').is_some() {}
        match chars.next_if(|c| *c == '\n' || *c == '\r') {
            Some('\r') => {
                chars.next_if_eq(&'\n');
            }
            Some(_) => {}
            None => return empty_lines,
        }
        empty_lines += 1;
    }
}

fn all_digits(s: &str, radix: u32) -> bool {
    !s.is_empty() && s.chars().all(|c| c.is_digit(radix))
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

    /// Decode the content of a double-quoted string: process escape
    /// sequences and fold line breaks as described in the YAML spec.
    pub fn parse_escape_sequences(text: &str) -> String {
        let mut result = String::with_capacity(text.len()); // Pre-allocate
        let mut chars = text.chars().peekable();
        // Unescaped white space is dropped if a line break follows it
        let mut pending_white = String::new();

        while let Some(ch) = chars.next() {
            if ch == '\n' || ch == '\r' {
                if ch == '\r' {
                    chars.next_if_eq(&'\n');
                }
                pending_white.clear();
                match skip_line_prefixes(&mut chars) {
                    0 => result.push(' '),
                    n => result.push_str(&"\n".repeat(n)),
                }
                continue;
            }
            if ch == ' ' || ch == '\t' {
                pending_white.push(ch);
                continue;
            }
            result.push_str(&pending_white);
            pending_white.clear();
            if ch == '\\' {
                if let Some(&escaped) = chars.peek() {
                    chars.next(); // consume the escaped character
                    match escaped {
                        // Standard escape sequences
                        'n' => result.push('\n'),
                        't' | '\t' => result.push('\t'),
                        'r' => result.push('\r'),
                        'b' => result.push('\x08'),
                        'f' => result.push('\x0C'),
                        'a' => result.push('\x07'), // bell
                        'e' => result.push('\x1B'), // escape
                        'v' => result.push('\x0B'), // vertical tab
                        '0' => result.push('\0'),   // null
                        '\\' => result.push('\\'),
                        '"' => result.push('"'),
                        '\'' => result.push('\''),
                        '/' => result.push('/'),
                        'N' => result.push('\u{85}'),
                        '_' => result.push('\u{A0}'),
                        'L' => result.push('\u{2028}'),
                        'P' => result.push('\u{2029}'),
                        ' ' => result.push(' '),
                        '\n' | '\r' => {
                            // An escaped line break is dropped, but empty
                            // lines after it each still produce a line feed
                            if escaped == '\r' {
                                chars.next_if_eq(&'\n');
                            }
                            let empty_lines = skip_line_prefixes(&mut chars);
                            result.push_str(&"\n".repeat(empty_lines));
                        }
                        'x' => push_unicode_escape(&mut result, &mut chars, 'x', 2),
                        'u' => push_unicode_escape(&mut result, &mut chars, 'u', 4),
                        'U' => push_unicode_escape(&mut result, &mut chars, 'U', 8),
                        // Unknown escape sequence - preserve as literal
                        _ => {
                            result.push('\\');
                            result.push(escaped);
                        }
                    }
                } else {
                    // Backslash at end of string
                    result.push('\\');
                }
            } else {
                result.push(ch);
            }
        }
        result.push_str(&pending_white);

        result
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

    /// Create a binary scalar from raw bytes
    #[cfg(feature = "base64")]
    pub fn binary(data: &[u8]) -> Self {
        let encoded = base64_encode(data);
        Self {
            value: encoded,
            style: ScalarStyle::Plain,
            scalar_type: ScalarType::Binary,
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

    /// Extract binary data if this is a binary scalar
    #[cfg(feature = "base64")]
    pub fn as_binary(&self) -> Option<Result<Vec<u8>, String>> {
        match self.scalar_type {
            ScalarType::Binary => Some(base64_decode(&self.value)),
            _ => None,
        }
    }

    /// Check if this is a binary scalar
    #[cfg(feature = "base64")]
    pub fn is_binary(&self) -> bool {
        self.scalar_type == ScalarType::Binary
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
                if self.is_valid_timestamp(&self.value) {
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

    /// Parse a YAML 1.2 core schema integer: `[-+]?[0-9]+` (decimal, even
    /// with leading zeros), `0o[0-7]+` or `0x[0-9a-fA-F]+`.
    pub(crate) fn parse_integer(value: &str) -> Option<i64> {
        if !Self::is_core_int(value) {
            return None;
        }
        let (radix, digits, is_negative) = if let Some(hex) = value.strip_prefix("0x") {
            (16, hex, false)
        } else if let Some(oct) = value.strip_prefix("0o") {
            (8, oct, false)
        } else if let Some(dec) = value.strip_prefix('-') {
            (10, dec, true)
        } else {
            (10, value.strip_prefix('+').unwrap_or(value), false)
        };
        // Parse the magnitude wider than i64 so that i64::MIN fits.
        let magnitude = i128::from_str_radix(digits, radix).ok()?;
        i64::try_from(if is_negative { -magnitude } else { magnitude }).ok()
    }

    /// Parse a YAML 1.2 core schema float, including `.inf` and `.nan`.
    pub(crate) fn parse_float(value: &str) -> Option<f64> {
        match value {
            ".inf" | ".Inf" | ".INF" | "+.inf" | "+.Inf" | "+.INF" => Some(f64::INFINITY),
            "-.inf" | "-.Inf" | "-.INF" => Some(f64::NEG_INFINITY),
            ".nan" | ".NaN" | ".NAN" => Some(f64::NAN),
            _ if Self::is_core_float(value) => value.parse::<f64>().ok(),
            _ => None,
        }
    }

    /// Parse a YAML 1.2 core schema boolean.
    pub(crate) fn parse_bool(value: &str) -> Option<bool> {
        match value {
            "true" | "True" | "TRUE" => Some(true),
            "false" | "False" | "FALSE" => Some(false),
            _ => None,
        }
    }

    /// Whether `value` matches the YAML 1.2 core schema `!!int` regex:
    /// `[-+]?[0-9]+`, `0o[0-7]+` or `0x[0-9a-fA-F]+`.
    ///
    /// This is a syntactic check, so it also holds for integers that do
    /// not fit in an `i64`.
    fn is_core_int(value: &str) -> bool {
        if let Some(hex) = value.strip_prefix("0x") {
            return all_digits(hex, 16);
        }
        if let Some(oct) = value.strip_prefix("0o") {
            return all_digits(oct, 8);
        }
        all_digits(value.strip_prefix(['+', '-']).unwrap_or(value), 10)
    }

    /// Whether `value` matches the finite part of the YAML 1.2 core schema
    /// `!!float` regex:
    /// `[-+]?(\.[0-9]+|[0-9]+(\.[0-9]*)?)([eE][-+]?[0-9]+)?`.
    fn is_core_float(value: &str) -> bool {
        let body = value.strip_prefix(['+', '-']).unwrap_or(value);
        let (mantissa, exponent) = match body.find(['e', 'E']) {
            Some(i) => (&body[..i], Some(&body[i + 1..])),
            None => (body, None),
        };
        let mantissa_ok = match mantissa.split_once('.') {
            Some(("", frac)) => all_digits(frac, 10),
            Some((int, frac)) => all_digits(int, 10) && frac.chars().all(|c| c.is_ascii_digit()),
            None => all_digits(mantissa, 10),
        };
        let exponent_ok = match exponent {
            Some(e) => all_digits(e.strip_prefix(['+', '-']).unwrap_or(e), 10),
            None => true,
        };
        mantissa_ok && exponent_ok
    }

    /// Whether `value` is written in YAML 1.1's bare-octal form (a leading
    /// `0` before more digits, as in `0755`).
    ///
    /// YAML 1.2's `!!int` has no such form: `0755` is seven hundred and
    /// fifty five, where 1.1 read it as 493. `0o755` is the 1.2 spelling.
    pub(crate) fn is_legacy_octal(value: &str) -> bool {
        let body = value.strip_prefix(['+', '-']).unwrap_or(value);
        body.len() > 1 && body.starts_with('0') && body.bytes().all(|b| b.is_ascii_digit())
    }

    /// Classify a plain (unquoted) scalar's text per the YAML 1.2 core
    /// schema tag-resolution rules.
    ///
    /// This is the single source of truth for what a plain scalar
    /// resolves to; the lexer and [`from_scalar`](Self::from_scalar)
    /// both delegate here so token-emit-time and CST-inspection-time
    /// classification cannot drift.
    ///
    /// [`auto_detect_type`](Self::auto_detect_type) builds on this and
    /// additionally recognises timestamps.
    pub fn classify_plain(value: &str) -> CoreScalarType {
        match value {
            "true" | "false" | "True" | "False" | "TRUE" | "FALSE" => {
                return CoreScalarType::Boolean
            }
            "null" | "Null" | "NULL" | "~" | "" => return CoreScalarType::Null,
            _ => {}
        }

        if Self::is_core_int(value) {
            return CoreScalarType::Integer;
        }

        // Unlike Rust's f64 parser, the core schema spells infinity and NaN
        // only with a leading dot, and has no `inf`, `nan` or `infinity`.
        if Self::parse_float(value).is_some() {
            return CoreScalarType::Float;
        }

        CoreScalarType::String
    }

    /// Turn the `Display` output of an `f32`/`f64` into a YAML float.
    ///
    /// Maps `NaN`/`inf`/`-inf` to `.nan`/`.inf`/`-.inf` and adds `.0` to
    /// integral values so they don't read back as integers.
    pub(crate) fn format_float(display: String) -> String {
        match display.as_str() {
            "NaN" => ".nan".to_string(),
            "inf" => ".inf".to_string(),
            "-inf" => "-.inf".to_string(),
            _ if display.contains(['.', 'e', 'E']) => display,
            _ => format!("{display}.0"),
        }
    }

    /// Auto-detect the most appropriate scalar type from a string value
    ///
    /// This follows [`classify_plain`](Self::classify_plain), except that
    /// text the core schema reads as a string is checked for a timestamp.
    pub fn auto_detect_type(value: &str) -> ScalarType {
        match Self::classify_plain(value) {
            CoreScalarType::String if Self::is_valid_timestamp_static(value) => {
                ScalarType::Timestamp
            }
            other => other.into(),
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
        } else if raw_text.starts_with('|') {
            ScalarStyle::Literal
        } else if raw_text.starts_with('>') {
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

    /// Create a ScalarValue from a tagged scalar node, keeping its tag and
    /// style.
    ///
    /// The core schema tags `!!int`, `!!float`, `!!bool` and `!!null` set
    /// the type when the content is a valid spelling of it; any other tag, or
    /// content the tag cannot resolve, gives a string.
    pub(crate) fn from_tagged(tagged: &crate::yaml::TaggedNode) -> Option<Self> {
        let tag = tagged.tag();
        let mut scalar = match tagged.value() {
            Some(inner) => Self::from_scalar(&inner),
            // A tag with no content, as in `k: !!null`
            None => Self::plain(tagged.as_string()?),
        };
        scalar.scalar_type = match tag.as_deref().and_then(core_tag_type) {
            Some(t) if core_tag_accepts(t, &scalar.value) => t.into(),
            _ => ScalarType::String,
        };
        scalar.tag = tag;
        Some(scalar)
    }

    /// Basic timestamp format validation
    fn is_valid_timestamp(&self, value: &str) -> bool {
        Self::is_valid_timestamp_static(value)
    }

    /// Static version of timestamp validation
    fn is_valid_timestamp_static(value: &str) -> bool {
        // Basic patterns for common timestamp formats
        // ISO 8601: YYYY-MM-DD or YYYY-MM-DDTHH:MM:SS etc.
        if Self::matches_iso8601_pattern(value) {
            return true;
        }

        // Unix timestamp (seconds since epoch)
        if let Ok(timestamp) = value.parse::<u64>() {
            // Reasonable range: between 1970 and 2100
            return timestamp > 0 && timestamp < 4_102_444_800; // 2100-01-01
        }

        false
    }

    /// Simple pattern matching for ISO 8601 timestamps
    fn matches_iso8601_pattern(value: &str) -> bool {
        let chars: Vec<char> = value.chars().collect();

        // Must be at least YYYY-MM-DD (10 chars)
        if chars.len() < 10 {
            return false;
        }

        // Check YYYY-MM-DD pattern
        if !(chars[0..4].iter().all(|c| c.is_ascii_digit())
            && chars[4] == '-'
            && chars[5..7].iter().all(|c| c.is_ascii_digit())
            && chars[7] == '-'
            && chars[8..10].iter().all(|c| c.is_ascii_digit()))
        {
            return false;
        }

        // Validate month and day ranges (basic validation)
        let month_str: String = chars[5..7].iter().collect();
        let day_str: String = chars[8..10].iter().collect();

        let year_str: String = chars[0..4].iter().collect();
        if let (Ok(year), Ok(month), Ok(day)) = (
            year_str.parse::<u16>(),
            month_str.parse::<u8>(),
            day_str.parse::<u8>(),
        ) {
            let leap = year % 4 == 0 && (year % 100 != 0 || year % 400 == 0);
            let days_in_month = match month {
                2 if leap => 29,
                2 => 28,
                4 | 6 | 9 | 11 => 30,
                _ => 31,
            };
            if !(1..=12).contains(&month) || !(1..=days_in_month).contains(&day) {
                return false;
            }
        } else {
            return false;
        }

        // If it's just YYYY-MM-DD, that's valid
        if chars.len() == 10 {
            return true;
        }

        // Check for time part: T or t or space followed by HH:MM:SS
        if chars.len() >= 19 {
            let sep = chars[10];
            if (sep == 'T' || sep == 't' || sep == ' ')
                && chars[11..13].iter().all(|c| c.is_ascii_digit())
                && chars[13] == ':'
                && chars[14..16].iter().all(|c| c.is_ascii_digit())
                && chars[16] == ':'
                && chars[17..19].iter().all(|c| c.is_ascii_digit())
            {
                // Validate hour, minute, second ranges
                let hour_str: String = chars[11..13].iter().collect();
                let minute_str: String = chars[14..16].iter().collect();
                let second_str: String = chars[17..19].iter().collect();

                if let (Ok(hour), Ok(minute), Ok(second)) = (
                    hour_str.parse::<u8>(),
                    minute_str.parse::<u8>(),
                    second_str.parse::<u8>(),
                ) {
                    if hour > 23 || minute > 59 || second > 59 {
                        return false;
                    }
                } else {
                    return false;
                }

                return Self::is_timestamp_tail(&chars[19..]);
            }
        }

        false
    }

    /// Whether `tail` is a valid fraction and time zone following the
    /// seconds of a timestamp: `(\.[0-9]*)?([ \t]*(Z|[-+][0-9][0-9]?(:[0-9][0-9])?))?`.
    fn is_timestamp_tail(tail: &[char]) -> bool {
        let digits = |s: &[char]| s.iter().all(|c| c.is_ascii_digit());
        let mut rest = tail;
        if let Some(('.', fraction)) = rest.split_first() {
            rest = &fraction[fraction.iter().take_while(|c| c.is_ascii_digit()).count()..];
        }
        if rest.is_empty() {
            return true;
        }
        let zone = &rest[rest.iter().take_while(|c| matches!(c, ' ' | '\t')).count()..];
        match zone {
            ['Z'] => true,
            ['+' | '-', offset @ ..] => {
                let (hour, minute) = match offset.iter().position(|&c| c == ':') {
                    Some(i) => (&offset[..i], &offset[i + 1..]),
                    None => (offset, &['0', '0'][..]),
                };
                matches!(hour.len(), 1 | 2) && digits(hour) && minute.len() == 2 && digits(minute)
            }
            _ => false,
        }
    }

    /// Detect the appropriate style for a value
    fn detect_style(value: &str) -> ScalarStyle {
        // A line break has to be decided first. A quoted scalar folds its
        // breaks into spaces, so `"x\ny\n"` written as `'x\ny\n'` reads back
        // as `x y`, while a literal block keeps them. A literal also answers
        // every reason needs_quoting gives, so nothing multi-line needs
        // quotes. Only double quotes can escape a carriage return or a
        // non-printable character, so those come first.
        if Self::needs_double_quotes(value) {
            return ScalarStyle::DoubleQuoted;
        }
        if value.contains('\n') {
            return ScalarStyle::Literal;
        }
        // Check if value needs quoting
        if Self::needs_quoting(value) {
            // Prefer single quotes if no single quotes in value
            if !value.contains('\'') {
                ScalarStyle::SingleQuoted
            } else {
                ScalarStyle::DoubleQuoted
            }
        } else {
            ScalarStyle::Plain
        }
    }

    /// Whether `value` can only be written as a double-quoted scalar.
    ///
    /// Characters outside the YAML c-printable set (spec 5.1), and the byte
    /// order mark, may only appear escaped. A carriage return is a line
    /// break, which plain, single-quoted and block scalars all normalise,
    /// and NEL, LS and PS are line breaks to YAML 1.1 readers.
    fn needs_double_quotes(value: &str) -> bool {
        value.chars().any(|c| {
            !matches!(c, '\t' | '\n' | '\x20'..='\x7E' | '\u{A0}'..='\u{FFFD}' | '\u{10000}'..)
                || matches!(c, '\u{2028}' | '\u{2029}' | '\u{FEFF}')
        })
    }

    /// Check if a value needs quoting when treated as a string
    fn needs_quoting(value: &str) -> bool {
        // Empty string needs quotes
        if value.is_empty() {
            return true;
        }
        Self::needs_double_quotes(value)
            || Self::resolves_to_non_string(value)
            || Self::needs_quoting_as_tagged(value)
    }

    /// Whether a plain scalar spelling `value` would be read back as
    /// something other than a string when it carries no tag.
    fn resolves_to_non_string(value: &str) -> bool {
        // Anything the core schema resolves to a non-string, such as
        // `.inf` or `.NaN`, has to be quoted to stay a string.
        if Self::classify_plain(value) != CoreScalarType::String {
            return true;
        }

        // Check for YAML keywords that would be misinterpreted
        // These need quotes when we want them as strings
        if value.eq_ignore_ascii_case("true")
            || value.eq_ignore_ascii_case("false")
            || value.eq_ignore_ascii_case("yes")
            || value.eq_ignore_ascii_case("no")
            || value.eq_ignore_ascii_case("on")
            || value.eq_ignore_ascii_case("off")
            || value.eq_ignore_ascii_case("null")
            || value == "~"
        {
            return true;
        }

        // Also quote things that look like numbers to preserve them as strings
        // Radix forms outside the core schema, such as `0b101` or `-0x1F`,
        // are quoted too, as YAML 1.1 readers take them for integers.
        let unsigned = value.strip_prefix(['+', '-']).unwrap_or(value);
        let radix_like = ["0b", "0B", "0o", "0O", "0x", "0X"]
            .iter()
            .any(|p| unsigned.strip_prefix(p).is_some_and(|d| all_digits(d, 16)));
        value.parse::<f64>().is_ok()
            || radix_like
            || Self::classify_plain(value) != CoreScalarType::String
    }

    /// Whether `value` needs quoting even with an explicit tag deciding its
    /// type, because as a plain scalar it would not read back as `value`.
    fn needs_quoting_as_tagged(value: &str) -> bool {
        // Check if starts with special characters. `#` is included because a
        // plain scalar starting with `#` reads as a comment: at column 0 the
        // whole line becomes one, and after whitespace (e.g. `key: #x`) the
        // `#x` becomes a trailing comment and the value is lost.
        if value.starts_with(|ch: char| {
            matches!(
                ch,
                '-' | '?'
                    | ':'
                    | '['
                    | ']'
                    | '{'
                    | '}'
                    | ','
                    | '>'
                    | '<'
                    | '!'
                    | '&'
                    | '*'
                    | '#'
                    | '%'
                    | '@'
                    | '`'
            )
        }) {
            return true;
        }

        // Document terminator (`...`) and start marker (`---`) at
        // column 0 begin a new stream document, so they must be quoted
        // when used as a scalar / key.
        if value.starts_with("...") || value.starts_with("---") {
            return true;
        }

        // Check for special characters that require quoting. `:` and `#` are
        // only ambiguous in context, and the context differs between them:
        // a `:` ends a key when followed by whitespace or end of input, while
        // a `#` starts a comment when *preceded* by whitespace (YAML 1.2
        // 6.6). So `a#b` is a fine plain scalar but `a #b` is not.
        let mut prev: Option<char> = None;
        let mut chars = value.chars().peekable();
        while let Some(ch) = chars.next() {
            match ch {
                '&' | '*' | '!' | '|' | '\'' | '"' | '%' => return true,
                ':' if chars.peek().map_or(true, |next| next.is_whitespace()) => {
                    return true;
                }
                '#' if prev.map_or(true, char::is_whitespace) => {
                    return true;
                }
                _ => {}
            }
            prev = Some(ch);
        }

        // Leading/trailing whitespace needs quotes
        if value != value.trim() {
            return true;
        }

        false
    }

    /// Render the scalar as a YAML string with proper escaping.
    ///
    /// A block scalar's body is indented two spaces. Use
    /// [`to_yaml_string_with_indent`](Self::to_yaml_string_with_indent) where
    /// the value sits inside an indented entry, whose body has to clear that
    /// entry's own column.
    pub fn to_yaml_string(&self) -> String {
        self.to_yaml_string_with_indent(2)
    }

    /// As [`to_yaml_string`](Self::to_yaml_string), indenting a block
    /// scalar's body by `block_indent` spaces.
    pub fn to_yaml_string_with_indent(&self, block_indent: usize) -> String {
        // Special data types carry their tag unless an explicit one is set
        let tag = self.tag.as_deref().or(match self.scalar_type {
            #[cfg(feature = "base64")]
            ScalarType::Binary => Some("!!binary"),
            ScalarType::Timestamp => Some("!!timestamp"),
            ScalarType::Regex => Some("!!regex"),
            _ => None,
        });
        let content = self.render_content(block_indent);
        match tag {
            Some(tag) if content.is_empty() => tag.to_string(),
            Some(tag) => format!("{tag} {content}"),
            None => content,
        }
    }

    /// Render the scalar without any tag.
    fn render_content(&self, block_indent: usize) -> String {
        match self.style {
            ScalarStyle::Plain => {
                // Check if we need to quote based on type vs content
                match self.scalar_type {
                    // With an explicit tag, the content can stay plain even
                    // where it would otherwise resolve to another type.
                    ScalarType::String if self.tag.is_some() => {
                        if Self::needs_double_quotes(&self.value) {
                            self.to_double_quoted()
                        } else if Self::needs_quoting_as_tagged(&self.value) {
                            self.to_single_quoted()
                        } else {
                            self.value.clone()
                        }
                    }
                    ScalarType::String => {
                        // For strings, quote if the content looks like a special value
                        if Self::needs_double_quotes(&self.value) {
                            self.to_double_quoted()
                        } else if Self::needs_quoting(&self.value) {
                            self.to_single_quoted()
                        } else {
                            self.value.clone()
                        }
                    }
                    // For non-strings, output as plain (unquoted)
                    ScalarType::Integer
                    | ScalarType::Float
                    | ScalarType::Boolean
                    | ScalarType::Null
                    | ScalarType::Timestamp
                    | ScalarType::Regex => self.value.clone(),
                    #[cfg(feature = "base64")]
                    ScalarType::Binary => self.value.clone(),
                }
            }
            ScalarStyle::SingleQuoted => self.to_single_quoted(),
            ScalarStyle::DoubleQuoted => self.to_double_quoted(),
            ScalarStyle::Literal => self.to_literal_with_indent(block_indent),
            ScalarStyle::Folded => self.to_folded_with_indent(block_indent),
        }
    }

    /// Convert to single-quoted string
    fn to_single_quoted(&self) -> String {
        // A single-quoted scalar folds a raw line break into a space, so a
        // lone `\r` can only be written escaped.
        if self.value.contains('\r') {
            return self.to_double_quoted();
        }
        // Escape single quotes by doubling them
        let escaped = self.value.replace('\'', "''");
        format!("'{escaped}'")
    }

    /// Convert to double-quoted string
    fn to_double_quoted(&self) -> String {
        let mut result = String::from("\"");
        for ch in self.value.chars() {
            match ch {
                '"' => result.push_str("\\\""),
                '\\' => result.push_str("\\\\"),
                '\n' => result.push_str("\\n"),
                '\r' => result.push_str("\\r"),
                '\t' => result.push_str("\\t"),
                '\x08' => result.push_str("\\b"),
                '\x0C' => result.push_str("\\f"),
                '\x07' => result.push_str("\\a"), // bell
                '\x1B' => result.push_str("\\e"), // escape
                '\x0B' => result.push_str("\\v"), // vertical tab
                '\0' => result.push_str("\\0"),   // null
                c if c.is_control() || (c as u32) > 0x7F => {
                    // Handle Unicode characters and control characters
                    let code_point = c as u32;
                    if code_point <= 0xFF {
                        result.push_str(&format!("\\x{code_point:02X}"));
                    } else if code_point <= 0xFFFF {
                        result.push_str(&format!("\\u{code_point:04X}"));
                    } else {
                        result.push_str(&format!("\\U{code_point:08X}"));
                    }
                }
                c => result.push(c),
            }
        }
        result.push('"');
        result
    }

    /// Convert to literal block scalar with specific indentation
    pub fn to_literal_with_indent(&self, indent: usize) -> String {
        self.to_block_with_indent('|', indent)
    }

    /// Convert to folded block scalar with specific indentation
    pub fn to_folded_with_indent(&self, indent: usize) -> String {
        self.to_block_with_indent('>', indent)
    }

    /// Render as a block scalar introduced by `marker` (`|` literal, `>` folded).
    fn to_block_with_indent(&self, marker: char, indent: usize) -> String {
        // Pick the chomping indicator that preserves the value's trailing
        // line breaks: a bare `|` clips to exactly one, `|-` strips them all
        // and `|+` keeps them. Otherwise reading the value back gains or
        // loses a newline.
        let trailing = self.value.len() - self.value.trim_end_matches('\n').len();
        let chomp = match trailing {
            0 => "-",
            1 => "",
            _ => "+",
        };
        // Content that already carries consistent indentation is preserved.
        // That indentation would be read as the block's own and stripped on
        // the way back in, though, losing the value's leading spaces, so say
        // where the content really starts: `|2` for `"  x\n  y"`.
        if let Some(content_indent) = self.detect_content_indentation() {
            if content_indent > 0 {
                // The indicator counts from the block's own column, so the
                // body has to sit that much further right for the value's
                // leading spaces to survive.
                let body = " ".repeat(indent);
                let shifted = self
                    .value
                    .lines()
                    .map(|line| {
                        if line.is_empty() {
                            String::new()
                        } else {
                            format!("{body}{line}")
                        }
                    })
                    .collect::<Vec<_>>()
                    .join("\n");
                return format!("{marker}{indent}{chomp}\n{shifted}");
            }
            return format!("{marker}{chomp}\n{}", self.value);
        }
        let indent_str = " ".repeat(indent);
        let indented = self
            .value
            .lines()
            .map(|line| {
                if line.trim().is_empty() {
                    String::new()
                } else {
                    format!("{indent_str}{line}")
                }
            })
            .collect::<Vec<_>>()
            .join("\n");
        format!("{marker}{chomp}\n{indented}")
    }

    /// Detect the minimum indentation level of non-empty lines in the content
    fn detect_content_indentation(&self) -> Option<usize> {
        let non_empty_lines: Vec<&str> = self
            .value
            .lines()
            .filter(|line| !line.trim().is_empty())
            .collect();

        if non_empty_lines.is_empty() {
            return None;
        }

        let mut min_indent = None;
        let mut all_have_same_indent = true;

        for line in non_empty_lines {
            let indent = line.len() - line.trim_start().len();
            match min_indent {
                None => min_indent = Some(indent),
                Some(current_min) => {
                    if indent != current_min {
                        all_have_same_indent = false;
                    }
                    min_indent = Some(current_min.min(indent));
                }
            }
        }

        // Only preserve indentation if all lines have some consistent structure
        if all_have_same_indent && min_indent.unwrap_or(0) > 0 {
            min_indent
        } else {
            None
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

impl From<crate::yaml::Scalar> for ScalarValue {
    fn from(scalar: crate::yaml::Scalar) -> Self {
        let value = scalar.as_string();
        ScalarValue::parse(&value)
    }
}

impl ScalarValue {
    /// The token kind for `text`, this scalar as rendered.
    fn token_kind(&self, text: &str) -> crate::lex::SyntaxKind {
        use crate::lex::SyntaxKind;
        if text.starts_with('\'') || text.starts_with('"') {
            return SyntaxKind::STRING;
        }
        match self.scalar_type {
            ScalarType::Integer => SyntaxKind::INT,
            ScalarType::Float => SyntaxKind::FLOAT,
            ScalarType::Boolean => SyntaxKind::BOOL,
            ScalarType::Null => SyntaxKind::NULL,
            _ => SyntaxKind::STRING,
        }
    }
}

impl crate::AsYaml for ScalarValue {
    fn as_node(&self) -> Option<&crate::yaml::SyntaxNode> {
        None
    }

    fn kind(&self) -> crate::as_yaml::YamlKind {
        match &self.tag {
            Some(tag) => crate::as_yaml::YamlKind::Tagged(std::borrow::Cow::Owned(tag.clone())),
            None => crate::as_yaml::YamlKind::Scalar,
        }
    }

    fn build_content(
        &self,
        builder: &mut rowan::GreenNodeBuilder,
        indent: usize,
        _flow_context: bool,
    ) -> bool {
        use crate::lex::SyntaxKind;
        if let Some(tag) = &self.tag {
            builder.start_node(SyntaxKind::TAGGED_NODE.into());
            builder.token(SyntaxKind::TAG.into(), tag);
            let content = self.render_content(indent + 2);
            // Like the parser, give a tag with no content an empty scalar.
            let kind = if content.is_empty() {
                SyntaxKind::NULL
            } else {
                builder.token(SyntaxKind::WHITESPACE.into(), " ");
                self.token_kind(&content)
            };
            builder.start_node(SyntaxKind::SCALAR.into());
            builder.token(kind.into(), &content);
            builder.finish_node();
            builder.finish_node();
            return false;
        }
        // Render with proper YAML quoting/escaping. When the value needs
        // quotes (e.g. the string "null", or text containing ": "), the
        // rendered text starts with a quote and must be tokenized as a
        // STRING regardless of the semantic type.
        // A block scalar's body has to clear the column its entry sits at,
        // or the text it renders to is not the value it stood for:
        // `a:\n  b: |-\n  x\n` has an empty scalar and a stray `x`.
        let text = self.to_yaml_string_with_indent(indent + 2);
        builder.start_node(SyntaxKind::SCALAR.into());
        builder.token(self.token_kind(&text).into(), &text);
        builder.finish_node();
        false
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
    fn round_trips_keys_needing_quotes() {
        use crate::Document;
        use std::str::FromStr;

        // Each of these was written unquoted and could not be read back:
        // " #" starts a comment (the old rule checked the character *after*
        // the `#`, but YAML keys on what precedes it), and a line break
        // turned the key into a block scalar.
        for key in [
            "a. #a", "a #c", "a#b", "a\nb", "a\rb", "x: y", "a:b", "plain", "a b", "#lead", "a:",
            "", "true", "123", "- x", "? x",
        ] {
            let doc = Document::from_str("z: 1\n").unwrap();
            doc.as_mapping().unwrap().set(key, "v");
            let text = doc.to_string();

            let reparsed = Document::from_str(&text)
                .unwrap_or_else(|e| panic!("{key:?} produced unparseable {text:?}: {e}"));
            let value = reparsed
                .as_mapping()
                .and_then(|m| m.get(key))
                .unwrap_or_else(|| panic!("{key:?} did not survive as {text:?}"));
            assert_eq!(value.to_string(), "v", "{key:?} in {text:?}");
        }
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
    fn test_escape_sequences_basic() {
        // Test basic escape sequences
        assert_eq!(
            ScalarValue::parse_escape_sequences("hello\\nworld"),
            "hello\nworld"
        );
        assert_eq!(
            ScalarValue::parse_escape_sequences("tab\\there"),
            "tab\there"
        );
        assert_eq!(
            ScalarValue::parse_escape_sequences("quote\\\"test"),
            "quote\"test"
        );
        assert_eq!(
            ScalarValue::parse_escape_sequences("back\\\\slash"),
            "back\\slash"
        );
        assert_eq!(
            ScalarValue::parse_escape_sequences("return\\rtest"),
            "return\rtest"
        );
    }

    #[test]
    fn test_escape_sequences_control_chars() {
        // Test control character escapes
        assert_eq!(ScalarValue::parse_escape_sequences("bell\\a"), "bell\x07");
        assert_eq!(
            ScalarValue::parse_escape_sequences("backspace\\b"),
            "backspace\x08"
        );
        assert_eq!(
            ScalarValue::parse_escape_sequences("formfeed\\f"),
            "formfeed\x0C"
        );
        assert_eq!(
            ScalarValue::parse_escape_sequences("escape\\e"),
            "escape\x1B"
        );
        assert_eq!(ScalarValue::parse_escape_sequences("vtab\\v"), "vtab\x0B");
        assert_eq!(ScalarValue::parse_escape_sequences("null\\0"), "null\0");
        assert_eq!(ScalarValue::parse_escape_sequences("slash\\/"), "slash/");
    }

    #[test]
    fn test_escape_sequences_unicode_x() {
        // Test \xNN escape sequences
        assert_eq!(ScalarValue::parse_escape_sequences("\\x41"), "A"); // 0x41 = 'A'
        assert_eq!(ScalarValue::parse_escape_sequences("\\x7A"), "z"); // 0x7A = 'z'
        assert_eq!(ScalarValue::parse_escape_sequences("\\x20"), " "); // 0x20 = space
        assert_eq!(ScalarValue::parse_escape_sequences("\\xFF"), "\u{FF}"); // 0xFF = ÿ

        // Test invalid hex sequences
        assert_eq!(ScalarValue::parse_escape_sequences("\\xGH"), "\\xGH"); // Invalid hex
        assert_eq!(ScalarValue::parse_escape_sequences("\\x4"), "\\x4"); // Incomplete
        assert_eq!(ScalarValue::parse_escape_sequences("\\x4Z"), "\\x4Z");
        assert_eq!(ScalarValue::parse_escape_sequences("\\xAZ"), "\\xAZ");
        assert_eq!(ScalarValue::parse_escape_sequences("\\x+4"), "\\x+4");
        assert_eq!(ScalarValue::parse_escape_sequences("\\u+041"), "\\u+041");
    }

    #[test]
    fn test_escape_sequences_unicode_u() {
        // Test \uNNNN escape sequences
        assert_eq!(ScalarValue::parse_escape_sequences("\\u0041"), "A"); // 0x0041 = 'A'
        assert_eq!(ScalarValue::parse_escape_sequences("\\u03B1"), "α"); // Greek alpha
        assert_eq!(ScalarValue::parse_escape_sequences("\\u2603"), "☃"); // Snowman
        assert_eq!(ScalarValue::parse_escape_sequences("\\u4E2D"), "中"); // Chinese character

        // Test invalid sequences
        assert_eq!(ScalarValue::parse_escape_sequences("\\uGHIJ"), "\\uGHIJ"); // Invalid hex
        assert_eq!(ScalarValue::parse_escape_sequences("\\u041"), "\\u041"); // Incomplete
    }

    #[test]
    fn test_escape_sequences_unicode_capital_u() {
        // Test \UNNNNNNNN escape sequences
        assert_eq!(ScalarValue::parse_escape_sequences("\\U00000041"), "A"); // 0x00000041 = 'A'
        assert_eq!(ScalarValue::parse_escape_sequences("\\U0001F603"), "😃"); // Smiley emoji
        assert_eq!(ScalarValue::parse_escape_sequences("\\U0001F4A9"), "💩"); // Pile of poo emoji

        // Test invalid sequences
        assert_eq!(
            ScalarValue::parse_escape_sequences("\\UGHIJKLMN"),
            "\\UGHIJKLMN"
        ); // Invalid hex
        assert_eq!(
            ScalarValue::parse_escape_sequences("\\U0000004"),
            "\\U0000004"
        ); // Incomplete
        assert_eq!(
            ScalarValue::parse_escape_sequences("\\UFFFFFFFF"),
            "\\UFFFFFFFF"
        ); // Invalid code point
    }

    #[test]
    fn test_escape_sequences_line_folding() {
        // An escaped space is content; the raw line break after it folds
        assert_eq!(
            ScalarValue::parse_escape_sequences("line\\ \nfolding"),
            "line  folding"
        );
        assert_eq!(ScalarValue::parse_escape_sequences("a\\\n  b"), "ab");
        assert_eq!(ScalarValue::parse_escape_sequences("a\\\r\n  b"), "ab");
        assert_eq!(
            ScalarValue::parse_escape_sequences("escaped\\nline\\nbreak"),
            "escaped\nline\nbreak"
        );
        assert_eq!(
            ScalarValue::parse_escape_sequences("remove\\\nline\\nbreak"),
            "removeline\nbreak"
        );
    }

    #[test]
    fn test_escape_sequences_mixed() {
        // Test mixed escape sequences
        let input = "Hello\\nWorld\\u0021\\x20\\U0001F44D";
        let expected = "Hello\nWorld! 👍";
        assert_eq!(ScalarValue::parse_escape_sequences(input), expected);

        // Test with quotes and backslashes
        let input = "Quote\\\"back\\\\slash\\ttab";
        let expected = "Quote\"back\\slash\ttab";
        assert_eq!(ScalarValue::parse_escape_sequences(input), expected);
    }

    #[test]
    fn test_escape_sequences_yaml_specific() {
        assert_eq!(
            ScalarValue::parse_escape_sequences("\\N\\_\\L\\P\\\t"),
            "\u{85}\u{A0}\u{2028}\u{2029}\t"
        );
    }

    #[test]
    fn test_escape_sequences_unknown() {
        // Test unknown escape sequences are preserved
        assert_eq!(ScalarValue::parse_escape_sequences("\\q"), "\\q");
        assert_eq!(ScalarValue::parse_escape_sequences("\\z"), "\\z");
        assert_eq!(ScalarValue::parse_escape_sequences("\\1"), "\\1");
    }

    #[test]
    fn test_indentation_preservation() {
        // Test preserving exact indentation in block scalars
        let content_with_indent = "  Line 1\n    Line 2 more indented\n  Line 3";
        let scalar = ScalarValue::literal(content_with_indent);

        // Should detect that content already has indentation and preserve it
        let yaml_output = scalar.to_literal_with_indent(2);
        assert_eq!(
            yaml_output,
            "|-\n    Line 1\n      Line 2 more indented\n    Line 3"
        );
    }

    #[test]
    fn test_indentation_detection() {
        // Test content with consistent indentation
        let consistent_content = "  Line 1\n  Line 2\n  Line 3";
        let scalar1 = ScalarValue::literal(consistent_content);
        assert_eq!(scalar1.detect_content_indentation(), Some(2));

        // Test content with no indentation
        let no_indent_content = "Line 1\nLine 2\nLine 3";
        let scalar2 = ScalarValue::literal(no_indent_content);
        assert_eq!(scalar2.detect_content_indentation(), None);

        // Test content with inconsistent indentation
        let inconsistent_content = "  Line 1\n    Line 2\n Line 3";
        let scalar3 = ScalarValue::literal(inconsistent_content);
        assert_eq!(scalar3.detect_content_indentation(), None);

        // Test empty content
        let empty_content = "";
        let scalar4 = ScalarValue::literal(empty_content);
        assert_eq!(scalar4.detect_content_indentation(), None);

        // Test content with only whitespace lines
        let whitespace_content = "  Line 1\n\n  Line 3";
        let scalar5 = ScalarValue::literal(whitespace_content);
        assert_eq!(scalar5.detect_content_indentation(), Some(2));
    }

    #[test]
    fn test_literal_with_custom_indent() {
        // Test applying custom indentation to unindented content
        let content = "Line 1\nLine 2\nLine 3";
        let scalar = ScalarValue::literal(content);

        let yaml_4_spaces = scalar.to_literal_with_indent(4);
        assert_eq!(yaml_4_spaces, "|-\n    Line 1\n    Line 2\n    Line 3");

        let yaml_1_space = scalar.to_literal_with_indent(1);
        assert_eq!(yaml_1_space, "|-\n Line 1\n Line 2\n Line 3");
    }

    #[test]
    fn test_folded_with_custom_indent() {
        // Test applying custom indentation to folded scalars
        let content = "Line 1\nLine 2\nLine 3";
        let scalar = ScalarValue::folded(content);

        let yaml_3_spaces = scalar.to_folded_with_indent(3);
        assert_eq!(yaml_3_spaces, ">-\n   Line 1\n   Line 2\n   Line 3");
    }

    #[test]
    fn test_mixed_empty_lines_preservation() {
        // Test handling of empty lines in block scalars
        let content_with_empty_lines = "Line 1\n\nLine 3\n\n\nLine 6";
        let scalar = ScalarValue::literal(content_with_empty_lines);

        let yaml_output = scalar.to_literal_with_indent(2);
        assert_eq!(yaml_output, "|-\n  Line 1\n\n  Line 3\n\n\n  Line 6");

        // Empty lines should remain empty (no indentation added)
        // Input has 3 empty lines; they should appear unchanged in the output
        let lines: Vec<&str> = yaml_output.lines().collect();
        let empty_line_count = lines.iter().filter(|line| line.is_empty()).count();
        assert_eq!(empty_line_count, 3);
    }

    #[test]
    fn test_escape_sequences_edge_cases() {
        // Test edge cases
        assert_eq!(ScalarValue::parse_escape_sequences(""), "");
        assert_eq!(ScalarValue::parse_escape_sequences("\\"), "\\");
        assert_eq!(
            ScalarValue::parse_escape_sequences("no escapes"),
            "no escapes"
        );
        assert_eq!(ScalarValue::parse_escape_sequences("\\\\\\\\"), "\\\\");
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
    #[cfg(feature = "base64")]
    fn test_binary_data_encoding() {
        // Test creating binary scalar from raw bytes
        let data = b"Hello, World!";
        let scalar = ScalarValue::binary(data);

        assert!(scalar.is_binary());
        assert_eq!(scalar.scalar_type(), ScalarType::Binary);

        // Should produce valid base64
        let yaml_output = scalar.to_yaml_string();
        assert!(yaml_output.starts_with("!!binary "));

        // Should be able to decode back to original data
        if let Some(decoded_result) = scalar.as_binary() {
            let decoded = decoded_result.expect("Should decode successfully");
            assert_eq!(decoded, data);
        } else {
            panic!("Should be able to extract binary data");
        }
    }

    #[test]
    #[cfg(feature = "base64")]
    fn test_base64_encoding_decoding() {
        // Test various byte sequences
        let test_cases = [
            b"".as_slice(),
            b"A",
            b"AB",
            b"ABC",
            b"ABCD",
            b"Hello, World!",
            &[0, 1, 2, 3, 255, 254, 253],
        ];

        for data in test_cases {
            let encoded = base64_encode(data);
            let decoded = base64_decode(&encoded).expect("Should decode successfully");
            assert_eq!(decoded, data, "Failed for data: {:?}", data);
        }
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
    fn test_timestamp_pattern_matching() {
        // Valid patterns
        assert!(ScalarValue::matches_iso8601_pattern("2023-12-25"));
        assert!(ScalarValue::matches_iso8601_pattern("2023-12-25T10:30:45"));
        assert!(ScalarValue::matches_iso8601_pattern("2023-12-25t10:30:45")); // Lowercase t
        assert!(ScalarValue::matches_iso8601_pattern("2023-12-25 10:30:45"));
        assert!(ScalarValue::matches_iso8601_pattern("2023-01-01T00:00:00"));
        assert!(ScalarValue::matches_iso8601_pattern(
            "2001-12-14t21:59:43.10-05:00"
        )); // Complex with lowercase t

        // Invalid patterns
        assert!(!ScalarValue::matches_iso8601_pattern("2023-13-25")); // Invalid month
        assert!(!ScalarValue::matches_iso8601_pattern("23-12-25")); // Wrong year format
        assert!(!ScalarValue::matches_iso8601_pattern("2023/12/25")); // Wrong separator
        assert!(!ScalarValue::matches_iso8601_pattern("not-a-date"));
        assert!(!ScalarValue::matches_iso8601_pattern("2023"));
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
    fn test_unix_timestamp_bounds() {
        // Unix epoch boundary: 0 is rejected, 1 accepted, and the upper bound
        // (2100-01-01) is exclusive.
        assert!(!ScalarValue::is_valid_timestamp_static("0"));
        assert!(ScalarValue::is_valid_timestamp_static("1"));
        assert!(ScalarValue::is_valid_timestamp_static("4102444799"));
        assert!(!ScalarValue::is_valid_timestamp_static("4102444800"));

        // The instance method delegates to the static one.
        let s = ScalarValue::string("x");
        assert!(s.is_valid_timestamp("1"));
        assert!(!s.is_valid_timestamp("0"));
    }

    #[test]
    fn test_iso8601_time_component_bounds() {
        // Hour/minute/second upper bounds: 23:59:59 is valid, anything past is not.
        assert!(ScalarValue::matches_iso8601_pattern("2023-01-01T23:59:59"));
        assert!(!ScalarValue::matches_iso8601_pattern("2023-01-01T24:00:00"));
        assert!(!ScalarValue::matches_iso8601_pattern("2023-01-01T10:60:00"));
        assert!(!ScalarValue::matches_iso8601_pattern("2023-01-01T10:30:60"));

        // Day bounds: 31 is valid, 0 and 32 are not.
        assert!(ScalarValue::matches_iso8601_pattern("2023-01-31"));
        assert!(!ScalarValue::matches_iso8601_pattern("2023-01-00"));
        assert!(!ScalarValue::matches_iso8601_pattern("2023-01-32"));

        // A malformed time separator falls through to invalid.
        assert!(!ScalarValue::matches_iso8601_pattern("2023-01-01X10:30:45"));
        // Time present but seconds field non-numeric.
        assert!(!ScalarValue::matches_iso8601_pattern("2023-01-01T10:30:xx"));

        // The two date separators must each be '-'. A non-dash in either
        // position must reject even though the digit groups parse as valid
        // month/day. This pins the separator conjuncts in the format check.
        assert!(!ScalarValue::matches_iso8601_pattern("2023X12-25"));
        assert!(!ScalarValue::matches_iso8601_pattern("2023-12X25"));
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
    fn test_parse_escape_single_quote() {
        // The \' escape decodes to a bare single quote.
        assert_eq!(ScalarValue::parse_escape_sequences("a\\'b"), "a'b");
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
        // followed by the content indented by two spaces.
        let s = ScalarValue::with_style("line one\nline two", ScalarStyle::Literal);
        assert_eq!(s.to_yaml_string(), "|-\n  line one\n  line two");

        let s = ScalarValue::with_style("line one\nline two", ScalarStyle::Folded);
        assert_eq!(s.to_yaml_string(), ">-\n  line one\n  line two");
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
    fn test_as_yaml_quotes_string_that_looks_special() {
        use std::str::FromStr;

        // A string whose value reads as a null/bool/number must be quoted so it
        // round-trips as a string, not the special value.
        let doc = crate::yaml::Document::from_str("key: x\n").unwrap();
        doc.as_mapping()
            .unwrap()
            .set("key", ScalarValue::string("null"));
        assert_eq!(doc.to_string(), "key: 'null'\n");

        // A string containing ": " must be quoted or it produces invalid YAML.
        let doc = crate::yaml::Document::from_str("key: x\n").unwrap();
        doc.as_mapping()
            .unwrap()
            .set("key", ScalarValue::string("has: colon"));
        assert_eq!(doc.to_string(), "key: 'has: colon'\n");

        // An actual null scalar stays bare.
        let doc = crate::yaml::Document::from_str("key: x\n").unwrap();
        doc.as_mapping().unwrap().set("key", ScalarValue::null());
        assert_eq!(doc.to_string(), "key: null\n");
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
    fn test_empty_plain_scalar_is_null() {
        assert_eq!(ScalarValue::classify_plain(""), CoreScalarType::Null);

        let doc: crate::yaml::Document = "a:\nb: 1\n".parse().unwrap();
        let node = doc.as_mapping().unwrap().get("a").unwrap();
        let value = ScalarValue::from_scalar(node.as_scalar().unwrap());
        assert_eq!(value.scalar_type(), ScalarType::Null);

        let doc: crate::yaml::Document = "x: 1\n".parse().unwrap();
        doc.as_mapping().unwrap().set("k", ScalarValue::parse(""));
        let reparsed: crate::yaml::Document = doc.to_string().parse().unwrap();
        let node = reparsed.as_mapping().unwrap().get("k").unwrap();
        let value = ScalarValue::from_scalar(node.as_scalar().unwrap());
        assert_eq!(value.scalar_type(), ScalarType::Null);
    }

    #[test]
    fn test_timestamp_rejects_impossible_dates_and_trailing_text() {
        for text in [
            "2023-02-30",
            "2023-02-29",
            "2023-04-31",
            "2100-02-29",
            "2023-01-01T10:00:00garbage",
            "2023-01-01T10:00:00Zx",
            "2023-01-01T10:00:00+",
            "2023-01-01T10:00:00+01:0",
        ] {
            assert!(!ScalarValue::matches_iso8601_pattern(text), "{text}");
            assert_eq!(
                ScalarValue::auto_detect_type(text),
                ScalarType::String,
                "{text}"
            );
        }
        for text in [
            "2024-02-29",
            "2000-02-29",
            "2023-01-01T10:00:00Z",
            "2023-01-01T10:00:00.123456Z",
            "2023-01-01T10:00:00.5+01:00",
            "2001-12-14 21:59:43.10 -5",
            "2001-12-14t21:59:43.10-05:00",
        ] {
            assert!(ScalarValue::matches_iso8601_pattern(text), "{text}");
        }
    }
}
