//! Scalar value wrapper with proper escaping and style support.

use std::fmt;

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

/// Type of a scalar value
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
}

impl ScalarValue {
    /// Create a new scalar value with automatic style detection
    pub fn new(value: impl Into<String>) -> Self {
        let value = value.into();
        let style = Self::detect_style(&value);
        // Detect the type - default to String for user-provided values
        let scalar_type = ScalarType::String;
        Self {
            value,
            style,
            scalar_type,
        }
    }

    /// Parse escape sequences in a double-quoted string
    pub fn parse_escape_sequences(text: &str) -> String {
        let mut result = String::new();
        let mut chars = text.chars().peekable();

        while let Some(ch) = chars.next() {
            if ch == '\\' {
                if let Some(&escaped) = chars.peek() {
                    chars.next(); // consume the escaped character
                    match escaped {
                        // Standard escape sequences
                        'n' => result.push('\n'),
                        't' => result.push('\t'),
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
                        // Line break escape (YAML specific)
                        ' ' => {
                            // Escaped space followed by line break - line folding
                            if let Some(&'\n') = chars.peek() {
                                chars.next(); // consume the newline
                                              // In YAML, escaped line breaks are folded to nothing
                                continue;
                            } else {
                                result.push(' ');
                            }
                        }
                        '\n' => {
                            // Escaped line break - removes the line break
                            continue;
                        }
                        // Unicode escapes
                        'x' => {
                            // \xNN - 2-digit hex
                            let hex_digits: String = chars.by_ref().take(2).collect();
                            if hex_digits.len() == 2 {
                                if let Ok(code) = u8::from_str_radix(&hex_digits, 16) {
                                    result.push(code as char);
                                } else {
                                    // Invalid hex, keep literal
                                    result.push('\\');
                                    result.push('x');
                                    result.push_str(&hex_digits);
                                }
                            } else {
                                // Incomplete hex escape
                                result.push('\\');
                                result.push('x');
                                result.push_str(&hex_digits);
                            }
                        }
                        'u' => {
                            // \uNNNN - 4-digit hex
                            let hex_digits: String = chars.by_ref().take(4).collect();
                            if hex_digits.len() == 4 {
                                if let Ok(code) = u16::from_str_radix(&hex_digits, 16) {
                                    if let Some(unicode_char) = char::from_u32(code as u32) {
                                        result.push(unicode_char);
                                    } else {
                                        // Invalid Unicode code point
                                        result.push('\\');
                                        result.push('u');
                                        result.push_str(&hex_digits);
                                    }
                                } else {
                                    // Invalid hex
                                    result.push('\\');
                                    result.push('u');
                                    result.push_str(&hex_digits);
                                }
                            } else {
                                // Incomplete hex escape
                                result.push('\\');
                                result.push('u');
                                result.push_str(&hex_digits);
                            }
                        }
                        'U' => {
                            // \UNNNNNNNN - 8-digit hex
                            let hex_digits: String = chars.by_ref().take(8).collect();
                            if hex_digits.len() == 8 {
                                if let Ok(code) = u32::from_str_radix(&hex_digits, 16) {
                                    if let Some(unicode_char) = char::from_u32(code) {
                                        result.push(unicode_char);
                                    } else {
                                        // Invalid Unicode code point
                                        result.push('\\');
                                        result.push('U');
                                        result.push_str(&hex_digits);
                                    }
                                } else {
                                    // Invalid hex
                                    result.push('\\');
                                    result.push('U');
                                    result.push_str(&hex_digits);
                                }
                            } else {
                                // Incomplete hex escape
                                result.push('\\');
                                result.push('U');
                                result.push_str(&hex_digits);
                            }
                        }
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

        result
    }

    /// Create a new scalar with a specific style
    pub fn with_style(value: impl Into<String>, style: ScalarStyle) -> Self {
        Self {
            value: value.into(),
            style,
            scalar_type: ScalarType::String,
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
        }
    }

    /// Get the raw value
    pub fn value(&self) -> &str {
        &self.value
    }

    /// Get the style
    pub fn style(&self) -> ScalarStyle {
        self.style
    }

    /// Detect the appropriate style for a value
    fn detect_style(value: &str) -> ScalarStyle {
        // Check if value needs quoting
        if Self::needs_quoting(value) {
            // Prefer single quotes if no single quotes in value
            if !value.contains('\'') {
                ScalarStyle::SingleQuoted
            } else {
                ScalarStyle::DoubleQuoted
            }
        } else if value.contains('\n') {
            // Multi-line strings use literal style
            ScalarStyle::Literal
        } else {
            ScalarStyle::Plain
        }
    }

    /// Check if a value needs quoting when treated as a string
    fn needs_quoting(value: &str) -> bool {
        // Empty string needs quotes
        if value.is_empty() {
            return true;
        }

        // Check for YAML keywords that would be misinterpreted
        // These need quotes when we want them as strings
        let lowercase = value.to_lowercase();
        if matches!(
            lowercase.as_str(),
            "true" | "false" | "yes" | "no" | "on" | "off" | "null" | "~"
        ) {
            return true;
        }

        // Also quote things that look like numbers to preserve them as strings
        if value.parse::<f64>().is_ok() || value.parse::<i64>().is_ok() {
            return true;
        }

        // Check for special characters that require quoting
        for ch in value.chars() {
            match ch {
                ':' | '#' | '&' | '*' | '!' | '|' | '>' | '\'' | '"' | '%' | '@' | '`' => {
                    return true
                }
                _ => {}
            }
        }

        // Check if starts with special characters
        if let Some(first) = value.chars().next() {
            if matches!(first, '-' | '?' | '[' | ']' | '{' | '}' | ',') {
                return true;
            }
        }

        // Leading/trailing whitespace needs quotes
        if value != value.trim() {
            return true;
        }

        false
    }

    /// Render the scalar as a YAML string with proper escaping
    pub fn to_yaml_string(&self) -> String {
        match self.style {
            ScalarStyle::Plain => {
                // Check if we need to quote based on type vs content
                match self.scalar_type {
                    ScalarType::String => {
                        // For strings, quote if the content looks like a special value
                        if Self::needs_quoting(&self.value) {
                            self.to_single_quoted()
                        } else {
                            self.value.clone()
                        }
                    }
                    // For non-strings, output as plain (unquoted)
                    ScalarType::Integer
                    | ScalarType::Float
                    | ScalarType::Boolean
                    | ScalarType::Null => self.value.clone(),
                }
            }
            ScalarStyle::SingleQuoted => self.to_single_quoted(),
            ScalarStyle::DoubleQuoted => self.to_double_quoted(),
            ScalarStyle::Literal => self.to_literal(),
            ScalarStyle::Folded => self.to_folded(),
        }
    }

    /// Convert to single-quoted string
    fn to_single_quoted(&self) -> String {
        // Escape single quotes by doubling them
        let escaped = self.value.replace('\'', "''");
        format!("'{}'", escaped)
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
                '/' => result.push_str("\\/"),    // forward slash (optional in YAML)
                c if c.is_control() || (c as u32) > 0x7F => {
                    // Handle Unicode characters and control characters
                    let code_point = c as u32;
                    if code_point <= 0xFF {
                        result.push_str(&format!("\\x{:02X}", code_point));
                    } else if code_point <= 0xFFFF {
                        result.push_str(&format!("\\u{:04X}", code_point));
                    } else {
                        result.push_str(&format!("\\U{:08X}", code_point));
                    }
                }
                c => result.push(c),
            }
        }
        result.push('"');
        result
    }

    /// Convert to literal block scalar
    fn to_literal(&self) -> String {
        // Literal scalars preserve newlines and don't escape
        format!("|\\n  {}", self.value.replace('\n', "\n  "))
    }

    /// Convert to folded block scalar
    fn to_folded(&self) -> String {
        // Folded scalars fold lines but preserve paragraph breaks
        format!(">\\n  {}", self.value.replace('\n', "\n  "))
    }
}

impl fmt::Display for ScalarValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_yaml_string())
    }
}

impl From<String> for ScalarValue {
    fn from(value: String) -> Self {
        Self::new(value)
    }
}

impl From<&str> for ScalarValue {
    fn from(value: &str) -> Self {
        Self::new(value)
    }
}

impl From<i32> for ScalarValue {
    fn from(value: i32) -> Self {
        Self {
            value: value.to_string(),
            style: ScalarStyle::Plain,
            scalar_type: ScalarType::Integer,
        }
    }
}

impl From<i64> for ScalarValue {
    fn from(value: i64) -> Self {
        Self {
            value: value.to_string(),
            style: ScalarStyle::Plain,
            scalar_type: ScalarType::Integer,
        }
    }
}

impl From<f32> for ScalarValue {
    fn from(value: f32) -> Self {
        Self {
            value: value.to_string(),
            style: ScalarStyle::Plain,
            scalar_type: ScalarType::Float,
        }
    }
}

impl From<f64> for ScalarValue {
    fn from(value: f64) -> Self {
        Self {
            value: value.to_string(),
            style: ScalarStyle::Plain,
            scalar_type: ScalarType::Float,
        }
    }
}

impl From<bool> for ScalarValue {
    fn from(value: bool) -> Self {
        Self {
            value: if value { "true" } else { "false" }.to_string(),
            style: ScalarStyle::Plain,
            scalar_type: ScalarType::Boolean,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_plain_scalars() {
        let scalar = ScalarValue::new("simple");
        assert_eq!(scalar.to_yaml_string(), "simple");

        let scalar = ScalarValue::new("hello world");
        assert_eq!(scalar.to_yaml_string(), "hello world");
    }

    #[test]
    fn test_values_needing_quotes() {
        // Boolean-like values
        let scalar = ScalarValue::new("true");
        assert_eq!(scalar.to_yaml_string(), "'true'");

        let scalar = ScalarValue::new("false");
        assert_eq!(scalar.to_yaml_string(), "'false'");

        let scalar = ScalarValue::new("yes");
        assert_eq!(scalar.to_yaml_string(), "'yes'");

        let scalar = ScalarValue::new("no");
        assert_eq!(scalar.to_yaml_string(), "'no'");

        // Null-like values
        let scalar = ScalarValue::new("null");
        assert_eq!(scalar.to_yaml_string(), "'null'");

        let scalar = ScalarValue::new("~");
        assert_eq!(scalar.to_yaml_string(), "'~'");

        // Numbers
        let scalar = ScalarValue::new("123");
        assert_eq!(scalar.to_yaml_string(), "'123'");

        let scalar = ScalarValue::new("3.14");
        assert_eq!(scalar.to_yaml_string(), "'3.14'");

        // Special characters
        let scalar = ScalarValue::new("value: something");
        assert_eq!(scalar.to_yaml_string(), "'value: something'");

        let scalar = ScalarValue::new("# comment");
        assert_eq!(scalar.to_yaml_string(), "'# comment'");

        // Leading/trailing whitespace
        let scalar = ScalarValue::new("  spaces  ");
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
    fn test_multiline() {
        let scalar = ScalarValue::new("line1\nline2\nline3");
        // Should auto-detect literal style for multiline
        assert_eq!(scalar.style(), ScalarStyle::Literal);
    }

    #[test]
    fn test_from_types() {
        let scalar = ScalarValue::from(42);
        assert_eq!(scalar.to_yaml_string(), "42");

        let scalar = ScalarValue::from(3.14);
        assert_eq!(scalar.to_yaml_string(), "3.14");

        let scalar = ScalarValue::from(true);
        assert_eq!(scalar.to_yaml_string(), "true");

        let scalar = ScalarValue::from(false);
        assert_eq!(scalar.to_yaml_string(), "false");
    }

    #[test]
    fn test_empty_string() {
        let scalar = ScalarValue::new("");
        assert_eq!(scalar.to_yaml_string(), "''");
    }

    #[test]
    fn test_special_start_chars() {
        let scalar = ScalarValue::new("-item");
        assert_eq!(scalar.to_yaml_string(), "'-item'");

        let scalar = ScalarValue::new("?key");
        assert_eq!(scalar.to_yaml_string(), "'?key'");

        let scalar = ScalarValue::new("[array]");
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
    fn test_escape_sequences_unicode_U() {
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
        // Test line folding with escaped spaces and newlines
        assert_eq!(
            ScalarValue::parse_escape_sequences("line\\ \nfolding"),
            "linefolding"
        );
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
    fn test_escape_sequences_unknown() {
        // Test unknown escape sequences are preserved
        assert_eq!(ScalarValue::parse_escape_sequences("\\q"), "\\q");
        assert_eq!(ScalarValue::parse_escape_sequences("\\z"), "\\z");
        assert_eq!(ScalarValue::parse_escape_sequences("\\1"), "\\1");
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
        assert!(yaml_string.contains("\\n"));
        assert!(yaml_string.contains("\\t"));
        assert!(yaml_string.contains("\\U"));

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
        assert!(yaml_string.contains("\\u") || yaml_string.contains("\\U"));

        // But the internal value should remain unchanged
        assert_eq!(scalar.value(), "Hello 世界 🌍");
    }
}
