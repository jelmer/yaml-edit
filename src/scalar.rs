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
                c if c.is_control() => {
                    result.push_str(&format!("\\x{:02x}", c as u32));
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
}
