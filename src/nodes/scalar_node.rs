use super::{Lang, SyntaxNode};
use crate::as_yaml::{AsYaml, YamlKind};
use crate::lex::SyntaxKind;
use crate::scalar::ScalarValue;
use crate::yaml::ValueNode;
use rowan::ast::AstNode;
use rowan::GreenNodeBuilder;
use std::fmt;

ast_node!(Scalar, SCALAR, "A YAML scalar value");

/// Chomping indicator for block scalars
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Chomping {
    /// Strip final line breaks (indicator: -)
    Strip,
    /// Keep final line breaks (indicator: +)
    Keep,
    /// Clip to single final line break (default, no indicator)
    Clip,
}

/// Error type for scalar type conversions
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ScalarConversionError {
    /// The scalar value is quoted, indicating it's a string type in YAML
    QuotedValue,
    /// The scalar value cannot be parsed as the target type
    ParseError(String),
}

impl fmt::Display for ScalarConversionError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ScalarConversionError::QuotedValue => {
                write!(f, "Cannot convert quoted scalar to numeric/boolean type")
            }
            ScalarConversionError::ParseError(msg) => {
                write!(f, "Failed to parse scalar: {}", msg)
            }
        }
    }
}

impl std::error::Error for ScalarConversionError {}

impl Scalar {
    /// Get the string value of this scalar
    pub fn value(&self) -> String {
        self.0.text().to_string()
    }

    /// Get the string representation of this scalar, properly unquoted and unescaped
    pub fn as_string(&self) -> String {
        let text = self.value();

        // Handle quoted strings
        if text.starts_with('"') && text.ends_with('"') {
            // Double-quoted string - handle escape sequences
            ScalarValue::parse_escape_sequences(&text[1..text.len() - 1])
        } else if text.starts_with('\'') && text.ends_with('\'') {
            // Single-quoted string - handle '' -> ' escape and fold multi-line strings
            let content = &text[1..text.len() - 1];
            let unescaped = content.replace("''", "'");
            // Only fold lines if actually multi-line
            if unescaped.contains('\n') {
                // Fold line breaks (newlines + indentation) to spaces per YAML spec
                // Using fold() avoids intermediate Vec allocation
                let mut result = String::new();
                for (i, line) in unescaped.lines().enumerate() {
                    if i > 0 {
                        result.push(' ');
                    }
                    result.push_str(line.trim());
                }
                result
            } else {
                unescaped
            }
        } else if text.starts_with('|') || text.starts_with('>') {
            // Block scalar (literal or folded)
            Self::parse_block_scalar(&text)
        } else {
            // Plain scalar - fold lines if multi-line
            if text.contains('\n') {
                // Multi-line plain scalar: fold newlines to spaces
                // Using manual iteration avoids intermediate Vec allocation
                let mut result = String::new();
                let mut first = true;
                for line in text.lines() {
                    let trimmed = line.trim();
                    if !trimmed.is_empty() {
                        if !first {
                            result.push(' ');
                        }
                        result.push_str(trimmed);
                        first = false;
                    }
                }
                result
            } else {
                text
            }
        }
    }

    /// Parse a block scalar (literal `|` or folded `>`) into its string content
    fn parse_block_scalar(text: &str) -> String {
        let mut lines = text.lines();
        let first_line = match lines.next() {
            Some(line) => line,
            None => return String::new(),
        };

        let is_literal = first_line.starts_with('|');

        // Parse chomping indicator and indentation from header
        let header = first_line.trim();
        let chomping = if header.contains('-') {
            Chomping::Strip
        } else if header.contains('+') {
            Chomping::Keep
        } else {
            Chomping::Clip
        };

        // Collect content lines
        let content_lines: Vec<&str> = lines.collect();
        if content_lines.is_empty() {
            return String::new();
        }

        // Detect base indentation from first non-empty line
        let base_indent = content_lines
            .iter()
            .find(|line| !line.trim().is_empty())
            .map(|line| line.chars().take_while(|c| *c == ' ').count())
            .unwrap_or(0);

        // Count trailing empty lines for Keep chomping
        let trailing_empty_count = content_lines
            .iter()
            .rev()
            .take_while(|line| line.trim().is_empty())
            .count();

        // Process content
        let mut result = String::new();
        let mut prev_was_empty = false;
        let mut prev_was_more_indented = false;

        for (i, line) in content_lines.iter().enumerate() {
            if line.trim().is_empty() {
                // Empty line
                if is_literal {
                    // Literal: each line (including empty) gets a newline after it
                    result.push('\n');
                } else {
                    // Folded: empty lines create paragraph breaks (single newline)
                    if !prev_was_empty && i > 0 {
                        // Add newline to create paragraph break
                        result.push('\n');
                    }
                }
                prev_was_empty = true;
                prev_was_more_indented = false;
            } else {
                // Non-empty line - strip base indentation
                let stripped = if line.len() >= base_indent {
                    &line[base_indent..]
                } else {
                    line.trim_start()
                };

                if is_literal {
                    // Literal: each line gets content + newline
                    result.push_str(stripped);
                    result.push('\n');
                    prev_was_more_indented = false;
                } else {
                    // Folded: check if line is more indented than base
                    let line_indent = line.chars().take_while(|c| *c == ' ').count();
                    let is_more_indented = line_indent > base_indent;

                    if is_more_indented {
                        // More-indented lines: preserve on their own line with extra indent
                        if i > 0 && !prev_was_empty && !prev_was_more_indented {
                            // Only add newline if transitioning from normal to more-indented
                            result.push('\n');
                        }
                        result.push_str(stripped);
                        result.push('\n');
                        prev_was_more_indented = true;
                    } else {
                        // Normal line: fold with previous unless after empty line or more-indented
                        if i > 0 {
                            if prev_was_empty || prev_was_more_indented {
                                // After paragraph break or more-indented section, don't add space
                                result.push_str(stripped);
                            } else {
                                // Join with space
                                result.push(' ');
                                result.push_str(stripped);
                            }
                        } else {
                            // First line
                            result.push_str(stripped);
                        }
                        prev_was_more_indented = false;
                    }
                }
                prev_was_empty = false;
            }
        }

        // Apply chomping
        match chomping {
            Chomping::Strip => {
                // Remove all trailing newlines
                result = result.trim_end_matches('\n').to_string();
            }
            Chomping::Clip => {
                // Keep single trailing newline
                result = result.trim_end_matches('\n').to_string();
                result.push('\n');
            }
            Chomping::Keep => {
                // Keep all trailing newlines - preserve the count we detected
                // Remove all trailing newlines first, then add back the original count
                result = result.trim_end_matches('\n').to_string();
                // Add one newline for the content line, plus trailing empties
                for _ in 0..=trailing_empty_count {
                    result.push('\n');
                }
            }
        }

        result
    }

    /// Check if this scalar is quoted
    pub fn is_quoted(&self) -> bool {
        let text = self.value();
        (text.starts_with('"') && text.ends_with('"'))
            || (text.starts_with('\'') && text.ends_with('\''))
    }

    /// Get the raw content of this scalar with outer quotes stripped, but
    /// without processing any escape sequences.
    ///
    /// For most purposes [`as_string`](Self::as_string) is more appropriate as
    /// it fully unescapes double-quoted strings (`\"`, `\\`, `\n`, etc.) and
    /// handles the `''` → `'` escape in single-quoted strings. Use this method
    /// only when you need the verbatim content without escape processing.
    pub fn unquoted_value(&self) -> String {
        let text = self.value();
        if self.is_quoted() {
            text[1..text.len() - 1].to_string()
        } else {
            text
        }
    }
}

impl Scalar {
    /// Replace the text content of this scalar with `value`.
    ///
    /// The token is stored with `SyntaxKind::STRING` regardless of the semantic
    /// type of `value` (e.g., setting `"42"` does not produce an `INT` token).
    /// If token-kind accuracy matters, build a replacement scalar node via the
    /// higher-level API instead.
    pub fn set_value(&self, value: &str) {
        let children_count = self.0.children_with_tokens().count();
        // Create a temporary node to wrap the token and extract a SyntaxToken
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(SyntaxKind::ROOT.into());
        builder.token(SyntaxKind::STRING.into(), value);
        builder.finish_node();
        let temp_node = SyntaxNode::new_root_mut(builder.finish());
        let new_token = temp_node
            .first_token()
            .expect("builder always emits a STRING token");
        self.0
            .splice_children(0..children_count, vec![new_token.into()]);
    }

    /// Get the byte offset range of this scalar in the source text.
    ///
    /// Returns the start and end byte offsets as a `TextPosition`.
    pub fn byte_range(&self) -> crate::TextPosition {
        self.0.text_range().into()
    }

    /// Get the line and column where this scalar starts.
    ///
    /// Requires the original source text to calculate line/column from byte offsets.
    /// Line and column numbers are 1-indexed.
    ///
    /// # Arguments
    ///
    /// * `source_text` - The original YAML source text
    pub fn start_position(&self, source_text: &str) -> crate::LineColumn {
        let range = self.byte_range();
        crate::byte_offset_to_line_column(source_text, range.start as usize)
    }

    /// Get the line and column where this scalar ends.
    ///
    /// Requires the original source text to calculate line/column from byte offsets.
    /// Line and column numbers are 1-indexed.
    ///
    /// # Arguments
    ///
    /// * `source_text` - The original YAML source text
    pub fn end_position(&self, source_text: &str) -> crate::LineColumn {
        let range = self.byte_range();
        crate::byte_offset_to_line_column(source_text, range.end as usize)
    }

    /// Try to interpret this scalar as an i64.
    ///
    /// Returns `None` if the scalar is quoted (string type) or cannot be parsed as an integer.
    /// Supports decimal, octal (0o), hexadecimal (0x), and binary (0b) notation.
    pub fn as_i64(&self) -> Option<i64> {
        TryInto::<i64>::try_into(self).ok()
    }

    /// Try to interpret this scalar as an f64.
    ///
    /// Returns `None` if the scalar is quoted (string type) or cannot be parsed as a float.
    pub fn as_f64(&self) -> Option<f64> {
        TryInto::<f64>::try_into(self).ok()
    }

    /// Try to interpret this scalar as a bool.
    ///
    /// Returns `None` if the scalar is quoted (string type) or is not a recognized boolean value.
    /// Recognizes: true, false, True, False, TRUE, FALSE, yes, no, Yes, No, YES, NO, on, off, On, Off, ON, OFF
    pub fn as_bool(&self) -> Option<bool> {
        TryInto::<bool>::try_into(self).ok()
    }

    /// Check if this scalar represents a null value.
    ///
    /// Returns `true` if the unquoted value is null, Null, NULL, ~, or empty.
    pub fn is_null(&self) -> bool {
        if self.is_quoted() {
            return false;
        }
        let val = self.as_string();
        matches!(val.as_str(), "null" | "Null" | "NULL" | "~" | "")
    }
}

impl AsYaml for Scalar {
    fn as_node(&self) -> Option<&SyntaxNode> {
        Some(&self.0)
    }

    fn kind(&self) -> YamlKind {
        YamlKind::Scalar
    }

    fn build_content(
        &self,
        builder: &mut rowan::GreenNodeBuilder,
        _indent: usize,
        _flow_context: bool,
    ) -> bool {
        crate::as_yaml::copy_node_content(builder, &self.0);
        // Scalars don't end with newlines
        false
    }

    fn is_inline(&self) -> bool {
        ValueNode::is_inline(self)
    }
}

// TryFrom implementations for typed access
impl TryFrom<&Scalar> for i64 {
    type Error = ScalarConversionError;

    fn try_from(scalar: &Scalar) -> Result<Self, Self::Error> {
        if scalar.is_quoted() {
            return Err(ScalarConversionError::QuotedValue);
        }

        let value = scalar.as_string();

        // Handle different number formats
        if let Some(hex) = value
            .strip_prefix("0x")
            .or_else(|| value.strip_prefix("0X"))
        {
            i64::from_str_radix(hex, 16)
                .map_err(|e| ScalarConversionError::ParseError(e.to_string()))
        } else if let Some(octal) = value
            .strip_prefix("0o")
            .or_else(|| value.strip_prefix("0O"))
        {
            i64::from_str_radix(octal, 8)
                .map_err(|e| ScalarConversionError::ParseError(e.to_string()))
        } else if let Some(binary) = value
            .strip_prefix("0b")
            .or_else(|| value.strip_prefix("0B"))
        {
            i64::from_str_radix(binary, 2)
                .map_err(|e| ScalarConversionError::ParseError(e.to_string()))
        } else {
            value
                .parse::<i64>()
                .map_err(|e| ScalarConversionError::ParseError(e.to_string()))
        }
    }
}

impl TryFrom<&Scalar> for f64 {
    type Error = ScalarConversionError;

    fn try_from(scalar: &Scalar) -> Result<Self, Self::Error> {
        if scalar.is_quoted() {
            return Err(ScalarConversionError::QuotedValue);
        }

        let value = scalar.as_string();

        // Handle special float values
        match value.as_str() {
            ".inf" | ".Inf" | ".INF" | "+.inf" | "+.Inf" | "+.INF" => Ok(f64::INFINITY),
            "-.inf" | "-.Inf" | "-.INF" => Ok(f64::NEG_INFINITY),
            ".nan" | ".NaN" | ".NAN" => Ok(f64::NAN),
            _ => value
                .parse::<f64>()
                .map_err(|e| ScalarConversionError::ParseError(e.to_string())),
        }
    }
}

impl TryFrom<&Scalar> for bool {
    type Error = ScalarConversionError;

    fn try_from(scalar: &Scalar) -> Result<Self, Self::Error> {
        if scalar.is_quoted() {
            return Err(ScalarConversionError::QuotedValue);
        }

        let value = scalar.as_string();

        // YAML 1.2 Core Schema boolean values
        match value.as_str() {
            "true" | "True" | "TRUE" => Ok(true),
            "false" | "False" | "FALSE" => Ok(false),
            // YAML 1.1 compatibility (commonly used)
            "yes" | "Yes" | "YES" | "on" | "On" | "ON" => Ok(true),
            "no" | "No" | "NO" | "off" | "Off" | "OFF" => Ok(false),
            _ => Err(ScalarConversionError::ParseError(format!(
                "'{}' is not a recognized boolean value",
                value
            ))),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::Document;
    use std::str::FromStr;

    #[test]
    fn test_json_array_quoted_strings_cst_structure() {
        // This test verifies that quoted strings in flow sequences (JSON arrays)
        // don't incorrectly consume trailing whitespace into the SCALAR node.
        //
        // The bug was that the parser would include NEWLINE and INDENT tokens
        // as children of the SCALAR node instead of as siblings.

        let json = r#"{
  "items": [
    "first",
    "second"
  ]
}"#;

        let doc = Document::from_str(json).unwrap();
        let mapping = doc.as_mapping().unwrap();
        let items = mapping.get("items").unwrap();
        let sequence = items.as_sequence().unwrap();

        // Get the scalars
        let values: Vec<_> = sequence
            .values()
            .filter_map(|node| {
                if let crate::YamlNode::Scalar(scalar) = node {
                    Some(scalar)
                } else {
                    None
                }
            })
            .collect();

        assert_eq!(values.len(), 2);

        // Both values should be clean quoted strings without trailing whitespace
        assert_eq!(
            values[0].value(),
            r#""first""#,
            "first item should not have trailing whitespace"
        );
        assert_eq!(
            values[1].value(),
            r#""second""#,
            "second item should not have trailing whitespace"
        );

        // as_string() should correctly unquote
        assert_eq!(values[0].as_string(), "first");
        assert_eq!(values[1].as_string(), "second");
    }

    #[test]
    fn test_compact_json_array() {
        // Compact JSON should also work correctly
        let json = r#"{"items": ["first", "second"]}"#;

        let doc = Document::from_str(json).unwrap();
        let mapping = doc.as_mapping().unwrap();
        let items = mapping.get("items").unwrap();
        let sequence = items.as_sequence().unwrap();

        let values: Vec<_> = sequence
            .values()
            .filter_map(|node| {
                if let crate::YamlNode::Scalar(scalar) = node {
                    Some(scalar)
                } else {
                    None
                }
            })
            .collect();

        assert_eq!(values.len(), 2);
        assert_eq!(values[0].value(), r#""first""#);
        assert_eq!(values[1].value(), r#""second""#);
        assert_eq!(values[0].as_string(), "first");
        assert_eq!(values[1].as_string(), "second");
    }

    #[test]
    fn test_yaml_flow_arrays_quoted_strings() {
        // YAML flow-style arrays should behave the same
        let yaml = r#"
items: ["first", "second"]
"#;

        let doc = Document::from_str(yaml).unwrap();
        let mapping = doc.as_mapping().unwrap();
        let items = mapping.get("items").unwrap();
        let sequence = items.as_sequence().unwrap();

        let values: Vec<_> = sequence
            .values()
            .filter_map(|node| {
                if let crate::YamlNode::Scalar(scalar) = node {
                    Some(scalar)
                } else {
                    None
                }
            })
            .collect();

        assert_eq!(values.len(), 2);
        assert_eq!(values[0].value(), r#""first""#);
        assert_eq!(values[1].value(), r#""second""#);
        assert_eq!(values[0].as_string(), "first");
        assert_eq!(values[1].as_string(), "second");
    }
}
