use super::{fresh_token, Lang, SyntaxNode};
use crate::as_yaml::{AsYaml, YamlKind};
use crate::lex::SyntaxKind;
use crate::scalar::ScalarValue;
use crate::yaml::ValueNode;
use rowan::ast::AstNode;
use std::borrow::Cow;
use std::fmt;

ast_node!(Scalar, SCALAR, "A YAML scalar value");

/// Chomping indicator for block scalars
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Chomping {
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
                write!(f, "Failed to parse scalar: {msg}")
            }
        }
    }
}

impl std::error::Error for ScalarConversionError {}

/// The chomping and explicit indentation indicators in `header`, the
/// first line of a block scalar. Only the run straight after `|` or `>`
/// holds them; a comment may follow (`> # a-b`) and its text says nothing
/// about either.
fn header_indicators(header: &str) -> (Chomping, Option<usize>) {
    let indicators: String = header
        .chars()
        .skip(1)
        .take_while(|c| c.is_ascii_digit() || *c == '+' || *c == '-')
        .collect();
    let chomping = if indicators.contains('-') {
        Chomping::Strip
    } else if indicators.contains('+') {
        Chomping::Keep
    } else {
        Chomping::Clip
    };
    let indent = indicators
        .chars()
        .find(|c| c.is_ascii_digit() && *c != '0')
        .and_then(|c| c.to_digit(10))
        .map(|d| d as usize);
    (chomping, indent)
}

/// The chomping and explicit indentation indicators of `scalar`, a block
/// scalar.
pub(crate) fn block_scalar_indicators(scalar: &SyntaxNode) -> (Chomping, Option<usize>) {
    let header: String = scalar
        .children_with_tokens()
        .filter_map(|c| c.into_token())
        .take_while(|t| t.kind() != SyntaxKind::NEWLINE)
        .map(|t| t.text().to_string())
        .collect();
    header_indicators(&header)
}

/// The column a block scalar's indentation indicator counts from: that of
/// the innermost collection entry holding `scalar`, or 0 at the document
/// root.
pub(crate) fn block_parent_column(scalar: &SyntaxNode) -> usize {
    scalar
        .ancestors()
        .find(|n| {
            matches!(
                n.kind(),
                SyntaxKind::MAPPING_ENTRY | SyntaxKind::SEQUENCE_ENTRY
            )
        })
        .map_or(0, |entry| crate::column::node_column(&entry))
}

/// Replace `\r\n` and lone `\r` line breaks with `\n`, which
/// `str::lines` does not split on.
fn normalize_line_breaks(text: &str) -> Cow<'_, str> {
    if text.contains('\r') {
        Cow::Owned(text.replace("\r\n", "\n").replace('\r', "\n"))
    } else {
        Cow::Borrowed(text)
    }
}

impl Scalar {
    /// Get the string value of this scalar
    pub fn value(&self) -> String {
        self.0.text().to_string()
    }

    /// Get the string representation of this scalar, properly unquoted and unescaped
    pub fn as_string(&self) -> String {
        let text = self.value();

        // Handle quoted strings
        if text.len() >= 2 && text.starts_with('"') && text.ends_with('"') {
            // Double-quoted string - handle escape sequences
            ScalarValue::parse_escape_sequences(&text[1..text.len() - 1])
        } else if text.len() >= 2 && text.starts_with('\'') && text.ends_with('\'') {
            // Single-quoted string - handle '' -> ' escape and fold multi-line strings
            let content = &text[1..text.len() - 1];
            let unescaped = content.replace("''", "'");
            // Only fold lines if actually multi-line
            if unescaped.contains(['\n', '\r']) {
                // A line break folds to a space, or to one newline per empty
                // line after it, and takes the white space around it along.
                let unescaped = normalize_line_breaks(&unescaped);
                let lines: Vec<&str> = unescaped.split('\n').collect();
                let last = lines.len() - 1;
                let mut result = lines[0].trim_end_matches([' ', '\t']).to_string();
                let mut empty_lines = 0;
                for (i, line) in lines.iter().enumerate().skip(1) {
                    let line = line.trim_start_matches([' ', '\t']);
                    let line = if i == last {
                        line
                    } else {
                        line.trim_end_matches([' ', '\t'])
                    };
                    if line.is_empty() && i != last {
                        empty_lines += 1;
                        continue;
                    }
                    match empty_lines {
                        0 => result.push(' '),
                        n => result.push_str(&"\n".repeat(n)),
                    }
                    empty_lines = 0;
                    result.push_str(line);
                }
                result
            } else {
                unescaped
            }
        } else if self.is_block() {
            Self::parse_block_scalar(&text, block_parent_column(&self.0))
        } else {
            // Plain scalar - fold lines if multi-line
            if text.contains(['\n', '\r']) {
                // Multi-line plain scalar: a single line break folds to a
                // space, and each additional one is kept, so a blank line
                // between two lines becomes a newline (YAML 1.2 line folding).
                let mut result = String::new();
                let mut pending_breaks = 0usize;
                for line in normalize_line_breaks(&text).lines() {
                    let trimmed = line.trim();
                    if trimmed.is_empty() {
                        // Only counts once content has started; leading blank
                        // lines are stripped along with the indentation.
                        if !result.is_empty() {
                            pending_breaks += 1;
                        }
                        continue;
                    }
                    if !result.is_empty() {
                        if pending_breaks == 0 {
                            result.push(' ');
                        } else {
                            for _ in 0..pending_breaks {
                                result.push('\n');
                            }
                        }
                    }
                    pending_breaks = 0;
                    result.push_str(trimmed);
                }
                result
            } else {
                text
            }
        }
    }

    /// Parse a block scalar (literal `|` or folded `>`) into its string content
    ///
    /// `parent_column` is the column of the collection the scalar is a node
    /// of, which an explicit indentation indicator counts from.
    fn parse_block_scalar(text: &str, parent_column: usize) -> String {
        let text = normalize_line_breaks(text);
        let mut lines = text.lines();
        let first_line = match lines.next() {
            Some(line) => line,
            None => return String::new(),
        };

        let is_literal = first_line.starts_with('|');
        let (chomping, explicit_indent) = header_indicators(first_line);

        // Collect content lines
        let content_lines: Vec<&str> = lines.collect();
        if content_lines.is_empty() {
            return String::new();
        }

        // An explicit indentation indicator (`|2`, `>2-`) says where the
        // content starts, counted from the parent collection's column.
        // Anything further right is part of the value: `|2` over a 4-space
        // body at the root is `"  x"`, not `"x"`. Without it, take the first
        // non-empty line's indentation as the base.
        let base_indent = match explicit_indent {
            Some(explicit) => parent_column + explicit,
            // With no content line at all, every line is empty however
            // many spaces it holds.
            None => content_lines
                .iter()
                .find(|line| !line.trim().is_empty())
                .map_or(usize::MAX, |line| crate::column::space_indent(line)),
        };

        // A line with no more than `base_indent` spaces and nothing else is
        // empty; other lines lose up to `base_indent` spaces of indentation.
        let mut result = String::new();
        let mut empty_lines = 0;
        // Whether the previous content line, if any, started with white space
        let mut prev_spaced: Option<bool> = None;

        for line in &content_lines {
            // base_indent is a character count, so we step by chars to
            // stay on UTF-8 boundaries even if the line starts with
            // multi-byte content at less than base_indent spaces of
            // indentation.
            let leading_spaces = crate::column::space_indent(line);
            let strip = leading_spaces.min(base_indent);
            let strip_bytes = line
                .char_indices()
                .nth(strip)
                .map_or(line.len(), |(i, _)| i);
            let stripped = &line[strip_bytes..];
            if stripped.is_empty() {
                empty_lines += 1;
                continue;
            }

            // Folding joins two lines with a space, unless empty lines
            // separate them or either starts with white space, in which
            // case every line break is kept.
            let spaced = stripped.starts_with([' ', '\t']);
            let breaks = match prev_spaced {
                Some(prev) if is_literal || prev || spaced => empty_lines + 1,
                Some(_) if empty_lines == 0 => {
                    result.push(' ');
                    0
                }
                _ => empty_lines,
            };
            result.push_str(&"\n".repeat(breaks));
            result.push_str(stripped);
            prev_spaced = Some(spaced);
            empty_lines = 0;
        }

        // `empty_lines` is now the number of trailing empty lines
        let has_content = prev_spaced.is_some();
        match chomping {
            Chomping::Strip => {}
            Chomping::Clip => {
                if has_content {
                    result.push('\n');
                }
            }
            Chomping::Keep => {
                // One newline ends the last content line, then one per
                // trailing blank. A body that is nothing but blank lines has
                // no content line to end, so it gets only the blanks:
                // `k: |+\n\n\n` is `"\n\n"`, as saphyr and PyYAML both
                // read it, not `"\n\n\n"`.
                let newlines = empty_lines + usize::from(has_content);
                result.push_str(&"\n".repeat(newlines));
            }
        }

        result
    }

    /// Check if this scalar is quoted
    pub fn is_quoted(&self) -> bool {
        let text = self.value();
        text.len() >= 2
            && ((text.starts_with('"') && text.ends_with('"'))
                || (text.starts_with('\'') && text.ends_with('\'')))
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
        let children: Vec<_> = self.0.children_with_tokens().collect();
        // A block scalar owns the line break that ends its body; keep it so
        // the next entry stays on its own line.
        let trailing_newlines = children
            .iter()
            .rev()
            .take_while(|c| c.kind() == SyntaxKind::NEWLINE)
            .count();
        crate::nodes::replace_children(
            &self.0,
            0..children.len() - trailing_newlines,
            vec![fresh_token(SyntaxKind::STRING, value).into()],
        );
    }

    crate::nodes::ast_node_spans!("scalar");

    /// Try to interpret this scalar as an i64.
    ///
    /// Returns `None` if the scalar is not a plain YAML 1.2 integer (decimal,
    /// `0o` octal or `0x` hexadecimal) or does not fit in an `i64`.
    pub fn as_i64(&self) -> Option<i64> {
        TryInto::<i64>::try_into(self).ok()
    }

    /// Try to interpret this scalar as an f64.
    ///
    /// Returns `None` if the scalar is not a plain YAML 1.2 float or
    /// integer.
    pub fn as_f64(&self) -> Option<f64> {
        TryInto::<f64>::try_into(self).ok()
    }

    /// Try to interpret this scalar as a bool.
    ///
    /// Returns `None` if the scalar is not a plain YAML 1.2 boolean: true,
    /// True, TRUE, false, False or FALSE.
    pub fn as_bool(&self) -> Option<bool> {
        TryInto::<bool>::try_into(self).ok()
    }

    /// Check if this scalar is a literal (`|`) or folded (`>`) block scalar
    pub fn is_block(&self) -> bool {
        let text = self.value();
        text.starts_with('|') || text.starts_with('>')
    }

    /// Check if this scalar represents a null value.
    ///
    /// Returns `true` if the plain value is null, Null, NULL, ~, or empty.
    /// Quoted and block scalars are always strings, never null.
    pub fn is_null(&self) -> bool {
        if self.is_quoted() || self.is_block() {
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

    /// The continuation lines of a multi-line plain or quoted scalar are
    /// shifted as for a flow collection, keeping their offset from the
    /// indentation of the line the scalar starts on, which becomes
    /// `indent`. Leading whitespace on those lines is not content, so the
    /// value is unchanged. A block scalar is copied as is: its body is laid
    /// out by the caller, and its indentation can be content.
    fn build_content(
        &self,
        builder: &mut rowan::GreenNodeBuilder,
        indent: usize,
        flow_context: bool,
    ) -> bool {
        let plain = !self.is_quoted() && !self.is_block();
        if flow_context && plain && self.value().contains([',', '[', ']', '{', '}']) {
            // Inside a flow collection a plain scalar ends at the indicator.
            return ScalarValue::double_quoted(self.as_string())
                .build_content(builder, indent, true);
        }
        if !ValueNode::is_inline(self) {
            crate::yaml::copy_node_to_builder(builder, &self.0);
            return false;
        }
        let delta = indent as isize - crate::column::line_indent(&self.0) as isize;
        let delta = crate::as_yaml::clear_parent_column(&self.0, indent, delta);
        builder.start_node(SyntaxKind::SCALAR.into());
        copy_shifting_continuation_lines(builder, &self.0, delta, false);
        builder.finish_node();
        // Scalars don't end with newlines
        false
    }

    fn is_inline(&self) -> bool {
        ValueNode::is_inline(self)
    }
}

/// Copy the children of `node`, moving the indentation of every line after
/// the first by `delta` columns, both between tokens (a plain scalar) and
/// within them (a quoted scalar). Empty lines stay empty. Returns whether
/// the copy ends with a line break.
fn copy_shifting_continuation_lines(
    builder: &mut rowan::GreenNodeBuilder,
    node: &SyntaxNode,
    delta: isize,
    mut after_newline: bool,
) -> bool {
    for child in node.children_with_tokens() {
        let token = match child {
            rowan::NodeOrToken::Node(n) => {
                builder.start_node(n.kind().into());
                after_newline = copy_shifting_continuation_lines(builder, &n, delta, after_newline);
                builder.finish_node();
                continue;
            }
            rowan::NodeOrToken::Token(t) => t,
        };
        match token.kind() {
            SyntaxKind::NEWLINE => {
                builder.token(token.kind().into(), token.text());
                after_newline = true;
                continue;
            }
            SyntaxKind::INDENT | SyntaxKind::WHITESPACE if after_newline => {
                let shifted = crate::as_yaml::shift_line_start(token.text(), delta);
                if !shifted.is_empty() {
                    builder.token(SyntaxKind::INDENT.into(), &shifted);
                }
            }
            kind => {
                if after_newline && delta > 0 {
                    builder.token(SyntaxKind::INDENT.into(), &" ".repeat(delta as usize));
                }
                builder.token(
                    kind.into(),
                    &crate::as_yaml::shift_inner_lines(token.text(), delta),
                );
            }
        }
        after_newline = false;
    }
    after_newline
}

// TryFrom implementations for typed access. These read the scalar the way
// the YAML 1.2 core schema resolves it, so only a plain scalar of the
// matching type converts.
fn typed_value(scalar: &Scalar) -> Result<ScalarValue, ScalarConversionError> {
    if scalar.is_quoted() {
        return Err(ScalarConversionError::QuotedValue);
    }
    Ok(ScalarValue::from_scalar(scalar))
}

impl TryFrom<&Scalar> for i64 {
    type Error = ScalarConversionError;

    fn try_from(scalar: &Scalar) -> Result<Self, Self::Error> {
        let value = typed_value(scalar)?;
        value.to_i64().ok_or_else(|| {
            ScalarConversionError::ParseError(format!(
                "'{}' is not an integer that fits in an i64",
                value.value()
            ))
        })
    }
}

impl TryFrom<&Scalar> for f64 {
    type Error = ScalarConversionError;

    fn try_from(scalar: &Scalar) -> Result<Self, Self::Error> {
        let value = typed_value(scalar)?;
        value
            .to_f64()
            .or_else(|| value.to_i64().map(|i| i as f64))
            .ok_or_else(|| {
                ScalarConversionError::ParseError(format!("'{}' is not a float", value.value()))
            })
    }
}

impl TryFrom<&Scalar> for bool {
    type Error = ScalarConversionError;

    fn try_from(scalar: &Scalar) -> Result<Self, Self::Error> {
        let value = typed_value(scalar)?;
        value.to_bool().ok_or_else(|| {
            ScalarConversionError::ParseError(format!(
                "'{}' is not a recognized boolean value",
                value.value()
            ))
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::Document;
    use std::str::FromStr;

    #[test]
    fn test_unterminated_quote_as_string_does_not_panic() {
        let (doc, errors) = Document::from_str_relaxed("\"");
        assert!(!errors.is_empty());
        let scalar = doc.as_scalar().expect("relaxed parse keeps a scalar");
        assert_eq!(scalar.as_string(), "\"");
        assert_eq!(scalar.unquoted_value(), "\"");

        let (doc, _) = Document::from_str_relaxed("'");
        let scalar = doc.as_scalar().expect("relaxed parse keeps a scalar");
        assert_eq!(scalar.as_string(), "'");
        assert_eq!(scalar.unquoted_value(), "'");
    }

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

    #[test]
    fn test_block_scalar_is_not_null() {
        // A block scalar is always a string, even when empty or when its
        // content spells a null keyword.
        for yaml in [
            "a: |\n",
            "a: >-\n",
            "a: |+\n",
            "a: |-\n  null\n",
            "a: >-\n  ~\n",
        ] {
            let doc = Document::from_str(yaml).unwrap();
            let scalar = doc
                .as_mapping()
                .unwrap()
                .get("a")
                .unwrap()
                .as_scalar()
                .unwrap()
                .clone();
            assert!(!scalar.is_null(), "{:?} parsed as null", yaml);
            assert_eq!(
                crate::scalar::ScalarValue::from_scalar(&scalar).scalar_type(),
                crate::scalar::ScalarType::String
            );
            assert!(
                !crate::yaml_eq(&scalar, &crate::scalar::ScalarValue::null()),
                "{:?}",
                yaml
            );
        }
    }

    #[test]
    fn test_double_quoted_line_folding() {
        let value = |yaml: &str| {
            let doc = Document::from_str(yaml).unwrap();
            let node = doc.as_mapping().unwrap().get("k").unwrap();
            node.as_scalar().unwrap().as_string()
        };
        assert_eq!(value("k: \"a\n  b\"\n"), "a b");
        assert_eq!(value("k: \"a\n\n  b\"\n"), "a\nb");
        assert_eq!(value("k: \"a\n \t\n\n  b\"\n"), "a\n\nb");
        assert_eq!(value("k: \"a   \n  b\"\n"), "a b");
        assert_eq!(value("k: \"a\\t\n  b\"\n"), "a\t b");
        assert_eq!(value("k: \" a\n  b \"\n"), " a b ");
        assert_eq!(value("k: \"a\r\n  b\"\r\n"), "a b");
        assert_eq!(value("k: \"a \\\n  b\"\n"), "a b");
        assert_eq!(value("k: \"a\\\n\n  b\"\n"), "a\nb");
    }

    #[test]
    fn test_parse_block_scalar_multibyte_after_dedent() {
        // Regression: `base_indent` is a char count but we sliced bytes,
        // which panicked when a continuation line started with a multi-byte
        // character at less than `base_indent` spaces of indentation.
        let yaml = ">\n  a\n\u{4f1}b\n";
        // We only care that it does not panic.
        let _ = super::Scalar::parse_block_scalar(yaml, 0);
    }

    #[test]
    fn test_as_f64_bare_inf_nan_is_none() {
        let doc = Document::from_str("[inf, nan, infinity, -Inf, .inf, -.Inf, .NaN, 1.5]").unwrap();
        let seq = doc.as_sequence().unwrap();
        let got: Vec<Option<f64>> = seq
            .values()
            .map(|n| n.as_scalar().unwrap().as_f64())
            .collect();
        assert_eq!(&got[..4], &[None, None, None, None]);
        assert_eq!(got[4], Some(f64::INFINITY));
        assert_eq!(got[5], Some(f64::NEG_INFINITY));
        assert!(got[6].unwrap().is_nan());
        assert_eq!(got[7], Some(1.5));
    }

    #[test]
    fn test_single_quoted_line_folding() {
        let value = |yaml: &str| {
            let doc = Document::from_str(yaml).unwrap();
            let node = doc.as_mapping().unwrap().get("k").unwrap();
            node.as_scalar().unwrap().as_string()
        };
        assert_eq!(value("k: 'a\n  b'\n"), "a b");
        assert_eq!(value("k: 'a\n\n  b'\n"), "a\nb");
        assert_eq!(value("k: 'a\n\n\n  b'\n"), "a\n\nb");
        assert_eq!(value("k: 'a \t\n \t\n\t b'\n"), "a\nb");
        assert_eq!(value("k: ' a\n  b '\n"), " a b ");
        assert_eq!(value("k: 'a\n  '\n"), "a ");
        assert_eq!(value("k: 'a\r\n\r\n  b'\r\n"), "a\nb");
        assert_eq!(value("k: 'a''\n  ''b'\n"), "a' 'b");
    }

    #[test]
    fn test_block_scalar_values() {
        let value = |yaml: &str, key: &str| {
            let doc = Document::from_str(yaml).unwrap();
            let node = doc.as_mapping().unwrap().get(key).unwrap();
            node.as_scalar().unwrap().as_string()
        };
        assert_eq!(value("k: >\n  a\n\n\n  b\n", "k"), "a\n\nb\n");
        assert_eq!(value("k: >\n\n  a\n", "k"), "\na\n");
        assert_eq!(value("k: >-\n  x\n  \ty\n", "k"), "x\n\ty");
        assert_eq!(value("k: >\n  a\n   b\n  c\n", "k"), "a\n b\nc\n");
        assert_eq!(value("k: >\n  a\n\n   b\n\n  c\n", "k"), "a\n\n b\n\nc\n");
        assert_eq!(value("k: |\n  a\n    \n \n  b\n", "k"), "a\n  \n\nb\n");
        assert_eq!(value("k: >\n\nj: 1\n", "k"), "");
        assert_eq!(value("k: |+\n\nj: 1\n", "k"), "\n");
        assert_eq!(value("k: |-\n\nj: 1\n", "k"), "");
        assert_eq!(value("k: |+\n   \n", "k"), "\n");
    }

    #[test]
    fn test_lone_carriage_return_line_breaks() {
        let value = |yaml: &str, key: &str| {
            let doc = Document::from_str(yaml).unwrap();
            let node = doc.as_mapping().unwrap().get(key).unwrap();
            node.as_scalar().unwrap().as_string()
        };
        assert_eq!(value("a: |\r  x\r  y\rb: 2\r", "a"), "x\ny\n");
        assert_eq!(value("a: |\r  x\r  y\rb: 2\r", "b"), "2");
        assert_eq!(value("a: b\r  c\rd: e\r", "a"), "b c");
        assert_eq!(value("a: b\r\r  c\rd: e\r", "a"), "b\nc");
    }
}
