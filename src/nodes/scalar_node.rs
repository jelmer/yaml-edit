use super::{Lang, SyntaxNode};
use crate::as_yaml::{AsYaml, YamlKind};
use crate::lex::SyntaxKind;
use crate::yaml::ValueNode;
use rowan::ast::AstNode;
use rowan::GreenNodeBuilder;
use std::fmt;

ast_node!(Scalar, SCALAR, "A YAML scalar value");

/// Emit the YAML 1.2 §6.5 fold decision for a run of `n` line breaks:
///   * n = 0: nothing (no break happened).
///   * n = 1: a single space (single break folds).
///   * n >= 2: n - 1 literal newlines (blank lines preserved as `\n`).
fn push_fold(out: &mut String, n: usize) {
    match n {
        0 => {}
        1 => out.push(' '),
        _ => {
            for _ in 1..n {
                out.push('\n');
            }
        }
    }
}

/// State for the fold logic in `decode_double_quoted`.
///
/// A source line either has "no break yet on this run" (`Inline`, with
/// possibly-pending same-line whitespace that we might yet trim), or is
/// mid-break-run (`InBreakRun`, tracking whether the run started with a
/// raw newline and how many extra breaks came after).
///
/// The invariant "extras and raw-leading only matter mid-run" is
/// encoded in the enum shape, so we cannot represent an
/// `extras > 0, in_break_run = false` combination that the previous
/// separate-locals version could accidentally reach.
enum RunState {
    Inline { pending_ws: String },
    InBreakRun { raw_leading: bool, extras: usize },
}

impl RunState {
    fn new() -> Self {
        Self::Inline {
            pending_ws: String::new(),
        }
    }

    /// A break run led by a raw newline. Its fold decision includes
    /// the leading break's contribution.
    fn raw_led() -> Self {
        Self::InBreakRun {
            raw_leading: true,
            extras: 0,
        }
    }

    /// A break run led by a `\<line-break>` escape. Its fold decision
    /// counts only subsequent breaks; the escape's own break is
    /// silently swallowed.
    fn escape_led() -> Self {
        Self::InBreakRun {
            raw_leading: false,
            extras: 0,
        }
    }

    /// Emit the fold decision for whatever state we're in, then reset
    /// to `Inline` with empty pending whitespace.
    ///
    /// The break count fed to `push_fold` is the number of physical
    /// line breaks in the run, EXCEPT that an escape-led run with no
    /// other breaks emits nothing (the leading `\<line-break>` cancels
    /// the fold-to-space that a raw leading break would produce).
    fn flush(&mut self, out: &mut String) {
        match self {
            Self::Inline { pending_ws } => {
                out.push_str(pending_ws);
            }
            Self::InBreakRun {
                raw_leading,
                extras,
            } => {
                // A raw leading break always counts; an escape-led
                // run only counts once there are other breaks after
                // it, so `\<newline>` alone produces nothing.
                let leading_counts = *raw_leading || *extras > 0;
                let breaks = if leading_counts { *extras + 1 } else { 0 };
                push_fold(out, breaks);
            }
        }
        *self = Self::new();
    }
}

/// Decode a double-quoted scalar body in one pass, resolving escape
/// sequences and folding raw line breaks together.
///
/// Follows YAML 1.2.2 §6.5 (flow folding) and §7.3.2 (double-quoted
/// escapes). A break run is any contiguous sequence of blanks, raw
/// line breaks, and `\<line-break>` escapes. The fold output depends
/// on whether the run started with a raw newline and how many extras
/// followed:
///
///   * no leading raw break, no extras => nothing (used when a run
///     starts with `\<line-break>` and contains no further breaks --
///     the escape swallows the fold-to-space contribution),
///   * leading raw break, no extras => one space,
///   * any N extras => N literal newlines.
///
/// `\<line-break>` participates in the fold like a raw break BUT it
/// does not set `raw_leading`, so a `\<line-break>` at the start of a
/// run drops the fold-to-space and later escapes just count as extras.
/// This matches saphyr's scanner, which is validated against the
/// yaml-test-suite (Spec Example 7.5).
fn decode_double_quoted(text: &str) -> String {
    let mut out = String::with_capacity(text.len());
    let mut state = RunState::new();
    let mut chars = text.chars().peekable();

    while let Some(ch) = chars.next() {
        match ch {
            '\n' => match &mut state {
                RunState::Inline { .. } => state = RunState::raw_led(),
                RunState::InBreakRun { extras, .. } => *extras += 1,
            },
            ' ' | '\t' => match &mut state {
                RunState::InBreakRun { .. } => {
                    // Whitespace inside a break run is inter-break
                    // indent; stripped, not content.
                }
                RunState::Inline { pending_ws } => {
                    pending_ws.push(ch);
                }
            },
            '\\' if matches!(chars.peek(), Some('\n')) => {
                // `\<line-break>` closes any current run (so its
                // fold decision uses the raw breaks it collected)
                // and starts a fresh run with `raw_leading = false`
                // so the escape's break contributes nothing on its
                // own. Preceding same-line whitespace is preserved
                // because `flush` on `Inline` writes `pending_ws`
                // out verbatim (the `\` protects it from fold-time
                // trimming).
                state.flush(&mut out);
                chars.next(); // consume '\n'
                state = RunState::escape_led();
            }
            '\\' => {
                state.flush(&mut out);
                decode_one_escape(&mut chars, &mut out);
            }
            _ => {
                state.flush(&mut out);
                out.push(ch);
            }
        }
    }
    state.flush(&mut out);
    out
}

/// Handle a single `\<x>` escape sequence. `chars` is positioned just
/// after the backslash; consumes the escape body and pushes the
/// decoded characters to `out`. Unknown escapes are passed through
/// verbatim to match the previous decoder's behavior.
fn decode_one_escape(chars: &mut std::iter::Peekable<std::str::Chars<'_>>, out: &mut String) {
    let Some(escaped) = chars.next() else {
        out.push('\\');
        return;
    };
    match escaped {
        // YAML 1.2.2 Table 5.7 escape sequences.
        '0' => out.push('\0'),
        'a' => out.push('\x07'),
        'b' => out.push('\x08'),
        't' | '\t' => out.push('\t'),
        'n' => out.push('\n'),
        'v' => out.push('\x0B'),
        'f' => out.push('\x0C'),
        'r' => out.push('\r'),
        'e' => out.push('\x1B'),
        ' ' => out.push(' '),
        '"' => out.push('"'),
        '/' => out.push('/'),
        '\\' => out.push('\\'),
        'N' => out.push('\u{85}'),   // next line
        '_' => out.push('\u{a0}'),   // non-breaking space
        'L' => out.push('\u{2028}'), // line separator
        'P' => out.push('\u{2029}'), // paragraph separator
        'x' => decode_hex_escape(chars, out, 2, 'x'),
        'u' => decode_hex_escape(chars, out, 4, 'u'),
        'U' => decode_hex_escape(chars, out, 8, 'U'),
        // Anything else is an error per spec. Preserve both the
        // backslash and the following character verbatim so callers
        // can see the raw text -- silently dropping the backslash
        // would corrupt data. The saphyr cross-check target skips
        // scalars that contain invalid escapes for exactly this
        // reason: comparing recovery strategies is not this decoder's
        // job.
        other => {
            out.push('\\');
            out.push(other);
        }
    }
}

/// Decode a `\xHH`, `\uHHHH`, or `\UHHHHHHHH` escape. On any failure
/// (short, non-hex, invalid code point) the raw escape is emitted
/// verbatim so callers can still reach later content.
fn decode_hex_escape(
    chars: &mut std::iter::Peekable<std::str::Chars<'_>>,
    out: &mut String,
    width: usize,
    prefix: char,
) {
    let mut collected = String::with_capacity(width);
    for _ in 0..width {
        match chars.peek() {
            Some(c) if c.is_ascii_hexdigit() => {
                collected.push(*c);
                chars.next();
            }
            _ => break,
        }
    }
    if collected.len() == width {
        if let Some(ch) = u32::from_str_radix(&collected, 16)
            .ok()
            .and_then(char::from_u32)
        {
            out.push(ch);
            return;
        }
    }
    out.push('\\');
    out.push(prefix);
    out.push_str(&collected);
}

/// Apply YAML 1.2 §6.5 flow-scalar line folding:
///   * Trailing whitespace on each line and leading whitespace on the
///     next are stripped.
///   * A single line break between non-empty content folds to a space.
///   * A run of `n >= 2` consecutive line breaks folds to `n - 1`
///     line breaks (i.e. one blank line becomes a literal `\n`).
///
/// Callers strip the quotes and process escapes first; this function
/// only handles the line-break folding step, which is common to both
/// single- and double-quoted scalars and to multi-line plain scalars.
fn fold_flow_line_breaks(text: &str) -> String {
    if !text.contains('\n') {
        return text.to_string();
    }
    // Split on `\n`. Each split yields `n+1` pieces for `n` line breaks,
    // so `["foo", "bar"]` came from `"foo\nbar"` (1 break) and folds to
    // `"foo bar"`; `["foo", "", "bar"]` came from 2 breaks and folds to
    // `"foo\nbar"`. In general a run of `k` empty pieces between two
    // non-empty pieces represents `k+1` breaks and folds to `k` newlines.
    // A trailing run of `k` empty pieces after the last non-empty piece
    // is `k` breaks and folds to `max(k-1, 0)` newlines (a single trailing
    // break becomes a space, per YAML 1.2 §6.5).
    //
    // Whitespace-trimming: strip trailing whitespace on any line that is
    // followed by a newline, and leading whitespace on any line that
    // follows a newline. That means the first piece only trims trailing
    // (no preceding newline) and the last piece only trims leading
    // (no following newline) -- trailing content whitespace on the very
    // last line is content, not fold-trim territory.
    let mut result = String::with_capacity(text.len());
    let mut lines = text.split('\n').peekable();
    let first = lines.next().unwrap();
    result.push_str(first.trim_end_matches([' ', '\t']));
    let mut pending_empties: usize = 0;
    while let Some(line) = lines.next() {
        let is_last = lines.peek().is_none();
        let trimmed = if is_last {
            line.trim_start_matches([' ', '\t'])
        } else {
            line.trim_matches([' ', '\t'])
        };
        if trimmed.is_empty() {
            pending_empties += 1;
            continue;
        }
        // `pending_empties` empty pieces between two non-empty pieces
        // represents `pending_empties + 1` line breaks.
        push_fold(&mut result, pending_empties + 1);
        pending_empties = 0;
        result.push_str(trimmed);
    }
    // Trailing empty pieces after the last non-empty piece: the count
    // is already the number of trailing line breaks.
    push_fold(&mut result, pending_empties);
    result
}

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
            // Double-quoted: single pass that resolves escapes and
            // folds raw line breaks together, because escape-produced
            // whitespace must not be trimmed by fold rules while raw
            // whitespace at line boundaries must be.
            decode_double_quoted(&text[1..text.len() - 1])
        } else if text.starts_with('\'') && text.ends_with('\'') {
            // Single-quoted string: `''` -> `'` then flow line-folding.
            let content = &text[1..text.len() - 1];
            let unescaped = content.replace("''", "'");
            fold_flow_line_breaks(&unescaped)
        } else if text.starts_with('|') || text.starts_with('>') {
            // Block scalar (literal or folded)
            Self::parse_block_scalar(&text)
        } else if text.contains('\n') {
            // Multi-line plain scalar: fold newlines to spaces.
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
                // Non-empty line - strip up to `base_indent` leading spaces.
                // base_indent is a character count, so we step by chars to
                // stay on UTF-8 boundaries even if the line starts with
                // multi-byte content at less than base_indent spaces of
                // indentation.
                let leading_spaces = line.chars().take_while(|c| *c == ' ').count();
                let strip = leading_spaces.min(base_indent);
                let strip_bytes = line
                    .char_indices()
                    .nth(strip)
                    .map(|(i, _)| i)
                    .unwrap_or(line.len());
                let stripped = &line[strip_bytes..];

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

    #[test]
    fn test_parse_block_scalar_multibyte_after_dedent() {
        // Regression: `base_indent` is a char count but we sliced bytes,
        // which panicked when a continuation line started with a multi-byte
        // character at less than `base_indent` spaces of indentation.
        let yaml = ">\n  a\n\u{4f1}b\n";
        // We only care that it does not panic.
        let _ = super::Scalar::parse_block_scalar(yaml);
    }

    #[test]
    fn test_flow_scalar_line_folding() {
        use std::str::FromStr;
        // Per YAML 1.2 §6.5 a single line-break in a flow scalar folds
        // to a space; a run of n breaks folds to n-1 breaks.
        let cases: &[(&str, &str)] = &[
            // Double-quoted, single break -> space.
            ("k: \"foo\nbar\"\n", "foo bar"),
            // Double-quoted, blank line -> single \n.
            ("k: \"foo\n\nbar\"\n", "foo\nbar"),
            // Double-quoted, two blank lines -> two \n.
            ("k: \"foo\n\n\nbar\"\n", "foo\n\nbar"),
            // Single-quoted, same rules.
            ("k: 'foo\nbar'\n", "foo bar"),
            ("k: 'foo\n\nbar'\n", "foo\nbar"),
            // Trailing single break folds to space; trailing run
            // folds to n-1 newlines.
            ("k: \"a\n\"\n", "a "),
            ("k: \"a\n\n\"\n", "a\n"),
            // Line-continuation `\<newline>` in double-quoted emits
            // nothing, and leading whitespace on the continuation
            // line is stripped (treated as indentation).
            ("k: \"foo\\\nbar\"\n", "foobar"),
            ("k: \"foo\\\n bar\"\n", "foobar"),
            // `\<space>` is a literal-space escape; the following
            // raw newline folds normally (single -> space), so this
            // yields two spaces before Y and one space between Y and /.
            ("k: \"\\ \nY \\/&\"\n", "  Y /&"),
            // Whitespace immediately before `\<newline>` is protected
            // by the escape: the `\` prevents the space from being
            // trimmed as trailing whitespace.
            ("k: \" \\\n\"\n", " "),
            // Trailing whitespace on the last line of a multi-line
            // single-quoted scalar is content (there is no following
            // newline to trigger fold-time trimming).
            ("k: 'a\nb '\n", "a b "),
            // Mixed raw and escaped line breaks per YAML 1.2 §7.3.2:
            // an escaped break in a run cancels the fold-to-space
            // contribution of THAT break, but subsequent raw breaks
            // still count toward the fold. Reference: saphyr, which
            // is validated against the yaml-test-suite.
            //
            // e   -> nothing (`\<nl>` swallows one break)
            ("k: \"a\\\nb\"\n", "ab"),
            // en  -> `\n` (escape then raw = 1 leftover break)
            ("k: \"a\\\n\nb\"\n", "a\nb"),
            // ne  -> space (raw first flushes as space, escape starts
            //        a new empty run)
            ("k: \"a\n\\\nb\"\n", "a b"),
            // enn -> `\n\n` (escape + 2 raw)
            ("k: \"a\\\n\n\nb\"\n", "a\n\nb"),
            // nen -> ` \n` (raw flushes to space, then escape+raw ->
            //        `\n`)
            ("k: \"a\n\\\n\nb\"\n", "a \nb"),
            // nne -> `\n` (2 raw flush to `\n`, escape starts empty
            //        run, nothing more)
            ("k: \"a\n\n\\\nb\"\n", "a\nb"),
            // een -> `\n` (first escape starts empty run, second
            //        escape closes and reopens, raw becomes extra)
            ("k: \"a\\\n\\\n\nb\"\n", "a\nb"),
            // Unicode named escapes from YAML 1.2 Table 5.7 (NEL,
            // NBSP, line separator, paragraph separator).
            ("k: \"\\N\"\n", "\u{85}"),
            ("k: \"\\_\"\n", "\u{a0}"),
            ("k: \"\\L\"\n", "\u{2028}"),
            ("k: \"\\P\"\n", "\u{2029}"),
            // `\<tab>` is the same as `\t`.
            ("k: \"\\\tx\"\n", "\tx"),
        ];
        for (yaml, expected) in cases {
            let doc = crate::yaml::Document::from_str(yaml).unwrap();
            let sc = match doc.as_mapping().unwrap().get("k").unwrap() {
                crate::as_yaml::YamlNode::Scalar(s) => s,
                _ => panic!("expected scalar"),
            };
            assert_eq!(
                &sc.as_string(),
                expected,
                "input {yaml:?}: expected {expected:?}"
            );
        }
    }
}
