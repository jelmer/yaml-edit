//! Quoting style selection, escaping and unescaping of scalar content.

use super::resolve::all_digits;
use super::{CoreScalarType, ScalarStyle, ScalarValue, INDICATORS};

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

impl ScalarValue {
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

    /// Detect the appropriate style for a value
    pub(super) fn detect_style(value: &str) -> ScalarStyle {
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
            // A block scalar holds a whitespace-only line as nothing but
            // trailing spaces, which editors and readers commonly strip.
            if value
                .split('\n')
                .any(|line| !line.is_empty() && line.trim_matches([' ', '\t']).is_empty())
            {
                return ScalarStyle::DoubleQuoted;
            }
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
    pub(super) fn needs_double_quotes(value: &str) -> bool {
        value.chars().any(|c| {
            !matches!(c, '\t' | '\n' | '\x20'..='\x7E' | '\u{A0}'..='\u{FFFD}' | '\u{10000}'..)
                || matches!(c, '\u{2028}' | '\u{2029}' | '\u{FEFF}')
        })
    }

    /// Check if a value needs quoting when treated as a string
    pub(super) fn needs_quoting(value: &str) -> bool {
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
        value.parse::<f64>().is_ok() || radix_like
    }

    /// Whether `value` needs quoting even with an explicit tag deciding its
    /// type, because as a plain scalar it would not read back as `value`.
    pub(super) fn needs_quoting_as_tagged(value: &str) -> bool {
        // `-`, `?` and `:` could start a plain scalar before a safe
        // character, but are quoted anyway, as is `<`, which is not an
        // indicator. A plain scalar starting with `#` would read as a
        // comment, and the value would be lost.
        if value.starts_with(INDICATORS) || value.starts_with('<') {
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

    /// Convert to single-quoted string, indenting continuation lines by
    /// `indent` spaces.
    pub(super) fn to_single_quoted(&self, indent: usize) -> String {
        // A single-quoted scalar folds a raw line break into a space, so a
        // lone `\r` can only be written escaped.
        if self.value.contains('\r') {
            return self.to_double_quoted();
        }
        // Line folding trims whitespace around a break, which only double
        // quotes can keep.
        let white = [' ', '\t'];
        if self
            .value
            .split('\n')
            .skip(1)
            .zip(self.value.split('\n'))
            .any(|(next, prev)| next.starts_with(white) || prev.ends_with(white))
        {
            return self.to_double_quoted();
        }
        // A run of n breaks folds to n - 1 newlines, or a space for n = 1.
        let mut result = String::from("'");
        let mut prev_empty = false;
        for (i, part) in self.value.split('\n').enumerate() {
            if i > 0 {
                result.push('\n');
                if !prev_empty || i == 1 {
                    result.push('\n');
                }
                if !part.is_empty() {
                    result.push_str(&" ".repeat(indent));
                }
            }
            result.push_str(&part.replace('\'', "''"));
            prev_empty = part.is_empty();
        }
        result.push('\'');
        result
    }

    /// Convert to double-quoted string
    pub(super) fn to_double_quoted(&self) -> String {
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
}

#[cfg(test)]
mod tests {
    use super::*;

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
    fn test_parse_escape_single_quote() {
        // The \' escape decodes to a bare single quote.
        assert_eq!(ScalarValue::parse_escape_sequences("a\\'b"), "a'b");
    }
}
