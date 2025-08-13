//! Scalar value wrapper with proper escaping and style support.

use std::fmt;

#[cfg(feature = "base64")]
use base64::{engine::general_purpose, Engine as _};

/// Base64 encode bytes for binary data
#[cfg(feature = "base64")]
fn base64_encode(input: &[u8]) -> String {
    general_purpose::STANDARD.encode(input)
}

/// Base64 encode bytes for binary data (disabled when base64 feature is off)
#[cfg(not(feature = "base64"))]
fn base64_encode(_input: &[u8]) -> String {
    panic!("Binary data support requires the 'base64' feature to be enabled")
}

/// Fallback base64 encode when base64 crate is not available
#[allow(dead_code)]
fn fallback_base64_encode(input: &[u8]) -> String {
    const BASE64_CHARS: &[u8] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
    let mut result = String::new();
    let chunks = input.chunks_exact(3);
    let remainder = chunks.remainder();

    for chunk in chunks {
        let b1 = chunk[0] as usize;
        let b2 = chunk[1] as usize;
        let b3 = chunk[2] as usize;

        let combined = (b1 << 16) | (b2 << 8) | b3;

        result.push(BASE64_CHARS[(combined >> 18) & 0x3F] as char);
        result.push(BASE64_CHARS[(combined >> 12) & 0x3F] as char);
        result.push(BASE64_CHARS[(combined >> 6) & 0x3F] as char);
        result.push(BASE64_CHARS[combined & 0x3F] as char);
    }

    // Handle remainder
    match remainder.len() {
        1 => {
            let b1 = remainder[0] as usize;
            let combined = b1 << 16;
            result.push(BASE64_CHARS[(combined >> 18) & 0x3F] as char);
            result.push(BASE64_CHARS[(combined >> 12) & 0x3F] as char);
            result.push_str("==");
        }
        2 => {
            let b1 = remainder[0] as usize;
            let b2 = remainder[1] as usize;
            let combined = (b1 << 16) | (b2 << 8);
            result.push(BASE64_CHARS[(combined >> 18) & 0x3F] as char);
            result.push(BASE64_CHARS[(combined >> 12) & 0x3F] as char);
            result.push(BASE64_CHARS[(combined >> 6) & 0x3F] as char);
            result.push('=');
        }
        _ => {}
    }

    result
}

/// Base64 decode string back to bytes
#[cfg(feature = "base64")]
fn base64_decode(input: &str) -> Result<Vec<u8>, String> {
    general_purpose::STANDARD
        .decode(input.trim())
        .map_err(|e| format!("Base64 decode error: {}", e))
}

/// Base64 decode string back to bytes (disabled when base64 feature is off)
#[cfg(not(feature = "base64"))]
fn base64_decode(_input: &str) -> Result<Vec<u8>, String> {
    Err("Binary data support requires the 'base64' feature to be enabled".to_string())
}

/// Fallback base64 decode when base64 crate is not available
#[allow(dead_code)]
fn fallback_base64_decode(input: &str) -> Result<Vec<u8>, String> {
    let input = input.trim();
    if input.is_empty() {
        return Ok(Vec::new());
    }

    // Check for valid base64 length (must be multiple of 4)
    if input.len() % 4 != 0 {
        return Err("Invalid base64 length - must be multiple of 4".to_string());
    }

    // Count padding chars
    let padding_count = input.chars().rev().take_while(|&c| c == '=').count();
    if padding_count > 2 {
        return Err("Too much padding".to_string());
    }

    // Extract the base64 content without padding
    let content_len = input.len() - padding_count;
    let content = &input[..content_len];

    let mut result = Vec::new();
    let mut chars = content.chars();

    while let Some(c1) = chars.next() {
        let c2 = chars.next().ok_or("Incomplete base64 group")?;
        let c3 = chars.next();
        let c4 = chars.next();

        let b1 = base64_char_to_index(c1)?;
        let b2 = base64_char_to_index(c2)?;
        let b3 = c3.map(base64_char_to_index).transpose()?;
        let b4 = c4.map(base64_char_to_index).transpose()?;

        let combined = (b1 << 18) | (b2 << 12) | (b3.unwrap_or(0) << 6) | b4.unwrap_or(0);

        result.push((combined >> 16) as u8);
        if b3.is_some() {
            result.push((combined >> 8) as u8);
        }
        if b4.is_some() {
            result.push(combined as u8);
        }
    }

    // Handle padding validation
    match padding_count {
        0 => {
            // No padding - content length must be multiple of 4
            if content_len % 4 != 0 {
                return Err("Missing padding".to_string());
            }
        }
        1 => {
            // One padding char - content should be 3 mod 4
            if content_len % 4 != 3 {
                return Err("Invalid padding for length".to_string());
            }
        }
        2 => {
            // Two padding chars - content should be 2 mod 4
            if content_len % 4 != 2 {
                return Err("Invalid padding for length".to_string());
            }
        }
        _ => return Err("Invalid padding count".to_string()),
    }

    Ok(result)
}

/// Convert base64 character to its 6-bit value
fn base64_char_to_index(c: char) -> Result<u32, String> {
    match c {
        'A'..='Z' => Ok(c as u32 - 'A' as u32),
        'a'..='z' => Ok(c as u32 - 'a' as u32 + 26),
        '0'..='9' => Ok(c as u32 - '0' as u32 + 52),
        '+' => Ok(62),
        '/' => Ok(63),
        _ => Err(format!("Invalid base64 character: {}", c)),
    }
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
        let mut result = String::with_capacity(text.len()); // Pre-allocate
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
                            let mut hex_chars = [0u8; 2];
                            let mut count = 0;
                            for (i, ch) in chars.by_ref().take(2).enumerate() {
                                if let Some(digit) = ch.to_digit(16) {
                                    hex_chars[i] = digit as u8;
                                    count += 1;
                                } else {
                                    // Put back invalid char
                                    result.push('\\');
                                    result.push('x');
                                    for &hex_char in hex_chars.iter().take(count) {
                                        result.push(char::from_digit(hex_char as u32, 16).unwrap());
                                    }
                                    result.push(ch);
                                    break;
                                }
                            }
                            if count == 2 {
                                let code = hex_chars[0] * 16 + hex_chars[1];
                                result.push(code as char);
                            } else if count > 0 {
                                // Incomplete hex escape
                                result.push('\\');
                                result.push('x');
                                for &hex_char in hex_chars.iter().take(count) {
                                    result.push(char::from_digit(hex_char as u32, 16).unwrap());
                                }
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

    /// Create a binary scalar from raw bytes
    #[cfg(feature = "base64")]
    pub fn binary(data: &[u8]) -> Self {
        let encoded = base64_encode(data);
        Self {
            value: encoded,
            style: ScalarStyle::Plain,
            scalar_type: ScalarType::Binary,
        }
    }

    /// Create a timestamp scalar
    pub fn timestamp(value: impl Into<String>) -> Self {
        Self {
            value: value.into(),
            style: ScalarStyle::Plain,
            scalar_type: ScalarType::Timestamp,
        }
    }

    /// Create a regex scalar
    pub fn regex(pattern: impl Into<String>) -> Self {
        Self {
            value: pattern.into(),
            style: ScalarStyle::Plain,
            scalar_type: ScalarType::Regex,
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

    /// Get the scalar type
    pub fn scalar_type(&self) -> ScalarType {
        self.scalar_type
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
            }),
            ScalarType::Integer => {
                if let Some(int_val) = Self::parse_integer(&self.value) {
                    Some(ScalarValue::from(int_val))
                } else {
                    None
                }
            }
            ScalarType::Float => {
                if let Ok(float_val) = self.value.parse::<f64>() {
                    Some(ScalarValue::from(float_val))
                } else {
                    None
                }
            }
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

    /// Parse an integer with support for various formats
    /// Supports: decimal, hexadecimal (0x), binary (0b), octal (0o and legacy 0)
    fn parse_integer(value: &str) -> Option<i64> {
        let value = value.trim();

        // Handle negative numbers
        let (is_negative, value) = if value.starts_with('-') {
            (true, &value[1..])
        } else if value.starts_with('+') {
            (false, &value[1..])
        } else {
            (false, value)
        };

        let parsed = if value.starts_with("0x") || value.starts_with("0X") {
            // Hexadecimal
            i64::from_str_radix(&value[2..], 16).ok()
        } else if value.starts_with("0b") || value.starts_with("0B") {
            // Binary
            i64::from_str_radix(&value[2..], 2).ok()
        } else if value.starts_with("0o") || value.starts_with("0O") {
            // Modern octal
            i64::from_str_radix(&value[2..], 8).ok()
        } else if value.starts_with('0')
            && value.len() > 1
            && value.chars().all(|c| c.is_ascii_digit())
        {
            // Legacy octal (starts with 0 but not 0x, 0b, 0o)
            i64::from_str_radix(value, 8).ok()
        } else {
            // Decimal
            value.parse::<i64>().ok()
        };

        parsed.map(|n| if is_negative { -n } else { n })
    }

    /// Auto-detect the most appropriate scalar type from a string value
    pub fn auto_detect_type(value: &str) -> ScalarType {
        // Check for null values first
        match value.to_lowercase().as_str() {
            "null" | "~" | "" => return ScalarType::Null,
            _ => {}
        }

        // Check for boolean values
        match value.to_lowercase().as_str() {
            "true" | "false" | "yes" | "no" | "on" | "off" => return ScalarType::Boolean,
            _ => {}
        }

        // Check for numbers with various formats
        if Self::parse_integer(value).is_some() {
            return ScalarType::Integer;
        }
        if value.parse::<f64>().is_ok() {
            return ScalarType::Float;
        }

        // Check for timestamps (basic patterns)
        if Self::is_valid_timestamp_static(value) {
            return ScalarType::Timestamp;
        }

        // Check for binary data (base64)
        #[cfg(feature = "base64")]
        if Self::looks_like_base64(value) && base64_decode(value).is_ok() {
            return ScalarType::Binary;
        }

        // Default to string
        ScalarType::String
    }

    /// Create a scalar with automatic type detection
    pub fn auto_typed(value: impl Into<String>) -> Self {
        let value = value.into();
        let scalar_type = Self::auto_detect_type(&value);
        let style = Self::detect_style(&value);

        Self {
            value,
            style,
            scalar_type,
        }
    }

    /// Check if a string looks like base64 encoded data
    #[cfg(feature = "base64")]
    fn looks_like_base64(value: &str) -> bool {
        if value.is_empty() {
            return false;
        }

        // Must be reasonable length and contain only base64 characters
        // Also need to check that padding is only at the end
        if value.len() < 4 || value.len() % 4 != 0 {
            return false;
        }

        let padding_count = value.chars().filter(|&c| c == '=').count();
        if padding_count > 2 {
            return false;
        }

        // Check all characters are valid base64
        if !value
            .chars()
            .all(|c| matches!(c, 'A'..='Z' | 'a'..='z' | '0'..='9' | '+' | '/' | '='))
        {
            return false;
        }

        // Check that padding is only at the end
        if padding_count > 0 {
            let padding_start = value.len() - padding_count;
            if !value[padding_start..].chars().all(|c| c == '=') {
                return false;
            }
            // Check that non-padding part doesn't contain '='
            if value[..padding_start].contains('=') {
                return false;
            }
        }

        // Final validation: try to decode it to ensure it's actually valid base64
        // This will catch cases like "SGVs" which looks valid but isn't proper base64
        base64_decode(value).is_ok()
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

        if let (Ok(month), Ok(day)) = (month_str.parse::<u8>(), day_str.parse::<u8>()) {
            if !(1..=12).contains(&month) || !(1..=31).contains(&day) {
                return false;
            }
        } else {
            return false;
        }

        // If it's just YYYY-MM-DD, that's valid
        if chars.len() == 10 {
            return true;
        }

        // Check for time part: T or space followed by HH:MM:SS
        if chars.len() >= 19 {
            let sep = chars[10];
            if (sep == 'T' || sep == ' ')
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

                return true;
            }
        }

        false
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
        if value.parse::<f64>().is_ok() || Self::parse_integer(value).is_some() {
            return true;
        }

        // Check for special characters that require quoting
        for ch in value.chars() {
            match ch {
                ':' | '#' | '&' | '*' | '!' | '|' | '>' | '\'' | '"' | '%' => return true,
                // Note: @ and ` are reserved but allowed in plain scalars in YAML 1.2
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
        // For special data types, always include the tag regardless of style
        let tag_prefix = match self.scalar_type {
            #[cfg(feature = "base64")]
            ScalarType::Binary => "!!binary ",
            ScalarType::Timestamp => "!!timestamp ",
            ScalarType::Regex => "!!regex ",
            _ => "",
        };

        let content = match self.style {
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
                    | ScalarType::Null
                    | ScalarType::Timestamp
                    | ScalarType::Regex => self.value.clone(),
                    #[cfg(feature = "base64")]
                    ScalarType::Binary => self.value.clone(),
                }
            }
            ScalarStyle::SingleQuoted => self.to_single_quoted(),
            ScalarStyle::DoubleQuoted => self.to_double_quoted(),
            ScalarStyle::Literal => self.to_literal(),
            ScalarStyle::Folded => self.to_folded(),
        };

        format!("{}{}", tag_prefix, content)
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
        self.to_literal_with_indent(2)
    }

    /// Convert to folded block scalar
    fn to_folded(&self) -> String {
        self.to_folded_with_indent(2)
    }

    /// Convert to literal block scalar with specific indentation
    pub fn to_literal_with_indent(&self, indent: usize) -> String {
        let indent_str = " ".repeat(indent);

        // Detect the existing indentation of the content
        let existing_indent = self.detect_content_indentation();

        // If content already has consistent indentation, preserve it
        if existing_indent.is_some() {
            format!("|\n{}", self.value)
        } else {
            // Add consistent indentation
            let indented = self
                .value
                .lines()
                .map(|line| {
                    if line.trim().is_empty() {
                        String::new()
                    } else {
                        format!("{}{}", indent_str, line)
                    }
                })
                .collect::<Vec<_>>()
                .join("\n");
            format!("|\n{}", indented)
        }
    }

    /// Convert to folded block scalar with specific indentation
    pub fn to_folded_with_indent(&self, indent: usize) -> String {
        let indent_str = " ".repeat(indent);

        // Detect the existing indentation of the content
        let existing_indent = self.detect_content_indentation();

        // If content already has consistent indentation, preserve it
        if existing_indent.is_some() {
            format!(">\n{}", self.value)
        } else {
            // Add consistent indentation
            let indented = self
                .value
                .lines()
                .map(|line| {
                    if line.trim().is_empty() {
                        String::new()
                    } else {
                        format!("{}{}", indent_str, line)
                    }
                })
                .collect::<Vec<_>>()
                .join("\n");
            format!(">\n{}", indented)
        }
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
    fn test_indentation_preservation() {
        // Test preserving exact indentation in block scalars
        let content_with_indent = "  Line 1\n    Line 2 more indented\n  Line 3";
        let scalar = ScalarValue::literal(content_with_indent);

        // Should detect that content already has indentation and preserve it
        let yaml_output = scalar.to_literal_with_indent(2);
        assert!(yaml_output.contains("  Line 1"));
        assert!(yaml_output.contains("    Line 2 more indented"));
        assert!(yaml_output.contains("  Line 3"));
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
        assert!(yaml_4_spaces.contains("    Line 1"));
        assert!(yaml_4_spaces.contains("    Line 2"));
        assert!(yaml_4_spaces.contains("    Line 3"));

        let yaml_1_space = scalar.to_literal_with_indent(1);
        assert!(yaml_1_space.contains(" Line 1"));
        assert!(yaml_1_space.contains(" Line 2"));
        assert!(yaml_1_space.contains(" Line 3"));
    }

    #[test]
    fn test_folded_with_custom_indent() {
        // Test applying custom indentation to folded scalars
        let content = "Line 1\nLine 2\nLine 3";
        let scalar = ScalarValue::folded(content);

        let yaml_3_spaces = scalar.to_folded_with_indent(3);
        assert!(yaml_3_spaces.starts_with(">\n"));
        assert!(yaml_3_spaces.contains("   Line 1"));
        assert!(yaml_3_spaces.contains("   Line 2"));
        assert!(yaml_3_spaces.contains("   Line 3"));
    }

    #[test]
    fn test_mixed_empty_lines_preservation() {
        // Test handling of empty lines in block scalars
        let content_with_empty_lines = "Line 1\n\nLine 3\n\n\nLine 6";
        let scalar = ScalarValue::literal(content_with_empty_lines);

        let yaml_output = scalar.to_literal_with_indent(2);
        assert!(yaml_output.contains("  Line 1"));
        assert!(yaml_output.contains("  Line 3"));
        assert!(yaml_output.contains("  Line 6"));

        // Empty lines should remain empty (no indentation added)
        let lines: Vec<&str> = yaml_output.lines().collect();
        assert!(lines.iter().any(|line| line.is_empty()));
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
            "1703515845", // Unix timestamp
        ];

        for ts in valid_timestamps {
            let scalar = ScalarValue::timestamp(ts);
            assert!(scalar.is_timestamp());
            assert_eq!(scalar.scalar_type(), ScalarType::Timestamp);
            assert_eq!(scalar.value(), ts);

            let yaml_output = scalar.to_yaml_string();
            assert_eq!(yaml_output, format!("!!timestamp {}", ts));
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
    fn test_type_coercion() {
        // Test coercing string to integer
        let str_scalar = ScalarValue::new("42");
        let int_scalar = str_scalar.coerce_to_type(ScalarType::Integer).unwrap();
        assert_eq!(int_scalar.scalar_type(), ScalarType::Integer);
        assert_eq!(int_scalar.value(), "42");

        // Test coercing string to boolean
        let bool_scalar = ScalarValue::new("true")
            .coerce_to_type(ScalarType::Boolean)
            .unwrap();
        assert_eq!(bool_scalar.scalar_type(), ScalarType::Boolean);
        assert_eq!(bool_scalar.value(), "true");

        // Test coercing boolean string variations
        let yes_scalar = ScalarValue::new("yes")
            .coerce_to_type(ScalarType::Boolean)
            .unwrap();
        assert_eq!(yes_scalar.value(), "true");

        let no_scalar = ScalarValue::new("no")
            .coerce_to_type(ScalarType::Boolean)
            .unwrap();
        assert_eq!(no_scalar.value(), "false");

        // Test failed coercion
        let str_scalar = ScalarValue::new("not_a_number");
        assert!(str_scalar.coerce_to_type(ScalarType::Integer).is_none());
    }

    #[test]
    fn test_auto_type_detection() {
        // Test various automatic type detections
        assert_eq!(ScalarValue::auto_detect_type("42"), ScalarType::Integer);
        assert_eq!(ScalarValue::auto_detect_type("3.14"), ScalarType::Float);
        assert_eq!(ScalarValue::auto_detect_type("true"), ScalarType::Boolean);
        assert_eq!(ScalarValue::auto_detect_type("false"), ScalarType::Boolean);
        assert_eq!(ScalarValue::auto_detect_type("yes"), ScalarType::Boolean);
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
        #[cfg(feature = "base64")]
        assert_eq!(
            ScalarValue::auto_detect_type("SGVsbG8gV29ybGQ="),
            ScalarType::Binary
        );
        #[cfg(not(feature = "base64"))]
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
    fn test_auto_typed_scalar_creation() {
        let int_scalar = ScalarValue::auto_typed("123");
        assert_eq!(int_scalar.scalar_type(), ScalarType::Integer);

        let bool_scalar = ScalarValue::auto_typed("true");
        assert_eq!(bool_scalar.scalar_type(), ScalarType::Boolean);

        let timestamp_scalar = ScalarValue::auto_typed("2023-12-25");
        assert_eq!(timestamp_scalar.scalar_type(), ScalarType::Timestamp);

        let string_scalar = ScalarValue::auto_typed("hello world");
        assert_eq!(string_scalar.scalar_type(), ScalarType::String);
    }

    #[test]
    fn test_timestamp_pattern_matching() {
        // Valid patterns
        assert!(ScalarValue::matches_iso8601_pattern("2023-12-25"));
        assert!(ScalarValue::matches_iso8601_pattern("2023-12-25T10:30:45"));
        assert!(ScalarValue::matches_iso8601_pattern("2023-12-25 10:30:45"));
        assert!(ScalarValue::matches_iso8601_pattern("2023-01-01T00:00:00"));

        // Invalid patterns
        assert!(!ScalarValue::matches_iso8601_pattern("2023-13-25")); // Invalid month
        assert!(!ScalarValue::matches_iso8601_pattern("23-12-25")); // Wrong year format
        assert!(!ScalarValue::matches_iso8601_pattern("2023/12/25")); // Wrong separator
        assert!(!ScalarValue::matches_iso8601_pattern("not-a-date"));
        assert!(!ScalarValue::matches_iso8601_pattern("2023"));
    }

    #[test]
    #[cfg(feature = "base64")]
    fn test_base64_detection() {
        // Valid base64 strings
        assert!(ScalarValue::looks_like_base64("SGVsbG8=")); // "Hello"
        assert!(ScalarValue::looks_like_base64("V29ybGQ=")); // "World"
        assert!(ScalarValue::looks_like_base64("SGVsbG8gV29ybGQ=")); // "Hello World"
        assert!(ScalarValue::looks_like_base64("AAAA")); // All A's

        // Invalid base64 strings
        assert!(!ScalarValue::looks_like_base64("Hello")); // No padding, wrong chars
        assert!(!ScalarValue::looks_like_base64("SGVsbG8")); // Missing padding (7 chars, should be 8 with padding)
        assert!(!ScalarValue::looks_like_base64("")); // Empty
        assert!(!ScalarValue::looks_like_base64("SGV@")); // Invalid character
        assert!(!ScalarValue::looks_like_base64("SGVsbG8g===")); // Too much padding
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
        let mut styled_binary = binary_scalar.clone();
        styled_binary.style = ScalarStyle::DoubleQuoted;

        // The to_yaml_string should still respect the scalar type for tagging
        assert!(styled_binary.to_yaml_string().contains("!!binary"));
    }

    #[test]
    fn test_type_checking_methods() {
        #[cfg(feature = "base64")]
        let binary_scalar = ScalarValue::binary(b"test");
        let timestamp_scalar = ScalarValue::timestamp("2023-12-25");
        let regex_scalar = ScalarValue::regex(r"\d+");
        let string_scalar = ScalarValue::new("hello");

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
    fn test_binary_number_parsing() {
        // Test binary number parsing (0b prefix)
        assert_eq!(ScalarValue::parse_integer("0b1010"), Some(10));
        assert_eq!(ScalarValue::parse_integer("0b11111111"), Some(255));
        assert_eq!(ScalarValue::parse_integer("0B101"), Some(5)); // Uppercase B
        assert_eq!(ScalarValue::parse_integer("-0b1010"), Some(-10));
        assert_eq!(ScalarValue::parse_integer("+0b101"), Some(5));

        // Test auto-detection
        assert_eq!(ScalarValue::auto_detect_type("0b1010"), ScalarType::Integer);
        assert_eq!(
            ScalarValue::auto_detect_type("0B11111111"),
            ScalarType::Integer
        );

        // Test invalid binary
        assert_eq!(ScalarValue::parse_integer("0b1012"), None); // Contains invalid digit
        assert_eq!(ScalarValue::parse_integer("0b"), None); // Empty after prefix
    }

    #[test]
    fn test_modern_octal_number_parsing() {
        // Test modern octal number parsing (0o prefix)
        assert_eq!(ScalarValue::parse_integer("0o755"), Some(493)); // 7*64 + 5*8 + 5
        assert_eq!(ScalarValue::parse_integer("0o644"), Some(420)); // 6*64 + 4*8 + 4
        assert_eq!(ScalarValue::parse_integer("0O777"), Some(511)); // Uppercase O
        assert_eq!(ScalarValue::parse_integer("-0o755"), Some(-493));
        assert_eq!(ScalarValue::parse_integer("+0o644"), Some(420));

        // Test auto-detection
        assert_eq!(ScalarValue::auto_detect_type("0o755"), ScalarType::Integer);
        assert_eq!(ScalarValue::auto_detect_type("0O644"), ScalarType::Integer);

        // Test invalid octal
        assert_eq!(ScalarValue::parse_integer("0o789"), None); // Contains invalid digit
        assert_eq!(ScalarValue::parse_integer("0o"), None); // Empty after prefix
    }

    #[test]
    fn test_legacy_octal_number_parsing() {
        // Test legacy octal number parsing (0 prefix)
        assert_eq!(ScalarValue::parse_integer("0755"), Some(493));
        assert_eq!(ScalarValue::parse_integer("0644"), Some(420));
        assert_eq!(ScalarValue::parse_integer("0777"), Some(511));

        // Test auto-detection
        assert_eq!(ScalarValue::auto_detect_type("0755"), ScalarType::Integer);
        assert_eq!(ScalarValue::auto_detect_type("0644"), ScalarType::Integer);

        // Test edge cases
        assert_eq!(ScalarValue::parse_integer("0"), Some(0)); // Single zero
        assert_eq!(ScalarValue::parse_integer("00"), Some(0)); // Double zero

        // Numbers starting with 0 but containing 8 or 9 should fail as octal
        assert_eq!(ScalarValue::parse_integer("0789"), None);
        assert_eq!(ScalarValue::parse_integer("0128"), None);
    }

    #[test]
    fn test_hexadecimal_number_parsing() {
        // Test hexadecimal number parsing (0x prefix) - should still work
        assert_eq!(ScalarValue::parse_integer("0xFF"), Some(255));
        assert_eq!(ScalarValue::parse_integer("0x1A"), Some(26));
        assert_eq!(ScalarValue::parse_integer("0XFF"), Some(255)); // Uppercase X
        assert_eq!(ScalarValue::parse_integer("-0xFF"), Some(-255));
        assert_eq!(ScalarValue::parse_integer("+0x1A"), Some(26));

        // Test auto-detection
        assert_eq!(ScalarValue::auto_detect_type("0xFF"), ScalarType::Integer);
        assert_eq!(ScalarValue::auto_detect_type("0X1A"), ScalarType::Integer);
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
        let binary_scalar = ScalarValue::auto_typed("0b1010");
        assert_eq!(binary_scalar.scalar_type(), ScalarType::Integer);
        assert_eq!(binary_scalar.value(), "0b1010");

        let octal_scalar = ScalarValue::auto_typed("0o755");
        assert_eq!(octal_scalar.scalar_type(), ScalarType::Integer);
        assert_eq!(octal_scalar.value(), "0o755");

        let hex_scalar = ScalarValue::auto_typed("0xFF");
        assert_eq!(hex_scalar.scalar_type(), ScalarType::Integer);
        assert_eq!(hex_scalar.value(), "0xFF");

        let legacy_octal_scalar = ScalarValue::auto_typed("0755");
        assert_eq!(legacy_octal_scalar.scalar_type(), ScalarType::Integer);
        assert_eq!(legacy_octal_scalar.value(), "0755");
    }
}
