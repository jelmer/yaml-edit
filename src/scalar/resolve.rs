//! Core schema type resolution and timestamp detection.

use super::decimal::signed_digits_to_decimal;
use super::{CoreScalarType, ScalarType, ScalarValue};

/// The non-string core schema type named by `tag`, if any.
fn core_tag_type(tag: &str) -> Option<CoreScalarType> {
    match crate::tag::yaml_org_name(tag)? {
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
        // The core schema's float pattern also matches decimal integers,
        // but not hex or octal ones.
        (CoreScalarType::Float, CoreScalarType::Integer) => ScalarValue::is_core_float(value),
        (t, c) => t == c,
    }
}

/// Take `min..=max` ASCII digits from the front of `rest`, returning their
/// value and how many there were.
fn take_number(rest: &mut &[char], min: usize, max: usize) -> Option<(u32, usize)> {
    let len = rest
        .iter()
        .take(max)
        .take_while(|c| c.is_ascii_digit())
        .count();
    if len < min {
        return None;
    }
    let value = rest[..len].iter().collect::<String>().parse().ok()?;
    *rest = &rest[len..];
    Some((value, len))
}

/// Take the first character of `rest` if `accept` accepts it.
fn take_char(rest: &mut &[char], accept: impl Fn(char) -> bool) -> Option<char> {
    let (&c, tail) = rest.split_first().filter(|(&c, _)| accept(c))?;
    *rest = tail;
    Some(c)
}

pub(super) fn all_digits(s: &str, radix: u32) -> bool {
    !s.is_empty() && s.chars().all(|c| c.is_digit(radix))
}

impl ScalarValue {
    /// Parse a YAML 1.2 core schema integer: `[-+]?[0-9]+` (decimal, even
    /// with leading zeros), `0o[0-7]+` or `0x[0-9a-fA-F]+`.
    pub(crate) fn parse_integer(value: &str) -> Option<i64> {
        let (radix, digits, is_negative) = Self::split_core_int(value)?;
        // Parse the magnitude wider than i64 so that i64::MIN fits.
        let magnitude = i128::from_str_radix(digits, radix).ok()?;
        i64::try_from(if is_negative { -magnitude } else { magnitude }).ok()
    }

    /// The decimal spelling of a YAML 1.2 core schema integer of any size,
    /// without leading zeros, a `+` sign or a sign on zero, so that equal
    /// values are spelled the same.
    pub(crate) fn canonical_integer(value: &str) -> Option<String> {
        let (radix, digits, is_negative) = Self::split_core_int(value)?;
        Some(signed_digits_to_decimal(digits, radix, is_negative))
    }

    /// Split a YAML 1.2 core schema integer into its radix, its digits and
    /// whether it is negative.
    fn split_core_int(value: &str) -> Option<(u32, &str, bool)> {
        if !Self::is_core_int(value) {
            return None;
        }
        Some(if let Some(hex) = value.strip_prefix("0x") {
            (16, hex, false)
        } else if let Some(oct) = value.strip_prefix("0o") {
            (8, oct, false)
        } else if let Some(dec) = value.strip_prefix('-') {
            (10, dec, true)
        } else {
            (10, value.strip_prefix('+').unwrap_or(value), false)
        })
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
    pub(crate) fn is_core_float(value: &str) -> bool {
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
    /// `0` before more octal digits, as in `0755`).
    ///
    /// YAML 1.2's `!!int` has no such form: `0755` is seven hundred and
    /// fifty five, where 1.1 read it as 493. `0o755` is the 1.2 spelling.
    pub(crate) fn is_legacy_octal(value: &str) -> bool {
        let body = value.strip_prefix(['+', '-']).unwrap_or(value);
        body.len() > 1 && body.starts_with('0') && all_digits(body, 8)
    }

    /// Whether `value` is a decimal with a leading zero and an 8 or 9 (`089`).
    ///
    /// YAML 1.2 reads it as an integer. To YAML 1.1 it is a string: it is not
    /// bare octal, and 1.1 decimal has no leading zero.
    pub(crate) fn is_leading_zero_decimal(value: &str) -> bool {
        let body = value.strip_prefix(['+', '-']).unwrap_or(value);
        body.len() > 1 && body.starts_with('0') && all_digits(body, 10) && !all_digits(body, 8)
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
            _ if Self::parse_bool(value).is_some() => return CoreScalarType::Boolean,
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
            CoreScalarType::String if Self::is_valid_timestamp(value) => ScalarType::Timestamp,
            other => other.into(),
        }
    }

    /// Create a ScalarValue from a tagged scalar node, keeping its tag and
    /// style.
    ///
    /// The core schema tags `!!int`, `!!float`, `!!bool` and `!!null` set
    /// the type when the content is a valid spelling of it; any other tag, or
    /// content the tag cannot resolve, gives a string.
    pub(crate) fn from_tagged(tagged: &crate::yaml::TaggedNode) -> Option<Self> {
        let scalar = Self::from_scalar(&tagged.value()?);
        Some(match tagged.tag() {
            Some(tag) => scalar.with_resolved_tag(tag),
            None => Self {
                scalar_type: ScalarType::String,
                ..scalar
            },
        })
    }

    /// Attach the tag written on this scalar's node, taking the type it
    /// names when the content is a valid spelling of it and a string
    /// otherwise.
    pub(crate) fn with_resolved_tag(self, tag: String) -> Self {
        let scalar_type = match core_tag_type(&tag) {
            Some(t) if core_tag_accepts(t, &self.value) => t.into(),
            _ => ScalarType::String,
        };
        Self {
            scalar_type,
            tag: Some(tag),
            ..self
        }
    }

    /// Set the type, for a tag that names one outside the core schema.
    pub(crate) fn with_type(self, scalar_type: ScalarType) -> Self {
        Self {
            scalar_type,
            ..self
        }
    }

    /// Basic timestamp format validation
    pub(super) fn is_valid_timestamp(value: &str) -> bool {
        // The forms of the YAML timestamp type
        // (https://yaml.org/type/timestamp.html): YYYY-MM-DD or
        // YYYY-MM-DDTHH:MM:SS etc. A number of seconds is not one of them.
        Self::matches_iso8601_pattern(value)
    }

    /// Whether `value` is a YAML 1.1 timestamp naming a valid date and
    /// time: a date `YYYY-MM-DD`, or
    /// `[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}([Tt]|[ \t]+)[0-9]{1,2}:[0-9]{2}:[0-9]{2}`
    /// followed by a fraction and time zone.
    fn matches_iso8601_pattern(value: &str) -> bool {
        let chars: Vec<char> = value.chars().collect();
        let rest = &mut &chars[..];
        let Some(((year, _), (month, month_len), (day, day_len))) = (|| {
            let year = take_number(rest, 4, 4)?;
            take_char(rest, |c| c == '-')?;
            let month = take_number(rest, 1, 2)?;
            take_char(rest, |c| c == '-')?;
            Some((year, month, take_number(rest, 1, 2)?))
        })() else {
            return false;
        };
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
        // A date on its own has two-digit month and day.
        if rest.is_empty() {
            return month_len == 2 && day_len == 2;
        }

        let time = (|| {
            if take_char(rest, |c| c == 'T' || c == 't').is_none() {
                take_char(rest, |c| c == ' ' || c == '\t')?;
                while take_char(rest, |c| c == ' ' || c == '\t').is_some() {}
            }
            let (hour, _) = take_number(rest, 1, 2)?;
            take_char(rest, |c| c == ':')?;
            let (minute, _) = take_number(rest, 2, 2)?;
            take_char(rest, |c| c == ':')?;
            Some((hour, minute, take_number(rest, 2, 2)?.0))
        })();
        match time {
            Some((hour, minute, second)) if hour <= 23 && minute <= 59 && second <= 59 => {
                Self::is_timestamp_tail(rest)
            }
            _ => false,
        }
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
}

#[cfg(test)]
mod tests {
    use super::*;

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
    fn test_unix_timestamp_bounds() {
        // A number of seconds is not a YAML timestamp, whatever its size.
        assert!(!ScalarValue::is_valid_timestamp("0"));
        assert!(!ScalarValue::is_valid_timestamp("1"));
        assert!(!ScalarValue::is_valid_timestamp("4102444799"));
        assert!(!ScalarValue::is_valid_timestamp("4102444800"));
        assert!(ScalarValue::is_valid_timestamp("2001-12-14"));

        // Coercion to a timestamp uses the same check.
        assert!(ScalarValue::string("1")
            .coerce_to_type(ScalarType::Timestamp)
            .is_none());
        assert_eq!(
            ScalarValue::from(1_700_000_000i64).coerce_to_type(ScalarType::Timestamp),
            None
        );
        assert_eq!(
            ScalarValue::string("2001-12-14").coerce_to_type(ScalarType::Timestamp),
            Some(ScalarValue::timestamp("2001-12-14"))
        );
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
