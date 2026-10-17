//! Rewriting YAML 1.1 constructs into their YAML 1.2 spellings.
//!
//! Several plain-scalar forms changed meaning between the versions, all
//! silently: none is a syntax error under either version.
//!
//! - A bare leading zero was octal, so `0755` was 493 and is now 755.
//! - `yes`, `no`, `on` and `off` (in any of their 1.1 capitalisations)
//!   were booleans, and are now plain strings.
//! - Binary (`0b1010`), signed hex (`-0x1F`), base 60 (`1:30`, `1:30.5`)
//!   and numbers with underscores (`1_000`, `0x_1F`) were numbers, and are
//!   now plain strings.
//! - Leading-zero decimals with an 8 or 9 (`09`), `0o` octal (`0o17`) and
//!   floats with an exponent but no `.` or no exponent sign (`1e3`,
//!   `1.0e3`) were strings, and are now numbers.
//!
//! [`to_yaml_1_2`] rewrites them so the document keeps the value a YAML 1.1
//! reader saw: `0755` becomes `0o755`, `yes` becomes `true`, `1:30`
//! becomes `90`, `1_000` becomes `1000` and `09` becomes `'09'`. 1.2 has no
//! signed octal, hex or binary, so `-0755` becomes the decimal `-493`.
//! Everything else, formatting and comments included, is left alone.
//!
//! Mapping keys are rewritten too, so a 1.1 key `yes` becomes `true`.
//!
//! The single-letter booleans `y` and `n` are left alone: they are rarely
//! meant as booleans, and 1.1 readers such as PyYAML treat them as strings.
//!
//! [`crate::validator::Rule::LegacyYaml11`] reports the octal,
//! leading-zero decimal and `yes`/`no`/`on`/`off` forms without changing
//! anything, for callers who would rather decide per site.

use crate::lex::SyntaxKind;
use crate::scalar::{digits_to_decimal, places_to_decimal, CoreScalarType, ScalarValue};
use crate::yaml::{Document, SyntaxNode};
use rowan::ast::AstNode;

/// A scalar that [`to_yaml_1_2`] rewrote.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Rewrite {
    /// The text as the document had it.
    pub before: String,
    /// The text it was replaced with.
    pub after: String,
}

/// Rewrite the YAML 1.1 constructs in `doc` to their 1.2 spellings,
/// returning what changed.
///
/// The value a YAML 1.1 reader saw is preserved, which is the reason to
/// migrate at all: `mode: 0755` meant 493 and becomes `mode: 0o755`, which
/// still means 493. `flag: yes` meant true and becomes `flag: true`.
///
/// A quoted scalar is already unambiguous, so it is never touched.
///
/// Run it once per document: the input is read as YAML 1.1, where the
/// `0o755` it writes for `0755` is a string, so a second run quotes it.
///
/// # Example
///
/// ```rust
/// # use std::str::FromStr;
/// # use yaml_edit::Document;
/// let doc = Document::from_str("mode: 0755\nflag: yes\n").unwrap();
/// let rewrites = yaml_edit::migrate::to_yaml_1_2(&doc);
///
/// assert_eq!(doc.to_string(), "mode: 0o755\nflag: true\n");
/// assert_eq!(rewrites.len(), 2);
/// ```
///
/// Mutates in place despite `&doc` (see crate docs on interior mutability).
pub fn to_yaml_1_2(doc: &Document) -> Vec<Rewrite> {
    let mut rewrites = Vec::new();
    rewrite_in(doc.syntax(), &mut rewrites);
    rewrites
}

fn rewrite_in(node: &SyntaxNode, rewrites: &mut Vec<Rewrite>) {
    for child in node.children() {
        if child.kind() == SyntaxKind::SCALAR {
            if let Some((after, kind)) = replacement_for(&child) {
                let before = child.text().to_string();
                let count = child.children_with_tokens().count();
                crate::nodes::replace_children(
                    &child,
                    0..count,
                    vec![crate::nodes::fresh_token(kind, &after).into()],
                );
                rewrites.push(Rewrite { before, after });
            }
            continue;
        }
        rewrite_in(&child, rewrites);
    }
}

/// The 1.2 spelling of `scalar`, with the token kind it should carry, or
/// `None` when the scalar is already unambiguous.
fn replacement_for(scalar: &SyntaxNode) -> Option<(String, SyntaxKind)> {
    // A scalar holding anything but its own value token (a block scalar's
    // header, say) is not a plain scalar to reinterpret.
    let mut tokens = scalar.children_with_tokens();
    let token = tokens.next()?.into_token()?;
    if tokens.next().is_some() {
        return None;
    }
    if !matches!(
        token.kind(),
        SyntaxKind::INT | SyntaxKind::FLOAT | SyntaxKind::STRING | SyntaxKind::BOOL
    ) {
        return None;
    }

    let text = token.text();
    // Quoting already settles the reading.
    if text.starts_with(['"', '\'']) {
        return None;
    }

    // So does an explicit tag: `!!str yes` is the string in both versions,
    // and `!!int 0755` is whatever the tag says it is.
    if scalar
        .parent()
        .is_some_and(|p| p.kind() == SyntaxKind::TAGGED_NODE)
    {
        return None;
    }

    if let Some(number) = yaml_1_1_number(text) {
        return Some(number);
    }

    match text {
        "yes" | "Yes" | "YES" | "on" | "On" | "ON" => Some(("true".into(), SyntaxKind::BOOL)),
        "no" | "No" | "NO" | "off" | "Off" | "OFF" => Some(("false".into(), SyntaxKind::BOOL)),
        _ if yaml_1_1_string(text) => Some((format!("'{text}'"), SyntaxKind::STRING)),
        _ => None,
    }
}

/// Whether a YAML 1.1 reader saw plain `text` as a string where the 1.2
/// core schema resolves it to something else.
///
/// The 1.2 `!!bool` and `!!null` forms are all 1.1 booleans and nulls, and
/// 1.2 hex, `.inf` and `.nan` are 1.1 numbers. That leaves, per the 1.1
/// `!!int` and `!!float` regular expressions:
///
/// - a decimal with a leading zero and an 8 or 9 (`09`), which is not 1.1
///   octal, while 1.1 decimal has no leading zero
/// - `0o` octal, which 1.1 did not have
/// - an exponent without a `.` (`1e3`) or without a sign (`1.0e3`), as 1.1
///   floats need both
fn yaml_1_1_string(text: &str) -> bool {
    match ScalarValue::classify_plain(text) {
        CoreScalarType::Integer => {
            let body = text.strip_prefix(['+', '-']).unwrap_or(text);
            body.starts_with("0o") || ScalarValue::is_leading_zero_decimal(text)
        }
        CoreScalarType::Float => text
            .find(['e', 'E'])
            .is_some_and(|i| !text[..i].contains('.') || !text[i + 1..].starts_with(['+', '-'])),
        _ => false,
    }
}

/// The 1.2 spelling of a YAML 1.1 number whose 1.2 reading differs, per the
/// 1.1 `!!int` and `!!float` regular expressions.
fn yaml_1_1_number(text: &str) -> Option<(String, SyntaxKind)> {
    let (sign, body) = match text.strip_prefix(['+', '-']) {
        Some(body) => (&text[..1], body),
        None => ("", text),
    };
    let negative = sign == "-";
    let decimal = |digits: &str, radix: u32| {
        let value = digits_to_decimal(digits, radix);
        let minus = if negative && value != "0" { "-" } else { "" };
        (format!("{minus}{value}"), SyntaxKind::INT)
    };

    if let Some(digits) = body.strip_prefix("0b") {
        return radix_digits(digits, 2).map(|d| decimal(&d, 2));
    }
    if let Some(written) = body.strip_prefix("0x") {
        let digits = radix_digits(written, 16)?;
        // 1.2 has no signed hex, and needs no rewrite for plain hex.
        if !sign.is_empty() {
            return Some(decimal(&digits, 16));
        }
        return written
            .contains('_')
            .then(|| (format!("0x{digits}"), SyntaxKind::INT));
    }
    if body.contains(':') {
        return sexagesimal(sign, body);
    }
    // A bare leading zero was octal. 1.2 has no signed `0o`, so a negative
    // one becomes decimal.
    if let Some(digits) = body.strip_prefix('0').and_then(|d| radix_digits(d, 8)) {
        if negative {
            return Some(decimal(&digits, 8));
        }
        let digits = digits.trim_start_matches('0');
        let digits = if digits.is_empty() { "0" } else { digits };
        return Some((format!("0o{digits}"), SyntaxKind::INT));
    }

    if !body.contains('_') || !body.starts_with(|c: char| c.is_ascii_digit()) {
        return None;
    }
    let (mantissa, exponent) = match body.find(['e', 'E']) {
        Some(i) => body.split_at(i),
        None => (body, ""),
    };
    let is_digits = |s: &str| s.bytes().all(|b| b.is_ascii_digit() || b == b'_');
    let kind = match mantissa.split_once('.') {
        None if exponent.is_empty() && is_digits(mantissa) && !mantissa.starts_with('0') => {
            SyntaxKind::INT
        }
        Some((int, frac)) if is_digits(int) && is_digits(frac) => SyntaxKind::FLOAT,
        _ => return None,
    };
    if !exponent.is_empty() {
        let digits = exponent[1..].strip_prefix(['+', '-'])?;
        if digits.is_empty() || !digits.bytes().all(|b| b.is_ascii_digit()) {
            return None;
        }
    }
    Some((format!("{sign}{}", body.replace('_', "")), kind))
}

/// The digits of a YAML 1.1 number in `radix` with its underscores
/// removed, or `None` if it is not one.
fn radix_digits(digits: &str, radix: u32) -> Option<String> {
    if !digits.chars().all(|c| c == '_' || c.is_digit(radix)) {
        return None;
    }
    let digits = digits.replace('_', "");
    (!digits.is_empty()).then_some(digits)
}

/// Rewrite a 1.1 base 60 number such as `1:30` (90) or `1:30.5` (90.5) to
/// decimal.
fn sexagesimal(sign: &str, body: &str) -> Option<(String, SyntaxKind)> {
    let mut parts = body.split(':');
    let first = parts.next()?;
    let mut rest: Vec<&str> = parts.collect();
    let last = rest.pop()?;
    let (last, frac) = match last.split_once('.') {
        Some((last, frac)) => (last, Some(frac)),
        None => (last, None),
    };
    // An integer may not start with 0; a float may.
    if !first.starts_with(|c: char| c.is_ascii_digit())
        || (frac.is_none() && first.starts_with('0'))
    {
        return None;
    }
    let mut places: Vec<(u32, u32)> = radix_digits(first, 10)?
        .chars()
        .map(|c| (10, c.to_digit(10).expect("checked by radix_digits")))
        .collect();
    for part in rest.into_iter().chain([last]) {
        let digit = match part.as_bytes() {
            [d @ b'0'..=b'9'] => d - b'0',
            [t @ b'0'..=b'5', d @ b'0'..=b'9'] => (t - b'0') * 10 + (d - b'0'),
            _ => return None,
        };
        places.push((60, digit.into()));
    }
    let value = places_to_decimal(places);
    match frac {
        None => Some((format!("{sign}{value}"), SyntaxKind::INT)),
        Some(frac) if frac.bytes().all(|b| b.is_ascii_digit() || b == b'_') => {
            let frac = frac.replace('_', "");
            let frac = if frac.is_empty() { "0" } else { &frac };
            Some((format!("{sign}{value}.{frac}"), SyntaxKind::FLOAT))
        }
        _ => None,
    }
}
