#![no_main]

//! Cross-check yaml-edit's scalar tag resolution against saphyr, a
//! YAML 1.2 conformant parser, as a semantic oracle.
//!
//! The other fuzz targets only check self-consistency (no panics,
//! CST invariants hold, round-trip is stable). This target catches
//! divergences where yaml-edit and a reference parser disagree about
//! *what a value means* -- e.g. reading `1` as a string instead of an
//! integer, or accepting `.nan` as a plain string.
//!
//! Strategy: parse the input with yaml-edit. For every plain (unquoted)
//! scalar in the CST, ask saphyr to resolve the same raw text via its
//! core-schema scalar parser and assert the resolved kinds agree.
//! Feeding the raw text into saphyr sidesteps disagreements about
//! scalar boundaries, duplicate keys, or flow-context parsing -- if
//! yaml-edit's text is `26 `, saphyr's answer for `26 ` is the ground
//! truth we compare against.

use libfuzzer_sys::fuzz_target;
use saphyr::ScalarOwned;
use std::borrow::Cow;
use std::str::FromStr;
use yaml_edit::{Document, ScalarStyle, ScalarType, ScalarValue, YamlNode};

#[derive(Debug, PartialEq)]
enum Kind {
    Null,
    Bool,
    Int,
    Float,
    String,
}

fn saphyr_kind(s: &ScalarOwned) -> Kind {
    match s {
        ScalarOwned::Null => Kind::Null,
        ScalarOwned::Boolean(_) => Kind::Bool,
        ScalarOwned::Integer(_) => Kind::Int,
        ScalarOwned::FloatingPoint(_) => Kind::Float,
        ScalarOwned::String(_) => Kind::String,
    }
}

/// Match the YAML 1.2 core-schema tag-resolution regexes exactly.
/// Returns `None` for inputs that don't strictly match any of the
/// null/bool/int/float patterns -- both parsers may legitimately
/// disagree on those (leniency zone), so the oracle skips them.
fn strict_core_schema_kind(text: &str) -> Option<Kind> {
    // Null
    if matches!(text, "null" | "Null" | "NULL" | "~") {
        return Some(Kind::Null);
    }
    // Bool
    if matches!(text, "true" | "True" | "TRUE" | "false" | "False" | "FALSE") {
        return Some(Kind::Bool);
    }
    // Int: 0 | -? [1-9] [0-9]* | 0o [0-7]+ | 0x [0-9a-fA-F]+
    // Per spec, only decimals may carry a sign; `-0x1` and `-0o7` are
    // NOT valid ints. Skip inputs that match the regex but overflow
    // i64 -- both parsers legitimately fall back to string there.
    if text == "0" {
        return Some(Kind::Int);
    }
    if let Some(hex) = text.strip_prefix("0x") {
        if !hex.is_empty()
            && hex.bytes().all(|b| b.is_ascii_hexdigit())
            && i64::from_str_radix(hex, 16).is_ok()
        {
            return Some(Kind::Int);
        }
    }
    if let Some(oct) = text.strip_prefix("0o") {
        if !oct.is_empty()
            && oct.bytes().all(|b| b.is_ascii_digit() && b < b'8')
            && i64::from_str_radix(oct, 8).is_ok()
        {
            return Some(Kind::Int);
        }
    }
    let decimal_body = text.strip_prefix('-').unwrap_or(text);
    if decimal_body.starts_with(|c: char| c.is_ascii_digit() && c != '0')
        && decimal_body.bytes().all(|b| b.is_ascii_digit())
        && text.parse::<i64>().is_ok()
    {
        return Some(Kind::Int);
    }
    // Float: [-+]? ( \. [0-9]+ | [0-9]+ (\. [0-9]*)? ) ([eE] [-+]? [0-9]+)?
    // plus the dotted infinity/NaN literals.
    if matches!(
        text,
        ".inf"
            | ".Inf"
            | ".INF"
            | "+.inf"
            | "+.Inf"
            | "+.INF"
            | "-.inf"
            | "-.Inf"
            | "-.INF"
            | ".nan"
            | ".NaN"
            | ".NAN"
    ) {
        return Some(Kind::Float);
    }
    // Skip float-regex inputs that overflow/underflow -- both parsers
    // are entitled to string-fall-back there.
    if matches_float_regex(text) && text.parse::<f64>().map(f64::is_finite).unwrap_or(false) {
        return Some(Kind::Float);
    }
    None
}

/// Match `[-+]? ( \. [0-9]+ | [0-9]+ (\. [0-9]*)? ) ([eE] [-+]? [0-9]+)?`,
/// with the additional constraint that the pattern must contain
/// either a decimal point or an exponent (a pure digit string is an
/// integer, not a float).
fn matches_float_regex(text: &str) -> bool {
    let body = text.strip_prefix(['+', '-']).unwrap_or(text);
    let (mantissa, exp) = match body.find(['e', 'E']) {
        Some(i) => (&body[..i], Some(&body[i + 1..])),
        None => (body, None),
    };
    let has_dot = mantissa.contains('.');
    if !has_dot && exp.is_none() {
        return false;
    }
    let mantissa_ok = if let Some(after_dot) = mantissa.strip_prefix('.') {
        !after_dot.is_empty() && after_dot.bytes().all(|b| b.is_ascii_digit())
    } else if let Some(dot) = mantissa.find('.') {
        let (whole, rest) = mantissa.split_at(dot);
        let frac = &rest[1..];
        !whole.is_empty()
            && whole.bytes().all(|b| b.is_ascii_digit())
            && frac.bytes().all(|b| b.is_ascii_digit())
    } else {
        !mantissa.is_empty() && mantissa.bytes().all(|b| b.is_ascii_digit())
    };
    if !mantissa_ok {
        return false;
    }
    let Some(exp) = exp else {
        return true;
    };
    let exp_body = exp.strip_prefix(['+', '-']).unwrap_or(exp);
    !exp_body.is_empty() && exp_body.bytes().all(|b| b.is_ascii_digit())
}

fn yaml_edit_kind(t: ScalarType) -> Option<Kind> {
    match t {
        ScalarType::Null => Some(Kind::Null),
        ScalarType::Boolean => Some(Kind::Bool),
        ScalarType::Integer => Some(Kind::Int),
        ScalarType::Float => Some(Kind::Float),
        ScalarType::String => Some(Kind::String),
        // Timestamp / Regex / Binary have no direct saphyr counterpart;
        // skip them rather than manufacture a false disagreement.
        _ => None,
    }
}

fn walk(node: YamlNode, input: &str) {
    match node {
        YamlNode::Scalar(scalar) => {
            let sv = ScalarValue::from_scalar(&scalar);
            // Only plain scalars carry interesting tag-resolution
            // information. Quoted forms are unambiguously !!str on
            // both sides.
            if sv.style() != ScalarStyle::Plain {
                return;
            }
            let text = scalar.value();
            // Match what `from_scalar` does: trim trailing whitespace
            // only. Plain scalars can't start with whitespace anyway
            // (the lexer would have emitted an INDENT), and using the
            // same trim keeps our "spec-strict" check and yaml-edit's
            // classify_plain looking at the same string.
            let trimmed = text.trim_end();
            // Empty plain scalars are a spec ambiguity around
            // implicit null; not the surface this oracle polices.
            if trimmed.is_empty() {
                return;
            }
            // Only assert when the raw text strictly matches a YAML 1.2
            // core-schema pattern. Non-matching text sits in a leniency
            // zone where both parsers can legitimately disagree (e.g.
            // yaml-edit accepts YAML 1.1 legacy octal, saphyr accepts
            // decimals with leading zeros); flagging those buries real
            // bugs. When we do have a strict-spec answer, use it as the
            // ground truth and require both parsers to agree.
            let Some(strict) = strict_core_schema_kind(trimmed) else {
                return;
            };
            let Some(mine) = yaml_edit_kind(sv.scalar_type()) else {
                return;
            };
            let theirs = saphyr_kind(&ScalarOwned::parse_from_cow(Cow::Borrowed(trimmed)));

            if mine != strict || theirs != strict {
                panic!(
                    "scalar-type divergence for value {:?} -- spec says {:?}, yaml-edit says {:?}, saphyr says {:?}\ninput:\n{}",
                    text, strict, mine, theirs, input,
                );
            }
        }
        YamlNode::Mapping(m) => {
            for (k, v) in m.iter() {
                walk(k, input);
                walk(v, input);
            }
        }
        YamlNode::Sequence(seq) => {
            for item in seq.values() {
                walk(item, input);
            }
        }
        // Aliases and tagged nodes have their own resolution rules;
        // skip them here.
        YamlNode::Alias(_) | YamlNode::TaggedNode(_) => {}
    }
}

fuzz_target!(|data: &[u8]| {
    let Ok(input) = std::str::from_utf8(data) else {
        return;
    };
    if input.len() > 100_000 {
        return;
    }
    // NUL and other C0 controls (except \t and \n) aren't valid in YAML
    // 1.2 and different parsers handle them inconsistently -- skip to
    // keep the oracle focused on real semantic disagreements.
    if input.bytes().any(|b| b < 0x20 && b != b'\t' && b != b'\n') {
        return;
    }

    let Ok(doc) = Document::from_str(input) else {
        return;
    };
    if let Some(m) = doc.as_mapping() {
        walk(YamlNode::Mapping(m), input);
    } else if let Some(s) = doc.as_sequence() {
        walk(YamlNode::Sequence(s), input);
    } else if let Some(sc) = doc.as_scalar() {
        walk(YamlNode::Scalar(sc), input);
    }
});
