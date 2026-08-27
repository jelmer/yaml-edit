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

/// yaml-edit accepts some scalar forms that YAML 1.2 core schema
/// rejects (mostly YAML 1.1 legacy). Skip these in the oracle so
/// documented permissiveness doesn't flag as a divergence.
fn is_yaml_edit_extension(text: &str) -> bool {
    let signed = text.starts_with(['+', '-']);
    let unsigned = text.strip_prefix(['+', '-']).unwrap_or(text);
    // Uppercase `0O` / `0X` integer prefixes (spec is lowercase only)
    // and signed non-decimal integers (`+0x1e`, `-0o7`, `+0b10`) --
    // spec allows a sign on decimals only.
    if unsigned.starts_with("0X") || unsigned.starts_with("0O") {
        return true;
    }
    if signed
        && (unsigned.starts_with("0x")
            || unsigned.starts_with("0o")
            || unsigned.starts_with("0b")
            || unsigned.starts_with("0B"))
    {
        return true;
    }
    false
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
            let trimmed = text.trim();
            // Empty plain scalars are a spec ambiguity around
            // implicit null; not the surface this oracle polices.
            if trimmed.is_empty() {
                return;
            }
            if is_yaml_edit_extension(trimmed) {
                return;
            }
            let Some(mine) = yaml_edit_kind(sv.scalar_type()) else {
                return;
            };
            let theirs = saphyr_kind(&ScalarOwned::parse_from_cow(Cow::Borrowed(trimmed)));

            if mine != theirs {
                panic!(
                    "scalar-type divergence for value {:?} -- yaml-edit resolved as {:?}, saphyr as {:?}\ninput:\n{}",
                    text, mine, theirs, input,
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
