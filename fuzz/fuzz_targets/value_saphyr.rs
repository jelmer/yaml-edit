#![no_main]

//! Compare the values yaml-edit reads from a document with saphyr's.
//!
//! `cross_parser_saphyr` compares how single plain scalars are typed. This
//! compares whole documents: on input that saphyr loads, that yaml-edit
//! parses without errors and that its validator accepts, every document's
//! `YamlValue` must be the value saphyr loads, scalars, collections, keys
//! and aliases included.
//!
//! Documents holding something the two represent differently are skipped:
//! tags other than the core schema's, merge keys (a YAML 1.1 type saphyr does
//! not apply), and mapping keys that are floats or collections.

use libfuzzer_sys::fuzz_target;
use saphyr::{LoadableYamlNode, Scalar, Yaml};
use std::str::FromStr;
use yaml_edit::validator::{Severity, Validator};
use yaml_edit::{ScalarType, ScalarValue, SyntaxKind, YamlFile, YamlValue};

#[path = "common/saphyr_input.rs"]
mod saphyr_input;

/// A value in a form both sides can be converted to.
#[derive(Debug, PartialEq)]
enum Value {
    Null,
    Bool(bool),
    Int(i64),
    Float(u64),
    Str(String),
    Seq(Vec<Value>),
    /// Entries sorted by key, keys in yaml-edit's string form.
    Map(Vec<(String, Value)>),
}

fn float(f: f64) -> Value {
    Value::Float(if f.is_nan() {
        f64::NAN.to_bits()
    } else {
        f.to_bits()
    })
}

/// Core schema tags, which saphyr resolves and yaml-edit keeps on scalars.
fn core_tag(tag: &str) -> bool {
    let name = tag
        .strip_prefix("tag:yaml.org,2002:")
        .or_else(|| tag.strip_prefix("!!"));
    matches!(name, Some("str" | "int" | "float" | "bool" | "null"))
}

fn from_scalar(s: &ScalarValue) -> Option<Value> {
    if s.tag().is_some_and(|t| !core_tag(t)) {
        return None;
    }
    Some(match s.scalar_type() {
        ScalarType::Null => Value::Null,
        ScalarType::Boolean => Value::Bool(s.to_bool()?),
        ScalarType::Integer => Value::Int(s.to_i64()?),
        ScalarType::Float => float(s.to_f64()?),
        _ => Value::Str(s.value().to_string()),
    })
}

fn from_yaml_edit(v: &YamlValue) -> Option<Value> {
    Some(match v {
        YamlValue::Scalar(s) => from_scalar(s)?,
        YamlValue::Sequence(items) => {
            Value::Seq(items.iter().map(from_yaml_edit).collect::<Option<_>>()?)
        }
        YamlValue::Mapping(m) => Value::Map(
            m.iter()
                .map(|(k, v)| Some((k.clone(), from_yaml_edit(v)?)))
                .collect::<Option<_>>()?,
        ),
        YamlValue::Tagged(tag, inner)
            if matches!(
                tag.as_str(),
                "!!map" | "!!seq" | "tag:yaml.org,2002:map" | "tag:yaml.org,2002:seq"
            ) =>
        {
            from_yaml_edit(inner)?
        }
        _ => return None,
    })
}

/// yaml-edit's string form of a scalar key.
fn key_string(key: &Yaml) -> Option<String> {
    match key {
        Yaml::Value(Scalar::Null) => Some("null".to_string()),
        Yaml::Value(Scalar::Boolean(b)) => Some(b.to_string()),
        Yaml::Value(Scalar::Integer(i)) => Some(i.to_string()),
        Yaml::Value(Scalar::String(s)) => Some(s.to_string()),
        _ => None,
    }
}

fn from_saphyr(y: &Yaml) -> Option<Value> {
    Some(match y {
        Yaml::Value(Scalar::Null) => Value::Null,
        Yaml::Value(Scalar::Boolean(b)) => Value::Bool(*b),
        Yaml::Value(Scalar::Integer(i)) => Value::Int(*i),
        Yaml::Value(Scalar::FloatingPoint(f)) => float(f.0),
        Yaml::Value(Scalar::String(s)) => Value::Str(s.to_string()),
        Yaml::Sequence(items) => Value::Seq(items.iter().map(from_saphyr).collect::<Option<_>>()?),
        Yaml::Mapping(m) => {
            let mut entries = Vec::new();
            for (k, v) in m {
                let k = key_string(k)?;
                if k == "<<" {
                    return None;
                }
                entries.push((k, from_saphyr(v)?));
            }
            entries.sort_by(|a, b| a.0.cmp(&b.0));
            // Two keys with the same string form, which yaml-edit rejects.
            if entries.windows(2).any(|w| w[0].0 == w[1].0) {
                return None;
            }
            Value::Map(entries)
        }
        _ => return None,
    })
}

/// Whether `mine` is `theirs`, but for saphyr bugs: it reads an empty node
/// with an anchor (`a: &x`) as an empty string rather than null, and an
/// empty clipped or stripped block scalar (`a: >`) as a line break.
// TODO: drop the exceptions once saphyr reads such nodes as the spec says.
fn same(mine: &Value, theirs: &Value) -> bool {
    match (mine, theirs) {
        (Value::Null, Value::Str(s)) => s.is_empty(),
        (Value::Str(a), Value::Str(b)) => a == b || (a.is_empty() && b == "\n"),
        (Value::Seq(a), Value::Seq(b)) => {
            a.len() == b.len() && a.iter().zip(b).all(|(a, b)| same(a, b))
        }
        (Value::Map(a), Value::Map(b)) => {
            let anchored_empty_key =
                a.iter().any(|(k, _)| k == "null") && !a.iter().any(|(k, _)| k.is_empty());
            let mut b: Vec<_> = b
                .iter()
                .map(|(k, v)| match k.as_str() {
                    "" if anchored_empty_key => ("null", v),
                    k => (k, v),
                })
                .collect();
            b.sort_by(|x, y| x.0.cmp(y.0));
            a.len() == b.len()
                && a.iter()
                    .zip(b)
                    .all(|((ka, va), (kb, vb))| ka == kb && same(va, vb))
        }
        _ => mine == theirs,
    }
}

/// Whether a string in `y` has a line that is a document marker, which
/// saphyr 0.0.12 takes into a block scalar indented by zero spaces
/// (`|\nx\n--- y\n`) where the spec ends the document.
// TODO: drop once saphyr ends block scalars at document markers.
fn holds_marker_line(y: &Yaml) -> bool {
    match y {
        Yaml::Value(Scalar::String(s)) => s.split('\n').any(|line| {
            ["---", "..."].iter().any(|m| {
                line.strip_prefix(m)
                    .is_some_and(|rest| rest.is_empty() || rest.starts_with([' ', '\t']))
            })
        }),
        Yaml::Sequence(items) => items.iter().any(holds_marker_line),
        Yaml::Mapping(m) => m
            .iter()
            .any(|(k, v)| holds_marker_line(k) || holds_marker_line(v)),
        Yaml::Tagged(_, inner) => holds_marker_line(inner),
        _ => false,
    }
}

/// Whether a mapping key in `file` has a tag, which the keys of a
/// `YamlValue` do not keep, and which saphyr 0.0.12 drops entries for
/// when the key is empty (`{!!str}`).
fn has_tagged_key(file: &YamlFile) -> bool {
    rowan::ast::AstNode::syntax(file)
        .descendants()
        .filter(|n| n.kind() == SyntaxKind::KEY)
        .any(|key| {
            key.descendants_with_tokens()
                .any(|t| t.kind() == SyntaxKind::TAG)
        })
}

fuzz_target!(|data: &[u8]| {
    let Ok(input) = std::str::from_utf8(data) else {
        return;
    };
    // saphyr 0.0.12 reads `++2` as the integer 2.
    // TODO: drop once saphyr types such scalars as strings.
    if input.len() > 10_000
        || saphyr_input::unsuitable(input)
        || input.contains("++")
        || input.contains("+-")
    {
        return;
    }
    let Ok(theirs) = Yaml::load_from_str(input) else {
        return;
    };
    let Ok(file) = YamlFile::from_str(input) else {
        return;
    };
    let rejected = Validator::new()
        .validate_syntax(rowan::ast::AstNode::syntax(&file))
        .into_iter()
        .any(|v| v.severity == Severity::Error);
    if rejected || has_tagged_key(&file) || theirs.iter().any(holds_marker_line) {
        return;
    }
    let docs: Vec<_> = file.documents().collect();
    if docs.len() != theirs.len() {
        panic!(
            "saphyr reads {} documents, yaml-edit {}\ninput: {input:?}",
            theirs.len(),
            docs.len()
        );
    }
    for (i, (doc, theirs)) in docs.iter().zip(&theirs).enumerate() {
        let Some(expected) = from_saphyr(theirs) else {
            continue;
        };
        let mine = YamlValue::from_document(doc);
        // Content a YamlValue cannot hold, such as a `!!set` with values,
        // which saphyr does not check.
        let Some(actual) = mine.ok().as_ref().and_then(from_yaml_edit) else {
            continue;
        };
        if !same(&actual, &expected) {
            panic!(
                "document {i} differs\ninput: {input:?}\nsaphyr:    {expected:?}\nyaml-edit: {actual:?}"
            );
        }
    }
});
