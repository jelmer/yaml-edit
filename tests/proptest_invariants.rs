//! Property-based tests for CST invariants under random mutations.
//!
//! For each generated seed document + random mutation sequence, we
//! assert `debug::validate_tree` and `debug::roundtrip_ok` after every
//! step. The mutation shrinker finds the smallest violating input.

use proptest::prelude::*;
use rowan::ast::AstNode;
use std::str::FromStr;
use yaml_edit::{debug, Document};

/// A random-but-plausible mutation to apply.
#[derive(Debug, Clone)]
enum Op {
    /// `mapping.set(key, string_value)`
    SetString(String, String),
    /// `mapping.set(key, int_value)`
    SetInt(String, i32),
    /// `mapping.remove(key)`
    Remove(String),
    /// `mapping.rename_key(old, new)`
    Rename(String, String),
    /// `mapping.get_sequence(key)?.push(value)`
    PushIntoSeq(String, String),
    /// `mapping.get_sequence(key)?.pop()`
    PopFromSeq(String),
    /// `mapping.get_mapping(key)?.set(inner_key, inner_value)`
    SetInNested(String, String, String),
}

/// A YAML key that only uses simple characters -- no colons, no quotes,
/// no whitespace edge cases. Keeps generation focused on mutation logic,
/// not parser edge cases.
fn key_strat() -> impl Strategy<Value = String> {
    "[a-z][a-z0-9_]{0,5}".prop_map(String::from)
}

/// Similarly simple scalar values.
fn value_strat() -> impl Strategy<Value = String> {
    "[a-z0-9_]{0,8}".prop_map(String::from)
}

fn op_strat() -> impl Strategy<Value = Op> {
    prop_oneof![
        (key_strat(), value_strat()).prop_map(|(k, v)| Op::SetString(k, v)),
        (key_strat(), any::<i32>()).prop_map(|(k, v)| Op::SetInt(k, v)),
        key_strat().prop_map(Op::Remove),
        (key_strat(), key_strat()).prop_map(|(a, b)| Op::Rename(a, b)),
        (key_strat(), value_strat()).prop_map(|(k, v)| Op::PushIntoSeq(k, v)),
        key_strat().prop_map(Op::PopFromSeq),
        (key_strat(), key_strat(), value_strat()).prop_map(|(k, ik, v)| Op::SetInNested(k, ik, v)),
    ]
}

/// A seed YAML document -- one of a handful of hand-picked shapes that
/// exercise the common structures without depending on the parser
/// handling weird inputs.
fn seed_strat() -> impl Strategy<Value = &'static str> {
    prop_oneof![
        Just("a: 1\n"),
        Just("a: 1\nb: 2\nc: 3\n"),
        Just("items:\n  - one\n  - two\n"),
        Just("root:\n  a: 1\n  b: 2\n"),
        Just("mixed:\n  list:\n    - x\n    - y\n  map:\n    k: v\n"),
        Just("existing: value\n"),
    ]
}

fn apply(doc: &Document, op: &Op) {
    let Some(mapping) = doc.as_mapping() else {
        return;
    };
    match op {
        Op::SetString(k, v) => mapping.set(k.as_str(), v.as_str()),
        Op::SetInt(k, v) => mapping.set(k.as_str(), *v as i64),
        Op::Remove(k) => {
            let _ = mapping.remove(k.as_str());
        }
        Op::Rename(a, b) => {
            let _ = mapping.rename_key(a.as_str(), b.as_str());
        }
        Op::PushIntoSeq(k, v) => {
            if let Some(seq) = mapping.get_sequence(k.as_str()) {
                seq.push(v.as_str());
            }
        }
        Op::PopFromSeq(k) => {
            if let Some(seq) = mapping.get_sequence(k.as_str()) {
                let _ = seq.pop();
            }
        }
        Op::SetInNested(k, ik, v) => {
            if let Some(nested) = mapping.get_mapping(k.as_str()) {
                nested.set(ik.as_str(), v.as_str());
            }
        }
    }
}

fn check(doc: &Document, context: &str) -> Result<(), TestCaseError> {
    let syntax = doc.syntax();
    if let Err(e) = debug::validate_tree(syntax) {
        return Err(TestCaseError::fail(format!(
            "invariant violated after {context}: {e}\ntext: {:?}",
            doc.to_string()
        )));
    }
    if let Err(e) = debug::roundtrip_ok(syntax) {
        return Err(TestCaseError::fail(format!(
            "roundtrip failed after {context}: {e}\ntext: {:?}",
            doc.to_string()
        )));
    }
    Ok(())
}

/// After a `set(k, v)`, `get(k)` must yield the same value we just wrote.
///
/// Complements roundtrip_ok: syntactic stability doesn't imply the
/// mutation actually stuck. If `set` silently no-ops or writes garbage,
/// roundtrip stays happy but semantics diverge. We decode via
/// `Scalar::as_string()` so quoting differences (`""` vs `''`) don't
/// register as mismatches.
fn check_set_stuck(doc: &Document, key: &str, expected: &str) -> Result<(), TestCaseError> {
    let Some(mapping) = doc.as_mapping() else {
        return Ok(());
    };
    let Some(actual) = mapping.get(key) else {
        return Err(TestCaseError::fail(format!(
            "set(\"{key}\", \"{expected}\") didn't stick: key missing after write\ntext: {:?}",
            doc.to_string(),
        )));
    };
    let Some(scalar) = actual.as_scalar() else {
        return Err(TestCaseError::fail(format!(
            "set(\"{key}\", \"{expected}\") didn't stick: value is not a scalar\ntext: {:?}",
            doc.to_string(),
        )));
    };
    let decoded = scalar.as_string();
    if decoded != expected {
        return Err(TestCaseError::fail(format!(
            "set(\"{key}\", \"{expected}\") didn't stick: decoded = {decoded:?}\ntext: {:?}",
            doc.to_string(),
        )));
    }
    Ok(())
}

proptest! {
    #![proptest_config(ProptestConfig {
        cases: 256,
        // Every mutation runs validate + roundtrip; large sequences
        // slow the suite without adding coverage.
        max_shrink_iters: 1024,
        ..ProptestConfig::default()
    })]

    #[test]
    fn seeded_document_mutations_preserve_invariants(
        seed in seed_strat(),
        ops in prop::collection::vec(op_strat(), 0..8),
    ) {
        let doc = Document::from_str(seed).unwrap();
        check(&doc, "parse")?;
        for (i, op) in ops.iter().enumerate() {
            apply(&doc, op);
            check(&doc, &format!("op[{i}] = {op:?}"))?;
            // Semantic check: a set-like op that succeeded should be
            // observable via get(). Skip ops whose semantics depend on
            // preconditions we haven't tracked (nested-mapping/seq ops
            // silently no-op if the target isn't the right kind).
            match op {
                Op::SetString(k, v) => check_set_stuck(&doc, k, v)?,
                Op::SetInt(k, v) => check_set_stuck(&doc, k, &v.to_string())?,
                _ => {}
            }
        }
    }
}
