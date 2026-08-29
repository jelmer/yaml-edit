//! Shared helpers for asserting CST invariants after mutations.
//!
//! Rust compiles this module separately into each integration test binary and
//! not every one uses both helpers, hence the `dead_code` allowance.

#![allow(dead_code)]

use rowan::ast::AstNode;
use yaml_edit::{debug, Document, YamlFile};

/// Assert that a `Document`'s CST is structurally valid and roundtrips
/// through the parser without change.
///
/// Call after every mutation in mutation-oriented tests. Catches
/// silent CST corruption (missing/duplicate NEWLINE, stacked INDENT,
/// unterminated block entries) even when the pre- and post-mutation
/// text look identical to the eye.
#[track_caller]
pub fn assert_cst_ok(doc: &Document) {
    let syntax = doc.syntax();
    if let Err(e) = debug::validate_tree(syntax) {
        panic!("CST invariant violated: {e}\n---\n{doc}\n---");
    }
    if let Err(e) = debug::roundtrip_ok(syntax) {
        panic!("roundtrip failed: {e}\n---\n{doc}\n---");
    }
}

/// Like [`assert_cst_ok`], but for a whole `YamlFile` (multi-document).
#[track_caller]
pub fn assert_file_cst_ok(file: &YamlFile) {
    let syntax = file.syntax();
    if let Err(e) = debug::validate_tree(syntax) {
        panic!("CST invariant violated: {e}\n---\n{file}\n---");
    }
    if let Err(e) = debug::roundtrip_ok(syntax) {
        panic!("roundtrip failed: {e}\n---\n{file}\n---");
    }
}
