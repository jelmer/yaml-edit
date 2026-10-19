//! Shared helpers for asserting CST invariants after mutations.
//!
//! Rust compiles this module separately into each integration test binary and
//! not every one uses every helper, hence the `dead_code` allowance.

#![allow(dead_code)]

pub mod edit_checks;
pub mod mutation_checks;

use rowan::ast::AstNode;
use std::str::FromStr;
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

/// Like [`assert_cst_ok`], and also require the tree to be the one a
/// re-parse of its text gives.
#[track_caller]
pub fn assert_parser_shaped(doc: &Document) {
    assert_cst_ok(doc);
    let reparsed = Document::from_str(&doc.to_string()).unwrap();
    assert_eq!(
        debug::shape_to_string(doc.syntax()),
        debug::shape_to_string(reparsed.syntax())
    );
}

/// Like [`assert_parser_shaped`], but for a whole `YamlFile`.
#[track_caller]
pub fn assert_file_parser_shaped(file: &YamlFile) {
    assert_file_cst_ok(file);
    let reparsed = YamlFile::from_str(&file.to_string()).unwrap();
    assert_eq!(
        debug::shape_to_string(file.syntax()),
        debug::shape_to_string(reparsed.syntax())
    );
}

/// Check that an edited `doc` renders as `expected` and is parser-shaped.
#[track_caller]
pub fn assert_edited(doc: &Document, expected: &str) {
    assert_eq!(doc.to_string(), expected);
    assert_parser_shaped(doc);
}

/// Like [`assert_edited`], but for a whole `YamlFile`.
#[track_caller]
pub fn assert_file_edited(file: &YamlFile, expected: &str) {
    assert_eq!(file.to_string(), expected);
    assert_file_parser_shaped(file);
}

/// Parse `source`, apply `edit` to it and check the result as
/// [`assert_edited`] does.
#[track_caller]
pub fn check_edit(source: &str, edit: impl FnOnce(&Document), expected: &str) {
    let doc = Document::from_str(source).unwrap();
    edit(&doc);
    assert_eq!(doc.to_string(), expected, "editing {source:?}");
    assert_parser_shaped(&doc);
}
