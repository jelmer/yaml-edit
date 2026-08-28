//! Shared helpers for asserting CST invariants after mutations.
//!
//! Rust compiles this module separately into each integration test binary and
//! not every one uses both helpers, hence the `dead_code` allowance.

#![allow(dead_code)]

use rowan::ast::AstNode;
use yaml_edit::{debug, Document, Lang, YamlFile};

/// Assert that `doc`'s tree satisfies the structural invariants and roundtrips.
pub fn assert_cst_ok(doc: &Document) {
    check(doc);
}

/// Assert that `file`'s tree satisfies the structural invariants and roundtrips.
pub fn assert_file_cst_ok(file: &YamlFile) {
    check(file);
}

fn check<N: AstNode<Language = Lang> + std::fmt::Display>(node: &N) {
    let syntax = node.syntax();
    if let Err(e) = debug::validate_tree(syntax) {
        panic!("invariant violated: {e}\n---\n{node}\n---");
    }
    if let Err(e) = debug::roundtrip_ok(syntax) {
        panic!("roundtrip failed: {e}\n---\n{node}\n---");
    }
}
