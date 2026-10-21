//! Shared helpers for asserting CST invariants after mutations.
//!
//! Rust compiles this module separately into each integration test binary and
//! not every one uses every helper, hence the `dead_code` allowance.

#![allow(dead_code)]

pub mod edit_checks;
pub mod mutation_checks;

use rowan::ast::AstNode;
use std::str::FromStr;
use yaml_edit::validator::Validator;
use yaml_edit::{debug, Document, SyntaxKind, YamlFile, YamlNode, YamlValue};

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

/// Parse `yaml`, requiring it to round trip byte for byte, with a valid CST
/// that holds no ERROR node.
#[track_caller]
pub fn parse_clean(yaml: &str) -> YamlFile {
    let file = YamlFile::from_str(yaml).unwrap_or_else(|e| panic!("{yaml:?}: {e}"));
    assert_eq!(file.to_string(), yaml);
    assert_file_cst_ok(&file);
    let tree = debug::tree_to_string(file.syntax());
    assert_eq!(count_nodes(&file, SyntaxKind::ERROR), 0, "{yaml:?}\n{tree}");
    file
}

/// How many nodes of `kind` the tree of `file` holds.
pub fn count_nodes(file: &YamlFile, kind: SyntaxKind) -> usize {
    file.syntax()
        .descendants()
        .filter(|n| n.kind() == kind)
        .count()
}

/// How many tokens of `kind` the tree of `file` holds.
pub fn count_tokens(file: &YamlFile, kind: SyntaxKind) -> usize {
    file.syntax()
        .descendants_with_tokens()
        .filter(|e| e.as_token().is_some_and(|t| t.kind() == kind))
        .count()
}

/// A node as compact text: flow collections as `[..]` and `{..}`, block
/// ones as `seq[..]` and `map{..}`, a null as `~`, a tagged node with its
/// tag in front and an alias as `*name`.
pub fn render(node: &YamlNode) -> String {
    match node {
        YamlNode::Scalar(s) if s.is_null() => "~".to_string(),
        YamlNode::Scalar(s) => s.as_string(),
        YamlNode::Sequence(s) => {
            let items: Vec<_> = s.values().map(|v| render(&v)).collect();
            assert_eq!(items.len(), s.len());
            let open = if s.is_flow_style() { "[" } else { "seq[" };
            format!("{open}{}]", items.join(", "))
        }
        YamlNode::Mapping(m) => {
            let entries: Vec<_> = m
                .iter()
                .map(|(k, v)| format!("{}: {}", render(&k), render(&v)))
                .collect();
            assert_eq!(entries.len(), m.len());
            let open = if m.is_flow_style() { "{" } else { "map{" };
            format!("{open}{}}}", entries.join(", "))
        }
        YamlNode::TaggedNode(t) => {
            let inner = if let Some(m) = t.as_mapping() {
                render(&YamlNode::Mapping(m))
            } else if let Some(s) = t.as_sequence() {
                render(&YamlNode::Sequence(s))
            } else {
                render(&YamlNode::Scalar(t.value().unwrap()))
            };
            format!("{} {inner}", t.tag().unwrap())
        }
        YamlNode::Alias(a) => format!("*{}", a.name()),
    }
}

/// Parse `yaml` as [`parse_clean`] does and render the root node of each
/// of its documents, `~` for an empty one.
#[track_caller]
pub fn render_documents(yaml: &str) -> Vec<String> {
    parse_clean(yaml)
        .documents()
        .map(|doc| {
            doc.syntax()
                .children()
                .find_map(YamlNode::from_syntax)
                .map_or_else(|| "~".to_string(), |root| render(&root))
        })
        .collect()
}

/// Check that each input parses as [`parse_clean`] does into one document,
/// whose root node renders as expected.
#[track_caller]
pub fn check_renders(cases: &[(&str, &str)]) {
    for (yaml, expected) in cases {
        assert_eq!(render_documents(yaml), [*expected], "{yaml:?}");
    }
}

/// The values of every document in `yaml`, which has to round trip.
#[track_caller]
pub fn document_values(yaml: &str) -> Vec<YamlValue> {
    let file = YamlFile::from_str(yaml).unwrap();
    assert_eq!(file.to_string(), yaml);
    file.documents()
        .map(|doc| YamlValue::from_document(&doc).unwrap())
        .collect()
}

/// The validator's findings for `file` as (message, location) pairs.
pub fn violations(file: &YamlFile) -> Vec<(String, String)> {
    Validator::new()
        .validate_syntax(file.syntax())
        .into_iter()
        .map(|v| (v.message, v.location.unwrap()))
        .collect()
}

/// Parse `yaml`, check the errors reported for it, and that the tree still
/// holds a document and all of the text.
#[track_caller]
pub fn parse_with_errors(yaml: &str, expected: &[&str]) -> YamlFile {
    let parsed = YamlFile::parse(yaml);
    assert_eq!(parsed.errors(), expected, "{yaml:?}");
    let file = parsed.tree();
    debug::validate_tree(file.syntax()).unwrap();
    assert!(file.document().is_some(), "{yaml:?}");
    assert_eq!(file.to_string(), yaml);
    file
}

/// Parse `yaml`, which has no parse errors, and check what the validator
/// reports about it.
#[track_caller]
pub fn parse_with_violations(yaml: &str, expected: &[(&str, &str)]) -> YamlFile {
    let file = parse_with_errors(yaml, &[]);
    let expected: Vec<(String, String)> = expected
        .iter()
        .map(|(m, l)| (m.to_string(), l.to_string()))
        .collect();
    assert_eq!(violations(&file), expected, "{yaml:?}");
    file
}
