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
use yaml_edit::{debug, Document, Lang, Mapping, Parse, SyntaxKind, YamlFile, YamlNode, YamlValue};

/// Assert that the tree holding `node` is structurally valid and roundtrips
/// through the parser without change.
///
/// Call after every mutation in mutation-oriented tests. Catches
/// silent CST corruption (missing/duplicate NEWLINE, stacked INDENT,
/// unterminated block entries) even when the pre- and post-mutation
/// text look identical to the eye.
#[track_caller]
pub fn assert_cst_ok(node: &impl AstNode<Language = Lang>) {
    let syntax = node.syntax();
    if let Err(e) = debug::validate_tree(syntax) {
        panic!("CST invariant violated: {e}\n---\n{syntax}\n---");
    }
    if let Err(e) = debug::roundtrip_ok(syntax) {
        panic!("roundtrip failed: {e}\n---\n{syntax}\n---");
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

/// Like [`assert_cst_ok`], and also require the tree holding `node` to be
/// the one a re-parse of its text gives.
#[track_caller]
pub fn assert_parser_shaped(node: &impl AstNode<Language = Lang>) {
    assert_cst_ok(node);
    let root = node
        .syntax()
        .ancestors()
        .last()
        .expect("a node is its own ancestor");
    let text = root.to_string();
    let parsed = Parse::parse_yaml(&text).tree();
    let reparsed = parsed
        .syntax()
        .descendants()
        .find(|n| n.kind() == root.kind())
        .unwrap_or_else(|| panic!("re-parse of {text:?} has no {:?}", root.kind()));
    assert_eq!(
        debug::shape_to_string(&root),
        debug::shape_to_string(&reparsed),
        "{text:?}"
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

/// Like [`assert_file_edited`], and the validator finds nothing wrong with
/// any of the documents.
#[track_caller]
pub fn assert_file_edited_valid(file: &YamlFile, expected: &str) {
    assert_file_edited(file, expected);
    let violations: Vec<String> = file
        .documents()
        .flat_map(|doc| Validator::new().validate(&doc))
        .map(|v| v.to_string())
        .collect();
    assert_eq!(violations, Vec::<String>::new());
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

/// The root mapping of the first document of `file`.
pub fn root_mapping(file: &YamlFile) -> Mapping {
    file.document().unwrap().as_mapping().unwrap()
}

/// The keys of the first document's root mapping.
pub fn keys(file: &YamlFile) -> Vec<String> {
    root_mapping(file).keys().map(|k| k.to_string()).collect()
}

/// The value of `key` in the root mapping of `src`.
pub fn value_of(src: &str, key: &str) -> YamlNode {
    root_mapping(&YamlFile::from_str(src).unwrap())
        .get(key)
        .unwrap()
}

/// Parse `source`, apply `edit` to it, check the result is shaped as the
/// parser would shape it, and return its text.
#[track_caller]
pub fn edit_file(source: &str, edit: impl FnOnce(&YamlFile)) -> String {
    let file = YamlFile::from_str(source).unwrap();
    edit(&file);
    assert_file_parser_shaped(&file);
    file.to_string()
}

/// Like [`edit_file`], but apply `edit` to the root mapping.
#[track_caller]
pub fn edit_root(source: &str, edit: impl FnOnce(&Mapping)) -> String {
    edit_file(source, |file| edit(&root_mapping(file)))
}

/// Like [`edit_file`], but parse `source` as a `Document`.
#[track_caller]
pub fn edited(source: &str, edit: impl FnOnce(&Document)) -> String {
    let doc = Document::from_str(source).unwrap();
    edit(&doc);
    assert_parser_shaped(&doc);
    doc.to_string()
}

/// Set `metadata.version`, and `metadata.status` if `yaml` has one, in
/// `file` parsed from `yaml`, checking that nothing else changes.
#[track_caller]
pub fn edit_metadata(file: &YamlFile, yaml: &str) {
    let metadata = root_mapping(file).get_mapping("metadata").unwrap();
    metadata.set("version", "0.1.1");
    assert_file_cst_ok(file);
    let expected = yaml.replace("version: 0.1.0", "version: 0.1.1");
    assert_eq!(file.to_string(), expected);
    if yaml.contains("status: draft") {
        metadata.set("status", "stable");
        assert_file_cst_ok(file);
        assert_eq!(
            file.to_string(),
            expected.replace("status: draft", "status: stable")
        );
    }
}

/// Parse `yaml`, requiring no errors and a byte-identical round trip.
#[track_caller]
pub fn parse(yaml: &str) -> YamlFile {
    let parsed = YamlFile::parse(yaml);
    assert_eq!(parsed.errors(), Vec::<String>::new(), "{yaml:?}");
    let file = parsed.tree();
    assert_eq!(file.to_string(), yaml);
    file
}

/// The node at `path` in the first document: mapping keys and sequence
/// indices separated by `.`, or the root node for an empty path.
#[track_caller]
pub fn node_at(file: &YamlFile, path: &str) -> YamlNode {
    let doc = file.document().expect("no document");
    let root = doc
        .syntax()
        .children()
        .find_map(YamlNode::from_syntax)
        .expect("empty document");
    path.split('.')
        .filter(|segment| !segment.is_empty())
        .fold(root, |node, segment| {
            let child = if node.is_mapping() {
                node.get(segment)
            } else {
                node.get_item(segment.parse().expect("sequence index"))
            };
            child.unwrap_or_else(|| panic!("nothing at {segment:?} of {path:?}"))
        })
}

/// Check the string value of the scalar at each path.
#[track_caller]
pub fn assert_strings(file: &YamlFile, expected: &[(&str, &str)]) {
    for (path, value) in expected {
        let node = node_at(file, path);
        let scalar = node
            .as_scalar()
            .unwrap_or_else(|| panic!("{path:?} is not a scalar"));
        assert_eq!(scalar.as_string(), *value, "at {path:?}");
    }
}

/// Describe a collection as e.g. "flow sequence of 3".
pub fn shape(node: &YamlNode) -> String {
    let style = |flow: bool| if flow { "flow" } else { "block" };
    if let Some(seq) = node.as_sequence() {
        format!("{} sequence of {}", style(seq.is_flow_style()), seq.len())
    } else if let Some(map) = node.as_mapping() {
        format!("{} mapping of {}", style(map.is_flow_style()), map.len())
    } else {
        panic!("not a collection: {node:?}")
    }
}

#[track_caller]
pub fn assert_shapes(file: &YamlFile, expected: &[(&str, &str)]) {
    for (path, value) in expected {
        assert_eq!(shape(&node_at(file, path)), *value, "at {path:?}");
    }
}
