// Checks on edited documents shared by tests/edit_invariants.rs and the
// edit_sequence fuzz target: finding collections to edit, the YamlValue
// paths that a model of the edit works with, and comparing the result.
//
// Shared via `mod common;` from tests and `include!` from the fuzz crate,
// so this file uses only outer comments and no module attributes.

use rowan::ast::AstNode;
use rowan::TextRange;
use std::collections::{BTreeSet, HashMap};
use yaml_edit::validator::{Rule, Severity, Validator, Violation};
use yaml_edit::{debug, Document, Lang, Mapping, SyntaxKind, YamlFile, YamlNode, YamlValue};

pub type SyntaxNode = rowan::SyntaxNode<Lang>;

#[derive(Clone, Debug)]
pub enum Step {
    Key(String),
    Index(usize),
}

/// A mapping or sequence to edit, found again in a fresh parse by its kind
/// and range.
pub struct Target {
    pub kind: SyntaxKind,
    pub range: TextRange,
    pub label: String,
    /// Where the collection sits in the document's `YamlValue`, if the
    /// semantic check applies to it.
    pub path: Option<Vec<Step>>,
    /// Whether the collection is inside a mapping key.
    pub in_key: bool,
}

/// Record the `YamlValue` path of every collection under `node`. Collections
/// inside keys, or under a key whose string form is unknown, get no path.
pub fn semantic_paths(
    node: &YamlNode,
    path: Option<Vec<Step>>,
    out: &mut HashMap<(SyntaxKind, TextRange), Option<Vec<Step>>>,
) {
    match node {
        YamlNode::Mapping(m) => {
            out.insert((SyntaxKind::MAPPING, m.syntax().text_range()), path.clone());
            for entry in m.entries() {
                let key = entry.key_node();
                if let Some(key) = &key {
                    semantic_paths(key, None, out);
                }
                if let Some(value) = entry.value_node() {
                    let child = path.clone().and_then(|p| {
                        let k = key
                            .as_ref()
                            .map_or(Some(String::new()), debug::key_string)?;
                        Some([p, vec![Step::Key(k)]].concat())
                    });
                    semantic_paths(&value, child, out);
                }
            }
        }
        YamlNode::Sequence(s) => {
            out.insert(
                (SyntaxKind::SEQUENCE, s.syntax().text_range()),
                path.clone(),
            );
            for (i, item) in s.values().enumerate() {
                let child = path.clone().map(|p| [p, vec![Step::Index(i)]].concat());
                semantic_paths(&item, child, out);
            }
        }
        YamlNode::TaggedNode(t) => {
            if let Some(m) = t.as_mapping() {
                semantic_paths(&YamlNode::Mapping(m), path, out);
            } else if let Some(s) = t.as_sequence() {
                semantic_paths(&YamlNode::Sequence(s), path, out);
            }
        }
        YamlNode::Scalar(_) | YamlNode::Alias(_) => {}
    }
}

pub fn anchor_names(node: &SyntaxNode) -> Vec<String> {
    node.children_with_tokens()
        .filter_map(|c| c.into_token())
        .filter(|t| t.kind() == SyntaxKind::ANCHOR)
        .map(|t| t.text().trim_start_matches('&').to_string())
        .collect()
}

pub fn alias_names(root: &SyntaxNode) -> BTreeSet<String> {
    root.descendants_with_tokens()
        .filter_map(|c| c.into_token())
        .filter(|t| t.kind() == SyntaxKind::REFERENCE)
        .map(|t| t.text().trim_start_matches('*').to_string())
        .collect()
}

pub fn root_node(doc: &Document) -> Option<YamlNode> {
    doc.syntax().children().find_map(YamlNode::from_syntax)
}

pub fn targets(doc: &Document) -> Vec<Target> {
    let mut paths = HashMap::new();
    if let Some(root) = root_node(doc) {
        semantic_paths(&root, Some(Vec::new()), &mut paths);
    }
    let aliases = alias_names(doc.syntax());
    let mut counts = (0, 0);
    let mut out = Vec::new();
    for node in doc.syntax().descendants() {
        let label = match node.kind() {
            SyntaxKind::MAPPING => {
                counts.0 += 1;
                format!("map{}", counts.0 - 1)
            }
            SyntaxKind::SEQUENCE => {
                counts.1 += 1;
                format!("seq{}", counts.1 - 1)
            }
            _ => continue,
        };
        let range = node.text_range();
        // If the collection or an enclosing node is aliased, an edit also
        // changes the alias, which the expected value does not model.
        let aliased = node
            .ancestors()
            .flat_map(|n| anchor_names(&n))
            .any(|a| aliases.contains(&a));
        let path = if aliased {
            None
        } else {
            paths.get(&(node.kind(), range)).cloned().flatten()
        };
        out.push(Target {
            kind: node.kind(),
            range,
            label,
            path,
            in_key: node.ancestors().any(|n| n.kind() == SyntaxKind::KEY),
        });
    }
    out
}

pub fn find_node(doc: &Document, kind: SyntaxKind, range: TextRange) -> SyntaxNode {
    doc.syntax()
        .descendants()
        .find(|n| n.kind() == kind && n.text_range() == range)
        .expect("target not found in a fresh parse")
}

/// The key of the `i`th entry, or `None` for an entry without one.
pub fn nth_key(m: &Mapping, i: usize) -> Option<YamlNode> {
    m.entries().nth(i).unwrap().key_node()
}

pub fn navigate<'a>(mut value: &'a mut YamlValue, path: &[Step]) -> Option<&'a mut YamlValue> {
    for step in path {
        while let YamlValue::Tagged(_, inner) = value {
            value = inner;
        }
        value = match (step, value) {
            (Step::Key(k), YamlValue::Mapping(m)) => m.get_mut(k)?,
            (Step::Index(i), YamlValue::Sequence(s)) => s.get_mut(*i)?,
            _ => return None,
        };
    }
    while let YamlValue::Tagged(_, inner) = value {
        value = inner;
    }
    Some(value)
}

/// Whether `holder`, a KEY, VALUE or SEQUENCE_ENTRY, has an anchor that
/// replacing its content keeps, and that `aliases` refer to.
pub fn holds_aliased_anchor(holder: &SyntaxNode, aliases: &BTreeSet<String>) -> bool {
    let tagged = holder
        .children()
        .next()
        .filter(|n| n.kind() == SyntaxKind::TAGGED_NODE);
    std::iter::once(holder.clone())
        .chain(tagged)
        .flat_map(|n| anchor_names(&n))
        .any(|a| aliases.contains(&a))
}

pub fn entry_part(m: &Mapping, i: usize, kind: SyntaxKind) -> Option<SyntaxNode> {
    m.entries()
        .nth(i)?
        .syntax()
        .children()
        .find(|n| n.kind() == kind)
}

/// Like `==`, but ignoring scalar styles, which edits may legitimately
/// change.
pub fn same_value(a: &YamlValue, b: &YamlValue) -> bool {
    match (a, b) {
        (YamlValue::Scalar(a), YamlValue::Scalar(b)) => {
            a.value() == b.value() && a.scalar_type() == b.scalar_type() && a.tag() == b.tag()
        }
        (YamlValue::Sequence(a), YamlValue::Sequence(b)) => {
            a.len() == b.len() && a.iter().zip(b).all(|(a, b)| same_value(a, b))
        }
        (YamlValue::Mapping(a), YamlValue::Mapping(b)) => {
            a.len() == b.len()
                && a.iter()
                    .zip(b)
                    .all(|((ka, va), (kb, vb))| ka == kb && same_value(va, vb))
        }
        (YamlValue::OrderedMapping(a), YamlValue::OrderedMapping(b))
        | (YamlValue::Pairs(a), YamlValue::Pairs(b)) => {
            a.len() == b.len()
                && a.iter()
                    .zip(b)
                    .all(|((ka, va), (kb, vb))| ka == kb && same_value(va, vb))
        }
        (YamlValue::Tagged(ta, a), YamlValue::Tagged(tb, b)) => ta == tb && same_value(a, b),
        _ => a == b,
    }
}

pub fn cst_ok(file: &YamlFile) -> Result<(), String> {
    debug::validate_tree(file.syntax())?;
    debug::roundtrip_ok(file.syntax())
}

pub fn validator_errors(file: &YamlFile) -> Vec<Violation> {
    Validator::new()
        .validate_syntax(file.syntax())
        .into_iter()
        .filter(|v| v.severity == Severity::Error)
        .collect()
}

pub fn duplicate_key_count(errors: &[Violation]) -> usize {
    errors
        .iter()
        .filter(|v| v.rule == Rule::DuplicateKeys)
        .count()
}

/// The messages of `errors`, leaving out duplicate keys if there are no
/// more than `allowed_duplicate_keys` of them. An edit inside a key can
/// make it equal to another key, as renaming a key by hand can.
pub fn unexpected_errors(errors: &[Violation], allowed_duplicate_keys: usize) -> Vec<String> {
    let allowed = duplicate_key_count(errors) <= allowed_duplicate_keys;
    errors
        .iter()
        .filter(|v| !(allowed && v.rule == Rule::DuplicateKeys))
        .map(|v| v.to_string())
        .collect()
}
