//! Checks on mapping keys: duplicates, and the !!set, !!pairs and !!omap types.

use super::{Rule, Violation};
use crate::anchor_resolution::{document_scope, is_merge_key, AnchorRegistry, MergedMapping};
use crate::as_yaml::SharedIdentities;
use crate::tag;
use crate::yaml::SyntaxNode;
use rowan::ast::AstNode;

/// Check that the members of a `!!set` have null values.
///
/// Like other content that does not suit a YAML 1.1 type, a value is
/// only a warning. An alias is not resolved here; the conversion to
/// `YamlValue` checks the node it refers to.
pub(super) fn check_set_values(node: &SyntaxNode, violations: &mut Vec<Violation>) {
    use crate::as_yaml::YamlNode;

    let Some(mapping) = crate::yaml::TaggedNode::cast(node.clone())
        .filter(|tagged| tagged.as_set().is_some())
        .and_then(|tagged| tagged.as_mapping())
    else {
        return;
    };
    for entry in mapping.entries() {
        let Some(value) = entry.value_node() else {
            continue;
        };
        if matches!(value, YamlNode::Alias(_)) {
            continue;
        }
        let is_null =
            crate::as_yaml::scalar_canonical(value.syntax()).is_some_and(|(t, _)| t == tag::NULL);
        if is_null {
            continue;
        }
        let member = entry
            .key_node()
            .map_or_else(String::new, |key| key.to_string());
        violations.push(Violation::warning_at(
            Rule::InvalidTag,
            value.syntax().text_range(),
            format!(
                "!!set member {member:?} has non-null value {:?}",
                value.to_string()
            ),
        ));
    }
}

/// Check that each item of a `!!omap` or `!!pairs` is a mapping,
/// untagged or tagged `!!map`, with a single entry once its merge keys
/// are resolved.
///
/// Like other content that does not suit a YAML 1.1 type, a bad item is
/// only a warning. An alias item, or a merge key that cannot be resolved,
/// is left to the conversion to `YamlValue`, which checks the node it
/// refers to.
pub(super) fn check_pairs_items(node: &SyntaxNode, violations: &mut Vec<Violation>) {
    use crate::as_yaml::YamlNode;

    let Some(tagged) = crate::yaml::TaggedNode::cast(node.clone()) else {
        return;
    };
    if !matches!(
        tagged.normalized_tag().as_deref(),
        Some(tag::OMAP | tag::PAIRS)
    ) {
        return;
    }
    let Some(written_tag) = tagged.tag() else {
        return;
    };
    let Some(sequence) = tagged.as_sequence() else {
        return;
    };
    let registry = std::cell::OnceCell::new();
    for item in sequence.values() {
        if matches!(item, YamlNode::Alias(_)) {
            continue;
        }
        if let Some(mapping) = pairs_item_mapping(&item) {
            match pairs_item_keys(&mapping, &registry) {
                Some(keys) if keys.len() != 1 => {}
                _ => continue,
            }
        }
        violations.push(Violation::warning_at(
            Rule::InvalidTag,
            item.syntax().text_range(),
            format!(
                "{written_tag} item {:?} is not a mapping with a single entry",
                item.to_string()
            ),
        ));
    }
}

/// Check that the items of a `!!omap` have distinct keys; a `!!pairs`
/// may repeat a key.
///
/// Like other content that does not suit a YAML 1.1 type, a repeated
/// key is only a warning. Keys compare as in
/// [`Self::check_duplicate_keys`], after merge keys are resolved.
pub(super) fn check_omap_keys(
    node: &SyntaxNode,
    identities: &mut SharedIdentities,
    violations: &mut Vec<Violation>,
) {
    let Some(tagged) = crate::yaml::TaggedNode::cast(node.clone()) else {
        return;
    };
    if tagged.normalized_tag().as_deref() != Some(tag::OMAP) {
        return;
    }
    let Some(sequence) = tagged.as_sequence() else {
        return;
    };
    let registry = std::cell::OnceCell::new();
    let keys = sequence.values().filter_map(|item| {
        let mapping = pairs_item_mapping(&item)?;
        let key = pairs_item_keys(&mapping, &registry)?.into_iter().next()?;
        Some((key, item.syntax().text_range()))
    });
    let mut seen: Vec<(usize, SyntaxNode)> = Vec::new();
    for (key, range) in keys {
        let Some(identity) = key_identity(&key, identities) else {
            continue;
        };
        if let Some((_, first)) = seen.iter().find(|(k, _)| *k == identity) {
            violations.push(Violation::warning_at(
                Rule::DuplicateKeys,
                range,
                format!(
                    "Duplicate !!omap key: {} (semantically equal to {})",
                    format_key(&key),
                    format_key(first)
                ),
            ));
        }
        seen.push((identity, key));
    }
}

/// The mapping that `item`, an item of a `!!omap` or `!!pairs`, is, if
/// it is one, untagged or tagged `!!map`.
fn pairs_item_mapping(item: &crate::as_yaml::YamlNode) -> Option<crate::yaml::Mapping> {
    use crate::as_yaml::YamlNode;

    match item {
        YamlNode::Mapping(mapping) => Some(mapping.clone()),
        YamlNode::TaggedNode(tagged) if tagged.normalized_tag().as_deref() == Some(tag::MAP) => {
            tagged.as_mapping()
        }
        _ => None,
    }
}

/// The KEY nodes of `mapping` with its merge keys resolved, or `None` if
/// one cannot be resolved. `registry` is built for the document the first
/// time a merge key needs it.
fn pairs_item_keys(
    mapping: &crate::yaml::Mapping,
    registry: &std::cell::OnceCell<AnchorRegistry>,
) -> Option<Vec<SyntaxNode>> {
    let key_node = |key: crate::as_yaml::YamlNode| {
        key.syntax()
            .ancestors()
            .find(|n| n.kind() == crate::SyntaxKind::KEY)
    };
    if !mapping.keys().any(|key| is_merge_key(&key)) {
        return Some(mapping.keys().filter_map(key_node).collect());
    }
    let registry =
        registry.get_or_init(|| AnchorRegistry::from_tree(&document_scope(mapping.syntax())));
    let merged = MergedMapping::build(mapping.clone(), registry).ok()?;
    Some(merged.keys().filter_map(key_node).collect())
}

/// Check for duplicate keys within a mapping node
///
/// Uses semantic comparison via `yaml_eq()`:
/// - `true` and `True` are duplicates (same boolean value)
/// - `1` and `0x1` are duplicates (same integer value)
/// - `"1"` and `1` are NOT duplicates (different types: string vs int)
/// - `null`, `~`, and empty key are all duplicates (all null)
/// - Works with complex keys (sequences, mappings) as well
/// - A key that is or holds an alias equals nothing
pub(super) fn check_duplicate_keys(
    node: &SyntaxNode,
    identities: &mut SharedIdentities,
    violations: &mut Vec<Violation>,
) {
    use crate::SyntaxKind;
    use std::collections::HashMap;

    let keys: Vec<(SyntaxNode, rowan::TextRange)> = node
        .children()
        .filter(|child| child.kind() == SyntaxKind::MAPPING_ENTRY)
        .filter_map(|child| {
            let key = child.children().find(|n| n.kind() == SyntaxKind::KEY)?;
            Some((key, child.text_range()))
        })
        .collect();
    // Only keys of the same kind can be equal, so a key with no other of
    // its kind is left alone.
    let kinds: Vec<Option<SyntaxKind>> = keys.iter().map(|(key, _)| identity_kind(key)).collect();
    let mut kind_counts: HashMap<SyntaxKind, usize> = HashMap::new();
    for kind in kinds.iter().flatten() {
        *kind_counts.entry(*kind).or_default() += 1;
    }

    // Key identity -> index of the first key with it
    let mut seen: HashMap<usize, usize> = HashMap::new();
    for (i, (key, range)) in keys.iter().enumerate() {
        if !kinds[i].is_some_and(|kind| kind_counts[&kind] > 1) {
            continue;
        }
        let Some(identity) = key_identity(key, identities) else {
            continue;
        };
        match seen.entry(identity) {
            std::collections::hash_map::Entry::Occupied(first) => {
                violations.push(Violation::error_at(
                    Rule::DuplicateKeys,
                    *range,
                    format!(
                        "Duplicate key: {} (semantically equal to {})",
                        format_key(key),
                        format_key(&keys[*first.get()].0)
                    ),
                ));
            }
            std::collections::hash_map::Entry::Vacant(slot) => {
                slot.insert(i);
            }
        }
    }
}

/// `key` as a duplicate key message shows it: its text, quoted, or `null`
/// for an empty key, which is null rather than the empty string.
fn format_key(key: &SyntaxNode) -> String {
    match key.text().to_string().trim() {
        "" => "null".to_string(),
        text => format!("{text:?}"),
    }
}

/// The identity of mapping key `key`, or `None` if it is or holds an alias.
///
/// Such a key is compared with nothing: the YAML test suite has
/// `{&a [a]: 1, *a : 2}` as valid (X38W). The identities of the nodes in
/// `key` are kept in `identities` for the keys among them, which would
/// otherwise work them out again on every level of nested collection keys.
fn key_identity(key: &SyntaxNode, identities: &mut SharedIdentities) -> Option<usize> {
    identities.identity(key).ok().flatten()
}

/// The kind of the node that gives `node`, a key or a node in one, its
/// identity, or `None` if it has none: keys of different kinds are never
/// equal.
fn identity_kind(node: &SyntaxNode) -> Option<crate::SyntaxKind> {
    use crate::SyntaxKind;
    match node.kind() {
        SyntaxKind::KEY => identity_kind(&node.first_child()?),
        // A tagged node without content is an empty scalar.
        SyntaxKind::TAGGED_NODE => node
            .first_child()
            .map_or(Some(SyntaxKind::SCALAR), |content| identity_kind(&content)),
        kind => Some(kind),
    }
}
