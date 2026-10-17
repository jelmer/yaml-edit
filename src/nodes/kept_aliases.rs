//! Keeping aliases resolving when the anchors they refer to are removed.
//!
//! An alias refers to the closest preceding node with its anchor. When an
//! edit removes or replaces a node holding anchors that later aliases refer
//! to, the first of those aliases is replaced with a copy of the anchored
//! node, anchor included, so it and the aliases after it keep resolving to
//! the same content. Anchors inside such a copy that would capture an alias
//! referring to another node are left off it.

use super::{SyntaxNode, SyntaxToken};
use crate::lex::SyntaxKind;
use rowan::ast::AstNode;
use std::collections::{HashMap, HashSet};

/// Run `edit`, which removes `removed` from the tree or replaces it, and
/// then make sure the aliases outside it still resolve to what they did.
///
/// `kept` is an anchor in `removed` that the edit puts on the replacement,
/// so that aliases to it follow the new value. `edit` returns the node
/// that now holds that anchor, if any.
pub(crate) fn keeping_aliases(
    removed: &SyntaxNode,
    kept: Option<SyntaxToken>,
    edit: impl FnOnce() -> Option<SyntaxNode>,
) {
    if !removed
        .descendants_with_tokens()
        .any(|c| c.kind() == SyntaxKind::ANCHOR)
    {
        edit();
        return;
    }
    let scope = removed
        .ancestors()
        .find(|n| n.kind() == SyntaxKind::DOCUMENT)
        .unwrap_or_else(|| removed.ancestors().last().expect("ancestors has self"));
    let (anchors, aliases) = scan(&scope);
    let inside = |n: &SyntaxNode| n.ancestors().any(|a| a == *removed);
    let kept_id = kept.and_then(|k| anchors.iter().position(|a| *a == k));
    let removed_ids: HashSet<usize> = anchors
        .iter()
        .enumerate()
        .filter(|(i, a)| Some(*i) != kept_id && a.parent().is_some_and(|p| inside(&p)))
        .map(|(i, _)| i)
        .collect();
    let broken: Vec<_> = aliases
        .iter()
        .filter(|(alias, target)| {
            !inside(alias) && target.is_some_and(|t| removed_ids.contains(&t))
        })
        .map(|(alias, _)| alias)
        .collect();
    if broken.is_empty() {
        edit();
        return;
    }
    // TODO: an alias with a tag or an anchor of its own, which the parser
    // tolerates but YAML does not allow, cannot be replaced with a copy.
    // Fail before the edit rather than leave it undefined.
    if let Some(alias) = broken.iter().find(|a| !replaceable(a)) {
        panic!("cannot keep alias {alias} resolving: it has a tag or anchor of its own");
    }

    // The removed nodes are copied from a snapshot taken before the edit.
    let snapshot = SyntaxNode::new_root_mut(scope.green().into_owned());
    let (snapshot_anchors, snapshot_aliases) = scan(&snapshot);
    assert_eq!(snapshot_anchors.len(), anchors.len(), "snapshot differs");
    assert_eq!(snapshot_aliases.len(), aliases.len(), "snapshot differs");

    let mut tracker = Tracker {
        scope,
        origins: Vec::with_capacity(anchors.len()),
        live_ids: Vec::new(),
        snapshot_ids: HashMap::new(),
        live_expected: Vec::new(),
        snapshot_expected: HashMap::new(),
        removed: removed_ids,
        copied_anchors: Vec::new(),
        copied_aliases: Vec::new(),
    };
    for (i, (live, snapshot)) in anchors.iter().zip(&snapshot_anchors).enumerate() {
        if tracker.removed.contains(&i) || Some(i) == kept_id {
            tracker.origins.push(Origin::Snapshot(snapshot.clone()));
        } else {
            tracker.origins.push(Origin::Live(live.clone()));
            tracker.live_ids.push((live.clone(), i));
        }
        tracker.snapshot_ids.insert(snapshot.clone(), i);
    }
    for ((live, target), (snapshot, _)) in aliases.iter().zip(&snapshot_aliases) {
        if !inside(live) {
            tracker.live_expected.push((live.clone(), *target));
        }
        tracker.snapshot_expected.insert(snapshot.clone(), *target);
    }

    let holder = edit();

    if let Some(k) = kept_id {
        match holder.as_ref().and_then(super::mapping::held_anchor_token) {
            Some(anchor) => {
                tracker.live_ids.push((anchor.clone(), k));
                tracker.origins[k] = Origin::Live(anchor);
            }
            // The edit dropped the anchor after all, so aliases to it get
            // the old node.
            None => {
                tracker.removed.insert(k);
            }
        }
    }
    tracker.repair();
}

/// The anchors and aliases under `scope` in document order, with the index
/// of the anchor each alias refers to.
fn scan(scope: &SyntaxNode) -> (Vec<SyntaxToken>, Vec<(SyntaxNode, Option<usize>)>) {
    let mut anchors = Vec::new();
    let mut aliases = Vec::new();
    let mut latest: HashMap<String, usize> = HashMap::new();
    for element in scope.descendants_with_tokens() {
        match element {
            rowan::NodeOrToken::Token(t) if t.kind() == SyntaxKind::ANCHOR => {
                latest.insert(anchor_name(&t).to_string(), anchors.len());
                anchors.push(t);
            }
            rowan::NodeOrToken::Node(n) if n.kind() == SyntaxKind::ALIAS => {
                let target = latest.get(&alias_name(&n)).copied();
                aliases.push((n, target));
            }
            _ => {}
        }
    }
    (anchors, aliases)
}

fn anchor_name(anchor: &SyntaxToken) -> &str {
    anchor.text().strip_prefix('&').unwrap_or(anchor.text())
}

fn alias_name(alias: &SyntaxNode) -> String {
    crate::yaml::Alias::cast(alias.clone())
        .expect("an ALIAS node")
        .name()
}

/// The node `anchor` is on: the tagged node it sits in after the tag, or
/// else the node after it.
fn anchored_node(anchor: &SyntaxToken) -> SyntaxNode {
    let parent = anchor.parent().expect("an anchor has a parent");
    if parent.kind() == SyntaxKind::TAGGED_NODE {
        return parent;
    }
    anchor
        .siblings_with_tokens(rowan::Direction::Next)
        .find_map(|c| c.into_node())
        .expect("the parser puts a node after every anchor")
}

/// The anchors in a node copied for an alias, in document order: its own
/// anchor, if outside the node, and those within it.
fn copied_anchors(anchor: Option<&SyntaxToken>, node: &SyntaxNode) -> Vec<SyntaxToken> {
    anchor
        .cloned()
        .into_iter()
        .chain(
            node.descendants_with_tokens()
                .filter_map(|c| c.into_token())
                .filter(|t| t.kind() == SyntaxKind::ANCHOR),
        )
        .collect()
}

fn descendant_aliases(node: &SyntaxNode) -> Vec<SyntaxNode> {
    node.descendants()
        .filter(|n| n.kind() == SyntaxKind::ALIAS)
        .collect()
}

/// Where the node of an anchor id is.
#[derive(Clone)]
enum Origin {
    Live(SyntaxToken),
    /// In the snapshot taken before the edit, for a removed node.
    Snapshot(SyntaxToken),
}

/// What repairing the aliases after an edit keeps track of.
///
/// Live nodes and tokens compare equal by position and green node, which
/// change as the tree is edited, so they are kept in lists rather than
/// hashed. The snapshot is never edited.
struct Tracker {
    /// The document, or tree, the edit was made in.
    scope: SyntaxNode,
    /// Per anchor id, the anchor whose node aliases with that id are to
    /// resolve to.
    origins: Vec<Origin>,
    /// The anchor id of each anchor. Copies share the id of the anchor
    /// they were copied from.
    live_ids: Vec<(SyntaxToken, usize)>,
    snapshot_ids: HashMap<SyntaxToken, usize>,
    /// The anchor id each alias referred to before the edit, or that of
    /// the original for an alias in a copy.
    live_expected: Vec<(SyntaxNode, Option<usize>)>,
    snapshot_expected: HashMap<SyntaxNode, Option<usize>>,
    /// Anchors the edit removed.
    removed: HashSet<usize>,
    /// Anchors and aliases in copies made here.
    copied_anchors: Vec<SyntaxToken>,
    copied_aliases: Vec<SyntaxNode>,
}

impl Tracker {
    /// The anchor id of `anchor`, in the tree.
    fn live_id(&mut self, anchor: &SyntaxToken) -> usize {
        if let Some((_, id)) = self.live_ids.iter().find(|(t, _)| t == anchor) {
            return *id;
        }
        // An anchor the edit added.
        let id = self.origins.len();
        self.origins.push(Origin::Live(anchor.clone()));
        self.live_ids.push((anchor.clone(), id));
        id
    }

    /// The anchor id `alias`, in the tree, is to resolve to, if known.
    fn expected(&self, alias: &SyntaxNode) -> Option<Option<usize>> {
        self.live_expected
            .iter()
            .find(|(a, _)| a == alias)
            .map(|(_, e)| *e)
    }

    /// Forget about nodes and tokens that are no longer in the tree.
    fn prune(&mut self) {
        let scope = self.scope.clone();
        let attached = |n: Option<SyntaxNode>| n.is_some_and(|n| n.ancestors().any(|a| a == scope));
        self.live_ids.retain(|(t, _)| attached(t.parent()));
        self.live_expected
            .retain(|(a, _)| attached(Some(a.clone())));
        self.copied_anchors.retain(|t| attached(t.parent()));
        self.copied_aliases.retain(|a| attached(Some(a.clone())));
    }

    /// Replace aliases that no longer resolve as they did with copies of
    /// what they referred to, until none are left.
    fn repair(&mut self) {
        while let Some((alias, target)) = self.first_broken() {
            self.copy_for(&alias, target);
        }
    }

    /// The first alias that resolves differently because of the edit or a
    /// copy made here, with the anchor id it should resolve to.
    fn first_broken(&mut self) -> Option<(SyntaxNode, usize)> {
        self.prune();
        let mut latest: HashMap<String, SyntaxToken> = HashMap::new();
        let elements: Vec<_> = self.scope.descendants_with_tokens().collect();
        for element in elements {
            match element {
                rowan::NodeOrToken::Token(t) if t.kind() == SyntaxKind::ANCHOR => {
                    latest.insert(anchor_name(&t).to_string(), t);
                }
                rowan::NodeOrToken::Node(alias) if alias.kind() == SyntaxKind::ALIAS => {
                    let Some(expected) = self.expected(&alias) else {
                        // An alias the edit added.
                        continue;
                    };
                    let current = latest.get(&alias_name(&alias)).cloned();
                    let current_id = current.as_ref().map(|t| self.live_id(t));
                    if current_id == expected {
                        continue;
                    }
                    let broken = expected.is_some_and(|e| self.removed.contains(&e))
                        || current
                            .as_ref()
                            .is_some_and(|t| self.copied_anchors.contains(t))
                        || self.copied_aliases.contains(&alias);
                    // An alias that was undefined can only be caught by an
                    // anchor of a copy that refers to itself, which has to
                    // stay; there is nothing to copy for it.
                    if let (true, Some(expected)) = (broken, expected) {
                        return Some((alias, expected));
                    }
                }
                _ => {}
            }
        }
        None
    }

    /// Replace `alias` with a copy of the node with anchor id `target`.
    fn copy_for(&mut self, alias: &SyntaxNode, target: usize) {
        let (origin, in_snapshot) = match &self.origins[target] {
            Origin::Live(t) => (t.clone(), false),
            Origin::Snapshot(t) => (t.clone(), true),
        };
        let node = anchored_node(&origin);
        let outer =
            (origin.parent().map(|p| p.kind()) != Some(SyntaxKind::TAGGED_NODE)).then_some(&origin);
        let holder = replace_alias(alias, &node, outer.map(|a| a.text()));

        let originals = copied_anchors(outer, &node);
        let copies: Vec<_> = holder
            .descendants_with_tokens()
            .filter_map(|c| c.into_token())
            .filter(|t| t.kind() == SyntaxKind::ANCHOR)
            .collect();
        assert_eq!(
            originals.len(),
            copies.len(),
            "copy of {node} lost or gained anchors: {holder}"
        );
        for (original, copy) in originals.iter().zip(&copies) {
            let id = if in_snapshot {
                self.snapshot_ids[original]
            } else {
                self.live_id(original)
            };
            self.live_ids.push((copy.clone(), id));
            self.copied_anchors.push(copy.clone());
        }

        let original_aliases = descendant_aliases(&node);
        let copied_aliases = descendant_aliases(&holder);
        assert_eq!(
            original_aliases.len(),
            copied_aliases.len(),
            "copy of {node} lost or gained aliases: {holder}"
        );
        for (original, copy) in original_aliases.iter().zip(copied_aliases) {
            let expected = if in_snapshot {
                self.snapshot_expected[original]
            } else {
                match self.expected(original) {
                    Some(expected) => expected,
                    None => self.current_target(original),
                }
            };
            self.live_expected.push((copy.clone(), expected));
            self.copied_aliases.push(copy);
        }

        // A copy of a node still in the tree only needs the anchors that
        // aliases in it refer to.
        for copy in copies.iter().rev() {
            let (captures_others, used) = self.captures(copy);
            if captures_others || (!in_snapshot && !used) {
                self.live_ids.retain(|(t, _)| t != copy);
                self.copied_anchors.retain(|t| t != copy);
                super::detach_anchor(copy);
            }
        }
    }

    /// The anchor id `alias`, in the tree, resolves to now.
    fn current_target(&mut self, alias: &SyntaxNode) -> Option<usize> {
        let name = alias_name(alias);
        let mut latest = None;
        for element in self.scope.descendants_with_tokens() {
            match element {
                rowan::NodeOrToken::Token(t)
                    if t.kind() == SyntaxKind::ANCHOR && anchor_name(&t) == name =>
                {
                    latest = Some(t);
                }
                rowan::NodeOrToken::Node(n) if n == *alias => break,
                _ => {}
            }
        }
        latest.map(|t| self.live_id(&t))
    }

    /// Whether `anchor`, in a copy, captures an alias that referred to
    /// another node, and whether it captures one that referred to its own.
    ///
    /// An anchor that aliases inside its own node refer to is not reported
    /// as capturing others, as the node could not be written out without it.
    fn captures(&mut self, anchor: &SyntaxToken) -> (bool, bool) {
        let id = self.live_id(anchor);
        let name = anchor_name(anchor).to_string();
        let recursive = descendant_aliases(&anchored_node(anchor))
            .iter()
            .any(|a| self.expected(a) == Some(Some(id)));
        let (mut others, mut used) = (false, false);
        let mut after = false;
        for element in self.scope.descendants_with_tokens() {
            match element {
                rowan::NodeOrToken::Token(t) if t == *anchor => after = true,
                rowan::NodeOrToken::Token(t)
                    if after && t.kind() == SyntaxKind::ANCHOR && anchor_name(&t) == name =>
                {
                    break;
                }
                rowan::NodeOrToken::Node(n)
                    if after && n.kind() == SyntaxKind::ALIAS && alias_name(&n) == name =>
                {
                    match self.expected(&n) {
                        Some(e) if e == Some(id) => used = true,
                        Some(_) => others = true,
                        None => {}
                    }
                }
                _ => {}
            }
        }
        (others && !recursive, used)
    }
}

/// Whether [`replace_alias`] can replace `alias`.
fn replaceable(alias: &SyntaxNode) -> bool {
    alias.parent().is_some_and(|p| {
        matches!(
            p.kind(),
            SyntaxKind::VALUE | SyntaxKind::SEQUENCE_ENTRY | SyntaxKind::KEY
        ) && super::mapping::held_anchor_token(&p).is_none()
    })
}

/// Replace `alias` with a copy of `node` under `anchor`, and return the
/// node that now holds the copy.
fn replace_alias(alias: &SyntaxNode, node: &SyntaxNode, anchor: Option<&str>) -> SyntaxNode {
    assert!(
        replaceable(alias),
        "cannot replace alias {alias} with a copy"
    );
    let value = crate::as_yaml::YamlNode::from_syntax(node.clone())
        .expect("an anchored node is a YAML node");
    let parent = alias.parent().expect("an alias in a document has a parent");
    let container = parent.parent();
    match parent.kind() {
        SyntaxKind::VALUE => {
            let entry = container
                .and_then(super::MappingEntry::cast)
                .expect("a VALUE is in a MAPPING_ENTRY");
            let flow = entry
                .syntax()
                .parent()
                .filter(|m| m.kind() == SyntaxKind::MAPPING)
                .and_then(super::Mapping::cast)
                .filter(|m| m.is_flow_style())
                .map(|m| super::flow_quoting(m.syntax()));
            match flow.and_then(|q| super::mapping::flow_form(&value, 2, q)) {
                Some(flow_value) => entry.replace_value_under(flow_value, true, anchor),
                None => entry.replace_value_under(value, flow.is_some(), anchor),
            }
            entry.value().expect("the entry was just given a value")
        }
        SyntaxKind::SEQUENCE_ENTRY => {
            let sequence = container
                .and_then(super::Sequence::cast)
                .expect("a SEQUENCE_ENTRY is in a SEQUENCE");
            let index = sequence
                .syntax()
                .children()
                .filter(|n| n.kind() == SyntaxKind::SEQUENCE_ENTRY)
                .position(|n| n == parent)
                .expect("the entry is in its sequence");
            sequence.replace_item_under(index, value, anchor)
        }
        SyntaxKind::KEY => {
            let entry = container.expect("a KEY is in a MAPPING_ENTRY");
            let mapping = entry
                .parent()
                .and_then(super::Mapping::cast)
                .expect("a MAPPING_ENTRY is in a MAPPING");
            mapping.replace_key(&entry, value, anchor)
        }
        kind => unreachable!("replaceable alias in a {kind:?}"),
    }
}
