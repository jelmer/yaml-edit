//! `Mapping::move_after` / `move_before` -- pull a key out of its
//! current position and re-insert it at a new position.

use super::helpers::{held_anchor, FlowInsertPos};
use super::{Mapping, MappingEntry};
use crate::lex::SyntaxKind;
use crate::nodes::{
    ensure_trailing_newline, entry_line_terminated, flow_separator, fresh_token,
    hand_line_break_to_mapping, has_child_token, strip_flow_separator, SyntaxNode,
};
use rowan::ast::AstNode;

/// The layout that belongs to a position in a mapping rather than to the
/// entry at it, so that it stays put when entries are moved around.
pub(super) struct SlotLayout {
    /// For each entry of a flow mapping, the separator it ends with, if
    /// that can be swapped for another entry's.
    flow_separators: Vec<Option<Vec<(SyntaxKind, String)>>>,
    /// Whether the last entry of a flow mapping has a trailing comma.
    last_has_comma: bool,
    /// Whether the last entry of a block mapping ends its line.
    last_terminated: bool,
}

/// Put a comma right after the content of `entry`, a flow mapping entry
/// that lacks one, ahead of any comment that follows it.
fn insert_flow_comma(entry: &SyntaxNode) {
    let at = entry
        .children_with_tokens()
        .filter(|c| c.as_node().is_some())
        .last()
        .map_or(0, |n| n.index() + 1);
    let mut tokens = vec![fresh_token(SyntaxKind::COMMA, ",").into()];
    let spaced = entry.children_with_tokens().nth(at).is_some_and(|c| {
        matches!(
            c.kind(),
            SyntaxKind::WHITESPACE | SyntaxKind::NEWLINE | SyntaxKind::INDENT
        )
    });
    if !spaced {
        tokens.push(fresh_token(SyntaxKind::WHITESPACE, " ").into());
    }
    entry.splice_children(at..at, tokens);
}

/// Remove the line break that ends `mapping`, a block mapping whose last
/// line had none before its entries were moved around. One that is part
/// of a block scalar's value stays, as removing it would change the value.
fn unterminate_last_line(mapping: &SyntaxNode) {
    let Some(last) = mapping
        .descendants_with_tokens()
        .filter_map(|c| c.into_token())
        .filter(|t| !t.text().is_empty())
        .last()
    else {
        return;
    };
    let in_scalar = last
        .parent_ancestors()
        .take_while(|n| n != mapping)
        .any(|n| n.kind() == SyntaxKind::SCALAR);
    if last.kind() == SyntaxKind::NEWLINE && !in_scalar {
        last.detach();
    }
}

impl Mapping {
    /// Take note of the layout of this mapping that belongs to positions
    /// rather than entries, for [`restore_slot_layout`](Self::restore_slot_layout).
    pub(super) fn slot_layout(&self) -> SlotLayout {
        let entries: Vec<SyntaxNode> = self.entries().map(|e| e.0).collect();
        let flow_separators = if self.is_flow_style() {
            entries
                .iter()
                .map(|e| {
                    flow_separator(e).map(|tokens| {
                        tokens
                            .iter()
                            .map(|t| (t.kind(), t.text().to_string()))
                            .collect()
                    })
                })
                .collect()
        } else {
            Vec::new()
        };
        let last = entries.last();
        SlotLayout {
            flow_separators,
            last_has_comma: last.is_some_and(|e| has_child_token(e, |k| k == SyntaxKind::COMMA)),
            last_terminated: last.map_or(true, entry_line_terminated),
        }
    }

    /// Give each position of this mapping, whose entries have been moved
    /// around since `layout` was taken, the layout it had then.
    pub(super) fn restore_slot_layout(&self, layout: &SlotLayout) {
        let entries: Vec<SyntaxNode> = self.entries().map(|e| e.0).collect();
        let Some((last, rest)) = entries.split_last() else {
            return;
        };
        if !self.is_flow_style() {
            for entry in rest {
                if !entry_line_terminated(entry) {
                    ensure_trailing_newline(entry);
                    hand_line_break_to_mapping(entry);
                }
            }
            if !layout.last_terminated {
                unterminate_last_line(&self.0);
            }
            return;
        }
        assert_eq!(
            entries.len(),
            layout.flow_separators.len(),
            "moving entries keeps their number"
        );
        for (entry, slot) in entries.iter().zip(&layout.flow_separators) {
            let is_last = entry == last;
            match (flow_separator(entry), slot) {
                (Some(own), Some(slot)) => {
                    let end = entry.children_with_tokens().count();
                    let tokens: Vec<_> = slot
                        .iter()
                        .map(|(kind, text)| rowan::NodeOrToken::Token(fresh_token(*kind, text)))
                        .collect();
                    crate::nodes::replace_children(entry, end - own.len()..end, tokens);
                }
                _ if is_last && !layout.last_has_comma => strip_flow_separator(entry),
                _ if !has_child_token(entry, |k| k == SyntaxKind::COMMA) => {
                    insert_flow_comma(entry)
                }
                _ => {}
            }
        }
    }

    /// Move-or-insert `new_key: new_value` next to the entry for `ref_key`:
    /// after it if `after` is set, before it otherwise. Returns `false` if
    /// `ref_key` is not present (nothing to anchor on).
    fn move_around(
        &self,
        ref_key: &impl crate::AsYaml,
        new_key: &impl crate::AsYaml,
        new_value: &impl crate::AsYaml,
        after: bool,
    ) -> bool {
        let Some(target) = self.find_entry_by_key(ref_key) else {
            return false;
        };
        let existing = self.find_entry_by_key(new_key);
        if existing
            .as_ref()
            .is_some_and(|e| e.syntax() == target.syntax())
        {
            self.set(new_key, new_value);
            return true;
        }
        let flow_context = self.is_flow_style();
        // Decided before the old entry goes, as that may be the only one
        // setting the mapping's style.
        let use_explicit_keys = self.uses_explicit_keys();
        let key_indent = self.detect_indentation_level();
        let flow = flow_context.then(|| crate::nodes::flow_quoting(&self.0));
        let target = target.syntax().clone();
        // Moving an entry is not appending one: the end of the mapping
        // keeps its trailing comma, or its lack of a final line break.
        let layout = self.slot_layout();
        // Given its own value, the entry moves as it is, anchor included.
        // TODO: a comment on the entry's line (`a: 1  # c`) is lost, as
        // the entry is rebuilt from its key and value nodes.
        let own_value = existing.filter(|e| {
            let value = e.value().and_then(|v| v.children().next());
            value.is_some() && value.as_ref() == new_value.as_node()
        });
        if let Some(old) = own_value {
            let key = old.key_node().expect("an entry found by its key has one");
            let value = old.value_node().expect("the entry has a value");
            let anchor = old.value().and_then(|v| held_anchor(&v));
            crate::nodes::kept_aliases::keeping_aliases_moved(&self.0, Some(&old.0), || {
                let children: Vec<_> = self.0.children_with_tokens().collect();
                let i = children
                    .iter()
                    .position(|c| c.as_node() == Some(&old.0))
                    .expect("the entry is in its mapping");
                self.splice_out_entry(&children, i, &old.0);
                let entry = MappingEntry::new_in(&key, &value, flow, use_explicit_keys, key_indent);
                self.insert_next_to(&entry.0, target, flow_context, after);
                self.put_value(&entry, &value, anchor.as_deref());
                self.restore_slot_layout(&layout);
                Some(entry.0)
            });
            return true;
        }
        // Contract: move, not duplicate. Drop any prior entry with the
        // same key first.
        let moved = self.remove(new_key).is_some();
        let entry = MappingEntry::new_in(new_key, new_value, flow, use_explicit_keys, key_indent);
        self.insert_next_to(&entry.0, target, flow_context, after);
        if moved {
            self.restore_slot_layout(&layout);
        }
        true
    }

    /// Insert `entry` after `target`, an entry of this mapping, if `after`
    /// is set, and before it otherwise.
    fn insert_next_to(&self, entry: &SyntaxNode, target: SyntaxNode, flow: bool, after: bool) {
        let children: Vec<_> = self.0.children_with_tokens().collect();
        match (flow, after) {
            (true, true) => self.insert_flow_entry_cst_at(entry, FlowInsertPos::After(target)),
            (true, false) => self.insert_flow_entry_cst_at(entry, FlowInsertPos::Before(target)),
            (false, true) => self.insert_block_entry_after(&children, entry, &target),
            (false, false) => self.insert_block_entry_before(&children, entry, &target),
        }
    }

    /// Move a key-value pair to immediately after an existing key.
    ///
    /// If `new_key` already exists in the mapping, it is first **removed** from its
    /// current position and then re-inserted after `after_key` with the new value -
    /// so the key ends up at the requested position regardless of where it was before.
    /// Passing the entry's current value, as [`get`](Self::get) returns it,
    /// moves the entry as it is, anchor included; see
    /// [`reorder_fields`](Self::reorder_fields) for what happens to aliases.
    ///
    /// If `after_key` is not found, returns `false` and leaves the mapping unchanged.
    /// Returns `true` on success.
    ///
    /// Use [`insert_after`](Self::insert_after) if you want existing entries to be
    /// updated in-place rather than moved.
    pub fn move_after(
        &self,
        after_key: impl crate::AsYaml,
        new_key: impl crate::AsYaml,
        new_value: impl crate::AsYaml,
    ) -> bool {
        self.move_around(&after_key, &new_key, &new_value, true)
    }

    /// Move a key-value pair to immediately before an existing key.
    ///
    /// If `new_key` already exists in the mapping, it is first **removed** from its
    /// current position and then re-inserted before `before_key` with the new value.
    /// Passing the entry's current value moves it as it is, as with
    /// [`move_after`](Self::move_after).
    ///
    /// If `before_key` is not found, returns `false` and leaves the mapping unchanged.
    /// Returns `true` on success.
    ///
    /// Use [`insert_before`](Self::insert_before) if you want existing entries to be
    /// updated in-place rather than moved.
    pub fn move_before(
        &self,
        before_key: impl crate::AsYaml,
        new_key: impl crate::AsYaml,
        new_value: impl crate::AsYaml,
    ) -> bool {
        self.move_around(&before_key, &new_key, &new_value, false)
    }
}
