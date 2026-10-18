//! `Mapping::move_after` / `move_before` -- pull a key out of its
//! current position and re-insert it at a new position.

use super::helpers::FlowInsertPos;
use super::{Mapping, MappingEntry};
use crate::nodes::trivia::{self, line_end, FlowSeparator};
use crate::nodes::SyntaxNode;
use rowan::ast::AstNode;

/// The layout that belongs to a position in a mapping rather than to the
/// entry at it, so that it stays put when entries are moved around.
pub(super) struct SlotLayout {
    /// For each entry of a flow mapping, the separator it ends with, if
    /// that can be swapped for another entry's.
    flow_separators: Vec<Option<FlowSeparator>>,
    /// Whether the last entry of a flow mapping has a trailing comma.
    last_has_comma: bool,
    /// Whether the last entry of a block mapping ends its line.
    last_terminated: bool,
}

impl Mapping {
    /// Take note of the layout of this mapping that belongs to positions
    /// rather than entries, for [`restore_slot_layout`](Self::restore_slot_layout).
    pub(super) fn slot_layout(&self) -> SlotLayout {
        let entries: Vec<SyntaxNode> = self.entries().map(|e| e.0).collect();
        let flow_separators = if self.is_flow_style() {
            entries.iter().map(FlowSeparator::of).collect()
        } else {
            Vec::new()
        };
        let last = entries.last();
        SlotLayout {
            flow_separators,
            last_has_comma: last.is_some_and(trivia::has_comma),
            last_terminated: last.map_or(true, |e| line_end(e).is_terminated()),
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
                if !line_end(entry).is_terminated() {
                    trivia::terminate(entry);
                }
            }
            if !layout.last_terminated {
                trivia::unterminate_last(&self.0);
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
            match slot {
                Some(slot) if FlowSeparator::swap(entry, slot) => {}
                _ if is_last && !layout.last_has_comma => FlowSeparator::strip(entry),
                _ => FlowSeparator::insert_comma(entry),
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
        // Given its own value, the entry moves as it is, comments and
        // anchor included, as reorder_fields would move it.
        let own_value = existing.filter(|e| {
            let value = e.value().and_then(|v| v.children().next());
            value.is_some() && value.as_ref() == new_value.as_node()
        });
        if let Some(old) = own_value {
            let mut order: Vec<SyntaxNode> = self
                .entries()
                .map(|e| e.0)
                .filter(|e| *e != old.0)
                .collect();
            let at = order
                .iter()
                .position(|e| e == target.syntax())
                .expect("the target is in its mapping");
            order.insert(at + usize::from(after), old.0);
            self.arrange_entries(&order);
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
        match (flow, after) {
            (true, true) => self.insert_flow_entry_cst_at(entry, FlowInsertPos::After(target)),
            (true, false) => self.insert_flow_entry_cst_at(entry, FlowInsertPos::Before(target)),
            (false, true) => self.insert_block_entry_after(entry, &target),
            (false, false) => self.insert_block_entry_before(entry, &target),
        }
    }

    /// Move a key-value pair to immediately after an existing key.
    ///
    /// If `new_key` already exists in the mapping, it is first **removed** from its
    /// current position and then re-inserted after `after_key` with the new value -
    /// so the key ends up at the requested position regardless of where it was before.
    /// Passing the entry's current value, as [`get`](Self::get) returns it,
    /// moves the entry as it is, comments and anchor included; see
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
