//! `Mapping::move_after` / `move_before` -- pull a key out of its
//! current position and re-insert it at a new position.

use super::helpers::{held_anchor, FlowInsertPos};
use super::{Mapping, MappingEntry};
use crate::nodes::SyntaxNode;
use rowan::ast::AstNode;

impl Mapping {
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
        // Given its own value, the entry moves as it is, anchor included.
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
                Some(entry.0)
            });
            return true;
        }
        // Contract: move, not duplicate. Drop any prior entry with the
        // same key first.
        self.remove(new_key);
        let entry = MappingEntry::new_in(new_key, new_value, flow, use_explicit_keys, key_indent);
        self.insert_next_to(&entry.0, target, flow_context, after);
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
