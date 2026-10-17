//! `Mapping::move_after` / `move_before` -- pull a key out of its
//! current position and re-insert it at a new position.

use super::helpers::FlowInsertPos;
use super::{Mapping, MappingEntry};
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
        if self
            .find_entry_by_key(new_key)
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
        // Contract: move, not duplicate. Drop any prior entry with the
        // same key first.
        self.remove(new_key);
        let entry = MappingEntry::new_at_indent(
            new_key,
            new_value,
            flow_context,
            use_explicit_keys,
            key_indent,
        );
        let target = target.syntax().clone();
        let children: Vec<_> = self.0.children_with_tokens().collect();
        match (flow_context, after) {
            (true, true) => self.insert_flow_entry_cst_at(&entry.0, FlowInsertPos::After(target)),
            (true, false) => self.insert_flow_entry_cst_at(&entry.0, FlowInsertPos::Before(target)),
            (false, true) => self.insert_block_entry_after(&children, &entry.0, &target),
            (false, false) => self.insert_block_entry_before(&children, &entry.0, &target),
        }
        true
    }

    /// Move a key-value pair to immediately after an existing key.
    ///
    /// If `new_key` already exists in the mapping, it is first **removed** from its
    /// current position and then re-inserted after `after_key` with the new value -
    /// so the key ends up at the requested position regardless of where it was before.
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
