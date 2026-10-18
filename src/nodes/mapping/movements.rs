//! `Mapping::move_after` / `move_before` -- pull a key out of its
//! current position and re-insert it at a new position.

use super::helpers::FlowInsertPos;
use super::{Mapping, MappingEntry};
use crate::lex::SyntaxKind;
use crate::nodes::trivia::{self, line_end};
use crate::nodes::{
    flow_separator, fresh_token, has_child_token, strip_flow_separator, SyntaxNode, SyntaxToken,
};
use rowan::ast::AstNode;

/// The layout that belongs to a position in a mapping rather than to the
/// entry at it, so that it stays put when entries are moved around.
pub(super) struct SlotLayout {
    /// For each entry of a flow mapping, the separator it ends with, if
    /// that can be swapped for another entry's.
    flow_separators: Vec<Option<Vec<(SyntaxKind, String)>>>,
    /// For each entry of a flow mapping that ends its line, the
    /// indentation of the line that follows.
    flow_line_indents: Vec<Option<Vec<(SyntaxKind, String)>>>,
    /// Whether the last entry of a flow mapping has a trailing comma.
    last_has_comma: bool,
    /// Whether the last entry of a block mapping ends its line.
    last_terminated: bool,
}

/// The indentation of the line after `entry`, a flow mapping entry, if
/// the entry ends with a line break followed only by that indentation.
fn flow_line_indent(entry: &SyntaxNode) -> Option<Vec<SyntaxToken>> {
    let children: Vec<_> = entry.children_with_tokens().collect();
    let start = children
        .iter()
        .rposition(|c| !matches!(c.kind(), SyntaxKind::WHITESPACE | SyntaxKind::INDENT))?;
    (children[start].kind() == SyntaxKind::NEWLINE).then(|| {
        children[start + 1..]
            .iter()
            .filter_map(|c| c.as_token().cloned())
            .collect()
    })
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

impl Mapping {
    /// Take note of the layout of this mapping that belongs to positions
    /// rather than entries, for [`restore_slot_layout`](Self::restore_slot_layout).
    pub(super) fn slot_layout(&self) -> SlotLayout {
        let entries: Vec<SyntaxNode> = self.entries().map(|e| e.0).collect();
        let texts = |tokens: Vec<SyntaxToken>| {
            tokens
                .iter()
                .map(|t| (t.kind(), t.text().to_string()))
                .collect()
        };
        let (flow_separators, flow_line_indents) = if self.is_flow_style() {
            entries
                .iter()
                .map(|e| (flow_separator(e).map(texts), flow_line_indent(e).map(texts)))
                .unzip()
        } else {
            (Vec::new(), Vec::new())
        };
        let last = entries.last();
        SlotLayout {
            flow_separators,
            flow_line_indents,
            last_has_comma: last.is_some_and(|e| has_child_token(e, |k| k == SyntaxKind::COMMA)),
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
        // The line after an entry that ends its line, after its comment,
        // is indented as the line after that position was.
        for (entry, slot) in entries.iter().zip(&layout.flow_line_indents) {
            let (Some(own), Some(slot)) = (flow_line_indent(entry), slot) else {
                continue;
            };
            let end = entry.children_with_tokens().count();
            let tokens: Vec<_> = slot
                .iter()
                .map(|(kind, text)| rowan::NodeOrToken::Token(fresh_token(*kind, text)))
                .collect();
            crate::nodes::replace_children(entry, end - own.len()..end, tokens);
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
