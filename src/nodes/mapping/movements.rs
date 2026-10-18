//! Moving entries around a mapping: `Mapping::reorder_fields`,
//! `move_after` and `move_before`.

use super::{Mapping, MappingEntry};
use crate::lex::SyntaxKind;
use crate::nodes::trivia::Slot;
use crate::nodes::trivia::{self, ends_with_line_break, FlowSeparator};
use crate::nodes::{fresh_token, Lang, SyntaxNode};
use rowan::ast::AstNode;

type Element = rowan::SyntaxElement<Lang>;

/// An entry being moved, with what goes along with it.
struct Group {
    /// The comment lines before it, with any blank lines among and after
    /// them.
    lines_before: Vec<Element>,
    entry: SyntaxNode,
}

/// The layout that belongs to a position in a mapping rather than to the
/// entry at it, so that it stays put when entries are moved around.
struct SlotLayout {
    /// For each entry of a flow mapping, the separator it ends with, if
    /// that can be swapped for another entry's.
    flow_separators: Vec<Option<FlowSeparator>>,
    /// Whether the last entry of a flow mapping has a trailing comma.
    last_has_comma: bool,
    /// Whether the last entry of a block mapping ends its line.
    last_terminated: bool,
}

impl Mapping {
    /// Put the entries of this mapping in the order of `order`, which holds
    /// each of them once. An entry takes along the rest of its line and, in
    /// a block mapping, the comment lines before it back to the previous
    /// entry or the blank lines after that; the layout that belongs to
    /// positions, including those blank lines, stays put.
    pub(super) fn arrange_entries(&self, order: &[SyntaxNode]) {
        let all: Vec<Element> = self.0.children_with_tokens().collect();
        let starts: Vec<usize> = all
            .iter()
            .enumerate()
            .filter(|(_, c)| c.kind() == SyntaxKind::MAPPING_ENTRY)
            .map(|(i, _)| i)
            .collect();
        assert_eq!(
            starts.len(),
            order.len(),
            "the order names every entry once"
        );
        let Some(&first) = starts.first() else {
            return;
        };
        let layout = self.slot_layout();
        let flow = self.is_flow_style();
        // A flow mapping's first entry has the indentation of its line
        // inside the mapping, as the others do.
        let mut prefix = all[..first].to_vec();
        let first_indent = prefix
            .last()
            .filter(|c| flow && c.kind() == SyntaxKind::INDENT)
            .cloned();
        if first_indent.is_some() {
            prefix.pop();
        }

        // What moves with each entry, and for each position the blank lines
        // after it and the INDENT starting the next entry's line.
        let mut groups: Vec<Group> = Vec::new();
        let mut gaps: Vec<Vec<Element>> = Vec::new();
        let mut line_indents = Vec::new();
        let mut trailing = Vec::new();
        let mut lines_before = Vec::new();
        for (i, &at) in starts.iter().enumerate() {
            let entry = all[at].as_node().expect("an entry is a node");
            groups.push(Group {
                lines_before: std::mem::take(&mut lines_before),
                entry: entry.clone(),
            });
            let Some(&next) = starts.get(i + 1) else {
                trailing = all[at + 1..].to_vec();
                let gap_end = leading_blank_lines_end(&trailing);
                gaps.push(trailing.drain(..gap_end).collect());
                break;
            };
            let mut between = all[at + 1..next].to_vec();
            let indent = between
                .last()
                .is_some_and(|c| c.kind() == SyntaxKind::INDENT)
                .then(|| between.pop())
                .flatten();
            line_indents.push(indent);
            lines_before = between.split_off(leading_blank_lines_end(&between));
            gaps.push(between);
        }

        let ordered: Vec<Group> = order
            .iter()
            .map(|wanted| {
                let pos = groups
                    .iter()
                    .position(|group| group.entry == *wanted)
                    .expect("the order names entries of this mapping");
                groups.remove(pos)
            })
            .collect();

        // A block scalar would take the blank lines after it into its
        // value, so they go after the next entry that can have them
        // instead, or failing that the one before.
        let ends_in_block_scalar: Vec<bool> = ordered
            .iter()
            .map(|group| crate::nodes::ending_block_scalar(&group.entry).is_some())
            .collect();
        for i in 0..gaps.len() {
            if !ends_in_block_scalar[i] || gaps[i].is_empty() {
                continue;
            }
            let home = (i + 1..gaps.len())
                .chain((0..i).rev())
                .find(|&j| !ends_in_block_scalar[j])
                .expect("blank lines follow an entry that does not end in a block scalar");
            let moved = std::mem::take(&mut gaps[i]);
            gaps[home].extend(moved);
        }

        // Comment lines now before the first entry go where the parser
        // puts them, ahead of the mapping.
        let lines_lead = !ordered[0].lines_before.is_empty();
        // Comment lines now first in a flow mapping may follow the `{` on
        // its line.
        let opening_line_open = flow
            && prefix
                .last()
                .is_some_and(|c| c.kind() != SyntaxKind::NEWLINE);
        let mut new_children = prefix;
        let last_gap = gaps.pop().expect("there is an entry");
        for (i, mut group) in ordered.into_iter().enumerate() {
            let indent = match i.checked_sub(1) {
                Some(i) => {
                    new_children.append(&mut gaps[i]);
                    line_indents[i].clone()
                }
                None if first_indent.is_some() => first_indent.clone(),
                // The first entry's line is indented outside a block
                // mapping, unless lines go before it.
                None if lines_lead => line_indents[0]
                    .as_ref()
                    .and_then(|c| c.as_token())
                    .map(|t| fresh_token(t.kind(), t.text()).into()),
                None => None,
            };
            if i == 0 && opening_line_open {
                if let Some(first) = group
                    .lines_before
                    .first_mut()
                    .filter(|c| c.kind() == SyntaxKind::INDENT)
                {
                    *first = fresh_token(SyntaxKind::WHITESPACE, &first.to_string()).into();
                }
            }
            new_children.extend(group.lines_before);
            new_children.extend(indent);
            new_children.push(group.entry.into());
        }
        new_children.extend(last_gap);
        new_children.extend(trailing);

        crate::nodes::kept_aliases::keeping_aliases_moved(&self.0, None, || {
            crate::nodes::replace_children(&self.0, 0..all.len(), new_children);
            if lines_lead && !flow {
                crate::yaml::move_leading_lines_out(&self.0);
            }
            self.restore_slot_layout(&layout);
            for entry in order {
                crate::nodes::replaced_value::settle_lines_after_block_scalar(entry);
            }
            None
        });
    }

    /// Take note of the layout of this mapping that belongs to positions
    /// rather than entries, for [`restore_slot_layout`](Self::restore_slot_layout).
    fn slot_layout(&self) -> SlotLayout {
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
            last_terminated: last.map_or(true, ends_with_line_break),
        }
    }

    /// Give each position of this mapping, whose entries have been moved
    /// around since `layout` was taken, the layout it had then.
    fn restore_slot_layout(&self, layout: &SlotLayout) {
        let entries: Vec<SyntaxNode> = self.entries().map(|e| e.0).collect();
        let Some((last, rest)) = entries.split_last() else {
            return;
        };
        if !self.is_flow_style() {
            for entry in rest {
                if !ends_with_line_break(entry) {
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
        if let Some(existing) = existing {
            // Given its own value, the entry moves as it is, comments and
            // anchor included, as reorder_fields would move it. Given
            // another, it is replaced first, its old value going as with
            // remove, so that its key moves the same way.
            let value = existing.value().and_then(|v| v.children().next());
            let own_value = value.is_some() && value == new_value.value_node();
            let old = if own_value {
                existing.0
            } else {
                self.replace_entry(&existing.0, new_key, new_value, false)
            };
            let mut order: Vec<SyntaxNode> =
                self.entries().map(|e| e.0).filter(|e| *e != old).collect();
            let at = order
                .iter()
                .position(|e| e == target.syntax())
                .expect("the target is in its mapping");
            order.insert(at + usize::from(after), old);
            self.arrange_entries(&order);
            return true;
        }
        let flow = self
            .is_flow_style()
            .then(|| crate::nodes::flow_quoting(&self.0));
        let entry = MappingEntry::new_in(
            new_key,
            new_value,
            flow,
            self.uses_explicit_keys(),
            self.detect_indentation_level(),
        );
        let target = target.syntax().clone();
        let slot = if after {
            Slot::After(target)
        } else {
            Slot::Before(target)
        };
        self.attach(&entry.0, slot);
        true
    }

    /// Move a key-value pair to immediately after an existing key.
    ///
    /// If `new_key` already exists in the mapping, that entry is rebuilt with the
    /// new value, the old one going as with [`remove`](Self::remove), and moved
    /// after `after_key` - so the key ends up at the requested position regardless
    /// of where it was before. Passing the entry's current value, as
    /// [`get`](Self::get) returns it, moves the entry as it is, comments and
    /// anchor included. Either way the entry moves as with
    /// [`reorder_fields`](Self::reorder_fields), which says what happens to aliases.
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
    /// If `new_key` already exists in the mapping, that entry is moved before
    /// `before_key`, with the new value, as with [`move_after`](Self::move_after).
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

fn is_blank(element: &Element) -> bool {
    matches!(
        element.kind(),
        SyntaxKind::NEWLINE | SyntaxKind::WHITESPACE | SyntaxKind::INDENT
    )
}

/// Where the blank lines that start `elements` end: after the last line
/// break before anything else.
fn leading_blank_lines_end(elements: &[Element]) -> usize {
    let first = elements
        .iter()
        .position(|e| !is_blank(e))
        .unwrap_or(elements.len());
    elements[..first]
        .iter()
        .rposition(|e| e.kind() == SyntaxKind::NEWLINE)
        .map_or(0, |i| i + 1)
}
