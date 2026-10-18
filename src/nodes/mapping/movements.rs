//! Moving entries around a mapping: `Mapping::reorder_fields`,
//! `move_after` and `move_before`.

use super::{Mapping, MappingEntry};
use crate::lex::SyntaxKind;
use crate::nodes::trivia::Slot;
use crate::nodes::trivia::{self, line_end, FlowSeparator};
use crate::nodes::{fresh_token, Lang, SyntaxNode, SyntaxToken};
use rowan::ast::AstNode;

type Element = rowan::SyntaxElement<Lang>;

/// An entry being moved, with what goes along with it.
struct Group {
    /// The comment lines before it, with any blank lines among and after
    /// them.
    lines_before: Vec<Element>,
    entry: SyntaxNode,
    /// The rest of its line that the parser put beside it, and in a flow
    /// mapping anything else up to the next entry.
    line: Vec<Element>,
}

/// The layout that belongs to a position in a mapping rather than to the
/// entry at it, so that it stays put when entries are moved around.
struct SlotLayout {
    /// For each entry of a flow mapping, the separator it ends with, if
    /// that can be swapped for another entry's.
    flow_separators: Vec<Option<FlowSeparator>>,
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

        // What moves with each entry, and for each position the blank lines
        // after it and the INDENT starting the next entry's line.
        let mut groups: Vec<Group> = Vec::new();
        let mut gaps: Vec<Vec<Element>> = Vec::new();
        let mut line_indents = Vec::new();
        let mut trailing = Vec::new();
        let mut lines_before = Vec::new();
        for (i, &at) in starts.iter().enumerate() {
            let entry = all[at].as_node().expect("an entry is a node");
            let line_end = trivia::index_after_line(entry);
            groups.push(Group {
                lines_before: std::mem::take(&mut lines_before),
                entry: entry.clone(),
                line: all[at + 1..line_end].to_vec(),
            });
            let Some(&next) = starts.get(i + 1) else {
                trailing = all[line_end..].to_vec();
                let gap_end = if flow {
                    0
                } else {
                    leading_blank_lines_end(&trailing)
                };
                gaps.push(trailing.drain(..gap_end).collect());
                break;
            };
            let mut between = all[line_end..next].to_vec();
            let indent = between
                .last()
                .is_some_and(|c| c.kind() == SyntaxKind::INDENT)
                .then(|| between.pop())
                .flatten();
            line_indents.push(indent);
            if flow {
                // TODO: the parser puts the comment lines between the
                // entries of a flow mapping in the entry above, and they go
                // with that rather than with the entry below.
                groups.last_mut().expect("just pushed").line.extend(between);
                gaps.push(Vec::new());
            } else {
                lines_before = between.split_off(leading_blank_lines_end(&between));
                gaps.push(between);
            }
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
            .map(|group| {
                group.line.iter().all(is_blank)
                    && crate::nodes::ending_block_scalar(&group.entry).is_some()
            })
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
        let mut new_children = all[..first].to_vec();
        let last_gap = gaps.pop().expect("there is an entry");
        for (i, group) in ordered.into_iter().enumerate() {
            let indent = match i.checked_sub(1) {
                Some(i) => {
                    new_children.append(&mut gaps[i]);
                    line_indents[i].clone()
                }
                // The first entry's line is indented outside the mapping,
                // unless lines go before it.
                None if lines_lead => line_indents[0]
                    .as_ref()
                    .and_then(|c| c.as_token())
                    .map(|t| fresh_token(t.kind(), t.text()).into()),
                None => None,
            };
            new_children.extend(group.lines_before);
            new_children.extend(indent);
            new_children.push(group.entry.into());
            new_children.extend(group.line);
        }
        new_children.extend(last_gap);
        new_children.extend(trailing);

        crate::nodes::kept_aliases::keeping_aliases_moved(&self.0, None, || {
            crate::nodes::replace_children(&self.0, 0..all.len(), new_children);
            if lines_lead {
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
        let texts = |tokens: Vec<SyntaxToken>| {
            tokens
                .iter()
                .map(|t| (t.kind(), t.text().to_string()))
                .collect()
        };
        let (flow_separators, flow_line_indents) = if self.is_flow_style() {
            entries
                .iter()
                .map(|e| (FlowSeparator::of(e), flow_line_indent(e).map(texts)))
                .unzip()
        } else {
            (Vec::new(), Vec::new())
        };
        let last = entries.last();
        SlotLayout {
            flow_separators,
            flow_line_indents,
            last_has_comma: last.is_some_and(trivia::has_comma),
            last_terminated: last.map_or(true, |e| line_end(e).is_terminated()),
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
        if let Some(existing) = existing {
            // Given its own value, the entry moves as it is, comments and
            // anchor included, as reorder_fields would move it. Given
            // another, it is replaced first, its old value going as with
            // remove, so that its key moves the same way.
            let value = existing.value().and_then(|v| v.children().next());
            let own_value = value.is_some() && value.as_ref() == new_value.as_node();
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
