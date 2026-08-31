//! `Mapping::move_after` / `move_before` -- pull a key out of its
//! current position and re-insert it at a new position, along with
//! their shared `move_flow_around` helper for flow-style mappings.
//!
//! Split out of `nodes/mapping/mod.rs` unchanged.

use super::helpers::FlowInsertPos;
use super::{key_content_matches, Mapping, MappingEntry};
use crate::lex::SyntaxKind;
use crate::nodes::SyntaxNode;
use crate::yaml::Document;
use rowan::ast::AstNode;
use rowan::GreenNodeBuilder;

impl Mapping {
    /// Move-or-insert `new_key: new_value` relative to `ref_key` in a
    /// flow-style mapping. Shared body of the flow branches in
    /// `move_before_impl` / `move_after_impl`. Returns `false` if
    /// `ref_key` is not present (nothing to anchor on).
    fn move_flow_around(
        &self,
        ref_key: &impl crate::AsYaml,
        new_key: &impl crate::AsYaml,
        new_value: &impl crate::AsYaml,
        where_at: fn(SyntaxNode) -> FlowInsertPos,
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
        // Contract: move, not duplicate. Drop any prior entry with the
        // same key first.
        if let Some(existing) = self.find_entry_by_key(new_key) {
            if let Some(idx) = self
                .0
                .children_with_tokens()
                .position(|c| c.as_node() == Some(existing.syntax()))
            {
                self.0.splice_children(idx..idx + 1, vec![]);
            }
        }
        let entry = MappingEntry::new_at_indent(
            new_key,
            new_value,
            /* flow_context */ true,
            self.uses_explicit_keys(),
            self.detect_indentation_level(),
        );
        self.insert_flow_entry_cst_at(&entry.0, where_at(target.syntax().clone()));
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
        self.move_after_impl(after_key, new_key, new_value)
    }

    /// Internal implementation for move_after
    fn move_after_impl(
        &self,
        after_key: impl crate::AsYaml,
        new_key: impl crate::AsYaml,
        new_value: impl crate::AsYaml,
    ) -> bool {
        let Some(target) = self.find_entry_by_key(&after_key) else {
            return false;
        };
        if self
            .find_entry_by_key(&new_key)
            .is_some_and(|e| e.syntax() == target.syntax())
        {
            self.set(&new_key, &new_value);
            return true;
        }
        if self.is_flow_style() {
            return self.move_flow_around(&after_key, &new_key, &new_value, FlowInsertPos::After);
        }

        let children: Vec<_> = self.0.children_with_tokens().collect();
        let mut insert_position = None;
        let mut found_key = false;
        let mut last_value_end = 0;

        // First, check if the new key already exists and remove it
        let mut i = 0;
        let mut removed_existing = false;
        while i < children.len() {
            if let Some(node) = children[i].as_node() {
                if node.kind() == SyntaxKind::MAPPING_ENTRY {
                    // Look inside the MAPPING_ENTRY for the KEY
                    for key_child in node.children() {
                        if key_child.kind() == SyntaxKind::KEY
                            && key_content_matches(&key_child, &new_key)
                        {
                            // Found existing key, remove this entire MAPPING_ENTRY
                            let mut remove_range = i..i + 1;

                            // Also remove any trailing newline
                            if i + 1 < children.len() {
                                if let Some(token) = children[i + 1].as_token() {
                                    if token.kind() == SyntaxKind::NEWLINE {
                                        remove_range = i..i + 2;
                                    }
                                }
                            }

                            self.0.splice_children(remove_range, vec![]);
                            removed_existing = true;
                            break;
                        }
                    }
                    if removed_existing {
                        // Need to refresh children list after removal
                        break;
                    }
                }
            }
            if !removed_existing {
                i += 1;
            }
        }

        // If we removed an existing key, refresh the children list
        let children = if removed_existing {
            self.0.children_with_tokens().collect()
        } else {
            children
        };

        // Find the position after the specified key's value
        for (i, child) in children.iter().enumerate() {
            if let Some(node) = child.as_node() {
                if node.kind() == SyntaxKind::MAPPING_ENTRY {
                    if found_key {
                        // Check if this MAPPING_ENTRY is at the root level
                        // Root level means it's not preceded by INDENT
                        let is_root_level = if i > 0 {
                            children
                                .get(i - 1)
                                .and_then(|c| c.as_token())
                                .map_or(true, |t| t.kind() != SyntaxKind::INDENT)
                        } else {
                            true
                        };

                        if is_root_level {
                            insert_position = Some(i);
                            break;
                        }
                    }
                    // Look inside the MAPPING_ENTRY for the KEY
                    for key_child in node.children() {
                        if key_child.kind() == SyntaxKind::KEY
                            && key_content_matches(&key_child, &after_key)
                        {
                            found_key = true;
                            last_value_end = i + 1; // After this entire MAPPING_ENTRY
                            break;
                        }
                    }
                } else if node.kind() == SyntaxKind::KEY {
                    if key_content_matches(node, &after_key) {
                        found_key = true;
                    }
                } else if node.kind() == SyntaxKind::SCALAR {
                    // For SCALAR nodes that might be keys
                    if key_content_matches(node, &after_key) && !found_key {
                        // This is likely the key we're looking for
                        found_key = true;
                        // Look ahead for the value
                        for (j, child_j) in children[(i + 1)..].iter().enumerate() {
                            if let Some(n) = child_j.as_node() {
                                if n.kind() == SyntaxKind::VALUE || n.kind() == SyntaxKind::SCALAR {
                                    last_value_end = i + 1 + j + 1;
                                    break;
                                }
                            }
                        }
                    }
                } else if node.kind() == SyntaxKind::VALUE && found_key {
                    // We're at the value of the found key
                    last_value_end = i + 1;
                }
            } else if let Some(token) = child.as_token() {
                if found_key && token.kind() == SyntaxKind::COMMENT {
                    // Check if this comment is at the top level (not indented)
                    // Top-level comments can be preceded by:
                    // 1. NEWLINE token (traditional case)
                    // 2. MAPPING_ENTRY node (when all newlines are inside the entry)
                    if i > 0 {
                        if let Some(prev) = children.get(i - 1) {
                            let is_top_level = if let Some(prev_token) = prev.as_token() {
                                // Preceded by token - check if it's NEWLINE (not INDENT)
                                prev_token.kind() == SyntaxKind::NEWLINE
                            } else if let Some(prev_node) = prev.as_node() {
                                // Preceded by node - check if it's a MAPPING_ENTRY
                                // (means all newlines were inside the entry)
                                prev_node.kind() == SyntaxKind::MAPPING_ENTRY
                            } else {
                                false
                            };

                            if is_top_level {
                                // Top-level comment - insert before it
                                insert_position = Some(i);
                                break;
                            }
                        }
                    }
                } else if found_key && token.kind() == SyntaxKind::NEWLINE {
                    // Check if this is a root-level newline (not inside nested content)
                    // Root-level means not preceded by INDENT
                    let is_root_level = if i > 0 {
                        children
                            .get(i - 1)
                            .and_then(|c| c.as_token())
                            .map_or(true, |t| t.kind() != SyntaxKind::INDENT)
                    } else {
                        true
                    };

                    if is_root_level && i + 1 < children.len() {
                        if let Some(next) = children.get(i + 1) {
                            if let Some(next_token) = next.as_token() {
                                if next_token.kind() == SyntaxKind::NEWLINE
                                    || next_token.kind() == SyntaxKind::COMMENT
                                {
                                    // Blank line or comment follows - insert before
                                    // them so the new entry is right after the target
                                    // key and blank lines are preserved before the
                                    // next key
                                    insert_position = Some(i);
                                    break;
                                }
                            } else if next.as_node().is_some() {
                                // Node follows (likely MAPPING_ENTRY) - insert before
                                // this separator newline so the new entry is right
                                // after the target key
                                insert_position = Some(i);
                                break;
                            }
                        }
                    }
                }
            }
        }

        // If we didn't find a newline but found the key, insert after the value
        if insert_position.is_none() && found_key && last_value_end > 0 {
            insert_position = Some(last_value_end);
        }

        if let Some(pos) = insert_position {
            // Create new elements for the key-value pair
            let mut new_elements = Vec::new();

            // Check if the previous entry has a trailing newline and add one if needed
            if pos > 0 {
                // Look backwards for the last MAPPING_ENTRY
                if let Some(prev_entry) = children[..pos].iter().rev().find_map(|child| {
                    child
                        .as_node()
                        .filter(|n| n.kind() == SyntaxKind::MAPPING_ENTRY)
                }) {
                    // Check if it ends with NEWLINE
                    let has_newline = prev_entry
                        .last_token()
                        .is_some_and(|t| t.kind() == SyntaxKind::NEWLINE);

                    // If not, add one to the previous entry (not to the mapping)
                    if !has_newline {
                        let entry_children_count = prev_entry.children_with_tokens().count();
                        let mut nl_builder = GreenNodeBuilder::new();
                        nl_builder.start_node(SyntaxKind::ROOT.into());
                        nl_builder.token(SyntaxKind::NEWLINE.into(), "\n");
                        nl_builder.finish_node();
                        let nl_node = SyntaxNode::new_root_mut(nl_builder.finish());
                        if let Some(token) = nl_node.first_token() {
                            prev_entry.splice_children(
                                entry_children_count..entry_children_count,
                                vec![token.into()],
                            );
                        }
                    }
                }
            }

            // Add indentation if needed
            // Check if we're inserting at root level by looking at the previous element
            let needs_indent = pos > 0
                && children
                    .get(pos - 1)
                    .and_then(|c| c.as_token())
                    .is_some_and(|t| t.kind() == SyntaxKind::INDENT);

            if needs_indent {
                let indent_level = self.detect_indentation_level();
                if indent_level > 0 {
                    let mut indent_builder = GreenNodeBuilder::new();
                    indent_builder.start_node(SyntaxKind::ROOT.into());
                    indent_builder.token(SyntaxKind::INDENT.into(), &" ".repeat(indent_level));
                    indent_builder.finish_node();
                    let indent_node = SyntaxNode::new_root_mut(indent_builder.finish());
                    if let Some(token) = indent_node.first_token() {
                        new_elements.push(token.into());
                    }
                }
            }

            // Create the MAPPING_ENTRY node
            let (entry, _has_trailing_newline) = self.create_mapping_entry(&new_key, &new_value);

            // Add the new entry (which already has its own trailing newline)
            new_elements.push(entry.into());

            // Splice in the new elements
            self.0.splice_children(pos..pos, new_elements);
            true
        } else {
            false
        }
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
        self.move_before_impl(before_key, new_key, new_value)
    }

    /// Internal implementation for move_before
    fn move_before_impl(
        &self,
        before_key: impl crate::AsYaml,
        new_key: impl crate::AsYaml,
        new_value: impl crate::AsYaml,
    ) -> bool {
        let Some(target) = self.find_entry_by_key(&before_key) else {
            return false;
        };
        if self
            .find_entry_by_key(&new_key)
            .is_some_and(|e| e.syntax() == target.syntax())
        {
            self.set(&new_key, &new_value);
            return true;
        }
        if self.is_flow_style() {
            return self.move_flow_around(&before_key, &new_key, &new_value, FlowInsertPos::Before);
        }

        let children: Vec<_> = self.0.children_with_tokens().collect();
        let mut insert_position = None;

        // First, check if the new key already exists and remove it
        let mut i = 0;
        let mut removed_existing = false;
        while i < children.len() {
            if let Some(node) = children[i].as_node() {
                if node.kind() == SyntaxKind::MAPPING_ENTRY {
                    // Look inside the MAPPING_ENTRY for the KEY
                    for key_child in node.children() {
                        if key_child.kind() == SyntaxKind::KEY
                            && key_content_matches(&key_child, &new_key)
                        {
                            // Found existing key, remove this entire MAPPING_ENTRY
                            let mut remove_range = i..i + 1;

                            // Also remove any trailing newline
                            if i + 1 < children.len() {
                                if let Some(token) = children[i + 1].as_token() {
                                    if token.kind() == SyntaxKind::NEWLINE {
                                        remove_range = i..i + 2;
                                    }
                                }
                            }

                            self.0.splice_children(remove_range, vec![]);
                            removed_existing = true;
                            break;
                        }
                    }
                    if removed_existing {
                        // Need to refresh children list after removal
                        break;
                    }
                } else if (node.kind() == SyntaxKind::KEY || node.kind() == SyntaxKind::SCALAR)
                    && key_content_matches(node, &new_key)
                {
                    // Found existing key, find its VALUE node and replace just that
                    // Look for colon, then VALUE node
                    for (offset, child_j) in children[(i + 1)..].iter().enumerate() {
                        if let Some(node) = child_j.as_node() {
                            if node.kind() == SyntaxKind::VALUE {
                                // Found the VALUE node to replace
                                // Build new VALUE node using the helper
                                let mut value_builder = GreenNodeBuilder::new();
                                Document::build_value_content(&mut value_builder, &new_value, 2);
                                let new_value_node =
                                    SyntaxNode::new_root_mut(value_builder.finish());

                                // Replace just the VALUE node
                                let j = i + 1 + offset;
                                self.0
                                    .splice_children(j..j + 1, vec![new_value_node.into()]);
                                return true;
                            }
                        }
                    }
                    // If no VALUE node found, something's wrong with the structure
                    return false;
                }
            }
            if !removed_existing {
                i += 1;
            }
        }

        // If we removed an existing key, refresh the children list
        let children = if removed_existing {
            self.0.children_with_tokens().collect()
        } else {
            children
        };

        // Find the position before the specified key
        for (i, child) in children.iter().enumerate() {
            if let Some(node) = child.as_node() {
                if node.kind() == SyntaxKind::MAPPING_ENTRY {
                    // Look inside the MAPPING_ENTRY for the KEY
                    for key_child in node.children() {
                        if key_child.kind() == SyntaxKind::KEY
                            && key_content_matches(&key_child, &before_key)
                        {
                            // Found the key, insert before this MAPPING_ENTRY
                            let mut line_start = i;
                            for j in (0..i).rev() {
                                if let Some(token) = children[j].as_token() {
                                    if token.kind() == SyntaxKind::NEWLINE {
                                        line_start = j + 1;
                                        break;
                                    }
                                }
                            }
                            insert_position = Some(line_start);
                            break;
                        }
                    }
                } else if (node.kind() == SyntaxKind::KEY || node.kind() == SyntaxKind::SCALAR)
                    && key_content_matches(node, &before_key)
                {
                    // Found the key, insert before it
                    // Look back to find the start of this line
                    let mut line_start = i;
                    for j in (0..i).rev() {
                        if let Some(token) = children[j].as_token() {
                            if token.kind() == SyntaxKind::NEWLINE {
                                line_start = j + 1;
                                break;
                            }
                        }
                    }
                    insert_position = Some(line_start);
                    break;
                }
            }
        }

        if let Some(pos) = insert_position {
            // Create new AST elements for the key-value pair
            // Build the complete key-value entry as separate nodes/tokens

            // Build each element as a SyntaxNode/Token
            let mut new_elements = Vec::new();

            // Create the MAPPING_ENTRY node
            let (entry, _has_trailing_newline) = self.create_mapping_entry(&new_key, &new_value);
            new_elements.push(entry.into());

            // Note: create_mapping_entry already adds a trailing newline to the MAPPING_ENTRY
            // (newline ownership model), so we don't add an extra one here

            // Splice in the new elements
            self.0.splice_children(pos..pos, new_elements);
            true
        } else {
            false
        }
    }
}
