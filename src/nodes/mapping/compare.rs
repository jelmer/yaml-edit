//! Structural CST node comparison for key equality.
//!
//! Used by insert_at_index_preserving (to detect that a newly-built
//! entry matches an existing one and should replace it) and by the
//! movement code (compare_key_nodes to locate anchor keys). Split out
//! of `nodes/mapping/mod.rs` unchanged.

use super::Mapping;
use crate::lex::SyntaxKind;
use crate::nodes::{entry_key, entry_value, Scalar, SyntaxNode};
use rowan::ast::AstNode;
use rowan::GreenNodeBuilder;

impl Mapping {
    /// Compare two key nodes structurally
    pub(crate) fn compare_key_nodes(&self, actual: &SyntaxNode, expected: &SyntaxNode) -> bool {
        // Both must be KEY nodes
        if actual.kind() != SyntaxKind::KEY || expected.kind() != SyntaxKind::KEY {
            return actual.kind() == expected.kind()
                && self.compare_nodes_structurally(actual, expected);
        }

        // Get the actual content nodes (skipping whitespace)
        let actual_content = self.get_key_content_nodes(actual);
        let expected_content = self.get_key_content_nodes(expected);

        if actual_content.len() != expected_content.len() {
            return false;
        }

        for (a, e) in actual_content.iter().zip(expected_content.iter()) {
            if !self.compare_nodes_structurally(a, e) {
                return false;
            }
        }

        true
    }

    /// Get the content nodes of a KEY, skipping whitespace and formatting
    fn get_key_content_nodes(&self, key_node: &SyntaxNode) -> Vec<SyntaxNode> {
        let mut nodes = Vec::new();
        for child in key_node.children_with_tokens() {
            match child {
                rowan::NodeOrToken::Node(n) => {
                    // Include all child nodes (sequences, mappings, etc.)
                    nodes.push(n);
                }
                rowan::NodeOrToken::Token(t) => {
                    // Include significant tokens as synthetic nodes
                    if t.kind() != SyntaxKind::WHITESPACE
                        && t.kind() != SyntaxKind::INDENT
                        && t.kind() != SyntaxKind::QUESTION
                    {
                        // Create a synthetic node for the token to enable comparison
                        let mut token_builder = GreenNodeBuilder::new();
                        token_builder.start_node(t.kind().into());
                        token_builder.token(t.kind().into(), t.text());
                        token_builder.finish_node();
                        nodes.push(SyntaxNode::new_root_mut(token_builder.finish()));
                    }
                }
            }
        }
        nodes
    }

    /// Compare nodes structurally (for complex keys like sequences and mappings)
    fn compare_nodes_structurally(&self, node1: &SyntaxNode, node2: &SyntaxNode) -> bool {
        if node1.kind() != node2.kind() {
            return false;
        }

        match node1.kind() {
            SyntaxKind::SCALAR => {
                // For SCALAR nodes, compare the semantic content (unquoted strings).
                // Kind is already confirmed so cast should not fail; use map_or(false, …)
                // as a safe fallback rather than unwrap.
                let s1 = Scalar::cast(node1.clone()).map(|s| s.as_string());
                let s2 = Scalar::cast(node2.clone()).map(|s| s.as_string());
                s1 == s2 && s1.is_some()
            }
            SyntaxKind::STRING => {
                // For string tokens, compare the actual content
                let mut iter1 = node1
                    .children_with_tokens()
                    .filter_map(|c| c.into_token())
                    .filter(|t| t.kind() == SyntaxKind::STRING);
                let mut iter2 = node2
                    .children_with_tokens()
                    .filter_map(|c| c.into_token())
                    .filter(|t| t.kind() == SyntaxKind::STRING);
                loop {
                    match (iter1.next(), iter2.next()) {
                        (Some(a), Some(b)) if a.text() == b.text() => continue,
                        (None, None) => return true,
                        _ => return false,
                    }
                }
            }
            SyntaxKind::SEQUENCE => {
                // Compare sequence entries
                let mut entries1 = node1
                    .children()
                    .filter(|n| n.kind() == SyntaxKind::SEQUENCE_ENTRY);
                let mut entries2 = node2
                    .children()
                    .filter(|n| n.kind() == SyntaxKind::SEQUENCE_ENTRY);
                loop {
                    match (entries1.next(), entries2.next()) {
                        (Some(e1), Some(e2)) => {
                            if !self.compare_sequence_entries(&e1, &e2) {
                                return false;
                            }
                        }
                        (None, None) => return true,
                        _ => return false,
                    }
                }
            }
            SyntaxKind::MAPPING => {
                // Compare mapping entries (order matters for keys)
                let mut entries1 = node1
                    .children()
                    .filter(|n| n.kind() == SyntaxKind::MAPPING_ENTRY);
                let mut entries2 = node2
                    .children()
                    .filter(|n| n.kind() == SyntaxKind::MAPPING_ENTRY);
                loop {
                    match (entries1.next(), entries2.next()) {
                        (Some(e1), Some(e2)) => {
                            if !self.compare_mapping_entries(&e1, &e2) {
                                return false;
                            }
                        }
                        (None, None) => return true,
                        _ => return false,
                    }
                }
            }
            _ => {
                // For other node types, compare token content
                let filter_tokens = |node: &SyntaxNode| {
                    node.children_with_tokens()
                        .filter_map(|c| c.into_token())
                        .filter(|t| {
                            t.kind() != SyntaxKind::WHITESPACE && t.kind() != SyntaxKind::INDENT
                        })
                };
                let mut iter1 = filter_tokens(node1);
                let mut iter2 = filter_tokens(node2);
                loop {
                    match (iter1.next(), iter2.next()) {
                        (Some(a), Some(b)) if a.kind() == b.kind() && a.text() == b.text() => {
                            continue
                        }
                        (None, None) => return true,
                        _ => return false,
                    }
                }
            }
        }
    }

    /// Compare sequence entries
    fn compare_sequence_entries(&self, entry1: &SyntaxNode, entry2: &SyntaxNode) -> bool {
        let value1 = entry_value(entry1);
        let value2 = entry_value(entry2);

        match (value1, value2) {
            (Some(v1), Some(v2)) => self.compare_nodes_structurally(&v1, &v2),
            (None, None) => true,
            _ => false,
        }
    }

    /// Compare mapping entries
    fn compare_mapping_entries(&self, entry1: &SyntaxNode, entry2: &SyntaxNode) -> bool {
        let key1 = entry_key(entry1);
        let key2 = entry_key(entry2);
        let value1 = entry_value(entry1);
        let value2 = entry_value(entry2);

        match ((key1, value1), (key2, value2)) {
            ((Some(k1), Some(v1)), (Some(k2), Some(v2))) => {
                self.compare_key_nodes(&k1, &k2) && self.compare_nodes_structurally(&v1, &v2)
            }
            ((Some(k1), None), (Some(k2), None)) => self.compare_key_nodes(&k1, &k2),
            ((None, Some(v1)), (None, Some(v2))) => self.compare_nodes_structurally(&v1, &v2),
            ((None, None), (None, None)) => true,
            _ => false,
        }
    }
}
