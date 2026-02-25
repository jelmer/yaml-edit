//! Debug utilities for inspecting and understanding YAML structures.
//!
//! This module provides debugging tools including:
//! - CST tree visualization
//! - Pretty Debug formatting showing actual values
//! - Visual diffs between documents
//! - Deep value inspection
//! - AST visualization (GraphViz output)
//!
//! These functions are useful for both contributors working on yaml-edit
//! and users debugging their YAML documents.

use crate::as_yaml::YamlNode;
use crate::lex::SyntaxKind;
use crate::yaml::{Document, Mapping, Scalar, Sequence, SyntaxNode};
use std::fmt;

/// Prints the CST (Concrete Syntax Tree) structure to stdout.
///
/// This is invaluable for understanding how YAML is parsed into the tree structure,
/// debugging formatting issues, and verifying that mutations produce the expected tree.
///
/// # Example
///
/// ```
/// use yaml_edit::{YamlFile, debug};
/// use rowan::ast::AstNode;
/// use std::str::FromStr;
///
/// let yaml = YamlFile::from_str("team:\n  - Alice\n  - Bob").unwrap();
/// debug::print_tree(yaml.syntax());
/// ```
///
/// Output:
/// ```text
/// DOCUMENT
///   MAPPING
///     MAPPING_ENTRY
///       KEY
///         SCALAR
///           STRING: "team"
///       COLON: ":"
///       VALUE
///         NEWLINE: "\n"
///         INDENT: "  "
///         SEQUENCE
///           SEQUENCE_ENTRY
///             DASH: "-"
///             WHITESPACE: " "
///             SCALAR
///               STRING: "Alice"
///             NEWLINE: "\n"
/// ...
/// ```
pub fn print_tree(node: &SyntaxNode) {
    print_tree_indent(node, 0);
}

/// Prints the CST structure with a custom starting indentation.
pub fn print_tree_indent(node: &SyntaxNode, indent: usize) {
    for child in node.children_with_tokens() {
        let prefix = "  ".repeat(indent);
        match child {
            rowan::NodeOrToken::Node(n) => {
                println!("{}{:?}", prefix, n.kind());
                print_tree_indent(&n, indent + 1);
            }
            rowan::NodeOrToken::Token(t) => {
                let text = t.text().replace('\n', "\\n").replace('\r', "\\r");
                println!("{}{:?}: {:?}", prefix, t.kind(), text);
            }
        }
    }
}

/// Returns a string representation of the CST structure.
///
/// Like `print_tree` but returns a string instead of printing to stdout.
///
/// # Example
///
/// ```
/// use yaml_edit::{YamlFile, debug};
/// use rowan::ast::AstNode;
/// use std::str::FromStr;
///
/// let yaml = YamlFile::from_str("name: Alice").unwrap();
/// let tree_str = debug::tree_to_string(yaml.syntax());
///
/// let expected = "DOCUMENT\n  MAPPING\n    MAPPING_ENTRY\n      KEY\n        SCALAR\n          STRING: \"name\"\n      COLON: \":\"\n      WHITESPACE: \" \"\n      VALUE\n        SCALAR\n          STRING: \"Alice\"\n";
/// assert_eq!(tree_str, expected);
/// ```
pub fn tree_to_string(node: &SyntaxNode) -> String {
    let mut result = String::new();
    tree_to_string_indent(node, 0, &mut result);
    result
}

fn tree_to_string_indent(node: &SyntaxNode, indent: usize, result: &mut String) {
    for child in node.children_with_tokens() {
        let prefix = "  ".repeat(indent);
        match child {
            rowan::NodeOrToken::Node(n) => {
                result.push_str(&format!("{}{:?}\n", prefix, n.kind()));
                tree_to_string_indent(&n, indent + 1, result);
            }
            rowan::NodeOrToken::Token(t) => {
                let text = t.text().replace('\n', "\\n").replace('\r', "\\r");
                result.push_str(&format!("{}{:?}: {:?}\n", prefix, t.kind(), text));
            }
        }
    }
}

/// Prints statistics about a YAML document's CST.
///
/// Shows counts of different node and token types, which can be useful
/// for understanding document complexity and debugging issues.
///
/// # Example
///
/// ```
/// use yaml_edit::{YamlFile, debug};
/// use rowan::ast::AstNode;
/// use std::str::FromStr;
///
/// let yaml = YamlFile::from_str("team:\n  - Alice\n  - Bob").unwrap();
/// debug::print_stats(yaml.syntax());
/// ```
pub fn print_stats(node: &SyntaxNode) {
    let mut node_counts = std::collections::HashMap::new();
    let mut token_counts = std::collections::HashMap::new();

    count_nodes(node, &mut node_counts, &mut token_counts);

    println!("=== Node Counts ===");
    let mut node_vec: Vec<_> = node_counts.iter().collect();
    node_vec.sort_by_key(|(_, count)| std::cmp::Reverse(**count));
    for (kind, count) in node_vec {
        println!("  {:?}: {}", kind, count);
    }

    println!("\n=== Token Counts ===");
    let mut token_vec: Vec<_> = token_counts.iter().collect();
    token_vec.sort_by_key(|(_, count)| std::cmp::Reverse(**count));
    for (kind, count) in token_vec {
        println!("  {:?}: {}", kind, count);
    }
}

fn count_nodes(
    node: &SyntaxNode,
    node_counts: &mut std::collections::HashMap<SyntaxKind, usize>,
    token_counts: &mut std::collections::HashMap<SyntaxKind, usize>,
) {
    *node_counts.entry(node.kind()).or_insert(0) += 1;

    for child in node.children_with_tokens() {
        match child {
            rowan::NodeOrToken::Node(n) => {
                count_nodes(&n, node_counts, token_counts);
            }
            rowan::NodeOrToken::Token(t) => {
                *token_counts.entry(t.kind()).or_insert(0) += 1;
            }
        }
    }
}

/// Validates that the CST follows expected structural invariants.
///
/// This is useful for testing that mutations don't break tree structure.
/// Returns `Ok(())` if the tree is valid, or an error message if issues are found.
///
/// Current checks:
/// - Every MAPPING_ENTRY has exactly one KEY
/// - Every MAPPING_ENTRY has exactly one COLON
/// - Every MAPPING_ENTRY has at most one VALUE
/// - Block-style MAPPING_ENTRY and SEQUENCE_ENTRY nodes end with NEWLINE
///
/// # Example
///
/// ```
/// use yaml_edit::{YamlFile, debug};
/// use rowan::ast::AstNode;
/// use std::str::FromStr;
///
/// let yaml = YamlFile::from_str("name: Alice\n").unwrap();
/// debug::validate_tree(yaml.syntax()).expect("Tree should be valid");
/// ```
pub fn validate_tree(node: &SyntaxNode) -> Result<(), String> {
    validate_node(node)
}

fn validate_node(node: &SyntaxNode) -> Result<(), String> {
    match node.kind() {
        SyntaxKind::MAPPING_ENTRY => {
            let keys: Vec<_> = node
                .children()
                .filter(|c| c.kind() == SyntaxKind::KEY)
                .collect();
            if keys.len() != 1 {
                return Err(format!(
                    "MAPPING_ENTRY should have exactly 1 KEY, found {}",
                    keys.len()
                ));
            }

            let colons: Vec<_> = node
                .children_with_tokens()
                .filter(|c| {
                    c.as_token()
                        .map(|t| t.kind() == SyntaxKind::COLON)
                        .unwrap_or(false)
                })
                .collect();
            if colons.len() != 1 {
                return Err(format!(
                    "MAPPING_ENTRY should have exactly 1 COLON, found {}",
                    colons.len()
                ));
            }

            let values: Vec<_> = node
                .children()
                .filter(|c| c.kind() == SyntaxKind::VALUE)
                .collect();
            if values.len() > 1 {
                return Err(format!(
                    "MAPPING_ENTRY should have at most 1 VALUE, found {}",
                    values.len()
                ));
            }

            // Check if block-style (not in flow collection)
            // Block entries should end with NEWLINE
            if !is_in_flow_collection(node) {
                if let Some(last_token) = node.last_token() {
                    if last_token.kind() != SyntaxKind::NEWLINE {
                        return Err(format!(
                            "Block-style MAPPING_ENTRY should end with NEWLINE, ends with {:?}",
                            last_token.kind()
                        ));
                    }
                }
            }
        }
        SyntaxKind::SEQUENCE_ENTRY => {
            // Check if block-style
            if !is_in_flow_collection(node) {
                if let Some(last_token) = node.last_token() {
                    if last_token.kind() != SyntaxKind::NEWLINE {
                        return Err(format!(
                            "Block-style SEQUENCE_ENTRY should end with NEWLINE, ends with {:?}",
                            last_token.kind()
                        ));
                    }
                }
            }
        }
        _ => {}
    }

    // Recursively validate children
    for child in node.children() {
        validate_node(&child)?;
    }

    Ok(())
}

fn is_in_flow_collection(node: &SyntaxNode) -> bool {
    // Look at the parent MAPPING or SEQUENCE node
    let mut current = node.parent();
    while let Some(parent) = current {
        match parent.kind() {
            SyntaxKind::MAPPING => {
                // Flow mapping has LEFT_BRACE and RIGHT_BRACE tokens
                for child in parent.children_with_tokens() {
                    if let Some(token) = child.as_token() {
                        if token.kind() == SyntaxKind::LEFT_BRACE {
                            return true;
                        }
                    }
                }
                return false;
            }
            SyntaxKind::SEQUENCE => {
                // Flow sequence has LEFT_BRACKET and RIGHT_BRACKET tokens
                for child in parent.children_with_tokens() {
                    if let Some(token) = child.as_token() {
                        if token.kind() == SyntaxKind::LEFT_BRACKET {
                            return true;
                        }
                    }
                }
                return false;
            }
            _ => current = parent.parent(),
        }
    }
    false
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::yaml::YamlFile;
    use rowan::ast::AstNode;
    use std::str::FromStr;

    #[test]
    fn test_print_tree() {
        let yaml = YamlFile::from_str("name: Alice").unwrap();
        // Should not panic
        print_tree(yaml.syntax());
    }

    #[test]
    fn test_tree_to_string() {
        let yaml = YamlFile::from_str("name: Alice").unwrap();
        let s = tree_to_string(yaml.syntax());
        let expected = "DOCUMENT\n  MAPPING\n    MAPPING_ENTRY\n      KEY\n        SCALAR\n          STRING: \"name\"\n      COLON: \":\"\n      WHITESPACE: \" \"\n      VALUE\n        SCALAR\n          STRING: \"Alice\"\n";
        assert_eq!(s, expected);
    }

    #[test]
    fn test_tree_to_string_sequence() {
        let yaml = YamlFile::from_str("- item1\n- item2\n").unwrap();
        let s = tree_to_string(yaml.syntax());
        assert_eq!(
            s,
            concat!(
                "DOCUMENT\n",
                "  SEQUENCE\n",
                "    SEQUENCE_ENTRY\n",
                "      DASH: \"-\"\n",
                "      WHITESPACE: \" \"\n",
                "      SCALAR\n",
                "        STRING: \"item1\"\n",
                "      NEWLINE: \"\\\\n\"\n",
                "    SEQUENCE_ENTRY\n",
                "      DASH: \"-\"\n",
                "      WHITESPACE: \" \"\n",
                "      SCALAR\n",
                "        STRING: \"item2\"\n",
                "      NEWLINE: \"\\\\n\"\n",
            )
        );
    }

    #[test]
    fn test_print_stats() {
        let yaml = YamlFile::from_str("name: Alice\nage: 30\n").unwrap();
        // Should not panic
        print_stats(yaml.syntax());
    }

    #[test]
    fn test_validate_tree() {
        let yaml = YamlFile::from_str("name: Alice\nage: 30\n").unwrap();
        validate_tree(yaml.syntax()).expect("Tree should be valid");
    }
}

/// Pretty-print a YAML document showing its structure and values.
///
/// This provides a human-readable view of the document structure,
/// showing both the logical YAML structure and the actual values.
///
/// # Example
///
/// ```
/// use yaml_edit::{Document, debug::PrettyDebug};
/// use std::str::FromStr;
///
/// let doc = Document::from_str("name: Alice\nage: 30").unwrap();
/// println!("{}", PrettyDebug(&doc));
/// ```
pub struct PrettyDebug<'a, T>(pub &'a T);

impl<'a> fmt::Display for PrettyDebug<'a, Document> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Document {{")?;

        if let Some(mapping) = self.0.as_mapping() {
            write_indented(f, &format!("{}", PrettyDebug(&mapping)), 1)?;
        } else if let Some(sequence) = self.0.as_sequence() {
            write_indented(f, &format!("{}", PrettyDebug(&sequence)), 1)?;
        } else if let Some(scalar) = self.0.as_scalar() {
            writeln!(f, "  {}", PrettyDebug(&scalar))?;
        }

        writeln!(f, "}}")
    }
}

impl<'a> fmt::Display for PrettyDebug<'a, Mapping> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Mapping [")?;

        for (key, value) in self.0.iter() {
            write!(f, "  {:?}: ", key)?;
            format_node(f, &value, 1)?;
            writeln!(f)?;
        }

        write!(f, "]")
    }
}

impl<'a> fmt::Display for PrettyDebug<'a, Sequence> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Sequence [")?;

        for (i, value) in self.0.values().enumerate() {
            write!(f, "  [{}]: ", i)?;
            format_node(f, &value, 1)?;
            writeln!(f)?;
        }

        write!(f, "]")
    }
}

impl<'a> fmt::Display for PrettyDebug<'a, Scalar> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Scalar({:?})", self.0.value())
    }
}

fn write_indented(f: &mut fmt::Formatter<'_>, text: &str, indent: usize) -> fmt::Result {
    let indent_str = "  ".repeat(indent);
    for line in text.lines() {
        writeln!(f, "{}{}", indent_str, line)?;
    }
    Ok(())
}

fn format_node(f: &mut fmt::Formatter<'_>, node: &YamlNode, indent: usize) -> fmt::Result {
    let indent_str = "  ".repeat(indent);

    match node {
        YamlNode::Scalar(s) => {
            let sv = crate::scalar::ScalarValue::from_scalar(s);
            write!(f, "{:?} (type: {:?})", sv.value(), sv.scalar_type())?;
        }
        YamlNode::Mapping(m) => {
            writeln!(f, "Mapping {{")?;
            for (k, v) in m.iter() {
                write!(f, "{}  ", indent_str)?;
                format_node(f, &k, 0)?;
                write!(f, ": ")?;
                format_node(f, &v, indent + 1)?;
                writeln!(f)?;
            }
            write!(f, "{}}}", indent_str)?;
        }
        YamlNode::Sequence(s) => {
            writeln!(f, "Sequence [")?;
            for (i, v) in s.values().enumerate() {
                write!(f, "{}  [{}]: ", indent_str, i)?;
                format_node(f, &v, indent + 1)?;
                writeln!(f)?;
            }
            write!(f, "{}]", indent_str)?;
        }
        YamlNode::Alias(a) => {
            write!(f, "Alias(*{})", a.name())?;
        }
        YamlNode::TaggedNode(t) => {
            write!(f, "Tagged({:?})", t.tag().unwrap_or_default())?;
        }
    }

    Ok(())
}

/// Value inspector for deep type analysis.
///
/// Provides detailed information about a YAML value including:
/// - Parsed type
/// - Raw text representation
/// - Coercion possibilities
///
/// # Example
///
/// ```
/// use yaml_edit::{YamlFile, debug::ValueInspector};
/// use std::str::FromStr;
///
/// let yaml = YamlFile::from_str("port: 8080").unwrap();
/// let doc = yaml.document().unwrap();
/// let mapping = doc.as_mapping().unwrap();
/// let value = mapping.get("port").unwrap();
///
/// println!("{}", ValueInspector(&value));
/// ```
pub struct ValueInspector<'a>(pub &'a crate::as_yaml::YamlNode);

impl<'a> fmt::Display for ValueInspector<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Value Inspector:")?;
        writeln!(f, "===============")?;

        match self.0 {
            crate::as_yaml::YamlNode::Scalar(s) => {
                writeln!(f, "Type: Scalar")?;
                writeln!(f, "  Value: {:?}", s.as_string())?;

                writeln!(f, "\nCoercion Capabilities:")?;
                writeln!(f, "  to_i64: {:?}", self.0.to_i64())?;
                writeln!(f, "  to_f64: {:?}", self.0.to_f64())?;
                writeln!(f, "  to_bool: {:?}", self.0.to_bool())?;
            }
            crate::as_yaml::YamlNode::Mapping(m) => {
                writeln!(f, "Type: Mapping")?;
                writeln!(f, "  Size: {} entries", m.len())?;
            }
            crate::as_yaml::YamlNode::Sequence(s) => {
                writeln!(f, "Type: Sequence")?;
                writeln!(f, "  Length: {} items", s.len())?;
            }
            crate::as_yaml::YamlNode::Alias(a) => {
                writeln!(f, "Type: Alias")?;
                writeln!(f, "  Anchor name: {}", a.name())?;
            }
            crate::as_yaml::YamlNode::TaggedNode(_) => {
                writeln!(f, "Type: TaggedNode")?;
            }
        }

        Ok(())
    }
}

/// Visual diff between two YAML documents.
///
/// Shows additions, deletions, and modifications between two versions.
///
/// # Example
///
/// ```
/// use yaml_edit::{Document, debug::VisualDiff};
/// use std::str::FromStr;
///
/// let before = Document::from_str("name: Alice\nage: 30").unwrap();
/// let after = Document::from_str("name: Bob\nage: 30").unwrap();
///
/// println!("{}", VisualDiff {
///     before: &before,
///     after: &after,
/// });
/// ```
pub struct VisualDiff<'a> {
    /// The original document
    pub before: &'a Document,
    /// The modified document
    pub after: &'a Document,
}

impl<'a> fmt::Display for VisualDiff<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Visual Diff:")?;
        writeln!(f, "============")?;

        writeln!(f, "\nBefore:")?;
        writeln!(f, "-------")?;
        for line in self.before.to_string().lines() {
            writeln!(f, "  {}", line)?;
        }

        writeln!(f, "\nAfter:")?;
        writeln!(f, "------")?;
        for line in self.after.to_string().lines() {
            writeln!(f, "  {}", line)?;
        }

        writeln!(f, "\nChanges:")?;
        writeln!(f, "--------")?;

        // Simple line-based diff
        let before_str = self.before.to_string();
        let after_str = self.after.to_string();
        let before_lines: Vec<_> = before_str.lines().collect();
        let after_lines: Vec<_> = after_str.lines().collect();

        for (i, (b, a)) in before_lines.iter().zip(after_lines.iter()).enumerate() {
            if b != a {
                writeln!(f, "Line {}: {:?} -> {:?}", i + 1, b, a)?;
            }
        }

        if before_lines.len() > after_lines.len() {
            writeln!(
                f,
                "Removed {} lines",
                before_lines.len() - after_lines.len()
            )?;
        } else if after_lines.len() > before_lines.len() {
            writeln!(f, "Added {} lines", after_lines.len() - before_lines.len())?;
        }

        Ok(())
    }
}

/// Generate GraphViz DOT format visualization of the CST.
///
/// This creates a visual graph representation suitable for rendering
/// with GraphViz tools like `dot`.
///
/// # Example
///
/// ```
/// use yaml_edit::{YamlFile, debug::graphviz_dot};
/// use rowan::ast::AstNode;
/// use std::str::FromStr;
///
/// let yaml = YamlFile::from_str("name: Alice").unwrap();
/// let dot = graphviz_dot(yaml.syntax());
/// // Save to file and render with: dot -Tpng output.dot -o output.png
/// ```
pub fn graphviz_dot(node: &SyntaxNode) -> String {
    let mut result = String::from("digraph CST {\n");
    result.push_str("  node [shape=box, fontname=\"Courier\"];\n");
    result.push_str("  edge [fontsize=10];\n\n");

    let mut counter = 0;
    graphviz_node(&mut result, node, None, &mut counter);

    result.push_str("}\n");
    result
}

fn graphviz_node(
    result: &mut String,
    node: &SyntaxNode,
    parent_id: Option<usize>,
    counter: &mut usize,
) {
    let node_id = *counter;
    *counter += 1;

    // Create node
    let label = format!("{:?}", node.kind());
    result.push_str(&format!("  n{} [label=\"{}\"];\n", node_id, label));

    // Link to parent
    if let Some(pid) = parent_id {
        result.push_str(&format!("  n{} -> n{};\n", pid, node_id));
    }

    // Process children
    for child in node.children_with_tokens() {
        match child {
            rowan::NodeOrToken::Node(n) => {
                graphviz_node(result, &n, Some(node_id), counter);
            }
            rowan::NodeOrToken::Token(t) => {
                let token_id = *counter;
                *counter += 1;

                let text = t
                    .text()
                    .replace('\\', "\\\\")
                    .replace('"', "\\\"")
                    .replace('\n', "\\\\n")
                    .replace('\r', "\\\\r");
                let label = format!("{:?}\\n{:?}", t.kind(), text);
                result.push_str(&format!(
                    "  n{} [label=\"{}\", shape=ellipse, style=filled, fillcolor=lightgray];\n",
                    token_id, label
                ));
                result.push_str(&format!("  n{} -> n{};\n", node_id, token_id));
            }
        }
    }
}

#[cfg(test)]
mod new_debug_tests {
    use super::*;
    use crate::yaml::YamlFile;
    use rowan::ast::AstNode;
    use std::str::FromStr;

    #[test]
    fn test_pretty_debug_document() {
        let doc = Document::from_str("name: Alice\nage: 30").unwrap();
        let output = format!("{}", PrettyDebug(&doc));

        // Check structure exists (exact formatting may vary)
        assert_eq!(output.lines().next(), Some("Document {"));
        assert_eq!(output.lines().last(), Some("}"));
    }

    #[test]
    fn test_value_inspector_scalar() {
        let yaml = YamlFile::from_str("port: 8080").unwrap();
        let doc = yaml.document().unwrap();
        let mapping = doc.as_mapping().unwrap();
        let value = mapping.get("port").unwrap();

        let output = format!("{}", ValueInspector(&value));

        assert_eq!(output.lines().next(), Some("Value Inspector:"));
        assert_eq!(output.lines().nth(1), Some("==============="));
        assert_eq!(output.lines().nth(2), Some("Type: Scalar"));
    }

    #[test]
    fn test_visual_diff_simple() {
        let before = Document::from_str("name: Alice").unwrap();
        let after = Document::from_str("name: Bob").unwrap();

        let diff = VisualDiff {
            before: &before,
            after: &after,
        };

        let output = format!("{}", diff);

        assert_eq!(output.lines().next(), Some("Visual Diff:"));
        assert_eq!(output.lines().nth(1), Some("============"));
    }

    #[test]
    fn test_graphviz_dot_structure() {
        let yaml = YamlFile::from_str("name: Alice").unwrap();
        let dot = graphviz_dot(yaml.syntax());

        assert_eq!(dot.lines().next(), Some("digraph CST {"));
        assert_eq!(dot.lines().last(), Some("}"));
    }

    #[test]
    fn test_graphviz_dot_contains_nodes() {
        let yaml = YamlFile::from_str("key: value").unwrap();
        let dot = graphviz_dot(yaml.syntax());

        // Should have node declarations (at least one "node" line in the DOT output)
        let node_count = dot.lines().filter(|l| l.trim().starts_with("node")).count();
        assert!(
            node_count >= 1,
            "expected at least one node declaration in dot output"
        );
    }
}
