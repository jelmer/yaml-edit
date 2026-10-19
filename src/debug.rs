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
use crate::nodes::has_child_token;
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
    let mut out = String::new();
    tree_to_string_indent(node, indent, &mut out, false);
    print!("{out}");
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
    tree_to_string_indent(node, 0, &mut result, false);
    result
}

/// Like [`tree_to_string`], but without the zero-width marker that an
/// empty `{}` or `[]` carries when it is to become a block collection once
/// it has an entry (see [`Mapping::new_pending_block`]). Re-parsing the
/// text cannot recover the marker, so this is what to compare between an
/// edited tree and a re-parse of its text.
///
/// [`Mapping::new_pending_block`]: crate::Mapping::new_pending_block
pub fn shape_to_string(node: &SyntaxNode) -> String {
    let mut result = String::new();
    tree_to_string_indent(node, 0, &mut result, true);
    result
}

fn is_pending_block_marker(token: &rowan::SyntaxToken<crate::yaml::Lang>) -> bool {
    token.kind() == SyntaxKind::WHITESPACE
        && token.text().is_empty()
        && token
            .parent()
            .is_some_and(|p| matches!(p.kind(), SyntaxKind::MAPPING | SyntaxKind::SEQUENCE))
}

/// Write `node`'s children to `result`, leaving out pending-block markers
/// for a shape.
fn tree_to_string_indent(node: &SyntaxNode, indent: usize, result: &mut String, shape: bool) {
    let prefix = "  ".repeat(indent);
    for child in node.children_with_tokens() {
        match child {
            rowan::NodeOrToken::Node(n) => {
                result.push_str(&format!("{}{:?}\n", prefix, n.kind()));
                tree_to_string_indent(&n, indent + 1, result, shape);
            }
            rowan::NodeOrToken::Token(t) if shape && is_pending_block_marker(&t) => {}
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
        println!("  {kind:?}: {count}");
    }

    println!("\n=== Token Counts ===");
    let mut token_vec: Vec<_> = token_counts.iter().collect();
    token_vec.sort_by_key(|(_, count)| std::cmp::Reverse(**count));
    for (kind, count) in token_vec {
        println!("  {kind:?}: {count}");
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
/// - Every MAPPING_ENTRY has exactly one COLON (zero allowed for
///   explicit-key entries with implicit-null values)
/// - Every MAPPING_ENTRY has at most one VALUE
/// - Block-style MAPPING_ENTRY and SEQUENCE_ENTRY nodes are terminated
///   by a NEWLINE -- either as their own last token, or via a trailing
///   comment/newline pair at the parent level, or (for MAPPING_ENTRYs)
///   an implicit-null value
/// - No two adjacent INDENT tokens (would render as doubled indentation)
/// - No two adjacent NEWLINE tokens at the tail of a MAPPING_ENTRY (would
///   render as a stray blank line)
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
    let node = &crate::nodes::immutable_copy(node);
    // Each pass walks the tree iteratively, so deep nesting cannot overflow
    // the stack.
    node.descendants().try_for_each(|n| validate_node(&n))?;
    node.descendants()
        .try_for_each(|n| validate_no_stacked_indents(&n))?;
    node.descendants()
        .try_for_each(|n| validate_no_double_trailing_newline(&n))
}

/// Roundtrip check: `parse(node.to_string()).to_string()` must equal
/// `node.to_string()`, and the re-parse must have no errors.
///
/// This catches classes of bugs where a mutation produces text that
/// looks acceptable but re-parses into a different structure (e.g.
/// missing/extra indentation that changes nesting).
///
/// Note: this shares our parser's blindspots. If a mutation produces
/// a text that our parser reads back into the same shape as the
/// pre-mutation CST, the check passes even when a different YAML
/// parser would disagree. It is *not* a differential test against an
/// oracle parser.
pub fn roundtrip_ok(node: &SyntaxNode) -> Result<(), String> {
    let text = node.to_string();
    let parse = crate::Parse::parse_yaml(&text);
    if !parse.positioned_errors().is_empty() {
        return Err(format!(
            "re-parse produced {} error(s); first: {}",
            parse.positioned_errors().len(),
            parse.positioned_errors()[0].message,
        ));
    }
    let round = parse.tree().to_string();
    if round != text {
        return Err(format!(
            "roundtrip mismatch:\n  original: {text:?}\n  reparsed: {round:?}",
        ));
    }
    Ok(())
}

/// The string under which [`YamlValue`](crate::YamlValue) holds `key` as a
/// mapping key, or `None` if `key` holds an alias, which cannot be
/// followed here.
///
/// ```
/// use yaml_edit::{debug, Document};
/// use std::str::FromStr;
///
/// let doc = Document::from_str("0x10: a\n{y: 1, x: 2}: b\n").unwrap();
/// let keys: Vec<_> = doc.as_mapping().unwrap().keys().collect();
/// assert_eq!(debug::key_string(&keys[0]), Some("16".to_string()));
/// assert_eq!(debug::key_string(&keys[1]), Some("{x: 2, y: 1}".to_string()));
/// ```
pub fn key_string(key: &YamlNode) -> Option<String> {
    let identity = crate::as_yaml::node_identity(key.syntax(), None).ok()??;
    Some(identity.key_string())
}

/// Panic unless `node` passes [`validate_tree`] and [`roundtrip_ok`].
///
/// Unit-test counterpart of the `assert_cst_ok` helper in `tests/common`;
/// call it on the root after every mutation.
#[cfg(test)]
#[track_caller]
pub(crate) fn assert_cst_ok(node: &impl rowan::ast::AstNode<Language = crate::yaml::Lang>) {
    let syntax = node.syntax();
    if let Err(e) = validate_tree(syntax) {
        panic!("CST invariant violated: {e}\n---\n{syntax}\n---");
    }
    if let Err(e) = roundtrip_ok(syntax) {
        panic!("roundtrip failed: {e}\n---\n{syntax}\n---");
    }
}

/// Panic unless `yaml` reads `expected`, has the tree a parse of that text
/// gives, and passes the validator.
#[cfg(test)]
#[track_caller]
pub(crate) fn assert_edited(yaml: &crate::yaml::YamlFile, expected: &str) {
    use rowan::ast::AstNode;
    use std::str::FromStr;
    assert_eq!(yaml.to_string(), expected);
    let reparsed = crate::yaml::YamlFile::from_str(expected).unwrap();
    assert_eq!(
        shape_to_string(yaml.syntax()),
        shape_to_string(reparsed.syntax())
    );
    let violations: Vec<String> = yaml
        .documents()
        .flat_map(|doc| crate::validator::Validator::new().validate(&doc))
        .map(|v| v.to_string())
        .collect();
    assert_eq!(violations, Vec::<String>::new());
}

/// Panic unless the tree holding `node` is the one the parser builds for
/// its text.
#[cfg(test)]
#[track_caller]
pub(crate) fn assert_parser_shaped(node: &impl rowan::ast::AstNode<Language = crate::yaml::Lang>) {
    use rowan::ast::AstNode;
    let root = node
        .syntax()
        .ancestors()
        .last()
        .expect("a node is its own ancestor");
    let text = root.to_string();
    let parsed = crate::Parse::parse_yaml(&text).tree();
    let reparsed = parsed
        .syntax()
        .descendants()
        .find(|n| n.kind() == root.kind())
        .unwrap_or_else(|| panic!("re-parse of {text:?} has no {:?}", root.kind()));
    assert_eq!(
        shape_to_string(&root),
        shape_to_string(&reparsed),
        "{text:?}"
    );
}

fn validate_node(node: &SyntaxNode) -> Result<(), String> {
    if matches!(
        node.kind(),
        SyntaxKind::MAPPING_ENTRY | SyntaxKind::SEQUENCE_ENTRY
    ) && crate::nodes::in_flow_collection(node)
    {
        validate_flow_entry_end(node)?;
    }
    validate_children(node)?;
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

            // A MAPPING_ENTRY normally has exactly one COLON. Two forms
            // may lack it:
            //   - Explicit-key entries (`? key\n`) with implicit-null value.
            //   - Flow shorthand entries (`{key}`, `{,}`) where the entire
            //     entry is implicit key: implicit-value with no COLON.
            let colons: Vec<_> = node
                .children_with_tokens()
                .filter(|c| c.as_token().is_some_and(|t| t.kind() == SyntaxKind::COLON))
                .collect();
            let is_explicit_key = has_child_token(node, |k| k == SyntaxKind::QUESTION);
            let in_flow = crate::nodes::in_flow_collection(node);
            let expected_colons = if is_explicit_key || in_flow {
                0..=1
            } else {
                1..=1
            };
            if !expected_colons.contains(&colons.len()) {
                return Err(format!(
                    "MAPPING_ENTRY should have {} COLON(s), found {}",
                    if is_explicit_key || in_flow {
                        "0 or 1"
                    } else {
                        "exactly 1"
                    },
                    colons.len()
                ));
            }

            let values: Vec<_> = node
                .children()
                .filter(|c| c.kind() == SyntaxKind::VALUE)
                .collect();
            if values.len() != 1 {
                return Err(format!(
                    "MAPPING_ENTRY should have exactly 1 VALUE, found {}",
                    values.len()
                ));
            }

            // Block MAPPING_ENTRYs must be terminated by a NEWLINE of
            // their own, or one in the block node they end with.
            //
            // The last entry of a mapping is allowed to lack a
            // terminator: an unterminated source doc (`a: 1\nb: 2`)
            // is legitimate YAML and must roundtrip.
            if !crate::nodes::in_flow_collection(node)
                && !is_last_entry_in_parent(node)
                && !entry_is_terminated(node)
            {
                return Err(format!(
                    "Block-style MAPPING_ENTRY not terminated by a NEWLINE (last token: {:?})",
                    node.last_token().map(|t| t.kind())
                ));
            }
        }
        // A comment directly in a block collection is a line of its own:
        // one ending an entry's line belongs to that entry. The entry may
        // end in an implicit null after its line break, as in `? - a\n`.
        SyntaxKind::MAPPING | SyntaxKind::SEQUENCE
            if !crate::nodes::starts_a_flow_collection(node) =>
        {
            let stray = node
                .children_with_tokens()
                .filter_map(|c| c.into_token())
                .filter(|t| t.kind() == SyntaxKind::COMMENT)
                .find(|t| {
                    std::iter::successors(t.prev_token(), |p| p.prev_token())
                        .find(|p| !p.text().is_empty())
                        .is_some_and(|p| {
                            !matches!(p.kind(), SyntaxKind::NEWLINE | SyntaxKind::INDENT)
                        })
                });
            if let Some(comment) = stray {
                return Err(format!(
                    "{:?} holds the comment {:?} ending an entry's line",
                    node.kind(),
                    comment.text()
                ));
            }
            // The lines after a nested collection's last entry belong to the
            // collection holding the next one, or to the root collection at
            // the end of the input.
            let nested = node
                .ancestors()
                .skip(1)
                .any(|n| matches!(n.kind(), SyntaxKind::MAPPING | SyntaxKind::SEQUENCE));
            let trailing_line = node
                .last_child_or_token()
                .and_then(|c| c.into_token())
                .filter(|t| !t.text().is_empty());
            if let Some(token) = trailing_line.filter(|_| nested) {
                return Err(format!(
                    "nested {:?} ends with {:?} {:?} after its last entry",
                    node.kind(),
                    token.kind(),
                    token.text()
                ));
            }
        }
        SyntaxKind::SEQUENCE_ENTRY
            if !crate::nodes::in_flow_collection(node)
                && !is_last_entry_in_parent(node)
                && !entry_is_terminated(node) =>
        {
            return Err(format!(
                "Block-style SEQUENCE_ENTRY not terminated by a NEWLINE (last token: {:?})",
                node.last_token().map(|t| t.kind())
            ));
        }
        _ => {}
    }

    Ok(())
}

/// Every KEY, VALUE and SEQUENCE_ENTRY holds a node, an omitted one being
/// the implicit null, and every SCALAR a token. A collection holds its
/// entries, with the trivia between them and its delimiters; a token no
/// entry could start goes in an ERROR node.
fn validate_children(node: &SyntaxNode) -> Result<(), String> {
    match node.kind() {
        SyntaxKind::KEY | SyntaxKind::VALUE | SyntaxKind::SEQUENCE_ENTRY
            if node.first_child().is_none() =>
        {
            Err(format!("{:?} holds no node", node.kind()))
        }
        SyntaxKind::SCALAR if node.first_token().is_none() => Err("SCALAR holds no token".into()),
        SyntaxKind::MAPPING | SyntaxKind::SEQUENCE => {
            let entry = if node.kind() == SyntaxKind::MAPPING {
                SyntaxKind::MAPPING_ENTRY
            } else {
                SyntaxKind::SEQUENCE_ENTRY
            };
            let flow = crate::nodes::starts_a_flow_collection(node);
            let stray = node.children_with_tokens().find(|c| match c {
                rowan::NodeOrToken::Node(n) => n.kind() != entry && n.kind() != SyntaxKind::ERROR,
                rowan::NodeOrToken::Token(t) => match t.kind() {
                    SyntaxKind::WHITESPACE
                    | SyntaxKind::NEWLINE
                    | SyntaxKind::INDENT
                    | SyntaxKind::COMMENT => false,
                    SyntaxKind::LEFT_BRACE
                    | SyntaxKind::RIGHT_BRACE
                    | SyntaxKind::LEFT_BRACKET
                    | SyntaxKind::RIGHT_BRACKET => !flow,
                    _ => true,
                },
            });
            match stray {
                Some(c) => Err(format!(
                    "{:?} holds {:?} outside its entries",
                    node.kind(),
                    c.kind()
                )),
                None => Ok(()),
            }
        }
        _ => Ok(()),
    }
}

/// A flow entry ends with the line of its comma, or of its content if it
/// has none: the lines after that, and the next line's indentation, are
/// the collection's.
fn validate_flow_entry_end(entry: &SyntaxNode) -> Result<(), String> {
    let children: Vec<_> = entry.children_with_tokens().collect();
    let end = children
        .iter()
        .rposition(|c| c.as_node().is_some() || c.kind() == SyntaxKind::COMMA)
        .map_or(0, |i| i + 1);
    let newline = children[end..]
        .iter()
        .position(|c| c.kind() == SyntaxKind::NEWLINE);
    match newline {
        Some(i) if end + i + 1 < children.len() => Err(format!(
            "flow {:?} holds {:?} after the end of its line",
            entry.kind(),
            children[end + i + 1].kind()
        )),
        _ => Ok(()),
    }
}

/// Is this entry terminated by a NEWLINE of its own, along with the
/// comment ending its line, if any?
///
/// A NEWLINE followed only by an implicit null's zero-width token counts
/// too: in `? - a\n` the line break ends the key's sequence, and the
/// entry's empty value comes after it. So does one followed by the spaces
/// of a block scalar's last line, which belong to its body.
fn entry_is_terminated(entry: &SyntaxNode) -> bool {
    crate::nodes::trivia::ends_line(entry)
}

/// Is this entry the last MAPPING_ENTRY / SEQUENCE_ENTRY child of its
/// parent MAPPING / SEQUENCE?
///
/// The last entry of a block collection is allowed to lack a trailing
/// NEWLINE (an unterminated source doc, `a: 1\nb: 2`, is legitimate
/// YAML and must roundtrip).
fn is_last_entry_in_parent(entry: &SyntaxNode) -> bool {
    // Walk siblings after `entry` looking for another of the same
    // kind. O(distance-to-next-same-kind-sibling) per call, so the
    // total cost across all entries in one parent is O(n), not O(n^2).
    let kind = entry.kind();
    let mut sib = entry.next_sibling();
    while let Some(s) = sib {
        if s.kind() == kind {
            return false;
        }
        sib = s.next_sibling();
    }
    true
}

/// Reject adjacent INDENT tokens among the children of `node`.
///
/// Two INDENTs concatenate at render time -- the sequence-under-empty-key
/// bug (fixed for #38) manifested exactly this way (parent VALUE's INDENT
/// followed by SEQUENCE's own INDENT rendered as 4 spaces).
fn validate_no_stacked_indents(node: &SyntaxNode) -> Result<(), String> {
    let mut prev_was_indent = false;
    for child in node.children_with_tokens() {
        match &child {
            rowan::NodeOrToken::Token(t) if t.kind() == SyntaxKind::INDENT => {
                if prev_was_indent {
                    return Err(format!(
                        "adjacent INDENT tokens inside {:?} -- would render as stacked indentation",
                        node.kind()
                    ));
                }
                prev_was_indent = true;
            }
            rowan::NodeOrToken::Token(_) => prev_was_indent = false,
            rowan::NodeOrToken::Node(_) => prev_was_indent = false,
        }
    }
    Ok(())
}

/// An entry should not end with two adjacent NEWLINE tokens as direct
/// children. Line breaks with a node between them, as around a flow
/// entry's value on a line of its own, are not a blank line.
///
/// That shape shows up when a placeholder NEWLINE isn't stripped after a
/// child collection gains content (see issue #18 and the sequence-under-
/// empty-key bug). Renders as a spurious blank line.
///
/// Only checked for MAPPING_ENTRY and SEQUENCE_ENTRY. Not applied to
/// MAPPING / SEQUENCE containers themselves: those legitimately carry
/// bare NEWLINE tokens between entries (blank-line separators are
/// valid formatting) and at the tail (trailing blank lines are valid
/// YAML input).
fn validate_no_double_trailing_newline(node: &SyntaxNode) -> Result<(), String> {
    let checks_apply = matches!(
        node.kind(),
        SyntaxKind::MAPPING_ENTRY | SyntaxKind::SEQUENCE_ENTRY
    );
    if checks_apply {
        let kinds: Vec<_> = node.children_with_tokens().map(|c| c.kind()).collect();
        if kinds.ends_with(&[SyntaxKind::NEWLINE, SyntaxKind::NEWLINE]) {
            return Err(format!(
                "{:?} ends with two NEWLINE tokens -- would render as a stray blank line: {:?}",
                node.kind(),
                node.text().to_string(),
            ));
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::yaml::YamlFile;
    use rowan::ast::AstNode;
    use std::str::FromStr;

    #[test]
    fn test_validate_tree_scales_linearly_with_nesting() {
        // Walking a node of the editable tree costs its depth.
        crate::scaling::assert_scales_linearly(
            "validate_tree",
            60,
            |n| {
                let keys: String = (0..n).map(|i| format!("{{k{i}: ")).collect();
                YamlFile::from_str(&format!("{keys}x{}", "}".repeat(n))).unwrap()
            },
            |file| validate_tree(file.syntax()).unwrap(),
        );
    }

    #[test]
    fn test_validate_tree_fits_a_small_stack() {
        // The walk used to recurse once per node.
        let text = format!("{}{}", "[{".repeat(127), "}]".repeat(127));
        let green = YamlFile::from_str(&text)
            .unwrap()
            .syntax()
            .green()
            .into_owned();
        // Dropping the last reference to a tree recurses through it, so
        // keep one out here.
        let tree = green.clone();
        std::thread::Builder::new()
            .stack_size(64 * 1024)
            .spawn(move || validate_tree(&SyntaxNode::new_root_mut(tree)))
            .unwrap()
            .join()
            .unwrap()
            .unwrap();
        drop(green);
    }

    /// A comment ending an entry's line belongs to the entry, not to the
    /// collection beside it.
    #[test]
    fn test_validate_tree_line_end_comment_outside_entry() {
        let file = YamlFile::from_str("- a  # c\n- b\n").unwrap();
        assert_eq!(validate_tree(file.syntax()), Ok(()));
        let entry = file
            .syntax()
            .descendants()
            .find(|n| n.kind() == SyntaxKind::SEQUENCE_ENTRY)
            .unwrap();
        let line_end: Vec<_> = entry
            .children_with_tokens()
            .skip_while(|c| c.kind() != SyntaxKind::COMMENT)
            .collect();
        for c in &line_end {
            c.detach();
        }
        let sequence = entry.parent().unwrap();
        crate::nodes::insert_children(&sequence, entry.index() + 1, line_end);
        assert_eq!(file.syntax().to_string(), "- a  # c\n- b\n");
        assert_eq!(
            validate_tree(file.syntax()),
            Err("SEQUENCE holds the comment \"# c\" ending an entry's line".to_string())
        );
    }

    /// An explicit key without a value that ends in a block node ends its
    /// line before the zero-width null of its value, so a comment after
    /// that is a line of its own.
    #[test]
    fn test_validate_tree_comment_line_after_explicit_block_key() {
        for yaml in ["? - a\n# c\n", "- ? - a\n# c\n"] {
            let file = YamlFile::from_str(yaml).unwrap();
            assert_eq!(validate_tree(file.syntax()), Ok(()), "{yaml:?}");
        }
    }

    /// A nested block collection ends in an entry: the lines after it are
    /// its parent's.
    #[test]
    fn test_validate_tree_nested_collection_ending_in_comment() {
        let file = YamlFile::from_str("a:\n  b: 1\n  # c\n").unwrap();
        assert_eq!(validate_tree(file.syntax()), Ok(()));
        let root = file.syntax().first_child().unwrap().first_child().unwrap();
        let nested = root
            .descendants()
            .find(|n| n.kind() == SyntaxKind::MAPPING && n != &root)
            .unwrap();
        let lines: Vec<_> = root
            .children_with_tokens()
            .skip_while(|c| c.kind() == SyntaxKind::MAPPING_ENTRY)
            .collect();
        for c in &lines {
            match c {
                rowan::NodeOrToken::Node(n) => n.detach(),
                rowan::NodeOrToken::Token(t) => t.detach(),
            }
        }
        crate::nodes::append_children(&nested, lines);
        assert_eq!(file.syntax().to_string(), "a:\n  b: 1\n  # c\n");
        assert_eq!(
            validate_tree(file.syntax()),
            Err("nested MAPPING ends with NEWLINE \"\\n\" after its last entry".to_string())
        );
    }

    #[test]
    fn test_validate_tree_implicit_null_ending_sequence_entry() {
        for yaml in [
            "- a:\n- c\n",
            "-\n  a: &b\n- c\n",
            "- &c : &a\n- c\n",
            "- &a\n- a\n-\n  &a : a\n  b: &b\n-\n  &c : &a\n",
        ] {
            let file = YamlFile::from_str(yaml).unwrap();
            assert_eq!(validate_tree(file.syntax()), Ok(()), "{yaml:?}");
        }
        let file = YamlFile::from_str("- a: 1\n- c\n").unwrap();
        let entry = file
            .syntax()
            .descendants()
            .find(|n| n.kind() == SyntaxKind::SEQUENCE_ENTRY)
            .unwrap();
        entry.last_token().unwrap().detach();
        assert_eq!(
            validate_tree(file.syntax()),
            Err(
                "Block-style SEQUENCE_ENTRY not terminated by a NEWLINE (last token: Some(INT))"
                    .to_string()
            )
        );
    }

    /// Line breaks with a node between them are not a blank line: in a
    /// flow entry the value may sit on its own line, between the line break
    /// after the `:` and the one ending the entry.
    #[test]
    fn test_validate_tree_newlines_around_value() {
        for yaml in ["{:\nv\n}\n", "{a:\n\nv}\n", "-\n\n  a\n"] {
            let file = YamlFile::from_str(yaml).unwrap();
            assert_eq!(validate_tree(file.syntax()), Ok(()), "{yaml:?}");
        }
        let file = YamlFile::from_str("- a\n- b\n").unwrap();
        let entry = file
            .syntax()
            .descendants()
            .find(|n| n.kind() == SyntaxKind::SEQUENCE_ENTRY)
            .unwrap();
        let blank = YamlFile::from_str("- a\n").unwrap();
        let newline = blank
            .syntax()
            .descendants_with_tokens()
            .find(|c| c.kind() == SyntaxKind::NEWLINE)
            .unwrap();
        newline.detach();
        crate::nodes::append_children(&entry, vec![newline]);
        assert_eq!(
            validate_tree(file.syntax()),
            Err(
                "SEQUENCE_ENTRY ends with two NEWLINE tokens -- would render as a stray blank line: \"- a\\n\\n\""
                    .to_string()
            )
        );
    }

    /// The parser gives an entry whose value was omitted, or whose `:` is
    /// missing, an implicit null VALUE, so one without is malformed.
    #[test]
    fn test_validate_tree_entry_without_value() {
        let file = YamlFile::parse("a:\n<<\n").tree();
        assert_eq!(validate_tree(file.syntax()), Ok(()));
        for entry in file
            .syntax()
            .descendants()
            .filter(|n| n.kind() == SyntaxKind::MAPPING_ENTRY)
            .collect::<Vec<_>>()
        {
            let value = entry
                .children()
                .find(|c| c.kind() == SyntaxKind::VALUE)
                .unwrap();
            let index = value.index();
            value.detach();
            assert_eq!(
                validate_tree(file.syntax()),
                Err("MAPPING_ENTRY should have exactly 1 VALUE, found 0".to_string())
            );
            crate::nodes::insert_children(&entry, index, vec![value.into()]);
            assert_eq!(file.syntax().to_string(), "a:\n<<\n");
        }
    }

    #[test]
    fn test_validate_tree_empty_slots() {
        let file = YamlFile::from_str("a: [b]\n").unwrap();
        let find = |kind| {
            file.syntax()
                .descendants()
                .find(|n| n.kind() == kind)
                .unwrap()
        };
        for kind in [
            SyntaxKind::KEY,
            SyntaxKind::VALUE,
            SyntaxKind::SEQUENCE_ENTRY,
        ] {
            let slot = find(kind);
            let node = slot.first_child().unwrap();
            let index = node.index();
            node.detach();
            assert_eq!(
                validate_tree(file.syntax()),
                Err(format!("{kind:?} holds no node"))
            );
            crate::nodes::insert_children(&slot, index, vec![node.into()]);
        }
        let token = find(SyntaxKind::SCALAR).first_token().unwrap();
        token.detach();
        assert_eq!(
            validate_tree(file.syntax()),
            Err("SCALAR holds no token".to_string())
        );
    }

    /// A token no entry starts with goes in an ERROR node.
    #[test]
    fn test_validate_tree_stray_token_in_collection() {
        let file = YamlFile::parse("[]:\n]:").tree();
        assert_eq!(validate_tree(file.syntax()), Ok(()));
        let error = file
            .syntax()
            .descendants()
            .find(|n| n.kind() == SyntaxKind::ERROR)
            .unwrap();
        let mapping = error.parent().unwrap();
        let token = error.first_token().unwrap();
        let index = error.index();
        error.detach();
        token.detach();
        crate::nodes::insert_children(&mapping, index, vec![token.into()]);
        assert_eq!(file.syntax().to_string(), "[]:\n]:");
        assert_eq!(
            validate_tree(file.syntax()),
            Err("MAPPING holds RIGHT_BRACKET outside its entries".to_string())
        );
    }

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
            write!(f, "  {key:?}: ")?;
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
            write!(f, "  [{i}]: ")?;
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
        writeln!(f, "{indent_str}{line}")?;
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
                write!(f, "{indent_str}  ")?;
                format_node(f, &k, 0)?;
                write!(f, ": ")?;
                format_node(f, &v, indent + 1)?;
                writeln!(f)?;
            }
            write!(f, "{indent_str}}}")?;
        }
        YamlNode::Sequence(s) => {
            writeln!(f, "Sequence [")?;
            for (i, v) in s.values().enumerate() {
                write!(f, "{indent_str}  [{i}]: ")?;
                format_node(f, &v, indent + 1)?;
                writeln!(f)?;
            }
            write!(f, "{indent_str}]")?;
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
            writeln!(f, "  {line}")?;
        }

        writeln!(f, "\nAfter:")?;
        writeln!(f, "------")?;
        for line in self.after.to_string().lines() {
            writeln!(f, "  {line}")?;
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
    result.push_str(&format!("  n{node_id} [label=\"{label}\"];\n"));

    // Link to parent
    if let Some(pid) = parent_id {
        result.push_str(&format!("  n{pid} -> n{node_id};\n"));
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
                let label = format!("{:?}\\n\\\"{}\\\"", t.kind(), text);
                result.push_str(&format!(
                    "  n{token_id} [label=\"{label}\", shape=ellipse, style=filled, fillcolor=lightgray];\n"
                ));
                result.push_str(&format!("  n{node_id} -> n{token_id};\n"));
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
    fn test_graphviz_dot_escapes_token_text() {
        let yaml = YamlFile::from_str("k: \"a\\\"b\\\\c\"\n").unwrap();
        let dot = graphviz_dot(yaml.syntax());
        let token_labels: Vec<&str> = dot
            .lines()
            .filter(|l| l.contains("shape=ellipse"))
            .map(|l| l.split(", shape=").next().unwrap().trim())
            .collect();
        assert_eq!(
            token_labels,
            vec![
                r#"n6 [label="STRING\n\"k\"""#,
                r#"n7 [label="COLON\n\":\"""#,
                r#"n8 [label="WHITESPACE\n\" \"""#,
                r#"n11 [label="STRING\n\"\"a\\\"b\\\\c\"\"""#,
                r#"n12 [label="NEWLINE\n\"\\n\"""#,
            ]
        );
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
