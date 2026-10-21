//! Checks on flow collections and what may appear in them.

use super::{Rule, Scope, Violation};
use crate::column::space_indent;
use crate::nodes::{is_block_scalar, starts_a_flow_collection};
use crate::yaml::SyntaxNode;

/// Is this flow entry an omitted one, i.e. does its value consist solely of
/// the zero-width implicit-null scalar the parser emits for an empty slot?
///
/// See the implicit-null section of the CST invariants in `nodes/mod.rs`.
fn entry_is_empty(entry: &SyntaxNode) -> bool {
    // Stop at the first real character rather than rendering the entry,
    // whose text covers all the nesting below it.
    entry
        .text()
        .try_for_each_chunk(|chunk| {
            if chunk.chars().all(|c| c.is_whitespace() || c == ',') {
                Ok(())
            } else {
                Err(())
            }
        })
        .is_ok()
}

/// Check that a block scalar does not appear inside a flow collection.
///
/// Flow content may only hold flow nodes. The parser reads `[k: |\n  x\n]`
/// as a plain scalar starting with `|`, while an edit can leave a real
/// block scalar there, so both shapes are checked.
pub(super) fn check_block_scalar_in_flow(
    node: &SyntaxNode,
    in_flow: bool,
    violations: &mut Vec<Violation>,
) {
    use crate::SyntaxKind;

    let Some(first) = node.first_token() else {
        return;
    };
    let is_block_header = is_block_scalar(node)
        || (first.kind() == SyntaxKind::STRING && first.text().starts_with(['|', '>']));
    if is_block_header && in_flow {
        violations.push(Violation::error_at(
            Rule::Other,
            first.text_range(),
            "Block scalar cannot appear inside a flow collection",
        ));
    }
}

/// Check for missing commas in flow collections
pub(super) fn check_flow_collection_commas(node: &SyntaxNode, violations: &mut Vec<Violation>) {
    if !starts_a_flow_collection(node) {
        return;
    }
    let entry_kind = match node.kind() {
        crate::SyntaxKind::MAPPING => crate::SyntaxKind::MAPPING_ENTRY,
        crate::SyntaxKind::SEQUENCE => crate::SyntaxKind::SEQUENCE_ENTRY,
        _ => return,
    };

    // A flow separator is stored inside the entry it follows (see the
    // flow-separator invariant in nodes/mod.rs), and an omitted entry
    // shows up as one whose value is the zero-width implicit null. So
    // `[a, , b]` and `[a, b, ,]` are entries with an empty value rather
    // than adjacent COMMA tokens.
    let entries: Vec<_> = node.children().filter(|n| n.kind() == entry_kind).collect();

    for entry in &entries {
        if entry_is_empty(entry) {
            violations.push(Violation::error_at(
                Rule::Other,
                node.text_range(),
                "Empty entry in flow collection",
            ));
            break;
        }
    }

    // Every entry but the last needs a separator; counting them instead
    // let a trailing comma stand in for a missing one.
    for pair in entries.windows(2) {
        if !crate::nodes::has_child_token(&pair[0], |k| k == crate::SyntaxKind::COMMA) {
            violations.push(Violation::error_at(
                Rule::MissingSyntax,
                pair[1].text_range(),
                "Missing comma between flow collection entries",
            ));
        }
    }
}

/// Check that a mapping without braces inside a flow collection is a
/// flow sequence entry.
///
/// `[k: v]` holds a single-pair mapping, but a flow node cannot be one,
/// so the parser's reading of `{k: ? a}` is invalid.
pub(super) fn check_flow_pair_placement(
    node: &SyntaxNode,
    in_flow: bool,
    violations: &mut Vec<Violation>,
) {
    if starts_a_flow_collection(node) || !in_flow {
        return;
    }
    let is_sequence_entry = node
        .parent()
        .filter(|p| p.kind() == crate::SyntaxKind::SEQUENCE_ENTRY)
        .and_then(|p| p.parent())
        .is_some_and(|s| starts_a_flow_collection(&s));
    if !is_sequence_entry {
        violations.push(Violation::error_at(
            Rule::Other,
            node.text_range(),
            "Mapping without braces can only be a flow sequence entry",
        ));
    }
}

/// Check that a multi-line flow collection indents its continuation lines.
///
/// A flow collection may span lines, but each continuation has to be
/// indented more than the block context it sits in; at column zero the
/// content would be read as a new block node instead (yaml-test-suite
/// 9C9N).
pub(super) fn check_flow_continuation_indent(
    node: &SyntaxNode,
    scope: Scope,
    violations: &mut Vec<Violation>,
) {
    if !starts_a_flow_collection(node) {
        return;
    }
    // Only the outermost flow collection needs checking; a nested one
    // shares its lines.
    if scope.in_flow {
        return;
    }

    // A `---` or `...` at the start of a line ends the document, so it
    // cannot appear inside a flow collection: the test suite's N782 is
    // the error `[\n--- ,\n...\n]`. Inside the collection the lexer
    // gives them as plain scalars, so no document-marker rule sees them.
    let mut line_start = true;
    for el in node.descendants_with_tokens() {
        let rowan::NodeOrToken::Token(t) = el else {
            continue;
        };
        match t.kind() {
            crate::SyntaxKind::NEWLINE => line_start = true,
            crate::SyntaxKind::INDENT | crate::SyntaxKind::WHITESPACE => {}
            _ if line_start && matches!(t.text(), "---" | "...") => {
                violations.push(Violation::error_at(
                    Rule::InvalidDocumentMarker,
                    t.text_range(),
                    "Document marker inside a flow collection",
                ));
                return;
            }
            _ => line_start = false,
        }
    }

    // Only a flow collection sitting inside a block collection has to
    // clear that block's indentation, or its continuation would read as
    // a new entry of that block (9C9N's `flow: [a,\nb,\nc]`). A flow
    // collection that is the document's own node has no such neighbour,
    // so its entries may start at column zero (4ABK).
    let Some(required) = scope.continuation_indent else {
        return;
    };

    // The indentation of the current line, while still at its start.
    // Tabs are not indentation, so only leading spaces count.
    let mut indent: Option<usize> = None;
    for el in node.descendants_with_tokens() {
        let rowan::NodeOrToken::Token(t) = el else {
            continue;
        };
        match t.kind() {
            crate::SyntaxKind::NEWLINE => indent = Some(0),
            crate::SyntaxKind::INDENT | crate::SyntaxKind::WHITESPACE if indent.is_some() => {
                indent = Some(space_indent(t.text()));
            }
            crate::SyntaxKind::COMMENT => indent = None,
            _ if indent.take().is_some_and(|i| i < required) => {
                violations.push(Violation::error_at(
                    Rule::InvalidIndentation,
                    node.text_range(),
                    "Flow collection continuation line must be indented",
                ));
                return;
            }
            _ => {}
        }
    }
}

/// Check that a TAG token is not immediately followed by a comma
///
/// Per YAML spec, a tag must be followed by whitespace and then the tagged value.
/// A comma immediately after a tag (without whitespace and value) is invalid.
/// Example: `!!str, xxx` is invalid; should be `!!str xxx` or `!!str "xxx"`
pub(super) fn check_tag_followed_by_comma(
    token: &rowan::SyntaxToken<crate::Lang>,
    violations: &mut Vec<Violation>,
) {
    // Look at the next sibling after the TAG token
    let mut current = token.next_sibling_or_token();

    // Skip whitespace to find the next meaningful element
    while let Some(sibling) = current {
        match &sibling {
            rowan::NodeOrToken::Token(t) => {
                match t.kind() {
                    crate::SyntaxKind::WHITESPACE | crate::SyntaxKind::NEWLINE => {
                        // Whitespace is expected, continue to next
                        current = t.next_sibling_or_token();
                        continue;
                    }
                    crate::SyntaxKind::COMMA => {
                        // Found a comma directly after the tag - this is invalid
                        violations.push(Violation::error_at(
                            Rule::InvalidTag,
                            token.text_range(),
                            "Invalid comma after tag",
                        ));
                        return;
                    }
                    _ => {
                        // Found some other token - that's fine
                        return;
                    }
                }
            }
            // A scalar starting with a comma (`!!str, xxx` in block
            // context) is reported by `check_plain_scalar_start`.
            rowan::NodeOrToken::Node(_) => return,
        }
    }
}
