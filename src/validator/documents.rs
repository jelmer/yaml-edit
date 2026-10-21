//! Checks on document markers and document-level content.

use super::{Rule, Violation};
use crate::nodes::starts_a_flow_collection;
use crate::yaml::SyntaxNode;

/// Check document marker placement
pub(super) fn check_document_marker_placement(node: &SyntaxNode, violations: &mut Vec<Violation>) {
    // Document markers should only appear at document boundaries
    // Check if marker is in inappropriate context (e.g., inside a quoted string)
    if let Some(parent) = node.parent() {
        if matches!(
            parent.kind(),
            crate::SyntaxKind::STRING | crate::SyntaxKind::SCALAR
        ) {
            violations.push(Violation::error_at(
                Rule::InvalidDocumentMarker,
                node.text_range(),
                "Document marker inside string is invalid",
            ));
        }
    }
}

/// Check that a node slot (a document, key, value, sequence entry or
/// tagged node) holds at most one node.
///
/// In `? a\n 5b: 1\n` the more indented line would continue the plain
/// scalar `a`, which then could not be an implicit key spanning lines;
/// the parser keeps the mapping as a second node of the key instead.
/// Likewise the `x` of `[q] x` is kept next to the flow sequence.
pub(super) fn check_single_content_node(node: &SyntaxNode, violations: &mut Vec<Violation>) {
    use crate::SyntaxKind;

    let Some(extra) = node
        .children()
        .filter(|c| {
            matches!(
                c.kind(),
                SyntaxKind::SCALAR
                    | SyntaxKind::MAPPING
                    | SyntaxKind::SEQUENCE
                    | SyntaxKind::TAGGED_NODE
                    | SyntaxKind::ALIAS
            )
        })
        .nth(1)
    else {
        return;
    };
    let what = match node.kind() {
        SyntaxKind::DOCUMENT => "Document",
        SyntaxKind::KEY => "Key",
        SyntaxKind::SEQUENCE_ENTRY => "Sequence entry",
        SyntaxKind::TAGGED_NODE => "Tagged node",
        _ => "Value",
    };
    violations.push(Violation::error_at(
        Rule::Other,
        extra.text_range(),
        format!("{what} holds more than one node"),
    ));
}

/// Check for anchors at document level without proper node attachment
///
/// Anchors should be attached to nodes (values), not floating at document level.
pub(super) fn check_document_level_anchors(node: &SyntaxNode, violations: &mut Vec<Violation>) {
    // Check for ANCHOR tokens that are direct children of DOCUMENT
    for child in node.children_with_tokens() {
        if let rowan::NodeOrToken::Token(token) = child {
            if token.kind() == crate::SyntaxKind::ANCHOR {
                // An anchor at document level annotates the document's
                // own node, which is the ordinary spelling of
                // `&sequence\n- a\n` and `&flowseq [ ... ]`. It is
                // stranded only when nothing follows it.
                let annotates_a_node = token
                    .siblings_with_tokens(rowan::Direction::Next)
                    .filter_map(|sibling| sibling.into_node())
                    .any(|n| {
                        matches!(
                            n.kind(),
                            crate::SyntaxKind::MAPPING
                                | crate::SyntaxKind::SEQUENCE
                                | crate::SyntaxKind::SCALAR
                                | crate::SyntaxKind::TAGGED_NODE
                                | crate::SyntaxKind::ALIAS
                        )
                    });
                if !annotates_a_node {
                    violations.push(Violation::error_at(
                        Rule::Other,
                        token.text_range(),
                        "Anchor must be attached to a node, not at document level",
                    ));
                }
            }
        }
    }
}

/// Check that content doesn't appear on same line as document start marker
///
/// According to YAML spec, content should not appear on the same line as
/// a document start marker (---).
pub(super) fn check_doc_start_token_content(
    token: &rowan::SyntaxToken<crate::Lang>,
    violations: &mut Vec<Violation>,
) {
    // Look at siblings after DOC_START token
    let mut found_newline = false;
    let mut found_content = false;

    // Check if there's content before a newline
    let mut current = token.next_sibling_or_token();
    while let Some(sibling) = current {
        let next = match &sibling {
            rowan::NodeOrToken::Token(t) => {
                if t.kind() == crate::SyntaxKind::NEWLINE {
                    found_newline = true;
                    break;
                }
                t.next_sibling_or_token()
            }
            rowan::NodeOrToken::Node(n) => {
                // Only a *block* collection is illegal here. YAML 1.2
                // `l-explicit-document` lets a node share the marker's
                // line, so `--- a`, `--- >`, `--- {a: 1}` and a tagged
                // node are all valid, as PyYAML reads them; a block
                // mapping or sequence (`--- key: v`, `--- - a`) is not.
                let is_flow = starts_a_flow_collection(n);
                match n.kind() {
                    crate::SyntaxKind::MAPPING | crate::SyntaxKind::SEQUENCE if !is_flow => {
                        found_content = true;
                        break;
                    }
                    _ => {}
                }
                n.next_sibling_or_token()
            }
        };
        current = next;
    }

    if found_content && !found_newline {
        violations.push(Violation::error_at(
            Rule::InvalidDocumentMarker,
            token.text_range(),
            "Content on same line as document start marker",
        ));
    }
}
