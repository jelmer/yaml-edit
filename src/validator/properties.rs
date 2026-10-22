//! Checks on node properties: anchors, aliases and tags.

use super::{Rule, Violation};
use crate::nodes::{child_token, has_child_token};
use crate::tag;
use crate::yaml::SyntaxNode;
use rowan::ast::AstNode;

/// Check for multiple anchors on the same node
pub(super) fn check_multiple_anchors(node: &SyntaxNode, violations: &mut Vec<Violation>) {
    // Count ANCHOR tokens (not nodes) in this node's children. An
    // anchor that annotates a complex key now sits inside that KEY, so
    // 6BFJ's `&mapping\n&key [ ... ]: v` leaves one here while 4JVG's
    // two anchors on a single value still leave both.
    let adjacent_anchors = node
        .children_with_tokens()
        .filter(|child| {
            child
                .as_token()
                .is_some_and(|t| t.kind() == crate::SyntaxKind::ANCHOR)
        })
        .count();

    if adjacent_anchors > 1 {
        violations.push(Violation::error_at(
            Rule::InvalidAnchor,
            node.text_range(),
            "Multiple anchors on the same node",
        ));
    }
}

/// A node has one tag, but the parser nests a second one, as in
/// `!a !b x`, as a TAGGED_NODE of its own.
pub(super) fn check_multiple_tags(node: &SyntaxNode, violations: &mut Vec<Violation>) {
    let has_tag = |n: &SyntaxNode| has_child_token(n, |k| k == crate::SyntaxKind::TAG);
    let inner = node
        .children()
        .find(|c| c.kind() == crate::SyntaxKind::TAGGED_NODE);
    if has_tag(node) && inner.is_some_and(|inner| has_tag(&inner)) {
        violations.push(Violation::error_at(
            Rule::InvalidTag,
            node.text_range(),
            "Multiple tags on the same node",
        ));
    }
}

/// Check that every alias refers to an anchor earlier in the same document
///
/// Anchors are scoped to a document and an alias can only refer to an
/// anchor that precedes it, so `a: *x` and `a: *x\nb: &x 1` are errors.
pub(super) fn check_undefined_aliases(document: &SyntaxNode, violations: &mut Vec<Violation>) {
    let mut anchors = std::collections::HashSet::new();
    // The lexer gives `*x` and `&x` opening a line of a block scalar body
    // or a plain scalar continuation as REFERENCE and ANCHOR too. The
    // parser keeps those inside the SCALAR as text, and wraps real
    // aliases in an ALIAS node.
    for token in document
        .descendants_with_tokens()
        .filter_map(|e| e.into_token())
        .filter(|t| t.parent().map(|p| p.kind()) != Some(crate::SyntaxKind::SCALAR))
    {
        match token.kind() {
            crate::SyntaxKind::ANCHOR => {
                anchors.insert(token.text()[1..].to_string());
            }
            crate::SyntaxKind::REFERENCE if !anchors.contains(&token.text()[1..]) => {
                violations.push(Violation::error_at(
                    Rule::InvalidAnchor,
                    token.text_range(),
                    format!("Undefined alias {}", token.text()),
                ));
            }
            _ => {}
        }
    }
}

/// Check for both anchor and alias on the same node
///
/// An alias node cannot have properties (spec production c-ns-alias-node),
/// so `&b *a` is invalid. The parser leaves the anchor as a sibling of the
/// ALIAS node, possibly with a tag around the alias (`&b !t *a`). An alias
/// further down, such as in `&b !t [*a]` or `&n\n  *a : v` (test suite
/// 26DV), belongs to a different node.
pub(super) fn check_anchor_and_alias(node: &SyntaxNode, violations: &mut Vec<Violation>) {
    use crate::SyntaxKind;

    let Some(anchor) = child_token(node, SyntaxKind::ANCHOR) else {
        return;
    };
    let is_alias = |n: &SyntaxNode| n.kind() == SyntaxKind::ALIAS;
    let has_alias = node.children().any(|child| {
        is_alias(&child)
            || (child.kind() == SyntaxKind::TAGGED_NODE && child.children().any(|c| is_alias(&c)))
    });
    if has_alias {
        violations.push(Violation::error_at(
            Rule::Other,
            anchor.text_range(),
            "Node cannot have both an anchor and be an alias",
        ));
    }
}

/// Check that an alias has no tag
///
/// An alias node cannot have properties (spec production c-ns-alias-node),
/// so `!t *a` is invalid.
pub(super) fn check_tag_on_alias(node: &SyntaxNode, violations: &mut Vec<Violation>) {
    if !node
        .children()
        .any(|child| child.kind() == crate::SyntaxKind::ALIAS)
    {
        return;
    }
    let Some(tag) = child_token(node, crate::SyntaxKind::TAG) else {
        return;
    };
    violations.push(Violation::error_at(
        Rule::InvalidTag,
        tag.text_range(),
        "An alias cannot have a tag",
    ));
}

/// Check that TAG tokens don't contain invalid characters
///
/// YAML spec restricts which characters can appear in tags.
/// Tags cannot contain: {, }, [, ], or comma (,)
pub(super) fn check_tag_characters(
    token: &rowan::SyntaxToken<crate::Lang>,
    violations: &mut Vec<Violation>,
) {
    let tag_text = token.text();

    // A verbatim tag `!<...>` carries a URI, where these are all legal:
    // `!<tag:yaml.org,2002:str>` is the test suite's 7FWL and UGM3. The
    // restriction is on the shorthand form, whose `ns-tag-char` excludes
    // the flow indicators.
    if tag_text.starts_with("!<") {
        return;
    }

    // Check for invalid characters in tags
    let invalid_chars = ['{', '}', '[', ']', ','];
    for ch in invalid_chars {
        if tag_text.contains(ch) {
            violations.push(Violation::error_at(
                Rule::InvalidTag,
                token.text_range(),
                format!("Invalid character '{ch}' in tag"),
            ));
            return; // Only report once per tag
        }
    }
}

/// Check that a `!!set` tags a mapping, whose keys are its members, and
/// a `!!omap` or `!!pairs` a sequence.
///
/// These are YAML 1.1 types rather than YAML 1.2 ones, so the wrong
/// content is only a warning. A tagged alias is left to
/// [`Self::check_tag_on_alias`].
pub(super) fn check_collection_tag_content(node: &SyntaxNode, violations: &mut Vec<Violation>) {
    let Some(tagged) = crate::yaml::TaggedNode::cast(node.clone()) else {
        return;
    };
    if node
        .children()
        .any(|child| child.kind() == crate::SyntaxKind::ALIAS)
    {
        return;
    }
    let message = match tagged.resolved_tag().as_deref() {
        Some(tag::SET) if tagged.as_set().is_none() => "!!set content is not a mapping",
        Some(tag::OMAP) if tagged.as_sequence().is_none() => "!!omap content is not a sequence",
        Some(tag::PAIRS) if tagged.as_sequence().is_none() => "!!pairs content is not a sequence",
        _ => return,
    };
    let Some(tag) = child_token(node, crate::SyntaxKind::TAG) else {
        return;
    };
    violations.push(Violation::warning_at(
        Rule::InvalidTag,
        tag.text_range(),
        message,
    ));
}
