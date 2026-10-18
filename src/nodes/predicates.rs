//! Predicates on the shape of CST nodes and tokens.

use super::{SyntaxNode, SyntaxToken};
use crate::lex::SyntaxKind;

/// Whether `node` is a block scalar, opening with its `|` or `>` header.
///
/// A plain scalar can hold a `|` or `>` token too, at the start of a
/// continuation line, so the header has to come first.
pub(crate) fn is_block_scalar(node: &SyntaxNode) -> bool {
    node.kind() == SyntaxKind::SCALAR
        && node
            .first_token()
            .is_some_and(|t| matches!(t.kind(), SyntaxKind::PIPE | SyntaxKind::GREATER))
}

/// Whether `token` is a single- or double-quoted scalar.
pub(crate) fn is_quoted_scalar(token: &SyntaxToken) -> bool {
    token.kind() == SyntaxKind::STRING && token.text().starts_with(['"', '\''])
}

/// Whether `node` is a flow collection, by its own delimiter.
///
/// A flow collection holds its `{`/`[` as a direct child; `first_token()`
/// instead descends the whole left spine, which made validating a large
/// mapping quadratic, and finds a flow key's delimiter in a block mapping.
pub(crate) fn starts_a_flow_collection(node: &SyntaxNode) -> bool {
    // The delimiter opens the collection, so it is among the first children;
    // scanning them all is O(entries) and ran per entry.
    node.children_with_tokens()
        .take_while(|c| c.as_token().is_some())
        .any(|c| matches!(c.kind(), SyntaxKind::LEFT_BRACE | SyntaxKind::LEFT_BRACKET))
}

/// Whether `node` sits inside a flow collection.
///
/// The nearest collection is not enough: the single pair in `[k: v]` is a
/// mapping without braces.
pub(crate) fn in_flow_collection(node: &SyntaxNode) -> bool {
    node.ancestors().skip(1).any(|a| {
        matches!(a.kind(), SyntaxKind::MAPPING | SyntaxKind::SEQUENCE)
            && starts_a_flow_collection(&a)
    })
}
