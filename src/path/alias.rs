//! Resolving aliases that a path descends through.

use super::parse::segment_display;
use super::{AliasPolicy, PathError, PathSegment};

/// Build an anchor registry covering the document `node` belongs to,
/// so aliases anywhere in the document resolve even when traversal
/// started from a nested `Mapping`. A node outside any document (e.g. a
/// detached `Mapping`) uses its own tree root.
pub(super) fn registry_for(
    node: &crate::yaml::SyntaxNode,
) -> crate::anchor_resolution::AnchorRegistry {
    use crate::anchor_resolution::{document_scope, AnchorRegistry};
    AnchorRegistry::from_tree(&document_scope(node))
}

/// The [`PathError`] for an alias at `segment` that
/// [`AnchorRegistry::resolve_alias_through`] could not resolve. An alias
/// that ends up referring to itself, as in `&a *a`, refers to no value
/// and is undefined as well.
///
/// [`AnchorRegistry::resolve_alias_through`]: crate::anchor_resolution::AnchorRegistry::resolve_alias_through
pub(super) fn undefined_alias(
    error: crate::anchor_resolution::AliasError,
    segment: &PathSegment,
) -> PathError {
    use crate::anchor_resolution::AliasError;
    let (AliasError::Undefined { alias } | AliasError::Recursive { alias }) = error;
    PathError::UndefinedAlias {
        at: segment_display(segment),
        alias,
    }
}

/// Resolve an alias that `segment` descends through.
///
/// Under [`AliasPolicy::Refuse`] this is an error. Under
/// [`AliasPolicy::Follow`] it returns the anchored node itself, so
/// writes land on the shared value. Under [`AliasPolicy::Expand`] it
/// returns a detached, anchor-free copy of the anchored node (see
/// [`expanded_copy`]), which the caller substitutes for the alias.
pub(super) fn resolve_alias(
    alias: &crate::yaml::Alias,
    segment: &PathSegment,
    policy: AliasPolicy,
) -> Result<crate::as_yaml::YamlNode, PathError> {
    use rowan::ast::AstNode;

    if policy == AliasPolicy::Refuse {
        return Err(PathError::AliasRefused {
            at: segment_display(segment),
            alias: alias.name(),
        });
    }
    let registry = registry_for(alias.syntax());
    crate::anchor_resolution::resolve_alias_with(alias, &registry, |target| {
        if policy == AliasPolicy::Expand {
            expanded_copy(target)
        } else {
            target.clone()
        }
    })
    .map_err(|e| undefined_alias(e, segment))
}

/// Build a detached copy of an anchored node to substitute for an alias.
///
/// The copy is dedented to column 0, as a detached root has no context to
/// measure its indentation from. It carries no anchors: the `&anchor` beside
/// the original stays behind in its parent VALUE, and anchors nested inside
/// are dropped so that later aliases keep referring to the original nodes.
fn expanded_copy(target: &crate::yaml::SyntaxNode) -> crate::yaml::SyntaxNode {
    use crate::lex::SyntaxKind;

    let mut builder = rowan::GreenNodeBuilder::new();
    builder.start_node(target.kind().into());
    let base = crate::column::node_column(target) as isize;
    crate::as_yaml::copy_node_content_reindent(&mut builder, target, -base);
    builder.finish_node();
    let copy = crate::yaml::SyntaxNode::new_root_mut(builder.finish());

    let anchors: Vec<_> = copy
        .descendants_with_tokens()
        .filter_map(|e| e.into_token())
        .filter(|t| t.kind() == SyntaxKind::ANCHOR)
        .collect();
    for anchor in anchors {
        crate::nodes::detach_anchor(&anchor);
    }
    copy
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_expand_copy_keeps_tag_after_anchor() {
        use crate::yaml::Document;
        use std::str::FromStr;

        for src in [
            "a: !!map &x\n  k: 1\nb: *x\n",
            "a: &x !!map\n  k: 1\nb: *x\n",
        ] {
            let doc = Document::from_str(src).unwrap();
            let Some(crate::as_yaml::YamlNode::Alias(alias)) = doc.as_mapping().unwrap().get("b")
            else {
                panic!("b is not an alias in {src:?}");
            };
            let segment = PathSegment::Key("b".to_string());
            let copy = resolve_alias(&alias, &segment, AliasPolicy::Expand).unwrap();
            assert_eq!(copy.to_string(), "!!map\nk: 1\n", "{src:?}");
        }
    }
}
