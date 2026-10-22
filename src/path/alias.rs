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
    if policy != AliasPolicy::Expand {
        return crate::anchor_resolution::resolve_alias_with(alias, &registry, |t| t.clone())
            .map_err(|e| undefined_alias(e, segment));
    }
    let undefined = || crate::anchor_resolution::AliasError::Undefined {
        alias: alias.name(),
    };
    registry
        .resolve_alias_through(alias)
        .and_then(|target| expanded_copy(target, alias.syntax(), &registry, &mut Vec::new()))
        .and_then(|copy| crate::as_yaml::YamlNode::from_syntax_peeled(copy).ok_or_else(undefined))
        .map_err(|e| undefined_alias(e, segment))
}

/// Build a detached copy of an anchored node to substitute for the alias
/// `outer`.
///
/// The copy is dedented to column 0, as a detached root has no context to
/// measure its indentation from. It carries no anchors: the `&anchor` beside
/// the original stays behind in its parent VALUE, and anchors nested inside
/// are dropped so that later aliases keep referring to the original nodes.
///
/// An alias inside the copy that would refer to another node in the place
/// of `outer`, because the anchor it referred to was dropped from the copy
/// or is defined again before `outer`, is replaced with a copy of the node
/// it referred to, made the same way. `open` holds the nodes being copied,
/// to stop at an alias that refers to a node containing it.
fn expanded_copy(
    target: &crate::yaml::SyntaxNode,
    outer: &crate::yaml::SyntaxNode,
    registry: &crate::anchor_resolution::AnchorRegistry,
    open: &mut Vec<crate::yaml::SyntaxNode>,
) -> Result<crate::yaml::SyntaxNode, crate::anchor_resolution::AliasError> {
    use crate::anchor_resolution::AliasError;
    use crate::lex::SyntaxKind;
    use rowan::ast::AstNode;

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

    let aliases = |node: &crate::yaml::SyntaxNode| -> Vec<crate::yaml::SyntaxNode> {
        node.descendants()
            .filter(|n| n.kind() == SyntaxKind::ALIAS)
            .collect()
    };
    let (originals, copies) = (aliases(target), aliases(&copy));
    assert_eq!(
        originals.len(),
        copies.len(),
        "a copy has the aliases of its original"
    );
    open.push(target.clone());
    for (original, copied) in originals.into_iter().zip(copies) {
        let original = crate::yaml::Alias::cast(original).expect("an ALIAS node is an alias");
        let name = original.name();
        let Some(referred) = registry.resolve_alias_at(&name, original.syntax()) else {
            // Already undefined where it was.
            continue;
        };
        if registry.resolve_alias_at(&name, outer) == Some(referred) {
            continue;
        }
        let node = registry.resolve_alias_through(&original)?;
        let node = match node.kind() {
            SyntaxKind::VALUE => node
                .children()
                .next()
                .ok_or_else(|| AliasError::Undefined {
                    alias: name.clone(),
                })?,
            _ => node.clone(),
        };
        if open.contains(&node) {
            return Err(AliasError::Recursive { alias: name });
        }
        let replacement = expanded_copy(&node, outer, registry, open)?;
        crate::nodes::replace_alias_with_copy(&copied, &replacement);
    }
    open.pop();
    Ok(copy)
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
