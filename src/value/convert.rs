//! Conversion of syntax nodes to [`YamlValue`]s, following aliases and
//! applying merge keys.

use crate::anchor_resolution::{
    is_merge_key, merge_sources, resolve_alias_node, AliasError, AnchorRegistry, MergedMapping,
    ResolveError,
};
use crate::as_yaml::{NodeIdentity, YamlNode};
use crate::lex::SyntaxKind;
use crate::value::YamlValue;
use crate::yaml::SyntaxNode;
use rowan::ast::AstNode;
use std::collections::hash_map::{Entry, HashMap};

/// The identity of `key` as a mapping key, following aliases through
/// `registry`; see [`NodeIdentity`]. Without a registry, an alias is a
/// [`ResolveError::UnresolvedAlias`].
fn key_identity(
    key: &YamlNode,
    registry: Option<&AnchorRegistry>,
) -> Result<NodeIdentity, ResolveError> {
    crate::as_yaml::key_identity(key, registry).map_err(|e| match (registry, e) {
        (None, AliasError::Undefined { alias }) => ResolveError::UnresolvedAlias { alias },
        (_, e) => e.into(),
    })
}

/// The node `alias` refers to through `registry`, as the anchored node
/// and its content, and the registry itself.
///
/// An alias to one of the nodes in `expanding` is recursive.
fn alias_target<'r>(
    alias: &crate::yaml::Alias,
    registry: Option<&'r AnchorRegistry>,
    expanding: &[SyntaxNode],
) -> Result<(&'r AnchorRegistry, SyntaxNode, YamlNode), ResolveError> {
    let name = alias.name();
    let Some(registry) = registry else {
        return Err(ResolveError::UnresolvedAlias { alias: name });
    };
    let target = registry
        .resolve_alias_at(&name, alias.syntax())
        .ok_or_else(|| AliasError::Undefined {
            alias: name.clone(),
        })?;
    if expanding.contains(target) {
        return Err(AliasError::Recursive { alias: name }.into());
    }
    let node = YamlNode::from_syntax_peeled(target.clone())
        .ok_or(AliasError::Undefined { alias: name })?;
    Ok((registry, target.clone(), node))
}

/// Apply merge keys to create a new merged mapping (returns BTreeMap for YamlValue::Mapping)
///
/// Direct keys win over merged ones, and merge sources are taken in the
/// precedence order [`merge_sources`] returns them. Keys are compared by
/// [`key_identity`], so a direct `16` shadows a merged `0x10`; within one
/// mapping, base or merged, an equal key is a
/// [`ResolveError::DuplicateKey`]. Each key is stored under its
/// [`NodeIdentity::key_string`], and distinct keys with the same string
/// form, such as `1` and `'1'`, are a [`ResolveError::KeyCollision`].
/// Without a registry, `<<` is an ordinary key and an alias key is a
/// [`ResolveError::UnresolvedAlias`].
fn apply_merge_keys(
    mapping: &crate::yaml::Mapping,
    registry: Option<&AnchorRegistry>,
    expanding: &mut Vec<SyntaxNode>,
) -> Result<std::collections::BTreeMap<String, YamlValue>, ResolveError> {
    let mut merged_pairs = std::collections::BTreeMap::new();
    let mut seen = SeenKeys::default();
    let sources = match registry {
        Some(registry) => merge_sources(mapping, registry)?,
        None => Vec::new(),
    };
    for source in std::iter::once(mapping).chain(&sources) {
        // The identities of the keys of this mapping, and the keys as
        // written.
        let mut own_keys: HashMap<NodeIdentity, YamlNode> = HashMap::new();
        for (key, value) in source.iter() {
            check_single_node_in_parent(key.syntax())?;
            if registry.is_some() && is_merge_key(&key) {
                continue;
            }
            let identity = key_identity(&key, registry)?;
            match own_keys.entry(identity.clone()) {
                Entry::Occupied(first) => {
                    return Err(ResolveError::DuplicateKey {
                        first: first.get().to_string(),
                        second: key.to_string(),
                    });
                }
                Entry::Vacant(slot) => {
                    slot.insert(key.clone());
                }
            }
            // An earlier mapping, the base or a merged one, shadows this key.
            if seen.by_identity.contains_key(&identity) {
                continue;
            }
            let key_str = seen.record(identity, key)?;
            if let Some(yaml_value) = resolved_value(value, registry, expanding)? {
                merged_pairs.insert(key_str, yaml_value);
            }
        }
    }
    Ok(merged_pairs)
}

/// Fail if `slot`, a node that holds a single node such as a VALUE, holds
/// more than one, so converting it would drop the rest.
fn check_single_node(slot: &SyntaxNode) -> Result<(), ResolveError> {
    let extra = slot
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
        .nth(1);
    match extra {
        Some(extra) => Err(ResolveError::ExtraNode {
            node: extra.text().to_string(),
        }),
        None => Ok(()),
    }
}

/// Like [`check_single_node`], for the slot `node` sits in.
fn check_single_node_in_parent(node: &SyntaxNode) -> Result<(), ResolveError> {
    match node.parent() {
        Some(parent)
            if matches!(
                parent.kind(),
                SyntaxKind::DOCUMENT
                    | SyntaxKind::KEY
                    | SyntaxKind::VALUE
                    | SyntaxKind::SEQUENCE_ENTRY
                    | SyntaxKind::TAGGED_NODE
            ) =>
        {
            check_single_node(&parent)
        }
        _ => Ok(()),
    }
}

/// Turn `node` into a [`YamlValue`], following aliases and applying merge
/// keys at every level.
///
/// Without a registry, as for [`YamlValue::cast`], an alias is a
/// [`ResolveError::UnresolvedAlias`] and merge keys are not applied.
///
/// `expanding` holds the anchored nodes currently being expanded, so an alias
/// that refers back to one of them is reported as recursive.
pub(crate) fn resolved_value(
    node: YamlNode,
    registry: Option<&AnchorRegistry>,
    expanding: &mut Vec<SyntaxNode>,
) -> Result<Option<YamlValue>, ResolveError> {
    check_single_node_in_parent(node.syntax())?;
    match node {
        YamlNode::Alias(alias) => {
            let (registry, target, target_node) = alias_target(&alias, registry, expanding)?;
            expanding.push(target);
            let value = resolved_value(target_node, Some(registry), expanding);
            expanding.pop();
            value
        }
        YamlNode::Mapping(mapping) => Ok(Some(YamlValue::Mapping(apply_merge_keys(
            &mapping, registry, expanding,
        )?))),
        YamlNode::Sequence(seq) => {
            let mut items = Vec::new();
            for item in seq.values() {
                items.extend(resolved_value(item, registry, expanding)?);
            }
            Ok(Some(YamlValue::Sequence(items)))
        }
        YamlNode::TaggedNode(tagged) => resolved_tagged(&tagged, registry, expanding),
        YamlNode::Scalar(scalar) => Ok(Some(YamlValue::Scalar(
            crate::scalar::ScalarValue::from_scalar(&scalar),
        ))),
    }
}

/// Resolve a tagged node like [`resolved_value`]: `!!set`, `!!omap` and
/// `!!pairs` get their own [`YamlValue`] variants, other tagged
/// collections are [`YamlValue::Tagged`] and a tagged scalar keeps its tag
/// on the [`ScalarValue`](crate::ScalarValue).
///
/// A tag on an alias, which YAML does not allow but the parser accepts,
/// applies to the node the alias refers to, as if the alias were a copy of
/// it.
fn resolved_tagged(
    tagged: &crate::yaml::TaggedNode,
    registry: Option<&AnchorRegistry>,
    expanding: &mut Vec<SyntaxNode>,
) -> Result<Option<YamlValue>, ResolveError> {
    check_single_node(tagged.syntax())?;
    let content = tagged.syntax().children().find_map(YamlNode::from_syntax);
    let (Some(tag), Some(content)) = (tagged.tag(), content) else {
        return Ok(crate::scalar::ScalarValue::from_tagged(tagged).map(YamlValue::Scalar));
    };
    match content {
        YamlNode::Alias(alias) => resolved_tagged_alias(&tag, &alias, registry, expanding),
        content => resolved_with_tag(&tag, content, registry, expanding),
    }
}

/// Resolve `alias`, which has tag `tag`, to the node it refers to under
/// that tag.
fn resolved_tagged_alias(
    tag: &str,
    alias: &crate::yaml::Alias,
    registry: Option<&AnchorRegistry>,
    expanding: &mut Vec<SyntaxNode>,
) -> Result<Option<YamlValue>, ResolveError> {
    let (registry, target, target_node) = alias_target(alias, registry, expanding)?;
    expanding.push(target);
    let value = match target_node {
        YamlNode::Alias(inner) => resolved_tagged_alias(tag, &inner, Some(registry), expanding),
        // The tag of the alias takes the place of that of the node.
        YamlNode::TaggedNode(tagged) => {
            match tagged.syntax().children().find_map(YamlNode::from_syntax) {
                Some(YamlNode::Alias(inner)) => {
                    resolved_tagged_alias(tag, &inner, Some(registry), expanding)
                }
                Some(content) => resolved_with_tag(tag, content, Some(registry), expanding),
                None => Ok(crate::scalar::ScalarValue::from_tagged(&tagged)
                    .map(|s| YamlValue::Scalar(s.with_resolved_tag(tag.to_string())))),
            }
        }
        content => resolved_with_tag(tag, content, Some(registry), expanding),
    };
    expanding.pop();
    value
}

/// Resolve `content`, an untagged scalar, mapping or sequence, as if it
/// had tag `tag`.
fn resolved_with_tag(
    tag: &str,
    content: YamlNode,
    registry: Option<&AnchorRegistry>,
    expanding: &mut Vec<SyntaxNode>,
) -> Result<Option<YamlValue>, ResolveError> {
    match (crate::tag::normalize(tag).as_ref(), content) {
        (crate::tag::SET, YamlNode::Mapping(mapping)) => {
            resolved_set(&mapping, registry, expanding).map(|set| Some(YamlValue::Set(set)))
        }
        (crate::tag::SET, content) => Err(ResolveError::InvalidSetContent {
            content: content.syntax().text().to_string(),
        }),
        (crate::tag::OMAP, YamlNode::Sequence(seq)) => Ok(Some(YamlValue::OrderedMapping(
            resolved_pairs(tag, &seq, registry, expanding)?,
        ))),
        (crate::tag::PAIRS, YamlNode::Sequence(seq)) => Ok(Some(YamlValue::Pairs(resolved_pairs(
            tag, &seq, registry, expanding,
        )?))),
        (crate::tag::OMAP | crate::tag::PAIRS, content) => Err(ResolveError::InvalidPairsContent {
            tag: tag.to_string(),
            content: content.syntax().text().to_string(),
        }),
        (_, YamlNode::Scalar(scalar)) => Ok(Some(YamlValue::Scalar(
            crate::scalar::ScalarValue::from_scalar(&scalar).with_resolved_tag(tag.to_string()),
        ))),
        (_, content) => Ok(resolved_value(content, registry, expanding)?
            .map(|value| YamlValue::Tagged(tag.to_string(), Box::new(value)))),
    }
}

/// The members of a `!!set` whose content is `mapping`.
fn resolved_set(
    mapping: &crate::yaml::Mapping,
    registry: Option<&AnchorRegistry>,
    expanding: &mut Vec<SyntaxNode>,
) -> Result<std::collections::BTreeSet<String>, ResolveError> {
    for entry in mapping.entries() {
        let Some(value) = entry.value_node() else {
            continue;
        };
        let is_null = matches!(
            resolved_value(value.clone(), registry, expanding)?,
            Some(YamlValue::Scalar(s)) if s.scalar_type() == crate::scalar::ScalarType::Null
        );
        if !is_null {
            return Err(ResolveError::NonNullSetValue {
                member: entry
                    .key_node()
                    .map_or_else(String::new, |key| key.to_string()),
                value: value.to_string(),
            });
        }
    }
    let mut members = std::collections::BTreeSet::new();
    let mut seen = SeenKeys::default();
    for member in mapping.keys() {
        let Some(YamlValue::Scalar(_)) = resolved_value(member.clone(), registry, expanding)?
        else {
            return Err(ResolveError::InvalidSetMember {
                member: member.to_string(),
            });
        };
        let identity = key_identity(&member, registry)?;
        members.insert(seen.record(identity, member)?);
    }
    Ok(members)
}

/// The entries of a `!!omap` or `!!pairs` sequence tagged `tag`, in order.
///
/// Each item has to be a mapping, untagged or tagged `!!map`, with a single
/// entry.
fn resolved_pairs(
    tag: &str,
    seq: &crate::yaml::Sequence,
    registry: Option<&AnchorRegistry>,
    expanding: &mut Vec<SyntaxNode>,
) -> Result<Vec<(String, YamlValue)>, ResolveError> {
    let mut pairs = Vec::new();
    let mut seen = SeenKeys::default();
    for item in seq.values() {
        let entries = match resolved_value(item.clone(), registry, expanding)? {
            Some(YamlValue::Mapping(entries)) => Some(entries),
            Some(YamlValue::Tagged(t, inner)) if crate::tag::normalize(&t) == crate::tag::MAP => {
                match *inner {
                    YamlValue::Mapping(entries) => Some(entries),
                    _ => None,
                }
            }
            _ => None,
        }
        .filter(|entries| entries.len() == 1)
        .ok_or_else(|| ResolveError::InvalidPairsItem {
            tag: tag.to_string(),
            item: item.to_string(),
        })?;
        if crate::tag::normalize(tag) == crate::tag::OMAP {
            let key = pair_key(tag, &item, registry)?;
            seen.record(key_identity(&key, registry)?, key)?;
        }
        pairs.extend(entries);
    }
    Ok(pairs)
}

/// The key of `item`, a `!!omap` or `!!pairs` item that converts to a
/// mapping with a single entry, as written.
fn pair_key(
    tag: &str,
    item: &YamlNode,
    registry: Option<&AnchorRegistry>,
) -> Result<YamlNode, ResolveError> {
    let invalid = || ResolveError::InvalidPairsItem {
        tag: tag.to_string(),
        item: item.to_string(),
    };
    let resolved = match registry {
        Some(registry) => resolve_alias_node(item.clone(), registry)?,
        None => item.clone(),
    };
    let mapping = match resolved {
        YamlNode::Mapping(mapping) => mapping,
        YamlNode::TaggedNode(tagged) => tagged.as_mapping().ok_or_else(invalid)?,
        _ => return Err(invalid()),
    };
    let key = match registry {
        Some(registry) => MergedMapping::new(mapping, registry)?.keys().next(),
        None => mapping.keys().next(),
    };
    key.ok_or_else(invalid)
}

/// The keys seen so far in one mapping, `!!set` or `!!omap`, as written,
/// by their identity and by their string form.
#[derive(Default)]
struct SeenKeys {
    by_identity: HashMap<NodeIdentity, YamlNode>,
    by_string: HashMap<String, YamlNode>,
}

impl SeenKeys {
    /// Add `key`, as written, with identity `identity`, and return its
    /// string form.
    ///
    /// A key equal to one seen before is a [`ResolveError::DuplicateKey`],
    /// and a distinct key with the same string form a
    /// [`ResolveError::KeyCollision`].
    fn record(&mut self, identity: NodeIdentity, key: YamlNode) -> Result<String, ResolveError> {
        if let Some(first) = self.by_identity.get(&identity) {
            return Err(ResolveError::DuplicateKey {
                first: first.to_string(),
                second: key.to_string(),
            });
        }
        let key_str = identity.key_string();
        if let Some(first) = self.by_string.get(&key_str) {
            return Err(ResolveError::KeyCollision {
                first: first.to_string(),
                second: key.to_string(),
            });
        }
        self.by_identity.insert(identity, key.clone());
        self.by_string.insert(key_str.clone(), key);
        Ok(key_str)
    }
}
