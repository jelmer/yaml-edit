//! Following a path down to the value it names.

use super::alias::{registry_for, resolve_alias, undefined_alias};
use super::parse::segment_display;
use super::{AliasPolicy, PathError, PathSegment};
use crate::yaml::Mapping;

/// Navigate through a YAML structure following path segments.
///
/// Handles both mapping keys and sequence indices. A numeric segment
/// like `997` in `foo.997` parses as `Index(997)` even though the user
/// may have meant it as a mapping key: when the current node is a
/// mapping, fall back to looking up the stringified form. Errors
/// distinguish "wrong container type" from "key/index not present".
pub(super) fn navigate_path(
    mut current: crate::as_yaml::YamlNode,
    segments: &[PathSegment],
    policy: AliasPolicy,
) -> Result<crate::as_yaml::YamlNode, PathError> {
    let mut holder = None;
    for segment in segments {
        current = descend_one(current, holder, segment, policy)?;
        holder = Some(segment);
    }
    Ok(current)
}

/// Resolve `current` if it is an alias, according to `policy`.
///
/// `segment` is only used to label errors: it is the segment that
/// produced `current`.
pub(super) fn resolve_for_read(
    current: crate::as_yaml::YamlNode,
    segment: &PathSegment,
    policy: AliasPolicy,
) -> Result<crate::as_yaml::YamlNode, PathError> {
    let crate::as_yaml::YamlNode::Alias(alias) = &current else {
        return Ok(current);
    };
    // A read leaves the tree alone, so there is nothing to expand into.
    let policy = match policy {
        AliasPolicy::Expand => AliasPolicy::Follow,
        policy => policy,
    };
    resolve_alias(alias, segment, policy)
}

/// Find the first scalar key spelled `segment`, whatever type it
/// resolves to, so a path can name keys such as `1`, `true` or `0x10`.
/// The match is on the key text: `m.16` does not reach a `0x10` key.
pub(super) fn spelled_key(
    mut keys: impl Iterator<Item = crate::as_yaml::YamlNode>,
    segment: &str,
) -> Option<crate::as_yaml::YamlNode> {
    keys.find(|k| k.as_scalar().is_some_and(|s| s.as_string() == segment))
}

/// Look up the key a path segment names in `mapping`'s own entries: a
/// string key equal to `key` if there is one, otherwise the first scalar
/// key spelled `key`.
fn lookup_own(mapping: &Mapping, key: &str) -> Option<crate::as_yaml::YamlNode> {
    mapping
        .get(key)
        .or_else(|| mapping.get(spelled_key(mapping.keys(), key)?))
}

/// Look up `key` in `mapping`, falling back to the keys it inherits
/// through `<<:` merge keys.
///
/// A direct entry always wins: YAML 1.1 says a mapping's own key
/// shadows one merged in from an anchor, so the merge keys are only
/// checked when there is no direct entry.
fn lookup_merged(
    mapping: &Mapping,
    key: &str,
    segment: &PathSegment,
) -> Result<crate::as_yaml::YamlNode, PathError> {
    use crate::anchor_resolution::{MergeError, MergedMapping};
    if let Some(found) = lookup_own(mapping, key) {
        return Ok(found);
    }
    let at = segment_display(segment);
    let registry = registry_for(rowan::ast::AstNode::syntax(mapping));
    let merged = MergedMapping::build(mapping.clone(), &registry).map_err(|e| match e {
        // A key holding an alias within its own anchored node is recursive.
        MergeError::Alias(e) => undefined_alias(e, segment),
        MergeError::InvalidOperand { operand } => PathError::InvalidMergeOperand {
            at: at.clone(),
            operand,
        },
    })?;
    merged
        .get(key)
        .or_else(|| merged.get(spelled_key(merged.keys(), key)?))
        .ok_or(PathError::NotFound { at })
}

/// Apply `segment` to `current`, the value of segment `holder` (`None`
/// for the root, whose errors name `segment` instead).
pub(super) fn descend_one(
    current: crate::as_yaml::YamlNode,
    holder: Option<&PathSegment>,
    segment: &PathSegment,
    policy: AliasPolicy,
) -> Result<crate::as_yaml::YamlNode, PathError> {
    let holder = holder.unwrap_or(segment);
    let current = resolve_for_read(current, holder, policy)?;
    let mismatch = || PathError::TypeMismatch {
        at: segment_display(holder),
    };
    match segment {
        PathSegment::Key(key) => {
            let mapping = current.mapping_content().ok_or_else(mismatch)?;
            lookup_merged(&mapping, key, segment)
        }
        PathSegment::Index(index) => {
            if let Some(seq) = current.sequence_content() {
                seq.get(*index).ok_or_else(|| PathError::NotFound {
                    at: segment_display(segment),
                })
            } else if let Some(map) = current.mapping_content() {
                // Fallback: numeric segment used as a mapping key.
                lookup_merged(&map, &index.to_string(), segment)
            } else {
                Err(mismatch())
            }
        }
    }
}
