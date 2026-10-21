//! Setting and removing the value at a path.

use super::alias::resolve_alias;
use super::parse::segment_display;
use super::read::{descend_one, resolve_for_read, spelled_key};
use super::{AliasPolicy, PathError, PathSegment, MAX_INDEX_GROWTH};
use crate::yaml::Mapping;

/// Whether an existing value may be replaced by a container the setter
/// creates. Only a bare scalar may: a tagged scalar, an alias, or a
/// collection of the wrong shape is user data we refuse to overwrite.
fn is_replaceable_scalar(node: &crate::as_yaml::YamlNode) -> bool {
    matches!(node, crate::as_yaml::YamlNode::Scalar(_))
}

/// Remove `key` from `mapping`, returning the value it held. The value is
/// read before removal, so it is the node that was in the tree.
fn take_value(
    mapping: &Mapping,
    key: impl crate::AsYaml + Copy,
) -> Option<crate::as_yaml::YamlNode> {
    let value = mapping.get(key)?;
    mapping.remove(key);
    Some(value)
}

/// Interpret a segment as a mapping key. `Index(n)` is stringified so
/// paths like `foo.997` (parsed as `foo` + `Index(997)`) still address
/// a mapping key `"997"`.
fn segment_key(segment: &PathSegment) -> String {
    match segment {
        PathSegment::Key(key) => key.clone(),
        PathSegment::Index(index) => index.to_string(),
    }
}

/// Remove a value at a nested path. Returns the removed
/// [`YamlNode`](crate::as_yaml::YamlNode) on success. Errors carry the
/// specific reason (type mismatch vs. not-found).
///
/// Removing by index from a sequence is intentionally unsupported (it
/// would shift every subsequent element) and reported as a
/// `PathError::TypeMismatch` at that segment.
pub(super) fn remove_path_impl(
    root: crate::as_yaml::YamlNode,
    holder: Option<&PathSegment>,
    segments: &[PathSegment],
    policy: AliasPolicy,
) -> Result<crate::as_yaml::YamlNode, PathError> {
    debug_assert!(!segments.is_empty(), "caller must reject empty paths");
    let seg = &segments[0];
    if segments.len() > 1 {
        let nested = descend_one(root, holder, seg, policy)?;
        return remove_path_impl(nested, Some(seg), &segments[1..], policy);
    }

    // The node we were handed may itself be an alias (e.g. `a: *shared`
    // then removing `a.timeout`). Resolving here means the final
    // container lookup below sees the anchored mapping.
    let holder = holder.unwrap_or(seg);
    let root = resolve_for_read(root, holder, policy)?;
    let mapping = root.mapping_content().ok_or_else(|| {
        // A numeric segment on a mapping is a stringified key; on a
        // sequence, index removal is unsupported.
        let index_on_sequence =
            matches!(seg, PathSegment::Index(_)) && root.sequence_content().is_some();
        PathError::TypeMismatch {
            at: segment_display(if index_on_sequence { seg } else { holder }),
        }
    })?;
    let key = segment_key(seg);
    take_value(&mapping, key.as_str())
        .or_else(|| take_value(&mapping, &spelled_key(mapping.keys(), &key)?))
        .ok_or_else(|| PathError::NotFound {
            at: segment_display(seg),
        })
}

/// Run `write`, which edits `container`'s child node at `child` (a
/// `children_with_tokens` index) without moving it, and put that child back
/// as it was if the write fails, e.g. when the rest of the path does not
/// fit the copy an expanded alias left behind.
fn undo_child_on_err(
    container: &crate::yaml::SyntaxNode,
    child: usize,
    write: impl FnOnce() -> Result<(), PathError>,
) -> Result<(), PathError> {
    let saved = container
        .children_with_tokens()
        .nth(child)
        .and_then(|c| c.into_node())
        .expect("child names a node")
        .green()
        .into_owned();
    let result = write();
    if result.is_err() {
        let restored = crate::yaml::SyntaxNode::new_root_mut(saved);
        crate::nodes::replace_children(container, child..child + 1, vec![restored.into()]);
    }
    result
}

/// Continue a write into `node`, dispatching on whether it is a mapping
/// or a sequence. `via` is the segment that produced `node`, used only
/// to label a type mismatch.
fn set_path_on_node<V: crate::AsYaml>(
    node: crate::as_yaml::YamlNode,
    segments: &[PathSegment],
    value: V,
    policy: AliasPolicy,
    via: &PathSegment,
) -> Result<(), PathError> {
    if let Some(m) = node.mapping_content() {
        set_path_on_mapping(&m, segments, value, policy)
    } else if let Some(seq) = node.sequence_content() {
        set_path_on_sequence(&seq, segments, value, policy)
    } else {
        Err(PathError::TypeMismatch {
            at: segment_display(via),
        })
    }
}

/// Set a value at a path on a mapping, creating intermediate mappings or
/// sequences as needed.
///
/// Uses only the public API (get_mapping, get_sequence, set) and does NOT
/// rebuild nodes. Recurses through `set_path_on_sequence` when a segment
/// dives into a sequence.
///
/// Errors when an intermediate segment lands on an existing scalar (we
/// won't overwrite user data implicitly).
pub(super) fn set_path_on_mapping<V: crate::AsYaml>(
    mapping: &Mapping,
    segments: &[PathSegment],
    value: V,
    policy: AliasPolicy,
) -> Result<(), PathError> {
    debug_assert!(!segments.is_empty(), "caller must reject empty paths");

    // First segment: numeric segments (from `foo.997`) are stringified
    // so they still address a mapping key. Without a string key of that
    // name, write to an existing scalar key spelled the same.
    let key = segment_key(&segments[0]);
    if mapping.contains_key(key.as_str()) {
        return set_path_in_slot(Slot::Key(mapping, key.as_str()), segments, value, policy);
    }
    match spelled_key(mapping.keys(), &key) {
        Some(existing) => set_path_in_slot(Slot::Key(mapping, &existing), segments, value, policy),
        None => set_path_in_slot(Slot::Key(mapping, key.as_str()), segments, value, policy),
    }
}

/// The place a write continues from: the value of a mapping entry or a
/// sequence item.
enum Slot<'a, K> {
    Key(&'a Mapping, K),
    Index(&'a crate::yaml::Sequence, usize),
}

impl<K: crate::AsYaml + Copy> Slot<'_, K> {
    fn get(&self) -> Option<crate::as_yaml::YamlNode> {
        match *self {
            Slot::Key(mapping, key) => mapping.get(key),
            Slot::Index(sequence, index) => sequence.get(index),
        }
    }

    fn set(&self, value: impl crate::AsYaml) {
        match *self {
            Slot::Key(mapping, key) => mapping.set(key, value),
            Slot::Index(sequence, index) => {
                sequence.set(index, value);
            }
        }
    }

    /// The container node and the `children_with_tokens` index of the
    /// entry holding this slot, which must exist.
    fn entry(&self) -> (&crate::yaml::SyntaxNode, usize) {
        match *self {
            Slot::Key(mapping, key) => (
                &mapping.0,
                mapping
                    .find_entry_index_by_key(key)
                    .expect("get just found this key"),
            ),
            Slot::Index(sequence, index) => (
                &sequence.0,
                crate::nodes::nth_entry_index(
                    &sequence.0,
                    crate::SyntaxKind::SEQUENCE_ENTRY,
                    index,
                )
                .expect("get just found this index"),
            ),
        }
    }

    /// Put an empty sequence or mapping in the slot. Under a mapping the
    /// parent's style is matched so block content is not mixed into a flow
    /// container. Under a sequence it is a flow-empty `[]` or `{}`, which
    /// has no pending-block marker and so stays flow through the
    /// subsequent set.
    fn insert_container(&self, sequence: bool) {
        match *self {
            Slot::Key(mapping, key) => match (sequence, mapping.is_flow_style()) {
                (true, true) => mapping.set(key, crate::yaml::Sequence::new_flow()),
                (true, false) => mapping.set(key, crate::yaml::Sequence::new_pending_block()),
                (false, true) => mapping.set(key, Mapping::new_flow()),
                (false, false) => mapping.set(key, Mapping::new_pending_block()),
            },
            Slot::Index(..) if sequence => self.set(
                crate::builder::SequenceBuilder::new()
                    .build_document()
                    .as_sequence()
                    .expect("SequenceBuilder always produces a sequence"),
            ),
            Slot::Index(..) => self.set(
                crate::builder::MappingBuilder::new()
                    .build_document()
                    .as_mapping()
                    .expect("MappingBuilder always produces a mapping"),
            ),
        }
    }
}

/// Continue a write at `slot`, which `segments[0]` addressed: set the
/// value there if this is the last segment, otherwise descend into the
/// container held there, creating one as needed.
fn set_path_in_slot<K: crate::AsYaml + Copy, V: crate::AsYaml>(
    slot: Slot<'_, K>,
    segments: &[PathSegment],
    value: V,
    policy: AliasPolicy,
) -> Result<(), PathError> {
    if segments.len() == 1 {
        // Replacing an alias outright is not writing *through* it -- no
        // other alias is affected -- so it needs no policy check.
        slot.set(value);
        return Ok(());
    }
    let (at, rest) = (&segments[0], &segments[1..]);
    let mismatch = || PathError::TypeMismatch {
        at: segment_display(at),
    };
    let existing = slot.get();

    // An intermediate alias: either refuse, follow it into the anchored
    // node, or replace it here with an anchor-free copy and continue in
    // that copy.
    if let Some(crate::as_yaml::YamlNode::Alias(alias)) = &existing {
        let resolved = resolve_alias(alias, at, policy)?;
        if policy != AliasPolicy::Expand {
            return set_path_on_node(resolved, rest, value, policy, at);
        }
        let (container, child) = slot.entry();
        return undo_child_on_err(container, child, || {
            slot.set(resolved);
            // Re-read through the container so we descend into the copy
            // that now lives in the tree, not the detached one.
            let target = slot.get().expect("set keeps the entry");
            set_path_on_node(target, rest, value, policy, at)
        });
    }

    // Descending through a scalar would overwrite user data, unless it is
    // a null placeholder (one the user wrote, or one pushed to grow a
    // sequence), which is replaced by the appropriate container.
    if existing
        .as_ref()
        .and_then(|n| n.as_scalar())
        .is_some_and(|sc| !sc.is_null())
    {
        return Err(mismatch());
    }

    let wants_sequence = matches!(rest[0], PathSegment::Index(_));
    if wants_sequence {
        if let Some(nested) = existing.as_ref().and_then(|n| n.sequence_content()) {
            return set_path_on_sequence(&nested, rest, value, policy);
        }
    }
    // Index on an existing mapping is a key (`m.0` / `m[0]`), same as
    // get_path. Do not replace the mapping with a sequence.
    if let Some(nested) = existing.as_ref().and_then(|n| n.mapping_content()) {
        return set_path_on_mapping(&nested, rest, value, policy);
    }
    if existing.as_ref().is_some_and(|n| !is_replaceable_scalar(n)) {
        return Err(mismatch());
    }

    check_new_path(rest)?;
    slot.insert_container(wants_sequence);
    let created = slot.get();
    if wants_sequence {
        let nested = created
            .and_then(|n| n.sequence_content())
            .expect("we just inserted a sequence here");
        set_path_on_sequence(&nested, rest, value, policy)
    } else {
        let nested = created
            .and_then(|n| n.mapping_content())
            .expect("we just inserted a mapping here");
        set_path_on_mapping(&nested, rest, value, policy)
    }
}

/// Check that a write of `segments` into containers about to be created
/// can succeed, before anything is written.
///
/// Every container on the way starts empty, so the only way such a write
/// fails is an index past the growth bound. Writing first and failing
/// later would leave a half-built path behind.
fn check_new_path(segments: &[PathSegment]) -> Result<(), PathError> {
    segments.iter().try_for_each(|segment| match segment {
        PathSegment::Index(index) if *index >= MAX_INDEX_GROWTH => Err(PathError::IndexTooFar {
            at: segment_display(segment),
            len: 0,
            index: *index,
        }),
        _ => Ok(()),
    })
}

/// Set a value at a path on a sequence, growing it and creating intermediate
/// containers as needed.
///
/// The segments slice must start with an `Index`. Missing entries up to
/// `index` are pushed as null scalars; the target entry is replaced (single-
/// segment path) or descended into (multi-segment path).
fn set_path_on_sequence<V: crate::AsYaml>(
    sequence: &crate::yaml::Sequence,
    segments: &[PathSegment],
    value: V,
    policy: AliasPolicy,
) -> Result<(), PathError> {
    debug_assert!(!segments.is_empty(), "caller must reject empty paths");

    let index = match &segments[0] {
        PathSegment::Index(i) => *i,
        // A string segment on a sequence is a real type mismatch: we
        // have no reasonable coercion to try (unlike numeric-on-mapping,
        // which stringifies).
        PathSegment::Key(_) => {
            return Err(PathError::TypeMismatch {
                at: segment_display(&segments[0]),
            });
        }
    };

    // Grow the sequence with null placeholders until `index` is in range,
    // within reason: an index far past the end would otherwise let a single
    // path allocate unboundedly.
    let len = sequence.len();
    // A single sequence edit is linear in its length, so pad in one splice
    // rather than one push per placeholder.
    if index >= len && index - len >= MAX_INDEX_GROWTH {
        return Err(PathError::IndexTooFar {
            at: segment_display(&segments[0]),
            len,
            index,
        });
    }
    if let Some(growth) = (index + 1).checked_sub(len) {
        check_new_path(&segments[1..])?;
        sequence.extend_with(crate::scalar::ScalarValue::null(), growth);
    }

    set_path_in_slot(
        Slot::<&str>::Index(sequence, index),
        segments,
        value,
        policy,
    )
}
