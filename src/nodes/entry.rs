//! Entry API for [`Mapping`](crate::Mapping).
//!
//! Modelled on [`std::collections::BTreeMap::entry`]. Obtain an [`Entry`]
//! via [`Mapping::entry`](crate::Mapping::entry).
//!
//! ```rust
//! use std::str::FromStr;
//! use yaml_edit::Document;
//!
//! let doc = Document::from_str("name: Alice\n").unwrap();
//! let mapping = doc.as_mapping().unwrap();
//!
//! mapping.entry("age").or_insert(30);
//! mapping.entry("name").or_insert("Bob");
//!
//! assert_eq!(doc.to_string(), "name: Alice\nage: 30\n");
//! ```

use crate::as_yaml::{AsYaml, YamlNode};
use crate::nodes::mapping::{Mapping, MappingEntry};
use crate::nodes::sequence::Sequence;

/// A view into a single entry in a [`Mapping`], which may either be vacant
/// or occupied.
///
/// Obtained from [`Mapping::entry`](crate::Mapping::entry).
#[derive(Debug)]
pub enum Entry<'a, K: AsYaml> {
    /// The key is already present in the mapping.
    Occupied(OccupiedEntry<'a>),
    /// The key is not present in the mapping.
    Vacant(VacantEntry<'a, K>),
}

/// A view into an occupied entry in a [`Mapping`].
#[derive(Debug)]
pub struct OccupiedEntry<'a> {
    pub(crate) mapping: &'a Mapping,
    pub(crate) entry: MappingEntry,
}

/// A view into a vacant entry in a [`Mapping`].
#[derive(Debug)]
pub struct VacantEntry<'a, K: AsYaml> {
    pub(crate) mapping: &'a Mapping,
    pub(crate) key: K,
}

impl<'a, K: AsYaml> Entry<'a, K> {
    /// Ensure a value is in the entry by inserting `default` if vacant, and
    /// return the resulting value as a [`YamlNode`].
    pub fn or_insert(self, default: impl AsYaml) -> YamlNode {
        match self {
            Entry::Occupied(o) => o.into_value(),
            Entry::Vacant(v) => v.insert(default),
        }
    }

    /// Ensure a value is in the entry by inserting the result of `default` if
    /// vacant, and return the resulting value as a [`YamlNode`].
    ///
    /// `default` is only called if the entry is vacant.
    pub fn or_insert_with<V, F>(self, default: F) -> YamlNode
    where
        V: AsYaml,
        F: FnOnce() -> V,
    {
        match self {
            Entry::Occupied(o) => o.into_value(),
            Entry::Vacant(v) => v.insert(default()),
        }
    }

    /// Ensure the entry holds a [`Mapping`] and return it.
    ///
    /// If the entry is vacant, an empty mapping is inserted. If the entry is
    /// occupied but the value is not a mapping, it is replaced with an empty
    /// mapping. Otherwise the existing nested mapping is returned unchanged.
    pub fn or_insert_mapping(self) -> Mapping {
        match self {
            Entry::Occupied(o) => {
                if let Some(existing) = o.get_mapping() {
                    return existing;
                }
                o.set_value(Mapping::new());
                o.get_mapping()
                    .expect("value was just replaced with a mapping")
            }
            Entry::Vacant(v) => {
                let VacantEntry { mapping, key } = v;
                mapping.set(&key, Mapping::new());
                mapping
                    .get_mapping(&key)
                    .expect("mapping was just inserted")
            }
        }
    }

    /// Ensure the entry holds a [`Sequence`] and return it.
    ///
    /// If the entry is vacant, an empty sequence is inserted. If the entry is
    /// occupied but the value is not a sequence, it is replaced with an empty
    /// sequence. Otherwise the existing nested sequence is returned unchanged.
    pub fn or_insert_sequence(self) -> Sequence {
        match self {
            Entry::Occupied(o) => {
                if let Some(existing) = o.get_sequence() {
                    return existing;
                }
                o.set_value(Sequence::new());
                o.get_sequence()
                    .expect("value was just replaced with a sequence")
            }
            Entry::Vacant(v) => {
                let VacantEntry { mapping, key } = v;
                mapping.set(&key, Sequence::new());
                mapping
                    .get_sequence(&key)
                    .expect("sequence was just inserted")
            }
        }
    }

    /// Provide in-place access to an occupied entry before any potential
    /// inserts.
    ///
    /// The closure is only called if the entry is occupied. Returns `self`
    /// unchanged so calls can be chained with [`or_insert`](Self::or_insert)
    /// and friends.
    ///
    /// The tree uses interior mutability, so the closure receives a shared
    /// reference; `OccupiedEntry::set_value` and friends still work through
    /// `&self`.
    pub fn and_modify<F>(self, f: F) -> Self
    where
        F: FnOnce(&OccupiedEntry<'a>),
    {
        if let Entry::Occupied(ref o) = self {
            f(o);
        }
        self
    }

    /// Return the key that this entry refers to, as a [`YamlNode`].
    ///
    /// For an occupied entry this is the existing key node in the tree. For
    /// a vacant entry it is the key that would be inserted, rendered as a
    /// fresh scalar node.
    pub fn key(&self) -> Option<YamlNode> {
        match self {
            Entry::Occupied(o) => o.key(),
            Entry::Vacant(v) => key_as_yaml_node(&v.key),
        }
    }
}

impl<'a> OccupiedEntry<'a> {
    /// Return the key of this entry as a [`YamlNode`].
    ///
    /// Returns `None` for malformed entries with no key node.
    pub fn key(&self) -> Option<YamlNode> {
        self.entry.key_node()
    }

    /// Return the current value of this entry as a [`YamlNode`].
    ///
    /// Returns `None` for malformed entries with no value node.
    pub fn get(&self) -> Option<YamlNode> {
        self.entry.value_node()
    }

    /// Return the value as a nested [`Mapping`], if it is one.
    pub fn get_mapping(&self) -> Option<Mapping> {
        self.get().and_then(|n| n.as_mapping().cloned())
    }

    /// Return the value as a nested [`Sequence`], if it is one.
    pub fn get_sequence(&self) -> Option<Sequence> {
        self.get().and_then(|n| n.as_sequence().cloned())
    }

    /// Replace the value of this entry with `value`.
    pub fn set_value(&self, value: impl AsYaml) {
        let flow_context = self.mapping.is_flow_style();
        self.entry.set_value(value, flow_context);
    }

    /// Remove this entry from the mapping and return the removed
    /// [`MappingEntry`] handle. The returned entry is detached from the tree.
    pub fn remove_entry(self) -> MappingEntry {
        self.entry.clone().remove();
        self.entry
    }

    /// Consume the [`OccupiedEntry`] and return the current value as a
    /// [`YamlNode`].
    ///
    /// Panics if the entry has no value node (indicates malformed input).
    pub(crate) fn into_value(self) -> YamlNode {
        self.entry
            .value_node()
            .expect("occupied entry has no value node")
    }
}

impl<'a, K: AsYaml> VacantEntry<'a, K> {
    /// Return a reference to the pending key.
    pub fn key(&self) -> &K {
        &self.key
    }

    /// Consume the [`VacantEntry`] and return the key that would have been
    /// inserted.
    pub fn into_key(self) -> K {
        self.key
    }

    /// Insert `value` into the mapping at this vacant entry's key, and
    /// return the newly inserted value as a [`YamlNode`].
    pub fn insert(self, value: impl AsYaml) -> YamlNode {
        self.mapping.set(&self.key, value);
        self.mapping
            .get(&self.key)
            .expect("value just inserted must be retrievable")
    }
}

fn key_as_yaml_node<K: AsYaml>(key: &K) -> Option<YamlNode> {
    use crate::lex::SyntaxKind;
    use rowan::GreenNodeBuilder;

    if let Some(node) = key.as_node() {
        return YamlNode::from_syntax(node.clone());
    }
    let mut builder = GreenNodeBuilder::new();
    builder.start_node(SyntaxKind::ROOT.into());
    key.build_content(&mut builder, 0, false);
    builder.finish_node();
    let root = crate::nodes::SyntaxNode::new_root(builder.finish());
    root.children().next().and_then(YamlNode::from_syntax)
}

#[cfg(test)]
mod tests {
    use crate::{Document, Entry};
    use std::str::FromStr;

    #[test]
    fn or_insert_on_vacant_inserts_and_returns_value() {
        let doc = Document::from_str("name: Alice\n").unwrap();
        let mapping = doc.as_mapping().unwrap();

        let v = mapping.entry("age").or_insert(30);
        assert_eq!(v.to_string().trim(), "30");
        assert_eq!(doc.to_string(), "name: Alice\nage: 30\n");
    }

    #[test]
    fn or_insert_on_occupied_returns_existing_value_and_does_not_overwrite() {
        let doc = Document::from_str("name: Alice\n").unwrap();
        let mapping = doc.as_mapping().unwrap();

        let v = mapping.entry("name").or_insert("Bob");
        assert_eq!(v.to_string().trim(), "Alice");
        assert_eq!(doc.to_string(), "name: Alice\n");
    }

    #[test]
    fn or_insert_with_only_calls_closure_when_vacant() {
        let doc = Document::from_str("count: 5\n").unwrap();
        let mapping = doc.as_mapping().unwrap();

        let mut called = false;
        let _ = mapping.entry("count").or_insert_with(|| {
            called = true;
            99_i64
        });
        assert!(!called);

        let mut called = false;
        let _ = mapping.entry("other").or_insert_with(|| {
            called = true;
            123_i64
        });
        assert!(called);
        assert_eq!(doc.to_string(), "count: 5\nother: 123\n");
    }

    #[test]
    fn and_modify_on_occupied_runs_closure_and_updates_value() {
        let doc = Document::from_str("count: 1\n").unwrap();
        let mapping = doc.as_mapping().unwrap();

        mapping.entry("count").and_modify(|e| e.set_value(42));
        assert_eq!(doc.to_string(), "count: 42\n");
    }

    #[test]
    fn and_modify_on_vacant_is_noop() {
        let doc = Document::from_str("count: 1\n").unwrap();
        let mapping = doc.as_mapping().unwrap();

        let mut called = false;
        mapping.entry("missing").and_modify(|_| {
            called = true;
        });
        assert!(!called);
        assert_eq!(doc.to_string(), "count: 1\n");
    }

    #[test]
    fn and_modify_then_or_insert_pattern() {
        let doc = Document::from_str("count: 1\n").unwrap();
        let mapping = doc.as_mapping().unwrap();

        mapping
            .entry("count")
            .and_modify(|e| e.set_value(2))
            .or_insert(0);
        assert_eq!(mapping.get("count").unwrap().to_string().trim(), "2");

        mapping
            .entry("new")
            .and_modify(|e| e.set_value("never"))
            .or_insert("first");
        assert_eq!(mapping.get("new").unwrap().to_string().trim(), "first");
    }

    #[test]
    fn entry_variants_expose_expected_state() {
        let doc = Document::from_str("k: v\n").unwrap();
        let mapping = doc.as_mapping().unwrap();

        match mapping.entry("k") {
            Entry::Occupied(o) => {
                assert_eq!(o.key().unwrap().to_string().trim(), "k");
                assert_eq!(o.get().unwrap().to_string().trim(), "v");
            }
            Entry::Vacant(_) => panic!("expected occupied"),
        }

        match mapping.entry("missing") {
            Entry::Vacant(v) => {
                assert_eq!(*v.key(), "missing");
            }
            Entry::Occupied(_) => panic!("expected vacant"),
        }
    }

    #[test]
    fn entry_key_agnostic_of_variant() {
        let doc = Document::from_str("k: v\n").unwrap();
        let mapping = doc.as_mapping().unwrap();

        let occupied_key = mapping.entry("k").key().unwrap();
        assert_eq!(occupied_key.to_string().trim(), "k");

        let vacant_key = mapping.entry("missing").key().unwrap();
        assert_eq!(vacant_key.to_string().trim(), "missing");
    }

    #[test]
    fn vacant_entry_insert_returns_new_value_node() {
        let doc = Document::from_str("a: 1\n").unwrap();
        let mapping = doc.as_mapping().unwrap();

        let Entry::Vacant(v) = mapping.entry("b") else {
            panic!("expected vacant");
        };
        let node = v.insert(2_i64);
        assert_eq!(node.to_string().trim(), "2");
        assert_eq!(doc.to_string(), "a: 1\nb: 2\n");
    }

    #[test]
    fn occupied_entry_remove_detaches_from_mapping() {
        let doc = Document::from_str("a: 1\nb: 2\n").unwrap();
        let mapping = doc.as_mapping().unwrap();

        let Entry::Occupied(o) = mapping.entry("a") else {
            panic!("expected occupied");
        };
        let removed = o.remove_entry();
        assert_eq!(removed.key_node().unwrap().to_string().trim(), "a");
        assert!(!mapping.contains_key("a"));
        assert_eq!(doc.to_string(), "b: 2\n");
    }

    #[test]
    fn or_insert_mapping_on_vacant_creates_and_returns_nested_mapping() {
        let doc = Document::from_str("existing: value\n").unwrap();
        let mapping = doc.as_mapping().unwrap();

        let nested = mapping.entry("nested").or_insert_mapping();
        nested.set("inner", "value");
        assert_eq!(
            doc.to_string(),
            "existing: value\nnested:\n  inner: value\n"
        );
    }

    #[test]
    fn or_insert_mapping_on_occupied_returns_existing_mapping() {
        let doc = Document::from_str("root:\n  a: 1\n").unwrap();
        let mapping = doc.as_mapping().unwrap();

        let nested = mapping.entry("root").or_insert_mapping();
        nested.set("b", 2);
        assert_eq!(doc.to_string(), "root:\n  a: 1\n  b: 2\n");
    }

    #[test]
    fn or_insert_sequence_on_vacant_creates_and_returns_nested_sequence() {
        let doc = Document::from_str("existing: value\n").unwrap();
        let mapping = doc.as_mapping().unwrap();

        let list = mapping.entry("items").or_insert_sequence();
        list.push("apple");
        list.push("banana");
        assert_eq!(
            doc.to_string(),
            "existing: value\nitems:\n  - apple\n  - banana\n"
        );
    }

    #[test]
    fn or_insert_sequence_on_occupied_returns_existing_sequence() {
        let doc = Document::from_str("items:\n  - apple\n").unwrap();
        let mapping = doc.as_mapping().unwrap();

        let list = mapping.entry("items").or_insert_sequence();
        list.push("banana");
        assert_eq!(doc.to_string(), "items:\n  - apple\n  - banana\n");
    }

    #[test]
    fn entry_preserves_comments_on_untouched_keys() {
        let src = "keep: this  # inline\nother: value\n";
        let doc = Document::from_str(src).unwrap();
        let mapping = doc.as_mapping().unwrap();

        mapping.entry("added").or_insert(1_i64);
        assert_eq!(
            doc.to_string(),
            "keep: this  # inline\nother: value\nadded: 1\n"
        );
    }

    #[test]
    fn entry_key_semantic_equality() {
        let doc = Document::from_str("\"name\": Alice\n").unwrap();
        let mapping = doc.as_mapping().unwrap();

        match mapping.entry("name") {
            Entry::Occupied(_) => {}
            Entry::Vacant(_) => panic!("expected occupied"),
        }
    }

    #[test]
    fn or_insert_with_can_return_different_types() {
        let doc = Document::from_str("a: 1\n").unwrap();
        let mapping = doc.as_mapping().unwrap();

        let _ = mapping.entry("s").or_insert_with(|| "hello");
        let _ = mapping.entry("n").or_insert_with(|| 42_i64);
        let _ = mapping.entry("b").or_insert_with(|| true);
        assert_eq!(doc.to_string(), "a: 1\ns: hello\nn: 42\nb: true\n");
    }

    #[test]
    fn or_insert_mapping_replaces_non_mapping_value() {
        let doc = Document::from_str("k: scalar\n").unwrap();
        let mapping = doc.as_mapping().unwrap();

        let nested = mapping.entry("k").or_insert_mapping();
        nested.set("inner", "v");
        assert_eq!(doc.to_string(), "k:\n  inner: v\n");
    }
}
