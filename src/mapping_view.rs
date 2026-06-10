//! Read-only mapping interface shared by [`Mapping`] and
//! [`MergedMapping`](crate::anchor_resolution::MergedMapping).
//!
//! [`MappingView`] abstracts over "anything that looks like a YAML mapping
//! for the purposes of reading it." It lets you write code that works
//! generically against both the underlying CST [`Mapping`] and the
//! alias/merge-key–resolving [`MergedMapping`](crate::anchor_resolution::MergedMapping)
//! view.
//!
//! Iterators are returned as boxed trait objects so the trait stays
//! object-safe and works on the crate's MSRV. Most uses pay one heap
//! allocation per iteration — negligible for an editing library.
//!
//! # Example
//!
//! ```
//! use yaml_edit::{Document, MappingView};
//! use yaml_edit::anchor_resolution::{DocumentMergedExt, MappingMergedExt};
//! use std::str::FromStr;
//!
//! fn count_keys(view: &impl MappingView) -> usize {
//!     view.keys().count()
//! }
//!
//! let yaml = "\
//! defaults: &d
//!   timeout: 30
//! prod:
//!   <<: *d
//!   host: prod.example.com
//! ";
//! let doc = Document::from_str(yaml).unwrap();
//! let root = doc.as_mapping().unwrap();
//!
//! // Works on a plain Mapping…
//! assert_eq!(count_keys(&root), 2);
//!
//! // …and on a MergedMapping.
//! let registry = doc.merged().unwrap();
//! let prod = registry.as_mapping().get_merged("prod").unwrap();
//! assert_eq!(count_keys(&prod), 2); // timeout + host
//! ```
//!
//! The trait is intentionally read-only: mutations only make sense on the
//! CST [`Mapping`], so [`set`](crate::yaml::Mapping::set),
//! [`remove`](crate::yaml::Mapping::remove), etc. remain on that type.

use crate::as_yaml::YamlNode;

/// A read-only "view" of a YAML mapping, implemented by both the CST
/// [`Mapping`](crate::yaml::Mapping) and the alias/merge-key–resolving
/// [`MergedMapping`](crate::anchor_resolution::MergedMapping).
///
/// See the [module docs](self) for an overview and example.
pub trait MappingView {
    /// Look up the value associated with `key`.
    ///
    /// Key matching is semantic (quoting style is ignored), so `"foo"`,
    /// `'foo'`, and `foo` all match the scalar `foo`.
    fn get(&self, key: &dyn crate::AsYaml) -> Option<YamlNode>;

    /// Returns `true` if [`get`](Self::get) would return `Some` for `key`.
    fn contains_key(&self, key: &dyn crate::AsYaml) -> bool {
        self.get(key).is_some()
    }

    /// Number of entries visible through this view.
    fn len(&self) -> usize {
        self.iter().count()
    }

    /// Returns `true` if the view has no entries.
    fn is_empty(&self) -> bool {
        self.iter().next().is_none()
    }

    /// Iterate over the keys.
    fn keys<'a>(&'a self) -> Box<dyn Iterator<Item = YamlNode> + 'a> {
        Box::new(self.iter().map(|(k, _)| k))
    }

    /// Iterate over the values.
    fn values<'a>(&'a self) -> Box<dyn Iterator<Item = YamlNode> + 'a> {
        Box::new(self.iter().map(|(_, v)| v))
    }

    /// Iterate over `(key, value)` pairs.
    fn iter<'a>(&'a self) -> Box<dyn Iterator<Item = (YamlNode, YamlNode)> + 'a>;
}

impl MappingView for crate::yaml::Mapping {
    fn get(&self, key: &dyn crate::AsYaml) -> Option<YamlNode> {
        crate::yaml::Mapping::get(self, key)
    }

    fn contains_key(&self, key: &dyn crate::AsYaml) -> bool {
        crate::yaml::Mapping::contains_key(self, key)
    }

    fn len(&self) -> usize {
        crate::yaml::Mapping::len(self)
    }

    fn is_empty(&self) -> bool {
        crate::yaml::Mapping::is_empty(self)
    }

    fn keys<'a>(&'a self) -> Box<dyn Iterator<Item = YamlNode> + 'a> {
        Box::new(crate::yaml::Mapping::keys(self))
    }

    fn values<'a>(&'a self) -> Box<dyn Iterator<Item = YamlNode> + 'a> {
        Box::new(crate::yaml::Mapping::values(self))
    }

    fn iter<'a>(&'a self) -> Box<dyn Iterator<Item = (YamlNode, YamlNode)> + 'a> {
        Box::new(crate::yaml::Mapping::iter(self))
    }
}

impl MappingView for crate::anchor_resolution::MergedMapping<'_> {
    fn get(&self, key: &dyn crate::AsYaml) -> Option<YamlNode> {
        crate::anchor_resolution::MergedMapping::get(self, key)
    }

    fn iter<'a>(&'a self) -> Box<dyn Iterator<Item = (YamlNode, YamlNode)> + 'a> {
        Box::new(crate::anchor_resolution::MergedMapping::iter(self))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::anchor_resolution::{
        DocumentMergedExt, DocumentResolvedExt, MappingMergedExt, MergedMapping,
    };
    use crate::Document;
    use std::str::FromStr;

    /// A function generic over `impl MappingView` — proves the trait is
    /// usable as an abstraction over both `Mapping` and `MergedMapping`.
    fn collect_keys<M: MappingView + ?Sized>(view: &M) -> Vec<String> {
        view.keys()
            .map(|k| k.as_scalar().map(|s| s.as_string()).unwrap_or_default())
            .collect()
    }

    fn doc(text: &str) -> Document {
        Document::from_str(text).expect("parse")
    }

    #[test]
    fn generic_works_on_plain_mapping() {
        let d = doc("a: 1\nb: 2\nc: 3\n");
        let root = d.as_mapping().unwrap();
        assert_eq!(collect_keys(&root), vec!["a", "b", "c"]);
        assert_eq!(<crate::yaml::Mapping as MappingView>::len(&root), 3);
    }

    #[test]
    fn generic_works_on_merged_mapping() {
        let yaml = "\
d: &d
  a: 1
  b: 2
m:
  <<: *d
  c: 3
";
        let d = doc(yaml);
        let reg = d.build_anchor_registry();
        let m = d.as_mapping().unwrap().get_mapping("m").unwrap();
        let merged = m.merged(&reg);

        // Direct + merged keys, in insertion-then-merge-source order.
        assert_eq!(collect_keys(&merged), vec!["c", "a", "b"]);
        assert_eq!(<MergedMapping as MappingView>::len(&merged), 3);
    }

    #[test]
    fn trait_get_and_contains() {
        let yaml = "\
d: &d
  x: 1
m:
  <<: *d
  y: 2
";
        let d = doc(yaml);
        let reg = d.build_anchor_registry();
        let m = d.as_mapping().unwrap().get_mapping("m").unwrap();
        let merged = m.merged(&reg);

        let view: &dyn MappingView = &merged;
        assert!(view.contains_key(&"x"));
        assert!(view.contains_key(&"y"));
        assert!(!view.contains_key(&"z"));
        assert_eq!(view.get(&"x").unwrap().to_i64(), Some(1));
    }

    #[test]
    fn trait_object_safety() {
        // Compile-time check: building `&dyn MappingView` works.
        let d = doc("a: 1\n");
        let root = d.as_mapping().unwrap();
        let view: &dyn MappingView = &root;
        assert_eq!(view.len(), 1);
    }

    #[test]
    fn is_empty_via_trait() {
        let d = doc("{}\n");
        let root = d.as_mapping().unwrap();
        let view: &dyn MappingView = &root;
        assert!(view.is_empty());
    }

    #[test]
    fn values_iter_via_trait() {
        let d = doc("a: 1\nb: 2\n");
        let root = d.as_mapping().unwrap();
        let view: &dyn MappingView = &root;
        let nums: Vec<i64> = view.values().filter_map(|v| v.to_i64()).collect();
        assert_eq!(nums, vec![1, 2]);
    }

    #[test]
    fn merged_view_via_document_extension() {
        // Round-trip the new trait through the owned MergedView container.
        let yaml = "\
d: &d
  k: 42
m:
  <<: *d
";
        let d = doc(yaml);
        let owned = d.merged().unwrap();
        let m = owned.as_mapping().get_merged("m").unwrap();
        // Use the trait method so the `&dyn AsYaml` signature is exercised.
        let view: &dyn MappingView = &m;
        assert_eq!(view.get(&"k").unwrap().to_i64(), Some(42));
    }
}
