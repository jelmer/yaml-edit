//! Read-only mapping interface shared by [`Mapping`] and
//! [`MergedMapping`](crate::anchor_resolution::MergedMapping).
//!
//! [`MappingView`] abstracts over "anything that looks like a YAML mapping
//! for the purposes of reading it." It lets you write code that works
//! generically against both the underlying CST [`Mapping`] and the
//! alias/merge-key-resolving [`MergedMapping`](crate::anchor_resolution::MergedMapping)
//! view.
//!
//! Iterators are returned as boxed trait objects so the trait stays
//! object-safe and works on the crate's MSRV. Most uses pay one heap
//! allocation per iteration - negligible for an editing library.
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
//! let registry = doc.merged().unwrap().unwrap();
//! let prod = registry.as_mapping().get_merged("prod").unwrap().unwrap();
//! assert_eq!(count_keys(&prod), 2); // timeout + host
//! ```
//!
//! The trait is intentionally read-only: mutations only make sense on the
//! CST [`Mapping`], so [`set`](crate::yaml::Mapping::set),
//! [`remove`](crate::yaml::Mapping::remove), etc. remain on that type.
//!
//! [`Mapping`]: crate::yaml::Mapping

use crate::as_yaml::YamlNode;

/// A read-only "view" of a YAML mapping, implemented by both the CST
/// [`Mapping`](crate::yaml::Mapping) and the alias/merge-key-resolving
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
