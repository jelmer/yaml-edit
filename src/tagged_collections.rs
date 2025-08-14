//! Tagged YAML collections (!!set, !!omap, !!pairs) support.

use crate::value::YamlValue;
use std::collections::BTreeSet;
use std::fmt;

/// Represents YAML tagged collections - special collection types with explicit tags
#[derive(Debug, Clone, PartialEq)]
pub enum TaggedCollection {
    /// A set of unique values (!!set)
    Set(BTreeSet<String>),
    /// An ordered mapping preserving key order (!!omap)
    OrderedMapping(Vec<(String, YamlValue)>),
    /// A sequence of key-value pairs allowing duplicates (!!pairs)
    Pairs(Vec<(String, YamlValue)>),
}

impl TaggedCollection {
    /// Create an empty set
    pub fn empty_set() -> Self {
        TaggedCollection::Set(BTreeSet::new())
    }

    /// Create a set from a BTreeSet
    pub fn from_set(set: BTreeSet<String>) -> Self {
        TaggedCollection::Set(set)
    }

    /// Create an empty ordered mapping
    pub fn empty_ordered_mapping() -> Self {
        TaggedCollection::OrderedMapping(Vec::new())
    }

    /// Create an ordered mapping from a vector of pairs
    pub fn from_ordered_mapping(pairs: Vec<(String, YamlValue)>) -> Self {
        TaggedCollection::OrderedMapping(pairs)
    }

    /// Create empty pairs
    pub fn empty_pairs() -> Self {
        TaggedCollection::Pairs(Vec::new())
    }

    /// Create pairs from a vector of pairs
    pub fn from_pairs(pairs: Vec<(String, YamlValue)>) -> Self {
        TaggedCollection::Pairs(pairs)
    }

    /// Check if this is a set
    #[inline]
    pub fn is_set(&self) -> bool {
        matches!(self, TaggedCollection::Set(_))
    }

    /// Check if this is an ordered mapping
    #[inline]
    pub fn is_ordered_mapping(&self) -> bool {
        matches!(self, TaggedCollection::OrderedMapping(_))
    }

    /// Check if this is pairs
    #[inline]
    pub fn is_pairs(&self) -> bool {
        matches!(self, TaggedCollection::Pairs(_))
    }

    /// Get as set if this is a set
    pub fn as_set(&self) -> Option<&BTreeSet<String>> {
        match self {
            TaggedCollection::Set(set) => Some(set),
            _ => None,
        }
    }

    /// Get as mutable set if this is a set
    pub fn as_set_mut(&mut self) -> Option<&mut BTreeSet<String>> {
        match self {
            TaggedCollection::Set(set) => Some(set),
            _ => None,
        }
    }

    /// Get as ordered mapping if this is an ordered mapping
    pub fn as_ordered_mapping(&self) -> Option<&[(String, YamlValue)]> {
        match self {
            TaggedCollection::OrderedMapping(pairs) => Some(pairs),
            _ => None,
        }
    }

    /// Get as mutable ordered mapping if this is an ordered mapping
    pub fn as_ordered_mapping_mut(&mut self) -> Option<&mut Vec<(String, YamlValue)>> {
        match self {
            TaggedCollection::OrderedMapping(pairs) => Some(pairs),
            _ => None,
        }
    }

    /// Get as pairs if this is pairs
    pub fn as_pairs(&self) -> Option<&[(String, YamlValue)]> {
        match self {
            TaggedCollection::Pairs(pairs) => Some(pairs),
            _ => None,
        }
    }

    /// Get as mutable pairs if this is pairs
    pub fn as_pairs_mut(&mut self) -> Option<&mut Vec<(String, YamlValue)>> {
        match self {
            TaggedCollection::Pairs(pairs) => Some(pairs),
            _ => None,
        }
    }

    /// Convert to YAML string representation
    pub fn to_yaml_string(&self, indent: usize) -> String {
        match self {
            TaggedCollection::Set(set) => {
                // Sets are represented as mappings with null values
                if set.is_empty() {
                    "!!set {}".to_string()
                } else {
                    let mut result = String::from("!!set");
                    result.push('\n');
                    let indent_str = " ".repeat(indent);
                    for item in set {
                        result.push_str(&indent_str);
                        result.push_str(item);
                        result.push_str(": null");
                        result.push('\n');
                    }
                    result.truncate(result.trim_end().len());
                    result
                }
            }
            TaggedCollection::OrderedMapping(pairs) => {
                // Ordered mappings are represented as sequences of single-key mappings
                if pairs.is_empty() {
                    "!!omap []".to_string()
                } else {
                    let mut result = String::from("!!omap");
                    result.push('\n');
                    let indent_str = " ".repeat(indent);
                    for (key, value) in pairs {
                        result.push_str(&indent_str);
                        result.push_str("  - ");
                        result.push_str(key);
                        result.push_str(": ");
                        match value {
                            YamlValue::Scalar(s) => result.push_str(&s.to_yaml_string()),
                            _ => {
                                result.push('\n');
                                result.push_str(&value.to_yaml_string(indent + 4));
                            }
                        }
                        result.push('\n');
                    }
                    result.truncate(result.trim_end().len());
                    result
                }
            }
            TaggedCollection::Pairs(pairs) => {
                // Pairs are represented as sequences of key-value pairs
                if pairs.is_empty() {
                    "!!pairs []".to_string()
                } else {
                    let mut result = String::from("!!pairs");
                    result.push('\n');
                    let indent_str = " ".repeat(indent);
                    for (key, value) in pairs {
                        result.push_str(&indent_str);
                        result.push_str("  - ");
                        result.push_str(key);
                        result.push_str(": ");
                        match value {
                            YamlValue::Scalar(s) => result.push_str(&s.to_yaml_string()),
                            _ => {
                                result.push('\n');
                                result.push_str(&value.to_yaml_string(indent + 4));
                            }
                        }
                        result.push('\n');
                    }
                    result.truncate(result.trim_end().len());
                    result
                }
            }
        }
    }

    /// Get the tag name for this collection type
    pub fn tag_name(&self) -> &'static str {
        match self {
            TaggedCollection::Set(_) => "!!set",
            TaggedCollection::OrderedMapping(_) => "!!omap",
            TaggedCollection::Pairs(_) => "!!pairs",
        }
    }
}

impl fmt::Display for TaggedCollection {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_yaml_string(0))
    }
}

// Convenience conversions
impl<T> From<BTreeSet<T>> for TaggedCollection
where
    T: Into<String>,
{
    fn from(set: BTreeSet<T>) -> Self {
        TaggedCollection::Set(set.into_iter().map(Into::into).collect())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_set_collection() {
        let mut set = BTreeSet::new();
        set.insert("item1".to_string());
        set.insert("item2".to_string());
        set.insert("item3".to_string());

        let collection = TaggedCollection::from_set(set);
        assert!(collection.is_set());
        let yaml = collection.to_yaml_string(0);
        assert!(yaml.contains("!!set"));
        assert!(yaml.contains("item1: null"));
        assert!(yaml.contains("item2: null"));
        assert!(yaml.contains("item3: null"));

        // Empty set
        let collection = TaggedCollection::empty_set();
        assert_eq!(collection.to_yaml_string(0), "!!set {}");
    }

    #[test]
    fn test_ordered_mapping_collection() {
        let pairs = vec![
            ("first".to_string(), YamlValue::from("value1")),
            ("second".to_string(), YamlValue::from("value2")),
            ("third".to_string(), YamlValue::from("value3")),
        ];

        let collection = TaggedCollection::from_ordered_mapping(pairs);
        assert!(collection.is_ordered_mapping());
        let yaml = collection.to_yaml_string(0);
        assert!(yaml.contains("!!omap"));
        assert!(yaml.contains("- first: value1"));
        assert!(yaml.contains("- second: value2"));
        assert!(yaml.contains("- third: value3"));

        // Empty ordered mapping
        let collection = TaggedCollection::empty_ordered_mapping();
        assert_eq!(collection.to_yaml_string(0), "!!omap []");
    }

    #[test]
    fn test_pairs_collection() {
        let pairs = vec![
            ("key1".to_string(), YamlValue::from("value1")),
            ("key1".to_string(), YamlValue::from("value2")), // Duplicate key allowed
            ("key2".to_string(), YamlValue::from("value3")),
        ];

        let collection = TaggedCollection::from_pairs(pairs);
        assert!(collection.is_pairs());
        let yaml = collection.to_yaml_string(0);
        assert!(yaml.contains("!!pairs"));
        assert!(yaml.contains("- key1: value1"));
        assert!(yaml.contains("- key1: value2"));
        assert!(yaml.contains("- key2: value3"));

        // Empty pairs
        let collection = TaggedCollection::empty_pairs();
        assert_eq!(collection.to_yaml_string(0), "!!pairs []");
    }

    #[test]
    fn test_tag_names() {
        assert_eq!(TaggedCollection::empty_set().tag_name(), "!!set");
        assert_eq!(
            TaggedCollection::empty_ordered_mapping().tag_name(),
            "!!omap"
        );
        assert_eq!(TaggedCollection::empty_pairs().tag_name(), "!!pairs");
    }
}
