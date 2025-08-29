//! Value wrapper that can represent any YAML value type (scalar, sequence, mapping).

use crate::lex::SyntaxKind;
use crate::scalar::ScalarValue;
use rowan::GreenNodeBuilder;
use std::collections::{BTreeMap, BTreeSet};
use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum QuotingContext {
    /// Default context - quote based on content
    Default,
    /// Inside ordered mapping or pairs - be less aggressive with quoting
    OrderedCollection,
    /// Key context - quote aggressively to ensure valid YAML keys
    Key,
}

/// Represents any YAML value - scalar, sequence, mapping, or special collections
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum YamlValue {
    /// A scalar value (string, number, boolean, null)
    Scalar(ScalarValue),
    /// A sequence of values (list/array)
    Sequence(Vec<YamlValue>),
    /// A mapping of key-value pairs
    Mapping(BTreeMap<String, YamlValue>),
    /// A set of unique values (!!set)
    Set(BTreeSet<String>),
    /// An ordered mapping preserving key order (!!omap)
    OrderedMapping(Vec<(String, YamlValue)>),
    /// A sequence of key-value pairs allowing duplicates (!!pairs)
    Pairs(Vec<(String, YamlValue)>),
}

impl YamlValue {
    /// Create a scalar value
    pub fn scalar(value: impl Into<ScalarValue>) -> Self {
        YamlValue::Scalar(value.into())
    }

    /// Create an empty sequence
    pub const fn sequence() -> Self {
        YamlValue::Sequence(Vec::new())
    }

    /// Create a sequence from a vector
    pub const fn from_sequence(items: Vec<YamlValue>) -> Self {
        YamlValue::Sequence(items)
    }

    /// Create an empty mapping
    pub const fn mapping() -> Self {
        YamlValue::Mapping(BTreeMap::new())
    }

    /// Create a mapping from a BTreeMap
    pub const fn from_mapping(map: BTreeMap<String, YamlValue>) -> Self {
        YamlValue::Mapping(map)
    }

    /// Create an empty set
    pub const fn set() -> Self {
        YamlValue::Set(BTreeSet::new())
    }

    /// Create a set from a BTreeSet
    pub const fn from_set(set: BTreeSet<String>) -> Self {
        YamlValue::Set(set)
    }

    /// Create an empty ordered mapping
    pub const fn ordered_mapping() -> Self {
        YamlValue::OrderedMapping(Vec::new())
    }

    /// Create an ordered mapping from a vector of pairs
    pub const fn from_ordered_mapping(pairs: Vec<(String, YamlValue)>) -> Self {
        YamlValue::OrderedMapping(pairs)
    }

    /// Create empty pairs
    pub const fn pairs() -> Self {
        YamlValue::Pairs(Vec::new())
    }

    /// Create pairs from a vector of pairs
    pub const fn from_pairs(pairs: Vec<(String, YamlValue)>) -> Self {
        YamlValue::Pairs(pairs)
    }

    /// Check if this is a scalar
    #[inline]
    pub fn is_scalar(&self) -> bool {
        matches!(self, YamlValue::Scalar(_))
    }

    /// Check if this is a sequence
    #[inline]
    pub fn is_sequence(&self) -> bool {
        matches!(self, YamlValue::Sequence(_))
    }

    /// Check if this is a mapping
    #[inline]
    pub fn is_mapping(&self) -> bool {
        matches!(self, YamlValue::Mapping(_))
    }

    /// Check if this is a set
    #[inline]
    pub fn is_set(&self) -> bool {
        matches!(self, YamlValue::Set(_))
    }

    /// Check if this is an ordered mapping
    #[inline]
    pub fn is_ordered_mapping(&self) -> bool {
        matches!(self, YamlValue::OrderedMapping(_))
    }

    /// Check if this is pairs
    #[inline]
    pub fn is_pairs(&self) -> bool {
        matches!(self, YamlValue::Pairs(_))
    }

    /// Get as scalar if this is a scalar
    pub fn as_scalar(&self) -> Option<&ScalarValue> {
        match self {
            YamlValue::Scalar(s) => Some(s),
            _ => None,
        }
    }

    /// Create a rowan green tree for this YAML value
    pub fn to_green_tree(&self) -> rowan::GreenNode {
        crate::yaml::create_yaml_content_green(self, false)
    }

    /// Create a rowan green tree for this YAML value when used as a key
    pub fn to_key_green_tree(&self) -> rowan::GreenNode {
        crate::yaml::create_yaml_content_green(self, true)
    }

    /// Get as sequence if this is a sequence
    pub fn as_sequence(&self) -> Option<&[YamlValue]> {
        match self {
            YamlValue::Sequence(seq) => Some(seq),
            _ => None,
        }
    }

    /// Get as mutable sequence if this is a sequence
    pub fn as_sequence_mut(&mut self) -> Option<&mut Vec<YamlValue>> {
        match self {
            YamlValue::Sequence(seq) => Some(seq),
            _ => None,
        }
    }

    /// Get as mapping if this is a mapping
    pub fn as_mapping(&self) -> Option<&BTreeMap<String, YamlValue>> {
        match self {
            YamlValue::Mapping(map) => Some(map),
            _ => None,
        }
    }

    /// Get as mutable mapping if this is a mapping
    pub fn as_mapping_mut(&mut self) -> Option<&mut BTreeMap<String, YamlValue>> {
        match self {
            YamlValue::Mapping(map) => Some(map),
            _ => None,
        }
    }

    /// Get as set if this is a set
    pub fn as_set(&self) -> Option<&BTreeSet<String>> {
        match self {
            YamlValue::Set(set) => Some(set),
            _ => None,
        }
    }

    /// Get as mutable set if this is a set
    pub fn as_set_mut(&mut self) -> Option<&mut BTreeSet<String>> {
        match self {
            YamlValue::Set(set) => Some(set),
            _ => None,
        }
    }

    /// Get as ordered mapping if this is an ordered mapping
    pub fn as_ordered_mapping(&self) -> Option<&[(String, YamlValue)]> {
        match self {
            YamlValue::OrderedMapping(pairs) => Some(pairs),
            _ => None,
        }
    }

    /// Get as mutable ordered mapping if this is an ordered mapping
    pub fn as_ordered_mapping_mut(&mut self) -> Option<&mut Vec<(String, YamlValue)>> {
        match self {
            YamlValue::OrderedMapping(pairs) => Some(pairs),
            _ => None,
        }
    }

    /// Get as pairs if this is pairs
    pub fn as_pairs(&self) -> Option<&[(String, YamlValue)]> {
        match self {
            YamlValue::Pairs(pairs) => Some(pairs),
            _ => None,
        }
    }

    /// Get as mutable pairs if this is pairs
    pub fn as_pairs_mut(&mut self) -> Option<&mut Vec<(String, YamlValue)>> {
        match self {
            YamlValue::Pairs(pairs) => Some(pairs),
            _ => None,
        }
    }

    /// Convert to AST green node that can be spliced into a YAML tree
    pub fn to_green_node(&self) -> rowan::GreenNode {
        let mut builder = GreenNodeBuilder::new();
        self.build_green_node(&mut builder, QuotingContext::Default);
        builder.finish()
    }

    /// Build the green node representation into the provided builder
    pub fn build_green_node(&self, builder: &mut GreenNodeBuilder, context: QuotingContext) {
        match self {
            YamlValue::Scalar(s) => {
                // For scalars, check if we need quoting based on the scalar type and content
                let value = s.value();
                let scalar_type = s.scalar_type();

                // Trust the scalar type - if it's specifically typed as a number/bool, don't quote
                let needs_quote = match scalar_type {
                    crate::scalar::ScalarType::Integer
                    | crate::scalar::ScalarType::Float
                    | crate::scalar::ScalarType::Boolean
                    | crate::scalar::ScalarType::Null => false,
                    _ => {
                        // For strings, check if content needs quoting based on context
                        let basic_needs_quote = value.is_empty() ||
                            value.contains(':') || value.contains('#') ||
                            value.contains('@') || value.contains('[') || value.contains(']') ||
                            value.contains('{') || value.contains('}') ||
                            // Check if it looks like a YAML keyword
                            matches!(value.to_lowercase().as_str(),
                                "true" | "false" | "yes" | "no" | "on" | "off" | "null" | "~") ||
                            // Check if it looks like a number but should be treated as string
                            value.parse::<f64>().is_ok();

                        // Add space quoting based on context
                        let space_needs_quote = value.contains(' ')
                            && match context {
                                QuotingContext::Default => {
                                    // In default context, only quote spaces if they're problematic
                                    value.starts_with(' ')
                                        || value.ends_with(' ')
                                        || value.contains(": ")
                                        || value.contains("# ")
                                }
                                QuotingContext::OrderedCollection => {
                                    // In ordered collections, be even more permissive
                                    value.starts_with(' ')
                                        || value.ends_with(' ')
                                        || value.contains(": ")
                                        || value.contains("# ")
                                }
                                QuotingContext::Key => {
                                    // For keys, always quote spaces to ensure valid YAML
                                    true
                                }
                            };

                        basic_needs_quote || space_needs_quote
                    }
                };

                if needs_quote {
                    let quoted = format!("'{}'", value.replace('\'', "''"));
                    builder.token(SyntaxKind::STRING.into(), &quoted);
                } else {
                    builder.token(SyntaxKind::STRING.into(), value);
                }
            }
            YamlValue::Sequence(seq) => {
                if seq.is_empty() {
                    // For empty sequences, use flow style []
                    builder.token(SyntaxKind::LEFT_BRACKET.into(), "[");
                    builder.token(SyntaxKind::RIGHT_BRACKET.into(), "]");
                } else {
                    builder.start_node(SyntaxKind::SEQUENCE.into());
                    for item in seq {
                        builder.token(SyntaxKind::DASH.into(), "-");
                        builder.token(SyntaxKind::WHITESPACE.into(), " ");
                        item.build_green_node(builder, context);
                        builder.token(SyntaxKind::NEWLINE.into(), "\n");
                    }
                    builder.finish_node();
                }
            }
            YamlValue::Mapping(map) => {
                if map.is_empty() {
                    // For empty mappings, use flow style {}
                    builder.token(SyntaxKind::LEFT_BRACE.into(), "{");
                    builder.token(SyntaxKind::RIGHT_BRACE.into(), "}");
                } else {
                    builder.start_node(SyntaxKind::MAPPING.into());
                    for (key, value) in map {
                        builder.start_node(SyntaxKind::KEY.into());
                        // Quote key if needed
                        let needs_quote = key.contains(':')
                            || key.contains('#')
                            || key.contains(' ')
                            || key.contains('@');
                        if needs_quote {
                            let quoted = format!("'{}'", key.replace('\'', "''"));
                            builder.token(SyntaxKind::STRING.into(), &quoted);
                        } else {
                            builder.token(SyntaxKind::STRING.into(), key);
                        }
                        builder.finish_node();

                        builder.token(SyntaxKind::COLON.into(), ":");
                        builder.token(SyntaxKind::WHITESPACE.into(), " ");

                        builder.start_node(SyntaxKind::VALUE.into());
                        value.build_green_node(builder, context);
                        builder.finish_node();

                        builder.token(SyntaxKind::NEWLINE.into(), "\n");
                    }
                    builder.finish_node();
                }
            }
            YamlValue::Set(set) => {
                builder.token(SyntaxKind::TAG.into(), "!!set");
                builder.token(SyntaxKind::WHITESPACE.into(), " ");
                if set.is_empty() {
                    // For empty sets, use flow style {}
                    builder.token(SyntaxKind::LEFT_BRACE.into(), "{");
                    builder.token(SyntaxKind::RIGHT_BRACE.into(), "}");
                } else {
                    builder.start_node(SyntaxKind::MAPPING.into());
                    for item in set {
                        builder.token(SyntaxKind::STRING.into(), item);
                        builder.token(SyntaxKind::COLON.into(), ":");
                        builder.token(SyntaxKind::WHITESPACE.into(), " ");
                        builder.token(SyntaxKind::NULL.into(), "null");
                        builder.token(SyntaxKind::NEWLINE.into(), "\n");
                    }
                    builder.finish_node();
                }
            }
            YamlValue::OrderedMapping(pairs) => {
                builder.token(SyntaxKind::TAG.into(), "!!omap");
                builder.token(SyntaxKind::NEWLINE.into(), "\n");
                builder.start_node(SyntaxKind::SEQUENCE.into());
                for (key, value) in pairs {
                    builder.token(SyntaxKind::WHITESPACE.into(), "  ");
                    builder.token(SyntaxKind::DASH.into(), "-");
                    builder.token(SyntaxKind::WHITESPACE.into(), " ");
                    builder.start_node(SyntaxKind::MAPPING.into());

                    builder.start_node(SyntaxKind::KEY.into());
                    builder.token(SyntaxKind::STRING.into(), key);
                    builder.finish_node();

                    builder.token(SyntaxKind::COLON.into(), ":");
                    builder.token(SyntaxKind::WHITESPACE.into(), " ");

                    builder.start_node(SyntaxKind::VALUE.into());
                    value.build_green_node(builder, QuotingContext::OrderedCollection);
                    builder.finish_node();

                    builder.finish_node();
                    builder.token(SyntaxKind::NEWLINE.into(), "\n");
                }
                builder.finish_node();
            }
            YamlValue::Pairs(pairs) => {
                builder.token(SyntaxKind::TAG.into(), "!!pairs");
                builder.token(SyntaxKind::NEWLINE.into(), "\n");
                builder.start_node(SyntaxKind::SEQUENCE.into());
                for (key, value) in pairs {
                    builder.token(SyntaxKind::WHITESPACE.into(), "  ");
                    builder.token(SyntaxKind::DASH.into(), "-");
                    builder.token(SyntaxKind::WHITESPACE.into(), " ");
                    builder.start_node(SyntaxKind::MAPPING.into());

                    builder.start_node(SyntaxKind::KEY.into());
                    builder.token(SyntaxKind::STRING.into(), key);
                    builder.finish_node();

                    builder.token(SyntaxKind::COLON.into(), ":");
                    builder.token(SyntaxKind::WHITESPACE.into(), " ");

                    builder.start_node(SyntaxKind::VALUE.into());
                    value.build_green_node(builder, QuotingContext::OrderedCollection);
                    builder.finish_node();

                    builder.finish_node();
                    builder.token(SyntaxKind::NEWLINE.into(), "\n");
                }
                builder.finish_node();
            }
        }
    }

    /// Convert to YAML string representation
    pub fn to_yaml_string(&self, indent: usize) -> String {
        match self {
            YamlValue::Scalar(s) => s.to_yaml_string(),
            YamlValue::Sequence(seq) => {
                if seq.is_empty() {
                    "[]".to_string()
                } else {
                    let mut result = String::with_capacity(seq.len() * 20); // Pre-allocate
                    let indent_str = " ".repeat(indent);
                    for item in seq {
                        result.push_str(&indent_str);
                        result.push_str("  - ");
                        match item {
                            YamlValue::Scalar(s) => result.push_str(&s.to_yaml_string()),
                            _ => {
                                result.push('\n');
                                result.push_str(&item.to_yaml_string(indent + 4));
                            }
                        }
                        result.push('\n');
                    }
                    result.truncate(result.trim_end().len());
                    result
                }
            }
            YamlValue::Mapping(map) => {
                if map.is_empty() {
                    "{}".to_string()
                } else {
                    let mut result = String::with_capacity(map.len() * 30); // Pre-allocate
                    let indent_str = " ".repeat(indent);
                    for (key, value) in map {
                        result.push_str(&indent_str);
                        result.push_str(key);
                        result.push_str(": ");
                        match value {
                            YamlValue::Scalar(s) => result.push_str(&s.to_yaml_string()),
                            _ => {
                                result.push('\n');
                                result.push_str(&value.to_yaml_string(indent + 2));
                            }
                        }
                        result.push('\n');
                    }
                    result.truncate(result.trim_end().len());
                    result
                }
            }
            YamlValue::Set(set) => {
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
            YamlValue::OrderedMapping(pairs) => {
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
            YamlValue::Pairs(pairs) => {
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
}

impl fmt::Display for YamlValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_yaml_string(0))
    }
}

// Convenience conversions from common types
impl From<String> for YamlValue {
    fn from(value: String) -> Self {
        YamlValue::Scalar(ScalarValue::from(value))
    }
}

impl From<&str> for YamlValue {
    fn from(value: &str) -> Self {
        YamlValue::Scalar(ScalarValue::from(value))
    }
}

impl From<i32> for YamlValue {
    fn from(value: i32) -> Self {
        YamlValue::Scalar(ScalarValue::from(value))
    }
}

impl From<i64> for YamlValue {
    fn from(value: i64) -> Self {
        YamlValue::Scalar(ScalarValue::from(value))
    }
}

impl From<f32> for YamlValue {
    fn from(value: f32) -> Self {
        YamlValue::Scalar(ScalarValue::from(value))
    }
}

impl From<f64> for YamlValue {
    fn from(value: f64) -> Self {
        YamlValue::Scalar(ScalarValue::from(value))
    }
}

impl From<bool> for YamlValue {
    fn from(value: bool) -> Self {
        YamlValue::Scalar(ScalarValue::from(value))
    }
}

impl From<ScalarValue> for YamlValue {
    fn from(value: ScalarValue) -> Self {
        YamlValue::Scalar(value)
    }
}

impl<T> From<Vec<T>> for YamlValue
where
    T: Into<YamlValue>,
{
    fn from(vec: Vec<T>) -> Self {
        let values: Vec<_> = vec.into_iter().map(Into::into).collect();
        YamlValue::Sequence(values)
    }
}

impl<K, V> From<BTreeMap<K, V>> for YamlValue
where
    K: Into<String>,
    V: Into<YamlValue>,
{
    fn from(map: BTreeMap<K, V>) -> Self {
        YamlValue::Mapping(map.into_iter().map(|(k, v)| (k.into(), v.into())).collect())
    }
}

impl<T> From<BTreeSet<T>> for YamlValue
where
    T: Into<String>,
{
    fn from(set: BTreeSet<T>) -> Self {
        YamlValue::Set(set.into_iter().map(Into::into).collect())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_scalar_value() {
        let val = YamlValue::from("hello");
        assert!(val.is_scalar());
        assert_eq!(val.to_yaml_string(0), "hello");

        let val = YamlValue::from(42);
        assert!(val.is_scalar());
        assert_eq!(val.to_yaml_string(0), "42");

        let val = YamlValue::from(true);
        assert!(val.is_scalar());
        assert_eq!(val.to_yaml_string(0), "true");
    }

    #[test]
    fn test_sequence_value() {
        let val = YamlValue::from(vec!["item1", "item2", "item3"]);
        assert!(val.is_sequence());
        let yaml = val.to_yaml_string(0);
        assert!(yaml.contains("- item1"));
        assert!(yaml.contains("- item2"));
        assert!(yaml.contains("- item3"));

        // Empty sequence
        let val = YamlValue::sequence();
        assert_eq!(val.to_yaml_string(0), "[]");
    }

    #[test]
    fn test_mapping_value() {
        let mut map = BTreeMap::new();
        map.insert("name", YamlValue::from("project"));
        map.insert("version", YamlValue::from("1.0.0"));

        let val = YamlValue::from(map);
        assert!(val.is_mapping());
        let yaml = val.to_yaml_string(0);
        assert!(yaml.contains("name: project"));
        assert!(yaml.contains("version: 1.0.0"));

        // Empty mapping
        let val = YamlValue::mapping();
        assert_eq!(val.to_yaml_string(0), "{}");
    }

    #[test]
    fn test_nested_structure() {
        let mut db_map = BTreeMap::new();
        db_map.insert("host", YamlValue::from("localhost"));
        db_map.insert("port", YamlValue::from(5432));

        let mut root_map = BTreeMap::new();
        root_map.insert("name", YamlValue::from("app"));
        root_map.insert("database", YamlValue::from(db_map));
        root_map.insert("features", YamlValue::from(vec!["auth", "logging"]));

        let val = YamlValue::from(root_map);
        let yaml = val.to_yaml_string(0);

        assert!(yaml.contains("name: app"));
        assert!(yaml.contains("database:"));
        assert!(yaml.contains("  host: localhost"));
        assert!(yaml.contains("  port: 5432"));
        assert!(yaml.contains("features:"));
        assert!(yaml.contains("  - auth"));
        assert!(yaml.contains("  - logging"));
    }

    #[test]
    fn test_set_value() {
        let mut set = BTreeSet::new();
        set.insert("item1".to_string());
        set.insert("item2".to_string());
        set.insert("item3".to_string());

        let val = YamlValue::from_set(set);
        assert!(val.is_set());
        let yaml = val.to_yaml_string(0);
        assert!(yaml.contains("!!set"));
        assert!(yaml.contains("item1: null"));
        assert!(yaml.contains("item2: null"));
        assert!(yaml.contains("item3: null"));

        // Empty set
        let val = YamlValue::set();
        assert_eq!(val.to_yaml_string(0), "!!set {}");
    }

    #[test]
    fn test_ordered_mapping_value() {
        let pairs = vec![
            ("first".to_string(), YamlValue::from("value1")),
            ("second".to_string(), YamlValue::from("value2")),
            ("third".to_string(), YamlValue::from("value3")),
        ];

        let val = YamlValue::from_ordered_mapping(pairs);
        assert!(val.is_ordered_mapping());
        let yaml = val.to_yaml_string(0);
        assert!(yaml.contains("!!omap"));
        assert!(yaml.contains("- first: value1"));
        assert!(yaml.contains("- second: value2"));
        assert!(yaml.contains("- third: value3"));

        // Empty ordered mapping
        let val = YamlValue::ordered_mapping();
        assert_eq!(val.to_yaml_string(0), "!!omap []");
    }

    #[test]
    fn test_pairs_value() {
        let pairs = vec![
            ("key1".to_string(), YamlValue::from("value1")),
            ("key1".to_string(), YamlValue::from("value2")), // Duplicate key allowed
            ("key2".to_string(), YamlValue::from("value3")),
        ];

        let val = YamlValue::from_pairs(pairs);
        assert!(val.is_pairs());
        let yaml = val.to_yaml_string(0);
        assert!(yaml.contains("!!pairs"));
        assert!(yaml.contains("- key1: value1"));
        assert!(yaml.contains("- key1: value2"));
        assert!(yaml.contains("- key2: value3"));

        // Empty pairs
        let val = YamlValue::pairs();
        assert_eq!(val.to_yaml_string(0), "!!pairs []");
    }
}
