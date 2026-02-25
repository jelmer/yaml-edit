//! Value wrapper that can represent any YAML value type (scalar, sequence, mapping).

use crate::scalar::ScalarValue;
use std::collections::{BTreeMap, BTreeSet};
use std::fmt;

/// Represents any YAML value - scalar, sequence, mapping, or special collections.
///
/// **Deprecated:** Prefer using syntax tree types (`Mapping`, `Sequence`,
/// `Scalar`) and the `AsYaml` trait instead — they preserve formatting and
/// work with the CST directly. `YamlValue` loses all formatting information.
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
    /// Try to cast a SyntaxNode to a YamlValue
    pub fn cast(node: rowan::SyntaxNode<crate::yaml::Lang>) -> Option<Self> {
        use crate::lex::SyntaxKind;
        use crate::yaml::{Mapping, Scalar, Sequence, TaggedNode};
        use rowan::ast::AstNode;

        match node.kind() {
            SyntaxKind::SCALAR => {
                // cast() only checks the kind, which we already matched, so it cannot fail here
                Scalar::cast(node)
                    .map(|scalar| YamlValue::Scalar(ScalarValue::from_scalar(&scalar)))
            }
            SyntaxKind::SEQUENCE => {
                if let Some(seq) = Sequence::cast(node) {
                    let items: Vec<YamlValue> = seq.items().filter_map(YamlValue::cast).collect();
                    Some(YamlValue::Sequence(items))
                } else {
                    None
                }
            }
            SyntaxKind::MAPPING => {
                if let Some(mapping) = Mapping::cast(node) {
                    let mut map = std::collections::BTreeMap::new();
                    for (key_node, value_node) in mapping.pairs() {
                        // Extract key as string (simplification for BTreeMap key)
                        let key_str = if let Some(key_scalar) = Scalar::cast(key_node.clone()) {
                            key_scalar.as_string()
                        } else {
                            key_node.text().to_string()
                        };

                        // Convert value node to YamlValue
                        if let Some(value_yaml) = YamlValue::cast(value_node) {
                            map.insert(key_str, value_yaml);
                        }
                    }
                    Some(YamlValue::Mapping(map))
                } else {
                    None
                }
            }
            SyntaxKind::TAGGED_NODE => {
                // Delegate to tag-aware special-collection helpers, then fall back
                // to extracting the plain string value from the inner scalar.
                TaggedNode::cast(node).and_then(|tagged| {
                    // Check for special collection tags first.
                    if let Some(set) = tagged.as_set() {
                        let members: BTreeSet<String> = set
                            .members()
                            .filter_map(|v| v.as_scalar().map(|s| s.as_string()))
                            .collect();
                        return Some(YamlValue::Set(members));
                    }
                    if let Some(entries) = tagged.as_ordered_mapping() {
                        let pairs: Vec<(String, YamlValue)> = entries
                            .into_iter()
                            .filter_map(|e| {
                                // key() returns the KEY wrapper node; get its scalar child
                                let key_node = e.key()?;
                                let key_scalar = Scalar::cast(key_node.children().next()?)?;
                                let key = key_scalar.as_string();
                                let val = e
                                    .value()
                                    .and_then(|v| v.children().next().and_then(YamlValue::cast))?;
                                Some((key, val))
                            })
                            .collect();
                        return Some(YamlValue::OrderedMapping(pairs));
                    }
                    if let Some(entries) = tagged.as_pairs() {
                        let pairs: Vec<(String, YamlValue)> = entries
                            .into_iter()
                            .filter_map(|e| {
                                let key_node = e.key()?;
                                let key_scalar = Scalar::cast(key_node.children().next()?)?;
                                let key = key_scalar.as_string();
                                let val = e
                                    .value()
                                    .and_then(|v| v.children().next().and_then(YamlValue::cast))?;
                                Some((key, val))
                            })
                            .collect();
                        return Some(YamlValue::Pairs(pairs));
                    }
                    // Plain tagged scalar: return the string value.
                    tagged
                        .as_string()
                        .map(|s| YamlValue::Scalar(ScalarValue::string(s)))
                })
            }
            SyntaxKind::VALUE => {
                // VALUE nodes contain the actual content as children
                node.children().next().and_then(YamlValue::cast)
            }
            _ => None,
        }
    }

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

    /// Parse a raw YAML string into a YamlValue
    /// This handles both simple scalars and complex structures
    pub fn parse_raw(yaml_str: &str) -> Self {
        use std::str::FromStr;

        // Try to parse as a complete YAML document
        if let Ok(parsed) = crate::YamlFile::from_str(yaml_str) {
            if let Some(doc) = parsed.document() {
                return Self::from_document(&doc);
            }
        }

        // Fallback: treat as a scalar value
        YamlValue::Scalar(ScalarValue::parse(yaml_str))
    }

    /// Convert a Document to a YamlValue
    pub fn from_document(doc: &crate::yaml::Document) -> Self {
        use rowan::ast::AstNode;
        doc.syntax()
            .children()
            .find_map(Self::cast)
            .unwrap_or_else(|| YamlValue::Scalar(ScalarValue::string("")))
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

    /// Return `true` if this value is a scalar (string, integer, float, bool, null, etc.).
    ///
    /// Use [`as_scalar`](Self::as_scalar) to access the inner [`ScalarValue`].
    #[inline]
    pub fn is_scalar(&self) -> bool {
        matches!(self, YamlValue::Scalar(_))
    }

    /// Return `true` if this value is a sequence (YAML list / array).
    ///
    /// Use [`as_sequence`](Self::as_sequence) to access the inner sequence.
    #[inline]
    pub fn is_sequence(&self) -> bool {
        matches!(self, YamlValue::Sequence(_))
    }

    /// Return `true` if this value is a plain mapping (`key: value` pairs).
    ///
    /// Use [`as_mapping`](Self::as_mapping) to access the inner mapping.
    #[inline]
    pub fn is_mapping(&self) -> bool {
        matches!(self, YamlValue::Mapping(_))
    }

    /// Return `true` if this value is a `!!set` tagged collection.
    #[inline]
    pub fn is_set(&self) -> bool {
        matches!(self, YamlValue::Set(_))
    }

    /// Return `true` if this value is a `!!omap` (ordered mapping) tagged collection.
    #[inline]
    pub fn is_ordered_mapping(&self) -> bool {
        matches!(self, YamlValue::OrderedMapping(_))
    }

    /// Return `true` if this value is a `!!pairs` tagged collection.
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

    /// Get a reference to the string value if this is a scalar
    ///
    /// Returns the string representation of the scalar value.
    /// Returns None for non-scalar types (mappings, sequences, etc).
    /// Note: All scalars can be represented as strings, regardless of their type.
    pub fn as_str(&self) -> Option<&str> {
        match self {
            YamlValue::Scalar(s) => Some(s.value()),
            _ => None,
        }
    }

    /// Try to convert this value to an i64
    ///
    /// Returns the integer value if this is a scalar with Integer type.
    /// Supports various formats (decimal, hexadecimal 0x, binary 0b, octal 0o).
    /// Returns None if this is not a scalar or not an integer type.
    ///
    /// Note: This respects YAML's type system - `42` (unquoted) is an integer,
    /// but `"42"` (quoted) is a string and will return None.
    pub fn to_i64(&self) -> Option<i64> {
        match self {
            YamlValue::Scalar(s) => {
                use crate::scalar::ScalarType;
                // Only parse as integer if the scalar type is Integer
                if s.scalar_type() == ScalarType::Integer {
                    ScalarValue::parse_integer(s.value())
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Try to convert this value to an f64
    ///
    /// Returns the float value if this is a scalar with Float type.
    /// Returns None if this is not a scalar or not a float type.
    ///
    /// Note: This respects YAML's type system - `3.14` (unquoted) is a float,
    /// but `"3.14"` (quoted) is a string and will return None.
    pub fn to_f64(&self) -> Option<f64> {
        match self {
            YamlValue::Scalar(s) => {
                use crate::scalar::ScalarType;
                // Only parse as float if the scalar type is Float
                if s.scalar_type() == ScalarType::Float {
                    s.value().trim().parse::<f64>().ok()
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Try to convert this value to a bool
    ///
    /// Returns the boolean value if this is a scalar with Boolean type.
    /// Recognizes: true, false, yes, no, on, off (case-insensitive).
    /// Returns None if this is not a scalar or not a boolean type.
    ///
    /// Note: This respects YAML's type system - `no` (unquoted) is boolean,
    /// but `"no"` (quoted) is a string and will return None.
    pub fn to_bool(&self) -> Option<bool> {
        match self {
            YamlValue::Scalar(s) => {
                use crate::scalar::ScalarType;
                // Only parse as boolean if the scalar type is Boolean
                if s.scalar_type() == ScalarType::Boolean {
                    match s.value().to_lowercase().as_str() {
                        "true" | "yes" | "on" => Some(true),
                        "false" | "no" | "off" => Some(false),
                        _ => None,
                    }
                } else {
                    None
                }
            }
            _ => None,
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

impl From<&YamlValue> for YamlValue {
    fn from(value: &YamlValue) -> Self {
        value.clone()
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

    #[test]
    fn test_as_str() {
        // Test with different scalar types - all should return Some
        let string_val = YamlValue::from("hello");
        assert_eq!(string_val.as_str(), Some("hello"));

        let int_val = YamlValue::from(42);
        assert_eq!(int_val.as_str(), Some("42"));

        let float_val = YamlValue::from(1.5);
        assert_eq!(float_val.as_str(), Some("1.5"));

        let bool_val = YamlValue::from(true);
        assert_eq!(bool_val.as_str(), Some("true"));

        // Test with non-scalar types - should return None
        let mapping = YamlValue::mapping();
        assert_eq!(mapping.as_str(), None);

        let sequence = YamlValue::sequence();
        assert_eq!(sequence.as_str(), None);

        let set = YamlValue::set();
        assert_eq!(set.as_str(), None);
    }

    #[test]
    fn test_to_i64() {
        use crate::scalar::ScalarValue;

        // Unquoted integers should parse
        let int_val = YamlValue::Scalar(ScalarValue::parse("42"));
        assert_eq!(int_val.to_i64(), Some(42));

        let neg_int = YamlValue::Scalar(ScalarValue::parse("-123"));
        assert_eq!(neg_int.to_i64(), Some(-123));

        // Hexadecimal
        let hex_val = YamlValue::Scalar(ScalarValue::parse("0x2A"));
        assert_eq!(hex_val.to_i64(), Some(42));

        // Binary
        let bin_val = YamlValue::Scalar(ScalarValue::parse("0b101010"));
        assert_eq!(bin_val.to_i64(), Some(42));

        // Octal
        let oct_val = YamlValue::Scalar(ScalarValue::parse("0o52"));
        assert_eq!(oct_val.to_i64(), Some(42));

        // Quoted string "42" is a string, not an integer
        let string_val = YamlValue::from("42");
        assert_eq!(string_val.to_i64(), None);

        // Boolean values should not convert to integers
        let bool_val = YamlValue::Scalar(ScalarValue::parse("true"));
        assert_eq!(bool_val.to_i64(), None);

        let no_val = YamlValue::Scalar(ScalarValue::parse("no"));
        assert_eq!(no_val.to_i64(), None);

        // Float values should not convert to integers
        let float_val = YamlValue::Scalar(ScalarValue::parse("3.14"));
        assert_eq!(float_val.to_i64(), None);

        // Non-scalar types should return None
        let mapping = YamlValue::mapping();
        assert_eq!(mapping.to_i64(), None);

        let sequence = YamlValue::sequence();
        assert_eq!(sequence.to_i64(), None);

        // Random strings should not convert
        let random_string = YamlValue::from("blah");
        assert_eq!(random_string.to_i64(), None);

        // Large integers
        let large_int = YamlValue::Scalar(ScalarValue::parse("23432"));
        assert_eq!(large_int.to_i64(), Some(23432));

        // Numbers that look like booleans but are typed as integers should work
        let zero = YamlValue::Scalar(ScalarValue::parse("0"));
        assert_eq!(zero.to_i64(), Some(0));

        let one = YamlValue::Scalar(ScalarValue::parse("1"));
        assert_eq!(one.to_i64(), Some(1));

        // But quoted "1" and "0" are strings
        let quoted_one = YamlValue::from("1");
        assert_eq!(quoted_one.to_i64(), None);

        let quoted_zero = YamlValue::from("0");
        assert_eq!(quoted_zero.to_i64(), None);
    }

    #[test]
    fn test_to_f64() {
        use crate::scalar::ScalarValue;

        // Unquoted floats should parse
        let float_val = YamlValue::Scalar(ScalarValue::parse("1.5"));
        assert_eq!(float_val.to_f64(), Some(1.5));

        let neg_float = YamlValue::Scalar(ScalarValue::parse("-2.5"));
        assert_eq!(neg_float.to_f64(), Some(-2.5));

        // Scientific notation
        let sci_val = YamlValue::Scalar(ScalarValue::parse("1.23e10"));
        assert_eq!(sci_val.to_f64(), Some(1.23e10));

        // Quoted string "3.14" is a string, not a float
        let string_val = YamlValue::from("3.14");
        assert_eq!(string_val.to_f64(), None);

        // Integer values should not convert to floats (type mismatch)
        let int_val = YamlValue::Scalar(ScalarValue::parse("42"));
        assert_eq!(int_val.to_f64(), None);

        // Boolean values should not convert
        let bool_val = YamlValue::Scalar(ScalarValue::parse("true"));
        assert_eq!(bool_val.to_f64(), None);

        // Non-scalar types should return None
        let mapping = YamlValue::mapping();
        assert_eq!(mapping.to_f64(), None);

        let sequence = YamlValue::sequence();
        assert_eq!(sequence.to_f64(), None);

        // Random strings should not convert
        let random_string = YamlValue::from("blah");
        assert_eq!(random_string.to_f64(), None);
    }

    #[test]
    fn test_to_bool() {
        use crate::scalar::ScalarValue;

        // Unquoted boolean values should parse
        let true_val = YamlValue::Scalar(ScalarValue::parse("true"));
        assert_eq!(true_val.to_bool(), Some(true));

        let false_val = YamlValue::Scalar(ScalarValue::parse("false"));
        assert_eq!(false_val.to_bool(), Some(false));

        let yes_val = YamlValue::Scalar(ScalarValue::parse("yes"));
        assert_eq!(yes_val.to_bool(), Some(true));

        let no_val = YamlValue::Scalar(ScalarValue::parse("no"));
        assert_eq!(no_val.to_bool(), Some(false));

        let on_val = YamlValue::Scalar(ScalarValue::parse("on"));
        assert_eq!(on_val.to_bool(), Some(true));

        let off_val = YamlValue::Scalar(ScalarValue::parse("off"));
        assert_eq!(off_val.to_bool(), Some(false));

        // Case insensitivity
        let true_upper = YamlValue::Scalar(ScalarValue::parse("TRUE"));
        assert_eq!(true_upper.to_bool(), Some(true));

        let yes_mixed = YamlValue::Scalar(ScalarValue::parse("Yes"));
        assert_eq!(yes_mixed.to_bool(), Some(true));

        // Quoted strings are NOT booleans
        let quoted_true = YamlValue::from("true");
        assert_eq!(quoted_true.to_bool(), None);

        let quoted_false = YamlValue::from("false");
        assert_eq!(quoted_false.to_bool(), None);

        let quoted_yes = YamlValue::from("yes");
        assert_eq!(quoted_yes.to_bool(), None);

        let quoted_no = YamlValue::from("no");
        assert_eq!(quoted_no.to_bool(), None);

        // Integers are NOT booleans (even 0 and 1)
        let zero = YamlValue::Scalar(ScalarValue::parse("0"));
        assert_eq!(zero.to_bool(), None);

        let one = YamlValue::Scalar(ScalarValue::parse("1"));
        assert_eq!(one.to_bool(), None);

        let large_int = YamlValue::Scalar(ScalarValue::parse("23432"));
        assert_eq!(large_int.to_bool(), None);

        // Quoted "1" and "0" are strings, not booleans
        let quoted_one = YamlValue::from("1");
        assert_eq!(quoted_one.to_bool(), None);

        let quoted_zero = YamlValue::from("0");
        assert_eq!(quoted_zero.to_bool(), None);

        // Random strings should not convert
        let random_string = YamlValue::from("blah");
        assert_eq!(random_string.to_bool(), None);

        // Non-scalar types should return None
        let mapping = YamlValue::mapping();
        assert_eq!(mapping.to_bool(), None);

        let sequence = YamlValue::sequence();
        assert_eq!(sequence.to_bool(), None);

        // Float values are not booleans
        let float_val = YamlValue::Scalar(ScalarValue::parse("3.14"));
        assert_eq!(float_val.to_bool(), None);
    }
}

// AsYaml trait implementation for YamlValue
use crate::as_yaml::{AsYaml, YamlKind};
use crate::yaml::SyntaxNode;

impl AsYaml for YamlValue {
    fn as_node(&self) -> Option<&SyntaxNode> {
        None
    }

    fn kind(&self) -> YamlKind {
        match self {
            YamlValue::Scalar(_) => YamlKind::Scalar,
            YamlValue::Sequence(_) => YamlKind::Sequence,
            YamlValue::Mapping(_) => YamlKind::Mapping,
            YamlValue::Set(_) => YamlKind::Tagged(std::borrow::Cow::Borrowed("!!set")),
            YamlValue::OrderedMapping(_) => YamlKind::Tagged(std::borrow::Cow::Borrowed("!!omap")),
            YamlValue::Pairs(_) => YamlKind::Tagged(std::borrow::Cow::Borrowed("!!pairs")),
        }
    }

    fn build_content(
        &self,
        builder: &mut rowan::GreenNodeBuilder,
        indent: usize,
        flow_context: bool,
    ) -> bool {
        use crate::lex::SyntaxKind;
        match self {
            YamlValue::Scalar(s) => {
                builder.start_node(SyntaxKind::SCALAR.into());
                let token_kind = if s.value().parse::<i64>().is_ok() {
                    SyntaxKind::INT
                } else if s.value().parse::<f64>().is_ok() {
                    SyntaxKind::FLOAT
                } else if s.value() == "true" || s.value() == "false" {
                    SyntaxKind::BOOL
                } else if s.value() == "null" {
                    SyntaxKind::NULL
                } else {
                    SyntaxKind::STRING
                };
                builder.token(token_kind.into(), s.value());
                builder.finish_node();
                false
            }
            YamlValue::Mapping(map) => map.build_content(builder, indent, flow_context),
            YamlValue::Sequence(seq) => seq.as_slice().build_content(builder, indent, flow_context),
            YamlValue::Set(set) => {
                if set.is_empty() {
                    // Empty set in flow style: !!set {}
                    builder.start_node(SyntaxKind::TAGGED_NODE.into());
                    builder.token(SyntaxKind::TAG.into(), "!!set");
                    builder.token(SyntaxKind::WHITESPACE.into(), " ");
                    builder.start_node(SyntaxKind::MAPPING.into());
                    builder.token(SyntaxKind::LEFT_BRACE.into(), "{");
                    builder.token(SyntaxKind::RIGHT_BRACE.into(), "}");
                    builder.finish_node(); // MAPPING
                    builder.finish_node(); // TAGGED_NODE
                    return false;
                }

                // Non-empty set: wrap in TAGGED_NODE node
                builder.start_node(SyntaxKind::TAGGED_NODE.into());
                builder.token(SyntaxKind::TAG.into(), "!!set");
                builder.token(SyntaxKind::NEWLINE.into(), "\n");
                builder.token(SyntaxKind::INDENT.into(), &" ".repeat(indent + 2));

                // Build as a mapping where each key maps to null (set semantics)
                let map: BTreeMap<String, YamlValue> = set
                    .iter()
                    .map(|k| (k.clone(), YamlValue::Scalar(ScalarValue::null())))
                    .collect();
                let ends_with_newline = map.build_content(builder, indent + 2, false);

                builder.finish_node(); // TAGGED_NODE
                                       // Return whether the inner content ended with newline
                ends_with_newline
            }
            YamlValue::OrderedMapping(omap) => {
                // Wrap in TAGGED_NODE node
                builder.start_node(SyntaxKind::TAGGED_NODE.into());
                builder.token(SyntaxKind::TAG.into(), "!!omap");
                builder.token(SyntaxKind::NEWLINE.into(), "\n");
                builder.token(SyntaxKind::INDENT.into(), &" ".repeat(indent + 2));

                // Build as a sequence of single-entry mappings to preserve order
                let seq: Vec<YamlValue> = omap
                    .iter()
                    .map(|(k, v)| {
                        let mut map = BTreeMap::new();
                        map.insert(k.clone(), v.clone());
                        YamlValue::Mapping(map)
                    })
                    .collect();
                let ends_with_newline = seq.as_slice().build_content(builder, indent + 2, false);

                builder.finish_node(); // TAGGED_NODE
                                       // Return whether the inner content ended with newline
                ends_with_newline
            }
            YamlValue::Pairs(pairs) => {
                // Wrap in TAGGED_NODE node
                builder.start_node(SyntaxKind::TAGGED_NODE.into());
                builder.token(SyntaxKind::TAG.into(), "!!pairs");
                builder.token(SyntaxKind::NEWLINE.into(), "\n");
                builder.token(SyntaxKind::INDENT.into(), &" ".repeat(indent + 2));

                // Build as a sequence of single-entry mappings
                let seq: Vec<YamlValue> = pairs
                    .iter()
                    .map(|(k, v)| {
                        let mut map = BTreeMap::new();
                        map.insert(k.clone(), v.clone());
                        YamlValue::Mapping(map)
                    })
                    .collect();
                let ends_with_newline = seq.as_slice().build_content(builder, indent + 2, false);

                builder.finish_node(); // TAGGED_NODE
                                       // Return whether the inner content ended with newline
                ends_with_newline
            }
        }
    }

    fn is_inline(&self) -> bool {
        match self {
            YamlValue::Scalar(_) => true,
            YamlValue::Mapping(map) => map.is_empty(),
            YamlValue::Sequence(seq) => seq.is_empty(),
            // For programmatically created tagged collections, we use the common style:
            // tag inline with key (e.g., "tags: !!set"), even when non-empty.
            // Note: Parsed YAML may have tags on separate lines, which is also valid,
            // but for YamlValue we choose this as the default formatting.
            YamlValue::Set(_) => true,
            YamlValue::OrderedMapping(_) => true,
            YamlValue::Pairs(_) => true,
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum CollectionStyle {
    Flow,
    Block,
}

impl AsYaml for BTreeMap<String, YamlValue> {
    fn as_node(&self) -> Option<&SyntaxNode> {
        None
    }

    fn kind(&self) -> YamlKind {
        YamlKind::Mapping
    }

    fn build_content(
        &self,
        builder: &mut rowan::GreenNodeBuilder,
        indent: usize,
        _flow_context: bool,
    ) -> bool {
        use crate::lex::SyntaxKind;

        let style = if self.is_empty() {
            CollectionStyle::Flow
        } else {
            CollectionStyle::Block
        };

        builder.start_node(SyntaxKind::MAPPING.into());

        match style {
            CollectionStyle::Flow => {
                builder.token(SyntaxKind::LEFT_BRACE.into(), "{");
                builder.token(SyntaxKind::RIGHT_BRACE.into(), "}");
            }
            CollectionStyle::Block => {
                let indent_str = " ".repeat(indent);
                let entries: Vec<_> = self.iter().collect();

                for (i, (key, value)) in entries.iter().enumerate() {
                    if i > 0 {
                        // Previous entry's newline serves as separator, just add indent
                        builder.token(SyntaxKind::INDENT.into(), &indent_str);
                    }

                    builder.start_node(SyntaxKind::MAPPING_ENTRY.into());

                    builder.start_node(SyntaxKind::KEY.into());
                    key.as_str().build_content(builder, 0, false);
                    builder.finish_node(); // KEY

                    builder.token(SyntaxKind::COLON.into(), ":");

                    builder.start_node(SyntaxKind::VALUE.into());

                    let is_complex =
                        value.kind() == YamlKind::Mapping || value.kind() == YamlKind::Sequence;

                    let value_ends_with_newline = if is_complex {
                        builder.token(SyntaxKind::NEWLINE.into(), "\n");
                        builder.token(SyntaxKind::INDENT.into(), &" ".repeat(indent + 2));
                        value.build_content(builder, indent + 2, _flow_context)
                    } else {
                        builder.token(SyntaxKind::WHITESPACE.into(), " ");
                        value.build_content(builder, 0, _flow_context)
                    };

                    builder.finish_node(); // VALUE

                    // Every MAPPING_ENTRY ends with newline
                    // Only add if value didn't already end with one
                    if !value_ends_with_newline {
                        builder.token(SyntaxKind::NEWLINE.into(), "\n");
                    }

                    builder.finish_node(); // MAPPING_ENTRY
                }
            }
        }

        builder.finish_node(); // MAPPING
                               // Only block-style non-empty mappings end with newline (from last entry)
        matches!(style, CollectionStyle::Block) && !self.is_empty()
    }

    fn is_inline(&self) -> bool {
        self.is_empty()
    }
}

impl AsYaml for Vec<YamlValue> {
    fn as_node(&self) -> Option<&SyntaxNode> {
        None
    }

    fn kind(&self) -> YamlKind {
        YamlKind::Sequence
    }

    fn build_content(
        &self,
        builder: &mut rowan::GreenNodeBuilder,
        indent: usize,
        flow_context: bool,
    ) -> bool {
        <&[YamlValue] as AsYaml>::build_content(&self.as_slice(), builder, indent, flow_context)
    }

    fn is_inline(&self) -> bool {
        <&[YamlValue] as AsYaml>::is_inline(&self.as_slice())
    }
}

impl AsYaml for &[YamlValue] {
    fn as_node(&self) -> Option<&SyntaxNode> {
        None
    }

    fn kind(&self) -> YamlKind {
        YamlKind::Sequence
    }

    fn build_content(
        &self,
        builder: &mut rowan::GreenNodeBuilder,
        indent: usize,
        _flow_context: bool,
    ) -> bool {
        use crate::lex::SyntaxKind;

        let style = if self.is_empty() {
            CollectionStyle::Flow
        } else {
            CollectionStyle::Block
        };

        builder.start_node(SyntaxKind::SEQUENCE.into());

        match style {
            CollectionStyle::Flow => {
                builder.token(SyntaxKind::LEFT_BRACKET.into(), "[");
                builder.token(SyntaxKind::RIGHT_BRACKET.into(), "]");
            }
            CollectionStyle::Block => {
                let indent_str = " ".repeat(indent);

                for (i, item) in self.iter().enumerate() {
                    if i > 0 {
                        // Previous entry's newline serves as separator, just add indent
                        builder.token(SyntaxKind::INDENT.into(), &indent_str);
                    }

                    builder.start_node(SyntaxKind::SEQUENCE_ENTRY.into());
                    builder.token(SyntaxKind::DASH.into(), "-");
                    builder.token(SyntaxKind::WHITESPACE.into(), " ");
                    let item_ends_with_newline =
                        item.build_content(builder, indent + 2, _flow_context);

                    // Every block-style SEQUENCE_ENTRY ends with NEWLINE
                    // Only add if item didn't already end with one
                    if !item_ends_with_newline {
                        builder.token(SyntaxKind::NEWLINE.into(), "\n");
                    }

                    builder.finish_node(); // SEQUENCE_ENTRY
                }
            }
        }

        builder.finish_node(); // SEQUENCE
                               // Only block-style non-empty sequences end with newline (from last entry)
        matches!(style, CollectionStyle::Block) && !self.is_empty()
    }

    fn is_inline(&self) -> bool {
        self.is_empty()
    }
}
