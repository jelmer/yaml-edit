//! Value wrapper that can represent any YAML value type (scalar, sequence, mapping).

use crate::scalar::{ScalarStyle, ScalarValue};
use std::collections::{BTreeMap, BTreeSet};
use std::fmt;

/// Represents any YAML value - scalar, sequence, mapping, or special collections.
///
/// **Deprecated:** Prefer using syntax tree types (`Mapping`, `Sequence`,
/// `Scalar`) and the `AsYaml` trait instead - they preserve formatting and
/// work with the CST directly. `YamlValue` keeps tags and scalar styles, but
/// loses comments, collection styles and other formatting.
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
    /// A sequence or mapping with an explicit tag, such as `!!map` or
    /// `!local`, other than the ones with their own variant above.
    ///
    /// Tags on scalars are kept on the [`ScalarValue`] instead; use
    /// [`YamlValue::tagged`] to attach a tag to either.
    Tagged(String, Box<YamlValue>),
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
                        // pairs() yields the KEY wrapper, so cast through
                        // extract_scalar to reach the scalar inside it.
                        // Casting the wrapper directly fails and falls back to
                        // raw text, which keeps the quotes on `'0'` and makes
                        // the entry unreachable by its actual key.
                        let key_str = match crate::yaml::extract_scalar(&key_node) {
                            Some(key_scalar) => key_scalar.as_string(),
                            None => key_node.text().to_string(),
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
                    let collection = tagged
                        .as_mapping()
                        .map(|m| m.syntax().clone())
                        .or_else(|| tagged.as_sequence().map(|s| s.syntax().clone()));
                    if let Some(collection) = collection {
                        let tag = tagged.tag()?;
                        return Some(YamlValue::Tagged(
                            tag,
                            Box::new(YamlValue::cast(collection)?),
                        ));
                    }
                    ScalarValue::from_tagged(&tagged).map(YamlValue::Scalar)
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

    /// Attach an explicit tag to `value`.
    ///
    /// A scalar keeps the tag itself (see [`ScalarValue::with_tag`]), any
    /// other value is wrapped in [`YamlValue::Tagged`].
    ///
    /// ```
    /// use yaml_edit::YamlValue;
    ///
    /// let v = YamlValue::tagged("!point", YamlValue::from(vec![1, 2]));
    /// assert_eq!(v.tag(), Some("!point"));
    /// assert_eq!(v.to_yaml_string(0), "!point\n  - 1\n  - 2");
    /// ```
    pub fn tagged(tag: impl Into<String>, value: YamlValue) -> Self {
        match value {
            YamlValue::Scalar(s) => YamlValue::Scalar(s.with_tag(tag)),
            v => YamlValue::Tagged(tag.into(), Box::new(v)),
        }
    }

    /// The explicit tag on this value, if any.
    ///
    /// `!!set`, `!!omap` and `!!pairs` values report their tag too.
    pub fn tag(&self) -> Option<&str> {
        match self {
            YamlValue::Scalar(s) => s.tag(),
            YamlValue::Sequence(_) | YamlValue::Mapping(_) => None,
            YamlValue::Set(_) => Some("!!set"),
            YamlValue::OrderedMapping(_) => Some("!!omap"),
            YamlValue::Pairs(_) => Some("!!pairs"),
            YamlValue::Tagged(tag, _) => Some(tag),
        }
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
    /// Reads the YAML 1.2 forms: decimal, hexadecimal `0x` and octal `0o`.
    /// Returns None if this is not a scalar or not an integer type.
    ///
    /// Note: This respects YAML's type system - `42` (unquoted) is an integer,
    /// but `"42"` (quoted) is a string and will return None.
    pub fn to_i64(&self) -> Option<i64> {
        self.as_scalar().and_then(ScalarValue::to_i64)
    }

    /// Try to convert this value to an f64
    ///
    /// Returns the float value if this is a scalar with Float type.
    /// Returns None if this is not a scalar or not a float type.
    ///
    /// Note: This respects YAML's type system - `3.14` (unquoted) is a float,
    /// but `"3.14"` (quoted) is a string and will return None.
    pub fn to_f64(&self) -> Option<f64> {
        self.as_scalar().and_then(ScalarValue::to_f64)
    }

    /// Try to convert this value to a bool
    ///
    /// Returns the boolean value if this is a scalar with Boolean type.
    /// Recognizes: true, True, TRUE, false, False, FALSE.
    /// Returns None if this is not a scalar or not a boolean type.
    ///
    /// Note: This respects YAML's type system - `false` (unquoted) is boolean,
    /// but `"false"` (quoted) is a string and will return None.
    pub fn to_bool(&self) -> Option<bool> {
        self.as_scalar().and_then(ScalarValue::to_bool)
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
                        item.push_entry_value(&mut result, indent + 4);
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
                        result.push_str(&Self::render_key(key));
                        result.push_str(": ");
                        value.push_entry_value(&mut result, indent + 2);
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
                        result.push_str(&Self::render_key(item));
                        result.push_str(": null");
                        result.push('\n');
                    }
                    result.truncate(result.trim_end().len());
                    result
                }
            }
            YamlValue::OrderedMapping(pairs) => Self::render_tagged_pairs("!!omap", pairs, indent),
            YamlValue::Pairs(pairs) => Self::render_tagged_pairs("!!pairs", pairs, indent),
            YamlValue::Tagged(tag, value) => {
                let content = value.to_yaml_string(indent);
                if value.is_block_collection() {
                    format!("{tag}\n{content}")
                } else {
                    format!("{tag} {content}")
                }
            }
        }
    }

    /// Whether this renders as a block collection, which has to start on a
    /// line of its own.
    fn is_block_collection(&self) -> bool {
        match self {
            YamlValue::Sequence(seq) => !seq.is_empty(),
            YamlValue::Mapping(map) => !map.is_empty(),
            _ => false,
        }
    }

    /// Render a mapping key, which has to fit on one line.
    fn render_key(key: &str) -> String {
        let scalar = ScalarValue::string(key);
        match scalar.style() {
            ScalarStyle::Literal | ScalarStyle::Folded => {
                ScalarValue::double_quoted(key).to_yaml_string()
            }
            _ => scalar.to_yaml_string(),
        }
    }

    /// Append this value after a `key: ` or `- ` indicator, where anything
    /// nested has to start at column `indent`.
    fn push_entry_value(&self, result: &mut String, indent: usize) {
        match self {
            // The indicator's own line is `indent - 2`: a key, or a `-`.
            YamlValue::Scalar(s) => result.push_str(&s.to_yaml_string_with_indent(indent - 2, 2)),
            v if v.is_block_collection() => {
                if result.ends_with(' ') {
                    result.pop();
                }
                result.push('\n');
                result.push_str(&self.to_yaml_string(indent));
            }
            // Empty collections are flow style and tagged ones open with
            // their tag, so both stay on the indicator's line.
            _ => result.push_str(&self.to_yaml_string(indent)),
        }
    }

    /// Render `!!omap` / `!!pairs`: a tagged sequence of single-key mappings.
    fn render_tagged_pairs(tag: &str, pairs: &[(String, YamlValue)], indent: usize) -> String {
        if pairs.is_empty() {
            return format!("{tag} []");
        }
        let mut result = String::from(tag);
        result.push('\n');
        let indent_str = " ".repeat(indent);
        for (key, value) in pairs {
            result.push_str(&indent_str);
            result.push_str("  - ");
            result.push_str(&Self::render_key(key));
            result.push_str(": ");
            // The key sits at indent + 4, so its value has to start right of it.
            value.push_entry_value(&mut result, indent + 6);
            result.push('\n');
        }
        result.truncate(result.trim_end().len());
        result
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
    use crate::debug::assert_cst_ok;

    fn render_as_yaml<T: crate::AsYaml>(v: &T) -> String {
        let mut builder = rowan::GreenNodeBuilder::new();
        v.build_content(&mut builder, 0, false);
        let green = builder.finish();
        let node = rowan::SyntaxNode::<crate::yaml::Lang>::new_root(green);
        node.text().to_string()
    }

    fn scalar_token_kind(v: &YamlValue) -> crate::lex::SyntaxKind {
        use rowan::ast::AstNode;
        let mut builder = rowan::GreenNodeBuilder::new();
        crate::AsYaml::build_content(v, &mut builder, 0, false);
        let green = builder.finish();
        let node = rowan::SyntaxNode::<crate::yaml::Lang>::new_root(green);
        let scalar = crate::yaml::Scalar::cast(node).expect("scalar node");
        scalar
            .syntax()
            .children_with_tokens()
            .filter_map(|c| c.into_token())
            .next()
            .expect("token")
            .kind()
    }

    #[test]
    fn test_scalar_build_content_token_kind() {
        use crate::lex::SyntaxKind;
        assert_eq!(
            scalar_token_kind(&YamlValue::from("hello")),
            SyntaxKind::STRING
        );
        assert_eq!(scalar_token_kind(&YamlValue::from(42)), SyntaxKind::INT);
        assert_eq!(scalar_token_kind(&YamlValue::from(1.5)), SyntaxKind::FLOAT);
        // Strings that happen to spell a bool or null keep their String type
        // and are quoted, so they tokenize as STRING - rendering them bare
        // would change their type on the next parse.
        assert_eq!(
            scalar_token_kind(&YamlValue::Scalar(ScalarValue::string("true"))),
            SyntaxKind::STRING
        );
        assert_eq!(
            scalar_token_kind(&YamlValue::Scalar(ScalarValue::string("false"))),
            SyntaxKind::STRING
        );
        assert_eq!(
            scalar_token_kind(&YamlValue::Scalar(ScalarValue::string("null"))),
            SyntaxKind::STRING
        );
        // Parsed values keep their detected type.
        assert_eq!(
            scalar_token_kind(&YamlValue::Scalar(ScalarValue::parse("true"))),
            SyntaxKind::BOOL
        );
        assert_eq!(
            scalar_token_kind(&YamlValue::Scalar(ScalarValue::parse("null"))),
            SyntaxKind::NULL
        );
    }

    #[test]
    fn test_scalar_build_content_quotes_like_scalar_value() {
        // YamlValue::Scalar delegates to ScalarValue so that strings needing
        // quotes get them. Emitting the raw value would re-parse as a
        // different type, or (for "a: b") as a different structure.
        for raw in ["null", "true", "42", "a: b", "", "#c", "- x"] {
            let sv = ScalarValue::string(raw);
            assert_eq!(
                render_as_yaml(&YamlValue::Scalar(sv.clone())),
                render_as_yaml(&sv),
                "YamlValue::Scalar({raw:?}) must render like the ScalarValue"
            );
        }
    }

    #[test]
    fn test_as_yaml_build_content_tagged_and_nested() {
        // Tagged collections render the tag, a newline, then content indented by
        // two spaces (indent + 2). A wrong arithmetic op would drop the indent.
        let omap = YamlValue::from_ordered_mapping(vec![
            ("a".to_string(), YamlValue::from("1")),
            ("b".to_string(), YamlValue::from("2")),
        ]);
        assert_eq!(render_as_yaml(&omap), "!!omap\n  - a: '1'\n  - b: '2'\n");

        let pairs = YamlValue::from_pairs(vec![("a".to_string(), YamlValue::from("1"))]);
        assert_eq!(render_as_yaml(&pairs), "!!pairs\n  - a: '1'\n");

        let set = YamlValue::from_set(BTreeSet::from(["a".to_string(), "b".to_string()]));
        assert_eq!(render_as_yaml(&set), "!!set\n  a: null\n  b: null\n");

        // A mapping whose value is a multi-entry mapping. The inner mapping's
        // own entry separators are indented by the indent passed down
        // (BTreeMap::build_content indent + 2), so a wrong op shows up on the
        // second inner key.
        let mut inner = BTreeMap::new();
        inner.insert("x".to_string(), YamlValue::from("1"));
        inner.insert("y".to_string(), YamlValue::from("2"));
        let mut outer = BTreeMap::new();
        outer.insert("o".to_string(), YamlValue::Mapping(inner));
        assert_eq!(render_as_yaml(&outer), "o:\n  x: '1'\n  y: '2'\n");

        // Pairs with two entries: the indent passed into the nested sequence
        // shows on the second item's "  - " separator (Pairs branch indent + 2).
        let pairs2 = YamlValue::from_pairs(vec![
            ("a".to_string(), YamlValue::from("1")),
            ("b".to_string(), YamlValue::from("2")),
        ]);
        assert_eq!(render_as_yaml(&pairs2), "!!pairs\n  - a: '1'\n  - b: '2'\n");

        // A sequence nested inside a sequence (&[]::build_content indent + 2).
        let seq = vec![YamlValue::Sequence(vec![YamlValue::from("a")])];
        assert_eq!(render_as_yaml(&seq), "- - a\n");

        // A sequence whose single item is a multi-entry mapping: the indent
        // handed to that item (&[]::build_content indent + 2) shows on the
        // second mapping key.
        let mut item = BTreeMap::new();
        item.insert("x".to_string(), YamlValue::from("1"));
        item.insert("y".to_string(), YamlValue::from("2"));
        let seq_of_map = vec![YamlValue::Mapping(item)];
        assert_eq!(render_as_yaml(&seq_of_map), "- x: '1'\n  y: '2'\n");
    }

    #[test]
    fn test_as_yaml_build_content() {
        use crate::AsYaml;

        // Vec<YamlValue> and &[YamlValue] render as a block sequence.
        let seq = vec![YamlValue::from("a"), YamlValue::from("b")];
        assert_eq!(render_as_yaml(&seq), "- a\n- b\n");
        assert_eq!(render_as_yaml(&seq.as_slice()), "- a\n- b\n");

        // BTreeMap renders as a block mapping (keys sorted).
        let mut m = BTreeMap::new();
        m.insert("k".to_string(), YamlValue::from("v"));
        m.insert("j".to_string(), YamlValue::from("w"));
        assert_eq!(render_as_yaml(&m), "j: w\nk: v\n");

        // Empty collections render in flow style.
        let empty_seq: Vec<YamlValue> = vec![];
        assert_eq!(render_as_yaml(&empty_seq), "[]");
        let empty_map: BTreeMap<String, YamlValue> = BTreeMap::new();
        assert_eq!(render_as_yaml(&empty_map), "{}");

        // A mapping whose value is a non-scalar gets a newline and deeper indent.
        let mut m2 = BTreeMap::new();
        m2.insert(
            "list".to_string(),
            YamlValue::Sequence(vec![YamlValue::from("a")]),
        );
        assert_eq!(render_as_yaml(&m2), "list:\n  - a\n");

        // YamlValue delegates to the right impl per variant.
        assert_eq!(
            render_as_yaml(&YamlValue::Sequence(seq.clone())),
            "- a\n- b\n"
        );
        assert_eq!(
            render_as_yaml(&YamlValue::Mapping(m.clone())),
            "j: w\nk: v\n"
        );
        assert_eq!(render_as_yaml(&YamlValue::from("scalar")), "scalar");

        // is_inline: empty collections are inline, non-empty block collections
        // are not. Scalars are always inline.
        assert!(!AsYaml::is_inline(&seq));
        assert!(AsYaml::is_inline(&empty_seq));
        assert!(!AsYaml::is_inline(&m));
        assert!(AsYaml::is_inline(&empty_map));
        assert!(!AsYaml::is_inline(&seq.as_slice()));
        assert!(AsYaml::is_inline(&empty_seq.as_slice()));
        assert!(AsYaml::is_inline(&YamlValue::from("x")));
        assert!(!AsYaml::is_inline(&YamlValue::Sequence(seq)));
        assert!(AsYaml::is_inline(&YamlValue::sequence()));
        assert!(!AsYaml::is_inline(&YamlValue::Mapping(m)));
        assert!(AsYaml::is_inline(&YamlValue::mapping()));
    }

    #[test]
    fn test_parse_raw_variants() {
        // parse_raw goes through YamlValue::cast, so each YAML shape must map to
        // the corresponding variant.
        assert!(YamlValue::parse_raw("hello").is_scalar());

        let seq = YamlValue::parse_raw("- a\n- b\n");
        assert_eq!(
            seq.as_sequence(),
            Some([YamlValue::from("a"), YamlValue::from("b")].as_slice())
        );

        let map = YamlValue::parse_raw("k: v\n");
        assert_eq!(map.as_mapping().map(|m| m.len()), Some(1));

        let set = YamlValue::parse_raw("!!set\n  a: ~\n  b: ~\n");
        assert_eq!(
            set.as_set(),
            Some(&BTreeSet::from(["a".to_string(), "b".to_string()]))
        );

        let omap = YamlValue::parse_raw("!!omap\n  - a: 1\n  - b: 2\n");
        assert_eq!(
            omap.as_ordered_mapping(),
            Some(
                [
                    ("a".to_string(), YamlValue::from(1)),
                    ("b".to_string(), YamlValue::from(2)),
                ]
                .as_slice()
            )
        );

        let pairs = YamlValue::parse_raw("!!pairs\n  - a: 1\n  - a: 2\n");
        assert_eq!(
            pairs.as_pairs(),
            Some(
                [
                    ("a".to_string(), YamlValue::from(1)),
                    ("a".to_string(), YamlValue::from(2)),
                ]
                .as_slice()
            )
        );
    }

    #[test]
    fn test_display_matches_to_yaml_string() {
        let map = {
            let mut m = BTreeMap::new();
            m.insert("name".to_string(), YamlValue::from("app"));
            YamlValue::Mapping(m)
        };
        assert_eq!(format!("{}", map), map.to_yaml_string(0));
        assert_eq!(format!("{}", map), "name: app");

        let scalar = YamlValue::from("hello");
        assert_eq!(format!("{}", scalar), "hello");
    }

    #[test]
    fn test_to_yaml_string_nested_indentation() {
        // Sequence holding a non-scalar item: nested mapping is indented by
        // indent + 4 relative to the dash.
        let mut inner = BTreeMap::new();
        inner.insert("h".to_string(), YamlValue::from("x"));
        let seq = YamlValue::Sequence(vec![YamlValue::Mapping(inner)]);
        assert_eq!(seq.to_yaml_string(0), "  -\n    h: x");

        // Mapping holding a non-scalar value: nested sequence is rendered at
        // indent + 2.
        let mut m = BTreeMap::new();
        m.insert(
            "k".to_string(),
            YamlValue::Sequence(vec![YamlValue::from("a")]),
        );
        assert_eq!(YamlValue::Mapping(m).to_yaml_string(0), "k:\n    - a");

        // Ordered mapping with a non-scalar value: the nested mapping has to
        // sit right of the key at indent + 4, or it becomes a sibling of it.
        let mut inner2 = BTreeMap::new();
        inner2.insert("x".to_string(), YamlValue::from("y"));
        let omap =
            YamlValue::from_ordered_mapping(vec![("a".to_string(), YamlValue::Mapping(inner2))]);
        assert_eq!(omap.to_yaml_string(0), "!!omap\n  - a:\n      x: y");

        let pairs = YamlValue::from_pairs(vec![(
            "a".to_string(),
            YamlValue::Sequence(vec![YamlValue::from("z")]),
        )]);
        assert_eq!(pairs.to_yaml_string(0), "!!pairs\n  - a:\n        - z");
    }

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
        assert_eq!(yaml, "  - item1\n  - item2\n  - item3");

        // Empty sequence
        let val = YamlValue::sequence();
        assert_eq!(val.to_yaml_string(0), "[]");
    }

    #[test]
    fn test_to_yaml_string_quotes_keys() {
        let mut m = BTreeMap::new();
        for k in ["a: b", "", "true", "x\ny"] {
            m.insert(k.to_string(), YamlValue::from("v"));
        }
        let v = YamlValue::Mapping(m);
        let out = v.to_yaml_string(0);
        assert_eq!(out, "'': v\n'a: b': v\n'true': v\n\"x\\ny\": v");
        assert_eq!(YamlValue::parse_raw(&out), v);
    }

    #[test]
    fn test_to_yaml_string_nested_block_scalars() {
        let mut inner = BTreeMap::new();
        inner.insert("s".to_string(), YamlValue::from("l1\nl2"));
        let mut m = BTreeMap::new();
        m.insert("n".to_string(), YamlValue::Mapping(inner));
        m.insert(
            "q".to_string(),
            YamlValue::from(vec![YamlValue::from("l1\nl2")]),
        );
        m.insert(
            "o".to_string(),
            YamlValue::from_ordered_mapping(vec![("a".into(), YamlValue::from("l1\nl2"))]),
        );
        let v = YamlValue::Mapping(m);
        let out = v.to_yaml_string(0);
        assert_eq!(
            out,
            "n:\n  s: |-\n    l1\n    l2\n\
             o: !!omap\n    - a: |-\n        l1\n        l2\n\
             q:\n    - |-\n      l1\n      l2"
        );
        assert_eq!(YamlValue::parse_raw(&out), v);
    }

    #[test]
    fn test_to_yaml_string_nested_keep_block_scalar() {
        let mut inner = BTreeMap::new();
        inner.insert(
            "k".to_string(),
            YamlValue::Scalar(ScalarValue::literal("x\ny\n")),
        );
        let mut m = BTreeMap::new();
        m.insert("o".to_string(), YamlValue::Mapping(inner));
        let v = YamlValue::Mapping(m);
        let out = v.to_yaml_string(0);
        assert_eq!(out, "o:\n  k: |\n    x\n    y");
        assert_eq!(YamlValue::parse_raw(&out), v);
    }

    #[test]
    fn test_to_yaml_string_nested_indented_block_scalar() {
        let mut inner = BTreeMap::new();
        inner.insert(
            "k".to_string(),
            YamlValue::Scalar(ScalarValue::literal("  x\n")),
        );
        let mut m = BTreeMap::new();
        m.insert("o".to_string(), YamlValue::Mapping(inner));
        let v = YamlValue::Mapping(m);
        let out = v.to_yaml_string(0);
        assert_eq!(out, "o:\n  k: |2\n      x");
        assert_eq!(YamlValue::parse_raw(&out), v);
    }

    #[test]
    fn test_to_yaml_string_nested_inline_values() {
        let set: BTreeSet<String> = ["#c", "a: b"].into_iter().map(String::from).collect();
        let mut m = BTreeMap::new();
        m.insert("s".to_string(), YamlValue::Set(set));
        m.insert("e".to_string(), YamlValue::mapping());
        m.insert(
            "l".to_string(),
            YamlValue::from(vec![YamlValue::sequence()]),
        );
        let v = YamlValue::Mapping(m);
        let out = v.to_yaml_string(0);
        assert_eq!(
            out,
            "e: {}\nl:\n    - []\ns: !!set\n  '#c': null\n  'a: b': null"
        );
        assert_eq!(YamlValue::parse_raw(&out), v);
    }

    fn tagged_value(src: &str) -> ScalarValue {
        let v = YamlValue::parse_raw(src);
        v.as_mapping().unwrap()["k"].as_scalar().unwrap().clone()
    }

    #[test]
    fn test_cast_core_tagged_scalars() {
        let s = tagged_value("k: !!int 42");
        assert_eq!(s.scalar_type(), crate::ScalarType::Integer);
        assert_eq!(s.to_i64(), Some(42));
        assert_eq!(s.tag(), Some("!!int"));
        assert_eq!(s.to_yaml_string(), "!!int 42");

        let s = tagged_value("k: !!int \"42\"");
        assert_eq!(s.to_i64(), Some(42));
        assert_eq!(s.style(), ScalarStyle::DoubleQuoted);
        assert_eq!(s.to_yaml_string(), "!!int \"42\"");

        let s = tagged_value("k: !<tag:yaml.org,2002:int> 7");
        assert_eq!(s.to_i64(), Some(7));
        assert_eq!(s.to_yaml_string(), "!<tag:yaml.org,2002:int> 7");

        let s = tagged_value("k: !!float 1.5");
        assert_eq!(s.scalar_type(), crate::ScalarType::Float);
        assert_eq!(s.to_f64(), Some(1.5));
        assert_eq!(s.to_yaml_string(), "!!float 1.5");

        let s = tagged_value("k: !!float 1");
        assert_eq!(s.to_f64(), Some(1.0));
        assert_eq!(s.to_yaml_string(), "!!float 1");

        let s = tagged_value("k: !!bool 'true'");
        assert_eq!(s.to_bool(), Some(true));
        assert_eq!(s.to_yaml_string(), "!!bool 'true'");

        let s = tagged_value("k: !!null null");
        assert_eq!(s.scalar_type(), crate::ScalarType::Null);
        assert_eq!(s.to_yaml_string(), "!!null null");

        let s = tagged_value("k: !!null\nj: 1");
        assert_eq!(s.scalar_type(), crate::ScalarType::Null);
        assert_eq!(s.to_yaml_string(), "!!null");

        let s = tagged_value("k: !!str 42");
        assert_eq!(s, ScalarValue::plain("42").with_tag("!!str"));
        assert_eq!(s.to_yaml_string(), "!!str 42");

        // Content that the tag cannot resolve stays a string.
        let s = tagged_value("k: !!int abc");
        assert_eq!(s, ScalarValue::plain("abc").with_tag("!!int"));
        assert_eq!(s.to_yaml_string(), "!!int abc");
    }

    #[test]
    fn test_tagged_values_round_trip() {
        for src in [
            "k: !!float 1",
            "k: !local bar",
            "k: !local \"bar\"",
            "k: !local 'a: b'",
            "k: !local true",
            "k: !local |\n  x\n  y",
            "k: ! 12",
            "!local bar",
            "k: !local\n  a: 1",
            "k: !local []",
            "k: !!map {}",
            "k: !!seq\n    - a",
            "k: !<tag:yaml.org,2002:set>\n  a: null",
            "!local\na: !!str 1",
            "!local\n  - a\n  - !!int 1",
        ] {
            let v = YamlValue::parse_raw(src);
            assert_eq!(v.to_yaml_string(0), src);
            assert_eq!(YamlValue::parse_raw(&v.to_yaml_string(0)), v);
        }
    }

    #[test]
    fn test_cast_tagged_collections() {
        let v = YamlValue::parse_raw("k: !local {a: 1}");
        let mut inner = BTreeMap::new();
        inner.insert("a".to_string(), YamlValue::from(1));
        let mut expected = BTreeMap::new();
        expected.insert(
            "k".to_string(),
            YamlValue::Tagged("!local".to_string(), Box::new(YamlValue::Mapping(inner))),
        );
        assert_eq!(v, YamlValue::Mapping(expected));

        let v = YamlValue::parse_raw("!!seq [a, b]");
        assert_eq!(
            v,
            YamlValue::tagged("!!seq", YamlValue::from(vec!["a", "b"]))
        );
        assert_eq!(v.tag(), Some("!!seq"));
    }

    #[test]
    fn test_tagged_constructor() {
        let v = YamlValue::tagged("!!str", YamlValue::from(1));
        assert_eq!(v, YamlValue::Scalar(ScalarValue::from(1).with_tag("!!str")));
        assert_eq!(v.tag(), Some("!!str"));
        assert_eq!(YamlValue::from(1).tag(), None);
        assert_eq!(YamlValue::set().tag(), Some("!!set"));
    }

    #[test]
    fn test_tagged_values_build_content() {
        let doc = crate::Document::new();
        doc.set("a", YamlValue::parse_raw("!!float 1"));
        assert_cst_ok(&doc);
        doc.set("b", YamlValue::parse_raw("!local 'x'"));
        assert_cst_ok(&doc);
        doc.set("c", YamlValue::parse_raw("!local [x]"));
        assert_cst_ok(&doc);
        doc.set("d", YamlValue::parse_raw("!!str"));
        assert_cst_ok(&doc);
        assert_eq!(
            doc.to_string(),
            "---\na: !!float 1\nb: !local 'x'\nc: !local\n  - x\nd: !!str\n"
        );
        let v = YamlValue::from_document(&doc);
        let m = v.as_mapping().unwrap();
        assert_eq!(m["a"], YamlValue::parse_raw("!!float 1"));
        assert_eq!(m["b"], YamlValue::parse_raw("!local 'x'"));
        assert_eq!(m["c"], YamlValue::parse_raw("!local [x]"));
        assert_eq!(m["d"], YamlValue::parse_raw("!!str"));
    }

    #[test]
    fn test_mapping_value() {
        let mut map = BTreeMap::new();
        map.insert("name", YamlValue::from("project"));
        map.insert("version", YamlValue::from("1.0.0"));

        let val = YamlValue::from(map);
        assert!(val.is_mapping());
        let yaml = val.to_yaml_string(0);
        assert_eq!(yaml, "name: project\nversion: 1.0.0");

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
        assert_eq!(
            yaml,
            "database:\n  host: localhost\n  port: 5432\nfeatures:\n    - auth\n    - logging\nname: app"
        );
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
        assert_eq!(yaml, "!!set\nitem1: null\nitem2: null\nitem3: null");

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
        assert_eq!(
            yaml,
            "!!omap\n  - first: value1\n  - second: value2\n  - third: value3"
        );

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
        assert_eq!(
            yaml,
            "!!pairs\n  - key1: value1\n  - key1: value2\n  - key2: value3"
        );

        // Empty pairs
        let val = YamlValue::pairs();
        assert_eq!(val.to_yaml_string(0), "!!pairs []");
    }

    #[test]
    fn test_predicates_and_accessors() {
        let scalar = YamlValue::from("s");
        let seq = YamlValue::Sequence(vec![YamlValue::from("a")]);
        let map = {
            let mut m = BTreeMap::new();
            m.insert("k".to_string(), YamlValue::from("v"));
            YamlValue::Mapping(m)
        };
        let set = YamlValue::from_set(BTreeSet::from(["x".to_string()]));
        let omap = YamlValue::from_ordered_mapping(vec![("a".to_string(), YamlValue::from("b"))]);
        let pairs = YamlValue::from_pairs(vec![("a".to_string(), YamlValue::from("b"))]);

        // Each predicate is true only for its own variant.
        assert!(scalar.is_scalar());
        assert!(!seq.is_scalar());

        assert!(seq.is_sequence());
        assert!(!scalar.is_sequence());

        assert!(map.is_mapping());
        assert!(!seq.is_mapping());

        assert!(set.is_set());
        assert!(!map.is_set());

        assert!(omap.is_ordered_mapping());
        assert!(!map.is_ordered_mapping());

        assert!(pairs.is_pairs());
        assert!(!omap.is_pairs());

        // Accessors return Some only for the matching variant, None otherwise.
        assert!(scalar.as_scalar().is_some());
        assert!(seq.as_scalar().is_none());

        assert_eq!(seq.as_sequence(), Some([YamlValue::from("a")].as_slice()));
        assert_eq!(scalar.as_sequence(), None);

        assert_eq!(map.as_mapping().map(|m| m.len()), Some(1));
        assert_eq!(scalar.as_mapping(), None);

        assert_eq!(set.as_set().map(|s| s.len()), Some(1));
        assert_eq!(map.as_set(), None);

        assert_eq!(omap.as_ordered_mapping().map(|p| p.len()), Some(1));
        assert_eq!(map.as_ordered_mapping(), None);

        assert_eq!(pairs.as_pairs().map(|p| p.len()), Some(1));
        assert_eq!(omap.as_pairs(), None);
    }

    #[test]
    fn test_accessors_mut() {
        let mut seq = YamlValue::Sequence(vec![YamlValue::from("a")]);
        seq.as_sequence_mut().unwrap().push(YamlValue::from("b"));
        assert_eq!(seq.as_sequence().map(|s| s.len()), Some(2));
        assert!(YamlValue::from("x").as_sequence_mut().is_none());

        let mut map = YamlValue::mapping();
        map.as_mapping_mut()
            .unwrap()
            .insert("k".to_string(), YamlValue::from("v"));
        assert_eq!(map.as_mapping().map(|m| m.len()), Some(1));
        assert!(YamlValue::from("x").as_mapping_mut().is_none());

        let mut set = YamlValue::set();
        set.as_set_mut().unwrap().insert("a".to_string());
        assert_eq!(set.as_set().map(|s| s.len()), Some(1));
        assert!(YamlValue::from("x").as_set_mut().is_none());

        let mut omap = YamlValue::ordered_mapping();
        omap.as_ordered_mapping_mut()
            .unwrap()
            .push(("a".to_string(), YamlValue::from("b")));
        assert_eq!(omap.as_ordered_mapping().map(|p| p.len()), Some(1));
        assert!(YamlValue::from("x").as_ordered_mapping_mut().is_none());

        let mut pairs = YamlValue::pairs();
        pairs
            .as_pairs_mut()
            .unwrap()
            .push(("a".to_string(), YamlValue::from("b")));
        assert_eq!(pairs.as_pairs().map(|p| p.len()), Some(1));
        assert!(YamlValue::from("x").as_pairs_mut().is_none());
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

        // Binary is not a YAML 1.2 integer form
        let bin_val = YamlValue::Scalar(ScalarValue::parse("0b101010"));
        assert_eq!(bin_val.to_i64(), None);

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

        // YAML 1.1 spellings are strings in YAML 1.2
        for text in ["yes", "no", "on", "off", "Yes", "tRUE"] {
            let value = YamlValue::Scalar(ScalarValue::parse(text));
            assert_eq!(value.to_bool(), None, "{text}");
        }

        let true_upper = YamlValue::Scalar(ScalarValue::parse("TRUE"));
        assert_eq!(true_upper.to_bool(), Some(true));

        let false_title = YamlValue::Scalar(ScalarValue::parse("False"));
        assert_eq!(false_title.to_bool(), Some(false));

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

/// Build a `!!omap` / `!!pairs` TAGGED_NODE wrapping a sequence of
/// single-entry mappings. Returns whether the content ended with a newline.
fn build_tagged_pair_seq(
    builder: &mut rowan::GreenNodeBuilder,
    tag: &str,
    pairs: &[(String, YamlValue)],
    indent: usize,
    flow_context: bool,
) -> bool {
    use crate::lex::SyntaxKind;

    builder.start_node(SyntaxKind::TAGGED_NODE.into());
    builder.token(SyntaxKind::TAG.into(), tag);
    if flow_context {
        builder.token(SyntaxKind::WHITESPACE.into(), " ");
    } else {
        builder.token(SyntaxKind::NEWLINE.into(), "\n");
        builder.token(SyntaxKind::INDENT.into(), &" ".repeat(indent + 2));
    }

    let seq: Vec<YamlValue> = pairs
        .iter()
        .map(|(k, v)| {
            let mut map = BTreeMap::new();
            map.insert(k.clone(), v.clone());
            YamlValue::Mapping(map)
        })
        .collect();
    let ends_with_newline = seq
        .as_slice()
        .build_content(builder, indent + 2, flow_context);

    builder.finish_node(); // TAGGED_NODE
    ends_with_newline
}

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
            YamlValue::Tagged(tag, _) => YamlKind::Tagged(std::borrow::Cow::Owned(tag.clone())),
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
            YamlValue::Scalar(s) => s.build_content(builder, indent, flow_context),
            YamlValue::Mapping(map) => map.build_content(builder, indent, flow_context),
            YamlValue::Sequence(seq) => seq.as_slice().build_content(builder, indent, flow_context),
            YamlValue::Set(set) => {
                builder.start_node(SyntaxKind::TAGGED_NODE.into());
                builder.token(SyntaxKind::TAG.into(), "!!set");
                // An empty mapping is written as `{}` in either context.
                let flow = flow_context || set.is_empty();
                if flow {
                    builder.token(SyntaxKind::WHITESPACE.into(), " ");
                } else {
                    builder.token(SyntaxKind::NEWLINE.into(), "\n");
                    builder.token(SyntaxKind::INDENT.into(), &" ".repeat(indent + 2));
                }

                // Build as a mapping where each key maps to null (set semantics)
                let map: BTreeMap<String, YamlValue> = set
                    .iter()
                    .map(|k| (k.clone(), YamlValue::Scalar(ScalarValue::null())))
                    .collect();
                let ends_with_newline = map.build_content(builder, indent + 2, flow);

                builder.finish_node(); // TAGGED_NODE
                                       // Return whether the inner content ended with newline
                ends_with_newline
            }
            YamlValue::OrderedMapping(pairs) => {
                build_tagged_pair_seq(builder, "!!omap", pairs, indent, flow_context)
            }
            YamlValue::Pairs(pairs) => {
                build_tagged_pair_seq(builder, "!!pairs", pairs, indent, flow_context)
            }
            YamlValue::Tagged(tag, value) => {
                builder.start_node(SyntaxKind::TAGGED_NODE.into());
                builder.token(SyntaxKind::TAG.into(), tag);
                let ends_with_newline = if value.is_block_collection() && !flow_context {
                    builder.token(SyntaxKind::NEWLINE.into(), "\n");
                    builder.token(SyntaxKind::INDENT.into(), &" ".repeat(indent + 2));
                    value.build_content(builder, indent + 2, false)
                } else {
                    builder.token(SyntaxKind::WHITESPACE.into(), " ");
                    value.build_content(builder, indent, flow_context)
                };
                builder.finish_node(); // TAGGED_NODE
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
            YamlValue::Tagged(_, _) => true,
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
        flow_context: bool,
    ) -> bool {
        use crate::lex::SyntaxKind;

        let style = if self.is_empty() || flow_context {
            CollectionStyle::Flow
        } else {
            CollectionStyle::Block
        };

        builder.start_node(SyntaxKind::MAPPING.into());

        match style {
            CollectionStyle::Flow => {
                builder.token(SyntaxKind::LEFT_BRACE.into(), "{");
                for (i, (key, value)) in self.iter().enumerate() {
                    builder.start_node(SyntaxKind::MAPPING_ENTRY.into());
                    builder.start_node(SyntaxKind::KEY.into());
                    ScalarValue::string(key).build_content(builder, 0, true);
                    builder.finish_node(); // KEY
                    builder.token(SyntaxKind::COLON.into(), ":");
                    builder.token(SyntaxKind::WHITESPACE.into(), " ");
                    builder.start_node(SyntaxKind::VALUE.into());
                    value.build_content(builder, indent, true);
                    builder.finish_node(); // VALUE
                    if i + 1 < self.len() {
                        builder.token(SyntaxKind::COMMA.into(), ",");
                        builder.token(SyntaxKind::WHITESPACE.into(), " ");
                    }
                    builder.finish_node(); // MAPPING_ENTRY
                }
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
                        value.build_content(builder, indent + 2, flow_context)
                    } else {
                        builder.token(SyntaxKind::WHITESPACE.into(), " ");
                        value.build_content(builder, 0, flow_context)
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
        flow_context: bool,
    ) -> bool {
        use crate::lex::SyntaxKind;

        let style = if self.is_empty() || flow_context {
            CollectionStyle::Flow
        } else {
            CollectionStyle::Block
        };

        builder.start_node(SyntaxKind::SEQUENCE.into());

        match style {
            CollectionStyle::Flow => {
                builder.token(SyntaxKind::LEFT_BRACKET.into(), "[");
                for (i, item) in self.iter().enumerate() {
                    builder.start_node(SyntaxKind::SEQUENCE_ENTRY.into());
                    item.build_content(builder, indent, true);
                    if i + 1 < self.len() {
                        builder.token(SyntaxKind::COMMA.into(), ",");
                        builder.token(SyntaxKind::WHITESPACE.into(), " ");
                    }
                    builder.finish_node(); // SEQUENCE_ENTRY
                }
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
                        item.build_content(builder, indent + 2, flow_context);

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
