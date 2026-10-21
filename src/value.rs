//! Value wrapper that can represent any YAML value type (scalar, sequence, mapping).

use crate::scalar::ScalarValue;
use std::collections::{BTreeMap, BTreeSet};
use std::fmt;

pub(crate) mod convert;

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
    ///
    /// Converted from a document, each key is held by its string form:
    /// the canonical value of a scalar (`0x10` is `"16"`, `~` is
    /// `"null"`) and a flow rendering of a collection, with mapping
    /// entries sorted (`{y: 1, x: 2}` is `"{x: 2, y: 1}"`). Equal keys
    /// have the same string form, however they are written.
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
    /// Convert a syntax node to a YamlValue.
    ///
    /// Returns `Ok(None)` if `node` is not a YAML value, such as a
    /// [`Document`](crate::Document) node. A `KEY` or `VALUE` wrapper is
    /// converted as its content.
    ///
    /// Content that a YamlValue cannot hold is an error rather than being
    /// dropped: an alias, which this cannot resolve without the document's
    /// anchors ([`YamlValue::from_document`] and
    /// [`DocumentResolvedExt::get_resolved`] resolve them), a mapping with
    /// the same key twice, distinct mapping keys with the same string form, such as `1` and
    /// `'1'`, and `!!set`, `!!omap` or `!!pairs` content of the wrong shape.
    /// A merge key (`<<`) is kept as an ordinary key.
    ///
    /// [`DocumentResolvedExt::get_resolved`]: crate::anchor_resolution::DocumentResolvedExt::get_resolved
    ///
    /// ```
    /// use std::str::FromStr;
    /// use rowan::ast::AstNode;
    /// use yaml_edit::{Document, YamlValue};
    /// use yaml_edit::anchor_resolution::ResolveError;
    ///
    /// let doc = Document::from_str("a: &x 1\nb: [*x, 2]\n").unwrap();
    /// let b = doc.as_mapping().unwrap().get("b").unwrap();
    /// assert_eq!(
    ///     YamlValue::cast(b.as_sequence().unwrap().syntax().clone()),
    ///     Err(ResolveError::UnresolvedAlias { alias: "x".to_string() })
    /// );
    /// ```
    pub fn cast(
        node: rowan::SyntaxNode<crate::yaml::Lang>,
    ) -> Result<Option<Self>, crate::anchor_resolution::ResolveError> {
        crate::as_yaml::YamlNode::from_syntax_peeled(node).map_or(Ok(None), |node| {
            convert::resolved_value(node, None, &mut Vec::new())
        })
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
    ///
    /// Input without a document, such as an empty string, is null.
    ///
    /// Fails with [`YamlError::Parse`](crate::YamlError::Parse) if
    /// `yaml_str` does not parse, with
    /// [`YamlError::InvalidOperation`](crate::YamlError::InvalidOperation)
    /// if it holds more than one document, and with
    /// [`YamlError::Resolve`](crate::YamlError::Resolve) where
    /// [`YamlValue::from_document`] fails. Convert each document of a
    /// multi-document stream with [`YamlValue::from_document`] instead.
    pub fn parse_raw(yaml_str: &str) -> crate::YamlResult<Self> {
        use std::str::FromStr;

        let file = crate::YamlFile::from_str(yaml_str)?;
        let mut docs = file.documents();
        let first = docs.next();
        if docs.next().is_some() {
            return Err(crate::YamlError::InvalidOperation {
                operation: "YamlValue::parse_raw".to_string(),
                reason: "Input contains multiple YAML documents. Use YamlFile::from_str() and convert each with YamlValue::from_document().".to_string(),
            });
        }
        match first {
            Some(doc) => Ok(Self::from_document(&doc)?),
            None => Ok(YamlValue::Scalar(ScalarValue::null())),
        }
    }

    /// Convert a Document to a YamlValue
    ///
    /// Unlike [`YamlValue::cast`], this resolves aliases against the
    /// document's anchors and applies merge keys (`<<`), as
    /// [`DocumentResolvedExt::get_resolved`] does. Merge keys are a YAML 1.1
    /// type that YAML 1.2 does not define, but are still widely used.
    /// An empty document is null.
    ///
    /// Fails if an alias is undefined or recursive, a merge key operand is
    /// not a mapping, or on other content [`YamlValue::cast`] reports.
    ///
    /// [`DocumentResolvedExt::get_resolved`]: crate::anchor_resolution::DocumentResolvedExt::get_resolved
    pub fn from_document(
        doc: &crate::yaml::Document,
    ) -> Result<Self, crate::anchor_resolution::ResolveError> {
        use rowan::ast::AstNode;
        let doc = crate::yaml::Document::cast(crate::nodes::immutable_copy(doc.syntax()))
            .expect("a copy of a document is a document");
        let registry = crate::anchor_resolution::AnchorRegistry::from_document(&doc);
        for child in doc.syntax().children() {
            let Some(node) = crate::as_yaml::YamlNode::from_syntax_peeled(child) else {
                continue;
            };
            if let Some(value) = convert::resolved_value(node, Some(&registry), &mut Vec::new())? {
                return Ok(value);
            }
        }
        // An empty node is null.
        Ok(YamlValue::Scalar(ScalarValue::null()))
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
            YamlValue::Set(_) => Some(crate::tag::SET),
            YamlValue::OrderedMapping(_) => Some(crate::tag::OMAP),
            YamlValue::Pairs(_) => Some(crate::tag::PAIRS),
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

    /// Convert to YAML string representation, written at column `indent`:
    /// a block mapping or sequence has every line indented by `indent`, and
    /// anything else has the lines after its first, such as the body of a
    /// block scalar, indented past it.
    ///
    /// The text has no final line break, except where a block scalar ends
    /// it with an empty line: that line needs its line break, or the value
    /// would read back without it.
    pub fn to_yaml_string(&self, indent: usize) -> String {
        let mut builder = rowan::GreenNodeBuilder::new();
        builder.start_node(crate::lex::SyntaxKind::ROOT.into());
        build_value(self, &mut builder, indent, None);
        builder.finish_node();
        let built = SyntaxNode::new_root(builder.finish()).text().to_string();
        let mut text = if self.is_block_collection() {
            " ".repeat(indent)
        } else {
            String::new()
        };
        text.push_str(built.strip_suffix('\n').unwrap_or(&built));
        if text.ends_with('\n') {
            text.push('\n');
        }
        text
    }

    /// Whether this is written as a block collection, which has to start
    /// on a line of its own.
    fn is_block_collection(&self) -> bool {
        match self {
            YamlValue::Sequence(seq) => !seq.is_empty(),
            YamlValue::Mapping(map) => !map.is_empty(),
            _ => false,
        }
    }
}

impl fmt::Display for YamlValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_yaml_string(0))
    }
}

// Convenience conversions from common types
macro_rules! impl_from_scalar {
    ($($t:ty),*) => {
        $(
            impl From<$t> for YamlValue {
                fn from(value: $t) -> Self {
                    YamlValue::Scalar(ScalarValue::from(value))
                }
            }
        )*
    };
}

impl_from_scalar!(String, &str, i32, i64, f32, f64, bool);

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

// AsYaml trait implementation for YamlValue
use crate::as_yaml::{
    build_in, build_mapping, build_sequence, build_tagged, AsYaml, FlowQuoting, YamlKind,
};
use crate::yaml::SyntaxNode;

/// Emit a `!!omap` / `!!pairs` TAGGED_NODE wrapping a sequence of
/// single-entry mappings. Returns whether the content ended with a newline.
fn build_tagged_pair_seq(
    builder: &mut rowan::GreenNodeBuilder,
    tag: &str,
    pairs: &[(String, YamlValue)],
    indent: usize,
    flow: Option<FlowQuoting>,
) -> bool {
    let block = flow.is_none() && !pairs.is_empty();
    build_tagged(builder, tag, block, indent, |builder| {
        let items = pairs
            .iter()
            .map(|(k, v)| YamlValue::Mapping(BTreeMap::from([(k.clone(), v.clone())])));
        build_sequence(builder, items, indent + 2, flow)
    })
}

/// Emit `value`, inside a flow collection quoting strings as `flow` says
/// if it is set.
fn build_value(
    value: &YamlValue,
    builder: &mut rowan::GreenNodeBuilder,
    indent: usize,
    flow: Option<FlowQuoting>,
) -> bool {
    match value {
        YamlValue::Scalar(s) => build_in(s, builder, indent, flow),
        YamlValue::Mapping(map) => build_mapping(builder, map.iter(), indent, flow),
        YamlValue::Sequence(seq) => build_sequence(builder, seq.iter(), indent, flow),
        YamlValue::Set(set) => crate::as_yaml::build_set_content(
            builder,
            set.iter().map(|m| ScalarValue::string(m.as_str())),
            indent,
            flow,
        ),
        YamlValue::OrderedMapping(pairs) => {
            build_tagged_pair_seq(builder, crate::tag::OMAP, pairs, indent, flow)
        }
        YamlValue::Pairs(pairs) => {
            build_tagged_pair_seq(builder, crate::tag::PAIRS, pairs, indent, flow)
        }
        YamlValue::Tagged(tag, value) => {
            let block = value.is_block_collection() && flow.is_none();
            let content_indent = if block { indent + 2 } else { indent };
            build_tagged(builder, tag, block, indent, |builder| {
                build_value(value, builder, content_indent, flow)
            })
        }
    }
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
            YamlValue::Set(_) => YamlKind::Tagged(std::borrow::Cow::Borrowed(crate::tag::SET)),
            YamlValue::OrderedMapping(_) => {
                YamlKind::Tagged(std::borrow::Cow::Borrowed(crate::tag::OMAP))
            }
            YamlValue::Pairs(_) => YamlKind::Tagged(std::borrow::Cow::Borrowed(crate::tag::PAIRS)),
            YamlValue::Tagged(tag, _) => YamlKind::Tagged(std::borrow::Cow::Owned(tag.clone())),
        }
    }

    fn build_content(
        &self,
        builder: &mut rowan::GreenNodeBuilder,
        indent: usize,
        flow_context: bool,
    ) -> bool {
        build_value(
            self,
            builder,
            indent,
            flow_context.then_some(FlowQuoting::Minimal),
        )
    }

    fn build_flow_content(
        &self,
        builder: &mut rowan::GreenNodeBuilder,
        indent: usize,
        quoting: FlowQuoting,
    ) -> bool {
        build_value(self, builder, indent, Some(quoting))
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
        build_mapping(
            builder,
            self.iter(),
            indent,
            flow_context.then_some(FlowQuoting::Minimal),
        )
    }

    fn build_flow_content(
        &self,
        builder: &mut rowan::GreenNodeBuilder,
        indent: usize,
        quoting: FlowQuoting,
    ) -> bool {
        build_mapping(builder, self.iter(), indent, Some(quoting))
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
        build_sequence(
            builder,
            self.iter(),
            indent,
            flow_context.then_some(FlowQuoting::Minimal),
        )
    }

    fn build_flow_content(
        &self,
        builder: &mut rowan::GreenNodeBuilder,
        indent: usize,
        quoting: FlowQuoting,
    ) -> bool {
        build_sequence(builder, self.iter(), indent, Some(quoting))
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
        build_sequence(
            builder,
            self.iter(),
            indent,
            flow_context.then_some(FlowQuoting::Minimal),
        )
    }

    fn build_flow_content(
        &self,
        builder: &mut rowan::GreenNodeBuilder,
        indent: usize,
        quoting: FlowQuoting,
    ) -> bool {
        build_sequence(builder, self.iter(), indent, Some(quoting))
    }

    fn is_inline(&self) -> bool {
        self.is_empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::str::FromStr;

    /// [`YamlValue::cast`] on the content of the single document in `yaml`.
    fn cast_root(yaml: &str) -> Result<Option<YamlValue>, crate::anchor_resolution::ResolveError> {
        use rowan::ast::AstNode;
        let doc = crate::Document::from_str(yaml).unwrap();
        let content = doc
            .syntax()
            .children()
            .find(|n| crate::as_yaml::YamlNode::from_syntax_peeled(n.clone()).is_some())
            .unwrap();
        YamlValue::cast(content)
    }

    #[test]
    fn test_cast_alias_is_error() {
        use crate::anchor_resolution::ResolveError;
        for yaml in [
            "x: &x 1\ny: [*x, 2]\n",
            "x: &x 1\ny: *x\n",
            "&x a: 1\n*x : 2\n",
            "x: &x a\ny: !!set {? *x}\n",
            "x: &x {a: 1}\ny: !!omap [*x]\n",
            "x: &x 1\ny: !!pairs [a: *x]\n",
            "x: &x 1\ny: !local [*x]\n",
            "x: &x {a: 1}\ny: {<<: *x}\n",
        ] {
            assert_eq!(
                cast_root(yaml),
                Err(ResolveError::UnresolvedAlias {
                    alias: "x".to_string()
                }),
                "{yaml:?}"
            );
        }
        assert_eq!(
            ResolveError::UnresolvedAlias {
                alias: "x".to_string()
            }
            .to_string(),
            "alias \"x\" cannot be converted without resolving it; \
             use get_resolved to resolve aliases"
        );
    }

    #[test]
    fn test_cast_key_collision_is_error() {
        use crate::anchor_resolution::ResolveError;
        assert_eq!(
            cast_root("1: a\n'1': b\n"),
            Err(ResolveError::KeyCollision {
                first: "1".to_string(),
                second: "'1'".to_string()
            })
        );
    }

    #[test]
    fn test_cast_invalid_special_collections() {
        use crate::anchor_resolution::ResolveError;
        assert_eq!(
            cast_root("!!set {? [a], ? b}\n"),
            Err(ResolveError::InvalidSetMember {
                member: "[a]".to_string()
            })
        );
        assert_eq!(
            cast_root("!!set [a, b]\n"),
            Err(ResolveError::InvalidSetContent {
                content: "[a, b]".to_string()
            })
        );
        assert_eq!(
            cast_root("!!omap {a: 1}\n"),
            Err(ResolveError::InvalidPairsContent {
                tag: "!!omap".to_string(),
                content: "{a: 1}".to_string()
            })
        );
        // A collection tag with no content tags an empty scalar.
        for (yaml, expected) in [
            (
                "!!set\n",
                ResolveError::InvalidSetContent {
                    content: String::new(),
                },
            ),
            (
                "!!omap\n",
                ResolveError::InvalidPairsContent {
                    tag: "!!omap".to_string(),
                    content: String::new(),
                },
            ),
            (
                "- !!pairs\n- a: 1\n",
                ResolveError::InvalidPairsContent {
                    tag: "!!pairs".to_string(),
                    content: String::new(),
                },
            ),
        ] {
            let doc = crate::Document::from_str(yaml).unwrap();
            assert_eq!(YamlValue::from_document(&doc), Err(expected), "{yaml:?}");
        }
        for (yaml, tag, item) in [
            ("!!omap [a, d: 3]\n", "!!omap", "a"),
            ("!!pairs [{b: 1, c: 2}]\n", "!!pairs", "{b: 1, c: 2}"),
        ] {
            assert_eq!(
                cast_root(yaml),
                Err(ResolveError::InvalidPairsItem {
                    tag: tag.to_string(),
                    item: item.to_string()
                }),
                "{yaml:?}"
            );
        }
    }

    #[test]
    fn test_cast_duplicate_key_is_error() {
        use crate::anchor_resolution::ResolveError;
        let expected = ResolveError::DuplicateKey {
            first: "a".to_string(),
            second: "a".to_string(),
        };
        let yaml = "a: 1\nb: 2\na: 3\n";
        assert_eq!(cast_root(yaml), Err(expected.clone()));
        let doc = crate::Document::from_str(yaml).unwrap();
        assert_eq!(YamlValue::from_document(&doc), Err(expected.clone()));
        match YamlValue::parse_raw(yaml) {
            Err(crate::YamlError::Resolve(err)) => assert_eq!(err, expected),
            other => panic!("unexpected result: {other:?}"),
        }
        // Without a registry `<<` is an ordinary key, so two of them are
        // duplicates.
        assert_eq!(
            cast_root("<<: {a: 1}\n<<: {b: 2}\n"),
            Err(ResolveError::DuplicateKey {
                first: "<<".to_string(),
                second: "<<".to_string(),
            })
        );
    }

    #[test]
    fn test_cast_keeps_merge_key() {
        assert_eq!(
            cast_root("<<: {a: 1}\nb: 2\n"),
            Ok(Some(YamlValue::Mapping(BTreeMap::from([
                (
                    "<<".to_string(),
                    YamlValue::Mapping(BTreeMap::from([("a".to_string(), YamlValue::from(1))]))
                ),
                ("b".to_string(), YamlValue::from(2)),
            ]))))
        );
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

    #[test]
    fn test_tagged_alias_to_tagged_alias() {
        use std::str::FromStr;
        let doc = crate::Document::from_str("b: &b 1\na: &a !u *b\nk: !t *a\n").unwrap();
        let value = YamlValue::from_document(&doc).unwrap();
        let YamlValue::Mapping(map) = value else {
            panic!("expected a mapping");
        };
        assert_eq!(
            map["k"],
            YamlValue::Scalar(ScalarValue::parse("1").with_resolved_tag("!t".to_string()))
        );
        assert_eq!(map["k"].tag(), Some("!t"));
    }

    #[test]
    fn test_omap_items_tagged_map() {
        use std::str::FromStr;
        let doc = crate::Document::from_str("o: !!omap [!!map {a: 1}, {b: 2}]\n").unwrap();
        let expected = YamlValue::OrderedMapping(vec![
            ("a".to_string(), YamlValue::from(1i64)),
            ("b".to_string(), YamlValue::from(2i64)),
        ]);
        let o = doc.as_mapping().unwrap().get("o").unwrap();
        assert_eq!(
            YamlValue::cast(o.syntax().clone()),
            Ok(Some(expected.clone()))
        );
        assert_eq!(
            YamlValue::from_document(&doc),
            Ok(YamlValue::from_mapping(BTreeMap::from([(
                "o".to_string(),
                expected
            )])))
        );

        let doc = crate::Document::from_str("o: !!omap [!!map [a]]\n").unwrap();
        assert_eq!(
            YamlValue::from_document(&doc),
            Err(crate::anchor_resolution::ResolveError::InvalidPairsItem {
                tag: "!!omap".to_string(),
                item: "!!map [a]".to_string(),
            })
        );
    }
}
