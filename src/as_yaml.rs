//! AsYaml trait and YamlNode for unified access to YAML values.
//!
//! ## Type hierarchy
//!
//! This library has two tiers:
//!
//! - **CST types** ([`Document`](crate::yaml::Document), [`Mapping`](crate::yaml::Mapping),
//!   [`Sequence`](crate::nodes::sequence::Sequence), [`Scalar`](crate::yaml::Scalar),
//!   [`TaggedNode`](crate::yaml::TaggedNode)) — format-preserving wrappers around the
//!   concrete syntax tree.  Parse a file to get these; navigate and mutate them in place.
//!
//! - **Input types** (`&str`, `i64`, `bool`, [`MappingBuilder`](crate::builder::MappingBuilder), …)
//!   — supply these to mutation methods such as
//!   [`Mapping::set`](crate::yaml::Mapping::set).  They implement [`AsYaml`] but are
//!   never returned from navigation methods.
//!
//! [`YamlNode`] is the type-erased return type for navigation: you get one back from
//! [`Mapping::get`](crate::yaml::Mapping::get), iterator methods like
//! [`Mapping::keys`](crate::yaml::Mapping::keys), etc.  It is always backed by a real
//! CST node; match on it to get the concrete type.

use crate::yaml::{Alias, Mapping, Scalar, Sequence, SyntaxNode, TaggedNode};
use rowan::ast::AstNode;
use std::borrow::Cow;
use std::fmt;

/// The kind of YAML value.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum YamlKind {
    /// A mapping (object/dictionary)
    Mapping,
    /// A sequence (array/list)
    Sequence,
    /// A scalar value (string, number, boolean, null)
    Scalar,
    /// An alias reference (e.g. `*anchor_name`)
    Alias,
    /// A document (top-level container)
    Document,
    /// A tagged value (e.g. `!!set`, `!!omap`, `!!pairs`).
    ///
    /// Known built-in tags use a `'static` string; custom tags carry an owned string.
    Tagged(Cow<'static, str>),
}

/// Trait for types that can be represented as YAML content.
///
/// Bridges the gap between CST nodes (which preserve formatting) and raw Rust
/// values (which are convenient for constructing new content).
///
/// # Using `AsYaml` as a bound
///
/// Mutation methods such as [`Mapping::set`](crate::yaml::Mapping::set) accept
/// `impl AsYaml`, so you can pass any of:
///
/// - A string literal (`"hello"`)
/// - A number (`42_i64`, `3.14_f64`, …)
/// - A boolean (`true`)
/// - An existing CST node ([`Mapping`], [`Sequence`], [`Scalar`], [`YamlNode`], …)
/// - A builder ([`MappingBuilder`](crate::builder::MappingBuilder), [`SequenceBuilder`](crate::builder::SequenceBuilder))
pub trait AsYaml {
    /// Returns a reference to the underlying `SyntaxNode` if one exists.
    ///
    /// CST wrappers (`Mapping`, `Sequence`, `Scalar`, `TaggedNode`, `YamlNode`)
    /// return `Some`.  Raw Rust types (`i64`, `String`, etc.) return `None`.
    fn as_node(&self) -> Option<&SyntaxNode>;

    /// Returns the kind of YAML value this represents.
    fn kind(&self) -> YamlKind;

    /// Serialize this value into a `GreenNodeBuilder`.
    ///
    /// CST-backed types copy their node content; raw types synthesize an
    /// appropriate CST structure.
    ///
    /// Returns `true` if the emitted content ends with a `NEWLINE` token
    /// (used to avoid double newlines when nesting collections).
    fn build_content(
        &self,
        builder: &mut rowan::GreenNodeBuilder,
        indent: usize,
        flow_context: bool,
    ) -> bool;

    /// Returns whether this value should be rendered on the same line as its key.
    ///
    /// `true` for scalars and empty collections; `false` for non-empty block
    /// collections.
    fn is_inline(&self) -> bool;
}

/// Compare two [`AsYaml`] values for semantic equality.
///
/// Semantic equality ignores formatting: `name`, `"name"`, and `'name'` all
/// compare equal.  Both sides can be any combination of CST nodes and raw
/// Rust values.
pub fn yaml_eq<A, B>(a: &A, b: &B) -> bool
where
    A: AsYaml + ?Sized,
    B: AsYaml + ?Sized,
{
    // If the left side has a backing node, dispatch on its concrete kind.
    if let Some(node) = a.as_node() {
        use crate::lex::SyntaxKind;
        return match node.kind() {
            SyntaxKind::SCALAR => Scalar::cast(node.clone()).is_some_and(|s| scalar_eq_rhs(&s, b)),
            SyntaxKind::MAPPING => {
                Mapping::cast(node.clone()).is_some_and(|m| mapping_eq_rhs(&m, b))
            }
            SyntaxKind::SEQUENCE => {
                Sequence::cast(node.clone()).is_some_and(|s| sequence_eq_rhs(&s, b))
            }
            SyntaxKind::TAGGED_NODE => {
                TaggedNode::cast(node.clone()).is_some_and(|t| tagged_eq_rhs(&t, b))
            }
            _ => false,
        };
    }

    // Left side is raw — try the right side's node instead (symmetric).
    if let Some(node) = b.as_node() {
        use crate::lex::SyntaxKind;
        return match node.kind() {
            SyntaxKind::SCALAR => Scalar::cast(node.clone()).is_some_and(|s| scalar_eq_rhs(&s, a)),
            SyntaxKind::MAPPING => {
                Mapping::cast(node.clone()).is_some_and(|m| mapping_eq_rhs(&m, a))
            }
            SyntaxKind::SEQUENCE => {
                Sequence::cast(node.clone()).is_some_and(|s| sequence_eq_rhs(&s, a))
            }
            SyntaxKind::TAGGED_NODE => {
                TaggedNode::cast(node.clone()).is_some_and(|t| tagged_eq_rhs(&t, a))
            }
            _ => false,
        };
    }

    // Both sides are raw — compare by kind, then decoded scalar string.
    if a.kind() != b.kind() {
        return false;
    }
    match (raw_scalar_str(a), raw_scalar_str(b)) {
        (Some(sa), Some(sb)) => sa == sb,
        _ => false,
    }
}

/// Extract the decoded scalar string from a raw (no-node) `AsYaml` value.
fn raw_scalar_str<T: AsYaml + ?Sized>(v: &T) -> Option<String> {
    if v.as_node().is_some() || v.kind() != YamlKind::Scalar {
        return None;
    }
    let mut builder = rowan::GreenNodeBuilder::new();
    v.build_content(&mut builder, 0, false);
    let green = builder.finish();
    let node = rowan::SyntaxNode::<crate::yaml::Lang>::new_root(green);
    Scalar::cast(node.clone())
        .map(|s| s.as_string())
        .or_else(|| Some(node.text().to_string()))
}

/// Get the semantic type and normalized value of a scalar for YAML-level comparison.
///
/// Returns (type_kind, normalized_value) where normalized values are comparable:
/// - Different integer formats (123, 0x7B, 0o173) normalize to same i64
/// - Different null representations (null, ~, Null) all become "null"
/// - Different boolean cases (true, True, TRUE) normalize to lowercase
fn scalar_semantic_value(scalar: &Scalar) -> Option<(crate::lex::SyntaxKind, String)> {
    use crate::lex::SyntaxKind;
    use crate::scalar::ScalarValue;

    // Get the first token to determine the lexical type
    let token = scalar.0.first_token()?;
    let kind = token.kind();
    let text = token.text();

    let normalized = match kind {
        SyntaxKind::INT => {
            // Normalize all integer representations to their numeric value
            ScalarValue::parse_integer(text)
                .map(|v| v.to_string())
                .unwrap_or_else(|| text.to_string())
        }
        SyntaxKind::FLOAT => {
            // Normalize float representations
            text.parse::<f64>()
                .map(|v| v.to_string())
                .unwrap_or_else(|_| text.to_string())
        }
        SyntaxKind::BOOL => {
            // Normalize booleans to lowercase
            text.to_lowercase()
        }
        SyntaxKind::NULL => {
            // All null representations become "null"
            "null".to_string()
        }
        SyntaxKind::STRING => {
            // For strings, use the unescaped/unquoted value
            scalar.as_string()
        }
        _ => {
            // Block scalars, MERGE_KEY, etc. - use as_string()
            scalar.as_string()
        }
    };

    Some((kind, normalized))
}

fn scalar_eq_rhs<B: AsYaml + ?Sized>(lhs: &Scalar, rhs: &B) -> bool {
    // Get or build the RHS scalar node
    let rhs_scalar = if let Some(node) = rhs.as_node() {
        let Some(scalar) = Scalar::cast(node.clone()) else {
            return false;
        };
        scalar
    } else {
        // RHS is a raw AsYaml value (e.g., &str, &i64) - build it into a scalar
        if rhs.kind() != YamlKind::Scalar {
            return false;
        }
        let mut builder = rowan::GreenNodeBuilder::new();
        rhs.build_content(&mut builder, 0, false);
        let green = builder.finish();
        let node = rowan::SyntaxNode::<crate::yaml::Lang>::new_root(green);
        let Some(scalar) = Scalar::cast(node) else {
            return false;
        };
        scalar
    };

    // Get semantic values for YAML-level comparison
    let Some((lhs_kind, lhs_value)) = scalar_semantic_value(lhs) else {
        return false;
    };
    let Some((rhs_kind, rhs_value)) = scalar_semantic_value(&rhs_scalar) else {
        return false;
    };

    // For YAML semantic equality:
    // - Types must match (STRING != INT even if text looks the same)
    // - Normalized values must match (0x7B == 123, true == True)
    lhs_kind == rhs_kind && lhs_value == rhs_value
}

fn mapping_eq_rhs<B: AsYaml + ?Sized>(lhs: &Mapping, rhs: &B) -> bool {
    let Some(node) = rhs.as_node() else {
        return false;
    };
    let Some(r) = Mapping::cast(node.clone()) else {
        return false;
    };
    let lhs_pairs: Vec<_> = lhs.pairs().collect();
    let rhs_pairs: Vec<_> = r.pairs().collect();
    if lhs_pairs.len() != rhs_pairs.len() {
        return false;
    }
    // pairs() yields raw KEY/VALUE wrapper nodes — peel them before comparing.
    lhs_pairs
        .iter()
        .zip(rhs_pairs.iter())
        .all(|((lk, lv), (rk, rv))| {
            let Some(lk) = YamlNode::from_syntax_peeled(lk.clone()) else {
                return false;
            };
            let Some(rk) = YamlNode::from_syntax_peeled(rk.clone()) else {
                return false;
            };
            let Some(lv) = YamlNode::from_syntax_peeled(lv.clone()) else {
                return false;
            };
            let Some(rv) = YamlNode::from_syntax_peeled(rv.clone()) else {
                return false;
            };
            yaml_eq(&lk, &rk) && yaml_eq(&lv, &rv)
        })
}

fn sequence_eq_rhs<B: AsYaml + ?Sized>(lhs: &Sequence, rhs: &B) -> bool {
    let Some(node) = rhs.as_node() else {
        return false;
    };
    let Some(r) = Sequence::cast(node.clone()) else {
        return false;
    };
    let lhs_items: Vec<_> = lhs.items().collect();
    let rhs_items: Vec<_> = r.items().collect();
    if lhs_items.len() != rhs_items.len() {
        return false;
    }
    // items() already yields peeled content nodes.
    lhs_items.iter().zip(rhs_items.iter()).all(|(l, r)| {
        match (
            YamlNode::from_syntax(l.clone()),
            YamlNode::from_syntax(r.clone()),
        ) {
            (Some(l), Some(r)) => yaml_eq(&l, &r),
            _ => false,
        }
    })
}

fn tagged_eq_rhs<B: AsYaml + ?Sized>(lhs: &TaggedNode, rhs: &B) -> bool {
    let Some(node) = rhs.as_node() else {
        return false;
    };
    TaggedNode::cast(node.clone())
        .is_some_and(|r| lhs.tag() == r.tag() && lhs.as_string() == r.as_string())
}

/// A type-erased handle to a CST node returned from navigation methods.
///
/// You get a `YamlNode` back from methods like
/// [`Mapping::get`](crate::yaml::Mapping::get),
/// [`Sequence::get`](crate::nodes::sequence::Sequence::get),
/// [`Mapping::keys`](crate::yaml::Mapping::keys), etc.
///
/// Match on the variants to get the concrete type, or use the helper
/// methods [`as_scalar`](Self::as_scalar), [`as_mapping`](Self::as_mapping), etc.
///
/// ```
/// use yaml_edit::{Document, YamlNode};
/// use std::str::FromStr;
///
/// let doc = Document::from_str("name: Alice\nage: 30").unwrap();
///
/// if let Some(node) = doc.get("name") {
///     match node {
///         YamlNode::Scalar(s) => println!("scalar: {}", s.as_string()),
///         YamlNode::Mapping(m) => println!("mapping with {} keys", m.len()),
///         YamlNode::Sequence(s) => println!("sequence with {} items", s.len()),
///         YamlNode::Alias(a) => println!("alias: *{}", a.name()),
///         YamlNode::TaggedNode(t) => println!("tagged: {:?}", t.tag()),
///     }
/// }
/// ```
#[derive(Debug, Clone, PartialEq)]
pub enum YamlNode {
    /// A scalar value (string, integer, float, boolean, null).
    Scalar(Scalar),
    /// A key-value mapping.
    Mapping(Mapping),
    /// An ordered sequence.
    Sequence(Sequence),
    /// An alias reference (e.g. `*anchor_name`).
    Alias(Alias),
    /// A tagged node (e.g. `!!set`, `!!omap`, `!!pairs`, or a custom tag).
    TaggedNode(TaggedNode),
}

impl YamlNode {
    /// Cast a `SyntaxNode` to a `YamlNode`.
    ///
    /// Returns `None` if the node's kind is not one of `SCALAR`, `MAPPING`,
    /// `SEQUENCE`, `ALIAS`, or `TAGGED_NODE`.
    pub fn from_syntax(node: SyntaxNode) -> Option<Self> {
        use crate::lex::SyntaxKind;
        match node.kind() {
            SyntaxKind::SCALAR => Scalar::cast(node).map(YamlNode::Scalar),
            SyntaxKind::MAPPING => Mapping::cast(node).map(YamlNode::Mapping),
            SyntaxKind::SEQUENCE => Sequence::cast(node).map(YamlNode::Sequence),
            SyntaxKind::ALIAS => Alias::cast(node).map(YamlNode::Alias),
            SyntaxKind::TAGGED_NODE => TaggedNode::cast(node).map(YamlNode::TaggedNode),
            _ => None,
        }
    }

    /// Cast a `SyntaxNode` to a `YamlNode`, peeling any `KEY` or `VALUE`
    /// wrapper first.
    ///
    /// Used internally where `mapping.pairs()` yields raw KEY/VALUE wrapper
    /// nodes that need to be unwrapped before semantic comparison.
    pub(crate) fn from_syntax_peeled(node: SyntaxNode) -> Option<Self> {
        use crate::lex::SyntaxKind;
        let inner = if matches!(node.kind(), SyntaxKind::KEY | SyntaxKind::VALUE) {
            node.children().next()?
        } else {
            node
        };
        Self::from_syntax(inner)
    }

    /// Returns the kind of YAML value this node represents.
    pub fn kind(&self) -> YamlKind {
        match self {
            YamlNode::Scalar(_) => YamlKind::Scalar,
            YamlNode::Mapping(_) => YamlKind::Mapping,
            YamlNode::Sequence(_) => YamlKind::Sequence,
            YamlNode::Alias(_) => YamlKind::Alias,
            YamlNode::TaggedNode(t) => t
                .tag()
                .map(|tag| YamlKind::Tagged(Cow::Owned(tag)))
                .unwrap_or(YamlKind::Scalar),
        }
    }

    /// Returns the underlying `SyntaxNode`.
    pub(crate) fn syntax(&self) -> &SyntaxNode {
        match self {
            YamlNode::Scalar(s) => s.syntax(),
            YamlNode::Mapping(m) => m.syntax(),
            YamlNode::Sequence(s) => s.syntax(),
            YamlNode::Alias(a) => a.syntax(),
            YamlNode::TaggedNode(t) => t.syntax(),
        }
    }

    /// Compare semantically with another value (ignores quoting/formatting).
    pub fn yaml_eq<O: AsYaml>(&self, other: &O) -> bool {
        yaml_eq(self, other)
    }

    /// If this node is a scalar, return a reference to it.
    pub fn as_scalar(&self) -> Option<&Scalar> {
        if let YamlNode::Scalar(s) = self {
            Some(s)
        } else {
            None
        }
    }

    /// If this node is a mapping, return a reference to it.
    pub fn as_mapping(&self) -> Option<&Mapping> {
        if let YamlNode::Mapping(m) = self {
            Some(m)
        } else {
            None
        }
    }

    /// If this node is a sequence, return a reference to it.
    pub fn as_sequence(&self) -> Option<&Sequence> {
        if let YamlNode::Sequence(s) = self {
            Some(s)
        } else {
            None
        }
    }

    /// If this node is a tagged node, return a reference to it.
    pub fn as_tagged(&self) -> Option<&TaggedNode> {
        if let YamlNode::TaggedNode(t) = self {
            Some(t)
        } else {
            None
        }
    }

    /// If this node is an alias, return a reference to it.
    pub fn as_alias(&self) -> Option<&Alias> {
        if let YamlNode::Alias(a) = self {
            Some(a)
        } else {
            None
        }
    }

    /// Returns `true` if this node is a scalar.
    pub fn is_scalar(&self) -> bool {
        matches!(self, YamlNode::Scalar(_))
    }

    /// Returns `true` if this node is a mapping.
    pub fn is_mapping(&self) -> bool {
        matches!(self, YamlNode::Mapping(_))
    }

    /// Returns `true` if this node is a sequence.
    pub fn is_sequence(&self) -> bool {
        matches!(self, YamlNode::Sequence(_))
    }

    /// Returns `true` if this node is a tagged node.
    pub fn is_tagged(&self) -> bool {
        matches!(self, YamlNode::TaggedNode(_))
    }

    /// Returns `true` if this node is an alias.
    pub fn is_alias(&self) -> bool {
        matches!(self, YamlNode::Alias(_))
    }

    /// If this node is a scalar, try to parse it as an `i64`.
    ///
    /// Returns `None` if this is not a scalar or cannot be parsed as an integer.
    pub fn to_i64(&self) -> Option<i64> {
        crate::scalar::ScalarValue::from_scalar(self.as_scalar()?).to_i64()
    }

    /// If this node is a scalar, try to parse it as an `f64`.
    ///
    /// Returns `None` if this is not a scalar or cannot be parsed as a float.
    pub fn to_f64(&self) -> Option<f64> {
        crate::scalar::ScalarValue::from_scalar(self.as_scalar()?).to_f64()
    }

    /// If this node is a scalar, try to parse it as a `bool`.
    ///
    /// Returns `None` if this is not a scalar or cannot be parsed as a boolean.
    pub fn to_bool(&self) -> Option<bool> {
        crate::scalar::ScalarValue::from_scalar(self.as_scalar()?).to_bool()
    }

    /// Get a value by key from a mapping node.
    ///
    /// Returns `None` if this node is not a mapping or the key is not found.
    pub fn get(&self, key: impl crate::AsYaml) -> Option<YamlNode> {
        self.as_mapping()?
            .get_node(key)
            .and_then(YamlNode::from_syntax)
    }

    /// Get a value by index from a sequence node.
    ///
    /// Returns `None` if this node is not a sequence or the index is out of bounds.
    pub fn get_item(&self, index: usize) -> Option<YamlNode> {
        self.as_sequence()?
            .items()
            .nth(index)
            .and_then(YamlNode::from_syntax)
    }
}

impl AsYaml for YamlNode {
    fn as_node(&self) -> Option<&SyntaxNode> {
        Some(self.syntax())
    }

    fn kind(&self) -> YamlKind {
        YamlNode::kind(self)
    }

    fn build_content(
        &self,
        builder: &mut rowan::GreenNodeBuilder,
        _indent: usize,
        _flow_context: bool,
    ) -> bool {
        let node = self.syntax();
        copy_node_content(builder, node);
        node.last_token()
            .map(|t| t.kind() == crate::lex::SyntaxKind::NEWLINE)
            .unwrap_or(false)
    }

    fn is_inline(&self) -> bool {
        use crate::yaml::ValueNode;
        match self {
            YamlNode::Scalar(_) => true,
            YamlNode::Mapping(m) => ValueNode::is_inline(m),
            YamlNode::Sequence(s) => ValueNode::is_inline(s),
            YamlNode::Alias(_) => true,
            YamlNode::TaggedNode(_) => true,
        }
    }
}

/// `yaml_node == "some_string"` — compares the node's semantic value.
impl PartialEq<str> for YamlNode {
    fn eq(&self, other: &str) -> bool {
        yaml_eq(self, &other)
    }
}

impl PartialEq<&str> for YamlNode {
    fn eq(&self, other: &&str) -> bool {
        yaml_eq(self, other)
    }
}

impl PartialEq<String> for YamlNode {
    fn eq(&self, other: &String) -> bool {
        yaml_eq(self, &other.as_str())
    }
}

impl fmt::Display for YamlNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.syntax().text())
    }
}

/// Blanket impl: any reference to an `AsYaml` type also implements `AsYaml`.
impl<T: AsYaml> AsYaml for &T {
    fn as_node(&self) -> Option<&SyntaxNode> {
        (*self).as_node()
    }

    fn kind(&self) -> YamlKind {
        (*self).kind()
    }

    fn build_content(
        &self,
        builder: &mut rowan::GreenNodeBuilder,
        indent: usize,
        flow_context: bool,
    ) -> bool {
        (*self).build_content(builder, indent, flow_context)
    }

    fn is_inline(&self) -> bool {
        (*self).is_inline()
    }
}

/// Recursively copy the children of `node` into `builder`.
pub(crate) fn copy_node_content(builder: &mut rowan::GreenNodeBuilder, node: &SyntaxNode) {
    for child in node.children_with_tokens() {
        match child {
            rowan::NodeOrToken::Node(n) => {
                builder.start_node(n.kind().into());
                copy_node_content(builder, &n);
                builder.finish_node();
            }
            rowan::NodeOrToken::Token(t) => {
                builder.token(t.kind().into(), t.text());
            }
        }
    }
}

/// Recursively copy the children of `node` into `builder`, adjusting indentation.
///
/// When copying WHITESPACE or INDENT tokens that appear after a NEWLINE,
/// replaces them with the specified `indent` amount. This is useful when
/// inserting pre-built sequences/mappings into a different nesting context.
pub(crate) fn copy_node_content_with_indent(
    builder: &mut rowan::GreenNodeBuilder,
    node: &SyntaxNode,
    indent: usize,
) {
    use crate::lex::SyntaxKind;
    let mut after_newline = false;

    for child in node.children_with_tokens() {
        match child {
            rowan::NodeOrToken::Node(n) => {
                // If this node follows a newline, add indent before starting the node
                if after_newline && indent > 0 {
                    builder.token(SyntaxKind::WHITESPACE.into(), &" ".repeat(indent));
                }
                builder.start_node(n.kind().into());
                copy_node_content_with_indent(builder, &n, indent);
                builder.finish_node();
                after_newline = false;
            }
            rowan::NodeOrToken::Token(t) => {
                match t.kind() {
                    SyntaxKind::NEWLINE => {
                        builder.token(t.kind().into(), t.text());
                        after_newline = true;
                    }
                    SyntaxKind::WHITESPACE | SyntaxKind::INDENT => {
                        // If this whitespace follows a newline, add our indent to the existing indent
                        if after_newline && indent > 0 {
                            let existing_indent = t.text().len();
                            let total_indent = indent + existing_indent;
                            builder.token(SyntaxKind::WHITESPACE.into(), &" ".repeat(total_indent));
                        } else if after_newline {
                            // indent == 0, skip the whitespace entirely
                        } else {
                            // Not after newline, keep as-is (e.g., space after dash)
                            builder.token(t.kind().into(), t.text());
                        }
                        after_newline = false;
                    }
                    SyntaxKind::DASH => {
                        // For sequence items: if DASH follows NEWLINE, add indent first
                        if after_newline && indent > 0 {
                            builder.token(SyntaxKind::WHITESPACE.into(), &" ".repeat(indent));
                        }
                        builder.token(t.kind().into(), t.text());
                        after_newline = false;
                    }
                    _ => {
                        // For any other token after a newline, add indent if needed
                        if after_newline && indent > 0 {
                            builder.token(SyntaxKind::WHITESPACE.into(), &" ".repeat(indent));
                        }
                        builder.token(t.kind().into(), t.text());
                        after_newline = false;
                    }
                }
            }
        }
    }
}

// AsYaml trait implementations for primitive types
//
// Uses macros to reduce boilerplate for the 12 numeric primitive types.

/// Macro to implement AsYaml for integer types
macro_rules! impl_as_yaml_int {
    ($($ty:ty),+ $(,)?) => {
        $(impl AsYaml for $ty {
            fn as_node(&self) -> Option<&crate::yaml::SyntaxNode> {
                None
            }

            fn kind(&self) -> YamlKind {
                YamlKind::Scalar
            }

            fn build_content(&self, builder: &mut rowan::GreenNodeBuilder, _indent: usize, _flow_context: bool) -> bool {
                use crate::lex::SyntaxKind;
                builder.start_node(SyntaxKind::SCALAR.into());
                builder.token(SyntaxKind::INT.into(), &self.to_string());
                builder.finish_node();
                false
            }

            fn is_inline(&self) -> bool {
                true
            }
        })+
    };
}

/// Macro to implement AsYaml for floating-point types
macro_rules! impl_as_yaml_float {
    ($($ty:ty),+ $(,)?) => {
        $(impl AsYaml for $ty {
            fn as_node(&self) -> Option<&crate::yaml::SyntaxNode> {
                None
            }

            fn kind(&self) -> YamlKind {
                YamlKind::Scalar
            }

            fn build_content(&self, builder: &mut rowan::GreenNodeBuilder, _indent: usize, _flow_context: bool) -> bool {
                use crate::lex::SyntaxKind;
                builder.start_node(SyntaxKind::SCALAR.into());
                let s = self.to_string();
                // Ensure floats always have a decimal point for YAML clarity
                let float_str = if s.contains('.') || s.contains('e') || s.contains('E') {
                    s
                } else {
                    format!("{}.0", s)
                };
                builder.token(SyntaxKind::FLOAT.into(), &float_str);
                builder.finish_node();
                false
            }

            fn is_inline(&self) -> bool {
                true
            }
        })+
    };
}

// Implement AsYaml for integer types
impl_as_yaml_int!(i64, i32, i16, i8, isize, u64, u32, u16, u8, usize);

// Implement AsYaml for floating-point types
impl_as_yaml_float!(f64, f32);

impl AsYaml for bool {
    fn as_node(&self) -> Option<&crate::yaml::SyntaxNode> {
        None
    }

    fn kind(&self) -> YamlKind {
        YamlKind::Scalar
    }

    fn build_content(
        &self,
        builder: &mut rowan::GreenNodeBuilder,
        _indent: usize,
        _flow_context: bool,
    ) -> bool {
        use crate::lex::SyntaxKind;
        builder.start_node(SyntaxKind::SCALAR.into());
        builder.token(
            SyntaxKind::BOOL.into(),
            if *self { "true" } else { "false" },
        );
        builder.finish_node();
        false
    }

    fn is_inline(&self) -> bool {
        true
    }
}

impl AsYaml for String {
    fn as_node(&self) -> Option<&crate::yaml::SyntaxNode> {
        None
    }

    fn kind(&self) -> YamlKind {
        YamlKind::Scalar
    }

    fn build_content(
        &self,
        builder: &mut rowan::GreenNodeBuilder,
        _indent: usize,
        flow_context: bool,
    ) -> bool {
        self.as_str().build_content(builder, _indent, flow_context)
    }

    fn is_inline(&self) -> bool {
        true
    }
}

impl AsYaml for &str {
    fn as_node(&self) -> Option<&crate::yaml::SyntaxNode> {
        None
    }

    fn kind(&self) -> YamlKind {
        YamlKind::Scalar
    }

    fn build_content(
        &self,
        builder: &mut rowan::GreenNodeBuilder,
        _indent: usize,
        flow_context: bool,
    ) -> bool {
        use crate::lex::SyntaxKind;
        use crate::scalar::ScalarValue;

        // In flow context (JSON), always use double-quoted strings for compatibility
        // In block context (YAML), use standard quoting rules
        let scalar = if flow_context {
            ScalarValue::double_quoted(*self)
        } else {
            ScalarValue::string(*self)
        };

        let yaml_text = scalar.to_yaml_string();
        // Both quoted and unquoted strings use STRING token kind;
        // the token text includes any quotes needed for disambiguation.
        builder.start_node(SyntaxKind::SCALAR.into());
        builder.token(SyntaxKind::STRING.into(), &yaml_text);
        builder.finish_node();
        false
    }

    fn is_inline(&self) -> bool {
        true
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::yaml::Document;
    use std::str::FromStr;

    #[test]
    fn test_yaml_eq_different_quoting_styles() {
        let yaml = r#"
plain: value
single: 'value'
double: "value"
"#;

        let doc = Document::from_str(yaml).unwrap();
        let mapping = doc.as_mapping().unwrap();

        let plain = mapping.get("plain").unwrap();
        let single = mapping.get("single").unwrap();
        let double = mapping.get("double").unwrap();

        // All three should be equal (semantic equality ignores quoting)
        assert!(yaml_eq(&plain, &single));
        assert!(yaml_eq(&single, &double));
        assert!(yaml_eq(&plain, &double));

        // Compare with raw strings
        assert!(yaml_eq(&plain, &"value"));
        assert!(yaml_eq(&single, &"value"));
        assert!(yaml_eq(&double, &"value"));
    }

    #[test]
    fn test_yaml_eq_escape_sequences() {
        let yaml = r#"
newline1: "line1\nline2"
newline2: "line1
line2"
tab1: "a\tb"
tab2: "a	b"
backslash1: "path\\file"
backslash2: 'path\file'
quote1: "say \"hi\""
quote2: 'say "hi"'
"#;

        let doc = Document::from_str(yaml).unwrap();
        let mapping = doc.as_mapping().unwrap();

        let newline1 = mapping.get("newline1").unwrap();
        let newline2 = mapping.get("newline2").unwrap();
        let tab1 = mapping.get("tab1").unwrap();
        let tab2 = mapping.get("tab2").unwrap();
        let backslash1 = mapping.get("backslash1").unwrap();
        let backslash2 = mapping.get("backslash2").unwrap();
        let quote1 = mapping.get("quote1").unwrap();
        let quote2 = mapping.get("quote2").unwrap();

        // Escaped newlines should equal actual newlines
        assert!(yaml_eq(&newline1, &newline2));

        // Escaped tabs should equal actual tabs
        assert!(yaml_eq(&tab1, &tab2));

        // Backslash handling: single-quoted strings don't interpret backslashes as escapes
        // So 'path\file' is literally "path\file" (one backslash)
        // And "path\\file" escapes to "path\file" (one backslash)
        // Therefore they ARE equal!
        assert!(yaml_eq(&backslash1, &backslash2));
        assert!(yaml_eq(&backslash1, &"path\\file"));
        assert!(yaml_eq(&backslash2, &"path\\file"));

        // Quote handling
        assert!(yaml_eq(&quote1, &quote2));
        assert!(yaml_eq(&quote1, &r#"say "hi""#));
    }

    #[test]
    fn test_yaml_eq_single_quote_escaping() {
        let yaml = r#"
single1: 'can''t'
single2: "can't"
"#;

        let doc = Document::from_str(yaml).unwrap();
        let mapping = doc.as_mapping().unwrap();

        let single1 = mapping.get("single1").unwrap();
        let single2 = mapping.get("single2").unwrap();

        // Single-quoted '' should equal double-quoted '
        assert!(yaml_eq(&single1, &single2));
        assert!(yaml_eq(&single1, &"can't"));
    }

    #[test]
    fn test_yaml_eq_unicode_escapes() {
        let yaml = r#"
unicode1: "hello\x20world"
unicode2: "hello world"
unicode3: "smiley\u0020face"
unicode4: "smiley face"
"#;

        let doc = Document::from_str(yaml).unwrap();
        let mapping = doc.as_mapping().unwrap();

        let unicode1 = mapping.get("unicode1").unwrap();
        let unicode2 = mapping.get("unicode2").unwrap();
        let unicode3 = mapping.get("unicode3").unwrap();
        let unicode4 = mapping.get("unicode4").unwrap();

        // \x20 is space
        assert!(yaml_eq(&unicode1, &unicode2));
        assert!(yaml_eq(&unicode1, &"hello world"));

        // \u0020 is also space
        assert!(yaml_eq(&unicode3, &unicode4));
        assert!(yaml_eq(&unicode3, &"smiley face"));
    }

    #[test]
    fn test_yaml_eq_with_comments() {
        let yaml1 = r#"
# This is a comment
key: value # inline comment
"#;

        let yaml2 = r#"
key: value
"#;

        let doc1 = Document::from_str(yaml1).unwrap();
        let doc2 = Document::from_str(yaml2).unwrap();

        let mapping1 = doc1.as_mapping().unwrap();
        let mapping2 = doc2.as_mapping().unwrap();

        // Mappings with and without comments should be equal (semantic equality)
        assert!(yaml_eq(&mapping1, &mapping2));

        let value1 = mapping1.get("key").unwrap();
        let value2 = mapping2.get("key").unwrap();

        assert!(yaml_eq(&value1, &value2));
        assert!(yaml_eq(&value1, &"value"));
    }

    #[test]
    fn test_yaml_eq_mappings() {
        let yaml1 = r#"
a: 1
b: 2
c: 3
"#;

        let yaml2 = r#"
a: 1
b: 2
c: 3
"#;

        let yaml3 = r#"
a: 1
c: 3
b: 2
"#;

        let doc1 = Document::from_str(yaml1).unwrap();
        let doc2 = Document::from_str(yaml2).unwrap();
        let doc3 = Document::from_str(yaml3).unwrap();

        let mapping1 = doc1.as_mapping().unwrap();
        let mapping2 = doc2.as_mapping().unwrap();
        let mapping3 = doc3.as_mapping().unwrap();

        // Same mappings should be equal
        assert!(yaml_eq(&mapping1, &mapping2));

        // Different order should NOT be equal (order matters for yaml_eq)
        assert!(!yaml_eq(&mapping1, &mapping3));
    }

    #[test]
    fn test_yaml_eq_sequences() {
        let yaml1 = r#"
- one
- two
- three
"#;

        let yaml2 = r#"
- one
- two
- three
"#;

        let yaml3 = r#"
- one
- three
- two
"#;

        let doc1 = Document::from_str(yaml1).unwrap();
        let doc2 = Document::from_str(yaml2).unwrap();
        let doc3 = Document::from_str(yaml3).unwrap();

        let seq1 = doc1.as_sequence().unwrap();
        let seq2 = doc2.as_sequence().unwrap();
        let seq3 = doc3.as_sequence().unwrap();

        // Same sequences should be equal
        assert!(yaml_eq(&seq1, &seq2));

        // Different order should NOT be equal
        assert!(!yaml_eq(&seq1, &seq3));
    }

    #[test]
    fn test_yaml_eq_nested_structures() {
        let yaml1 = r#"
outer:
  inner:
    key: "value"
"#;

        let yaml2 = r#"
outer:
  inner:
    key: 'value'
"#;

        let doc1 = Document::from_str(yaml1).unwrap();
        let doc2 = Document::from_str(yaml2).unwrap();

        let mapping1 = doc1.as_mapping().unwrap();
        let mapping2 = doc2.as_mapping().unwrap();

        // Nested structures with different quoting should be equal
        assert!(yaml_eq(&mapping1, &mapping2));
    }

    #[test]
    fn test_yaml_eq_special_characters() {
        let yaml = r#"
bell: "\a"
escape: "\e"
"null": "\0"
backspace: "\b"
formfeed: "\f"
carriagereturn: "\r"
verticaltab: "\v"
"#;

        let doc = Document::from_str(yaml).unwrap();
        let mapping = doc.as_mapping().unwrap();

        // Note: These all compare STRING values with escaped chars against raw Rust strings
        // They should be equal because both sides are STRING type
        assert!(yaml_eq(&mapping.get("bell").unwrap(), &"\x07"));
        assert!(yaml_eq(&mapping.get("escape").unwrap(), &"\x1B"));
        assert!(yaml_eq(&mapping.get("null").unwrap(), &"\0"));
        assert!(yaml_eq(&mapping.get("backspace").unwrap(), &"\x08"));
        assert!(yaml_eq(&mapping.get("formfeed").unwrap(), &"\x0C"));
        assert!(yaml_eq(&mapping.get("carriagereturn").unwrap(), &"\r"));
        assert!(yaml_eq(&mapping.get("verticaltab").unwrap(), &"\x0B"));
    }

    #[test]
    fn test_yaml_eq_empty_strings() {
        let yaml = r#"
empty1: ""
empty2: ''
"#;

        let doc = Document::from_str(yaml).unwrap();
        let mapping = doc.as_mapping().unwrap();

        let empty1 = mapping.get("empty1").unwrap();
        let empty2 = mapping.get("empty2").unwrap();

        assert!(yaml_eq(&empty1, &empty2));
        assert!(yaml_eq(&empty1, &""));
    }

    #[test]
    fn test_yaml_eq_whitespace_handling() {
        let yaml = r#"
spaces1: "  leading"
spaces2: "trailing  "
spaces3: "  both  "
plain: value
"#;

        let doc = Document::from_str(yaml).unwrap();
        let mapping = doc.as_mapping().unwrap();

        let spaces1 = mapping.get("spaces1").unwrap();
        let spaces2 = mapping.get("spaces2").unwrap();
        let spaces3 = mapping.get("spaces3").unwrap();
        let plain = mapping.get("plain").unwrap();

        // Whitespace should be preserved exactly
        assert!(yaml_eq(&spaces1, &"  leading"));
        assert!(yaml_eq(&spaces2, &"trailing  "));
        assert!(yaml_eq(&spaces3, &"  both  "));
        assert!(yaml_eq(&plain, &"value"));

        // These should NOT be equal
        assert!(!yaml_eq(&spaces1, &"leading"));
        assert!(!yaml_eq(&spaces2, &"trailing"));
    }

    #[test]
    fn test_yaml_eq_different_types() {
        let yaml = r#"
string: "123"
number: 123
sequence:
  - item
mapping:
  key: value
"#;

        let doc = Document::from_str(yaml).unwrap();
        let mapping = doc.as_mapping().unwrap();

        let string_val = mapping.get("string").unwrap();
        let number_val = mapping.get("number").unwrap();
        let sequence_val = mapping.get("sequence").unwrap();
        let mapping_val = mapping.get("mapping").unwrap();

        // yaml_eq does YAML-level semantic comparison:
        // "123" (string) and 123 (integer) are DIFFERENT types
        assert!(!yaml_eq(&string_val, &number_val));
        assert!(!yaml_eq(&string_val, &sequence_val));
        assert!(!yaml_eq(&string_val, &mapping_val));
        assert!(!yaml_eq(&number_val, &sequence_val));
        assert!(!yaml_eq(&sequence_val, &mapping_val));
    }

    #[test]
    fn test_yaml_eq_line_folding_escapes() {
        let yaml = r#"
folded1: "line1\
 line2"
folded2: "line1 line2"
"#;

        let doc = Document::from_str(yaml).unwrap();
        let mapping = doc.as_mapping().unwrap();

        let folded1 = mapping.get("folded1").unwrap();
        let folded2 = mapping.get("folded2").unwrap();

        // Per YAML spec, backslash at end of line folds to a space
        assert!(yaml_eq(&folded1, &folded2));
        assert!(yaml_eq(&folded1, &"line1 line2"));
    }

    #[test]
    fn test_yaml_eq_complex_unicode() {
        let yaml = r#"
emoji1: "😀"
emoji2: "\U0001F600"
"#;

        let doc = Document::from_str(yaml).unwrap();
        let mapping = doc.as_mapping().unwrap();

        let emoji1 = mapping.get("emoji1").unwrap();
        let emoji2 = mapping.get("emoji2").unwrap();

        // Unicode escape for emoji should equal literal emoji
        assert!(yaml_eq(&emoji1, &emoji2));
        assert!(yaml_eq(&emoji1, &"😀"));
    }

    #[test]
    fn test_yaml_eq_block_scalars() {
        let yaml1 = "literal1: |\n  line1\n  line2\n";
        let yaml2 = "literal2: |\n  line1\n  line2\n";

        let doc1 = Document::from_str(yaml1).unwrap();
        let doc2 = Document::from_str(yaml2).unwrap();

        let mapping1 = doc1.as_mapping().unwrap();
        let mapping2 = doc2.as_mapping().unwrap();

        let literal1 = mapping1.get("literal1").unwrap();
        let literal2 = mapping2.get("literal2").unwrap();

        // Same literal block scalars should be equal
        assert!(yaml_eq(&literal1, &literal2));
    }

    #[test]
    fn test_yaml_eq_null_and_special_values() {
        let yaml = r#"
null1: null
null2: null
null3:
empty: ""
"#;

        let doc = Document::from_str(yaml).unwrap();
        let mapping = doc.as_mapping().unwrap();

        let null1 = mapping.get("null1").unwrap();
        let null2 = mapping.get("null2").unwrap();
        let null3 = mapping.get("null3").unwrap();
        let empty = mapping.get("empty").unwrap();

        // Same null representations should be equal (both are NULL type)
        assert!(yaml_eq(&null1, &null2));

        // null3 (implicit null via empty value) should also be equal to explicit null
        // Per YAML spec, "key:" is semantically identical to "key: null"
        assert!(yaml_eq(&null3, &null1));
        assert!(yaml_eq(&null3, &null2));

        // NULL type != STRING type (even though empty might look like null)
        assert!(!yaml_eq(&null1, &empty));

        // NULL scalar != STRING "null" (different types)
        assert!(!yaml_eq(&null1, &"null"));
    }

    #[test]
    fn test_yaml_eq_boolean_representations() {
        let yaml = r#"
true1: true
true2: True
true3: TRUE
false1: false
false2: False
"#;

        let doc = Document::from_str(yaml).unwrap();
        let mapping = doc.as_mapping().unwrap();

        let true1 = mapping.get("true1").unwrap();
        let true2 = mapping.get("true2").unwrap();
        let true3 = mapping.get("true3").unwrap();
        let false1 = mapping.get("false1").unwrap();
        let false2 = mapping.get("false2").unwrap();

        // Different case booleans ARE semantically equal (normalized to lowercase)
        assert!(yaml_eq(&true1, &true2));
        assert!(yaml_eq(&true1, &true3));
        assert!(yaml_eq(&false1, &false2));

        // BOOL scalars do NOT equal STRING scalars
        assert!(!yaml_eq(&true1, &"true"));
        assert!(!yaml_eq(&false1, &"false"));
    }

    #[test]
    fn test_yaml_eq_numeric_formats() {
        let yaml = r#"
decimal: 123
octal: 0o173
hex: 0x7B
"#;

        let doc = Document::from_str(yaml).unwrap();
        let mapping = doc.as_mapping().unwrap();

        let decimal = mapping.get("decimal").unwrap();
        let octal = mapping.get("octal").unwrap();
        let hex = mapping.get("hex").unwrap();

        // Different numeric formats ARE semantically equal (all equal 123)
        assert!(yaml_eq(&decimal, &octal));
        assert!(yaml_eq(&decimal, &hex));
        assert!(yaml_eq(&octal, &hex));

        // INT scalars do NOT equal STRING scalars (even if text is same)
        assert!(!yaml_eq(&decimal, &"123"));

        // But they DO equal raw integer values
        assert!(yaml_eq(&decimal, &123));
        assert!(yaml_eq(&octal, &123));
        assert!(yaml_eq(&hex, &123));
    }

    #[test]
    fn test_yaml_eq_with_anchors() {
        let yaml = r#"
original: &anchor value
duplicate: value
"#;

        let doc = Document::from_str(yaml).unwrap();
        let mapping = doc.as_mapping().unwrap();

        let original = mapping.get("original").unwrap();
        let duplicate = mapping.get("duplicate").unwrap();

        // Values with anchors should equal plain values (anchor syntax is ignored)
        assert!(yaml_eq(&original, &duplicate));
        assert!(yaml_eq(&original, &"value"));
    }

    #[test]
    fn test_yaml_eq_flow_vs_block_collections() {
        let yaml = r#"
flow_seq: [1, 2, 3]
block_seq:
  - 1
  - 2
  - 3
flow_map: {a: 1, b: 2}
block_map:
  a: 1
  b: 2
"#;

        let doc = Document::from_str(yaml).unwrap();
        let mapping = doc.as_mapping().unwrap();

        let flow_seq = mapping.get("flow_seq").unwrap();
        let block_seq = mapping.get("block_seq").unwrap();
        let flow_map = mapping.get("flow_map").unwrap();
        let block_map = mapping.get("block_map").unwrap();

        // Flow and block styles should be semantically equal
        assert!(yaml_eq(&flow_seq, &block_seq));
        assert!(yaml_eq(&flow_map, &block_map));
    }
}
