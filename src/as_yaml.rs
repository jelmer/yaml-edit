//! AsYaml trait and YamlNode for unified access to YAML values.
//!
//! ## Type hierarchy
//!
//! This library has two tiers:
//!
//! - **CST types** ([`Document`](crate::yaml::Document), [`Mapping`](crate::yaml::Mapping),
//!   [`Sequence`](crate::nodes::sequence::Sequence), [`Scalar`](crate::yaml::Scalar),
//!   [`TaggedNode`](crate::yaml::TaggedNode)) - format-preserving wrappers around the
//!   concrete syntax tree.  Parse a file to get these; navigate and mutate them in place.
//!
//! - **Input types** (`&str`, `i64`, `bool`, [`MappingBuilder`](crate::builder::MappingBuilder), …):
//!   supply these to mutation methods such as
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

    /// Like `build_content` inside a flow collection, quoting strings that
    /// have no explicit style as `quoting` says.
    #[doc(hidden)]
    fn build_flow_content(
        &self,
        builder: &mut rowan::GreenNodeBuilder,
        indent: usize,
        _quoting: FlowQuoting,
    ) -> bool {
        self.build_content(builder, indent, true)
    }
}

/// How strings without an explicit style are quoted inside a flow
/// collection.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FlowQuoting {
    /// Only where a plain scalar would not read back as the same string.
    Minimal,
    /// Always double-quoted, as in JSON.
    Double,
}

/// Emit `value` in a flow collection quoting strings as `flow` says, or in
/// block context if `flow` is `None`.
pub(crate) fn build_in<T: AsYaml + ?Sized>(
    value: &T,
    builder: &mut rowan::GreenNodeBuilder,
    indent: usize,
    flow: Option<FlowQuoting>,
) -> bool {
    match flow {
        Some(quoting) => value.build_flow_content(builder, indent, quoting),
        None => value.build_content(builder, indent, false),
    }
}

/// Compare two [`AsYaml`] values for semantic equality.
///
/// Two nodes are equal when they have the same resolved tag and the same
/// canonical value, as in the YAML 1.2 spec. Untagged plain scalars resolve
/// through the core schema and quoted or block scalars are `!!str`, so
/// `name`, `"name"`, `'name'` and `!!str name` all compare equal, as do
/// `1`, `0x1` and `!!int 1`, while `1` and `'1'` do not. Mappings compare
/// regardless of key order.
///
/// Both sides can be any combination of CST nodes and raw Rust values.
/// Aliases are not resolved and never compare equal.
pub fn yaml_eq<A, B>(a: &A, b: &B) -> bool
where
    A: AsYaml + ?Sized,
    B: AsYaml + ?Sized,
{
    match (to_syntax(a), to_syntax(b)) {
        (Some(a), Some(b)) => node_eq(&a, &b),
        _ => false,
    }
}

/// The node backing `v`, or for a raw value, the CST it builds.
fn to_syntax<T: AsYaml + ?Sized>(v: &T) -> Option<SyntaxNode> {
    use crate::lex::SyntaxKind;
    if let Some(node) = v.as_node() {
        return Some(node.clone());
    }
    let mut builder = rowan::GreenNodeBuilder::new();
    builder.start_node(SyntaxKind::ROOT.into());
    // Build in flow context: a multi-line string renders there as one
    // double-quoted scalar rather than a literal block, whose trailing
    // newline would otherwise make `"a\nb"` compare unequal to itself.
    v.build_content(&mut builder, 0, true);
    builder.finish_node();
    SyntaxNode::new_root(builder.finish()).first_child()
}

/// A node reduced to its resolved tag and the content the tag applies to.
struct Resolved {
    tag: String,
    content: Content,
}

enum Content {
    /// The canonical value of a scalar.
    Scalar(String),
    Mapping(Mapping),
    Sequence(Sequence),
}

/// Spell `tag` in its `!!` shorthand when it is in the `tag:yaml.org,2002:`
/// namespace, so both spellings compare equal.
fn shorthand_tag(tag: &str) -> String {
    match tag
        .strip_prefix("!<tag:yaml.org,2002:")
        .and_then(|t| t.strip_suffix('>'))
    {
        Some(name) => format!("!!{name}"),
        None => tag.to_string(),
    }
}

/// The resolved tag of a collection written with `tag`, if any.
fn collection_tag(tag: Option<String>, default: &str) -> String {
    match tag.as_deref() {
        // The non-specific tag `!` resolves a collection by its kind.
        None | Some("!") => default.to_string(),
        Some(t) => shorthand_tag(t),
    }
}

/// Resolve a scalar to its tag and canonical value.
fn resolve_scalar(scalar: crate::scalar::ScalarValue) -> Resolved {
    use crate::scalar::{ScalarType, ScalarValue};
    let text = scalar.value();
    let canonical = |parsed: Option<String>| parsed.unwrap_or_else(|| text.to_string());
    let (tag, value) = match scalar.scalar_type() {
        ScalarType::Integer => (
            "!!int".to_string(),
            canonical(ScalarValue::canonical_integer(text)),
        ),
        ScalarType::Float => (
            "!!float".to_string(),
            canonical(ScalarValue::parse_float(text).map(|f| f.to_string())),
        ),
        ScalarType::Boolean => (
            "!!bool".to_string(),
            canonical(ScalarValue::parse_bool(text).map(|b| b.to_string())),
        ),
        ScalarType::Null => ("!!null".to_string(), String::new()),
        _ => {
            // A scalar with the non-specific tag `!` is a string.
            let tag = match scalar.tag().map(shorthand_tag) {
                Some(t) if t != "!" => t,
                _ => "!!str".to_string(),
            };
            (tag, text.to_string())
        }
    };
    Resolved {
        tag,
        content: Content::Scalar(value),
    }
}

/// Resolve a value node, or the KEY or VALUE wrapper holding one.
fn resolve(node: &SyntaxNode) -> Option<Resolved> {
    use crate::lex::SyntaxKind;
    match node.kind() {
        SyntaxKind::TAGGED_NODE => {
            let tagged = TaggedNode::cast(node.clone())?;
            match node.first_child() {
                Some(content) => resolve_content(&content, tagged.tag()),
                None => crate::scalar::ScalarValue::from_tagged(&tagged).map(resolve_scalar),
            }
        }
        SyntaxKind::KEY | SyntaxKind::VALUE => resolve(&node.first_child()?),
        _ => resolve_content(node, None),
    }
}

/// Resolve an untagged node, or one whose tag `tag` was found outside it.
fn resolve_content(node: &SyntaxNode, tag: Option<String>) -> Option<Resolved> {
    use crate::lex::SyntaxKind;
    use crate::scalar::ScalarValue;
    match node.kind() {
        SyntaxKind::SCALAR => {
            let scalar = ScalarValue::from_scalar(&Scalar::cast(node.clone())?);
            Some(resolve_scalar(match tag {
                Some(tag) => scalar.with_resolved_tag(tag),
                None => scalar,
            }))
        }
        SyntaxKind::MAPPING => Some(Resolved {
            tag: collection_tag(tag, "!!map"),
            content: Content::Mapping(Mapping::cast(node.clone())?),
        }),
        SyntaxKind::SEQUENCE => Some(Resolved {
            tag: collection_tag(tag, "!!seq"),
            content: Content::Sequence(Sequence::cast(node.clone())?),
        }),
        _ => None,
    }
}

/// The resolved tag and canonical value of a scalar node, tagged or not,
/// or of the scalar a KEY or VALUE wrapper holds.
///
/// Returns `None` for anything that is not a scalar.
pub(crate) fn scalar_canonical(node: &SyntaxNode) -> Option<(String, String)> {
    match resolve(node)? {
        Resolved {
            tag,
            content: Content::Scalar(value),
        } => Some((tag, value)),
        _ => None,
    }
}

/// Spec equality of two CST nodes.
pub(crate) fn node_eq(a: &SyntaxNode, b: &SyntaxNode) -> bool {
    let (Some(a), Some(b)) = (resolve(a), resolve(b)) else {
        return false;
    };
    a.tag == b.tag
        && match (&a.content, &b.content) {
            (Content::Scalar(a), Content::Scalar(b)) => a == b,
            (Content::Mapping(a), Content::Mapping(b)) => mapping_eq(a, b),
            (Content::Sequence(a), Content::Sequence(b)) => sequence_eq(a, b),
            _ => false,
        }
}

fn mapping_eq(lhs: &Mapping, rhs: &Mapping) -> bool {
    let lhs_pairs: Vec<_> = lhs.pairs().collect();
    let rhs_pairs: Vec<_> = rhs.pairs().collect();
    if lhs_pairs.len() != rhs_pairs.len() {
        return false;
    }
    // Mappings are unordered, so match each pair against a not yet used pair
    // on the other side.
    let mut used = vec![false; rhs_pairs.len()];
    lhs_pairs.iter().all(|(lk, lv)| {
        let found = rhs_pairs
            .iter()
            .zip(&used)
            .position(|((rk, rv), u)| !u && node_eq(lk, rk) && node_eq(lv, rv));
        found.map(|i| used[i] = true).is_some()
    })
}

fn sequence_eq(lhs: &Sequence, rhs: &Sequence) -> bool {
    // items() already yields peeled content nodes.
    lhs.len() == rhs.len() && lhs.items().zip(rhs.items()).all(|(l, r)| node_eq(&l, &r))
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
                .map_or(YamlKind::Scalar, |tag| YamlKind::Tagged(Cow::Owned(tag))),
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

    /// The mapping this node is, looking through a tag on it.
    pub(crate) fn mapping_content(&self) -> Option<Mapping> {
        match self {
            YamlNode::Mapping(m) => Some(m.clone()),
            YamlNode::TaggedNode(t) => t.as_mapping(),
            _ => None,
        }
    }

    /// The sequence this node is, looking through a tag on it.
    pub(crate) fn sequence_content(&self) -> Option<Sequence> {
        match self {
            YamlNode::Sequence(s) => Some(s.clone()),
            YamlNode::TaggedNode(t) => t.as_sequence(),
            _ => None,
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
        indent: usize,
        flow_context: bool,
    ) -> bool {
        match self {
            YamlNode::Scalar(s) => s.build_content(builder, indent, flow_context),
            YamlNode::Mapping(m) => m.build_content(builder, indent, flow_context),
            YamlNode::Sequence(s) => s.build_content(builder, indent, flow_context),
            YamlNode::Alias(a) => a.build_content(builder, indent, flow_context),
            YamlNode::TaggedNode(t) => t.build_content(builder, indent, flow_context),
        }
    }

    fn is_inline(&self) -> bool {
        use crate::yaml::ValueNode;
        match self {
            YamlNode::Scalar(s) => ValueNode::is_inline(s),
            YamlNode::Mapping(m) => ValueNode::is_inline(m),
            YamlNode::Sequence(s) => ValueNode::is_inline(s),
            YamlNode::Alias(_) => true,
            YamlNode::TaggedNode(t) => ValueNode::is_inline(t),
        }
    }
}

/// `yaml_node == "some_string"` - compares the node's semantic value.
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
///
/// The `?Sized` bound makes this work for trait objects (`&dyn AsYaml`),
/// which lets [`MappingView`](crate::MappingView) - whose methods take
/// `&dyn AsYaml` - forward keys to inherent methods that take
/// `impl AsYaml`.
impl<T: AsYaml + ?Sized> AsYaml for &T {
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

    fn build_flow_content(
        &self,
        builder: &mut rowan::GreenNodeBuilder,
        indent: usize,
        quoting: FlowQuoting,
    ) -> bool {
        (*self).build_flow_content(builder, indent, quoting)
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

/// Copy the children of `node` into `builder`, shifting every post-newline
/// whitespace by `delta` columns (may be negative to dedent). Only spaces
/// are added or removed; tabs after them are kept. Line-leading whitespace
/// is written as INDENT, as the parser does.
///
/// Newline state is tracked across node boundaries: a NEWLINE inside a
/// child node correctly marks
/// the next sibling token as being at the start of a line, so
/// sibling INDENT tokens between entries (which is where the parser
/// places them for block sequences and mappings) are shifted too.
/// Returns whether the last emitted token was a NEWLINE.
pub(crate) fn copy_node_content_reindent(
    builder: &mut rowan::GreenNodeBuilder,
    node: &SyntaxNode,
    delta: isize,
) -> bool {
    use crate::lex::SyntaxKind;
    let mut after_newline = false;
    let pad = |n: isize| " ".repeat(n.max(0) as usize);

    for child in node.children_with_tokens() {
        match child {
            rowan::NodeOrToken::Node(n) => {
                // Empty nodes (e.g. an implicit `null` value after `? key`
                // in a set) emit nothing, so they must not affect the
                // `after_newline` state or trigger a pre-node indent. Their
                // zero-width content still has to come along.
                if n.text_range().is_empty() {
                    builder.start_node(n.kind().into());
                    copy_node_content(builder, &n);
                    builder.finish_node();
                    continue;
                }
                if after_newline && delta > 0 {
                    builder.token(SyntaxKind::INDENT.into(), &pad(delta));
                }
                builder.start_node(n.kind().into());
                after_newline = copy_node_content_reindent(builder, &n, delta);
                builder.finish_node();
            }
            rowan::NodeOrToken::Token(t) => match t.kind() {
                SyntaxKind::NEWLINE => {
                    builder.token(t.kind().into(), t.text());
                    after_newline = true;
                }
                SyntaxKind::WHITESPACE | SyntaxKind::INDENT => {
                    if after_newline {
                        let shifted = shift_line_start(t.text(), delta);
                        if !shifted.is_empty() {
                            builder.token(SyntaxKind::INDENT.into(), &shifted);
                        }
                    } else {
                        builder.token(t.kind().into(), t.text());
                    }
                    after_newline = false;
                }
                _ => {
                    if after_newline && delta > 0 {
                        builder.token(SyntaxKind::INDENT.into(), &pad(delta));
                    }
                    builder.token(t.kind().into(), t.text());
                    after_newline = false;
                }
            },
        }
    }
    after_newline
}

/// `text`, the whitespace at the start of a line, with `delta` spaces added
/// or up to `-delta` leading spaces removed. Tabs are not indentation, so
/// any that follow the spaces are kept.
fn shift_line_start(text: &str, delta: isize) -> String {
    if delta >= 0 {
        return format!("{}{text}", " ".repeat(delta as usize));
    }
    let spaces = text.len() - text.trim_start_matches(' ').len();
    text[spaces.min(delta.unsigned_abs())..].to_string()
}

/// Column at which `node` starts on its line.
///
/// For a MAPPING or SEQUENCE this is the column of its first entry; for
/// a MAPPING_ENTRY it's the column of the key. Returns 0 for anything at
/// the start of the tree's text.
pub(crate) fn source_base_indent(node: &SyntaxNode) -> usize {
    // Walk backwards through preceding tokens (climbing to parent siblings
    // when we run out), accumulating characters until we hit a NEWLINE or
    // the start of the tree. This works regardless of how the parser
    // distributed INDENT/WHITESPACE tokens (siblings, inside a
    // `SEQUENCE_ENTRY`'s dash-space, etc.) and only materializes text
    // subtrees that we've already confirmed contain a newline.
    let mut column: usize = 0;
    let mut cursor = node.prev_sibling_or_token();
    let mut current = node.clone();
    loop {
        while let Some(item) = cursor {
            match item {
                rowan::NodeOrToken::Token(t) => {
                    let text = t.text();
                    if let Some(nl) = text.rfind('\n') {
                        column += text[nl + 1..].chars().count();
                        return column;
                    }
                    column += text.chars().count();
                    cursor = t.prev_sibling_or_token();
                }
                rowan::NodeOrToken::Node(n) => {
                    // For nodes containing no newline we still need the
                    // character count (bytes lie for multi-byte input);
                    // materialize.
                    let text = n.text().to_string();
                    if let Some(nl) = text.rfind('\n') {
                        column += text[nl + 1..].chars().count();
                        return column;
                    }
                    column += text.chars().count();
                    cursor = n.prev_sibling_or_token();
                }
            }
        }
        // Ran out of siblings; climb up and continue with the parent's siblings.
        match current.parent() {
            Some(p) => {
                cursor = p.prev_sibling_or_token();
                current = p;
            }
            None => return column,
        }
    }
}

/// Indentation of the line on which `node` starts.
pub(crate) fn source_line_indent(node: &SyntaxNode) -> usize {
    let mut prefix = Vec::new();
    let mut cursor = node.first_token().and_then(|t| t.prev_token());
    while let Some(token) = cursor {
        match token.text().rfind('\n') {
            Some(nl) => {
                prefix.push(token.text()[nl + 1..].to_string());
                break;
            }
            None => prefix.push(token.text().to_string()),
        }
        cursor = token.prev_token();
    }
    prefix.reverse();
    prefix.concat().chars().take_while(|&c| c == ' ').count()
}

/// Base column to measure a copied collection's indentation from: for a
/// flow collection, whose continuation lines are only bound to the
/// indentation of the line it starts on, that line's indentation;
/// otherwise the column of its first entry.
pub(crate) fn source_reindent_base(node: &SyntaxNode, is_flow: bool) -> usize {
    if is_flow {
        source_line_indent(node)
    } else {
        source_base_indent(node)
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
                let float_str = crate::scalar::ScalarValue::format_float(self.to_string());
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
        indent: usize,
        flow_context: bool,
    ) -> bool {
        self.as_str().build_content(builder, indent, flow_context)
    }

    fn is_inline(&self) -> bool {
        true
    }

    fn build_flow_content(
        &self,
        builder: &mut rowan::GreenNodeBuilder,
        indent: usize,
        quoting: FlowQuoting,
    ) -> bool {
        self.as_str().build_flow_content(builder, indent, quoting)
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
        indent: usize,
        flow_context: bool,
    ) -> bool {
        crate::scalar::ScalarValue::string(*self).build_content(builder, indent, flow_context)
    }

    fn is_inline(&self) -> bool {
        true
    }

    fn build_flow_content(
        &self,
        builder: &mut rowan::GreenNodeBuilder,
        indent: usize,
        quoting: FlowQuoting,
    ) -> bool {
        crate::scalar::ScalarValue::string(*self).build_flow_content(builder, indent, quoting)
    }
}

/// `Some(v)` behaves exactly like `v`; `None` emits a bare `null` scalar.
impl<T: AsYaml> AsYaml for Option<T> {
    fn as_node(&self) -> Option<&crate::yaml::SyntaxNode> {
        self.as_ref().and_then(AsYaml::as_node)
    }

    fn kind(&self) -> YamlKind {
        match self {
            Some(v) => v.kind(),
            None => YamlKind::Scalar,
        }
    }

    fn build_content(
        &self,
        builder: &mut rowan::GreenNodeBuilder,
        indent: usize,
        flow_context: bool,
    ) -> bool {
        match self {
            Some(v) => v.build_content(builder, indent, flow_context),
            None => crate::scalar::ScalarValue::null().build_content(builder, indent, flow_context),
        }
    }

    fn is_inline(&self) -> bool {
        match self {
            Some(v) => v.is_inline(),
            None => true,
        }
    }

    fn build_flow_content(
        &self,
        builder: &mut rowan::GreenNodeBuilder,
        indent: usize,
        quoting: FlowQuoting,
    ) -> bool {
        match self {
            Some(v) => v.build_flow_content(builder, indent, quoting),
            None => self.build_content(builder, indent, true),
        }
    }
}

/// Emit a `!!set`: a tagged mapping with each member as a key and a null
/// value. Returns whether the content ends with a newline.
pub(crate) fn build_set_content<T: AsYaml>(
    builder: &mut rowan::GreenNodeBuilder,
    members: impl ExactSizeIterator<Item = T>,
    indent: usize,
    flow_context: Option<FlowQuoting>,
) -> bool {
    use crate::lex::SyntaxKind;
    use crate::scalar::ScalarValue;

    let len = members.len();
    // An empty mapping is written as `{}` in either context.
    let quoting = flow_context.unwrap_or(FlowQuoting::Minimal);
    let flow = flow_context.is_some() || len == 0;

    builder.start_node(SyntaxKind::TAGGED_NODE.into());
    builder.token(SyntaxKind::TAG.into(), "!!set");
    if flow {
        builder.token(SyntaxKind::WHITESPACE.into(), " ");
    } else {
        builder.token(SyntaxKind::NEWLINE.into(), "\n");
        builder.token(SyntaxKind::INDENT.into(), &" ".repeat(indent + 2));
    }

    builder.start_node(SyntaxKind::MAPPING.into());
    if flow {
        builder.token(SyntaxKind::LEFT_BRACE.into(), "{");
    }
    let indent_str = " ".repeat(indent + 2);
    for (i, member) in members.enumerate() {
        if i > 0 && !flow {
            builder.token(SyntaxKind::INDENT.into(), &indent_str);
        }
        builder.start_node(SyntaxKind::MAPPING_ENTRY.into());
        builder.start_node(SyntaxKind::KEY.into());
        crate::nodes::build_key_content(builder, &member, indent + 2, flow.then_some(quoting));
        builder.finish_node(); // KEY
        builder.token(SyntaxKind::COLON.into(), ":");
        builder.token(SyntaxKind::WHITESPACE.into(), " ");
        builder.start_node(SyntaxKind::VALUE.into());
        ScalarValue::null().build_content(builder, 0, flow);
        builder.finish_node(); // VALUE
        if !flow {
            builder.token(SyntaxKind::NEWLINE.into(), "\n");
        } else if i + 1 < len {
            builder.token(SyntaxKind::COMMA.into(), ",");
            builder.token(SyntaxKind::WHITESPACE.into(), " ");
        }
        builder.finish_node(); // MAPPING_ENTRY
    }
    if flow {
        builder.token(SyntaxKind::RIGHT_BRACE.into(), "}");
    }
    builder.finish_node(); // MAPPING

    builder.finish_node(); // TAGGED_NODE
    !flow
}

/// A `!!set` with the members as its keys, in sorted order.
///
/// There is no impl for `HashSet`, whose iteration order would make the
/// output change from run to run.
impl<T: AsYaml> AsYaml for std::collections::BTreeSet<T> {
    fn as_node(&self) -> Option<&crate::yaml::SyntaxNode> {
        None
    }

    fn kind(&self) -> YamlKind {
        YamlKind::Tagged(Cow::Borrowed("!!set"))
    }

    fn build_content(
        &self,
        builder: &mut rowan::GreenNodeBuilder,
        indent: usize,
        flow_context: bool,
    ) -> bool {
        build_set_content(
            builder,
            self.iter(),
            indent,
            flow_context.then_some(FlowQuoting::Minimal),
        )
    }

    fn is_inline(&self) -> bool {
        // The tag goes on the key's line, as in `tags: !!set`.
        true
    }

    fn build_flow_content(
        &self,
        builder: &mut rowan::GreenNodeBuilder,
        indent: usize,
        quoting: FlowQuoting,
    ) -> bool {
        build_set_content(builder, self.iter(), indent, Some(quoting))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::debug::assert_cst_ok;
    use crate::yaml::{Document, YamlFile};
    use std::collections::BTreeSet;
    use std::str::FromStr;

    /// Check that the `!!set` in `doc` is laid out as the parser would lay
    /// out its text.
    ///
    /// Only the set itself is compared: `Mapping::set` puts the space before
    /// a block tagged value inside VALUE, where the parser has it before
    /// VALUE (see the TODO in `build_block_value_node`).
    #[track_caller]
    fn assert_set_matches_reparse(doc: &Document) {
        assert_cst_ok(doc);
        let reparsed = Document::from_str(&doc.to_string()).unwrap();
        let tagged = |d: &Document| {
            d.syntax()
                .descendants()
                .find(|n| n.kind() == crate::lex::SyntaxKind::TAGGED_NODE)
                .map(|n| crate::debug::tree_to_string(&n))
                .unwrap()
        };
        assert_eq!(tagged(doc), tagged(&reparsed));
    }

    #[test]
    fn test_btreeset_block() {
        let doc = Document::from_str("name: project\n").unwrap();
        doc.set("tags", BTreeSet::from(["web", "db"]));
        assert_set_matches_reparse(&doc);
        assert_eq!(
            doc.to_string(),
            "name: project\ntags: !!set\n  db: null\n  web: null\n"
        );
    }

    #[test]
    fn test_btreeset_nested_block() {
        let doc = Document::from_str("a:\n  b: 1\n").unwrap();
        let inner = doc.as_mapping().unwrap().get_mapping("a").unwrap();
        inner.set("s", BTreeSet::from([1_i64, 2]));
        assert_set_matches_reparse(&doc);
        assert_eq!(
            doc.to_string(),
            "a:\n  b: 1\n  s: !!set\n    1: null\n    2: null\n"
        );
    }

    #[test]
    fn test_btreeset_empty() {
        let doc = Document::from_str("name: project\n").unwrap();
        doc.set("tags", BTreeSet::<String>::new());
        assert_set_matches_reparse(&doc);
        assert_eq!(doc.to_string(), "name: project\ntags: !!set {}\n");
    }

    #[test]
    fn test_btreeset_flow() {
        let doc = Document::from_str("m: {a: 1}\n").unwrap();
        let inner = doc.as_mapping().unwrap().get_mapping("m").unwrap();
        inner.set("s", BTreeSet::from(["y", "x"]));
        assert_set_matches_reparse(&doc);
        assert_eq!(doc.to_string(), "m: {a: 1, s: !!set {x: null, y: null}}\n");
    }

    #[test]
    fn test_btreeset_quotes_members() {
        let doc = Document::from_str("name: project\n").unwrap();
        doc.set("tags", BTreeSet::from(["true", "a: b", "x\ny"]));
        assert_set_matches_reparse(&doc);
        assert_eq!(
            doc.to_string(),
            "name: project\ntags: !!set\n  'a: b': null\n  'true': null\n  \"x\\ny\": null\n"
        );
    }

    #[test]
    fn test_btreeset_reads_back_as_set() {
        let doc = Document::from_str("name: project\n").unwrap();
        doc.set("tags", BTreeSet::from(["web", "db"]));
        let tags = doc.as_mapping().unwrap().get("tags").unwrap();
        let set = tags.as_tagged().unwrap().as_set().unwrap();
        assert_eq!(set.len(), 2);
        assert!(set.contains("db"));
        assert!(set.contains("web"));
        assert!(!set.contains("null"));
    }

    #[test]
    fn test_mapping_set_preserves_alias_kind() {
        let doc = Document::from_str("shared: &shared value\na: *shared\nb: old\n").unwrap();
        let mapping = doc.as_mapping().unwrap();
        mapping.set("b", mapping.get("a").unwrap());
        assert_cst_ok(&doc);
        assert_eq!(
            doc.to_string(),
            "shared: &shared value\na: *shared\nb: *shared\n"
        );

        match mapping.get("b").unwrap() {
            YamlNode::Alias(alias) => {
                assert_eq!(alias.name(), "shared");
                assert_eq!(alias.value(), "*shared");
            }
            other => panic!("Expected alias, got {:?}", other),
        }
    }

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

        // Escaped newlines should equal empty lines, which fold to newlines
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

        // yaml1 has a stream-level leading comment, so parse via YamlFile
        // rather than Document::from_str (which rejects stream-scoped content).
        let doc1 = YamlFile::from_str(yaml1).unwrap().document().unwrap();
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
    fn test_yaml_eq_mapping_ignores_order() {
        let doc1 = Document::from_str("{a: 1, b: 2}").unwrap();
        let doc2 = Document::from_str("{b: 2, a: 1}").unwrap();
        let doc3 = Document::from_str("{b: 1, a: 2}").unwrap();
        let m1 = doc1.as_mapping().unwrap();
        let m2 = doc2.as_mapping().unwrap();
        let m3 = doc3.as_mapping().unwrap();
        assert!(yaml_eq(&m1, &m2));
        assert!(yaml_eq(&m2, &m1));
        assert!(!yaml_eq(&m1, &m3));
    }

    #[test]
    fn test_yaml_eq_mapping_duplicate_keys() {
        let doc1 = Document::from_str("{a: 1, a: 1}").unwrap();
        let doc2 = Document::from_str("{a: 1, b: 1}").unwrap();
        let m1 = doc1.as_mapping().unwrap();
        let m2 = doc2.as_mapping().unwrap();
        assert!(!yaml_eq(&m1, &m2));
        assert!(!yaml_eq(&m2, &m1));
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

        // Mappings are unordered, so key order does not matter
        assert!(yaml_eq(&mapping1, &mapping3));
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
folded1: "line1 \
  line2"
folded2: "line1 line2"
"#;

        let doc = Document::from_str(yaml).unwrap();
        let mapping = doc.as_mapping().unwrap();

        let folded1 = mapping.get("folded1").unwrap();
        let folded2 = mapping.get("folded2").unwrap();

        // An escaped line break is dropped along with the next line's
        // indentation, but white space before it is kept
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

    #[test]
    fn test_option_none_emits_null() {
        let doc = Document::from_str("key: value\n").unwrap();
        doc.as_mapping().unwrap().set("key", None::<&str>);
        assert_cst_ok(&doc);
        assert_eq!(doc.to_string(), "key: null\n");
    }

    #[test]
    fn test_option_some_delegates() {
        let doc = Document::from_str("key: value\n").unwrap();
        doc.as_mapping().unwrap().set("key", Some("hello"));
        assert_cst_ok(&doc);
        assert_eq!(doc.to_string(), "key: hello\n");

        let doc = Document::from_str("key: value\n").unwrap();
        doc.as_mapping().unwrap().set("key", Some(42i64));
        assert_cst_ok(&doc);
        assert_eq!(doc.to_string(), "key: 42\n");
    }

    #[test]
    fn test_set_float_special_values() {
        let cases: [(f64, &str); 6] = [
            (f64::NAN, "k: .nan\n"),
            (f64::INFINITY, "k: .inf\n"),
            (f64::NEG_INFINITY, "k: -.inf\n"),
            (1.0, "k: 1.0\n"),
            (-0.0, "k: -0.0\n"),
            (1.5, "k: 1.5\n"),
        ];
        for (value, expected) in cases {
            let doc = Document::from_str("k: 0\n").unwrap();
            doc.as_mapping().unwrap().set("k", value);
            assert_cst_ok(&doc);
            assert_eq!(doc.to_string(), expected);
            let reparsed = Document::from_str(&doc.to_string()).unwrap();
            let got = reparsed
                .as_mapping()
                .unwrap()
                .get("k")
                .unwrap()
                .as_scalar()
                .unwrap()
                .as_f64()
                .unwrap();
            assert_eq!(got.to_bits(), value.to_bits());
        }

        let doc = Document::from_str("k: 0\n").unwrap();
        doc.as_mapping().unwrap().set("k", f32::INFINITY);
        assert_cst_ok(&doc);
        assert_eq!(doc.to_string(), "k: .inf\n");
    }

    #[test]
    fn test_yaml_eq_special_float_spellings() {
        let doc =
            Document::from_str("a: .inf\nb: .Inf\nc: .nan\nd: .NaN\ne: -.inf\nf: -.INF\n").unwrap();
        let m = doc.as_mapping().unwrap();
        assert!(yaml_eq(&m.get("a").unwrap(), &m.get("b").unwrap()));
        assert!(yaml_eq(&m.get("c").unwrap(), &m.get("d").unwrap()));
        assert!(yaml_eq(&m.get("e").unwrap(), &m.get("f").unwrap()));
        assert!(!yaml_eq(&m.get("a").unwrap(), &m.get("e").unwrap()));
        assert!(yaml_eq(&m.get("b").unwrap(), &f64::INFINITY));
    }

    #[test]
    fn test_yaml_eq_integers_outside_i64() {
        let yaml = "dec: 18446744073709551616\n\
                    hex: 0x10000000000000000\n\
                    oct: 0o2000000000000000000000\n\
                    padded: 00018446744073709551616\n\
                    tagged: !!int 0x10000000000000000\n\
                    other: 18446744073709551617\n\
                    min: 9223372036854775808\n\
                    plusmin: +9223372036854775808\n\
                    negmin: -9223372036854775809\n\
                    neghex: -0x8000000000000001\n";
        assert!(eq_values(yaml, "dec", "hex"));
        assert!(eq_values(yaml, "dec", "oct"));
        assert!(eq_values(yaml, "dec", "padded"));
        assert!(eq_values(yaml, "dec", "tagged"));
        assert!(!eq_values(yaml, "dec", "other"));
        assert!(eq_values(yaml, "min", "plusmin"));
        assert!(!eq_values(yaml, "min", "negmin"));
        // `-0x...` is not a core schema integer, so it is a string.
        assert!(!eq_values(yaml, "negmin", "neghex"));
        let doc = Document::from_str("a: 0xFFFFFFFFFFFFFFFF\n").unwrap();
        let a = doc.as_mapping().unwrap().get("a").unwrap();
        assert!(yaml_eq(&a, &u64::MAX));
    }

    #[test]
    fn test_yaml_eq_negative_zero() {
        assert!(eq_values("a: -0\nb: 0\nc: +000\n", "a", "b"));
        assert!(eq_values("a: -0\nb: 0\nc: +000\n", "a", "c"));
    }

    fn eq_values(yaml: &str, a: &str, b: &str) -> bool {
        let doc = Document::from_str(yaml).unwrap();
        let m = doc.as_mapping().unwrap();
        let (a, b) = (m.get(a).unwrap(), m.get(b).unwrap());
        let forward = yaml_eq(&a, &b);
        assert_eq!(forward, yaml_eq(&b, &a));
        forward
    }

    fn eq_docs(a: &str, b: &str) -> bool {
        let (d1, d2) = (
            Document::from_str(a).unwrap(),
            Document::from_str(b).unwrap(),
        );
        let (m1, m2) = (d1.as_mapping().unwrap(), d2.as_mapping().unwrap());
        let forward = yaml_eq(&m1, &m2);
        assert_eq!(forward, yaml_eq(&m2, &m1));
        forward
    }

    #[test]
    fn test_yaml_eq_tagged_key() {
        assert!(eq_docs("{a: 1}", "{!!str a: 1}"));
        assert!(eq_docs("a: 1\n", "!!str a: 1\n"));
        assert!(eq_docs("a: 1\n", "? !!str a\n: 1\n"));
        assert!(!eq_docs("1: a\n", "!!str 1: a\n"));
        assert!(eq_docs("'1': a\n", "!!str 1: a\n"));
        assert!(eq_docs("1: a\n", "!!int 0x1: a\n"));
    }

    #[test]
    fn test_yaml_eq_tagged_value() {
        assert!(eq_docs("{a: 1}", "{a: !!int 1}"));
        assert!(eq_docs("a: 1\n", "a: !!int\n  1\n"));
        assert!(!eq_docs("a: 1\n", "a: !!str 1\n"));
    }

    #[test]
    fn test_yaml_eq_tagged_scalars() {
        let yaml = "hex: !!int 0x1\n\
                    int: !!int 1\n\
                    plain: 1\n\
                    quoted: '1'\n\
                    str: !!str 1\n\
                    verbatim: !<tag:yaml.org,2002:int> 01\n\
                    float: !!float 1\n\
                    plainfloat: 1.0\n\
                    nonspecific: ! 1\n\
                    custom: !foo 1\n\
                    custom2: !bar 1\n\
                    bool: !!bool TRUE\n\
                    plainbool: true\n\
                    nulltag: !!null ~\n\
                    plainnull:\n";
        assert!(eq_values(yaml, "hex", "int"));
        assert!(eq_values(yaml, "hex", "plain"));
        assert!(eq_values(yaml, "verbatim", "plain"));
        assert!(!eq_values(yaml, "plain", "quoted"));
        assert!(eq_values(yaml, "str", "quoted"));
        assert!(!eq_values(yaml, "str", "plain"));
        assert!(eq_values(yaml, "float", "plainfloat"));
        assert!(!eq_values(yaml, "float", "plain"));
        assert!(eq_values(yaml, "nonspecific", "quoted"));
        assert!(!eq_values(yaml, "custom", "quoted"));
        assert!(!eq_values(yaml, "custom", "custom2"));
        assert!(eq_values(yaml, "custom", "custom"));
        assert!(eq_values(yaml, "bool", "plainbool"));
        assert!(eq_values(yaml, "nulltag", "plainnull"));
    }

    #[test]
    fn test_yaml_eq_tagged_scalar_raw() {
        let doc = Document::from_str("a: !!int 0x1\nb: !!str 1\n").unwrap();
        let m = doc.as_mapping().unwrap();
        assert!(yaml_eq(&m.get("a").unwrap(), &1_i64));
        assert!(!yaml_eq(&m.get("a").unwrap(), &"1"));
        assert!(yaml_eq(&m.get("b").unwrap(), &"1"));
        assert!(!yaml_eq(&m.get("b").unwrap(), &1_i64));
    }

    #[test]
    fn test_yaml_eq_tagged_collections() {
        let yaml = "map: !!map {a: 1}\n\
                    plainmap: {a: 1}\n\
                    custommap: !foo {a: 1}\n\
                    set1: !!set {a, b}\n\
                    set2: !!set {b, a}\n\
                    set3: !!set {a, c}\n\
                    seq: !!seq [1, 2]\n\
                    plainseq: [1, 2]\n";
        assert!(eq_values(yaml, "map", "plainmap"));
        assert!(!eq_values(yaml, "custommap", "plainmap"));
        assert!(eq_values(yaml, "set1", "set2"));
        assert!(!eq_values(yaml, "set1", "set3"));
        assert!(eq_values(yaml, "seq", "plainseq"));
        assert!(!eq_values(yaml, "seq", "plainmap"));
    }

    #[test]
    fn test_yaml_eq_cst_vs_raw_collections() {
        use crate::value::YamlValue;
        use crate::ScalarValue;
        use std::collections::BTreeMap;

        let doc = Document::from_str("{x: 1, y: 2}").unwrap();
        let cst = doc.as_mapping().unwrap();
        let raw = YamlValue::Mapping(BTreeMap::from([
            ("x".to_string(), YamlValue::scalar(1_i64)),
            ("y".to_string(), YamlValue::scalar(2_i64)),
        ]));
        assert!(yaml_eq(&cst, &raw));
        assert!(yaml_eq(&raw, &cst));

        let other = YamlValue::Mapping(BTreeMap::from([
            ("x".to_string(), YamlValue::scalar(1_i64)),
            ("y".to_string(), YamlValue::scalar("2")),
        ]));
        assert!(!yaml_eq(&cst, &other));
        assert!(!yaml_eq(&raw, &other));
        assert!(yaml_eq(&raw, &raw.clone()));

        let doc = Document::from_str("[1, two]").unwrap();
        let cst = doc.as_sequence().unwrap();
        let raw = YamlValue::from_sequence(vec![
            YamlValue::scalar(ScalarValue::parse("1")),
            YamlValue::scalar("two"),
        ]);
        assert!(yaml_eq(&cst, &raw));
        assert!(yaml_eq(&raw, &cst));
        assert!(!yaml_eq(&cst, &YamlValue::from_sequence(vec![])));
    }
}
