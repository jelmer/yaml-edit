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
//! - **Input types** (`&str`, `i64`, `bool`, [`YamlValue`](crate::YamlValue), …):
//!   supply these to mutation methods such as
//!   [`Mapping::set`](crate::yaml::Mapping::set).  They implement [`AsYaml`] but are
//!   never returned from navigation methods.
//!
//! [`YamlNode`] is the type-erased return type for navigation: you get one back from
//! [`Mapping::get`](crate::yaml::Mapping::get), iterator methods like
//! [`Mapping::keys`](crate::yaml::Mapping::keys), etc.  It is always backed by a real
//! CST node; match on it to get the concrete type.

use crate::tag;
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
/// - A [`YamlValue`](crate::YamlValue), `Vec<YamlValue>` or `BTreeMap<String, YamlValue>`
///
/// A CST node is copied with the comments inside it, but not those around
/// it, such as a comment line before it or a comment after a scalar.
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

    /// The node this value is copied from: [`as_node`](Self::as_node), or
    /// for a document its root.
    #[doc(hidden)]
    fn value_node(&self) -> Option<SyntaxNode> {
        self.as_node().cloned()
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

/// Feed a hash of `value` into `state`, consistent with [`yaml_eq`]: values
/// that compare equal hash equally, so they can be used as keys in hash
/// maps with an equality defined by [`yaml_eq`].
///
/// [`yaml_eq`] is not reflexive for values containing aliases or content
/// that cannot be resolved, so such values need care as hash map keys.
pub fn yaml_hash<T, H>(value: &T, state: &mut H)
where
    T: AsYaml + ?Sized,
    H: std::hash::Hasher,
{
    use std::hash::Hash;
    if let Some(Ok(Some(identity))) = to_syntax(value).map(|node| node_identity(&node, None)) {
        identity.hash(state);
    }
}

/// The node backing `v`, or for a raw value, the CST it builds.
fn to_syntax<T: AsYaml + ?Sized>(v: &T) -> Option<SyntaxNode> {
    use crate::lex::SyntaxKind;
    if let Some(node) = v.value_node() {
        return Some(node);
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

/// The identity of a node under YAML node equality: its resolved tag and
/// canonical content, with aliases followed. Two nodes are equal, and so
/// the same mapping key, exactly when their identities are.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct NodeIdentity(Identity<NodeIdentity>);

/// A node's identity, with that of each node in it given as a `T`.
///
/// Mapping entries are kept sorted, since mappings are unordered.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) enum Identity<T> {
    Scalar {
        tag: String,
        value: String,
    },
    Sequence {
        tag: String,
        items: Vec<T>,
    },
    Mapping {
        tag: String,
        entries: Vec<(T, T)>,
    },
    /// An alias that is not followed, as for [`lookup_identity`].
    Alias {
        tag: Option<String>,
        name: String,
    },
}

impl NodeIdentity {
    /// The string form of a node with this identity as a key of a
    /// [`YamlValue`](crate::YamlValue) mapping: the canonical value of a
    /// scalar, without its tag, and a flow rendering of a collection.
    ///
    /// Equal nodes have the same string form, however they are written.
    pub(crate) fn key_string(&self) -> String {
        match &self.0 {
            Identity::Scalar { value, .. } => value.clone(),
            _ => self.flow_text(),
        }
    }

    /// Flow YAML text for a node with this identity, which reads back as an
    /// equal node.
    fn flow_text(&self) -> String {
        use crate::scalar::ScalarValue;
        let tagged = |tag: &str, default: &str, content: String| {
            if tag == default {
                content
            } else {
                format!("{tag} {content}")
            }
        };
        match &self.0 {
            Identity::Scalar { tag, value } if tag == tag::STR => {
                ScalarValue::string(value.as_str()).to_flow_string()
            }
            Identity::Scalar { tag, value } => {
                let plain = resolve_scalar(ScalarValue::parse(value.as_str()));
                if !value.is_empty() && plain.tag == *tag && plain.value == *value {
                    value.clone()
                } else {
                    format!(
                        "{tag} {}",
                        ScalarValue::string(value.as_str()).to_flow_string()
                    )
                }
            }
            Identity::Sequence { tag, items } => {
                let items: Vec<_> = items.iter().map(Self::flow_text).collect();
                tagged(tag, tag::SEQ, format!("[{}]", items.join(", ")))
            }
            Identity::Mapping { tag, entries } => {
                let entries: Vec<_> = entries
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k.flow_text(), v.flow_text()))
                    .collect();
                tagged(tag, tag::MAP, format!("{{{}}}", entries.join(", ")))
            }
            Identity::Alias { tag, name } => match tag {
                Some(tag) => format!("{tag} *{name}"),
                None => format!("*{name}"),
            },
        }
    }
}

/// The resolved tag of a collection written with `tag`, if any.
fn collection_tag(tag: Option<String>, default: &str) -> String {
    match tag.as_deref() {
        // The non-specific tag `!` resolves a collection by its kind.
        None | Some("!") => default.to_string(),
        Some(t) => tag::normalize(t).into_owned(),
    }
}

/// A scalar's resolved tag and canonical value.
struct ResolvedScalar {
    tag: String,
    value: String,
}

/// Resolve a scalar to its tag and canonical value.
fn resolve_scalar(scalar: crate::scalar::ScalarValue) -> ResolvedScalar {
    use crate::scalar::{ScalarType, ScalarValue};
    let text = scalar.value();
    let canonical = |parsed: Option<String>| parsed.unwrap_or_else(|| text.to_string());
    let (tag, value) = match scalar.scalar_type() {
        ScalarType::Integer => (
            tag::INT.to_string(),
            canonical(ScalarValue::canonical_integer(text)),
        ),
        ScalarType::Float => (
            tag::FLOAT.to_string(),
            canonical(
                ScalarValue::parse_float(text).map(|f| ScalarValue::format_float(f.to_string())),
            ),
        ),
        ScalarType::Boolean => (
            tag::BOOL.to_string(),
            canonical(ScalarValue::parse_bool(text).map(|b| b.to_string())),
        ),
        ScalarType::Null => (tag::NULL.to_string(), "null".to_string()),
        _ => {
            // A scalar with the non-specific tag `!` is a string.
            let tag = match scalar.tag() {
                Some(t) if t != "!" => tag::normalize(t).into_owned(),
                _ => tag::STR.to_string(),
            };
            (tag, text.to_string())
        }
    };
    ResolvedScalar { tag, value }
}

/// The identity of `node`, a value node or the KEY or VALUE wrapper holding
/// one.
///
/// Aliases are followed through `registry`; without one an alias is
/// undefined, as no anchors are known.
/// Returns `Ok(None)` if `node` is not a value node.
pub(crate) fn node_identity(
    node: &SyntaxNode,
    registry: Option<&crate::anchor_resolution::AnchorRegistry>,
) -> IdentityResult<NodeIdentity> {
    let aliases = match registry {
        Some(registry) => Aliases::Follow(registry),
        None => Aliases::Undefined,
    };
    identity_with_tag(node, None, aliases, &mut Vec::new(), &mut Trees)
}

/// The identity of `node` for looking it up as a key among a mapping's
/// own keys: as [`node_identity`], but with each alias standing for itself
/// rather than for the node it refers to, so that a key holding an alias
/// matches a key written with the same alias (`? [*a]`).
///
/// Returns `None` if `node` is not a value node.
pub(crate) fn lookup_identity(node: &SyntaxNode) -> Option<NodeIdentity> {
    identity_with_tag(node, None, Aliases::ByName, &mut Vec::new(), &mut Trees)
        .expect("aliases are not followed")
}

/// What [`identity_with_tag`] does with an alias.
#[derive(Clone, Copy)]
enum Aliases<'a> {
    /// Report it as undefined, as no anchors are known.
    Undefined,
    /// Follow it to the node it refers to.
    Follow(&'a crate::anchor_resolution::AnchorRegistry),
    /// Take it as itself, by its name and tag.
    ByName,
}

/// The identity of a node, `None` if it is not a value node.
pub(crate) type IdentityResult<T> = Result<Option<T>, crate::anchor_resolution::AliasError>;

/// The identity of `key` as a mapping key, following aliases through
/// `registry`; see [`NodeIdentity`]. Without a registry an alias is
/// undefined.
pub(crate) fn key_identity(
    key: &YamlNode,
    registry: Option<&crate::anchor_resolution::AnchorRegistry>,
) -> Result<NodeIdentity, crate::anchor_resolution::AliasError> {
    Ok(node_identity(key.syntax(), registry)?.expect("a YamlNode is a value node"))
}

/// Makes the identity of a node out of those of the nodes in it.
trait IdentityBuilder {
    type Id: Ord;

    fn build(&mut self, identity: Identity<Self::Id>) -> Self::Id;

    /// The identity worked out before for `node`, written without a tag
    /// from outside it.
    fn known(&self, _node: &SyntaxNode) -> Option<IdentityResult<Self::Id>> {
        None
    }

    fn remember(&mut self, _node: &SyntaxNode, _identity: &IdentityResult<Self::Id>) {}
}

/// Builds each identity as a [`NodeIdentity`] of its own.
struct Trees;

impl IdentityBuilder for Trees {
    type Id = NodeIdentity;

    fn build(&mut self, identity: Identity<NodeIdentity>) -> NodeIdentity {
        NodeIdentity(identity)
    }
}

/// The identities of the nodes of one tree, as numbers that are equal
/// exactly when the identities are.
///
/// A node's identity covers all the nodes in it. Each of those is worked
/// out once and then shared, so finding the identities of nested keys
/// takes time linear in the size of the tree. Aliases are undefined, as
/// for [`node_identity`] without a registry.
#[derive(Default)]
pub(crate) struct SharedIdentities {
    numbers: std::collections::HashMap<Identity<usize>, usize>,
    nodes: std::collections::HashMap<SyntaxNode, IdentityResult<usize>>,
}

impl SharedIdentities {
    /// The identity of `node`, as [`node_identity`] without a registry
    /// gives it.
    pub(crate) fn identity(&mut self, node: &SyntaxNode) -> IdentityResult<usize> {
        identity_with_tag(node, None, Aliases::Undefined, &mut Vec::new(), self)
    }
}

impl IdentityBuilder for SharedIdentities {
    type Id = usize;

    fn build(&mut self, identity: Identity<usize>) -> usize {
        let next = self.numbers.len();
        *self.numbers.entry(identity).or_insert(next)
    }

    fn known(&self, node: &SyntaxNode) -> Option<IdentityResult<usize>> {
        self.nodes.get(node).cloned()
    }

    fn remember(&mut self, node: &SyntaxNode, identity: &IdentityResult<usize>) {
        self.nodes.insert(node.clone(), identity.clone());
    }
}

/// The identity of `node` with `tag`, if set, in place of its own tag.
///
/// `expanding` holds the anchored nodes whose aliases are being followed,
/// so an alias within its own anchored node is reported as recursive.
fn identity_with_tag<B: IdentityBuilder>(
    node: &SyntaxNode,
    tag: Option<String>,
    aliases: Aliases,
    expanding: &mut Vec<SyntaxNode>,
    builder: &mut B,
) -> IdentityResult<B::Id> {
    use crate::lex::SyntaxKind;

    // A tag from outside changes the identity, so only one without is
    // shared, and only that of a node with others in it saves any work.
    let holds_nodes = matches!(
        node.kind(),
        SyntaxKind::MAPPING | SyntaxKind::SEQUENCE | SyntaxKind::TAGGED_NODE
    );
    if tag.is_some() || !holds_nodes {
        return build_identity(node, tag, aliases, expanding, builder);
    }
    if let Some(identity) = builder.known(node) {
        return identity;
    }
    let identity = build_identity(node, None, aliases, expanding, builder);
    builder.remember(node, &identity);
    identity
}

/// [`identity_with_tag`], without looking for an identity worked out
/// before.
fn build_identity<B: IdentityBuilder>(
    node: &SyntaxNode,
    tag: Option<String>,
    aliases: Aliases,
    expanding: &mut Vec<SyntaxNode>,
    builder: &mut B,
) -> IdentityResult<B::Id> {
    use crate::anchor_resolution::AliasError;
    use crate::lex::SyntaxKind;
    use crate::scalar::ScalarValue;

    let with_tag = |scalar: ScalarValue, tag: Option<String>| match tag {
        Some(tag) => scalar.with_resolved_tag(tag),
        None => scalar,
    };
    let scalar = |builder: &mut B, scalar: ScalarValue, tag: Option<String>| {
        let ResolvedScalar { tag, value } = resolve_scalar(with_tag(scalar, tag));
        builder.build(Identity::Scalar { tag, value })
    };
    match node.kind() {
        SyntaxKind::KEY | SyntaxKind::VALUE => match node.first_child() {
            Some(content) => identity_with_tag(&content, tag, aliases, expanding, builder),
            None => Ok(None),
        },
        SyntaxKind::TAGGED_NODE => {
            let Some(tagged) = TaggedNode::cast(node.clone()) else {
                return Ok(None);
            };
            // A tag passed in is that of an alias to this node, which takes
            // the place of the node's own.
            let tag = tag.or_else(|| tagged.tag());
            match node.first_child() {
                Some(content) => identity_with_tag(&content, tag, aliases, expanding, builder),
                None => Ok(ScalarValue::from_tagged(&tagged).map(|s| scalar(builder, s, tag))),
            }
        }
        SyntaxKind::ALIAS => {
            let Some(alias) = Alias::cast(node.clone()) else {
                return Ok(None);
            };
            let registry = match aliases {
                Aliases::Follow(registry) => registry,
                Aliases::ByName => {
                    return Ok(Some(builder.build(Identity::Alias {
                        tag,
                        name: alias.name(),
                    })))
                }
                Aliases::Undefined => {
                    return Err(AliasError::Undefined {
                        alias: alias.name(),
                    })
                }
            };
            let target = registry.resolve_alias_through(&alias)?;
            if expanding.contains(target) {
                return Err(AliasError::Recursive {
                    alias: alias.name(),
                });
            }
            expanding.push(target.clone());
            let identity = identity_with_tag(target, tag, aliases, expanding, builder);
            expanding.pop();
            identity
        }
        SyntaxKind::SCALAR => {
            Ok(Scalar::cast(node.clone())
                .map(|s| scalar(builder, ScalarValue::from_scalar(&s), tag)))
        }
        SyntaxKind::MAPPING => {
            let Some(mapping) = Mapping::cast(node.clone()) else {
                return Ok(None);
            };
            let mut entries = Vec::new();
            for (key, value) in mapping.pairs() {
                let key = identity_with_tag(&key, None, aliases, expanding, builder)?;
                let value = identity_with_tag(&value, None, aliases, expanding, builder)?;
                let (Some(key), Some(value)) = (key, value) else {
                    return Ok(None);
                };
                entries.push((key, value));
            }
            entries.sort();
            Ok(Some(builder.build(Identity::Mapping {
                tag: collection_tag(tag, tag::MAP),
                entries,
            })))
        }
        SyntaxKind::SEQUENCE => {
            let Some(sequence) = Sequence::cast(node.clone()) else {
                return Ok(None);
            };
            let mut items = Vec::new();
            for item in sequence.items() {
                let Some(item) = identity_with_tag(&item, None, aliases, expanding, builder)?
                else {
                    return Ok(None);
                };
                items.push(item);
            }
            Ok(Some(builder.build(Identity::Sequence {
                tag: collection_tag(tag, tag::SEQ),
                items,
            })))
        }
        _ => Ok(None),
    }
}

/// The identity of `v`, as [`node_identity`] gives it for the node backing
/// `v` or, for a raw value, the CST it builds.
pub(crate) fn identity_of<T: AsYaml + ?Sized>(
    v: &T,
    registry: Option<&crate::anchor_resolution::AnchorRegistry>,
) -> Result<Option<NodeIdentity>, crate::anchor_resolution::AliasError> {
    match to_syntax(v) {
        Some(node) => node_identity(&node, registry),
        None => Ok(None),
    }
}

/// The resolved tag and canonical value of a scalar node, tagged or not,
/// or of the scalar a KEY or VALUE wrapper holds.
///
/// Returns `None` for anything that is not a scalar.
pub(crate) fn scalar_canonical(node: &SyntaxNode) -> Option<(String, String)> {
    match node_identity(node, None) {
        Ok(Some(NodeIdentity(Identity::Scalar { tag, value }))) => Some((tag, value)),
        _ => None,
    }
}

/// Spec equality of two CST nodes. An alias equals nothing, as it cannot
/// be followed here.
fn node_eq(a: &SyntaxNode, b: &SyntaxNode) -> bool {
    match (node_identity(a, None), node_identity(b, None)) {
        (Ok(Some(a)), Ok(Some(b))) => a == b,
        _ => false,
    }
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

    /// Get the byte offset range of this node in the source text.
    pub fn byte_range(&self) -> crate::TextPosition {
        self.syntax().text_range().into()
    }

    /// If this node is a scalar, try to parse it as an `i64`.
    ///
    /// Returns `None` if this is not a scalar or cannot be parsed as an integer.
    pub fn to_i64(&self) -> Option<i64> {
        crate::scalar::ScalarValue::from_scalar(self.as_scalar()?).to_i64()
    }

    /// If this node is a scalar, try to parse it as an `f64`.
    ///
    /// Returns `None` if this is not a float scalar, such as for an integer.
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

    fn value_node(&self) -> Option<SyntaxNode> {
        (*self).value_node()
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
/// whitespace, and the lines inside a multi-line quoted scalar, by `delta`
/// columns (may be negative to dedent). Only spaces are added or removed;
/// tabs after them are kept. Line-leading whitespace is written as INDENT,
/// as the parser does.
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
                    builder.token(t.kind().into(), &shift_inner_lines(t.text(), delta));
                    after_newline = false;
                }
            },
        }
    }
    after_newline
}

/// `delta`, raised where needed so that every line of `node` after its
/// first ends up past column `parent`. The continuation lines of a flow node
/// or multi-line scalar have to clear the block entry it is nested in, but
/// at the top of a document they may start at column 0.
pub(crate) fn clear_parent_column(node: &SyntaxNode, parent: usize, delta: isize) -> isize {
    let shallowest = node
        .text()
        .to_string()
        .split(['\n', '\r'])
        .skip(1)
        .filter(|line| !line.trim().is_empty())
        .map(crate::column::space_indent)
        .min();
    shallowest.map_or(delta, |column| {
        delta.max(parent as isize + 1 - column as isize)
    })
}

/// `text`, a line or the whitespace at its start, with `delta` spaces
/// added or up to `-delta` leading spaces removed. Tabs are not
/// indentation, so any that follow the spaces are kept.
pub(crate) fn shift_line_start(text: &str, delta: isize) -> String {
    if delta >= 0 {
        return format!("{}{text}", " ".repeat(delta as usize));
    }
    let spaces = crate::column::space_indent(text);
    text[spaces.min(delta.unsigned_abs())..].to_string()
}

/// `text` with the indentation of each line after the first moved by
/// `delta` columns, leaving empty lines empty.
pub(crate) fn shift_inner_lines(text: &str, delta: isize) -> Cow<'_, str> {
    if delta == 0 || !text.contains(['\n', '\r']) {
        return Cow::Borrowed(text);
    }
    // A `\r\n` splits into a line ending in `\r` and an empty one.
    let mut lines = text.split_inclusive(['\n', '\r']);
    let mut shifted = lines.next().unwrap_or_default().to_string();
    for line in lines {
        if line.trim_end_matches(['\n', '\r']).is_empty() {
            shifted.push_str(line);
        } else {
            shifted.push_str(&shift_line_start(line, delta));
        }
    }
    Cow::Owned(shifted)
}

/// Base column to measure a copied collection's indentation from: for a
/// flow collection, whose continuation lines are only bound to the
/// indentation of the line it starts on, that line's indentation;
/// otherwise the column of its first entry.
pub(crate) fn source_reindent_base(node: &SyntaxNode, is_flow: bool) -> usize {
    if is_flow {
        crate::column::line_indent(node)
    } else {
        crate::column::node_column(node)
    }
}

/// Implement AsYaml for primitives that build as the scalar `$to_scalar`
/// turns them into.
macro_rules! impl_as_yaml_scalar {
    ($to_scalar:expr; $($ty:ty),+ $(,)?) => {
        $(impl AsYaml for $ty {
            fn as_node(&self) -> Option<&crate::yaml::SyntaxNode> {
                None
            }

            fn kind(&self) -> YamlKind {
                YamlKind::Scalar
            }

            fn build_content(&self, builder: &mut rowan::GreenNodeBuilder, indent: usize, _flow_context: bool) -> bool {
                let to_scalar: fn(&$ty) -> crate::scalar::ScalarValue = $to_scalar;
                to_scalar(self).build(builder, indent, None)
            }

            fn is_inline(&self) -> bool {
                true
            }
        })+
    };
}

impl_as_yaml_scalar!(
    |v| crate::scalar::ScalarValue::plain(v.to_string()).with_type(crate::scalar::ScalarType::Integer);
    i64, i32, i16, i8, isize, u64, u32, u16, u8, usize
);
impl_as_yaml_scalar!(|v| crate::scalar::ScalarValue::from(*v); f64, f32, bool);

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

/// Emit a TAGGED_NODE with `tag`, followed by the content `content` emits:
/// on a line of its own indented past `indent` if `block` is set, after a
/// space otherwise. Returns whether the content ends with a newline.
pub(crate) fn build_tagged(
    builder: &mut rowan::GreenNodeBuilder,
    tag: &str,
    block: bool,
    indent: usize,
    content: impl FnOnce(&mut rowan::GreenNodeBuilder) -> bool,
) -> bool {
    use crate::lex::SyntaxKind;

    builder.start_node(SyntaxKind::TAGGED_NODE.into());
    builder.token(SyntaxKind::TAG.into(), tag);
    if block {
        builder.token(SyntaxKind::NEWLINE.into(), "\n");
        builder.token(SyntaxKind::INDENT.into(), &" ".repeat(indent + 2));
    } else {
        builder.token(SyntaxKind::WHITESPACE.into(), " ");
    }
    let ends_with_newline = content(builder);
    builder.finish_node(); // TAGGED_NODE
    ends_with_newline
}

/// Emit a mapping of `entries`, in flow style quoting strings as `flow`
/// says if it is set or there are no entries, in block style otherwise.
/// Returns whether the content ends with a newline.
pub(crate) fn build_mapping<K: AsYaml, V: AsYaml>(
    builder: &mut rowan::GreenNodeBuilder,
    entries: impl ExactSizeIterator<Item = (K, V)>,
    indent: usize,
    flow: Option<FlowQuoting>,
) -> bool {
    use crate::lex::SyntaxKind;

    let len = entries.len();
    let block = flow.is_none() && len > 0;
    // Entries of an empty block mapping would be written as `{}`.
    let flow = (!block).then_some(flow.unwrap_or(FlowQuoting::Minimal));

    builder.start_node(SyntaxKind::MAPPING.into());
    if !block {
        builder.token(SyntaxKind::LEFT_BRACE.into(), "{");
    }
    for (i, (key, value)) in entries.enumerate() {
        if i > 0 && block && indent > 0 {
            // The previous entry's newline ends its line.
            builder.token(SyntaxKind::INDENT.into(), &" ".repeat(indent));
        }
        builder.start_node(SyntaxKind::MAPPING_ENTRY.into());
        builder.start_node(SyntaxKind::KEY.into());
        crate::nodes::build_key_content(builder, &key, indent, flow);
        builder.finish_node(); // KEY
        builder.token(SyntaxKind::COLON.into(), ":");
        let starts_block = block && !value.is_inline();
        let ends_with_newline = if starts_block {
            builder.start_node(SyntaxKind::VALUE.into());
            builder.token(SyntaxKind::NEWLINE.into(), "\n");
            builder.token(SyntaxKind::INDENT.into(), &" ".repeat(indent + 2));
            build_in(&value, builder, indent + 2, None)
        } else {
            builder.token(SyntaxKind::WHITESPACE.into(), " ");
            builder.start_node(SyntaxKind::VALUE.into());
            // A block scalar's body has to clear the key's column.
            build_in(&value, builder, indent, flow)
        };
        builder.finish_node(); // VALUE
        if block {
            if !ends_with_newline {
                builder.token(SyntaxKind::NEWLINE.into(), "\n");
            }
        } else if i + 1 < len {
            builder.token(SyntaxKind::COMMA.into(), ",");
            builder.token(SyntaxKind::WHITESPACE.into(), " ");
        }
        builder.finish_node(); // MAPPING_ENTRY
    }
    if !block {
        builder.token(SyntaxKind::RIGHT_BRACE.into(), "}");
    }
    builder.finish_node(); // MAPPING
    block
}

/// Emit a sequence of `items`, in flow style quoting strings as `flow`
/// says if it is set or there are no items, in block style otherwise.
/// Returns whether the content ends with a newline.
pub(crate) fn build_sequence<T: AsYaml>(
    builder: &mut rowan::GreenNodeBuilder,
    items: impl ExactSizeIterator<Item = T>,
    indent: usize,
    flow: Option<FlowQuoting>,
) -> bool {
    use crate::lex::SyntaxKind;

    let len = items.len();
    let block = flow.is_none() && len > 0;
    let flow = (!block).then_some(flow.unwrap_or(FlowQuoting::Minimal));

    builder.start_node(SyntaxKind::SEQUENCE.into());
    if !block {
        builder.token(SyntaxKind::LEFT_BRACKET.into(), "[");
    }
    for (i, item) in items.enumerate() {
        if i > 0 && block && indent > 0 {
            // The previous entry's newline ends its line.
            builder.token(SyntaxKind::INDENT.into(), &" ".repeat(indent));
        }
        builder.start_node(SyntaxKind::SEQUENCE_ENTRY.into());
        if block {
            builder.token(SyntaxKind::DASH.into(), "-");
            builder.token(SyntaxKind::WHITESPACE.into(), " ");
            // An inline item, such as a block scalar, indents its body past
            // the dash; a block collection aligns with its first entry.
            let item_indent = if item.is_inline() { indent } else { indent + 2 };
            if !build_in(&item, builder, item_indent, None) {
                builder.token(SyntaxKind::NEWLINE.into(), "\n");
            }
        } else {
            build_in(&item, builder, indent, flow);
            if i + 1 < len {
                builder.token(SyntaxKind::COMMA.into(), ",");
                builder.token(SyntaxKind::WHITESPACE.into(), " ");
            }
        }
        builder.finish_node(); // SEQUENCE_ENTRY
    }
    if !block {
        builder.token(SyntaxKind::RIGHT_BRACKET.into(), "]");
    }
    builder.finish_node(); // SEQUENCE
    block
}

/// Emit a `!!set`: a tagged mapping with each member as a key and a null
/// value. Returns whether the content ends with a newline.
pub(crate) fn build_set_content<T: AsYaml>(
    builder: &mut rowan::GreenNodeBuilder,
    members: impl ExactSizeIterator<Item = T>,
    indent: usize,
    flow: Option<FlowQuoting>,
) -> bool {
    let block = flow.is_none() && members.len() > 0;
    build_tagged(builder, tag::SET, block, indent, |builder| {
        let entries = members.map(|m| (m, crate::scalar::ScalarValue::null()));
        build_mapping(builder, entries, indent + 2, flow)
    })
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
        YamlKind::Tagged(Cow::Borrowed(tag::SET))
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
