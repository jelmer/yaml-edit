//! Builder pattern for constructing YAML documents fluently.

use crate::{
    as_yaml::{AsYaml, YamlNode},
    lex::SyntaxKind,
    yaml::{Document, SyntaxNode, YamlFile},
};
use rowan::{ast::AstNode, Checkpoint, GreenNodeBuilder};

/// A builder for constructing YAML documents with a fluent API.
pub struct YamlBuilder {
    file: YamlFile,
}

impl YamlBuilder {
    /// Start building from a scalar value.
    pub fn scalar(value: impl AsYaml) -> Self {
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(SyntaxKind::ROOT.into());
        builder.start_node(SyntaxKind::DOCUMENT.into());
        if !value.build_content(&mut builder, 0, false) {
            builder.token(SyntaxKind::NEWLINE.into(), "\n");
        }
        builder.finish_node();
        builder.finish_node();
        let green = builder.finish();
        YamlBuilder {
            file: YamlFile(rowan::SyntaxNode::new_root_mut(green)),
        }
    }

    /// Start building from an alias (`*name`).
    ///
    /// `name` is the anchor name without the `*` prefix.
    ///
    /// # Examples
    ///
    /// ```
    /// use yaml_edit::YamlBuilder;
    ///
    /// let yaml = YamlBuilder::alias("shared").build();
    /// assert_eq!(yaml.to_string(), "*shared\n");
    /// ```
    pub fn alias(name: impl AsRef<str>) -> Self {
        Self::scalar(crate::yaml::Alias::new(name))
    }

    /// Start building from a sequence.
    pub fn sequence() -> SequenceBuilder {
        SequenceBuilder::new()
    }

    /// Start building from a mapping.
    pub fn mapping() -> MappingBuilder {
        MappingBuilder::new()
    }

    /// Build the final YAML file.
    pub fn build(self) -> YamlFile {
        self.file
    }
}

impl Default for YamlBuilder {
    fn default() -> Self {
        Self::mapping().build()
    }
}

/// Builder for YAML sequences.
pub struct SequenceBuilder {
    builder: GreenNodeBuilder<'static>,
    /// Column of this sequence's dashes. Only a top-level sequence sits at
    /// column 0; a nested one starts on the line after its parent's `-` or
    /// `key:`.
    indent: usize,
    /// Where the SEQUENCE node starts, set once the first entry is emitted.
    start: Option<Checkpoint>,
}

/// Emit what precedes the next entry of a block collection at column
/// `indent`, recording in `start` where the collection node begins.
///
/// The parser keeps the line break and indent before a nested collection's
/// first entry in the parent VALUE or SEQUENCE_ENTRY, so `start` comes after
/// them; later entries get an INDENT sibling inside the collection.
fn begin_entry(
    builder: &mut GreenNodeBuilder<'static>,
    indent: usize,
    start: &mut Option<Checkpoint>,
) {
    if start.is_some() {
        if indent > 0 {
            builder.token(SyntaxKind::INDENT.into(), &" ".repeat(indent));
        }
        return;
    }
    if indent > 0 {
        builder.token(SyntaxKind::NEWLINE.into(), "\n");
        builder.token(SyntaxKind::INDENT.into(), &" ".repeat(indent));
    }
    *start = Some(builder.checkpoint());
}

/// Wrap the entries emitted since `start` in a `kind` node, or emit an
/// empty flow collection if there were none: an empty block collection
/// serializes as nothing and reparses as null under a key (issue #37).
fn finish_collection(
    builder: &mut GreenNodeBuilder<'static>,
    kind: SyntaxKind,
    start: Option<Checkpoint>,
) {
    let Some(start) = start else {
        builder.start_node(kind.into());
        if kind == SyntaxKind::SEQUENCE {
            builder.token(SyntaxKind::LEFT_BRACKET.into(), "[");
            builder.token(SyntaxKind::RIGHT_BRACKET.into(), "]");
        } else {
            builder.token(SyntaxKind::LEFT_BRACE.into(), "{");
            builder.token(SyntaxKind::RIGHT_BRACE.into(), "}");
        }
        builder.finish_node();
        return;
    };
    builder.start_node_at(start, kind.into());
    builder.finish_node();
}

/// Finish a collection filled by a closure after its parent's `key:` or
/// `-`, terminating the parent entry. A non-empty one already sits on the
/// following lines; an empty one goes on the same line. In a mapping entry,
/// `value_start` is where the VALUE node begins, which takes in the line
/// break before a block collection.
fn finish_nested(
    builder: &mut GreenNodeBuilder<'static>,
    kind: SyntaxKind,
    start: Option<Checkpoint>,
    value_start: Option<Checkpoint>,
) {
    if start.is_some() {
        finish_collection(builder, kind, start);
        if let Some(value_start) = value_start {
            builder.start_node_at(value_start, SyntaxKind::VALUE.into());
            builder.finish_node();
        }
        builder.finish_node();
        return;
    }
    builder.token(SyntaxKind::WHITESPACE.into(), " ");
    if value_start.is_some() {
        builder.start_node(SyntaxKind::VALUE.into());
    }
    finish_collection(builder, kind, None);
    if value_start.is_some() {
        builder.finish_node();
    }
    finish_entry(builder, false);
}

/// Close the current entry, terminating its line unless its value did.
fn finish_entry(builder: &mut GreenNodeBuilder<'static>, ends_with_newline: bool) {
    if !ends_with_newline {
        builder.token(SyntaxKind::NEWLINE.into(), "\n");
    }
    builder.finish_node();
}

/// Finish a top-level collection and its document.
fn finish_top_level(
    mut builder: GreenNodeBuilder<'static>,
    kind: SyntaxKind,
    start: Option<Checkpoint>,
) -> YamlBuilder {
    let empty = start.is_none();
    finish_collection(&mut builder, kind, start);
    // Block entries end their own lines; `[]`/`{}` leaves it to the document.
    if empty {
        builder.token(SyntaxKind::NEWLINE.into(), "\n");
    }
    builder.finish_node(); // DOCUMENT
    builder.finish_node(); // ROOT
    YamlBuilder {
        file: YamlFile(rowan::SyntaxNode::new_root_mut(builder.finish())),
    }
}

/// The collection a builder's [`build`](MappingBuilder::build) produced.
fn root_node(built: YamlBuilder) -> YamlNode {
    built
        .build()
        .document()
        .and_then(|d| d.syntax().first_child())
        .and_then(YamlNode::from_syntax)
        .expect("builders always produce a document holding a collection")
}

/// Start a MAPPING_ENTRY and emit its `key:`, quoting the key as needed to
/// keep it a string.
fn start_mapping_entry(builder: &mut GreenNodeBuilder<'static>, key: &str) {
    builder.start_node(SyntaxKind::MAPPING_ENTRY.into());
    builder.start_node(SyntaxKind::KEY.into());
    crate::nodes::build_key_content(builder, &key, 0, None);
    builder.finish_node();
    builder.token(SyntaxKind::COLON.into(), ":");
}

/// Emit the children of a VALUE built by
/// [`build_block_value_node`](crate::nodes::mapping::build_block_value_node),
/// wrapped in VALUE again if `wrap`, after the space that separates them
/// from the `:` or `-` if they start on that line. Returns whether they end
/// with a newline.
fn emit_block_value(
    builder: &mut GreenNodeBuilder<'static>,
    source: &SyntaxNode,
    target: usize,
    wrap: bool,
) -> bool {
    let block = crate::nodes::mapping::build_block_value_node(source, target, None);
    if block.on_key_line {
        builder.token(SyntaxKind::WHITESPACE.into(), " ");
    }
    if wrap {
        crate::yaml::copy_node_to_builder(builder, &block.node);
    } else {
        crate::as_yaml::copy_node_content(builder, &block.node);
    }
    block.ends_with_newline
}

impl SequenceBuilder {
    /// Create a new empty sequence builder.
    pub fn new() -> Self {
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(SyntaxKind::ROOT.into());
        builder.start_node(SyntaxKind::DOCUMENT.into());
        Self::nested(builder, 0)
    }

    fn nested(builder: GreenNodeBuilder<'static>, indent: usize) -> Self {
        SequenceBuilder {
            builder,
            indent,
            start: None,
        }
    }

    /// Emit the next entry's leading whitespace and open it up to its `-`.
    fn start_entry(&mut self) {
        begin_entry(&mut self.builder, self.indent, &mut self.start);
        self.builder.start_node(SyntaxKind::SEQUENCE_ENTRY.into());
        self.builder.token(SyntaxKind::DASH.into(), "-");
    }

    /// Add a value to the sequence. Accepts any type implementing [`AsYaml`]:
    /// `&str`, `String`, `i64`, `bool`, `f64`, CST nodes, etc.
    pub fn item(mut self, value: impl AsYaml) -> Self {
        self.start_entry();
        let nested = self.indent + 2;
        let block_node = value.value_node().filter(|n| {
            !value.is_inline() && !matches!(n.kind(), SyntaxKind::MAPPING | SyntaxKind::SEQUENCE)
        });
        let ends_with_newline = match block_node {
            // Block scalars and tagged block content.
            Some(node) => emit_block_value(&mut self.builder, &node, nested, false),
            // A block collection starts on the dash line, its later lines
            // lined up under the first.
            None => {
                self.builder.token(SyntaxKind::WHITESPACE.into(), " ");
                let indent = if value.is_inline() {
                    self.indent
                } else {
                    nested
                };
                value.build_content(&mut self.builder, indent, false)
            }
        };
        finish_entry(&mut self.builder, ends_with_newline);
        self
    }

    /// Add a nested sequence to this sequence.
    pub fn sequence<F>(mut self, f: F) -> Self
    where
        F: FnOnce(SequenceBuilder) -> SequenceBuilder,
    {
        self.start_entry();
        let filled = f(SequenceBuilder::nested(self.builder, self.indent + 2));
        self.builder = filled.builder;
        finish_nested(&mut self.builder, SyntaxKind::SEQUENCE, filled.start, None);
        self
    }

    /// Add a nested mapping to this sequence.
    pub fn mapping<F>(mut self, f: F) -> Self
    where
        F: FnOnce(MappingBuilder) -> MappingBuilder,
    {
        self.start_entry();
        let filled = f(MappingBuilder::nested(self.builder, self.indent + 2));
        self.builder = filled.builder;
        finish_nested(&mut self.builder, SyntaxKind::MAPPING, filled.start, None);
        self
    }

    /// Insert a pre-built SequenceBuilder into this sequence.
    pub fn insert_sequence(self, other: SequenceBuilder) -> Self {
        self.insert_node(other.into_node())
    }

    /// Insert a pre-built MappingBuilder into this sequence.
    pub fn insert_mapping(self, other: MappingBuilder) -> Self {
        self.insert_node(other.into_node())
    }

    /// Add a collection from another builder, laid out the way the
    /// closure-based methods lay it out: on the lines after the `-`.
    fn insert_node(mut self, node: YamlNode) -> Self {
        if node.is_inline() {
            // `[]` or `{}`
            return self.item(node);
        }
        self.start_entry();
        let ends_with_newline =
            emit_block_value(&mut self.builder, node.syntax(), self.indent + 2, false);
        finish_entry(&mut self.builder, ends_with_newline);
        self
    }

    /// Finish this builder on its own and return the SEQUENCE node.
    fn into_node(self) -> YamlNode {
        root_node(self.build())
    }

    /// Build the sequence into a YamlBuilder.
    pub fn build(self) -> YamlBuilder {
        finish_top_level(self.builder, SyntaxKind::SEQUENCE, self.start)
    }

    /// Build the sequence directly into a Document.
    pub fn build_document(self) -> Document {
        self.build()
            .build()
            .document()
            .expect("YamlBuilder always produces a document node")
    }
}

impl Default for SequenceBuilder {
    fn default() -> Self {
        Self::new()
    }
}

/// Builder for YAML mappings.
pub struct MappingBuilder {
    builder: GreenNodeBuilder<'static>,
    /// Column of this mapping's keys; see [`SequenceBuilder`].
    indent: usize,
    /// Where the MAPPING node starts, set once the first entry is emitted.
    start: Option<Checkpoint>,
}

impl MappingBuilder {
    /// Create a new empty mapping builder.
    pub fn new() -> Self {
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(SyntaxKind::ROOT.into());
        builder.start_node(SyntaxKind::DOCUMENT.into());
        Self::nested(builder, 0)
    }

    fn nested(builder: GreenNodeBuilder<'static>, indent: usize) -> Self {
        MappingBuilder {
            builder,
            indent,
            start: None,
        }
    }

    /// Emit the next entry's leading whitespace and open it up to its `:`.
    fn start_entry(&mut self, key: &str) {
        begin_entry(&mut self.builder, self.indent, &mut self.start);
        start_mapping_entry(&mut self.builder, key);
    }

    /// Add a key-value pair. The value can be any type implementing [`AsYaml`]:
    /// `&str`, `String`, `i64`, `bool`, `f64`, CST nodes, etc.
    ///
    /// The key is always a string: it is quoted if it would otherwise read
    /// as another type or not parse as a key at all.
    pub fn pair(mut self, key: impl Into<String>, value: impl AsYaml) -> Self {
        self.start_entry(&key.into());
        let nested = self.indent + 2;
        let ends_with_newline = match value.value_node().filter(|_| !value.is_inline()) {
            Some(node) => emit_block_value(&mut self.builder, &node, nested, true),
            None if value.is_inline() => {
                self.builder.token(SyntaxKind::WHITESPACE.into(), " ");
                self.builder.start_node(SyntaxKind::VALUE.into());
                let ends = value.build_content(&mut self.builder, self.indent, false);
                self.builder.finish_node();
                ends
            }
            None => {
                self.builder.start_node(SyntaxKind::VALUE.into());
                self.builder.token(SyntaxKind::NEWLINE.into(), "\n");
                self.builder
                    .token(SyntaxKind::INDENT.into(), &" ".repeat(nested));
                let ends = value.build_content(&mut self.builder, nested, false);
                self.builder.finish_node();
                ends
            }
        };
        finish_entry(&mut self.builder, ends_with_newline);
        self
    }

    /// Add a key-value pair with a sequence value.
    pub fn sequence<F>(mut self, key: impl Into<String>, f: F) -> Self
    where
        F: FnOnce(SequenceBuilder) -> SequenceBuilder,
    {
        self.start_entry(&key.into());
        let value_start = self.builder.checkpoint();
        let filled = f(SequenceBuilder::nested(self.builder, self.indent + 2));
        self.builder = filled.builder;
        finish_nested(
            &mut self.builder,
            SyntaxKind::SEQUENCE,
            filled.start,
            Some(value_start),
        );
        self
    }

    /// Add a key-value pair with a mapping value.
    pub fn mapping<F>(mut self, key: impl Into<String>, f: F) -> Self
    where
        F: FnOnce(MappingBuilder) -> MappingBuilder,
    {
        self.start_entry(&key.into());
        let value_start = self.builder.checkpoint();
        let filled = f(MappingBuilder::nested(self.builder, self.indent + 2));
        self.builder = filled.builder;
        finish_nested(
            &mut self.builder,
            SyntaxKind::MAPPING,
            filled.start,
            Some(value_start),
        );
        self
    }

    /// Insert a key-value pair with a pre-built SequenceBuilder.
    pub fn insert_sequence(self, key: impl Into<String>, other: SequenceBuilder) -> Self {
        self.pair(key, other.into_node())
    }

    /// Insert a key-value pair with a pre-built MappingBuilder.
    pub fn insert_mapping(self, key: impl Into<String>, other: MappingBuilder) -> Self {
        self.pair(key, other.into_node())
    }

    /// Finish this builder on its own and return the MAPPING node.
    fn into_node(self) -> YamlNode {
        root_node(self.build())
    }

    /// Build the mapping into a YamlBuilder.
    pub fn build(self) -> YamlBuilder {
        finish_top_level(self.builder, SyntaxKind::MAPPING, self.start)
    }

    /// Build the mapping directly into a Document.
    pub fn build_document(self) -> Document {
        self.build()
            .build()
            .document()
            .expect("YamlBuilder always produces a document node")
    }
}

impl Default for MappingBuilder {
    fn default() -> Self {
        Self::new()
    }
}
