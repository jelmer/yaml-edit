//! Builder pattern for constructing YAML documents fluently.

use crate::{
    as_yaml::{AsYaml, YamlKind},
    lex::SyntaxKind,
    yaml::{Document, YamlFile},
};
use rowan::GreenNodeBuilder;

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
    indent: usize,
    count: usize,
    /// Whether the output so far ends with a newline
    ends_with_newline: bool,
    /// Whether the first item has to start on a new line
    break_first: bool,
}

/// Emit the `\n` and indent that start a block collection item, if needed.
fn emit_line_start(builder: &mut GreenNodeBuilder<'static>, newline_first: bool, indent: usize) {
    if newline_first {
        builder.token(SyntaxKind::NEWLINE.into(), "\n");
    }
    if indent > 0 {
        builder.token(SyntaxKind::WHITESPACE.into(), &" ".repeat(indent));
    }
}

/// Emit the `\n`, indent and `-` that introduce a block sequence item.
fn emit_dash(builder: &mut GreenNodeBuilder<'static>, newline_first: bool, indent: usize) {
    emit_line_start(builder, newline_first, indent);
    builder.token(SyntaxKind::DASH.into(), "-");
}

/// Emit the `\n`, indent and `key:` that introduce a block mapping entry.
fn emit_key(
    builder: &mut GreenNodeBuilder<'static>,
    newline_first: bool,
    indent: usize,
    key: &str,
) {
    emit_line_start(builder, newline_first, indent);
    builder.start_node(SyntaxKind::SCALAR.into());
    builder.token(SyntaxKind::VALUE.into(), key);
    builder.finish_node();
    builder.token(SyntaxKind::COLON.into(), ":");
}

/// Emit an empty flow collection (` []` or ` {}`) after a `-` or `key:`.
fn emit_empty_flow(builder: &mut GreenNodeBuilder<'static>, kind: SyntaxKind) {
    builder.token(SyntaxKind::WHITESPACE.into(), " ");
    builder.start_node(kind.into());
    emit_empty_flow_brackets(builder, kind);
    builder.finish_node();
}

/// An empty collection without `[]`/`{}` serializes as nothing and reparses
/// as null when placed under a key (issue #37).
fn emit_empty_flow_brackets(builder: &mut GreenNodeBuilder<'static>, kind: SyntaxKind) {
    if kind == SyntaxKind::SEQUENCE {
        builder.token(SyntaxKind::LEFT_BRACKET.into(), "[");
        builder.token(SyntaxKind::RIGHT_BRACKET.into(), "]");
    } else {
        builder.token(SyntaxKind::LEFT_BRACE.into(), "{");
        builder.token(SyntaxKind::RIGHT_BRACE.into(), "}");
    }
}

/// Emit a pre-built collection after a `-` or `key:`, on the following lines
/// at `indent + 2`, or as `[]`/`{}` when it is empty. Returns whether the
/// output now ends with a newline.
fn emit_inserted(
    builder: &mut GreenNodeBuilder<'static>,
    node: &rowan::SyntaxNode<crate::yaml::Lang>,
    indent: usize,
) -> bool {
    if node.children_with_tokens().next().is_none() {
        emit_empty_flow(builder, node.kind());
        return false;
    }
    builder.token(SyntaxKind::NEWLINE.into(), "\n");
    copy_nested_at(builder, node, indent)
}

/// Copy a detached collection's content in at `indent + 2`.
///
/// The caller has just emitted the newline that ends the `-` or `key:`
/// introducer, so the copy starts at the beginning of a line: the first
/// entry needs its indent written out here, and
/// [`copy_node_content_reindent`](crate::as_yaml::copy_node_content_reindent)
/// shifts every later line by the same amount.
///
/// # Why copying is allowed here
///
/// "Prefer targeted splices over whole-node rebuilds" in
/// [`crate::nodes`] forbids reconstructing a collection, because doing so
/// discards anchors, tags, comments and quoting the rebuild didn't know
/// about. That reasoning needs an existing tree to damage, and there
/// isn't one on this path: `node` comes from
/// [`finish_into_inner_node`], which closes out a
/// [`GreenNodeBuilder`] that the caller filled token by token. It has
/// never been parsed from source, holds nothing a user chose, and the
/// `insert_*` methods take their donor by value, so no one else can
/// observe it.
///
/// `node` must therefore be builder-owned. Never hand this a node
/// reached from a parsed [`YamlFile`] - re-indenting such a node would
/// rewrite formatting the user wrote. Splice it instead.
fn copy_nested_at(
    builder: &mut GreenNodeBuilder<'static>,
    node: &rowan::SyntaxNode<crate::yaml::Lang>,
    indent: usize,
) -> bool {
    let nested = indent + 2;
    builder.token(SyntaxKind::WHITESPACE.into(), &" ".repeat(nested));
    crate::as_yaml::copy_node_content_reindent(builder, node, nested as isize)
}

/// Close out a detached builder's SEQUENCE/MAPPING, DOCUMENT and ROOT nodes
/// and hand back the collection node itself, ready to copy into a parent.
///
/// The result is a throwaway tree built from `builder`'s own tokens, not
/// part of any document, which is what lets [`copy_nested_at`] re-indent
/// it wholesale.
fn finish_into_inner_node(
    mut builder: GreenNodeBuilder<'static>,
) -> Option<rowan::SyntaxNode<crate::yaml::Lang>> {
    use rowan::ast::AstNode;
    builder.finish_node(); // SEQUENCE or MAPPING
    builder.finish_node(); // DOCUMENT
    builder.finish_node(); // ROOT
    let root = rowan::SyntaxNode::<crate::yaml::Lang>::new_root(builder.finish());
    let doc = Document::cast(root.first_child()?)?;
    doc.syntax().children().next()
}

/// Close a nested collection whose content was emitted after `checkpoint`,
/// writing `[]`/`{}` instead when it got no entries. Returns whether the
/// output now ends with a newline.
fn finish_nested(
    builder: &mut GreenNodeBuilder<'static>,
    checkpoint: rowan::Checkpoint,
    kind: SyntaxKind,
    count: usize,
    ends_with_newline: bool,
) -> bool {
    if count == 0 {
        emit_empty_flow(builder, kind);
        return false;
    }
    builder.start_node_at(checkpoint, kind.into());
    builder.finish_node();
    ends_with_newline
}

/// Finish a top-level collection and its document. The output always ends
/// with a newline, which a trailing block scalar needs to keep its last
/// line break. The newline goes after the collection node, so the
/// collection can still be inserted elsewhere as is.
fn finish_top_level(
    mut builder: GreenNodeBuilder<'static>,
    kind: SyntaxKind,
    count: usize,
    ends_with_newline: bool,
) -> YamlBuilder {
    if count == 0 {
        emit_empty_flow_brackets(&mut builder, kind);
    }
    builder.finish_node(); // SEQUENCE or MAPPING
    if !ends_with_newline {
        builder.token(SyntaxKind::NEWLINE.into(), "\n");
    }
    builder.finish_node(); // DOCUMENT
    builder.finish_node(); // ROOT
    YamlBuilder {
        file: YamlFile(rowan::SyntaxNode::new_root_mut(builder.finish())),
    }
}

impl SequenceBuilder {
    /// Create a new empty sequence builder.
    pub fn new() -> Self {
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(SyntaxKind::ROOT.into());
        builder.start_node(SyntaxKind::DOCUMENT.into());
        builder.start_node(SyntaxKind::SEQUENCE.into());
        SequenceBuilder {
            builder,
            indent: 0,
            count: 0,
            ends_with_newline: false,
            break_first: false,
        }
    }

    fn nested(builder: GreenNodeBuilder<'static>, indent: usize) -> Self {
        SequenceBuilder {
            builder,
            indent,
            count: 0,
            ends_with_newline: false,
            break_first: true,
        }
    }

    fn emit_item_preamble(&mut self) {
        let newline_first = if self.count == 0 {
            self.break_first
        } else {
            !self.ends_with_newline
        };
        emit_dash(&mut self.builder, newline_first, self.indent);
    }

    /// Add a value to the sequence. Accepts any type implementing [`AsYaml`]:
    /// `&str`, `String`, `i64`, `bool`, `f64`, CST nodes, etc.
    pub fn item(mut self, value: impl AsYaml) -> Self {
        self.emit_item_preamble();

        // Check the kind of value to determine formatting
        let ends_with_newline = match (value.is_inline(), value.kind()) {
            // Inline values (scalars, flow collections) go on same line
            (true, _) => {
                self.builder.token(SyntaxKind::WHITESPACE.into(), " ");
                value.build_content(&mut self.builder, self.indent, false)
            }
            // Block mappings and sequences start on same line as dash
            // Their content will handle indentation via copy_node_content_with_indent
            (false, YamlKind::Mapping | YamlKind::Sequence) => {
                self.builder.token(SyntaxKind::WHITESPACE.into(), " ");
                value.build_content(&mut self.builder, self.indent + 2, false)
            }
            // Block scalars (literal/folded) need newline before them
            (false, _) => {
                self.builder.token(SyntaxKind::NEWLINE.into(), "\n");
                value.build_content(&mut self.builder, self.indent + 2, false)
            }
        };

        self.count += 1;
        self.ends_with_newline = ends_with_newline;
        self
    }

    /// Add a nested sequence to this sequence.
    pub fn sequence<F>(mut self, f: F) -> Self
    where
        F: FnOnce(SequenceBuilder) -> SequenceBuilder,
    {
        self.emit_item_preamble();
        let checkpoint = self.builder.checkpoint();
        let filled = f(SequenceBuilder::nested(self.builder, self.indent + 2));
        let SequenceBuilder {
            mut builder,
            count,
            ends_with_newline,
            ..
        } = filled;
        self.ends_with_newline = finish_nested(
            &mut builder,
            checkpoint,
            SyntaxKind::SEQUENCE,
            count,
            ends_with_newline,
        );
        self.builder = builder;
        self.count += 1;
        self
    }

    /// Add a nested mapping to this sequence.
    pub fn mapping<F>(mut self, f: F) -> Self
    where
        F: FnOnce(MappingBuilder) -> MappingBuilder,
    {
        self.emit_item_preamble();
        let checkpoint = self.builder.checkpoint();
        let filled = f(MappingBuilder::nested(self.builder, self.indent + 2));
        let MappingBuilder {
            mut builder,
            count,
            ends_with_newline,
            ..
        } = filled;
        self.ends_with_newline = finish_nested(
            &mut builder,
            checkpoint,
            SyntaxKind::MAPPING,
            count,
            ends_with_newline,
        );
        self.builder = builder;
        self.count += 1;
        self
    }

    /// Insert a pre-built SequenceBuilder into this sequence.
    pub fn insert_sequence(self, other: SequenceBuilder) -> Self {
        let node = finish_into_inner_node(other.builder);
        self.insert_node(node)
    }

    /// Insert a pre-built MappingBuilder into this sequence.
    pub fn insert_mapping(self, other: MappingBuilder) -> Self {
        let node = finish_into_inner_node(other.builder);
        self.insert_node(node)
    }

    fn insert_node(mut self, node: Option<rowan::SyntaxNode<crate::yaml::Lang>>) -> Self {
        let Some(node) = node else {
            return self;
        };
        self.emit_item_preamble();
        self.ends_with_newline = emit_inserted(&mut self.builder, &node, self.indent);
        self.count += 1;
        self
    }

    /// Build the sequence into a YamlBuilder.
    pub fn build(self) -> YamlBuilder {
        finish_top_level(
            self.builder,
            SyntaxKind::SEQUENCE,
            self.count,
            self.ends_with_newline,
        )
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
    indent: usize,
    count: usize,
    /// Whether the output so far ends with a newline
    ends_with_newline: bool,
    /// Whether the first entry has to start on a new line
    break_first: bool,
}

impl MappingBuilder {
    /// Create a new empty mapping builder.
    pub fn new() -> Self {
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(SyntaxKind::ROOT.into());
        builder.start_node(SyntaxKind::DOCUMENT.into());
        builder.start_node(SyntaxKind::MAPPING.into());
        MappingBuilder {
            builder,
            indent: 0,
            count: 0,
            ends_with_newline: false,
            break_first: false,
        }
    }

    fn nested(builder: GreenNodeBuilder<'static>, indent: usize) -> Self {
        MappingBuilder {
            builder,
            indent,
            count: 0,
            ends_with_newline: false,
            break_first: true,
        }
    }

    fn emit_key_preamble(&mut self, key: &str) {
        let newline_first = if self.count == 0 {
            self.break_first
        } else {
            !self.ends_with_newline
        };
        emit_key(&mut self.builder, newline_first, self.indent, key);
    }

    /// Add a key-value pair. The value can be any type implementing [`AsYaml`]:
    /// `&str`, `String`, `i64`, `bool`, `f64`, CST nodes, etc.
    pub fn pair(mut self, key: impl Into<String>, value: impl AsYaml) -> Self {
        self.emit_key_preamble(&key.into());
        self.ends_with_newline = if value.is_inline() {
            self.builder.token(SyntaxKind::WHITESPACE.into(), " ");
            value.build_content(&mut self.builder, self.indent, false)
        } else {
            self.builder.token(SyntaxKind::NEWLINE.into(), "\n");
            value.build_content(&mut self.builder, self.indent + 2, false)
        };
        self.count += 1;
        self
    }

    /// Add a key-value pair with a sequence value.
    pub fn sequence<F>(mut self, key: impl Into<String>, f: F) -> Self
    where
        F: FnOnce(SequenceBuilder) -> SequenceBuilder,
    {
        self.emit_key_preamble(&key.into());
        let checkpoint = self.builder.checkpoint();
        let filled = f(SequenceBuilder::nested(self.builder, self.indent + 2));
        let SequenceBuilder {
            mut builder,
            count,
            ends_with_newline,
            ..
        } = filled;
        self.ends_with_newline = finish_nested(
            &mut builder,
            checkpoint,
            SyntaxKind::SEQUENCE,
            count,
            ends_with_newline,
        );
        self.builder = builder;
        self.count += 1;
        self
    }

    /// Add a key-value pair with a mapping value.
    pub fn mapping<F>(mut self, key: impl Into<String>, f: F) -> Self
    where
        F: FnOnce(MappingBuilder) -> MappingBuilder,
    {
        self.emit_key_preamble(&key.into());
        let checkpoint = self.builder.checkpoint();
        let filled = f(MappingBuilder::nested(self.builder, self.indent + 2));
        let MappingBuilder {
            mut builder,
            count,
            ends_with_newline,
            ..
        } = filled;
        self.ends_with_newline = finish_nested(
            &mut builder,
            checkpoint,
            SyntaxKind::MAPPING,
            count,
            ends_with_newline,
        );
        self.builder = builder;
        self.count += 1;
        self
    }

    /// Insert a key-value pair with a pre-built SequenceBuilder.
    pub fn insert_sequence(self, key: impl Into<String>, other: SequenceBuilder) -> Self {
        let node = finish_into_inner_node(other.builder);
        self.insert_node(key.into(), node)
    }

    /// Insert a key-value pair with a pre-built MappingBuilder.
    pub fn insert_mapping(self, key: impl Into<String>, other: MappingBuilder) -> Self {
        let node = finish_into_inner_node(other.builder);
        self.insert_node(key.into(), node)
    }

    fn insert_node(
        mut self,
        key: String,
        node: Option<rowan::SyntaxNode<crate::yaml::Lang>>,
    ) -> Self {
        let Some(node) = node else {
            return self;
        };
        self.emit_key_preamble(&key);
        self.ends_with_newline = emit_inserted(&mut self.builder, &node, self.indent);
        self.count += 1;
        self
    }

    /// Build the mapping into a YamlBuilder.
    pub fn build(self) -> YamlBuilder {
        finish_top_level(
            self.builder,
            SyntaxKind::MAPPING,
            self.count,
            self.ends_with_newline,
        )
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::str::FromStr;

    #[test]
    fn test_scalar_builder() {
        let yaml = YamlBuilder::scalar("hello world").build();
        assert_eq!(yaml.to_string(), "hello world\n");
    }

    #[test]
    fn test_alias_builder() {
        let yaml = YamlBuilder::alias("shared").build();
        assert_eq!(yaml.to_string(), "*shared\n");
    }

    #[test]
    fn test_mapping_builder_alias_value() {
        let yaml = YamlBuilder::mapping()
            .pair("ref", crate::yaml::Alias::new("shared"))
            .build()
            .build();
        assert_eq!(yaml.to_string(), "ref: *shared\n");
    }

    #[test]
    fn test_sequence_builder() {
        let yaml = YamlBuilder::sequence()
            .item("first")
            .item("second")
            .item("third")
            .build()
            .build();
        assert_eq!(yaml.to_string(), "- first\n- second\n- third\n");
    }

    #[test]
    fn test_mapping_builder() {
        let yaml = YamlBuilder::mapping()
            .pair("name", "John Doe")
            .pair("age", "30")
            .pair("city", "New York")
            .build()
            .build();
        assert_eq!(
            yaml.to_string(),
            "name: John Doe\nage: '30'\ncity: New York\n"
        );
    }

    #[test]
    fn test_nested_structure() {
        let yaml = YamlBuilder::mapping()
            .pair("version", "1.0")
            .sequence("dependencies", |s| {
                s.item("serde").item("tokio").item("reqwest")
            })
            .mapping("database", |m| {
                m.pair("host", "localhost")
                    .pair("port", "5432")
                    .pair("name", "myapp")
            })
            .build()
            .build();
        assert_eq!(
            yaml.to_string(),
            "version: '1.0'\ndependencies:\n  - serde\n  - tokio\n  - reqwest\ndatabase:\n  host: localhost\n  port: '5432'\n  name: myapp\n"
        );
    }

    #[test]
    fn test_deeply_nested() {
        let yaml = YamlBuilder::mapping()
            .mapping("level1", |m| {
                m.mapping("level2", |m| {
                    m.mapping("level3", |m| m.pair("deep", "value"))
                })
            })
            .build()
            .build();
        assert_eq!(
            yaml.to_string(),
            "level1:\n  level2:\n    level3:\n      deep: value\n"
        );
    }

    #[test]
    fn test_empty_collections() {
        // Empty sequence and mapping serialize to the flow-empty form so
        // that placing them under a key doesn't reparse as null (issue #37).
        let empty_seq = YamlBuilder::sequence().build().build();
        assert_eq!(empty_seq.to_string().trim(), "[]");

        let empty_map = YamlBuilder::mapping().build().build();
        assert_eq!(empty_map.to_string().trim(), "{}");
    }

    #[test]
    fn test_special_characters_in_values() {
        let yaml = YamlBuilder::mapping()
            .pair("url", "https://example.com:8080/path?query=value")
            .pair("email", "user@example.com")
            .pair("path", "/usr/local/bin")
            .pair("special", "value: with: colons")
            .build()
            .build();
        assert_eq!(
            yaml.to_string(),
            "url: https://example.com:8080/path?query=value\nemail: user@example.com\npath: /usr/local/bin\nspecial: 'value: with: colons'\n"
        );
    }

    #[test]
    fn test_numeric_string_values() {
        let yaml = YamlBuilder::mapping()
            .pair("int_string", "42")
            .pair("float_string", "3.14")
            .pair("hex_string", "0xFF")
            .pair("octal_string", "0o755")
            .pair("binary_string", "0b1010")
            .build()
            .build();
        assert_eq!(
            yaml.to_string(),
            "int_string: '42'\nfloat_string: '3.14'\nhex_string: '0xFF'\noctal_string: '0o755'\nbinary_string: '0b1010'\n"
        );
    }

    #[test]
    fn test_sequences_with_nested_mappings() {
        let yaml = YamlBuilder::sequence()
            .mapping(|m| m.pair("id", "1").pair("name", "Alice"))
            .mapping(|m| m.pair("id", "2").pair("name", "Bob"))
            .mapping(|m| m.pair("id", "3").pair("name", "Charlie"))
            .build()
            .build();
        assert_eq!(
            yaml.to_string(),
            "-\n  id: '1'\n  name: Alice\n-\n  id: '2'\n  name: Bob\n-\n  id: '3'\n  name: Charlie\n"
        );
    }

    #[test]
    fn test_sequences_with_nested_sequences() {
        let yaml = YamlBuilder::sequence()
            .sequence(|s| s.item("1").item("2").item("3"))
            .sequence(|s| s.item("a").item("b").item("c"))
            .sequence(|s| s.item("x").item("y").item("z"))
            .build()
            .build();
        assert_eq!(
            yaml.to_string(),
            "-\n  - '1'\n  - '2'\n  - '3'\n-\n  - a\n  - b\n  - c\n-\n  - x\n  - y\n  - z\n"
        );
    }

    #[test]
    fn test_mixed_nesting_depth() {
        let yaml = YamlBuilder::mapping()
            .sequence("list", |s| {
                s.item("simple")
                    .mapping(|m| m.pair("key", "value"))
                    .sequence(|s2| s2.item("nested1").item("nested2"))
            })
            .mapping("object", |m| {
                m.pair("simple", "value")
                    .sequence("list", |s| s.item("item1").item("item2"))
                    .mapping("nested", |m2| m2.pair("deep", "value"))
            })
            .build()
            .build();
        assert_eq!(
            yaml.to_string(),
            "list:\n  - simple\n  -\n    key: value\n  -\n    - nested1\n    - nested2\nobject:\n  simple: value\n  list:\n    - item1\n    - item2\n  nested:\n    deep: value\n"
        );
    }

    #[test]
    fn test_boolean_and_null_strings() {
        let yaml = YamlBuilder::mapping()
            .pair("bool_true", "true")
            .pair("bool_false", "false")
            .pair("yes", "yes")
            .pair("no", "no")
            .pair("null_value", "null")
            .pair("tilde", "~")
            .build()
            .build();
        assert_eq!(
            yaml.to_string(),
            "bool_true: 'true'\nbool_false: 'false'\nyes: 'yes'\nno: 'no'\nnull_value: 'null'\ntilde: '~'\n"
        );
    }

    #[test]
    fn test_long_strings() {
        let yaml = YamlBuilder::mapping()
            .pair("short", "test")
            .pair("long", "a".repeat(100))
            .build()
            .build();
        assert_eq!(
            yaml.to_string(),
            format!("short: test\nlong: {}\n", "a".repeat(100))
        );
    }

    #[test]
    fn test_unicode_values() {
        let yaml = YamlBuilder::mapping()
            .pair("emoji", "🎉🚀💻")
            .pair("chinese", "你好世界")
            .pair("arabic", "مرحبا بالعالم")
            .pair("mixed", "Hello 世界 🌍")
            .build()
            .build();
        assert_eq!(
            yaml.to_string(),
            "emoji: 🎉🚀💻\nchinese: 你好世界\narabic: مرحبا بالعالم\nmixed: Hello 世界 🌍\n"
        );
    }

    #[test]
    fn test_build_document_convenience_sequence() {
        let doc = YamlBuilder::sequence()
            .item("first")
            .item("second")
            .build_document();

        let text = doc.to_string();
        assert_eq!(text.trim(), "- first\n- second");
    }

    #[test]
    fn test_build_document_convenience_mapping() {
        let doc = YamlBuilder::mapping()
            .pair("name", "test")
            .pair("version", "1.0")
            .build_document();

        let text = doc.to_string();
        assert_eq!(text.trim(), "name: test\nversion: '1.0'");
    }

    #[test]
    fn test_insert_pre_built_sequence() {
        let doc = YamlBuilder::mapping()
            .pair("name", "my-app")
            .sequence("dependencies", |s| s.item("serde").item("tokio"))
            .build_document();

        let text = doc.to_string();
        assert_eq!(
            text.trim(),
            "name: my-app\ndependencies:\n  - serde\n  - tokio"
        );
    }

    #[test]
    fn test_insert_pre_built_mapping() {
        let doc = YamlBuilder::mapping()
            .pair("name", "my-app")
            .mapping("database", |m| {
                m.pair("host", "localhost").pair("port", 5432)
            })
            .build_document();

        let text = doc.to_string();
        assert_eq!(
            text.trim(),
            "name: my-app\ndatabase:\n  host: localhost\n  port: 5432"
        );
    }

    #[test]
    fn test_insert_in_sequence() {
        let doc = YamlBuilder::sequence()
            .item("first")
            .sequence(|s| s.item("a").item("b"))
            .mapping(|m| m.pair("key", "value"))
            .build_document();

        let text = doc.to_string();
        assert_eq!(text.trim(), "- first\n-\n  - a\n  - b\n-\n  key: value");
    }

    #[test]
    fn test_insert_sequence_indents_like_closure() {
        let inserted = YamlBuilder::mapping()
            .pair("name", "my-app")
            .insert_sequence(
                "dependencies",
                SequenceBuilder::new().item("serde").item("tokio"),
            )
            .build_document()
            .to_string();
        let closure = YamlBuilder::mapping()
            .pair("name", "my-app")
            .sequence("dependencies", |s| s.item("serde").item("tokio"))
            .build_document()
            .to_string();

        assert_eq!(
            inserted.trim(),
            "name: my-app\ndependencies:\n  - serde\n  - tokio"
        );
        assert_eq!(inserted, closure);
    }

    #[test]
    fn test_insert_mapping_indents_like_closure() {
        let inserted = YamlBuilder::mapping()
            .pair("name", "my-app")
            .insert_mapping(
                "database",
                MappingBuilder::new()
                    .pair("host", "localhost")
                    .pair("port", 5432),
            )
            .build_document()
            .to_string();
        let closure = YamlBuilder::mapping()
            .pair("name", "my-app")
            .mapping("database", |m| {
                m.pair("host", "localhost").pair("port", 5432)
            })
            .build_document()
            .to_string();

        assert_eq!(
            inserted.trim(),
            "name: my-app\ndatabase:\n  host: localhost\n  port: 5432"
        );
        assert_eq!(inserted, closure);
    }

    #[test]
    fn test_insert_into_sequence_indents_like_closure() {
        let inserted = YamlBuilder::sequence()
            .item("first")
            .insert_sequence(SequenceBuilder::new().item("a").item("b"))
            .insert_mapping(MappingBuilder::new().pair("key", "value"))
            .build_document()
            .to_string();
        let closure = YamlBuilder::sequence()
            .item("first")
            .sequence(|s| s.item("a").item("b"))
            .mapping(|m| m.pair("key", "value"))
            .build_document()
            .to_string();

        assert_eq!(inserted.trim(), "- first\n-\n  - a\n  - b\n-\n  key: value");
        assert_eq!(inserted, closure);
    }

    #[test]
    fn test_insert_mapping_with_nested_collection() {
        // The donor's own nested content is already indented relative to the
        // donor; re-indenting must shift it as a block, not flatten it.
        let doc = YamlBuilder::mapping()
            .insert_mapping(
                "outer",
                MappingBuilder::new()
                    .pair("x", "1")
                    .mapping("deep", |m| m.pair("p", "9")),
            )
            .build_document();

        assert_eq!(
            doc.to_string().trim(),
            "outer:\n  x: '1'\n  deep:\n    p: '9'"
        );
    }

    #[test]
    fn test_complex_pre_built_structure() {
        let doc = YamlBuilder::mapping()
            .pair("version", "1.0")
            .pair("name", "my-application")
            .mapping("database", |m| {
                m.pair("host", "localhost")
                    .pair("port", 5432)
                    .pair("name", "myapp")
            })
            .sequence("dependencies", |s| {
                s.item("serde").item("tokio").item("reqwest")
            })
            .build_document();

        let text = doc.to_string();
        assert_eq!(
            text.trim(),
            "version: '1.0'\nname: my-application\ndatabase:\n  host: localhost\n  port: 5432\n  name: myapp\ndependencies:\n  - serde\n  - tokio\n  - reqwest"
        );
    }

    #[test]
    fn test_pair_with_typed_values() {
        let yaml = YamlBuilder::mapping()
            .pair("port", 5432_i64)
            .pair("debug", true)
            .pair("ratio", 1.5_f64)
            .build()
            .build();
        assert_eq!(yaml.to_string(), "port: 5432\ndebug: true\nratio: 1.5\n");
    }

    // Tests from sequence_builder_mapping_formatting.rs

    #[test]
    fn test_sequence_builder_with_block_mappings() {
        use crate::Document;
        use std::str::FromStr;

        // Parse a YAML document with duplicate keys (each has a mapping value)
        let yaml = r#"
Reference:
  Author: Stefan Kurze
  Title: Wörterbücher und Textdateien durchsuchen mit grafischem Frontend
  Journal: LinuxUser
  Year: 2003
Reference:
  Author: Michael Vogelbacher
  Title: Service und Informationen aus dem Netz
  Journal: LinuxUser
  Year: 2001
"#;

        let doc = Document::from_str(yaml).unwrap();
        let mapping = doc.as_mapping().unwrap();

        // Collect the duplicate Reference values
        let mut reference_values = Vec::new();
        for (key, value) in &mapping {
            if let Some(key_scalar) = key.as_scalar() {
                if key_scalar.as_string() == "Reference" {
                    reference_values.push(value);
                }
            }
        }

        // Remove all Reference keys
        while mapping.remove("Reference").is_some() {}

        // Create a sequence from the collected values
        let mut seq_builder = SequenceBuilder::new();
        for value in &reference_values {
            seq_builder = seq_builder.item(value);
        }
        let seq_doc = seq_builder.build_document();

        // Set the sequence back
        if let Some(seq) = seq_doc.as_sequence() {
            mapping.set("Reference", seq);
        }

        let result = doc.to_string();

        // Expected format: each mapping item should start with dash at base indent
        // and mapping content should be properly indented
        let expected = r#"Reference:
- Author: Stefan Kurze
  Title: Wörterbücher und Textdateien durchsuchen mit grafischem Frontend
  Journal: LinuxUser
  Year: 2003
- Author: Michael Vogelbacher
  Title: Service und Informationen aus dem Netz
  Journal: LinuxUser
  Year: 2001
"#;

        assert_eq!(result.trim(), expected.trim());
    }

    #[test]
    fn test_sequence_builder_simple_mapping() {
        use crate::Document;
        use std::str::FromStr;

        let yaml = r#"
item:
  key: value
  foo: bar
"#;

        let doc = Document::from_str(yaml).unwrap();
        let mapping = doc.as_mapping().unwrap();
        let item_value = mapping.get("item").unwrap();
        let item_mapping = item_value.as_mapping().unwrap();

        // Create a sequence with this mapping
        let seq = SequenceBuilder::new().item(item_mapping).build_document();

        let result = seq.to_string();

        // Should format as:
        // - key: value
        //   foo: bar
        let expected = "- key: value\n  foo: bar";
        assert_eq!(result.trim(), expected);
    }

    #[test]
    fn test_nested_empty_collections() {
        let yaml = YamlBuilder::mapping()
            .mapping("empty", |m| m)
            .sequence("es", |s| s)
            .build()
            .build();
        assert_eq!(yaml.to_string(), "empty: {}\nes: []\n");

        let yaml = YamlBuilder::sequence()
            .sequence(|s| s)
            .mapping(|m| m)
            .build()
            .build();
        assert_eq!(yaml.to_string(), "- []\n- {}\n");
    }

    #[test]
    fn test_insert_empty_collections() {
        let yaml = YamlBuilder::mapping()
            .insert_mapping("m", MappingBuilder::new())
            .insert_sequence("s", SequenceBuilder::new())
            .build()
            .build();
        assert_eq!(yaml.to_string(), "m: {}\ns: []\n");

        let yaml = YamlBuilder::sequence()
            .insert_sequence(SequenceBuilder::new())
            .insert_mapping(MappingBuilder::new())
            .build()
            .build();
        assert_eq!(yaml.to_string(), "- []\n- {}\n");
    }

    #[test]
    fn test_trailing_block_scalar_keeps_line_break() {
        let yaml = YamlBuilder::mapping().pair("k", "a\n").build().build();
        assert_eq!(yaml.to_string(), "k: |\n  a\n");
        let doc = Document::from_str(&yaml.to_string()).unwrap();
        assert_eq!(
            doc.get("k").unwrap().as_scalar().unwrap().as_string(),
            "a\n"
        );

        let yaml = YamlBuilder::sequence().item("a\n\n").build().build();
        assert_eq!(yaml.to_string(), "- |+\n  a\n\n");
        let doc = Document::from_str(&yaml.to_string()).unwrap();
        let seq = doc.as_sequence().unwrap();
        assert_eq!(
            seq.get(0).unwrap().as_scalar().unwrap().as_string(),
            "a\n\n"
        );
    }

    #[test]
    fn test_item_after_nested_collection() {
        let yaml = YamlBuilder::sequence()
            .sequence(|s| s.item(1))
            .item("x")
            .build()
            .build();
        assert_eq!(yaml.to_string(), "-\n  - 1\n- x\n");
    }
}
