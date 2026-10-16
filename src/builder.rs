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
    builder.token(SyntaxKind::NEWLINE.into(), "\n");
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

/// Start a MAPPING_ENTRY and emit its `key:`.
fn start_mapping_entry(builder: &mut GreenNodeBuilder<'static>, key: &str) {
    builder.start_node(SyntaxKind::MAPPING_ENTRY.into());
    builder.start_node(SyntaxKind::KEY.into());
    builder.start_node(SyntaxKind::SCALAR.into());
    builder.token(SyntaxKind::STRING.into(), key);
    builder.finish_node();
    builder.finish_node();
    builder.token(SyntaxKind::COLON.into(), ":");
}

/// Emit the children of a VALUE built by
/// [`build_block_value_node`](crate::nodes::mapping::build_block_value_node),
/// wrapped in VALUE again if `wrap`. Returns whether they end with a newline.
fn emit_block_value(
    builder: &mut GreenNodeBuilder<'static>,
    source: &SyntaxNode,
    target: usize,
    wrap: bool,
) -> bool {
    let (value, ends_with_newline) = crate::nodes::mapping::build_block_value_node(source, target);
    if wrap {
        crate::yaml::copy_node_to_builder(builder, &value);
    } else {
        crate::yaml::add_node_children_to(builder, &value);
    }
    ends_with_newline
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

    /// Close the current entry, terminating its line unless its value did.
    fn finish_entry(&mut self, ends_with_newline: bool) {
        if !ends_with_newline {
            self.builder.token(SyntaxKind::NEWLINE.into(), "\n");
        }
        self.builder.finish_node();
    }

    /// Add a value to the sequence. Accepts any type implementing [`AsYaml`]:
    /// `&str`, `String`, `i64`, `bool`, `f64`, CST nodes, etc.
    pub fn item(mut self, value: impl AsYaml) -> Self {
        self.start_entry();
        let nested = self.indent + 2;
        let block_node = value.as_node().filter(|n| {
            !value.is_inline() && !matches!(n.kind(), SyntaxKind::MAPPING | SyntaxKind::SEQUENCE)
        });
        let ends_with_newline = match block_node {
            // Block scalars and tagged block content.
            Some(node) => emit_block_value(&mut self.builder, node, nested, false),
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
        self.finish_entry(ends_with_newline);
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
        self.builder.finish_node();
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
        self.builder.finish_node();
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
        self.finish_entry(ends_with_newline);
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

    /// Close the current entry, terminating its line unless its value did.
    fn finish_entry(&mut self, ends_with_newline: bool) {
        if !ends_with_newline {
            self.builder.token(SyntaxKind::NEWLINE.into(), "\n");
        }
        self.builder.finish_node();
    }

    /// Add a key-value pair. The value can be any type implementing [`AsYaml`]:
    /// `&str`, `String`, `i64`, `bool`, `f64`, CST nodes, etc.
    pub fn pair(mut self, key: impl Into<String>, value: impl AsYaml) -> Self {
        self.start_entry(&key.into());
        let nested = self.indent + 2;
        let ends_with_newline = match value.as_node().filter(|_| !value.is_inline()) {
            Some(node) => emit_block_value(&mut self.builder, node, nested, true),
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
        self.finish_entry(ends_with_newline);
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
        self.builder.finish_node();
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
        self.builder.finish_node();
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::debug;
    use std::str::FromStr;

    /// The built tree must be exactly what parsing its own text gives.
    #[track_caller]
    fn assert_parser_shaped(file: &YamlFile) {
        let syntax = file.syntax();
        debug::validate_tree(syntax).unwrap();
        debug::roundtrip_ok(syntax).unwrap();
        let reparsed = YamlFile::from_str(&file.to_string()).unwrap();
        assert_eq!(
            debug::tree_to_string(syntax),
            debug::tree_to_string(reparsed.syntax())
        );
    }

    #[test]
    fn test_built_mapping_behaves_like_parsed() {
        let doc = MappingBuilder::new()
            .pair("a", 1)
            .pair("b", "x")
            .build_document();
        let mapping = doc.as_mapping().unwrap();
        assert_eq!(mapping.len(), 2);
        assert_eq!(mapping.get("a").unwrap().to_i64(), Some(1));
        let keys: Vec<String> = mapping
            .keys()
            .map(|k| k.as_scalar().unwrap().as_string())
            .collect();
        assert_eq!(keys, vec!["a", "b"]);
        mapping.set("c", 3);
        assert_eq!(doc.to_string(), "a: 1\nb: x\nc: 3\n");
        debug::validate_tree(doc.syntax()).unwrap();
        debug::roundtrip_ok(doc.syntax()).unwrap();
    }

    #[test]
    fn test_built_sequence_behaves_like_parsed() {
        let doc = SequenceBuilder::new().item(1).item("x").build_document();
        let seq = doc.as_sequence().unwrap();
        assert_eq!(seq.len(), 2);
        assert_eq!(seq.get(0).unwrap().to_i64(), Some(1));
        seq.push(3);
        assert_eq!(doc.to_string(), "- 1\n- x\n- 3\n");
        debug::validate_tree(doc.syntax()).unwrap();
        debug::roundtrip_ok(doc.syntax()).unwrap();
    }

    #[test]
    fn test_built_nested_behaves_like_parsed() {
        let doc = MappingBuilder::new()
            .mapping("db", |m| m.pair("host", "h"))
            .sequence("deps", |s| s.item("serde"))
            .build_document();
        let db = doc.get("db").unwrap();
        assert_eq!(db.as_mapping().unwrap().len(), 1);
        db.as_mapping().unwrap().set("port", 5432);
        let deps = doc.get("deps").unwrap();
        deps.as_sequence().unwrap().push("tokio");
        assert_eq!(
            doc.to_string(),
            "db:\n  host: h\n  port: 5432\ndeps:\n  - serde\n  - tokio\n"
        );
        debug::validate_tree(doc.syntax()).unwrap();
        debug::roundtrip_ok(doc.syntax()).unwrap();
    }

    #[test]
    fn test_builder_trees_match_parser() {
        let cases = vec![
            YamlBuilder::scalar("hello").build(),
            MappingBuilder::new()
                .pair("a", 1)
                .pair("b", "x")
                .build()
                .build(),
            SequenceBuilder::new().item(1).item("x").build().build(),
            MappingBuilder::new().build().build(),
            SequenceBuilder::new().build().build(),
            MappingBuilder::new()
                .mapping("empty", |m| m)
                .sequence("es", |s| s)
                .insert_mapping("m", MappingBuilder::new())
                .insert_sequence("s", SequenceBuilder::new())
                .build()
                .build(),
            SequenceBuilder::new()
                .sequence(|s| s)
                .mapping(|m| m)
                .insert_sequence(SequenceBuilder::new())
                .insert_mapping(MappingBuilder::new())
                .build()
                .build(),
            MappingBuilder::new()
                .pair("version", "1.0")
                .sequence("deps", |s| s.item("serde").item("tokio"))
                .mapping("db", |m| {
                    m.pair("host", "localhost")
                        .mapping("opts", |m| m.pair("a", true).pair("b", 1.5))
                })
                .build()
                .build(),
            SequenceBuilder::new()
                .mapping(|m| m.pair("id", 1).pair("name", "Alice"))
                .sequence(|s| s.item("a").sequence(|s| s.item("b")))
                .item("x")
                .build()
                .build(),
            MappingBuilder::new()
                .sequence("list", |s| {
                    s.item("simple")
                        .mapping(|m| m.pair("key", "value"))
                        .sequence(|s2| s2.item("n1").item("n2"))
                })
                .insert_mapping(
                    "outer",
                    MappingBuilder::new()
                        .pair("x", "1")
                        .mapping("deep", |m| m.pair("p", "9"))
                        .sequence("l", |s| s.item(1).mapping(|m| m.pair("q", 2))),
                )
                .build()
                .build(),
            SequenceBuilder::new()
                .item("first")
                .insert_sequence(SequenceBuilder::new().item("a").item("b"))
                .insert_mapping(
                    MappingBuilder::new()
                        .pair("key", "value")
                        .sequence("s", |s| s.item(1)),
                )
                .build()
                .build(),
            MappingBuilder::new()
                .pair("ref", crate::yaml::Alias::new("shared"))
                .pair("n", crate::ScalarValue::null())
                .build()
                .build(),
        ];
        for yaml in &cases {
            assert_parser_shaped(yaml);
        }
    }

    #[test]
    fn test_builder_node_values_match_parser() {
        let src = Document::from_str("item:\n  key: value\n  list:\n    - 1\n").unwrap();
        let item = src.get("item").unwrap();
        let mapping = item.as_mapping().unwrap();
        let list = mapping.get("list").unwrap();

        let yaml = SequenceBuilder::new()
            .item(mapping)
            .item(list.as_sequence().unwrap())
            .build()
            .build();
        assert_eq!(yaml.to_string(), "- key: value\n  list:\n    - 1\n- - 1\n");
        assert_parser_shaped(&yaml);

        let yaml = MappingBuilder::new()
            .pair("m", mapping)
            .mapping("outer", |m| m.pair("l", list.as_sequence().unwrap()))
            .build()
            .build();
        assert_eq!(
            yaml.to_string(),
            "m:\n  key: value\n  list:\n    - 1\nouter:\n  l:\n    - 1\n"
        );
        assert_parser_shaped(&yaml);
    }

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
