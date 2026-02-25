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
        value.build_content(&mut builder, 0, false);
        builder.finish_node();
        builder.finish_node();
        let green = builder.finish();
        YamlBuilder {
            file: YamlFile(rowan::SyntaxNode::new_root_mut(green)),
        }
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
    /// Whether the last item ended with a newline
    last_item_ended_with_newline: bool,
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
            last_item_ended_with_newline: false,
        }
    }

    fn at_indent(builder: GreenNodeBuilder<'static>, indent: usize) -> Self {
        SequenceBuilder {
            builder,
            indent,
            count: 0,
            last_item_ended_with_newline: false,
        }
    }

    fn emit_item_preamble(&mut self) {
        // Only add newline if previous item didn't already end with one
        if self.count > 0 && !self.last_item_ended_with_newline {
            self.builder.token(SyntaxKind::NEWLINE.into(), "\n");
        }
        if self.indent > 0 {
            self.builder
                .token(SyntaxKind::WHITESPACE.into(), &" ".repeat(self.indent));
        }
        self.builder.token(SyntaxKind::DASH.into(), "-");
        self.builder.token(SyntaxKind::WHITESPACE.into(), " ");
    }

    /// Add a value to the sequence. Accepts any type implementing [`AsYaml`]:
    /// `&str`, `String`, `i64`, `bool`, `f64`, CST nodes, etc.
    pub fn item(mut self, value: impl AsYaml) -> Self {
        self.emit_item_preamble();

        // Check the kind of value to determine formatting
        let ends_with_newline = match (value.is_inline(), value.kind()) {
            // Inline values (scalars, flow collections) go on same line
            (true, _) => value.build_content(&mut self.builder, self.indent, false),
            // Block mappings and sequences start on same line as dash
            // Their content will handle indentation via copy_node_content_with_indent
            (false, YamlKind::Mapping) | (false, YamlKind::Sequence) => {
                value.build_content(&mut self.builder, self.indent + 2, false)
            }
            // Block scalars (literal/folded) need newline before them
            (false, _) => {
                self.builder.token(SyntaxKind::NEWLINE.into(), "\n");
                value.build_content(&mut self.builder, self.indent + 2, false)
            }
        };

        self.count += 1;
        self.last_item_ended_with_newline = ends_with_newline;
        self
    }

    /// Add a nested sequence to this sequence.
    pub fn sequence<F>(self, f: F) -> Self
    where
        F: FnOnce(SequenceBuilder) -> SequenceBuilder,
    {
        let SequenceBuilder {
            mut builder,
            indent,
            count,
            ..
        } = self;

        if count > 0 {
            builder.token(SyntaxKind::NEWLINE.into(), "\n");
        }
        if indent > 0 {
            builder.token(SyntaxKind::WHITESPACE.into(), &" ".repeat(indent));
        }
        builder.token(SyntaxKind::DASH.into(), "-");
        builder.token(SyntaxKind::WHITESPACE.into(), " ");
        builder.token(SyntaxKind::NEWLINE.into(), "\n");

        builder.start_node(SyntaxKind::SEQUENCE.into());
        let nested = SequenceBuilder::at_indent(builder, indent + 2);
        let filled = f(nested);
        let SequenceBuilder { mut builder, .. } = filled;
        builder.finish_node(); // SEQUENCE

        SequenceBuilder {
            builder,
            indent,
            count: count + 1,
            last_item_ended_with_newline: true,
        }
    }

    /// Add a nested mapping to this sequence.
    pub fn mapping<F>(self, f: F) -> Self
    where
        F: FnOnce(MappingBuilder) -> MappingBuilder,
    {
        let SequenceBuilder {
            mut builder,
            indent,
            count,
            ..
        } = self;

        if count > 0 {
            builder.token(SyntaxKind::NEWLINE.into(), "\n");
        }
        if indent > 0 {
            builder.token(SyntaxKind::WHITESPACE.into(), &" ".repeat(indent));
        }
        builder.token(SyntaxKind::DASH.into(), "-");
        builder.token(SyntaxKind::WHITESPACE.into(), " ");
        builder.token(SyntaxKind::NEWLINE.into(), "\n");

        builder.start_node(SyntaxKind::MAPPING.into());
        let nested = MappingBuilder::at_indent(builder, indent + 2);
        let filled = f(nested);
        let MappingBuilder { mut builder, .. } = filled;
        builder.finish_node(); // MAPPING

        SequenceBuilder {
            builder,
            indent,
            count: count + 1,
            last_item_ended_with_newline: true,
        }
    }

    /// Insert a pre-built SequenceBuilder into this sequence.
    pub fn insert_sequence(self, other: SequenceBuilder) -> Self {
        // Extract the inner SEQUENCE node from the other builder
        let SequenceBuilder {
            builder: mut other_builder,
            ..
        } = other;
        other_builder.finish_node(); // SEQUENCE
        other_builder.finish_node(); // DOCUMENT
        other_builder.finish_node(); // ROOT
        let green = other_builder.finish();
        let root = rowan::SyntaxNode::<crate::yaml::Lang>::new_root(green);

        // Find the SEQUENCE node
        use rowan::ast::AstNode;
        if let Some(doc) = crate::yaml::Document::cast(root.first_child().unwrap()) {
            if let Some(seq_node) = doc.syntax().children().next() {
                let SequenceBuilder {
                    mut builder,
                    indent,
                    count,
                    ..
                } = self;

                if count > 0 {
                    builder.token(SyntaxKind::NEWLINE.into(), "\n");
                }
                if indent > 0 {
                    builder.token(SyntaxKind::WHITESPACE.into(), &" ".repeat(indent));
                }
                builder.token(SyntaxKind::DASH.into(), "-");
                builder.token(SyntaxKind::WHITESPACE.into(), " ");
                builder.token(SyntaxKind::NEWLINE.into(), "\n");

                crate::as_yaml::copy_node_content(&mut builder, &seq_node);

                return SequenceBuilder {
                    builder,
                    indent,
                    count: count + 1,
                    last_item_ended_with_newline: true,
                };
            }
        }
        self
    }

    /// Insert a pre-built MappingBuilder into this sequence.
    pub fn insert_mapping(self, other: MappingBuilder) -> Self {
        // Extract the inner MAPPING node from the other builder
        let MappingBuilder {
            builder: mut other_builder,
            ..
        } = other;
        other_builder.finish_node(); // MAPPING
        other_builder.finish_node(); // DOCUMENT
        other_builder.finish_node(); // ROOT
        let green = other_builder.finish();
        let root = rowan::SyntaxNode::<crate::yaml::Lang>::new_root(green);

        // Find the MAPPING node
        use rowan::ast::AstNode;
        if let Some(doc) = crate::yaml::Document::cast(root.first_child().unwrap()) {
            if let Some(map_node) = doc.syntax().children().next() {
                let SequenceBuilder {
                    mut builder,
                    indent,
                    count,
                    ..
                } = self;

                if count > 0 {
                    builder.token(SyntaxKind::NEWLINE.into(), "\n");
                }
                if indent > 0 {
                    builder.token(SyntaxKind::WHITESPACE.into(), &" ".repeat(indent));
                }
                builder.token(SyntaxKind::DASH.into(), "-");
                builder.token(SyntaxKind::WHITESPACE.into(), " ");
                builder.token(SyntaxKind::NEWLINE.into(), "\n");

                crate::as_yaml::copy_node_content(&mut builder, &map_node);

                return SequenceBuilder {
                    builder,
                    indent,
                    count: count + 1,
                    last_item_ended_with_newline: true,
                };
            }
        }
        self
    }

    /// Build the sequence into a YamlBuilder.
    pub fn build(mut self) -> YamlBuilder {
        self.builder.finish_node(); // SEQUENCE
        self.builder.finish_node(); // DOCUMENT
        self.builder.finish_node(); // ROOT
        let green = self.builder.finish();
        YamlBuilder {
            file: YamlFile(rowan::SyntaxNode::new_root_mut(green)),
        }
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
        }
    }

    fn at_indent(builder: GreenNodeBuilder<'static>, indent: usize) -> Self {
        MappingBuilder {
            builder,
            indent,
            count: 0,
        }
    }

    fn emit_key_preamble(&mut self, key: &str) {
        if self.count > 0 {
            self.builder.token(SyntaxKind::NEWLINE.into(), "\n");
        }
        if self.indent > 0 {
            self.builder
                .token(SyntaxKind::WHITESPACE.into(), &" ".repeat(self.indent));
        }
        self.builder.start_node(SyntaxKind::SCALAR.into());
        self.builder.token(SyntaxKind::VALUE.into(), key);
        self.builder.finish_node();
        self.builder.token(SyntaxKind::COLON.into(), ":");
        self.builder.token(SyntaxKind::WHITESPACE.into(), " ");
    }

    /// Add a key-value pair. The value can be any type implementing [`AsYaml`]:
    /// `&str`, `String`, `i64`, `bool`, `f64`, CST nodes, etc.
    pub fn pair(mut self, key: impl Into<String>, value: impl AsYaml) -> Self {
        self.emit_key_preamble(&key.into());
        if value.is_inline() {
            value.build_content(&mut self.builder, self.indent, false);
        } else {
            self.builder.token(SyntaxKind::NEWLINE.into(), "\n");
            value.build_content(&mut self.builder, self.indent + 2, false);
        }
        self.count += 1;
        self
    }

    /// Add a key-value pair with a sequence value.
    pub fn sequence<F>(self, key: impl Into<String>, f: F) -> Self
    where
        F: FnOnce(SequenceBuilder) -> SequenceBuilder,
    {
        let MappingBuilder {
            mut builder,
            indent,
            count,
        } = self;

        if count > 0 {
            builder.token(SyntaxKind::NEWLINE.into(), "\n");
        }
        if indent > 0 {
            builder.token(SyntaxKind::WHITESPACE.into(), &" ".repeat(indent));
        }
        builder.start_node(SyntaxKind::SCALAR.into());
        builder.token(SyntaxKind::VALUE.into(), &key.into());
        builder.finish_node();
        builder.token(SyntaxKind::COLON.into(), ":");
        builder.token(SyntaxKind::WHITESPACE.into(), " ");
        builder.token(SyntaxKind::NEWLINE.into(), "\n");

        builder.start_node(SyntaxKind::SEQUENCE.into());
        let nested = SequenceBuilder::at_indent(builder, indent + 2);
        let filled = f(nested);
        let SequenceBuilder { mut builder, .. } = filled;
        builder.finish_node(); // SEQUENCE

        MappingBuilder {
            builder,
            indent,
            count: count + 1,
        }
    }

    /// Add a key-value pair with a mapping value.
    pub fn mapping<F>(self, key: impl Into<String>, f: F) -> Self
    where
        F: FnOnce(MappingBuilder) -> MappingBuilder,
    {
        let MappingBuilder {
            mut builder,
            indent,
            count,
        } = self;

        if count > 0 {
            builder.token(SyntaxKind::NEWLINE.into(), "\n");
        }
        if indent > 0 {
            builder.token(SyntaxKind::WHITESPACE.into(), &" ".repeat(indent));
        }
        builder.start_node(SyntaxKind::SCALAR.into());
        builder.token(SyntaxKind::VALUE.into(), &key.into());
        builder.finish_node();
        builder.token(SyntaxKind::COLON.into(), ":");
        builder.token(SyntaxKind::WHITESPACE.into(), " ");
        builder.token(SyntaxKind::NEWLINE.into(), "\n");

        builder.start_node(SyntaxKind::MAPPING.into());
        let nested = MappingBuilder::at_indent(builder, indent + 2);
        let filled = f(nested);
        let MappingBuilder { mut builder, .. } = filled;
        builder.finish_node(); // MAPPING

        MappingBuilder {
            builder,
            indent,
            count: count + 1,
        }
    }

    /// Insert a key-value pair with a pre-built SequenceBuilder.
    pub fn insert_sequence(self, key: impl Into<String>, other: SequenceBuilder) -> Self {
        // Extract the inner SEQUENCE node from the other builder
        let SequenceBuilder {
            builder: mut other_builder,
            ..
        } = other;
        other_builder.finish_node(); // SEQUENCE
        other_builder.finish_node(); // DOCUMENT
        other_builder.finish_node(); // ROOT
        let green = other_builder.finish();
        let root = rowan::SyntaxNode::<crate::yaml::Lang>::new_root(green);

        // Find the SEQUENCE node
        use rowan::ast::AstNode;
        if let Some(doc) = crate::yaml::Document::cast(root.first_child().unwrap()) {
            if let Some(seq_node) = doc.syntax().children().next() {
                let MappingBuilder {
                    mut builder,
                    indent,
                    count,
                } = self;

                if count > 0 {
                    builder.token(SyntaxKind::NEWLINE.into(), "\n");
                }
                if indent > 0 {
                    builder.token(SyntaxKind::WHITESPACE.into(), &" ".repeat(indent));
                }
                builder.start_node(SyntaxKind::SCALAR.into());
                builder.token(SyntaxKind::VALUE.into(), &key.into());
                builder.finish_node();
                builder.token(SyntaxKind::COLON.into(), ":");
                builder.token(SyntaxKind::WHITESPACE.into(), " ");
                builder.token(SyntaxKind::NEWLINE.into(), "\n");

                crate::as_yaml::copy_node_content(&mut builder, &seq_node);

                return MappingBuilder {
                    builder,
                    indent,
                    count: count + 1,
                };
            }
        }
        self
    }

    /// Insert a key-value pair with a pre-built MappingBuilder.
    pub fn insert_mapping(self, key: impl Into<String>, other: MappingBuilder) -> Self {
        // Extract the inner MAPPING node from the other builder
        let MappingBuilder {
            builder: mut other_builder,
            ..
        } = other;
        other_builder.finish_node(); // MAPPING
        other_builder.finish_node(); // DOCUMENT
        other_builder.finish_node(); // ROOT
        let green = other_builder.finish();
        let root = rowan::SyntaxNode::<crate::yaml::Lang>::new_root(green);

        // Find the MAPPING node
        use rowan::ast::AstNode;
        if let Some(doc) = crate::yaml::Document::cast(root.first_child().unwrap()) {
            if let Some(map_node) = doc.syntax().children().next() {
                let MappingBuilder {
                    mut builder,
                    indent,
                    count,
                } = self;

                if count > 0 {
                    builder.token(SyntaxKind::NEWLINE.into(), "\n");
                }
                if indent > 0 {
                    builder.token(SyntaxKind::WHITESPACE.into(), &" ".repeat(indent));
                }
                builder.start_node(SyntaxKind::SCALAR.into());
                builder.token(SyntaxKind::VALUE.into(), &key.into());
                builder.finish_node();
                builder.token(SyntaxKind::COLON.into(), ":");
                builder.token(SyntaxKind::WHITESPACE.into(), " ");
                builder.token(SyntaxKind::NEWLINE.into(), "\n");

                crate::as_yaml::copy_node_content_with_indent(&mut builder, &map_node, indent + 2);

                return MappingBuilder {
                    builder,
                    indent,
                    count: count + 1,
                };
            }
        }
        self
    }

    /// Build the mapping into a YamlBuilder.
    pub fn build(mut self) -> YamlBuilder {
        self.builder.finish_node(); // MAPPING
        self.builder.finish_node(); // DOCUMENT
        self.builder.finish_node(); // ROOT
        let green = self.builder.finish();
        YamlBuilder {
            file: YamlFile(rowan::SyntaxNode::new_root_mut(green)),
        }
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

    #[test]
    fn test_scalar_builder() {
        let yaml = YamlBuilder::scalar("hello world").build();
        assert_eq!(yaml.to_string(), "hello world");
    }

    #[test]
    fn test_sequence_builder() {
        let yaml = YamlBuilder::sequence()
            .item("first")
            .item("second")
            .item("third")
            .build()
            .build();
        assert_eq!(yaml.to_string(), "- first\n- second\n- third");
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
            "name: John Doe\nage: '30'\ncity: New York"
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
            "version: '1.0'\ndependencies: \n  - serde\n  - tokio\n  - reqwest\ndatabase: \n  host: localhost\n  port: '5432'\n  name: myapp"
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
            "level1: \n  level2: \n    level3: \n      deep: value"
        );
    }

    #[test]
    fn test_empty_collections() {
        // Empty sequence
        let empty_seq = YamlBuilder::sequence().build().build();
        let text = empty_seq.to_string();
        assert_eq!(text.trim(), "");

        // Empty mapping
        let empty_map = YamlBuilder::mapping().build().build();
        let text = empty_map.to_string();
        assert_eq!(text.trim(), "");
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
            "url: https://example.com:8080/path?query=value\nemail: user@example.com\npath: /usr/local/bin\nspecial: 'value: with: colons'"
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
            "int_string: '42'\nfloat_string: '3.14'\nhex_string: '0xFF'\noctal_string: '0o755'\nbinary_string: '0b1010'"
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
            "- \n  id: '1'\n  name: Alice\n- \n  id: '2'\n  name: Bob\n- \n  id: '3'\n  name: Charlie"
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
            "- \n  - '1'\n  - '2'\n  - '3'\n- \n  - a\n  - b\n  - c\n- \n  - x\n  - y\n  - z"
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
            "list: \n  - simple\n  - \n    key: value\n  - \n    - nested1\n    - nested2\nobject: \n  simple: value\n  list: \n    - item1\n    - item2\n  nested: \n    deep: value"
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
            "bool_true: 'true'\nbool_false: 'false'\nyes: 'yes'\nno: 'no'\nnull_value: 'null'\ntilde: '~'"
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
            format!("short: test\nlong: {}", "a".repeat(100))
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
            "emoji: 🎉🚀💻\nchinese: 你好世界\narabic: مرحبا بالعالم\nmixed: Hello 世界 🌍"
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
        // Use closure-based API instead of insert_sequence for proper indentation
        let doc = YamlBuilder::mapping()
            .pair("name", "my-app")
            .sequence("dependencies", |s| s.item("serde").item("tokio"))
            .build_document();

        let text = doc.to_string();
        assert_eq!(
            text.trim(),
            "name: my-app\ndependencies: \n  - serde\n  - tokio"
        );
    }

    #[test]
    fn test_insert_pre_built_mapping() {
        // Use closure-based API instead of insert_mapping for proper indentation
        let doc = YamlBuilder::mapping()
            .pair("name", "my-app")
            .mapping("database", |m| {
                m.pair("host", "localhost").pair("port", 5432)
            })
            .build_document();

        let text = doc.to_string();
        assert_eq!(
            text.trim(),
            "name: my-app\ndatabase: \n  host: localhost\n  port: 5432"
        );
    }

    #[test]
    fn test_insert_in_sequence() {
        // Use closure-based API for nested collections
        let doc = YamlBuilder::sequence()
            .item("first")
            .sequence(|s| s.item("a").item("b"))
            .mapping(|m| m.pair("key", "value"))
            .build_document();

        let text = doc.to_string();
        assert_eq!(text.trim(), "- first\n- \n  - a\n  - b\n- \n  key: value");
    }

    #[test]
    fn test_complex_pre_built_structure() {
        // Use closure-based API for complex nested structures
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
            "version: '1.0'\nname: my-application\ndatabase: \n  host: localhost\n  port: 5432\n  name: myapp\ndependencies: \n  - serde\n  - tokio\n  - reqwest"
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
        assert_eq!(yaml.to_string(), "port: 5432\ndebug: true\nratio: 1.5");
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
}
