//! Builder pattern for constructing YAML documents fluently.

use crate::{lex::SyntaxKind, yaml::Yaml};
use rowan::GreenNodeBuilder;

/// A builder for constructing YAML documents with a fluent API.
pub struct YamlBuilder {
    root: BuilderNode,
}

/// Internal representation of nodes being built.
#[derive(Debug, Clone)]
enum BuilderNode {
    Scalar(String),
    Sequence(Vec<BuilderNode>),
    Mapping(Vec<(String, BuilderNode)>),
    Document(Box<BuilderNode>),
}

impl YamlBuilder {
    /// Create a new YAML builder.
    pub fn new() -> Self {
        YamlBuilder {
            root: BuilderNode::Document(Box::new(BuilderNode::Mapping(Vec::new()))),
        }
    }

    /// Start building from a scalar value.
    pub fn scalar(value: impl Into<String>) -> Self {
        YamlBuilder {
            root: BuilderNode::Document(Box::new(BuilderNode::Scalar(value.into()))),
        }
    }

    /// Start building from a sequence.
    pub fn sequence() -> SequenceBuilder {
        SequenceBuilder { items: Vec::new() }
    }

    /// Start building from a mapping.
    pub fn mapping() -> MappingBuilder {
        MappingBuilder { pairs: Vec::new() }
    }

    /// Build the final YAML document.
    pub fn build(self) -> Yaml {
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(SyntaxKind::ROOT.into());

        match self.root {
            BuilderNode::Document(inner) => {
                builder.start_node(SyntaxKind::DOCUMENT.into());
                Self::build_node(&mut builder, *inner);
                builder.finish_node();
            }
            node => {
                builder.start_node(SyntaxKind::DOCUMENT.into());
                Self::build_node(&mut builder, node);
                builder.finish_node();
            }
        }

        builder.finish_node();
        let green = builder.finish();
        Yaml::from(rowan::SyntaxNode::new_root_mut(green))
    }

    fn build_node(builder: &mut GreenNodeBuilder, node: BuilderNode) {
        match node {
            BuilderNode::Scalar(value) => {
                builder.start_node(SyntaxKind::SCALAR.into());
                builder.token(SyntaxKind::VALUE.into(), &value);
                builder.finish_node();
            }
            BuilderNode::Sequence(items) => {
                builder.start_node(SyntaxKind::SEQUENCE.into());
                for item in items {
                    builder.token(SyntaxKind::DASH.into(), "-");
                    builder.token(SyntaxKind::WHITESPACE.into(), " ");
                    Self::build_node(builder, item);
                    builder.token(SyntaxKind::NEWLINE.into(), "\n");
                }
                builder.finish_node();
            }
            BuilderNode::Mapping(pairs) => {
                builder.start_node(SyntaxKind::MAPPING.into());
                for (i, (key, value)) in pairs.iter().enumerate() {
                    if i > 0 {
                        // No newline before first item
                        builder.token(SyntaxKind::NEWLINE.into(), "\n");
                    }
                    builder.start_node(SyntaxKind::SCALAR.into());
                    builder.token(SyntaxKind::VALUE.into(), key);
                    builder.finish_node();
                    builder.token(SyntaxKind::COLON.into(), ":");
                    builder.token(SyntaxKind::WHITESPACE.into(), " ");
                    Self::build_node_inline(builder, value.clone());
                }
                builder.finish_node();
            }
            BuilderNode::Document(inner) => {
                Self::build_node(builder, *inner);
            }
        }
    }

    fn build_node_inline(builder: &mut GreenNodeBuilder, node: BuilderNode) {
        match node {
            BuilderNode::Scalar(value) => {
                builder.start_node(SyntaxKind::SCALAR.into());
                builder.token(SyntaxKind::VALUE.into(), &value);
                builder.finish_node();
            }
            BuilderNode::Sequence(_) | BuilderNode::Mapping(_) => {
                // For nested structures, add newline and indent
                builder.token(SyntaxKind::NEWLINE.into(), "\n");
                builder.token(SyntaxKind::WHITESPACE.into(), "  ");
                Self::build_node(builder, node);
            }
            BuilderNode::Document(inner) => {
                Self::build_node_inline(builder, *inner);
            }
        }
    }
}

impl Default for YamlBuilder {
    fn default() -> Self {
        Self::new()
    }
}

/// Builder for YAML sequences.
pub struct SequenceBuilder {
    items: Vec<BuilderNode>,
}

impl SequenceBuilder {
    /// Add a scalar value to the sequence.
    pub fn item(mut self, value: impl Into<String>) -> Self {
        self.items.push(BuilderNode::Scalar(value.into()));
        self
    }

    /// Add a nested sequence to this sequence.
    pub fn sequence<F>(mut self, f: F) -> Self
    where
        F: FnOnce(SequenceBuilder) -> SequenceBuilder,
    {
        let nested = f(SequenceBuilder { items: Vec::new() });
        self.items.push(BuilderNode::Sequence(nested.items));
        self
    }

    /// Add a nested mapping to this sequence.
    pub fn mapping<F>(mut self, f: F) -> Self
    where
        F: FnOnce(MappingBuilder) -> MappingBuilder,
    {
        let nested = f(MappingBuilder { pairs: Vec::new() });
        self.items.push(BuilderNode::Mapping(nested.pairs));
        self
    }

    /// Build the sequence into a YamlBuilder.
    pub fn build(self) -> YamlBuilder {
        YamlBuilder {
            root: BuilderNode::Document(Box::new(BuilderNode::Sequence(self.items))),
        }
    }
}

/// Builder for YAML mappings.
pub struct MappingBuilder {
    pairs: Vec<(String, BuilderNode)>,
}

impl MappingBuilder {
    /// Add a key-value pair with a scalar value.
    pub fn pair(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.pairs
            .push((key.into(), BuilderNode::Scalar(value.into())));
        self
    }

    /// Add a key-value pair with a sequence value.
    pub fn sequence<F>(mut self, key: impl Into<String>, f: F) -> Self
    where
        F: FnOnce(SequenceBuilder) -> SequenceBuilder,
    {
        let nested = f(SequenceBuilder { items: Vec::new() });
        self.pairs
            .push((key.into(), BuilderNode::Sequence(nested.items)));
        self
    }

    /// Add a key-value pair with a mapping value.
    pub fn mapping<F>(mut self, key: impl Into<String>, f: F) -> Self
    where
        F: FnOnce(MappingBuilder) -> MappingBuilder,
    {
        let nested = f(MappingBuilder { pairs: Vec::new() });
        self.pairs
            .push((key.into(), BuilderNode::Mapping(nested.pairs)));
        self
    }

    /// Build the mapping into a YamlBuilder.
    pub fn build(self) -> YamlBuilder {
        YamlBuilder {
            root: BuilderNode::Document(Box::new(BuilderNode::Mapping(self.pairs))),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_scalar_builder() {
        let yaml = YamlBuilder::scalar("hello world").build();
        let text = yaml.to_string();
        assert!(text.contains("hello world"));
    }

    #[test]
    fn test_sequence_builder() {
        let yaml = YamlBuilder::sequence()
            .item("first")
            .item("second")
            .item("third")
            .build()
            .build();

        let text = yaml.to_string();
        assert!(text.contains("first"));
        assert!(text.contains("second"));
        assert!(text.contains("third"));
    }

    #[test]
    fn test_mapping_builder() {
        let yaml = YamlBuilder::mapping()
            .pair("name", "John Doe")
            .pair("age", "30")
            .pair("city", "New York")
            .build()
            .build();

        let text = yaml.to_string();
        assert!(text.contains("name"));
        assert!(text.contains("John Doe"));
        assert!(text.contains("age"));
        assert!(text.contains("30"));
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

        let text = yaml.to_string();
        assert!(text.contains("version"));
        assert!(text.contains("dependencies"));
        assert!(text.contains("serde"));
        assert!(text.contains("database"));
        assert!(text.contains("localhost"));
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

        let text = yaml.to_string();
        assert!(text.contains("level1"));
        assert!(text.contains("level2"));
        assert!(text.contains("level3"));
        assert!(text.contains("deep"));
        assert!(text.contains("value"));
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

        let text = yaml.to_string();
        assert!(text.contains("https://example.com:8080/path?query=value"));
        assert!(text.contains("user@example.com"));
        assert!(text.contains("/usr/local/bin"));
        assert!(text.contains("value: with: colons"));
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

        let text = yaml.to_string();
        assert!(text.contains("42"));
        assert!(text.contains("3.14"));
        assert!(text.contains("0xFF"));
        assert!(text.contains("0o755"));
        assert!(text.contains("0b1010"));
    }

    #[test]
    fn test_sequences_with_nested_mappings() {
        let yaml = YamlBuilder::sequence()
            .mapping(|m| m.pair("id", "1").pair("name", "Alice"))
            .mapping(|m| m.pair("id", "2").pair("name", "Bob"))
            .mapping(|m| m.pair("id", "3").pair("name", "Charlie"))
            .build()
            .build();

        let text = yaml.to_string();
        assert!(text.contains("Alice"));
        assert!(text.contains("Bob"));
        assert!(text.contains("Charlie"));
    }

    #[test]
    fn test_sequences_with_nested_sequences() {
        let yaml = YamlBuilder::sequence()
            .sequence(|s| s.item("1").item("2").item("3"))
            .sequence(|s| s.item("a").item("b").item("c"))
            .sequence(|s| s.item("x").item("y").item("z"))
            .build()
            .build();

        let text = yaml.to_string();
        assert!(text.contains("1"));
        assert!(text.contains("2"));
        assert!(text.contains("3"));
        assert!(text.contains("a"));
        assert!(text.contains("b"));
        assert!(text.contains("c"));
        assert!(text.contains("x"));
        assert!(text.contains("y"));
        assert!(text.contains("z"));
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

        let text = yaml.to_string();
        assert!(text.contains("list"));
        assert!(text.contains("simple"));
        assert!(text.contains("nested1"));
        assert!(text.contains("object"));
        assert!(text.contains("item1"));
        assert!(text.contains("deep"));
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

        let text = yaml.to_string();
        assert!(text.contains("true"));
        assert!(text.contains("false"));
        assert!(text.contains("yes"));
        assert!(text.contains("no"));
        assert!(text.contains("null"));
        assert!(text.contains("~"));
    }

    #[test]
    fn test_long_strings() {
        let long_string = "a".repeat(100);
        let yaml = YamlBuilder::mapping()
            .pair("short", "test")
            .pair("long", &long_string)
            .build()
            .build();

        let text = yaml.to_string();
        assert!(text.contains("test"));
        assert!(text.contains(&long_string));
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

        let text = yaml.to_string();
        assert!(text.contains("🎉🚀💻"));
        assert!(text.contains("你好世界"));
        assert!(text.contains("مرحبا بالعالم"));
        assert!(text.contains("Hello 世界 🌍"));
    }
}
