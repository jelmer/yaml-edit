//! Lossless YAML parser and editor.

use crate::{
    error::YamlResult,
    error_recovery::{ErrorBuilder, ErrorRecoveryContext, ParseContext, RecoveryStrategy},
    lex::{lex, SyntaxKind},
    parse::Parse,
    scalar::ScalarValue,
    value::YamlValue,
    PositionedParseError,
};
use rowan::ast::AstNode;
use rowan::GreenNodeBuilder;
use std::collections::HashMap;
use std::path::Path;
use std::str::FromStr;

/// A positioned parse error containing location information.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ParsedYaml {
    pub green_node: rowan::GreenNode,
    pub errors: Vec<String>,
    pub positioned_errors: Vec<PositionedParseError>,
}

/// YAML language type for rowan.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Lang {}

impl rowan::Language for Lang {
    type Kind = SyntaxKind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        unsafe { std::mem::transmute::<u16, SyntaxKind>(raw.0) }
    }

    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        kind.into()
    }
}

type SyntaxNode = rowan::SyntaxNode<Lang>;

/// A macro to create AST node wrappers.
macro_rules! ast_node {
    ($ast:ident, $kind:ident, $doc:expr) => {
        #[doc = $doc]
        #[derive(PartialEq, Eq, Hash)]
        pub struct $ast(SyntaxNode);

        impl std::fmt::Debug for $ast {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_struct(stringify!($ast))
                    .field("syntax", &self.0)
                    .finish()
            }
        }

        impl AstNode for $ast {
            type Language = Lang;

            fn can_cast(kind: SyntaxKind) -> bool {
                kind == SyntaxKind::$kind
            }

            fn cast(syntax: SyntaxNode) -> Option<Self> {
                if Self::can_cast(syntax.kind()) {
                    Some(Self(syntax))
                } else {
                    None
                }
            }

            fn syntax(&self) -> &SyntaxNode {
                &self.0
            }
        }

        impl From<SyntaxNode> for $ast {
            fn from(node: SyntaxNode) -> Self {
                $ast(node)
            }
        }

        impl std::fmt::Display for $ast {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", self.0.text())
            }
        }
    };
}

ast_node!(
    Yaml,
    ROOT,
    "A YAML document containing one or more documents"
);
ast_node!(Document, DOCUMENT, "A single YAML document");
ast_node!(Sequence, SEQUENCE, "A YAML sequence (list)");
ast_node!(Mapping, MAPPING, "A YAML mapping (key-value pairs)");
ast_node!(MappingEntry, MAPPING_ENTRY, "A key-value pair in a YAML mapping");
ast_node!(SequenceEntry, SEQUENCE_ENTRY, "An entry in a YAML sequence");
ast_node!(Scalar, SCALAR, "A YAML scalar value");

/// A virtual AST node for YAML sets (!!set tagged scalars)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Set(SyntaxNode);

impl Set {
    /// Cast a SyntaxNode to a Set only if it's a TAGGED_SCALAR with !!set tag
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        if node.kind() == SyntaxKind::TAGGED_SCALAR {
            if let Some(tagged_scalar) = TaggedScalar::cast(node.clone()) {
                if tagged_scalar.tag() == Some("!!set".to_string()) {
                    return Some(Set(node));
                }
            }
        }
        None
    }
    
    /// Get the underlying SyntaxNode
    pub fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
    
    /// Get an iterator over the set members as YamlValues
    pub fn members(&self) -> Vec<YamlValue> {
        // Sets are represented as mappings where keys are the set members
        // The Set node IS the TAGGED_SCALAR node, find MAPPING as its child
        if let Some(mapping_node) = self.0.children().find(|n| n.kind() == SyntaxKind::MAPPING) {
            let mut members = Vec::new();
            
            // Check if we have MAPPING_ENTRY nodes (regular format: `key: null`)
            let has_mapping_entries = mapping_node.children()
                .any(|n| n.kind() == SyntaxKind::MAPPING_ENTRY);
            
            if has_mapping_entries {
                // Regular format with MAPPING_ENTRY nodes
                for entry in mapping_node.children().filter(|n| n.kind() == SyntaxKind::MAPPING_ENTRY) {
                    if let Some(key_node) = entry.children().find(|n| n.kind() == SyntaxKind::KEY) {
                        if let Some(scalar_child) = key_node.children().find(|n| n.kind() == SyntaxKind::SCALAR) {
                            if let Some(scalar) = Scalar::cast(scalar_child) {
                                members.push(YamlValue::Scalar(ScalarValue::new(scalar.value().trim())));
                            }
                        }
                    }
                }
            } else {
                // Explicit key format with KEY nodes directly under MAPPING (? key)
                for key_node in mapping_node.children().filter(|n| n.kind() == SyntaxKind::KEY) {
                    if let Some(scalar_child) = key_node.children().find(|n| n.kind() == SyntaxKind::SCALAR) {
                        if let Some(scalar) = Scalar::cast(scalar_child) {
                            members.push(YamlValue::Scalar(ScalarValue::new(scalar.value().trim())));
                        }
                    }
                }
            }
            
            members
        } else {
            Vec::new()
        }
    }
    
    /// Get the number of items in the set
    pub fn len(&self) -> usize {
        self.members().len()
    }
    
    /// Check if the set is empty
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
    
    /// Check if the set contains a specific value
    pub fn contains(&self, value: &YamlValue) -> bool {
        self.members().iter().any(|member| member == value)
    }
}
ast_node!(
    TaggedScalar,
    TAGGED_SCALAR,
    "A YAML tagged scalar (tag + value)"
);
ast_node!(Directive, DIRECTIVE, "A YAML directive like %YAML 1.2");

// Utility functions for YAML scalar handling

/// Parse a YAML scalar text, handling different quote styles
fn parse_yaml_scalar_text(text: &str) -> String {
    let trimmed = text.trim();
    
    // Handle double quotes
    if trimmed.starts_with('"') && trimmed.ends_with('"') && trimmed.len() >= 2 {
        // Parse escape sequences in double-quoted strings
        let inner = &trimmed[1..trimmed.len()-1];
        return unescape_double_quoted(inner);
    }
    
    // Handle single quotes
    if trimmed.starts_with('\'') && trimmed.ends_with('\'') && trimmed.len() >= 2 {
        // Single quotes: only '' needs unescaping to '
        let inner = &trimmed[1..trimmed.len()-1];
        return inner.replace("''", "'");
    }
    
    // Plain scalar - return as-is
    trimmed.to_string()
}

/// Unescape a double-quoted string following YAML spec
fn unescape_double_quoted(s: &str) -> String {
    let mut result = String::new();
    let mut chars = s.chars();
    
    while let Some(ch) = chars.next() {
        if ch == '\\' {
            if let Some(next) = chars.next() {
                match next {
                    'n' => result.push('\n'),
                    'r' => result.push('\r'),
                    't' => result.push('\t'),
                    '\\' => result.push('\\'),
                    '"' => result.push('"'),
                    '/' => result.push('/'),
                    'b' => result.push('\u{0008}'),
                    'f' => result.push('\u{000C}'),
                    'a' => result.push('\u{0007}'),
                    'v' => result.push('\u{000B}'),
                    'e' => result.push('\u{001B}'),
                    ' ' => result.push(' '),
                    'N' => result.push('\u{0085}'),
                    '_' => result.push('\u{00A0}'),
                    'L' => result.push('\u{2028}'),
                    'P' => result.push('\u{2029}'),
                    'x' => {
                        // Hex escape \xHH
                        let hex: String = chars.by_ref().take(2).collect();
                        if let Ok(code) = u8::from_str_radix(&hex, 16) {
                            result.push(code as char);
                        } else {
                            result.push('\\');
                            result.push('x');
                            result.push_str(&hex);
                        }
                    }
                    'u' => {
                        // Unicode escape \uHHHH
                        let hex: String = chars.by_ref().take(4).collect();
                        if let Ok(code) = u32::from_str_radix(&hex, 16) {
                            if let Some(ch) = char::from_u32(code) {
                                result.push(ch);
                            } else {
                                result.push('\\');
                                result.push('u');
                                result.push_str(&hex);
                            }
                        } else {
                            result.push('\\');
                            result.push('u');
                            result.push_str(&hex);
                        }
                    }
                    'U' => {
                        // Unicode escape \UHHHHHHHH
                        let hex: String = chars.by_ref().take(8).collect();
                        if let Ok(code) = u32::from_str_radix(&hex, 16) {
                            if let Some(ch) = char::from_u32(code) {
                                result.push(ch);
                            } else {
                                result.push('\\');
                                result.push('U');
                                result.push_str(&hex);
                            }
                        } else {
                            result.push('\\');
                            result.push('U');
                            result.push_str(&hex);
                        }
                    }
                    _ => {
                        result.push('\\');
                        result.push(next);
                    }
                }
            } else {
                result.push('\\');
            }
        } else {
            result.push(ch);
        }
    }
    
    result
}

// Helper function to create a green token
fn create_token_green(kind: SyntaxKind, text: &str) -> rowan::GreenToken {
    rowan::GreenToken::new(kind.into(), text)
}

// Helper function to recursively add node children to a builder
fn add_node_children_to(builder: &mut GreenNodeBuilder, node: &SyntaxNode) {
    for child in node.children_with_tokens() {
        match child {
            rowan::NodeOrToken::Node(child_node) => {
                builder.start_node(child_node.kind().into());
                add_node_children_to(builder, &child_node);
                builder.finish_node();
            }
            rowan::NodeOrToken::Token(token) => {
                builder.token(token.kind().into(), token.text());
            }
        }
    }
}

// MappingEntry methods
impl MappingEntry {
    /// Get the underlying syntax node (for debugging/testing)
    #[cfg(test)]
    pub fn syntax(&self) -> &SyntaxNode {
        &self.0
    }
    
    /// Get the KEY node from this mapping entry
    pub fn key(&self) -> Option<SyntaxNode> {
        self.0.children().find(|n| n.kind() == SyntaxKind::KEY)
    }
    
    /// Get the VALUE node from this mapping entry
    pub fn value(&self) -> Option<SyntaxNode> {
        self.0.children().find(|n| n.kind() == SyntaxKind::VALUE)
    }
    
    /// Create a new mapping entry from key and value YamlValues
    pub fn from_yaml_values(key: &YamlValue, value: &YamlValue) -> Self {
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(SyntaxKind::MAPPING_ENTRY.into());
        Document::build_key_content(&mut builder, key, 0);
        builder.token(SyntaxKind::COLON.into(), ":");
        builder.token(SyntaxKind::WHITESPACE.into(), " ");
        Document::build_value_content(&mut builder, value, 0);
        builder.finish_node();
        MappingEntry(SyntaxNode::new_root_mut(builder.finish()))
    }
    
    /// Replace the value of this mapping entry in place, preserving key and comments
    pub fn replace_value(&mut self, new_value: &YamlValue) {
        // Build new VALUE node
        let mut value_builder = GreenNodeBuilder::new();
        value_builder.start_node(SyntaxKind::VALUE.into());
        Document::build_value_content_only(&mut value_builder, new_value, 0);
        value_builder.finish_node();
        let new_value_node = SyntaxNode::new_root_mut(value_builder.finish());
        
        // Build a new entry preserving everything except the VALUE
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(SyntaxKind::MAPPING_ENTRY.into());
        
        let mut value_replaced = false;
        for child in self.0.children_with_tokens() {
            match child {
                rowan::NodeOrToken::Node(n) if n.kind() == SyntaxKind::VALUE => {
                    // Replace the VALUE node by adding it directly
                    builder.start_node(SyntaxKind::VALUE.into());
                    add_node_children_to(&mut builder, &new_value_node);
                    builder.finish_node();
                    value_replaced = true;
                }
                rowan::NodeOrToken::Node(n) => {
                    // Preserve the node (like KEY) with its structure
                    builder.start_node(n.kind().into());
                    add_node_children_to(&mut builder, &n);
                    builder.finish_node();
                }
                rowan::NodeOrToken::Token(t) => {
                    // Preserve all tokens including whitespace and comments
                    builder.token(t.kind().into(), t.text());
                }
            }
        }
        
        // If no VALUE node was found, add one after the colon
        if !value_replaced {
            builder.token(SyntaxKind::WHITESPACE.into(), " ");
            add_node_children_to(&mut builder, &new_value_node);
        }
        
        builder.finish_node();
        self.0 = SyntaxNode::new_root_mut(builder.finish());
    }
    
    /// Remove this mapping entry from its parent mapping
    pub fn discard(self) {
        self.0.detach();
    }
}

// SequenceEntry methods
impl SequenceEntry {
    /// Get the VALUE node from this sequence entry
    pub fn value(&self) -> Option<SyntaxNode> {
        self.0.children().find(|n| n.kind() == SyntaxKind::VALUE)
    }
    
    /// Create a new sequence entry from a YamlValue
    pub fn from_yaml_value(value: &YamlValue) -> Self {
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(SyntaxKind::SEQUENCE_ENTRY.into());
        builder.token(SyntaxKind::DASH.into(), "-");
        builder.token(SyntaxKind::WHITESPACE.into(), " ");
        Document::build_value_content(&mut builder, value, 0);
        builder.finish_node();
        SequenceEntry(SyntaxNode::new_root_mut(builder.finish()))
    }
}

impl Default for Yaml {
    fn default() -> Self {
        Self::new()
    }
}

impl Yaml {
    /// Create a new empty YAML document.
    pub fn new() -> Yaml {
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(SyntaxKind::ROOT.into());
        builder.finish_node();
        Yaml(SyntaxNode::new_root_mut(builder.finish()))
    }

    /// Parse YAML text, returning a Parse result
    pub fn parse(text: &str) -> Parse<Yaml> {
        Parse::parse_yaml(text)
    }

    /// Parse YAML from a file path
    pub fn from_path<P: AsRef<Path>>(path: P) -> Result<Yaml, crate::Error> {
        let contents = std::fs::read_to_string(path)?;
        Ok(Self::from_str(&contents)?)
    }

    /// Get all documents in this YAML file
    pub fn documents(&self) -> impl Iterator<Item = Document> {
        self.0.children().filter_map(Document::cast)
    }

    /// Get the first document, if any
    pub fn document(&self) -> Option<Document> {
        self.documents().next()
    }

    /// Ensure there's at least one document, creating an empty one if needed
    /// Returns the first document
    pub fn ensure_document(&mut self) -> Document {
        if self.documents().next().is_none() {
            // No document exists, add an empty one
            let doc = Document::new_mapping();
            self.push_document(doc);
        }
        self.documents()
            .next()
            .expect("Document should exist after ensuring")
    }

    /// Get all directives in this YAML file
    pub fn directives(&self) -> impl Iterator<Item = Directive> {
        self.0.children().filter_map(Directive::cast)
    }

    /// Add a directive to the beginning of this YAML file
    pub fn add_directive(&mut self, directive_text: &str) {
        let mut new_nodes = Vec::new();

        // Create directive node
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(SyntaxKind::DIRECTIVE.into());
        builder.token(SyntaxKind::DIRECTIVE.into(), directive_text);
        builder.finish_node();
        let directive_node = builder.finish();

        new_nodes.push(directive_node.into());
        new_nodes.push(create_token_green(SyntaxKind::NEWLINE, "\n").into());

        // Insert at the beginning
        let new_green = self.0.green().splice_children(0..0, new_nodes);
        self.0 = SyntaxNode::new_root_mut(new_green);
    }

    /// Add a new document to the end of this YAML file
    pub fn push_document(&mut self, document: Document) {
        let children_count = self.0.children_with_tokens().count();
        let mut new_nodes = Vec::new();

        // Add separator if there are existing documents
        if self.0.children().count() > 0 {
            new_nodes.push(create_token_green(SyntaxKind::NEWLINE, "\n").into());
            new_nodes.push(create_token_green(SyntaxKind::DOC_START, "---").into());
            new_nodes.push(create_token_green(SyntaxKind::NEWLINE, "\n").into());
        }

        // Add the new document
        new_nodes.push(document.0.green().into());

        // Splice at the end of existing children
        let new_green = self
            .0
            .green()
            .splice_children(children_count..children_count, new_nodes);
        self.0 = SyntaxNode::new_root_mut(new_green);
    }

    /// Set a key-value pair in the first document (assumes first document is a mapping)
    pub fn set(&mut self, key: impl Into<ScalarValue>, value: impl Into<YamlValue>) {
        if let Some(mut doc) = self.document() {
            doc.set(key, value);
        }
    }

    /// Insert a key-value pair after a specific existing key in the first document
    ///
    /// This method accepts any YamlValue types for positioning key, new key, and value, supporting:
    /// - Scalars (strings, numbers, booleans, null)
    /// - Sequences (arrays/lists)
    /// - Mappings (nested objects)
    /// - Sets, ordered mappings, and pairs
    ///
    /// # Arguments
    /// * `after_key` - The key after which to insert the new pair (can be any YamlValue type)
    /// * `key` - The new key (can be any type that converts to YamlValue)
    /// * `value` - The new value (can be any type that converts to YamlValue)
    ///
    /// # Returns
    /// `true` if the reference key was found and insertion succeeded, `false` otherwise
    pub fn insert_after(
        &mut self,
        after_key: impl Into<YamlValue>,
        key: impl Into<YamlValue>,
        value: impl Into<YamlValue>,
    ) -> bool {
        if let Some(mut doc) = self.document() {
            doc.insert_after(after_key, key, value)
        } else {
            false
        }
    }

    /// Insert a key-value pair before a specific existing key in the first document
    ///
    /// This method accepts any YamlValue types for positioning key, new key, and value, supporting:
    /// - Scalars (strings, numbers, booleans, null)
    /// - Sequences (arrays/lists)
    /// - Mappings (nested objects)
    /// - Sets, ordered mappings, and pairs
    ///
    /// # Arguments
    /// * `before_key` - The key before which to insert the new pair (can be any YamlValue type)
    /// * `key` - The new key (can be any type that converts to YamlValue)
    /// * `value` - The new value (can be any type that converts to YamlValue)
    ///
    /// # Returns
    /// `true` if the reference key was found and insertion succeeded, `false` otherwise
    pub fn insert_before(
        &mut self,
        before_key: impl Into<YamlValue>,
        key: impl Into<YamlValue>,
        value: impl Into<YamlValue>,
    ) -> bool {
        if let Some(mut doc) = self.document() {
            doc.insert_before(before_key, key, value)
        } else {
            false
        }
    }

    /// Insert a key-value pair at a specific index in the first document
    ///
    /// This method accepts any YamlValue types for key and value, supporting:
    /// - Scalars (strings, numbers, booleans, null)
    /// - Sequences (arrays/lists)
    /// - Mappings (nested objects)
    /// - Sets, ordered mappings, and pairs
    ///
    /// # Arguments
    /// * `index` - The position at which to insert the new pair (0-based)
    /// * `key` - The new key (can be any type that converts to YamlValue)
    /// * `value` - The new value (can be any type that converts to YamlValue)
    pub fn insert_at_index(
        &mut self,
        index: usize,
        key: impl Into<YamlValue>,
        value: impl Into<YamlValue>,
    ) {
        if let Some(mut doc) = self.document() {
            doc.insert_at_index(index, key, value);
            // Mutations happen directly on the document, no need to replace
        } else {
            // If no document exists, create one without the --- marker for consistency
            // with normal parsed YAML
            let mut builder = GreenNodeBuilder::new();
            builder.start_node(SyntaxKind::DOCUMENT.into());
            builder.start_node(SyntaxKind::MAPPING.into());
            builder.finish_node(); // End MAPPING
            builder.finish_node(); // End DOCUMENT
            let doc = Document(SyntaxNode::new_root_mut(builder.finish()));

            // Add the document to the ROOT
            self.0.splice_children(0..0, vec![doc.0.into()]);

            // Now get the document again and insert
            if let Some(mut doc) = self.document() {
                doc.insert_at_index(index, key, value);
            }
        }
    }

    /// Helper method to recursively add node children to the yaml builder
    fn add_node_children_to_yaml(builder: &mut GreenNodeBuilder, node: &SyntaxNode) {
        for child in node.children_with_tokens() {
            match child {
                rowan::NodeOrToken::Node(child_node) => {
                    builder.start_node(child_node.kind().into());
                    Self::add_node_children_to_yaml(builder, &child_node);
                    builder.finish_node();
                }
                rowan::NodeOrToken::Token(token) => {
                    builder.token(token.kind().into(), token.text());
                }
            }
        }
    }

    /// Replace the first document with a new one
    fn replace_first_document(&mut self, new_doc: Document) {
        // Find the first document and replace it
        let children: Vec<_> = self.0.children_with_tokens().collect();
        let mut doc_range = None;

        for (i, child) in children.iter().enumerate() {
            if let Some(node) = child.as_node() {
                if Document::can_cast(node.kind()) {
                    doc_range = Some(i..i + 1);
                    break;
                }
            }
        }

        if let Some(range) = doc_range {
            let new_green = self
                .0
                .green()
                .splice_children(range, vec![new_doc.0.green().into()]);
            self.0 = SyntaxNode::new_root_mut(new_green);
        }
    }
}

impl Default for Document {
    fn default() -> Self {
        Self::new()
    }
}

impl Document {
    /// Create a new document
    pub fn new() -> Document {
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(SyntaxKind::DOCUMENT.into());
        // Add the document start marker "---"
        builder.token(SyntaxKind::DOC_START.into(), "---");
        builder.token(SyntaxKind::WHITESPACE.into(), "\n");
        builder.finish_node();
        Document(SyntaxNode::new_root_mut(builder.finish()))
    }

    /// Create a new document with an empty mapping
    pub fn new_mapping() -> Document {
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(SyntaxKind::DOCUMENT.into());
        // Add the document start marker "---"
        builder.token(SyntaxKind::DOC_START.into(), "---");
        builder.token(SyntaxKind::WHITESPACE.into(), "\n");
        builder.start_node(SyntaxKind::MAPPING.into());
        builder.finish_node(); // End MAPPING
        builder.finish_node(); // End DOCUMENT
        Document(SyntaxNode::new_root_mut(builder.finish()))
    }

    /// Load a document from a file
    pub fn load_from_file<P: AsRef<Path>>(path: P) -> YamlResult<Document> {
        let content = std::fs::read_to_string(path)?;
        let parsed = Yaml::parse(&content);
        Ok(parsed.tree().documents().next().unwrap_or_default())
    }

    /// Load a document from a file (alias for load_from_file)
    pub fn from_file<P: AsRef<Path>>(path: P) -> YamlResult<Document> {
        Self::load_from_file(path)
    }

    /// Save the document to a file, creating directories as needed
    pub fn save_to_file<P: AsRef<Path>>(&self, path: P) -> YamlResult<()> {
        let path = path.as_ref();
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent)?;
        }
        let mut content = self.to_yaml_string();
        // Ensure the file ends with a newline
        if !content.ends_with('\n') {
            content.push('\n');
        }
        std::fs::write(path, content)?;
        Ok(())
    }

    /// Get the root node of this document (could be mapping, sequence, or scalar)
    pub fn root_node(&self) -> Option<SyntaxNode> {
        self.0.children().find(|child| {
            matches!(
                child.kind(),
                SyntaxKind::MAPPING
                    | SyntaxKind::SEQUENCE
                    | SyntaxKind::SCALAR
                    | SyntaxKind::TAGGED_SCALAR
            )
        })
    }

    /// Get this document as a mapping, if it is one
    pub fn as_mapping(&self) -> Option<Mapping> {
        self.root_node().and_then(Mapping::cast)
    }

    /// Get this document as a mutable mapping, if it is one
    pub fn as_mapping_mut(&mut self) -> Option<Mapping> {
        // Since Mapping wraps a SyntaxNode which is already mutable, we can just return it
        self.root_node().and_then(Mapping::cast)
    }

    /// Get this document as a sequence, if it is one
    pub fn as_sequence(&self) -> Option<Sequence> {
        self.root_node().and_then(Sequence::cast)
    }

    /// Get this document as a scalar, if it is one
    pub fn as_scalar(&self) -> Option<Scalar> {
        self.root_node().and_then(Scalar::cast)
    }

    /// Convert the document to a YAML string
    pub fn to_yaml_string(&self) -> String {
        self.0.text().to_string()
    }

    /// Check if the document contains a key (assumes document is a mapping)
    pub fn contains_key(&self, key: &YamlValue) -> bool {
        self.as_mapping().is_some_and(|m| m.contains_key(key))
    }

    /// Get a value from the document (assumes document is a mapping)
    pub fn get(&self, key: &YamlValue) -> Option<SyntaxNode> {
        self.as_mapping().and_then(|m| m.get(key))
    }

    /// Set a scalar value in the document (assumes document is a mapping)
    pub fn set(&mut self, key: impl Into<ScalarValue>, value: impl Into<YamlValue>) {
        if let Some(mut mapping) = self.as_mapping_mut() {
            mapping.set(key, value);
            // Changes are applied directly via splice_children, no need to replace
        } else {
            // If document is not a mapping, create one
            let mut mapping = Mapping::new();
            mapping.set(key, value);
            *self = Self::from_mapping(mapping);
        }
    }

    /// Set a key-value pair with field ordering support
    /// If the key exists, updates its value. If the key doesn't exist, inserts it
    /// at the correct position based on the provided field order.
    /// Fields not in the order list are placed at the end.
    pub fn set_with_field_order(
        &mut self,
        key: impl Into<YamlValue>,
        value: impl Into<YamlValue>,
        field_order: &[&str],
    ) {
        if let Some(mut mapping) = self.as_mapping_mut() {
            mapping.set_with_field_order(key, value, field_order);
            // Changes are applied directly via splice_children, no need to replace
        } else {
            // If document is not a mapping, create one with the ordered field
            let mut mapping = Mapping::new();
            mapping.set_with_field_order(key, value, field_order);
            *self = Self::from_mapping(mapping);
        }
    }

    /// Set a raw string value in the document (no escaping)
    /// This method parses the raw YAML value into proper AST nodes
    pub fn set_raw(&mut self, key: &str, value: &str) {
        // Parse the raw YAML value to determine its actual type
        use std::str::FromStr;

        // Try to parse the value as YAML to get the proper structure
        if let Ok(parsed_yaml) = crate::Yaml::from_str(value) {
            if let Some(doc) = parsed_yaml.document() {
                if let Some(mapping) = doc.as_mapping() {
                    // If the value is a mapping, get the first value from it
                    if let Some((_, first_value)) = mapping.pairs().next() {
                        if let Some(_first_value_node) = first_value {
                            // Use the parsed node structure
                            if let Some(mut self_mapping) = self.as_mapping_mut() {
                                // For complex parsing, we still need to use the raw method for now
                                // TODO: Implement full AST-based insertion for complex values
                                self_mapping.set_raw(key, value);
                                // Changes are applied directly via splice_children
                                return;
                            }
                        }
                    }
                } else {
                    // It's a scalar value, use auto-typed parsing
                    self.set(ScalarValue::new(key), ScalarValue::from_yaml(value));
                    return;
                }
            }
        }

        // Fallback: treat as string scalar
        self.set(ScalarValue::new(key), ScalarValue::new(value));
    }

    /// Remove a key from the document (assumes document is a mapping)
    pub fn remove(&mut self, key: &YamlValue) -> bool {
        if let Some(mapping) = self.as_mapping() {
            if let Some(entry) = mapping.find_entry_by_key(key) {
                entry.discard();
                return true;
            }
        }
        false
    }

    /// Get all key nodes from the document (assumes document is a mapping)
    pub fn key_nodes(&self) -> Vec<SyntaxNode> {
        self.as_mapping()
            .map_or_else(Vec::new, |m| m.key_nodes().collect())
    }

    /// Get all keys from the document as YamlValues (assumes document is a mapping)
    pub fn keys(&self) -> Vec<YamlValue> {
        self.key_nodes().into_iter().filter_map(|key_node| {
            // KEY nodes contain the actual key content as children
            key_node.children().next()
                .and_then(|content| YamlValue::cast(content))
        }).collect()
    }

    /// Check if the document is empty
    pub fn is_empty(&self) -> bool {
        self.as_mapping().is_none_or(|m| m.is_empty())
    }

    /// Create a document from a mapping
    pub fn from_mapping(mapping: Mapping) -> Self {
        // Create a document directly from the mapping syntax node to avoid recursion
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(SyntaxKind::DOCUMENT.into());
        // Add the document start marker "---"
        builder.token(SyntaxKind::DOC_START.into(), "---");
        builder.token(SyntaxKind::WHITESPACE.into(), "\n");

        // Add the mapping with all its children
        builder.start_node(SyntaxKind::MAPPING.into());
        let mapping_green = mapping.0.green();
        for child in mapping_green.children() {
            match child {
                rowan::NodeOrToken::Node(n) => {
                    builder.start_node(n.kind().into());
                    Self::add_green_node_children(&mut builder, n);
                    builder.finish_node();
                }
                rowan::NodeOrToken::Token(t) => {
                    builder.token(t.kind().into(), t.text());
                }
            }
        }
        builder.finish_node(); // End MAPPING
        builder.finish_node(); // End DOCUMENT

        Document(SyntaxNode::new_root_mut(builder.finish()))
    }

    /// Helper method to recursively add green node children to the builder
    fn add_green_node_children(builder: &mut GreenNodeBuilder, node: &rowan::GreenNodeData) {
        for child in node.children() {
            match child {
                rowan::NodeOrToken::Node(n) => {
                    builder.start_node(n.kind().into());
                    Self::add_green_node_children(builder, n);
                    builder.finish_node();
                }
                rowan::NodeOrToken::Token(t) => {
                    builder.token(t.kind().into(), t.text());
                }
            }
        }
    }

    /// Helper method to recursively add node children to the builder
    fn add_node_children(builder: &mut GreenNodeBuilder, node: &SyntaxNode) {
        for child in node.children_with_tokens() {
            match child {
                rowan::NodeOrToken::Node(child_node) => {
                    builder.start_node(child_node.kind().into());
                    Self::add_node_children(builder, &child_node);
                    builder.finish_node();
                }
                rowan::NodeOrToken::Token(token) => {
                    builder.token(token.kind().into(), token.text());
                }
            }
        }
    }

    /// Insert a key-value pair after a specific existing key (assumes document is a mapping)
    /// This version accepts any YamlValue types for positioning key, new key, and value
    /// This method preserves existing formatting, comments, and whitespace
    pub fn insert_after(
        &mut self,
        after_key: impl Into<YamlValue>,
        key: impl Into<YamlValue>,
        value: impl Into<YamlValue>,
    ) -> bool {
        let after_key_value = after_key.into();
        let key_value = key.into();
        let value_value = value.into();

        // Convert keys to strings for comparison (only keys need string conversion for lookup)
        let after_key_str = after_key_value.to_string();

        // Check if document is a mapping
        if let Some(mut mapping) = self.as_mapping() {
            // Use the AST-preserving method on the mapping - changes propagate automatically
            mapping.insert_after_preserving(&after_key_str, key_value, value_value)
        } else {
            false
        }
    }

    /// Insert a key-value pair before a specific existing key (assumes document is a mapping)
    /// This version accepts any YamlValue types for positioning key, new key, and value
    /// This method preserves existing formatting, comments, and whitespace
    pub fn insert_before(
        &mut self,
        before_key: impl Into<YamlValue>,
        key: impl Into<YamlValue>,
        value: impl Into<YamlValue>,
    ) -> bool {
        let before_key_value = before_key.into();
        let key_value = key.into();
        let value_value = value.into();

        // Convert keys to strings for comparison (only keys need string conversion for lookup)
        let before_key_str = before_key_value.to_string();

        // Check if document is a mapping
        if let Some(mut mapping) = self.as_mapping() {
            // Use the AST-preserving method on the mapping - changes propagate automatically
            mapping.insert_before_preserving(&before_key_str, key_value, value_value)
        } else {
            false
        }
    }

    /// Helper to build a MAPPING_ENTRY node
    fn build_mapping_entry(key: &str, value: &YamlValue, indent: usize) -> rowan::GreenNode {
        let mut entry_builder = GreenNodeBuilder::new();
        entry_builder.start_node(SyntaxKind::MAPPING_ENTRY.into());

        // Add key
        entry_builder.start_node(SyntaxKind::KEY.into());
        entry_builder.start_node(SyntaxKind::SCALAR.into());
        entry_builder.token(SyntaxKind::STRING.into(), key);
        entry_builder.finish_node(); // SCALAR
        entry_builder.finish_node(); // KEY

        // Add colon and space
        entry_builder.token(SyntaxKind::COLON.into(), ":");
        entry_builder.token(SyntaxKind::WHITESPACE.into(), " ");

        // Add value
        Self::build_value_content(&mut entry_builder, value, indent);

        entry_builder.finish_node(); // MAPPING_ENTRY
        entry_builder.finish()
    }

    /// Helper to build a SEQUENCE_ENTRY node
    fn build_sequence_entry(value: &YamlValue, indent: usize) -> rowan::GreenNode {
        let mut entry_builder = GreenNodeBuilder::new();
        entry_builder.start_node(SyntaxKind::SEQUENCE_ENTRY.into());
        entry_builder.token(SyntaxKind::DASH.into(), "-");
        entry_builder.token(SyntaxKind::WHITESPACE.into(), " ");

        // Add the item value
        match value {
            YamlValue::Scalar(s) => {
                entry_builder.token(SyntaxKind::STRING.into(), &s.to_yaml_string());
            }
            _ => {
                // For complex nested items, we'd need deeper recursion
                entry_builder.token(SyntaxKind::STRING.into(), &value.to_yaml_string(indent));
            }
        }

        entry_builder.finish_node(); // SEQUENCE_ENTRY
        entry_builder.finish()
    }

    /// Helper to build KEY content based on YamlValue type
    fn build_key_content(builder: &mut GreenNodeBuilder, key: &YamlValue, indent: usize) {
        builder.start_node(SyntaxKind::KEY.into());

        match key {
            YamlValue::Scalar(s) => {
                builder.start_node(SyntaxKind::SCALAR.into());
                builder.token(SyntaxKind::STRING.into(), &s.to_yaml_string());
                builder.finish_node(); // SCALAR
            }
            _ => {
                // Complex key (sequence, mapping, etc.) - use explicit key syntax
                builder.token(SyntaxKind::QUESTION.into(), "?");
                builder.token(SyntaxKind::WHITESPACE.into(), " ");
                builder.token(SyntaxKind::STRING.into(), &key.to_yaml_string(indent));
            }
        }

        builder.finish_node(); // KEY
    }

    /// Helper to build VALUE content based on YamlValue type
    fn build_value_content(builder: &mut GreenNodeBuilder, value: &YamlValue, indent: usize) {
        builder.start_node(SyntaxKind::VALUE.into());
        Self::build_value_content_only(builder, value, indent);
        builder.finish_node(); // VALUE
    }
    
    /// Build value content without the VALUE wrapper node
    fn build_value_content_only(builder: &mut GreenNodeBuilder, value: &YamlValue, indent: usize) {
        match value {
            YamlValue::Scalar(s) => {
                builder.start_node(SyntaxKind::SCALAR.into());
                builder.token(SyntaxKind::STRING.into(), &s.to_yaml_string());
                builder.finish_node(); // SCALAR
            }
            YamlValue::Mapping(map) => {
                if map.is_empty() {
                    builder.token(SyntaxKind::STRING.into(), "{}");
                } else {
                    // Complex mapping - for now use string representation
                    // TODO: Implement full recursive AST building
                    builder.token(SyntaxKind::STRING.into(), &value.to_yaml_string(indent));
                }
            }
            YamlValue::Sequence(seq) => {
                if seq.is_empty() {
                    builder.token(SyntaxKind::STRING.into(), "[]");
                } else {
                    // Complex sequence - for now use string representation
                    // TODO: Implement full recursive AST building
                    builder.token(SyntaxKind::STRING.into(), &value.to_yaml_string(indent));
                }
            }
            YamlValue::Set(_) | YamlValue::OrderedMapping(_) | YamlValue::Pairs(_) => {
                // Special collections use tagged syntax
                builder.token(SyntaxKind::STRING.into(), &value.to_yaml_string(indent));
            }
        }
    }

    /// Insert a key-value pair at a specific index (assumes document is a mapping)
    /// This version accepts any YamlValue types for both key and value
    /// This method preserves existing formatting, comments, and whitespace
    pub fn insert_at_index(
        &mut self,
        index: usize,
        key: impl Into<YamlValue>,
        value: impl Into<YamlValue>,
    ) {
        let key_value = key.into();
        let value_value = value.into();

        // Convert key to string for comparison
        let key_str = key_value.to_string();

        // Find the mapping node within the document
        for child in self.0.children() {
            if child.kind() == SyntaxKind::MAPPING {
                // Work directly on the mapping's children
                let mapping_children: Vec<_> = child.children_with_tokens().collect();

                // Check if key exists and update it
                for mapping_child in mapping_children.iter() {
                    if let Some(node) = mapping_child.as_node() {
                        if node.kind() == SyntaxKind::MAPPING_ENTRY {
                            // Look for KEY node inside MAPPING_ENTRY
                            if let Some(key_node) =
                                node.children().find(|n| n.kind() == SyntaxKind::KEY)
                            {
                                let key_text = key_node.text().to_string();
                                if key_text.trim() == key_str.trim() {
                                    // Found existing key, find its VALUE node within this MAPPING_ENTRY
                                    for value_node in node.children() {
                                        if value_node.kind() == SyntaxKind::VALUE {
                                            // Build new VALUE node using the helper
                                            let mut value_builder = GreenNodeBuilder::new();
                                            Document::build_value_content(
                                                &mut value_builder,
                                                &value_value,
                                                2,
                                            );
                                            let new_value_node =
                                                SyntaxNode::new_root_mut(value_builder.finish());

                                            // Find position of VALUE node within MAPPING_ENTRY
                                            let entry_children: Vec<_> =
                                                node.children_with_tokens().collect();
                                            for (k, entry_child) in
                                                entry_children.iter().enumerate()
                                            {
                                                if let Some(n) = entry_child.as_node() {
                                                    if n.kind() == SyntaxKind::VALUE {
                                                        // Replace the VALUE node within the MAPPING_ENTRY
                                                        node.splice_children(
                                                            k..k + 1,
                                                            vec![new_value_node.into()],
                                                        );
                                                        return;
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                // Key doesn't exist, add it to the mapping at the specified index
                // Count the number of key-value pairs to determine where to insert
                let mut pair_count = 0;
                let mut insertion_point = None;
                let mut current_pos = 0;

                for element in child.children_with_tokens() {
                    if let Some(node) = element.as_node() {
                        if node.kind() == SyntaxKind::MAPPING_ENTRY {
                            if pair_count == index {
                                // Found the position to insert before this entry
                                insertion_point = Some(current_pos);
                                break;
                            }
                            pair_count += 1;
                        }
                    }
                    current_pos += 1;
                }

                // If we didn't find a position (index >= number of pairs), append at end
                if insertion_point.is_none() {
                    insertion_point = Some(child.children_with_tokens().count());
                }

                let insert_pos = insertion_point.unwrap();
                let mut new_elements = Vec::new();

                // Check if we need a leading newline
                let needs_leading_newline = if insert_pos > 0 && pair_count > 0 {
                    // Check if there's already a newline before the insertion point
                    let elements: Vec<_> = child.children_with_tokens().collect();
                    if insert_pos > 0 {
                        if let Some(prev_element) = elements.get(insert_pos - 1) {
                            if let Some(token) = prev_element.as_token() {
                                token.kind() != SyntaxKind::NEWLINE
                            } else {
                                true // Previous element is a node, need newline
                            }
                        } else {
                            true
                        }
                    } else {
                        false
                    }
                } else {
                    false
                };

                if needs_leading_newline {
                    let mut nl_builder = GreenNodeBuilder::new();
                    nl_builder.start_node(SyntaxKind::ROOT.into());
                    nl_builder.token(SyntaxKind::NEWLINE.into(), "\n");
                    nl_builder.finish_node();
                    let nl_node = SyntaxNode::new_root_mut(nl_builder.finish());
                    if let Some(token) = nl_node.first_token() {
                        new_elements.push(token.into());
                    }
                }

                // Build a MAPPING_ENTRY node containing the key-value pair
                let mut entry_builder = GreenNodeBuilder::new();
                entry_builder.start_node(SyntaxKind::MAPPING_ENTRY.into());

                // Add the key as a KEY node
                entry_builder.start_node(SyntaxKind::KEY.into());
                entry_builder.start_node(SyntaxKind::SCALAR.into());
                entry_builder.token(SyntaxKind::STRING.into(), &key_str);
                entry_builder.finish_node(); // SCALAR
                entry_builder.finish_node(); // KEY

                // Add colon and space
                entry_builder.token(SyntaxKind::COLON.into(), ":");
                entry_builder.token(SyntaxKind::WHITESPACE.into(), " ");

                // Determine current indentation by looking at existing entries
                let current_indent = if pair_count > 0 {
                    // Find indentation of existing entries
                    for child_elem in child.children_with_tokens() {
                        if let Some(node) = child_elem.as_node() {
                            if node.kind() == SyntaxKind::MAPPING_ENTRY {
                                // Look for WHITESPACE or INDENT before this entry
                                if let Some(prev) = node.prev_sibling_or_token() {
                                    if let Some(token) = prev.as_token() {
                                        if token.kind() == SyntaxKind::WHITESPACE
                                            || token.kind() == SyntaxKind::INDENT
                                        {
                                            break; // Found an indented entry, use 2-space indent
                                        }
                                    }
                                }
                                break;
                            }
                        }
                    }
                    2 // Default to 2-space indent for nested content
                } else {
                    0 // No indentation for top-level
                };

                // Use the factored helper to build the value
                Self::build_value_content(&mut entry_builder, &value_value, current_indent);

                entry_builder.finish_node(); // MAPPING_ENTRY

                let entry_node = SyntaxNode::new_root_mut(entry_builder.finish());
                new_elements.push(entry_node.into());

                // Add trailing newline (every entry should end with a newline)
                let mut nl_builder = GreenNodeBuilder::new();
                nl_builder.start_node(SyntaxKind::ROOT.into());
                nl_builder.token(SyntaxKind::NEWLINE.into(), "\n");
                nl_builder.finish_node();
                let nl_node = SyntaxNode::new_root_mut(nl_builder.finish());
                if let Some(token) = nl_node.first_token() {
                    new_elements.push(token.into());
                }

                // Insert at the specified position
                child.splice_children(insert_pos..insert_pos, new_elements);
                return;
            }
        }

        // If no mapping exists, create one
        let mut mapping = Mapping::new();
        mapping.insert_at_index_preserving(index, &key_str, &value_value);
        *self = Self::from_mapping(mapping);
    }

    /// Set a string value (convenient method)
    pub fn set_string(&mut self, key: &str, value: &str) {
        self.set(key, ScalarValue::new(value));
    }

    /// Set a value of any YAML type (scalar, sequence, mapping)
    pub fn set_value(&mut self, key: impl Into<ScalarValue>, value: YamlValue) {
        if let Some(mut mapping) = self.as_mapping() {
            mapping.set_value(key, value);
        } else {
            // Create a new mapping if document is empty
            let mut mapping = Mapping::new();
            mapping.set_value(key, value);
            *self = Self::from_mapping(mapping);
        }
    }

    /// Get a string value for a key (returns None if not found)
    pub fn get_string(&self, key: &str) -> Option<String> {
        let key_yaml = YamlValue::Scalar(ScalarValue::new(key));
        self.get(&key_yaml).map(|node| {
            if node.kind() == SyntaxKind::VALUE {
                // Check if VALUE node contains a TAGGED_SCALAR child
                for child in node.children() {
                    if let Some(tagged_scalar) = TaggedScalar::cast(child) {
                        return tagged_scalar.value().map(|s| s.value()).unwrap_or_default();
                    }
                }

                // Fallback: VALUE nodes contain raw text, parse it like a scalar
                let text = node.text().to_string();

                // Use the same parsing logic as Scalar::as_string()
                if text.starts_with('"') && text.ends_with('"') {
                    // Double-quoted string - handle escape sequences
                    ScalarValue::parse_escape_sequences(&text[1..text.len() - 1])
                } else if text.starts_with('\'') && text.ends_with('\'') {
                    // Single-quoted string - only handle '' -> '
                    text[1..text.len() - 1].replace("''", "'")
                } else {
                    // Plain string - trim trailing newlines that might have been added by save_to_file
                    text.trim_end_matches('\n').to_string()
                }
            } else if let Some(tagged_scalar) = TaggedScalar::cast(node.clone()) {
                // TAGGED_SCALAR nodes - return just the value part
                tagged_scalar.value().map(|s| s.value()).unwrap_or_default()
            } else if let Some(scalar) = Scalar::cast(node.clone()) {
                // SCALAR nodes need to be processed - use as_string to handle quote stripping
                scalar.as_string()
            } else {
                // For other node types, use raw text - trim trailing newlines
                node.text().to_string().trim_end_matches('\n').to_string()
            }
        })
    }

    /// Check if a key's value is an array/sequence
    pub fn is_array(&self, key: &str) -> bool {
        let key_yaml = YamlValue::Scalar(ScalarValue::new(key));
        self.get(&key_yaml)
            .map(|node| node.kind() == SyntaxKind::SEQUENCE)
            .unwrap_or(false)
    }

    /// Get the first element of an array as a string (for array to string conversion)
    pub fn get_array_first_string(&self, key: &str) -> Option<String> {
        let key_yaml = YamlValue::Scalar(ScalarValue::new(key));
        self.get(&key_yaml).and_then(|node| {
            if node.kind() == SyntaxKind::SEQUENCE {
                // Try to extract first element from sequence
                // This is a simplified implementation
                let text = node.text().to_string();
                if text.trim().starts_with('[') && text.trim().ends_with(']') {
                    // Flow style array [item1, item2]
                    let inner = text.trim().strip_prefix('[')?.strip_suffix(']')?;
                    let first_item = inner.split(',').next()?.trim();
                    Some(first_item.trim_matches('"').trim_matches('\'').to_string())
                } else {
                    // Block style array - use the sequence items iterator
                    if let Some(sequence) = Sequence::cast(node) {
                        sequence.items().next().and_then(|first_item| {
                            if first_item.kind() == SyntaxKind::SCALAR {
                                Scalar::cast(first_item).map(|scalar| scalar.value())
                            } else {
                                // For complex first items, return a simple representation
                                Some(first_item.text().to_string().trim().to_string())
                            }
                        })
                    } else {
                        None
                    }
                }
            } else {
                None
            }
        })
    }

    /// Reorder fields in the document according to the specified order
    /// Fields not in the order list will appear after the ordered fields
    pub fn reorder_fields(&mut self, order: &[YamlValue]) {
        if let Some(mapping) = self.as_mapping_mut() {
            let mut ordered_entries = Vec::new();
            let mut remaining_entries = Vec::new();
            
            // Find entries in the specified order
            for order_key in order {
                if let Some(entry) = mapping.find_entry_by_key(order_key) {
                    ordered_entries.push(entry.0.clone());
                }
            }
            
            // Find remaining entries not in the order list
            for entry in mapping.entries() {
                let mut found_in_order = false;
                for order_key in order {
                    if mapping.entry_has_key(&entry, order_key) {
                        found_in_order = true;
                        break;
                    }
                }
                if !found_in_order {
                    remaining_entries.push(entry.0.clone());
                }
            }
            
            // Build new children list: ordered entries + remaining entries
            let mut new_children = Vec::new();
            for entry_node in ordered_entries.into_iter().chain(remaining_entries.into_iter()) {
                new_children.push(entry_node.into());
            }
            
            // Replace all mapping children with reordered ones
            let children_count = mapping.0.children_with_tokens().count();
            mapping.0.splice_children(0..children_count, new_children);
        }
    }

    /// Get directives that may be associated with this document
    /// Note: In YAML, directives typically appear at the file level, not document level
    pub fn get_parent_directives(&self) -> Vec<Directive> {
        // This is a placeholder implementation
        // In a real implementation, you might need to traverse up to find parent directives
        Vec::new()
    }

    /// Set the field ordering for the document (assumes document is a mapping)
    /// Fields will be reordered according to the provided order. Any fields not
    /// in the order list will be placed at the end in their original order.
    pub fn set_field_order(&mut self, field_order: &[YamlValue]) {
        self.reorder_fields(field_order);
    }

    /// Add a directive to the beginning of the parent YAML
    /// This is a convenience method that operates on the document's parent if available
    pub fn add_directive(&mut self, _directive_text: &str) -> Result<(), &'static str> {
        // This is a placeholder - in practice, directives should be added to the root YAML node
        Err("Directives must be added to the root YAML node, not individual documents")
    }

    /// Create a new document syntax node with a mapping containing multiple entries
    fn create_mapping_with_all_entries(&self, pairs: Vec<(String, String)>) -> SyntaxNode {
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(SyntaxKind::DOCUMENT.into());
        // Add the document start marker "---"
        builder.token(SyntaxKind::DOC_START.into(), "---");
        builder.token(SyntaxKind::WHITESPACE.into(), "\n");
        builder.start_node(SyntaxKind::MAPPING.into());

        for (i, (key, value)) in pairs.iter().enumerate() {
            if i > 0 {
                // Add newline between entries
                builder.token(SyntaxKind::WHITESPACE.into(), "\n");
            }

            // Create MAPPING_ENTRY node
            builder.start_node(SyntaxKind::MAPPING_ENTRY.into());

            // Add KEY node
            builder.start_node(SyntaxKind::KEY.into());
            builder.start_node(SyntaxKind::SCALAR.into());
            builder.token(SyntaxKind::STRING.into(), key);
            builder.finish_node(); // SCALAR
            builder.finish_node(); // KEY

            // Add colon and space
            builder.token(SyntaxKind::COLON.into(), ":");
            builder.token(SyntaxKind::WHITESPACE.into(), " ");

            // Add VALUE node
            builder.start_node(SyntaxKind::VALUE.into());
            builder.start_node(SyntaxKind::SCALAR.into());
            builder.token(SyntaxKind::STRING.into(), value);
            builder.finish_node(); // SCALAR
            builder.finish_node(); // VALUE

            builder.finish_node(); // MAPPING_ENTRY
        }

        builder.finish_node(); // MAPPING
        builder.finish_node(); // DOCUMENT

        SyntaxNode::new_root(builder.finish())
    }

    /// Validate this document against a YAML schema
    ///
    /// # Examples
    ///
    /// ```rust
    /// use yaml_edit::{Document, Schema, SchemaValidator};
    ///
    /// let yaml = r#"
    /// name: "John"
    /// age: 30
    /// active: true
    /// "#;
    ///
    /// let parsed = yaml.parse::<yaml_edit::Yaml>().unwrap();
    /// let doc = parsed.document().unwrap();
    ///
    /// // Validate against JSON schema
    /// let validator = SchemaValidator::json();
    /// match doc.validate_schema(&validator) {
    ///     Ok(_) => println!("Valid JSON schema"),
    ///     Err(errors) => {
    ///         for error in errors {
    ///             println!("Validation error: {}", error);
    ///         }
    ///     }
    /// }
    /// ```
    pub fn validate_schema(
        &self,
        validator: &crate::schema::SchemaValidator,
    ) -> crate::schema::ValidationResult<()> {
        validator.validate(self)
    }

    /// Validate this document against the Failsafe schema (strings, mappings, sequences only)
    ///
    /// The Failsafe schema is the most restrictive YAML schema, allowing only
    /// strings, mappings, and sequences. All scalars are treated as strings.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use yaml_edit::Parse;
    ///
    /// let yaml = r#"
    /// name: John
    /// items:
    ///   - apple
    ///   - banana
    /// "#;
    ///
    /// let parsed = yaml.parse::<yaml_edit::Yaml>().unwrap();
    /// let doc = parsed.document().unwrap();
    ///
    /// match doc.validate_failsafe() {
    ///     Ok(_) => println!("Valid Failsafe schema"),
    ///     Err(errors) => {
    ///         for error in errors {
    ///             println!("Validation error: {}", error);
    ///         }
    ///     }
    /// }
    /// ```
    pub fn validate_failsafe(&self) -> crate::schema::ValidationResult<()> {
        let validator = crate::schema::SchemaValidator::failsafe();
        validator.validate(self)
    }

    /// Validate this document against the JSON schema (JSON-compatible types only)
    ///
    /// The JSON schema allows strings, integers, floats, booleans, null values,
    /// arrays, and objects - all types that are compatible with JSON.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use yaml_edit::Parse;
    ///
    /// let yaml = r#"
    /// name: "John"
    /// age: 30
    /// height: 5.9
    /// active: true
    /// metadata: null
    /// "#;
    ///
    /// let parsed = yaml.parse::<yaml_edit::Yaml>().unwrap();
    /// let doc = parsed.document().unwrap();
    ///
    /// match doc.validate_json() {
    ///     Ok(_) => println!("Valid JSON schema"),
    ///     Err(errors) => {
    ///         for error in errors {
    ///             println!("Validation error: {}", error);
    ///         }
    ///     }
    /// }
    /// ```
    pub fn validate_json(&self) -> crate::schema::ValidationResult<()> {
        let validator = crate::schema::SchemaValidator::json();
        validator.validate(self)
    }

    /// Validate this document against the Core schema (all YAML 1.2 types)
    ///
    /// The Core schema is the most permissive YAML schema, allowing all YAML 1.2
    /// types including timestamps, regex patterns, binary data, and more.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use yaml_edit::Parse;
    ///
    /// let yaml = r#"
    /// name: "John"
    /// created: 2023-12-25T10:30:45Z
    /// pattern: !!regex '\d{3}-\d{4}'
    /// data: !!binary "SGVsbG8gV29ybGQ="
    /// "#;
    ///
    /// let parsed = yaml.parse::<yaml_edit::Yaml>().unwrap();
    /// let doc = parsed.document().unwrap();
    ///
    /// match doc.validate_core() {
    ///     Ok(_) => println!("Valid Core schema"),
    ///     Err(errors) => {
    ///         for error in errors {
    ///             println!("Validation error: {}", error);
    ///         }
    ///     }
    /// }
    /// ```
    pub fn validate_core(&self) -> crate::schema::ValidationResult<()> {
        let validator = crate::schema::SchemaValidator::core();
        validator.validate(self)
    }

    /// Check if this document can be coerced to match a specific schema
    ///
    /// This method checks if the document values can be automatically converted
    /// to match the target schema without strict type enforcement.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use yaml_edit::{Parse, SchemaValidator};
    ///
    /// let yaml = r#"
    /// count: "42"     # string that looks like number
    /// enabled: "true" # string that looks like boolean
    /// "#;
    ///
    /// let parsed = yaml.parse::<yaml_edit::Yaml>().unwrap();
    /// let doc = parsed.document().unwrap();
    /// let json_validator = SchemaValidator::json();
    ///
    /// match doc.can_coerce_to_schema(&json_validator) {
    ///     Ok(_) => println!("Document can be coerced to JSON schema"),
    ///     Err(errors) => {
    ///         for error in errors {
    ///             println!("Coercion error: {}", error);
    ///         }
    ///     }
    /// }
    /// ```
    pub fn can_coerce_to_schema(
        &self,
        validator: &crate::schema::SchemaValidator,
    ) -> crate::schema::ValidationResult<()> {
        validator.can_coerce(self)
    }

    /// Replace the document's content with a modified mapping
    pub fn replace_with_mapping(&mut self, mapping: Mapping) {
        // Use surgical splice_children to replace just the mapping node, preserving everything else
        let children: Vec<_> = self.0.children_with_tokens().collect();
        let mut mapping_position = None;

        // Find the position of the existing MAPPING node
        for (i, child) in children.iter().enumerate() {
            if let Some(node) = child.as_node() {
                if node.kind() == SyntaxKind::MAPPING {
                    mapping_position = Some(i);
                    break;
                }
            }
        }

        if let Some(pos) = mapping_position {
            // Replace the existing mapping with the modified one directly using splice_children
            let new_mapping_green = mapping.0.green().into();
            let new_elements = vec![rowan::NodeOrToken::Node(new_mapping_green)];

            // Use surgical splice to replace only the mapping node, preserving all formatting/comments
            let new_green = self.0.green().splice_children(pos..pos + 1, new_elements);
            self.0 = SyntaxNode::new_root_mut(new_green);
        } else {
            // No existing mapping found - need to add one
            // TODO: Handle edge case where document has no mapping node
            // For now, just add the mapping after any existing document start marker
            let mut insert_pos = 0;
            let mut need_newline = false;

            for (i, child) in children.iter().enumerate() {
                if let Some(token) = child.as_token() {
                    if token.kind() == SyntaxKind::DOC_START {
                        insert_pos = i + 1;
                        need_newline = true;
                        break;
                    }
                }
            }

            let new_mapping_green = mapping.0.green().into();
            let mut new_elements = vec![];

            if need_newline {
                new_elements.push(rowan::NodeOrToken::Token(rowan::GreenToken::new(
                    SyntaxKind::NEWLINE.into(),
                    "\n",
                )));
            }
            new_elements.push(rowan::NodeOrToken::Node(new_mapping_green));

            let new_green = self
                .0
                .green()
                .splice_children(insert_pos..insert_pos, new_elements);
            self.0 = SyntaxNode::new_root_mut(new_green);
        }
    }

    /// Create a new document syntax node with a mapping containing multiple entries (supports YamlValue strings)
    fn create_mapping_with_all_entries_value(&self, pairs: Vec<(String, String)>) -> SyntaxNode {
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(SyntaxKind::DOCUMENT.into());
        // Add the document start marker "---"
        builder.token(SyntaxKind::DOC_START.into(), "---");
        builder.token(SyntaxKind::WHITESPACE.into(), "\n");
        builder.start_node(SyntaxKind::MAPPING.into());

        for (i, (key, value)) in pairs.iter().enumerate() {
            if i > 0 {
                // Add newline between entries
                builder.token(SyntaxKind::WHITESPACE.into(), "\n");
            }

            // Add key
            builder.start_node(SyntaxKind::KEY.into());
            builder.start_node(SyntaxKind::SCALAR.into());
            builder.token(SyntaxKind::VALUE.into(), key);
            builder.finish_node(); // SCALAR
            builder.finish_node(); // KEY

            // Add colon and space
            builder.token(SyntaxKind::COLON.into(), ":");
            builder.token(SyntaxKind::WHITESPACE.into(), " ");

            // Add value - this might be a complex YAML structure
            if value.contains('\n')
                || value.starts_with('[')
                || value.starts_with('{')
                || value.starts_with("!!")
            {
                // Complex value that needs special handling
                builder.token(SyntaxKind::WHITESPACE.into(), "\n");
                // Indent the complex value
                for line in value.lines() {
                    builder.token(SyntaxKind::WHITESPACE.into(), "  ");
                    builder.token(SyntaxKind::VALUE.into(), line);
                    builder.token(SyntaxKind::WHITESPACE.into(), "\n");
                }
            } else {
                // Simple scalar value
                builder.start_node(SyntaxKind::VALUE.into());
                builder.token(SyntaxKind::VALUE.into(), value);
                builder.finish_node();
            }
        }

        builder.finish_node(); // MAPPING
        builder.finish_node(); // DOCUMENT

        SyntaxNode::new_root(builder.finish())
    }
}

impl Default for Mapping {
    fn default() -> Self {
        Self::new()
    }
}

impl Mapping {
    /// Get all keys in this mapping as strings
    pub fn keys(&self) -> impl Iterator<Item = String> + '_ {
        self.entries().filter_map(|entry| {
            entry.key().and_then(|key_node| {
                // Extract string value from KEY node
                if let Some(scalar) = key_node.children().find_map(Scalar::cast) {
                    Some(scalar.value())
                } else {
                    // Fallback to text for complex keys
                    Some(key_node.text().to_string())
                }
            })
        })
    }
    
    /// Get all key-value pairs in this mapping
    pub fn pairs(&self) -> impl Iterator<Item = (Option<SyntaxNode>, Option<SyntaxNode>)> + '_ {
        // Create an iterator that lazily finds key-value pairs
        struct PairsIterator<'a> {
            children: Vec<SyntaxNode>,
            index: usize,
            _marker: std::marker::PhantomData<&'a ()>,
        }

        impl<'a> Iterator for PairsIterator<'a> {
            type Item = (Option<SyntaxNode>, Option<SyntaxNode>);

            fn next(&mut self) -> Option<Self::Item> {
                while self.index < self.children.len() {
                    let child = &self.children[self.index];

                    // Only process MAPPING_ENTRY nodes
                    if child.kind() == SyntaxKind::MAPPING_ENTRY {
                        self.index += 1;

                        // Extract key and value from the MAPPING_ENTRY
                        let key_node = child.children().find(|n| n.kind() == SyntaxKind::KEY);
                        let value_node = child.children().find(|n| n.kind() == SyntaxKind::VALUE);

                        // Return the actual KEY and VALUE nodes, not just their children
                        // This allows complex keys (sequences, mappings) to be preserved
                        let key = key_node;
                        let value = value_node;

                        return Some((key, value));
                    }

                    // Skip non-MAPPING_ENTRY nodes
                    self.index += 1;
                }
                None
            }
        }

        PairsIterator {
            children: self.0.children().collect(),
            index: 0,
            _marker: std::marker::PhantomData,
        }
    }

    /// Get the value for a specific key as YamlValue
    pub fn get(&self, key: &YamlValue) -> Option<SyntaxNode> {
        self.find_entry_by_key(key).and_then(|entry| entry.value())
    }

    /// Get a value as a Mapping if it is one
    pub fn get_mapping(&self, key: &str) -> Option<Mapping> {
        let key_yaml = YamlValue::Scalar(ScalarValue::new(key));
        self.get(&key_yaml).and_then(|value_node| {
            // The value node is a VALUE node, look for MAPPING inside it
            value_node.children().find_map(Mapping::cast)
        })
    }

    /// Modify a nested mapping in place
    /// Returns true if the key exists and contains a mapping that was successfully modified
    pub fn modify_mapping<F>(&mut self, key: &str, f: F) -> bool
    where
        F: FnOnce(&mut Mapping),
    {
        // Find the MAPPING_ENTRY for this key
        let children: Vec<_> = self.0.children_with_tokens().collect();
        for (i, child) in children.iter().enumerate() {
            if let Some(node) = child.as_node() {
                if node.kind() == SyntaxKind::MAPPING_ENTRY {
                    if let Some(key_node) = node.children().find(|n| n.kind() == SyntaxKind::KEY) {
                        if key_node.text().to_string().trim() == key.trim() {
                            // Found the entry, now find the VALUE node
                            if let Some(value_node) =
                                node.children().find(|n| n.kind() == SyntaxKind::VALUE)
                            {
                                // Check if the value is a mapping
                                if let Some(mapping_node) = value_node
                                    .children()
                                    .find(|n| n.kind() == SyntaxKind::MAPPING)
                                {
                                    // Create a mutable Mapping and apply the function
                                    let mut mapping = Mapping(mapping_node.clone());
                                    f(&mut mapping);

                                    // Replace the old MAPPING_ENTRY with updated one
                                    let entry_children: Vec<_> =
                                        node.children_with_tokens().collect();
                                    let mut builder = GreenNodeBuilder::new();
                                    builder.start_node(SyntaxKind::MAPPING_ENTRY.into());

                                    for entry_child in entry_children {
                                        match entry_child {
                                            rowan::NodeOrToken::Node(n)
                                                if n.kind() == SyntaxKind::VALUE =>
                                            {
                                                // Replace the VALUE node
                                                builder.start_node(SyntaxKind::VALUE.into());

                                                // First copy all non-MAPPING children from the original VALUE node (preserving structure)
                                                for value_child in n.children_with_tokens() {
                                                    match value_child {
                                                        rowan::NodeOrToken::Node(child_node)
                                                            if child_node.kind()
                                                                == SyntaxKind::MAPPING =>
                                                        {
                                                            // Replace the MAPPING node with our updated mapping
                                                            self.copy_node_to_builder(
                                                                &mut builder,
                                                                &mapping.0,
                                                            );
                                                        }
                                                        rowan::NodeOrToken::Node(child_node) => {
                                                            // Copy other nodes as-is (preserving formatting)
                                                            self.copy_node_to_builder(
                                                                &mut builder,
                                                                &child_node,
                                                            );
                                                        }
                                                        rowan::NodeOrToken::Token(token) => {
                                                            // Copy tokens as-is (preserving newlines, indents, etc)
                                                            builder.token(
                                                                token.kind().into(),
                                                                token.text(),
                                                            );
                                                        }
                                                    }
                                                }

                                                builder.finish_node(); // VALUE
                                            }
                                            rowan::NodeOrToken::Node(n) => {
                                                self.copy_node_to_builder(&mut builder, &n);
                                            }
                                            rowan::NodeOrToken::Token(t) => {
                                                builder.token(t.kind().into(), t.text());
                                            }
                                        }
                                    }

                                    builder.finish_node(); // MAPPING_ENTRY
                                    let new_entry = SyntaxNode::new_root_mut(builder.finish());
                                    self.0.splice_children(i..i + 1, vec![new_entry.into()]);
                                    return true;
                                }
                            }
                        }
                    }
                }
            }
        }
        false
    }

    /// Get a value as a Sequence if it is one
    pub fn get_sequence(&self, key: &str) -> Option<Sequence> {
        let key_yaml = YamlValue::Scalar(ScalarValue::new(key));
        self.get(&key_yaml).and_then(|value_node| {
            // The value node is a VALUE node, look for SEQUENCE inside it
            value_node.children().find_map(Sequence::cast)
        })
    }

    /// Check if the mapping contains a specific key
    pub fn contains_key(&self, key: &YamlValue) -> bool {
        self.find_entry_by_key(key).is_some()
    }

    /// Get all key nodes in the mapping
    pub fn key_nodes(&self) -> impl Iterator<Item = SyntaxNode> + '_ {
        self.pairs().filter_map(|(k, _)| k)
    }

    /// Check if the mapping is empty
    pub fn is_empty(&self) -> bool {
        self.pairs().next().is_none()
    }

    /// Create a new empty mapping
    pub fn new() -> Self {
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(SyntaxKind::MAPPING.into());
        builder.finish_node();
        Mapping(SyntaxNode::new_root_mut(builder.finish()))
    }

    /// Reorder fields according to the specified order
    /// Fields not in the order list will appear after the ordered fields
    pub fn reorder_fields(&mut self, order: &[&str]) {
        // Collect all current key-value pairs
        let pairs_iter = self.pairs();
        let mut all_pairs: Vec<(String, String)> = Vec::with_capacity(16); // Pre-allocate reasonable size
        for (key_opt, value_opt) in pairs_iter {
            if let (Some(key), Some(value)) = (key_opt, value_opt) {
                let value_text = value.text().to_string();
                // Extract scalar from KEY node
                let key_text = if let Some(scalar_node) = key.children().find(|child| child.kind() == SyntaxKind::SCALAR) {
                    if let Some(scalar) = Scalar::cast(scalar_node) {
                        scalar.value()
                    } else {
                        key.text().to_string()
                    }
                } else {
                    key.text().to_string()
                };
                all_pairs.push((key_text, value_text.trim().to_string()));
            }
        }

        // Clear current mapping and rebuild in order
        *self = Self::new();

        // First add fields in the specified order
        for &field_name in order {
            if let Some(pos) = all_pairs.iter().position(|(k, _)| k == field_name) {
                let (_, value) = all_pairs.remove(pos);
                self.set_raw(field_name, &value);
            }
        }

        // Then add any remaining fields that weren't in the order list
        for (key, value) in all_pairs {
            self.set_raw(&key, &value);
        }
    }

    /// Set a value of any YAML type (scalar, sequence, mapping)
    pub fn set_value(&mut self, key: impl Into<ScalarValue>, value: YamlValue) {
        match value {
            YamlValue::Scalar(scalar) => {
                self.set(key, scalar);
            }
            YamlValue::Sequence(items) => {
                // Create a flow-style sequence for now
                let items_str: Vec<String> = items
                    .into_iter()
                    .map(|item| match item {
                        YamlValue::Scalar(s) => s.to_yaml_string(),
                        YamlValue::Sequence(_) => "[...]".to_string(), // Nested sequences
                        YamlValue::Mapping(_) => "{...}".to_string(),  // Nested mappings
                        YamlValue::Set(_) => "!!set{...}".to_string(),
                        YamlValue::OrderedMapping(_) => "!!omap[...]".to_string(),
                        YamlValue::Pairs(_) => "!!pairs[...]".to_string(),
                    })
                    .collect();
                let sequence_yaml = format!("[{}]", items_str.join(", "));
                let key_str = key.into().to_yaml_string();
                self.set_raw(&key_str, &sequence_yaml);
            }
            YamlValue::Mapping(map) => {
                // Create a flow-style mapping for now
                let pairs: Vec<String> = map
                    .into_iter()
                    .map(|(k, v)| {
                        let value_str = match v {
                            YamlValue::Scalar(s) => s.to_yaml_string(),
                            YamlValue::Sequence(_) => "[...]".to_string(),
                            YamlValue::Mapping(_) => "{...}".to_string(),
                            YamlValue::Set(_) => "!!set{...}".to_string(),
                            YamlValue::OrderedMapping(_) => "!!omap[...]".to_string(),
                            YamlValue::Pairs(_) => "!!pairs[...]".to_string(),
                        };
                        format!("{}: {}", k, value_str)
                    })
                    .collect();
                let mapping_yaml = format!("{{{}}}", pairs.join(", "));
                let key_str = key.into().to_yaml_string();
                self.set_raw(&key_str, &mapping_yaml);
            }
            YamlValue::Set(set) => {
                // Convert set to YAML representation
                let yaml_str = YamlValue::Set(set).to_yaml_string(0);
                let key_str = key.into().to_yaml_string();
                self.set_raw(&key_str, &yaml_str);
            }
            YamlValue::OrderedMapping(pairs) => {
                // Convert ordered mapping to YAML representation
                let yaml_str = YamlValue::OrderedMapping(pairs).to_yaml_string(0);
                let key_str = key.into().to_yaml_string();
                self.set_raw(&key_str, &yaml_str);
            }
            YamlValue::Pairs(pairs) => {
                // Convert pairs to YAML representation
                let yaml_str = YamlValue::Pairs(pairs).to_yaml_string(0);
                let key_str = key.into().to_yaml_string();
                self.set_raw(&key_str, &yaml_str);
            }
        }
    }
}

impl Sequence {
    /// Get all items in this sequence
    pub fn items(&self) -> impl Iterator<Item = SyntaxNode> + '_ {
        self.0.children().filter_map(|child| {
            if child.kind() == SyntaxKind::SEQUENCE_ENTRY {
                // Look for the actual item within the SEQUENCE_ENTRY
                // Skip DASH and WHITESPACE tokens, find the actual value node
                child.children().find(|n| {
                    matches!(
                        n.kind(),
                        SyntaxKind::SCALAR | SyntaxKind::MAPPING | SyntaxKind::SEQUENCE
                    )
                })
            } else {
                None
            }
        })
    }
}

impl TaggedScalar {
    /// Get the tag part of this tagged scalar (e.g., "!custom" from "!custom value")
    pub fn tag(&self) -> Option<String> {
        // Find the tag token in the children
        for child in self.0.children_with_tokens() {
            if let rowan::NodeOrToken::Token(token) = child {
                if token.kind() == SyntaxKind::TAG {
                    return Some(token.text().to_string());
                }
            }
        }
        None
    }

    /// Get the value part of this tagged scalar (without the tag)
    pub fn value(&self) -> Option<Scalar> {
        // Find the nested SCALAR node
        for child in self.0.children() {
            if child.kind() == SyntaxKind::SCALAR {
                return Scalar::cast(child);
            }
        }
        None
    }

    /// Get the string value of this tagged scalar (just the value part)
    pub fn as_string(&self) -> Option<String> {
        if let Some(scalar) = self.value() {
            Some(scalar.value())
        } else {
            // Handle cases where the value might be nested deeper
            self.extract_deepest_string_value()
        }
    }

    /// Extract the deepest string value, handling nested tag structures
    fn extract_deepest_string_value(&self) -> Option<String> {
        Self::find_string_token_recursive(&self.0)
    }

    /// Recursively search for the first STRING token in the tree
    fn find_string_token_recursive(node: &rowan::SyntaxNode<crate::yaml::Lang>) -> Option<String> {
        // Check tokens first
        for child in node.children_with_tokens() {
            if let rowan::NodeOrToken::Token(token) = child {
                if token.kind() == SyntaxKind::STRING {
                    return Some(token.text().to_string());
                }
            }
        }

        // Then check child nodes recursively
        for child in node.children() {
            if let Some(result) = Self::find_string_token_recursive(&child) {
                return Some(result);
            }
        }

        None
    }

    /// Extract a set from this tagged scalar if it has a !!set tag
    pub fn as_set(&self) -> Option<Set> {
        Set::cast(self.0.clone())
    }

    /// Extract ordered mapping from this tagged scalar if it has a !!omap tag
    pub fn as_ordered_mapping(&self) -> Option<Vec<(YamlValue, YamlValue)>> {
        if self.tag() != Some("!!omap".to_string()) {
            return None;
        }

        let mut pairs = Vec::new();

        // Find the sequence node within this tagged scalar
        for child in self.0.children() {
            if let Some(sequence) = Sequence::cast(child) {
                // Extract single-key mappings from the sequence
                for item in sequence.items() {
                    if let Some(mapping) = Mapping::cast(item) {
                        // Each mapping should have exactly one key-value pair
                        let mapping_pairs: Vec<_> = mapping.pairs().collect();
                        if mapping_pairs.len() == 1 {
                            let (key_opt, value_opt) = &mapping_pairs[0];
                            if let (Some(key), Some(value)) = (key_opt, value_opt) {
                                let key_yaml_value = self.yaml_node_to_yaml_value(key.clone());
                                let value_yaml_value = self.yaml_node_to_yaml_value(value.clone());
                                pairs.push((key_yaml_value, value_yaml_value));
                            }
                        }
                    }
                }
                break;
            }
        }

        Some(pairs)
    }

    /// Extract pairs from this tagged scalar if it has a !!pairs tag
    pub fn as_pairs(&self) -> Option<Vec<(YamlValue, YamlValue)>> {
        if self.tag() != Some("!!pairs".to_string()) {
            return None;
        }

        let mut pairs = Vec::new();

        // Find the sequence node within this tagged scalar
        for child in self.0.children() {
            if let Some(sequence) = Sequence::cast(child) {
                // Extract key-value pairs from the sequence
                for item in sequence.items() {
                    if let Some(mapping) = Mapping::cast(item) {
                        // Each mapping should have exactly one key-value pair
                        let mapping_pairs: Vec<_> = mapping.pairs().collect();
                        if mapping_pairs.len() == 1 {
                            let (key_opt, value_opt) = &mapping_pairs[0];
                            if let (Some(key), Some(value)) = (key_opt, value_opt) {
                                let key_yaml_value = self.yaml_node_to_yaml_value(key.clone());
                                let value_yaml_value = self.yaml_node_to_yaml_value(value.clone());
                                pairs.push((key_yaml_value, value_yaml_value));
                            }
                        }
                    }
                }
                break;
            }
        }

        Some(pairs)
    }

    /// Convert a YAML syntax node to a YamlValue (helper for special collections)
    fn yaml_node_to_yaml_value(&self, node: rowan::SyntaxNode<Lang>) -> crate::value::YamlValue {
        match node.kind() {
            SyntaxKind::KEY => {
                // KEY nodes contain the actual key content as a child
                if let Some(child) = node.children().next() {
                    return self.yaml_node_to_yaml_value(child);
                }
                // If no child found, return the text
                crate::value::YamlValue::scalar(crate::scalar::ScalarValue::new(node.text().to_string()))
            }
            SyntaxKind::VALUE => {
                // VALUE nodes contain the actual value as a child
                if let Some(child) = node.children().next() {
                    return self.yaml_node_to_yaml_value(child);
                }
                // If no child found, return null
                crate::value::YamlValue::scalar(crate::scalar::ScalarValue::null())
            }
            SyntaxKind::SCALAR => {
                if let Some(scalar) = Scalar::cast(node) {
                    crate::value::YamlValue::scalar(crate::scalar::ScalarValue::new(
                        scalar.as_string(),
                    ))
                } else {
                    crate::value::YamlValue::scalar(crate::scalar::ScalarValue::null())
                }
            }
            SyntaxKind::SEQUENCE => {
                let mut items = Vec::new();
                if let Some(sequence) = Sequence::cast(node) {
                    for item in sequence.items() {
                        items.push(self.yaml_node_to_yaml_value(item));
                    }
                }
                crate::value::YamlValue::from_sequence(items)
            }
            SyntaxKind::MAPPING => {
                let mut map = std::collections::BTreeMap::new();
                if let Some(mapping) = Mapping::cast(node) {
                    for (key_opt, value_opt) in mapping.pairs() {
                        if let (Some(key), Some(value)) = (key_opt, value_opt) {
                            let key_yaml = self.yaml_node_to_yaml_value(key);
                            let value_yaml = self.yaml_node_to_yaml_value(value);
                            // For BTreeMap, we need string keys, so convert YamlValue to string
                            let key_string = key_yaml.to_yaml_string(0);
                            map.insert(key_string, value_yaml);
                        }
                    }
                }
                crate::value::YamlValue::from_mapping(map)
            }
            _ => crate::value::YamlValue::scalar(crate::scalar::ScalarValue::null()),
        }
    }
}

impl Clone for Scalar {
    fn clone(&self) -> Self {
        Scalar(self.0.clone())
    }
}

impl Scalar {
    /// Get the string value of this scalar
    pub fn value(&self) -> String {
        self.0.text().to_string()
    }

    /// Get the string representation of this scalar, properly unquoted and unescaped
    pub fn as_string(&self) -> String {
        let text = self.value();

        // Handle quoted strings
        if text.starts_with('"') && text.ends_with('"') {
            // Double-quoted string - handle escape sequences
            ScalarValue::parse_escape_sequences(&text[1..text.len() - 1])
        } else if text.starts_with('\'') && text.ends_with('\'') {
            // Single-quoted string - only handle '' -> '
            text[1..text.len() - 1].replace("''", "'")
        } else {
            // Plain string
            text
        }
    }

    /// Check if this scalar is quoted
    pub fn is_quoted(&self) -> bool {
        let text = self.value();
        (text.starts_with('"') && text.ends_with('"'))
            || (text.starts_with('\'') && text.ends_with('\''))
    }

    /// Get the unquoted value
    pub fn unquoted_value(&self) -> String {
        let text = self.value();
        if self.is_quoted() {
            // Simple unquoting - a full implementation would handle escapes
            text[1..text.len() - 1].to_string()
        } else {
            text
        }
    }
}

impl FromStr for Yaml {
    type Err = crate::ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Yaml::parse(s).to_result()
    }
}

/// Internal parser state
struct Parser {
    tokens: Vec<(SyntaxKind, String)>,
    current_token_index: usize,
    builder: GreenNodeBuilder<'static>,
    errors: Vec<String>,
    positioned_errors: Vec<PositionedParseError>,
    in_flow_context: bool,
    /// Registry of anchors defined in the current document
    anchor_registry: HashMap<String, rowan::GreenNode>,
    /// Track currently resolving aliases to detect circular references
    resolving_aliases: Vec<String>,
    /// Error recovery context for better error messages
    error_context: ErrorRecoveryContext,
    /// Track if we're parsing a value (to prevent nested implicit mappings)
    in_value_context: bool,
}

impl Parser {
    fn new(text: &str) -> Self {
        let lexed = lex(text);
        let mut tokens = Vec::new();

        for (kind, token_text) in lexed {
            tokens.push((kind, token_text.to_string()));
        }

        // Reverse tokens so we can use pop() to get the next token
        let token_count = tokens.len();
        tokens.reverse();

        Self {
            tokens,
            current_token_index: token_count,
            builder: GreenNodeBuilder::new(),
            errors: Vec::new(),
            positioned_errors: Vec::new(),
            in_flow_context: false,
            anchor_registry: HashMap::new(),
            resolving_aliases: Vec::new(),
            error_context: ErrorRecoveryContext::new(text.to_string()),
            in_value_context: false,
        }
    }

    fn parse(mut self) -> ParsedYaml {
        self.builder.start_node(SyntaxKind::ROOT.into());

        self.skip_ws_and_newlines();

        // Parse any directives at the beginning
        while self.current() == Some(SyntaxKind::DIRECTIVE) {
            self.parse_directive();
            self.skip_ws_and_newlines();
        }

        // Parse documents
        // Always parse at least one document
        if self.current().is_some() && self.current() != Some(SyntaxKind::EOF) {
            self.parse_document();
            self.skip_ws_and_newlines();

            // Parse additional documents (can have directives before each)
            while self.current() == Some(SyntaxKind::DOC_START)
                || self.current() == Some(SyntaxKind::DIRECTIVE)
            {
                // Parse any directives before this document
                while self.current() == Some(SyntaxKind::DIRECTIVE) {
                    self.parse_directive();
                    self.skip_ws_and_newlines();
                }

                // Parse the document if we have content
                if self.current() == Some(SyntaxKind::DOC_START)
                    || (self.current().is_some() && self.current() != Some(SyntaxKind::EOF))
                {
                    self.parse_document();
                    self.skip_ws_and_newlines();
                } else {
                    break;
                }
            }
        }

        self.builder.finish_node();

        ParsedYaml {
            green_node: self.builder.finish(),
            errors: self.errors,
            positioned_errors: self.positioned_errors,
        }
    }

    fn parse_document(&mut self) {
        self.builder.start_node(SyntaxKind::DOCUMENT.into());

        // Handle document start marker
        if self.current() == Some(SyntaxKind::DOC_START) {
            self.bump();
            self.skip_ws_and_newlines();
        }

        // Parse the document content
        if self.current().is_some()
            && self.current() != Some(SyntaxKind::DOC_END)
            && self.current() != Some(SyntaxKind::DOC_START)
        {
            self.parse_value();
        }

        // Handle document end marker
        if self.current() == Some(SyntaxKind::DOC_END) {
            self.bump();
        }

        self.builder.finish_node();
    }

    fn parse_value(&mut self) {
        self.parse_value_with_base_indent(0);
    }

    fn parse_value_with_base_indent(&mut self, base_indent: usize) {
        match self.current() {
            Some(SyntaxKind::COMMENT) => {
                // Preserve the comment and continue parsing the actual value
                self.bump(); // consume and preserve the comment
                self.skip_ws_and_newlines(); // skip any whitespace/newlines after comment
                                             // Now parse the actual value
                self.parse_value_with_base_indent(base_indent);
            }
            Some(SyntaxKind::DASH) if !self.in_flow_context => {
                self.parse_sequence_with_base_indent(base_indent)
            }
            Some(SyntaxKind::ANCHOR) => self.parse_anchored_value(),
            Some(SyntaxKind::REFERENCE) => self.parse_alias(),
            Some(SyntaxKind::TAG) => self.parse_tagged_value(),
            Some(SyntaxKind::MERGE_KEY) => {
                // Merge key is always a mapping
                self.parse_mapping();
            }
            Some(SyntaxKind::QUESTION) => {
                // Explicit key indicator - parse complex mapping
                self.parse_explicit_key_mapping();
            }
            Some(SyntaxKind::PIPE) => self.parse_literal_block_scalar(),
            Some(SyntaxKind::GREATER) => self.parse_folded_block_scalar(),
            Some(
                SyntaxKind::STRING
                | SyntaxKind::INT
                | SyntaxKind::FLOAT
                | SyntaxKind::BOOL
                | SyntaxKind::NULL,
            ) => {
                // In flow context, always parse as scalar
                // In block context, check if it's a mapping key
                // But not if we're already in a value context (prevents implicit nested mappings)
                if !self.in_flow_context && !self.in_value_context && self.is_mapping_key() {
                    self.parse_mapping_with_base_indent(base_indent);
                } else {
                    self.parse_scalar();
                }
            }
            Some(SyntaxKind::LEFT_BRACKET) => {
                // Check if this is a complex key in a mapping
                // But not if we're already in a value context
                if !self.in_flow_context && !self.in_value_context && self.is_complex_mapping_key()
                {
                    self.parse_complex_key_mapping();
                } else {
                    self.parse_flow_sequence();
                }
            }
            Some(SyntaxKind::LEFT_BRACE) => {
                // Check if this is a complex key in a mapping
                // But not if we're already in a value context
                if !self.in_flow_context && !self.in_value_context && self.is_complex_mapping_key()
                {
                    self.parse_complex_key_mapping();
                } else {
                    self.parse_flow_mapping();
                }
            }
            Some(SyntaxKind::INDENT) => {
                // We have an indented block - consume the indent and see what follows
                self.bump(); // consume INDENT
                self.parse_value(); // parse whatever comes after the indent
            }
            _ => self.parse_scalar(),
        }
    }

    fn parse_anchored_value(&mut self) {
        let mut anchor_name = None;

        // Extract anchor name from the token
        if self.current() == Some(SyntaxKind::ANCHOR) {
            if let Some((_, token_text)) = self.tokens.last() {
                // Extract anchor name from "&anchor_name" format
                if let Some(name) = token_text.strip_prefix('&') {
                    anchor_name = Some(name.to_string());
                }
            }
            self.bump(); // consume anchor token
            self.skip_whitespace();
        }

        // Parse the actual value that is being anchored
        let _value_start = self.builder.checkpoint();
        self.parse_value();

        // Register the anchor if we have a name
        if let Some(name) = anchor_name {
            // Get the green node for the value we just parsed
            // This is a simplified approach - in a full implementation, we'd need to capture
            // the exact node that was created for the anchored value
            // For now, we'll just store a marker that the anchor exists
            let placeholder_node = {
                let mut temp_builder = GreenNodeBuilder::new();
                temp_builder.start_node(SyntaxKind::SCALAR.into());
                temp_builder.token(SyntaxKind::STRING.into(), "anchored_value_placeholder");
                temp_builder.finish_node();
                temp_builder.finish()
            };
            self.anchor_registry.insert(name, placeholder_node);
        }
    }

    fn parse_alias(&mut self) {
        let mut alias_name = None;

        // Extract alias name from the token
        if self.current() == Some(SyntaxKind::REFERENCE) {
            if let Some((_, token_text)) = self.tokens.last() {
                // Extract alias name from "*alias_name" format
                if let Some(name) = token_text.strip_prefix('*') {
                    alias_name = Some(name.to_string());
                }
            }
        }

        // Check if the anchor exists in our registry and validate
        if let Some(name) = &alias_name {
            if !self.anchor_registry.contains_key(name) {
                self.add_error(format!("Undefined alias: {}", name));
            } else if self.resolving_aliases.contains(name) {
                // Circular reference detected
                self.add_error(format!("Circular reference detected for alias: {}", name));
            } else {
                // Track that we're resolving this alias
                self.resolving_aliases.push(name.clone());
            }
        }

        // Create a scalar node and just consume the reference token
        // The token itself already contains the full "*alias_name" text
        self.builder.start_node(SyntaxKind::SCALAR.into());
        if self.current() == Some(SyntaxKind::REFERENCE) {
            self.bump(); // This preserves the original "*alias_name" token
        }
        self.builder.finish_node();

        // Pop the alias from the resolving stack if we added it
        if let Some(name) = alias_name {
            if let Some(pos) = self.resolving_aliases.iter().position(|x| x == &name) {
                self.resolving_aliases.remove(pos);
            }
        }
    }

    fn parse_scalar(&mut self) {
        self.builder.start_node(SyntaxKind::SCALAR.into());

        // Handle quotes
        if matches!(
            self.current(),
            Some(SyntaxKind::QUOTE | SyntaxKind::SINGLE_QUOTE)
        ) {
            let quote_type = self.current().unwrap();
            self.bump(); // opening quote

            // Consume all tokens until the closing quote
            while self.current().is_some() && self.current() != Some(quote_type) {
                self.bump();
            }

            if self.current() == Some(quote_type) {
                self.bump(); // closing quote
            } else {
                let expected_quote = if quote_type == SyntaxKind::QUOTE {
                    "\""
                } else {
                    "'"
                };
                let current_text = self.current_text();
                let error_msg = self.create_detailed_error(
                    "Unterminated quoted string",
                    &format!("closing quote {}", expected_quote),
                    current_text.as_deref(),
                );
                self.add_error_and_recover(error_msg, quote_type);
            }
        } else {
            // Handle typed scalar tokens from lexer
            if matches!(
                self.current(),
                Some(
                    SyntaxKind::STRING
                        | SyntaxKind::UNTERMINATED_STRING
                        | SyntaxKind::INT
                        | SyntaxKind::FLOAT
                        | SyntaxKind::BOOL
                        | SyntaxKind::NULL
                )
            ) {
                // Check for unterminated string and add error
                if self.current() == Some(SyntaxKind::UNTERMINATED_STRING) {
                    self.add_error("Unterminated quoted string".to_string());
                }
                if !self.in_flow_context {
                    // For plain scalars in block context, consume all tokens on the same line
                    // This handles complex values like timestamps with spaces
                    let mut last_was_whitespace = false;
                    while let Some(kind) = self.current() {
                        if matches!(kind, SyntaxKind::NEWLINE | SyntaxKind::COMMENT) {
                            // If the last token was whitespace and we hit a comment,
                            // don't consume that whitespace - it belongs with the comment
                            if last_was_whitespace && kind == SyntaxKind::COMMENT {
                                // Need to undo the last whitespace consumption
                                // Can't undo easily, so we'll have to handle this differently
                            }
                            break;
                        }
                        // In block context, stop at flow collection delimiters
                        if matches!(
                            kind,
                            SyntaxKind::LEFT_BRACKET
                                | SyntaxKind::LEFT_BRACE
                                | SyntaxKind::RIGHT_BRACKET
                                | SyntaxKind::RIGHT_BRACE
                                | SyntaxKind::COMMA
                        ) {
                            break;
                        }
                        
                        // Check ahead to see if next token is a comment
                        if kind == SyntaxKind::WHITESPACE {
                            // Look ahead to see if a comment follows
                            if self.tokens.len() >= 2 {
                                let next_kind = self.tokens[self.tokens.len() - 2].0;
                                if next_kind == SyntaxKind::COMMENT {
                                    // Don't consume this whitespace, it precedes a comment
                                    break;
                                }
                            }
                        }
                        
                        last_was_whitespace = kind == SyntaxKind::WHITESPACE;
                        self.bump();
                    }
                } else {
                    // In flow context, only consume the current token
                    self.bump();
                }
            } else {
                // Fallback: consume tokens until we hit structure
                while let Some(kind) = self.current() {
                    if matches!(
                        kind,
                        SyntaxKind::NEWLINE
                            | SyntaxKind::DASH
                            | SyntaxKind::COMMENT
                            | SyntaxKind::DOC_START
                            | SyntaxKind::DOC_END
                    ) {
                        break;
                    }

                    // In flow context, colons are allowed in scalars (for IPv6, URLs, etc.)
                    // In block context, stop at colons as they indicate mapping structure
                    if kind == SyntaxKind::COLON {
                        if self.in_flow_context {
                            // In flow context, allow colons in scalars but stop at delimiters
                            // No break here - continue consuming
                        } else {
                            // In block context, stop at colons (mapping structure)
                            break;
                        }
                    }

                    // In flow context, stop at flow collection delimiters
                    if self.in_flow_context
                        && matches!(
                            kind,
                            SyntaxKind::LEFT_BRACKET
                                | SyntaxKind::RIGHT_BRACKET
                                | SyntaxKind::LEFT_BRACE
                                | SyntaxKind::RIGHT_BRACE
                                | SyntaxKind::COMMA
                        )
                    {
                        break;
                    }
                    self.bump();
                }
            }
        }

        self.builder.finish_node();
    }

    fn parse_tagged_value(&mut self) {
        // Peek at the tag to determine what kind of collection to parse
        let tag_text = self.peek_tag_text();

        match tag_text.as_deref() {
            Some("!!set") => self.parse_tagged_set(),
            Some("!!omap") => self.parse_tagged_omap(),
            Some("!!pairs") => self.parse_tagged_pairs(),
            _ => {
                // Default tagged scalar behavior
                self.builder.start_node(SyntaxKind::TAGGED_SCALAR.into());
                self.bump(); // TAG token

                // Skip any whitespace after the tag
                while matches!(self.current(), Some(SyntaxKind::WHITESPACE)) {
                    self.bump();
                }

                // Parse the value that follows the tag as a nested scalar
                self.builder.start_node(SyntaxKind::SCALAR.into());
                self.parse_tagged_scalar_content();
                self.builder.finish_node();

                self.builder.finish_node();
            }
        }
    }

    fn peek_tag_text(&self) -> Option<String> {
        if let Some((kind, text)) = self.tokens.last() {
            if *kind == SyntaxKind::TAG {
                return Some(text.clone());
            }
        }
        None
    }

    fn parse_tagged_set(&mut self) {
        self.parse_tagged_collection(true); // true = parse as mapping
    }

    fn parse_tagged_omap(&mut self) {
        self.parse_tagged_collection(false); // false = parse as sequence
    }

    fn parse_tagged_pairs(&mut self) {
        self.parse_tagged_collection(false); // false = parse as sequence
    }

    fn parse_tagged_collection(&mut self, is_mapping: bool) {
        self.builder.start_node(SyntaxKind::TAGGED_SCALAR.into());

        // Consume the tag
        self.bump(); // TAG token

        // Skip any whitespace after the tag
        while matches!(self.current(), Some(SyntaxKind::WHITESPACE)) {
            self.bump();
        }

        // Parse the following structure based on type
        match self.current() {
            Some(SyntaxKind::LEFT_BRACE) if is_mapping => self.parse_flow_mapping(),
            Some(SyntaxKind::LEFT_BRACKET) if !is_mapping => self.parse_flow_sequence(),
            Some(SyntaxKind::NEWLINE) => {
                self.bump(); // consume newline
                             // Check if next token is indent (for indented content)
                if self.current() == Some(SyntaxKind::INDENT) {
                    self.bump(); // consume indent
                }
                if is_mapping {
                    self.parse_mapping();
                } else {
                    self.parse_sequence();
                }
            }
            _ => {
                if is_mapping {
                    self.parse_mapping();
                } else {
                    self.parse_sequence();
                }
            }
        }

        self.builder.finish_node();
    }

    fn parse_tagged_scalar_content(&mut self) {
        // Parse the actual value after the tag
        match self.current() {
            Some(SyntaxKind::QUOTE | SyntaxKind::SINGLE_QUOTE) => {
                // Handle quoted strings
                let quote_type = self.current().unwrap();
                self.bump(); // opening quote

                while self.current().is_some() && self.current() != Some(quote_type) {
                    self.bump();
                }

                if self.current() == Some(quote_type) {
                    self.bump(); // closing quote
                } else {
                    let expected_quote = if quote_type == SyntaxKind::QUOTE {
                        "\""
                    } else {
                        "'"
                    };
                    let current_text = self.current_text();
                    let error_msg = self.create_detailed_error(
                        "Unterminated quoted string in flow collection",
                        &format!("closing quote {}", expected_quote),
                        current_text.as_deref(),
                    );
                    self.add_error_and_recover(error_msg, quote_type);
                }
            }
            Some(SyntaxKind::PIPE) => {
                // Handle literal block scalar
                self.bump(); // consume PIPE
                self.parse_block_scalar_header();
                self.parse_block_scalar_content();
            }
            Some(SyntaxKind::GREATER) => {
                // Handle folded block scalar
                self.bump(); // consume GREATER
                self.parse_block_scalar_header();
                self.parse_block_scalar_content();
            }
            Some(
                SyntaxKind::STRING
                | SyntaxKind::INT
                | SyntaxKind::FLOAT
                | SyntaxKind::BOOL
                | SyntaxKind::NULL,
            ) => {
                // Handle typed literals
                self.bump();
            }
            _ => {
                // Handle plain scalars - consume until we hit structure or newline
                while let Some(kind) = self.current() {
                    if matches!(
                        kind,
                        SyntaxKind::NEWLINE
                            | SyntaxKind::COLON
                            | SyntaxKind::DASH
                            | SyntaxKind::COMMENT
                            | SyntaxKind::DOC_START
                            | SyntaxKind::DOC_END
                            | SyntaxKind::COMMA
                            | SyntaxKind::RIGHT_BRACKET
                            | SyntaxKind::RIGHT_BRACE
                    ) {
                        break;
                    }
                    self.bump();
                }
            }
        }
    }

    fn parse_literal_block_scalar(&mut self) {
        self.builder.start_node(SyntaxKind::SCALAR.into());
        self.bump(); // consume PIPE
        self.parse_block_scalar_header();
        self.parse_block_scalar_content();
        self.builder.finish_node();
    }

    fn parse_folded_block_scalar(&mut self) {
        self.builder.start_node(SyntaxKind::SCALAR.into());
        self.bump(); // consume GREATER
        self.parse_block_scalar_header();
        self.parse_block_scalar_content();
        self.builder.finish_node();
    }

    fn parse_block_scalar_header(&mut self) {
        // TODO: Handle explicit indentation indicators (1-9) and chomping indicators (+, -)
        // For now, just consume any following characters on the same line
        while let Some(kind) = self.current() {
            match kind {
                SyntaxKind::NEWLINE | SyntaxKind::COMMENT => break,
                _ => self.bump(),
            }
        }

        // Consume the newline after the header
        if self.current() == Some(SyntaxKind::NEWLINE) {
            self.bump();
        }
    }

    fn parse_block_scalar_content(&mut self) {
        // Consume all indented content that follows
        while let Some(kind) = self.current() {
            match kind {
                // Stop at document markers or unindented content
                SyntaxKind::DOC_START | SyntaxKind::DOC_END => break,
                // Continue consuming content and whitespace
                _ => self.bump(),
            }

            // Check if we've reached unindented content (end of block scalar)
            if self.is_at_unindented_content() {
                break;
            }
        }
    }

    fn is_at_unindented_content(&self) -> bool {
        // Simple heuristic: if we see a newline followed by a non-whitespace character
        // at the beginning of a line, we've reached unindented content
        if self.current() == Some(SyntaxKind::NEWLINE) {
            // Look ahead to the next token
            let mut i = self.current_token_index + 1;
            while i < self.tokens.len() {
                match self.tokens[i].0 {
                    SyntaxKind::WHITESPACE | SyntaxKind::INDENT => {
                        i += 1;
                        continue;
                    }
                    SyntaxKind::NEWLINE => return false, // Empty line, continue
                    SyntaxKind::COMMENT => return false, // Comment line, continue
                    _ => {
                        // Check if this is indented content by looking at the token text
                        let token_text = &self.tokens[i].1;
                        return !token_text.starts_with("  ") && !token_text.starts_with("\t");
                    }
                }
            }
        }
        false
    }

    fn parse_mapping(&mut self) {
        self.parse_mapping_with_base_indent(0);
    }

    fn parse_mapping_with_base_indent(&mut self, base_indent: usize) {
        self.builder.start_node(SyntaxKind::MAPPING.into());
        self.error_context.push_context(ParseContext::Mapping);

        while self.current().is_some() {
            if !self.is_mapping_key() && !self.is_complex_mapping_key() {
                break;
            }

            // Check for complex keys (sequences or mappings as keys)
            if self.current() == Some(SyntaxKind::LEFT_BRACKET)
                || self.current() == Some(SyntaxKind::LEFT_BRACE)
            {
                self.builder.start_node(SyntaxKind::KEY.into());
                if self.current() == Some(SyntaxKind::LEFT_BRACKET) {
                    self.parse_flow_sequence();
                } else if self.current() == Some(SyntaxKind::LEFT_BRACE) {
                    self.parse_flow_mapping();
                }
                self.builder.finish_node();

                self.skip_ws_and_newlines();

                if self.current() == Some(SyntaxKind::COLON) {
                    self.bump();
                    self.skip_whitespace();

                    self.builder.start_node(SyntaxKind::VALUE.into());
                    if self.current().is_some() && self.current() != Some(SyntaxKind::NEWLINE) {
                        self.parse_value();
                    } else if self.current() == Some(SyntaxKind::NEWLINE) {
                        self.bump();
                        if self.current() == Some(SyntaxKind::INDENT) {
                            self.bump();
                            self.parse_value();
                        }
                    }
                    self.builder.finish_node();
                } else {
                    let error_msg = self.create_detailed_error(
                        "Missing colon in mapping",
                        "':' after key",
                        self.current_text().as_deref(),
                    );
                    self.add_error_and_recover(error_msg, SyntaxKind::COLON);
                }
            }
            // Check for explicit key indicator
            else if self.current() == Some(SyntaxKind::QUESTION) {
                // Parse explicit key
                self.bump(); // consume '?'
                self.skip_whitespace();

                self.builder.start_node(SyntaxKind::KEY.into());
                if self.current().is_some() && self.current() != Some(SyntaxKind::NEWLINE) {
                    self.parse_value();
                }
                self.builder.finish_node();

                self.skip_ws_and_newlines();

                // Parse value if there's a colon
                if self.current() == Some(SyntaxKind::COLON) {
                    self.bump(); // consume ':'
                    self.skip_whitespace();

                    self.builder.start_node(SyntaxKind::VALUE.into());
                    if self.current().is_some() && self.current() != Some(SyntaxKind::NEWLINE) {
                        self.parse_value();
                    } else if self.current() == Some(SyntaxKind::NEWLINE) {
                        self.bump(); // consume newline
                        if self.current() == Some(SyntaxKind::INDENT) {
                            self.bump(); // consume indent
                            self.parse_value();
                        }
                    }
                    self.builder.finish_node();
                } else {
                    // No value, just a key
                    self.builder.start_node(SyntaxKind::VALUE.into());
                    self.builder.finish_node();
                }
            } else {
                self.parse_mapping_key_value_pair();
            }

            // After parsing a mapping entry, check indentation before continuing
            if base_indent > 0 {
                if self.skip_ws_and_newlines_with_indent_check(base_indent) {
                    // Dedent detected, stop parsing this mapping
                    break;
                }
            } else {
                self.skip_ws_and_newlines();
            }
        }

        self.builder.finish_node();
        self.error_context.pop_context();
    }

    fn parse_sequence(&mut self) {
        self.parse_sequence_with_base_indent(0);
    }

    fn parse_sequence_with_base_indent(&mut self, base_indent: usize) {
        self.builder.start_node(SyntaxKind::SEQUENCE.into());
        self.error_context.push_context(ParseContext::Sequence);

        while self.current() == Some(SyntaxKind::DASH) {
            // Start SEQUENCE_ENTRY node to wrap the entire item
            self.builder.start_node(SyntaxKind::SEQUENCE_ENTRY.into());

            self.bump(); // consume dash
            self.skip_whitespace();

            if self.current().is_some() && self.current() != Some(SyntaxKind::NEWLINE) {
                // Pass base_indent to ensure nested content doesn't consume wrong tokens
                self.parse_value_with_base_indent(base_indent);
            } else if self.current() == Some(SyntaxKind::NEWLINE) {
                // Check if next line is indented (nested content for sequence item)
                self.bump(); // consume newline
                if self.current() == Some(SyntaxKind::INDENT) {
                    let indent_level = self
                        .tokens
                        .last()
                        .map(|(_, text)| text.len())
                        .unwrap_or(0);
                    self.bump(); // consume indent
                                 // Parse the indented content as the sequence item value
                    self.parse_value_with_base_indent(indent_level);
                }
            }

            // Finish SEQUENCE_ENTRY node
            self.builder.finish_node();

            // After parsing a sequence item, check indentation before continuing
            if base_indent > 0 {
                if self.skip_ws_and_newlines_with_indent_check(base_indent) {
                    // Dedent detected, stop parsing this sequence
                    break;
                }
            } else {
                self.skip_ws_and_newlines();
            }
        }

        self.builder.finish_node();
        self.error_context.pop_context();
    }

    fn parse_flow_sequence(&mut self) {
        self.builder.start_node(SyntaxKind::SEQUENCE.into());
        self.error_context.push_context(ParseContext::FlowSequence);

        self.bump(); // consume [
        self.skip_ws_and_newlines(); // Support comments and newlines in flow sequences

        let prev_flow = self.in_flow_context;
        self.in_flow_context = true;

        while self.current() != Some(SyntaxKind::RIGHT_BRACKET) && self.current().is_some() {
            // Check if we have a valid flow sequence element
            // In flow sequences, we should not have bare colons or other structural elements
            if matches!(self.current(), Some(SyntaxKind::COLON)) {
                // This is an error - colons are not valid in flow sequences
                let error_msg = self.create_detailed_error(
                    "Unexpected colon in flow sequence",
                    "comma or ']'",
                    self.current_text().as_deref(),
                );
                self.add_error(error_msg);
                // Break out of the loop to avoid infinite loop
                break;
            }

            // Start SEQUENCE_ENTRY node to wrap the item
            self.builder.start_node(SyntaxKind::SEQUENCE_ENTRY.into());
            self.parse_value();
            self.builder.finish_node(); // Finish SEQUENCE_ENTRY

            self.skip_ws_and_newlines(); // Support comments after values

            if self.current() == Some(SyntaxKind::COMMA) {
                self.bump();
                self.skip_ws_and_newlines(); // Support comments after commas
            } else if self.current() != Some(SyntaxKind::RIGHT_BRACKET) && self.current().is_some()
            {
                // No comma found and not at closing bracket
                // Check if we should break to avoid infinite loops
                if matches!(
                    self.current(),
                    Some(SyntaxKind::COLON) | Some(SyntaxKind::DASH)
                ) {
                    // These tokens indicate we've likely left the flow sequence context
                    break;
                }
            }
        }

        self.in_flow_context = prev_flow;

        if self.current() == Some(SyntaxKind::RIGHT_BRACKET) {
            self.bump();
        } else {
            let error_msg = self.create_detailed_error(
                "Unclosed flow sequence",
                "']' to close sequence",
                self.current_text().as_deref(),
            );
            self.add_error_and_recover(error_msg, SyntaxKind::RIGHT_BRACKET);
        }

        self.builder.finish_node();
        self.error_context.pop_context();
    }

    fn parse_flow_mapping(&mut self) {
        self.builder.start_node(SyntaxKind::MAPPING.into());
        self.error_context.push_context(ParseContext::FlowMapping);

        self.bump(); // consume {
        self.skip_ws_and_newlines(); // Support comments and newlines in flow mappings

        let prev_flow = self.in_flow_context;
        self.in_flow_context = true;

        while self.current() != Some(SyntaxKind::RIGHT_BRACE) && self.current().is_some() {
            // Check for unexpected structural tokens that indicate we've left flow context
            if matches!(self.current(), Some(SyntaxKind::DASH)) {
                // Dash at this position means we've likely exited the flow mapping
                break;
            }

            // Start MAPPING_ENTRY node to wrap the key-value pair
            self.builder.start_node(SyntaxKind::MAPPING_ENTRY.into());

            // Parse key - wrap in KEY node
            self.builder.start_node(SyntaxKind::KEY.into());
            self.parse_value();
            self.builder.finish_node();

            self.skip_ws_and_newlines(); // Support comments after keys

            if self.current() == Some(SyntaxKind::COLON) {
                self.bump();
                self.skip_ws_and_newlines(); // Support comments after colons

                // Parse value - wrap in VALUE node
                self.builder.start_node(SyntaxKind::VALUE.into());
                self.parse_value();
                self.builder.finish_node();
            } else {
                let error_msg = self.create_detailed_error(
                    "Missing colon in flow mapping",
                    "':' after key",
                    self.current_text().as_deref(),
                );
                self.add_error_and_recover(error_msg, SyntaxKind::COLON);
            }

            // Finish MAPPING_ENTRY node
            self.builder.finish_node();

            self.skip_ws_and_newlines(); // Support comments after values

            if self.current() == Some(SyntaxKind::COMMA) {
                self.bump();
                self.skip_ws_and_newlines(); // Support comments after commas
            }
        }

        self.in_flow_context = prev_flow;

        if self.current() == Some(SyntaxKind::RIGHT_BRACE) {
            self.bump();
        } else {
            let error_msg = self.create_detailed_error(
                "Unclosed flow mapping",
                "'}' to close mapping",
                self.current_text().as_deref(),
            );
            self.add_error_and_recover(error_msg, SyntaxKind::RIGHT_BRACE);
        }

        self.builder.finish_node();
        self.error_context.pop_context();
    }

    fn parse_directive(&mut self) {
        self.builder.start_node(SyntaxKind::DIRECTIVE.into());

        if self.current() == Some(SyntaxKind::DIRECTIVE) {
            self.bump(); // consume the directive token
        } else {
            self.add_error("Expected directive".to_string());
        }

        self.builder.finish_node();
    }

    fn parse_explicit_key_mapping(&mut self) {
        // Parse mapping with explicit key indicator '?'
        self.builder.start_node(SyntaxKind::MAPPING.into());

        while self.current() == Some(SyntaxKind::QUESTION) {
            // Parse explicit key
            self.bump(); // consume '?'
            self.skip_whitespace();

            // Parse key - can be any value including sequences and mappings
            self.builder.start_node(SyntaxKind::KEY.into());

            // Parse the first part of the key
            if self.current().is_some() && self.current() != Some(SyntaxKind::NEWLINE) {
                self.parse_value();
            }

            // Check if this is a multiline key (newline followed by indent)
            // Only for scalar keys, not sequences or mappings
            if self.current() == Some(SyntaxKind::NEWLINE) {
                // Peek ahead to see if there's an indent after the newline
                // Since tokens are reversed, peek at the second-to-last token
                if self.tokens.len() >= 2 {
                    let (next_kind, _) = &self.tokens[self.tokens.len() - 2];
                    if *next_kind == SyntaxKind::INDENT {
                        // Check what comes after the indent (at position len() - 3)
                        if self.tokens.len() >= 3 {
                            let (token_after_indent, _) = &self.tokens[self.tokens.len() - 3];
                            // If it's a DASH, this is a sequence continuation which was already
                            // handled by parse_value() above - don't try to parse it as multiline scalar
                            if *token_after_indent != SyntaxKind::DASH {
                                // This is a multiline scalar key continuation
                                self.bump(); // consume newline
                                self.bump(); // consume indent

                                // Parse scalar tokens at this indentation level as part of the key
                                while self.current().is_some()
                                    && self.current() != Some(SyntaxKind::NEWLINE)
                                    && self.current() != Some(SyntaxKind::COLON)
                                {
                                    self.parse_scalar();
                                    if self.current() == Some(SyntaxKind::WHITESPACE) {
                                        self.bump(); // consume whitespace between key parts
                                    }
                                }
                            }
                        }
                    }
                }
            }

            self.builder.finish_node();

            self.skip_ws_and_newlines();

            // Parse value if there's a colon
            if self.current() == Some(SyntaxKind::COLON) {
                self.bump(); // consume ':'
                self.skip_whitespace();

                self.builder.start_node(SyntaxKind::VALUE.into());
                if self.current().is_some() && self.current() != Some(SyntaxKind::NEWLINE) {
                    self.parse_value();
                } else if self.current() == Some(SyntaxKind::NEWLINE) {
                    // Check if next line is indented (nested content)
                    self.bump(); // consume newline
                    if self.current() == Some(SyntaxKind::INDENT) {
                        self.bump(); // consume indent
                        self.parse_value();
                    }
                }
                self.builder.finish_node();
            } else {
                // No value, just a key
                self.builder.start_node(SyntaxKind::VALUE.into());
                self.builder.finish_node();
            }

            self.skip_ws_and_newlines();

            // Check if there are more entries
            if self.current() != Some(SyntaxKind::QUESTION) && !self.is_mapping_key() {
                break;
            }
        }

        // Continue parsing regular mapping entries if any
        while self.current().is_some() && self.is_mapping_key() {
            self.parse_mapping_key_value_pair();
            self.skip_ws_and_newlines();
        }

        self.builder.finish_node();
    }

    fn parse_complex_key_mapping(&mut self) {
        // Parse mapping where the key is a complex structure (sequence or mapping)
        self.builder.start_node(SyntaxKind::MAPPING.into());

        // Parse the complex key
        self.builder.start_node(SyntaxKind::KEY.into());
        if self.current() == Some(SyntaxKind::LEFT_BRACKET) {
            self.parse_flow_sequence();
        } else if self.current() == Some(SyntaxKind::LEFT_BRACE) {
            self.parse_flow_mapping();
        }
        self.builder.finish_node();

        self.skip_ws_and_newlines(); // Allow newlines between key and colon

        // Expect colon
        if self.current() == Some(SyntaxKind::COLON) {
            self.bump();
            self.skip_whitespace();

            // Parse value
            self.builder.start_node(SyntaxKind::VALUE.into());
            if self.current().is_some() && self.current() != Some(SyntaxKind::NEWLINE) {
                self.parse_value();
            } else if self.current() == Some(SyntaxKind::NEWLINE) {
                self.bump(); // consume newline
                if self.current() == Some(SyntaxKind::INDENT) {
                    self.bump(); // consume indent
                    self.parse_value();
                }
            }
            self.builder.finish_node();
        } else {
            let error_msg = self.create_detailed_error(
                "Missing colon in complex mapping",
                "':' after complex key",
                self.current_text().as_deref(),
            );
            self.add_error_and_recover(error_msg, SyntaxKind::COLON);
        }

        self.skip_ws_and_newlines();

        // Continue parsing more entries if they exist
        while self.current().is_some() {
            if self.current() == Some(SyntaxKind::QUESTION) {
                // Switch to explicit key parsing
                self.parse_explicit_key_entries();
                break;
            } else if self.is_complex_mapping_key()
                || (self.is_mapping_key() && self.current() != Some(SyntaxKind::QUESTION))
            {
                // Parse another entry
                self.builder.start_node(SyntaxKind::KEY.into());

                if self.current() == Some(SyntaxKind::LEFT_BRACKET) {
                    self.parse_flow_sequence();
                } else if self.current() == Some(SyntaxKind::LEFT_BRACE) {
                    self.parse_flow_mapping();
                } else if matches!(
                    self.current(),
                    Some(
                        SyntaxKind::STRING
                            | SyntaxKind::INT
                            | SyntaxKind::FLOAT
                            | SyntaxKind::BOOL
                            | SyntaxKind::NULL
                            | SyntaxKind::MERGE_KEY
                    )
                ) {
                    self.bump();
                }
                self.builder.finish_node();

                self.skip_whitespace();

                if self.current() == Some(SyntaxKind::COLON) {
                    self.bump();
                    self.skip_whitespace();

                    self.builder.start_node(SyntaxKind::VALUE.into());
                    if self.current().is_some() && self.current() != Some(SyntaxKind::NEWLINE) {
                        self.parse_value();
                    } else if self.current() == Some(SyntaxKind::NEWLINE) {
                        self.bump();
                        if self.current() == Some(SyntaxKind::INDENT) {
                            self.bump();
                            self.parse_value();
                        }
                    }
                    self.builder.finish_node();
                }

                self.skip_ws_and_newlines();
            } else {
                break;
            }
        }

        self.builder.finish_node();
    }

    fn parse_explicit_key_entries(&mut self) {
        // Helper to continue parsing explicit key entries within a mapping
        while self.current() == Some(SyntaxKind::QUESTION) {
            self.bump(); // consume '?'
            self.skip_whitespace();

            self.builder.start_node(SyntaxKind::KEY.into());
            if self.current().is_some() && self.current() != Some(SyntaxKind::NEWLINE) {
                self.parse_value();
            }
            self.builder.finish_node();

            self.skip_ws_and_newlines();

            if self.current() == Some(SyntaxKind::COLON) {
                self.bump();
                self.skip_whitespace();

                self.builder.start_node(SyntaxKind::VALUE.into());
                if self.current().is_some() && self.current() != Some(SyntaxKind::NEWLINE) {
                    self.parse_value();
                } else if self.current() == Some(SyntaxKind::NEWLINE) {
                    self.bump();
                    if self.current() == Some(SyntaxKind::INDENT) {
                        self.bump();
                        self.parse_value();
                    }
                }
                self.builder.finish_node();
            } else {
                self.builder.start_node(SyntaxKind::VALUE.into());
                self.builder.finish_node();
            }

            self.skip_ws_and_newlines();
        }
    }

    fn is_complex_mapping_key(&self) -> bool {
        // Check if a flow sequence or mapping is used as a key
        if !matches!(
            self.current(),
            Some(SyntaxKind::LEFT_BRACKET) | Some(SyntaxKind::LEFT_BRACE)
        ) {
            return false;
        }

        // Look ahead to find matching closing bracket/brace and then check for colon
        let mut depth = 0;
        let start_kind = self.current();
        let close_kind = match start_kind {
            Some(SyntaxKind::LEFT_BRACKET) => SyntaxKind::RIGHT_BRACKET,
            Some(SyntaxKind::LEFT_BRACE) => SyntaxKind::RIGHT_BRACE,
            _ => return false,
        };

        let mut found_close = false;
        for kind in self.upcoming_tokens() {
            if !found_close {
                if Some(kind) == start_kind {
                    depth += 1;
                } else if kind == close_kind {
                    if depth == 0 {
                        // Found matching close
                        found_close = true;
                    } else {
                        depth -= 1;
                    }
                }
            } else {
                // We've found the closing bracket/brace, now look for colon
                match kind {
                    SyntaxKind::WHITESPACE | SyntaxKind::INDENT => continue,
                    SyntaxKind::COLON => return true,
                    _ => return false,
                }
            }
        }
        false
    }

    fn parse_mapping_value(&mut self) {
        // When parsing the value part of a mapping, be more conservative about
        // interpreting content as nested mappings. Only parse as mapping if
        // it's clearly a structured value, otherwise parse as scalar.
        match self.current() {
            Some(SyntaxKind::DASH) if !self.in_flow_context => self.parse_sequence(),
            Some(SyntaxKind::ANCHOR) => self.parse_anchored_value(),
            Some(SyntaxKind::REFERENCE) => self.parse_alias(),
            Some(SyntaxKind::TAG) => self.parse_tagged_value(),
            Some(SyntaxKind::QUESTION) => {
                // Explicit key indicator - parse complex mapping
                self.parse_explicit_key_mapping();
            }
            Some(SyntaxKind::PIPE) => self.parse_literal_block_scalar(),
            Some(SyntaxKind::GREATER) => self.parse_folded_block_scalar(),
            Some(SyntaxKind::LEFT_BRACKET) => {
                // Check if this is a complex key in a mapping
                if !self.in_flow_context && self.is_complex_mapping_key() {
                    self.parse_complex_key_mapping();
                } else {
                    self.parse_flow_sequence();
                }
            }
            Some(SyntaxKind::LEFT_BRACE) => {
                // Check if this is a complex key in a mapping
                if !self.in_flow_context && self.is_complex_mapping_key() {
                    self.parse_complex_key_mapping();
                } else {
                    self.parse_flow_mapping();
                }
            }
            _ => {
                // For all other cases in mapping values, parse as scalar
                // This handles URLs and other complex scalar values containing colons
                self.parse_scalar();
            }
        }
    }

    fn is_mapping_key(&self) -> bool {
        // Check if this is an explicit key indicator
        if self.current() == Some(SyntaxKind::QUESTION) {
            return true;
        }

        // Check if this is a merge key
        if self.current() == Some(SyntaxKind::MERGE_KEY) {
            return true;
        }

        // If current token is a dash, this is not a mapping key
        if self.current() == Some(SyntaxKind::DASH) {
            return false;
        }

        // Look ahead to see if there's a colon after the current token
        // A valid mapping key should have a colon immediately after (with only whitespace)
        let upcoming = self.upcoming_tokens();
        for kind in upcoming {
            match kind {
                SyntaxKind::COLON => {
                    return true;
                }
                SyntaxKind::WHITESPACE => continue,
                // Any other token means this is not a simple mapping key
                _ => {
                    return false;
                }
            }
        }
        false
    }

    fn skip_whitespace(&mut self) {
        self.skip_tokens(&[SyntaxKind::WHITESPACE]);
    }

    fn skip_tokens(&mut self, kinds: &[SyntaxKind]) {
        while let Some(current) = self.current() {
            if kinds.contains(&current) {
                self.bump();
            } else {
                break;
            }
        }
    }

    fn get_current_indentation_level(&self) -> usize {
        // For a DASH token, we want to find the INDENT that was consumed just before this line
        // In our case, the pattern should be: ... NEWLINE, INDENT, [WHITESPACE], DASH

        let current_idx = self.tokens.len().saturating_sub(1);

        // Look for INDENT token that is immediately before the current content
        // We expect the pattern: INDENT, [optional WHITESPACE], current_token
        if current_idx >= 1 && self.tokens[current_idx - 1].0 == SyntaxKind::INDENT {
            return self.tokens[current_idx - 1].1.len();
        }

        if current_idx >= 2
            && self.tokens[current_idx - 1].0 == SyntaxKind::WHITESPACE
            && self.tokens[current_idx - 2].0 == SyntaxKind::INDENT
        {
            return self.tokens[current_idx - 2].1.len();
        }

        // If that doesn't work, fall back to finding any recent INDENT
        for i in (0..current_idx).rev() {
            if self.tokens[i].0 == SyntaxKind::INDENT {
                // Check if this INDENT is followed by content on the same line
                let mut j = i + 1;
                while j < self.tokens.len() {
                    match self.tokens[j].0 {
                        SyntaxKind::WHITESPACE => {
                            j += 1;
                            continue;
                        }
                        SyntaxKind::NEWLINE => {
                            // This INDENT is followed by a newline, so it's not for current line
                            break;
                        }
                        _ => {
                            // This INDENT is followed by content, check if it leads to current
                            if j <= current_idx {
                                return self.tokens[i].1.len();
                            }
                            break;
                        }
                    }
                }
            }
        }

        0 // Default to no indentation
    }

    fn get_dash_indentation_level(&self) -> usize {
        // For DASH tokens, we know they should be at the beginning of a line after NEWLINE + INDENT
        // Since the current token is DASH, look backwards for NEWLINE + INDENT pattern

        let current_idx = self.tokens.len().saturating_sub(1);

        // Look backwards through the token stack for NEWLINE followed by INDENT
        for i in (1..current_idx).rev() {
            if self.tokens[i].0 == SyntaxKind::NEWLINE {
                // Check if the next token (closer to current) is INDENT
                if i > 0 && self.tokens[i - 1].0 == SyntaxKind::INDENT {
                    // This NEWLINE is followed by INDENT, but we need to check if this
                    // INDENT is for the current DASH
                    let indent_len = self.tokens[i - 1].1.len();
                    // Return the most recent INDENT that has length >= 2 (typical sequence indentation)
                    // This handles both base sequence level (2) and nested levels
                    if indent_len >= 2 {
                        return indent_len;
                    }
                }
            }
        }

        0 // Default if pattern not found
    }

    fn skip_ws_and_newlines_with_indent_check(&mut self, base_indent: usize) -> bool {
        // Skip whitespace and newlines, but return true if we encounter dedent

        // Handle case where we're already positioned at content tokens
        // This can happen when a previous indentation check consumed whitespace/newlines
        match self.current() {
            Some(SyntaxKind::STRING)
            | Some(SyntaxKind::INT)
            | Some(SyntaxKind::FLOAT)
            | Some(SyntaxKind::BOOL)
            | Some(SyntaxKind::NULL) => {
                // We're at content - check if it's at a lower indentation than base_indent
                let current_indent = self.get_current_indentation_level();
                if current_indent < base_indent {
                    return true;
                }
                // If not dedented, continue with normal logic
            }
            Some(SyntaxKind::DASH) => {
                // DASH tokens are special - they continue sequences if at the same indentation level
                // For DASH tokens, we need to check if this is a sequence continuation
                // Look for the pattern: NEWLINE, INDENT(base_indent), [WHITESPACE], DASH
                let current_indent = self.get_dash_indentation_level();
                if current_indent < base_indent {
                    return true;
                }
                // DASH at same or greater indentation continues the sequence
                return false; // Continue parsing
            }
            _ => {
                // Normal case - continue with whitespace skipping logic
            }
        }

        while self.current().is_some() {
            match self.current() {
                Some(SyntaxKind::WHITESPACE) | Some(SyntaxKind::COMMENT) => {
                    self.bump();
                }
                Some(SyntaxKind::NEWLINE) => {
                    self.bump();
                    // Check next token for indentation
                    if let Some(SyntaxKind::INDENT) = self.current() {
                        if let Some((_, text)) = self.tokens.last() {
                            if text.len() < base_indent {
                                // Dedent detected - don't consume the indent token
                                return true;
                            }
                        }
                        self.bump(); // consume indent if at appropriate level
                    } else if !matches!(
                        self.current(),
                        Some(SyntaxKind::WHITESPACE)
                            | Some(SyntaxKind::NEWLINE)
                            | Some(SyntaxKind::COMMENT)
                            | None
                    ) {
                        // Non-whitespace at start of line means indentation 0
                        if base_indent > 0 {
                            return true; // dedent detected
                        }
                    }
                }
                Some(SyntaxKind::INDENT) => {
                    // Standalone indent token
                    if let Some((_, text)) = self.tokens.last() {
                        if text.len() < base_indent {
                            return true; // dedent detected
                        }
                    }
                    self.bump();
                }
                _ => break,
            }
        }
        false
    }

    fn skip_ws_and_newlines(&mut self) {
        self.skip_tokens(&[
            SyntaxKind::WHITESPACE,
            SyntaxKind::NEWLINE,
            SyntaxKind::INDENT,
            SyntaxKind::COMMENT,
        ]);
    }

    fn parse_mapping_key_value_pair(&mut self) {
        // Start MAPPING_ENTRY node to wrap the entire key-value pair
        self.builder.start_node(SyntaxKind::MAPPING_ENTRY.into());

        // Parse regular key
        self.builder.start_node(SyntaxKind::KEY.into());
        if self.current() == Some(SyntaxKind::MERGE_KEY) {
            self.builder.start_node(SyntaxKind::SCALAR.into());
            self.bump(); // consume the merge key token
            self.builder.finish_node(); // SCALAR
        } else if matches!(
            self.current(),
            Some(
                SyntaxKind::STRING
                    | SyntaxKind::INT
                    | SyntaxKind::FLOAT
                    | SyntaxKind::BOOL
                    | SyntaxKind::NULL
            )
        ) {
            self.builder.start_node(SyntaxKind::SCALAR.into());
            self.bump(); // consume the key token
            self.builder.finish_node(); // SCALAR
        }
        self.builder.finish_node(); // KEY

        self.skip_whitespace();

        // Expect colon
        if self.current() == Some(SyntaxKind::COLON) {
            self.bump();
            self.skip_whitespace();

            // Parse value - wrap in VALUE node
            self.builder.start_node(SyntaxKind::VALUE.into());
            if self.current().is_some() && self.current() != Some(SyntaxKind::NEWLINE) {
                self.parse_mapping_value();
            } else if self.current() == Some(SyntaxKind::NEWLINE) {
                // Check if next line is indented (nested content) or starts with a sequence
                self.bump(); // consume newline
                if self.current() == Some(SyntaxKind::INDENT) {
                    let indent_level = self
                        .tokens
                        .last()
                        .map(|(_, text)| text.len())
                        .unwrap_or(0);
                    self.bump(); // consume indent
                                 // Parse the indented content as the value, tracking indent level
                    self.parse_value_with_base_indent(indent_level);
                } else if self.current() == Some(SyntaxKind::DASH) {
                    // Zero-indented sequence (same indentation as key)
                    // This is valid YAML: the sequence is the value for the key
                    self.parse_sequence();
                }
            }
            // Empty VALUE node if no value present
            self.builder.finish_node();
        } else {
            let error_msg = self.create_detailed_error(
                "Missing colon in mapping",
                "':' after key",
                self.current_text().as_deref(),
            );
            self.add_error_and_recover(error_msg, SyntaxKind::COLON);
        }

        // Consume any trailing whitespace and comments before closing MAPPING_ENTRY
        while self.current() == Some(SyntaxKind::WHITESPACE) {
            self.bump();
        }
        if self.current() == Some(SyntaxKind::COMMENT) {
            self.bump();
        }

        // Finish MAPPING_ENTRY node
        self.builder.finish_node();
    }

    fn bump(&mut self) {
        if let Some((kind, text)) = self.tokens.pop() {
            self.builder.token(kind.into(), &text);
            if self.current_token_index > 0 {
                self.current_token_index -= 1;
            }
            // Update error context position
            self.error_context.advance(text.len());
        }
    }

    fn current(&self) -> Option<SyntaxKind> {
        self.tokens.last().map(|(kind, _)| *kind)
    }

    fn current_text(&self) -> Option<String> {
        self.tokens.last().map(|(_, text)| text.clone())
    }

    /// Iterator over upcoming tokens starting from the next token (not current)
    fn upcoming_tokens(&self) -> impl Iterator<Item = SyntaxKind> + '_ {
        // Since tokens are in reverse order (last is current), we need to iterate
        // from the second-to-last token backwards to the beginning
        let len = self.tokens.len();
        (0..len.saturating_sub(1))
            .rev()
            .map(move |i| self.tokens[i].0)
    }

    fn add_error(&mut self, message: String) {
        // Create positioned error with line/column info
        let token_text = self.current_text();
        let token_len = token_text.as_ref().map(|s| s.len()).unwrap_or(1);
        let positioned_error = self.error_context.create_error(message, token_len);

        self.errors.push(positioned_error.message.clone());
        self.positioned_errors.push(positioned_error);
    }

    /// Add an error with recovery
    fn add_error_and_recover(&mut self, message: String, expected: SyntaxKind) {
        self.add_error(message);

        // Determine recovery strategy
        let found = self.current();
        let strategy = self.error_context.suggest_recovery(expected, found);

        match strategy {
            RecoveryStrategy::SkipToken => {
                // Skip the problematic token
                if self.current().is_some() {
                    self.bump();
                }
            }
            RecoveryStrategy::SkipToEndOfLine => {
                // Skip to end of line
                while self.current().is_some() && self.current() != Some(SyntaxKind::NEWLINE) {
                    self.bump();
                }
            }
            RecoveryStrategy::InsertToken(kind) => {
                // Insert synthetic token
                self.builder.token(kind.into(), "");
            }
            RecoveryStrategy::SyncToSafePoint => {
                // Find next safe synchronization point
                let sync_point = self
                    .error_context
                    .find_sync_point(&self.tokens, self.tokens.len() - self.current_token_index);
                let tokens_to_skip = sync_point - (self.tokens.len() - self.current_token_index);
                for _ in 0..tokens_to_skip {
                    if self.current().is_some() {
                        self.bump();
                    }
                }
            }
        }
    }

    /// Create a detailed error message with helpful suggestions
    fn create_detailed_error(
        &mut self,
        base_message: &str,
        expected: &str,
        found: Option<&str>,
    ) -> String {
        let mut builder = ErrorBuilder::new(base_message);
        builder = builder.expected(expected);

        if let Some(found_str) = found {
            builder = builder.found(found_str);
        } else if let Some(token) = self.current_text() {
            builder = builder.found(format!("'{}'", token));
        } else {
            builder = builder.found("end of input");
        }

        // Add context
        let context = match self.error_context.current_context() {
            ParseContext::Mapping => "in mapping",
            ParseContext::Sequence => "in sequence",
            ParseContext::FlowMapping => "in flow mapping",
            ParseContext::FlowSequence => "in flow sequence",
            ParseContext::BlockScalar => "in block scalar",
            ParseContext::QuotedString => "in quoted string",
            _ => "at document level",
        };
        builder = builder.context(context);

        // Add helpful suggestions based on the error type
        let suggestion = self.get_error_suggestion(base_message, expected, found);
        if let Some(suggestion_text) = suggestion {
            builder = builder.suggestion(suggestion_text);
        }

        builder.build()
    }

    /// Generate helpful suggestions for common errors
    fn get_error_suggestion(
        &self,
        base_message: &str,
        expected: &str,
        found: Option<&str>,
    ) -> Option<String> {
        if base_message.contains("Unterminated quoted string") {
            return Some(
                "Add closing quote or check for unescaped quotes within the string".to_string(),
            );
        }

        if base_message.contains("Missing colon") || expected.contains("':'") {
            return Some("Add ':' after the key, or check for proper indentation".to_string());
        }

        if base_message.contains("Unclosed flow sequence") {
            return Some(
                "Add ']' to close the array, or check for missing commas between elements"
                    .to_string(),
            );
        }

        if base_message.contains("Unclosed flow mapping") {
            return Some(
                "Add '}' to close the object, or check for missing commas between key-value pairs"
                    .to_string(),
            );
        }

        if let Some(found_text) = found {
            if found_text.contains('\n') {
                return Some(
                    "Unexpected newline - check indentation and YAML structure".to_string(),
                );
            }

            if found_text.contains('\t') {
                return Some(
                    "Tabs are not allowed in YAML - use spaces for indentation".to_string(),
                );
            }
        }

        None
    }
}

/// Parse YAML text
pub fn parse(text: &str) -> ParsedYaml {
    let parser = Parser::new(text);
    parser.parse()
}

// Additional editing methods for existing types

// Helper functions for creating YAML nodes
fn create_scalar_green(value: &str) -> rowan::GreenNode {
    let mut builder = GreenNodeBuilder::new();
    builder.start_node(SyntaxKind::SCALAR.into());
    builder.token(SyntaxKind::VALUE.into(), value);
    builder.finish_node();
    builder.finish()
}

// Editing methods for Mapping
impl Mapping {
    /// Find a mapping entry by its key
    pub fn find_entry_by_key(&self, key: &YamlValue) -> Option<MappingEntry> {
        // Build a temporary key node for comparison
        let mut builder = GreenNodeBuilder::new();
        Document::build_key_content(&mut builder, key, 0);
        let expected_key = SyntaxNode::new_root_mut(builder.finish());
        
        // Look through all mapping entries
        self.0.children()
            .filter_map(MappingEntry::cast)
            .find(|entry| {
                if let Some(entry_key) = entry.key() {
                    self.compare_key_nodes(&entry_key, &expected_key)
                } else {
                    false
                }
            })
    }
    
    
    /// Get all mapping entries
    pub fn entries(&self) -> impl Iterator<Item = MappingEntry> {
        self.0.children().filter_map(MappingEntry::cast)
    }
    
    /// Find the index of a mapping entry by its key
    pub fn find_entry_index_by_key(&self, key: &YamlValue) -> Option<usize> {
        // Build a temporary key node for comparison
        let mut builder = GreenNodeBuilder::new();
        Document::build_key_content(&mut builder, key, 0);
        let expected_key = SyntaxNode::new_root_mut(builder.finish());
        
        // Look through all children (not just entries, to get accurate index)
        self.0.children_with_tokens()
            .enumerate()
            .find_map(|(i, child)| {
                if let Some(node) = child.as_node() {
                    if let Some(entry) = MappingEntry::cast(node.clone()) {
                        if let Some(entry_key) = entry.key() {
                            if self.compare_key_nodes(&entry_key, &expected_key) {
                                return Some(i);
                            }
                        }
                    }
                }
                None
            })
    }
    
    /// Set a key-value pair with a scalar value, replacing if exists or adding if new
    /// This method automatically escapes the key and value as needed.
    pub fn set(&mut self, key: impl Into<ScalarValue>, value: impl Into<YamlValue>) {
        let key_yaml = YamlValue::Scalar(key.into());
        let value_yaml = value.into();
        self.set_key_value_cst(&key_yaml, &value_yaml);
    }

    /// Internal method to set a key-value pair using CST building
    fn set_key_value_cst(&mut self, key: &YamlValue, value: &YamlValue) {
        // First, look for an existing entry with this key
        let children: Vec<_> = self.0.children_with_tokens().collect();
        
        for (i, child) in children.iter().enumerate() {
            if let Some(node) = child.as_node() {
                if node.kind() == SyntaxKind::MAPPING_ENTRY {
                    if let Some(entry) = MappingEntry::cast(node.clone()) {
                        // Check if this entry matches our key
                        if self.entry_has_key(&entry, key) {
                            // Found it! Update the value in place
                            let mut mutable_entry = entry;
                            mutable_entry.replace_value(value);
                            
                            // Replace the old entry with the updated one
                            self.0.splice_children(i..i + 1, vec![mutable_entry.0.into()]);
                            return;
                        }
                    }
                }
            }
        }
        
        // Entry doesn't exist, create a new one
        let new_entry = MappingEntry::from_yaml_values(key, value);
        self.insert_or_replace_entry_cst(&new_entry.0, key);
    }
    
    /// Helper to check if an entry has a specific key
    fn entry_has_key(&self, entry: &MappingEntry, key: &YamlValue) -> bool {
        if let Some(entry_key) = entry.key() {
            Self::yaml_value_matches_node(key, &entry_key)
        } else {
            false
        }
    }
    
    /// Compare a YamlValue to a SyntaxNode using proper CST casting
    fn yaml_value_matches_node(yaml_value: &YamlValue, node: &SyntaxNode) -> bool {
        // First unwrap KEY/VALUE wrapper nodes
        let actual_node = if node.kind() == SyntaxKind::KEY || node.kind() == SyntaxKind::VALUE {
            if let Some(child) = node.first_child() {
                child
            } else {
                return false;
            }
        } else {
            node.clone()
        };
        
        match yaml_value {
            YamlValue::Scalar(yaml_scalar) => {
                if let Some(node_scalar) = Scalar::cast(actual_node) {
                    node_scalar.value() == yaml_scalar.value()
                } else {
                    false
                }
            }
            YamlValue::Sequence(yaml_seq) => {
                if let Some(node_seq) = Sequence::cast(actual_node) {
                    let node_items: Vec<_> = node_seq.items().collect();
                    if node_items.len() != yaml_seq.len() {
                        return false;
                    }
                    
                    for (yaml_item, node_item) in yaml_seq.iter().zip(node_items.iter()) {
                        if !Self::yaml_value_matches_node(yaml_item, node_item) {
                            return false;
                        }
                    }
                    true
                } else {
                    false
                }
            }
            YamlValue::Mapping(yaml_map) => {
                if let Some(node_mapping) = Mapping::cast(actual_node) {
                    let node_pairs: Vec<_> = node_mapping.pairs().collect();
                    if node_pairs.len() != yaml_map.len() {
                        return false;
                    }
                    
                    for (yaml_key, yaml_value) in yaml_map {
                        let yaml_key_value = YamlValue::Scalar(ScalarValue::from(yaml_key.as_str()));
                        let mut found_match = false;
                        
                        for (node_key_opt, node_value_opt) in &node_pairs {
                            if let (Some(node_key), Some(node_value)) = (node_key_opt, node_value_opt) {
                                // Both key and value are now SyntaxNodes, supporting complex keys
                                if Self::yaml_value_matches_node(&yaml_key_value, node_key) &&
                                   Self::yaml_value_matches_node(yaml_value, node_value) {
                                    found_match = true;
                                    break;
                                }
                            }
                        }
                        
                        if !found_match {
                            return false;
                        }
                    }
                    true
                } else {
                    false
                }
            }
            YamlValue::Set(_) => {
                unimplemented!("Set comparison not yet implemented")
            }
            YamlValue::OrderedMapping(_) => {
                unimplemented!("OrderedMapping comparison not yet implemented")
            }
            YamlValue::Pairs(_) => {
                unimplemented!("Pairs comparison not yet implemented")
            }
        }
    }

    /// Internal method to set a key-value pair using proper KEY/VALUE nodes
    fn set_key_value(&mut self, key: &str, value: &str) {
        // Look through children to find MAPPING_ENTRY nodes
        let children: Vec<_> = self.0.children_with_tokens().collect();
        for (i, child) in children.iter().enumerate() {
            if let Some(node) = child.as_node() {
                if node.kind() == SyntaxKind::MAPPING_ENTRY {
                    // Check if this entry contains the key we're looking for
                    if let Some(key_node) = node.children().find(|n| n.kind() == SyntaxKind::KEY) {
                        if key_node.text().to_string().trim() == key.trim() {
                            // Found the entry! Build a new one with updated value
                            let entry_children: Vec<_> = node.children_with_tokens().collect();
                            let mut builder = GreenNodeBuilder::new();
                            builder.start_node(SyntaxKind::MAPPING_ENTRY.into());

                            for entry_child in entry_children {
                                match entry_child {
                                    rowan::NodeOrToken::Node(n)
                                        if n.kind() == SyntaxKind::VALUE =>
                                    {
                                        // Replace the VALUE node with new value, preserving trailing whitespace
                                        builder.start_node(SyntaxKind::VALUE.into());

                                        // Check if the original VALUE has any trailing whitespace
                                        let mut trailing_whitespace = String::new();
                                        for child in n.children_with_tokens() {
                                            if let rowan::NodeOrToken::Node(scalar) = &child {
                                                if scalar.kind() == SyntaxKind::SCALAR {
                                                    // Check if the SCALAR's text has trailing whitespace
                                                    let scalar_text = scalar.text().to_string();
                                                    // Find where the actual value ends and whitespace begins
                                                    let trimmed_len = scalar_text.trim_end().len();
                                                    if trimmed_len < scalar_text.len() {
                                                        trailing_whitespace =
                                                            scalar_text[trimmed_len..].to_string();
                                                    }
                                                }
                                            }
                                        }

                                        // Build new SCALAR with value
                                        builder.start_node(SyntaxKind::SCALAR.into());
                                        builder.token(SyntaxKind::STRING.into(), value);
                                        // Add any trailing whitespace from original
                                        if !trailing_whitespace.is_empty() {
                                            builder.token(
                                                SyntaxKind::WHITESPACE.into(),
                                                &trailing_whitespace,
                                            );
                                        }
                                        builder.finish_node(); // SCALAR
                                        builder.finish_node(); // VALUE
                                    }
                                    rowan::NodeOrToken::Node(n) => {
                                        // Copy other nodes as-is
                                        self.copy_node_to_builder(&mut builder, &n);
                                    }
                                    rowan::NodeOrToken::Token(t) => {
                                        // Copy tokens as-is
                                        builder.token(t.kind().into(), t.text());
                                    }
                                }
                            }

                            builder.finish_node(); // MAPPING_ENTRY
                            let new_entry = SyntaxNode::new_root_mut(builder.finish());

                            // Replace the old MAPPING_ENTRY with the new one
                            self.0.splice_children(i..i + 1, vec![new_entry.into()]);
                            return;
                        }
                    }
                }
            }
        }

        // Key doesn't exist, add new MAPPING_ENTRY at the end
        let children_with_tokens: Vec<_> = self.0.children_with_tokens().collect();
        let count = children_with_tokens.len();

        // Check if there's a trailing newline we need to preserve
        let has_trailing_newline = if count > 0 {
            if let Some(last) = children_with_tokens.last() {
                if let Some(token) = last.as_token() {
                    token.kind() == SyntaxKind::NEWLINE
                } else {
                    false
                }
            } else {
                false
            }
        } else {
            false
        };

        // Determine insertion position - before trailing newline if it exists
        let insert_pos = if has_trailing_newline {
            count - 1
        } else {
            count
        };

        let mut new_elements = Vec::new();

        // Add newline before if there are existing entries (but not if we're at position 0)
        // Also check that the previous token isn't already a newline to avoid double newlines
        let needs_newline = if insert_pos > 0 {
            // Check what's at insert_pos - 1
            if let Some(prev) = children_with_tokens.get(insert_pos - 1) {
                // Only add newline if previous isn't already a newline
                if let Some(token) = prev.as_token() {
                    token.kind() != SyntaxKind::NEWLINE
                } else {
                    true // Previous is a node, we need a newline
                }
            } else {
                true
            }
        } else {
            false
        };
        
        if needs_newline {
            let mut nl_builder = GreenNodeBuilder::new();
            nl_builder.start_node(SyntaxKind::ROOT.into());
            nl_builder.token(SyntaxKind::NEWLINE.into(), "\n");
            nl_builder.finish_node();
            let nl_node = SyntaxNode::new_root_mut(nl_builder.finish());
            if let Some(token) = nl_node.first_token() {
                new_elements.push(token.into());
            }
        }

        // Detect the indentation level of existing entries
        let indent_level = self.detect_indentation_level();

        // Add indentation if needed
        if indent_level > 0 {
            let indent_str = " ".repeat(indent_level);
            let mut indent_builder = GreenNodeBuilder::new();
            indent_builder.start_node(SyntaxKind::ROOT.into());
            indent_builder.token(SyntaxKind::INDENT.into(), &indent_str);
            indent_builder.finish_node();
            let indent_node = SyntaxNode::new_root_mut(indent_builder.finish());
            if let Some(token) = indent_node.first_token() {
                new_elements.push(token.into());
            }
        }

        // Build a new MAPPING_ENTRY
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(SyntaxKind::MAPPING_ENTRY.into());

        // Add KEY node
        builder.start_node(SyntaxKind::KEY.into());
        builder.start_node(SyntaxKind::SCALAR.into());
        builder.token(SyntaxKind::STRING.into(), key);
        builder.finish_node(); // SCALAR
        builder.finish_node(); // KEY

        // Add colon and space
        builder.token(SyntaxKind::COLON.into(), ":");
        builder.token(SyntaxKind::WHITESPACE.into(), " ");

        // Add VALUE node
        builder.start_node(SyntaxKind::VALUE.into());
        builder.start_node(SyntaxKind::SCALAR.into());
        builder.token(SyntaxKind::STRING.into(), value);
        builder.finish_node(); // SCALAR
        builder.finish_node(); // VALUE

        builder.finish_node(); // MAPPING_ENTRY

        let new_entry = SyntaxNode::new_root_mut(builder.finish());
        new_elements.push(new_entry.clone().into());

        // If we're appending at the end and there's no trailing newline, add one
        // This ensures YAML documents end with a newline per spec
        // Exception: Don't add if this is a flow mapping (starts with {)
        let is_flow_mapping = if count > 0 {
            if let Some(first) = children_with_tokens.first() {
                if let Some(token) = first.as_token() {
                    token.kind() == SyntaxKind::LEFT_BRACE
                } else {
                    false
                }
            } else {
                false
            }
        } else {
            false
        };

        if !has_trailing_newline && insert_pos == count && !is_flow_mapping {
            let mut nl_builder = GreenNodeBuilder::new();
            nl_builder.start_node(SyntaxKind::ROOT.into());
            nl_builder.token(SyntaxKind::NEWLINE.into(), "\n");
            nl_builder.finish_node();
            let nl_node = SyntaxNode::new_root_mut(nl_builder.finish());
            if let Some(token) = nl_node.first_token() {
                new_elements.push(token.into());
            }
        }

        // Insert at the calculated position
        self.0.splice_children(insert_pos..insert_pos, new_elements);
    }

    /// Internal method to insert or replace an entry using CST nodes
    fn insert_or_replace_entry_cst(&mut self, new_entry: &SyntaxNode, key: &YamlValue) {
        // Convert key to string for comparison (simplified approach)
        let key_str = match key {
            YamlValue::Scalar(s) => s.to_yaml_string(),
            _ => key.to_yaml_string(0), // For complex keys, use full representation
        };
        
        // Look for existing entry to replace
        let children: Vec<_> = self.0.children_with_tokens().collect();
        for (i, child) in children.iter().enumerate() {
            if let Some(node) = child.as_node() {
                if node.kind() == SyntaxKind::MAPPING_ENTRY {
                    if let Some(key_node) = node.children().find(|n| n.kind() == SyntaxKind::KEY) {
                        if key_node.text().to_string().trim() == key_str.trim() {
                            // Found existing entry - replace it
                            self.0.splice_children(i..i + 1, vec![new_entry.clone().into()]);
                            return;
                        }
                    }
                }
            }
        }
        
        // Entry doesn't exist - add it at the end
        let count = children.len();
        let mut new_elements = Vec::new();
        
        // Add newline if needed
        let has_trailing_newline = if count > 0 {
            if let Some(last) = children.last() {
                if let Some(token) = last.as_token() {
                    token.kind() == SyntaxKind::NEWLINE
                } else {
                    false
                }
            } else {
                false
            }
        } else {
            false
        };
        
        let insert_pos = if has_trailing_newline { count - 1 } else { count };
        
        // Add indentation if needed
        let indent_level = self.detect_indentation_level();
        if indent_level > 0 && count > 0 {
            let mut builder = rowan::GreenNodeBuilder::new();
            builder.start_node(SyntaxKind::ROOT.into());
            builder.token(SyntaxKind::NEWLINE.into(), "\n");
            builder.token(SyntaxKind::INDENT.into(), &" ".repeat(indent_level));
            builder.finish_node();
            let node = SyntaxNode::new_root_mut(builder.finish());
            // Get ALL tokens, not just the first one
            for child in node.children_with_tokens() {
                if let rowan::NodeOrToken::Token(token) = child {
                    new_elements.push(token.into());
                }
            }
        } else if count > 0 {
            let mut builder = rowan::GreenNodeBuilder::new();
            builder.start_node(SyntaxKind::ROOT.into());
            builder.token(SyntaxKind::NEWLINE.into(), "\n");
            builder.finish_node();
            let node = SyntaxNode::new_root_mut(builder.finish());
            if let Some(token) = node.first_token() {
                new_elements.push(token.into());
            }
        }
        
        new_elements.push(new_entry.clone().into());
        
        if !has_trailing_newline && insert_pos == count {
            let mut builder = rowan::GreenNodeBuilder::new();
            builder.start_node(SyntaxKind::ROOT.into());
            builder.token(SyntaxKind::NEWLINE.into(), "\n");
            builder.finish_node();
            let node = SyntaxNode::new_root_mut(builder.finish());
            if let Some(token) = node.first_token() {
                new_elements.push(token.into());
            }
        }
        
        self.0.splice_children(insert_pos..insert_pos, new_elements);
    }

    /// Helper method to check if a key node matches a YamlValue key using CST comparison
    fn keys_match(&self, key_node: &SyntaxNode, yaml_key: &YamlValue) -> bool {
        // Build a temporary CST node for the yaml_key to compare structurally
        let mut builder = GreenNodeBuilder::new();
        Document::build_key_content(&mut builder, yaml_key, 0);
        let expected_key = SyntaxNode::new_root_mut(builder.finish());
        
        // Compare the CST structures directly
        self.compare_key_nodes(&key_node, &expected_key)
    }
    
    /// Compare two key nodes structurally
    fn compare_key_nodes(&self, actual: &SyntaxNode, expected: &SyntaxNode) -> bool {
        // Both must be KEY nodes
        if actual.kind() != SyntaxKind::KEY || expected.kind() != SyntaxKind::KEY {
            return actual.kind() == expected.kind() && self.compare_nodes_structurally(actual, expected);
        }
        
        // Get the actual content nodes (skipping whitespace)
        let actual_content = self.get_key_content_nodes(actual);
        let expected_content = self.get_key_content_nodes(expected);
        
        if actual_content.len() != expected_content.len() {
            return false;
        }
        
        for (a, e) in actual_content.iter().zip(expected_content.iter()) {
            if !self.compare_nodes_structurally(a, e) {
                return false;
            }
        }
        
        true
    }
    
    /// Get the content nodes of a KEY, skipping whitespace and formatting
    fn get_key_content_nodes(&self, key_node: &SyntaxNode) -> Vec<SyntaxNode> {
        let mut nodes = Vec::new();
        for child in key_node.children_with_tokens() {
            match child {
                rowan::NodeOrToken::Node(n) => {
                    // Include all child nodes (sequences, mappings, etc.)
                    nodes.push(n);
                }
                rowan::NodeOrToken::Token(t) => {
                    // Include significant tokens as synthetic nodes
                    if t.kind() != SyntaxKind::WHITESPACE && 
                       t.kind() != SyntaxKind::INDENT &&
                       t.kind() != SyntaxKind::QUESTION {
                        // Create a synthetic node for the token to enable comparison
                        let mut token_builder = GreenNodeBuilder::new();
                        token_builder.start_node(t.kind().into());
                        token_builder.token(t.kind().into(), t.text());
                        token_builder.finish_node();
                        nodes.push(SyntaxNode::new_root_mut(token_builder.finish()));
                    }
                }
            }
        }
        nodes
    }
    
    /// Compare nodes structurally (for complex keys like sequences and mappings)
    fn compare_nodes_structurally(&self, node1: &SyntaxNode, node2: &SyntaxNode) -> bool {
        if node1.kind() != node2.kind() {
            return false;
        }
        
        match node1.kind() {
            SyntaxKind::SCALAR => {
                // For SCALAR nodes, compare the semantic content (unquoted strings)
                if let (Some(scalar1), Some(scalar2)) = (Scalar::cast(node1.clone()), Scalar::cast(node2.clone())) {
                    scalar1.as_string() == scalar2.as_string()
                } else {
                    false
                }
            }
            SyntaxKind::STRING => {
                // For string tokens, compare the actual content
                let text1: Vec<_> = node1.children_with_tokens()
                    .filter_map(|c| c.into_token())
                    .filter(|t| t.kind() == SyntaxKind::STRING)
                    .map(|t| t.text().to_string())
                    .collect();
                let text2: Vec<_> = node2.children_with_tokens()
                    .filter_map(|c| c.into_token())
                    .filter(|t| t.kind() == SyntaxKind::STRING)
                    .map(|t| t.text().to_string())
                    .collect();
                text1 == text2
            }
            SyntaxKind::SEQUENCE => {
                // Compare sequence entries
                let entries1: Vec<_> = node1.children()
                    .filter(|n| n.kind() == SyntaxKind::SEQUENCE_ENTRY)
                    .collect();
                let entries2: Vec<_> = node2.children()
                    .filter(|n| n.kind() == SyntaxKind::SEQUENCE_ENTRY)
                    .collect();
                
                if entries1.len() != entries2.len() {
                    return false;
                }
                
                for (e1, e2) in entries1.iter().zip(entries2.iter()) {
                    if !self.compare_sequence_entries(e1, e2) {
                        return false;
                    }
                }
                true
            }
            SyntaxKind::MAPPING => {
                // Compare mapping entries (order matters for keys)
                let entries1: Vec<_> = node1.children()
                    .filter(|n| n.kind() == SyntaxKind::MAPPING_ENTRY)
                    .collect();
                let entries2: Vec<_> = node2.children()
                    .filter(|n| n.kind() == SyntaxKind::MAPPING_ENTRY)
                    .collect();
                
                if entries1.len() != entries2.len() {
                    return false;
                }
                
                for (e1, e2) in entries1.iter().zip(entries2.iter()) {
                    if !self.compare_mapping_entries(e1, e2) {
                        return false;
                    }
                }
                true
            }
            _ => {
                // For other node types, compare token content
                let tokens1: Vec<_> = node1.children_with_tokens()
                    .filter_map(|c| c.into_token())
                    .filter(|t| t.kind() != SyntaxKind::WHITESPACE && t.kind() != SyntaxKind::INDENT)
                    .map(|t| (t.kind(), t.text().to_string()))
                    .collect();
                let tokens2: Vec<_> = node2.children_with_tokens()
                    .filter_map(|c| c.into_token())
                    .filter(|t| t.kind() != SyntaxKind::WHITESPACE && t.kind() != SyntaxKind::INDENT)
                    .map(|t| (t.kind(), t.text().to_string()))
                    .collect();
                tokens1 == tokens2
            }
        }
    }
    
    /// Compare sequence entries
    fn compare_sequence_entries(&self, entry1: &SyntaxNode, entry2: &SyntaxNode) -> bool {
        let value1 = entry1.children().find(|n| n.kind() == SyntaxKind::VALUE);
        let value2 = entry2.children().find(|n| n.kind() == SyntaxKind::VALUE);
        
        match (value1, value2) {
            (Some(v1), Some(v2)) => self.compare_nodes_structurally(&v1, &v2),
            (None, None) => true,
            _ => false,
        }
    }
    
    /// Compare mapping entries
    fn compare_mapping_entries(&self, entry1: &SyntaxNode, entry2: &SyntaxNode) -> bool {
        let key1 = entry1.children().find(|n| n.kind() == SyntaxKind::KEY);
        let key2 = entry2.children().find(|n| n.kind() == SyntaxKind::KEY);
        let value1 = entry1.children().find(|n| n.kind() == SyntaxKind::VALUE);
        let value2 = entry2.children().find(|n| n.kind() == SyntaxKind::VALUE);
        
        match ((key1, value1), (key2, value2)) {
            ((Some(k1), Some(v1)), (Some(k2), Some(v2))) => {
                self.compare_key_nodes(&k1, &k2) && self.compare_nodes_structurally(&v1, &v2)
            }
            ((Some(k1), None), (Some(k2), None)) => self.compare_key_nodes(&k1, &k2),
            ((None, Some(v1)), (None, Some(v2))) => self.compare_nodes_structurally(&v1, &v2),
            ((None, None), (None, None)) => true,
            _ => false,
        }
    }

    
    /// Check if a key node matches a field name by comparing CST structure
    fn key_node_matches_field(&self, key_node: &SyntaxNode, field: &str) -> bool {
        // Build a temporary KEY node for the field name to compare
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(SyntaxKind::KEY.into());
        builder.start_node(SyntaxKind::SCALAR.into());
        builder.token(SyntaxKind::STRING.into(), field);
        builder.finish_node(); // SCALAR
        builder.finish_node(); // KEY
        let expected_key = SyntaxNode::new_root_mut(builder.finish());
        
        // Use our structural comparison to check if they match
        // This handles all quote styles and formatting differences properly
        self.compare_key_content_for_field_match(key_node, &expected_key)
    }
    
    /// Compare key content for field matching (simpler than full key comparison)
    fn compare_key_content_for_field_match(&self, actual_key: &SyntaxNode, expected_key: &SyntaxNode) -> bool {
        // For field matching, we only care about scalar keys
        // Extract the actual scalar content from both keys
        let actual_scalar = self.extract_scalar_from_key(actual_key);
        let expected_scalar = self.extract_scalar_from_key(expected_key);
        
        match (actual_scalar, expected_scalar) {
            (Some(actual), Some(expected)) => {
                // Compare the scalar values
                self.scalars_match(&actual, &expected)
            }
            _ => false,
        }
    }
    
    /// Extract scalar node from a KEY node if it contains one
    fn extract_scalar_from_key(&self, key_node: &SyntaxNode) -> Option<SyntaxNode> {
        // A KEY node might contain:
        // - Direct STRING token (simple scalar)
        // - SCALAR node containing STRING
        // - Complex structure (sequence/mapping) - return None
        
        for child in key_node.children_with_tokens() {
            match child {
                rowan::NodeOrToken::Token(t) if t.kind() == SyntaxKind::STRING => {
                    // Create a synthetic SCALAR node for the token
                    let mut builder = GreenNodeBuilder::new();
                    builder.start_node(SyntaxKind::SCALAR.into());
                    builder.token(SyntaxKind::STRING.into(), t.text());
                    builder.finish_node();
                    return Some(SyntaxNode::new_root_mut(builder.finish()));
                }
                rowan::NodeOrToken::Node(n) if n.kind() == SyntaxKind::SCALAR => {
                    return Some(n);
                }
                _ => {}
            }
        }
        None
    }
    
    /// Check if two scalar nodes match (handling different quote styles)
    fn scalars_match(&self, scalar1: &SyntaxNode, scalar2: &SyntaxNode) -> bool {
        // Extract the actual string content from both scalars
        let content1 = self.extract_scalar_content(scalar1);
        let content2 = self.extract_scalar_content(scalar2);
        content1 == content2
    }
    
    /// Extract the actual content from a SCALAR node, handling quotes properly
    fn extract_scalar_content(&self, scalar_node: &SyntaxNode) -> String {
        // Look for STRING tokens within the scalar
        for child in scalar_node.children_with_tokens() {
            if let rowan::NodeOrToken::Token(t) = child {
                if t.kind() == SyntaxKind::STRING {
                    let text = t.text();
                    // Parse the YAML scalar properly based on its style
                    return parse_yaml_scalar_text(text);
                }
            }
        }
        String::new()
    }

    /// Set a key-value pair with field ordering support
    /// If the key exists, updates its value. If the key doesn't exist, inserts it
    /// at the correct position based on the provided field order.
    /// Fields not in the order list are placed at the end.
    pub fn set_with_field_order(
        &mut self,
        key: impl Into<YamlValue>,
        value: impl Into<YamlValue>,
        field_order: &[&str],
    ) {
        let key_yaml = key.into();
        let value_yaml = value.into();
        
        // First check if the key already exists - if so, just update it
        let children: Vec<_> = self.0.children_with_tokens().collect();
        for child in children.iter() {
            if let Some(node) = child.as_node() {
                if node.kind() == SyntaxKind::MAPPING_ENTRY {
                    if let Some(key_node) = node.children().find(|n| n.kind() == SyntaxKind::KEY) {
                        if self.keys_match(&key_node, &key_yaml) {
                            // Key exists, update its value using CST
                            self.set_key_value_cst(&key_yaml, &value_yaml);
                            return;
                        }
                    }
                }
            }
        }

        // Key doesn't exist, need to find the correct insertion position based on field order
        // Build a temporary key node to compare against field names
        let mut key_builder = GreenNodeBuilder::new();
        Document::build_key_content(&mut key_builder, &key_yaml, 0);
        let key_node = SyntaxNode::new_root_mut(key_builder.finish());
        
        // Find position of this key in the field order (if it matches any)
        let key_position_in_order = field_order.iter().position(|&field| {
            self.key_node_matches_field(&key_node, field)
        });

        if let Some(key_index) = key_position_in_order {
            // Key is in the field order, find the right position to insert
            let mut insert_after_node: Option<SyntaxNode> = None;
            let mut insert_before_node: Option<SyntaxNode> = None;

            // Look backwards in field_order to find the last existing key before this one
            for &field in field_order.iter().take(key_index).rev() {
                for child in children.iter() {
                    if let Some(node) = child.as_node() {
                        if node.kind() == SyntaxKind::MAPPING_ENTRY {
                            if let Some(key_node) = node.children().find(|n| n.kind() == SyntaxKind::KEY) {
                                if self.key_node_matches_field(&key_node, field) {
                                    insert_after_node = Some(node.clone());
                                    break;
                                }
                            }
                        }
                    }
                }
                if insert_after_node.is_some() {
                    break;
                }
            }

            // If no predecessor found, look forwards for the first existing key after this one
            if insert_after_node.is_none() {
                for &field in field_order.iter().skip(key_index + 1) {
                    for child in children.iter() {
                        if let Some(node) = child.as_node() {
                            if node.kind() == SyntaxKind::MAPPING_ENTRY {
                                if let Some(key_node) = node.children().find(|n| n.kind() == SyntaxKind::KEY) {
                                    if self.key_node_matches_field(&key_node, field) {
                                        insert_before_node = Some(node.clone());
                                        break;
                                    }
                                }
                            }
                        }
                    }
                    if insert_before_node.is_some() {
                        break;
                    }
                }
            }

            // Build the new entry using CST
            let mut builder = GreenNodeBuilder::new();
            builder.start_node(SyntaxKind::MAPPING_ENTRY.into());
            Document::build_key_content(&mut builder, &key_yaml, 0);
            builder.token(SyntaxKind::COLON.into(), ":");
            builder.token(SyntaxKind::WHITESPACE.into(), " ");
            Document::build_value_content(&mut builder, &value_yaml, 0);
            builder.finish_node();
            let new_entry = SyntaxNode::new_root_mut(builder.finish());

            // Insert at the appropriate position
            if let Some(after_node) = insert_after_node {
                // Find the index of after_node and insert after it
                if let Some(idx) = children.iter().position(|c| {
                    c.as_node().map_or(false, |n| n == &after_node)
                }) {
                    // Insert after the found node
                    // Create newline token properly
                    let mut nl_builder = GreenNodeBuilder::new();
                    nl_builder.start_node(SyntaxKind::ROOT.into());
                    nl_builder.token(SyntaxKind::NEWLINE.into(), "\n");
                    nl_builder.finish_node();
                    let nl_node = SyntaxNode::new_root_mut(nl_builder.finish());
                    let new_elements = vec![
                        nl_node.first_token().unwrap().into(),
                        new_entry.into()
                    ];
                    self.0.splice_children(idx + 1..idx + 1, new_elements);
                }
            } else if let Some(before_node) = insert_before_node {
                // Find the index of before_node and insert before it
                if let Some(idx) = children.iter().position(|c| {
                    c.as_node().map_or(false, |n| n == &before_node)
                }) {
                    // Insert before the found node
                    // Create newline token properly
                    let mut nl_builder = GreenNodeBuilder::new();
                    nl_builder.start_node(SyntaxKind::ROOT.into());
                    nl_builder.token(SyntaxKind::NEWLINE.into(), "\n");
                    nl_builder.finish_node();
                    let nl_node = SyntaxNode::new_root_mut(nl_builder.finish());
                    let new_elements = vec![
                        new_entry.into(),
                        nl_node.first_token().unwrap().into()
                    ];
                    self.0.splice_children(idx..idx, new_elements);
                }
            } else {
                // No existing ordered keys, just append using CST
                self.set_key_value_cst(&key_yaml, &value_yaml);
            }
        } else {
            // Key is not in field order, append at the end using CST
            self.set_key_value_cst(&key_yaml, &value_yaml);
        }
    }

    /// Set a key-value pair, replacing if exists or adding if new
    /// This is the low-level method that doesn't escape values.
    pub fn set_raw(&mut self, key: &str, value: &str) {
        // Look through children to find the key-value pair
        let children: Vec<_> = self.0.children_with_tokens().collect();

        for (i, child) in children.iter().enumerate() {
            // Look for KEY nodes
            if let Some(node) = child.as_node() {
                if node.kind() == SyntaxKind::KEY {
                    // Check if this key matches
                    let key_text = node.text().to_string();
                    if key_text.trim() == key.trim() {
                        // Found the key! Now find the corresponding VALUE node
                        // It should be after the colon
                        for j in (i + 1)..children.len() {
                            if let Some(token) = children[j].as_token() {
                                if token.kind() == SyntaxKind::COLON {
                                    // Found the colon, now find the VALUE node
                                    for k in (j + 1)..children.len() {
                                        if let Some(value_node) = children[k].as_node() {
                                            if value_node.kind() == SyntaxKind::VALUE {
                                                // Found the value to replace!
                                                // We need to preserve the structure of the VALUE node
                                                // The VALUE node may contain tokens with trailing whitespace
                                                // We should replace just the value content, not the whitespace

                                                // Build a new VALUE node that preserves trailing whitespace
                                                let mut builder = GreenNodeBuilder::new();
                                                builder.start_node(SyntaxKind::VALUE.into());

                                                // Check if the original VALUE has trailing whitespace
                                                let original_text = value_node.text().to_string();
                                                let trimmed = original_text.trim_end();
                                                let trailing_ws = &original_text[trimmed.len()..];

                                                // Add the new value with the preserved trailing whitespace
                                                builder.token(
                                                    SyntaxKind::STRING.into(),
                                                    &format!("{}{}", value, trailing_ws),
                                                );
                                                builder.finish_node();
                                                let new_value =
                                                    SyntaxNode::new_root_mut(builder.finish());

                                                // Replace just the VALUE node
                                                self.0.splice_children(
                                                    k..k + 1,
                                                    vec![new_value.into()],
                                                );
                                                return;
                                            }
                                        }
                                    }
                                    break;
                                }
                            }
                        }
                    }
                }
            }
        }

        // Key doesn't exist, add new entry at the end
        let count = self.0.children_with_tokens().count();

        // Detect the indentation level of existing entries
        let indent_level = self.detect_indentation_level();

        // Build an entry node containing the new key-value pair
        let mut entry_builder = GreenNodeBuilder::new();
        entry_builder.start_node(SyntaxKind::ROOT.into()); // Temporary root for building

        // Add newline if there are existing entries
        if count > 0 {
            entry_builder.token(SyntaxKind::NEWLINE.into(), "\n");
        }

        // Add indentation to match other entries
        if indent_level > 0 {
            let indent_str = " ".repeat(indent_level);
            entry_builder.token(SyntaxKind::INDENT.into(), &indent_str);
        }

        // Add the key as a SCALAR
        entry_builder.start_node(SyntaxKind::SCALAR.into());
        entry_builder.token(SyntaxKind::STRING.into(), key);
        entry_builder.finish_node();

        // Add colon and space
        entry_builder.token(SyntaxKind::COLON.into(), ":");
        entry_builder.token(SyntaxKind::WHITESPACE.into(), " ");

        // Add the value as a SCALAR
        entry_builder.start_node(SyntaxKind::SCALAR.into());
        entry_builder.token(SyntaxKind::STRING.into(), value);
        entry_builder.finish_node();

        entry_builder.finish_node();

        // Create a temporary node to extract the elements
        let temp_node = SyntaxNode::new_root_mut(entry_builder.finish());
        let new_elements: Vec<_> = temp_node.children_with_tokens().collect();

        // Append at the end
        self.0.splice_children(count..count, new_elements);
    }

    /// Detect the indentation level used by entries in this mapping
    pub fn detect_indentation_level(&self) -> usize {
        // Look for an INDENT token that precedes a KEY
        for child in self.0.children_with_tokens() {
            if let Some(token) = child.as_token() {
                if token.kind() == SyntaxKind::INDENT {
                    return token.text().len();
                }
            }
        }
        0 // No indentation found, must be root level
    }

    // Helper to rebuild mapping from pairs - similar to deb822 approach
    fn rebuild_from_pairs(&mut self, pairs: Vec<(Scalar, SyntaxNode)>) {
        // Build a new complete MAPPING node with all children
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(SyntaxKind::MAPPING.into());

        for (i, (key_scalar, value_node)) in pairs.iter().enumerate() {
            // Add MAPPING_ENTRY node
            builder.start_node(SyntaxKind::MAPPING_ENTRY.into());

            // Add KEY node
            builder.start_node(SyntaxKind::KEY.into());
            builder.start_node(SyntaxKind::SCALAR.into());
            builder.token(SyntaxKind::VALUE.into(), &key_scalar.value());
            builder.finish_node(); // SCALAR
            builder.finish_node(); // KEY

            // Add colon and space
            builder.token(SyntaxKind::COLON.into(), ":");
            builder.token(SyntaxKind::WHITESPACE.into(), " ");

            // Add VALUE node
            builder.start_node(SyntaxKind::VALUE.into());
            builder.start_node(SyntaxKind::SCALAR.into());
            builder.token(SyntaxKind::VALUE.into(), &value_node.text().to_string());
            builder.finish_node(); // SCALAR
            builder.finish_node(); // VALUE

            builder.finish_node(); // MAPPING_ENTRY

            // Add newline after each entry
            if i < pairs.len() - 1 {
                builder.token(SyntaxKind::NEWLINE.into(), "\n");
            }
        }

        builder.finish_node();

        // Create the new mapping node and replace self
        let new_mapping = SyntaxNode::new_root_mut(builder.finish());

        // Get all children from the new mapping
        let new_children: Vec<_> = new_mapping.children_with_tokens().collect();

        // Replace all children of the current mapping node
        let child_count = self.0.children_with_tokens().count();
        self.0.splice_children(0..child_count, new_children);
    }

    /// Insert a key-value pair after an existing key, preserving formatting
    /// Returns true if successful, false if the reference key wasn't found
    pub fn insert_after_preserving(
        &mut self,
        after_key: &str,
        new_key: impl Into<YamlValue>,
        new_value: impl Into<YamlValue>,
    ) -> bool {
        let new_key_value = new_key.into();
        let new_value_value = new_value.into();
        let new_key_str = new_key_value.to_string();

        self.insert_after_preserving_impl(&after_key, &new_key_str, &new_value_value)
    }

    /// Internal implementation for insert_after_preserving
    fn insert_after_preserving_impl(
        &mut self,
        after_key: &str,
        new_key: &str,
        new_value: &YamlValue,
    ) -> bool {
        let children: Vec<_> = self.0.children_with_tokens().collect();
        let mut insert_position = None;
        let mut found_key = false;
        let mut last_value_end = 0;

        // First, check if the new key already exists and remove it
        let mut i = 0;
        let mut removed_existing = false;
        while i < children.len() {
            if let Some(node) = children[i].as_node() {
                if node.kind() == SyntaxKind::MAPPING_ENTRY {
                    // Look inside the MAPPING_ENTRY for the KEY
                    for key_child in node.children() {
                        if key_child.kind() == SyntaxKind::KEY {
                            let key_text = key_child.text().to_string();
                            if key_text.trim() == new_key {
                                // Found existing key, remove this entire MAPPING_ENTRY
                                let mut remove_range = i..i + 1;

                                // Also remove any trailing newline
                                if i + 1 < children.len() {
                                    if let Some(token) = children[i + 1].as_token() {
                                        if token.kind() == SyntaxKind::NEWLINE {
                                            remove_range = i..i + 2;
                                        }
                                    }
                                }

                                self.0.splice_children(remove_range, vec![]);
                                removed_existing = true;
                                break;
                            }
                        }
                    }
                    if removed_existing {
                        // Need to refresh children list after removal
                        break;
                    }
                }
            }
            if !removed_existing {
                i += 1;
            }
        }

        // If we removed an existing key, refresh the children list
        let children = if removed_existing {
            self.0.children_with_tokens().collect()
        } else {
            children
        };

        // Find the position after the specified key's value
        for (i, child) in children.iter().enumerate() {
            if let Some(node) = child.as_node() {
                if node.kind() == SyntaxKind::MAPPING_ENTRY {
                    // Look inside the MAPPING_ENTRY for the KEY
                    for key_child in node.children() {
                        if key_child.kind() == SyntaxKind::KEY {
                            let key_text = key_child.text().to_string();
                            if key_text.trim() == after_key {
                                found_key = true;
                                last_value_end = i + 1; // After this entire MAPPING_ENTRY
                                break;
                            }
                        }
                    }
                } else if node.kind() == SyntaxKind::KEY {
                    // For KEY nodes, check the text content
                    let key_text = node.text().to_string();
                    if key_text.trim() == after_key {
                        found_key = true;
                    }
                } else if node.kind() == SyntaxKind::SCALAR {
                    // For SCALAR nodes that might be keys
                    let scalar_text = node.text().to_string();
                    if scalar_text.trim() == after_key && !found_key {
                        // This is likely the key we're looking for
                        found_key = true;
                        // Look ahead for the value
                        for j in (i + 1)..children.len() {
                            if let Some(n) = children[j].as_node() {
                                if n.kind() == SyntaxKind::VALUE || n.kind() == SyntaxKind::SCALAR {
                                    last_value_end = j + 1;
                                    break;
                                }
                            }
                        }
                    }
                } else if node.kind() == SyntaxKind::VALUE && found_key {
                    // We're at the value of the found key
                    last_value_end = i + 1;
                }
            } else if let Some(token) = child.as_token() {
                if found_key && token.kind() == SyntaxKind::NEWLINE {
                    // Found newline after the key-value pair
                    insert_position = Some(i + 1);
                    break;
                }
            }
        }

        // If we didn't find a newline but found the key, insert after the value
        if insert_position.is_none() && found_key && last_value_end > 0 {
            insert_position = Some(last_value_end);
        }

        if let Some(pos) = insert_position {
            // Create new elements for the key-value pair
            let mut new_elements = Vec::new();

            // Check if the position is right after a newline token
            let inserting_after_newline = if pos > 0 {
                if let Some(token) = children.get(pos - 1).and_then(|c| c.as_token()) {
                    token.kind() == SyntaxKind::NEWLINE
                } else {
                    false
                }
            } else {
                false
            };

            // Add newline token only if we're not already after a newline
            if !inserting_after_newline {
                let mut newline_builder = GreenNodeBuilder::new();
                newline_builder.start_node(SyntaxKind::ROOT.into());
                newline_builder.token(SyntaxKind::NEWLINE.into(), "\n");
                newline_builder.finish_node();
                let newline_node = SyntaxNode::new_root_mut(newline_builder.finish());
                if let Some(token) = newline_node.first_token() {
                    new_elements.push(token.into());
                }
            }

            // Create the MAPPING_ENTRY node
            let entry = self.create_mapping_entry(new_key, new_value);
            new_elements.push(entry.into());

            // Add trailing newline only if there are more elements after this position
            if pos < children.len() {
                let mut trailing_newline_builder = GreenNodeBuilder::new();
                trailing_newline_builder.start_node(SyntaxKind::ROOT.into());
                trailing_newline_builder.token(SyntaxKind::NEWLINE.into(), "\n");
                trailing_newline_builder.finish_node();
                let trailing_newline_node =
                    SyntaxNode::new_root_mut(trailing_newline_builder.finish());
                if let Some(token) = trailing_newline_node.first_token() {
                    new_elements.push(token.into());
                }
            }

            // Splice in the new elements
            self.0.splice_children(pos..pos, new_elements);
            true
        } else {
            false
        }
    }

    /// Insert a key-value pair before an existing key, preserving formatting
    /// If the new key already exists, it will replace the existing value
    pub fn insert_before_preserving(
        &mut self,
        before_key: &str,
        new_key: impl Into<YamlValue>,
        new_value: impl Into<YamlValue>,
    ) -> bool {
        let new_key_value = new_key.into();
        let new_value_value = new_value.into();
        let new_key_str = new_key_value.to_string();

        self.insert_before_preserving_impl(&before_key, &new_key_str, &new_value_value)
    }

    /// Internal implementation for insert_before_preserving
    fn insert_before_preserving_impl(
        &mut self,
        before_key: &str,
        new_key: &str,
        new_value: &YamlValue,
    ) -> bool {
        let children: Vec<_> = self.0.children_with_tokens().collect();
        let mut insert_position = None;

        // First, check if the new key already exists and remove it
        let mut i = 0;
        let mut removed_existing = false;
        while i < children.len() {
            if let Some(node) = children[i].as_node() {
                if node.kind() == SyntaxKind::MAPPING_ENTRY {
                    // Look inside the MAPPING_ENTRY for the KEY
                    for key_child in node.children() {
                        if key_child.kind() == SyntaxKind::KEY {
                            let key_text = key_child.text().to_string();
                            if key_text.trim() == new_key {
                                // Found existing key, remove this entire MAPPING_ENTRY
                                let mut remove_range = i..i + 1;

                                // Also remove any trailing newline
                                if i + 1 < children.len() {
                                    if let Some(token) = children[i + 1].as_token() {
                                        if token.kind() == SyntaxKind::NEWLINE {
                                            remove_range = i..i + 2;
                                        }
                                    }
                                }

                                self.0.splice_children(remove_range, vec![]);
                                removed_existing = true;
                                break;
                            }
                        }
                    }
                    if removed_existing {
                        // Need to refresh children list after removal
                        break;
                    }
                } else if node.kind() == SyntaxKind::KEY || node.kind() == SyntaxKind::SCALAR {
                    let key_text = node.text().to_string();
                    if key_text.trim() == new_key {
                        // Found existing key, find its VALUE node and replace just that
                        // Look for colon, then VALUE node
                        for j in (i + 1)..children.len() {
                            if let Some(node) = children[j].as_node() {
                                if node.kind() == SyntaxKind::VALUE {
                                    // Found the VALUE node to replace
                                    // Build new VALUE node using the helper
                                    let mut value_builder = GreenNodeBuilder::new();
                                    Document::build_value_content(&mut value_builder, new_value, 2);
                                    let new_value_node =
                                        SyntaxNode::new_root_mut(value_builder.finish());

                                    // Replace just the VALUE node
                                    self.0
                                        .splice_children(j..j + 1, vec![new_value_node.into()]);
                                    return true;
                                }
                            }
                        }
                        // If no VALUE node found, something's wrong with the structure
                        return false;
                    }
                }
            }
            if !removed_existing {
                i += 1;
            }
        }

        // If we removed an existing key, refresh the children list
        let children = if removed_existing {
            self.0.children_with_tokens().collect()
        } else {
            children
        };

        // Find the position before the specified key
        for (i, child) in children.iter().enumerate() {
            if let Some(node) = child.as_node() {
                if node.kind() == SyntaxKind::MAPPING_ENTRY {
                    // Look inside the MAPPING_ENTRY for the KEY
                    for key_child in node.children() {
                        if key_child.kind() == SyntaxKind::KEY {
                            let key_text = key_child.text().to_string();
                            if key_text.trim() == before_key {
                                // Found the key, insert before this MAPPING_ENTRY
                                let mut line_start = i;
                                for j in (0..i).rev() {
                                    if let Some(token) = children[j].as_token() {
                                        if token.kind() == SyntaxKind::NEWLINE {
                                            line_start = j + 1;
                                            break;
                                        }
                                    }
                                }
                                insert_position = Some(line_start);
                                break;
                            }
                        }
                    }
                } else if node.kind() == SyntaxKind::KEY || node.kind() == SyntaxKind::SCALAR {
                    let key_text = node.text().to_string();
                    if key_text.trim() == before_key {
                        // Found the key, insert before it
                        // Look back to find the start of this line
                        let mut line_start = i;
                        for j in (0..i).rev() {
                            if let Some(token) = children[j].as_token() {
                                if token.kind() == SyntaxKind::NEWLINE {
                                    line_start = j + 1;
                                    break;
                                }
                            }
                        }
                        insert_position = Some(line_start);
                        break;
                    }
                }
            }
        }

        if let Some(pos) = insert_position {
            // Create new AST elements for the key-value pair
            // Build the complete key-value entry as separate nodes/tokens

            // We need to build each element as a SyntaxNode/Token and collect them
            let mut new_elements = Vec::new();

            // Create the MAPPING_ENTRY node
            let entry = self.create_mapping_entry(new_key, new_value);
            new_elements.push(entry.into());

            // Add newline token
            let mut newline_builder = GreenNodeBuilder::new();
            newline_builder.start_node(SyntaxKind::ROOT.into());
            newline_builder.token(SyntaxKind::NEWLINE.into(), "\n");
            newline_builder.finish_node();
            let newline_node = SyntaxNode::new_root_mut(newline_builder.finish());
            if let Some(token) = newline_node.first_token() {
                new_elements.push(token.into());
            }

            // Splice in the new elements
            self.0.splice_children(pos..pos, new_elements);
            true
        } else {
            false
        }
    }

    /// Insert a key-value pair at a specific index, preserving formatting
    /// If the new key already exists, it will replace the existing value
    pub fn insert_at_index_preserving(
        &mut self,
        _index: usize,
        new_key: &str,
        new_value: &YamlValue,
    ) {
        // Check if the key already exists - if so, update just the VALUE node
        let children: Vec<_> = self.0.children_with_tokens().collect();

        for (i, child) in children.iter().enumerate() {
            if let Some(node) = child.as_node() {
                if node.kind() == SyntaxKind::KEY || node.kind() == SyntaxKind::SCALAR {
                    let key_text = node.text().to_string();
                    println!("Found key: '{}'", key_text.trim());
                    if key_text.trim() == new_key {
                        // Found existing key, find its VALUE node and replace just that
                        for j in (i + 1)..children.len() {
                            if let Some(value_node) = children[j].as_node() {
                                if value_node.kind() == SyntaxKind::VALUE {
                                    // Build new VALUE node using the helper
                                    let mut value_builder = GreenNodeBuilder::new();
                                    Document::build_value_content(&mut value_builder, new_value, 2);
                                    let new_value_node =
                                        SyntaxNode::new_root_mut(value_builder.finish());

                                    // Replace just the VALUE node
                                    self.0
                                        .splice_children(j..j + 1, vec![new_value_node.into()]);
                                    return;
                                }
                            }
                        }
                    }
                }
            }
        }

        // Key doesn't exist, add it at the end (ignoring index for now)
        // TODO: Implement proper index-based insertion
        let count = self.0.children_with_tokens().count();
        let mut new_elements = Vec::new();

        // Add newline if there are existing entries
        if count > 0 {
            let mut nl_builder = GreenNodeBuilder::new();
            nl_builder.start_node(SyntaxKind::ROOT.into());
            nl_builder.token(SyntaxKind::NEWLINE.into(), "\n");
            nl_builder.finish_node();
            let nl_node = SyntaxNode::new_root_mut(nl_builder.finish());
            if let Some(token) = nl_node.first_token() {
                new_elements.push(token.into());
            }
        }

        // Create the MAPPING_ENTRY node
        let entry = self.create_mapping_entry(new_key, new_value);
        new_elements.push(entry.into());

        // Add to the end
        self.0.splice_children(count..count, new_elements);
    }

    /// Helper to detect indentation level at a position from children vector
    fn detect_indentation_from_children(
        &self,
        children: &[rowan::SyntaxElement<Lang>],
        _position: usize,
    ) -> String {
        // Look for whitespace before keys in the mapping
        for i in 0..children.len() {
            if let Some(node) = children[i].as_node() {
                if node.kind() == SyntaxKind::KEY || node.kind() == SyntaxKind::SCALAR {
                    // Check if there's whitespace before this key
                    if i > 0 {
                        if let Some(token) = children[i - 1].as_token() {
                            if token.kind() == SyntaxKind::WHITESPACE {
                                let text = token.text();
                                // Only use if it looks like indentation (not just a single space)
                                if text.chars().all(|c| c == ' ' || c == '\t') && text.len() > 1 {
                                    return text.to_string();
                                }
                            }
                        }
                    }
                }
            }
        }
        String::new()
    }

    /// Remove a key-value pair
    pub fn remove(&mut self, key: &str) -> bool {
        let children: Vec<_> = self.0.children_with_tokens().collect();

        // Find the entry to remove
        for (i, child) in children.iter().enumerate() {
            if let Some(node) = child.as_node() {
                if node.kind() == SyntaxKind::MAPPING_ENTRY {
                    if let Some(key_node) = node.children().find(|n| n.kind() == SyntaxKind::KEY) {
                        if key_node.text().to_string().trim() == key.trim() {
                            // Found the entry to remove

                            // Check if this is the last MAPPING_ENTRY
                            let is_last = !children.iter().skip(i + 1).any(|c| {
                                c.as_node()
                                    .map_or(false, |n| n.kind() == SyntaxKind::MAPPING_ENTRY)
                            });

                            if is_last && i > 0 {
                                // For the last entry, remove the preceding newline and the entry
                                if let Some(token) = children[i - 1].as_token() {
                                    if token.kind() == SyntaxKind::NEWLINE {
                                        // Remove the entry first, then the newline
                                        // (removing in reverse order to avoid index shifting issues)
                                        self.0.splice_children(i..(i + 1), vec![]);
                                        // Now the newline is still at index i-1
                                        self.0.splice_children((i - 1)..i, vec![]);
                                        return true;
                                    }
                                }
                                // No preceding newline, just remove the entry
                                self.0.splice_children(i..(i + 1), vec![]);
                            } else if i + 1 < children.len() {
                                // For non-last entries, also remove the following newline if present
                                if let Some(token) = children[i + 1].as_token() {
                                    if token.kind() == SyntaxKind::NEWLINE {
                                        // Remove both the entry and the following newline
                                        self.0.splice_children(i..(i + 2), vec![]);
                                        return true;
                                    }
                                }
                                // No following newline, just remove the entry
                                self.0.splice_children(i..(i + 1), vec![]);
                            } else {
                                // Just remove the entry
                                self.0.splice_children(i..(i + 1), vec![]);
                            }
                            return true;
                        }
                    }
                }
            }
        }
        false
    }

    /// Rename a key while preserving its value and formatting, with proper escaping
    pub fn rename_key(&mut self, old_key: &str, new_key: impl Into<ScalarValue>) -> bool {
        let new_key_scalar = new_key.into();
        self.rename_key_raw(old_key, &new_key_scalar.to_yaml_string())
    }

    /// Rename a key while preserving its value and formatting
    /// This is the low-level method that doesn't escape the new key.
    pub fn rename_key_raw(&mut self, old_key: &str, new_key: &str) -> bool {
        let children: Vec<_> = self.0.children_with_tokens().collect();

        for (i, child) in children.iter().enumerate() {
            if let Some(node) = child.as_node() {
                if node.kind() == SyntaxKind::MAPPING_ENTRY {
                    // Find the KEY node within this entry
                    if let Some(key_node) = node.children().find(|n| n.kind() == SyntaxKind::KEY) {
                        if key_node.text().to_string().trim() == old_key.trim() {
                            // We need to replace just the KEY node within the MAPPING_ENTRY
                            // First, get the MAPPING_ENTRY's children
                            let entry_children: Vec<_> = node.children_with_tokens().collect();

                            // Build a new MAPPING_ENTRY with the new key
                            let mut builder = GreenNodeBuilder::new();
                            builder.start_node(SyntaxKind::MAPPING_ENTRY.into());

                            for entry_child in entry_children {
                                match entry_child {
                                    rowan::NodeOrToken::Node(n) if n.kind() == SyntaxKind::KEY => {
                                        // Replace the KEY node
                                        builder.start_node(SyntaxKind::KEY.into());
                                        builder.start_node(SyntaxKind::SCALAR.into());
                                        builder.token(SyntaxKind::VALUE.into(), new_key);
                                        builder.finish_node(); // SCALAR
                                        builder.finish_node(); // KEY
                                    }
                                    rowan::NodeOrToken::Node(n) => {
                                        // Copy other nodes as-is
                                        self.copy_node_to_builder(&mut builder, &n);
                                    }
                                    rowan::NodeOrToken::Token(t) => {
                                        // Copy tokens as-is
                                        builder.token(t.kind().into(), t.text());
                                    }
                                }
                            }

                            builder.finish_node();
                            let new_entry = SyntaxNode::new_root_mut(builder.finish());

                            // Replace the old MAPPING_ENTRY with the new one
                            self.0.splice_children(i..i + 1, vec![new_entry.into()]);
                            return true;
                        }
                    }
                }
            }
        }
        false
    }

    /// Helper to create a MAPPING_ENTRY node from key and value strings
    fn create_mapping_entry(&self, key: &str, value: &YamlValue) -> SyntaxNode {
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(SyntaxKind::MAPPING_ENTRY.into());

        // Add KEY node
        builder.start_node(SyntaxKind::KEY.into());
        builder.start_node(SyntaxKind::SCALAR.into());
        builder.token(SyntaxKind::STRING.into(), key);
        builder.finish_node(); // SCALAR
        builder.finish_node(); // KEY

        // Add colon and space
        builder.token(SyntaxKind::COLON.into(), ":");
        builder.token(SyntaxKind::WHITESPACE.into(), " ");

        // Add VALUE node using the helper
        Document::build_value_content(&mut builder, value, 2);

        builder.finish_node(); // MAPPING_ENTRY
        SyntaxNode::new_root_mut(builder.finish())
    }

    /// Helper to copy a node and all its children to a builder
    fn copy_node_to_builder(&self, builder: &mut GreenNodeBuilder, node: &SyntaxNode) {
        builder.start_node(node.kind().into());
        for child in node.children_with_tokens() {
            match child {
                rowan::NodeOrToken::Node(n) => {
                    self.copy_node_to_builder(builder, &n);
                }
                rowan::NodeOrToken::Token(t) => {
                    builder.token(t.kind().into(), t.text());
                }
            }
        }
        builder.finish_node();
    }

    /// Set a value at a nested path with proper escaping (e.g., "db.host" to set db: {host: value})
    pub fn set_path(&mut self, path: &str, value: impl Into<ScalarValue>) {
        let value_scalar = value.into();
        self.set_path_raw(path, &value_scalar.to_yaml_string());
    }

    /// Set a value at a nested path (e.g., "db.host" to set db: {host: value})
    /// This is the low-level method that doesn't escape values.
    pub fn set_path_raw(&mut self, path: &str, value: &str) {
        let parts: Vec<&str> = path.split('.').collect();
        if parts.len() == 1 {
            self.set_raw(parts[0], value);
            return;
        }

        // Navigate to nested structure
        let (first, rest) = parts.split_first().unwrap();

        // Build the nested structure from the path
        let nested_value = create_nested_value(rest, value);

        // Set the value using the mapping's existing set method
        self.set_value(ScalarValue::new((*first).to_string()), nested_value);
    }

    /// Insert a key-value pair after a specific existing key
    /// Returns true if the reference key was found and insertion succeeded
    pub fn insert_after(
        &mut self,
        after_key: impl Into<YamlValue>,
        key: impl Into<YamlValue>,
        value: impl Into<YamlValue>,
    ) -> bool {
        let after_key_yaml = after_key.into();
        let key_yaml = key.into();
        let value_yaml = value.into();
        
        // Check if the new key already exists - if so, just update it
        if let Some(_existing) = self.find_entry_by_key(&key_yaml) {
            self.set_key_value_cst(&key_yaml, &value_yaml);
            return true;
        }
        
        // Find the position of after_key
        if let Some(after_index) = self.find_entry_index_by_key(&after_key_yaml) {
            // Create the new mapping entry
            let new_entry = MappingEntry::from_yaml_values(&key_yaml, &value_yaml);
            
            // Build the elements to insert (newline + entry)
            let mut new_elements = Vec::new();
            
            // Add newline
            let mut nl_builder = GreenNodeBuilder::new();
            nl_builder.start_node(SyntaxKind::ROOT.into());
            nl_builder.token(SyntaxKind::NEWLINE.into(), "\n");
            nl_builder.finish_node();
            let nl_node = SyntaxNode::new_root_mut(nl_builder.finish());
            if let Some(token) = nl_node.first_token() {
                new_elements.push(token.into());
            }
            
            // Add indentation if needed
            let indent_level = self.detect_indentation_level();
            if indent_level > 0 {
                let mut indent_builder = GreenNodeBuilder::new();
                indent_builder.start_node(SyntaxKind::ROOT.into());
                indent_builder.token(SyntaxKind::INDENT.into(), &" ".repeat(indent_level));
                indent_builder.finish_node();
                let indent_node = SyntaxNode::new_root_mut(indent_builder.finish());
                if let Some(token) = indent_node.first_token() {
                    new_elements.push(token.into());
                }
            }
            
            // Add the new entry
            new_elements.push(new_entry.0.into());
            
            // Insert after the found position
            self.0.splice_children(after_index + 1..after_index + 1, new_elements);
            return true;
        }
        
        false
    }


    /// Insert a key-value pair before a specific existing key
    /// Returns true if the reference key was found and insertion succeeded
    pub fn insert_before(
        &mut self,
        before_key: impl Into<YamlValue>,
        key: impl Into<YamlValue>,
        value: impl Into<YamlValue>,
    ) -> bool {
        let before_key_yaml = before_key.into();
        let key_yaml = key.into();
        let value_yaml = value.into();
        
        // Check if the new key already exists - if so, just update it
        if let Some(_existing) = self.find_entry_by_key(&key_yaml) {
            self.set_key_value_cst(&key_yaml, &value_yaml);
            return true;
        }
        
        // Find the position of before_key
        if let Some(before_index) = self.find_entry_index_by_key(&before_key_yaml) {
            // Create the new mapping entry
            let new_entry = MappingEntry::from_yaml_values(&key_yaml, &value_yaml);
            
            // Build the elements to insert (entry + newline)
            let mut new_elements = Vec::new();
            
            // Add indentation if needed (for the new entry)
            let indent_level = self.detect_indentation_level();
            if indent_level > 0 && before_index > 0 {
                let mut indent_builder = GreenNodeBuilder::new();
                indent_builder.start_node(SyntaxKind::ROOT.into());
                indent_builder.token(SyntaxKind::INDENT.into(), &" ".repeat(indent_level));
                indent_builder.finish_node();
                let indent_node = SyntaxNode::new_root_mut(indent_builder.finish());
                if let Some(token) = indent_node.first_token() {
                    new_elements.push(token.into());
                }
            }
            
            // Add the new entry
            new_elements.push(new_entry.0.into());
            
            // Add newline after the entry
            let mut nl_builder = GreenNodeBuilder::new();
            nl_builder.start_node(SyntaxKind::ROOT.into());
            nl_builder.token(SyntaxKind::NEWLINE.into(), "\n");
            nl_builder.finish_node();
            let nl_node = SyntaxNode::new_root_mut(nl_builder.finish());
            if let Some(token) = nl_node.first_token() {
                new_elements.push(token.into());
            }
            
            // Insert before the found position
            self.0.splice_children(before_index..before_index, new_elements);
            return true;
        }
        
        false
    }


    /// Insert a key-value pair at a specific index (0-based)
    /// If index is out of bounds, appends at the end
    pub fn insert_at_index(
        &mut self,
        index: usize,
        key: impl Into<YamlValue>,
        value: impl Into<YamlValue>,
    ) {
        let key_yaml = key.into();
        let value_yaml = value.into();
        
        // Check if the key already exists - if so, just update it
        if let Some(_existing) = self.find_entry_by_key(&key_yaml) {
            self.set_key_value_cst(&key_yaml, &value_yaml);
            return;
        }
        
        // Create the new mapping entry
        let new_entry = MappingEntry::from_yaml_values(&key_yaml, &value_yaml);
        
        // Count existing entries to determine actual insertion position
        let entry_count = self.entries().count();
        let actual_index = index.min(entry_count);
        
        // Find the position in children_with_tokens corresponding to the nth entry
        let mut entry_positions = Vec::new();
        for (i, child) in self.0.children_with_tokens().enumerate() {
            if let Some(node) = child.as_node() {
                if MappingEntry::cast(node.clone()).is_some() {
                    entry_positions.push(i);
                }
            }
        }
        
        // Determine where to insert
        let insert_pos = if actual_index < entry_positions.len() {
            entry_positions[actual_index]
        } else {
            self.0.children_with_tokens().count()
        };
        
        // Build the elements to insert
        let mut new_elements = Vec::new();
        
        // Add leading newline and indentation if not inserting at the beginning
        if insert_pos > 0 {
            let mut nl_builder = GreenNodeBuilder::new();
            nl_builder.start_node(SyntaxKind::ROOT.into());
            nl_builder.token(SyntaxKind::NEWLINE.into(), "\n");
            nl_builder.finish_node();
            let nl_node = SyntaxNode::new_root_mut(nl_builder.finish());
            if let Some(token) = nl_node.first_token() {
                new_elements.push(token.into());
            }
            
            let indent_level = self.detect_indentation_level();
            if indent_level > 0 {
                let mut indent_builder = GreenNodeBuilder::new();
                indent_builder.start_node(SyntaxKind::ROOT.into());
                indent_builder.token(SyntaxKind::INDENT.into(), &" ".repeat(indent_level));
                indent_builder.finish_node();
                let indent_node = SyntaxNode::new_root_mut(indent_builder.finish());
                if let Some(token) = indent_node.first_token() {
                    new_elements.push(token.into());
                }
            }
        }
        
        // Add the new entry
        new_elements.push(new_entry.0.into());
        
        // Insert at the calculated position
        self.0.splice_children(insert_pos..insert_pos, new_elements);
    }

}

/// Create a nested YamlValue from a path
fn create_nested_value(path_parts: &[&str], final_value: &str) -> YamlValue {
    use std::collections::BTreeMap;

    if path_parts.is_empty() {
        return YamlValue::Scalar(ScalarValue::new(final_value.to_string()));
    }

    if path_parts.len() == 1 {
        // Create a single-level mapping
        let mut map = BTreeMap::new();
        map.insert(
            path_parts[0].to_string(),
            YamlValue::Scalar(ScalarValue::new(final_value.to_string())),
        );
        return YamlValue::Mapping(map);
    }

    // Recursively create nested mappings
    let (first, rest) = path_parts.split_first().unwrap();
    let mut map = BTreeMap::new();
    map.insert((*first).to_string(), create_nested_value(rest, final_value));
    YamlValue::Mapping(map)
}

fn create_scalar_node(value: &str) -> SyntaxNode {
    SyntaxNode::new_root_mut(create_scalar_green(value))
}

fn create_sequence_item_green(
    value: &str,
) -> Vec<rowan::NodeOrToken<rowan::GreenNode, rowan::GreenToken>> {
    vec![
        create_token_green(SyntaxKind::DASH, "-").into(),
        create_token_green(SyntaxKind::WHITESPACE, " ").into(),
        create_scalar_green(value).into(),
        create_token_green(SyntaxKind::NEWLINE, "\n").into(),
    ]
}

fn create_sequence_item_green_with_indent(
    value: &str,
    indentation: &str,
) -> Vec<rowan::NodeOrToken<rowan::GreenNode, rowan::GreenToken>> {
    vec![
        create_token_green(SyntaxKind::NEWLINE, "\n").into(),
        create_token_green(SyntaxKind::WHITESPACE, indentation).into(),
        create_token_green(SyntaxKind::DASH, "-").into(),
        create_token_green(SyntaxKind::WHITESPACE, " ").into(),
        create_scalar_green(value).into(),
    ]
}

// Editing methods for Sequence
impl Sequence {
    /// Add an item to the end of the sequence (generic version)
    pub fn push<T: Into<ScalarValue>>(&mut self, value: T) {
        let scalar_value = value.into();
        self.push_str(&scalar_value.to_string());
    }

    /// Add a string item to the end of the sequence
    pub fn push_str(&mut self, value: &str) {
        // Detect the indentation by looking at existing SEQUENCE_ENTRY nodes
        let mut indentation = "  ".to_string(); // Default indentation

        // Look for existing SEQUENCE_ENTRY nodes to determine indentation
        // First, check if there's an INDENT token between entries
        let all_children: Vec<_> = self.0.children_with_tokens().collect();
        for child in all_children.iter() {
            if let Some(token) = child.as_token() {
                if token.kind() == SyntaxKind::INDENT {
                    indentation = token.text().to_string();
                    break;
                }
            }
        }

        // If no INDENT token found, look within SEQUENCE_ENTRY nodes
        if indentation == "  " {
            for child in self.0.children() {
                if child.kind() == SyntaxKind::SEQUENCE_ENTRY {
                    // Look for WHITESPACE token before DASH
                    let tokens: Vec<_> = child.children_with_tokens().collect();
                    for (i, token) in tokens.iter().enumerate() {
                        if let Some(t) = token.as_token() {
                            if t.kind() == SyntaxKind::WHITESPACE && i + 1 < tokens.len() {
                                if let Some(next_t) = tokens[i + 1].as_token() {
                                    if next_t.kind() == SyntaxKind::DASH {
                                        // Found indentation before dash
                                        indentation = t.text().to_string();
                                        break;
                                    }
                                }
                            }
                        }
                    }
                    if !indentation.is_empty() && indentation != "  " {
                        break;
                    }
                }
            }
        }

        // Build a newline token and a SEQUENCE_ENTRY node separately
        let mut newline_builder = GreenNodeBuilder::new();
        newline_builder.start_node(SyntaxKind::ROOT.into());
        newline_builder.token(SyntaxKind::NEWLINE.into(), "\n");
        newline_builder.finish_node();
        let newline_node = SyntaxNode::new_root_mut(newline_builder.finish());
        let newline_token = newline_node.first_token().unwrap();

        // Build the SEQUENCE_ENTRY node
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(SyntaxKind::SEQUENCE_ENTRY.into());

        // Add indentation, dash, space, and value
        builder.token(SyntaxKind::WHITESPACE.into(), &indentation);
        builder.token(SyntaxKind::DASH.into(), "-");
        builder.token(SyntaxKind::WHITESPACE.into(), " ");

        // Add the scalar value
        builder.start_node(SyntaxKind::SCALAR.into());
        builder.token(SyntaxKind::VALUE.into(), value);
        builder.finish_node(); // SCALAR

        builder.finish_node(); // SEQUENCE_ENTRY

        let new_entry = SyntaxNode::new_root_mut(builder.finish());

        // Find the position to insert - before any trailing newlines
        let children: Vec<_> = self.0.children_with_tokens().collect();
        let mut insert_pos = children.len();

        // Find trailing newlines and insert before them
        for i in (0..children.len()).rev() {
            if let Some(token) = children[i].as_token() {
                if token.kind() == SyntaxKind::NEWLINE {
                    insert_pos = i;
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        // Add the newline and new entry before trailing newlines
        self.0.splice_children(
            insert_pos..insert_pos,
            vec![newline_token.into(), new_entry.into()],
        );
    }

    /// Insert an item at a specific position
    pub fn insert(&mut self, index: usize, value: &str) {
        let children: Vec<_> = self.0.children_with_tokens().collect();
        let mut item_count = 0;
        let mut insert_pos = children.len();

        // Find the position to insert at
        for (i, child) in children.iter().enumerate() {
            if let Some(token) = child.as_token() {
                if token.kind() == SyntaxKind::DASH {
                    if item_count == index {
                        insert_pos = i;
                        break;
                    }
                    item_count += 1;
                }
            }
        }

        let new_item_tokens = create_sequence_item_green(value);
        // Convert green nodes/tokens to syntax nodes/tokens
        let syntax_elements: Vec<_> = new_item_tokens
            .into_iter()
            .map(|element| match element {
                rowan::NodeOrToken::Node(green_node) => SyntaxNode::new_root_mut(green_node).into(),
                rowan::NodeOrToken::Token(green_token) => {
                    let mut builder = GreenNodeBuilder::new();
                    builder.start_node(SyntaxKind::ROOT.into());
                    builder.token(green_token.kind(), green_token.text());
                    builder.finish_node();
                    let temp_node = SyntaxNode::new_root_mut(builder.finish());
                    temp_node.first_token().unwrap().into()
                }
            })
            .collect();
        self.0
            .splice_children(insert_pos..insert_pos, syntax_elements);
    }

    /// Replace an item at a specific position  
    pub fn set(&mut self, index: usize, value: &str) -> bool {
        let children: Vec<_> = self.0.children_with_tokens().collect();
        let mut item_count = 0;

        for (i, child) in children.iter().enumerate() {
            if let Some(node) = child.as_node() {
                if node.kind() == SyntaxKind::SEQUENCE_ENTRY {
                    if item_count == index {
                        // Build a new SEQUENCE_ENTRY with the new value
                        let entry_children: Vec<_> = node.children_with_tokens().collect();
                        let mut builder = GreenNodeBuilder::new();
                        builder.start_node(SyntaxKind::SEQUENCE_ENTRY.into());

                        for entry_child in entry_children {
                            match entry_child {
                                rowan::NodeOrToken::Node(n) if n.kind() == SyntaxKind::SCALAR => {
                                    // Replace the SCALAR node
                                    builder.start_node(SyntaxKind::SCALAR.into());
                                    builder.token(SyntaxKind::VALUE.into(), value);
                                    builder.finish_node();
                                }
                                rowan::NodeOrToken::Node(n) => {
                                    // Copy other nodes as-is
                                    self.copy_node_to_builder(&mut builder, &n);
                                }
                                rowan::NodeOrToken::Token(t) => {
                                    // Copy tokens as-is
                                    builder.token(t.kind().into(), t.text());
                                }
                            }
                        }

                        builder.finish_node();
                        let new_entry = SyntaxNode::new_root_mut(builder.finish());

                        // Replace the old SEQUENCE_ENTRY with the new one
                        self.0.splice_children(i..i + 1, vec![new_entry.into()]);
                        return true;
                    }
                    item_count += 1;
                }
            }
        }
        false
    }

    /// Helper to copy a node and all its children to a builder
    fn copy_node_to_builder(&self, builder: &mut GreenNodeBuilder, node: &SyntaxNode) {
        builder.start_node(node.kind().into());
        for child in node.children_with_tokens() {
            match child {
                rowan::NodeOrToken::Node(n) => {
                    self.copy_node_to_builder(builder, &n);
                }
                rowan::NodeOrToken::Token(t) => {
                    builder.token(t.kind().into(), t.text());
                }
            }
        }
        builder.finish_node();
    }
    /// Remove an item at a specific position
    pub fn remove(&mut self, index: usize) -> Option<String> {
        let children: Vec<_> = self.0.children().collect();

        // Handle flow-style sequences [item1, item2]
        if self.is_flow_style() {
            let mut item_count = 0;
            for (i, child) in children.iter().enumerate() {
                if child.kind() == SyntaxKind::SCALAR {
                    if item_count == index {
                        if let Some(scalar) = Scalar::cast(child.clone()) {
                            let value = scalar.value();

                            // Remove the scalar and surrounding punctuation
                            let mut start = i;
                            let mut end = i + 1;

                            // Remove preceding comma if not first item
                            if start > 0 && children[start - 1].kind() == SyntaxKind::COMMA {
                                start -= 1;
                            }
                            // Remove following comma if exists
                            else if end < children.len()
                                && children[end].kind() == SyntaxKind::COMMA
                            {
                                end += 1;
                            }

                            self.0.splice_children(start..end, vec![]);
                            return Some(value);
                        }
                    }
                    item_count += 1;
                }
            }
        } else {
            // Handle block-style sequences with dashes
            let mut item_count = 0;
            for (i, child) in children.iter().enumerate() {
                if child.kind() == SyntaxKind::DASH {
                    if item_count == index {
                        // Find the complete item to remove (dash, space, value, newline)
                        let start = i;
                        let mut end = i + 1;
                        let mut removed_value = None;

                        while end < children.len() {
                            let child_kind = children[end].kind();
                            if child_kind == SyntaxKind::SCALAR {
                                if let Some(scalar) = Scalar::cast(children[end].clone()) {
                                    removed_value = Some(scalar.value());
                                }
                            }
                            end += 1;
                            if child_kind == SyntaxKind::NEWLINE {
                                break;
                            }
                        }

                        self.0.splice_children(start..end, vec![]);
                        return removed_value;
                    }
                    item_count += 1;
                }
            }
        }
        None
    }

    /// Check if this sequence is in flow style [item1, item2]
    fn is_flow_style(&self) -> bool {
        self.0
            .children()
            .any(|child| child.kind() == SyntaxKind::LEFT_BRACKET)
    }

    /// Get an item at a specific position
    pub fn get_item(&self, index: usize) -> Option<SyntaxNode> {
        let mut item_count = 0;
        for child in self.0.children() {
            if matches!(
                child.kind(),
                SyntaxKind::SCALAR | SyntaxKind::MAPPING | SyntaxKind::SEQUENCE
            ) {
                if item_count == index {
                    return Some(child);
                }
                item_count += 1;
            }
        }
        None
    }

    /// Set an item at a specific position (generic version)
    pub fn set_item<T: Into<ScalarValue>>(&mut self, index: usize, value: T) -> bool {
        let scalar_value = value.into();
        self.set(index, &scalar_value.to_string())
    }
}

// Editing methods for Scalar
impl Scalar {
    /// Set the value of this scalar
    pub fn set_value(&mut self, value: &str) {
        let children_count = self.0.children_with_tokens().count();
        // Create a temporary node to wrap the token and extract a SyntaxToken
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(SyntaxKind::ROOT.into());
        builder.token(SyntaxKind::VALUE.into(), value);
        builder.finish_node();
        let temp_node = SyntaxNode::new_root_mut(builder.finish());
        let new_token = temp_node.first_token().unwrap();
        self.0
            .splice_children(0..children_count, vec![new_token.into()]);
    }
}

// Methods for Directive
impl Directive {
    /// Get the full directive text (e.g., "%YAML 1.2")
    pub fn text(&self) -> String {
        self.0.text().to_string()
    }

    /// Get the directive name (e.g., "YAML" from "%YAML 1.2")
    pub fn name(&self) -> Option<String> {
        let text = self.text();
        if let Some(directive_content) = text.strip_prefix('%') {
            directive_content
                .split_whitespace()
                .next()
                .map(|s| s.to_string())
        } else {
            None
        }
    }

    /// Get the directive value (e.g., "1.2" from "%YAML 1.2")
    pub fn value(&self) -> Option<String> {
        let text = self.text();
        if let Some(directive_content) = text.strip_prefix('%') {
            let parts: Vec<&str> = directive_content.split_whitespace().collect();
            if parts.len() > 1 {
                Some(parts[1..].join(" "))
            } else {
                None
            }
        } else {
            None
        }
    }

    /// Check if this is a YAML version directive
    pub fn is_yaml_version(&self) -> bool {
        self.name().as_deref() == Some("YAML")
    }

    /// Check if this is a TAG directive
    pub fn is_tag(&self) -> bool {
        self.name().as_deref() == Some("TAG")
    }

    /// Create a new YAML version directive
    pub fn new_yaml_version(version: &str) -> Self {
        let directive_text = format!("%YAML {}", version);
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(SyntaxKind::DIRECTIVE.into());
        builder.token(SyntaxKind::DIRECTIVE.into(), &directive_text);
        builder.finish_node();
        Directive(SyntaxNode::new_root_mut(builder.finish()))
    }

    /// Create a new TAG directive
    pub fn new_tag(handle: &str, prefix: &str) -> Self {
        let directive_text = format!("%TAG {} {}", handle, prefix);
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(SyntaxKind::DIRECTIVE.into());
        builder.token(SyntaxKind::DIRECTIVE.into(), &directive_text);
        builder.finish_node();
        Directive(SyntaxNode::new_root_mut(builder.finish()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_mapping() {
        let yaml = "key: value";
        let parsed = Yaml::from_str(yaml).unwrap();
        let doc = parsed.document().unwrap();
        let mapping = doc.as_mapping().unwrap();

        // Basic structure test
        assert_eq!(parsed.to_string().trim(), "key: value");

        // Test get functionality
        let value = mapping.get(&YamlValue::from("key"));
        assert!(value.is_some());
    }

    #[test]
    fn test_simple_sequence() {
        let yaml = "- item1\n- item2";
        let parsed = Yaml::from_str(yaml);
        assert!(parsed.is_ok());
    }

    #[test]
    fn test_complex_yaml() {
        let yaml = r#"
name: my-app
version: 1.0.0
dependencies:
  - serde
  - tokio
config:
  port: 8080
  enabled: true
"#;
        let parsed = Yaml::from_str(yaml).unwrap();
        assert_eq!(parsed.documents().count(), 1);

        let doc = parsed.document().unwrap();
        assert!(doc.as_mapping().is_some());
    }

    #[test]
    fn test_multiple_documents() {
        let yaml = r#"---
doc: first
---
doc: second
...
"#;
        let parsed = Yaml::from_str(yaml).unwrap();
        assert_eq!(parsed.documents().count(), 2);
    }

    #[test]
    fn test_flow_styles() {
        let yaml = r#"
array: [1, 2, 3]
object: {key: value, another: 42}
"#;
        let parsed = Yaml::from_str(yaml).unwrap();
        assert!(parsed.document().is_some());
    }

    #[test]
    fn test_scalar_types_parsing() {
        let yaml = r#"
string: hello
integer: 42
float: 3.14
bool_true: true
bool_false: false
null_value: null
tilde: ~
"#;
        let parsed = Yaml::from_str(yaml).unwrap();
        let doc = parsed.document().unwrap();
        let mapping = doc.as_mapping().unwrap();

        // All keys should be accessible
        assert!(mapping.get(&YamlValue::from("string")).is_some());
        assert!(mapping.get(&YamlValue::from("integer")).is_some());
        assert!(mapping.get(&YamlValue::from("float")).is_some());
        assert!(mapping.get(&YamlValue::from("bool_true")).is_some());
        assert!(mapping.get(&YamlValue::from("bool_false")).is_some());
        assert!(mapping.get(&YamlValue::from("null_value")).is_some());
        assert!(mapping.get(&YamlValue::from("tilde")).is_some());
    }

    #[test]
    fn test_preserve_formatting() {
        let yaml = r#"# Comment at start
key:   value    # inline comment

# Another comment
list:
  - item1   
  - item2
"#;
        let parsed = Yaml::from_str(yaml).unwrap();
        let output = parsed.to_string();

        // Should preserve comments
        assert!(output.contains("# Comment at start"));
        assert!(output.contains("# inline comment"));
        assert!(output.contains("# Another comment"));

        // Should preserve extra spaces
        assert!(output.contains("key:   value"));
    }

    #[test]
    fn test_quoted_strings() {
        let yaml = r#"
single: 'single quoted'
double: "double quoted"
plain: unquoted
"#;
        let parsed = Yaml::from_str(yaml).unwrap();
        let doc = parsed.document().unwrap();
        let mapping = doc.as_mapping().unwrap();

        assert!(mapping.get(&YamlValue::from("single")).is_some());
        assert!(mapping.get(&YamlValue::from("double")).is_some());
        assert!(mapping.get(&YamlValue::from("plain")).is_some());
    }

    #[test]
    fn test_nested_structures() {
        let yaml = r#"
root:
  nested:
    deeply:
      value: 42
  list:
    - item1
    - item2
"#;
        let parsed = Yaml::from_str(yaml).unwrap();
        assert!(parsed.document().is_some());
    }

    #[test]
    fn test_empty_values() {
        let yaml = r#"
empty_string: ""
empty_after_colon:
another_key: value
"#;
        let parsed = Yaml::from_str(yaml).unwrap();
        let doc = parsed.document().unwrap();
        let mapping = doc.as_mapping().unwrap();

        assert!(mapping.get(&YamlValue::from("empty_string")).is_some());
        assert!(mapping.get(&YamlValue::from("another_key")).is_some());
    }

    #[test]
    fn test_special_characters() {
        let yaml = r#"
special: "line1\nline2"
unicode: "emoji 😀"
escaped: 'it\'s escaped'
"#;
        let result = Yaml::from_str(yaml);
        // Should parse without panicking
        assert!(result.is_ok());
    }

    // Editing tests
    #[test]
    fn test_mapping_set_new_key() {
        let yaml = "existing: value";
        let parsed = Yaml::from_str(yaml).unwrap();

        // Get the document and set on it
        let mut doc = parsed.document().expect("Should have a document");
        doc.set("new_key", "new_value");

        let output = doc.to_string();

        let expected = r#"existing: value
new_key: new_value"#;
        assert_eq!(output.trim(), expected);
    }

    #[test]
    fn test_mapping_rename_key() {
        let yaml = "old_name: value";
        let parsed = Yaml::from_str(yaml).unwrap();

        if let Some(doc) = parsed.document() {
            if let Some(mut mapping) = doc.as_mapping() {
                let renamed = mapping.rename_key("old_name", "new_name");
                assert!(renamed);
                let output = parsed.to_string();
                assert!(output.contains("new_name"));
                assert!(!output.contains("old_name"));
            }
        }
    }

    #[test]
    fn test_mapping_remove_key() {
        let yaml = "key1: value1\nkey2: value2";
        let parsed = Yaml::from_str(yaml).unwrap();

        if let Some(doc) = parsed.document() {
            if let Some(mut mapping) = doc.as_mapping() {
                let removed = mapping.remove("key1");
                assert!(removed);
                let output = parsed.to_string();
                assert!(!output.contains("key1"));
                assert!(output.contains("key2"));
            }
        }
    }

    #[test]
    fn test_sequence_operations() {
        let yaml = "- item1\n- item2";
        let parsed = Yaml::from_str(yaml).unwrap();

        if let Some(doc) = parsed.document() {
            if let Some(mut seq) = doc.as_sequence() {
                // Test push
                seq.push("item3");
                let output = parsed.to_string();
                assert!(output.contains("item3"));

                // Test insert
                seq.insert(0, "item0");
                let output = parsed.to_string();
                assert!(output.contains("item0"));
            }
        }
    }

    #[test]
    fn test_error_handling() {
        // Invalid YAML should return error
        let yaml = "key: value\n  invalid indentation for key";
        let result = Yaml::from_str(yaml);
        // For now, just check it doesn't panic
        let _ = result;
    }

    // Directive tests
    #[test]
    fn test_parse_yaml_version_directive() {
        let yaml = "%YAML 1.2\n---\nkey: value";
        let parsed = Yaml::from_str(yaml).unwrap();

        let directives: Vec<_> = parsed.directives().collect();
        assert_eq!(directives.len(), 1);

        let directive = &directives[0];
        assert!(directive.is_yaml_version());
        assert_eq!(directive.name(), Some("YAML".to_string()));
        assert_eq!(directive.value(), Some("1.2".to_string()));
        assert_eq!(directive.text(), "%YAML 1.2");
    }

    #[test]
    fn test_parse_tag_directive() {
        let yaml = "%TAG ! tag:example.com,2000:app/\n---\nkey: value";
        let parsed = Yaml::from_str(yaml).unwrap();

        let directives: Vec<_> = parsed.directives().collect();
        assert_eq!(directives.len(), 1);

        let directive = &directives[0];
        assert!(directive.is_tag());
        assert_eq!(directive.name(), Some("TAG".to_string()));
        assert_eq!(
            directive.value(),
            Some("! tag:example.com,2000:app/".to_string())
        );
    }

    #[test]
    fn test_anchor_exact_output() {
        let yaml = "key: &anchor value\nref: *anchor";
        let parsed = Yaml::from_str(yaml).unwrap();
        let output = parsed.to_string();

        // Test exact output to ensure no duplication
        assert_eq!(output, "key: &anchor value\nref: *anchor");
    }

    #[test]
    fn test_anchor_with_different_value_types() {
        let yaml = r#"string_anchor: &str_val "hello"
int_anchor: &int_val 42
bool_anchor: &bool_val true
null_anchor: &null_val null
str_ref: *str_val
int_ref: *int_val
bool_ref: *bool_val
null_ref: *null_val"#;

        let parsed = Yaml::from_str(yaml);
        assert!(
            parsed.is_ok(),
            "Should parse anchors with different value types"
        );

        let yaml_doc = parsed.unwrap();
        let output = yaml_doc.to_string();

        // Check exact preservation
        assert!(output.contains("&str_val"));
        assert!(output.contains("&int_val"));
        assert!(output.contains("&bool_val"));
        assert!(output.contains("&null_val"));
        assert!(output.contains("*str_val"));
        assert!(output.contains("*int_val"));
        assert!(output.contains("*bool_val"));
        assert!(output.contains("*null_val"));
    }

    #[test]
    fn test_undefined_alias_generates_error() {
        let yaml = "key: *undefined";
        let parse_result = Parse::parse_yaml(yaml);

        assert!(
            parse_result.has_errors(),
            "Should have error for undefined alias"
        );
        let errors = parse_result.errors();
        assert!(
            errors.iter().any(|e| e.contains("undefined")),
            "Error should mention the undefined alias"
        );
    }

    #[test]
    fn test_anchor_names_with_alphanumeric_chars() {
        // Test valid anchor names with underscores and numbers (YAML spec compliant)
        let yaml1 = "key1: &anchor_123 val1\nref1: *anchor_123";
        let parsed1 = Yaml::from_str(yaml1);
        assert!(
            parsed1.is_ok(),
            "Should parse anchors with underscores and numbers"
        );

        let output1 = parsed1.unwrap().to_string();
        assert!(output1.contains("&anchor_123"));
        assert!(output1.contains("*anchor_123"));

        let yaml2 = "key2: &AnchorName val2\nref2: *AnchorName";
        let parsed2 = Yaml::from_str(yaml2);
        assert!(parsed2.is_ok(), "Should parse anchors with mixed case");

        let output2 = parsed2.unwrap().to_string();
        assert!(output2.contains("&AnchorName"));
        assert!(output2.contains("*AnchorName"));

        let yaml3 = "key3: &anchor123abc val3\nref3: *anchor123abc";
        let parsed3 = Yaml::from_str(yaml3);
        assert!(
            parsed3.is_ok(),
            "Should parse anchors with letters and numbers"
        );

        let output3 = parsed3.unwrap().to_string();
        assert!(output3.contains("&anchor123abc"));
        assert!(output3.contains("*anchor123abc"));
    }

    #[test]
    fn test_anchor_in_sequence_detailed() {
        let yaml = r#"items:
  - &first_item value1
  - second_item
  - *first_item"#;

        let parsed = Yaml::from_str(yaml);
        assert!(parsed.is_ok(), "Should parse anchors in sequences");

        let yaml_doc = parsed.unwrap();
        let output = yaml_doc.to_string();
        assert!(output.contains("&first_item"));
        assert!(output.contains("*first_item"));
    }

    #[test]
    fn test_preserve_whitespace_around_anchors() {
        let yaml = "key:  &anchor   value  \nref:  *anchor  ";
        let parsed = Yaml::from_str(yaml).unwrap();
        let output = parsed.to_string();

        // Should preserve whitespace around anchors and aliases
        assert!(output.contains("&anchor"));
        assert!(output.contains("*anchor"));
    }

    #[test]
    fn test_multiple_directives() {
        let yaml = "%YAML 1.2\n%TAG ! tag:example.com,2000:app/\n---\nkey: value";
        let parsed = Yaml::from_str(yaml).unwrap();

        let directives: Vec<_> = parsed.directives().collect();
        assert_eq!(directives.len(), 2);

        assert!(directives[0].is_yaml_version());
        assert!(directives[1].is_tag());
    }

    #[test]
    fn test_create_yaml_version_directive() {
        let directive = Directive::new_yaml_version("1.2");
        assert!(directive.is_yaml_version());
        assert_eq!(directive.value(), Some("1.2".to_string()));
        assert_eq!(directive.text(), "%YAML 1.2");
    }

    #[test]
    fn test_create_tag_directive() {
        let directive = Directive::new_tag("!", "tag:example.com,2000:app/");
        assert!(directive.is_tag());
        assert_eq!(directive.text(), "%TAG ! tag:example.com,2000:app/");
    }

    #[test]
    fn test_add_directive_to_yaml() {
        let mut yaml = Yaml::new();
        yaml.add_directive("%YAML 1.2");

        let output = yaml.to_string();
        assert!(output.contains("%YAML 1.2"));

        let directives: Vec<_> = yaml.directives().collect();
        assert_eq!(directives.len(), 1);
    }

    #[test]
    fn test_yaml_with_directive_and_content() {
        let mut yaml = Yaml::new();
        yaml.add_directive("%YAML 1.2");

        let mut doc = Document::new_mapping();
        doc.set_string("name", "test");
        yaml.push_document(doc);

        let output = yaml.to_string();
        assert!(output.contains("%YAML 1.2"));
        assert!(output.contains("name: test"));

        // Should have both directive and document
        let directives: Vec<_> = yaml.directives().collect();
        let documents: Vec<_> = yaml.documents().collect();
        assert_eq!(directives.len(), 1);
        assert_eq!(documents.len(), 1);
    }

    #[test]
    fn test_directive_preservation_in_parsing() {
        let input = "%YAML 1.2\n%TAG ! tag:example.com,2000:app/\n---\nkey: value\n";
        let parsed = Yaml::from_str(input).unwrap();
        let output = parsed.to_string();

        // Check that directives are preserved
        assert!(output.contains("%YAML 1.2"));
        assert!(output.contains("%TAG ! tag:example.com,2000:app/"));
        assert!(output.contains("key: value"));
    }

    #[test]
    fn test_literal_block_scalar_basic() {
        let yaml = r#"literal: |
  Line 1
  Line 2
  Line 3
"#;
        let parsed = Yaml::from_str(yaml);
        assert!(parsed.is_ok(), "Should parse basic literal block scalar");

        let yaml_doc = parsed.unwrap();
        let output = yaml_doc.to_string();

        // Should preserve the literal block scalar format exactly
        assert_eq!(output, yaml);
    }

    #[test]
    fn test_folded_block_scalar_basic() {
        let yaml = r#"folded: >
  This is a very long line that will be folded
  into a single line in the output
  but preserves paragraph breaks.

  This is a new paragraph.
"#;
        let parsed = Yaml::from_str(yaml);
        assert!(parsed.is_ok(), "Should parse basic folded block scalar");

        let yaml_doc = parsed.unwrap();
        let output = yaml_doc.to_string();

        // Should preserve the folded block scalar format exactly
        assert_eq!(output, yaml);
    }

    #[test]
    fn test_literal_block_scalar_with_chomping_indicators() {
        // Test strip indicator (-)
        let yaml1 = r#"strip: |-
  Line 1
  Line 2

"#;
        let parsed1 = Yaml::from_str(yaml1);
        assert!(
            parsed1.is_ok(),
            "Should parse literal block scalar with strip indicator"
        );

        let output1 = parsed1.unwrap().to_string();
        assert!(output1.contains("|-"));

        // Test keep indicator (+)
        let yaml2 = r#"keep: |+
  Line 1
  Line 2

"#;
        let parsed2 = Yaml::from_str(yaml2);
        assert!(
            parsed2.is_ok(),
            "Should parse literal block scalar with keep indicator"
        );

        let output2 = parsed2.unwrap().to_string();
        assert!(output2.contains("|+"));
    }

    #[test]
    fn test_folded_block_scalar_with_chomping_indicators() {
        // Test strip indicator (-)
        let yaml1 = r#"strip: >-
  Folded content that should
  be stripped of final newlines
"#;
        let parsed1 = Yaml::from_str(yaml1);
        assert!(
            parsed1.is_ok(),
            "Should parse folded block scalar with strip indicator"
        );

        let output1 = parsed1.unwrap().to_string();
        assert!(output1.contains(">-"));

        // Test keep indicator (+)
        let yaml2 = r#"keep: >+
  Folded content that should
  keep all final newlines

"#;
        let parsed2 = Yaml::from_str(yaml2);
        assert!(
            parsed2.is_ok(),
            "Should parse folded block scalar with keep indicator"
        );

        let output2 = parsed2.unwrap().to_string();
        assert!(output2.contains(">+"));
    }

    #[test]
    fn test_block_scalar_with_explicit_indentation() {
        let yaml1 = r#"explicit: |2
    Two space indent
    Another line
"#;
        let parsed1 = Yaml::from_str(yaml1);
        assert!(
            parsed1.is_ok(),
            "Should parse literal block scalar with explicit indentation"
        );

        let output1 = parsed1.unwrap().to_string();
        assert!(output1.contains("|2"));

        let yaml2 = r#"folded_explicit: >3
      Three space indent
      Another folded line
"#;
        let parsed2 = Yaml::from_str(yaml2);
        assert!(
            parsed2.is_ok(),
            "Should parse folded block scalar with explicit indentation"
        );

        let output2 = parsed2.unwrap().to_string();
        assert!(output2.contains(">3"));
    }

    #[test]
    fn test_block_scalar_in_mapping() {
        let yaml = r#"description: |
  This is a multi-line
  description that should
  preserve line breaks.
  
  It can have multiple paragraphs too.

summary: >
  This is a summary that
  should be folded into
  a single line.

version: "1.0"
"#;
        let parsed = Yaml::from_str(yaml);
        assert!(
            parsed.is_ok(),
            "Should parse block scalars in mapping context"
        );

        let yaml_doc = parsed.unwrap();
        let output = yaml_doc.to_string();

        assert!(output.contains("|"));
        assert!(output.contains(">"));
        assert!(output.contains("This is a multi-line"));
        assert!(output.contains("This is a summary"));
        assert!(output.contains("version: \"1.0\""));
    }

    #[test]
    fn test_mixed_block_and_regular_scalars() {
        let yaml = r#"config:
  name: "My App"
  description: |
    This application does many things:
    - Feature 1
    - Feature 2
    - Feature 3
  summary: >
    A brief summary that spans
    multiple lines but should
    be folded together.
  version: 1.0
  enabled: true
"#;
        let parsed = Yaml::from_str(yaml);
        assert!(
            parsed.is_ok(),
            "Should parse mixed block and regular scalars"
        );

        let yaml_doc = parsed.unwrap();
        let output = yaml_doc.to_string();

        // Check for all different scalar types
        assert!(output.contains("\"My App\"")); // quoted string
        assert!(output.contains("|")); // literal block
        assert!(output.contains(">")); // folded block
        assert!(output.contains("1.0")); // number
        assert!(output.contains("true")); // boolean
    }

    #[test]
    fn test_block_scalar_edge_cases() {
        // Empty block scalar
        let yaml1 = r#"empty_literal: |
empty_folded: >
"#;
        let parsed1 = Yaml::from_str(yaml1);
        assert!(parsed1.is_ok(), "Should parse empty block scalars");

        // Block scalar with only whitespace
        let yaml2 = r#"whitespace: |
  
  
"#;
        let parsed2 = Yaml::from_str(yaml2);
        assert!(
            parsed2.is_ok(),
            "Should parse block scalar with only whitespace"
        );

        // Block scalar followed immediately by another key
        let yaml3 = r#"first: |
  Content
second: value
"#;
        let parsed3 = Yaml::from_str(yaml3);
        assert!(
            parsed3.is_ok(),
            "Should parse block scalar followed by other keys"
        );

        let output3 = parsed3.unwrap().to_string();
        assert!(output3.contains("first"));
        assert!(output3.contains("second"));
    }

    #[test]
    fn test_literal_block_scalar_advanced_formatting() {
        let yaml = r#"poem: |
  Roses are red,
  Violets are blue,
  YAML is great,
  And so are you!

  This is another stanza
  with different content.
    And this line has extra indentation.
  Back to normal indentation.

  Final stanza.
"#;
        let parsed = Yaml::from_str(yaml);
        assert!(parsed.is_ok(), "Should parse complex literal block scalar");

        let output = parsed.unwrap().to_string();

        // Verify all content is preserved
        assert!(output.contains("Roses are red,"));
        assert!(output.contains("Violets are blue,"));
        assert!(output.contains("This is another stanza"));
        assert!(output.contains("    And this line has extra indentation."));
        assert!(output.contains("Back to normal indentation."));
        assert!(output.contains("Final stanza."));

        // Verify the literal block scalar marker is preserved
        assert!(output.contains("poem: |"));
    }

    #[test]
    fn test_folded_block_scalar_paragraph_handling() {
        let yaml = r#"description: >
  This is the first paragraph that should
  be folded into a single line when processed
  by a YAML parser.

  This is a second paragraph that should
  also be folded but kept separate from
  the first paragraph.


  This is a third paragraph after
  multiple blank lines.

  Final paragraph.
"#;
        let parsed = Yaml::from_str(yaml);
        assert!(
            parsed.is_ok(),
            "Should parse folded block scalar with paragraphs"
        );

        let output = parsed.unwrap().to_string();

        // All content should be preserved as-is (lossless)
        assert!(output.contains("This is the first paragraph"));
        assert!(output.contains("This is a second paragraph"));
        assert!(output.contains("This is a third paragraph"));
        assert!(output.contains("Final paragraph."));
        assert!(output.contains("description: >"));
    }

    #[test]
    fn test_block_scalars_with_special_characters() {
        let yaml = r#"special_chars: |
  Line with colons: key: value
  Line with dashes - and more - dashes
  Line with quotes "double" and 'single'
  Line with brackets [array] and braces {object}
  Line with pipes | and greater than >
  Line with at @ and hash # symbols
  Line with percent % and exclamation !
  
backslash_test: >
  This line has a backslash \ in it
  And this line has multiple \\ backslashes
  
unicode_test: |
  This line has unicode: 你好世界
  And emojis: 🚀 🎉 ✨
"#;
        let parsed = Yaml::from_str(yaml);
        assert!(
            parsed.is_ok(),
            "Should parse block scalars with special characters"
        );

        let output = parsed.unwrap().to_string();

        // Verify special characters are preserved
        assert!(output.contains("key: value"));
        assert!(output.contains("- and more -"));
        assert!(output.contains("\"double\" and 'single'"));
        assert!(output.contains("[array] and braces {object}"));
        assert!(output.contains("pipes | and greater than >"));
        assert!(output.contains("backslash \\ in it"));
        assert!(output.contains("multiple \\\\ backslashes"));
        assert!(output.contains("你好世界"));
        assert!(output.contains("🚀 🎉 ✨"));
    }

    #[test]
    fn test_block_scalar_chomping_detailed() {
        // Test clip indicator (default - no explicit indicator)
        let yaml_clip = r#"clip: |
  Line 1
  Line 2

"#;
        let parsed_clip = Yaml::from_str(yaml_clip);
        assert!(
            parsed_clip.is_ok(),
            "Should parse block scalar with default clipping"
        );

        // Test strip indicator (-)
        let yaml_strip = r#"strip: |-
  Line 1
  Line 2



"#;
        let parsed_strip = Yaml::from_str(yaml_strip);
        assert!(
            parsed_strip.is_ok(),
            "Should parse block scalar with strip indicator"
        );

        // Test keep indicator (+)
        let yaml_keep = r#"keep: |+
  Line 1
  Line 2



"#;
        let parsed_keep = Yaml::from_str(yaml_keep);
        assert!(
            parsed_keep.is_ok(),
            "Should parse block scalar with keep indicator"
        );

        // Verify indicators are preserved
        let output_clip = parsed_clip.unwrap().to_string();
        let output_strip = parsed_strip.unwrap().to_string();
        let output_keep = parsed_keep.unwrap().to_string();

        assert!(output_clip.contains("clip: |"));
        assert!(output_strip.contains("strip: |-"));
        assert!(output_keep.contains("keep: |+"));
    }

    #[test]
    fn test_block_scalar_explicit_indentation_detailed() {
        // Test individual cases to isolate the issue
        let yaml1 = r#"indent1: |1
 Single space indent
"#;
        let parsed1 = Yaml::from_str(yaml1);
        assert!(parsed1.is_ok(), "Should parse |1 block scalar");
        let output1 = parsed1.unwrap().to_string();
        assert_eq!(output1, yaml1);

        let yaml2 = r#"indent2: |2
  Two space indent
"#;
        let parsed2 = Yaml::from_str(yaml2);
        assert!(parsed2.is_ok(), "Should parse |2 block scalar");
        let output2 = parsed2.unwrap().to_string();
        assert_eq!(output2, yaml2);

        let yaml3 = r#"folded_indent: >2
  Two space folded
  content spans lines
"#;
        let parsed3 = Yaml::from_str(yaml3);
        assert!(parsed3.is_ok(), "Should parse >2 folded block scalar");
        let output3 = parsed3.unwrap().to_string();
        assert_eq!(output3, yaml3);
    }

    #[test]
    fn test_block_scalar_combined_indicators() {
        let yaml = r#"strip_with_indent: |2-
  Content with explicit indent
  and strip chomping


keep_with_indent: >3+
   Content with explicit indent
   and keep chomping



folded_strip: >-
  Folded content
  with strip indicator

literal_keep: |+
  Literal content
  with keep indicator


"#;
        let parsed = Yaml::from_str(yaml);
        assert!(
            parsed.is_ok(),
            "Should parse block scalars with combined indicators"
        );

        let output = parsed.unwrap().to_string();

        assert!(output.contains("strip_with_indent: |2-"));
        assert!(output.contains("keep_with_indent: >3+"));
        assert!(output.contains("folded_strip: >-"));
        assert!(output.contains("literal_keep: |+"));
    }

    #[test]
    fn test_block_scalar_edge_cases_comprehensive() {
        // Block scalar with only whitespace lines
        let yaml1 = r#"whitespace_only: |
  
    
  
"#;
        let parsed1 = Yaml::from_str(yaml1);
        assert!(
            parsed1.is_ok(),
            "Should handle block scalar with only whitespace"
        );

        // Block scalar with mixed indentation
        let yaml2 = r#"mixed_indent: |
  Normal line
    Indented line
  Back to normal
      More indented
  Normal again
"#;
        let parsed2 = Yaml::from_str(yaml2);
        assert!(parsed2.is_ok(), "Should handle mixed indentation levels");

        // Block scalar followed immediately by another mapping
        let yaml3 = r#"first: |
  Content
immediate: value
another: |
  More content
final: end
"#;
        let parsed3 = Yaml::from_str(yaml3);
        assert!(
            parsed3.is_ok(),
            "Should handle multiple block scalars in mapping"
        );

        let output3 = parsed3.unwrap().to_string();
        assert!(output3.contains("first: |"));
        assert!(output3.contains("immediate: value"));
        assert!(output3.contains("another: |"));
        assert!(output3.contains("final: end"));
    }

    #[test]
    fn test_block_scalar_with_comments() {
        let yaml = r#"# Main configuration
config: |  # This is a literal block
  # This comment is inside the block
  line1: value1
  # Another internal comment
  line2: value2
  
# Outside comment
other: >  # Folded block comment
  This content spans
  # This hash is part of the content, not a comment
  multiple lines
"#;
        let parsed = Yaml::from_str(yaml);
        assert!(parsed.is_ok(), "Should parse block scalars with comments");

        let output = parsed.unwrap().to_string();
        assert!(output.contains("# Main configuration"));
        assert!(output.contains("config: |"));
        assert!(output.contains("# This comment is inside the block"));
        assert!(output.contains("# Outside comment"));
        assert!(output.contains("other: >"));
        assert!(output.contains("# This hash is part of the content"));
    }

    #[test]
    fn test_block_scalar_empty_and_minimal() {
        let yaml = r#"empty_literal: |

empty_folded: >

minimal_literal: |
  x

minimal_folded: >
  y

just_newlines: |



just_spaces: |
   
   
   
"#;
        let parsed = Yaml::from_str(yaml);
        assert!(
            parsed.is_ok(),
            "Should handle empty and minimal block scalars"
        );

        let output = parsed.unwrap().to_string();
        assert!(output.contains("empty_literal: |"));
        assert!(output.contains("empty_folded: >"));
        assert!(output.contains("minimal_literal: |"));
        assert!(output.contains("minimal_folded: >"));
        assert!(output.contains("just_newlines: |"));
        assert!(output.contains("just_spaces: |"));
    }

    #[test]
    fn test_block_scalar_with_document_markers() {
        let yaml = r#"---
doc1: |
  This is the first document
  with a literal block scalar.

next_key: value
---
doc2: >
  This is the second document
  with a folded block scalar.

another_key: another_value
...
"#;
        let parsed = Yaml::from_str(yaml);
        assert!(
            parsed.is_ok(),
            "Should parse block scalars with document markers"
        );

        let output = parsed.unwrap().to_string();
        assert!(output.contains("---"));
        assert!(output.contains("doc1: |"));
        assert!(output.contains("This is the first document"));
        assert!(output.contains("doc2: >"));
        assert!(output.contains("This is the second document"));
        assert!(output.contains("..."));
    }

    #[test]
    fn test_block_scalar_formatting_preservation() {
        let original = r#"preserve_me: |
  Line with    multiple    spaces
  Line with	tabs	here
  Line with trailing spaces   
  
  Empty line above and below
  
  Final line
"#;
        let parsed = Yaml::from_str(original);
        assert!(parsed.is_ok(), "Should preserve exact formatting");

        let output = parsed.unwrap().to_string();

        // The output should be identical to input (lossless)
        assert!(output.contains("Line with    multiple    spaces"));
        assert!(output.contains("Line with	tabs	here"));
        assert!(output.contains("Line with trailing spaces   "));
        assert!(output.contains("Final line"));
    }

    #[test]
    fn test_block_scalar_complex_yaml_content() {
        let yaml = r#"yaml_content: |
  # This block contains YAML-like content
  nested:
    - item: value
    - item: another
  
  mapping:
    key1: |
      Even more nested literal content
    key2: value
    
  anchors: &anchor
    anchor_content: data
    
  reference: *anchor
  
quoted_yaml: >
  This folded block contains
  YAML structures: {key: value, array: [1, 2, 3]}
  that should be treated as plain text.
"#;
        let parsed = Yaml::from_str(yaml);
        assert!(
            parsed.is_ok(),
            "Should parse block scalars containing YAML-like structures"
        );

        let output = parsed.unwrap().to_string();
        assert!(output.contains("yaml_content: |"));
        assert!(output.contains("# This block contains YAML-like content"));
        assert!(output.contains("nested:"));
        assert!(output.contains("- item: value"));
        assert!(output.contains("anchors: &anchor"));
        assert!(output.contains("reference: *anchor"));
        assert!(output.contains("quoted_yaml: >"));
        assert!(output.contains("{key: value, array: [1, 2, 3]}"));
    }

    #[test]
    fn test_block_scalar_performance_large_content() {
        // Test with a reasonably large block scalar
        let mut large_content = String::new();
        for i in 1..=100 {
            large_content.push_str(&format!(
                "  Line number {} with some content that makes it longer\n",
                i
            ));
        }

        let yaml = format!(
            "large_literal: |\n{}\nlarge_folded: >\n{}\n",
            large_content, large_content
        );

        let parsed = Yaml::parse(&yaml);
        assert!(
            !parsed.has_errors(),
            "Should parse large block scalars without errors"
        );

        let output = parsed.tree().to_string();
        assert!(output.contains("large_literal: |"));
        assert!(output.contains("large_folded: >"));
        assert!(output.contains("Line number 1"));
        assert!(output.contains("Line number 50"));
        assert!(output.contains("Line number 100"));
    }

    #[test]
    fn test_block_scalar_error_recovery() {
        // Test that parser can recover from malformed block scalars
        let yaml = r#"good_key: value
bad_block: |
incomplete_key
another_good: works
"#;
        let parsed = Yaml::parse(yaml);

        // Should still parse the valid parts
        let output = parsed.tree().to_string();
        assert!(output.contains("good_key: value"));
        assert!(output.contains("another_good: works"));
    }

    #[test]
    fn test_block_scalar_with_flow_structures() {
        let yaml = r#"mixed_styles: |
  This literal block contains:
  - A flow sequence: [1, 2, 3]
  - A flow mapping: {key: value, other: data}
  - Mixed content: [a, {nested: true}, c]

flow_then_block:
  flow_seq: [item1, item2]
  block_literal: |
    This comes after flow style
    and should work fine.
  flow_map: {after: block}
"#;
        let parsed = Yaml::from_str(yaml);
        assert!(parsed.is_ok(), "Should parse mixed flow and block styles");

        let output = parsed.unwrap().to_string();
        assert!(output.contains("mixed_styles: |"));
        assert!(output.contains("[1, 2, 3]"));
        assert!(output.contains("{key: value, other: data}"));
        assert!(output.contains("flow_seq: [item1, item2]"));
        assert!(output.contains("block_literal: |"));
        assert!(output.contains("flow_map: {after: block}"));
    }

    #[test]
    fn test_block_scalar_indentation_edge_cases() {
        // Test with no content after block indicator
        let yaml1 = r#"empty: |
next: value"#;
        let parsed1 = Yaml::from_str(yaml1);
        assert!(parsed1.is_ok(), "Should handle empty block followed by key");

        // Test with inconsistent indentation that should still work
        let yaml2 = r#"inconsistent: |
  normal indent
    more indent  
  back to normal
      even more
  normal
"#;
        let parsed2 = Yaml::from_str(yaml2);
        assert!(
            parsed2.is_ok(),
            "Should handle inconsistent but valid indentation"
        );

        // Test with tab characters (should work in block scalars)
        let yaml3 = "tabs: |\n\tTab indented line\n\tAnother tab line\n";
        let parsed3 = Yaml::from_str(yaml3);
        assert!(
            parsed3.is_ok(),
            "Should handle tab characters in block scalars"
        );
    }

    #[test]
    fn test_block_scalar_with_anchors_and_aliases() {
        let yaml = r#"template: &template |
  This is a template
  with multiple lines
  that can be referenced.

instance1: *template

instance2: 
  content: *template
  other: value

modified: |
  <<: *template
  Additional content here
"#;
        let parsed = Yaml::from_str(yaml);
        assert!(
            parsed.is_ok(),
            "Should parse block scalars with anchors and aliases"
        );

        let output = parsed.unwrap().to_string();
        assert!(output.contains("template: &template |"));
        assert!(output.contains("instance1: *template"));
        assert!(output.contains("content: *template"));
        assert!(output.contains("<<: *template"));
    }

    #[test]
    fn test_block_scalar_newline_variations() {
        // Test with different newline styles
        let yaml_unix = "unix: |\n  Line 1\n  Line 2\n";
        let parsed_unix = Yaml::from_str(yaml_unix);
        assert!(parsed_unix.is_ok(), "Should handle Unix newlines");

        let yaml_windows = "windows: |\r\n  Line 1\r\n  Line 2\r\n";
        let parsed_windows = Yaml::from_str(yaml_windows);
        assert!(parsed_windows.is_ok(), "Should handle Windows newlines");

        // Verify content is preserved
        let output_unix = parsed_unix.unwrap().to_string();
        let output_windows = parsed_windows.unwrap().to_string();

        assert!(output_unix.contains("unix: |"));
        assert!(output_windows.contains("windows: |"));
        assert!(output_unix.contains("Line 1"));
        assert!(output_windows.contains("Line 1"));
    }

    #[test]
    fn test_block_scalar_boundary_detection() {
        // Test that block scalars properly end at mapping boundaries
        let yaml = r#"config:
  description: |
    This is a configuration
    with multiple lines.
  
  name: "MyApp"
  version: 1.0
  
  settings: >
    These are settings that
    span multiple lines too.
  
  debug: true
"#;
        let parsed = Yaml::from_str(yaml);
        assert!(
            parsed.is_ok(),
            "Should properly detect block scalar boundaries"
        );

        let output = parsed.unwrap().to_string();
        assert!(output.contains("description: |"));
        assert!(output.contains("This is a configuration"));
        assert!(output.contains("name: \"MyApp\""));
        assert!(output.contains("version: 1.0"));
        assert!(output.contains("settings: >"));
        assert!(output.contains("These are settings"));
        assert!(output.contains("debug: true"));
    }

    #[test]
    fn test_block_scalar_with_numeric_content() {
        let yaml = r#"numbers_as_text: |
  123
  45.67
  -89
  +100
  0xFF
  1e5
  true
  false
  null

calculations: >
  The result is: 2 + 2 = 4
  And 10 * 5 = 50
  Also: 100% complete
"#;
        let parsed = Yaml::from_str(yaml);
        assert!(
            parsed.is_ok(),
            "Should parse numeric content as text in block scalars"
        );

        let output = parsed.unwrap().to_string();
        assert!(output.contains("numbers_as_text: |"));
        assert!(output.contains("123"));
        assert!(output.contains("45.67"));
        assert!(output.contains("+100"));
        assert!(output.contains("calculations: >"));
        assert!(output.contains("2 + 2 = 4"));
        assert!(output.contains("100% complete"));
    }

    #[test]
    fn test_block_scalar_exact_preservation() {
        // Test that block scalars are preserved exactly as written (lossless)
        let test_cases = vec![
            // Simple literal block
            r#"simple: |
  Hello World
"#,
            // Simple folded block
            r#"folded: >
  Hello World
"#,
            // With chomping indicators
            r#"strip: |-
  Content

keep: |+
  Content

"#,
            // With explicit indentation
            r#"explicit: |2
  Two space indent
"#,
            // Complex real-world example
            r#"config:
  script: |
    #!/bin/bash
    echo "Starting deployment"
    
    for service in api web worker; do
        echo "Deploying $service"
        kubectl apply -f $service.yaml
    done
  
  description: >
    This configuration defines a deployment
    script that will be executed during
    the CI/CD pipeline.
"#,
        ];

        for (i, yaml) in test_cases.iter().enumerate() {
            let parsed = Yaml::from_str(yaml);
            assert!(parsed.is_ok(), "Test case {} should parse successfully", i);

            let output = parsed.unwrap().to_string();
            assert_eq!(
                output, *yaml,
                "Test case {} should preserve exact formatting",
                i
            );
        }
    }

    #[test]
    fn test_block_scalar_chomping_exact() {
        let yaml_strip = r#"strip: |-
  Content
"#;
        let parsed_strip = Yaml::from_str(yaml_strip).unwrap();
        assert_eq!(parsed_strip.to_string(), yaml_strip);

        let yaml_keep = r#"keep: |+
  Content

"#;
        let parsed_keep = Yaml::from_str(yaml_keep).unwrap();
        assert_eq!(parsed_keep.to_string(), yaml_keep);

        let yaml_folded_strip = r#"folded: >-
  Content
"#;
        let parsed_folded_strip = Yaml::from_str(yaml_folded_strip).unwrap();
        assert_eq!(parsed_folded_strip.to_string(), yaml_folded_strip);
    }

    #[test]
    fn test_block_scalar_indentation_exact() {
        let yaml1 = r#"indent1: |1
 Single space
"#;
        let parsed1 = Yaml::from_str(yaml1).unwrap();
        assert_eq!(parsed1.to_string(), yaml1);

        let yaml2 = r#"indent2: |2
  Two spaces
"#;
        let parsed2 = Yaml::from_str(yaml2).unwrap();
        assert_eq!(parsed2.to_string(), yaml2);

        let yaml3 = r#"combined: |3+
   Content with keep

"#;
        let parsed3 = Yaml::from_str(yaml3).unwrap();
        assert_eq!(parsed3.to_string(), yaml3);
    }

    #[test]
    fn test_block_scalar_mapping_exact() {
        let yaml = r#"description: |
  Line 1
  Line 2

summary: >
  Folded content

version: "1.0"
"#;
        let parsed = Yaml::from_str(yaml).unwrap();
        assert_eq!(parsed.to_string(), yaml);
    }

    #[test]
    fn test_block_scalar_sequence_exact() {
        let yaml = r#"items:
  - |
    First item content
    with multiple lines
  
  - >
    Second item folded
    content
  
  - regular_item
"#;
        let parsed = Yaml::from_str(yaml).unwrap();
        assert_eq!(parsed.to_string(), yaml);
    }

    #[test]
    fn test_block_scalar_empty_exact() {
        let yaml1 = r#"empty: |

"#;
        let parsed1 = Yaml::from_str(yaml1).unwrap();
        assert_eq!(parsed1.to_string(), yaml1);

        let yaml2 = r#"empty_folded: >

"#;
        let parsed2 = Yaml::from_str(yaml2).unwrap();
        assert_eq!(parsed2.to_string(), yaml2);
    }

    #[test]
    fn test_document_stream_features() {
        // Test 1: Multi-document with end markers
        let yaml1 = "---\ndoc1: first\n---\ndoc2: second\n...\n";
        let parsed1 = Yaml::from_str(yaml1).unwrap();
        assert_eq!(parsed1.documents().count(), 2);
        assert_eq!(parsed1.to_string(), yaml1);

        // Test 2: Single document with explicit markers
        let yaml2 = "---\nkey: value\n...\n";
        let parsed2 = Yaml::from_str(yaml2).unwrap();
        assert_eq!(parsed2.documents().count(), 1);
        assert_eq!(parsed2.to_string(), yaml2);

        // Test 3: Document with only end marker
        let yaml3 = "key: value\n...\n";
        let parsed3 = Yaml::from_str(yaml3).unwrap();
        assert_eq!(parsed3.documents().count(), 1);
        assert_eq!(parsed3.to_string(), yaml3);
    }

    #[test]
    fn test_document_level_directives() {
        // Test document-level directives with multi-document stream
        let yaml = "%YAML 1.2\n%TAG ! tag:example.com,2000:app/\n---\nfirst: doc\n...\n%YAML 1.2\n---\nsecond: doc\n...\n";
        let parsed = Yaml::from_str(yaml).unwrap();
        assert_eq!(parsed.documents().count(), 2);
        assert_eq!(parsed.to_string(), yaml);
    }

    #[test]
    fn test_empty_documents_in_stream() {
        // Test empty documents in multi-document stream
        let yaml = "---\n---\nkey: value\n---\n...\n";
        let parsed = Yaml::from_str(yaml).unwrap();
        assert_eq!(parsed.documents().count(), 3);
        assert_eq!(parsed.to_string(), yaml);
    }

    #[test]
    fn test_mixed_document_end_markers() {
        // Test documents with mixed end marker usage
        let yaml = "---\nfirst: doc\n...\n---\nsecond: doc\n---\nthird: doc\n...\n";
        let parsed = Yaml::from_str(yaml).unwrap();
        assert_eq!(parsed.documents().count(), 3);
        assert_eq!(parsed.to_string(), yaml);
    }

    #[test]
    fn test_complex_document_stream() {
        let yaml = r#"%YAML 1.2
%TAG ! tag:example.com,2000:app/
---
template: &anchor
  key: !custom value
instance:
  <<: *anchor
  extra: data
...
%YAML 1.2
---
- item1
- item2: nested
...
---
literal: |
  Block content
  Multiple lines
folded: >
  Folded content
  on multiple lines
...
"#;
        let parsed = Yaml::from_str(yaml).unwrap();
        assert_eq!(parsed.documents().count(), 3);
        assert_eq!(parsed.to_string(), yaml);
    }

    #[test]
    fn test_number_format_parsing() {
        // Test binary numbers
        let yaml = Yaml::from_str("value: 0b1010").unwrap();
        assert_eq!(yaml.to_string().trim(), "value: 0b1010");

        let yaml = Yaml::from_str("value: 0B1111").unwrap();
        assert_eq!(yaml.to_string().trim(), "value: 0B1111");

        // Test modern octal numbers
        let yaml = Yaml::from_str("value: 0o755").unwrap();
        assert_eq!(yaml.to_string().trim(), "value: 0o755");

        let yaml = Yaml::from_str("value: 0O644").unwrap();
        assert_eq!(yaml.to_string().trim(), "value: 0O644");

        // Test with signs
        let yaml = Yaml::from_str("value: -0b1010").unwrap();
        assert_eq!(yaml.to_string().trim(), "value: -0b1010");

        let yaml = Yaml::from_str("value: +0o755").unwrap();
        assert_eq!(yaml.to_string().trim(), "value: +0o755");

        // Test legacy formats still work
        let yaml = Yaml::from_str("value: 0755").unwrap();
        assert_eq!(yaml.to_string().trim(), "value: 0755");

        let yaml = Yaml::from_str("value: 0xFF").unwrap();
        assert_eq!(yaml.to_string().trim(), "value: 0xFF");
    }

    #[test]
    fn test_invalid_number_formats_as_strings() {
        // Invalid formats should be preserved as strings
        let yaml = Yaml::from_str("value: 0b2").unwrap();
        assert_eq!(yaml.to_string().trim(), "value: 0b2");

        let yaml = Yaml::from_str("value: 0o9").unwrap();
        assert_eq!(yaml.to_string().trim(), "value: 0o9");

        let yaml = Yaml::from_str("value: 0xGH").unwrap();
        assert_eq!(yaml.to_string().trim(), "value: 0xGH");
    }

    #[test]
    fn test_number_formats_in_complex_structures() {
        let input = r#"
config:
  permissions: 0o755
  flags: 0b11010
  color: 0xFF00FF
  count: 42"#;

        let yaml = Yaml::from_str(input).unwrap();
        let output = yaml.to_string();

        assert!(output.contains("0o755"));
        assert!(output.contains("0b11010"));
        assert!(output.contains("0xFF00FF"));
        assert!(output.contains("42"));
    }

    #[test]
    fn test_editing_operations() {
        // Test basic editing operations
        let yaml = Yaml::from_str("name: old-name\nversion: 1.0.0").unwrap();
        if let Some(mut doc) = yaml.document() {
            doc.set_string("name", "new-name");
            doc.set_string("version", "2.0.0");

            let output = doc.to_yaml_string();
            assert!(output.contains("new-name"));
            assert!(output.contains("2.0.0"));

            // Verify values can be retrieved
            assert_eq!(doc.get_string("name"), Some("new-name".to_string()));
            assert_eq!(doc.get_string("version"), Some("2.0.0".to_string()));
        }
    }

    #[test]
    fn test_timestamp_parsing_and_validation() {
        use crate::scalar::{ScalarType, ScalarValue};

        // Test various timestamp formats are recognized as timestamps
        let test_cases = vec![
            ("2001-12-14 21:59:43.10 -5", true), // Space-separated with timezone
            ("2001-12-15T02:59:43.1Z", true),    // ISO 8601 with Z
            ("2002-12-14", true),                // Date only
            ("2001-12-14t21:59:43.10-05:00", true), // Lowercase t
            ("2001-12-14 21:59:43.10", true),    // No timezone
            ("2001-12-14T21:59:43", true),       // No fractional seconds
            ("not-a-timestamp", false),          // Invalid
            ("2001-13-14", false),               // Invalid month
            ("2001-12-32", false),               // Invalid day
        ];

        for (timestamp_str, should_be_valid) in test_cases {
            let scalar = ScalarValue::from_yaml(timestamp_str);

            if should_be_valid {
                assert_eq!(
                    scalar.scalar_type(),
                    ScalarType::Timestamp,
                    "Failed to recognize '{}' as timestamp",
                    timestamp_str
                );
                assert!(scalar.is_timestamp());

                // Verify it preserves the original format
                assert_eq!(scalar.value(), timestamp_str);

                // Test YAML parsing preserves it
                let yaml = format!("timestamp: {}", timestamp_str);
                let parsed = Yaml::from_str(&yaml).unwrap();
                let output = parsed.to_string();
                assert!(
                    output.contains(timestamp_str),
                    "Timestamp '{}' not preserved in output",
                    timestamp_str
                );
            } else {
                assert_ne!(
                    scalar.scalar_type(),
                    ScalarType::Timestamp,
                    "'{}' should not be recognized as timestamp",
                    timestamp_str
                );
            }
        }

        // Test timestamp in different contexts
        let yaml_with_timestamps = r#"
created_at: 2001-12-14 21:59:43.10 -5
updated_at: 2001-12-15T02:59:43.1Z
date_only: 2002-12-14
timestamps_in_array:
  - 2001-12-14 21:59:43.10 -5
  - 2001-12-15T02:59:43.1Z
  - 2002-12-14"#;

        let parsed = Yaml::from_str(yaml_with_timestamps).unwrap();
        let output = parsed.to_string();

        // All timestamps should be preserved
        assert!(output.contains("2001-12-14 21:59:43.10 -5"));
        assert!(output.contains("2001-12-15T02:59:43.1Z"));
        assert!(output.contains("2002-12-14"));
    }

    #[test]
    fn test_regex_support_in_yaml() {
        use crate::scalar::{ScalarType, ScalarValue};

        // Test 1: Parse YAML with regex tags (using simpler patterns)
        let yaml_with_regex = r#"
patterns:
  digits: !!regex '\d+'
  word: !!regex '\w+'
  simple: !!regex 'test'"#;

        let parsed = Yaml::from_str(yaml_with_regex).unwrap();
        let output = parsed.to_string();

        // All regex patterns should be preserved
        assert!(
            output.contains("!!regex"),
            "Output should contain regex tags"
        );

        // Count the number of regex tags to ensure all are preserved
        let input_regex_count = yaml_with_regex.matches("!!regex").count();
        let output_regex_count = output.matches("!!regex").count();
        assert_eq!(
            input_regex_count, output_regex_count,
            "All regex tags should be preserved: input {}, output {}",
            input_regex_count, output_regex_count
        );

        // Test 2: Verify regex scalars are correctly identified
        let regex_scalar = ScalarValue::regex(r"^\d{4}-\d{2}-\d{2}$");
        assert_eq!(regex_scalar.scalar_type(), ScalarType::Regex);
        assert!(regex_scalar.is_regex());
        assert_eq!(regex_scalar.value(), r"^\d{4}-\d{2}-\d{2}$");
        assert_eq!(
            regex_scalar.to_yaml_string(),
            r"!!regex ^\d{4}-\d{2}-\d{2}$"
        );

        // Test 3: Round-trip parsing
        let yaml_simple = "pattern: !!regex '\\d+'";
        let parsed_simple = Yaml::from_str(yaml_simple).unwrap();
        let output_simple = parsed_simple.to_string();
        assert!(
            output_simple.contains("!!regex"),
            "Simple regex should preserve tag"
        );
        assert!(output_simple.contains("pattern:"), "Should preserve key");

        // Test 4: Complex regex patterns
        let complex_regex = r#"validation: !!regex '^https?://(?:[-\w.])+(?:\:[0-9]+)?'"#;
        let parsed_complex = Yaml::from_str(complex_regex).unwrap();
        let output_complex = parsed_complex.to_string();
        assert!(
            output_complex.contains("!!regex"),
            "Complex regex should preserve tag"
        );
        assert!(
            output_complex.contains("validation:"),
            "Should preserve key"
        );
    }

    #[test]
    fn test_regex_in_different_contexts() {
        // Test 1: Regex in sequences
        let yaml_sequence = r#"
patterns:
  - !!regex '\d+'
  - !!regex '[a-z]+'
  - normal_string
  - !!regex '.*@.*\..*'
"#;

        let parsed_seq = Yaml::from_str(yaml_sequence).unwrap();
        let output_seq = parsed_seq.to_string();

        // Should preserve all regex tags in sequence
        assert_eq!(
            output_seq.matches("!!regex").count(),
            3,
            "Should preserve 3 regex tags in sequence"
        );
        assert!(
            output_seq.contains("normal_string"),
            "Should preserve non-regex items"
        );

        // Test 2: Nested mappings with regex (using simple patterns)
        let yaml_nested = r#"
validation:
  email: !!regex '[^@]+@[^@]+\.[a-z]+'
  phone: !!regex '\d{3}-\d{3}-\d{4}'
  config:
    debug_pattern: !!regex 'DEBUG:.*'
    nested:
      deep_pattern: !!regex 'ERROR'
"#;

        let parsed_nested = Yaml::from_str(yaml_nested).unwrap();
        let output_nested = parsed_nested.to_string();

        assert_eq!(
            output_nested.matches("!!regex").count(),
            4,
            "Should preserve 4 regex tags in nested structure"
        );
        assert!(
            output_nested.contains("email:"),
            "Should preserve structure"
        );
        assert!(
            output_nested.contains("nested:"),
            "Should preserve nested structure"
        );

        // Test 3: Mixed collections
        let yaml_mixed = r#"
mixed_collection:
  - name: "test"
    patterns: [!!regex '\d+', !!regex '\w+']
  - patterns:
      simple: !!regex 'test'
      complex: !!regex '^(?:[0-9]{1,3}\.){3}[0-9]{1,3}$'
"#;

        let parsed_mixed = Yaml::from_str(yaml_mixed).unwrap();
        let output_mixed = parsed_mixed.to_string();

        assert_eq!(
            output_mixed.matches("!!regex").count(),
            4,
            "Should preserve 4 regex tags in mixed collections"
        );
        assert!(
            output_mixed.contains("mixed_collection:"),
            "Should preserve root structure"
        );

        // Test 4: Flow style with regex
        let yaml_flow =
            r#"inline_patterns: {email: !!regex '[^@]+@[^@]+', phone: !!regex '\d{3}-\d{4}'}"#;

        let parsed_flow = Yaml::from_str(yaml_flow).unwrap();
        let output_flow = parsed_flow.to_string();

        assert_eq!(
            output_flow.matches("!!regex").count(),
            2,
            "Should preserve 2 regex tags in flow style"
        );
    }

    #[test]
    fn test_regex_parsing_edge_cases() {
        // Test 1: Regex with various quote styles (step by step)
        // Test various quote styles
        let yaml_quotes = r#"
patterns:
  single_quoted: !!regex 'pattern with spaces'
  double_quoted: !!regex "pattern_without_escapes"
  unquoted: !!regex simple_pattern
"#;

        let parsed_quotes = Yaml::from_str(yaml_quotes).unwrap();
        let output_quotes = parsed_quotes.to_string();
        assert_eq!(
            output_quotes.matches("!!regex").count(),
            3,
            "Should preserve all regex tags"
        );

        // Test 2: Empty and whitespace patterns
        let yaml_empty = r#"
empty: !!regex ''
whitespace: !!regex '   '
tabs: !!regex '	'
"#;

        let parsed_empty = Yaml::from_str(yaml_empty);
        assert!(
            parsed_empty.is_ok(),
            "Should parse empty/whitespace regex patterns"
        );
        let output_empty = parsed_empty.unwrap().to_string();
        assert_eq!(
            output_empty.matches("!!regex").count(),
            3,
            "Should preserve empty regex patterns"
        );

        // Test 3: Regex with special characters (avoiding YAML conflicts)
        let yaml_special = r#"special: !!regex 'pattern_with_underscores_and_123'"#;

        let parsed_special = Yaml::from_str(yaml_special);
        assert!(
            parsed_special.is_ok(),
            "Should parse regex with safe special characters"
        );
        let output_special = parsed_special.unwrap().to_string();
        assert!(
            output_special.contains("!!regex"),
            "Should preserve regex tag"
        );

        // Test 4: Verify regex scalars maintain their properties after parsing
        let yaml_verify = r#"test_pattern: !!regex '\d{4}-\d{2}-\d{2}'"#;
        let parsed_verify = Yaml::from_str(yaml_verify).unwrap();
        let output_verify = parsed_verify.to_string();

        // Verify the regex pattern is preserved in output
        assert!(
            output_verify.contains("!!regex"),
            "Should preserve regex tag"
        );
        assert!(
            output_verify.contains(r"\d{4}-\d{2}-\d{2}"),
            "Should preserve regex pattern"
        );
        assert!(
            output_verify.contains("test_pattern:"),
            "Should preserve key"
        );

        // Test 5: Multiple regex patterns in one document
        let yaml_multiple = r#"
patterns:
  email: !!regex '^[^\s@]+@[^\s@]+\.[^\s@]+$'
  phone: !!regex '^\+?[\d\s\-\(\)]{10,}$'
  url: !!regex '^https?://[^\s]+$'
  ipv4: !!regex '^(?:[0-9]{1,3}\.){3}[0-9]{1,3}$'
  uuid: !!regex '^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$'
"#;

        let parsed_multiple = Yaml::from_str(yaml_multiple);
        assert!(
            parsed_multiple.is_ok(),
            "Should parse multiple regex patterns"
        );
        let output_multiple = parsed_multiple.unwrap().to_string();
        assert_eq!(
            output_multiple.matches("!!regex").count(),
            5,
            "Should preserve all 5 regex patterns"
        );

        // Verify each pattern is preserved in output
        assert!(
            output_multiple.contains("email:"),
            "Should preserve email key"
        );
        assert!(
            output_multiple.contains("phone:"),
            "Should preserve phone key"
        );
        assert!(output_multiple.contains("url:"), "Should preserve url key");
        assert!(
            output_multiple.contains("ipv4:"),
            "Should preserve ipv4 key"
        );
        assert!(
            output_multiple.contains("uuid:"),
            "Should preserve uuid key"
        );
    }

    #[test]
    fn test_enhanced_comment_support() {
        // Test improvements: mid-line comments, comments in flow collections,
        // and better comment positioning preservation

        // Test 1: Comments in flow sequences
        let yaml1 = r#"flow_seq: [
    item1, # comment after item1
    item2, # comment after item2
    item3  # comment after item3
]"#;
        let parsed1 = Yaml::from_str(yaml1).unwrap();
        let output1 = parsed1.to_string();
        assert!(output1.contains("# comment after item1"));
        assert!(output1.contains("# comment after item2"));
        assert!(output1.contains("# comment after item3"));

        // Test 2: Comments in flow mappings
        let yaml2 = r#"flow_map: {
    key1: val1, # comment after first pair
    key2: val2, # comment after second pair
    key3: val3  # comment after third pair
}"#;
        let parsed2 = Yaml::from_str(yaml2).unwrap();
        let output2 = parsed2.to_string();
        assert!(output2.contains("# comment after first pair"));
        assert!(output2.contains("# comment after second pair"));
        assert!(output2.contains("# comment after third pair"));

        // Test 3: Mixed nested structures with comments
        let yaml3 = r#"config:
  servers: [
    {name: web1, port: 80},   # Web server 1
    {name: web2, port: 80},   # Web server 2
    {name: db1, port: 5432}   # Database server
  ] # End servers array"#;
        let parsed3 = Yaml::from_str(yaml3).unwrap();
        let output3 = parsed3.to_string();
        assert!(output3.contains("# Web server 1"));
        assert!(output3.contains("# Web server 2"));
        assert!(output3.contains("# Database server"));
        assert!(output3.contains("# End servers array"));

        // Test 4: Comments between sequence items (block style)
        let yaml4 = r#"items:
  - first   # First item comment
  - second  # Second item comment
  # Comment between items
  - third   # Third item comment"#;
        let parsed4 = Yaml::from_str(yaml4).unwrap();
        let output4 = parsed4.to_string();
        assert!(output4.contains("# First item comment"));
        assert!(output4.contains("# Second item comment"));
        assert!(output4.contains("# Comment between items"));
        assert!(output4.contains("# Third item comment"));

        // Test 5: Round-trip preservation
        for yaml in [yaml1, yaml2, yaml3, yaml4] {
            let parsed = Yaml::from_str(yaml).unwrap();
            let output = parsed.to_string();
            let reparsed = Yaml::from_str(&output);
            assert!(reparsed.is_ok(), "Round-trip parsing should succeed");
        }
    }

    #[test]
    fn test_document_schema_validation_api() {
        // Test the new Document API methods for schema validation

        // JSON-compatible document
        let json_yaml = r#"
name: "John"
age: 30
active: true
items:
  - "apple"
  - 42
  - true
"#;
        let doc = Yaml::from_str(json_yaml).unwrap().document().unwrap();

        // Test JSON schema validation - should pass
        assert!(
            doc.validate_json().is_ok(),
            "JSON-compatible document should pass JSON validation"
        );

        // Test Core schema validation - should pass
        assert!(
            doc.validate_core().is_ok(),
            "Valid document should pass Core validation"
        );

        // Test Failsafe schema validation - should fail due to numbers and booleans (in strict mode)
        // Note: Non-strict failsafe might allow coercion, so test strict mode
        let failsafe_strict = crate::schema::SchemaValidator::failsafe().strict();
        assert!(
            doc.validate_schema(&failsafe_strict).is_err(),
            "Document with numbers and booleans should fail strict Failsafe validation"
        );

        // YAML-specific document
        let yaml_specific = r#"
name: "Test"
created: 2023-12-25T10:30:45Z
pattern: !!regex '\d+'
data: !!binary "SGVsbG8="
"#;
        let yaml_doc = Yaml::from_str(yaml_specific).unwrap().document().unwrap();

        // Test Core schema - should pass
        assert!(
            yaml_doc.validate_core().is_ok(),
            "YAML-specific types should pass Core validation"
        );

        // Test JSON schema - should fail due to timestamp, regex, binary
        assert!(
            yaml_doc.validate_json().is_err(),
            "YAML-specific types should fail JSON validation"
        );

        // Test Failsafe schema - should fail
        assert!(
            yaml_doc.validate_failsafe().is_err(),
            "YAML-specific types should fail Failsafe validation"
        );

        // String-only document
        let string_only = r#"
name: hello
message: world
items:
  - apple
  - banana
nested:
  key: value
"#;
        let str_doc = Yaml::from_str(string_only).unwrap().document().unwrap();

        // All schemas should pass (strings are allowed in all schemas)
        assert!(
            str_doc.validate_failsafe().is_ok(),
            "String-only document should pass Failsafe validation"
        );
        assert!(
            str_doc.validate_json().is_ok(),
            "String-only document should pass JSON validation"
        );
        assert!(
            str_doc.validate_core().is_ok(),
            "String-only document should pass Core validation"
        );
    }

    #[test]
    fn test_mapping_simple_set() {
        let yaml = "key1: value1";
        let parsed = Yaml::from_str(yaml).unwrap();

        // Get document and add a new key
        let mut doc = parsed.document().expect("Should have a document");
        doc.set("key2", "value2");

        let output = doc.to_string();
        println!("After set:\n{}", output);

        let expected = r#"key1: value1
key2: value2"#;
        assert_eq!(output.trim(), expected);
    }

    #[test]
    fn test_mapping_insert_after() {
        let yaml = r#"first: 1
second: 2
fourth: 4"#;

        let parsed = Yaml::from_str(yaml).unwrap();
        println!("Original YAML:\n{}", parsed);

        let mut doc = parsed.document().expect("Should have a document");

        // Insert after "second"
        let success = doc.insert_after("second", "third", "3");
        assert!(
            success,
            "insert_after should succeed when reference key exists"
        );

        let output = doc.to_string();
        println!("Output after insert_after:\n{}", output);

        // Check exact output - should preserve original structure and insert correctly
        let expected = r#"first: 1
second: 2
third: '3'
fourth: 4"#;
        assert_eq!(output.trim(), expected);

        // Test inserting after non-existent key
        let failed = doc.insert_after("nonexistent", "new_key", "new_value");
        assert!(
            !failed,
            "insert_after should fail when reference key doesn't exist"
        );

        // Test updating existing key through insert_after
        let updated = doc.insert_after("first", "second", "2_updated");
        assert!(updated, "insert_after should update existing key");
        // Verify the string output contains what we expect
        let updated_output = doc.to_string();
        assert!(
            updated_output.contains("second: 2_updated"),
            "Output should contain 'second: 2_updated', got: {}",
            updated_output
        );
    }

    #[test]
    fn test_mapping_insert_before() {
        let yaml = r#"first: 1
third: 3
fourth: 4"#;

        let parsed = Yaml::from_str(yaml).unwrap();
        let mut doc = parsed.document().expect("Should have a document");

        // Insert before "third"
        let success = doc.insert_before("third", "second", "2");
        assert!(
            success,
            "insert_before should succeed when reference key exists"
        );

        let output = doc.to_string();

        // Check exact output - should preserve original structure and insert correctly
        let expected = r#"first: 1
second: '2'
third: 3
fourth: 4"#;
        assert_eq!(output.trim(), expected);

        // Test inserting before non-existent key
        let failed = doc.insert_before("nonexistent", "new_key", "new_value");
        assert!(
            !failed,
            "insert_before should fail when reference key doesn't exist"
        );

        // Test updating existing key through insert_before
        let updated = doc.insert_before("fourth", "third", "3_updated");
        assert!(updated, "insert_before should update existing key");
        // Verify the string output contains what we expect
        let output = doc.to_string();
        assert!(
            output.contains("third: 3_updated"),
            "Output should contain 'third: 3_updated', got: {}",
            output
        );
    }

    #[test]
    fn test_mapping_insert_at_index() {
        let yaml = r#"first: 1
third: 3"#;

        let parsed = Yaml::from_str(yaml).unwrap();
        let mut doc = parsed.document().expect("Should have a document");

        // Insert at index 1 (between first and third)
        doc.insert_at_index(1, "second", "2");

        let output = doc.to_string();

        // Check exact output - should preserve original structure and insert correctly
        let expected = r#"first: 1
second: '2'
third: 3"#;
        assert_eq!(output.trim(), expected);

        // Insert at index 0 (beginning)
        doc.insert_at_index(0, "zero", "0");
        let output2 = doc.to_string();
        println!("Output2 after inserting at index 0:\n{}", output2);
        let lines2: Vec<&str> = output2.trim().lines().collect();
        assert!(
            lines2[0].starts_with("zero:"), // First line should be "zero:"
            "Expected zero at index 0, but lines are: {:?}",
            lines2
        );

        // Insert at out-of-bounds index (should append at end)
        doc.insert_at_index(100, "last", "999");
        let output3 = doc.to_string();
        let lines3: Vec<&str> = output3.trim().lines().collect();
        assert!(lines3.last().unwrap().starts_with("last:"));

        // Test updating existing key through insert_at_index
        doc.insert_at_index(2, "first", "1_updated");
        // Verify the string output contains what we expect
        let final_output = doc.to_string();
        assert!(
            final_output.contains("first: 1_updated"),
            "Output should contain 'first: 1_updated', got: {}",
            final_output
        );
    }

    #[test]
    fn test_mapping_insert_special_characters() {
        let yaml = "key1: value1";

        let parsed = Yaml::from_str(yaml).unwrap();
        let mut doc = parsed.document().expect("Should have a document");

        // Test with special characters that need escaping
        doc.insert_after("key1", "special:key", "value:with:colons");
        doc.insert_before("key1", "key with spaces", "value with spaces");
        doc.insert_at_index(1, "key@symbol", "value#hash");

        // Verify all keys are present
        assert!(doc.contains_key(&YamlValue::from("special:key")));
        assert!(doc.contains_key(&YamlValue::from("key with spaces")));
        assert!(doc.contains_key(&YamlValue::from("key@symbol")));

        // Parse the output to verify it's valid YAML
        let output = doc.to_string();
        let reparsed = Yaml::from_str(&output);
        assert!(reparsed.is_ok(), "Output should be valid YAML");
    }

    #[test]
    fn test_mapping_insert_empty_values() {
        let yaml = "key1: value1";

        let parsed = Yaml::from_str(yaml).unwrap();
        let mut doc = parsed.document().expect("Should have a document");

        // Test with empty values
        doc.insert_after("key1", "empty", "");
        doc.insert_before("key1", "null_key", ScalarValue::null());

        assert!(doc.contains_key(&YamlValue::from("empty")));
        assert!(doc.contains_key(&YamlValue::from("null_key")));

        // Verify the output is valid YAML
        let output = parsed.to_string();
        let reparsed = Yaml::from_str(&output);
        assert!(
            reparsed.is_ok(),
            "Output with empty values should be valid YAML"
        );
    }

    #[test]
    fn test_document_schema_coercion_api() {
        // Test the coercion API
        let coercion_yaml = r#"
count: "42"
enabled: "true"
rate: "3.14"
items:
  - "100"
  - "false"
"#;
        let doc = Yaml::from_str(coercion_yaml).unwrap().document().unwrap();
        let json_validator = crate::schema::SchemaValidator::json();

        // Test coercion - should pass because strings can be coerced to numbers/booleans
        assert!(
            doc.can_coerce_to_schema(&json_validator).is_ok(),
            "Strings that look like numbers/booleans should be coercible to JSON types"
        );

        // Test with non-coercible types
        let non_coercible = r#"
timestamp: !!timestamp "2023-01-01"
pattern: !!regex '\d+'
"#;
        let non_coer_doc = Yaml::from_str(non_coercible).unwrap().document().unwrap();

        // Should fail coercion to JSON schema
        assert!(
            non_coer_doc.can_coerce_to_schema(&json_validator).is_err(),
            "YAML-specific types should not be coercible to JSON schema"
        );
    }

    #[test]
    fn test_document_schema_validation_errors() {
        // Test that error messages contain useful path information
        let nested_yaml = r#"
users:
  - name: "Alice"
    age: 25
    metadata:
      created: !!timestamp "2023-01-01"
      active: true
  - name: "Bob"
    score: 95.5
"#;
        let doc = Yaml::from_str(nested_yaml).unwrap().document().unwrap();

        // Test Failsafe validation with detailed error checking (strict mode)
        let failsafe_strict = crate::schema::SchemaValidator::failsafe().strict();
        let failsafe_result = doc.validate_schema(&failsafe_strict);
        assert!(
            failsafe_result.is_err(),
            "Nested document with numbers should fail strict Failsafe validation"
        );

        let errors = failsafe_result.unwrap_err();
        assert!(!errors.is_empty(), "Should have validation errors");

        // Check that error paths are meaningful
        let error_messages: Vec<String> = errors.iter().map(|e| e.to_string()).collect();
        let has_path_info = error_messages
            .iter()
            .any(|msg| msg.contains("root.") || msg.contains("["));
        assert!(
            has_path_info,
            "Errors should contain path information: {:?}",
            error_messages
        );

        // Test JSON validation
        let json_result = doc.validate_json();
        assert!(
            json_result.is_err(),
            "Document with timestamp should fail JSON validation"
        );

        let json_errors = json_result.unwrap_err();
        let timestamp_error = json_errors
            .iter()
            .any(|e| e.message().contains("json schema"));
        assert!(timestamp_error, "Should have JSON schema error");
    }

    #[test]
    fn test_document_schema_validation_with_custom_validator() {
        // Test using the general validate_schema method
        let yaml = r#"
name: "HelloWorld"
count: 42
active: true
"#;
        let doc = Yaml::from_str(yaml).unwrap().document().unwrap();

        // Create different validators and test
        let json_validator = crate::schema::SchemaValidator::json();
        let core_validator = crate::schema::SchemaValidator::core();

        // Test with custom validators
        assert!(
            doc.validate_schema(&core_validator).is_ok(),
            "Should pass Core validation"
        );
        assert!(
            doc.validate_schema(&json_validator).is_ok(),
            "Should pass JSON validation"
        );
        // Non-strict failsafe might allow coercion, so test strict mode
        let failsafe_strict = crate::schema::SchemaValidator::failsafe().strict();
        assert!(
            doc.validate_schema(&failsafe_strict).is_err(),
            "Should fail strict Failsafe validation"
        );

        // Test strict mode
        let strict_json = crate::schema::SchemaValidator::json().strict();
        // The document contains integers and booleans which are valid in JSON schema
        assert!(
            doc.validate_schema(&strict_json).is_ok(),
            "Should pass strict JSON validation (integers and booleans are JSON-compatible)"
        );

        let strict_failsafe = crate::schema::SchemaValidator::failsafe().strict();
        assert!(
            doc.validate_schema(&strict_failsafe).is_err(),
            "Should fail strict Failsafe validation"
        );
    }

    #[test]
    fn test_insert_after_with_sequence() {
        let yaml = "name: project\nversion: 1.0.0";
        let parsed = Yaml::from_str(yaml).unwrap();
        let mut doc = parsed.document().expect("Should have a document");

        // Insert a sequence after "name"
        let features = YamlValue::from(vec!["feature1", "feature2", "feature3"]);
        let success = doc.insert_after("name", "features", features);
        assert!(success, "insert_after should succeed");

        let output = doc.to_string();
        println!("Output with sequence:\n{}", output);

        // Verify exact output
        let expected = r#"name: project
features:     - feature1
    - feature2
    - feature3
version: 1.0.0"#;
        assert_eq!(output.trim(), expected);

        // Verify it's valid YAML
        let reparsed = Yaml::from_str(&output);
        assert!(reparsed.is_ok(), "Output should be valid YAML");
    }

    #[test]
    fn test_insert_before_with_mapping() {
        let yaml = "name: project\nversion: 1.0.0";
        let parsed = Yaml::from_str(yaml).unwrap();
        let mut doc = parsed.document().expect("Should have a document");

        // Insert a nested mapping before "version"
        let mut db_config = std::collections::BTreeMap::new();
        db_config.insert("host".to_string(), YamlValue::from("localhost"));
        db_config.insert("port".to_string(), YamlValue::from(5432));
        db_config.insert("database".to_string(), YamlValue::from("mydb"));

        let database = YamlValue::from_mapping(db_config);
        let success = doc.insert_before("version", "database", database);
        assert!(success, "insert_before should succeed");

        let output = doc.to_string();
        println!("Output with nested mapping:\n{}", output);

        // Verify the nested mapping is present and in correct order
        assert!(output.contains("name: project"));
        assert!(output.contains("database:"));
        assert!(output.contains("host: localhost"));
        assert!(output.contains("port: 5432"));
        assert!(output.contains("version: 1.0.0"));

        // Verify it's valid YAML
        let reparsed = Yaml::from_str(&output);
        assert!(reparsed.is_ok(), "Output should be valid YAML");
    }

    #[test]
    fn test_insert_at_index_with_mixed_types() {
        let yaml = "name: project";
        let parsed = Yaml::from_str(yaml).unwrap();
        let mut doc = parsed.document().expect("Should have a document");

        // Insert different types at various indices
        doc.insert_at_index(1, "version", YamlValue::from("1.0.0"));
        doc.insert_at_index(2, "active", YamlValue::from(true));
        doc.insert_at_index(3, "count", YamlValue::from(42));

        let features = YamlValue::from(vec!["auth", "logging"]);
        doc.insert_at_index(4, "features", features);

        let output = doc.to_string();
        println!("Output with mixed types:\n{}", output);

        // Verify all values are present
        assert!(output.contains("name: project"));
        assert!(output.contains("version: 1.0.0"));
        assert!(output.contains("active: true"));
        assert!(output.contains("count: 42"));
        assert!(output.contains("features:"));
        assert!(output.contains("auth"));
        assert!(output.contains("logging"));

        // Verify it's valid YAML
        let reparsed = Yaml::from_str(&output);
        assert!(reparsed.is_ok(), "Output should be valid YAML");
    }

    #[test]
    fn test_insert_with_null_and_special_scalars() {
        let yaml = "name: project";
        let parsed = Yaml::from_str(yaml).unwrap();
        let mut doc = parsed.document().expect("Should have a document");

        // Insert various scalar types
        doc.insert_after("name", "null_value", YamlValue::from(ScalarValue::null()));
        doc.insert_after("null_value", "empty_string", YamlValue::from(""));
        doc.insert_after("empty_string", "number", YamlValue::from(1.234));
        doc.insert_after("number", "boolean", YamlValue::from(false));

        let output = doc.to_string();
        println!("Output with special scalars:\n{}", output);

        // Verify all values are present
        assert!(output.contains("name: project"));
        assert!(output.contains("null_value"));
        assert!(output.contains("empty_string"));
        assert!(output.contains("number: 1.234"));
        assert!(output.contains("boolean: false"));

        // Verify it's valid YAML
        let reparsed = Yaml::from_str(&output);
        assert!(reparsed.is_ok(), "Output should be valid YAML");
    }

    #[test]
    fn test_insert_ordering_preservation() {
        let yaml = "first: 1\nthird: 3\nfifth: 5";
        let parsed = Yaml::from_str(yaml).unwrap();
        let mut doc = parsed.document().expect("Should have a document");

        // Insert items to create proper ordering
        doc.insert_after("first", "second", YamlValue::from(2));
        doc.insert_before("fifth", "fourth", YamlValue::from(4));

        let output = doc.to_string();

        // Check exact output - should preserve original structure and insert correctly
        let expected = r#"first: 1
second: 2
third: 3
fourth: 4
fifth: 5"#;
        assert_eq!(output.trim(), expected);

        // Verify it's valid YAML
        let reparsed = Yaml::from_str(&output);
        assert!(reparsed.is_ok(), "Output should be valid YAML");
    }

    #[test]
    fn test_insert_with_yamlvalue_positioning() {
        let yaml = "name: project\nversion: 1.0\nactive: true";
        let parsed = Yaml::from_str(yaml).unwrap();
        let mut doc = parsed.document().expect("Should have a document");

        // Test positioning with different YamlValue types

        // Position after a string value (using YamlValue instead of &str)
        let string_key = YamlValue::from("name");
        let success = doc.insert_after(string_key, "description", "A sample project");
        assert!(success, "Should find string key");

        // Position after a numeric value
        let numeric_key = YamlValue::from(1.0);
        let success = doc.insert_after(numeric_key, "build", "gradle");
        assert!(
            !success,
            "Should not find numeric key (1.0) when actual key is string 'version'"
        );

        // Position after a boolean value
        let bool_key = YamlValue::from(true);
        let success = doc.insert_after(bool_key, "test", "enabled");
        assert!(
            !success,
            "Should not find boolean key (true) when actual key is string 'active'"
        );

        // But string representation should work
        let bool_string_key = YamlValue::from("true");
        let success = doc.insert_after(bool_string_key, "test_mode", "development");
        assert!(!success, "Should not find 'true' key when value is true");

        let output = doc.to_string();
        println!("Output with YamlValue positioning:\n{}", output);

        // Should only have the description added after name
        assert!(output.contains("name: project"));
        assert!(output.contains("description: A sample project"));
        assert!(output.contains("version: 1.0"));
        assert!(output.contains("active: true"));
        assert!(!output.contains("build: gradle"));
        assert!(!output.contains("test: enabled"));
        assert!(!output.contains("test_mode: development"));

        // Check exact output - should preserve original structure and only insert description after name
        let expected = r#"name: project
description: A sample project
version: 1.0
active: true"#;
        assert_eq!(output.trim(), expected);
    }

    #[test]
    fn test_insert_complex_nested_structure() {
        let yaml = "name: project";
        let parsed = Yaml::from_str(yaml).unwrap();
        let mut doc = parsed.document().expect("Should have a document");

        // Create a complex nested structure
        let mut server_config = std::collections::BTreeMap::new();
        server_config.insert("host".to_string(), YamlValue::from("0.0.0.0"));
        server_config.insert("port".to_string(), YamlValue::from(8080));

        let mut app_config = std::collections::BTreeMap::new();
        app_config.insert("server".to_string(), YamlValue::from_mapping(server_config));
        app_config.insert("debug".to_string(), YamlValue::from(true));
        app_config.insert(
            "features".to_string(),
            YamlValue::from(vec!["api", "web", "cli"]),
        );

        doc.insert_after("name", "config", YamlValue::from_mapping(app_config));

        let output = doc.to_string();
        println!("Output with complex nested structure:\n{}", output);

        // Verify nested structure is present
        assert!(output.contains("name: project"));
        assert!(output.contains("config:"));
        assert!(output.contains("server:"));
        assert!(output.contains("host: 0.0.0.0"));
        assert!(output.contains("port: 8080"));
        assert!(output.contains("debug: true"));
        assert!(output.contains("features:"));
        assert!(output.contains("api"));
        assert!(output.contains("web"));
        assert!(output.contains("cli"));

        // Verify it's valid YAML
        let reparsed = Yaml::from_str(&output);
        assert!(reparsed.is_ok(), "Output should be valid YAML");
    }

    #[test]
    fn test_insert_with_yaml_sets() {
        let yaml = "name: project";
        let parsed = Yaml::from_str(yaml).unwrap();
        let mut doc = parsed.document().expect("Should have a document");

        // Insert a YAML set
        let mut tags = std::collections::BTreeSet::new();
        tags.insert("production".to_string());
        tags.insert("database".to_string());
        tags.insert("web".to_string());

        let yaml_set = YamlValue::from_set(tags);
        doc.insert_after("name", "tags", yaml_set);

        let output = doc.to_string();
        println!("Output with YAML set:\n{}", output);

        // Verify set structure is present
        assert!(output.contains("name: project"));
        assert!(output.contains("tags:"));
        assert!(output.contains("!!set"));
        assert!(output.contains("production: null"));
        assert!(output.contains("database: null"));
        assert!(output.contains("web: null"));

        // Verify it's valid YAML
        let reparsed = Yaml::from_str(&output);
        assert!(reparsed.is_ok(), "Output should be valid YAML");
    }

    #[test]
    fn test_insert_with_ordered_mappings() {
        let yaml = "name: project";
        let parsed = Yaml::from_str(yaml).unwrap();
        let mut doc = parsed.document().expect("Should have a document");

        // Insert a YAML ordered mapping (!!omap)
        let ordered_steps = vec![
            ("compile".to_string(), YamlValue::from("gcc main.c")),
            ("test".to_string(), YamlValue::from("./a.out test")),
            (
                "package".to_string(),
                YamlValue::from("tar -czf app.tar.gz ."),
            ),
        ];

        let yaml_omap = YamlValue::from_ordered_mapping(ordered_steps);
        doc.insert_after("name", "build_steps", yaml_omap);

        let output = doc.to_string();
        println!("Output with ordered mapping:\n{}", output);

        // Verify ordered mapping structure is present
        assert!(output.contains("name: project"));
        assert!(output.contains("build_steps:"));
        assert!(output.contains("!!omap"));
        assert!(output.contains("- compile: gcc main.c"));
        assert!(output.contains("- test: ./a.out test"));
        assert!(output.contains("- package: tar -czf app.tar.gz ."));

        // Verify it's valid YAML
        let reparsed = Yaml::from_str(&output);
        assert!(reparsed.is_ok(), "Output should be valid YAML");
    }

    #[test]
    fn test_insert_with_pairs() {
        let yaml = "name: project";
        let parsed = Yaml::from_str(yaml).unwrap();
        let mut doc = parsed.document().expect("Should have a document");

        // Insert a YAML pairs collection (!!pairs - allows duplicate keys)
        let connection_attempts = vec![
            ("server".to_string(), YamlValue::from("primary.db")),
            ("server".to_string(), YamlValue::from("secondary.db")), // Duplicate key allowed
            ("server".to_string(), YamlValue::from("tertiary.db")),  // Another duplicate
            ("timeout".to_string(), YamlValue::from(30)),
        ];

        let yaml_pairs = YamlValue::from_pairs(connection_attempts);
        doc.insert_after("name", "connections", yaml_pairs);

        let output = doc.to_string();
        println!("Output with pairs:\n{}", output);

        // Verify pairs structure is present
        assert!(output.contains("name: project"));
        assert!(output.contains("connections:"));
        assert!(output.contains("!!pairs"));
        assert!(output.contains("- server: primary.db"));
        assert!(output.contains("- server: secondary.db"));
        assert!(output.contains("- server: tertiary.db"));
        assert!(output.contains("- timeout: 30"));

        // Verify it's valid YAML
        let reparsed = Yaml::from_str(&output);
        assert!(reparsed.is_ok(), "Output should be valid YAML");
    }

    #[test]
    fn test_insert_with_empty_collections() {
        // Test each empty collection type individually to avoid chaining issues

        // Test empty sequence
        let yaml1 = "name: project";
        let parsed1 = Yaml::from_str(yaml1).unwrap();
        let mut doc1 = parsed1.document().expect("Should have a document");
        doc1.insert_after("name", "empty_list", YamlValue::sequence());
        let output1 = doc1.to_string();
        println!("Output with empty sequence:\n{}", output1);
        assert!(output1.contains("name: project"));
        assert!(output1.contains("empty_list:"));
        assert!(output1.contains("[]"));

        // Test empty mapping
        let yaml2 = "name: project";
        let parsed2 = Yaml::from_str(yaml2).unwrap();
        let mut doc2 = parsed2.document().expect("Should have a document");
        doc2.insert_after("name", "empty_map", YamlValue::mapping());
        let output2 = doc2.to_string();
        println!("Output with empty mapping:\n{}", output2);
        assert!(output2.contains("name: project"));
        assert!(output2.contains("empty_map:"));
        assert!(output2.contains("{}"));

        // Test empty set
        let yaml3 = "name: project";
        let parsed3 = Yaml::from_str(yaml3).unwrap();
        let mut doc3 = parsed3.document().expect("Should have a document");
        doc3.insert_after("name", "empty_set", YamlValue::set());
        let output3 = doc3.to_string();
        println!("Output with empty set:\n{}", output3);
        assert!(output3.contains("name: project"));
        assert!(output3.contains("empty_set:"));
        assert!(output3.contains("!!set {}"));

        // Verify all are valid YAML
        assert!(
            Yaml::from_str(&output1).is_ok(),
            "Empty sequence output should be valid YAML"
        );
        assert!(
            Yaml::from_str(&output2).is_ok(),
            "Empty mapping output should be valid YAML"
        );
        assert!(
            Yaml::from_str(&output3).is_ok(),
            "Empty set output should be valid YAML"
        );
    }

    #[test]
    fn test_insert_with_deeply_nested_sequences() {
        let yaml = "name: project";
        let parsed = Yaml::from_str(yaml).unwrap();
        let mut doc = parsed.document().expect("Should have a document");

        // Create deeply nested sequence with mixed types
        let level3_config = YamlValue::from(vec![
            YamlValue::from("debug"),
            YamlValue::from(true),
            YamlValue::from(8080),
        ]);

        let mut level2_map = std::collections::BTreeMap::new();
        level2_map.insert("config".to_string(), level3_config);
        level2_map.insert("name".to_string(), YamlValue::from("service"));

        let level1_sequence = YamlValue::from(vec![
            YamlValue::from("item1"),
            YamlValue::from_mapping(level2_map),
            YamlValue::from(42),
        ]);

        doc.insert_after("name", "nested_data", level1_sequence);

        let output = doc.to_string();
        println!("Output with deeply nested sequence:\n{}", output);

        // Verify nested structure
        assert!(output.contains("name: project"));
        assert!(output.contains("nested_data:"));
        assert!(output.contains("- item1"));
        assert!(output.contains("config:"));
        assert!(output.contains("- debug"));
        assert!(output.contains("- true"));
        assert!(output.contains("- 8080"));
        assert!(output.contains("name: service"));
        assert!(output.contains("- 42"));

        // Verify it's valid YAML
        let reparsed = Yaml::from_str(&output);
        assert!(reparsed.is_ok(), "Output should be valid YAML");
    }

    #[test]
    fn test_document_level_insertion_with_complex_types() {
        // Test the Document-level API with complex types separately to avoid chaining issues

        // Test Document.insert_after with sequence
        let mut doc1 = Document::new();
        doc1.set_string("name", "project");
        let features = YamlValue::from(vec!["auth", "api", "web"]);
        let success = doc1.insert_after("name", "features", features);
        assert!(success, "Document insert_after should succeed");
        let output1 = doc1.to_yaml_string();
        println!("Document output with sequence:\n{}", output1);
        assert!(output1.contains("name: project"));
        assert!(output1.contains("features:"));
        assert!(output1.contains("- auth"));

        // Test Document.insert_before with mapping
        let mut doc2 = Document::new();
        doc2.set_string("name", "project");
        doc2.set_string("version", "1.0.0");
        let mut db_config = std::collections::BTreeMap::new();
        db_config.insert("host".to_string(), YamlValue::from("localhost"));
        db_config.insert("port".to_string(), YamlValue::from(5432));
        let database = YamlValue::from_mapping(db_config);
        let success = doc2.insert_before("version", "database", database);
        assert!(success, "Document insert_before should succeed");
        let output2 = doc2.to_yaml_string();
        println!("Document output with mapping:\n{}", output2);
        assert!(output2.contains("name: project"));
        assert!(output2.contains("database:"));
        assert!(output2.contains("host: localhost"));
        assert!(output2.contains("port: 5432"));
        assert!(output2.contains("version: 1.0.0"));

        // Test Document.insert_at_index with set
        let mut doc3 = Document::new();
        doc3.set_string("name", "project");
        let mut tags = std::collections::BTreeSet::new();
        tags.insert("production".to_string());
        tags.insert("database".to_string());
        let tag_set = YamlValue::from_set(tags);
        doc3.insert_at_index(1, "tags", tag_set);
        let output3 = doc3.to_yaml_string();
        println!("Document output with set:\n{}", output3);
        assert!(output3.contains("name: project"));
        assert!(output3.contains("tags:"));
        assert!(output3.contains("!!set"));
        assert!(output3.contains("production: null"));

        // Verify all are valid YAML by parsing
        assert!(
            Yaml::from_str(&output1).is_ok(),
            "Sequence output should be valid YAML"
        );
        assert!(
            Yaml::from_str(&output2).is_ok(),
            "Mapping output should be valid YAML"
        );
        assert!(
            Yaml::from_str(&output3).is_ok(),
            "Set output should be valid YAML"
        );
    }

    #[test]
    fn test_ast_preservation_comments_in_mapping() {
        // Test that comments within mappings are preserved during insertions
        let yaml = r#"---
# Header comment
key1: value1  # Inline comment 1
# Middle comment
key2: value2  # Inline comment 2
# Footer comment
"#;
        let mut doc = Yaml::from_str(yaml).unwrap().document().unwrap();

        println!("Original YAML:\n{}", yaml);

        // Insert a new key using AST-preserving method - changes propagate automatically
        if let Some(mut mapping) = doc.as_mapping_mut() {
            mapping.insert_after_preserving("key1", "new_key", "new_value");
        }

        let result = doc.to_string();
        println!("Result after insertion:\n{}", result);

        // These should be preserved with transparent mutations
        assert!(
            result.contains("# Header comment"),
            "Header comment should be preserved"
        );
        assert!(
            result.contains("# Inline comment 1"),
            "Inline comment 1 should be preserved"
        );
        assert!(
            result.contains("# Middle comment"),
            "Middle comment should be preserved"
        );
        assert!(
            result.contains("# Inline comment 2"),
            "Inline comment 2 should be preserved"
        );
        assert!(
            result.contains("# Footer comment"),
            "Footer comment should be preserved"
        );
    }

    #[test]
    fn test_ast_preservation_whitespace_in_mapping() {
        // Test that whitespace and formatting within mappings are preserved
        let yaml = r#"---
key1:    value1


key2:        value2
"#;
        let mut doc = Yaml::from_str(yaml).unwrap().document().unwrap();

        // Insert a new key using AST-preserving method
        if let Some(mut mapping) = doc.as_mapping_mut() {
            mapping.insert_after_preserving("key1", "new_key", "new_value");
        }

        let result = doc.to_string();

        // These formatting details should be preserved but currently are not
        assert!(
            result.contains("key1:    value1"),
            "Extra spaces after colon should be preserved"
        );
        assert!(
            result.contains("\n\n\nkey2:"),
            "Multiple newlines should be preserved"
        );
        assert!(
            result.contains("key2:        value2"),
            "Long spacing should be preserved"
        );
    }

    #[test]
    fn test_ast_preservation_complex_structure() {
        // Test preservation of complex structure with nested comments
        let yaml = r#"---
# Configuration file
database:  # Database settings
  host: localhost  # Default host
  port: 5432      # Default port
  # Connection pool settings  
  pool:
    min: 1    # Minimum connections
    max: 10   # Maximum connections

# Server configuration
server:
  port: 8080  # HTTP port
"#;
        let mut doc = Yaml::from_str(yaml).unwrap().document().unwrap();

        // Insert a new top-level key
        if let Some(mut mapping) = doc.as_mapping_mut() {
            mapping.insert_after_preserving("database", "logging", "debug");
        }

        let result = doc.to_string();

        // All these comments and structure should be preserved
        assert!(
            result.contains("# Configuration file"),
            "Top comment should be preserved"
        );
        assert!(
            result.contains("# Database settings"),
            "Section comment should be preserved"
        );
        assert!(
            result.contains("# Default host"),
            "Inline comment should be preserved"
        );
        assert!(
            result.contains("# Connection pool settings"),
            "Nested comment should be preserved"
        );
        assert!(
            result.contains("# Server configuration"),
            "Later section comment should be preserved"
        );
    }
}
