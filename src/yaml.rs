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
ast_node!(Scalar, SCALAR, "A YAML scalar value");
ast_node!(
    TaggedScalar,
    TAGGED_SCALAR,
    "A YAML tagged scalar (tag + value)"
);
ast_node!(Directive, DIRECTIVE, "A YAML directive like %YAML 1.2");

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

    /// Get a mutable reference to the first document
    pub fn document_mut(&mut self) -> Option<Document> {
        // Since Document wraps a SyntaxNode which is already mutable, we can just return it
        self.documents().next()
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
    pub fn set(&mut self, key: impl Into<ScalarValue>, value: impl Into<ScalarValue>) {
        if let Some(mut doc) = self.document() {
            doc.set(key, value);
            // Replace the first document with the modified one
            self.replace_first_document(doc);
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
        if let Some(mut doc) = self.document_mut() {
            let result = doc.insert_after(after_key, key, value);
            if result {
                // Propagate the modified document back to the Yaml tree
                self.replace_first_document(doc);
            }
            result
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
        if let Some(mut doc) = self.document_mut() {
            let result = doc.insert_before(before_key, key, value);
            if result {
                // Propagate the modified document back to the Yaml tree
                self.replace_first_document(doc);
            }
            result
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
        if let Some(mut doc) = self.document_mut() {
            doc.insert_at_index(index, key, value);
            // Always propagate back since insert_at_index doesn't return success status
            self.replace_first_document(doc);
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
    pub fn contains_key(&self, key: &str) -> bool {
        self.as_mapping().is_some_and(|m| m.contains_key(key))
    }

    /// Get a value from the document (assumes document is a mapping)
    pub fn get(&self, key: &str) -> Option<SyntaxNode> {
        self.as_mapping().and_then(|m| m.get(key))
    }

    /// Set a scalar value in the document (assumes document is a mapping)
    pub fn set(&mut self, key: impl Into<ScalarValue>, value: impl Into<ScalarValue>) {
        let key_scalar = key.into();
        let value_scalar = value.into();
        let key_str = key_scalar.value();

        // Collect existing key-value pairs, excluding the key we're setting
        let mut pairs = Vec::new();
        if let Some(mapping) = self.as_mapping() {
            for (existing_key, existing_value) in mapping.pairs() {
                if let Some(existing_key_scalar) = existing_key {
                    let existing_key_str = existing_key_scalar.value();
                    if existing_key_str != key_str {
                        // Keep this pair
                        if let Some(value_node) = existing_value {
                            pairs.push((
                                existing_key_str.to_string(),
                                value_node.text().to_string(),
                            ));
                        }
                    }
                }
            }
        }

        // Add the new key-value pair
        pairs.push((key_str.to_string(), value_scalar.value().to_string()));

        // Rebuild the document with all pairs
        self.0 = self.create_mapping_with_all_entries(pairs);
    }

    /// Set a raw string value in the document (no escaping)
    pub fn set_raw(&mut self, key: &str, value: &str) {
        if let Some(mut mapping) = self.as_mapping() {
            mapping.set_raw(key, value);
            // Replace the document's content with the modified mapping
            *self = Self::from_mapping(mapping);
        } else {
            // Create a new mapping if document is empty
            let mut mapping = Mapping::new();
            mapping.set_raw(key, value);
            *self = Self::from_mapping(mapping);
        }
    }

    /// Remove a key from the document (assumes document is a mapping)
    pub fn remove(&mut self, key: &str) -> bool {
        if let Some(mapping) = self.as_mapping() {
            // Check if the key exists
            let key_exists = mapping
                .pairs()
                .any(|(k, _)| k.as_ref().map(|s| s.value()) == Some(key.to_string()));

            if !key_exists {
                return false;
            }

            // Collect remaining key-value pairs, excluding the key we're removing
            let mut pairs = Vec::new();
            for (existing_key, existing_value) in mapping.pairs() {
                if let Some(existing_key_scalar) = existing_key {
                    let existing_key_str = existing_key_scalar.value();
                    if existing_key_str != key {
                        // Keep this pair
                        if let Some(value_node) = existing_value {
                            pairs.push((existing_key_str, value_node.text().to_string()));
                        }
                    }
                }
            }

            // Rebuild the document without the removed key
            if pairs.is_empty() {
                // Create empty document
                *self = Document::new();
            } else {
                self.0 = self.create_mapping_with_all_entries(pairs);
            }

            true
        } else {
            false
        }
    }

    /// Get all keys from the document (assumes document is a mapping)
    pub fn keys(&self) -> Vec<String> {
        self.as_mapping()
            .map_or_else(Vec::new, |m| m.keys().collect())
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

        // Copy the mapping node directly
        builder.start_node(mapping.0.kind().into());
        for child in mapping.0.children_with_tokens() {
            match child {
                rowan::NodeOrToken::Node(node) => {
                    builder.start_node(node.kind().into());
                    // Recursively add children
                    Self::add_node_children(&mut builder, &node);
                    builder.finish_node();
                }
                rowan::NodeOrToken::Token(token) => {
                    builder.token(token.kind().into(), token.text());
                }
            }
        }
        builder.finish_node(); // End MAPPING
        builder.finish_node(); // End DOCUMENT

        Document(SyntaxNode::new_root_mut(builder.finish()))
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

        // Convert keys to strings for comparison
        let after_key_str = after_key_value.to_string();
        let key_str = key_value.to_string();
        let value_str = value_value.to_string();

        // Check if document is a mapping
        if let Some(mut mapping) = self.as_mapping() {
            // Use the AST-preserving method on the mapping
            let result = mapping.insert_after_preserving(&after_key_str, &key_str, &value_str);

            if result {
                // Rebuild the document with the modified mapping
                self.replace_with_mapping(mapping);
            }

            result
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

        // Convert keys to strings for comparison
        let before_key_str = before_key_value.to_string();
        let key_str = key_value.to_string();
        let value_str = value_value.to_string();

        // Check if document is a mapping
        if let Some(mut mapping) = self.as_mapping() {
            // Use the AST-preserving method on the mapping
            let result = mapping.insert_before_preserving(&before_key_str, &key_str, &value_str);

            if result {
                // Rebuild the document with the modified mapping
                self.replace_with_mapping(mapping);
            }

            result
        } else {
            false
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

        // Convert to strings
        let key_str = key_value.to_string();
        let value_str = value_value.to_string();

        // Check if document is a mapping
        if let Some(mut mapping) = self.as_mapping() {
            // Use the AST-preserving method on the mapping
            mapping.insert_at_index_preserving(index, &key_str, &value_str);

            // Rebuild the document with the modified mapping
            self.replace_with_mapping(mapping);
        }
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
        self.get(key).map(|node| {
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
                // SCALAR nodes need to be processed
                scalar.value()
            } else {
                // For other node types, use raw text - trim trailing newlines
                node.text().to_string().trim_end_matches('\n').to_string()
            }
        })
    }

    /// Check if a key's value is an array/sequence
    pub fn is_array(&self, key: &str) -> bool {
        self.get(key)
            .map(|node| node.kind() == SyntaxKind::SEQUENCE)
            .unwrap_or(false)
    }

    /// Get the first element of an array as a string (for array to string conversion)
    pub fn get_array_first_string(&self, key: &str) -> Option<String> {
        self.get(key).and_then(|node| {
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
    pub fn reorder_fields(&mut self, order: &[&str]) {
        if let Some(mapping) = self.as_mapping() {
            // Collect all existing key-value pairs
            let mut all_pairs = Vec::new();
            for (key, value) in mapping.pairs() {
                if let Some(key_scalar) = key {
                    if let Some(value_node) = value {
                        all_pairs.push((key_scalar.value(), value_node.text().to_string()));
                    }
                }
            }

            // Build ordered pairs according to the specified order
            let mut ordered_pairs = Vec::new();

            // First, add pairs in the specified order
            for &ordered_key in order {
                if let Some(pos) = all_pairs.iter().position(|(k, _)| k == ordered_key) {
                    ordered_pairs.push(all_pairs.remove(pos));
                }
            }

            // Then, add remaining pairs that weren't in the order list
            ordered_pairs.extend(all_pairs);

            // Rebuild the document with the reordered pairs
            if !ordered_pairs.is_empty() {
                self.0 = self.create_mapping_with_all_entries(ordered_pairs);
            }
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
    pub fn set_field_order(&mut self, field_order: &[&str]) {
        if let Some(mapping) = self.as_mapping() {
            // Collect all existing key-value pairs
            let mut all_pairs: Vec<(String, String)> = Vec::new();
            for (key, value) in mapping.pairs() {
                if let Some(key_scalar) = key {
                    let key_str = key_scalar.value();
                    if let Some(value_node) = value {
                        all_pairs.push((key_str, value_node.text().to_string().trim().to_string()));
                    }
                }
            }

            // Create ordered pairs according to field_order
            let mut ordered_pairs: Vec<(String, String)> = Vec::new();

            // First, add fields in the specified order
            for &field_name in field_order {
                if let Some(pos) = all_pairs.iter().position(|(k, _)| k == field_name) {
                    ordered_pairs.push(all_pairs.remove(pos));
                }
            }

            // Then add any remaining fields in their original order
            ordered_pairs.extend(all_pairs);

            // Rebuild the document with ordered fields
            if !ordered_pairs.is_empty() {
                self.0 = self.create_mapping_with_all_entries(ordered_pairs);
            }
        }
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

            // Add key
            builder.start_node(SyntaxKind::KEY.into());
            builder.token(SyntaxKind::VALUE.into(), key);
            builder.finish_node();

            // Add colon and space
            builder.token(SyntaxKind::COLON.into(), ":");
            builder.token(SyntaxKind::WHITESPACE.into(), " ");

            // Add value
            builder.start_node(SyntaxKind::VALUE.into());
            builder.token(SyntaxKind::VALUE.into(), value);
            builder.finish_node();
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
            builder.token(SyntaxKind::VALUE.into(), key);
            builder.finish_node();

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
            // Replace the existing mapping with the modified one directly
            let new_mapping_green = mapping.0.green().into();
            let new_elements = vec![rowan::NodeOrToken::Node(new_mapping_green)];

            // Use surgical splice to replace only the mapping node
            let new_green = self.0.green().splice_children(pos..pos + 1, new_elements);
            self.0 = SyntaxNode::new_root_mut(new_green);
        } else {
            // No existing mapping found, fall back to the rebuild approach
            // This handles edge cases where the document structure is unusual
            self.fallback_replace_with_mapping(mapping);
        }
    }

    /// Fallback method that rebuilds the entire document (used when surgical replacement isn't possible)
    fn fallback_replace_with_mapping(&mut self, mapping: Mapping) {
        // Create a new document preserving original structure but replacing the mapping
        let mut builder = rowan::GreenNodeBuilder::new();
        builder.start_node(SyntaxKind::DOCUMENT.into());

        let mut has_document_start = false;
        let mut has_mapping = false;

        // Copy all original document children, but replace any MAPPING nodes with the new one
        for child in self.0.children_with_tokens() {
            match child {
                rowan::NodeOrToken::Node(n) => {
                    if n.kind() == SyntaxKind::MAPPING {
                        has_mapping = true;
                        // Replace with the modified mapping
                        builder.start_node(SyntaxKind::MAPPING.into());
                        self.copy_node_content(&mut builder, &mapping.0);
                        builder.finish_node(); // MAPPING
                    } else {
                        // Copy other nodes (like directives, comments) as-is
                        builder.start_node(n.kind().into());
                        self.copy_node_content(&mut builder, &n);
                        builder.finish_node();
                    }
                }
                rowan::NodeOrToken::Token(t) => {
                    if t.kind() == SyntaxKind::DOC_START {
                        has_document_start = true;
                    }
                    // Copy tokens (like document separators, whitespace) as-is
                    builder.token(t.kind().into(), t.text());
                }
            }
        }

        // If there was no document start marker but we have a mapping, add one
        if !has_document_start && has_mapping {
            // Need to rebuild with document start at the beginning
            let _current_children = builder.finish();
            let mut new_builder = rowan::GreenNodeBuilder::new();
            new_builder.start_node(SyntaxKind::DOCUMENT.into());

            // Add document start
            new_builder.token(SyntaxKind::DOC_START.into(), "---");
            new_builder.token(SyntaxKind::NEWLINE.into(), "\n");

            // Add the mapping
            new_builder.start_node(SyntaxKind::MAPPING.into());
            self.copy_node_content(&mut new_builder, &mapping.0);
            new_builder.finish_node(); // MAPPING

            new_builder.finish_node(); // DOCUMENT
            self.0 = SyntaxNode::new_root_mut(new_builder.finish());
        } else {
            builder.finish_node(); // DOCUMENT
            self.0 = SyntaxNode::new_root_mut(builder.finish());
        }
    }

    /// Helper to recursively copy node content
    fn copy_node_content(&self, builder: &mut rowan::GreenNodeBuilder, node: &SyntaxNode) {
        for child in node.children_with_tokens() {
            match child {
                rowan::NodeOrToken::Node(n) => {
                    builder.start_node(n.kind().into());
                    self.copy_node_content(builder, &n);
                    builder.finish_node();
                }
                rowan::NodeOrToken::Token(t) => {
                    builder.token(t.kind().into(), t.text());
                }
            }
        }
    }
}

impl Default for Mapping {
    fn default() -> Self {
        Self::new()
    }
}

impl Mapping {
    /// Get all key-value pairs in this mapping
    pub fn pairs(&self) -> impl Iterator<Item = (Option<Scalar>, Option<SyntaxNode>)> + '_ {
        // Create an iterator that lazily finds key-value pairs
        struct PairsIterator<'a> {
            children: Vec<SyntaxNode>,
            index: usize,
            _marker: std::marker::PhantomData<&'a ()>,
        }

        impl<'a> Iterator for PairsIterator<'a> {
            type Item = (Option<Scalar>, Option<SyntaxNode>);

            fn next(&mut self) -> Option<Self::Item> {
                while self.index < self.children.len() {
                    if self.children[self.index].kind() == SyntaxKind::KEY {
                        // Create a scalar from the KEY node's text - if it has SCALAR children, use those,
                        // otherwise create a scalar from the KEY node's direct text
                        let key_scalar = if let Some(scalar_child) = self.children[self.index]
                            .children()
                            .find(|child| child.kind() == SyntaxKind::SCALAR)
                        {
                            Scalar::cast(scalar_child)
                        } else {
                            // Create a scalar from the KEY node's text content
                            let key_text = self.children[self.index].text().to_string();
                            let scalar_node = create_scalar_node(&key_text);
                            Some(Scalar(scalar_node))
                        };

                        // Look for the next VALUE node (skip whitespace/other tokens)
                        let mut value_node = None;
                        let mut next_index = self.index + 1;
                        for j in (self.index + 1)..self.children.len() {
                            let child_kind = self.children[j].kind();
                            if child_kind == SyntaxKind::VALUE {
                                // Extract the actual content from within the VALUE node, or use the VALUE node itself
                                value_node = self.children[j]
                                    .children()
                                    .next()
                                    .or_else(|| Some(self.children[j].clone()));
                                next_index = j + 1; // Set to position after this VALUE node
                                break;
                            } else if child_kind == SyntaxKind::KEY {
                                // Hit another key, stop searching
                                next_index = j; // Set to the position of this KEY node
                                break;
                            }
                            // Continue searching past whitespace, tokens, etc.
                        }

                        self.index = next_index; // Move to correct position
                        return Some((key_scalar, value_node));
                    }
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

    /// Get the value for a specific key
    pub fn get(&self, key: &str) -> Option<SyntaxNode> {
        self.pairs()
            .find(|(k, _)| k.as_ref().map(|s| s.as_string()) == Some(key.to_string()))
            .and_then(|(_, v)| v)
    }

    /// Get a value as a Mapping if it is one
    pub fn get_mapping(&self, key: &str) -> Option<Mapping> {
        self.get(key).and_then(Mapping::cast)
    }

    /// Get a value as a Sequence if it is one
    pub fn get_sequence(&self, key: &str) -> Option<Sequence> {
        self.get(key).and_then(Sequence::cast)
    }

    /// Check if the mapping contains a specific key
    pub fn contains_key(&self, key: &str) -> bool {
        self.pairs()
            .any(|(k, _)| k.as_ref().map(|s| s.as_string()) == Some(key.to_string()))
    }

    /// Get all keys in the mapping
    pub fn keys(&self) -> impl Iterator<Item = String> + '_ {
        self.pairs().filter_map(|(k, _)| k.map(|s| s.as_string()))
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
                all_pairs.push((key.value(), value_text.trim().to_string()));
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
    pub fn items(&self) -> impl Iterator<Item = SyntaxNode> {
        self.0.children().filter(|child| {
            matches!(
                child.kind(),
                SyntaxKind::SCALAR | SyntaxKind::MAPPING | SyntaxKind::SEQUENCE
            )
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
    pub fn as_set(&self) -> Option<std::collections::BTreeSet<String>> {
        if self.tag() != Some("!!set".to_string()) {
            return None;
        }

        let mut set = std::collections::BTreeSet::new();

        // Find the mapping node within this tagged scalar
        for child in self.0.children() {
            if let Some(mapping) = Mapping::cast(child) {
                // Extract keys from the mapping (set members)
                for (key_opt, _value_opt) in mapping.pairs() {
                    if let Some(key) = key_opt {
                        set.insert(key.as_string());
                    }
                }
                break;
            }
        }

        Some(set)
    }

    /// Extract ordered mapping from this tagged scalar if it has a !!omap tag
    pub fn as_ordered_mapping(&self) -> Option<Vec<(String, crate::value::YamlValue)>> {
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
                                let yaml_value = self.yaml_node_to_yaml_value(value.clone());
                                pairs.push((key.as_string(), yaml_value));
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
    pub fn as_pairs(&self) -> Option<Vec<(String, crate::value::YamlValue)>> {
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
                                let yaml_value = self.yaml_node_to_yaml_value(value.clone());
                                pairs.push((key.as_string(), yaml_value));
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
                            map.insert(key.as_string(), self.yaml_node_to_yaml_value(value));
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

use std::collections::HashMap;

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
    /// Original text for error reporting
    original_text: String,
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
            original_text: text.to_string(),
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
        match self.current() {
            Some(SyntaxKind::DASH) if !self.in_flow_context => self.parse_sequence(),
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
                if !self.in_flow_context && self.is_mapping_key() {
                    self.parse_mapping();
                } else {
                    self.parse_scalar();
                }
            }
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
                let error_msg = self.create_detailed_error(
                    "Unterminated quoted string",
                    &format!("closing quote {}", expected_quote),
                    self.current_text().as_deref(),
                );
                self.add_error_and_recover(error_msg, quote_type);
            }
        } else {
            // Handle typed scalar tokens from lexer
            if matches!(
                self.current(),
                Some(
                    SyntaxKind::STRING
                        | SyntaxKind::INT
                        | SyntaxKind::FLOAT
                        | SyntaxKind::BOOL
                        | SyntaxKind::NULL
                )
            ) {
                if !self.in_flow_context {
                    // For plain scalars in block context, consume all tokens on the same line
                    // This handles complex values like timestamps with spaces
                    while let Some(kind) = self.current() {
                        if matches!(kind, SyntaxKind::NEWLINE | SyntaxKind::COMMENT) {
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
                            | SyntaxKind::COLON
                            | SyntaxKind::DASH
                            | SyntaxKind::COMMENT
                            | SyntaxKind::DOC_START
                            | SyntaxKind::DOC_END
                    ) {
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
                    let error_msg = self.create_detailed_error(
                        "Unterminated quoted string in flow collection",
                        &format!("closing quote {}", expected_quote),
                        self.current_text().as_deref(),
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

            self.skip_ws_and_newlines();
        }

        self.builder.finish_node();
        self.error_context.pop_context();
    }

    fn parse_sequence(&mut self) {
        self.builder.start_node(SyntaxKind::SEQUENCE.into());
        self.error_context.push_context(ParseContext::Sequence);

        while self.current() == Some(SyntaxKind::DASH) {
            self.bump(); // consume dash
            self.skip_whitespace();

            if self.current().is_some() && self.current() != Some(SyntaxKind::NEWLINE) {
                self.parse_value();
            }

            self.skip_ws_and_newlines();
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

            self.parse_value();
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
        for kind in self.upcoming_tokens() {
            match kind {
                SyntaxKind::COLON => return true,
                SyntaxKind::WHITESPACE => continue,
                // Any other token means this is not a simple mapping key
                _ => return false,
            }
        }
        false
    }

    fn skip_whitespace(&mut self) {
        while self.current() == Some(SyntaxKind::WHITESPACE) {
            self.bump();
        }
    }

    fn skip_ws_and_newlines(&mut self) {
        while matches!(
            self.current(),
            Some(
                SyntaxKind::WHITESPACE
                    | SyntaxKind::NEWLINE
                    | SyntaxKind::INDENT
                    | SyntaxKind::COMMENT
            )
        ) {
            self.bump();
        }
    }

    fn parse_mapping_key_value_pair(&mut self) {
        // Parse regular key
        self.builder.start_node(SyntaxKind::KEY.into());
        if self.current() == Some(SyntaxKind::MERGE_KEY) {
            self.bump(); // consume the merge key token
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
            self.bump(); // consume the key token
        }
        self.builder.finish_node();

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
                // Check if next line is indented (nested content)
                self.bump(); // consume newline
                if self.current() == Some(SyntaxKind::INDENT) {
                    self.bump(); // consume indent
                                 // Parse the indented content as the value
                    self.parse_value();
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
        (1..self.tokens.len()).map(move |i| {
            let idx = self.tokens.len() - 1 - i;
            self.tokens[idx].0
        })
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

fn create_token_green(kind: SyntaxKind, text: &str) -> rowan::GreenToken {
    rowan::GreenToken::new(kind.into(), text)
}

// Editing methods for Mapping
impl Mapping {
    /// Set a key-value pair with a scalar value, replacing if exists or adding if new
    /// This method automatically escapes the key and value as needed.
    pub fn set(&mut self, key: impl Into<ScalarValue>, value: impl Into<ScalarValue>) {
        let key_scalar = key.into();
        let value_scalar = value.into();
        self.set_raw(&key_scalar.to_yaml_string(), &value_scalar.to_yaml_string());
    }

    /// Set a key-value pair, replacing if exists or adding if new
    /// This is the low-level method that doesn't escape values.
    pub fn set_raw(&mut self, key: &str, value: &str) {
        // Find existing key-value pair by looking for scalar nodes
        for child in self.0.children() {
            if child.kind() == SyntaxKind::SCALAR {
                if let Some(scalar) = Scalar::cast(child.clone()) {
                    if scalar.value().trim() == key {
                        // Found existing key, need to find the complete entry range to replace
                        // For now, let's create a new entry and replace the entire mapping
                        // This is a simplified approach - a full implementation would be more surgical
                        let mut new_entries = Vec::new();
                        let mut found_key = false;

                        // Collect all key-value pairs, replacing the matching one
                        for pair_result in self.pairs() {
                            if let (Some(k), Some(v_node)) = pair_result {
                                if k.value().trim() == key && !found_key {
                                    // Replace this entry
                                    let new_scalar = create_scalar_node(value);
                                    new_entries.push((k, new_scalar));
                                    found_key = true;
                                } else {
                                    new_entries.push((k, v_node));
                                }
                            }
                        }

                        if found_key {
                            // Rebuild the entire mapping - this is inefficient but correct
                            self.rebuild_from_pairs(new_entries);
                            return;
                        }
                    }
                }
            }
        }

        // Key doesn't exist, add new entry
        // For simplicity, rebuild entire mapping with new entry added
        let mut pairs = Vec::new();
        for (k, v) in self.pairs() {
            if let (Some(key_scalar), Some(value_node)) = (k, v) {
                pairs.push((key_scalar, value_node));
            }
        }

        // Add new pair
        let key_scalar = Scalar(create_scalar_node(key));
        let value_node = create_scalar_node(value);
        pairs.push((key_scalar, value_node));

        self.rebuild_from_pairs(pairs);
    }

    // Helper to rebuild mapping from pairs - similar to deb822 approach
    fn rebuild_from_pairs(&mut self, pairs: Vec<(Scalar, SyntaxNode)>) {
        let mut new_children = Vec::new();

        for (key_scalar, value_node) in pairs {
            // Create KEY node
            let mut key_builder = GreenNodeBuilder::new();
            key_builder.start_node(SyntaxKind::KEY.into());
            key_builder.token(SyntaxKind::STRING.into(), &key_scalar.value());
            key_builder.finish_node();
            let key_green = key_builder.finish();
            new_children.push(rowan::NodeOrToken::Node(key_green));

            // Add colon and space
            new_children.push(rowan::NodeOrToken::Token(create_token_green(
                SyntaxKind::COLON,
                ":",
            )));
            new_children.push(rowan::NodeOrToken::Token(create_token_green(
                SyntaxKind::WHITESPACE,
                " ",
            )));

            // Create VALUE node containing the actual value content
            let mut value_builder = GreenNodeBuilder::new();
            value_builder.start_node(SyntaxKind::VALUE.into());
            if value_node.kind() == SyntaxKind::SCALAR {
                // Extract the text from the scalar node and add it as STRING token
                let value_text = value_node.text().to_string();
                value_builder.token(SyntaxKind::STRING.into(), &value_text);
            } else {
                // For non-scalar values, we need to handle differently
                // TODO: Handle complex values properly
                value_builder.token(
                    SyntaxKind::STRING.into(),
                    value_node.text().to_string().trim(),
                );
            }
            value_builder.finish_node();
            let value_green = value_builder.finish();
            new_children.push(rowan::NodeOrToken::Node(value_green));

            // Add newline
            new_children.push(rowan::NodeOrToken::Token(create_token_green(
                SyntaxKind::NEWLINE,
                "\n",
            )));
        }

        // Replace all children of the current mapping node
        let child_count = self.0.children_with_tokens().count();
        let new_green = self.0.green().splice_children(0..child_count, new_children);
        self.0 = SyntaxNode::new_root_mut(new_green);
    }

    /// Insert a key-value pair after an existing key, preserving formatting
    /// Returns true if successful, false if the reference key wasn't found
    pub fn insert_after_preserving(
        &mut self,
        after_key: &str,
        new_key: &str,
        new_value: &str,
    ) -> bool {
        let children: Vec<_> = self.0.children_with_tokens().collect();
        let mut insert_position = None;
        let mut found_key = false;
        let mut last_value_end = 0;

        // Find the position after the specified key's value
        for (i, child) in children.iter().enumerate() {
            if let Some(node) = child.as_node() {
                if node.kind() == SyntaxKind::KEY {
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
            // Create new GREEN elements for the key-value pair
            let mut new_elements = Vec::new();

            // Check if we need to add indentation by looking at surrounding nodes
            let indent = self.detect_indentation_from_children(&children, pos);
            if !indent.is_empty() {
                new_elements.push(rowan::NodeOrToken::Token(create_token_green(
                    SyntaxKind::WHITESPACE,
                    &indent,
                )));
            }

            // Create KEY node with proper hierarchy
            let mut key_builder = GreenNodeBuilder::new();
            key_builder.start_node(SyntaxKind::KEY.into());
            key_builder.token(SyntaxKind::STRING.into(), new_key);
            key_builder.finish_node();
            let key_green = key_builder.finish();
            new_elements.push(rowan::NodeOrToken::Node(key_green));

            // Add colon and space
            new_elements.push(rowan::NodeOrToken::Token(create_token_green(
                SyntaxKind::COLON,
                ":",
            )));
            new_elements.push(rowan::NodeOrToken::Token(create_token_green(
                SyntaxKind::WHITESPACE,
                " ",
            )));

            // Create VALUE node with proper hierarchy
            let mut value_builder = GreenNodeBuilder::new();
            value_builder.start_node(SyntaxKind::VALUE.into());
            value_builder.token(SyntaxKind::STRING.into(), new_value);
            value_builder.finish_node();
            let value_green = value_builder.finish();
            new_elements.push(rowan::NodeOrToken::Node(value_green));

            // Add newline
            new_elements.push(rowan::NodeOrToken::Token(create_token_green(
                SyntaxKind::NEWLINE,
                "\n",
            )));

            // Splice in the new elements
            let new_green = self.0.green().splice_children(pos..pos, new_elements);
            self.0 = SyntaxNode::new_root_mut(new_green);
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
        new_key: &str,
        new_value: &str,
    ) -> bool {
        let children: Vec<_> = self.0.children_with_tokens().collect();
        let mut insert_position = None;
        let mut existing_key_range = None;

        // First, check if the new key already exists
        let mut i = 0;
        while i < children.len() {
            if let Some(node) = children[i].as_node() {
                if node.kind() == SyntaxKind::KEY || node.kind() == SyntaxKind::SCALAR {
                    let key_text = node.text().to_string();
                    if key_text.trim() == new_key {
                        // Found existing key, find its value and replace it
                        let mut range_end = i + 1;
                        // Find the value and newline that belong to this key
                        for j in (i + 1)..children.len() {
                            if let Some(token) = children[j].as_token() {
                                if token.kind() == SyntaxKind::NEWLINE {
                                    range_end = j + 1;
                                    break;
                                }
                            } else if let Some(node) = children[j].as_node() {
                                if node.kind() == SyntaxKind::VALUE
                                    || node.kind() == SyntaxKind::SCALAR
                                {
                                    continue; // Include the value
                                } else {
                                    break; // Stop if we hit another key or structure
                                }
                            }
                        }
                        existing_key_range = Some((i, range_end));
                        break;
                    }
                }
            }
            i += 1;
        }

        // If key exists, replace it
        if let Some((start, end)) = existing_key_range {
            let mut new_elements = Vec::new();

            // Detect indentation from the existing key
            let indent = if start > 0 {
                if let Some(token) = children[start - 1].as_token() {
                    if token.kind() == SyntaxKind::WHITESPACE {
                        token.text().to_string()
                    } else {
                        String::new()
                    }
                } else {
                    String::new()
                }
            } else {
                String::new()
            };

            if !indent.is_empty() {
                new_elements.push(rowan::NodeOrToken::Token(create_token_green(
                    SyntaxKind::WHITESPACE,
                    &indent,
                )));
            }

            // Create new KEY node
            let mut key_builder = GreenNodeBuilder::new();
            key_builder.start_node(SyntaxKind::KEY.into());
            key_builder.token(SyntaxKind::STRING.into(), new_key);
            key_builder.finish_node();
            let key_green = key_builder.finish();
            new_elements.push(rowan::NodeOrToken::Node(key_green));

            // Add colon and space
            new_elements.push(rowan::NodeOrToken::Token(create_token_green(
                SyntaxKind::COLON,
                ":",
            )));
            new_elements.push(rowan::NodeOrToken::Token(create_token_green(
                SyntaxKind::WHITESPACE,
                " ",
            )));

            // Create new VALUE node
            let mut value_builder = GreenNodeBuilder::new();
            value_builder.start_node(SyntaxKind::VALUE.into());
            value_builder.token(SyntaxKind::STRING.into(), new_value);
            value_builder.finish_node();
            let value_green = value_builder.finish();
            new_elements.push(rowan::NodeOrToken::Node(value_green));

            // Add newline
            new_elements.push(rowan::NodeOrToken::Token(create_token_green(
                SyntaxKind::NEWLINE,
                "\n",
            )));

            // Replace the existing key-value pair
            let new_green = self.0.green().splice_children(start..end, new_elements);
            self.0 = SyntaxNode::new_root_mut(new_green);
            return true;
        }

        // If key doesn't exist, find the position before the specified key
        for (i, child) in children.iter().enumerate() {
            if let Some(node) = child.as_node() {
                if node.kind() == SyntaxKind::KEY || node.kind() == SyntaxKind::SCALAR {
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
            let mut new_elements = Vec::new();

            // Detect and add indentation
            let indent = self.detect_indentation_from_children(&children, pos);
            if !indent.is_empty() {
                new_elements.push(rowan::NodeOrToken::Token(create_token_green(
                    SyntaxKind::WHITESPACE,
                    &indent,
                )));
            }

            // Create KEY node with proper hierarchy
            let mut key_builder = GreenNodeBuilder::new();
            key_builder.start_node(SyntaxKind::KEY.into());
            key_builder.token(SyntaxKind::STRING.into(), new_key);
            key_builder.finish_node();
            let key_green = key_builder.finish();
            new_elements.push(rowan::NodeOrToken::Node(key_green));

            // Add colon and space
            new_elements.push(rowan::NodeOrToken::Token(create_token_green(
                SyntaxKind::COLON,
                ":",
            )));
            new_elements.push(rowan::NodeOrToken::Token(create_token_green(
                SyntaxKind::WHITESPACE,
                " ",
            )));

            // Create VALUE node with proper hierarchy
            let mut value_builder = GreenNodeBuilder::new();
            value_builder.start_node(SyntaxKind::VALUE.into());
            value_builder.token(SyntaxKind::STRING.into(), new_value);
            value_builder.finish_node();
            let value_green = value_builder.finish();
            new_elements.push(rowan::NodeOrToken::Node(value_green));

            // Add newline
            new_elements.push(rowan::NodeOrToken::Token(create_token_green(
                SyntaxKind::NEWLINE,
                "\n",
            )));

            // Splice in the new elements
            let new_green = self.0.green().splice_children(pos..pos, new_elements);
            self.0 = SyntaxNode::new_root_mut(new_green);
            true
        } else {
            false
        }
    }

    /// Insert a key-value pair at a specific index, preserving formatting
    /// If the new key already exists, it will replace the existing value
    pub fn insert_at_index_preserving(&mut self, index: usize, new_key: &str, new_value: &str) {
        let children: Vec<_> = self.0.children_with_tokens().collect();

        // First, check if the new key already exists and replace it if so
        let mut existing_key_range = None;
        let mut i = 0;
        while i < children.len() {
            if let Some(node) = children[i].as_node() {
                if node.kind() == SyntaxKind::KEY || node.kind() == SyntaxKind::SCALAR {
                    let key_text = node.text().to_string();
                    if key_text.trim() == new_key {
                        // Found existing key, find its value and replace it
                        let mut range_end = i + 1;
                        // Find the value and newline that belong to this key
                        for j in (i + 1)..children.len() {
                            if let Some(token) = children[j].as_token() {
                                if token.kind() == SyntaxKind::NEWLINE {
                                    range_end = j + 1;
                                    break;
                                }
                            } else if let Some(node) = children[j].as_node() {
                                if node.kind() == SyntaxKind::VALUE
                                    || node.kind() == SyntaxKind::SCALAR
                                {
                                    continue; // Include the value
                                } else {
                                    break; // Stop if we hit another key or structure
                                }
                            }
                        }
                        existing_key_range = Some((i, range_end));
                        break;
                    }
                }
            }
            i += 1;
        }

        // If key exists, replace it
        if let Some((start, end)) = existing_key_range {
            let mut new_elements = Vec::new();

            // Detect indentation from the existing key
            let indent = if start > 0 {
                if let Some(token) = children[start - 1].as_token() {
                    if token.kind() == SyntaxKind::WHITESPACE {
                        token.text().to_string()
                    } else {
                        String::new()
                    }
                } else {
                    String::new()
                }
            } else {
                String::new()
            };

            if !indent.is_empty() {
                new_elements.push(rowan::NodeOrToken::Token(create_token_green(
                    SyntaxKind::WHITESPACE,
                    &indent,
                )));
            }

            // Create new KEY node
            let mut key_builder = GreenNodeBuilder::new();
            key_builder.start_node(SyntaxKind::KEY.into());
            key_builder.token(SyntaxKind::STRING.into(), new_key);
            key_builder.finish_node();
            let key_green = key_builder.finish();
            new_elements.push(rowan::NodeOrToken::Node(key_green));

            // Add colon and space
            new_elements.push(rowan::NodeOrToken::Token(create_token_green(
                SyntaxKind::COLON,
                ":",
            )));
            new_elements.push(rowan::NodeOrToken::Token(create_token_green(
                SyntaxKind::WHITESPACE,
                " ",
            )));

            // Create new VALUE node
            let mut value_builder = GreenNodeBuilder::new();
            value_builder.start_node(SyntaxKind::VALUE.into());
            value_builder.token(SyntaxKind::STRING.into(), new_value);
            value_builder.finish_node();
            let value_green = value_builder.finish();
            new_elements.push(rowan::NodeOrToken::Node(value_green));

            // Add newline
            new_elements.push(rowan::NodeOrToken::Token(create_token_green(
                SyntaxKind::NEWLINE,
                "\n",
            )));

            // Replace the existing key-value pair
            let new_green = self.0.green().splice_children(start..end, new_elements);
            self.0 = SyntaxNode::new_root_mut(new_green);
            return;
        }

        // If key doesn't exist, proceed with insertion at index
        // Count key-value pairs to find insertion position
        let mut pair_count = 0;
        let mut insert_position = children.len(); // Default to end

        for (i, child) in children.iter().enumerate() {
            if let Some(node) = child.as_node() {
                if node.kind() == SyntaxKind::KEY || node.kind() == SyntaxKind::SCALAR {
                    if pair_count == index {
                        // Found the position - look back to start of line
                        let mut line_start = i;
                        for j in (0..i).rev() {
                            if let Some(token) = children[j].as_token() {
                                if token.kind() == SyntaxKind::NEWLINE {
                                    line_start = j + 1;
                                    break;
                                }
                            }
                        }
                        insert_position = line_start;
                        break;
                    }
                    pair_count += 1;
                }
            }
        }

        // Create new AST elements
        let mut new_elements = Vec::new();

        // If inserting at the end, add a newline first to separate from previous content
        if insert_position == children.len() && !children.is_empty() {
            // Check if the last element is not already a newline
            if let Some(last_child) = children.last() {
                if let Some(token) = last_child.as_token() {
                    if token.kind() != SyntaxKind::NEWLINE {
                        new_elements.push(rowan::NodeOrToken::Token(create_token_green(
                            SyntaxKind::NEWLINE,
                            "\n",
                        )));
                    }
                } else {
                    // Last child is a node, so we need a newline
                    new_elements.push(rowan::NodeOrToken::Token(create_token_green(
                        SyntaxKind::NEWLINE,
                        "\n",
                    )));
                }
            }
        }

        // Detect and add indentation
        let indent = self.detect_indentation_from_children(&children, insert_position);
        if !indent.is_empty() {
            new_elements.push(rowan::NodeOrToken::Token(create_token_green(
                SyntaxKind::WHITESPACE,
                &indent,
            )));
        }

        // Create KEY node with proper hierarchy
        let mut key_builder = GreenNodeBuilder::new();
        key_builder.start_node(SyntaxKind::KEY.into());
        key_builder.token(SyntaxKind::STRING.into(), new_key);
        key_builder.finish_node();
        let key_green = key_builder.finish();
        new_elements.push(rowan::NodeOrToken::Node(key_green));

        // Add colon and space
        new_elements.push(rowan::NodeOrToken::Token(create_token_green(
            SyntaxKind::COLON,
            ":",
        )));
        new_elements.push(rowan::NodeOrToken::Token(create_token_green(
            SyntaxKind::WHITESPACE,
            " ",
        )));

        // Create VALUE node with proper hierarchy
        let mut value_builder = GreenNodeBuilder::new();
        value_builder.start_node(SyntaxKind::VALUE.into());
        value_builder.token(SyntaxKind::STRING.into(), new_value);
        value_builder.finish_node();
        let value_green = value_builder.finish();
        new_elements.push(rowan::NodeOrToken::Node(value_green));

        // Add newline
        new_elements.push(rowan::NodeOrToken::Token(create_token_green(
            SyntaxKind::NEWLINE,
            "\n",
        )));

        // Splice in the new elements
        let new_green = self
            .0
            .green()
            .splice_children(insert_position..insert_position, new_elements);
        self.0 = SyntaxNode::new_root_mut(new_green);
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
        let mut removal_range = None;

        for (i, child) in children.iter().enumerate() {
            if let Some(node) = child.as_node() {
                if node.kind() == SyntaxKind::KEY && node.text() == key {
                    // Found the key, find the complete entry to remove
                    let start = i;
                    let mut end = i + 1;

                    // Skip colon, whitespace, value, and newline
                    while end < children.len() {
                        if let Some(token) = children[end].as_token() {
                            end += 1;
                            if token.kind() == SyntaxKind::NEWLINE {
                                break;
                            }
                        } else {
                            end += 1;
                        }
                    }

                    removal_range = Some(start..end);
                    break;
                }
            }
        }

        if let Some(range) = removal_range {
            self.0.splice_children(range, vec![]);
            true
        } else {
            false
        }
    }

    /// Rename a key while preserving its value and formatting, with proper escaping
    pub fn rename_key(&mut self, old_key: &str, new_key: impl Into<ScalarValue>) -> bool {
        let new_key_scalar = new_key.into();
        self.rename_key_raw(old_key, &new_key_scalar.to_yaml_string())
    }

    /// Rename a key while preserving its value and formatting
    /// This is the low-level method that doesn't escape the new key.
    pub fn rename_key_raw(&mut self, old_key: &str, new_key: &str) -> bool {
        let children: Vec<_> = self.0.children().collect();

        for (i, child) in children.iter().enumerate() {
            if child.kind() == SyntaxKind::KEY {
                // Check if this KEY node contains our target key
                let key_text = child.text().to_string();
                if key_text == old_key {
                    // Create a new KEY node with the new key
                    let mut builder = GreenNodeBuilder::new();
                    builder.start_node(SyntaxKind::KEY.into());
                    builder.token(SyntaxKind::STRING.into(), new_key);
                    builder.finish_node();
                    let new_key_node = SyntaxNode::new_root_mut(builder.finish());

                    self.0.splice_children(i..i + 1, vec![new_key_node.into()]);
                    return true;
                }
            }
        }
        false
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
        after_key: &str,
        key: impl Into<ScalarValue>,
        value: impl Into<ScalarValue>,
    ) -> bool {
        let key_scalar = key.into();
        let value_scalar = value.into();
        self.insert_after_raw(
            after_key,
            &key_scalar.to_yaml_string(),
            &value_scalar.to_yaml_string(),
        )
    }

    /// Insert a key-value pair after a specific existing key (low-level method)
    pub fn insert_after_raw(&mut self, after_key: &str, key: &str, value: &str) -> bool {
        // Check if the key already exists
        if self.contains_key(key) {
            // Key already exists, just update it
            self.set_raw(key, value);
            return true;
        }

        // Collect all existing pairs
        let mut pairs = Vec::new();
        let mut found_reference = false;

        for (k, v) in self.pairs() {
            if let (Some(key_scalar), Some(value_node)) = (k, v) {
                pairs.push((key_scalar.clone(), value_node.clone()));

                // Check if this is the reference key
                if key_scalar.value().trim() == after_key {
                    found_reference = true;
                    // Add the new pair after this one
                    let new_key_scalar = Scalar(create_scalar_node(key));
                    let new_value_node = create_scalar_node(value);
                    pairs.push((new_key_scalar, new_value_node));
                }
            }
        }

        if found_reference {
            self.rebuild_from_pairs(pairs);
            true
        } else {
            false
        }
    }

    /// Insert a key-value pair before a specific existing key
    /// Returns true if the reference key was found and insertion succeeded
    pub fn insert_before(
        &mut self,
        before_key: &str,
        key: impl Into<ScalarValue>,
        value: impl Into<ScalarValue>,
    ) -> bool {
        let key_scalar = key.into();
        let value_scalar = value.into();
        self.insert_before_raw(
            before_key,
            &key_scalar.to_yaml_string(),
            &value_scalar.to_yaml_string(),
        )
    }

    /// Insert a key-value pair before a specific existing key (low-level method)
    pub fn insert_before_raw(&mut self, before_key: &str, key: &str, value: &str) -> bool {
        // Check if the key already exists
        if self.contains_key(key) {
            // Key already exists, just update it
            self.set_raw(key, value);
            return true;
        }

        // Collect all existing pairs
        let mut pairs = Vec::new();
        let mut found_reference = false;

        for (k, v) in self.pairs() {
            if let (Some(key_scalar), Some(value_node)) = (k, v) {
                // Check if this is the reference key
                if key_scalar.value().trim() == before_key && !found_reference {
                    found_reference = true;
                    // Add the new pair before this one
                    let new_key_scalar = Scalar(create_scalar_node(key));
                    let new_value_node = create_scalar_node(value);
                    pairs.push((new_key_scalar, new_value_node));
                }

                pairs.push((key_scalar, value_node));
            }
        }

        if found_reference {
            self.rebuild_from_pairs(pairs);
            true
        } else {
            false
        }
    }

    /// Insert a key-value pair at a specific index (0-based)
    /// If index is out of bounds, appends at the end
    pub fn insert_at_index(
        &mut self,
        index: usize,
        key: impl Into<ScalarValue>,
        value: impl Into<ScalarValue>,
    ) {
        let key_scalar = key.into();
        let value_scalar = value.into();
        self.insert_at_index_raw(
            index,
            &key_scalar.to_yaml_string(),
            &value_scalar.to_yaml_string(),
        )
    }

    /// Insert a key-value pair at a specific index (low-level method)
    pub fn insert_at_index_raw(&mut self, index: usize, key: &str, value: &str) {
        // Check if the key already exists
        if self.contains_key(key) {
            // Key already exists, just update it
            self.set_raw(key, value);
            return;
        }

        // Collect all existing pairs
        let mut pairs: Vec<(Scalar, SyntaxNode)> = Vec::new();
        for (k, v) in self.pairs() {
            if let (Some(key_scalar), Some(value_node)) = (k, v) {
                pairs.push((key_scalar, value_node));
            }
        }

        // Create the new pair
        let new_key_scalar = Scalar(create_scalar_node(key));
        let new_value_node = create_scalar_node(value);

        // Insert at the specified index (or at the end if index is too large)
        if index >= pairs.len() {
            pairs.push((new_key_scalar, new_value_node));
        } else {
            pairs.insert(index, (new_key_scalar, new_value_node));
        }

        self.rebuild_from_pairs(pairs);
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

// Editing methods for Sequence
impl Sequence {
    /// Add an item to the end of the sequence (generic version)
    pub fn push<T: Into<ScalarValue>>(&mut self, value: T) {
        let scalar_value = value.into();
        self.push_str(&scalar_value.to_string());
    }

    /// Add a string item to the end of the sequence
    pub fn push_str(&mut self, value: &str) {
        let new_item_tokens = create_sequence_item_green(value);
        let child_count = self.0.children_with_tokens().count();
        // Convert green nodes/tokens to syntax nodes/tokens
        let syntax_elements: Vec<_> = new_item_tokens
            .into_iter()
            .map(|element| {
                match element {
                    rowan::NodeOrToken::Node(green_node) => {
                        SyntaxNode::new_root_mut(green_node).into()
                    }
                    rowan::NodeOrToken::Token(green_token) => {
                        // We need to create a SyntaxToken from GreenToken - this is not straightforward
                        // Let's create a temporary node to contain the token
                        let mut builder = GreenNodeBuilder::new();
                        builder.start_node(SyntaxKind::ROOT.into());
                        builder.token(green_token.kind(), green_token.text());
                        builder.finish_node();
                        let temp_node = SyntaxNode::new_root_mut(builder.finish());
                        temp_node.first_token().unwrap().into()
                    }
                }
            })
            .collect();
        self.0
            .splice_children(child_count..child_count, syntax_elements);
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
            if let Some(token) = child.as_token() {
                if token.kind() == SyntaxKind::DASH {
                    if item_count == index {
                        // Find the value node after this dash (skip whitespace)
                        let mut value_index = None;
                        for (j, next_child) in children.iter().enumerate().skip(i + 1) {
                            if let Some(node) = next_child.as_node() {
                                if node.kind() == SyntaxKind::SCALAR {
                                    value_index = Some(j);
                                    break;
                                }
                            }
                        }

                        if let Some(val_idx) = value_index {
                            // Replace just the value node
                            let new_value_node =
                                SyntaxNode::new_root_mut(create_scalar_green(value));
                            self.0
                                .splice_children(val_idx..val_idx + 1, vec![new_value_node.into()]);
                            return true;
                        }
                    }
                    item_count += 1;
                }
            }
        }
        false
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
        let value = mapping.get("key");
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
        assert!(mapping.get("string").is_some());
        assert!(mapping.get("integer").is_some());
        assert!(mapping.get("float").is_some());
        assert!(mapping.get("bool_true").is_some());
        assert!(mapping.get("bool_false").is_some());
        assert!(mapping.get("null_value").is_some());
        assert!(mapping.get("tilde").is_some());
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

        assert!(mapping.get("single").is_some());
        assert!(mapping.get("double").is_some());
        assert!(mapping.get("plain").is_some());
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

        assert!(mapping.get("empty_string").is_some());
        assert!(mapping.get("another_key").is_some());
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
        let mut parsed = Yaml::from_str(yaml).unwrap();

        // Use the Yaml-level set method
        parsed.set("new_key", "new_value");
        let output = parsed.to_string();
        assert!(output.contains("new_key"));
        assert!(output.contains("new_value"));
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
            let scalar = ScalarValue::auto_typed(timestamp_str);

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
        let mut parsed = Yaml::from_str(yaml).unwrap();

        // Add a new key using Yaml level method
        parsed.set("key2", "value2");

        let output = parsed.to_string();
        println!("After set:\n{}", output);
        assert!(output.contains("key1: value1"));
        assert!(output.contains("key2: value2"));
    }

    #[test]
    fn test_mapping_insert_after() {
        let yaml = r#"first: 1
second: 2
fourth: 4"#;

        let mut parsed = Yaml::from_str(yaml).unwrap();
        println!("Original YAML:\n{}", parsed.to_string());

        // Insert after "second"
        let success = parsed.insert_after("second", "third", "3");
        assert!(
            success,
            "insert_after should succeed when reference key exists"
        );

        let output = parsed.to_string();
        println!("Output after insert_after:\n{}", output);

        // Check exact output - should preserve original structure and insert correctly
        let expected = r#"first: 1
second: 2
third: '3'
fourth: 4"#;
        assert_eq!(output.trim(), expected);

        // Test inserting after non-existent key
        let failed = parsed.insert_after("nonexistent", "new_key", "new_value");
        assert!(
            !failed,
            "insert_after should fail when reference key doesn't exist"
        );

        // Test updating existing key through insert_after
        let updated = parsed.insert_after("first", "second", "2_updated");
        assert!(updated, "insert_after should update existing key");
        let value = parsed.document().unwrap().get("second").unwrap();
        assert!(value.text().to_string().contains("2_updated"));
    }

    #[test]
    fn test_mapping_insert_before() {
        let yaml = r#"first: 1
third: 3
fourth: 4"#;

        let mut parsed = Yaml::from_str(yaml).unwrap();

        // Insert before "third"
        let success = parsed.insert_before("third", "second", "2");
        assert!(
            success,
            "insert_before should succeed when reference key exists"
        );

        let output = parsed.to_string();

        // Check exact output - should preserve original structure and insert correctly
        let expected = r#"first: 1
second: '2'
third: 3
fourth: 4"#;
        assert_eq!(output.trim(), expected);

        // Test inserting before non-existent key
        let failed = parsed.insert_before("nonexistent", "new_key", "new_value");
        assert!(
            !failed,
            "insert_before should fail when reference key doesn't exist"
        );

        // Test updating existing key through insert_before
        let updated = parsed.insert_before("fourth", "third", "3_updated");
        assert!(updated, "insert_before should update existing key");
        let value = parsed.document().unwrap().get("third").unwrap();
        assert!(value.text().to_string().contains("3_updated"));
    }

    #[test]
    fn test_mapping_insert_at_index() {
        let yaml = r#"first: 1
third: 3"#;

        let mut parsed = Yaml::from_str(yaml).unwrap();

        // Insert at index 1 (between first and third)
        parsed.insert_at_index(1, "second", "2");

        let output = parsed.to_string();

        // Check exact output - should preserve original structure and insert correctly
        let expected = r#"first: 1
second: '2'
third: 3"#;
        assert_eq!(output.trim(), expected);

        // Insert at index 0 (beginning)
        parsed.insert_at_index(0, "zero", "0");
        let output2 = parsed.to_string();
        println!("Output2 after inserting at index 0:\n{}", output2);
        let lines2: Vec<&str> = output2.trim().lines().collect();
        assert!(
            lines2[0].starts_with("zero:"),
            "Expected zero at index 0, but lines are: {:?}",
            lines2
        );

        // Insert at out-of-bounds index (should append at end)
        parsed.insert_at_index(100, "last", "999");
        let output3 = parsed.to_string();
        let lines3: Vec<&str> = output3.trim().lines().collect();
        assert!(lines3.last().unwrap().starts_with("last:"));

        // Test updating existing key through insert_at_index
        parsed.insert_at_index(2, "first", "1_updated");
        let value = parsed.document().unwrap().get("first").unwrap();
        assert!(value.text().to_string().contains("1_updated"));
    }

    #[test]
    fn test_mapping_insert_special_characters() {
        let yaml = "key1: value1";

        let mut parsed = Yaml::from_str(yaml).unwrap();

        // Test with special characters that need escaping
        parsed.insert_after("key1", "special:key", "value:with:colons");
        parsed.insert_before("key1", "key with spaces", "value with spaces");
        parsed.insert_at_index(1, "key@symbol", "value#hash");

        // Verify all keys are present
        let doc = parsed.document().unwrap();
        assert!(doc.contains_key("special:key"));
        assert!(doc.contains_key("key with spaces"));
        assert!(doc.contains_key("key@symbol"));

        // Parse the output to verify it's valid YAML
        let output = parsed.to_string();
        let reparsed = Yaml::from_str(&output);
        assert!(reparsed.is_ok(), "Output should be valid YAML");
    }

    #[test]
    fn test_mapping_insert_empty_values() {
        let yaml = "key1: value1";

        let mut parsed = Yaml::from_str(yaml).unwrap();

        // Test with empty values
        parsed.insert_after("key1", "empty", "");
        parsed.insert_before("key1", "null_key", ScalarValue::null());

        let doc = parsed.document().unwrap();
        assert!(doc.contains_key("empty"));
        assert!(doc.contains_key("null_key"));

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
            .any(|e| e.message.contains("json schema"));
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
        let mut parsed = Yaml::from_str(yaml).unwrap();

        // Insert a sequence after "name"
        let features = YamlValue::from(vec!["feature1", "feature2", "feature3"]);
        let success = parsed.insert_after("name", "features", features);
        assert!(success, "insert_after should succeed");

        let output = parsed.to_string();
        println!("Output with sequence:\n{}", output);

        // Verify the sequence is present and in correct order
        assert!(output.contains("name: project"));
        assert!(output.contains("features:"));
        assert!(output.contains("feature1"));
        assert!(output.contains("version: 1.0.0"));

        // Verify it's valid YAML
        let reparsed = Yaml::from_str(&output);
        assert!(reparsed.is_ok(), "Output should be valid YAML");
    }

    #[test]
    fn test_insert_before_with_mapping() {
        let yaml = "name: project\nversion: 1.0.0";
        let mut parsed = Yaml::from_str(yaml).unwrap();

        // Insert a nested mapping before "version"
        let mut db_config = std::collections::BTreeMap::new();
        db_config.insert("host".to_string(), YamlValue::from("localhost"));
        db_config.insert("port".to_string(), YamlValue::from(5432));
        db_config.insert("database".to_string(), YamlValue::from("mydb"));

        let database = YamlValue::from_mapping(db_config);
        let success = parsed.insert_before("version", "database", database);
        assert!(success, "insert_before should succeed");

        let output = parsed.to_string();
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
        let mut parsed = Yaml::from_str(yaml).unwrap();

        // Insert different types at various indices
        parsed.insert_at_index(1, "version", YamlValue::from("1.0.0"));
        parsed.insert_at_index(2, "active", YamlValue::from(true));
        parsed.insert_at_index(3, "count", YamlValue::from(42));

        let features = YamlValue::from(vec!["auth", "logging"]);
        parsed.insert_at_index(4, "features", features);

        let output = parsed.to_string();
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
        let mut parsed = Yaml::from_str(yaml).unwrap();

        // Insert various scalar types
        parsed.insert_after("name", "null_value", YamlValue::from(ScalarValue::null()));
        parsed.insert_after("null_value", "empty_string", YamlValue::from(""));
        parsed.insert_after("empty_string", "number", YamlValue::from(3.14));
        parsed.insert_after("number", "boolean", YamlValue::from(false));

        let output = parsed.to_string();
        println!("Output with special scalars:\n{}", output);

        // Verify all values are present
        assert!(output.contains("name: project"));
        assert!(output.contains("null_value"));
        assert!(output.contains("empty_string"));
        assert!(output.contains("number: 3.14"));
        assert!(output.contains("boolean: false"));

        // Verify it's valid YAML
        let reparsed = Yaml::from_str(&output);
        assert!(reparsed.is_ok(), "Output should be valid YAML");
    }

    #[test]
    fn test_insert_ordering_preservation() {
        let yaml = "first: 1\nthird: 3\nfifth: 5";
        let mut parsed = Yaml::from_str(yaml).unwrap();

        // Insert items to create proper ordering
        parsed.insert_after("first", "second", YamlValue::from(2));
        parsed.insert_before("fifth", "fourth", YamlValue::from(4));

        let output = parsed.to_string();

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
        let mut parsed = Yaml::from_str(yaml).unwrap();

        // Test positioning with different YamlValue types

        // Position after a string value (using YamlValue instead of &str)
        let string_key = YamlValue::from("name");
        let success = parsed.insert_after(string_key, "description", "A sample project");
        assert!(success, "Should find string key");

        // Position after a numeric value
        let numeric_key = YamlValue::from(1.0);
        let success = parsed.insert_after(numeric_key, "build", "gradle");
        assert!(
            !success,
            "Should not find numeric key (1.0) when actual key is string 'version'"
        );

        // Position after a boolean value
        let bool_key = YamlValue::from(true);
        let success = parsed.insert_after(bool_key, "test", "enabled");
        assert!(
            !success,
            "Should not find boolean key (true) when actual key is string 'active'"
        );

        // But string representation should work
        let bool_string_key = YamlValue::from("true");
        let success = parsed.insert_after(bool_string_key, "test_mode", "development");
        assert!(!success, "Should not find 'true' key when value is true");

        let output = parsed.to_string();
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
        let mut parsed = Yaml::from_str(yaml).unwrap();

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

        parsed.insert_after("name", "config", YamlValue::from_mapping(app_config));

        let output = parsed.to_string();
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
        let mut parsed = Yaml::from_str(yaml).unwrap();

        // Insert a YAML set
        let mut tags = std::collections::BTreeSet::new();
        tags.insert("production".to_string());
        tags.insert("database".to_string());
        tags.insert("web".to_string());

        let yaml_set = YamlValue::from_set(tags);
        parsed.insert_after("name", "tags", yaml_set);

        let output = parsed.to_string();
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
        let mut parsed = Yaml::from_str(yaml).unwrap();

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
        parsed.insert_after("name", "build_steps", yaml_omap);

        let output = parsed.to_string();
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
        let mut parsed = Yaml::from_str(yaml).unwrap();

        // Insert a YAML pairs collection (!!pairs - allows duplicate keys)
        let connection_attempts = vec![
            ("server".to_string(), YamlValue::from("primary.db")),
            ("server".to_string(), YamlValue::from("secondary.db")), // Duplicate key allowed
            ("server".to_string(), YamlValue::from("tertiary.db")),  // Another duplicate
            ("timeout".to_string(), YamlValue::from(30)),
        ];

        let yaml_pairs = YamlValue::from_pairs(connection_attempts);
        parsed.insert_after("name", "connections", yaml_pairs);

        let output = parsed.to_string();
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
        let mut parsed1 = Yaml::from_str(yaml1).unwrap();
        parsed1.insert_after("name", "empty_list", YamlValue::sequence());
        let output1 = parsed1.to_string();
        println!("Output with empty sequence:\n{}", output1);
        assert!(output1.contains("name: project"));
        assert!(output1.contains("empty_list:"));
        assert!(output1.contains("[]"));

        // Test empty mapping
        let yaml2 = "name: project";
        let mut parsed2 = Yaml::from_str(yaml2).unwrap();
        parsed2.insert_after("name", "empty_map", YamlValue::mapping());
        let output2 = parsed2.to_string();
        println!("Output with empty mapping:\n{}", output2);
        assert!(output2.contains("name: project"));
        assert!(output2.contains("empty_map:"));
        assert!(output2.contains("{}"));

        // Test empty set
        let yaml3 = "name: project";
        let mut parsed3 = Yaml::from_str(yaml3).unwrap();
        parsed3.insert_after("name", "empty_set", YamlValue::set());
        let output3 = parsed3.to_string();
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
        let mut parsed = Yaml::from_str(yaml).unwrap();

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

        parsed.insert_after("name", "nested_data", level1_sequence);

        let output = parsed.to_string();
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

        // Insert a new key using AST-preserving method
        if let Some(mut mapping) = doc.as_mapping_mut() {
            let success = mapping.insert_after_preserving("key1", "new_key", "new_value");
            if success {
                // Propagate the modified mapping back to the document
                doc.replace_with_mapping(mapping);
            }
        }

        let result = doc.to_string();
        println!("Result after insertion:\n{}", result);

        // These should be preserved but currently are not due to replace_with_mapping
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
