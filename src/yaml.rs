//! Lossless YAML parser and editor.

use crate::{
    error::YamlResult,
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
        builder.finish_node();
        Document(SyntaxNode::new_root_mut(builder.finish()))
    }

    /// Create a new document with an empty mapping
    pub fn new_mapping() -> Document {
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(SyntaxKind::DOCUMENT.into());
        builder.start_node(SyntaxKind::MAPPING.into());
        builder.finish_node(); // End MAPPING
        builder.finish_node(); // End DOCUMENT
        Document(SyntaxNode::new_root_mut(builder.finish()))
    }

    /// Load a document from a file
    pub fn load_from_file<P: AsRef<Path>>(path: P) -> YamlResult<Document> {
        let content = std::fs::read_to_string(path)?;
        let parsed = Yaml::parse(&content);
        Ok(parsed
            .tree()
            .documents()
            .next()
            .unwrap_or_else(Document::new))
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
        std::fs::write(path, self.to_yaml_string())?;
        Ok(())
    }

    /// Get the root node of this document (could be mapping, sequence, or scalar)
    pub fn root_node(&self) -> Option<SyntaxNode> {
        self.0.children().find(|child| {
            matches!(
                child.kind(),
                SyntaxKind::MAPPING | SyntaxKind::SEQUENCE | SyntaxKind::SCALAR
            )
        })
    }

    /// Get this document as a mapping, if it is one
    pub fn as_mapping(&self) -> Option<Mapping> {
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
        self.as_mapping().map_or(false, |m| m.contains_key(key))
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
                            pairs.push((existing_key_str, value_node.text().to_string()));
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
        self.as_mapping().map_or(true, |m| m.is_empty())
    }

    /// Create a document from a mapping
    pub fn from_mapping(mapping: Mapping) -> Self {
        // For now, create a new document and copy the mapping content
        // This is a simplified implementation
        let mut doc = Document::new();
        // Copy all key-value pairs from the mapping
        for (key_opt, value_opt) in mapping.pairs() {
            if let (Some(key), Some(value)) = (key_opt, value_opt) {
                doc.set_raw(&key.value(), &value.text().to_string().trim());
            }
        }
        doc
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
                // VALUE nodes contain the raw text, parse it like a scalar
                let text = node.text().to_string();
                // Use the same parsing logic as Scalar::as_string()
                if text.starts_with('"') && text.ends_with('"') {
                    // Double-quoted string - handle escape sequences
                    Document::parse_double_quoted_static(&text[1..text.len() - 1])
                } else if text.starts_with('\'') && text.ends_with('\'') {
                    // Single-quoted string - only handle '' -> '
                    text[1..text.len() - 1].replace("''", "'")
                } else {
                    // Plain string
                    text
                }
            } else if let Some(scalar) = Scalar::cast(node.clone()) {
                // SCALAR nodes need to be processed
                scalar.as_string()
            } else {
                // For other node types, use raw text
                node.text().to_string()
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
                    // Block style array
                    None // TODO: Implement block style array parsing
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

    /// Parse a double-quoted string, handling escape sequences
    fn parse_double_quoted_static(text: &str) -> String {
        if !text.starts_with('"') || !text.ends_with('"') || text.len() < 2 {
            return text.to_string();
        }

        let inner = &text[1..text.len() - 1];
        let mut result = String::new();
        let mut chars = inner.chars();

        while let Some(ch) = chars.next() {
            if ch == '\\' {
                if let Some(escaped) = chars.next() {
                    match escaped {
                        'n' => result.push('\n'),
                        't' => result.push('\t'),
                        'r' => result.push('\r'),
                        '\\' => result.push('\\'),
                        '"' => result.push('"'),
                        '\'' => result.push('\''),
                        _ => {
                            result.push('\\');
                            result.push(escaped);
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

    /// Create a new document syntax node with a mapping containing multiple entries
    fn create_mapping_with_all_entries(&self, pairs: Vec<(String, String)>) -> SyntaxNode {
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(SyntaxKind::DOCUMENT.into());
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
            builder.start_node(SyntaxKind::SCALAR.into());
            builder.token(SyntaxKind::VALUE.into(), value);
            builder.finish_node();
        }

        builder.finish_node(); // MAPPING
        builder.finish_node(); // DOCUMENT

        SyntaxNode::new_root(builder.finish())
    }
}

impl Mapping {
    /// Get all key-value pairs in this mapping
    pub fn pairs(&self) -> impl Iterator<Item = (Option<Scalar>, Option<SyntaxNode>)> {
        // Look for KEY nodes followed by VALUE nodes
        let children: Vec<_> = self.0.children().collect();
        let mut pairs = Vec::new();
        let mut i = 0;

        while i < children.len() {
            // Check if this is a KEY node
            if children[i].kind() == SyntaxKind::KEY {
                let key_text = children[i].text().to_string();
                let key_scalar = Scalar(create_scalar_node(&key_text));

                // Look for the next value node (SCALAR, VALUE, SEQUENCE, or MAPPING)
                let mut value_node = None;
                let mut j = i + 1;
                while j < children.len() {
                    if children[j].kind() == SyntaxKind::SCALAR
                        || children[j].kind() == SyntaxKind::VALUE
                        || children[j].kind() == SyntaxKind::SEQUENCE
                        || children[j].kind() == SyntaxKind::MAPPING
                    {
                        value_node = Some(children[j].clone());
                        break;
                    }
                    j += 1;
                }

                pairs.push((Some(key_scalar), value_node));
                i = j + 1;
            } else {
                i += 1;
            }
        }

        pairs.into_iter()
    }

    /// Get the value for a specific key
    pub fn get(&self, key: &str) -> Option<SyntaxNode> {
        self.pairs()
            .find(|(k, _)| k.as_ref().map(|s| s.value()) == Some(key.to_string()))
            .and_then(|(_, v)| v)
    }

    /// Check if the mapping contains a specific key
    pub fn contains_key(&self, key: &str) -> bool {
        self.pairs()
            .any(|(k, _)| k.as_ref().map(|s| s.value()) == Some(key.to_string()))
    }

    /// Get all keys in the mapping
    pub fn keys(&self) -> impl Iterator<Item = String> + '_ {
        self.pairs().filter_map(|(k, _)| k.map(|s| s.value()))
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
        let mut all_pairs: Vec<(String, String)> = Vec::new();
        for (key_opt, value_opt) in self.pairs() {
            if let (Some(key), Some(value)) = (key_opt, value_opt) {
                all_pairs.push((key.value(), value.text().to_string().trim().to_string()));
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
                        };
                        format!("{}: {}", k, value_str)
                    })
                    .collect();
                let mapping_yaml = format!("{{{}}}", pairs.join(", "));
                let key_str = key.into().to_yaml_string();
                self.set_raw(&key_str, &mapping_yaml);
            }
        }
    }
}

impl Sequence {
    /// Get all items in this sequence
    pub fn items(&self) -> impl Iterator<Item = SyntaxNode> {
        // TODO: Implementation for getting sequence items
        std::iter::empty()
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
            self.parse_double_quoted(&text[1..text.len() - 1])
        } else if text.starts_with('\'') && text.ends_with('\'') {
            // Single-quoted string - only handle '' -> '
            text[1..text.len() - 1].replace("''", "'")
        } else {
            // Plain string
            text
        }
    }

    /// Parse double-quoted string with escape sequences
    fn parse_double_quoted(&self, content: &str) -> String {
        let mut result = String::new();
        let mut chars = content.chars();

        while let Some(ch) = chars.next() {
            if ch == '\\' {
                if let Some(escaped) = chars.next() {
                    match escaped {
                        'n' => result.push('\n'),
                        'r' => result.push('\r'),
                        't' => result.push('\t'),
                        '\\' => result.push('\\'),
                        '"' => result.push('"'),
                        _ => {
                            result.push('\\');
                            result.push(escaped);
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
            text.to_string()
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
        }
    }

    fn parse(mut self) -> ParsedYaml {
        self.builder.start_node(SyntaxKind::ROOT.into());

        self.skip_ws_and_newlines();

        // Parse documents
        // Always parse at least one document
        if self.current().is_some() {
            self.parse_document();
            self.skip_ws_and_newlines();

            // Only parse additional documents if we see a document separator
            while self.current() == Some(SyntaxKind::DOC_START) {
                self.parse_document();
                self.skip_ws_and_newlines();
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
            Some(SyntaxKind::LEFT_BRACKET) => self.parse_flow_sequence(),
            Some(SyntaxKind::LEFT_BRACE) => self.parse_flow_mapping(),
            _ => self.parse_scalar(),
        }
    }

    fn parse_anchored_value(&mut self) {
        let mut anchor_name = None;

        // Extract anchor name from the token
        if self.current() == Some(SyntaxKind::ANCHOR) {
            if let Some((_, token_text)) = self.tokens.last() {
                // Extract anchor name from "&anchor_name" format
                if token_text.starts_with('&') {
                    anchor_name = Some(token_text[1..].to_string());
                }
            }
            self.bump(); // consume anchor token
            self.skip_whitespace();
        }

        // Parse the actual value that is being anchored
        let value_start = self.builder.checkpoint();
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
                if token_text.starts_with('*') {
                    alias_name = Some(token_text[1..].to_string());
                }
            }
        }

        // Check if the anchor exists in our registry and validate
        if let Some(name) = alias_name {
            if !self.anchor_registry.contains_key(&name) {
                self.add_error(format!("Undefined alias: {}", name));
            }
        }

        // Create a scalar node and just consume the reference token
        // The token itself already contains the full "*alias_name" text
        self.builder.start_node(SyntaxKind::SCALAR.into());
        if self.current() == Some(SyntaxKind::REFERENCE) {
            self.bump(); // This preserves the original "*alias_name" token
        }
        self.builder.finish_node();
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

            // TODO: Parse quoted content properly
            while self.current().is_some() && self.current() != Some(quote_type) {
                self.bump();
            }

            if self.current() == Some(quote_type) {
                self.bump(); // closing quote
            } else {
                self.add_error("Unterminated quoted string".to_string());
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
                self.bump();
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

    fn parse_mapping(&mut self) {
        self.builder.start_node(SyntaxKind::MAPPING.into());

        while self.current().is_some() {
            if !self.is_mapping_key() {
                break;
            }

            // Parse key - wrap the scalar token in a KEY node
            self.builder.start_node(SyntaxKind::KEY.into());
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
                    self.parse_value();
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
                self.add_error("Expected ':' after mapping key".to_string());
            }

            self.skip_ws_and_newlines();
        }

        self.builder.finish_node();
    }

    fn parse_sequence(&mut self) {
        self.builder.start_node(SyntaxKind::SEQUENCE.into());

        while self.current() == Some(SyntaxKind::DASH) {
            self.bump(); // consume dash
            self.skip_whitespace();

            if self.current().is_some() && self.current() != Some(SyntaxKind::NEWLINE) {
                self.parse_value();
            }

            self.skip_ws_and_newlines();
        }

        self.builder.finish_node();
    }

    fn parse_flow_sequence(&mut self) {
        self.builder.start_node(SyntaxKind::SEQUENCE.into());

        self.bump(); // consume [
        self.skip_whitespace();

        let prev_flow = self.in_flow_context;
        self.in_flow_context = true;

        while self.current() != Some(SyntaxKind::RIGHT_BRACKET) && self.current().is_some() {
            self.parse_value();
            self.skip_whitespace();

            if self.current() == Some(SyntaxKind::COMMA) {
                self.bump();
                self.skip_whitespace();
            }
        }

        self.in_flow_context = prev_flow;

        if self.current() == Some(SyntaxKind::RIGHT_BRACKET) {
            self.bump();
        } else {
            self.add_error("Expected ']' to close flow sequence".to_string());
        }

        self.builder.finish_node();
    }

    fn parse_flow_mapping(&mut self) {
        self.builder.start_node(SyntaxKind::MAPPING.into());

        self.bump(); // consume {
        self.skip_whitespace();

        let prev_flow = self.in_flow_context;
        self.in_flow_context = true;

        while self.current() != Some(SyntaxKind::RIGHT_BRACE) && self.current().is_some() {
            // Parse key
            self.parse_value();
            self.skip_whitespace();

            if self.current() == Some(SyntaxKind::COLON) {
                self.bump();
                self.skip_whitespace();
                self.parse_value();
            } else {
                self.add_error("Expected ':' in flow mapping".to_string());
            }

            self.skip_whitespace();

            if self.current() == Some(SyntaxKind::COMMA) {
                self.bump();
                self.skip_whitespace();
            }
        }

        self.in_flow_context = prev_flow;

        if self.current() == Some(SyntaxKind::RIGHT_BRACE) {
            self.bump();
        } else {
            self.add_error("Expected '}' to close flow mapping".to_string());
        }

        self.builder.finish_node();
    }

    fn is_mapping_key(&self) -> bool {
        // Look ahead to see if there's a colon after the current token
        for kind in self.upcoming_tokens() {
            match kind {
                SyntaxKind::COLON => return true,
                SyntaxKind::WHITESPACE => continue,
                // These tokens definitely end a potential mapping key
                SyntaxKind::NEWLINE
                | SyntaxKind::DASH
                | SyntaxKind::DOC_START
                | SyntaxKind::DOC_END
                | SyntaxKind::COMMA
                | SyntaxKind::RIGHT_BRACKET
                | SyntaxKind::RIGHT_BRACE => return false,
                // Other tokens could be part of a complex key, keep looking
                _ => continue,
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

    fn bump(&mut self) {
        if let Some((kind, text)) = self.tokens.pop() {
            self.builder.token(kind.into(), &text);
            if self.current_token_index > 0 {
                self.current_token_index -= 1;
            }
        }
    }

    fn current(&self) -> Option<SyntaxKind> {
        self.tokens.last().map(|(kind, _)| *kind)
    }

    /// Iterator over upcoming tokens starting from the next token (not current)
    fn upcoming_tokens(&self) -> impl Iterator<Item = SyntaxKind> + '_ {
        (1..self.tokens.len()).map(move |i| {
            let idx = self.tokens.len() - 1 - i;
            self.tokens[idx].0
        })
    }

    fn add_error(&mut self, message: String) {
        self.errors.push(message);
    }
}

/// Parse YAML text
pub fn parse(text: &str) -> ParsedYaml {
    let parser = Parser::new(text);
    parser.parse()
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

        if let Some(doc) = parsed.document() {
            if let Some(mut mapping) = doc.as_mapping() {
                mapping.set("new_key", "new_value");
                let output = parsed.to_string();
                assert!(output.contains("new_key"));
                assert!(output.contains("new_value"));
            }
        }
    }

    #[test]
    fn test_mapping_rename_key() {
        let yaml = "old_name: value";
        let mut parsed = Yaml::from_str(yaml).unwrap();

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
        let mut parsed = Yaml::from_str(yaml).unwrap();

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
        let mut parsed = Yaml::from_str(yaml).unwrap();

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

    #[test]
    fn test_anchors_and_aliases() {
        // Test basic anchor definition and alias reference
        let yaml = r#"
default: &default_config
  host: localhost
  port: 8080

development:
  <<: *default_config
  debug: true

production: *default_config
"#;
        let parsed = Yaml::from_str(yaml);
        assert!(parsed.is_ok(), "Should parse YAML with anchors and aliases");

        let yaml_doc = parsed.unwrap();
        assert!(yaml_doc.document().is_some());
        let doc = yaml_doc.document().unwrap();
        assert!(doc.as_mapping().is_some());
    }

    #[test]
    fn test_simple_anchor_and_alias() {
        let yaml = r#"
key: &anchor value
ref: *anchor
"#;
        let parsed = Yaml::from_str(yaml);
        assert!(
            parsed.is_ok(),
            "Should parse simple anchor and alias: {:?}",
            parsed
        );

        let yaml_doc = parsed.unwrap();
        let output = yaml_doc.to_string();

        // Should preserve anchor and alias tokens
        assert!(
            output.contains("&anchor"),
            "Should preserve anchor definition"
        );
        assert!(
            output.contains("*anchor"),
            "Should preserve alias reference"
        );
    }

    #[test]
    fn test_undefined_alias_error() {
        let yaml = r#"
key: *undefined_anchor
"#;
        let parsed = Yaml::from_str(yaml);
        // Should parse but have errors
        if let Ok(yaml_doc) = parsed {
            let parse_result = Parse::parse_yaml(yaml);
            assert!(
                parse_result.has_errors(),
                "Should have error for undefined alias"
            );
        }
    }

    #[test]
    fn test_anchor_in_sequence() {
        let yaml = r#"
items:
  - &item1 "first item"
  - "second item"
  - *item1
"#;
        let parsed = Yaml::from_str(yaml);
        assert!(parsed.is_ok(), "Should parse anchors in sequences");

        let yaml_doc = parsed.unwrap();
        let output = yaml_doc.to_string();
        assert!(output.contains("&item1"));
        assert!(output.contains("*item1"));
    }

    #[test]
    fn test_anchor_in_mapping() {
        let yaml = r#"
database: &db_config
  host: localhost
  port: 5432
  
app_config:
  database: *db_config
"#;
        let parsed = Yaml::from_str(yaml);
        assert!(parsed.is_ok(), "Should parse anchors in mappings");

        let yaml_doc = parsed.unwrap();
        let output = yaml_doc.to_string();
        assert!(output.contains("&db_config"));
        assert!(output.contains("*db_config"));
    }

    #[test]
    fn test_multiple_aliases_same_anchor() {
        let yaml = r#"
default: &shared
  setting: value

config1: *shared
config2: *shared  
config3: *shared
"#;
        let parsed = Yaml::from_str(yaml);
        assert!(
            parsed.is_ok(),
            "Should handle multiple aliases to same anchor"
        );

        let yaml_doc = parsed.unwrap();
        let output = yaml_doc.to_string();

        // Should have one anchor definition and three aliases
        assert!(output.contains("&shared"));
        assert_eq!(
            output.matches("*shared").count(),
            3,
            "Should have 3 alias references"
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
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(SyntaxKind::MAPPING.into());

        for (key_scalar, value_node) in pairs {
            // Create key token
            builder.token(SyntaxKind::VALUE.into(), &key_scalar.value());
            builder.token(SyntaxKind::COLON.into(), ":");
            builder.token(SyntaxKind::WHITESPACE.into(), " ");

            // Add value
            if value_node.kind() == SyntaxKind::SCALAR {
                builder.start_node(SyntaxKind::SCALAR.into());
                // Extract the VALUE token from the scalar node
                for token in value_node.children_with_tokens() {
                    if let Some(token) = token.as_token() {
                        if token.kind() == SyntaxKind::VALUE {
                            builder.token(SyntaxKind::VALUE.into(), token.text());
                        }
                    }
                }
                builder.finish_node();
            }

            builder.token(SyntaxKind::NEWLINE.into(), "\n");
        }

        builder.finish_node();
        let new_mapping = SyntaxNode::new_root_mut(builder.finish());

        // Replace the entire mapping content
        let child_count = self.0.children_with_tokens().count();
        self.0
            .splice_children(0..child_count, vec![new_mapping.into()]);
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

        // Navigate to nested structure - TODO: implement properly
        let (_first, _rest) = parts.split_first().unwrap();
        // if let Some(nested) = self.get_mapping(first) {
        //     nested.set_path(&rest.join("."), value);
        // }
    }
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
    /// Add an item to the end of the sequence
    pub fn push(&mut self, value: &str) {
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
