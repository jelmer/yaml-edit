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
        // For now, create a new document and copy the mapping content
        // This is a simplified implementation
        let mut doc = Document::new();
        // Copy all key-value pairs from the mapping
        for (key_opt, value_opt) in mapping.pairs() {
            if let (Some(key), Some(value)) = (key_opt, value_opt) {
                doc.set_raw(&key.value(), value.text().to_string().trim());
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
            builder.start_node(SyntaxKind::SCALAR.into());
            builder.token(SyntaxKind::VALUE.into(), value);
            builder.finish_node();
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
                        let key_text = self.children[self.index].text().to_string();
                        let key_scalar = Scalar(create_scalar_node(&key_text));

                        // Look for the next value node
                        let mut value_node = None;
                        let mut j = self.index + 1;
                        while j < self.children.len() {
                            let kind = self.children[j].kind();
                            if kind == SyntaxKind::SCALAR
                                || kind == SyntaxKind::VALUE
                                || kind == SyntaxKind::SEQUENCE
                                || kind == SyntaxKind::MAPPING
                            {
                                value_node = Some(self.children[j].clone());
                                break;
                            }
                            j += 1;
                        }

                        self.index = j + 1;
                        return Some((Some(key_scalar), value_node));
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
            .find(|(k, _)| k.as_ref().map(|s| s.value()) == Some(key.to_string()))
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
        self.find_string_token_recursive(&self.0)
    }

    /// Recursively search for the first STRING token in the tree
    fn find_string_token_recursive(
        &self,
        node: &rowan::SyntaxNode<crate::yaml::Lang>,
    ) -> Option<String> {
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
            if let Some(result) = self.find_string_token_recursive(&child) {
                return Some(result);
            }
        }

        None
    }
}

impl Scalar {
    /// Get the string value of this scalar
    pub fn value(&self) -> String {
        self.0.text().to_string()
    }

    /// Get the string representation of this scalar, properly unquoted and unescaped
    pub fn as_string(&self) -> String {
        // Check if this scalar contains TAG tokens (mixed content)
        if self.contains_tag_tokens() {
            return self.extract_string_tokens_only();
        }

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

    /// Check if this scalar contains TAG tokens mixed with content
    fn contains_tag_tokens(&self) -> bool {
        for child in self.0.children_with_tokens() {
            if let rowan::NodeOrToken::Token(token) = child {
                if token.kind() == SyntaxKind::TAG {
                    return true;
                }
            }
        }
        false
    }

    /// Extract only value tokens (STRING, INT, FLOAT, BOOL, NULL) from a scalar that contains mixed TAG and value tokens
    fn extract_string_tokens_only(&self) -> String {
        let mut result = String::new();

        for child in self.0.children_with_tokens() {
            if let rowan::NodeOrToken::Token(token) = child {
                match token.kind() {
                    SyntaxKind::STRING
                    | SyntaxKind::INT
                    | SyntaxKind::FLOAT
                    | SyntaxKind::BOOL
                    | SyntaxKind::NULL => {
                        result.push_str(token.text());
                    }
                    _ => {} // Skip TAG, WHITESPACE, etc.
                }
            }
        }

        result
    }

    /// Parse double-quoted string with escape sequences
    fn parse_double_quoted(&self, content: &str) -> String {
        ScalarValue::parse_escape_sequences(content)
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
    /// Track currently resolving aliases to detect circular references
    resolving_aliases: Vec<String>,
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
                if token_text.starts_with('*') {
                    alias_name = Some(token_text[1..].to_string());
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

            // Parse quoted content by consuming the appropriate tokens
            // For format-preserving parsing, we consume all tokens until the closing quote
            while self.current().is_some() && self.current() != Some(quote_type) {
                // Handle content within quotes (including escape sequences)
                match self.current() {
                    Some(SyntaxKind::STRING) => self.bump(), // String content
                    Some(_) => self.bump(), // Other content (escape sequences, spaces, newlines, etc.)
                    None => break,
                }
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

    fn parse_tagged_value(&mut self) {
        self.builder.start_node(SyntaxKind::TAGGED_SCALAR.into());

        // Consume the tag token - this becomes part of the tagged scalar
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
                    self.add_error("Unterminated quoted string".to_string());
                }
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

        // Consume the '|' indicator
        self.bump(); // PIPE token

        // Handle block scalar header (chomping and indentation indicators)
        self.parse_block_scalar_header();

        // Parse the block scalar content
        self.parse_block_scalar_content();

        self.builder.finish_node();
    }

    fn parse_folded_block_scalar(&mut self) {
        self.builder.start_node(SyntaxKind::SCALAR.into());

        // Consume the '>' indicator
        self.bump(); // GREATER token

        // Handle block scalar header (chomping and indentation indicators)
        self.parse_block_scalar_header();

        // Parse the block scalar content
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

        while self.current().is_some() {
            if !self.is_mapping_key() {
                break;
            }

            // Parse key - wrap the scalar token in a KEY node
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
        self.skip_ws_and_newlines(); // Support comments and newlines in flow sequences

        let prev_flow = self.in_flow_context;
        self.in_flow_context = true;

        while self.current() != Some(SyntaxKind::RIGHT_BRACKET) && self.current().is_some() {
            self.parse_value();
            self.skip_ws_and_newlines(); // Support comments after values

            if self.current() == Some(SyntaxKind::COMMA) {
                self.bump();
                self.skip_ws_and_newlines(); // Support comments after commas
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
        self.skip_ws_and_newlines(); // Support comments and newlines in flow mappings

        let prev_flow = self.in_flow_context;
        self.in_flow_context = true;

        while self.current() != Some(SyntaxKind::RIGHT_BRACE) && self.current().is_some() {
            // Parse key
            self.parse_value();
            self.skip_ws_and_newlines(); // Support comments after keys

            if self.current() == Some(SyntaxKind::COLON) {
                self.bump();
                self.skip_ws_and_newlines(); // Support comments after colons
                self.parse_value();
            } else {
                self.add_error("Expected ':' in flow mapping".to_string());
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
            self.add_error("Expected '}' to close flow mapping".to_string());
        }

        self.builder.finish_node();
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

    fn is_mapping_key(&self) -> bool {
        // Check if this is a merge key
        if self.current() == Some(SyntaxKind::MERGE_KEY) {
            return true;
        }

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

        // Navigate to nested structure
        let (first, rest) = parts.split_first().unwrap();

        // Build the nested structure from the path
        let nested_value = create_nested_value(rest, value);

        // Set the value using the mapping's existing set method
        self.set_value(ScalarValue::new(first.to_string()), nested_value);
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
    map.insert(first.to_string(), create_nested_value(rest, final_value));
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
        if text.starts_with('%') {
            text[1..].split_whitespace().next().map(|s| s.to_string())
        } else {
            None
        }
    }

    /// Get the directive value (e.g., "1.2" from "%YAML 1.2")
    pub fn value(&self) -> Option<String> {
        let text = self.text();
        if text.starts_with('%') {
            let parts: Vec<&str> = text[1..].split_whitespace().collect();
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
        let parsed = Yaml::from_str(yaml).unwrap();

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
}
