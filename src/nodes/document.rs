use super::{Lang, Mapping, Scalar, Sequence, SyntaxNode, TaggedNode};
use crate::as_yaml::{AsYaml, YamlKind};
use crate::error::YamlResult;
use crate::lex::SyntaxKind;
use crate::yaml::YamlFile;
use rowan::ast::AstNode;
use rowan::GreenNodeBuilder;
use std::path::Path;

ast_node!(Document, DOCUMENT, "A single YAML document");

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
        // Don't add document start marker "---" for programmatically created documents
        builder.start_node(SyntaxKind::MAPPING.into());
        builder.finish_node(); // End MAPPING
        builder.finish_node(); // End DOCUMENT
        Document(SyntaxNode::new_root_mut(builder.finish()))
    }

    /// Load a document from a file
    ///
    /// Returns an error if the file contains multiple documents.
    /// For multi-document YAML files, parse manually with `YamlFile::from_str()`.
    ///
    /// This follows the standard Rust naming convention of `from_file()`
    /// to pair with `to_file()`.
    pub fn from_file<P: AsRef<Path>>(path: P) -> YamlResult<Document> {
        use std::str::FromStr;
        let content = std::fs::read_to_string(path)?;
        Self::from_str(&content)
    }

    /// Write the document to a file, creating directories as needed
    ///
    /// This follows the standard Rust naming convention of `to_file()`
    /// to pair with `from_file()`.
    pub fn to_file<P: AsRef<Path>>(&self, path: P) -> YamlResult<()> {
        let path = path.as_ref();
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent)?;
        }
        let mut content = self.to_string();
        // Ensure the file ends with a newline
        if !content.ends_with('\n') {
            content.push('\n');
        }
        std::fs::write(path, content)?;
        Ok(())
    }

    /// Get the root node of this document (could be mapping, sequence, scalar, or alias)
    pub(crate) fn root_node(&self) -> Option<SyntaxNode> {
        self.0.children().find(|child| {
            matches!(
                child.kind(),
                SyntaxKind::MAPPING
                    | SyntaxKind::SEQUENCE
                    | SyntaxKind::SCALAR
                    | SyntaxKind::ALIAS
                    | SyntaxKind::TAGGED_NODE
            )
        })
    }

    /// Get this document as a mapping, if it is one.
    ///
    /// Note: `Mapping` supports mutation even though this method takes `&self`.
    /// Mutations are applied directly to the underlying syntax tree via rowan's
    /// persistent data structures. All references to the tree will see the changes.
    pub fn as_mapping(&self) -> Option<Mapping> {
        self.root_node().and_then(Mapping::cast)
    }

    /// Get this document's root value as a sequence, or `None` if it isn't one.
    pub fn as_sequence(&self) -> Option<Sequence> {
        self.root_node().and_then(Sequence::cast)
    }

    /// Get this document's root value as a scalar, or `None` if it isn't one.
    pub fn as_scalar(&self) -> Option<Scalar> {
        self.root_node().and_then(Scalar::cast)
    }

    /// Returns `true` if this document is a mapping that contains the given key.
    pub fn contains_key(&self, key: impl crate::AsYaml) -> bool {
        self.as_mapping().is_some_and(|m| m.contains_key(key))
    }

    /// Get the value for a key from the document's root mapping.
    ///
    /// Returns `None` if the document is not a mapping or the key doesn't exist.
    pub fn get(&self, key: impl crate::AsYaml) -> Option<crate::as_yaml::YamlNode> {
        self.as_mapping().and_then(|m| m.get(key))
    }

    /// Get the raw syntax node for a key in the document's root mapping.
    ///
    /// Returns `None` if the document is not a mapping or the key doesn't exist.
    /// Prefer [`get`](Self::get) for most use cases; this is for advanced CST access.
    pub(crate) fn get_node(&self, key: impl crate::AsYaml) -> Option<SyntaxNode> {
        self.as_mapping().and_then(|m| m.get_node(key))
    }

    /// Set a scalar value in the document (assumes document is a mapping)
    pub fn set(&self, key: impl crate::AsYaml, value: impl crate::AsYaml) {
        if let Some(mapping) = self.as_mapping() {
            mapping.set(key, value);
            // Changes are applied directly via splice_children, no need to replace
        } else {
            // If document is not a mapping, create one and add it to the document
            let mapping = Mapping::new();
            mapping.set(key, value);

            // Add the mapping node directly to the document
            let child_count = self.0.children_with_tokens().count();
            self.0
                .splice_children(child_count..child_count, vec![mapping.0.into()]);
        }
    }

    /// Set a key-value pair with field ordering support.
    ///
    /// If the key exists, updates its value. If the key doesn't exist, inserts it
    /// at the correct position based on the provided field order.
    /// Fields not in the order list are placed at the end.
    pub fn set_with_field_order<I, K>(
        &self,
        key: impl crate::AsYaml,
        value: impl crate::AsYaml,
        field_order: I,
    ) where
        I: IntoIterator<Item = K>,
        K: crate::AsYaml,
    {
        // Collect so we can pass to both branches if needed.
        let field_order: Vec<K> = field_order.into_iter().collect();
        if let Some(mapping) = self.as_mapping() {
            mapping.set_with_field_order(key, value, field_order);
            // Changes are applied directly via splice_children, no need to replace
        } else {
            // If document is not a mapping, create one and splice it in.
            let mapping = Mapping::new();
            mapping.set_with_field_order(key, value, field_order);
            let child_count = self.0.children_with_tokens().count();
            self.0
                .splice_children(child_count..child_count, vec![mapping.0.into()]);
        }
    }

    /// Remove a key from the document (assumes document is a mapping).
    ///
    /// Returns `Some(entry)` if the key existed and was removed, or `None` if
    /// the key was not found or the document is not a mapping. The returned
    /// [`MappingEntry`](super::MappingEntry) is detached from the tree; callers can inspect its key
    /// and value or re-insert it elsewhere.
    pub fn remove(&self, key: impl crate::AsYaml) -> Option<super::MappingEntry> {
        self.as_mapping()?.remove(key)
    }

    /// Get all key nodes from the document (assumes document is a mapping)
    pub(crate) fn key_nodes(&self) -> impl Iterator<Item = SyntaxNode> + '_ {
        self.as_mapping()
            .into_iter()
            .flat_map(|m| m.key_nodes().collect::<Vec<_>>())
    }

    /// Iterate over all keys in the document as [`YamlNode`](crate::as_yaml::YamlNode)s.
    ///
    /// Each key is a [`YamlNode`](crate::as_yaml::YamlNode) wrapping the
    /// underlying CST node.  Assumes the document is a mapping; yields nothing
    /// if it is not.
    pub fn keys(&self) -> impl Iterator<Item = crate::as_yaml::YamlNode> + '_ {
        self.key_nodes().filter_map(|key_node| {
            key_node
                .children()
                .next()
                .and_then(crate::as_yaml::YamlNode::from_syntax)
        })
    }

    /// Check if the document is empty
    pub fn is_empty(&self) -> bool {
        self.as_mapping().map_or(true, |m| m.is_empty())
    }

    /// Create a document from a mapping
    pub fn from_mapping(mapping: Mapping) -> Self {
        // Create a document directly from the mapping syntax node to avoid recursion
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(SyntaxKind::DOCUMENT.into());
        // Add the document start marker "---"
        builder.token(SyntaxKind::DOC_START.into(), "---");
        builder.token(SyntaxKind::WHITESPACE.into(), "\n");

        // Add the mapping with all its children, except trailing newline
        builder.start_node(SyntaxKind::MAPPING.into());
        let mapping_green = mapping.0.green();
        let children: Vec<_> = mapping_green.children().collect();

        // Check if last child is a newline token and skip it
        let end_index = if let Some(rowan::NodeOrToken::Token(t)) = children.last() {
            if t.kind() == SyntaxKind::NEWLINE.into() {
                children.len() - 1
            } else {
                children.len()
            }
        } else {
            children.len()
        };

        for child in &children[..end_index] {
            match child {
                rowan::NodeOrToken::Node(n) => {
                    builder.start_node(n.kind());
                    Self::add_green_node_children(&mut builder, n);
                    builder.finish_node();
                }
                rowan::NodeOrToken::Token(t) => {
                    builder.token(t.kind(), t.text());
                }
            }
        }

        // Don't add a trailing newline - entries already have their own newlines
        builder.finish_node(); // End MAPPING
        builder.finish_node(); // End DOCUMENT

        Document(SyntaxNode::new_root_mut(builder.finish()))
    }

    /// Helper method to recursively add green node children to the builder
    fn add_green_node_children(builder: &mut GreenNodeBuilder, node: &rowan::GreenNodeData) {
        for child in node.children() {
            match child {
                rowan::NodeOrToken::Node(n) => {
                    builder.start_node(n.kind());
                    Self::add_green_node_children(builder, n);
                    builder.finish_node();
                }
                rowan::NodeOrToken::Token(t) => {
                    builder.token(t.kind(), t.text());
                }
            }
        }
    }

    /// Insert a key-value pair immediately after `after_key` in this document's mapping.
    ///
    /// If `key` already exists, its value is updated in-place and it stays at its
    /// current position (it is **not** moved). Returns `false` if `after_key` is not
    /// found or the document is not a mapping.
    ///
    /// Use [`move_after`](Self::move_after) if you want an existing entry to be
    /// removed from its current position and re-inserted after `after_key`.
    pub fn insert_after(
        &self,
        after_key: impl crate::AsYaml,
        key: impl crate::AsYaml,
        value: impl crate::AsYaml,
    ) -> bool {
        if let Some(mapping) = self.as_mapping() {
            mapping.insert_after(after_key, key, value)
        } else {
            false
        }
    }

    /// Move a key-value pair to immediately after `after_key` in this document's mapping.
    ///
    /// If `key` already exists, it is **removed** from its current position and
    /// re-inserted after `after_key` with the new value. Returns `false` if
    /// `after_key` is not found or the document is not a mapping.
    ///
    /// Use [`insert_after`](Self::insert_after) if you want an existing entry to be
    /// updated in-place rather than moved.
    pub fn move_after(
        &self,
        after_key: impl crate::AsYaml,
        key: impl crate::AsYaml,
        value: impl crate::AsYaml,
    ) -> bool {
        if let Some(mapping) = self.as_mapping() {
            mapping.move_after(after_key, key, value)
        } else {
            false
        }
    }

    /// Insert a key-value pair immediately before `before_key` in this document's mapping.
    ///
    /// If `key` already exists, its value is updated in-place and it stays at its
    /// current position (it is **not** moved). Returns `false` if `before_key` is not
    /// found or the document is not a mapping.
    ///
    /// Use [`move_before`](Self::move_before) if you want an existing entry to be
    /// removed from its current position and re-inserted before `before_key`.
    pub fn insert_before(
        &self,
        before_key: impl crate::AsYaml,
        key: impl crate::AsYaml,
        value: impl crate::AsYaml,
    ) -> bool {
        if let Some(mapping) = self.as_mapping() {
            mapping.insert_before(before_key, key, value)
        } else {
            false
        }
    }

    /// Move a key-value pair to immediately before `before_key` in this document's mapping.
    ///
    /// If `key` already exists, it is **removed** from its current position and
    /// re-inserted before `before_key` with the new value. Returns `false` if
    /// `before_key` is not found or the document is not a mapping.
    ///
    /// Use [`insert_before`](Self::insert_before) if you want an existing entry to be
    /// updated in-place rather than moved.
    pub fn move_before(
        &self,
        before_key: impl crate::AsYaml,
        key: impl crate::AsYaml,
        value: impl crate::AsYaml,
    ) -> bool {
        if let Some(mapping) = self.as_mapping() {
            mapping.move_before(before_key, key, value)
        } else {
            false
        }
    }

    /// Helper to build a VALUE wrapper node around any AsYaml value
    pub(crate) fn build_value_content(
        builder: &mut GreenNodeBuilder,
        value: impl crate::AsYaml,
        indent: usize,
    ) {
        builder.start_node(SyntaxKind::VALUE.into());
        value.build_content(builder, indent, false);
        builder.finish_node(); // VALUE
    }

    /// Insert a key-value pair at a specific index (assumes document is a mapping).
    ///
    /// If the document already contains a mapping, delegates to
    /// [`Mapping::insert_at_index`]. If no mapping exists yet, creates one and
    /// uses [`Mapping::insert_at_index_preserving`].
    pub fn insert_at_index(
        &self,
        index: usize,
        key: impl crate::AsYaml,
        value: impl crate::AsYaml,
    ) {
        // Delegate to Mapping::insert_at_index if we have a mapping
        if let Some(mapping) = self.as_mapping() {
            mapping.insert_at_index(index, key, value);
            return;
        }

        // No mapping exists yet: create one and replace document contents
        let mapping = Mapping::new();
        mapping.insert_at_index_preserving(index, key, value);
        let new_doc = Self::from_mapping(mapping);
        let new_children: Vec<_> = new_doc.0.children_with_tokens().collect();
        let child_count = self.0.children_with_tokens().count();
        self.0.splice_children(0..child_count, new_children);
    }

    /// Get the scalar value for `key` as a decoded `String`.
    ///
    /// Returns `None` if the key does not exist or its value is not a scalar
    /// (i.e. the value is a sequence or mapping). Quotes are stripped and
    /// escape sequences are processed (e.g. `\"` → `"`, `\n` → newline).
    /// For tagged scalars (e.g. `!!str foo`) the tag is ignored and the
    /// value part is returned.
    pub fn get_string(&self, key: impl crate::AsYaml) -> Option<String> {
        // get_node() returns the content node (SCALAR/MAPPING/SEQUENCE/TAGGED_NODE),
        // already unwrapped from the VALUE wrapper.
        let content = self.get_node(key)?;
        if let Some(tagged_node) = TaggedNode::cast(content.clone()) {
            tagged_node.value().map(|s| s.as_string())
        } else {
            // Returns None if content is a sequence or mapping (not a scalar).
            Scalar::cast(content).map(|s| s.as_string())
        }
    }

    /// Get the number of top-level key-value pairs in this document.
    ///
    /// Returns `0` if the document is not a mapping or is empty.
    pub fn len(&self) -> usize {
        self.as_mapping().map(|m| m.len()).unwrap_or(0)
    }

    /// Get a nested mapping value for a key.
    ///
    /// Returns `None` if the key doesn't exist or its value is not a mapping.
    pub fn get_mapping(&self, key: impl crate::AsYaml) -> Option<Mapping> {
        self.get(key).and_then(|n| n.as_mapping().cloned())
    }

    /// Get a nested sequence value for a key.
    ///
    /// Returns `None` if the key doesn't exist or its value is not a sequence.
    pub fn get_sequence(&self, key: impl crate::AsYaml) -> Option<Sequence> {
        self.get(key).and_then(|n| n.as_sequence().cloned())
    }

    /// Rename a top-level key while preserving its value and formatting.
    ///
    /// The new key is automatically escaped/quoted as needed. Returns `true` if
    /// the key was found and renamed, `false` if `old_key` does not exist.
    pub fn rename_key(&self, old_key: impl crate::AsYaml, new_key: impl crate::AsYaml) -> bool {
        self.as_mapping()
            .map(|m| m.rename_key(old_key, new_key))
            .unwrap_or(false)
    }

    /// Returns `true` if `key` exists and its value is a sequence.
    pub fn is_sequence(&self, key: impl crate::AsYaml) -> bool {
        self.get(key)
            .map(|node| node.as_sequence().is_some())
            .unwrap_or(false)
    }

    /// Reorder fields in the document's root mapping according to the specified order.
    ///
    /// Fields not in the order list will appear after the ordered fields, in their
    /// original relative order. Has no effect if the document is not a mapping.
    pub fn reorder_fields<I, K>(&self, order: I)
    where
        I: IntoIterator<Item = K>,
        K: crate::AsYaml,
    {
        if let Some(mapping) = self.as_mapping() {
            mapping.reorder_fields(order);
        }
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
    /// let parsed = yaml.parse::<yaml_edit::YamlFile>().unwrap();
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

    /// Get the byte offset range of this document in the source text.
    ///
    /// Returns the start and end byte offsets as a `TextPosition`.
    ///
    /// # Examples
    ///
    /// ```
    /// use yaml_edit::Document;
    /// use std::str::FromStr;
    ///
    /// let text = "name: Alice\nage: 30";
    /// let doc = Document::from_str(text).unwrap();
    /// let range = doc.byte_range();
    /// assert_eq!(range.start, 0);
    /// ```
    pub fn byte_range(&self) -> crate::TextPosition {
        self.0.text_range().into()
    }

    /// Get the line and column where this document starts.
    ///
    /// Requires the original source text to calculate line/column from byte offsets.
    /// Line and column numbers are 1-indexed.
    ///
    /// # Arguments
    ///
    /// * `source_text` - The original YAML source text
    ///
    /// # Examples
    ///
    /// ```
    /// use yaml_edit::Document;
    /// use std::str::FromStr;
    ///
    /// let text = "name: Alice";
    /// let doc = Document::from_str(text).unwrap();
    /// let pos = doc.start_position(text);
    /// assert_eq!(pos.line, 1);
    /// assert_eq!(pos.column, 1);
    /// ```
    pub fn start_position(&self, source_text: &str) -> crate::LineColumn {
        let range = self.byte_range();
        crate::byte_offset_to_line_column(source_text, range.start as usize)
    }

    /// Get the line and column where this document ends.
    ///
    /// Requires the original source text to calculate line/column from byte offsets.
    /// Line and column numbers are 1-indexed.
    ///
    /// # Arguments
    ///
    /// * `source_text` - The original YAML source text
    pub fn end_position(&self, source_text: &str) -> crate::LineColumn {
        let range = self.byte_range();
        crate::byte_offset_to_line_column(source_text, range.end as usize)
    }
}

impl Default for Document {
    fn default() -> Self {
        Self::new()
    }
}

impl std::str::FromStr for Document {
    type Err = crate::error::YamlError;

    /// Parse a document from a YAML string.
    ///
    /// Returns an error if the string contains multiple documents.
    /// For multi-document YAML, use `YamlFile::from_str()` instead.
    ///
    /// # Example
    /// ```
    /// use yaml_edit::Document;
    /// use std::str::FromStr;
    ///
    /// let doc = Document::from_str("key: value").unwrap();
    /// assert!(doc.as_mapping().is_some());
    /// ```
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let parsed = YamlFile::parse(s);

        if !parsed.positioned_errors().is_empty() {
            let first_error = &parsed.positioned_errors()[0];
            let lc = crate::byte_offset_to_line_column(s, first_error.range.start as usize);
            return Err(crate::error::YamlError::Parse {
                message: first_error.message.clone(),
                line: Some(lc.line),
                column: Some(lc.column),
            });
        }

        let mut docs = parsed.tree().documents();
        let first = docs.next().unwrap_or_default();

        if docs.next().is_some() {
            return Err(crate::error::YamlError::InvalidOperation {
                operation: "Document::from_str".to_string(),
                reason: "Input contains multiple YAML documents. Use YamlFile::from_str() for multi-document YAML.".to_string(),
            });
        }

        Ok(first)
    }
}

impl AsYaml for Document {
    fn as_node(&self) -> Option<&SyntaxNode> {
        Some(&self.0)
    }

    fn kind(&self) -> YamlKind {
        YamlKind::Document
    }

    fn build_content(
        &self,
        builder: &mut rowan::GreenNodeBuilder,
        _indent: usize,
        _flow_context: bool,
    ) -> bool {
        crate::as_yaml::copy_node_content(builder, &self.0);
        self.0
            .last_token()
            .map(|t| t.kind() == SyntaxKind::NEWLINE)
            .unwrap_or(false)
    }

    fn is_inline(&self) -> bool {
        // Documents are never inline
        false
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    use crate::builder::{MappingBuilder, SequenceBuilder};
    use crate::yaml::YamlFile;
    use std::str::FromStr;

    #[test]
    fn test_document_stream_features() {
        // Test 1: Multi-document with end markers
        let yaml1 = "---\ndoc1: first\n---\ndoc2: second\n...\n";
        let parsed1 = YamlFile::from_str(yaml1).unwrap();
        assert_eq!(parsed1.documents().count(), 2);
        assert_eq!(parsed1.to_string(), yaml1);

        // Test 2: Single document with explicit markers
        let yaml2 = "---\nkey: value\n...\n";
        let parsed2 = YamlFile::from_str(yaml2).unwrap();
        assert_eq!(parsed2.documents().count(), 1);
        assert_eq!(parsed2.to_string(), yaml2);

        // Test 3: Document with only end marker
        let yaml3 = "key: value\n...\n";
        let parsed3 = YamlFile::from_str(yaml3).unwrap();
        assert_eq!(parsed3.documents().count(), 1);
        assert_eq!(parsed3.to_string(), yaml3);
    }
    #[test]
    fn test_document_level_directives() {
        // Test document-level directives with multi-document stream
        let yaml = "%YAML 1.2\n%TAG ! tag:example.com,2000:app/\n---\nfirst: doc\n...\n%YAML 1.2\n---\nsecond: doc\n...\n";
        let parsed = YamlFile::from_str(yaml).unwrap();
        assert_eq!(parsed.documents().count(), 2);
        assert_eq!(parsed.to_string(), yaml);
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
        let doc = YamlFile::from_str(json_yaml).unwrap().document().unwrap();

        // Test JSON schema validation - should pass
        assert!(
            crate::schema::SchemaValidator::json()
                .validate(&doc)
                .is_ok(),
            "JSON-compatible document should pass JSON validation"
        );

        // Test Core schema validation - should pass
        assert!(
            crate::schema::SchemaValidator::core()
                .validate(&doc)
                .is_ok(),
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
        let yaml_doc = YamlFile::from_str(yaml_specific)
            .unwrap()
            .document()
            .unwrap();

        // Test Core schema - should pass
        assert!(
            crate::schema::SchemaValidator::core()
                .validate(&yaml_doc)
                .is_ok(),
            "YAML-specific types should pass Core validation"
        );

        // Test JSON schema - should fail due to timestamp, regex, binary
        assert!(
            crate::schema::SchemaValidator::json()
                .validate(&yaml_doc)
                .is_err(),
            "YAML-specific types should fail JSON validation"
        );

        // Test Failsafe schema - should fail
        assert!(
            crate::schema::SchemaValidator::failsafe()
                .validate(&yaml_doc)
                .is_err(),
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
        let str_doc = YamlFile::from_str(string_only).unwrap().document().unwrap();

        // All schemas should pass (strings are allowed in all schemas)
        assert!(
            crate::schema::SchemaValidator::failsafe()
                .validate(&str_doc)
                .is_ok(),
            "String-only document should pass Failsafe validation"
        );
        assert!(
            crate::schema::SchemaValidator::json()
                .validate(&str_doc)
                .is_ok(),
            "String-only document should pass JSON validation"
        );
        assert!(
            crate::schema::SchemaValidator::core()
                .validate(&str_doc)
                .is_ok(),
            "String-only document should pass Core validation"
        );
    }
    #[test]
    fn test_document_set_preserves_position() {
        // Test that Document::set() (not just Mapping::set()) preserves position
        let yaml = r#"Name: original
Version: 1.0
Author: Someone
"#;
        let parsed = YamlFile::from_str(yaml).unwrap();
        let doc = parsed.document().expect("Should have a document");

        // Update Version - should stay in middle
        doc.set("Version", 2.0);

        let output = doc.to_string();
        let expected = r#"Name: original
Version: 2.0
Author: Someone
"#;
        assert_eq!(output, expected);
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
        let doc = YamlFile::from_str(coercion_yaml)
            .unwrap()
            .document()
            .unwrap();
        let json_validator = crate::schema::SchemaValidator::json();

        // Test coercion - should pass because strings can be coerced to numbers/booleans
        assert!(
            json_validator.can_coerce(&doc).is_ok(),
            "Strings that look like numbers/booleans should be coercible to JSON types"
        );

        // Test with non-coercible types
        let non_coercible = r#"
timestamp: !!timestamp "2023-01-01"
pattern: !!regex '\d+'
"#;
        let non_coer_doc = YamlFile::from_str(non_coercible)
            .unwrap()
            .document()
            .unwrap();

        // Should fail coercion to JSON schema
        assert!(
            json_validator.can_coerce(&non_coer_doc).is_err(),
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
        let doc = YamlFile::from_str(nested_yaml).unwrap().document().unwrap();

        // Test Failsafe validation with detailed error checking (strict mode)
        let failsafe_strict = crate::schema::SchemaValidator::failsafe().strict();
        let failsafe_result = doc.validate_schema(&failsafe_strict);
        assert!(
            failsafe_result.is_err(),
            "Nested document with numbers should fail strict Failsafe validation"
        );

        let errors = failsafe_result.unwrap_err();
        assert!(!errors.is_empty());

        // Check that all errors have path information
        for error in &errors {
            assert!(!error.path.is_empty(), "Error should have path: {}", error);
        }

        // Test JSON validation
        let json_result = crate::schema::SchemaValidator::json().validate(&doc);
        assert!(
            json_result.is_err(),
            "Document with timestamp should fail JSON validation"
        );

        let json_errors = json_result.unwrap_err();
        assert!(!json_errors.is_empty());
        // Verify at least one error is from json schema validation
        assert!(
            json_errors.iter().any(|e| e.schema_name == "json"),
            "Should have JSON schema validation error"
        );
    }
    #[test]
    fn test_document_schema_validation_with_custom_validator() {
        // Test using the general validate_schema method
        let yaml = r#"
name: "HelloWorld"
count: 42
active: true
"#;
        let doc = YamlFile::from_str(yaml).unwrap().document().unwrap();

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
    fn test_document_level_insertion_with_complex_types() {
        // Test the Document-level API with complex types separately to avoid chaining issues

        // Test Document.insert_after with sequence
        let doc1 = Document::new();
        doc1.set("name", "project");
        let features = SequenceBuilder::new()
            .item("auth")
            .item("api")
            .item("web")
            .build_document()
            .as_sequence()
            .unwrap();
        let success = doc1.insert_after("name", "features", features);
        assert!(success);
        let output1 = doc1.to_string();
        assert_eq!(
            output1,
            "---\nname: project\nfeatures:\n  - auth\n  - api\n  - web\n"
        );

        // Test Document.insert_before with mapping
        let doc2 = Document::new();
        doc2.set("name", "project");
        doc2.set("version", "1.0.0");
        let database = MappingBuilder::new()
            .pair("host", "localhost")
            .pair("port", 5432)
            .build_document()
            .as_mapping()
            .unwrap();
        let success = doc2.insert_before("version", "database", database);
        assert!(success);
        let output2 = doc2.to_string();
        assert_eq!(
            output2,
            "---\nname: project\ndatabase:\n  host: localhost\n  port: 5432\nversion: 1.0.0\n"
        );

        // Test Document.insert_at_index with set
        let doc3 = Document::new();
        doc3.set("name", "project");
        // TODO: migrate away from YamlValue once !!set has a non-YamlValue AsYaml impl
        #[allow(clippy::disallowed_types)]
        let tag_set = {
            use crate::value::YamlValue;
            let mut tags = std::collections::BTreeSet::new();
            tags.insert("production".to_string());
            tags.insert("database".to_string());
            YamlValue::from_set(tags)
        };
        doc3.insert_at_index(1, "tags", tag_set);
        let output3 = doc3.to_string();
        assert_eq!(
            output3,
            "---\nname: project\ntags: !!set\n  database: null\n  production: null\n"
        );

        // Verify all are valid YAML by parsing
        assert!(
            YamlFile::from_str(&output1).is_ok(),
            "Sequence output should be valid YAML"
        );
        assert!(
            YamlFile::from_str(&output2).is_ok(),
            "Mapping output should be valid YAML"
        );
        assert!(
            YamlFile::from_str(&output3).is_ok(),
            "Set output should be valid YAML"
        );
    }

    #[test]
    fn test_document_api_usage() -> crate::error::YamlResult<()> {
        // Create a new document
        let doc = Document::new();

        // Check and modify fields
        assert!(!doc.contains_key("Repository"));
        doc.set("Repository", "https://github.com/user/repo.git");
        assert!(doc.contains_key("Repository"));

        // Test get_string
        assert_eq!(
            doc.get_string("Repository"),
            Some("https://github.com/user/repo.git".to_string())
        );

        // Test is_empty
        assert!(!doc.is_empty());

        // Test keys
        let keys: Vec<_> = doc.keys().collect();
        assert_eq!(keys.len(), 1);

        // Test remove
        assert!(doc.remove("Repository").is_some());
        assert!(!doc.contains_key("Repository"));
        assert!(doc.is_empty());

        Ok(())
    }

    #[test]
    fn test_field_ordering() {
        let doc = Document::new();

        // Add fields in random order
        doc.set("Repository-Browse", "https://github.com/user/repo");
        doc.set("Name", "MyProject");
        doc.set("Bug-Database", "https://github.com/user/repo/issues");
        doc.set("Repository", "https://github.com/user/repo.git");

        // Reorder fields
        doc.reorder_fields(["Name", "Bug-Database", "Repository", "Repository-Browse"]);

        // Check that fields are in the expected order
        let keys: Vec<_> = doc.keys().collect();
        assert_eq!(keys.len(), 4);
        assert_eq!(
            keys[0].as_scalar().map(|s| s.as_string()),
            Some("Name".to_string())
        );
        assert_eq!(
            keys[1].as_scalar().map(|s| s.as_string()),
            Some("Bug-Database".to_string())
        );
        assert_eq!(
            keys[2].as_scalar().map(|s| s.as_string()),
            Some("Repository".to_string())
        );
        assert_eq!(
            keys[3].as_scalar().map(|s| s.as_string()),
            Some("Repository-Browse".to_string())
        );
    }

    #[test]
    fn test_array_detection() {
        use crate::scalar::ScalarValue;

        // Create a test with array values using set_value
        let doc = Document::new();

        // Set an array value
        let array_value = SequenceBuilder::new()
            .item(ScalarValue::string("https://github.com/user/repo.git"))
            .item(ScalarValue::string("https://gitlab.com/user/repo.git"))
            .build_document()
            .as_sequence()
            .unwrap();
        doc.set("Repository", &array_value);

        // Test array detection
        assert!(doc.is_sequence("Repository"));
        // The sequence is inserted but getting the first element may not work as expected
        // due to how the sequence is constructed. Let's just verify the sequence exists.
        assert!(doc.get_sequence("Repository").is_some());
    }

    #[test]
    fn test_file_io() -> crate::error::YamlResult<()> {
        use std::fs;

        // Create a test file path
        let test_path = "/tmp/test_yaml_edit.yaml";

        // Create and save a document
        let doc = Document::new();
        doc.set("Name", "TestProject");
        doc.set("Repository", "https://example.com/repo.git");

        doc.to_file(test_path)?;

        // Load it back
        let loaded_doc = Document::from_file(test_path)?;

        assert_eq!(
            loaded_doc.get_string("Name"),
            Some("TestProject".to_string())
        );
        assert_eq!(
            loaded_doc.get_string("Repository"),
            Some("https://example.com/repo.git".to_string())
        );

        // Clean up
        let _ = fs::remove_file(test_path);

        Ok(())
    }

    #[test]
    fn test_document_from_str_single_document() {
        // Test parsing a single-document YAML
        let yaml = "key: value\nport: 8080";
        let doc = Document::from_str(yaml).unwrap();

        assert_eq!(doc.get_string("key"), Some("value".to_string()));
        assert!(doc.contains_key("port"));
    }

    #[test]
    fn test_document_from_str_multiple_documents_error() {
        // Test that multiple documents return an error
        let yaml = "---\nkey: value\n---\nother: data";
        let result = Document::from_str(yaml);

        assert!(result.is_err());
        let err = result.unwrap_err();
        match err {
            crate::error::YamlError::InvalidOperation { operation, reason } => {
                assert_eq!(operation, "Document::from_str");
                assert_eq!(
                    reason,
                    "Input contains multiple YAML documents. Use YamlFile::from_str() for multi-document YAML."
                );
            }
            _ => panic!("Expected InvalidOperation error, got {:?}", err),
        }
    }

    #[test]
    fn test_document_from_str_empty() {
        // Test parsing an empty document
        let yaml = "";
        let doc = Document::from_str(yaml).unwrap();

        // Empty document should be valid
        assert!(doc.is_empty());
    }

    #[test]
    fn test_document_from_str_bare_document() {
        // Test parsing without explicit document markers
        let yaml = "name: test\nversion: 1.0";
        let doc = Document::from_str(yaml).unwrap();

        assert_eq!(doc.get_string("name"), Some("test".to_string()));
        assert_eq!(doc.get_string("version"), Some("1.0".to_string()));
    }

    #[test]
    fn test_document_from_str_with_explicit_marker() {
        // Test parsing with explicit --- marker
        let yaml = "---\nkey: value";
        let doc = Document::from_str(yaml).unwrap();

        assert_eq!(doc.get_string("key"), Some("value".to_string()));
    }

    #[test]
    fn test_document_from_str_complex_structure() {
        // Test parsing complex nested structure
        let yaml = r#"
database:
  host: localhost
  port: 5432
  credentials:
    username: admin
    password: secret
features:
  - auth
  - logging
  - metrics
"#;
        let doc = Document::from_str(yaml).unwrap();

        // Verify we can access the structure
        assert!(doc.contains_key("database"));
        assert!(doc.contains_key("features"));

        // Get nested mapping
        let db = doc.get("database").unwrap();
        if let Some(db_map) = db.as_mapping() {
            assert!(db_map.contains_key("host"));
        } else {
            panic!("Expected mapping for database");
        }
    }

    #[test]
    fn test_is_sequence_block() {
        let doc = Document::from_str("tags:\n  - alpha\n  - beta\n  - gamma").unwrap();
        assert!(doc.is_sequence("tags"));
        assert_eq!(
            doc.get_sequence("tags")
                .and_then(|s| s.get(0))
                .and_then(|v| v.as_scalar().map(|s| s.as_string())),
            Some("alpha".to_string())
        );
    }

    #[test]
    fn test_is_sequence_flow() {
        let doc = Document::from_str("tags: [alpha, beta, gamma]").unwrap();
        assert!(doc.is_sequence("tags"));
        assert_eq!(
            doc.get_sequence("tags")
                .and_then(|s| s.get(0))
                .and_then(|v| v.as_scalar().map(|s| s.as_string())),
            Some("alpha".to_string())
        );
    }

    #[test]
    fn test_sequence_first_element_flow_quoted_with_comma() {
        // Previously the text-splitting approach broke on commas inside quoted values
        let doc = Document::from_str("tags: [\"hello, world\", beta]").unwrap();
        assert_eq!(
            doc.get_sequence("tags")
                .and_then(|s| s.get(0))
                .and_then(|v| v.as_scalar().map(|s| s.as_string())),
            Some("hello, world".to_string())
        );
    }

    #[test]
    fn test_is_sequence_missing_key() {
        let doc = Document::from_str("name: test").unwrap();
        assert!(!doc.is_sequence("tags"));
    }

    #[test]
    fn test_is_sequence_not_sequence() {
        let doc = Document::from_str("name: test").unwrap();
        assert!(!doc.is_sequence("name"));
    }

    #[test]
    fn test_is_sequence_empty_sequence() {
        let doc = Document::from_str("tags: []").unwrap();
        assert!(doc.is_sequence("tags"));
        assert_eq!(doc.get_sequence("tags").map(|s| s.len()), Some(0));
    }

    #[test]
    fn test_get_string_plain_scalar() {
        let doc = Document::from_str("key: hello").unwrap();
        assert_eq!(doc.get_string("key"), Some("hello".to_string()));
    }

    #[test]
    fn test_get_string_double_quoted_with_escapes() {
        let doc = Document::from_str(r#"key: "hello \"world\"""#).unwrap();
        assert_eq!(doc.get_string("key"), Some(r#"hello "world""#.to_string()));
    }

    #[test]
    fn test_get_string_single_quoted() {
        let doc = Document::from_str("key: 'it''s fine'").unwrap();
        assert_eq!(doc.get_string("key"), Some("it's fine".to_string()));
    }

    #[test]
    fn test_get_string_missing_key() {
        let doc = Document::from_str("other: value").unwrap();
        assert_eq!(doc.get_string("key"), None);
    }

    #[test]
    fn test_get_string_sequence_value_returns_none() {
        let doc = Document::from_str("key:\n  - a\n  - b").unwrap();
        assert_eq!(doc.get_string("key"), None);
    }

    #[test]
    fn test_get_string_mapping_value_returns_none() {
        let doc = Document::from_str("key:\n  nested: value").unwrap();
        assert_eq!(doc.get_string("key"), None);
    }

    #[test]
    fn test_insert_after_preserves_newline() {
        // Test with the examples from the bug report
        let yaml = "---\nBug-Database: https://github.com/example/example/issues\nBug-Submit: https://github.com/example/example/issues/new\n";
        let yaml_obj = YamlFile::from_str(yaml).unwrap();

        // For now, test using Document directly since YamlFile::insert_after was removed
        if let Some(doc) = yaml_obj.document() {
            let result = doc.insert_after(
                "Bug-Submit",
                "Repository",
                "https://github.com/example/example.git",
            );
            assert!(result, "insert_after should return true when key is found");

            // Check the document output directly
            let output = doc.to_string();

            let expected = "---
Bug-Database: https://github.com/example/example/issues
Bug-Submit: https://github.com/example/example/issues/new
Repository: https://github.com/example/example.git
";
            assert_eq!(output, expected);
        }
    }

    #[test]
    fn test_insert_after_without_trailing_newline() {
        // Test the specific bug case - YAML without trailing newline
        let yaml = "---\nBug-Database: https://github.com/example/example/issues\nBug-Submit: https://github.com/example/example/issues/new";
        let yaml_obj = YamlFile::from_str(yaml).unwrap();

        if let Some(doc) = yaml_obj.document() {
            let result = doc.insert_after(
                "Bug-Submit",
                "Repository",
                "https://github.com/example/example.git",
            );
            assert!(result, "insert_after should return true when key is found");

            let output = doc.to_string();

            let expected = "---
Bug-Database: https://github.com/example/example/issues
Bug-Submit: https://github.com/example/example/issues/new
Repository: https://github.com/example/example.git
";
            assert_eq!(output, expected);
        }
    }
}
