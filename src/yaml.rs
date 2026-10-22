//! Lossless YAML parser and editor.

use crate::{lex::SyntaxKind, parse::Parse, PositionedParseError};
use rowan::ast::AstNode;
use rowan::GreenNodeBuilder;
use std::path::Path;
use std::str::FromStr;

/// The raw result of parsing a YAML file, before it is wrapped in a [`YamlFile`] node.
///
/// Contains the green CST root and any errors encountered during parsing.
/// Most callers should use [`YamlFile::parse`] instead, which returns a [`Parse<YamlFile>`]
/// wrapper with the same information in a more ergonomic form.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct ParsedYaml {
    /// The immutable green-tree root produced by the parser.
    pub(crate) green_node: rowan::GreenNode,
    /// Structured parse errors with source positions.
    pub(crate) positioned_errors: Vec<PositionedParseError>,
}

// Import Lang, SyntaxNode, and ast_node! macro from nodes module
use crate::nodes::{ast_node, has_child_token, SyntaxToken};
pub use crate::nodes::{Lang, SyntaxNode};

// Re-export extracted AST nodes from nodes module
pub use crate::nodes::{
    Alias, Comment, CommentTextError, Directive, Document, Entry, Mapping, MappingEntry,
    OccupiedEntry, Scalar, ScalarConversionError, Sequence, TaggedNode, VacantEntry,
};

ast_node!(
    YamlFile,
    ROOT,
    "A YAML file containing one or more documents"
);

impl YamlFile {
    /// Capture an independent snapshot of this YAML file.
    ///
    /// The returned value shares the underlying immutable green-node data
    /// with `self` at the time of the call, but lives in its own mutable
    /// tree: subsequent mutations to `self` do not propagate to the snapshot.
    /// Pair with [`Self::tree_eq`] to detect later mutations.
    pub fn snapshot(&self) -> Self {
        YamlFile(SyntaxNode::new_root_mut(self.0.green().into_owned()))
    }

    /// Returns true iff the syntax trees of `self` and `other` are
    /// value-equal. An O(1) pointer-identity fast path makes this free for
    /// trees that still share state with a recent `snapshot()`.
    pub fn tree_eq(&self, other: &Self) -> bool {
        let a = self.0.green();
        let b = other.0.green();
        let a_ref: &rowan::GreenNodeData = &a;
        let b_ref: &rowan::GreenNodeData = &b;
        std::ptr::eq(a_ref as *const _, b_ref as *const _) || a_ref == b_ref
    }
}

/// Trait for value nodes (Mapping, Sequence, Scalar) with inline detection
pub trait ValueNode: rowan::ast::AstNode<Language = Lang> {
    /// Returns whether this value should be rendered inline
    fn is_inline(&self) -> bool;
}

impl ValueNode for Mapping {
    fn is_inline(&self) -> bool {
        // Flow-style mappings contain brace tokens.
        has_child_token(&self.0, |k| {
            matches!(k, SyntaxKind::LEFT_BRACE | SyntaxKind::RIGHT_BRACE)
        })
    }
}

impl ValueNode for Sequence {
    fn is_inline(&self) -> bool {
        // Flow-style sequences contain bracket tokens.
        has_child_token(&self.0, |k| {
            matches!(k, SyntaxKind::LEFT_BRACKET | SyntaxKind::RIGHT_BRACKET)
        })
    }
}

impl ValueNode for Scalar {
    fn is_inline(&self) -> bool {
        // Block scalars (literal `|` and folded `>`) span multiple lines
        // starting on a NEW line after their indicator, so they don't
        // render inline. Everything else - plain, quoted, tagged scalars -
        // sits on the same line as its key.
        !crate::nodes::is_block_scalar(&self.0)
    }
}

impl ValueNode for TaggedNode {
    fn is_inline(&self) -> bool {
        // A tagged node is inline iff the value it wraps is inline. Look
        // for the first NEWLINE inside; if it appears before the wrapped
        // node, the value sits on the next line (block form).
        let mut seen_newline = false;
        for child in self.0.children_with_tokens() {
            match child {
                rowan::NodeOrToken::Token(t) if t.kind() == SyntaxKind::NEWLINE => {
                    seen_newline = true;
                }
                rowan::NodeOrToken::Node(n) => {
                    if seen_newline {
                        return false;
                    }
                    return match n.kind() {
                        SyntaxKind::MAPPING => Mapping::cast(n).is_some_and(|m| m.is_inline()),
                        SyntaxKind::SEQUENCE => Sequence::cast(n).is_some_and(|s| s.is_inline()),
                        SyntaxKind::SCALAR => Scalar::cast(n).is_some_and(|s| s.is_inline()),
                        _ => true,
                    };
                }
                _ => {}
            }
        }
        true
    }
}

/// Move the comment and blank lines at the start of a block collection out
/// in front of it, where the parser puts the lines before a collection's
/// first entry. They are left there when the first entry is removed.
///
/// A collection that started on the line of a `- ` or `? ` indicator then
/// starts on the line after it, so that the lines do not join the
/// indicator's: `- a: 1\n  # c\n  b: 2\n` with `a` removed gives
/// `-\n  # c\n  b: 2\n`.
pub(crate) fn move_leading_lines_out(collection: &SyntaxNode) {
    let leading: Vec<_> = collection
        .children_with_tokens()
        .map_while(|el| el.into_token())
        .take_while(|t| {
            matches!(
                t.kind(),
                SyntaxKind::COMMENT
                    | SyntaxKind::INDENT
                    | SyntaxKind::WHITESPACE
                    | SyntaxKind::NEWLINE
            )
        })
        .collect();
    if leading.is_empty() {
        return;
    }
    // A detached collection has nothing to move the lines into.
    let Some(parent) = collection.parent() else {
        return;
    };
    let starts_with_comment = leading[0].kind() == SyntaxKind::COMMENT;
    let mut lines: Vec<rowan::NodeOrToken<SyntaxNode, rowan::SyntaxToken<Lang>>> = Vec::new();
    match leading[0].prev_token() {
        // The space after a `- ` or `? ` indicator.
        Some(space) if space.kind() == SyntaxKind::WHITESPACE => {
            space.detach();
            lines.push(crate::nodes::fresh_token(SyntaxKind::NEWLINE, "\n").into());
            // A comment that ended the removed entry's line has no indent
            // of its own; it takes the entries' column.
            let column = leading
                .iter()
                .rev()
                .find(|t| t.kind() == SyntaxKind::INDENT)
                .filter(|_| starts_with_comment);
            if let Some(column) = column {
                lines.push(crate::nodes::fresh_token(SyntaxKind::INDENT, column.text()).into());
            }
        }
        // The first line has an indent of its own, or is blank.
        Some(indent) if indent.kind() == SyntaxKind::INDENT && !starts_with_comment => {
            indent.detach();
        }
        _ => {}
    }
    lines.extend(
        leading
            .iter()
            .map(|t| crate::nodes::fresh_token(t.kind(), t.text()).into()),
    );
    crate::nodes::replace_children(collection, 0..leading.len(), vec![]);
    // Only the stream's first document holds the lines before its root;
    // after a `...` or another document, they are the stream's unless a
    // `---` comes first.
    let (host, at) = match parent.parent() {
        Some(root)
            if parent.kind() == SyntaxKind::DOCUMENT
                && parent.prev_sibling_or_token().is_some()
                && collection.prev_sibling_or_token().is_none() =>
        {
            (root, parent.index())
        }
        _ => (parent, collection.index()),
    };
    crate::nodes::insert_children(&host, at, lines);
}

/// Whether `indent` starts a comment line.
pub(crate) fn indents_comment_line(indent: &rowan::SyntaxToken<Lang>) -> bool {
    indent
        .next_token()
        .is_some_and(|t| t.kind() == SyntaxKind::COMMENT)
}

/// The byte order mark `node` starts with, if any.
fn leading_bom(node: &SyntaxNode) -> Option<SyntaxToken> {
    node.first_child_or_token()?
        .into_token()
        .filter(|t| t.kind() == SyntaxKind::BOM)
}

/// A virtual AST node for YAML sets (!!set tagged scalars)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Set(SyntaxNode);

impl Set {
    /// Cast a SyntaxNode to a Set only if it's a TAGGED_NODE with !!set tag
    /// whose content is a mapping.
    ///
    /// A `!!set` on anything else is not a valid set and has no members;
    /// the validator reports it.
    pub fn cast(node: SyntaxNode) -> Option<Self> {
        let tagged_node = TaggedNode::cast(node.clone())?;
        if tagged_node.resolved_tag().as_deref() == Some(crate::tag::SET)
            && tagged_node.as_mapping().is_some()
        {
            Some(Set(node))
        } else {
            None
        }
    }

    /// Return the inner `Mapping` of this set (the !!set body).
    fn inner_mapping(&self) -> Option<Mapping> {
        self.0.children().find_map(Mapping::cast)
    }

    /// Iterate over the set members, preserving formatting.
    ///
    /// Each member is yielded as a [`YamlNode`](crate::YamlNode) wrapping the
    /// key content node (the scalar, mapping, or sequence that forms the set
    /// member). The iteration order follows the document order.
    ///
    /// To compare members semantically (ignoring quoting style), use
    /// [`yaml_eq`](crate::yaml_eq) on the returned nodes.
    pub fn members(&self) -> impl Iterator<Item = crate::as_yaml::YamlNode> + '_ {
        // The members are the keys of the mapping, each with a null value.
        self.inner_mapping()
            .into_iter()
            .flat_map(|m| m.keys().collect::<Vec<_>>())
    }

    /// Get the number of members in the set.
    pub fn len(&self) -> usize {
        self.members().count()
    }

    /// Check if the set is empty.
    pub fn is_empty(&self) -> bool {
        self.members().next().is_none()
    }

    /// Check if the set contains a specific value.
    pub fn contains(&self, value: impl crate::AsYaml) -> bool {
        self.members()
            .any(|member| crate::as_yaml::yaml_eq(&member, &value))
    }
}

// CST abstraction layer - hides wrapper node complexity

/// Extract the actual content from a VALUE or KEY wrapper node
fn extract_content_node(wrapper: &SyntaxNode) -> Option<SyntaxNode> {
    use crate::lex::SyntaxKind;
    match wrapper.kind() {
        SyntaxKind::VALUE | SyntaxKind::KEY => wrapper.children().next(),
        _ => Some(wrapper.clone()),
    }
}

/// Cast the content of `node` to `T`, looking through a VALUE or KEY
/// wrapper.
pub(crate) fn extract<T: AstNode<Language = Lang>>(node: &SyntaxNode) -> Option<T> {
    extract_content_node(node).and_then(T::cast)
}

/// Copy a syntax node and all its children recursively to a builder.
pub(crate) fn copy_node_to_builder(builder: &mut GreenNodeBuilder, node: &SyntaxNode) {
    builder.start_node(node.kind().into());
    crate::as_yaml::copy_node_content(builder, node);
    builder.finish_node();
}

// Debug helper to dump CST structure
pub(crate) fn dump_cst_to_string(node: &SyntaxNode, indent: usize) -> String {
    let mut result = String::new();
    let indent_str = "  ".repeat(indent);

    for child in node.children_with_tokens() {
        match child {
            rowan::NodeOrToken::Node(n) => {
                result.push_str(&format!("{}{:?}\n", indent_str, n.kind()));
                result.push_str(&dump_cst_to_string(&n, indent + 1));
            }
            rowan::NodeOrToken::Token(t) => {
                result.push_str(&format!("{}  {:?} {:?}\n", indent_str, t.kind(), t.text()));
            }
        }
    }
    result
}

impl Default for YamlFile {
    fn default() -> Self {
        Self::new()
    }
}

impl YamlFile {
    /// Create a new empty YAML document.
    pub fn new() -> YamlFile {
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(SyntaxKind::ROOT.into());
        builder.finish_node();
        YamlFile(SyntaxNode::new_root_mut(builder.finish()))
    }

    /// Parse YAML text, returning a Parse result
    pub fn parse(text: &str) -> Parse<YamlFile> {
        Parse::parse_yaml(text)
    }

    /// Parse YAML from a file path
    pub fn from_path<P: AsRef<Path>>(path: P) -> Result<YamlFile, crate::YamlError> {
        let contents = std::fs::read_to_string(path)?;
        Self::from_str(&contents)
    }

    /// Parse YAML text, allowing syntax errors.
    ///
    /// Returns the parsed tree even if there are parse errors, along with
    /// a list of error messages. This allows for error-resilient tooling
    /// that can work with partial or invalid input.
    ///
    /// # Example
    /// ```
    /// use yaml_edit::YamlFile;
    ///
    /// let (yaml_file, errors) = YamlFile::from_str_relaxed("key: [unclosed");
    /// // Tree is usable even with errors
    /// assert!(!errors.is_empty());
    /// assert!(yaml_file.document().is_some());
    /// ```
    pub fn from_str_relaxed(s: &str) -> (YamlFile, Vec<String>) {
        let parsed = YamlFile::parse(s);
        let errors = parsed.errors();
        (parsed.tree(), errors)
    }

    /// Parse YAML from a file path, allowing syntax errors.
    ///
    /// Returns the parsed tree even if there are parse errors, along with
    /// a list of error messages. I/O errors are still returned as `Err`.
    pub fn from_path_relaxed<P: AsRef<Path>>(
        path: P,
    ) -> Result<(YamlFile, Vec<String>), std::io::Error> {
        let contents = std::fs::read_to_string(path)?;
        Ok(Self::from_str_relaxed(&contents))
    }

    /// Read YAML from a `Read` object, allowing syntax errors.
    ///
    /// Returns the parsed tree even if there are parse errors, along with
    /// a list of error messages. I/O errors are still returned as `Err`.
    pub fn read_relaxed<R: std::io::Read>(
        mut r: R,
    ) -> Result<(YamlFile, Vec<String>), std::io::Error> {
        let mut buf = String::new();
        r.read_to_string(&mut buf)?;
        Ok(Self::from_str_relaxed(&buf))
    }

    /// Get all documents in this YAML file
    pub fn documents(&self) -> impl Iterator<Item = Document> {
        self.0.children().filter_map(Document::cast)
    }

    /// Iterate over the comments in this file, in source order.
    ///
    /// This includes comments inside documents as well as any comments between
    /// or outside documents.
    ///
    /// # Example
    /// ```
    /// use yaml_edit::YamlFile;
    /// use std::str::FromStr;
    ///
    /// let file = YamlFile::from_str("# header\nkey: value # trailing\n").unwrap();
    /// let comments: Vec<_> = file.comments().map(|c| c.content().to_string()).collect();
    /// assert_eq!(comments, vec!["header", "trailing"]);
    /// ```
    pub fn comments(&self) -> impl Iterator<Item = Comment> {
        crate::nodes::comment::comments(&self.0)
    }

    /// Get the first document in this YAML file, or `None` if there are none.
    ///
    /// Most YAML files have exactly one document. Use [`documents`](Self::documents)
    /// to iterate over all documents in a multi-document file.
    pub fn document(&self) -> Option<Document> {
        self.documents().next()
    }

    /// Ensure this `YamlFile` contains at least one document, creating an empty mapping document if needed.
    ///
    /// Returns the first document.
    pub fn ensure_document(&self) -> Document {
        if self.documents().next().is_none() {
            // No document exists, add an empty one
            let doc = Document::new_mapping();
            self.push_document(doc);
        }
        self.documents()
            .next()
            .expect("Document should exist after ensuring")
    }

    /// Iterate over all YAML directives (e.g. `%YAML 1.2`) in this file.
    pub fn directives(&self) -> impl Iterator<Item = Directive> {
        self.0.children().filter_map(Directive::cast)
    }

    /// Prepend a YAML directive to this file.
    ///
    /// `directive_text` should be the full directive line without a trailing
    /// newline, e.g. `"%YAML 1.2"` or `"%TAG ! tag:example.com,2000:app/"`.
    /// The directive is inserted before all existing content, and a `---`
    /// marker is added to the first document if it does not have one, since
    /// a document following directives must start explicitly.
    ///
    /// Note: the parser does not currently enforce that directives appear
    /// before any document node; callers are responsible for ordering.
    pub fn add_directive(&self, directive_text: &str) {
        if let Some(doc) = self.documents().next() {
            // A byte order mark must stay first in the stream, so one opening
            // the first document moves up to the stream ahead of the directive.
            if let Some(bom) = leading_bom(doc.syntax()) {
                bom.detach();
                crate::nodes::insert_children(&self.0, 0, vec![bom.into()]);
            }
            if !has_child_token(doc.syntax(), |k| k == SyntaxKind::DOC_START) {
                crate::nodes::insert_children(
                    doc.syntax(),
                    0,
                    vec![
                        crate::nodes::fresh_token(SyntaxKind::DOC_START, "---").into(),
                        crate::nodes::fresh_token(SyntaxKind::NEWLINE, "\n").into(),
                    ],
                );
            }
        }

        let mut builder = GreenNodeBuilder::new();
        builder.start_node(SyntaxKind::DIRECTIVE.into());
        builder.token(SyntaxKind::DIRECTIVE.into(), directive_text);
        builder.finish_node();
        let directive_node = SyntaxNode::new_root_mut(builder.finish());

        let index = usize::from(leading_bom(&self.0).is_some());
        crate::nodes::insert_children(
            &self.0,
            index,
            vec![
                directive_node.into(),
                crate::nodes::fresh_token(SyntaxKind::NEWLINE, "\n").into(),
            ],
        );
    }

    /// Add a new document to the end of this YAML file
    ///
    /// If the file already has documents or directives, the new document is
    /// started with a `---` marker (unless it has one) on a line of its own.
    ///
    /// A byte order mark is only allowed at the start of the stream or in
    /// front of a document's `---`, so a byte order mark opening `document`
    /// is kept in front of its `---`. With no earlier document to follow, it
    /// moves to the start of the stream, or is dropped if the stream already
    /// starts with one.
    pub fn push_document(&self, document: Document) {
        let bom = leading_bom(document.syntax());
        let needs_start = self
            .0
            .children()
            .any(|n| matches!(n.kind(), SyntaxKind::DOCUMENT | SyntaxKind::DIRECTIVE));
        if !self.0.text_range().is_empty() && !crate::nodes::trivia::ends_with_line_break(&self.0) {
            match self.0.last_child_or_token() {
                Some(rowan::NodeOrToken::Node(last)) if last.kind() == SyntaxKind::DOCUMENT => {
                    crate::nodes::trivia::terminate_last_line(&last, "\n")
                }
                _ => crate::nodes::append_children(
                    &self.0,
                    vec![crate::nodes::fresh_token(SyntaxKind::NEWLINE, "\n").into()],
                ),
            }
        }
        if needs_start && !has_child_token(document.syntax(), |k| k == SyntaxKind::DOC_START) {
            let index = usize::from(bom.is_some());
            crate::nodes::insert_children(
                document.syntax(),
                index,
                vec![
                    crate::nodes::fresh_token(SyntaxKind::DOC_START, "---").into(),
                    crate::nodes::fresh_token(SyntaxKind::NEWLINE, "\n").into(),
                ],
            );
        }
        if let Some(bom) = bom.filter(|_| !self.0.text_range().is_empty()) {
            bom.detach();
            self.place_bom(bom);
        }
        // Before the first document, the stream's blank lines, and comments
        // unless it starts with `---`, are that document's.
        let prefix: Vec<_> = self.0.children_with_tokens().collect();
        if !needs_start
            && !(has_child_token(document.syntax(), |k| k == SyntaxKind::DOC_START)
                && has_child_token(&self.0, |k| k == SyntaxKind::COMMENT))
        {
            for c in &prefix {
                c.detach();
            }
            crate::nodes::insert_children(document.syntax(), 0, prefix);
        }
        crate::nodes::append_children(&self.0, vec![document.0.into()]);
    }

    /// Put the byte order mark of a document about to be appended where the
    /// parser reads it.
    fn place_bom(&self, bom: SyntaxToken) {
        match self.0.last_child_or_token() {
            // A marker in front of `---` ends the document before it, unless
            // that one was closed with `...`.
            Some(rowan::NodeOrToken::Node(last))
                if last.kind() == SyntaxKind::DOCUMENT
                    && !has_child_token(&last, |k| k == SyntaxKind::DOC_END) =>
            {
                crate::nodes::append_children(&last, vec![bom.into()]);
            }
            _ if self.documents().next().is_some() => {
                crate::nodes::append_children(&self.0, vec![bom.into()]);
            }
            // Directives or comments only: the marker belongs at the start of
            // the stream, which needs only one.
            _ if leading_bom(&self.0).is_some() => {}
            _ => crate::nodes::insert_children(&self.0, 0, vec![bom.into()]),
        }
    }

    /// Set a key-value pair in the first document's mapping.
    ///
    /// If the key exists its value is replaced; if not, a new entry is appended.
    /// If the file has no document yet, one is added (see
    /// [`ensure_document`](Self::ensure_document)).
    ///
    /// Returns `false`, leaving the file unchanged, if the first document's
    /// root is a sequence or another non-mapping node (see [`Document::set`]).
    ///
    /// Mutates in place despite `&self` (see crate docs on interior mutability).
    pub fn set(&self, key: impl crate::AsYaml, value: impl crate::AsYaml) -> bool {
        self.ensure_document().set(key, value)
    }

    /// Insert a key-value pair immediately after `after_key` in the first document.
    ///
    /// Delegates to [`Document::insert_after`], which in turn calls
    /// [`Mapping::insert_after`]. If `key` already exists it is updated
    /// in-place rather than moved. Returns `false` if `after_key` is not found
    /// or the document is not a mapping.
    ///
    /// Mutates in place despite `&self` (see crate docs on interior mutability).
    pub fn insert_after(
        &self,
        after_key: impl crate::AsYaml,
        key: impl crate::AsYaml,
        value: impl crate::AsYaml,
    ) -> bool {
        self.document()
            .is_some_and(|doc| doc.insert_after(after_key, key, value))
    }

    /// Insert a key-value pair immediately before `before_key` in the first document.
    ///
    /// Delegates to [`Document::insert_before`], which in turn calls
    /// [`Mapping::insert_before`]. If `key` already exists it is updated
    /// in-place rather than moved. Returns `false` if `before_key` is not found
    /// or the document is not a mapping.
    ///
    /// Mutates in place despite `&self` (see crate docs on interior mutability).
    pub fn insert_before(
        &self,
        before_key: impl crate::AsYaml,
        key: impl crate::AsYaml,
        value: impl crate::AsYaml,
    ) -> bool {
        self.document()
            .is_some_and(|doc| doc.insert_before(before_key, key, value))
    }

    /// Move a key-value pair to immediately after `after_key` in the first document.
    ///
    /// Delegates to [`Document::move_after`]. If `key` already exists it is
    /// **removed** from its current position and re-inserted after `after_key`.
    /// Returns `false` if `after_key` is not found or the document is not a mapping.
    ///
    /// Use [`insert_after`](Self::insert_after) if you want an existing entry to be
    /// updated in-place rather than moved.
    ///
    /// Mutates in place despite `&self` (see crate docs on interior mutability).
    pub fn move_after(
        &self,
        after_key: impl crate::AsYaml,
        key: impl crate::AsYaml,
        value: impl crate::AsYaml,
    ) -> bool {
        self.document()
            .is_some_and(|doc| doc.move_after(after_key, key, value))
    }

    /// Move a key-value pair to immediately before `before_key` in the first document.
    ///
    /// Delegates to [`Document::move_before`]. If `key` already exists it is
    /// **removed** from its current position and re-inserted before `before_key`.
    /// Returns `false` if `before_key` is not found or the document is not a mapping.
    ///
    /// Use [`insert_before`](Self::insert_before) if you want an existing entry to be
    /// updated in-place rather than moved.
    ///
    /// Mutates in place despite `&self` (see crate docs on interior mutability).
    pub fn move_before(
        &self,
        before_key: impl crate::AsYaml,
        key: impl crate::AsYaml,
        value: impl crate::AsYaml,
    ) -> bool {
        self.document()
            .is_some_and(|doc| doc.move_before(before_key, key, value))
    }

    /// Insert a key-value pair at a specific index (0-based) in the first document.
    ///
    /// Delegates to [`Document::insert_at_index`]. If `key` already exists it
    /// is updated in-place rather than moved. If `index` is out of bounds the
    /// entry is appended at the end. If the first document has no root node,
    /// a mapping is created, as is a document if the file has none (see
    /// [`ensure_document`](Self::ensure_document)).
    ///
    /// Returns `false`, leaving the file unchanged, if the first document's
    /// root is a sequence or another non-mapping node.
    ///
    /// Mutates in place despite `&self` (see crate docs on interior mutability).
    pub fn insert_at_index(
        &self,
        index: usize,
        key: impl crate::AsYaml,
        value: impl crate::AsYaml,
    ) -> bool {
        self.ensure_document().insert_at_index(index, key, value)
    }
}

impl FromStr for YamlFile {
    type Err = crate::YamlError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let parsed = YamlFile::parse(s);
        if !parsed.positioned_errors().is_empty() {
            let first = &parsed.positioned_errors()[0];
            let lc = crate::byte_offset_to_line_column(s, first.range.start as usize);
            return Err(crate::YamlError::Parse {
                message: first.message.clone(),
                line: Some(lc.line),
                column: Some(lc.column),
            });
        }
        Ok(parsed.tree())
    }
}

pub(crate) use crate::parser::parse;

// Editing methods for Sequence

#[cfg(test)]
mod tests {
    // Tests for next_flow_element_is_implicit_mapping lookahead function
    mod lookahead_tests {
        use crate::lex::lex;
        use crate::parser::implicit_mapping_starts;

        /// Helper to test lookahead without creating full parser
        fn check_implicit_mapping(yaml: &str) -> bool {
            let flow = format!("[{yaml}]");
            let tokens = lex(&flow);
            let kinds: Vec<_> = tokens[1..tokens.len() - 1]
                .iter()
                .map(|(kind, _)| *kind)
                .collect();
            implicit_mapping_starts(&kinds).first() == Some(&true)
        }

        #[test]
        fn test_simple_implicit_mapping() {
            // Looking at: 'key' : value (inside [ ... ])
            // Should detect colon at depth 0
            assert!(check_implicit_mapping("'key' : value"));
        }

        #[test]
        fn test_simple_value_no_mapping() {
            // Looking at: value, ... (stops at comma, no colon)
            assert!(!check_implicit_mapping("value ,"));
        }

        #[test]
        fn test_value_with_comma() {
            // Looking at: value, more (comma at depth 0, not a mapping)
            assert!(!check_implicit_mapping("value , more"));
        }

        #[test]
        fn test_nested_sequence_as_key() {
            // Looking at: [a, b]: value (colon after nested sequence completes)
            assert!(check_implicit_mapping("[a, b]: value"));
        }

        #[test]
        fn test_nested_mapping_not_implicit() {
            // Looking at: {a: 1}, b (colon is inside braces at depth 1, not depth 0)
            assert!(!check_implicit_mapping("{a: 1}, b"));
        }

        #[test]
        fn test_deeply_nested_key() {
            // Looking at: [[a]]: value (multiple levels of nesting)
            assert!(check_implicit_mapping("[[a]]: value"));
        }

        #[test]
        fn test_with_whitespace() {
            // Looking at: 'key'  :  value (whitespace shouldn't affect detection)
            assert!(check_implicit_mapping("'key'  :  value"));
        }

        #[test]
        fn test_mapping_value_in_sequence() {
            // Looking at: a, {key: value} (second element has colon but inside braces)
            // First element is just "a" before comma
            assert!(!check_implicit_mapping("a, {key: value}"));
        }

        #[test]
        fn test_multiple_colons() {
            // Looking at: key1: value1, key2: value2 (first element is mapping)
            assert!(check_implicit_mapping("key1: value1, key2: value2"));
        }

        #[test]
        fn test_url_not_mapping() {
            // Looking at: http://example.com (colon in URL, but no space after)
            // Lexer should tokenize this as single string token
            assert!(!check_implicit_mapping("http://example.com"));
        }
    }
}
