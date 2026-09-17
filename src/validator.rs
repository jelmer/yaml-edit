//! YAML specification validator
//!
//! This module provides strict YAML 1.2 specification validation.
//! While the parser is lenient and focuses on error recovery,
//! this validator enforces strict spec compliance.
//!
//! ## Usage
//!
//! ```ignore
//! use yaml_edit::{Yaml, validator::Validator};
//!
//! let yaml = Yaml::parse("some: yaml");
//! let validator = Validator::new();
//! let violations = validator.validate(&yaml);
//!
//! if violations.is_empty() {
//!     println!("Strictly spec-compliant!");
//! } else {
//!     for violation in violations {
//!         println!("{}", violation);
//!     }
//! }
//! ```

use crate::nodes::has_child_token;
use crate::yaml::{Document, SyntaxNode};
use rowan::ast::AstNode;
use std::fmt;

/// A YAML specification violation found during validation
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Violation {
    /// Human-readable description of the violation
    pub message: String,
    /// Location in the source (line:column format)
    pub location: Option<String>,
    /// Byte range in the source text where the violation occurred
    pub text_range: Option<crate::TextPosition>,
    /// Severity of the violation
    pub severity: Severity,
    /// Specific rule that was violated
    pub rule: Rule,
}

/// Severity level of a spec violation
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[non_exhaustive]
pub enum Severity {
    /// Error: Strictly invalid per YAML 1.2 spec
    Error,
    /// Warning: Deprecated or discouraged but technically valid
    Warning,
}

/// Specific YAML spec rules that can be violated
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[non_exhaustive]
pub enum Rule {
    /// Invalid indentation
    InvalidIndentation,
    /// Document markers in wrong context
    InvalidDocumentMarker,
    /// Invalid tab usage
    InvalidTabUsage,
    /// Missing required syntax elements
    MissingSyntax,
    /// Invalid escape sequence
    InvalidEscape,
    /// Duplicate keys in mapping
    DuplicateKeys,
    /// Invalid anchor/alias usage
    InvalidAnchor,
    /// Invalid tag
    InvalidTag,
    /// Other spec violations
    Other,
}

impl fmt::Display for Violation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.location {
            Some(loc) => write!(
                f,
                "[{}] {}: {} ({:?})",
                match self.severity {
                    Severity::Error => "ERROR",
                    Severity::Warning => "WARN",
                },
                loc,
                self.message,
                self.rule
            ),
            None => write!(
                f,
                "[{}] {} ({:?})",
                match self.severity {
                    Severity::Error => "ERROR",
                    Severity::Warning => "WARN",
                },
                self.message,
                self.rule
            ),
        }
    }
}

/// YAML 1.2 specification validator
///
/// Performs strict validation checks on parsed YAML documents.
/// The parser itself is lenient and focuses on error recovery,
/// while this validator enforces strict spec compliance.
#[derive(Debug, Clone)]
pub struct Validator {
    /// Configuration options
    config: ValidatorConfig,
}

/// Configuration for the validator
#[derive(Debug, Clone)]
pub struct ValidatorConfig {
    /// Check for duplicate keys in mappings
    pub check_duplicate_keys: bool,
    /// Check indentation rules
    pub check_indentation: bool,
    /// Check tab usage restrictions
    pub check_tabs: bool,
    /// Check document marker placement
    pub check_document_markers: bool,
    /// Check anchor/alias validity
    pub check_anchors: bool,
}

impl Default for ValidatorConfig {
    fn default() -> Self {
        Self {
            check_duplicate_keys: true,
            check_indentation: true,
            check_tabs: true,
            check_document_markers: true,
            check_anchors: true,
        }
    }
}

/// Walk up to the ROOT node from the given node, or return the node itself if it is already ROOT.
fn find_root(node: &SyntaxNode) -> SyntaxNode {
    if node.kind() == crate::SyntaxKind::ROOT {
        return node.clone();
    }
    node.ancestors()
        .find(|n| n.kind() == crate::SyntaxKind::ROOT)
        .unwrap_or_else(|| node.clone())
}

/// Does this subtree hold real YAML content, as opposed to only whitespace,
/// newlines and document markers?
fn has_content(node: &SyntaxNode) -> bool {
    node.descendants().any(|n| {
        matches!(
            n.kind(),
            crate::SyntaxKind::MAPPING
                | crate::SyntaxKind::SEQUENCE
                | crate::SyntaxKind::SCALAR
                | crate::SyntaxKind::STRING
                | crate::SyntaxKind::TAGGED_NODE
        )
    })
}

/// Convert a rowan TextRange to a TextPosition.
/// The zero-based column `token` starts at.
///
/// Walks back through preceding tokens to the last NEWLINE rather than
/// serializing the document: this runs once per DASH, and rebuilding the whole
/// text each time made validation quadratic in the file size.
fn token_column(token: &crate::nodes::SyntaxToken) -> usize {
    let mut col = 0;
    let mut cur = token.prev_token();
    while let Some(t) = cur {
        let text = t.text();
        if let Some(i) = text.rfind('\n') {
            col += text[i + 1..].chars().count();
            return col;
        }
        col += text.chars().count();
        cur = t.prev_token();
    }
    col
}

/// Is this flow entry an omitted one, i.e. does its value consist solely of
/// the zero-width implicit-null scalar the parser emits for an empty slot?
///
/// See the implicit-null section of the CST invariants in `nodes/mod.rs`.
fn entry_is_empty(entry: &SyntaxNode) -> bool {
    entry.text_range().is_empty()
        || entry
            .text()
            .to_string()
            .trim_matches(|c: char| c.is_whitespace() || c == ',')
            .is_empty()
}

fn range_to_text_position(range: rowan::TextRange) -> crate::TextPosition {
    crate::TextPosition::new(u32::from(range.start()), u32::from(range.end()))
}

impl Violation {
    /// An error-severity violation with no associated source range.
    fn error(rule: Rule, message: impl Into<String>) -> Self {
        Violation {
            message: message.into(),
            location: None,
            text_range: None,
            severity: Severity::Error,
            rule,
        }
    }

    /// An error-severity violation covering `range` in the source.
    fn error_at(rule: Rule, range: rowan::TextRange, message: impl Into<String>) -> Self {
        Violation {
            text_range: Some(range_to_text_position(range)),
            ..Violation::error(rule, message)
        }
    }
}

impl Validator {
    /// Create a new validator with default configuration
    pub fn new() -> Self {
        Self {
            config: ValidatorConfig::default(),
        }
    }

    /// Create a validator with custom configuration
    pub fn with_config(config: ValidatorConfig) -> Self {
        Self { config }
    }

    /// Validate a YAML document against YAML 1.2 spec
    ///
    /// Returns a list of spec violations. Empty list means strictly compliant.
    pub fn validate(&self, doc: &Document) -> Vec<Violation> {
        let mut violations = Vec::new();

        // Check for duplicate directives at document level
        self.check_duplicate_directives(doc.syntax(), &mut violations);

        // Check for directive without document content
        self.check_directive_without_document(doc.syntax(), &mut violations);

        // Walk the syntax tree and check for violations
        // This will catch ERROR nodes created by parser (including content after doc end)
        self.validate_node(doc.syntax(), &mut violations);

        violations
    }

    /// Validate from a syntax node (can be ROOT, DOCUMENT, or any node)
    ///
    /// This is useful when you need to validate the full parse tree including directives
    /// that may not be attached to a specific document.
    pub fn validate_syntax(&self, node: &SyntaxNode) -> Vec<Violation> {
        let mut violations = Vec::new();

        // Check for duplicate directives
        self.check_duplicate_directives(node, &mut violations);

        // Check for directives without document content
        self.check_directives_at_root(node, &mut violations);

        // Check for directives after documents without document end marker
        self.check_directive_after_document(node, &mut violations);

        // Check that a `%TAG` shorthand is only used in its own document
        self.check_tag_shorthand_scope(node, &mut violations);

        // Walk the syntax tree and check for violations
        self.validate_node(node, &mut violations);

        violations
    }

    /// Check that a `%TAG` shorthand is only used in the document that
    /// declares it.
    ///
    /// A directive applies to the document it introduces, not to the whole
    /// stream, so `!prefix!` declared before the first `---` is undefined in
    /// every later document. The YAML test suite's QLJ7 is exactly that.
    fn check_tag_shorthand_scope(&self, node: &SyntaxNode, violations: &mut Vec<Violation>) {
        let mut handles: Vec<String> = Vec::new();
        let mut documents_seen = 0usize;

        for child in node.children_with_tokens() {
            match child {
                rowan::NodeOrToken::Node(ref n) if n.kind() == crate::SyntaxKind::DIRECTIVE => {
                    // `%TAG !handle! prefix` -- collect the handle it defines.
                    let text = n.text().to_string();
                    let mut words = text.split_whitespace();
                    if words.next() == Some("%TAG") {
                        if let Some(handle) = words.next() {
                            if documents_seen == 0 {
                                handles.push(handle.to_string());
                            }
                        }
                    }
                }
                rowan::NodeOrToken::Node(ref n) if n.kind() == crate::SyntaxKind::DOCUMENT => {
                    documents_seen += 1;
                    if documents_seen < 2 || handles.is_empty() {
                        continue;
                    }
                    // Any tag in a later document using a first-document
                    // handle is undefined there.
                    for tag in n
                        .descendants_with_tokens()
                        .filter_map(|c| c.into_token())
                        .filter(|t| t.kind() == crate::SyntaxKind::TAG)
                    {
                        // The lexer splits `!prefix!A` into the TAG tokens
                        // `!prefix` and `!A`, so match the handle without
                        // its closing `!`.
                        if let Some(handle) = handles.iter().find(|h| {
                            tag.text() == h.as_str() || tag.text() == h.trim_end_matches('!')
                        }) {
                            violations.push(Violation::error_at(
                                Rule::Other,
                                tag.text_range(),
                                format!(
                                    "Tag shorthand {handle} is only defined in the document that declares it"
                                ),
                            ));
                        }
                    }
                }
                _ => {}
            }
        }
    }

    fn validate_node(&self, node: &SyntaxNode, violations: &mut Vec<Violation>) {
        use crate::SyntaxKind;

        // Check for tabs in any node's tokens
        if self.config.check_tabs {
            self.check_tab_usage(node, violations);
        }

        // Check current node type
        // Check for multiple anchor tokens on any node (anchors are tokens, not nodes)
        if self.config.check_anchors {
            self.check_multiple_anchors(node, violations);
        }

        match node.kind() {
            SyntaxKind::ERROR => {
                // Parser has marked this as erroneous content
                // Report it as a validation error
                let content = node.text().to_string();
                let preview = if content.len() > 50 {
                    format!("{}...", &content[..50])
                } else {
                    content
                };
                violations.push(Violation::error_at(
                    Rule::Other,
                    node.text_range(),
                    format!("Invalid content in document: {preview:?}"),
                ));
            }
            SyntaxKind::MAPPING_ENTRY => {
                // Check for multiline implicit keys
                self.check_implicit_key_multiline(node, violations);
                // Check for block sequence on same line as mapping key
                self.check_sequence_on_same_line_as_key(node, violations);
            }
            SyntaxKind::SCALAR => {
                // Check for invalid escape sequences in quoted strings
                self.check_escape_sequences(node, violations);
                // Check for content on same line as block scalar indicator
                self.check_block_scalar_indicator(node, violations);
                // Check block scalar content indentation consistency
                self.check_block_scalar_content_indent(node, violations);
                // Check for trailing content after quoted strings
                self.check_trailing_content_after_quoted(node, violations);
                // Check for colons in plain scalar values
                self.check_colon_in_plain_scalar(node, violations);
                // Check for document markers inside quoted strings
                self.check_document_marker_in_string(node, violations);
                // Check for directives inside document content (e.g. %YAML after ---)
                self.check_directive_in_content(node, violations);
            }
            SyntaxKind::DOC_START | SyntaxKind::DOC_END if self.config.check_document_markers => {
                self.check_document_marker_placement(node, violations);
            }
            SyntaxKind::MAPPING => {
                self.check_flow_collection_commas(node, violations);
                self.check_flow_continuation_indent(node, violations);
                self.check_block_mapping_entries_on_same_line(node, violations);
                self.check_mapping_entry_indentation(node, violations);
                if self.config.check_duplicate_keys {
                    self.check_duplicate_keys(node, violations);
                }
            }
            SyntaxKind::SEQUENCE => {
                self.check_flow_collection_commas(node, violations);
                self.check_flow_continuation_indent(node, violations);
                self.check_sequence_entry_in_flow(node, violations);
            }
            SyntaxKind::VALUE => {
                self.check_anchor_and_alias(node, violations);
            }
            SyntaxKind::DOCUMENT => {
                self.check_document_level_anchors(node, violations);
            }
            _ => {}
        }

        // Check tokens (like COMMENT, DOC_START, TAG) that are children but not nodes
        for element in node.children_with_tokens() {
            if let Some(token) = element.as_token() {
                // Check COMMENT tokens for whitespace separation
                if token.kind() == crate::SyntaxKind::COMMENT {
                    self.check_comment_token_whitespace(token, violations);
                }
                // Check DOC_START tokens for content on same line
                if token.kind() == crate::SyntaxKind::DOC_START {
                    self.check_doc_start_token_content(token, violations);
                }
                // Check TAG tokens for invalid characters
                if token.kind() == crate::SyntaxKind::TAG {
                    self.check_tag_characters(token, violations);
                    self.check_tag_followed_by_comma(token, violations);
                }
            }
        }

        // Check indentation rules
        self.check_sequence_indentation(node, violations);
        self.check_quoted_string_indentation(node, violations);

        // Recursively validate child nodes
        for child in node.children() {
            self.validate_node(&child, violations);
        }
    }

    /// Check for directive without document content
    fn check_directive_without_document(
        &self,
        doc_node: &SyntaxNode,
        violations: &mut Vec<Violation>,
    ) {
        let root = find_root(doc_node);

        // Check if there are any DIRECTIVE nodes
        let has_directives = root
            .descendants()
            .any(|n| n.kind() == crate::SyntaxKind::DIRECTIVE);

        if !has_directives {
            return;
        }

        // Check if the document has any actual content
        // A document with only whitespace, newlines, or document markers is considered empty
        let has_content = has_content(doc_node);

        if !has_content {
            violations.push(Violation::error(
                Rule::Other,
                "Directive requires a document with content",
            ));
        }
    }

    /// Check for directives at root level without following document
    ///
    /// This checks if the ROOT node has DIRECTIVE children but no DOCUMENT children with content.
    fn check_directives_at_root(&self, node: &SyntaxNode, violations: &mut Vec<Violation>) {
        use crate::SyntaxKind;

        let check_node = find_root(node);

        // Check if there are any DIRECTIVE children
        let has_directives = check_node
            .children()
            .any(|child| child.kind() == SyntaxKind::DIRECTIVE);

        if !has_directives {
            return;
        }

        // Check if there's a DOCUMENT child with actual content
        let has_document_with_content = check_node
            .children()
            .any(|child| child.kind() == SyntaxKind::DOCUMENT && has_content(&child));

        if !has_document_with_content {
            violations.push(Violation::error(
                Rule::Other,
                "Directive without document content",
            ));
        }
    }

    /// Check for directives appearing after documents without document end marker (...)
    ///
    /// Per YAML spec, if a directive appears after document content, the document
    /// must be explicitly ended with `...` before the directive.
    fn check_directive_after_document(&self, node: &SyntaxNode, violations: &mut Vec<Violation>) {
        use crate::SyntaxKind;

        let check_node = find_root(node);

        // Track if we've seen a document with content
        let mut seen_document_with_content = false;

        for child in check_node.children() {
            match child.kind() {
                SyntaxKind::DOCUMENT => {
                    // Whether this document carries a DOC_END is rechecked from
                    // the DIRECTIVE arm below, by walking back to this sibling.
                    if has_content(&child) {
                        seen_document_with_content = true;
                    }
                }
                SyntaxKind::DIRECTIVE if seen_document_with_content => {
                    // If we've seen a document with content and the last document didn't have DOC_END
                    // Check if the previous DOCUMENT had a DOC_END
                    let mut prev_sibling = child.prev_sibling();
                    let mut found_doc_with_end = false;

                    while let Some(prev) = prev_sibling {
                        if prev.kind() == SyntaxKind::DOCUMENT {
                            // Check if this document has DOC_END
                            let has_doc_end = prev
                                .children_with_tokens()
                                .any(|t| t.kind() == SyntaxKind::DOC_END);

                            if has_doc_end {
                                found_doc_with_end = true;
                            }
                            break;
                        }
                        prev_sibling = prev.prev_sibling();
                    }

                    if !found_doc_with_end {
                        violations.push(Violation::error(
                            Rule::Other,
                            "Directive after document requires document end marker (...)",
                        ));
                    }
                }
                _ => {}
            }
        }
    }

    /// Check for directive tokens inside document content.
    ///
    /// When the parser encounters `%YAML 1.2` after a `---` without a preceding `...`,
    /// it parses the directive as scalar content. This check catches that case.
    fn check_directive_in_content(&self, node: &SyntaxNode, violations: &mut Vec<Violation>) {
        use crate::SyntaxKind;

        let has_directive = node
            .children_with_tokens()
            .any(|c| c.kind() == SyntaxKind::DIRECTIVE);
        if has_directive {
            violations.push(Violation::error_at(Rule::Other, node.text_range(), "Directive in document content (missing document end marker `...` before directive)".to_string()));
        }
    }

    /// Check for duplicate YAML directives
    fn check_duplicate_directives(&self, doc_node: &SyntaxNode, violations: &mut Vec<Violation>) {
        use std::collections::HashMap;

        let root = find_root(doc_node);

        // Collect all directives and count by type
        let mut directive_counts: HashMap<String, usize> = HashMap::new();

        for node in root.descendants() {
            if node.kind() == crate::SyntaxKind::DIRECTIVE {
                // Get the directive text (e.g., "%YAML 1.2" or "%TAG ! tag:yaml.org,2002:")
                let text = node.text().to_string();

                // Extract directive type (YAML, TAG, etc.)
                if let Some(directive_type) = text.split_whitespace().next() {
                    *directive_counts
                        .entry(directive_type.to_string())
                        .or_insert(0) += 1;
                }
            }
        }

        // Check for duplicates
        for (directive_type, count) in directive_counts {
            if count > 1 {
                violations.push(Violation::error(
                    Rule::Other,
                    format!("Duplicate {directive_type} directive"),
                ));
            }
        }
    }

    /// Check for multiple anchors on the same node
    fn check_multiple_anchors(&self, node: &SyntaxNode, violations: &mut Vec<Violation>) {
        // Count ANCHOR tokens (not nodes) in this node's children. An
        // anchor that annotates a complex key now sits inside that KEY, so
        // 6BFJ's `&mapping\n&key [ ... ]: v` leaves one here while 4JVG's
        // two anchors on a single value still leave both.
        let adjacent_anchors = node
            .children_with_tokens()
            .filter(|child| {
                child
                    .as_token()
                    .is_some_and(|t| t.kind() == crate::SyntaxKind::ANCHOR)
            })
            .count();

        if adjacent_anchors > 1 {
            violations.push(Violation::error_at(
                Rule::InvalidAnchor,
                node.text_range(),
                "Multiple anchors on the same node",
            ));
        }
    }

    /// Check for invalid escape sequences in quoted strings
    fn check_escape_sequences(&self, node: &SyntaxNode, violations: &mut Vec<Violation>) {
        // Check first character to see if this is a quoted string - no allocation needed
        let first_char = node.first_token().and_then(|t| t.text().chars().next());
        if first_char != Some('"') {
            return;
        }

        // Scan for escape sequences (requires one allocation for the text)
        let text = node.text().to_string();
        let mut chars = text.chars().peekable();

        while let Some(ch) = chars.next() {
            if ch == '\\' {
                if let Some(&next) = chars.peek() {
                    // Valid escape sequences in YAML 1.2
                    // A `\\` before a line break escapes that break, folding
                    // the line: the test suite's 565N wraps a long binary
                    // value that way.
                    let valid_escapes = [
                        '0', 'a', 'b', 't', 'n', 'v', 'f', 'r', 'e', ' ', '"', '/', '\\', 'N', '_',
                        'L', 'P', 'x', 'u', 'U', '\n', '\r', '\t',
                    ];

                    if !valid_escapes.contains(&next) {
                        violations.push(Violation::error_at(
                            Rule::InvalidEscape,
                            node.text_range(),
                            format!("Invalid escape sequence: \\{next}"),
                        ));
                        return; // Found one, no need to continue
                    }
                    // Consume the escaped character, or `\\$` reads as a
                    // valid `\\` followed by an invalid `\$` (test suite
                    // 6SLA).
                    chars.next();
                }
            }
        }
    }

    /// Check for content on the same line as block scalar indicator (| or >)
    ///
    /// Per YAML spec, block scalar content must start on the line after the indicator.
    /// Only chomping indicators (+/-) and indentation indicators (1-9) are allowed
    /// on the same line as the block scalar indicator.
    fn check_block_scalar_indicator(&self, node: &SyntaxNode, violations: &mut Vec<Violation>) {
        // Check if this scalar has a GREATER (folded) or PIPE (literal) indicator
        let has_block_indicator = has_child_token(node, |k| {
            matches!(k, crate::SyntaxKind::GREATER | crate::SyntaxKind::PIPE)
        });

        if !has_block_indicator {
            return;
        }

        // Check if any STRING tokens appear before the first NEWLINE after the indicator
        let mut found_indicator = false;
        let mut found_newline = false;

        for child in node.children_with_tokens() {
            if let rowan::NodeOrToken::Token(token) = child {
                // Mark when we find the block indicator
                if matches!(
                    token.kind(),
                    crate::SyntaxKind::GREATER | crate::SyntaxKind::PIPE
                ) {
                    found_indicator = true;
                    continue;
                }

                // After indicator, before newline
                if found_indicator && !found_newline {
                    match token.kind() {
                        crate::SyntaxKind::NEWLINE => {
                            found_newline = true;
                        }
                        // A chomping indicator and an indentation digit may
                        // share the indicator's line; the lexer gives `-`,
                        // `+`, `2-` and `-2` as STRING, so they are not the
                        // content this rule is looking for.
                        crate::SyntaxKind::STRING
                            if token.text().chars().all(|c| matches!(c, '+' | '-'))
                                || (token.text().len() == 2
                                    && token.text().chars().any(|c| c.is_ascii_digit())
                                    && token.text().chars().any(|c| matches!(c, '+' | '-'))) => {}
                        crate::SyntaxKind::STRING => {
                            // Found content on same line as indicator
                            violations.push(Violation::error(
                                Rule::Other,
                                "Block scalar content cannot appear on same line as indicator",
                            ));
                            return;
                        }
                        // An indentation indicator is a single digit 1-9;
                        // `|0` and `|10` are errors the YAML test suite
                        // expects (2G84).
                        // `|2-` and `|-2` both carry the digit 2, so strip a
                        // chomping indicator before checking it (D83L).
                        crate::SyntaxKind::INT
                            if !matches!(
                                token.text().trim_matches(['+', '-']),
                                "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
                            ) =>
                        {
                            violations.push(Violation::error(
                                Rule::Other,
                                "Block scalar indentation indicator must be a digit 1-9",
                            ));
                            return;
                        }
                        // WHITESPACE, COMMENT, and chomping/indentation indicators are OK
                        _ => {}
                    }
                }
            }
        }
    }

    /// Check block scalar (`|` or `>`) content indentation.
    ///
    /// Once a blank line at indentation N has appeared, no later
    /// non-blank content line may be at indentation < N (per YAML 1.2
    /// example 5LLU). This flags the case where the content indent
    /// implicitly established by empty-line whitespace runs is later
    /// under-cut.
    fn check_block_scalar_content_indent(
        &self,
        node: &SyntaxNode,
        violations: &mut Vec<Violation>,
    ) {
        let has_block_indicator = has_child_token(node, |k| {
            matches!(k, crate::SyntaxKind::GREATER | crate::SyntaxKind::PIPE)
        });
        if !has_block_indicator {
            return;
        }

        // Walk tokens after the block indicator's NEWLINE. Track the
        // deepest INDENT seen so far (from blank-only lines); flag any
        // subsequent INDENT + non-blank content whose text is shorter.
        let mut past_first_newline = false;
        let mut max_blank_indent = 0usize;
        let mut pending_indent: Option<rowan::SyntaxToken<crate::Lang>> = None;
        for child in node.children_with_tokens() {
            let Some(token) = child.as_token().cloned() else {
                continue;
            };
            if !past_first_newline {
                if token.kind() == crate::SyntaxKind::NEWLINE {
                    past_first_newline = true;
                }
                continue;
            }
            match token.kind() {
                crate::SyntaxKind::INDENT => {
                    pending_indent = Some(token);
                }
                crate::SyntaxKind::NEWLINE => {
                    // Blank line: the pending INDENT (if any) tells us
                    // how far the blank line was padded. Update the
                    // running maximum.
                    if let Some(ind) = pending_indent.take() {
                        max_blank_indent = max_blank_indent.max(ind.text().len());
                    }
                }
                _ => {
                    // Non-blank content. Compare the pending INDENT to
                    // the observed max_blank_indent.
                    if let Some(ind) = pending_indent.take() {
                        if ind.text().len() < max_blank_indent {
                            violations.push(Violation::error_at(Rule::Other, ind.text_range(), format!( "Block scalar content under-indented ({} spaces) relative to preceding blank line ({} spaces)", ind.text().len(), max_blank_indent )));
                            return;
                        }
                    }
                }
            }
        }
    }

    /// Check for trailing content after quoted strings
    ///
    /// After a quoted string (double or single) closes, only whitespace, newlines,
    /// or comments should follow. Additional content on the same line is invalid.
    fn check_trailing_content_after_quoted(
        &self,
        node: &SyntaxNode,
        violations: &mut Vec<Violation>,
    ) {
        let mut found_quoted = false;
        let mut found_quote_end = false;
        let mut found_newline = false;

        for child in node.children_with_tokens() {
            if let rowan::NodeOrToken::Token(token) = child {
                match token.kind() {
                    crate::SyntaxKind::STRING => {
                        let text = token.text();

                        // Check if this is a quoted string (starts with " or ')
                        if !found_quoted && (text.starts_with('"') || text.starts_with('\'')) {
                            found_quoted = true;

                            // Check if quote ends in this same token
                            if text.len() > 1 && (text.ends_with('"') || text.ends_with('\'')) {
                                found_quote_end = true;
                            }
                        } else if found_quoted && !found_quote_end {
                            // Still inside the quoted string
                            if text.ends_with('"') || text.ends_with('\'') {
                                found_quote_end = true;
                            }
                        } else if found_quote_end && !found_newline {
                            // Found content after quoted string ended, before newline
                            violations.push(Violation::error(
                                Rule::Other,
                                "Trailing content after quoted string",
                            ));
                            return;
                        }
                    }
                    crate::SyntaxKind::NEWLINE => {
                        found_newline = true;
                    }
                    crate::SyntaxKind::WHITESPACE | crate::SyntaxKind::COMMENT => {
                        // These are allowed after quoted strings
                    }
                    _ => {}
                }
            }
        }
    }

    /// Check for colons in plain scalar values
    ///
    /// Plain scalars in block context cannot contain `: ` (colon followed by space)
    /// without being quoted. This indicates an attempt to create a nested mapping
    /// within a plain scalar, which is invalid.
    fn check_colon_in_plain_scalar(&self, node: &SyntaxNode, violations: &mut Vec<Violation>) {
        // Check if this scalar contains COLON tokens
        let has_colon = has_child_token(node, |k| k == crate::SyntaxKind::COLON);

        if !has_colon {
            return;
        }

        // Check if this is a quoted string (which can contain colons)
        let is_quoted = node.first_token().is_some_and(|t| {
            let text = t.text();
            text.starts_with('"') || text.starts_with('\'')
        });

        if is_quoted {
            return;
        }

        // Check if this scalar is inside a VALUE node (not a KEY)
        // Keys can have plain text without issues, but values with colons need special handling
        let parent_is_value = node
            .parent()
            .is_some_and(|p| p.kind() == crate::SyntaxKind::VALUE);

        if parent_is_value {
            violations.push(Violation::error(
                Rule::Other,
                "Plain scalar value cannot contain mapping syntax (colon)",
            ));
        }
    }

    /// Check for document markers (--- or ...) appearing in quoted strings
    ///
    /// Document markers on their own line should always be recognized as document
    /// boundaries, even if they appear to be within a quoted string. A quoted string
    /// containing "\n---\n" or "\n...\n" is invalid.
    fn check_document_marker_in_string(&self, node: &SyntaxNode, violations: &mut Vec<Violation>) {
        // Get the text of the scalar
        let text = node.text().to_string();

        // Check if this is a quoted string
        if !text.starts_with('"') && !text.starts_with('\'') {
            return;
        }

        // Check for document markers on their own line within the string
        // Look for \n--- or \n... where the marker is followed by \n or end of string
        if text.contains("\n---\n")
            || text.contains("\n---\"")
            || text.contains("\n---'")
            || text.contains("\n...\n")
            || text.contains("\n...\"")
            || text.contains("\n...'")
        {
            violations.push(Violation::error_at(
                Rule::InvalidDocumentMarker,
                node.text_range(),
                "Document marker on its own line inside quoted string",
            ));
        }
    }

    /// Check for tab usage in whitespace nodes
    fn check_tab_usage(&self, node: &SyntaxNode, violations: &mut Vec<Violation>) {
        // Only check whitespace nodes - more efficient than serializing everything
        // Check each token directly without allocation
        for token in node.children_with_tokens() {
            if let rowan::NodeOrToken::Token(token) = token {
                // A quoted scalar keeps its continuation lines inside one
                // token, so check its text directly: a continuation opening
                // with a tab supplies no indentation at all, which
                // DK95/01's `foo: "bar\n\tbaz"` needs. A blank tab line
                // (DK95/04) indents nothing and is fine.
                // Only where indentation is actually required: a quoted
                // scalar that is the document's own node needs none, so a
                // tab there is content (7A4E, PRH3), while one inside a
                // mapping value must clear the key's column (DK95/01).
                if node.kind() == crate::SyntaxKind::SCALAR
                    && node
                        .parent()
                        .is_some_and(|p| p.kind() == crate::SyntaxKind::VALUE)
                    && token.text().starts_with(['"', '\''])
                    && token
                        .text()
                        .split('\n')
                        .skip(1)
                        .any(|line| line.starts_with('\t') && !line.trim().is_empty())
                {
                    violations.push(Violation::error_at(
                        Rule::InvalidTabUsage,
                        token.text_range(),
                        "Tabs are not allowed for indentation in YAML",
                    ));
                    return;
                }
                // Only an INDENT that supplies a node's indentation can hold
                // an illegal tab. YAML 1.2 forbids a tab in `s-indent`, but
                // elsewhere -- inside a scalar's continuation line, or as
                // separation at the stream's own level -- it is content or
                // legal whitespace, which is how the test suite reads 4ZYM,
                // HS5T, NB6Z, UV7Q, 6CA3, K54U and T5N4.
                if !matches!(
                    node.kind(),
                    crate::SyntaxKind::VALUE
                        | crate::SyntaxKind::MAPPING
                        | crate::SyntaxKind::SEQUENCE
                        | crate::SyntaxKind::MAPPING_ENTRY
                        | crate::SyntaxKind::SEQUENCE_ENTRY
                ) {
                    continue;
                }
                // A flow collection at the stream's own level is not
                // indentation-sensitive, so a tab inside one is separation
                // whitespace (6CA3's `\t[\n\t]`). Nested in a block
                // collection it still has to clear that block's column, as
                // Y79Y/003's `- [\n\tfoo,` does not.
                let is_flow = node.first_token().is_some_and(|t| {
                    matches!(
                        t.kind(),
                        crate::SyntaxKind::LEFT_BRACKET | crate::SyntaxKind::LEFT_BRACE
                    )
                });
                let inside_a_block = node.ancestors().skip(1).any(|a| {
                    matches!(
                        a.kind(),
                        crate::SyntaxKind::MAPPING | crate::SyntaxKind::SEQUENCE
                    )
                });
                if is_flow && !inside_a_block {
                    continue;
                }
                // A tab may separate an indicator from a scalar (`- \tx`),
                // but a block collection after it needs `s-indent`, which
                // forbids tabs: `-\t-` and `?\t-` are errors the test suite
                // expects (Y79Y), while `-\t-1` is the scalar `-1`.
                // The tab has to separate the indicator from the node on
                // the same line; trailing whitespace before a line break
                // (DC7X's `seq:\t`) indents nothing.
                if token.kind() == crate::SyntaxKind::WHITESPACE
                    && token.text().contains('\t')
                    && !token
                        .next_sibling_or_token()
                        .and_then(|n| n.into_node())
                        .and_then(|n| n.first_token())
                        .is_some_and(|t| t.kind() == crate::SyntaxKind::NEWLINE)
                    && token
                        .next_sibling_or_token()
                        .and_then(|n| n.into_node())
                        .is_some_and(|n| {
                            let opens_a_block = |k| {
                                matches!(
                                    k,
                                    crate::SyntaxKind::MAPPING | crate::SyntaxKind::SEQUENCE
                                )
                            };
                            opens_a_block(n.kind())
                                || (matches!(
                                    n.kind(),
                                    crate::SyntaxKind::VALUE | crate::SyntaxKind::KEY
                                ) && n.children().any(|c| opens_a_block(c.kind())))
                        })
                {
                    violations.push(Violation::error_at(
                        Rule::InvalidTabUsage,
                        token.text_range(),
                        "Tabs are not allowed for indentation in YAML",
                    ));
                    return;
                }
                if token.kind() != crate::SyntaxKind::INDENT {
                    continue;
                }
                // Check the token text directly - this is a cheap slice operation
                if token.text().contains('\t') {
                    violations.push(Violation::error_at(
                        Rule::InvalidTabUsage,
                        token.text_range(),
                        "Tabs are not allowed for indentation in YAML",
                    ));
                    return; // Found one, no need to keep checking
                }
            }
        }
    }

    /// Check document marker placement
    fn check_document_marker_placement(&self, node: &SyntaxNode, violations: &mut Vec<Violation>) {
        // Document markers should only appear at document boundaries
        // Check if marker is in inappropriate context (e.g., inside a quoted string)
        if let Some(parent) = node.parent() {
            if matches!(
                parent.kind(),
                crate::SyntaxKind::STRING | crate::SyntaxKind::SCALAR
            ) {
                violations.push(Violation::error_at(
                    Rule::InvalidDocumentMarker,
                    node.text_range(),
                    "Document marker inside string is invalid",
                ));
            }
        }
    }

    /// Check for missing commas in flow collections
    fn check_flow_collection_commas(&self, node: &SyntaxNode, violations: &mut Vec<Violation>) {
        // Check first token to see if this is a flow collection - avoid full serialization
        let first_token = node.first_token();
        let is_flow_mapping = first_token
            .as_ref()
            .is_some_and(|t| t.kind() == crate::SyntaxKind::LEFT_BRACE);
        let is_flow_sequence = first_token
            .as_ref()
            .is_some_and(|t| t.kind() == crate::SyntaxKind::LEFT_BRACKET);

        if !is_flow_mapping && !is_flow_sequence {
            return;
        }

        // Count entries and commas
        let entry_kind = if is_flow_mapping {
            crate::SyntaxKind::MAPPING_ENTRY
        } else {
            crate::SyntaxKind::SEQUENCE_ENTRY
        };

        // A flow separator is stored inside the entry it follows (see the
        // flow-separator invariant in nodes/mod.rs), and an omitted entry
        // shows up as one whose value is the zero-width implicit null. So
        // `[a, , b]` and `[a, b, ,]` are entries with an empty value rather
        // than adjacent COMMA tokens.
        let entries: Vec<_> = node.children().filter(|n| n.kind() == entry_kind).collect();
        let entry_count = entries.len();
        let comma_count = entries
            .iter()
            .filter(|e| crate::nodes::has_child_token(e, |k| k == crate::SyntaxKind::COMMA))
            .count();

        for entry in &entries {
            if entry_is_empty(entry) {
                violations.push(Violation::error_at(
                    Rule::Other,
                    node.text_range(),
                    "Empty entry in flow collection",
                ));
                break;
            }
        }

        // Flow collections need n-1 commas for n entries (except when trailing comma)
        if entry_count > 1 && comma_count < entry_count - 1 {
            violations.push(Violation::error(Rule::MissingSyntax, format!( "Flow collection missing commas: {entry_count} entries but only {comma_count} commas" )));
        }
    }

    /// Check for multiple mapping entries on the same line in block mappings
    ///
    /// In block mappings (not flow mappings with {}), each mapping entry should
    /// be on its own line. Multiple entries on the same line are invalid.
    fn check_block_mapping_entries_on_same_line(
        &self,
        node: &SyntaxNode,
        violations: &mut Vec<Violation>,
    ) {
        // Check if this is a flow mapping (which allows same-line entries)
        let first_token = node.first_token();
        let is_flow_mapping = first_token.as_ref().is_some_and(|t| t.text() == "{");

        if is_flow_mapping {
            return; // Flow mappings can have entries on same line
        }

        // Check for consecutive MAPPING_ENTRY nodes without NEWLINE between them
        let mut prev_entry: Option<SyntaxNode> = None;

        for child in node.children() {
            if child.kind() == crate::SyntaxKind::MAPPING_ENTRY {
                if let Some(prev) = prev_entry {
                    // Check if there's a NEWLINE between prev and current entry.
                    // The newline may be inside the previous entry (as its last
                    // token) or between entries as a sibling token.
                    let has_newline_between = {
                        // First check if the previous entry ends with a newline.
                        //
                        // An explicit key's entry ends with the zero-width
                        // implicit-null scalar of its VALUE, so look past
                        // tokens that render as nothing: `? a\n? b\n` has its
                        // newline inside the first entry, not between them.
                        let prev_ends_with_newline = prev
                            .descendants_with_tokens()
                            .filter_map(|c| c.into_token())
                            .filter(|t| !t.text().is_empty())
                            .last()
                            .is_some_and(|t| t.kind() == crate::SyntaxKind::NEWLINE);

                        if prev_ends_with_newline {
                            true
                        } else {
                            // Check sibling tokens between the entries
                            let mut current_sibling = prev.next_sibling_or_token();
                            let mut found_newline = false;

                            while let Some(sibling) = current_sibling {
                                if let rowan::NodeOrToken::Node(n) = &sibling {
                                    if n == &child {
                                        break;
                                    }
                                }

                                if let rowan::NodeOrToken::Token(t) = &sibling {
                                    if t.kind() == crate::SyntaxKind::NEWLINE {
                                        found_newline = true;
                                        break;
                                    }
                                }

                                current_sibling = sibling.next_sibling_or_token();
                            }

                            found_newline
                        }
                    };

                    if !has_newline_between {
                        violations.push(Violation::error(
                            Rule::Other,
                            "Block mapping entries must be on separate lines",
                        ));
                        return; // One violation is enough
                    }
                }

                prev_entry = Some(child);
            }
        }
    }

    /// Check that sibling MAPPING_ENTRY nodes inside a block mapping
    /// share a consistent indent.
    ///
    /// The parser is deliberately lenient: it accepts
    ///     k1: v1
    ///      k2: v2
    /// and produces two MAPPING_ENTRY children separated by an INDENT
    /// " " token as a direct child of MAPPING. Similarly for
    ///     key:
    ///       ok: 1
    ///      wrong: 2
    /// (yaml-test-suite EW3V / DMG6 / N4JP / U44R). Flag inconsistent
    /// leading indent tokens between block entries.
    fn check_mapping_entry_indentation(&self, node: &SyntaxNode, violations: &mut Vec<Violation>) {
        // Flow mappings ({...}) don't have INDENT tokens between entries.
        let first_token = node.first_token();
        let is_flow_mapping = first_token
            .as_ref()
            .is_some_and(|t| t.kind() == crate::SyntaxKind::LEFT_BRACE);
        if is_flow_mapping {
            return;
        }

        // Determine the expected indent for entries in this mapping.
        //   - Nested mapping (parent VALUE has NEWLINE + INDENT before
        //     us): use that INDENT's text.
        //   - Root mapping (child of DOCUMENT): the empty string.
        //
        // Then walk direct children. Every INDENT token that precedes a
        // MAPPING_ENTRY (i.e. separates two entries) must equal the
        // expected indent. Otherwise the parser has admitted a
        // wrong-indented sibling that the YAML spec rejects (see
        // yaml-test-suite EW3V / DMG6 / N4JP / U44R).
        // A mapping that is a sequence entry's value has its INDENT in the
        // SEQUENCE_ENTRY rather than a VALUE, in the same position relative
        // to the MAPPING. Without it the expected indent fell back to the
        // empty string and every entry of `-\n  a: 1\n  b: 2\n` looked
        // wrongly indented.
        let expected_indent: String = node
            .parent()
            .filter(|p| {
                matches!(
                    p.kind(),
                    crate::SyntaxKind::VALUE | crate::SyntaxKind::SEQUENCE_ENTRY
                )
            })
            .and_then(|parent_value| {
                // Look for the INDENT token that immediately precedes
                // this MAPPING in the parent VALUE.
                let mut last_indent: Option<String> = None;
                for el in parent_value.children_with_tokens() {
                    match el {
                        rowan::NodeOrToken::Node(n) if &n == node => break,
                        rowan::NodeOrToken::Token(t) if t.kind() == crate::SyntaxKind::INDENT => {
                            last_indent = Some(t.text().to_string());
                        }
                        _ => {}
                    }
                }
                last_indent
            })
            .or_else(|| {
                // A compact mapping on a sequence entry's own line has no
                // INDENT of its own (`- key: value\n  key2: value2\n`); its
                // entries line up under the dash's gap. Take the column from
                // the node's offset within its line.
                let parent = node.parent()?;
                if parent.kind() != crate::SyntaxKind::SEQUENCE_ENTRY {
                    return None;
                }
                // Walk back over the preceding tokens to the line break,
                // summing their widths. Materialising the whole document to
                // find the line start made this quadratic in its size.
                let mut column = 0usize;
                let mut token = node.first_token()?.prev_token();
                while let Some(t) = token {
                    if t.kind() == crate::SyntaxKind::NEWLINE {
                        break;
                    }
                    column += t.text().len();
                    token = t.prev_token();
                }
                Some(" ".repeat(column))
            })
            .unwrap_or_default();

        let mut seen_entry = false;
        for child in node.children_with_tokens() {
            match child {
                rowan::NodeOrToken::Token(t) if t.kind() == crate::SyntaxKind::INDENT => {
                    if seen_entry && t.text() != expected_indent {
                        violations.push(Violation::error_at(Rule::Other, t.text_range(), format!( "Sibling block mapping entries have inconsistent indentation (expected {:?}, found {:?})", expected_indent, t.text() )));
                    }
                }
                rowan::NodeOrToken::Node(n) if n.kind() == crate::SyntaxKind::MAPPING_ENTRY => {
                    seen_entry = true;
                }
                _ => {}
            }
        }
    }

    /// Check for SEQUENCE_ENTRY nodes in flow sequences
    ///
    /// Flow sequences (using []) should not have SEQUENCE_ENTRY children.
    /// SEQUENCE_ENTRY is only for block sequences (using -). In flow sequences,
    /// values appear directly without the - marker.
    /// Check that a multi-line flow collection indents its continuation lines.
    ///
    /// A flow collection may span lines, but each continuation has to be
    /// indented more than the block context it sits in; at column zero the
    /// content would be read as a new block node instead (yaml-test-suite
    /// 9C9N).
    fn check_flow_continuation_indent(&self, node: &SyntaxNode, violations: &mut Vec<Violation>) {
        let first = node.first_token();
        let is_flow = first.as_ref().is_some_and(|t| {
            matches!(
                t.kind(),
                crate::SyntaxKind::LEFT_BRACE | crate::SyntaxKind::LEFT_BRACKET
            )
        });
        if !is_flow {
            return;
        }
        // Only the outermost flow collection needs checking; a nested one
        // shares its lines.
        if node.ancestors().skip(1).any(|a| {
            matches!(
                a.kind(),
                crate::SyntaxKind::MAPPING | crate::SyntaxKind::SEQUENCE
            ) && a.first_token().is_some_and(|t| {
                matches!(
                    t.kind(),
                    crate::SyntaxKind::LEFT_BRACE | crate::SyntaxKind::LEFT_BRACKET
                )
            })
        }) {
            return;
        }

        // A `---` or `...` at the start of a line ends the document, so it
        // cannot appear inside a flow collection: the test suite's N782 is
        // the error `[\n--- ,\n...\n]`. Inside the collection the lexer
        // gives them as plain scalars, so no document-marker rule sees them.
        let mut line_start = true;
        for el in node.descendants_with_tokens() {
            let rowan::NodeOrToken::Token(t) = el else {
                continue;
            };
            match t.kind() {
                crate::SyntaxKind::NEWLINE => line_start = true,
                crate::SyntaxKind::INDENT | crate::SyntaxKind::WHITESPACE => {}
                _ if line_start && matches!(t.text(), "---" | "...") => {
                    violations.push(Violation::error_at(
                        Rule::InvalidDocumentMarker,
                        t.text_range(),
                        "Document marker inside a flow collection",
                    ));
                    return;
                }
                _ => line_start = false,
            }
        }

        // Only a flow collection sitting inside a block collection can be
        // confused with one: at column zero its continuation would read as a
        // new entry of that block (9C9N's `flow: [a,\nb,\nc]`). A flow
        // collection that is the document's own node has no such neighbour,
        // so its entries may start at column zero (4ABK).
        if !node.ancestors().skip(1).any(|a| {
            matches!(
                a.kind(),
                crate::SyntaxKind::MAPPING | crate::SyntaxKind::SEQUENCE
            )
        }) {
            return;
        }

        let mut after_newline = false;
        for el in node.descendants_with_tokens() {
            let rowan::NodeOrToken::Token(t) = el else {
                continue;
            };
            match t.kind() {
                crate::SyntaxKind::NEWLINE => after_newline = true,
                crate::SyntaxKind::INDENT | crate::SyntaxKind::WHITESPACE if after_newline => {
                    after_newline = false;
                }
                _ if after_newline => {
                    violations.push(Violation::error_at(
                        Rule::InvalidIndentation,
                        node.text_range(),
                        "Flow collection continuation line must be indented",
                    ));
                    return;
                }
                _ => {}
            }
        }
    }

    fn check_sequence_entry_in_flow(&self, node: &SyntaxNode, violations: &mut Vec<Violation>) {
        // Check if this is a flow sequence
        let first_token = node.first_token();
        let is_flow_sequence = first_token.as_ref().is_some_and(|t| t.text() == "[");

        if !is_flow_sequence {
            return;
        }

        // Flow entries are SEQUENCE_ENTRY nodes too, so their mere presence
        // says nothing. Block syntax leaking in shows up as an entry whose
        // value is a bare `-`, which the lexer hands back as scalar text
        // rather than a DASH token.
        for child in node.children() {
            if child.kind() == crate::SyntaxKind::SEQUENCE_ENTRY
                && child
                    .text()
                    .to_string()
                    .trim_matches(|c: char| c.is_whitespace() || c == ',')
                    == "-"
            {
                violations.push(Violation::error_at(
                    Rule::Other,
                    node.text_range(),
                    "Flow sequence cannot use block sequence syntax (-)",
                ));
                return; // One violation is enough
            }
        }
    }

    /// Check for anchors at document level without proper node attachment
    ///
    /// Anchors should be attached to nodes (values), not floating at document level.
    /// This is often the result of syntax errors like `&anchor - item`.
    fn check_document_level_anchors(&self, node: &SyntaxNode, violations: &mut Vec<Violation>) {
        // Check for ANCHOR tokens that are direct children of DOCUMENT
        for child in node.children_with_tokens() {
            if let rowan::NodeOrToken::Token(token) = child {
                if token.kind() == crate::SyntaxKind::ANCHOR {
                    // An anchor at document level annotates the document's
                    // own node, which is the ordinary spelling of
                    // `&sequence\n- a\n` and `&flowseq [ ... ]`. It is
                    // stranded only when nothing follows it, or when what
                    // follows on its own line is a sequence entry, which
                    // cannot sit there: `&anchor - sequence entry` is the
                    // test suite's SY6V error.
                    let mut annotates_a_node = false;
                    let mut same_line = true;
                    for sibling in token.siblings_with_tokens(rowan::Direction::Next).skip(1) {
                        match sibling {
                            rowan::NodeOrToken::Token(ref t)
                                if t.kind() == crate::SyntaxKind::NEWLINE =>
                            {
                                same_line = false;
                            }
                            rowan::NodeOrToken::Node(ref n) => {
                                let opens_an_entry = same_line
                                    && n.kind() == crate::SyntaxKind::SCALAR
                                    && n.text().to_string().starts_with("- ");
                                if !opens_an_entry
                                    && matches!(
                                        n.kind(),
                                        crate::SyntaxKind::MAPPING
                                            | crate::SyntaxKind::SEQUENCE
                                            | crate::SyntaxKind::SCALAR
                                            | crate::SyntaxKind::TAGGED_NODE
                                            | crate::SyntaxKind::ALIAS
                                    )
                                {
                                    annotates_a_node = true;
                                    break;
                                }
                            }
                            _ => {}
                        }
                    }
                    if !annotates_a_node {
                        violations.push(Violation::error_at(
                            Rule::Other,
                            token.text_range(),
                            "Anchor must be attached to a node, not at document level",
                        ));
                    }
                }
            }
        }
    }

    /// Check for both anchor and alias on the same value
    ///
    /// Per YAML spec, a node can have an anchor (defining a reusable node) OR
    /// be an alias (referencing another node), but not both.
    fn check_anchor_and_alias(&self, node: &SyntaxNode, violations: &mut Vec<Violation>) {
        let mut has_anchor = false;
        let mut has_alias = false;

        // Check for ANCHOR tokens at this level
        for child in node.children_with_tokens() {
            if let rowan::NodeOrToken::Token(token) = child {
                if token.kind() == crate::SyntaxKind::ANCHOR {
                    has_anchor = true;
                }
            }
        }

        // Check for REFERENCE tokens (used inside SCALAR-shaped values
        // and inside dedicated ALIAS nodes) anywhere among descendants.
        // We can't restrict to SCALAR-only because the parser emits
        // `&b *a` with the ALIAS node as a sibling of the ANCHOR token,
        // not wrapped in a SCALAR.
        for token in node
            .descendants_with_tokens()
            .filter_map(|el| el.into_token())
        {
            if token.kind() == crate::SyntaxKind::REFERENCE {
                has_alias = true;
                break;
            }
        }

        if has_anchor && has_alias {
            violations.push(Violation::error(
                Rule::Other,
                "Node cannot have both an anchor and be an alias",
            ));
        }
    }

    /// Check that comment tokens have whitespace separation
    ///
    /// YAML spec requires that comment markers (#) must be separated from other content
    /// by whitespace. This checks if a COMMENT token appears without preceding whitespace.
    fn check_comment_token_whitespace(
        &self,
        token: &rowan::SyntaxToken<crate::Lang>,
        violations: &mut Vec<Violation>,
    ) {
        // What matters is the token before the comment in document order,
        // not the previous sibling: a comment opening a VALUE (`hr: # c`)
        // has no sibling before it, and the separating whitespace sits one
        // level up. Walking siblings alone reported 5 files the test suite
        // marks valid.
        let Some(prev) = token.prev_token() else {
            return;
        };
        if !matches!(
            prev.kind(),
            crate::SyntaxKind::WHITESPACE | crate::SyntaxKind::NEWLINE | crate::SyntaxKind::INDENT
        ) {
            violations.push(Violation::error_at(
                Rule::Other,
                token.text_range(),
                "Comment without whitespace separation",
            ));
        }
    }

    /// Check that content doesn't appear on same line as document start marker
    ///
    /// According to YAML spec, content should not appear on the same line as
    /// a document start marker (---).
    fn check_doc_start_token_content(
        &self,
        token: &rowan::SyntaxToken<crate::Lang>,
        violations: &mut Vec<Violation>,
    ) {
        // Look at siblings after DOC_START token
        let mut found_newline = false;
        let mut found_content = false;

        // Check if there's content before a newline
        let mut current = token.next_sibling_or_token();
        while let Some(sibling) = current {
            let next = match &sibling {
                rowan::NodeOrToken::Token(t) => {
                    match t.kind() {
                        crate::SyntaxKind::NEWLINE => {
                            found_newline = true;
                            break;
                        }
                        crate::SyntaxKind::WHITESPACE | crate::SyntaxKind::COMMENT => {
                            // Whitespace and comments are OK
                        }
                        _ => {}
                    }
                    t.next_sibling_or_token()
                }
                rowan::NodeOrToken::Node(n) => {
                    // Only a *block* collection is illegal here. YAML 1.2
                    // `l-explicit-document` lets a node share the marker's
                    // line, so `--- a`, `--- >`, `--- {a: 1}` and a tagged
                    // node are all valid, as PyYAML reads them; a block
                    // mapping or sequence (`--- key: v`, `--- - a`) is not.
                    let is_flow = n.first_token().is_some_and(|t| {
                        matches!(
                            t.kind(),
                            crate::SyntaxKind::LEFT_BRACKET | crate::SyntaxKind::LEFT_BRACE
                        )
                    });
                    match n.kind() {
                        crate::SyntaxKind::MAPPING | crate::SyntaxKind::SEQUENCE if !is_flow => {
                            found_content = true;
                            break;
                        }
                        _ => {}
                    }
                    n.next_sibling_or_token()
                }
            };
            current = next;
        }

        if found_content && !found_newline {
            violations.push(Violation::error(
                Rule::InvalidDocumentMarker,
                "Content on same line as document start marker",
            ));
        }
    }

    /// Check that TAG tokens don't contain invalid characters
    ///
    /// YAML spec restricts which characters can appear in tags.
    /// Tags cannot contain: {, }, [, ], or comma (,)
    fn check_tag_characters(
        &self,
        token: &rowan::SyntaxToken<crate::Lang>,
        violations: &mut Vec<Violation>,
    ) {
        let tag_text = token.text();

        // A verbatim tag `!<...>` carries a URI, where these are all legal:
        // `!<tag:yaml.org,2002:str>` is the test suite's 7FWL and UGM3. The
        // restriction is on the shorthand form, whose `ns-tag-char` excludes
        // the flow indicators.
        if tag_text.starts_with("!<") {
            return;
        }

        // Check for invalid characters in tags
        let invalid_chars = ['{', '}', '[', ']', ','];
        for ch in invalid_chars {
            if tag_text.contains(ch) {
                violations.push(Violation::error_at(
                    Rule::InvalidTag,
                    token.text_range(),
                    format!("Invalid character '{ch}' in tag"),
                ));
                return; // Only report once per tag
            }
        }
    }

    /// Check that a TAG token is not immediately followed by a comma
    ///
    /// Per YAML spec, a tag must be followed by whitespace and then the tagged value.
    /// A comma immediately after a tag (without whitespace and value) is invalid.
    /// Example: `!!str, xxx` is invalid; should be `!!str xxx` or `!!str "xxx"`
    fn check_tag_followed_by_comma(
        &self,
        token: &rowan::SyntaxToken<crate::Lang>,
        violations: &mut Vec<Violation>,
    ) {
        // Look at the next sibling after the TAG token
        let mut current = token.next_sibling_or_token();

        // Skip whitespace to find the next meaningful element
        while let Some(sibling) = current {
            match &sibling {
                rowan::NodeOrToken::Token(t) => {
                    match t.kind() {
                        crate::SyntaxKind::WHITESPACE | crate::SyntaxKind::NEWLINE => {
                            // Whitespace is expected, continue to next
                            current = t.next_sibling_or_token();
                            continue;
                        }
                        crate::SyntaxKind::COMMA => {
                            // Found a comma directly after the tag - this is invalid
                            violations.push(Violation::error_at(
                                Rule::InvalidTag,
                                token.text_range(),
                                "Invalid comma after tag",
                            ));
                            return;
                        }
                        _ => {
                            // Found some other token - that's fine
                            return;
                        }
                    }
                }
                rowan::NodeOrToken::Node(n) => {
                    // Found a node - check if it's a SCALAR that starts with a comma
                    if n.kind() == crate::SyntaxKind::SCALAR {
                        // Check if the first token in this scalar is a comma
                        for child in n.children_with_tokens() {
                            if let rowan::NodeOrToken::Token(t) = child {
                                // In block context a `,` is scalar content, so
                                // the comma that makes this invalid shows up as
                                // the first character of the scalar rather than
                                // as its own COMMA token.
                                if t.kind() == crate::SyntaxKind::COMMA || t.text().starts_with(',')
                                {
                                    // The scalar starts with a comma - invalid after a tag
                                    violations.push(Violation::error(
                                        Rule::InvalidTag,
                                        "Invalid comma after tag",
                                    ));
                                    return;
                                } else if t.kind() != crate::SyntaxKind::WHITESPACE
                                    && t.kind() != crate::SyntaxKind::NEWLINE
                                {
                                    // Found a non-comma, non-whitespace token - that's fine
                                    return;
                                }
                            }
                        }
                    }
                    // Other node types are fine
                    return;
                }
            }
        }
    }

    /// Check that implicit keys don't span multiple lines
    ///
    /// YAML spec restricts implicit keys (keys without explicit ? marker) to a single line.
    /// This checks if a KEY node in a MAPPING_ENTRY contains newline characters.
    /// Two variants of "multiline" show up:
    ///   1. A NEWLINE token as a direct descendant (unquoted key spanning
    ///      multiple lexed tokens, e.g. yaml-test-suite 8KB6 in flow context).
    ///   2. A quoted STRING token whose source text contains a raw '\n'
    ///      (yaml-test-suite 7LBH / D49Q / JKF3). The whole quoted body
    ///      is one lex token, so a NEWLINE-token check misses it.
    fn check_implicit_key_multiline(
        &self,
        entry_node: &SyntaxNode,
        violations: &mut Vec<Violation>,
    ) {
        // Explicit-key entries (`? key\n : value`) are allowed to span
        // multiple lines by construction; the QUESTION indicator makes
        // them explicit rather than implicit.
        // The `?` sits in the MAPPING_ENTRY for a block explicit key, but
        // inside the KEY for a flow one (`{\n? explicit: entry,\n?\n}`), so
        // look in both -- the test suite's DFF7 has the latter.
        let is_explicit = has_child_token(entry_node, |k| k == crate::SyntaxKind::QUESTION)
            || entry_node
                .children()
                .filter(|c| c.kind() == crate::SyntaxKind::KEY)
                .any(|key| has_child_token(&key, |k| k == crate::SyntaxKind::QUESTION));
        if is_explicit {
            return;
        }

        for child in entry_node.children() {
            if child.kind() != crate::SyntaxKind::KEY {
                continue;
            }
            let spans_lines = child.descendants_with_tokens().any(|el| match el {
                rowan::NodeOrToken::Token(t) => {
                    t.kind() == crate::SyntaxKind::NEWLINE || t.text().contains('\n')
                }
                _ => false,
            });
            // A flow scalar may span lines, so a key inside a flow
            // collection is allowed to: the test suite's 8KB6, 9BXH, 9SA2
            // and NJ66 all key an entry with `{ multi\n  line: value}`. The
            // restriction is on an implicit key in block context (7LBH).
            // What matters is the collection this entry belongs to: a flow
            // one lets its keys span lines (8KB6's `- { multi\n  line: v}`),
            // a block one does not, even when the key is itself a flow
            // collection (C2SP's `[23\n]: 42`, DK4H).
            // A flow collection holds its own `{`/`[` as a direct child;
            // first_token() would instead reach into the key's flow
            // sequence, making C2SP's block mapping look like a flow one.
            let in_flow = entry_node
                .parent()
                .is_some_and(|m| starts_a_flow_collection(&m));
            if spans_lines && !in_flow {
                violations.push(Violation::error_at(
                    Rule::Other,
                    child.text_range(),
                    "Implicit key cannot span multiple lines",
                ));
                return; // Only report once per entry
            }
        }

        // The key can also be separated from its own COLON by a line break
        // (`[ "key"\n  :value ]`): the NEWLINE is then a sibling of KEY
        // rather than part of it.
        //
        // Inside a flow *mapping* that is an ordinary entry spread over
        // lines, which the test suite's 5MUD and K3WX (`{ "foo"\n  :bar }`)
        // allow; only a flow sequence needs an implicit key there, which is
        // ZXT5's error.
        if entry_node
            .parent()
            .is_some_and(|m| m.kind() == crate::SyntaxKind::MAPPING && starts_a_flow_collection(&m))
        {
            return;
        }
        let mut seen_key = false;
        for el in entry_node.children_with_tokens() {
            match el {
                rowan::NodeOrToken::Node(n) if n.kind() == crate::SyntaxKind::KEY => {
                    seen_key = true;
                }
                rowan::NodeOrToken::Token(t) if seen_key => match t.kind() {
                    crate::SyntaxKind::NEWLINE => {
                        violations.push(Violation::error_at(
                            Rule::Other,
                            entry_node.text_range(),
                            "Implicit key cannot span multiple lines",
                        ));
                        return;
                    }
                    crate::SyntaxKind::COLON => return,
                    _ => {}
                },
                _ => {}
            }
        }
    }

    /// Check for block sequence starting on same line as mapping key
    ///
    /// YAML 1.2 spec section 6.3.1 requires block sequences to start on a new line
    /// after the mapping key and colon. Example of invalid YAML:
    /// ```yaml
    /// key: - a
    ///      - b
    /// ```
    fn check_sequence_on_same_line_as_key(
        &self,
        entry_node: &SyntaxNode,
        violations: &mut Vec<Violation>,
    ) {
        use crate::SyntaxKind;

        // Find the KEY and VALUE nodes within the MAPPING_ENTRY
        let mut key_node: Option<SyntaxNode> = None;
        let mut value_node: Option<SyntaxNode> = None;

        for child in entry_node.children() {
            match child.kind() {
                SyntaxKind::KEY => key_node = Some(child),
                SyntaxKind::VALUE => value_node = Some(child),
                _ => {}
            }
        }

        // If there's no value, nothing to check
        let Some(value) = value_node else { return };

        // An explicit key's value may be a compact sequence on the `:` line
        // (`? k\n: - one\n  - two\n`), which YAML 1.2 allows and PyYAML
        // accepts. Only a plain key's `:` requires the line break, so this
        // rule does not apply to an entry introduced by `?`.
        if entry_node
            .children_with_tokens()
            .filter_map(|c| c.into_token())
            .any(|t| t.kind() == SyntaxKind::QUESTION)
        {
            return;
        }

        // Check if the value is a block sequence
        let mut sequence_node: Option<SyntaxNode> = None;
        for child in value.children() {
            if child.kind() == SyntaxKind::SEQUENCE {
                sequence_node = Some(child);
                break;
            }
        }

        let Some(sequence) = sequence_node else {
            return;
        };

        // Check if this is a block sequence (not flow)
        let first_token = sequence.first_token();
        let is_flow_sequence = first_token
            .as_ref()
            .is_some_and(|t| t.kind() == crate::SyntaxKind::LEFT_BRACKET);

        if is_flow_sequence {
            return; // Flow sequences can be on same line
        }

        // Check for a NEWLINE between key (or COLON) and the sequence

        // Find the COLON token that separates key and value
        let mut found_colon = false;
        let mut has_newline = false;

        // The parser puts the line break inside the VALUE when the sequence
        // is indentless (`k:\n- a\n`), where the entries sit at the key's
        // own column. That is valid YAML, so look there first; the scan
        // below only sees tokens at the MAPPING_ENTRY level.
        for child in value.children_with_tokens() {
            match child {
                rowan::NodeOrToken::Token(t) if t.kind() == SyntaxKind::NEWLINE => {
                    has_newline = true;
                    break;
                }
                rowan::NodeOrToken::Node(ref n) if n == &sequence => break,
                _ => {}
            }
        }

        if let Some(key) = key_node.filter(|_| !has_newline) {
            // Start from after the key
            let mut current = key.next_sibling_or_token();

            while let Some(element) = current {
                if let rowan::NodeOrToken::Token(t) = &element {
                    if t.kind() == SyntaxKind::COLON {
                        found_colon = true;
                    } else if found_colon && t.kind() == SyntaxKind::NEWLINE {
                        has_newline = true;
                        break;
                    }
                }

                // Stop if we reach the sequence node
                if let rowan::NodeOrToken::Node(n) = &element {
                    if n == &sequence {
                        break;
                    }
                }

                current = element.next_sibling_or_token();
            }
        }

        // If there's no newline between the colon and the sequence, it's invalid
        if !has_newline {
            violations.push(Violation::error(
                Rule::Other,
                "Block sequence cannot start on same line as mapping key",
            ));
        }
    }

    /// Check sequence items have consistent indentation (ZVH3)
    fn check_sequence_indentation(&self, node: &SyntaxNode, violations: &mut Vec<Violation>) {
        use crate::SyntaxKind;

        // Only check SEQUENCE nodes
        if node.kind() != SyntaxKind::SEQUENCE {
            return;
        }

        let mut dash_columns: Vec<usize> = Vec::new();

        // Recursively collect all DASH tokens in this sequence and nested sequences
        fn collect_dashes(
            node: &rowan::SyntaxNode<crate::Lang>,
            dashes: &mut Vec<rowan::SyntaxToken<crate::Lang>>,
        ) {
            for child in node.children_with_tokens() {
                match child {
                    rowan::NodeOrToken::Token(token) if token.kind() == crate::SyntaxKind::DASH => {
                        dashes.push(token);
                    }
                    rowan::NodeOrToken::Node(n)
                        if n.kind() == crate::SyntaxKind::SEQUENCE_ENTRY =>
                    {
                        // Collect dashes from sequence entries
                        collect_dashes(&n, dashes);
                    }
                    _ => {}
                }
            }
        }

        let mut dashes = Vec::new();
        collect_dashes(node, &mut dashes);

        for token in dashes {
            dash_columns.push(token_column(&token));
        }

        // Check if all dashes are at the same column (consistent indentation)
        if let Some(&first_col) = dash_columns.first() {
            for &col in &dash_columns[1..] {
                if col != first_col {
                    violations.push(Violation::error(
                        Rule::InvalidIndentation,
                        "Inconsistent sequence item indentation",
                    ));
                    return; // Only report once
                }
            }
        }
    }

    /// Check multiline quoted strings have proper indentation (QB6E)
    fn check_quoted_string_indentation(&self, node: &SyntaxNode, violations: &mut Vec<Violation>) {
        use crate::SyntaxKind;

        // Only check SCALAR nodes
        if node.kind() != SyntaxKind::SCALAR {
            return;
        }

        // Check if this is a quoted string that spans multiple lines
        let text = node.text().to_string();
        if !text.starts_with('"') && !text.starts_with('\'') {
            return; // Not a quoted string
        }

        if !text.contains('\n') {
            return; // Single line, no indentation to check
        }

        // A scalar that is the document's own node starts at column zero and
        // its continuations need clear no column, so there is nothing to
        // check: the test suite's 6WPF, TL85, Q8AD and NP9H all open a
        // quoted scalar at the document level. Inside a mapping value or a
        // sequence entry the continuation must clear the enclosing column.
        // A `---` or `...` at the start of a line ends the document, so it
        // cannot sit inside a quoted scalar: the test suite's 9MQT/01 is the
        // error `--- "a\n... x\nb"`. The lexer keeps the whole quoted body
        // in one token, so no document-marker rule sees it.
        if text
            .split('\n')
            .skip(1)
            .any(|line| line.starts_with("---") || line.starts_with("..."))
        {
            violations.push(Violation::error_at(
                Rule::InvalidDocumentMarker,
                node.text_range(),
                "Document marker inside a quoted scalar",
            ));
            return;
        }

        match node.parent() {
            None => return,
            Some(parent) if parent.kind() == crate::SyntaxKind::DOCUMENT => return,
            Some(_) => {}
        }

        // For multiline quoted strings, continuation lines should be indented

        // Check each line after the first
        let lines: Vec<&str> = text.split('\n').collect();
        if lines.len() > 1 {
            // Continuation lines (between opening and closing quote) should have consistent indentation
            // In YAML, they should be indented at least as much as the opening line
            for (i, line) in lines.iter().enumerate().skip(1) {
                if i == lines.len() - 1 && line.trim().is_empty() {
                    // Last line might just be the closing quote
                    continue;
                }

                // Count leading spaces
                let leading_spaces = line.len() - line.trim_start().len();

                // Continuation lines starting at column 0 are invalid
                // (they should be indented at least to align with content)
                if leading_spaces == 0 && !line.trim().is_empty() {
                    violations.push(Violation::error(
                        Rule::InvalidIndentation,
                        "Wrong indented multiline quoted scalar",
                    ));
                    return;
                }
            }
        }
    }

    /// Check for duplicate keys within a mapping node
    ///
    /// Uses semantic comparison via `yaml_eq()`:
    /// - `true` and `True` are duplicates (same boolean value)
    /// - `1` and `0x1` are duplicates (same integer value)
    /// - `"1"` and `1` are NOT duplicates (different types: string vs int)
    /// - `null`, `~`, and empty key are all duplicates (all null)
    /// - Works with complex keys (sequences, mappings) as well
    fn check_duplicate_keys(&self, node: &SyntaxNode, violations: &mut Vec<Violation>) {
        use crate::nodes::{Mapping, Scalar, Sequence};
        use crate::yaml_eq;
        use crate::SyntaxKind;
        use std::collections::HashMap;

        // Collect all KEY nodes with their text representation and parent entry range
        let keys: Vec<(SyntaxNode, String, rowan::TextRange)> = node
            .children()
            .filter(|child| child.kind() == SyntaxKind::MAPPING_ENTRY)
            .filter_map(|child| {
                let entry_range = child.text_range();
                child
                    .children()
                    .find(|n| n.kind() == SyntaxKind::KEY)
                    .map(|key_node| {
                        // Only allocate once: trim() returns &str, then to_string() once
                        let key_text = key_node.text().to_string();
                        let key_text = key_text.trim().to_string();
                        (key_node, key_text, entry_range)
                    })
            })
            .collect();

        // Compare semantically, but bucket scalar keys by their normalized
        // value first so the common all-scalar mapping costs one hash per key
        // instead of a pairwise sweep. Collection keys are rare and have no
        // cheap normal form, so they keep the pairwise comparison.
        let format_key = |s: &str| {
            if s.is_empty() {
                "\"\"".to_string()
            } else {
                format!("{s:?}")
            }
        };
        let mut report = |first_text: &str, dup_text: &str, at: rowan::TextRange| {
            violations.push(Violation::error_at(
                Rule::DuplicateKeys,
                at,
                format!(
                    "Duplicate key: {} (semantically equal to {})",
                    format_key(dup_text),
                    format_key(first_text)
                ),
            ));
        };

        // (normalized scalar key) -> index of the first key that produced it
        let mut seen_scalars: HashMap<(SyntaxKind, String), usize> = HashMap::new();
        let mut collection_keys: Vec<usize> = Vec::new();

        for i in 0..keys.len() {
            let Some(inner) = keys[i].0.children().next() else {
                continue;
            };
            let normalized = Scalar::cast(inner.clone())
                .filter(|_| inner.kind() == SyntaxKind::SCALAR)
                .and_then(|s| crate::as_yaml::scalar_semantic_value(&s));

            match normalized {
                Some(norm) => match seen_scalars.entry(norm) {
                    std::collections::hash_map::Entry::Occupied(first) => {
                        let first_idx = *first.get();
                        report(&keys[first_idx].1, &keys[i].1, keys[i].2);
                    }
                    std::collections::hash_map::Entry::Vacant(slot) => {
                        slot.insert(i);
                    }
                },
                None => {
                    // A collection key, or a scalar with no normal form.
                    for &j in &collection_keys {
                        let (Some(v1), Some(v2)) =
                            (keys[j].0.children().next(), keys[i].0.children().next())
                        else {
                            continue;
                        };
                        let are_equal = match (v1.kind(), v2.kind()) {
                            (SyntaxKind::SCALAR, SyntaxKind::SCALAR) => Scalar::cast(v1)
                                .zip(Scalar::cast(v2))
                                .is_some_and(|(s1, s2)| yaml_eq(&s1, &s2)),
                            (SyntaxKind::SEQUENCE, SyntaxKind::SEQUENCE) => Sequence::cast(v1)
                                .zip(Sequence::cast(v2))
                                .is_some_and(|(s1, s2)| yaml_eq(&s1, &s2)),
                            (SyntaxKind::MAPPING, SyntaxKind::MAPPING) => Mapping::cast(v1)
                                .zip(Mapping::cast(v2))
                                .is_some_and(|(m1, m2)| yaml_eq(&m1, &m2)),
                            _ => false,
                        };
                        if are_equal {
                            report(&keys[j].1, &keys[i].1, keys[i].2);
                            break;
                        }
                    }
                    collection_keys.push(i);
                }
            }
        }
    }
}

impl Default for Validator {
    fn default() -> Self {
        Self::new()
    }
}

/// Whether `node` is a flow collection, by its own delimiter.
///
/// A flow collection holds its `{`/`[` as a direct child; `first_token()`
/// instead descends the whole left spine, which made validating a large
/// mapping quadratic.
fn starts_a_flow_collection(node: &SyntaxNode) -> bool {
    // The delimiter opens the collection, so it is among the first children;
    // scanning them all is O(entries) and ran per entry.
    node.children_with_tokens()
        .take_while(|c| c.as_token().is_some())
        .filter_map(|c| c.into_token())
        .any(|t| {
            matches!(
                t.kind(),
                crate::SyntaxKind::LEFT_BRACE | crate::SyntaxKind::LEFT_BRACKET
            )
        })
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::str::FromStr;

    #[test]
    fn test_validator_basic() {
        let doc = Document::from_str("key: value").unwrap();
        let validator = Validator::new();
        let violations = validator.validate(&doc);

        // Simple valid YAML should have no violations
        assert_eq!(violations.len(), 0);
    }

    #[test]
    fn test_duplicate_key_detection_is_semantic() {
        // Keys that differ textually but mean the same thing are duplicates;
        // bucketing them by normalized value must not lose these.
        for (src, expected) in [
            ("a: 1\n\"a\": 2\n", 1),
            ("1: a\n0x1: b\n", 1),
            ("n: 1\nnull: 2\n~: 3\n", 1),
            ("[1,2]: a\n[1, 2]: b\n", 1),
            ("a: 1\nb: 2\n", 0),
        ] {
            let doc = Document::from_str(src).unwrap();
            let dups = Validator::new()
                .validate(&doc)
                .into_iter()
                .filter(|v| v.message.contains("Duplicate key"))
                .count();
            assert_eq!(dups, expected, "{src:?}");
        }
    }

    #[test]
    fn test_duplicate_key_detection_scales_linearly() {
        // This check used to compare every key against every other one, which
        // made validating a large generated mapping quadratic. Guard the
        // shape rather than a wall-clock number: 4x the keys should cost far
        // less than the 16x a pairwise sweep would.
        let build = |n: usize| {
            let mut s = String::new();
            for i in 0..n {
                s.push_str(&format!("key{i}: value{i}\n"));
            }
            Document::from_str(&s).unwrap()
        };
        let time = |doc: &Document| {
            let start = std::time::Instant::now();
            let _ = Validator::new().validate(doc);
            start.elapsed()
        };

        let small = build(500);
        let large = build(2000);
        // Warm up so the first parse does not skew the comparison.
        let _ = time(&small);
        let small_t = time(&small);
        let large_t = time(&large);

        assert!(
            large_t < small_t * 12,
            "validation looks superlinear: 500 keys took {small_t:?}, 2000 took {large_t:?}"
        );
    }

    #[test]
    fn test_validator_accepts_well_formed_flow_collections() {
        // The separators in a flow collection live inside the entry they
        // follow, so counting only the collection's direct children found no
        // commas at all and every well-formed flow collection was reported as
        // missing them. Flow entries are also SEQUENCE_ENTRY nodes, which used
        // to read as block syntax leaking into the flow.
        for src in [
            "a: {x: 1, y: 2}\n",
            "a: [1, 2]\n",
            "[1, 2, 3]\n",
            "{a: 1, b: 2, c: 3}\n",
            "a: !!seq [1, 2]\n",
            "a: &anc [1, 2]\n",
        ] {
            let doc = Document::from_str(src).unwrap();
            assert_eq!(Validator::new().validate(&doc), vec![], "{src:?}");
        }
    }

    #[test]
    fn test_validator_flags_malformed_flow_collections() {
        // Omitted entries and a stray block dash stay errors, and so do the
        // layout mistakes that the comma check used to catch only by
        // accident (yaml-test-suite 9C9N, ZXT5, 9MAG, CTN5, G5U8, YJV2).
        for src in [
            "[ , a, b ]\n",
            "[ a, b, , ]\n",
            "[-]\n",
            "- [-, -]\n",
            "flow: [a,\nb,\nc]\n",
            "[ \"key\"\n  :value ]\n",
        ] {
            let doc = Document::from_str(src).unwrap();
            assert_ne!(Validator::new().validate(&doc), vec![], "{src:?}");
        }
    }

    #[test]
    fn test_validator_inconsistent_sequence_indent() {
        let even = Document::from_str("- a\n- b\n").unwrap();
        let even_v = Validator::new().validate(&even);
        assert!(
            !even_v
                .iter()
                .any(|v| v.message.contains("Inconsistent sequence item indentation")),
            "{even_v:?}"
        );

        // `- a\n - b\n` is not two misaligned entries: a `-` indented past
        // the entry above it is plain-scalar content, so this is the single
        // item `a - b`, as both saphyr and PyYAML read it. The parser folds
        // it, leaving one DASH token and nothing for this rule to flag.
        let folded = Document::from_str("- a\n - b\n").unwrap();
        let folded_seq = folded.as_sequence().expect("sequence");
        assert_eq!(folded_seq.len(), 1);
        assert_eq!(
            folded_seq.get(0).unwrap().as_scalar().unwrap().as_string(),
            "a - b"
        );
        let folded_v = Validator::new().validate(&folded);
        assert!(
            !folded_v
                .iter()
                .any(|v| v.message.contains("Inconsistent sequence item indentation")),
            "{folded_v:?}"
        );
    }

    #[test]
    fn test_validator_tabs_debug() {
        let yaml = "---\na:\n\tb:\n\t\tc: value";
        let doc = Document::from_str(yaml).unwrap();
        let validator = Validator::new();

        // Walk the tree and check for tabs
        let mut found_tab = false;
        for child in doc.syntax().descendants_with_tokens() {
            if let rowan::NodeOrToken::Token(token) = child {
                if token.text().contains('\t') {
                    println!(
                        "Found tab in token: {:?} = {:?}",
                        token.kind(),
                        token.text()
                    );
                    found_tab = true;
                }
            }
        }

        println!("Found tab in tree: {}", found_tab);

        let violations = validator.validate(&doc);
        println!("Violations: {}", violations.len());
        for v in &violations {
            println!("  {}", v);
        }

        assert!(found_tab, "Tabs should be in the syntax tree");
    }

    #[test]
    fn test_validator_missing_comma() {
        let doc = Document::from_str("{foo: 1 bar: 2}").unwrap();
        let validator = Validator::new();
        let violations = validator.validate(&doc);

        // Should detect missing comma in flow mapping
        println!("Found {} violations:", violations.len());
        for v in &violations {
            println!("  {}", v);
        }

        assert!(
            !violations.is_empty(),
            "Expected violations for missing comma, got none"
        );
    }

    #[test]
    fn test_validator_invalid_escape() {
        let doc = Document::from_str("\"\\.\"\n").unwrap();
        let validator = Validator::new();
        let violations = validator.validate(&doc);

        // Should detect invalid escape sequence
        assert!(
            !violations.is_empty(),
            "Expected violations for invalid escape \\., got none"
        );
        assert_eq!(violations[0].rule, Rule::InvalidEscape);
    }

    #[test]
    fn test_validator_multiple_anchors() {
        // Test simple case
        let doc = Document::from_str("&a &b key: value").unwrap();
        let validator = Validator::new();
        let violations = validator.validate(&doc);

        assert!(
            !violations.is_empty(),
            "Expected violations for multiple anchors, got none"
        );
        assert_eq!(violations[0].rule, Rule::InvalidAnchor);

        // Test 4JVG case
        let yaml = "top1: &node1\n  &k1 key1: val1\ntop2: &node2\n  &v2 val2\n";
        let doc2 = Document::from_str(yaml).unwrap();
        let violations2 = validator.validate(&doc2);

        // Should detect 2 violations (one for each VALUE node with 2 anchors)
        assert!(
            violations2.len() >= 2,
            "Expected at least 2 violations for 4JVG"
        );
    }

    #[test]
    fn test_validator_duplicate_directive() {
        let yaml = "%YAML 1.2\n%YAML 1.2\n---\nkey: value\n";
        let doc = crate::YamlFile::from_str(yaml).unwrap().document().unwrap();
        let validator = Validator::new();
        let violations = validator.validate(&doc);

        assert_eq!(
            violations.len(),
            1,
            "Expected exactly one violation for duplicate YAML directive"
        );
        assert_eq!(violations[0].message, "Duplicate %YAML directive");
    }

    #[test]
    fn test_validator_duplicate_keys() {
        let yaml = "a: 1\nb: 2\na: 3\n";
        let doc = Document::from_str(yaml).unwrap();
        let validator = Validator::new();
        let violations = validator.validate(&doc);

        let dup_violations: Vec<_> = violations
            .iter()
            .filter(|v| v.rule == Rule::DuplicateKeys)
            .collect();
        assert_eq!(
            dup_violations.len(),
            1,
            "Expected exactly one DuplicateKeys violation, got: {:?}",
            dup_violations
        );
        assert_eq!(
            dup_violations[0].message,
            "Duplicate key: \"a\" (semantically equal to \"a\")"
        );
    }

    #[test]
    fn test_validator_no_duplicate_keys() {
        let yaml = "a: 1\nb: 2\nc: 3\n";
        let doc = Document::from_str(yaml).unwrap();
        let validator = Validator::new();
        let violations = validator.validate(&doc);

        let dup_violations: Vec<_> = violations
            .iter()
            .filter(|v| v.rule == Rule::DuplicateKeys)
            .collect();
        assert_eq!(
            dup_violations.len(),
            0,
            "Expected no DuplicateKeys violations"
        );
    }

    #[test]
    fn test_validator_duplicate_keys_disabled() {
        let yaml = "a: 1\nb: 2\na: 3\n";
        let doc = Document::from_str(yaml).unwrap();
        let validator = Validator::with_config(ValidatorConfig {
            check_duplicate_keys: false,
            ..ValidatorConfig::default()
        });
        let violations = validator.validate(&doc);

        let dup_violations: Vec<_> = violations
            .iter()
            .filter(|v| v.rule == Rule::DuplicateKeys)
            .collect();
        assert_eq!(
            dup_violations.len(),
            0,
            "Expected no violations when duplicate key check is disabled"
        );
    }

    #[test]
    fn test_validator_semantic_duplicate_keys() {
        let validator = Validator::new();

        // Test 1: Different quote styles - should be duplicates
        let yaml1 = "'a': 1\na: 2";
        let doc1 = Document::from_str(yaml1).unwrap();
        let violations1 = validator.validate(&doc1);
        assert_eq!(
            violations1
                .iter()
                .filter(|v| v.rule == Rule::DuplicateKeys)
                .count(),
            1,
            "Quoted 'a' and unquoted a should be duplicates"
        );

        // Test 2: Different boolean representations - should be duplicates
        let yaml2 = "true: 1\nTrue: 2";
        let doc2 = Document::from_str(yaml2).unwrap();
        let violations2 = validator.validate(&doc2);
        assert_eq!(
            violations2
                .iter()
                .filter(|v| v.rule == Rule::DuplicateKeys)
                .count(),
            1,
            "true and True should be duplicates"
        );

        // Test 3: Different integer representations - should be duplicates
        let yaml3 = "1: one\n0x1: hex";
        let doc3 = Document::from_str(yaml3).unwrap();
        let violations3 = validator.validate(&doc3);
        assert_eq!(
            violations3
                .iter()
                .filter(|v| v.rule == Rule::DuplicateKeys)
                .count(),
            1,
            "1 and 0x1 should be duplicates"
        );

        // Test 4: Different null representations - should be duplicates
        let yaml4 = "null: 1\n~: 2";
        let doc4 = Document::from_str(yaml4).unwrap();
        let violations4 = validator.validate(&doc4);
        assert_eq!(
            violations4
                .iter()
                .filter(|v| v.rule == Rule::DuplicateKeys)
                .count(),
            1,
            "null and ~ should be duplicates"
        );

        // Test 5: String vs int - should NOT be duplicates (different types)
        let yaml5 = "\"1\": string\n1: int";
        let doc5 = Document::from_str(yaml5).unwrap();
        let violations5 = validator.validate(&doc5);
        assert_eq!(
            violations5
                .iter()
                .filter(|v| v.rule == Rule::DuplicateKeys)
                .count(),
            0,
            "String '1' and int 1 should not be duplicates"
        );

        // Test 6: Float vs int - should NOT be duplicates (different types)
        let yaml6 = "1.0: float\n1: int";
        let doc6 = Document::from_str(yaml6).unwrap();
        let violations6 = validator.validate(&doc6);
        assert_eq!(
            violations6
                .iter()
                .filter(|v| v.rule == Rule::DuplicateKeys)
                .count(),
            0,
            "Float 1.0 and int 1 should not be duplicates"
        );
    }

    #[test]
    fn test_validator_directive_without_document() {
        // Test 9MMA: Directive without any document
        let yaml = "%YAML 1.2\n";
        let doc = crate::YamlFile::from_str(yaml)
            .unwrap()
            .document()
            .unwrap_or_default();

        // Debug: check if directive exists in tree
        let root = doc
            .syntax()
            .parent()
            .unwrap_or_else(|| doc.syntax().clone());
        let directive_count = root
            .descendants()
            .filter(|n| n.kind() == crate::SyntaxKind::DIRECTIVE)
            .count();
        let content_count = doc
            .syntax()
            .descendants()
            .filter(|n| {
                matches!(
                    n.kind(),
                    crate::SyntaxKind::MAPPING
                        | crate::SyntaxKind::SEQUENCE
                        | crate::SyntaxKind::SCALAR
                        | crate::SyntaxKind::TAGGED_NODE
                )
            })
            .count();

        let validator = Validator::new();
        let violations = validator.validate(&doc);

        // Only check if directives are actually in the tree
        if directive_count > 0 && content_count == 0 {
            assert!(
                !violations.is_empty(),
                "Expected violation for directive without document (directives={}, content={})",
                directive_count,
                content_count
            );
        }
    }

    #[test]
    fn test_validator_content_after_doc_end() {
        // Test 3HFZ: Content after document end marker. The suite marks it
        // an error case, and the parser now reports it, so take the tree
        // from parse(); the validator still detects it from the ERROR node.
        let yaml = "---\nkey: value\n... invalid\n";
        let parsed = crate::YamlFile::parse(yaml);
        assert_eq!(parsed.errors().len(), 1);
        let doc = parsed.tree().document().unwrap();

        let validator = Validator::new();
        let violations = validator.validate(&doc);

        let invalid_content_violations: Vec<_> = violations
            .iter()
            .filter(|v| v.message.starts_with("Invalid content in document:"))
            .collect();
        assert_eq!(
            invalid_content_violations.len(),
            1,
            "Expected exactly one 'Invalid content' violation for content after document end marker"
        );
    }

    #[test]
    fn test_validator_directive_with_tagged_node_content() {
        // A document with a tagged scalar following a directive should NOT be
        // reported as "directive without content" - TAGGED_NODE is real content.
        let yaml = "%YAML 1.2\n---\n!custom foo\n";
        let doc = crate::YamlFile::from_str(yaml).unwrap().document().unwrap();
        let validator = Validator::new();
        let violations = validator.validate(&doc);

        assert_eq!(
            violations.len(),
            0,
            "Tagged scalar is real content; valid document should have no violations"
        );
    }

    #[test]
    fn test_validator_with_config() {
        let config = ValidatorConfig {
            check_duplicate_keys: false,
            ..Default::default()
        };
        let validator = Validator::with_config(config);

        let doc = Document::from_str("key: value").unwrap();
        let violations = validator.validate(&doc);

        assert_eq!(violations.len(), 0);
    }

    #[test]
    fn test_violation_display() {
        let violation = Violation {
            message: "Test violation".to_string(),
            location: Some("1:5".to_string()),
            text_range: None,
            severity: Severity::Error,
            rule: Rule::InvalidIndentation,
        };

        assert_eq!(
            format!("{}", violation),
            "[ERROR] 1:5: Test violation (InvalidIndentation)"
        );
    }

    #[test]
    fn test_u99r_invalid_comma_in_tag() {
        // Test U99R: Invalid comma after tag
        let yaml = "- !!str, xxx\n";
        use crate::YamlFile;
        let file = YamlFile::from_str(yaml).unwrap();
        let validator = Validator::new();

        // Print tree for debugging
        println!("\n=== Syntax tree ===");
        crate::debug::print_tree(file.syntax());

        let violations = validator.validate_syntax(file.syntax());
        println!("\n=== Violations ({}) ===", violations.len());
        for v in &violations {
            println!("  {}", v);
        }

        assert!(
            !violations.is_empty(),
            "Expected violation for invalid comma after tag"
        );
        assert_eq!(violations.len(), 1);
        assert_eq!(violations[0].message, "Invalid comma after tag");
        assert_eq!(violations[0].rule, Rule::InvalidTag);
    }

    #[test]
    fn test_comment_whitespace() {
        use crate::YamlFile;

        // Comment must be separated by whitespace
        let yaml = "key: \"value\"# invalid comment\n";
        let parsed = YamlFile::from_str(yaml).expect("Should parse");

        let validator = Validator::new();
        let violations = validator.validate_syntax(parsed.syntax());

        assert!(
            !violations.is_empty(),
            "Should catch comment without whitespace"
        );
        assert_eq!(
            violations[0].message,
            "Comment without whitespace separation"
        );
    }

    #[test]
    fn test_doc_start_content() {
        use crate::YamlFile;

        // Content should not appear on same line as document start marker
        let yaml = "--- key1: value1\n    key2: value2\n";
        let parsed = YamlFile::from_str(yaml).expect("Should parse");

        let validator = Validator::new();
        let violations = validator.validate_syntax(parsed.syntax());

        assert!(
            !violations.is_empty(),
            "Should catch content on doc start line"
        );
        assert_eq!(
            violations[0].message,
            "Content on same line as document start marker"
        );
    }

    #[test]
    fn test_directive_in_document_content() {
        // %YAML directive after --- without preceding ... is invalid
        let input = "%YAML 1.2\n---\n%YAML 1.2\n---\n";
        let file = crate::YamlFile::from_str(input).unwrap();
        let validator = Validator::new();
        use rowan::ast::AstNode;
        let violations = validator.validate_syntax(file.syntax());

        assert_eq!(
            violations.len(),
            1,
            "Expected one violation for directive in content, got: {:?}",
            violations
        );
        assert_eq!(
            violations[0].message,
            "Directive in document content (missing document end marker `...` before directive)"
        );
    }
}
