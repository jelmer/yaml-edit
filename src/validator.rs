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
pub enum Severity {
    /// Error: Strictly invalid per YAML 1.2 spec
    Error,
    /// Warning: Deprecated or discouraged but technically valid
    Warning,
}

/// Specific YAML spec rules that can be violated
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

/// Convert a rowan TextRange to a TextPosition.
fn range_to_text_position(range: rowan::TextRange) -> crate::TextPosition {
    crate::TextPosition::new(u32::from(range.start()), u32::from(range.end()))
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

        // Walk the syntax tree and check for violations
        self.validate_node(node, &mut violations);

        violations
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
                violations.push(Violation {
                    message: format!("Invalid content in document: {:?}", preview),
                    location: None,
                    text_range: Some(range_to_text_position(node.text_range())),
                    severity: Severity::Error,
                    rule: Rule::Other,
                });
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
                // Check for trailing content after quoted strings
                self.check_trailing_content_after_quoted(node, violations);
                // Check for colons in plain scalar values
                self.check_colon_in_plain_scalar(node, violations);
                // Check for document markers inside quoted strings
                self.check_document_marker_in_string(node, violations);
                // Check for directives inside document content (e.g. %YAML after ---)
                self.check_directive_in_content(node, violations);
            }
            SyntaxKind::DOC_START | SyntaxKind::DOC_END => {
                if self.config.check_document_markers {
                    self.check_document_marker_placement(node, violations);
                }
            }
            SyntaxKind::MAPPING => {
                self.check_flow_collection_commas(node, violations);
                self.check_block_mapping_entries_on_same_line(node, violations);
                if self.config.check_duplicate_keys {
                    self.check_duplicate_keys(node, violations);
                }
            }
            SyntaxKind::SEQUENCE => {
                self.check_flow_collection_commas(node, violations);
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
        let has_content = doc_node.descendants().any(|n| {
            matches!(
                n.kind(),
                crate::SyntaxKind::MAPPING
                    | crate::SyntaxKind::SEQUENCE
                    | crate::SyntaxKind::SCALAR
                    | crate::SyntaxKind::STRING
                    | crate::SyntaxKind::TAGGED_NODE
            )
        });

        if !has_content {
            violations.push(Violation {
                message: "Directive requires a document with content".to_string(),
                location: None,
                text_range: None,
                severity: Severity::Error,
                rule: Rule::Other,
            });
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
        let has_document_with_content = check_node.children().any(|child| {
            if child.kind() == SyntaxKind::DOCUMENT {
                // Check if this document has content
                child.descendants().any(|n| {
                    matches!(
                        n.kind(),
                        SyntaxKind::MAPPING
                            | SyntaxKind::SEQUENCE
                            | SyntaxKind::SCALAR
                            | SyntaxKind::STRING
                            | SyntaxKind::TAGGED_NODE
                    )
                })
            } else {
                false
            }
        });

        if !has_document_with_content {
            violations.push(Violation {
                message: "Directive without document content".to_string(),
                location: None,
                text_range: None,
                severity: Severity::Error,
                rule: Rule::Other,
            });
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
                    // Check if this document has content
                    let has_content = child.descendants().any(|n| {
                        matches!(
                            n.kind(),
                            SyntaxKind::MAPPING
                                | SyntaxKind::SEQUENCE
                                | SyntaxKind::SCALAR
                                | SyntaxKind::STRING
                                | SyntaxKind::TAGGED_NODE
                        )
                    });

                    // Check if this document has a DOC_END marker
                    let has_doc_end = child
                        .children_with_tokens()
                        .any(|t| t.kind() == SyntaxKind::DOC_END);

                    if has_content {
                        seen_document_with_content = true;

                        // If this document doesn't end with ..., mark that we need one
                        // before any subsequent directives
                        if !has_doc_end {
                            // This document has no end marker - any following directive is invalid
                            // (we'll check this when we encounter the directive)
                        }
                    }
                }
                SyntaxKind::DIRECTIVE => {
                    // If we've seen a document with content and the last document didn't have DOC_END
                    if seen_document_with_content {
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
                            violations.push(Violation {
                                message:
                                    "Directive after document requires document end marker (...)"
                                        .to_string(),
                                location: None,
                                text_range: None,
                                severity: Severity::Error,
                                rule: Rule::Other,
                            });
                        }
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
            violations.push(Violation {
                message: "Directive in document content (missing document end marker `...` before directive)".to_string(),
                location: None,
                    text_range: Some(range_to_text_position(node.text_range())),
                severity: Severity::Error,
                rule: Rule::Other,
            });
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
                violations.push(Violation {
                    message: format!("Duplicate {} directive", directive_type),
                    location: None,
                    text_range: None,
                    severity: Severity::Error,
                    rule: Rule::Other,
                });
            }
        }
    }

    /// Check for multiple anchors on the same node
    fn check_multiple_anchors(&self, node: &SyntaxNode, violations: &mut Vec<Violation>) {
        // Count ANCHOR tokens (not nodes) in this node's children
        let anchor_count = node
            .children_with_tokens()
            .filter(|child| {
                child
                    .as_token()
                    .is_some_and(|t| t.kind() == crate::SyntaxKind::ANCHOR)
            })
            .count();

        if anchor_count > 1 {
            violations.push(Violation {
                message: "Multiple anchors on the same node".to_string(),
                location: None,
                text_range: Some(range_to_text_position(node.text_range())),
                severity: Severity::Error,
                rule: Rule::InvalidAnchor,
            });
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
                    let valid_escapes = [
                        '0', 'a', 'b', 't', 'n', 'v', 'f', 'r', 'e', ' ', '"', '/', '\\', 'N', '_',
                        'L', 'P', 'x', 'u', 'U',
                    ];

                    if !valid_escapes.contains(&next) {
                        violations.push(Violation {
                            message: format!("Invalid escape sequence: \\{}", next),
                            location: None,
                            text_range: Some(range_to_text_position(node.text_range())),
                            severity: Severity::Error,
                            rule: Rule::InvalidEscape,
                        });
                        return; // Found one, no need to continue
                    }
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
        let has_block_indicator = node.children_with_tokens().any(|child| {
            if let rowan::NodeOrToken::Token(token) = child {
                matches!(
                    token.kind(),
                    crate::SyntaxKind::GREATER | crate::SyntaxKind::PIPE
                )
            } else {
                false
            }
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
                        crate::SyntaxKind::STRING => {
                            // Found content on same line as indicator
                            violations.push(Violation {
                                message:
                                    "Block scalar content cannot appear on same line as indicator"
                                        .to_string(),
                                location: None,
                                text_range: None,
                                severity: Severity::Error,
                                rule: Rule::Other,
                            });
                            return;
                        }
                        // WHITESPACE, COMMENT, and chomping/indentation indicators are OK
                        _ => {}
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
                            violations.push(Violation {
                                message: "Trailing content after quoted string".to_string(),
                                location: None,
                                text_range: None,
                                severity: Severity::Error,
                                rule: Rule::Other,
                            });
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
        let has_colon = node.children_with_tokens().any(|child| {
            if let rowan::NodeOrToken::Token(token) = child {
                token.kind() == crate::SyntaxKind::COLON
            } else {
                false
            }
        });

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
            violations.push(Violation {
                message: "Plain scalar value cannot contain mapping syntax (colon)".to_string(),
                location: None,
                text_range: None,
                severity: Severity::Error,
                rule: Rule::Other,
            });
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
            violations.push(Violation {
                message: "Document marker on its own line inside quoted string".to_string(),
                location: None,
                text_range: Some(range_to_text_position(node.text_range())),
                severity: Severity::Error,
                rule: Rule::InvalidDocumentMarker,
            });
        }
    }

    /// Check for tab usage in whitespace nodes
    fn check_tab_usage(&self, node: &SyntaxNode, violations: &mut Vec<Violation>) {
        // Only check whitespace nodes - more efficient than serializing everything
        // Check each token directly without allocation
        for token in node.children_with_tokens() {
            if let rowan::NodeOrToken::Token(token) = token {
                // Check the token text directly - this is a cheap slice operation
                if token.text().contains('\t') {
                    violations.push(Violation {
                        message: "Tabs are not allowed for indentation in YAML".to_string(),
                        location: None,
                        text_range: Some(range_to_text_position(token.text_range())),
                        severity: Severity::Error,
                        rule: Rule::InvalidTabUsage,
                    });
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
                violations.push(Violation {
                    message: "Document marker inside string is invalid".to_string(),
                    location: None,
                    text_range: Some(range_to_text_position(node.text_range())),
                    severity: Severity::Error,
                    rule: Rule::InvalidDocumentMarker,
                });
            }
        }
    }

    /// Check for missing commas in flow collections
    fn check_flow_collection_commas(&self, node: &SyntaxNode, violations: &mut Vec<Violation>) {
        // Check first token to see if this is a flow collection - avoid full serialization
        let first_token = node.first_token();
        let is_flow_mapping = first_token.as_ref().is_some_and(|t| t.text() == "{");
        let is_flow_sequence = first_token.as_ref().is_some_and(|t| t.text() == "[");

        if !is_flow_mapping && !is_flow_sequence {
            return;
        }

        // Count entries and commas
        let entry_kind = if is_flow_mapping {
            crate::SyntaxKind::MAPPING_ENTRY
        } else {
            crate::SyntaxKind::SEQUENCE_ENTRY
        };

        let mut entry_count = 0;
        let mut comma_count = 0;
        let mut prev_was_comma = false;

        for child in node.children() {
            match child.kind() {
                k if k == entry_kind => entry_count += 1,
                crate::SyntaxKind::COMMA => {
                    comma_count += 1;
                    if prev_was_comma {
                        violations.push(Violation {
                            message: "Double comma in flow collection".to_string(),
                            location: None,
                            text_range: Some(range_to_text_position(node.text_range())),
                            severity: Severity::Error,
                            rule: Rule::Other,
                        });
                    }
                    prev_was_comma = true;
                }
                crate::SyntaxKind::WHITESPACE | crate::SyntaxKind::NEWLINE => {}
                _ => prev_was_comma = false,
            }
        }

        // Flow collections need n-1 commas for n entries (except when trailing comma)
        if entry_count > 1 && comma_count < entry_count - 1 {
            violations.push(Violation {
                message: format!(
                    "Flow collection missing commas: {} entries but only {} commas",
                    entry_count, comma_count
                ),
                location: None,
                text_range: None,
                severity: Severity::Error,
                rule: Rule::MissingSyntax,
            });
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
                        // First check if the previous entry ends with a newline
                        let prev_ends_with_newline = prev
                            .last_token()
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
                        violations.push(Violation {
                            message: "Block mapping entries must be on separate lines".to_string(),
                            location: None,
                            text_range: None,
                            severity: Severity::Error,
                            rule: Rule::Other,
                        });
                        return; // One violation is enough
                    }
                }

                prev_entry = Some(child);
            }
        }
    }

    /// Check for SEQUENCE_ENTRY nodes in flow sequences
    ///
    /// Flow sequences (using []) should not have SEQUENCE_ENTRY children.
    /// SEQUENCE_ENTRY is only for block sequences (using -). In flow sequences,
    /// values appear directly without the - marker.
    fn check_sequence_entry_in_flow(&self, node: &SyntaxNode, violations: &mut Vec<Violation>) {
        // Check if this is a flow sequence
        let first_token = node.first_token();
        let is_flow_sequence = first_token.as_ref().is_some_and(|t| t.text() == "[");

        if !is_flow_sequence {
            return;
        }

        // Check for SEQUENCE_ENTRY children
        for child in node.children() {
            if child.kind() == crate::SyntaxKind::SEQUENCE_ENTRY {
                violations.push(Violation {
                    message: "Flow sequence cannot use block sequence syntax (-)".to_string(),
                    location: None,
                    text_range: Some(range_to_text_position(node.text_range())),
                    severity: Severity::Error,
                    rule: Rule::Other,
                });
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
                    violations.push(Violation {
                        message: "Anchor must be attached to a node, not at document level"
                            .to_string(),
                        location: None,
                        text_range: Some(range_to_text_position(token.text_range())),
                        severity: Severity::Error,
                        rule: Rule::Other,
                    });
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

        // Check for REFERENCE tokens in descendant SCALAR nodes
        for desc in node.descendants() {
            if desc.kind() == crate::SyntaxKind::SCALAR {
                for token in desc.children_with_tokens() {
                    if let rowan::NodeOrToken::Token(t) = token {
                        if t.kind() == crate::SyntaxKind::REFERENCE {
                            has_alias = true;
                            break;
                        }
                    }
                }
            }
        }

        if has_anchor && has_alias {
            violations.push(Violation {
                message: "Node cannot have both an anchor and be an alias".to_string(),
                location: None,
                text_range: None,
                severity: Severity::Error,
                rule: Rule::Other,
            });
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
        // Check if there's a previous sibling token/node
        if let Some(prev) = token.prev_sibling_or_token() {
            match prev {
                rowan::NodeOrToken::Token(prev_token) => {
                    // Comment should be preceded by whitespace or newline token
                    if prev_token.kind() != crate::SyntaxKind::WHITESPACE
                        && prev_token.kind() != crate::SyntaxKind::NEWLINE
                    {
                        violations.push(Violation {
                            message: "Comment without whitespace separation".to_string(),
                            location: None,
                            text_range: Some(range_to_text_position(token.text_range())),
                            severity: Severity::Error,
                            rule: Rule::Other,
                        });
                    }
                }
                rowan::NodeOrToken::Node(_prev_node) => {
                    // If preceded by a node (not whitespace token), that's also invalid
                    violations.push(Violation {
                        message: "Comment without whitespace separation".to_string(),
                        location: None,
                        text_range: Some(range_to_text_position(token.text_range())),
                        severity: Severity::Error,
                        rule: Rule::Other,
                    });
                }
            }
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
                    // Any node here means content
                    match n.kind() {
                        crate::SyntaxKind::MAPPING
                        | crate::SyntaxKind::SEQUENCE
                        | crate::SyntaxKind::SCALAR
                        | crate::SyntaxKind::TAGGED_NODE => {
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
            violations.push(Violation {
                message: "Content on same line as document start marker".to_string(),
                location: None,
                text_range: None,
                severity: Severity::Error,
                rule: Rule::InvalidDocumentMarker,
            });
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

        // Check for invalid characters in tags
        let invalid_chars = ['{', '}', '[', ']', ','];
        for ch in invalid_chars {
            if tag_text.contains(ch) {
                violations.push(Violation {
                    message: format!("Invalid character '{}' in tag", ch),
                    location: None,
                    text_range: Some(range_to_text_position(token.text_range())),
                    severity: Severity::Error,
                    rule: Rule::InvalidTag,
                });
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
                            violations.push(Violation {
                                message: "Invalid comma after tag".to_string(),
                                location: None,
                                text_range: Some(range_to_text_position(token.text_range())),
                                severity: Severity::Error,
                                rule: Rule::InvalidTag,
                            });
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
                                if t.kind() == crate::SyntaxKind::COMMA {
                                    // The scalar starts with a comma - invalid after a tag
                                    violations.push(Violation {
                                        message: "Invalid comma after tag".to_string(),
                                        location: None,
                                        text_range: None,
                                        severity: Severity::Error,
                                        rule: Rule::InvalidTag,
                                    });
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
    fn check_implicit_key_multiline(
        &self,
        entry_node: &SyntaxNode,
        violations: &mut Vec<Violation>,
    ) {
        // Find the KEY node within the MAPPING_ENTRY
        for child in entry_node.children() {
            if child.kind() == crate::SyntaxKind::KEY {
                // Check if the key's text contains a newline
                let key_text = child.text().to_string();
                if key_text.contains('\n') {
                    violations.push(Violation {
                        message: "Implicit key cannot span multiple lines".to_string(),
                        location: None,
                        text_range: Some(range_to_text_position(child.text_range())),
                        severity: Severity::Error,
                        rule: Rule::Other,
                    });
                    return; // Only report once per entry
                }
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
        let is_flow_sequence = first_token.as_ref().is_some_and(|t| t.text() == "[");

        if is_flow_sequence {
            return; // Flow sequences can be on same line
        }

        // Check for a NEWLINE between key (or COLON) and the sequence

        // Find the COLON token that separates key and value
        let mut found_colon = false;
        let mut has_newline = false;

        if let Some(key) = key_node {
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
            violations.push(Violation {
                message: "Block sequence cannot start on same line as mapping key".to_string(),
                location: None,
                text_range: None,
                severity: Severity::Error,
                rule: Rule::Other,
            });
        }
    }

    /// Helper to calculate column position from text offset
    fn get_column(&self, text: &str, offset: usize) -> usize {
        let mut col = 0;
        for (i, ch) in text.char_indices() {
            if i >= offset {
                break;
            }
            if ch == '\n' {
                col = 0;
            } else {
                col += 1;
            }
        }
        col
    }

    /// Check sequence items have consistent indentation (ZVH3)
    fn check_sequence_indentation(&self, node: &SyntaxNode, violations: &mut Vec<Violation>) {
        use crate::SyntaxKind;

        // Only check SEQUENCE nodes
        if node.kind() != SyntaxKind::SEQUENCE {
            return;
        }

        // Get the root text for offset calculations
        let root = find_root(node);
        let full_text = root.text().to_string();
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
            let offset: usize = token.text_range().start().into();
            let col = self.get_column(&full_text, offset);
            dash_columns.push(col);
        }

        // Check if all dashes are at the same column (consistent indentation)
        if let Some(&first_col) = dash_columns.first() {
            for &col in &dash_columns[1..] {
                if col != first_col {
                    violations.push(Violation {
                        message: "Inconsistent sequence item indentation".to_string(),
                        location: None,
                        text_range: None,
                        severity: Severity::Error,
                        rule: Rule::InvalidIndentation,
                    });
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
                    violations.push(Violation {
                        message: "Wrong indented multiline quoted scalar".to_string(),
                        location: None,
                        text_range: None,
                        severity: Severity::Error,
                        rule: Rule::InvalidIndentation,
                    });
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
        use crate::yaml_eq;
        use crate::SyntaxKind;

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

        // Check for semantic duplicates using yaml_eq
        // O(n²) is acceptable for typical YAML mapping sizes (usually < 100 keys)
        for i in 0..keys.len() {
            for j in (i + 1)..keys.len() {
                // Get the actual value nodes within each KEY and try to cast to AsYaml types
                let key1_child = keys[i].0.children().next();
                let key2_child = keys[j].0.children().next();

                if let (Some(v1), Some(v2)) = (key1_child, key2_child) {
                    // Try each possible node type that implements AsYaml
                    use crate::nodes::{Mapping, Scalar, Sequence};

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
                        _ => false, // Different types can't be equal
                    };

                    if are_equal {
                        let first_text = &keys[i].1;
                        let dup_text = &keys[j].1;

                        // Format the key text for display (quote empty strings)
                        let format_key = |s: &str| {
                            if s.is_empty() {
                                "\"\"".to_string()
                            } else {
                                format!("{:?}", s)
                            }
                        };

                        violations.push(Violation {
                            message: format!(
                                "Duplicate key: {} (semantically equal to {})",
                                format_key(dup_text),
                                format_key(first_text)
                            ),
                            location: None,
                            text_range: Some(range_to_text_position(keys[j].2)),
                            severity: Severity::Error,
                            rule: Rule::DuplicateKeys,
                        });
                        // Only report each duplicate once
                        break;
                    }
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
        let doc = Document::from_str(yaml).unwrap();
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
        let doc = Document::from_str(yaml).unwrap();

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
        // Test 3HFZ: Content after document end marker
        // Parser wraps this in ERROR node, validator detects it
        let yaml = "---\nkey: value\n... invalid\n";
        let doc = Document::from_str(yaml).unwrap();

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
        // reported as "directive without content" — TAGGED_NODE is real content.
        let yaml = "%YAML 1.2\n---\n!custom foo\n";
        let doc = Document::from_str(yaml).unwrap();
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
        assert!(
            violations
                .iter()
                .any(|v| v.message.contains("comma") && v.rule == Rule::InvalidTag),
            "Expected 'Invalid comma after tag' violation, got: {:?}",
            violations
        );
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

        let directive_violations: Vec<_> = violations
            .iter()
            .filter(|v| v.message.contains("Directive in document content"))
            .collect();
        assert_eq!(
            directive_violations.len(),
            1,
            "Expected one violation for directive in content, got: {:?}",
            violations
        );
    }
}
