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

use crate::nodes::{
    has_child_token, in_flow_collection, is_block_scalar, is_quoted_scalar,
    starts_a_flow_collection,
};
use crate::yaml::{Document, SyntaxNode};
use rowan::ast::AstNode;
use std::fmt;

/// A YAML specification violation found during validation
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Violation {
    /// Human-readable description of the violation
    pub message: String,
    /// Location in the source, as 1-based `line:column` of the range start
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
    /// A construct that YAML 1.1 read differently from YAML 1.2
    LegacyYaml11,
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

/// The stream's directives, grouped by the document they introduce. Every
/// document gets a group, possibly empty; directives that no document
/// follows form a last group with `None`.
fn directive_groups(root: &SyntaxNode) -> Vec<(Vec<SyntaxNode>, Option<SyntaxNode>)> {
    let mut groups = Vec::new();
    let mut directives = Vec::new();
    for child in root.children() {
        match child.kind() {
            crate::SyntaxKind::DIRECTIVE => directives.push(child),
            crate::SyntaxKind::DOCUMENT => {
                groups.push((std::mem::take(&mut directives), Some(child)));
            }
            _ => {}
        }
    }
    if !directives.is_empty() {
        groups.push((directives, None));
    }
    groups
}

/// The handle a `%TAG` directive declares.
fn tag_handle(directive: &SyntaxNode) -> Option<String> {
    let text = directive.text().to_string();
    let mut words = text.split_whitespace();
    if words.next() != Some("%TAG") {
        return None;
    }
    words.next().map(str::to_string)
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

/// The zero-based column `token` starts at.
///
/// Walks back through preceding tokens to the last line break rather than
/// serializing the document: this runs for many tokens, and rebuilding the
/// whole text each time made validation quadratic in the file size.
fn token_column(token: &crate::nodes::SyntaxToken) -> usize {
    let mut col = 0;
    let mut cur = token.prev_token();
    while let Some(t) = cur {
        // A byte order mark is not part of the line's content.
        if t.kind() == crate::SyntaxKind::BOM {
            break;
        }
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

/// The least indentation a continuation line of `node` needs: one more
/// than the column of the innermost block collection entry holding it, or
/// `None` outside any block collection.
fn continuation_indent(node: &SyntaxNode) -> Option<usize> {
    let entry = node.ancestors().skip(1).find(|a| {
        matches!(
            a.kind(),
            crate::SyntaxKind::MAPPING_ENTRY | crate::SyntaxKind::SEQUENCE_ENTRY
        ) && a.parent().is_some_and(|c| !starts_a_flow_collection(&c))
    })?;
    Some(token_column(&entry.first_token()?) + 1)
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

/// Set each violation's `line:column` location from its range, using the
/// text of the tree `node` belongs to.
fn fill_locations(node: &SyntaxNode, violations: &mut [Violation]) {
    let tree = node.ancestors().last().unwrap_or_else(|| node.clone());
    let text = tree.text().to_string();
    let line_starts: Vec<usize> = std::iter::once(0)
        .chain(text.match_indices('\n').map(|(i, _)| i + 1))
        .collect();
    for v in violations.iter_mut() {
        let Some(range) = v.text_range else {
            continue;
        };
        let offset = range.start as usize;
        let line = line_starts.partition_point(|&s| s <= offset) - 1;
        let column = text[line_starts[line]..offset].chars().count();
        v.location = Some(crate::LineColumn::new(line + 1, column + 1).to_string());
    }
}

fn range_to_text_position(range: rowan::TextRange) -> crate::TextPosition {
    crate::TextPosition::new(u32::from(range.start()), u32::from(range.end()))
}

impl Violation {
    /// An error-severity violation covering `range` in the source.
    ///
    /// `location` is filled in once validation knows the source text.
    fn error_at(rule: Rule, range: rowan::TextRange, message: impl Into<String>) -> Self {
        Violation {
            message: message.into(),
            location: None,
            text_range: Some(range_to_text_position(range)),
            severity: Severity::Error,
            rule,
        }
    }

    /// A warning-severity violation covering `range` in the source.
    ///
    /// The document is valid YAML 1.2; a warning says only that some other
    /// implementation may read it differently.
    fn warning_at(rule: Rule, range: rowan::TextRange, message: impl Into<String>) -> Self {
        Violation {
            severity: Severity::Warning,
            ..Violation::error_at(rule, range, message)
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

        // Only the directives that introduce this document apply to it.
        if let Some((directives, document)) = directive_groups(&find_root(doc.syntax()))
            .into_iter()
            .find(|(_, d)| d.as_ref() == Some(doc.syntax()))
        {
            self.check_directives(&directives, document.as_ref(), &mut violations);
        }

        // Walk the syntax tree and check for violations
        // This will catch ERROR nodes created by parser (including content after doc end)
        self.validate_node(doc.syntax(), &mut violations);

        fill_locations(doc.syntax(), &mut violations);
        violations
    }

    /// Validate from a syntax node (can be ROOT, DOCUMENT, or any node)
    ///
    /// This is useful when you need to validate the full parse tree including directives
    /// that may not be attached to a specific document.
    pub fn validate_syntax(&self, node: &SyntaxNode) -> Vec<Violation> {
        let mut violations = Vec::new();

        for (directives, document) in directive_groups(&find_root(node)) {
            self.check_directives(&directives, document.as_ref(), &mut violations);
        }

        // Check for directives after documents without document end marker
        self.check_directive_after_document(node, &mut violations);

        // Check that a `%TAG` shorthand is only used in its own document
        self.check_tag_shorthand_scope(node, &mut violations);

        // Walk the syntax tree and check for violations
        self.validate_node(node, &mut violations);

        fill_locations(node, &mut violations);
        violations
    }

    /// Check that a `%TAG` shorthand is only used in the document that
    /// declares it.
    ///
    /// A directive applies to the document it introduces, not to the whole
    /// stream, so `!prefix!` declared before the first `---` is undefined in
    /// every later document. The YAML test suite's QLJ7 is exactly that.
    fn check_tag_shorthand_scope(&self, node: &SyntaxNode, violations: &mut Vec<Violation>) {
        // Handles declared for earlier documents. A document that declares
        // the same handle again (the test suite's 5TYM) may use it.
        let mut declared_earlier: Vec<String> = Vec::new();

        for (directives, document) in directive_groups(&find_root(node)) {
            let own: Vec<String> = directives.iter().filter_map(tag_handle).collect();
            if let Some(document) = document {
                for tag in document
                    .descendants_with_tokens()
                    .filter_map(|c| c.into_token())
                    .filter(|t| t.kind() == crate::SyntaxKind::TAG)
                {
                    // Only a named handle (`!prefix!A`) can be undefined;
                    // `!` and `!!` fall back to their defaults.
                    let text = tag.text();
                    let Some(named) = text
                        .strip_prefix('!')
                        .and_then(|rest| rest.find('!'))
                        .filter(|&i| i > 0)
                        .map(|i| &text[..i + 2])
                    else {
                        continue;
                    };
                    if let Some(handle) = declared_earlier
                        .iter()
                        .find(|h| h.as_str() == named && !own.contains(h))
                    {
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
            declared_earlier.extend(own);
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
                let preview = match content.char_indices().nth(50) {
                    Some((end, _)) => format!("{}...", &content[..end]),
                    None => content,
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
                // Check for block collection on same line as mapping key
                self.check_collection_on_same_line_as_key(node, violations);
            }
            SyntaxKind::SCALAR => {
                // Check for invalid escape sequences in quoted strings
                self.check_escape_sequences(node, violations);
                // Check for content on same line as block scalar indicator
                self.check_block_scalar_indicator(node, violations);
                self.check_block_scalar_header_indent(node, violations);
                // Check block scalar content indentation consistency
                self.check_block_scalar_content_indent(node, violations);
                // Check for trailing content after quoted strings
                self.check_trailing_content_after_quoted(node, violations);
                // Check for colons in plain scalar values
                self.check_colon_in_plain_scalar(node, violations);
                self.check_plain_scalar_start(node, violations);
                self.check_block_scalar_in_flow(node, violations);
                // Check for directives inside document content (e.g. %YAML after ---)
                self.check_directive_in_content(node, violations);
                // Warn about constructs YAML 1.1 read differently
                self.check_legacy_yaml_1_1(node, violations);
            }
            SyntaxKind::DOC_START | SyntaxKind::DOC_END if self.config.check_document_markers => {
                self.check_document_marker_placement(node, violations);
            }
            SyntaxKind::MAPPING => {
                self.check_flow_collection_commas(node, violations);
                self.check_flow_pair_placement(node, violations);
                self.check_flow_continuation_indent(node, violations);
                self.check_block_mapping_entries_on_same_line(node, violations);
                self.check_block_mapping_after_properties(node, violations);
                self.check_mapping_entry_indentation(node, violations);
                if self.config.check_duplicate_keys {
                    self.check_duplicate_keys(node, violations);
                }
            }
            SyntaxKind::SEQUENCE => {
                self.check_flow_collection_commas(node, violations);
                self.check_flow_continuation_indent(node, violations);
            }
            SyntaxKind::KEY => {
                self.check_anchor_and_alias(node, violations);
                self.check_single_content_node(node, violations);
            }
            SyntaxKind::SEQUENCE_ENTRY => {
                self.check_anchor_and_alias(node, violations);
                self.check_single_content_node(node, violations);
            }
            SyntaxKind::VALUE => {
                self.check_anchor_and_alias(node, violations);
                self.check_single_content_node(node, violations);
            }
            SyntaxKind::TAGGED_NODE => {
                self.check_anchor_and_alias(node, violations);
                self.check_single_content_node(node, violations);
                self.check_tag_on_alias(node, violations);
                self.check_collection_tag_content(node, violations);
                self.check_pairs_items(node, violations);
                self.check_omap_keys(node, violations);
                self.check_set_values(node, violations);
            }
            SyntaxKind::DOCUMENT => {
                self.check_anchor_and_alias(node, violations);
                self.check_single_content_node(node, violations);
                self.check_document_level_anchors(node, violations);
                if self.config.check_anchors {
                    self.check_undefined_aliases(node, violations);
                }
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

    /// Check the directives that introduce `document`, or that end the
    /// stream when it is `None`.
    fn check_directives(
        &self,
        directives: &[SyntaxNode],
        document: Option<&SyntaxNode>,
        violations: &mut Vec<Violation>,
    ) {
        let Some(last) = directives.last() else {
            return;
        };

        // `l-directive-document` is the directives followed by an explicit
        // document, so they need a `---` (B63P's `%YAML 1.2\n...\n` and
        // 9MMA have none). The document itself may be empty.
        if !document.is_some_and(|d| has_child_token(d, |k| k == crate::SyntaxKind::DOC_START)) {
            violations.push(Violation::error_at(
                Rule::Other,
                last.text_range(),
                "Directive without document start marker (---)",
            ));
        }

        let mut seen_yaml = false;
        let mut tag_handles: Vec<String> = Vec::new();
        for directive in directives {
            let text = directive.text().to_string();
            let mut words = text.split_whitespace();
            match words.next() {
                Some("%YAML") => {
                    if seen_yaml {
                        violations.push(Violation::error_at(
                            Rule::Other,
                            directive.text_range(),
                            "Duplicate %YAML directive",
                        ));
                    }
                    seen_yaml = true;
                    // One `major.minor` version, then at most a comment,
                    // which needs whitespace before its `#`: H7TQ's
                    // `%YAML 1.2 foo` and MUS6/00's `%YAML 1.1#...` are errors.
                    let is_version = |v: &str| {
                        v.split_once('.').is_some_and(|(major, minor)| {
                            [major, minor]
                                .iter()
                                .all(|n| !n.is_empty() && n.bytes().all(|b| b.is_ascii_digit()))
                        })
                    };
                    if !words.next().is_some_and(is_version)
                        || words.next().is_some_and(|w| !w.starts_with('#'))
                    {
                        violations.push(Violation::error_at(
                            Rule::Other,
                            directive.text_range(),
                            "Malformed %YAML directive",
                        ));
                    }
                }
                // A handle may be declared once per document.
                Some("%TAG") => {
                    if let Some(handle) = words.next() {
                        if tag_handles.iter().any(|h| h == handle) {
                            violations.push(Violation::error_at(
                                Rule::Other,
                                directive.text_range(),
                                format!("Duplicate %TAG directive for {handle}"),
                            ));
                        }
                        tag_handles.push(handle.to_string());
                    }
                }
                _ => {}
            }
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
                    // The previous document has to end with `...`, which
                    // can be its last child or, as in 6WLZ, follow it at
                    // the stream's level.
                    let mut found_doc_with_end = false;
                    for prev in child.siblings_with_tokens(rowan::Direction::Prev).skip(1) {
                        if prev.kind() == SyntaxKind::DOC_END {
                            found_doc_with_end = true;
                            break;
                        }
                        if let Some(prev) =
                            prev.as_node().filter(|n| n.kind() == SyntaxKind::DOCUMENT)
                        {
                            found_doc_with_end =
                                has_child_token(prev, |k| k == SyntaxKind::DOC_END);
                            break;
                        }
                    }

                    if !found_doc_with_end {
                        violations.push(Violation::error_at(
                            Rule::Other,
                            child.text_range(),
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

        // A block scalar's body is literal text, though the lexer still
        // gives a `%` line there as a DIRECTIVE token: W4TN's
        // `--- |\n%!PS-Adobe-2.0`.
        if is_block_scalar(node) {
            return;
        }

        // A `%` line may continue a plain scalar (XLQ9 reads
        // `---\nscalar\n%YAML 1.2\n` as `scalar %YAML 1.2`), but `%` cannot
        // start one (MUS6/01), nor can the line follow a closed quoted
        // scalar (9HCY).
        let mut prev: Option<crate::nodes::SyntaxToken> = None;
        for token in node.children_with_tokens().filter_map(|c| c.into_token()) {
            if token.kind() == SyntaxKind::DIRECTIVE {
                let continues_plain = prev
                    .as_ref()
                    .is_some_and(|p| p.kind() == SyntaxKind::STRING && !is_quoted_scalar(p));
                if !continues_plain {
                    violations.push(Violation::error_at(Rule::Other, node.text_range(), "Directive in document content (missing document end marker `...` before directive)".to_string()));
                    return;
                }
            }
            if !matches!(
                token.kind(),
                SyntaxKind::NEWLINE | SyntaxKind::WHITESPACE | SyntaxKind::INDENT
            ) {
                prev = Some(token);
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
                    // `\x`, `\u` and `\U` take exactly 2, 4 and 8 hex digits.
                    let digits = match next {
                        'x' => 2,
                        'u' => 4,
                        'U' => 8,
                        _ => continue,
                    };
                    let code: String = chars.by_ref().take(digits).collect();
                    if code.len() != digits || !code.chars().all(|c| c.is_ascii_hexdigit()) {
                        violations.push(Violation::error_at(
                            Rule::InvalidEscape,
                            node.text_range(),
                            format!("Invalid escape sequence: \\{next}{code}"),
                        ));
                        return;
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
        // A plain scalar may hold a `|` or `>` token on a continuation line.
        if !is_block_scalar(node) {
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
                            violations.push(Violation::error_at(
                                Rule::Other,
                                token.text_range(),
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
                            violations.push(Violation::error_at(
                                Rule::Other,
                                token.text_range(),
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

    /// Check that a block scalar header starting a later line than its key
    /// or its node's properties is indented past the parent node.
    ///
    /// Before the header, `s-separate(n+1)` requires at least `n+1` spaces
    /// of indentation on a new line, so the parser's reading of
    /// `a: !!str\n|\n x\n` as a tagged block scalar, or of `a:\n|\n x\n`
    /// as a block scalar, is not valid YAML. The YAML test suite's M5C3 has
    /// the header two columns left of its tag, which is still past the key.
    fn check_block_scalar_header_indent(&self, node: &SyntaxNode, violations: &mut Vec<Violation>) {
        if !is_block_scalar(node) {
            return;
        }
        let Some(header) = node.first_token() else {
            return;
        };
        let Some(min_indent) = continuation_indent(node) else {
            return;
        };
        if token_column(&header) < min_indent {
            violations.push(Violation::error_at(
                Rule::Other,
                header.text_range(),
                "Block scalar header must be indented past its parent node",
            ));
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
        if !is_block_scalar(node) {
            return;
        }
        // An indentation indicator fixes the body's column, so a deeper
        // leading line is content rather than indentation.
        if crate::nodes::scalar_node::block_scalar_indicators(node)
            .1
            .is_some()
        {
            return;
        }

        // Walk tokens after the block indicator's NEWLINE. Track the
        // deepest INDENT seen so far (from blank-only lines); flag any
        // subsequent INDENT + non-blank content whose text is shorter.
        //
        // Indentation is spaces only (s-indent), so a tab in an INDENT
        // token starts the line's content: R4YG's " \t" line is indented
        // one, not two, and its expected events keep the tab.
        fn space_indent(token: &rowan::SyntaxToken<crate::Lang>) -> usize {
            token.text().chars().take_while(|c| *c == ' ').count()
        }

        let mut past_first_newline = false;
        let mut max_blank_indent = 0usize;
        let mut body_indent: Option<usize> = None;
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
                    // how far the blank line was padded. A tab makes the
                    // line non-blank, so it carries no indentation claim.
                    if let Some(ind) = pending_indent.take() {
                        if !ind.text().contains('\t') {
                            max_blank_indent = max_blank_indent.max(space_indent(&ind));
                        }
                    }
                }
                _ => {
                    // Non-blank content. Once a content line has set the
                    // body's indent, a deeper blank line is content rather
                    // than indentation and cannot raise the bar: H2RW's
                    // 4-space blank contributes two spaces to a body
                    // indented 2, as its expected events show. Only a blank
                    // deeper than the body's own indent, seen before any
                    // content, is 5LLU's error.
                    if let Some(ind) = pending_indent.take() {
                        if body_indent.is_none() {
                            let indent = space_indent(&ind);
                            body_indent = Some(indent);
                            if indent < max_blank_indent {
                                violations.push(Violation::error_at(Rule::Other, ind.text_range(), format!( "Block scalar content under-indented ({indent} spaces) relative to preceding blank line ({max_blank_indent} spaces)" )));
                                return;
                            }
                        }
                    }
                }
            }
        }
    }

    /// Warn about plain scalars that YAML 1.1 resolved differently.
    ///
    /// The document is valid either way, so these are warnings: they say the
    /// text means one thing here and another to a 1.1 reader.
    ///
    /// Bare-octal is the case that changes a value silently. `0755` was 493
    /// in YAML 1.1 and is 755 in 1.2, with no syntax error either way, so a
    /// file carried across versions changes meaning without complaint. The
    /// 1.1 booleans (`yes`, `no`, `on`, `off`) are already strings here, as
    /// 1.2 requires, but a 1.1 reader still takes them as booleans. A
    /// leading-zero decimal with an 8 or 9 (`089`) is the reverse: an integer
    /// here, but a string to a 1.1 reader.
    ///
    /// The other numbers whose reading changed are covered too, matching
    /// what [`crate::migrate::to_yaml_1_2`] rewrites: binary, signed hex,
    /// base 60 and underscored numbers were 1.1 numbers and are strings here,
    /// while `0o` octal and exponents without a `.` or sign (`1e3`) were 1.1
    /// strings.
    fn check_legacy_yaml_1_1(&self, node: &SyntaxNode, violations: &mut Vec<Violation>) {
        // Only a plain scalar resolves by its text; a quoted one is a string
        // in every version.
        let Some(token) = node.children_with_tokens().find_map(|c| {
            c.into_token().filter(|t| {
                matches!(
                    t.kind(),
                    crate::SyntaxKind::INT
                        | crate::SyntaxKind::FLOAT
                        | crate::SyntaxKind::STRING
                        | crate::SyntaxKind::BOOL
                )
            })
        }) else {
            return;
        };
        if is_quoted_scalar(&token) {
            return;
        }
        let text = token.text();

        if crate::ScalarValue::is_legacy_octal(text) {
            let sign = if text.starts_with('-') { "-" } else { "" };
            let digits = text.strip_prefix(['+', '-']).unwrap_or(text);
            let as_octal = crate::scalar::digits_to_decimal(digits, 8);
            let as_decimal = crate::scalar::digits_to_decimal(digits, 10);
            // The 1.2 spelling `0o755` is a string to a 1.1 reader; only the
            // decimal reads the same in both.
            violations.push(Violation::warning_at(
                Rule::LegacyYaml11,
                token.text_range(),
                format!(
                    "`{text}` is YAML 1.1 octal, read as {sign}{as_octal}; YAML 1.2 reads it as {sign}{as_decimal}. Write `{sign}{as_octal}` to keep the octal meaning",
                ),
            ));
            return;
        }

        if let Some((after, kind)) = crate::migrate::yaml_1_1_number(text) {
            // The migration's `0o` octal is a string to a 1.1 reader, so the
            // decimal is suggested instead. Unsigned hex reads the same in both.
            let (value, suggestion, kind) = if kind == crate::SyntaxKind::FLOAT {
                (after.clone(), after, "float")
            } else if let Some(hex) = after.strip_prefix("0x") {
                (crate::scalar::digits_to_decimal(hex, 16), after, "integer")
            } else if let Some(octal) = after.strip_prefix("0o") {
                let value = crate::scalar::digits_to_decimal(octal, 8);
                (value.clone(), value, "integer")
            } else {
                (after.clone(), after, "integer")
            };
            violations.push(Violation::warning_at(
                Rule::LegacyYaml11,
                token.text_range(),
                format!(
                    "`{text}` is the {kind} {value} in YAML 1.1; YAML 1.2 reads it as a string. Write `{suggestion}` to keep the {kind} meaning",
                ),
            ));
            return;
        }

        if crate::ScalarValue::is_leading_zero_decimal(text) {
            let minus = if text.starts_with('-') { "-" } else { "" };
            let digits = text.strip_prefix(['+', '-']).unwrap_or(text);
            let as_decimal = crate::scalar::digits_to_decimal(digits, 10);
            violations.push(Violation::warning_at(
                Rule::LegacyYaml11,
                token.text_range(),
                format!(
                    "`{text}` is a string in YAML 1.1; YAML 1.2 reads it as the integer {minus}{as_decimal}. Write `'{text}'` to keep the string meaning",
                ),
            ));
            return;
        }

        if crate::migrate::yaml_1_1_string(text) {
            let reading = match text.strip_prefix("0o") {
                Some(octal) => {
                    format!("the integer {}", crate::scalar::digits_to_decimal(octal, 8))
                }
                None => "a float".to_string(),
            };
            violations.push(Violation::warning_at(
                Rule::LegacyYaml11,
                token.text_range(),
                format!(
                    "`{text}` is a string in YAML 1.1; YAML 1.2 reads it as {reading}. Write `'{text}'` to keep the string meaning",
                ),
            ));
            return;
        }

        if matches!(
            text,
            "yes" | "no" | "on" | "off" | "Yes" | "No" | "On" | "Off" | "YES" | "NO" | "ON" | "OFF"
        ) {
            violations.push(Violation::warning_at(
                Rule::LegacyYaml11,
                token.text_range(),
                format!("`{text}` is the string \"{text}\" in YAML 1.2, but a boolean to a YAML 1.1 reader; quote it or write true/false to be unambiguous"),
            ));
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
                        if !found_quoted && is_quoted_scalar(&token) {
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
                            violations.push(Violation::error_at(
                                Rule::Other,
                                token.text_range(),
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
        let Some(colon) = node
            .children_with_tokens()
            .filter_map(|c| c.into_token())
            .find(|t| t.kind() == crate::SyntaxKind::COLON)
        else {
            return;
        };

        // A quoted or block scalar's colons are content: the test suite's
        // 4WA9 is `- aaa: |2\n    xxx\n  bbb: |\n    xxx\n`.
        if node.first_token().is_some_and(|t| is_quoted_scalar(&t)) || is_block_scalar(node) {
            return;
        }

        // Check if this scalar is inside a VALUE node (not a KEY)
        // Keys can have plain text without issues, but values with colons need special handling
        let parent_is_value = node
            .parent()
            .is_some_and(|p| p.kind() == crate::SyntaxKind::VALUE);

        if parent_is_value {
            violations.push(Violation::error_at(
                Rule::Other,
                colon.text_range(),
                "Plain scalar value cannot contain mapping syntax (colon)",
            ));
        }
    }

    /// Check that a plain scalar does not open with an indicator.
    ///
    /// The parser reads `a: ,b`, `- ]` and `[a, - b]` as plain scalars, but
    /// none can start one: `,`, `]`, `}`, `%` and the reserved `@` and `` ` ``
    /// never can, and a `-` can only
    /// when a safe character follows it, as in `-1` (G5U8 is `[-, -]`).
    /// Outside a flow collection a `- ` usually opens a block sequence
    /// entry, but after a tag or anchor on the same line the parser reads
    /// it as a plain scalar (SY6V is `&anchor - sequence entry`).
    fn check_plain_scalar_start(&self, node: &SyntaxNode, violations: &mut Vec<Violation>) {
        use crate::SyntaxKind;

        let Some(first) = node.first_token() else {
            return;
        };
        let text = first.text();
        let unsafe_after_dash: &[char] = if in_flow_collection(node) {
            &[' ', '\t', '\r', '\n', ',', '[', ']', '{', '}']
        } else {
            &[' ', '\t', '\r', '\n']
        };
        // A `%` line is lexed as a DIRECTIVE, which
        // `check_directive_in_content` reports.
        let invalid = text.starts_with([',', ']', '}'])
            || (first.kind() == SyntaxKind::STRING && text.starts_with(['%', '@', '`']))
            || text.strip_prefix('-').is_some_and(|rest| {
                // `{-: 1}` is fine: the colon is safe, and the lexer
                // splits it off as its own token.
                (rest.is_empty() && first.next_token().map(|t| t.kind()) != Some(SyntaxKind::COLON))
                    || rest.starts_with(unsafe_after_dash)
            });
        if invalid {
            let start = text.chars().next().unwrap_or_default();
            violations.push(Violation::error_at(
                Rule::Other,
                first.text_range(),
                format!("Plain scalar cannot start with {start:?}"),
            ));
        }
    }

    /// Check that a block scalar does not appear inside a flow collection.
    ///
    /// Flow content may only hold flow nodes. The parser reads `[k: |\n  x\n]`
    /// as a plain scalar starting with `|`, while an edit can leave a real
    /// block scalar there, so both shapes are checked.
    fn check_block_scalar_in_flow(&self, node: &SyntaxNode, violations: &mut Vec<Violation>) {
        use crate::SyntaxKind;

        let Some(first) = node.first_token() else {
            return;
        };
        let is_block_header = is_block_scalar(node)
            || (first.kind() == SyntaxKind::STRING && first.text().starts_with(['|', '>']));
        if is_block_header && in_flow_collection(node) {
            violations.push(Violation::error_at(
                Rule::Other,
                first.text_range(),
                "Block scalar cannot appear inside a flow collection",
            ));
        }
    }

    /// Check for a tab used as indentation among `node`'s own tokens.
    fn check_tab_usage(&self, node: &SyntaxNode, violations: &mut Vec<Violation>) {
        use crate::SyntaxKind;
        let tab = match node.kind() {
            SyntaxKind::SCALAR => {
                block_scalar_tab_indent(node).or_else(|| quoted_scalar_tab_indent(node))
            }
            // Only an INDENT that supplies a node's indentation can hold
            // an illegal tab. YAML 1.2 forbids a tab in `s-indent`, but
            // elsewhere -- inside a scalar's continuation line, or as
            // separation at the stream's own level -- it is content or
            // legal whitespace, which is how the test suite reads 4ZYM,
            // HS5T, NB6Z, UV7Q, 6CA3, K54U and T5N4.
            SyntaxKind::VALUE
            | SyntaxKind::MAPPING
            | SyntaxKind::SEQUENCE
            | SyntaxKind::MAPPING_ENTRY
            | SyntaxKind::SEQUENCE_ENTRY => block_tab_indent(node),
            _ => None,
        };
        if let Some(token) = tab {
            violations.push(Violation::error_at(
                Rule::InvalidTabUsage,
                token.text_range(),
                "Tabs are not allowed for indentation in YAML",
            ));
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
        if !starts_a_flow_collection(node) {
            return;
        }
        let entry_kind = match node.kind() {
            crate::SyntaxKind::MAPPING => crate::SyntaxKind::MAPPING_ENTRY,
            crate::SyntaxKind::SEQUENCE => crate::SyntaxKind::SEQUENCE_ENTRY,
            _ => return,
        };

        // A flow separator is stored inside the entry it follows (see the
        // flow-separator invariant in nodes/mod.rs), and an omitted entry
        // shows up as one whose value is the zero-width implicit null. So
        // `[a, , b]` and `[a, b, ,]` are entries with an empty value rather
        // than adjacent COMMA tokens.
        let entries: Vec<_> = node.children().filter(|n| n.kind() == entry_kind).collect();

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

        // Every entry but the last needs a separator; counting them instead
        // let a trailing comma stand in for a missing one.
        for pair in entries.windows(2) {
            if !crate::nodes::has_child_token(&pair[0], |k| k == crate::SyntaxKind::COMMA) {
                violations.push(Violation::error_at(
                    Rule::MissingSyntax,
                    pair[1].text_range(),
                    "Missing comma between flow collection entries",
                ));
            }
        }
    }

    /// Check that a mapping without braces inside a flow collection is a
    /// flow sequence entry.
    ///
    /// `[k: v]` holds a single-pair mapping, but a flow node cannot be one,
    /// so the parser's reading of `{k: ? a}` is invalid.
    fn check_flow_pair_placement(&self, node: &SyntaxNode, violations: &mut Vec<Violation>) {
        if starts_a_flow_collection(node) || !in_flow_collection(node) {
            return;
        }
        let is_sequence_entry = node
            .parent()
            .filter(|p| p.kind() == crate::SyntaxKind::SEQUENCE_ENTRY)
            .and_then(|p| p.parent())
            .is_some_and(|s| starts_a_flow_collection(&s));
        if !is_sequence_entry {
            violations.push(Violation::error_at(
                Rule::Other,
                node.text_range(),
                "Mapping without braces can only be a flow sequence entry",
            ));
        }
    }

    /// Check that a node slot (a document, key, value, sequence entry or
    /// tagged node) holds at most one node.
    ///
    /// In `? a\n 5b: 1\n` the more indented line would continue the plain
    /// scalar `a`, which then could not be an implicit key spanning lines;
    /// the parser keeps the mapping as a second node of the key instead.
    /// Likewise the `x` of `[q] x` is kept next to the flow sequence.
    fn check_single_content_node(&self, node: &SyntaxNode, violations: &mut Vec<Violation>) {
        use crate::SyntaxKind;

        let Some(extra) = node
            .children()
            .filter(|c| {
                matches!(
                    c.kind(),
                    SyntaxKind::SCALAR
                        | SyntaxKind::MAPPING
                        | SyntaxKind::SEQUENCE
                        | SyntaxKind::TAGGED_NODE
                        | SyntaxKind::ALIAS
                )
            })
            .nth(1)
        else {
            return;
        };
        let what = match node.kind() {
            SyntaxKind::DOCUMENT => "Document",
            SyntaxKind::KEY => "Key",
            SyntaxKind::SEQUENCE_ENTRY => "Sequence entry",
            SyntaxKind::TAGGED_NODE => "Tagged node",
            _ => "Value",
        };
        violations.push(Violation::error_at(
            Rule::Other,
            extra.text_range(),
            format!("{what} holds more than one node"),
        ));
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
                        // In a nested mapping the next line's indentation
                        // follows that newline (`k:\n  ? a\n  ? b\n`).
                        let prev_ends_with_newline = prev
                            .descendants_with_tokens()
                            .filter_map(|c| c.into_token())
                            .filter(|t| {
                                !t.text().is_empty()
                                    && !matches!(
                                        t.kind(),
                                        crate::SyntaxKind::INDENT | crate::SyntaxKind::WHITESPACE
                                    )
                            })
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
                        violations.push(Violation::error_at(
                            Rule::Other,
                            child.text_range(),
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
        if starts_a_flow_collection(node) {
            return;
        }

        // Determine the expected indent for entries in this mapping.
        //   - Nested mapping (parent VALUE has NEWLINE + INDENT before
        //     us): use that INDENT's text.
        //   - Otherwise (root, compact or explicit-entry mapping): the
        //     column of its first entry.
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
        // wrongly indented. A tagged mapping (`k: !t\n  a: 1\n`, C4HZ) has
        // it in the TAGGED_NODE.
        let expected_indent: String = node
            .parent()
            .filter(|p| {
                matches!(
                    p.kind(),
                    crate::SyntaxKind::VALUE
                        | crate::SyntaxKind::SEQUENCE_ENTRY
                        | crate::SyntaxKind::TAGGED_NODE
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
                // entries line up under the dash's gap. The same goes for one
                // after an explicit `?` or `:`. A root mapping may be
                // indented too (`  a: 1\n  b: 2\n`), and after a leading
                // comment its INDENT is not even in the DOCUMENT. Take the
                // column from the node's offset within its line.
                let parent = node.parent()?;
                if !matches!(
                    parent.kind(),
                    crate::SyntaxKind::SEQUENCE_ENTRY
                        | crate::SyntaxKind::KEY
                        | crate::SyntaxKind::VALUE
                        | crate::SyntaxKind::DOCUMENT
                ) {
                    return None;
                }
                Some(" ".repeat(token_column(&node.first_token()?)))
            })
            .unwrap_or_default();

        // Only an INDENT that introduces a sibling entry states that
        // entry's indentation. One followed by a NEWLINE pads a blank
        // line (H2RW), and a trailing one belongs to the enclosing
        // construct rather than to this mapping (V9D5's explicit key).
        let mut seen_entry = false;
        let mut pending: Option<rowan::SyntaxToken<crate::Lang>> = None;
        for child in node.children_with_tokens() {
            match child {
                rowan::NodeOrToken::Token(t) if t.kind() == crate::SyntaxKind::INDENT => {
                    pending = Some(t);
                }
                rowan::NodeOrToken::Node(n) if n.kind() == crate::SyntaxKind::MAPPING_ENTRY => {
                    if let Some(t) = pending.take() {
                        if seen_entry && t.text() != expected_indent {
                            violations.push(Violation::error_at(Rule::Other, t.text_range(), format!( "Sibling block mapping entries have inconsistent indentation (expected {:?}, found {:?})", expected_indent, t.text() )));
                        }
                    }
                    seen_entry = true;
                }
                _ => {
                    pending = None;
                }
            }
        }
    }

    /// Check that a multi-line flow collection indents its continuation lines.
    ///
    /// A flow collection may span lines, but each continuation has to be
    /// indented more than the block context it sits in; at column zero the
    /// content would be read as a new block node instead (yaml-test-suite
    /// 9C9N).
    fn check_flow_continuation_indent(&self, node: &SyntaxNode, violations: &mut Vec<Violation>) {
        if !starts_a_flow_collection(node) {
            return;
        }
        // Only the outermost flow collection needs checking; a nested one
        // shares its lines.
        if in_flow_collection(node) {
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

        // Only a flow collection sitting inside a block collection has to
        // clear that block's indentation, or its continuation would read as
        // a new entry of that block (9C9N's `flow: [a,\nb,\nc]`). A flow
        // collection that is the document's own node has no such neighbour,
        // so its entries may start at column zero (4ABK).
        let Some(required) = continuation_indent(node) else {
            return;
        };

        // The indentation of the current line, while still at its start.
        // Tabs are not indentation, so only leading spaces count.
        let mut indent: Option<usize> = None;
        for el in node.descendants_with_tokens() {
            let rowan::NodeOrToken::Token(t) = el else {
                continue;
            };
            match t.kind() {
                crate::SyntaxKind::NEWLINE => indent = Some(0),
                crate::SyntaxKind::INDENT | crate::SyntaxKind::WHITESPACE if indent.is_some() => {
                    indent = Some(t.text().chars().take_while(|c| *c == ' ').count());
                }
                crate::SyntaxKind::COMMENT => indent = None,
                _ if indent.take().is_some_and(|i| i < required) => {
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

    /// Check for anchors at document level without proper node attachment
    ///
    /// Anchors should be attached to nodes (values), not floating at document level.
    fn check_document_level_anchors(&self, node: &SyntaxNode, violations: &mut Vec<Violation>) {
        // Check for ANCHOR tokens that are direct children of DOCUMENT
        for child in node.children_with_tokens() {
            if let rowan::NodeOrToken::Token(token) = child {
                if token.kind() == crate::SyntaxKind::ANCHOR {
                    // An anchor at document level annotates the document's
                    // own node, which is the ordinary spelling of
                    // `&sequence\n- a\n` and `&flowseq [ ... ]`. It is
                    // stranded only when nothing follows it.
                    let annotates_a_node = token
                        .siblings_with_tokens(rowan::Direction::Next)
                        .filter_map(|sibling| sibling.into_node())
                        .any(|n| {
                            matches!(
                                n.kind(),
                                crate::SyntaxKind::MAPPING
                                    | crate::SyntaxKind::SEQUENCE
                                    | crate::SyntaxKind::SCALAR
                                    | crate::SyntaxKind::TAGGED_NODE
                                    | crate::SyntaxKind::ALIAS
                            )
                        });
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

    /// Check that every alias refers to an anchor earlier in the same document
    ///
    /// Anchors are scoped to a document and an alias can only refer to an
    /// anchor that precedes it, so `a: *x` and `a: *x\nb: &x 1` are errors.
    fn check_undefined_aliases(&self, document: &SyntaxNode, violations: &mut Vec<Violation>) {
        let mut anchors = std::collections::HashSet::new();
        // The lexer gives `*x` and `&x` opening a line of a block scalar body
        // or a plain scalar continuation as REFERENCE and ANCHOR too. The
        // parser keeps those inside the SCALAR as text, and wraps real
        // aliases in an ALIAS node.
        for token in document
            .descendants_with_tokens()
            .filter_map(|e| e.into_token())
            .filter(|t| t.parent().map(|p| p.kind()) != Some(crate::SyntaxKind::SCALAR))
        {
            match token.kind() {
                crate::SyntaxKind::ANCHOR => {
                    anchors.insert(token.text()[1..].to_string());
                }
                crate::SyntaxKind::REFERENCE if !anchors.contains(&token.text()[1..]) => {
                    violations.push(Violation::error_at(
                        Rule::InvalidAnchor,
                        token.text_range(),
                        format!("Undefined alias {}", token.text()),
                    ));
                }
                _ => {}
            }
        }
    }

    /// Check for both anchor and alias on the same node
    ///
    /// An alias node cannot have properties (spec production c-ns-alias-node),
    /// so `&b *a` is invalid. The parser leaves the anchor as a sibling of the
    /// ALIAS node, possibly with a tag around the alias (`&b !t *a`). An alias
    /// further down, such as in `&b !t [*a]` or `&n\n  *a : v` (test suite
    /// 26DV), belongs to a different node.
    fn check_anchor_and_alias(&self, node: &SyntaxNode, violations: &mut Vec<Violation>) {
        use crate::SyntaxKind;

        let Some(anchor) = node
            .children_with_tokens()
            .filter_map(|c| c.into_token())
            .find(|t| t.kind() == SyntaxKind::ANCHOR)
        else {
            return;
        };
        let is_alias = |n: &SyntaxNode| n.kind() == SyntaxKind::ALIAS;
        let has_alias = node.children().any(|child| {
            is_alias(&child)
                || (child.kind() == SyntaxKind::TAGGED_NODE
                    && child.children().any(|c| is_alias(&c)))
        });
        if has_alias {
            violations.push(Violation::error_at(
                Rule::Other,
                anchor.text_range(),
                "Node cannot have both an anchor and be an alias",
            ));
        }
    }

    /// Check that an alias has no tag
    ///
    /// An alias node cannot have properties (spec production c-ns-alias-node),
    /// so `!t *a` is invalid.
    fn check_tag_on_alias(&self, node: &SyntaxNode, violations: &mut Vec<Violation>) {
        if !node
            .children()
            .any(|child| child.kind() == crate::SyntaxKind::ALIAS)
        {
            return;
        }
        let Some(tag) = node
            .children_with_tokens()
            .filter_map(|c| c.into_token())
            .find(|t| t.kind() == crate::SyntaxKind::TAG)
        else {
            return;
        };
        violations.push(Violation::error_at(
            Rule::InvalidTag,
            tag.text_range(),
            "An alias cannot have a tag",
        ));
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
        // A BOM leaves the comment at the start of its line, which is
        // separation enough (l-document-prefix).
        if !matches!(
            prev.kind(),
            crate::SyntaxKind::WHITESPACE
                | crate::SyntaxKind::NEWLINE
                | crate::SyntaxKind::INDENT
                | crate::SyntaxKind::BOM
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
                    let is_flow = starts_a_flow_collection(n);
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
            violations.push(Violation::error_at(
                Rule::InvalidDocumentMarker,
                token.text_range(),
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
                // A scalar starting with a comma (`!!str, xxx` in block
                // context) is reported by `check_plain_scalar_start`.
                rowan::NodeOrToken::Node(_) => return,
            }
        }
    }

    /// Check that a block mapping does not start on the line of its tag or
    /// anchor, as in `!x ? a`.
    ///
    /// Node properties of a block collection have to be followed by a line
    /// break (`s-l+block-collection`). An implicit key on that line takes the
    /// properties itself, so only an explicit key gets here.
    fn check_block_mapping_after_properties(
        &self,
        node: &SyntaxNode,
        violations: &mut Vec<Violation>,
    ) {
        if starts_a_flow_collection(node) {
            return;
        }
        let mut prev = node.prev_sibling_or_token();
        while let Some(rowan::NodeOrToken::Token(token)) = &prev {
            if token.kind() != crate::SyntaxKind::WHITESPACE {
                break;
            }
            prev = token.prev_sibling_or_token();
        }
        let Some(rowan::NodeOrToken::Token(property)) = prev else {
            return;
        };
        if matches!(
            property.kind(),
            crate::SyntaxKind::TAG | crate::SyntaxKind::ANCHOR
        ) {
            violations.push(Violation::error_at(
                Rule::Other,
                node.text_range(),
                "A block mapping cannot start on the line of its tag or anchor",
            ));
        }
    }

    /// Check that a `!!set` tags a mapping, whose keys are its members, and
    /// a `!!omap` or `!!pairs` a sequence.
    ///
    /// These are YAML 1.1 types rather than YAML 1.2 ones, so the wrong
    /// content is only a warning. A tagged alias is left to
    /// [`Self::check_tag_on_alias`].
    fn check_collection_tag_content(&self, node: &SyntaxNode, violations: &mut Vec<Violation>) {
        let Some(tagged) = crate::yaml::TaggedNode::cast(node.clone()) else {
            return;
        };
        if node
            .children()
            .any(|child| child.kind() == crate::SyntaxKind::ALIAS)
        {
            return;
        }
        let message = match tagged.tag().as_deref() {
            Some("!!set") if tagged.as_set().is_none() => "!!set content is not a mapping",
            Some("!!omap") if tagged.as_sequence().is_none() => "!!omap content is not a sequence",
            Some("!!pairs") if tagged.as_sequence().is_none() => {
                "!!pairs content is not a sequence"
            }
            _ => return,
        };
        let Some(tag) = node
            .children_with_tokens()
            .filter_map(|c| c.into_token())
            .find(|t| t.kind() == crate::SyntaxKind::TAG)
        else {
            return;
        };
        violations.push(Violation::warning_at(
            Rule::InvalidTag,
            tag.text_range(),
            message,
        ));
    }

    /// Check that the members of a `!!set` have null values.
    ///
    /// Like other content that does not suit a YAML 1.1 type, a value is
    /// only a warning. An alias is not resolved here; the conversion to
    /// `YamlValue` checks the node it refers to.
    fn check_set_values(&self, node: &SyntaxNode, violations: &mut Vec<Violation>) {
        use crate::as_yaml::YamlNode;

        let Some(mapping) = crate::yaml::TaggedNode::cast(node.clone())
            .filter(|tagged| tagged.as_set().is_some())
            .and_then(|tagged| tagged.as_mapping())
        else {
            return;
        };
        for entry in mapping.entries() {
            let Some(value) = entry.value_node() else {
                continue;
            };
            if matches!(value, YamlNode::Alias(_)) {
                continue;
            }
            let is_null = crate::as_yaml::scalar_canonical(value.syntax())
                .is_some_and(|(tag, _)| tag == "!!null");
            if is_null {
                continue;
            }
            let member = entry
                .key_node()
                .map_or_else(String::new, |key| key.to_string());
            violations.push(Violation::warning_at(
                Rule::InvalidTag,
                value.syntax().text_range(),
                format!(
                    "!!set member {member:?} has non-null value {:?}",
                    value.to_string()
                ),
            ));
        }
    }

    /// Check that each item of a `!!omap` or `!!pairs` is a mapping,
    /// untagged or tagged `!!map`, with a single entry.
    ///
    /// Like other content that does not suit a YAML 1.1 type, a bad item is
    /// only a warning. An alias is not resolved here; the conversion to
    /// `YamlValue` checks the node it refers to.
    // TODO: resolve merge keys, which can change the number of entries.
    fn check_pairs_items(&self, node: &SyntaxNode, violations: &mut Vec<Violation>) {
        use crate::as_yaml::YamlNode;

        let Some(tagged) = crate::yaml::TaggedNode::cast(node.clone()) else {
            return;
        };
        let Some(tag) = tagged
            .tag()
            .filter(|tag| matches!(tag.as_str(), "!!omap" | "!!pairs"))
        else {
            return;
        };
        let Some(sequence) = tagged.as_sequence() else {
            return;
        };
        for item in sequence.values() {
            let mapping = match &item {
                YamlNode::Alias(_) => continue,
                YamlNode::Mapping(mapping) => Some(mapping.clone()),
                YamlNode::TaggedNode(tagged) if tagged.tag().as_deref() == Some("!!map") => {
                    tagged.as_mapping()
                }
                _ => None,
            };
            if mapping.is_some_and(|mapping| mapping.entries().count() == 1) {
                continue;
            }
            violations.push(Violation::warning_at(
                Rule::InvalidTag,
                item.syntax().text_range(),
                format!(
                    "{tag} item {:?} is not a mapping with a single entry",
                    item.to_string()
                ),
            ));
        }
    }

    /// Check that the items of a `!!omap` have distinct keys; a `!!pairs`
    /// may repeat a key.
    ///
    /// Like other content that does not suit a YAML 1.1 type, a repeated
    /// key is only a warning. Keys compare as in
    /// [`Self::check_duplicate_keys`].
    fn check_omap_keys(&self, node: &SyntaxNode, violations: &mut Vec<Violation>) {
        use crate::SyntaxKind;

        let Some(tagged) = crate::yaml::TaggedNode::cast(node.clone()) else {
            return;
        };
        if tagged.tag().as_deref() != Some("!!omap") {
            return;
        }
        let Some(sequence) = tagged.as_sequence() else {
            return;
        };
        // The key of each item that is a mapping, `!!map` or untagged, and
        // the range of its entry.
        let keys = sequence.items().filter_map(|item| {
            let mapping = match crate::yaml::TaggedNode::cast(item.clone()) {
                Some(tagged) if tagged.tag().as_deref() == Some("!!map") => tagged.as_mapping()?,
                Some(_) => return None,
                None => crate::yaml::Mapping::cast(item)?,
            };
            let entry = mapping
                .syntax()
                .children()
                .find(|n| n.kind() == SyntaxKind::MAPPING_ENTRY)?;
            let key = entry.children().find(|n| n.kind() == SyntaxKind::KEY)?;
            Some((key, entry.text_range()))
        });
        let mut seen: Vec<(crate::as_yaml::NodeIdentity, SyntaxNode)> = Vec::new();
        for (key, range) in keys {
            let Some(identity) = key_identity(&key) else {
                continue;
            };
            if let Some((_, first)) = seen.iter().find(|(k, _)| *k == identity) {
                violations.push(Violation::warning_at(
                    Rule::DuplicateKeys,
                    range,
                    format!(
                        "Duplicate !!omap key: {:?} (semantically equal to {:?})",
                        key.text().to_string().trim(),
                        first.text().to_string().trim()
                    ),
                ));
            }
            seen.push((identity, key));
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

    /// Check for block sequence or mapping starting on same line as mapping key
    ///
    /// YAML 1.2 spec section 6.3.1 requires block collections to start on a new
    /// line after the mapping key and colon. Example of invalid YAML:
    /// ```yaml
    /// key: - a
    ///      - b
    /// ```
    fn check_collection_on_same_line_as_key(
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

        // An explicit key's value may be a compact collection on the `:` line
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

        // Only a block collection has to start on a new line, and only in
        // block context: `[k: ? a]` holds a mapping without braces.
        let Some(collection) = value
            .children()
            .find(|c| matches!(c.kind(), SyntaxKind::SEQUENCE | SyntaxKind::MAPPING))
        else {
            return;
        };
        if starts_a_flow_collection(&collection) || in_flow_collection(entry_node) {
            return;
        }

        // Check for a NEWLINE between key (or COLON) and the collection

        // Find the COLON token that separates key and value
        let mut found_colon = false;
        let mut has_newline = false;

        // The parser puts the line break inside the VALUE when the collection
        // is indentless (`k:\n- a\n`), where the entries sit at the key's
        // own column. That is valid YAML, so look there first; the scan
        // below only sees tokens at the MAPPING_ENTRY level.
        for child in value.children_with_tokens() {
            match child {
                rowan::NodeOrToken::Token(t) if t.kind() == SyntaxKind::NEWLINE => {
                    has_newline = true;
                    break;
                }
                rowan::NodeOrToken::Node(ref n) if n == &collection => break,
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

                // Stop if we reach the collection node
                if let rowan::NodeOrToken::Node(n) = &element {
                    if n == &collection {
                        break;
                    }
                }

                current = element.next_sibling_or_token();
            }
        }

        // If there's no newline between the colon and the collection, it's invalid
        if !has_newline {
            let what = if collection.kind() == SyntaxKind::MAPPING {
                "mapping"
            } else {
                "sequence"
            };
            violations.push(Violation::error_at(
                Rule::Other,
                collection.text_range(),
                format!("Block {what} cannot start on same line as mapping key"),
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

        // Check if all dashes are at the same column (consistent indentation)
        let Some((first, rest)) = dashes.split_first() else {
            return;
        };
        let first_col = token_column(first);
        if let Some(misplaced) = rest.iter().find(|t| token_column(t) != first_col) {
            violations.push(Violation::error_at(
                Rule::InvalidIndentation,
                misplaced.text_range(),
                "Inconsistent sequence item indentation",
            ));
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
        // The marker has to be followed by whitespace or the line's end:
        // `---x` and `---"` are content (9MQT/00).
        if text.split('\n').skip(1).any(|line| {
            (line.starts_with("---") || line.starts_with("..."))
                && line[3..]
                    .chars()
                    .next()
                    .map_or(true, |c| matches!(c, ' ' | '\t' | '\r'))
        }) {
            violations.push(Violation::error_at(
                Rule::InvalidDocumentMarker,
                node.text_range(),
                "Document marker inside a quoted scalar",
            ));
            return;
        }

        // Continuation lines have to clear the enclosing block entry.
        let Some(required) = continuation_indent(node) else {
            return;
        };

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

                // A line opening with a tab is left to check_tab_usage.
                let leading_spaces = line.chars().take_while(|c| *c == ' ').count();
                if leading_spaces < required && !line.trim().is_empty() && !line.starts_with('\t') {
                    violations.push(Violation::error_at(
                        Rule::InvalidIndentation,
                        node.text_range(),
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
    /// - A key that is or holds an alias equals nothing
    fn check_duplicate_keys(&self, node: &SyntaxNode, violations: &mut Vec<Violation>) {
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

        // Key identity -> index of the first key with it
        let mut seen: HashMap<crate::as_yaml::NodeIdentity, usize> = HashMap::new();
        for (i, (key, text, range)) in keys.iter().enumerate() {
            let Some(identity) = key_identity(key) else {
                continue;
            };
            match seen.entry(identity) {
                std::collections::hash_map::Entry::Occupied(first) => {
                    report(&keys[*first.get()].1, text, *range);
                }
                std::collections::hash_map::Entry::Vacant(slot) => {
                    slot.insert(i);
                }
            }
        }
    }
}

/// The identity of mapping key `key`, or `None` if it is or holds an alias.
///
/// Such a key is compared with nothing: the YAML test suite has
/// `{&a [a]: 1, *a : 2}` as valid (X38W).
fn key_identity(key: &SyntaxNode) -> Option<crate::as_yaml::NodeIdentity> {
    crate::as_yaml::node_identity(key, None).ok().flatten()
}

impl Default for Validator {
    fn default() -> Self {
        Self::new()
    }
}

/// The tab indenting the body of `node`, a block scalar, if its body holds
/// no content to continue.
///
/// Then a tab line is the body's indentation, which may not be one: the
/// test suite's Y79Y/000 is the error `foo: |\n\t\nbar: 1`.
fn block_scalar_tab_indent(node: &SyntaxNode) -> Option<crate::nodes::SyntaxToken> {
    if !is_block_scalar(node) {
        return None;
    }
    // The body's first indentation, right after the header's line break.
    // The tab has to *be* the indentation. After a space it is content,
    // which is how the suite reads R4YG's `- >\n \t\n detected`.
    node.children_with_tokens()
        .filter_map(|c| c.into_token())
        .find(|t| t.kind() == crate::SyntaxKind::INDENT)
        .filter(|t| t.text().starts_with('\t'))
}

/// The token of `node`, a quoted scalar, whose continuation line opens with
/// a tab where indentation is required.
///
/// A quoted scalar keeps its continuation lines inside one token, so its
/// text is checked directly: a continuation opening with a tab supplies no
/// indentation at all, which DK95/01's `foo: "bar\n\tbaz"` needs. A blank
/// tab line (DK95/04) indents nothing and is fine. A quoted scalar that is
/// the document's own node needs no indentation, so a tab there is content
/// (7A4E, PRH3), while one inside a mapping value must clear the key's
/// column (DK95/01).
fn quoted_scalar_tab_indent(node: &SyntaxNode) -> Option<crate::nodes::SyntaxToken> {
    if !node.parent().is_some_and(|p| {
        matches!(
            p.kind(),
            crate::SyntaxKind::VALUE | crate::SyntaxKind::SEQUENCE_ENTRY
        )
    }) {
        return None;
    }
    node.children_with_tokens()
        .filter_map(|c| c.into_token())
        .find(|t| {
            is_quoted_scalar(t)
                && t.text()
                    .split('\n')
                    .skip(1)
                    .any(|line| line.starts_with('\t') && !line.trim().is_empty())
        })
}

/// The first token of `node`, a block collection, its entry or a value,
/// that uses a tab as indentation.
fn block_tab_indent(node: &SyntaxNode) -> Option<crate::nodes::SyntaxToken> {
    // A flow collection at the stream's own level is not
    // indentation-sensitive, so a tab inside one is separation
    // whitespace (6CA3's `\t[\n\t]`). Nested in a block collection it
    // still has to clear that block's column, as Y79Y/003's `- [\n\tfoo,`
    // does not.
    let is_flow = starts_a_flow_collection(node);
    let inside_a_block = node.ancestors().skip(1).any(|a| {
        matches!(
            a.kind(),
            crate::SyntaxKind::MAPPING | crate::SyntaxKind::SEQUENCE
        )
    });
    if is_flow && !inside_a_block {
        return None;
    }
    node.children_with_tokens()
        .filter_map(|c| c.into_token())
        .find(|t| tab_before_block_collection(t) || tab_indent(t, is_flow))
}

/// Whether `token` is white space holding a tab that separates an indicator
/// from a block collection.
///
/// A tab may separate an indicator from a scalar (`- \tx`) or a flow
/// collection (`seq:\t[]`), but a block collection after it needs
/// `s-indent`, which forbids tabs: `-\t-` and `?\t-` are errors the test
/// suite expects (Y79Y), while `-\t-1` is the scalar `-1`. The tab has to
/// separate the indicator from the node on the same line; trailing
/// whitespace before a line break (DC7X's `seq:\t`) indents nothing.
fn tab_before_block_collection(token: &crate::nodes::SyntaxToken) -> bool {
    if token.kind() != crate::SyntaxKind::WHITESPACE || !token.text().contains('\t') {
        return false;
    }
    let Some(next) = token.next_sibling_or_token().and_then(|n| n.into_node()) else {
        return false;
    };
    let opens_a_block = |n: &SyntaxNode| {
        matches!(
            n.kind(),
            crate::SyntaxKind::MAPPING | crate::SyntaxKind::SEQUENCE
        ) && !starts_a_flow_collection(n)
    };
    !next
        .first_token()
        .is_some_and(|t| t.kind() == crate::SyntaxKind::NEWLINE)
        && (opens_a_block(&next)
            || (matches!(
                next.kind(),
                crate::SyntaxKind::VALUE | crate::SyntaxKind::KEY
            ) && next.children().any(|c| opens_a_block(&c))))
}

/// Whether `token` is an INDENT made of or starting with a tab, in a
/// collection that is flow if `in_flow`.
fn tab_indent(token: &crate::nodes::SyntaxToken, in_flow: bool) -> bool {
    if token.kind() != crate::SyntaxKind::INDENT {
        return false;
    }
    // A blank or comment-only line indents nothing, so tabs there are fine
    // (DK95/04, Y79Y/002).
    if token.next_token().map_or(true, |t| {
        matches!(
            t.kind(),
            crate::SyntaxKind::NEWLINE | crate::SyntaxKind::COMMENT
        )
    }) {
        return false;
    }
    // After leading spaces a tab is separation rather than indentation
    // (DK95/00's `foo:\n \tbar`, 6HB6), unless a block collection entry
    // follows, whose column it would move (DK95/06). In a VALUE the next
    // sibling is the whole nested collection rather than its first entry
    // (`a:\n \t- x`).
    let starts_block_entry = !in_flow
        && token
            .next_sibling_or_token()
            .is_some_and(|n| match n.kind() {
                crate::SyntaxKind::MAPPING_ENTRY | crate::SyntaxKind::SEQUENCE_ENTRY => true,
                crate::SyntaxKind::MAPPING | crate::SyntaxKind::SEQUENCE => {
                    !n.as_node().is_some_and(starts_a_flow_collection)
                }
                _ => false,
            });
    if token.text().starts_with(' ') && !starts_block_entry {
        return false;
    }
    token.text().contains('\t')
}

#[cfg(test)]
mod tests {
    use super::*;
    use rowan::ast::AstNode;
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
            ("a: 1\n!!str a: 2\n", 1),
            ("1: a\n!!int 0x1: b\n", 1),
            ("1: a\n'1': b\n", 0),
            ("1: a\n!!str 1: b\n", 0),
            ("? [1]\n: a\n? !!seq [1]\n: b\n", 1),
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

        // Only the second entry really carries two anchors on one node: in
        // the first, `&node1` anchors the nested mapping and `&k1` its key,
        // which is why the test suite calls the same shape valid in 7BMT.
        assert_eq!(violations2.len(), 1, "{violations2:?}");
    }

    #[test]
    fn test_validator_undefined_alias() {
        let undefined = |src: &str| -> Vec<(String, crate::TextPosition)> {
            let yaml = crate::YamlFile::from_str(src).unwrap();
            Validator::new()
                .validate_syntax(yaml.syntax())
                .into_iter()
                .filter(|v| v.rule == Rule::InvalidAnchor)
                .map(|v| (v.message, v.text_range.unwrap()))
                .collect()
        };
        let at = |start: u32, end: u32| crate::TextPosition::new(start, end);

        assert_eq!(
            undefined("a: *x\n"),
            vec![("Undefined alias *x".to_string(), at(3, 5))]
        );
        // The anchor must precede the alias.
        assert_eq!(
            undefined("a: *x\nb: &x 1\n"),
            vec![("Undefined alias *x".to_string(), at(3, 5))]
        );
        // Anchors do not carry over into the next document.
        assert_eq!(
            undefined("a: &x 1\n---\nb: *x\n"),
            vec![("Undefined alias *x".to_string(), at(15, 17))]
        );
        assert_eq!(undefined("a: &x 1\nb: *x\n"), vec![]);
        assert_eq!(
            undefined("--- &x 1\n--- &x 2\n--- [*x]\n"),
            vec![("Undefined alias *x".to_string(), at(23, 25))]
        );
        assert_eq!(undefined("[&x a, *x, {*x : b}]\n"), vec![]);
        // `*x` and `&x` in scalar text are content, not node properties.
        assert_eq!(undefined("a: |\n  *x\n"), vec![]);
        assert_eq!(undefined("a: >\n  text\n  *x\n"), vec![]);
        assert_eq!(undefined("a: b\n  *x\n"), vec![]);
        assert_eq!(
            undefined("a: |\n  &x\nb: *x\n"),
            vec![("Undefined alias *x".to_string(), at(13, 15))]
        );
        assert_eq!(
            undefined("a: b\n  &x\nc: *x\n"),
            vec![("Undefined alias *x".to_string(), at(13, 15))]
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
    fn test_validator_duplicate_keys_outside_i64() {
        let doc = Document::from_str(
            "18446744073709551616: a\n0x10000000000000000: b\n9223372036854775808: c\n+9223372036854775808: d\n",
        )
        .unwrap();
        let messages: Vec<_> = Validator::new()
            .validate(&doc)
            .into_iter()
            .filter(|v| v.rule == Rule::DuplicateKeys)
            .map(|v| v.message)
            .collect();
        assert_eq!(
            messages,
            vec![
                "Duplicate key: \"0x10000000000000000\" (semantically equal to \"18446744073709551616\")",
                "Duplicate key: \"+9223372036854775808\" (semantically equal to \"9223372036854775808\")",
            ]
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
        let file = crate::YamlFile::from_str("- !!str, xxx\n").unwrap();
        let violations = Validator::new().validate_syntax(file.syntax());
        assert_eq!(
            violations
                .iter()
                .map(|v| (v.message.as_str(), v.rule, v.text_range.map(|r| r.start)))
                .collect::<Vec<_>>(),
            vec![("Plain scalar cannot start with ','", Rule::Other, Some(7))]
        );
    }

    #[test]
    fn test_plain_scalar_cannot_start_with_an_indicator() {
        let messages = |yaml: &str| -> Vec<(String, Option<u32>)> {
            let file = crate::YamlFile::from_str(yaml).unwrap();
            Validator::new()
                .validate_syntax(file.syntax())
                .into_iter()
                .map(|v| (v.message, v.text_range.map(|r| r.start)))
                .collect()
        };
        for (yaml, error, start) in [
            ("]}]}", "Plain scalar cannot start with ']'", 0),
            ("a: ,b\n", "Plain scalar cannot start with ','", 3),
            ("- a\n- }\n", "Plain scalar cannot start with '}'", 6),
            ("[-1, - 2]", "Plain scalar cannot start with '-'", 5),
            ("[a, -]", "Plain scalar cannot start with '-'", 4),
            ("{- : 1}", "Plain scalar cannot start with '-'", 1),
            ("a: @x\n", "Plain scalar cannot start with '@'", 3),
            ("- `x\n", "Plain scalar cannot start with '`'", 2),
            ("[%x]", "Plain scalar cannot start with '%'", 1),
            ("!x - a\n", "Plain scalar cannot start with '-'", 3),
            ("k: !x - a\n", "Plain scalar cannot start with '-'", 6),
            ("- &a - a\n", "Plain scalar cannot start with '-'", 5),
            ("- !x -\n", "Plain scalar cannot start with '-'", 5),
            (
                "&anchor - sequence entry\n",
                "Plain scalar cannot start with '-'",
                8,
            ),
        ] {
            assert_eq!(
                messages(yaml),
                vec![(error.to_string(), Some(start))],
                "{yaml:?}"
            );
        }
        for yaml in [
            "[-1, -a]",
            "{-: 1}",
            "a: x]\n",
            "a: x,y\n",
            "[a\n -]",
            "a: x%y@z`\n",
            "a: '@x'\n",
            "a: b\n  - c\n",
            "k: !x -1\n",
            "- &a -a\n",
            "!x -[a]\n",
        ] {
            assert_eq!(messages(yaml), vec![], "{yaml:?}");
        }
    }

    #[test]
    fn test_document_marker_in_quoted_scalar() {
        let messages = |yaml: &str| -> Vec<String> {
            let file = crate::YamlFile::from_str(yaml).unwrap();
            Validator::new()
                .validate_syntax(file.syntax())
                .into_iter()
                .map(|v| v.message)
                .collect()
        };
        for yaml in [
            "a: \"x\n---\ny\"\n",
            "'x\n... y'\n",
            "--- \"a\n...\tb\"\n",
            "--- \"a\n--- b\"\n",
            "---\n\"\n---\n\"\n",
        ] {
            assert_eq!(
                messages(yaml),
                vec!["Document marker inside a quoted scalar".to_string()],
                "{yaml:?}"
            );
        }
        for yaml in [
            "\"x\n---y\"\n",
            "--- \"a\n---\"\n",
            "'a\n...x'\n",
            "--- \"a\n---x\nb\"\n",
            "--- 'a\n...'\n",
        ] {
            assert_eq!(messages(yaml), Vec::<String>::new(), "{yaml:?}");
        }
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

        // A byte order mark may directly precede a comment line.
        let parsed = YamlFile::from_str("\u{FEFF}# c\na: 1\n").unwrap();
        assert_eq!(validator.validate_syntax(parsed.syntax()), vec![]);
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
    fn test_doc_start_block_sequence() {
        assert_eq!(
            located("--- - 1\n"),
            vec![(
                "Content on same line as document start marker".to_string(),
                "1:1".to_string()
            )]
        );
        assert_eq!(
            located("--- - 1\n- 2\n"),
            vec![
                (
                    "Content on same line as document start marker".to_string(),
                    "1:1".to_string()
                ),
                (
                    "Inconsistent sequence item indentation".to_string(),
                    "2:1".to_string()
                )
            ]
        );
        assert_eq!(located("--- # c\n- 1\n"), vec![]);
    }

    fn messages(yaml: &str) -> Vec<String> {
        let file = crate::YamlFile::from_str(yaml).unwrap();
        Validator::new()
            .validate_syntax(file.syntax())
            .into_iter()
            .map(|v| v.message)
            .collect()
    }

    fn located(yaml: &str) -> Vec<(String, String)> {
        let file = crate::YamlFile::from_str(yaml).unwrap();
        Validator::new()
            .validate_syntax(file.syntax())
            .into_iter()
            .map(|v| (v.message, v.location.unwrap()))
            .collect()
    }

    #[test]
    fn test_block_scalar_in_flow_collection() {
        for (yaml, location) in [
            ("[|\n  x\n]\n", "1:2"),
            ("[k: |\n  x\n]\n", "1:5"),
            ("{k: >\n  x\n}\n", "1:5"),
            ("- [a, !!str >-\n    x\n  ]\n", "1:13"),
            ("{|: x}\n", "1:2"),
        ] {
            assert_eq!(
                located(yaml),
                vec![(
                    "Block scalar cannot appear inside a flow collection".to_string(),
                    location.to_string()
                )],
                "{yaml:?}"
            );
        }
        for yaml in ["[a|b, c>d]\n", "k: |\n  [x]\n", "{k: '|'}\n", "[\"> x\"]\n"] {
            assert_eq!(located(yaml), vec![], "{yaml:?}");
        }
    }

    #[test]
    fn test_flow_entries_need_commas() {
        // A trailing comma used to make up for one missing between entries.
        for (yaml, locations) in [
            ("[k:\n  x: 1,\n]\n", vec!["2:4"]),
            ("{a: 1 b: 2, }\n", vec!["1:8"]),
            ("{foo: 1 bar: 2}\n", vec!["1:12"]),
            ("[a: b: c, d: e: f]\n", vec!["1:6", "1:15"]),
            ("{\n foo: 1\n bar: 2 }\n", vec!["3:5"]),
        ] {
            assert_eq!(
                located(yaml),
                locations
                    .into_iter()
                    .map(|l| (
                        "Missing comma between flow collection entries".to_string(),
                        l.to_string()
                    ))
                    .collect::<Vec<_>>(),
                "{yaml:?}"
            );
        }
        for yaml in ["[a, b c, ]\n", "{a: 1, b: 2,}\n", "[a,\n b]\n", "[a b]\n"] {
            assert_eq!(located(yaml), vec![], "{yaml:?}");
        }
    }

    #[test]
    fn test_flow_pair_only_as_sequence_entry() {
        for (yaml, location) in [
            ("[k: ? a]\n", "1:5"),
            ("{k: ? a}\n", "1:5"),
            ("[k: ? a: b]\n", "1:5"),
            ("- {x: [y, k: ? a]}\n", "1:14"),
        ] {
            assert_eq!(
                located(yaml),
                vec![(
                    "Mapping without braces can only be a flow sequence entry".to_string(),
                    location.to_string()
                )],
                "{yaml:?}"
            );
        }
        for yaml in [
            "[? a]\n",
            "[k: v]\n",
            "[[? a: b]]\n",
            "{? a}\n",
            "[? a\n? b]\n",
        ] {
            assert_eq!(located(yaml), vec![], "{yaml:?}");
        }
    }

    #[test]
    fn test_key_holds_one_node() {
        for (yaml, location) in [
            ("? a\n 5b: 1\n", "2:2"),
            ("? a\n  5b: 1\n", "2:3"),
            ("? !t a\n 5b: 1\n", "2:2"),
            ("? [a]\n 5b: 1\n", "2:2"),
            ("- ? a\n   5b: 1\n", "2:4"),
        ] {
            assert_eq!(
                located(yaml),
                vec![(
                    "Key holds more than one node".to_string(),
                    location.to_string()
                )],
                "{yaml:?}"
            );
        }
        for yaml in [
            "? a5\n",
            "? a\n5b: 1\n",
            "? a\n:5b: 1\n",
            "? a\n  b\n",
            "? &x\n  5b: 1\n",
            "? !t\n  5b: 1\n",
            "x:\n  ? a\n  5b: 1\n",
            "- ? a\n  5b: 1\n",
        ] {
            assert_eq!(located(yaml), vec![], "{yaml:?}");
        }
    }

    #[test]
    fn test_compact_mapping_in_explicit_entry() {
        for yaml in [
            "? a: 1\n  b: 2\n",
            "?   a: 1\n    b: 2\n",
            "? x\n: a: 1\n  b: 2\n",
            "- ? a: 1\n    b: 2\n  : c: 3\n    d: 4\n",
        ] {
            assert_eq!(located(yaml), vec![], "{yaml:?}");
        }
        assert_eq!(
            located("? a: 1\n b: 2\n"),
            vec![(
                "Sibling block mapping entries have inconsistent indentation \
                 (expected \"  \", found \" \")"
                    .to_string(),
                "2:1".to_string()
            )]
        );
    }

    #[test]
    fn test_indented_root_mapping() {
        for yaml in [
            "  a: 1\n  b: 2\n",
            "\n  a: 1\n  b: 2\n",
            "# c\n  a: 1\n  b: 2\n",
            "---\n  a: 1\n  b: 2\n",
            "--- # c\n  a: 1\n  b: 2\n",
            "a: 1\n---\n  b: 1\n  c: 2\n",
            "  - a\n  - b\n",
            "# c\n  - a\n  - b\n",
        ] {
            assert_eq!(located(yaml), vec![], "{yaml:?}");
        }
        assert_eq!(
            located("  a: 1\n   b: 2\n"),
            vec![(
                "Sibling block mapping entries have inconsistent indentation \
                 (expected \"  \", found \"   \")"
                    .to_string(),
                "2:1".to_string()
            )]
        );
    }

    #[test]
    fn test_directives_apply_per_document() {
        for yaml in [
            "%YAML 1.2\n--- a\n...\n%YAML 1.2\n--- b\n",
            "%TAG !a! tag:a,\n%TAG !b! tag:b,\n--- !a!x 1\n",
            "--- a\n...\n%YAML 1.2\n--- b\n",
            "%TAG !e! tag:a,\n--- !e!x a\n...\n%TAG !e! tag:b,\n--- !e!y b\n",
            "%YAML 1.2\n---\n",
            "%YAML 1.1  # comment\n---\n",
            "%YAM 1.1\n---\n",
            "--- |\n%x\n...\n--- b\n",
            "--- |\n%x\n--- b\n",
            "---\nscalar\n%YAML 1.2\n...\n--- b\n",
        ] {
            assert_eq!(messages(yaml), Vec::<String>::new(), "{yaml:?}");
        }
    }

    #[test]
    fn test_validate_only_checks_the_documents_own_directives() {
        let file = crate::YamlFile::from_str("%YAML 1.2\n--- a\n...\n---\n").unwrap();
        for doc in file.documents() {
            assert_eq!(Validator::new().validate(&doc), vec![]);
        }
    }

    #[test]
    fn test_directive_errors() {
        for (yaml, expected) in [
            ("%YAML 1.2\n%YAML 1.2\n---\na\n", "Duplicate %YAML directive"),
            ("%TAG !a! x\n%TAG !a! y\n---\na\n", "Duplicate %TAG directive for !a!"),
            ("%YAML 1.2\n...\n", "Directive without document start marker (---)"),
            ("%YAML 1.2 foo\n---\n", "Malformed %YAML directive"),
            ("%YAML 1.1#c\n---\n", "Malformed %YAML directive"),
            (
                "a: b\n%YAML 1.2\n--- c\n",
                "Directive after document requires document end marker (...)",
            ),
            (
                "---\n%x\n...\n--- b\n",
                "Directive in document content (missing document end marker `...` before directive)",
            ),
            (
                "%TAG !e! tag:a,\n--- !e!x a\n...\n--- !e!y b\n",
                "Tag shorthand !e! is only defined in the document that declares it",
            ),
        ] {
            assert_eq!(messages(yaml), vec![expected.to_string()], "{yaml:?}");
        }
    }

    #[test]
    fn test_tabs_that_are_not_indentation() {
        for yaml in [
            "foo: 1\n\t\nbar: 2\n",
            "k: v\n \t\nj: w\n",
            "- a\n- b\n\t\n- c\n",
            "a: [\n  b,\n\t\n  c]\n",
            "a: 1 # c\n# d\n\t# e\nb: 2\n",
            "foo:\n \tbar\n",
            "k: [a,\n  \tb]\n",
            "a:\n \t[x]\n",
            "a:\n \t{x: y}\n",
        ] {
            let file = crate::YamlFile::from_str(yaml).unwrap();
            assert_eq!(
                Validator::new().validate_syntax(file.syntax()),
                vec![],
                "{yaml:?}"
            );
        }
    }

    #[test]
    fn test_tab_indentation_rejected() {
        // DK95/06 is the third: the tab moves the entry's column.
        for yaml in [
            "a:\n\tb: 1\n",
            "- [\n\tfoo,\n foo\n ]\n",
            "foo:\n  a: 1\n  \tb: 2\n",
            "a:\n \t- x\n",
            "a:\n \tb: 1\n",
        ] {
            let file = crate::YamlFile::from_str(yaml).unwrap();
            let tab_errors: Vec<_> = Validator::new()
                .validate_syntax(file.syntax())
                .into_iter()
                .filter(|v| v.rule == Rule::InvalidTabUsage)
                .map(|v| v.message)
                .collect();
            assert_eq!(
                tab_errors,
                vec!["Tabs are not allowed for indentation in YAML".to_string()],
                "{yaml:?}"
            );
        }
    }

    #[test]
    fn test_error_preview_truncates_on_char_boundary() {
        // The preview used to be cut at byte 50, which panicked when that
        // fell inside a multi-byte character.
        let yaml = format!("a: b\n{}\u{e9}yz\n", "x".repeat(49));
        let (file, _) = crate::YamlFile::from_str_relaxed(&yaml);
        let violations = Validator::new().validate_syntax(file.syntax());
        let messages: Vec<_> = violations.iter().map(|v| v.message.as_str()).collect();
        assert_eq!(
            messages,
            vec![format!(
                "Invalid content in document: \"{}\u{e9}...\"",
                "x".repeat(49)
            )]
        );
    }

    #[test]
    fn test_columns_ignore_byte_order_mark() {
        for yaml in ["\u{feff}- a\n- b\n", "\u{feff}- a: 1\n  b: 2\n"] {
            let file = crate::YamlFile::from_str(yaml).unwrap();
            assert_eq!(
                Validator::new().validate_syntax(file.syntax()),
                vec![],
                "{yaml:?}"
            );
        }
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

    #[test]
    fn test_nested_mapping_indentation_accepted() {
        for yaml in [
            "k: !t\n  a: 1\n  b: 2\n",
            "- !!map\n  a: 1\n  b: 2\n",
            "- &a !t\n  a: 1\n  b: 2\n",
            "k:\n  ? a\n  ? b\n",
            "set: !!set\n  ? a\n  ? b\n",
        ] {
            let file = crate::YamlFile::from_str(yaml).unwrap();
            assert_eq!(
                Validator::new().validate_syntax(file.syntax()),
                vec![],
                "{yaml:?}"
            );
        }
    }

    #[test]
    fn test_set_content_must_be_mapping() {
        for (yaml, location) in [
            ("s: !!set [a, b]\n", "1:4"),
            ("- !!set\n  - a\n", "1:3"),
            ("s: !!set\nt: 1\n", "1:4"),
            ("!!set\n", "1:1"),
        ] {
            let file = crate::YamlFile::from_str(yaml).unwrap();
            let violations: Vec<_> = Validator::new()
                .validate_syntax(file.syntax())
                .into_iter()
                .map(|v| (v.message, v.location, v.severity, v.rule))
                .collect();
            assert_eq!(
                violations,
                vec![(
                    "!!set content is not a mapping".to_string(),
                    Some(location.to_string()),
                    Severity::Warning,
                    Rule::InvalidTag
                )],
                "{yaml:?}"
            );
        }
        for yaml in ["s: !!set {? a}\n", "s: !!set &x\n  ? a\n", "s: !!set {}\n"] {
            let file = crate::YamlFile::from_str(yaml).unwrap();
            assert_eq!(
                Validator::new().validate_syntax(file.syntax()),
                vec![],
                "{yaml:?}"
            );
        }
    }

    #[test]
    fn test_block_mapping_after_properties() {
        for (yaml, location) in [
            ("!x ? a\n", "1:4"),
            ("&a ? a\n", "1:4"),
            ("k: !x &a ? a\n", "1:10"),
            ("- !x ? a\n", "1:6"),
        ] {
            let file = crate::YamlFile::from_str(yaml).unwrap();
            let violations: Vec<_> = Validator::new()
                .validate_syntax(file.syntax())
                .into_iter()
                .map(|v| (v.message, v.location, v.severity, v.rule))
                .collect();
            assert_eq!(
                violations,
                vec![(
                    "A block mapping cannot start on the line of its tag or anchor".to_string(),
                    Some(location.to_string()),
                    Severity::Error,
                    Rule::Other
                )],
                "{yaml:?}"
            );
        }
        for yaml in [
            "!x\n? a\n",
            "&a\n? a\n",
            "!x a: 1\n",
            "k: !x {? a}\n",
            "- ? a\n",
            "? &a a\n",
        ] {
            let file = crate::YamlFile::from_str(yaml).unwrap();
            assert_eq!(
                Validator::new().validate_syntax(file.syntax()),
                vec![],
                "{yaml:?}"
            );
        }
    }

    #[test]
    fn test_anchor_on_alias() {
        let both = "Node cannot have both an anchor and be an alias";
        for (yaml, expected) in [
            ("a: &a 1\nb:\n- &b *a\n", vec![(both, "3:3", Rule::Other)]),
            ("- &a 1\n- &b *a\n", vec![(both, "2:3", Rule::Other)]),
            (
                "--- &b *a\n",
                vec![
                    (both, "1:5", Rule::Other),
                    ("Undefined alias *a", "1:8", Rule::InvalidAnchor),
                ],
            ),
            ("a: &a 1\nb: [&b *a]\n", vec![(both, "2:5", Rule::Other)]),
            (
                "a: &a 1\nb: [&b *a : v]\n",
                vec![(both, "2:5", Rule::Other)],
            ),
            (
                "a: &a 1\nb: {&b *a : v}\n",
                vec![(both, "2:5", Rule::Other)],
            ),
            ("a: &a 1\n? &b *a\n: v\n", vec![(both, "2:3", Rule::Other)]),
            ("a: &a 1\n&b *a : v\n", vec![(both, "2:1", Rule::Other)]),
            (
                "a: &a 1\nb:\n- &b *a : v\n",
                vec![(both, "3:3", Rule::Other)],
            ),
            (
                "a: &a 1\nk:\n  &b *a : v\n",
                vec![(both, "3:3", Rule::Other)],
            ),
            ("a: &a 1\n? x\n: &b *a\n", vec![(both, "3:3", Rule::Other)]),
            ("a: &a 1\nk: &b\n  *a\n", vec![(both, "2:4", Rule::Other)]),
            (
                "a: &a 1\nk: &b !t *a\n",
                vec![
                    (both, "2:4", Rule::Other),
                    ("An alias cannot have a tag", "2:7", Rule::InvalidTag),
                ],
            ),
        ] {
            let file = crate::YamlFile::from_str(yaml).unwrap();
            let violations: Vec<_> = Validator::new()
                .validate_syntax(file.syntax())
                .into_iter()
                .map(|v| (v.message, v.location, v.severity, v.rule))
                .collect();
            let expected: Vec<_> = expected
                .into_iter()
                .map(|(message, location, rule)| {
                    (
                        message.to_string(),
                        Some(location.to_string()),
                        Severity::Error,
                        rule,
                    )
                })
                .collect();
            assert_eq!(violations, expected, "{yaml:?}");
        }
        for yaml in [
            "a: &a 1\nk: &c !t\n  x: *a\n",
            "a: &a 1\nk: &c !t [*a]\n",
            "a: &a 1\nk: &c [*a]\n",
            "a: &a 1\nk: &c x *a\n",
            "a: &a 1\nt: &n\n  *a : v\n",
            "a: &a 1\nb: &n\n- *a\n",
        ] {
            let file = crate::YamlFile::from_str(yaml).unwrap();
            assert_eq!(
                Validator::new().validate_syntax(file.syntax()),
                vec![],
                "{yaml:?}"
            );
        }
    }

    #[test]
    fn test_tag_on_alias() {
        for (yaml, expected) in [
            (
                "a: &a 1\nk: !t *a\n",
                vec![("An alias cannot have a tag", "2:4", Rule::InvalidTag)],
            ),
            (
                "- &a 1\n- !t *a\n",
                vec![("An alias cannot have a tag", "2:3", Rule::InvalidTag)],
            ),
            (
                "[&a 1, !t *a]\n",
                vec![("An alias cannot have a tag", "1:8", Rule::InvalidTag)],
            ),
            (
                "a: &a 1\nb: {!!str *a : v}\n",
                vec![("An alias cannot have a tag", "2:5", Rule::InvalidTag)],
            ),
            (
                "a: &a 1\n!t *a : v\n",
                vec![("An alias cannot have a tag", "2:1", Rule::InvalidTag)],
            ),
            (
                "a: &a 1\nb:\n- !t *a : v\n",
                vec![("An alias cannot have a tag", "3:3", Rule::InvalidTag)],
            ),
            (
                "a: &a 1\nb: {!t *a : v}\n",
                vec![("An alias cannot have a tag", "2:5", Rule::InvalidTag)],
            ),
            (
                "a: &a 1\nk: !t\n  *a\n",
                vec![("An alias cannot have a tag", "2:4", Rule::InvalidTag)],
            ),
            (
                "a: &a 1\nk: !t &b *a\n",
                vec![
                    (
                        "Node cannot have both an anchor and be an alias",
                        "2:7",
                        Rule::Other,
                    ),
                    ("An alias cannot have a tag", "2:4", Rule::InvalidTag),
                ],
            ),
        ] {
            let file = crate::YamlFile::from_str(yaml).unwrap();
            let violations: Vec<_> = Validator::new()
                .validate_syntax(file.syntax())
                .into_iter()
                .map(|v| (v.message, v.location, v.severity, v.rule))
                .collect();
            let expected: Vec<_> = expected
                .into_iter()
                .map(|(message, location, rule)| {
                    (
                        message.to_string(),
                        Some(location.to_string()),
                        Severity::Error,
                        rule,
                    )
                })
                .collect();
            assert_eq!(violations, expected, "{yaml:?}");
        }
        for yaml in ["a: &a 1\nk: !t &b x\nl: *a\n", "a: &a 1\nk: !t\n  *a : v\n"] {
            let file = crate::YamlFile::from_str(yaml).unwrap();
            assert_eq!(
                Validator::new().validate_syntax(file.syntax()),
                vec![],
                "{yaml:?}"
            );
        }
    }

    #[test]
    fn test_pairs_content_must_be_sequence() {
        for (yaml, location, message) in [
            (
                "o: !!omap {a: 1}\n",
                "1:4",
                "!!omap content is not a sequence",
            ),
            (
                "- !!pairs\n  a: 1\n",
                "1:3",
                "!!pairs content is not a sequence",
            ),
            (
                "o: !!omap\n  3\n",
                "1:4",
                "!!omap content is not a sequence",
            ),
            (
                "- !!omap\n- a: 1\n",
                "1:3",
                "!!omap content is not a sequence",
            ),
        ] {
            let file = crate::YamlFile::from_str(yaml).unwrap();
            let violations: Vec<_> = Validator::new()
                .validate_syntax(file.syntax())
                .into_iter()
                .map(|v| (v.message, v.location, v.severity, v.rule))
                .collect();
            assert_eq!(
                violations,
                vec![(
                    message.to_string(),
                    Some(location.to_string()),
                    Severity::Warning,
                    Rule::InvalidTag
                )],
                "{yaml:?}"
            );
        }
        for yaml in [
            "o: !!omap [a: 1]\n",
            "o: !!pairs &x\n  - a: 1\n  - a: 2\n",
            "o: !!omap []\n",
        ] {
            let file = crate::YamlFile::from_str(yaml).unwrap();
            assert_eq!(
                Validator::new().validate_syntax(file.syntax()),
                vec![],
                "{yaml:?}"
            );
        }
    }

    #[test]
    fn test_tagged_alias_reported_once() {
        for yaml in [
            "a: &a [x]\nb: !!omap *a\n",
            "a: &a [x]\nb: !!pairs *a\n",
            "a: &a {x: }\nb: !!set *a\n",
            "a: &a [x]\nb: !!set *a\n",
        ] {
            let file = crate::YamlFile::from_str(yaml).unwrap();
            let violations: Vec<_> = Validator::new()
                .validate_syntax(file.syntax())
                .into_iter()
                .map(|v| (v.message, v.location, v.severity, v.rule))
                .collect();
            assert_eq!(
                violations,
                vec![(
                    "An alias cannot have a tag".to_string(),
                    Some("2:4".to_string()),
                    Severity::Error,
                    Rule::InvalidTag
                )],
                "{yaml:?}"
            );
        }
    }

    #[test]
    fn test_pairs_items_must_be_single_entry_mappings() {
        for (yaml, message, location) in [
            (
                "o: !!omap [a]\n",
                "!!omap item \"a\" is not a mapping with a single entry",
                "1:12",
            ),
            (
                "o: !!omap [{a: 1, b: 2}]\n",
                "!!omap item \"{a: 1, b: 2}\" is not a mapping with a single entry",
                "1:12",
            ),
            (
                "o: !!pairs\n- !x {a: 1}\n",
                "!!pairs item \"!x {a: 1}\" is not a mapping with a single entry",
                "2:3",
            ),
            (
                "o: !!pairs [!!map [a]]\n",
                "!!pairs item \"!!map [a]\" is not a mapping with a single entry",
                "1:13",
            ),
            (
                "o: !!omap\n- [a: 1]\n",
                "!!omap item \"[a: 1]\" is not a mapping with a single entry",
                "2:3",
            ),
            (
                "o: !!omap\n- {}\n",
                "!!omap item \"{}\" is not a mapping with a single entry",
                "2:3",
            ),
        ] {
            let file = crate::YamlFile::from_str(yaml).unwrap();
            let violations: Vec<_> = Validator::new()
                .validate_syntax(file.syntax())
                .into_iter()
                .map(|v| (v.message, v.location, v.severity, v.rule))
                .collect();
            assert_eq!(
                violations,
                vec![(
                    message.to_string(),
                    Some(location.to_string()),
                    Severity::Warning,
                    Rule::InvalidTag
                )],
                "{yaml:?}"
            );
        }
        for yaml in [
            "o: !!omap [a: 1, b: 2]\n",
            "o: !!pairs [!!map {a: 1}, a: 2]\n",
            "o: !!omap\n- a: 1\n- b:\n",
            "m: &m {a: 1}\no: !!omap [*m]\n",
            "o: [a, {b: 1, c: 2}]\n",
            "o: !x [a]\n",
        ] {
            let file = crate::YamlFile::from_str(yaml).unwrap();
            assert_eq!(
                Validator::new().validate_syntax(file.syntax()),
                vec![],
                "{yaml:?}"
            );
        }
    }

    #[test]
    fn test_repeated_omap_key() {
        for (yaml, message, location) in [
            (
                "o: !!omap [a: 1, a: 2]\n",
                "Duplicate !!omap key: \"a\" (semantically equal to \"a\")",
                "1:18",
            ),
            (
                "o: !!omap\n- 16: a\n- b: 2\n- 0x10: c\n",
                "Duplicate !!omap key: \"0x10\" (semantically equal to \"16\")",
                "4:3",
            ),
            (
                "o: !!omap [!!map {a: 1}, a: 2]\n",
                "Duplicate !!omap key: \"a\" (semantically equal to \"a\")",
                "1:26",
            ),
        ] {
            let file = crate::YamlFile::from_str(yaml).unwrap();
            let violations: Vec<_> = Validator::new()
                .validate_syntax(file.syntax())
                .into_iter()
                .map(|v| (v.message, v.location, v.severity, v.rule))
                .collect();
            assert_eq!(
                violations,
                vec![(
                    message.to_string(),
                    Some(location.to_string()),
                    Severity::Warning,
                    Rule::DuplicateKeys
                )],
                "{yaml:?}"
            );
        }
        for yaml in [
            "o: !!omap [a: 1, b: 2]\n",
            "o: !!omap [1: a, '1': b]\n",
            "o: !!pairs [a: 1, a: 2]\n",
            "o: [a: 1, a: 2]\n",
        ] {
            let file = crate::YamlFile::from_str(yaml).unwrap();
            assert_eq!(
                Validator::new().validate_syntax(file.syntax()),
                vec![],
                "{yaml:?}"
            );
        }
        // An item that is not a mapping has no key to repeat.
        let file = crate::YamlFile::from_str("o: !!omap [!x {a: 1}, a: 2]\n").unwrap();
        let rules: Vec<_> = Validator::new()
            .validate_syntax(file.syntax())
            .into_iter()
            .map(|v| v.rule)
            .collect();
        assert_eq!(rules, vec![Rule::InvalidTag]);
        // A repeated !!set member is a repeated mapping key.
        let file = crate::YamlFile::from_str("s: !!set {? a, ? a}\n").unwrap();
        let violations: Vec<_> = Validator::new()
            .validate_syntax(file.syntax())
            .into_iter()
            .map(|v| (v.message, v.location, v.severity, v.rule))
            .collect();
        assert_eq!(
            violations,
            vec![(
                "Duplicate key: \"? a\" (semantically equal to \"? a\")".to_string(),
                Some("1:16".to_string()),
                Severity::Error,
                Rule::DuplicateKeys
            )]
        );
    }

    #[test]
    fn test_set_values_must_be_null() {
        for (yaml, message, location) in [
            (
                "s: !!set {a: 1}\n",
                "!!set member \"a\" has non-null value \"1\"",
                "1:14",
            ),
            (
                "s: !!set\n  a: [x]\n  b:\n",
                "!!set member \"a\" has non-null value \"[x]\"",
                "2:6",
            ),
            (
                "s: !!set\n  ? a\n  : x\n",
                "!!set member \"a\" has non-null value \"x\"",
                "3:5",
            ),
        ] {
            let file = crate::YamlFile::from_str(yaml).unwrap();
            let violations: Vec<_> = Validator::new()
                .validate_syntax(file.syntax())
                .into_iter()
                .map(|v| (v.message, v.location, v.severity, v.rule))
                .collect();
            assert_eq!(
                violations,
                vec![(
                    message.to_string(),
                    Some(location.to_string()),
                    Severity::Warning,
                    Rule::InvalidTag
                )],
                "{yaml:?}"
            );
        }
        for yaml in [
            "s: !!set {a: null, b: ~, ? c, d}\n",
            "s: !!set\n  a:\n  ? b\n  c: !!null\n",
            "n: &n ~\ns: !!set {a: *n}\n",
        ] {
            let file = crate::YamlFile::from_str(yaml).unwrap();
            assert_eq!(
                Validator::new().validate_syntax(file.syntax()),
                vec![],
                "{yaml:?}"
            );
        }
    }

    #[test]
    fn test_violations_carry_locations() {
        for (yaml, expected) in [
            (
                "%YAML 1.2\n%YAML 1.2\n---\na\n",
                ("Duplicate %YAML directive", "2:1"),
            ),
            (
                "---\nk: v\n%YAML 1.2\n---\n",
                (
                    "Directive after document requires document end marker (...)",
                    "3:1",
                ),
            ),
            (
                "a: |0\n  b\n",
                (
                    "Block scalar indentation indicator must be a digit 1-9",
                    "1:5",
                ),
            ),
            (
                "a: \"b\" c\n",
                ("Trailing content after quoted string", "1:8"),
            ),
            (
                "x: 1\na: b: c\n",
                (
                    "Plain scalar value cannot contain mapping syntax (colon)",
                    "2:5",
                ),
            ),
            (
                "k: - a\n",
                (
                    "Block sequence cannot start on same line as mapping key",
                    "1:4",
                ),
            ),
            (
                "k: [q]: v\n",
                (
                    "Block mapping cannot start on same line as mapping key",
                    "1:4",
                ),
            ),
            (
                "\u{e9}: \"a\nb\"\n",
                ("Wrong indented multiline quoted scalar", "1:4"),
            ),
        ] {
            let file = crate::YamlFile::from_str(yaml).unwrap();
            let violations: Vec<_> = Validator::new()
                .validate_syntax(file.syntax())
                .into_iter()
                .map(|v| (v.message, v.location))
                .collect();
            assert_eq!(
                violations,
                vec![(expected.0.to_string(), Some(expected.1.to_string()))],
                "{yaml:?}"
            );
        }
    }

    #[test]
    fn test_validate_fills_locations() {
        let doc = Document::from_str("a: 1\nb: 2\na: 3\n").unwrap();
        let locations: Vec<_> = Validator::new()
            .validate(&doc)
            .into_iter()
            .map(|v| v.location)
            .collect();
        assert_eq!(locations, vec![Some("3:1".to_string())]);
    }

    #[test]
    fn test_escape_needs_its_hex_digits() {
        for (yaml, expected) in [
            ("k: \"\\xZZ\"\n", "Invalid escape sequence: \\xZZ"),
            ("k: \"a\\x4z\"\n", "Invalid escape sequence: \\x4z"),
            ("k: \"\\u12\"\n", "Invalid escape sequence: \\u12\""),
            (
                "k: \"\\U0001F60\"\n",
                "Invalid escape sequence: \\U0001F60\"",
            ),
        ] {
            assert_eq!(messages(yaml), vec![expected.to_string()], "{yaml:?}");
        }
        assert_eq!(
            messages("k: \"\\u00e9\\x41\\U0001F600\"\n"),
            Vec::<String>::new()
        );
    }

    #[test]
    fn test_continuation_must_clear_enclosing_entry() {
        for (yaml, expected) in [
            (
                "a:\n  b: [x,\n  y]\n",
                "Flow collection continuation line must be indented",
            ),
            (
                "a:\n  b: \"x\n  y\"\n",
                "Wrong indented multiline quoted scalar",
            ),
            (
                "- a: \"x\n  y\"\n",
                "Wrong indented multiline quoted scalar",
            ),
            (
                "- \"a\n\tb\"\n",
                "Tabs are not allowed for indentation in YAML",
            ),
            (
                "k: \"a\n\tb\"\n",
                "Tabs are not allowed for indentation in YAML",
            ),
        ] {
            assert_eq!(messages(yaml), vec![expected.to_string()], "{yaml:?}");
        }
        for yaml in [
            "a:\n  b: [x,\n   y]\n",
            "a:\n  b: \"x\n   y\"\n",
            "- a: \"x\n   y\"\n",
            "k: [a,\n# c\n  b]\n",
        ] {
            assert_eq!(messages(yaml), Vec::<String>::new(), "{yaml:?}");
        }
    }

    #[test]
    fn test_blank_line_deeper_than_explicit_indent_is_content() {
        // With an indentation indicator a line indented past the body is
        // content, not a leading blank line that sets the indentation.
        for yaml in [
            "k: |2-\n   \n  x\n",
            "- >2\n   \n  x\n",
            "a:\n  k: |2\n     \n    x\n",
        ] {
            assert_eq!(messages(yaml), Vec::<String>::new(), "{yaml:?}");
        }
        assert_eq!(
            messages("k: |\n   \n  x\n"),
            vec![
                "Block scalar content under-indented (2 spaces) relative to preceding blank line (3 spaces)"
                    .to_string()
            ]
        );
    }

    #[test]
    fn test_block_mapping_with_flow_collection_key() {
        // The mapping's first token is the key's `{` or `[`, which made it
        // look like a flow collection.
        for yaml in [
            "{ a: b }:\n  hr: 65\nx: y\n",
            "{ a: b }: c\nx: y\n",
            "{ a: b }:\n- 1\nx: y\n",
            "- { a: b }:\n    hr: 65\n  x: y\n",
            "k:\n  { a: b }:\n    hr: 65\n  x: y\n",
            "[ a ]:\n  hr: 65\nx: y\n",
            "[a]: 1\n[b]: 2\n",
        ] {
            assert_eq!(messages(yaml), Vec::<String>::new(), "{yaml:?}");
        }
        for (yaml, expected) in [
            (
                "{a: b}:\n  hr: 65\n x: y\n",
                "Sibling block mapping entries have inconsistent indentation (expected \"\", found \" \")",
            ),
            (
                "[a]: 1\nb: [x,\ny]\n",
                "Flow collection continuation line must be indented",
            ),
            (
                "{a: b}: 1\nb: [x,\ny]\n",
                "Flow collection continuation line must be indented",
            ),
            (
                "a:\n \t{x: y}: z\n",
                "Tabs are not allowed for indentation in YAML",
            ),
            (
                "--- {a: b}: c\n",
                "Content on same line as document start marker",
            ),
            (
                "--- [a]: c\n",
                "Content on same line as document start marker",
            ),
        ] {
            assert_eq!(messages(yaml), vec![expected.to_string()], "{yaml:?}");
        }
    }

    #[test]
    fn test_tab_before_flow_collection_is_separation() {
        // Only a block collection needs `s-indent` after an indicator; a flow
        // collection is separated from it like a scalar.
        for yaml in [
            "a: b\nseq:\t[]\n",
            "a: b\nseq:\t[1]\n",
            "seq:\t{}\n",
            "k:\n  seq:\t[]\n  x: 1\n",
            "- a\n-\t[]\n",
            "-\t{a: b}\n",
            "?\t[a]\n:\t{b: c}\n",
            "seq:\t&a [x]\n",
        ] {
            assert_eq!(messages(yaml), Vec::<String>::new(), "{yaml:?}");
        }
        for yaml in ["-\t{a: b}: c\n", "-\t[a]: c\n"] {
            assert_eq!(
                messages(yaml),
                vec!["Tabs are not allowed for indentation in YAML".to_string()],
                "{yaml:?}"
            );
        }
    }

    #[test]
    fn test_block_indicator_in_plain_scalar_continuation() {
        // A `|` or `>` opening a continuation line is plain scalar content.
        for yaml in ["a: b\n  > c\n", "a: b\n  | c\n", "- x\n  | y\n"] {
            assert_eq!(messages(yaml), Vec::<String>::new(), "{yaml:?}");
        }
    }
}
