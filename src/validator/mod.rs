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

mod directives;
mod documents;
mod scalars;

use crate::anchor_resolution::{document_scope, is_merge_key, AnchorRegistry, MergedMapping};
use crate::as_yaml::SharedIdentities;
use crate::column::{space_indent, ColumnIndex};
use crate::nodes::{
    child_token, has_child_token, in_flow_collection, is_block_scalar, is_quoted_scalar,
    starts_a_flow_collection,
};
use crate::tag;
use crate::yaml::{Document, SyntaxNode};
use rowan::ast::AstNode;
use std::fmt;

use directives::{
    check_directive_after_document, check_directive_in_content, check_directives,
    check_tag_shorthand_scope,
};
use documents::{
    check_doc_start_token_content, check_document_level_anchors, check_document_marker_placement,
    check_single_content_node,
};
use scalars::{
    check_block_scalar_content_indent, check_block_scalar_header_indent,
    check_block_scalar_indicator, check_colon_in_plain_scalar, check_escape_sequences,
    check_flow_indicator_in_plain_scalar, check_legacy_yaml_1_1, check_plain_scalar_start,
    check_quoted_string_indentation, check_trailing_content_after_quoted,
};

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

/// The least indentation a continuation line of `node` needs: one more
/// than the column of the innermost block collection entry holding it, or
/// `None` outside any block collection.
fn continuation_indent(node: &SyntaxNode, columns: &ColumnIndex) -> Option<usize> {
    let entry = node.ancestors().skip(1).find(is_block_entry)?;
    Some(columns.column(&entry.first_token()?) + 1)
}

/// Whether `node` is an entry of a collection without flow delimiters.
fn is_block_entry(node: &SyntaxNode) -> bool {
    matches!(
        node.kind(),
        crate::SyntaxKind::MAPPING_ENTRY | crate::SyntaxKind::SEQUENCE_ENTRY
    ) && node.parent().is_some_and(|c| !starts_a_flow_collection(&c))
}

/// What a node being walked passes on to the nodes in it.
#[derive(Clone, Copy)]
struct Scope {
    /// Whether they sit inside a flow collection.
    in_flow: bool,
    /// The least indentation their continuation lines need, as
    /// [`continuation_indent`] gives it.
    continuation_indent: Option<usize>,
}

impl Scope {
    /// The scope of the nodes in `node`, which sits in this one.
    fn enter(self, node: &SyntaxNode, columns: &ColumnIndex) -> Self {
        let opens_flow = matches!(
            node.kind(),
            crate::SyntaxKind::MAPPING | crate::SyntaxKind::SEQUENCE
        ) && starts_a_flow_collection(node);
        Scope {
            in_flow: self.in_flow || opens_flow,
            continuation_indent: if is_block_entry(node) {
                columns.column_at(node.text_range().start()).map(|c| c + 1)
            } else {
                self.continuation_indent
            },
        }
    }
}

/// Is this flow entry an omitted one, i.e. does its value consist solely of
/// the zero-width implicit-null scalar the parser emits for an empty slot?
///
/// See the implicit-null section of the CST invariants in `nodes/mod.rs`.
fn entry_is_empty(entry: &SyntaxNode) -> bool {
    // Stop at the first real character rather than rendering the entry,
    // whose text covers all the nesting below it.
    entry
        .text()
        .try_for_each_chunk(|chunk| {
            if chunk.chars().all(|c| c.is_whitespace() || c == ',') {
                Ok(())
            } else {
                Err(())
            }
        })
        .is_ok()
}

/// Set each violation's `line:column` location from its range, using the
/// text of the tree `node` belongs to.
fn fill_locations(node: &SyntaxNode, violations: &mut [Violation]) {
    // Indexing the whole stream for each of its documents made validating
    // them one by one quadratic, even when they were all valid.
    if violations.is_empty() {
        return;
    }
    let tree = node.ancestors().last().unwrap_or_else(|| node.clone());
    let index = crate::line_index::LineIndex::new(tree.text().to_string());
    for v in violations.iter_mut() {
        let Some(range) = v.text_range else {
            continue;
        };
        v.location = Some(index.line_column(range.start as usize).to_string());
    }
}

impl Violation {
    /// An error-severity violation covering `range` in the source.
    ///
    /// `location` is filled in once validation knows the source text.
    fn error_at(rule: Rule, range: rowan::TextRange, message: impl Into<String>) -> Self {
        Violation {
            message: message.into(),
            location: None,
            text_range: Some(range.into()),
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
        let doc = crate::nodes::immutable_copy(doc.syntax());

        // Only the directives that introduce this document apply to it,
        // which are those since the document before it.
        let mut directives: Vec<SyntaxNode> = doc
            .siblings(rowan::Direction::Prev)
            .skip(1)
            .take_while(|n| n.kind() != crate::SyntaxKind::DOCUMENT)
            .filter(|n| n.kind() == crate::SyntaxKind::DIRECTIVE)
            .collect();
        directives.reverse();
        check_directives(&directives, Some(&doc), &mut violations);

        // Walk the syntax tree and check for violations
        // This will catch ERROR nodes created by parser (including content after doc end)
        self.validate_node(&doc, &mut violations);

        fill_locations(&doc, &mut violations);
        violations
    }

    /// Validate from a syntax node (can be ROOT, DOCUMENT, or any node)
    ///
    /// This is useful when you need to validate the full parse tree including directives
    /// that may not be attached to a specific document.
    pub fn validate_syntax(&self, node: &SyntaxNode) -> Vec<Violation> {
        let mut violations = Vec::new();
        let node = &crate::nodes::immutable_copy(node);

        for (directives, document) in directive_groups(&find_root(node)) {
            check_directives(&directives, document.as_ref(), &mut violations);
        }

        // Check for directives after documents without document end marker
        check_directive_after_document(node, &mut violations);

        // Check that a `%TAG` shorthand is only used in its own document
        check_tag_shorthand_scope(node, &mut violations);

        check_explicit_key_separation(node, &mut violations);

        // Walk the syntax tree and check for violations
        self.validate_node(node, &mut violations);

        fill_locations(node, &mut violations);
        violations
    }

    /// Check `node` and everything below it, in document order.
    ///
    /// The walk is iterative, so deep nesting cannot overflow the stack.
    fn validate_node(&self, node: &SyntaxNode, violations: &mut Vec<Violation>) {
        let columns = ColumnIndex::new(node);
        let mut identities = SharedIdentities::default();
        // The scope of each node being walked; asking each node for its
        // ancestors made the walk quadratic in the nesting depth.
        let mut scopes = vec![Scope {
            in_flow: in_flow_collection(node),
            continuation_indent: continuation_indent(node, &columns),
        }];
        for event in node.preorder() {
            match event {
                rowan::WalkEvent::Enter(n) => {
                    let scope = *scopes.last().expect("the walk's own scope stays");
                    self.check_node(&n, &columns, &mut identities, scope, violations);
                    scopes.push(scope.enter(&n, &columns));
                }
                rowan::WalkEvent::Leave(_) => {
                    scopes.pop();
                }
            }
        }
    }

    /// Run the checks for `node` itself, not those of its children.
    fn check_node(
        &self,
        node: &SyntaxNode,
        columns: &ColumnIndex,
        identities: &mut SharedIdentities,
        scope: Scope,
        violations: &mut Vec<Violation>,
    ) {
        use crate::SyntaxKind;
        let in_flow = scope.in_flow;

        // Check for tabs in any node's tokens
        if self.config.check_tabs {
            check_tab_usage(node, violations);
        }

        // Check current node type
        // Check for multiple anchor tokens on any node (anchors are tokens, not nodes)
        if self.config.check_anchors {
            check_multiple_anchors(node, violations);
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
                check_implicit_key_multiline(node, columns, violations);
                // Check for block collection on same line as mapping key
                check_collection_on_same_line_as_key(node, in_flow, violations);
                check_explicit_value_column(node, columns, in_flow, violations);
            }
            SyntaxKind::SCALAR => {
                // Check for invalid escape sequences in quoted strings
                check_escape_sequences(node, violations);
                // Check for content on same line as block scalar indicator
                check_block_scalar_indicator(node, violations);
                check_block_scalar_header_indent(node, columns, scope, violations);
                // Check block scalar content indentation consistency
                check_block_scalar_content_indent(node, violations);
                // Check for trailing content after quoted strings
                check_trailing_content_after_quoted(node, violations);
                // Check for colons in plain scalar values
                check_colon_in_plain_scalar(node, violations);
                check_plain_scalar_start(node, in_flow, violations);
                check_flow_indicator_in_plain_scalar(node, in_flow, violations);
                check_block_scalar_in_flow(node, in_flow, violations);
                // Check for directives inside document content (e.g. %YAML after ---)
                check_directive_in_content(node, violations);
                // Warn about constructs YAML 1.1 read differently
                check_legacy_yaml_1_1(node, violations);
            }
            SyntaxKind::DOC_START | SyntaxKind::DOC_END if self.config.check_document_markers => {
                check_document_marker_placement(node, violations);
            }
            SyntaxKind::MAPPING => {
                check_flow_collection_commas(node, violations);
                check_flow_pair_placement(node, in_flow, violations);
                check_flow_continuation_indent(node, scope, violations);
                check_block_mapping_entries_on_same_line(node, violations);
                check_block_mapping_after_properties(node, violations);
                check_mapping_entry_indentation(node, columns, violations);
                if self.config.check_duplicate_keys {
                    check_duplicate_keys(node, identities, violations);
                }
            }
            SyntaxKind::SEQUENCE => {
                check_flow_collection_commas(node, violations);
                check_flow_continuation_indent(node, scope, violations);
            }
            SyntaxKind::KEY | SyntaxKind::SEQUENCE_ENTRY | SyntaxKind::VALUE => {
                check_anchor_and_alias(node, violations);
                check_single_content_node(node, violations);
            }
            SyntaxKind::TAGGED_NODE => {
                check_anchor_and_alias(node, violations);
                check_single_content_node(node, violations);
                check_tag_on_alias(node, violations);
                check_multiple_tags(node, violations);
                check_collection_tag_content(node, violations);
                check_pairs_items(node, violations);
                check_omap_keys(node, identities, violations);
                check_set_values(node, violations);
            }
            SyntaxKind::DOCUMENT => {
                check_anchor_and_alias(node, violations);
                check_single_content_node(node, violations);
                check_document_level_anchors(node, violations);
                if self.config.check_anchors {
                    check_undefined_aliases(node, violations);
                }
            }
            _ => {}
        }

        // Check tokens (like COMMENT, DOC_START, TAG) that are children but not nodes
        for element in node.children_with_tokens() {
            if let Some(token) = element.as_token() {
                // Check COMMENT tokens for whitespace separation
                if token.kind() == crate::SyntaxKind::COMMENT {
                    check_comment_token_whitespace(token, violations);
                }
                // Check DOC_START tokens for content on same line
                if token.kind() == crate::SyntaxKind::DOC_START {
                    check_doc_start_token_content(token, violations);
                }
                // The lexer leaves a `&` or `*` with no name after it as
                // such a token, where a node may start.
                match token.kind() {
                    crate::SyntaxKind::AMPERSAND => violations.push(Violation::error_at(
                        Rule::InvalidAnchor,
                        token.text_range(),
                        "Anchor without a name",
                    )),
                    crate::SyntaxKind::ASTERISK => violations.push(Violation::error_at(
                        Rule::InvalidAnchor,
                        token.text_range(),
                        "Alias without a name",
                    )),
                    _ => {}
                }
                // Check TAG tokens for invalid characters
                if token.kind() == crate::SyntaxKind::TAG {
                    check_tag_characters(token, violations);
                    check_tag_followed_by_comma(token, violations);
                }
            }
        }

        // Check indentation rules
        check_sequence_indentation(node, columns, violations);
        check_quoted_string_indentation(node, scope, violations);
    }
}

/// Check for multiple anchors on the same node
fn check_multiple_anchors(node: &SyntaxNode, violations: &mut Vec<Violation>) {
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

/// A node has one tag, but the parser nests a second one, as in
/// `!a !b x`, as a TAGGED_NODE of its own.
fn check_multiple_tags(node: &SyntaxNode, violations: &mut Vec<Violation>) {
    let has_tag = |n: &SyntaxNode| has_child_token(n, |k| k == crate::SyntaxKind::TAG);
    let inner = node
        .children()
        .find(|c| c.kind() == crate::SyntaxKind::TAGGED_NODE);
    if has_tag(node) && inner.is_some_and(|inner| has_tag(&inner)) {
        violations.push(Violation::error_at(
            Rule::InvalidTag,
            node.text_range(),
            "Multiple tags on the same node",
        ));
    }
}

/// Check that a block scalar does not appear inside a flow collection.
///
/// Flow content may only hold flow nodes. The parser reads `[k: |\n  x\n]`
/// as a plain scalar starting with `|`, while an edit can leave a real
/// block scalar there, so both shapes are checked.
fn check_block_scalar_in_flow(node: &SyntaxNode, in_flow: bool, violations: &mut Vec<Violation>) {
    use crate::SyntaxKind;

    let Some(first) = node.first_token() else {
        return;
    };
    let is_block_header = is_block_scalar(node)
        || (first.kind() == SyntaxKind::STRING && first.text().starts_with(['|', '>']));
    if is_block_header && in_flow {
        violations.push(Violation::error_at(
            Rule::Other,
            first.text_range(),
            "Block scalar cannot appear inside a flow collection",
        ));
    }
}

/// Check for a tab used as indentation among `node`'s own tokens.
fn check_tab_usage(node: &SyntaxNode, violations: &mut Vec<Violation>) {
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
        SyntaxKind::DOCUMENT
        | SyntaxKind::KEY
        | SyntaxKind::VALUE
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

/// Check for missing commas in flow collections
fn check_flow_collection_commas(node: &SyntaxNode, violations: &mut Vec<Violation>) {
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
fn check_flow_pair_placement(node: &SyntaxNode, in_flow: bool, violations: &mut Vec<Violation>) {
    if starts_a_flow_collection(node) || !in_flow {
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

/// Check for multiple mapping entries on the same line in block mappings
///
/// In block mappings (not flow mappings with {}), each mapping entry should
/// be on its own line. Multiple entries on the same line are invalid.
fn check_block_mapping_entries_on_same_line(node: &SyntaxNode, violations: &mut Vec<Violation>) {
    // Check if this is a flow mapping (which allows same-line entries)
    let first_token = node.first_token();
    let is_flow_mapping = first_token.as_ref().is_some_and(|t| t.text() == "{");

    if is_flow_mapping {
        return; // Flow mappings can have entries on same line
    }

    let entries: Vec<SyntaxNode> = node
        .children()
        .filter(|c| c.kind() == crate::SyntaxKind::MAPPING_ENTRY)
        .collect();
    // One violation is enough.
    if let Some(pair) = entries.windows(2).find(|w| !newline_between(&w[0], &w[1])) {
        violations.push(Violation::error_at(
            Rule::Other,
            pair[1].text_range(),
            "Block mapping entries must be on separate lines",
        ));
    }
}

/// Whether a line break separates mapping entry `prev` from the entry
/// `next` that follows it.
///
/// The newline may be inside `prev` (as its last token) or between the
/// entries as a sibling token.
fn newline_between(prev: &SyntaxNode, next: &SyntaxNode) -> bool {
    // An explicit key's entry ends with the zero-width implicit-null
    // scalar of its VALUE, so look past tokens that render as nothing:
    // `? a\n? b\n` has its newline inside the first entry, not between
    // them. In a nested mapping the next line's indentation follows that
    // newline (`k:\n  ? a\n  ? b\n`).
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
    prev_ends_with_newline
        || prev
            .siblings_with_tokens(rowan::Direction::Next)
            .skip(1)
            .take_while(|el| el.as_node() != Some(next))
            .any(|el| el.kind() == crate::SyntaxKind::NEWLINE)
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
fn check_mapping_entry_indentation(
    node: &SyntaxNode,
    columns: &ColumnIndex,
    violations: &mut Vec<Violation>,
) {
    // Flow mappings ({...}) don't have INDENT tokens between entries.
    if starts_a_flow_collection(node) {
        return;
    }

    // Every INDENT token that precedes a MAPPING_ENTRY (i.e. separates
    // two entries) must equal the expected indent. Otherwise the parser
    // has admitted a wrong-indented sibling that the YAML spec rejects
    // (see yaml-test-suite EW3V / DMG6 / N4JP / U44R).
    let expected_indent = expected_entry_indent(node, columns);

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
                        violations.push(Violation::error_at(
                            Rule::Other,
                            t.text_range(),
                            format!(
                                "Sibling block mapping entries have inconsistent indentation (expected {expected_indent:?}, found {:?})",
                                t.text()
                            ),
                        ));
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

/// The indentation every entry of block mapping `node` should have.
///
/// For a nested mapping (its parent VALUE has NEWLINE + INDENT before it)
/// that is the INDENT's text; otherwise (root, compact or explicit-entry
/// mapping) the column of its first entry.
fn expected_entry_indent(node: &SyntaxNode, columns: &ColumnIndex) -> String {
    // A mapping that is a sequence entry's value has its INDENT in the
    // SEQUENCE_ENTRY rather than a VALUE, in the same position relative
    // to the MAPPING. Without it the expected indent fell back to the
    // empty string and every entry of `-\n  a: 1\n  b: 2\n` looked
    // wrongly indented. A tagged mapping (`k: !t\n  a: 1\n`, C4HZ) has
    // it in the TAGGED_NODE.
    node.parent()
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
            Some(" ".repeat(columns.column(&node.first_token()?)))
        })
        .unwrap_or_default()
}

/// Check that a multi-line flow collection indents its continuation lines.
///
/// A flow collection may span lines, but each continuation has to be
/// indented more than the block context it sits in; at column zero the
/// content would be read as a new block node instead (yaml-test-suite
/// 9C9N).
fn check_flow_continuation_indent(
    node: &SyntaxNode,
    scope: Scope,
    violations: &mut Vec<Violation>,
) {
    if !starts_a_flow_collection(node) {
        return;
    }
    // Only the outermost flow collection needs checking; a nested one
    // shares its lines.
    if scope.in_flow {
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
    let Some(required) = scope.continuation_indent else {
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
                indent = Some(space_indent(t.text()));
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

/// Check that every alias refers to an anchor earlier in the same document
///
/// Anchors are scoped to a document and an alias can only refer to an
/// anchor that precedes it, so `a: *x` and `a: *x\nb: &x 1` are errors.
fn check_undefined_aliases(document: &SyntaxNode, violations: &mut Vec<Violation>) {
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
fn check_anchor_and_alias(node: &SyntaxNode, violations: &mut Vec<Violation>) {
    use crate::SyntaxKind;

    let Some(anchor) = child_token(node, SyntaxKind::ANCHOR) else {
        return;
    };
    let is_alias = |n: &SyntaxNode| n.kind() == SyntaxKind::ALIAS;
    let has_alias = node.children().any(|child| {
        is_alias(&child)
            || (child.kind() == SyntaxKind::TAGGED_NODE && child.children().any(|c| is_alias(&c)))
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
fn check_tag_on_alias(node: &SyntaxNode, violations: &mut Vec<Violation>) {
    if !node
        .children()
        .any(|child| child.kind() == crate::SyntaxKind::ALIAS)
    {
        return;
    }
    let Some(tag) = child_token(node, crate::SyntaxKind::TAG) else {
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
    token: &rowan::SyntaxToken<crate::Lang>,
    violations: &mut Vec<Violation>,
) {
    // What matters is the token before the comment in document order,
    // not the previous sibling: a comment opening a VALUE (`hr: # c`)
    // has no sibling before it, and the separating whitespace sits one
    // level up. Walking siblings alone reported 5 files the test suite
    // marks valid.
    // An empty node (`? |\n# c`) has a zero-width token, which separates
    // nothing either way.
    let Some(prev) = std::iter::successors(token.prev_token(), |t| t.prev_token())
        .find(|t| !t.text().is_empty())
    else {
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

/// Check that TAG tokens don't contain invalid characters
///
/// YAML spec restricts which characters can appear in tags.
/// Tags cannot contain: {, }, [, ], or comma (,)
fn check_tag_characters(token: &rowan::SyntaxToken<crate::Lang>, violations: &mut Vec<Violation>) {
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
fn check_block_mapping_after_properties(node: &SyntaxNode, violations: &mut Vec<Violation>) {
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
fn check_collection_tag_content(node: &SyntaxNode, violations: &mut Vec<Violation>) {
    let Some(tagged) = crate::yaml::TaggedNode::cast(node.clone()) else {
        return;
    };
    if node
        .children()
        .any(|child| child.kind() == crate::SyntaxKind::ALIAS)
    {
        return;
    }
    let message = match tagged.normalized_tag().as_deref() {
        Some(tag::SET) if tagged.as_set().is_none() => "!!set content is not a mapping",
        Some(tag::OMAP) if tagged.as_sequence().is_none() => "!!omap content is not a sequence",
        Some(tag::PAIRS) if tagged.as_sequence().is_none() => "!!pairs content is not a sequence",
        _ => return,
    };
    let Some(tag) = child_token(node, crate::SyntaxKind::TAG) else {
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
fn check_set_values(node: &SyntaxNode, violations: &mut Vec<Violation>) {
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
        let is_null =
            crate::as_yaml::scalar_canonical(value.syntax()).is_some_and(|(t, _)| t == tag::NULL);
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
/// untagged or tagged `!!map`, with a single entry once its merge keys
/// are resolved.
///
/// Like other content that does not suit a YAML 1.1 type, a bad item is
/// only a warning. An alias item, or a merge key that cannot be resolved,
/// is left to the conversion to `YamlValue`, which checks the node it
/// refers to.
fn check_pairs_items(node: &SyntaxNode, violations: &mut Vec<Violation>) {
    use crate::as_yaml::YamlNode;

    let Some(tagged) = crate::yaml::TaggedNode::cast(node.clone()) else {
        return;
    };
    if !matches!(
        tagged.normalized_tag().as_deref(),
        Some(tag::OMAP | tag::PAIRS)
    ) {
        return;
    }
    let Some(written_tag) = tagged.tag() else {
        return;
    };
    let Some(sequence) = tagged.as_sequence() else {
        return;
    };
    let registry = std::cell::OnceCell::new();
    for item in sequence.values() {
        if matches!(item, YamlNode::Alias(_)) {
            continue;
        }
        if let Some(mapping) = pairs_item_mapping(&item) {
            match pairs_item_keys(&mapping, &registry) {
                Some(keys) if keys.len() != 1 => {}
                _ => continue,
            }
        }
        violations.push(Violation::warning_at(
            Rule::InvalidTag,
            item.syntax().text_range(),
            format!(
                "{written_tag} item {:?} is not a mapping with a single entry",
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
/// [`Self::check_duplicate_keys`], after merge keys are resolved.
fn check_omap_keys(
    node: &SyntaxNode,
    identities: &mut SharedIdentities,
    violations: &mut Vec<Violation>,
) {
    let Some(tagged) = crate::yaml::TaggedNode::cast(node.clone()) else {
        return;
    };
    if tagged.normalized_tag().as_deref() != Some(tag::OMAP) {
        return;
    }
    let Some(sequence) = tagged.as_sequence() else {
        return;
    };
    let registry = std::cell::OnceCell::new();
    let keys = sequence.values().filter_map(|item| {
        let mapping = pairs_item_mapping(&item)?;
        let key = pairs_item_keys(&mapping, &registry)?.into_iter().next()?;
        Some((key, item.syntax().text_range()))
    });
    let mut seen: Vec<(usize, SyntaxNode)> = Vec::new();
    for (key, range) in keys {
        let Some(identity) = key_identity(&key, identities) else {
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

/// The mapping that `item`, an item of a `!!omap` or `!!pairs`, is, if
/// it is one, untagged or tagged `!!map`.
fn pairs_item_mapping(item: &crate::as_yaml::YamlNode) -> Option<crate::yaml::Mapping> {
    use crate::as_yaml::YamlNode;

    match item {
        YamlNode::Mapping(mapping) => Some(mapping.clone()),
        YamlNode::TaggedNode(tagged) if tagged.normalized_tag().as_deref() == Some(tag::MAP) => {
            tagged.as_mapping()
        }
        _ => None,
    }
}

/// The KEY nodes of `mapping` with its merge keys resolved, or `None` if
/// one cannot be resolved. `registry` is built for the document the first
/// time a merge key needs it.
fn pairs_item_keys(
    mapping: &crate::yaml::Mapping,
    registry: &std::cell::OnceCell<AnchorRegistry>,
) -> Option<Vec<SyntaxNode>> {
    let key_node = |key: crate::as_yaml::YamlNode| {
        key.syntax()
            .ancestors()
            .find(|n| n.kind() == crate::SyntaxKind::KEY)
    };
    if !mapping.keys().any(|key| is_merge_key(&key)) {
        return Some(mapping.keys().filter_map(key_node).collect());
    }
    let registry =
        registry.get_or_init(|| AnchorRegistry::from_tree(&document_scope(mapping.syntax())));
    let merged = MergedMapping::build(mapping.clone(), registry).ok()?;
    Some(merged.keys().filter_map(key_node).collect())
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
    entry_node: &SyntaxNode,
    columns: &ColumnIndex,
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
    if !in_flow {
        let multiline_key = entry_node
            .children()
            .filter(|c| c.kind() == crate::SyntaxKind::KEY)
            .find(|key| columns.has_line_break(key.text_range()));
        if let Some(key) = multiline_key {
            // Only report once per entry
            violations.push(Violation::error_at(
                Rule::Other,
                key.text_range(),
                "Implicit key cannot span multiple lines",
            ));
            return;
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
    if line_break_before_colon(entry_node) {
        violations.push(Violation::error_at(
            Rule::Other,
            entry_node.text_range(),
            "Implicit key cannot span multiple lines",
        ));
    }
}

/// Whether a line break separates the key of mapping entry `entry` from
/// its `:`.
fn line_break_before_colon(entry: &SyntaxNode) -> bool {
    entry
        .children_with_tokens()
        .skip_while(|el| el.kind() != crate::SyntaxKind::KEY)
        .filter_map(|el| el.into_token())
        .find(|t| {
            matches!(
                t.kind(),
                crate::SyntaxKind::NEWLINE | crate::SyntaxKind::COLON
            )
        })
        .is_some_and(|t| t.kind() == crate::SyntaxKind::NEWLINE)
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
    entry_node: &SyntaxNode,
    in_flow: bool,
    violations: &mut Vec<Violation>,
) {
    use crate::SyntaxKind;

    let key_node = entry_node
        .children()
        .filter(|c| c.kind() == SyntaxKind::KEY)
        .last();
    let value_node = entry_node
        .children()
        .filter(|c| c.kind() == SyntaxKind::VALUE)
        .last();

    // If there's no value, nothing to check
    let Some(value) = value_node else { return };

    // An explicit key's value may be a compact collection on the `:` line
    // (`? k\n: - one\n  - two\n`), which YAML 1.2 allows and PyYAML
    // accepts. Only a plain key's `:` requires the line break, so this
    // rule does not apply to an entry introduced by `?`.
    if has_child_token(entry_node, |k| k == SyntaxKind::QUESTION) {
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
    if starts_a_flow_collection(&collection) || in_flow {
        return;
    }

    if !line_break_before_collection(key_node.as_ref(), &value, &collection) {
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

/// Whether a line break separates the `:` after `key` from `collection`,
/// the block collection held in the entry's `value`.
fn line_break_before_collection(
    key: Option<&SyntaxNode>,
    value: &SyntaxNode,
    collection: &SyntaxNode,
) -> bool {
    use crate::SyntaxKind;
    // The parser puts the line break inside the VALUE when the collection
    // is indentless (`k:\n- a\n`), where the entries sit at the key's
    // own column. That is valid YAML, so look there first; the scan
    // below only sees tokens at the MAPPING_ENTRY level.
    let in_value = value
        .children_with_tokens()
        .take_while(|el| el.as_node() != Some(collection))
        .any(|el| el.kind() == SyntaxKind::NEWLINE);
    in_value
        || key.is_some_and(|key| {
            key.siblings_with_tokens(rowan::Direction::Next)
                .filter_map(|el| el.into_token())
                .skip_while(|t| t.kind() != SyntaxKind::COLON)
                .any(|t| t.kind() == SyntaxKind::NEWLINE)
        })
}

/// Check sequence items have consistent indentation (ZVH3)
fn check_sequence_indentation(
    node: &SyntaxNode,
    columns: &ColumnIndex,
    violations: &mut Vec<Violation>,
) {
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
                rowan::NodeOrToken::Node(n) if n.kind() == crate::SyntaxKind::SEQUENCE_ENTRY => {
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
    let first_col = columns.column(first);
    if let Some(misplaced) = rest.iter().find(|t| columns.column(t) != first_col) {
        violations.push(Violation::error_at(
            Rule::InvalidIndentation,
            misplaced.text_range(),
            "Inconsistent sequence item indentation",
        ));
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
fn check_duplicate_keys(
    node: &SyntaxNode,
    identities: &mut SharedIdentities,
    violations: &mut Vec<Violation>,
) {
    use crate::SyntaxKind;
    use std::collections::HashMap;

    let keys: Vec<(SyntaxNode, rowan::TextRange)> = node
        .children()
        .filter(|child| child.kind() == SyntaxKind::MAPPING_ENTRY)
        .filter_map(|child| {
            let key = child.children().find(|n| n.kind() == SyntaxKind::KEY)?;
            Some((key, child.text_range()))
        })
        .collect();
    // Only keys of the same kind can be equal, so a key with no other of
    // its kind is left alone.
    let kinds: Vec<Option<SyntaxKind>> = keys.iter().map(|(key, _)| identity_kind(key)).collect();
    let mut kind_counts: HashMap<SyntaxKind, usize> = HashMap::new();
    for kind in kinds.iter().flatten() {
        *kind_counts.entry(*kind).or_default() += 1;
    }

    let format_key = |key: &SyntaxNode| {
        let text = key.text().to_string();
        match text.trim() {
            "" => "\"\"".to_string(),
            s => format!("{s:?}"),
        }
    };

    // Key identity -> index of the first key with it
    let mut seen: HashMap<usize, usize> = HashMap::new();
    for (i, (key, range)) in keys.iter().enumerate() {
        if !kinds[i].is_some_and(|kind| kind_counts[&kind] > 1) {
            continue;
        }
        let Some(identity) = key_identity(key, identities) else {
            continue;
        };
        match seen.entry(identity) {
            std::collections::hash_map::Entry::Occupied(first) => {
                violations.push(Violation::error_at(
                    Rule::DuplicateKeys,
                    *range,
                    format!(
                        "Duplicate key: {} (semantically equal to {})",
                        format_key(key),
                        format_key(&keys[*first.get()].0)
                    ),
                ));
            }
            std::collections::hash_map::Entry::Vacant(slot) => {
                slot.insert(i);
            }
        }
    }
}

/// The identity of mapping key `key`, or `None` if it is or holds an alias.
///
/// Such a key is compared with nothing: the YAML test suite has
/// `{&a [a]: 1, *a : 2}` as valid (X38W). The identities of the nodes in
/// `key` are kept in `identities` for the keys among them, which would
/// otherwise work them out again on every level of nested collection keys.
fn key_identity(key: &SyntaxNode, identities: &mut SharedIdentities) -> Option<usize> {
    identities.identity(key).ok().flatten()
}

/// The kind of the node that gives `node`, a key or a node in one, its
/// identity, or `None` if it has none: keys of different kinds are never
/// equal.
fn identity_kind(node: &SyntaxNode) -> Option<crate::SyntaxKind> {
    use crate::SyntaxKind;
    match node.kind() {
        SyntaxKind::KEY => identity_kind(&node.first_child()?),
        // A tagged node without content is an empty scalar.
        SyntaxKind::TAGGED_NODE => node
            .first_child()
            .map_or(Some(SyntaxKind::SCALAR), |content| identity_kind(&content)),
        kind => Some(kind),
    }
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
    child_token(node, crate::SyntaxKind::INDENT).filter(|t| t.text().starts_with('\t'))
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
                    .split(['\n', '\r'])
                    .skip(1)
                    .any(|line| line.starts_with('\t') && !line.trim().is_empty())
        })
}

/// The first token of `node`, a block collection, its entry or a value,
/// that uses a tab as indentation.
fn block_tab_indent(node: &SyntaxNode) -> Option<crate::nodes::SyntaxToken> {
    let is_flow = starts_a_flow_collection(node);
    let tab = node
        .children_with_tokens()
        .filter_map(|c| c.into_token())
        .find(|t| tab_before_block_collection(t) || tab_indent(t, is_flow))?;
    // A flow collection at the stream's own level is not
    // indentation-sensitive, so a tab anywhere inside one is separation
    // whitespace (6CA3's `\t[\n\t]`). Nested in a block collection it
    // still has to clear that block's column, as Y79Y/003's `- [\n\tfoo,`
    // does not. Block collections cannot sit inside flow ones, so the
    // outermost collection tells the two apart. Only a candidate tab pays
    // for the walk up, so deep nesting without tabs stays linear.
    let outermost = node
        .ancestors()
        .filter(|a| {
            matches!(
                a.kind(),
                crate::SyntaxKind::MAPPING | crate::SyntaxKind::SEQUENCE
            )
        })
        .last();
    if outermost.is_some_and(|c| starts_a_flow_collection(&c)) {
        return None;
    }
    Some(tab)
}

/// Whether `token` is white space holding a tab that separates an indicator
/// from a block collection.
///
/// A tab may separate an indicator from a scalar (`- \tx`) or a flow
/// collection (`seq:\t[]`), but a block collection after it needs
/// `s-indent`, which forbids tabs: `-\t-` and `?\t-` are errors the test
/// suite expects (Y79Y), while `-\t-1` is the scalar `-1`. The tab has to
/// separate the indicator from the node on the same line; trailing
/// whitespace before a line break (DC7X's `seq:\t`) or a comment
/// (`a:\t#`) indents nothing.
fn tab_before_block_collection(token: &crate::nodes::SyntaxToken) -> bool {
    use crate::SyntaxKind;
    if token.kind() != SyntaxKind::WHITESPACE || !token.text().contains('\t') {
        return false;
    }
    let Some(next) = token.next_sibling_or_token().and_then(|n| n.into_node()) else {
        return false;
    };
    let opens_a_block = |n: &SyntaxNode| {
        matches!(n.kind(), SyntaxKind::MAPPING | SyntaxKind::SEQUENCE)
            && !starts_a_flow_collection(n)
    };
    let collection = if opens_a_block(&next) {
        next
    } else if matches!(next.kind(), SyntaxKind::VALUE | SyntaxKind::KEY) {
        match next.children().find(opens_a_block) {
            Some(c) => c,
            None => return false,
        }
    } else {
        return false;
    };
    let first_entry = collection
        .children()
        .find(|c| {
            matches!(
                c.kind(),
                SyntaxKind::MAPPING_ENTRY | SyntaxKind::SEQUENCE_ENTRY
            )
        })
        .map_or(collection.text_range().start(), |e| e.text_range().start());
    !std::iter::successors(token.next_token(), |t| t.next_token())
        .take_while(|t| t.text_range().start() < first_entry)
        .any(|t| t.kind() == SyntaxKind::NEWLINE)
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
    // After leading spaces, or at the stream's own level where `s-indent`
    // is empty, a tab is separation rather than indentation (DK95/00's
    // `foo:\n \tbar`, 6HB6, Q5MG), unless a block collection entry
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
    let at_stream_level = token
        .parent()
        .is_some_and(|p| p.kind() == crate::SyntaxKind::DOCUMENT);
    if (token.text().starts_with(' ') || at_stream_level) && !starts_block_entry {
        return false;
    }
    token.text().contains('\t')
}

/// Check that an explicit key indicator is followed by white space or a
/// line break, as `c-mapping-key` needs. The lexer reads `{?, a}` as an
/// explicit key all the same, as nothing else could start there.
fn check_explicit_key_separation(node: &SyntaxNode, violations: &mut Vec<Violation>) {
    use crate::SyntaxKind;

    for question in node
        .descendants_with_tokens()
        .filter_map(|c| c.into_token())
        .filter(|t| t.kind() == SyntaxKind::QUESTION)
    {
        let separated = std::iter::successors(question.next_token(), |t| t.next_token())
            .find(|t| !t.text().is_empty())
            .map_or(true, |t| {
                matches!(t.kind(), SyntaxKind::WHITESPACE | SyntaxKind::NEWLINE)
            });
        if !separated {
            violations.push(Violation::error_at(
                Rule::Other,
                question.text_range(),
                "Explicit key indicator '?' must be followed by white space".to_string(),
            ));
        }
    }
}

/// Check that the `:` of a block explicit entry that opens its line is at
/// the column of the entry's `?`, as `l-block-map-explicit-value` needs.
/// The parser takes one indented past it as the entry's value all the same
/// (`? a\n : b\n`).
fn check_explicit_value_column(
    entry: &SyntaxNode,
    columns: &ColumnIndex,
    in_flow: bool,
    violations: &mut Vec<Violation>,
) {
    use crate::SyntaxKind;

    if in_flow {
        return;
    }
    let (Some(question), Some(colon)) = (
        child_token(entry, SyntaxKind::QUESTION),
        child_token(entry, SyntaxKind::COLON),
    ) else {
        return;
    };
    let opens_line = std::iter::successors(colon.prev_token(), |t| t.prev_token())
        .find(|t| {
            !t.text().is_empty() && !matches!(t.kind(), SyntaxKind::INDENT | SyntaxKind::WHITESPACE)
        })
        .map_or(true, |t| t.kind() == SyntaxKind::NEWLINE);
    if opens_line && columns.column(&colon) != columns.column(&question) {
        violations.push(Violation::error_at(
            Rule::Other,
            colon.text_range(),
            "Explicit value indicator ':' must be at the column of its '?'".to_string(),
        ));
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rowan::ast::AstNode;
    use std::str::FromStr;

    #[test]
    fn test_deep_nesting_fits_a_small_stack() {
        // The walk used to recurse once per node, which overflowed the stack
        // under the fuzzer's address sanitizer on 200 nested `[`.
        let text = format!("{}{}", "[{".repeat(127), "}]".repeat(127));
        let green = crate::YamlFile::from_str(&text)
            .unwrap()
            .syntax()
            .green()
            .into_owned();
        // Dropping the last reference to a tree recurses through it, so
        // keep one out here.
        let tree = green.clone();
        let violations = std::thread::Builder::new()
            .stack_size(SMALL_STACK)
            .spawn(move || {
                Validator::new()
                    .validate_syntax(&SyntaxNode::new_root_mut(tree))
                    .len()
            })
            .unwrap()
            .join()
            .unwrap();
        assert_eq!(violations, 0);
        drop(green);
    }

    /// A stack small enough to overflow when walking a tree recurses
    /// through all of its levels.
    const SMALL_STACK: usize = 64 * 1024;
}
