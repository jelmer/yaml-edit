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
mod flow;
mod keys;
mod properties;
mod scalars;
mod structure;

use crate::as_yaml::SharedIdentities;
use crate::column::ColumnIndex;
use crate::nodes::{in_flow_collection, starts_a_flow_collection};
use crate::yaml::{Document, SyntaxNode};
use rowan::ast::AstNode;
use std::fmt;

use directives::{
    check_directive_after_document, check_directive_in_content, check_directives,
    check_tag_handles_declared, check_tag_shorthand_scope,
};
use documents::{
    check_doc_start_token_content, check_document_level_anchors, check_document_marker_placement,
    check_single_content_node,
};
use flow::{
    check_block_scalar_in_flow, check_flow_collection_commas, check_flow_continuation_indent,
    check_flow_pair_placement, check_tag_followed_by_comma,
};
use keys::{
    check_duplicate_keys, check_merge_operands, check_omap_keys, check_pairs_items,
    check_set_values, DocumentRegistry,
};
use properties::{
    check_anchor_and_alias, check_collection_tag_content, check_multiple_anchors,
    check_multiple_tags, check_tag_characters, check_tag_on_alias, check_undefined_aliases,
};
use scalars::{
    check_block_scalar_content_indent, check_block_scalar_header_indent,
    check_block_scalar_indicator, check_colon_in_plain_scalar, check_escape_sequences,
    check_flow_indicator_in_plain_scalar, check_legacy_yaml_1_1, check_plain_scalar_start,
    check_quoted_string_indentation, check_trailing_content_after_quoted,
};
use structure::{
    check_block_mapping_after_properties, check_block_mapping_entries_on_same_line,
    check_collection_on_same_line_as_key, check_comment_token_whitespace,
    check_explicit_key_separation, check_explicit_value_column, check_implicit_key_multiline,
    check_mapping_entry_indentation, check_sequence_indentation, check_tab_usage,
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
        check_tag_handles_declared(&directives, &doc, &mut violations);

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
        let mut registry = DocumentRegistry::default();
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
                    self.check_node(
                        &n,
                        &columns,
                        &mut identities,
                        &mut registry,
                        scope,
                        violations,
                    );
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
        registry: &mut DocumentRegistry,
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
                check_merge_operands(node, registry, violations);
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

impl Default for Validator {
    fn default() -> Self {
        Self::new()
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
