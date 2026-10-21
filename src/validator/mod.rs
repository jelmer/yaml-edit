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
mod scalars;
mod structure;

use crate::anchor_resolution::{document_scope, is_merge_key, AnchorRegistry, MergedMapping};
use crate::as_yaml::SharedIdentities;
use crate::column::ColumnIndex;
use crate::nodes::{child_token, has_child_token, in_flow_collection, starts_a_flow_collection};
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
use flow::{
    check_block_scalar_in_flow, check_flow_collection_commas, check_flow_continuation_indent,
    check_flow_pair_placement, check_tag_followed_by_comma,
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
