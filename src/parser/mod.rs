//! YAML parser: consumes lex tokens and builds a rowan CST.
//!
//! The public entry point is [`parse`], which the crate re-exports as
//! `crate::yaml::parse`. Topic-focused parsing logic lives in the
//! sub-modules:
//!
//! - [`scalars`] -- plain / quoted / block scalars, aliases, tagged collections
//! - [`flow`] -- `{...}` mappings and `[...]` sequences
//! - [`block`] -- block-style mappings and sequences, explicit-key
//!   mappings, complex-key mappings, key-value pair dispatch
//!
//! This file owns the driver (`parse`, `parse_document`,
//! `parse_value_with_base_indent`) and the shared navigation / error /
//! token-access helpers that every sub-module reaches for via `pub(super)`.

mod block;
mod flow;
mod scalars;

use scalars::is_plain_scalar_kind;

#[cfg(test)]
pub(crate) use flow::has_implicit_mapping_pattern;

use crate::{
    error_recovery::{ErrorBuilder, ErrorRecoveryContext, ParseContext, RecoveryStrategy},
    lex::{lex, SyntaxKind},
    yaml::ParsedYaml,
    ParseErrorKind, PositionedParseError,
};
use rowan::GreenNodeBuilder;

/// Maximum nesting depth for collections and stacked node properties.
/// Beyond this, the parser returns an error rather than risking stack
/// overflow or unbounded RSS growth from pathological input like
/// `{{{{...}}}}` or a long `a:\n  a:\n  ...` chain.
pub(super) const MAX_FLOW_DEPTH: usize = 256;

/// Internal parser state
///
/// Fields are `pub(super)` so the topic sub-modules
/// ([`scalars`], [`flow`], [`block`]) can add `impl Parser` blocks
/// without going through accessor methods for every field.
pub(super) struct Parser {
    pub(super) tokens: Vec<(SyntaxKind, String)>,
    pub(super) current_token_index: usize,
    pub(super) builder: GreenNodeBuilder<'static>,
    pub(super) errors: Vec<String>,
    pub(super) positioned_errors: Vec<PositionedParseError>,
    pub(super) in_flow_context: bool,
    /// Error recovery context for better error messages
    pub(super) error_context: ErrorRecoveryContext,
    /// Track if we're parsing a value (to prevent nested implicit mappings)
    pub(super) in_value_context: bool,
    /// Track the current line's indentation level for plain scalar continuation
    pub(super) current_line_indent: usize,
    /// Whether an equally indented line continues a plain scalar here.
    ///
    /// A continuation normally has to be indented past the scalar's own line,
    /// so it cannot be read as the next entry of the enclosing mapping. That
    /// does not apply to the document's own node, which has no enclosing
    /// collection (`ab\ncd` is one scalar), nor inside a sequence entry,
    /// where a continuation only has to clear the sequence's own column
    /// (`- x\n y`).
    pub(super) equal_indent_continues_scalar: bool,
    /// Column a plain scalar's continuation must clear, when that is not the
    /// scalar's own line.
    ///
    /// A lone tag annotates a block node that starts on a later, more
    /// indented line, and the body is parsed with that line's column as its
    /// base. The scalar itself is still a node of the enclosing collection
    /// though, so its continuation only has to clear *that* column: `!\n  a\n b\n`
    /// is the tagged scalar `a b`, exactly as the untagged `x\n  a\n b\n`
    /// is `x a b`.
    pub(super) scalar_continuation_floor: Option<usize>,
    /// Whether the node being parsed sits in a block mapping's value
    /// position, where a block body has to clear the key's column.
    ///
    /// A tag hands this to the anchor that may follow it: `k: !!str &a` runs
    /// the tag arm, which declines to adopt a dedented sibling, and then
    /// falls through to the anchor arm. Without the flag that arm asks the
    /// same question as a document-level anchor and answers it differently,
    /// adopting the sibling the tag arm just refused.
    pub(super) annotation_in_value_position: bool,
    /// Column of the `-` of the block sequence entry being parsed, if any.
    ///
    /// A `-` on a later line opens the next entry when it sits at this
    /// column, and is plain-scalar content when it is indented past it:
    /// `a:\n- x\n  - y\n` is the single item `x - y`. Comparing against the
    /// scalar's own line cannot tell those apart, since an explicit key
    /// (`? - a\n  - b\n`) puts its entries deeper than the scalar too.
    pub(super) sequence_entry_column: Option<usize>,
    /// Whether the node being parsed is the document's own, with no
    /// enclosing collection.
    ///
    /// A block scalar there may hold body lines at column 0 (`|\nx\ny\n` is
    /// the scalar `x\ny`). Nested under a key it may not: `a: |\nb: 1\n` has
    /// an empty scalar and keeps `b` as a sibling.
    pub(super) node_is_document_root: bool,
    /// Current depth of nested flow collections ([...] / {...}).
    pub(super) flow_depth: usize,
    /// Depth of `parse_value_with_base_indent` recursion (block and flow).
    pub(super) nesting_depth: usize,
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
            error_context: ErrorRecoveryContext::new(text.to_string()),
            in_value_context: false,
            equal_indent_continues_scalar: false,
            scalar_continuation_floor: None,
            annotation_in_value_position: false,
            sequence_entry_column: None,
            node_is_document_root: false,
            current_line_indent: 0,
            flow_depth: 0,
            nesting_depth: 0,
        }
    }

    fn parse(mut self) -> ParsedYaml {
        self.builder.start_node(SyntaxKind::ROOT.into());

        // Handle BOM (Byte Order Mark) at the start of file
        // BOM is allowed per YAML spec and should be processed transparently
        if self.current() == Some(SyntaxKind::BOM) {
            self.bump(); // Add BOM to tree but continue parsing
        }

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

        // Consume any remaining tokens as ERROR nodes
        // A lenient parser should consume all input, not leave it unparsed
        while self.current().is_some() && self.current() != Some(SyntaxKind::EOF) {
            // A `...` ends the document it closes and belongs to the tree,
            // not to an ERROR node; a `---` or `%YAML` starts the next one.
            // This loop drives multi-document parsing as well as the sweep,
            // so it is entered with nothing to sweep -- make the ERROR node
            // only when there are stray tokens to hold.
            let has_stray = self.current().is_some()
                && self.current() != Some(SyntaxKind::EOF)
                && self.current() != Some(SyntaxKind::DOC_START)
                && self.current() != Some(SyntaxKind::DIRECTIVE)
                && self.current() != Some(SyntaxKind::DOC_END);
            if has_stray {
                self.builder.start_node(SyntaxKind::ERROR.into());
                while self.current().is_some()
                    && self.current() != Some(SyntaxKind::EOF)
                    && self.current() != Some(SyntaxKind::DOC_START)
                    && self.current() != Some(SyntaxKind::DIRECTIVE)
                    && self.current() != Some(SyntaxKind::DOC_END)
                {
                    self.bump();
                }
                self.builder.finish_node();
            }

            // A `...` closes the document before it. What follows may be a
            // fresh document with no `---` of its own (`a\n...\nb: 1\n` is
            // two documents), so parse one rather than sweeping it away.
            if self.current() == Some(SyntaxKind::DOC_END) {
                self.bump();
                self.skip_ws_and_newlines();
                if self.current().is_some()
                    && self.current() != Some(SyntaxKind::EOF)
                    && self.current() != Some(SyntaxKind::DOC_START)
                    && self.current() != Some(SyntaxKind::DIRECTIVE)
                    && self.current() != Some(SyntaxKind::DOC_END)
                {
                    self.parse_document();
                    self.skip_ws_and_newlines();
                }
                continue;
            }

            // If we hit a document/directive marker, try to parse it
            if self.current() == Some(SyntaxKind::DOC_START)
                || self.current() == Some(SyntaxKind::DIRECTIVE)
            {
                // Parse any directives
                while self.current() == Some(SyntaxKind::DIRECTIVE) {
                    self.parse_directive();
                    self.skip_ws_and_newlines();
                }

                // Parse document if present
                if self.current().is_some() && self.current() != Some(SyntaxKind::EOF) {
                    self.parse_document();
                    self.skip_ws_and_newlines();
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
            // A plain scalar that *is* the document has no enclosing
            // collection, so its continuation lines need clear no column:
            // `" a\nb\n"` is the single scalar `a b` even though the second
            // line is less indented than the first, as both saphyr and
            // PyYAML read it.
            //
            // Only when the root really is such a scalar. Relaxing the floor
            // for every root value would also fold a stray line after a
            // finished collection (`- a\n- b\ninvalid\n`, YAML test suite
            // TD5N), which is an error rather than a continuation.
            let root_is_plain_scalar = self
                .current()
                .is_some_and(crate::parser::scalars::is_plain_scalar_kind)
                && !self.is_mapping_key();
            let outer_floor = self.scalar_continuation_floor;
            let outer_root = self.node_is_document_root;
            self.equal_indent_continues_scalar = true;
            self.node_is_document_root = true;
            if root_is_plain_scalar {
                self.scalar_continuation_floor = Some(0);
            }
            self.parse_value();
            self.node_is_document_root = outer_root;
            self.scalar_continuation_floor = outer_floor;
            self.equal_indent_continues_scalar = false;
        }

        // Handle document end marker
        if self.current() == Some(SyntaxKind::DOC_END) {
            self.bump();

            // Check for content after document end marker (spec violation)
            self.skip_whitespace();
            if self.current().is_some()
                && self.current() != Some(SyntaxKind::NEWLINE)
                && self.current() != Some(SyntaxKind::EOF)
                && self.current() != Some(SyntaxKind::DOC_START)
                && self.current() != Some(SyntaxKind::DIRECTIVE)
            {
                // Found content after DOC_END - wrap it in an ERROR node
                self.builder.start_node(SyntaxKind::ERROR.into());
                while self.current().is_some()
                    && self.current() != Some(SyntaxKind::NEWLINE)
                    && self.current() != Some(SyntaxKind::EOF)
                    && self.current() != Some(SyntaxKind::DOC_START)
                    && self.current() != Some(SyntaxKind::DIRECTIVE)
                {
                    self.bump();
                }
                self.builder.finish_node();
            }
        }

        self.builder.finish_node();
    }

    pub(super) fn parse_value(&mut self) {
        self.parse_value_with_base_indent(0);
    }

    pub(super) fn parse_value_with_base_indent(&mut self, base_indent: usize) {
        if self.nesting_depth >= MAX_FLOW_DEPTH {
            self.add_error(
                format!("Collection nested too deeply (limit {MAX_FLOW_DEPTH})"),
                ParseErrorKind::Other,
            );
            if self.current().is_some() {
                self.bump();
            }
            return;
        }
        self.nesting_depth += 1;
        match self.current() {
            Some(SyntaxKind::COMMENT) => {
                // Preserve the comment and continue parsing the actual value
                self.bump(); // consume and preserve the comment
                self.skip_ws_and_newlines(); // skip any whitespace/newlines after comment
                                             // Now parse the actual value
                self.parse_value_with_base_indent(base_indent);
            }
            Some(SyntaxKind::DASH) if !self.in_flow_context => {
                self.parse_sequence_with_base_indent(base_indent)
            }
            Some(SyntaxKind::ANCHOR) => {
                // An anchor alone on its line annotates a block node starting
                // on a later line, just as a lone tag does. Ask before
                // consuming the ANCHOR: tagged_block_node_indent reads from
                // the current token and steps over a leading anchor itself.
                //
                // Only reached outside a mapping value; parse_mapping_value
                // keeps a value-position anchor on the implicit-null path, so
                // a dedented sibling entry stays a sibling.
                let body_indent =
                    self.tagged_block_node_indent(base_indent, self.annotation_in_value_position);
                self.bump(); // consume and emit anchor token to CST
                self.skip_whitespace();
                match body_indent {
                    Some(indent) => {
                        self.skip_ws_and_newlines();
                        if self.current() == Some(SyntaxKind::DASH) {
                            self.parse_sequence_with_base_indent(indent);
                        } else {
                            // As in the tagged case, a plain scalar body is
                            // still a node of the collection the anchor sits
                            // in, so its continuation only has to clear the
                            // anchor's own column.
                            let outer_floor = self.scalar_continuation_floor;
                            self.scalar_continuation_floor = Some(base_indent);
                            self.parse_value_with_base_indent(indent);
                            self.scalar_continuation_floor = outer_floor;
                        }
                    }
                    None => self.parse_value_with_base_indent(base_indent),
                }
            }
            Some(SyntaxKind::REFERENCE) => self.parse_alias(),
            Some(SyntaxKind::TAG) => {
                // `!!str a: b` at document / block level -- the tag
                // annotates the KEY of an implicit mapping, not the
                // whole document. Detect that shape (TAG [WS] scalar
                // [WS] COLON) and dispatch to mapping parsing so
                // parse_mapping_key_value_pair can consume the tag as
                // part of the KEY. Otherwise fall back to the default
                // tag-wraps-following-value behaviour.
                if !self.in_flow_context && !self.in_value_context && self.is_mapping_key() {
                    self.parse_mapping_with_base_indent(base_indent);
                } else {
                    self.parse_tagged_value(base_indent);
                }
            }
            Some(SyntaxKind::MERGE_KEY) => {
                if self.in_flow_context {
                    self.builder.start_node(SyntaxKind::SCALAR.into());
                    self.bump();
                    self.builder.finish_node();
                } else {
                    self.parse_mapping_with_base_indent(base_indent);
                }
            }
            Some(SyntaxKind::QUESTION) => {
                // Explicit key indicator - parse complex mapping
                self.parse_explicit_key_mapping(base_indent);
            }
            Some(SyntaxKind::PIPE) => self.parse_literal_block_scalar(),
            Some(SyntaxKind::GREATER) => self.parse_folded_block_scalar(),
            Some(kind) if is_plain_scalar_kind(kind) => {
                // In flow context, always parse as scalar
                // In block context, check if it's a mapping key
                // But not if we're already in a value context (prevents implicit nested mappings)
                if !self.in_flow_context && !self.in_value_context && self.is_mapping_key() {
                    self.parse_mapping_with_base_indent(base_indent);
                } else {
                    self.parse_scalar();
                }
            }
            Some(SyntaxKind::LEFT_BRACKET) => {
                // Check if this is a complex key in a mapping
                // But not if we're already in a value context
                if !self.in_flow_context && !self.in_value_context && self.is_complex_mapping_key()
                {
                    self.parse_complex_key_mapping();
                } else {
                    self.parse_flow_sequence();
                }
            }
            Some(SyntaxKind::LEFT_BRACE) => {
                // Check if this is a complex key in a mapping
                // But not if we're already in a value context
                if !self.in_flow_context && !self.in_value_context && self.is_complex_mapping_key()
                {
                    self.parse_complex_key_mapping();
                } else {
                    self.parse_flow_mapping();
                }
            }
            Some(SyntaxKind::INDENT) => {
                // We have an indented block - consume the indent and see what follows
                self.bump(); // consume INDENT
                self.parse_value(); // parse whatever comes after the indent
            }
            Some(SyntaxKind::NEWLINE) => {
                // Check if next line has indented content
                self.bump(); // consume newline
                if self.current() == Some(SyntaxKind::INDENT) {
                    let indent_level = self.tokens.last().map_or(0, |(_, text)| text.len());
                    self.bump(); // consume indent
                    self.parse_value_with_base_indent(indent_level);
                } else {
                    // No indented content -- implicit-null value. Emit the
                    // zero-width `SCALAR { NULL "" }` shape used everywhere
                    // else so every value slot has one scalar/collection.
                    self.emit_implicit_null();
                }
            }
            // A line that starts at the colon is a mapping entry whose key
            // is empty: `: v` maps null to `v`, as the YAML test suite has
            // it (2JQS expects `=VAL :` for the key). parse_scalar consumes
            // nothing here, so without this the colon and everything after
            // it was stranded in an ERROR node with no parse error.
            Some(SyntaxKind::COLON) if !self.in_flow_context => {
                self.parse_mapping_with_base_indent(base_indent)
            }
            _ => self.parse_scalar(),
        }
        self.nesting_depth -= 1;
    }
}

impl Parser {
    fn parse_directive(&mut self) {
        self.builder.start_node(SyntaxKind::DIRECTIVE.into());

        if self.current() == Some(SyntaxKind::DIRECTIVE) {
            self.bump(); // consume the directive token
        } else {
            self.add_error("Expected directive".to_string(), ParseErrorKind::Other);
        }

        self.builder.finish_node();
    }

    pub(super) fn skip_whitespace(&mut self) {
        self.skip_tokens(&[SyntaxKind::WHITESPACE]);
    }

    pub(super) fn skip_tokens(&mut self, kinds: &[SyntaxKind]) {
        while let Some(current) = self.current() {
            if kinds.contains(&current) {
                self.bump();
            } else {
                break;
            }
        }
    }

    /// Check if the current position is dedented relative to base_indent.
    /// This is used when we encounter a token (like COMMENT) and need to check if it's dedented.
    /// Returns true if dedent detected.
    pub(super) fn is_at_dedented_position(&self, base_indent: usize) -> bool {
        // Use the tracked current_line_indent instead of searching backwards through tokens.
        // This works because current_line_indent is updated by bump() when INDENT/NEWLINE
        // tokens are consumed. After skip_whitespace_only_with_dedent_check() consumes
        // whitespace and INDENT tokens, current_line_indent contains the correct indentation
        // level for the current line.
        if base_indent == 0 {
            // At root level (base_indent=0), any indentation means content doesn't belong at root
            self.current_line_indent > 0
        } else {
            // At nested level, check if current line indentation is less than expected
            self.current_line_indent < base_indent
        }
    }

    /// Skip only WHITESPACE, NEWLINE, and INDENT tokens. Returns true if dedent detected.
    /// Does NOT emit COMMENT tokens - caller must handle those separately.
    pub(super) fn skip_whitespace_only_with_dedent_check(&mut self, base_indent: usize) -> bool {
        while self.current().is_some() {
            match self.current() {
                Some(SyntaxKind::WHITESPACE) => {
                    self.bump();
                }
                Some(SyntaxKind::NEWLINE) => {
                    self.bump();
                    // Check next token for indentation
                    match self.current() {
                        Some(SyntaxKind::INDENT) => {
                            // A blank line's indentation says nothing about the
                            // block we are in, so never read it as a dedent.
                            let blank_line = self.indent_is_blank_line();
                            if let Some((_, text)) = self.tokens.last() {
                                if !blank_line && text.len() < base_indent {
                                    // Dedent detected - don't consume the indent token
                                    return true;
                                }
                                if !blank_line && base_indent == 0 && !text.is_empty() {
                                    // At root level, any indentation means content doesn't belong at root
                                    return true;
                                }
                            }
                            self.bump(); // consume indent if at appropriate level
                        }
                        Some(SyntaxKind::COMMENT) => {
                            // COMMENT at column 0 (no INDENT after NEWLINE).
                            //
                            // The comment's own column says nothing about the
                            // block: what ends it is the next line with
                            // content. `a:\n  - x\n# c\n  - y\n` keeps both
                            // entries, as both saphyr and PyYAML read it.
                            if base_indent > 0 {
                                return match self.indent_after_comment_lines() {
                                    // Content still inside the block: the
                                    // comment belongs to it, not to whatever
                                    // encloses us.
                                    Some(indent) => indent < base_indent,
                                    // Nothing follows, so the comment is a
                                    // trailer of the enclosing scope.
                                    None => true,
                                };
                            }
                            // base_indent==0, let caller handle the comment
                            return false;
                        }
                        Some(SyntaxKind::WHITESPACE | SyntaxKind::NEWLINE) => {
                            // More whitespace, continue loop
                        }
                        None => {
                            // End of input
                            return false;
                        }
                        _ => {
                            // Content at column 0
                            if base_indent > 0 {
                                return true; // dedent detected
                            }
                            // base_indent==0, let caller handle
                            return false;
                        }
                    }
                }
                Some(SyntaxKind::INDENT) => {
                    // Standalone indent token (NEWLINE was consumed by prior entry)
                    let blank_line = self.indent_is_blank_line();
                    if let Some((_, text)) = self.tokens.last() {
                        if !blank_line && text.len() < base_indent {
                            return true; // dedent detected
                        }
                    }
                    self.bump();
                }
                _ => {
                    // Content or COMMENT found, stop skipping
                    return false;
                }
            }
        }
        false
    }

    pub(super) fn skip_ws_and_newlines(&mut self) {
        self.skip_tokens(&[
            SyntaxKind::WHITESPACE,
            SyntaxKind::NEWLINE,
            SyntaxKind::INDENT,
            SyntaxKind::COMMENT,
        ]);
    }

    /// Parse the VALUE node that follows a COLON in a block mapping entry.
    ///
    /// Three shapes end up here: content on the same line, content on the
    /// next line indented under the key, and nothing at all (the key was
    /// followed by a dedent or by EOF), which yields an implicit null.
    ///
    /// `track_indent` selects how a nested value is bounded. When set, the
    /// column of the INDENT just consumed becomes the value's base indent,
    /// so content at or left of it belongs to an enclosing collection.
    pub(super) fn parse_value_after_colon(&mut self, track_indent: bool) {
        self.builder.start_node(SyntaxKind::VALUE.into());
        if self.current().is_some() && self.current() != Some(SyntaxKind::NEWLINE) {
            self.parse_value();
        } else if self.current() == Some(SyntaxKind::NEWLINE) {
            self.bump(); // consume newline
            if self.current() == Some(SyntaxKind::INDENT) {
                self.bump(); // consume indent
                if track_indent {
                    let value_indent = self.current_line_indent;
                    self.parse_value_with_base_indent(value_indent);
                } else {
                    self.parse_value();
                }
            } else {
                // Colon-then-dedent: implicit-null value.
                self.emit_implicit_null();
            }
        } else {
            // Colon at EOF: implicit-null value.
            self.emit_implicit_null();
        }
        self.builder.finish_node();
    }

    /// Emit the zero-width `SCALAR { NULL "" }` that fills every value slot
    /// the source left empty (`key:`, `{a}`, `- `, ...). See the
    /// implicit-null section of the CST invariants in `nodes/mod.rs`: every
    /// KEY, VALUE and SEQUENCE_ENTRY holds exactly one scalar or collection,
    /// and this token renders as nothing so round-trips stay lossless.
    pub(super) fn emit_implicit_null(&mut self) {
        self.builder.start_node(SyntaxKind::SCALAR.into());
        self.builder.token(SyntaxKind::NULL.into(), "");
        self.builder.finish_node();
    }

    /// [`emit_implicit_null`](Self::emit_implicit_null) wrapped in a VALUE
    /// node, for the mapping shapes that wrap their values.
    pub(super) fn emit_implicit_null_value(&mut self) {
        self.builder.start_node(SyntaxKind::VALUE.into());
        self.emit_implicit_null();
        self.builder.finish_node();
    }

    pub(super) fn bump(&mut self) {
        if let Some((kind, text)) = self.tokens.pop() {
            // Track line indentation for plain scalar continuation
            match kind {
                SyntaxKind::INDENT => {
                    self.current_line_indent = text.len();
                }
                SyntaxKind::NEWLINE => {
                    // Reset to 0 until we see the next INDENT
                    self.current_line_indent = 0;
                }
                SyntaxKind::DASH => {
                    self.current_line_indent += text.len();
                }
                _ => {}
            }

            self.builder.token(kind.into(), &text);
            if self.current_token_index > 0 {
                self.current_token_index -= 1;
            }
            // Update error context position
            self.error_context.advance(text.len());
        }
    }

    pub(super) fn current(&self) -> Option<SyntaxKind> {
        self.tokens.last().map(|(kind, _)| *kind)
    }

    pub(super) fn current_text(&self) -> Option<&str> {
        self.tokens.last().map(|(_, text)| text.as_str())
    }

    /// Iterator over upcoming tokens starting from the next token (not current)
    pub(super) fn upcoming_tokens(&self) -> impl Iterator<Item = SyntaxKind> + '_ {
        // Since tokens are in reverse order (last is current), we need to iterate
        // from the second-to-last token backwards to the beginning
        let len = self.tokens.len();
        (0..len.saturating_sub(1))
            .rev()
            .map(move |i| self.tokens[i].0)
    }

    /// Whether the INDENT at the current position is only the leading
    /// whitespace of an otherwise blank line.
    ///
    /// A blank line's indentation is not significant in YAML, so such an
    /// INDENT must not be read as a dedent out of the enclosing block.
    /// Indentation of the next line that carries content, looking past any
    /// run of comment-only and blank lines, or `None` if there is none.
    ///
    /// A comment's own column says nothing about the block it sits in: YAML
    /// lets `a:\n  - x\n# c\n  - y\n` keep both entries, because what ends
    /// the sequence is the next content line, not the comment. The caller is
    /// positioned on the COMMENT token.
    pub(super) fn indent_after_comment_lines(&self) -> Option<usize> {
        // `tokens` is in reverse order, so walk it backwards from the current
        // token to read the lines that follow.
        let mut rest = self.tokens.iter().rev().map(|(kind, text)| (*kind, text));
        // The COMMENT we are standing on, and its line break.
        if !matches!(rest.next(), Some((SyntaxKind::COMMENT, _))) {
            return None;
        }
        // Step over the comment's own line break, then read whole lines
        // until one carries content. A line is blank when a NEWLINE follows
        // its leading whitespace directly, and a comment line ends at the
        // COMMENT token, so both leave the loop looking at a line break.
        if !matches!(rest.next(), Some((SyntaxKind::NEWLINE, _))) {
            return None;
        }
        loop {
            let mut indent = 0;
            let mut token = rest.next()?;
            if token.0 == SyntaxKind::INDENT {
                indent = token.1.len();
                token = rest.next()?;
            }
            match token.0 {
                // A blank line's indentation is not significant either; its
                // NEWLINE is the one we just read.
                SyntaxKind::NEWLINE => continue,
                // Another comment line: step over its line break too.
                SyntaxKind::COMMENT => {
                    if !matches!(rest.next(), Some((SyntaxKind::NEWLINE, _))) {
                        return None;
                    }
                    continue;
                }
                _ => return Some(indent),
            }
        }
    }

    pub(super) fn indent_is_blank_line(&self) -> bool {
        matches!(
            self.upcoming_tokens().next(),
            Some(SyntaxKind::NEWLINE) | None
        )
    }

    pub(super) fn add_error(&mut self, message: String, kind: ParseErrorKind) {
        // Create positioned error with line/column info
        let token_len = self.current_text().map_or(1, |s| s.len());
        let positioned_error = self.error_context.create_error(message, token_len, kind);

        self.errors.push(positioned_error.message.clone());
        self.positioned_errors.push(positioned_error);
    }

    /// Add an error with recovery
    pub(super) fn add_error_and_recover(
        &mut self,
        message: String,
        expected: SyntaxKind,
        kind: ParseErrorKind,
    ) {
        self.add_error(message, kind);

        // Determine recovery strategy
        let found = self.current();
        let strategy = self.error_context.suggest_recovery(expected, found);

        match strategy {
            RecoveryStrategy::SkipToken => {
                // Skip the problematic token
                if self.current().is_some() {
                    self.bump();
                }
            }
            RecoveryStrategy::SkipToEndOfLine => {
                // Skip to end of line
                while self.current().is_some() && self.current() != Some(SyntaxKind::NEWLINE) {
                    self.bump();
                }
            }
            RecoveryStrategy::InsertToken(kind) => {
                // Insert synthetic token
                self.builder.token(kind.into(), "");
            }
            RecoveryStrategy::SyncToSafePoint => {
                // Find next safe synchronization point
                let sync_point = self
                    .error_context
                    .find_sync_point(&self.tokens, self.tokens.len() - self.current_token_index);
                let tokens_to_skip = sync_point - (self.tokens.len() - self.current_token_index);
                for _ in 0..tokens_to_skip {
                    if self.current().is_some() {
                        self.bump();
                    }
                }
            }
        }
    }

    /// Create a detailed error message with helpful suggestions
    pub(super) fn create_detailed_error(
        &self,
        base_message: &str,
        expected: &str,
        found: Option<&str>,
    ) -> String {
        let mut builder = ErrorBuilder::new(base_message);
        builder = builder.expected(expected);

        if let Some(found_str) = found {
            builder = builder.found(found_str);
        } else if let Some(token) = self.current_text() {
            builder = builder.found(format!("'{token}'"));
        } else {
            builder = builder.found("end of input");
        }

        // Add context
        let context = match self.error_context.current_context() {
            ParseContext::Mapping => "in mapping",
            ParseContext::Sequence => "in sequence",
            ParseContext::FlowMapping => "in flow mapping",
            ParseContext::FlowSequence => "in flow sequence",
            ParseContext::BlockScalar => "in block scalar",
            ParseContext::QuotedString => "in quoted string",
            _ => "at document level",
        };
        builder = builder.context(context);

        // Add helpful suggestions based on the error type
        let suggestion = self.get_error_suggestion(base_message, expected, found);
        if let Some(suggestion_text) = suggestion {
            builder = builder.suggestion(suggestion_text);
        }

        builder.build()
    }

    /// Generate helpful suggestions for common errors
    fn get_error_suggestion(
        &self,
        base_message: &str,
        expected: &str,
        found: Option<&str>,
    ) -> Option<String> {
        if base_message.contains("Unterminated quoted string") {
            return Some(
                "Add closing quote or check for unescaped quotes within the string".to_string(),
            );
        }

        if base_message.contains("Missing colon") || expected.contains("':'") {
            return Some("Add ':' after the key, or check for proper indentation".to_string());
        }

        if base_message.contains("Unclosed flow sequence") {
            return Some(
                "Add ']' to close the array, or check for missing commas between elements"
                    .to_string(),
            );
        }

        if base_message.contains("Unclosed flow mapping") {
            return Some(
                "Add '}' to close the object, or check for missing commas between key-value pairs"
                    .to_string(),
            );
        }

        if let Some(found_text) = found {
            if found_text.contains('\n') {
                return Some(
                    "Unexpected newline - check indentation and YAML structure".to_string(),
                );
            }

            if found_text.contains('\t') {
                return Some(
                    "Tabs are not allowed in YAML - use spaces for indentation".to_string(),
                );
            }
        }

        None
    }
}

/// Parse YAML text
pub(crate) fn parse(text: &str) -> ParsedYaml {
    let parser = Parser::new(text);
    parser.parse()
}
