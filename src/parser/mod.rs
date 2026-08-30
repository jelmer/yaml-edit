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
            self.builder.start_node(SyntaxKind::ERROR.into());

            // Consume tokens until we hit EOF or a document/directive marker
            while self.current().is_some()
                && self.current() != Some(SyntaxKind::EOF)
                && self.current() != Some(SyntaxKind::DOC_START)
                && self.current() != Some(SyntaxKind::DIRECTIVE)
            {
                self.bump();
            }

            self.builder.finish_node();

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
            self.parse_value();
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
                self.bump(); // consume and emit anchor token to CST
                self.skip_whitespace();
                self.parse_value_with_base_indent(base_indent);
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
                    self.parse_tagged_value();
                }
            }
            Some(SyntaxKind::MERGE_KEY) => {
                // Merge key is always a mapping
                self.parse_mapping_with_base_indent(base_indent);
            }
            Some(SyntaxKind::QUESTION) => {
                // Explicit key indicator - parse complex mapping
                self.parse_explicit_key_mapping();
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
                    // No indented content means empty/null value - create empty scalar
                    self.builder.start_node(SyntaxKind::SCALAR.into());
                    self.builder.finish_node();
                }
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
                            if let Some((_, text)) = self.tokens.last() {
                                if text.len() < base_indent {
                                    // Dedent detected - don't consume the indent token
                                    return true;
                                }
                                if base_indent == 0 && !text.is_empty() {
                                    // At root level, any indentation means content doesn't belong at root
                                    return true;
                                }
                            }
                            self.bump(); // consume indent if at appropriate level
                        }
                        Some(SyntaxKind::COMMENT) => {
                            // COMMENT at column 0 (no INDENT after NEWLINE)
                            if base_indent > 0 {
                                // This is dedented - don't consume it
                                return true;
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
                    if let Some((_, text)) = self.tokens.last() {
                        if text.len() < base_indent {
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
