//! YAML parser: consumes lex tokens and builds a rowan CST.
//!
//! Extracted from `crate::yaml` unchanged. The public entry point is
//! [`parse`], which the crate exposes via `crate::yaml::parse`.

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
const MAX_FLOW_DEPTH: usize = 256;

/// Internal parser state
struct Parser {
    tokens: Vec<(SyntaxKind, String)>,
    current_token_index: usize,
    builder: GreenNodeBuilder<'static>,
    errors: Vec<String>,
    positioned_errors: Vec<PositionedParseError>,
    in_flow_context: bool,
    /// Error recovery context for better error messages
    error_context: ErrorRecoveryContext,
    /// Track if we're parsing a value (to prevent nested implicit mappings)
    in_value_context: bool,
    /// Track the current line's indentation level for plain scalar continuation
    current_line_indent: usize,
    /// Current depth of nested flow collections ([...] / {...}).
    flow_depth: usize,
    /// Depth of `parse_value_with_base_indent` recursion (block and flow).
    nesting_depth: usize,
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

    fn parse_value(&mut self) {
        self.parse_value_with_base_indent(0);
    }

    fn parse_value_with_base_indent(&mut self, base_indent: usize) {
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

    fn parse_alias(&mut self) {
        // Create an alias node and consume the reference token
        // The token itself already contains the full "*alias_name" text
        self.builder.start_node(SyntaxKind::ALIAS.into());
        if self.current() == Some(SyntaxKind::REFERENCE) {
            self.bump(); // This preserves the original "*alias_name" token
        }
        self.builder.finish_node();
    }

    fn parse_scalar(&mut self) {
        self.builder.start_node(SyntaxKind::SCALAR.into());

        // Handle quotes
        if matches!(
            self.current(),
            Some(SyntaxKind::QUOTE | SyntaxKind::SINGLE_QUOTE)
        ) {
            let quote_type = self
                .current()
                .expect("current token is Some: checked by matches! guard above");
            self.bump(); // opening quote

            // Consume all tokens until the closing quote
            while self.current().is_some() && self.current() != Some(quote_type) {
                self.bump();
            }

            if self.current() == Some(quote_type) {
                self.bump(); // closing quote
            } else {
                let expected_quote = if quote_type == SyntaxKind::QUOTE {
                    "\""
                } else {
                    "'"
                };
                let error_msg = self.create_detailed_error(
                    "Unterminated quoted string",
                    &format!("closing quote {expected_quote}"),
                    self.current_text(),
                );
                self.add_error_and_recover(
                    error_msg,
                    quote_type,
                    ParseErrorKind::UnterminatedString,
                );
            }
        } else {
            // Handle typed scalar tokens from lexer
            if matches!(
                self.current(),
                Some(
                    SyntaxKind::STRING
                        | SyntaxKind::UNTERMINATED_STRING
                        | SyntaxKind::INT
                        | SyntaxKind::FLOAT
                        | SyntaxKind::BOOL
                        | SyntaxKind::NULL
                )
            ) {
                // Check for unterminated string and add error
                if self.current() == Some(SyntaxKind::UNTERMINATED_STRING) {
                    self.add_error(
                        "Unterminated quoted string".to_string(),
                        ParseErrorKind::UnterminatedString,
                    );
                }
                if !self.in_flow_context {
                    // For plain scalars in block context, handle multi-line plain scalars
                    // per YAML spec: continuation lines must be more indented than the scalar's starting line
                    //
                    // Use current_line_indent which tracks the actual line indentation.
                    // CRITICAL: For inline scalars in sequence items (where indent==0 because the
                    // INDENT token was already consumed), we MUST NOT try continuation because we
                    // can't distinguish between continuation and the next mapping key.
                    let scalar_indent = self.current_line_indent;

                    while let Some(kind) = self.current() {
                        if kind == SyntaxKind::COMMENT {
                            // Stop at comments
                            break;
                        }

                        if kind == SyntaxKind::NEWLINE {
                            // Check if next line continues the scalar (more indented)
                            if self.is_plain_scalar_continuation(scalar_indent) {
                                // Fold the newline - consume it and following whitespace
                                self.bump(); // consume NEWLINE

                                // Skip INDENT and WHITESPACE on next line
                                while matches!(
                                    self.current(),
                                    Some(SyntaxKind::INDENT | SyntaxKind::WHITESPACE)
                                ) {
                                    self.bump();
                                }

                                // Continue consuming scalar content on next line
                                continue;
                            }
                            // Next line is not a continuation - stop here
                            break;
                        }

                        // In block context, stop at flow collection delimiters
                        if matches!(
                            kind,
                            SyntaxKind::LEFT_BRACKET
                                | SyntaxKind::LEFT_BRACE
                                | SyntaxKind::RIGHT_BRACKET
                                | SyntaxKind::RIGHT_BRACE
                                | SyntaxKind::COMMA
                        ) {
                            break;
                        }

                        // Check ahead to see if next token is a comment
                        if kind == SyntaxKind::WHITESPACE {
                            // Look ahead to see if a comment follows
                            if self.tokens.len() >= 2 {
                                let next_kind = self.tokens[self.tokens.len() - 2].0;
                                if next_kind == SyntaxKind::COMMENT {
                                    // Don't consume this whitespace, it precedes a comment
                                    break;
                                }
                            }
                        }

                        self.bump();
                    }
                } else {
                    // In flow context, consume tokens until we hit a delimiter
                    // This handles multi-word keys like "omitted value"
                    // Plain scalars in flow context can span multiple lines (YAML 1.2 spec)

                    // Check if this is a quoted string (STRING token starting with quote)
                    // Quoted strings are complete in a single token and should not consume
                    // trailing newlines/whitespace
                    let is_quoted_string = matches!(self.current(), Some(SyntaxKind::STRING))
                        && self
                            .current_text()
                            .is_some_and(|text| text.starts_with('"') || text.starts_with('\''));

                    self.bump(); // Consume the initial typed token

                    // For quoted strings, we're done - the token contains the complete value.
                    // For plain scalars, keep consuming for multi-word/multi-line scalars.
                    if !is_quoted_string {
                        while let Some(kind) = self.current() {
                            // Check for flow delimiters and comments (but not NEWLINE - plain scalars can span lines)
                            if matches!(
                                kind,
                                SyntaxKind::COMMA
                                    | SyntaxKind::RIGHT_BRACE
                                    | SyntaxKind::RIGHT_BRACKET
                                    | SyntaxKind::COMMENT
                            ) {
                                break;
                            }

                            // NEWLINE in flow context: consume it and continue reading the scalar
                            // The scalar continues on the next line
                            if kind == SyntaxKind::NEWLINE {
                                self.bump(); // consume the newline
                                             // Skip any indentation/whitespace that follows
                                while matches!(
                                    self.current(),
                                    Some(SyntaxKind::WHITESPACE | SyntaxKind::INDENT)
                                ) {
                                    self.bump();
                                }
                                // Continue with the main loop to consume more scalar content
                                continue;
                            }

                            // Stop at trailing whitespace before delimiters
                            // For "[ a , b ]", stop at whitespace before comma
                            // For "{omitted value:,}", consume whitespace between words
                            if kind == SyntaxKind::WHITESPACE {
                                // Peek at what comes after the whitespace
                                // tokens are popped from end, so earlier indices are further ahead
                                if self.tokens.len() >= 2 {
                                    // Look at the token after this whitespace
                                    let after_whitespace = self.tokens[self.tokens.len() - 2].0;
                                    if matches!(
                                        after_whitespace,
                                        SyntaxKind::COMMA
                                            | SyntaxKind::RIGHT_BRACE
                                            | SyntaxKind::RIGHT_BRACKET
                                            | SyntaxKind::NEWLINE
                                            | SyntaxKind::COMMENT
                                    ) {
                                        // Whitespace followed by delimiter or comment - stop here (don't consume whitespace)
                                        break;
                                    }
                                    // Otherwise whitespace is between words - continue to consume it
                                }
                            }

                            // Handle colons: stop if colon is followed by delimiter
                            if kind == SyntaxKind::COLON && self.tokens.len() >= 2 {
                                let next_kind = self.tokens[self.tokens.len() - 2].0;
                                if matches!(
                                    next_kind,
                                    SyntaxKind::COMMA
                                        | SyntaxKind::RIGHT_BRACE
                                        | SyntaxKind::RIGHT_BRACKET
                                        | SyntaxKind::WHITESPACE
                                        | SyntaxKind::NEWLINE
                                ) {
                                    // Colon followed by delimiter - this is key-value separator
                                    break;
                                }
                            }

                            self.bump();
                        }
                    }
                }
            } else {
                // Fallback: consume tokens until we hit structure
                while let Some(kind) = self.current() {
                    if matches!(
                        kind,
                        SyntaxKind::NEWLINE
                            | SyntaxKind::DASH
                            | SyntaxKind::COMMENT
                            | SyntaxKind::DOC_START
                            | SyntaxKind::DOC_END
                    ) {
                        break;
                    }

                    // In flow context, colons are allowed in scalars (for IPv6, URLs, etc.)
                    // In block context, stop at colons as they indicate mapping structure
                    if kind == SyntaxKind::COLON {
                        if self.in_flow_context {
                            // In flow context, check if this colon is followed by a delimiter
                            // If so, it's a key-value separator, not part of the scalar
                            if self.tokens.len() >= 2 {
                                let next_kind = self.tokens[self.tokens.len() - 2].0;
                                if matches!(
                                    next_kind,
                                    SyntaxKind::COMMA
                                        | SyntaxKind::RIGHT_BRACE
                                        | SyntaxKind::RIGHT_BRACKET
                                        | SyntaxKind::WHITESPACE
                                        | SyntaxKind::NEWLINE
                                ) {
                                    // Colon followed by delimiter - stop here
                                    break;
                                }
                            }
                            // Otherwise, allow colons in scalars (URLs, etc.) - continue consuming
                        } else {
                            // In block context, stop at colons (mapping structure)
                            break;
                        }
                    }

                    // In flow context, stop at flow collection delimiters
                    if self.in_flow_context
                        && matches!(
                            kind,
                            SyntaxKind::LEFT_BRACKET
                                | SyntaxKind::RIGHT_BRACKET
                                | SyntaxKind::LEFT_BRACE
                                | SyntaxKind::RIGHT_BRACE
                                | SyntaxKind::COMMA
                        )
                    {
                        break;
                    }
                    self.bump();
                }
            }
        }

        self.builder.finish_node();
    }

    fn parse_tagged_value(&mut self) {
        // Peek at the tag to determine what kind of collection to parse
        let tag_text = self.peek_tag_text();

        match tag_text {
            Some("!!set") => self.parse_tagged_set(),
            Some("!!omap") => self.parse_tagged_omap(),
            Some("!!pairs") => self.parse_tagged_pairs(),
            _ => {
                // Default tagged value behavior - tags can be applied to scalars, mappings, or sequences
                self.builder.start_node(SyntaxKind::TAGGED_NODE.into());
                self.bump(); // TAG token

                // Skip any whitespace after the tag
                while matches!(self.current(), Some(SyntaxKind::WHITESPACE)) {
                    self.bump();
                }

                // Parse whatever value follows the tag (scalar, flow mapping, flow sequence, etc.)
                self.parse_value();

                self.builder.finish_node();
            }
        }
    }

    fn peek_tag_text(&self) -> Option<&str> {
        self.tokens
            .last()
            .filter(|(kind, _)| *kind == SyntaxKind::TAG)
            .map(|(_, text)| text.as_str())
    }

    fn parse_tagged_set(&mut self) {
        self.parse_tagged_collection(true); // true = parse as mapping
    }

    fn parse_tagged_omap(&mut self) {
        self.parse_tagged_collection(false); // false = parse as sequence
    }

    fn parse_tagged_pairs(&mut self) {
        self.parse_tagged_collection(false); // false = parse as sequence
    }

    fn parse_tagged_collection(&mut self, is_mapping: bool) {
        self.builder.start_node(SyntaxKind::TAGGED_NODE.into());

        // Consume the tag
        self.bump(); // TAG token

        // Skip any whitespace after the tag
        while matches!(self.current(), Some(SyntaxKind::WHITESPACE)) {
            self.bump();
        }

        // Parse the following structure based on type
        match self.current() {
            Some(SyntaxKind::LEFT_BRACE) if is_mapping => self.parse_flow_mapping(),
            Some(SyntaxKind::LEFT_BRACKET) if !is_mapping => self.parse_flow_sequence(),
            Some(SyntaxKind::NEWLINE) => {
                self.bump(); // consume newline
                             // Check if next token is indent (for indented content)
                if self.current() == Some(SyntaxKind::INDENT) {
                    self.bump(); // consume indent
                }
                // Anchor the inner block on the indent we just
                // consumed so a column-0 sibling entry dedents out of
                // this tagged collection instead of being absorbed.
                let inner_base = self.current_line_indent;
                if is_mapping {
                    self.parse_mapping_with_base_indent(inner_base);
                } else {
                    self.parse_sequence_with_base_indent(inner_base);
                }
            }
            _ => {
                let inner_base = self.current_line_indent;
                if is_mapping {
                    self.parse_mapping_with_base_indent(inner_base);
                } else {
                    self.parse_sequence_with_base_indent(inner_base);
                }
            }
        }

        self.builder.finish_node();
    }

    fn parse_literal_block_scalar(&mut self) {
        self.builder.start_node(SyntaxKind::SCALAR.into());
        self.bump(); // consume PIPE
        self.parse_block_scalar_header();
        self.parse_block_scalar_content();
        self.builder.finish_node();
    }

    fn parse_folded_block_scalar(&mut self) {
        self.builder.start_node(SyntaxKind::SCALAR.into());
        self.bump(); // consume GREATER
        self.parse_block_scalar_header();
        self.parse_block_scalar_content();
        self.builder.finish_node();
    }

    fn parse_block_scalar_header(&mut self) {
        // Parse optional indentation indicator (1-9) and chomping indicator (+, -)
        // Format: |<indent><chomp> or |<chomp><indent>
        // Examples: |2, |-, |+, |2-, |-2, |2+, |+2

        while let Some(kind) = self.current() {
            match kind {
                SyntaxKind::NEWLINE | SyntaxKind::COMMENT => break,
                SyntaxKind::INT => {
                    // Indentation indicator (1-9)
                    if let Some(text) = self.current_text() {
                        if text.len() == 1
                            && text
                                .chars()
                                .next()
                                .expect("text is non-empty: len == 1 checked above")
                                .is_ascii_digit()
                        {
                            self.bump(); // Consume the digit
                        } else {
                            // Not a single digit, stop
                            break;
                        }
                    } else {
                        break;
                    }
                }
                SyntaxKind::STRING => {
                    // Could be chomping indicator or other text
                    if let Some(text) = self.current_text() {
                        if text == "+" || text == "-" {
                            self.bump(); // Consume chomping indicator
                        } else {
                            // Some other text, stop parsing header
                            break;
                        }
                    } else {
                        break;
                    }
                }
                SyntaxKind::WHITESPACE => {
                    // Whitespace before comment or newline
                    self.bump();
                }
                _ => {
                    // Unknown token, stop parsing header
                    break;
                }
            }
        }

        // Consume optional comment
        if self.current() == Some(SyntaxKind::COMMENT) {
            self.bump();
        }

        // Consume the newline after the header
        if self.current() == Some(SyntaxKind::NEWLINE) {
            self.bump();
        }
    }

    fn parse_block_scalar_content(&mut self) {
        // Consume all indented content that follows
        let mut last_was_newline = false;
        let mut base_indent: Option<usize> = None;
        let mut first_content_indent: Option<usize> = None;

        while let Some(kind) = self.current() {
            // Detect first content indentation to use as base
            if kind == SyntaxKind::INDENT && first_content_indent.is_none() {
                first_content_indent = self.current_text().map(|t| t.len());
            }

            // Set base_indent after seeing first INDENT token
            if base_indent.is_none() && first_content_indent.is_some() {
                base_indent = first_content_indent;
            }

            // Check if we've reached unindented content BEFORE consuming
            if self.is_at_unindented_content_for_block_scalar(last_was_newline, base_indent) {
                break;
            }

            match kind {
                // Stop at document markers
                SyntaxKind::DOC_START | SyntaxKind::DOC_END => break,
                // Track newlines to detect line starts
                SyntaxKind::NEWLINE => {
                    self.bump();
                    last_was_newline = true;
                    continue;
                }
                // Continue consuming content and whitespace
                _ => {
                    self.bump();
                    last_was_newline = false;
                }
            }
        }
    }

    fn is_at_unindented_content_for_block_scalar(
        &self,
        after_newline: bool,
        base_indent: Option<usize>,
    ) -> bool {
        // Check if we've reached content at the beginning of a line (unindented)
        // Only check for structural tokens if we're at the start of a line
        if after_newline {
            // After a newline, check if the next token is unindented
            let current = self.current();

            // COLON or QUESTION at start of line means end of block scalar
            if matches!(current, Some(SyntaxKind::COLON | SyntaxKind::QUESTION)) {
                return true;
            }

            // If we have base_indent, check if current line has less indentation
            if let Some(base) = base_indent {
                if current == Some(SyntaxKind::INDENT) {
                    if let Some(text) = self.current_text() {
                        if text.len() < base {
                            // Current line has less indentation than base - end of block scalar
                            return true;
                        }
                    }
                }
            }

            // If we don't see INDENT, we've reached unindented content
            if current != Some(SyntaxKind::INDENT)
                && current != Some(SyntaxKind::WHITESPACE)
                && current != Some(SyntaxKind::NEWLINE)
                && current != Some(SyntaxKind::COMMENT)
            {
                // This is unindented content at the start of a line
                return true;
            }
        }
        false
    }

    fn parse_mapping_with_base_indent(&mut self, base_indent: usize) {
        self.builder.start_node(SyntaxKind::MAPPING.into());
        self.error_context.push_context(ParseContext::Mapping);

        while self.current().is_some() {
            let tokens_before_iter = self.tokens.len();
            // Skip whitespace, break on dedent
            if self.skip_whitespace_only_with_dedent_check(base_indent) {
                break;
            }

            // Emit comments as children of MAPPING
            loop {
                if self.current() == Some(SyntaxKind::COMMENT) {
                    // At root level (base_indent=0) all comments belong here since
                    // there's no parent scope, even if indented.
                    if base_indent > 0 && self.is_at_dedented_position(base_indent) {
                        break;
                    }
                    self.bump();
                    if self.current() == Some(SyntaxKind::NEWLINE) {
                        self.bump();
                    }
                    if self.skip_whitespace_only_with_dedent_check(base_indent) {
                        break;
                    }
                } else {
                    break;
                }
            }

            // Check dedent via tracked line indentation (covers the case where
            // MAPPING_ENTRY consumed its trailing NEWLINE before we could detect
            // the dedent in skip_whitespace_only_with_dedent_check).
            if base_indent > 0 && self.is_at_dedented_position(base_indent) {
                break;
            }

            // No mapping key found - exit
            if !self.is_mapping_key() && !self.is_complex_mapping_key() {
                break;
            }

            // Check for complex keys (sequences or mappings as keys)
            if self.current() == Some(SyntaxKind::LEFT_BRACKET)
                || self.current() == Some(SyntaxKind::LEFT_BRACE)
            {
                // Start a MAPPING_ENTRY to wrap this key-value pair
                self.builder.start_node(SyntaxKind::MAPPING_ENTRY.into());

                self.builder.start_node(SyntaxKind::KEY.into());
                if self.current() == Some(SyntaxKind::LEFT_BRACKET) {
                    self.parse_flow_sequence();
                } else if self.current() == Some(SyntaxKind::LEFT_BRACE) {
                    self.parse_flow_mapping();
                }
                self.builder.finish_node();

                self.skip_ws_and_newlines();

                if self.current() == Some(SyntaxKind::COLON) {
                    self.bump();
                    self.skip_whitespace();

                    self.builder.start_node(SyntaxKind::VALUE.into());
                    if self.current().is_some() && self.current() != Some(SyntaxKind::NEWLINE) {
                        self.parse_value();
                    } else if self.current() == Some(SyntaxKind::NEWLINE) {
                        self.bump();
                        if self.current() == Some(SyntaxKind::INDENT) {
                            self.bump();
                            self.parse_value();
                        }
                    }
                    self.builder.finish_node();
                } else {
                    let error_msg = self.create_detailed_error(
                        "Missing colon in mapping",
                        "':' after key",
                        self.current_text(),
                    );
                    self.add_error_and_recover(error_msg, SyntaxKind::COLON, ParseErrorKind::Other);
                }

                // Finish the MAPPING_ENTRY node
                self.builder.finish_node();
            }
            // Check for explicit key indicator
            else if self.current() == Some(SyntaxKind::QUESTION) {
                // Start a MAPPING_ENTRY to wrap this key-value pair
                self.builder.start_node(SyntaxKind::MAPPING_ENTRY.into());

                // Parse explicit key
                self.bump(); // consume '?'
                self.skip_whitespace();

                self.builder.start_node(SyntaxKind::KEY.into());
                if self.current().is_some() && self.current() != Some(SyntaxKind::NEWLINE) {
                    self.parse_value();
                }
                self.builder.finish_node();

                self.skip_ws_and_newlines();

                // Parse value if there's a colon
                if self.current() == Some(SyntaxKind::COLON) {
                    self.bump(); // consume ':'
                    self.skip_whitespace();

                    self.builder.start_node(SyntaxKind::VALUE.into());
                    if self.current().is_some() && self.current() != Some(SyntaxKind::NEWLINE) {
                        self.parse_value();
                    } else if self.current() == Some(SyntaxKind::NEWLINE) {
                        self.bump(); // consume newline
                        if self.current() == Some(SyntaxKind::INDENT) {
                            self.bump(); // consume indent
                            self.parse_value();
                        }
                    }
                    self.builder.finish_node();
                } else {
                    // No value, just a key - create explicit null value
                    self.builder.start_node(SyntaxKind::VALUE.into());
                    self.builder.start_node(SyntaxKind::SCALAR.into());
                    self.builder.token(SyntaxKind::NULL.into(), "");
                    self.builder.finish_node();
                    self.builder.finish_node();
                }

                // Finish the MAPPING_ENTRY node
                self.builder.finish_node();
            } else {
                self.parse_mapping_key_value_pair(base_indent);
            }

            // Progress guard: if no token was consumed this iteration we
            // would loop forever (e.g. when is_mapping_key() is fooled by a
            // delimiter such as `}` followed by `:`, and synthetic-token
            // recovery never advances).
            if self.tokens.len() == tokens_before_iter {
                let unexpected = self.current_text().unwrap_or("").to_string();
                self.add_error(
                    format!("Unexpected token in mapping: {unexpected:?}"),
                    ParseErrorKind::Other,
                );
                self.bump();
            }
        }

        self.builder.finish_node();
        self.error_context.pop_context();
    }

    fn parse_sequence(&mut self) {
        self.parse_sequence_with_base_indent(0);
    }

    fn parse_sequence_with_base_indent(&mut self, base_indent: usize) {
        self.builder.start_node(SyntaxKind::SEQUENCE.into());
        self.error_context.push_context(ParseContext::Sequence);

        while self.current().is_some() {
            // Skip whitespace, break on dedent
            if self.skip_whitespace_only_with_dedent_check(base_indent) {
                break;
            }

            // Emit comments as children of SEQUENCE
            loop {
                if self.current() == Some(SyntaxKind::COMMENT) {
                    // At root level (base_indent=0) all comments belong here since
                    // there's no parent scope, even if indented.
                    if base_indent > 0 && self.is_at_dedented_position(base_indent) {
                        break;
                    }
                    self.bump();
                    if self.current() == Some(SyntaxKind::NEWLINE) {
                        self.bump();
                    }
                    if self.skip_whitespace_only_with_dedent_check(base_indent) {
                        break;
                    }
                } else {
                    break;
                }
            }

            // Check dedent via tracked line indentation (covers the case where
            // SEQUENCE_ENTRY consumed its trailing NEWLINE before we could detect
            // the dedent in skip_whitespace_only_with_dedent_check).
            if base_indent > 0 && self.is_at_dedented_position(base_indent) {
                break;
            }

            // No dash - exit
            if self.current() != Some(SyntaxKind::DASH) {
                break;
            }
            // Start SEQUENCE_ENTRY node to wrap the entire item
            self.builder.start_node(SyntaxKind::SEQUENCE_ENTRY.into());

            self.bump(); // consume dash
            self.skip_whitespace();

            // Record the dash's line indentation for the item value parsing
            let item_indent = self.current_line_indent;

            if self.current().is_some() && self.current() != Some(SyntaxKind::NEWLINE) {
                // Use item's line indent so nested mappings parse at the right level
                self.parse_value_with_base_indent(item_indent);
            } else if self.current() == Some(SyntaxKind::NEWLINE) {
                // Check if next line is indented (nested content for sequence item)
                self.bump(); // consume newline
                if self.current() == Some(SyntaxKind::INDENT) {
                    let indent_level = self.tokens.last().map_or(0, |(_, text)| text.len());
                    self.bump(); // consume indent
                                 // Parse the indented content as the sequence item value
                    self.parse_value_with_base_indent(indent_level);
                }
            }

            // Block-style SEQUENCE_ENTRY owns its NEWLINE terminator (DESIGN.md)
            if self.current() == Some(SyntaxKind::NEWLINE) {
                self.bump();
            }

            // Finish SEQUENCE_ENTRY node
            self.builder.finish_node();
        }

        self.builder.finish_node();
        self.error_context.pop_context();
    }

    /// Checks if the upcoming tokens form an implicit mapping pattern (key: value).
    ///
    /// This scans forward through the token buffer to detect if there's a colon at
    /// depth 0 (not nested inside brackets/braces) before hitting a comma or closing bracket.
    ///
    /// Scans from current token forward through upcoming tokens.
    ///
    /// # Examples
    /// - `[ 'key' : value ]` → true (colon at depth 0)
    /// - `[ value ]` → false (no colon before closing bracket)
    /// - `[ [a, b]: value ]` → true (colon after nested collection completes)
    /// - `[ {a: 1}, b ]` → false (colon is inside braces, not at depth 0)
    fn next_flow_element_is_implicit_mapping(&self) -> bool {
        // Chain current token with upcoming tokens (no allocation needed)
        let tokens = std::iter::once(self.current().unwrap_or(SyntaxKind::EOF))
            .chain(self.upcoming_tokens());
        has_implicit_mapping_pattern(tokens)
    }

    /// Parse an implicit flow mapping (key: value without braces).
    /// Used inside flow sequences: [ key: value ] is valid YAML.
    fn parse_implicit_flow_mapping(&mut self) {
        self.builder.start_node(SyntaxKind::MAPPING.into());
        self.builder.start_node(SyntaxKind::MAPPING_ENTRY.into());

        // Parse key
        self.builder.start_node(SyntaxKind::KEY.into());
        self.parse_value();
        self.builder.finish_node();

        self.skip_ws_and_newlines();

        // Consume colon
        if self.current() == Some(SyntaxKind::COLON) {
            self.bump();
            self.skip_ws_and_newlines();
        }

        // Parse value
        self.builder.start_node(SyntaxKind::VALUE.into());
        // Check if value is omitted (implicit null)
        if matches!(
            self.current(),
            Some(SyntaxKind::COMMA | SyntaxKind::RIGHT_BRACKET)
        ) {
            // Omitted value - leave VALUE node empty
        } else {
            self.parse_value();
        }
        self.builder.finish_node();

        self.builder.finish_node(); // MAPPING_ENTRY
        self.builder.finish_node(); // MAPPING
    }
}

/// Kinds emitted by the lexer for plain (unquoted) scalar content.
fn is_plain_scalar_kind(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::STRING
            | SyntaxKind::INT
            | SyntaxKind::FLOAT
            | SyntaxKind::BOOL
            | SyntaxKind::NULL
    )
}

/// Standalone helper to detect implicit mapping pattern in flow collections.
/// Takes an iterator of SyntaxKind tokens (in reverse order, as stored in Parser).
/// Returns true if there's a colon at depth 0 before any comma or closing bracket.
pub(crate) fn has_implicit_mapping_pattern(tokens: impl Iterator<Item = SyntaxKind>) -> bool {
    let mut depth = 0;

    for kind in tokens {
        match kind {
            // Opening brackets/braces increase nesting depth
            SyntaxKind::LEFT_BRACE | SyntaxKind::LEFT_BRACKET => {
                depth += 1;
            }
            // Closing brackets/braces decrease nesting depth
            SyntaxKind::RIGHT_BRACE | SyntaxKind::RIGHT_BRACKET => {
                if depth == 0 {
                    // Closing bracket at our level - end of element without finding colon
                    return false;
                }
                depth -= 1;
            }
            // At depth 0 (not inside nested collections), check for colon or separator
            SyntaxKind::COLON if depth == 0 => {
                // Found colon at our level - this is an implicit mapping
                return true;
            }
            SyntaxKind::COMMA if depth == 0 => {
                // Found separator at our level - not a mapping
                return false;
            }
            // Skip whitespace, newlines, and other tokens
            _ => {}
        }
    }

    // Reached end of tokens without finding colon or separator
    false
}

impl Parser {
    fn parse_flow_sequence(&mut self) {
        self.builder.start_node(SyntaxKind::SEQUENCE.into());
        self.error_context.push_context(ParseContext::FlowSequence);

        if self.flow_depth >= MAX_FLOW_DEPTH {
            self.add_error(
                format!("Flow collection nested too deeply (limit {MAX_FLOW_DEPTH})"),
                ParseErrorKind::Other,
            );
            // Consume everything up to a closing delimiter to recover, then
            // bail out without recursing further.
            while let Some(kind) = self.current() {
                if matches!(
                    kind,
                    SyntaxKind::RIGHT_BRACKET | SyntaxKind::DOC_START | SyntaxKind::DOC_END
                ) {
                    break;
                }
                self.bump();
            }
            if self.current() == Some(SyntaxKind::RIGHT_BRACKET) {
                self.bump();
            }
            self.builder.finish_node();
            self.error_context.pop_context();
            return;
        }
        self.flow_depth += 1;

        self.bump(); // consume [
        self.skip_ws_and_newlines(); // Support comments and newlines in flow sequences

        let prev_flow = self.in_flow_context;
        self.in_flow_context = true;

        while self.current() != Some(SyntaxKind::RIGHT_BRACKET) && self.current().is_some() {
            let tokens_before = self.tokens.len();

            // Start SEQUENCE_ENTRY node to wrap the item
            self.builder.start_node(SyntaxKind::SEQUENCE_ENTRY.into());

            // Check if this element is an implicit mapping (key: value)
            // Per YAML spec, [ key: value ] is valid - a sequence containing a mapping
            if self.next_flow_element_is_implicit_mapping() {
                // Parse as implicit flow mapping
                self.parse_implicit_flow_mapping();
            } else {
                // Parse as regular value
                self.parse_value();
            }

            self.skip_ws_and_newlines(); // Support comments after values

            // Flow-style SEQUENCE_ENTRY owns its COMMA terminator (except last entry)
            if self.current() == Some(SyntaxKind::COMMA) {
                self.bump();
                self.skip_ws_and_newlines(); // Support comments after commas
            }

            self.builder.finish_node(); // Finish SEQUENCE_ENTRY

            if self.current() != Some(SyntaxKind::RIGHT_BRACKET) && self.current().is_some() {
                // No comma found and not at closing bracket
                // Check if we should break to avoid infinite loops
                if matches!(
                    self.current(),
                    Some(SyntaxKind::DASH | SyntaxKind::DOC_START | SyntaxKind::DOC_END)
                ) {
                    // These tokens indicate we've left the flow sequence context or hit invalid syntax
                    break;
                }
            }

            // Guarantee progress: if no token was consumed this iteration we
            // would loop forever (e.g. on stray `}` inside `[...]`). Report
            // the unexpected token and skip it.
            if self.tokens.len() == tokens_before {
                let unexpected = self.current_text().unwrap_or("").to_string();
                self.add_error(
                    format!("Unexpected token in flow sequence: {unexpected:?}"),
                    ParseErrorKind::Other,
                );
                self.bump();
            }
        }

        self.in_flow_context = prev_flow;

        if self.current() == Some(SyntaxKind::RIGHT_BRACKET) {
            self.bump();
        } else {
            let error_msg = self.create_detailed_error(
                "Unclosed flow sequence",
                "']' to close sequence",
                self.current_text(),
            );
            self.add_error_and_recover(
                error_msg,
                SyntaxKind::RIGHT_BRACKET,
                ParseErrorKind::UnclosedFlowSequence,
            );
        }

        self.flow_depth -= 1;
        self.builder.finish_node();
        self.error_context.pop_context();
    }

    fn parse_flow_mapping(&mut self) {
        self.builder.start_node(SyntaxKind::MAPPING.into());
        self.error_context.push_context(ParseContext::FlowMapping);

        if self.flow_depth >= MAX_FLOW_DEPTH {
            self.add_error(
                format!("Flow collection nested too deeply (limit {MAX_FLOW_DEPTH})"),
                ParseErrorKind::Other,
            );
            while let Some(kind) = self.current() {
                if matches!(
                    kind,
                    SyntaxKind::RIGHT_BRACE | SyntaxKind::DOC_START | SyntaxKind::DOC_END
                ) {
                    break;
                }
                self.bump();
            }
            if self.current() == Some(SyntaxKind::RIGHT_BRACE) {
                self.bump();
            }
            self.builder.finish_node();
            self.error_context.pop_context();
            return;
        }
        self.flow_depth += 1;

        self.bump(); // consume {
        self.skip_ws_and_newlines(); // Support comments and newlines in flow mappings

        let prev_flow = self.in_flow_context;
        self.in_flow_context = true;

        while self.current() != Some(SyntaxKind::RIGHT_BRACE) && self.current().is_some() {
            // Check for unexpected structural tokens that indicate we've left flow context
            if matches!(
                self.current(),
                Some(SyntaxKind::DASH | SyntaxKind::DOC_START | SyntaxKind::DOC_END)
            ) {
                // These tokens indicate we've exited the flow mapping or hit invalid syntax
                break;
            }

            let tokens_before = self.tokens.len();

            // Start MAPPING_ENTRY node to wrap the key-value pair
            self.builder.start_node(SyntaxKind::MAPPING_ENTRY.into());

            // Parse key - wrap in KEY node
            self.builder.start_node(SyntaxKind::KEY.into());

            // Handle explicit key indicator (?) in flow context
            if self.current() == Some(SyntaxKind::QUESTION) {
                self.bump(); // consume '?'
                self.skip_whitespace();
            }

            self.parse_value();
            self.builder.finish_node();

            self.skip_ws_and_newlines(); // Support comments after keys

            if self.current() == Some(SyntaxKind::COLON) {
                self.bump();
                self.skip_ws_and_newlines(); // Support comments after colons

                // Check if value is omitted (comma or closing brace after colon)
                // In YAML, `key:,` or `key:}` means key has null value
                if matches!(
                    self.current(),
                    Some(SyntaxKind::COMMA | SyntaxKind::RIGHT_BRACE)
                ) {
                    // Omitted value - create VALUE node with implicit null scalar
                    self.builder.start_node(SyntaxKind::VALUE.into());
                    self.builder.start_node(SyntaxKind::SCALAR.into());
                    self.builder.token(SyntaxKind::NULL.into(), "");
                    self.builder.finish_node(); // SCALAR
                    self.builder.finish_node(); // VALUE
                } else {
                    // Parse value - wrap in VALUE node
                    self.builder.start_node(SyntaxKind::VALUE.into());
                    self.parse_value();
                    self.builder.finish_node();
                }
            } else if matches!(
                self.current(),
                Some(SyntaxKind::COMMA | SyntaxKind::RIGHT_BRACE)
            ) {
                // No colon, but followed by comma or closing brace
                // This means the key itself has a null value (shorthand for key: null)
                // Create VALUE node with implicit null scalar
                self.builder.start_node(SyntaxKind::VALUE.into());
                self.builder.start_node(SyntaxKind::SCALAR.into());
                self.builder.token(SyntaxKind::NULL.into(), "");
                self.builder.finish_node(); // SCALAR
                self.builder.finish_node(); // VALUE
            } else {
                let error_msg = self.create_detailed_error(
                    "Missing colon in flow mapping",
                    "':' after key",
                    self.current_text(),
                );
                self.add_error_and_recover(error_msg, SyntaxKind::COLON, ParseErrorKind::Other);
            }

            self.skip_ws_and_newlines(); // Support comments after values

            // Flow-style entries own their COMMA terminator (except last entry)
            if self.current() == Some(SyntaxKind::COMMA) {
                self.bump();
                self.skip_ws_and_newlines(); // Support comments after commas
            }

            // Finish MAPPING_ENTRY node
            self.builder.finish_node();

            // Guarantee progress: if no token was consumed this iteration we
            // would loop forever (e.g. on stray `]` inside `{...}`). Report
            // the unexpected token and skip it.
            if self.tokens.len() == tokens_before {
                let unexpected = self.current_text().unwrap_or("").to_string();
                self.add_error(
                    format!("Unexpected token in flow mapping: {unexpected:?}"),
                    ParseErrorKind::Other,
                );
                self.bump();
            }
        }

        self.in_flow_context = prev_flow;

        if self.current() == Some(SyntaxKind::RIGHT_BRACE) {
            self.bump();
        } else {
            let error_msg = self.create_detailed_error(
                "Unclosed flow mapping",
                "'}' to close mapping",
                self.current_text(),
            );
            self.add_error_and_recover(
                error_msg,
                SyntaxKind::RIGHT_BRACE,
                ParseErrorKind::UnclosedFlowMapping,
            );
        }

        self.flow_depth -= 1;
        self.builder.finish_node();
        self.error_context.pop_context();
    }

    fn parse_directive(&mut self) {
        self.builder.start_node(SyntaxKind::DIRECTIVE.into());

        if self.current() == Some(SyntaxKind::DIRECTIVE) {
            self.bump(); // consume the directive token
        } else {
            self.add_error("Expected directive".to_string(), ParseErrorKind::Other);
        }

        self.builder.finish_node();
    }

    fn parse_explicit_key_mapping(&mut self) {
        // Parse mapping with explicit key indicator '?'
        self.builder.start_node(SyntaxKind::MAPPING.into());

        while self.current() == Some(SyntaxKind::QUESTION) {
            // Start a MAPPING_ENTRY to wrap this key-value pair
            self.builder.start_node(SyntaxKind::MAPPING_ENTRY.into());

            // Parse explicit key
            self.bump(); // consume '?'
            self.skip_whitespace();

            // Parse key - can be any value including sequences and mappings
            self.builder.start_node(SyntaxKind::KEY.into());

            // Parse the first part of the key
            if self.current().is_some() && self.current() != Some(SyntaxKind::NEWLINE) {
                self.parse_value();
            }

            // Check if this is a multiline key (newline followed by indent)
            // Only for scalar keys, not sequences or mappings
            if self.current() == Some(SyntaxKind::NEWLINE) {
                // Peek ahead to see if there's an indent after the newline
                // Since tokens are reversed, peek at the second-to-last token
                if self.tokens.len() >= 2 {
                    let (next_kind, _) = &self.tokens[self.tokens.len() - 2];
                    if *next_kind == SyntaxKind::INDENT {
                        // Check what comes after the indent (at position len() - 3)
                        if self.tokens.len() >= 3 {
                            let (token_after_indent, _) = &self.tokens[self.tokens.len() - 3];
                            // If it's a DASH, this is a sequence continuation which was already
                            // handled by parse_value() above - don't try to parse it as multiline scalar
                            if *token_after_indent != SyntaxKind::DASH {
                                // This is a multiline scalar key continuation
                                self.bump(); // consume newline
                                self.bump(); // consume indent

                                // Parse scalar tokens at this indentation level as part of the key
                                while self.current().is_some()
                                    && self.current() != Some(SyntaxKind::NEWLINE)
                                    && self.current() != Some(SyntaxKind::COLON)
                                {
                                    let before = self.tokens.len();
                                    self.parse_scalar();
                                    if self.current() == Some(SyntaxKind::WHITESPACE) {
                                        self.bump(); // consume whitespace between key parts
                                    }
                                    // Progress guard: parse_scalar() can return without
                                    // consuming tokens for kinds it doesn't handle (e.g.
                                    // COMMENT). Break to avoid an infinite loop.
                                    if self.tokens.len() == before {
                                        break;
                                    }
                                }
                            }
                        }
                    }
                }
            }

            self.builder.finish_node();

            self.skip_ws_and_newlines();

            // Parse value if there's a colon
            if self.current() == Some(SyntaxKind::COLON) {
                self.bump(); // consume ':'
                self.skip_whitespace();

                self.builder.start_node(SyntaxKind::VALUE.into());
                if self.current().is_some() && self.current() != Some(SyntaxKind::NEWLINE) {
                    self.parse_value();
                } else if self.current() == Some(SyntaxKind::NEWLINE) {
                    // Check if next line is indented (nested content)
                    self.bump(); // consume newline
                    if self.current() == Some(SyntaxKind::INDENT) {
                        self.bump(); // consume indent
                        self.parse_value();
                    }
                }
                self.builder.finish_node();
            } else {
                // No value, just a key - create explicit null value
                self.builder.start_node(SyntaxKind::VALUE.into());
                self.builder.start_node(SyntaxKind::SCALAR.into());
                self.builder.token(SyntaxKind::NULL.into(), "");
                self.builder.finish_node();
                self.builder.finish_node();
            }

            // Finish the MAPPING_ENTRY node
            self.builder.finish_node();

            self.skip_ws_and_newlines();

            // Check if there are more entries
            if self.current() != Some(SyntaxKind::QUESTION) && !self.is_mapping_key() {
                break;
            }
        }

        // Continue parsing regular mapping entries if any
        while self.current().is_some() && self.is_mapping_key() {
            let tokens_before_iter = self.tokens.len();
            // is_mapping_key() returns true for QUESTION, but
            // parse_mapping_key_value_pair does not consume a `?` key - that
            // would loop forever. Re-enter explicit-key handling for `?`.
            if self.current() == Some(SyntaxKind::QUESTION) {
                self.parse_explicit_key_entries();
                break;
            }
            self.parse_mapping_key_value_pair(0);
            self.skip_ws_and_newlines();
            // Progress guard against any future case where the body consumes
            // nothing (e.g. recovery via synthetic-token insertion).
            if self.tokens.len() == tokens_before_iter {
                let unexpected = self.current_text().unwrap_or("").to_string();
                self.add_error(
                    format!("Unexpected token in explicit-key mapping: {unexpected:?}"),
                    ParseErrorKind::Other,
                );
                self.bump();
            }
        }

        self.builder.finish_node();
    }

    fn parse_complex_key_mapping(&mut self) {
        // Parse mapping where the key is a complex structure (sequence or mapping)
        self.builder.start_node(SyntaxKind::MAPPING.into());

        // Start a MAPPING_ENTRY to wrap this key-value pair
        self.builder.start_node(SyntaxKind::MAPPING_ENTRY.into());

        // Parse the complex key
        self.builder.start_node(SyntaxKind::KEY.into());
        if self.current() == Some(SyntaxKind::LEFT_BRACKET) {
            self.parse_flow_sequence();
        } else if self.current() == Some(SyntaxKind::LEFT_BRACE) {
            self.parse_flow_mapping();
        }
        self.builder.finish_node();

        self.skip_ws_and_newlines(); // Allow newlines between key and colon

        // Expect colon
        if self.current() == Some(SyntaxKind::COLON) {
            self.bump();
            self.skip_whitespace();

            // Parse value
            self.builder.start_node(SyntaxKind::VALUE.into());
            if self.current().is_some() && self.current() != Some(SyntaxKind::NEWLINE) {
                self.parse_value();
            } else if self.current() == Some(SyntaxKind::NEWLINE) {
                self.bump(); // consume newline
                if self.current() == Some(SyntaxKind::INDENT) {
                    self.bump(); // consume indent
                    self.parse_value();
                }
            }
            self.builder.finish_node();
        } else {
            let error_msg = self.create_detailed_error(
                "Missing colon in complex mapping",
                "':' after complex key",
                self.current_text(),
            );
            self.add_error_and_recover(error_msg, SyntaxKind::COLON, ParseErrorKind::Other);
        }

        // Finish the first MAPPING_ENTRY node
        self.builder.finish_node();

        self.skip_ws_and_newlines();

        // Continue parsing more entries if they exist
        while self.current().is_some() {
            let tokens_before_iter = self.tokens.len();
            if self.current() == Some(SyntaxKind::QUESTION) {
                // Switch to explicit key parsing
                self.parse_explicit_key_entries();
                break;
            } else if self.is_complex_mapping_key()
                || (self.is_mapping_key() && self.current() != Some(SyntaxKind::QUESTION))
            {
                // Start a MAPPING_ENTRY for this additional entry
                self.builder.start_node(SyntaxKind::MAPPING_ENTRY.into());

                // Parse another entry
                self.builder.start_node(SyntaxKind::KEY.into());

                if self.current() == Some(SyntaxKind::LEFT_BRACKET) {
                    self.parse_flow_sequence();
                } else if self.current() == Some(SyntaxKind::LEFT_BRACE) {
                    self.parse_flow_mapping();
                } else if matches!(
                    self.current(),
                    Some(
                        SyntaxKind::STRING
                            | SyntaxKind::INT
                            | SyntaxKind::FLOAT
                            | SyntaxKind::BOOL
                            | SyntaxKind::NULL
                            | SyntaxKind::MERGE_KEY
                    )
                ) {
                    self.bump();
                }
                self.builder.finish_node();

                self.skip_whitespace();

                if self.current() == Some(SyntaxKind::COLON) {
                    self.bump();
                    self.skip_whitespace();

                    self.builder.start_node(SyntaxKind::VALUE.into());
                    if self.current().is_some() && self.current() != Some(SyntaxKind::NEWLINE) {
                        self.parse_value();
                    } else if self.current() == Some(SyntaxKind::NEWLINE) {
                        self.bump();
                        if self.current() == Some(SyntaxKind::INDENT) {
                            self.bump();
                            self.parse_value();
                        }
                    }
                    self.builder.finish_node();
                }

                // Finish the MAPPING_ENTRY node
                self.builder.finish_node();

                self.skip_ws_and_newlines();
            } else {
                break;
            }

            // Progress guard: if is_mapping_key() returned true but nothing
            // consumed the current token (e.g. `]:` at top level), break to
            // avoid an infinite loop.
            if self.tokens.len() == tokens_before_iter {
                let unexpected = self.current_text().unwrap_or("").to_string();
                self.add_error(
                    format!("Unexpected token in complex mapping: {unexpected:?}"),
                    ParseErrorKind::Other,
                );
                self.bump();
            }
        }

        self.builder.finish_node();
    }

    fn parse_explicit_key_entries(&mut self) {
        // Helper to continue parsing explicit key entries within a mapping
        while self.current() == Some(SyntaxKind::QUESTION) {
            // Start a MAPPING_ENTRY to wrap this key-value pair
            self.builder.start_node(SyntaxKind::MAPPING_ENTRY.into());

            self.bump(); // consume '?'
            self.skip_whitespace();

            self.builder.start_node(SyntaxKind::KEY.into());
            if self.current().is_some() && self.current() != Some(SyntaxKind::NEWLINE) {
                self.parse_value();
            }
            self.builder.finish_node();

            self.skip_ws_and_newlines();

            if self.current() == Some(SyntaxKind::COLON) {
                self.bump();
                self.skip_whitespace();

                self.builder.start_node(SyntaxKind::VALUE.into());
                if self.current().is_some() && self.current() != Some(SyntaxKind::NEWLINE) {
                    self.parse_value();
                } else if self.current() == Some(SyntaxKind::NEWLINE) {
                    self.bump();
                    if self.current() == Some(SyntaxKind::INDENT) {
                        self.bump();
                        self.parse_value();
                    }
                }
                self.builder.finish_node();
            } else {
                // No value, just a key - create explicit null value
                self.builder.start_node(SyntaxKind::VALUE.into());
                self.builder.start_node(SyntaxKind::SCALAR.into());
                self.builder.token(SyntaxKind::NULL.into(), "");
                self.builder.finish_node();
                self.builder.finish_node();
            }

            // Finish the MAPPING_ENTRY node
            self.builder.finish_node();

            self.skip_ws_and_newlines();
        }
    }

    fn is_complex_mapping_key(&self) -> bool {
        // Check if a flow sequence or mapping is used as a key
        if !matches!(
            self.current(),
            Some(SyntaxKind::LEFT_BRACKET | SyntaxKind::LEFT_BRACE)
        ) {
            return false;
        }

        // Look ahead to find matching closing bracket/brace and then check for colon
        let mut depth = 0;
        let start_kind = self.current();
        let close_kind = match start_kind {
            Some(SyntaxKind::LEFT_BRACKET) => SyntaxKind::RIGHT_BRACKET,
            Some(SyntaxKind::LEFT_BRACE) => SyntaxKind::RIGHT_BRACE,
            _ => return false,
        };

        let mut found_close = false;
        for kind in self.upcoming_tokens() {
            if !found_close {
                if Some(kind) == start_kind {
                    depth += 1;
                } else if kind == close_kind {
                    if depth == 0 {
                        // Found matching close
                        found_close = true;
                    } else {
                        depth -= 1;
                    }
                }
            } else {
                // We've found the closing bracket/brace, now look for colon
                match kind {
                    SyntaxKind::WHITESPACE | SyntaxKind::INDENT => continue,
                    SyntaxKind::COLON => return true,
                    _ => return false,
                }
            }
        }
        false
    }

    fn parse_mapping_value(&mut self) {
        // When parsing the value part of a mapping, be more conservative about
        // interpreting content as nested mappings. Only parse as mapping if
        // it's clearly a structured value, otherwise parse as scalar.
        match self.current() {
            Some(SyntaxKind::DASH) if !self.in_flow_context => self.parse_sequence(),
            Some(SyntaxKind::ANCHOR) => {
                self.bump(); // consume and emit anchor token to CST
                self.skip_whitespace();
                self.parse_value_with_base_indent(0);
            }
            Some(SyntaxKind::REFERENCE) => self.parse_alias(),
            Some(SyntaxKind::TAG) => self.parse_tagged_value(),
            Some(SyntaxKind::QUESTION) => {
                // Explicit key indicator - parse complex mapping
                self.parse_explicit_key_mapping();
            }
            Some(SyntaxKind::PIPE) => self.parse_literal_block_scalar(),
            Some(SyntaxKind::GREATER) => self.parse_folded_block_scalar(),
            Some(SyntaxKind::LEFT_BRACKET) => {
                // Check if this is a complex key in a mapping
                if !self.in_flow_context && self.is_complex_mapping_key() {
                    self.parse_complex_key_mapping();
                } else {
                    self.parse_flow_sequence();
                }
            }
            Some(SyntaxKind::LEFT_BRACE) => {
                // Check if this is a complex key in a mapping
                if !self.in_flow_context && self.is_complex_mapping_key() {
                    self.parse_complex_key_mapping();
                } else {
                    self.parse_flow_mapping();
                }
            }
            _ => {
                // For all other cases in mapping values, parse as scalar
                // This handles URLs and other complex scalar values containing colons
                self.parse_scalar();
            }
        }
    }

    fn is_mapping_key(&self) -> bool {
        // Check if this is an explicit key indicator
        if self.current() == Some(SyntaxKind::QUESTION) {
            return true;
        }

        // Check if this is a merge key
        if self.current() == Some(SyntaxKind::MERGE_KEY) {
            return true;
        }

        // If current token is a dash, this is not a mapping key
        if self.current() == Some(SyntaxKind::DASH) {
            return false;
        }

        // Look ahead to see if there's a colon after the current token.
        // Plain scalars can contain spaces, so a key may span multiple scalar
        // tokens separated by whitespace before the terminating colon
        // (e.g. `abc cba: value`).
        //
        // Any number of leading TAG / ANCHOR tokens annotate the key
        // (`!!str &a1 "foo":`); skip past them and any WHITESPACE, then
        // apply the usual scan.
        let mut saw_scalar = false;
        for kind in self.upcoming_tokens() {
            if kind == SyntaxKind::COLON {
                return true;
            }
            if !saw_scalar && matches!(kind, SyntaxKind::TAG | SyntaxKind::ANCHOR) {
                continue;
            }
            if is_plain_scalar_kind(kind) {
                saw_scalar = true;
                continue;
            }
            if kind == SyntaxKind::WHITESPACE {
                continue;
            }
            return false;
        }
        false
    }

    fn skip_whitespace(&mut self) {
        self.skip_tokens(&[SyntaxKind::WHITESPACE]);
    }

    fn skip_tokens(&mut self, kinds: &[SyntaxKind]) {
        while let Some(current) = self.current() {
            if kinds.contains(&current) {
                self.bump();
            } else {
                break;
            }
        }
    }

    /// Check if a plain scalar continues on the next line after a NEWLINE
    /// This looks ahead to see if the next line has content at greater indentation
    fn is_plain_scalar_continuation(&self, scalar_indent: usize) -> bool {
        // Current token should be NEWLINE. Peek ahead to see what follows.
        // Tokens are in reverse order, so we look at earlier indices (closer to front)
        let current_idx = self.tokens.len().saturating_sub(1);

        if current_idx == 0 {
            return false; // No more tokens
        }

        // Look at tokens after the NEWLINE
        // Since tokens are reversed, indices before current_idx are "ahead" in the stream
        let mut peek_idx = current_idx.saturating_sub(1);

        // Skip INDENT token if present and extract indentation level
        let next_line_indent = self
            .tokens
            .get(peek_idx)
            .and_then(|(kind, text)| {
                if *kind == SyntaxKind::INDENT {
                    peek_idx = peek_idx.saturating_sub(1);
                    Some(text.len())
                } else {
                    None
                }
            })
            .unwrap_or(0);

        // Skip WHITESPACE tokens
        while self
            .tokens
            .get(peek_idx)
            .is_some_and(|(kind, _)| *kind == SyntaxKind::WHITESPACE)
        {
            peek_idx = peek_idx.saturating_sub(1);
        }

        // Check if we have content token using safe get()
        let has_content = self.tokens.get(peek_idx).is_some_and(|(kind, _)| {
            matches!(
                kind,
                SyntaxKind::STRING
                    | SyntaxKind::INT
                    | SyntaxKind::FLOAT
                    | SyntaxKind::BOOL
                    | SyntaxKind::NULL
                    | SyntaxKind::UNTERMINATED_STRING
            )
        });

        if !has_content || next_line_indent <= scalar_indent {
            return false;
        }

        // Check if the next line is a mapping key (has a COLON after the content)
        // If so, it's not a continuation - it's a new mapping key
        if peek_idx > 0 {
            let mut check_idx = peek_idx.saturating_sub(1);

            // Skip any whitespace after the content
            while self
                .tokens
                .get(check_idx)
                .is_some_and(|(kind, _)| *kind == SyntaxKind::WHITESPACE)
            {
                if check_idx == 0 {
                    break;
                }
                check_idx = check_idx.saturating_sub(1);
            }

            // If we find a COLON, this is a mapping key, not a scalar continuation
            if self
                .tokens
                .get(check_idx)
                .is_some_and(|(kind, _)| *kind == SyntaxKind::COLON)
            {
                return false;
            }
        }

        true
    }

    /// Check if the current position is dedented relative to base_indent.
    /// This is used when we encounter a token (like COMMENT) and need to check if it's dedented.
    /// Returns true if dedent detected.
    fn is_at_dedented_position(&self, base_indent: usize) -> bool {
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
    fn skip_whitespace_only_with_dedent_check(&mut self, base_indent: usize) -> bool {
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

    fn skip_ws_and_newlines(&mut self) {
        self.skip_tokens(&[
            SyntaxKind::WHITESPACE,
            SyntaxKind::NEWLINE,
            SyntaxKind::INDENT,
            SyntaxKind::COMMENT,
        ]);
    }

    fn parse_mapping_key_value_pair(&mut self, base_indent: usize) {
        // Start MAPPING_ENTRY node to wrap the entire key-value pair
        self.builder.start_node(SyntaxKind::MAPPING_ENTRY.into());

        // Parse regular key
        self.builder.start_node(SyntaxKind::KEY.into());

        // Absorb any number of TAG / ANCHOR annotations preceding the
        // key scalar (`&anchor a:`, `!!str foo:`, `!!str &a1 "foo":`).
        while matches!(self.current(), Some(SyntaxKind::ANCHOR | SyntaxKind::TAG)) {
            self.bump(); // consume tag or anchor token
            self.skip_whitespace();
        }

        if self.current() == Some(SyntaxKind::MERGE_KEY) {
            self.builder.start_node(SyntaxKind::SCALAR.into());
            self.bump(); // consume the merge key token
            self.builder.finish_node(); // SCALAR
        } else if self.current() == Some(SyntaxKind::REFERENCE) {
            // Handle alias as key (*b:)
            self.parse_alias();
        } else if self.current().is_some_and(is_plain_scalar_kind) {
            self.builder.start_node(SyntaxKind::SCALAR.into());
            self.bump();
            // Plain scalars can contain spaces, so absorb any following
            // whitespace + scalar tokens until we reach the terminating colon
            // (e.g. `abc cba: value`).
            while self.current() == Some(SyntaxKind::WHITESPACE)
                && self
                    .upcoming_tokens()
                    .next()
                    .is_some_and(is_plain_scalar_kind)
            {
                self.bump(); // WHITESPACE inside the plain scalar
                self.bump(); // next scalar segment
            }
            self.builder.finish_node(); // SCALAR
        }
        self.builder.finish_node(); // KEY

        self.skip_whitespace();

        // Expect colon
        if self.current() == Some(SyntaxKind::COLON) {
            self.bump();
            self.skip_whitespace();

            // Parse value - wrap in VALUE node
            self.builder.start_node(SyntaxKind::VALUE.into());
            let mut has_value = false;
            if self.current().is_some()
                && self.current() != Some(SyntaxKind::NEWLINE)
                && self.current() != Some(SyntaxKind::COMMENT)
            {
                // Inline value on the same line as the colon
                self.parse_mapping_value();
                has_value = true;

                // Capture any trailing whitespace and comment on the same line (before NEWLINE)
                // This keeps inline comments like "value  # comment" together in the VALUE node
                if self.current() == Some(SyntaxKind::WHITESPACE) {
                    self.bump(); // emit whitespace inside VALUE
                }
                if self.current() == Some(SyntaxKind::COMMENT) {
                    self.bump(); // emit inline comment inside VALUE
                }
            } else if self.current() == Some(SyntaxKind::COMMENT) {
                // Comment after colon with no inline value
                // The comment belongs to the VALUE, and any indented content after it
                // also belongs to this VALUE (e.g., "key:  # comment\n  nested: value")
                self.bump(); // consume comment inside VALUE

                if self.current() == Some(SyntaxKind::NEWLINE) {
                    self.bump(); // consume newline inside VALUE

                    if self.current() == Some(SyntaxKind::INDENT) {
                        let indent_level = self.tokens.last().map_or(0, |(_, text)| text.len());
                        self.bump(); // consume indent inside VALUE
                                     // Parse the indented content as part of this VALUE
                        self.parse_value_with_base_indent(indent_level);
                        has_value = true;
                    }
                }
                // If no indented content follows the comment, has_value stays false → implicit null
            } else if self.current() == Some(SyntaxKind::NEWLINE) {
                self.skip_ws_and_newlines();
                if self.current_line_indent > base_indent {
                    // Nested value is more indented than the enclosing mapping's
                    // base indent - belongs to this key.
                    self.parse_value_with_base_indent(self.current_line_indent);
                    has_value = true;
                } else if self.current_line_indent == base_indent
                    && self.current() == Some(SyntaxKind::DASH)
                {
                    // Zero-indented sequence (same indentation as key)
                    // This is valid YAML: the sequence is the value for the key
                    self.parse_sequence_with_base_indent(base_indent);
                    has_value = true;
                }
                // Otherwise the "value" would be at the parent's indent or
                // less, so this key has an implicit null value and what
                // follows is a sibling entry.
            }

            // If no value present, create an implicit null scalar
            if !has_value {
                self.builder.start_node(SyntaxKind::SCALAR.into());
                self.builder.token(SyntaxKind::NULL.into(), "");
                self.builder.finish_node();
            }

            self.builder.finish_node(); // VALUE
        } else {
            let error_msg = self.create_detailed_error(
                "Missing colon in mapping",
                "':' after key",
                self.current_text(),
            );
            self.add_error_and_recover(error_msg, SyntaxKind::COLON, ParseErrorKind::Other);
        }

        // Consume any trailing inline whitespace before closing MAPPING_ENTRY
        // Note: Inline comments are consumed within the VALUE node itself.
        // Any COMMENT token here would be on a separate line and should not
        // be consumed as part of this entry (it may be dedented).
        while self.current() == Some(SyntaxKind::WHITESPACE) {
            self.bump();
        }

        // Block-style entries own their NEWLINE terminator (DESIGN.md)
        if self.current() == Some(SyntaxKind::NEWLINE) {
            self.bump();
        }

        // Finish MAPPING_ENTRY node
        self.builder.finish_node();
    }

    fn bump(&mut self) {
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

    fn current(&self) -> Option<SyntaxKind> {
        self.tokens.last().map(|(kind, _)| *kind)
    }

    fn current_text(&self) -> Option<&str> {
        self.tokens.last().map(|(_, text)| text.as_str())
    }

    /// Iterator over upcoming tokens starting from the next token (not current)
    fn upcoming_tokens(&self) -> impl Iterator<Item = SyntaxKind> + '_ {
        // Since tokens are in reverse order (last is current), we need to iterate
        // from the second-to-last token backwards to the beginning
        let len = self.tokens.len();
        (0..len.saturating_sub(1))
            .rev()
            .map(move |i| self.tokens[i].0)
    }

    fn add_error(&mut self, message: String, kind: ParseErrorKind) {
        // Create positioned error with line/column info
        let token_len = self.current_text().map_or(1, |s| s.len());
        let positioned_error = self.error_context.create_error(message, token_len, kind);

        self.errors.push(positioned_error.message.clone());
        self.positioned_errors.push(positioned_error);
    }

    /// Add an error with recovery
    fn add_error_and_recover(
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
    fn create_detailed_error(
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
