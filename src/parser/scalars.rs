//! Scalar parsing: aliases, plain / quoted / block scalars, and the
//! tagged-collection wrappers (`!!set`, `!!omap`, `!!pairs`).
//!
//! Split out of `parser/mod.rs` unchanged.

use super::Parser;
use crate::lex::SyntaxKind;
use crate::ParseErrorKind;

impl Parser {
    pub(super) fn parse_alias(&mut self) {
        // Create an alias node and consume the reference token
        // The token itself already contains the full "*alias_name" text
        self.builder.start_node(SyntaxKind::ALIAS.into());
        if self.current() == Some(SyntaxKind::REFERENCE) {
            self.bump(); // This preserves the original "*alias_name" token
        }
        self.builder.finish_node();
    }

    pub(super) fn parse_scalar(&mut self) {
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

    pub(super) fn parse_tagged_value(&mut self) {
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

    pub(super) fn parse_literal_block_scalar(&mut self) {
        self.builder.start_node(SyntaxKind::SCALAR.into());
        self.bump(); // consume PIPE
        self.parse_block_scalar_header();
        self.parse_block_scalar_content();
        self.builder.finish_node();
    }

    pub(super) fn parse_folded_block_scalar(&mut self) {
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
}

/// Kinds emitted by the lexer for plain (unquoted) scalar content.
pub(super) fn is_plain_scalar_kind(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::STRING
            | SyntaxKind::INT
            | SyntaxKind::FLOAT
            | SyntaxKind::BOOL
            | SyntaxKind::NULL
    )
}
