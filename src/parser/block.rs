//! Block-style parsing: block mappings (`k: v` on separate lines),
//! block sequences (`- item`), explicit-key mappings (`? key\n: val`),
//! and complex-key mappings (flow collections used as keys).
//!
//! Split out of `parser/mod.rs` unchanged.

use super::scalars::is_plain_scalar_kind;
use super::Parser;
use crate::error_recovery::ParseContext;
use crate::lex::SyntaxKind;
use crate::ParseErrorKind;

impl Parser {
    pub(super) fn parse_mapping_with_base_indent(&mut self, base_indent: usize) {
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

    pub(super) fn parse_sequence_with_base_indent(&mut self, base_indent: usize) {
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
    pub(super) fn parse_explicit_key_mapping(&mut self) {
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

    pub(super) fn parse_complex_key_mapping(&mut self) {
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

    pub(super) fn is_complex_mapping_key(&self) -> bool {
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

    pub(super) fn is_mapping_key(&self) -> bool {
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
}
