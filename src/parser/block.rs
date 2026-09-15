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
    /// Consume comment lines sitting between entries of a block collection.
    ///
    /// Returns true when the comments ran into a dedent, meaning the
    /// collection has ended and the caller should stop.
    ///
    /// At root level (`base_indent == 0`) every comment belongs to this
    /// collection, indented or not, because there is no enclosing scope.
    fn absorb_entry_comments(&mut self, base_indent: usize) -> bool {
        while self.current() == Some(SyntaxKind::COMMENT) {
            if base_indent > 0 && self.is_at_dedented_position(base_indent) {
                return true;
            }
            self.bump();
            if self.current() == Some(SyntaxKind::NEWLINE) {
                self.bump();
            }
            if self.skip_whitespace_only_with_dedent_check(base_indent) {
                return true;
            }
        }
        false
    }

    pub(super) fn parse_mapping_with_base_indent(&mut self, base_indent: usize) {
        // Entries inside a mapping are bounded by their key's column.
        self.equal_indent_continues_scalar = false;
        self.builder.start_node(SyntaxKind::MAPPING.into());
        self.error_context.push_context(ParseContext::Mapping);

        while self.current().is_some() {
            let tokens_before_iter = self.tokens.len();
            // Skip whitespace, break on dedent
            if self.skip_whitespace_only_with_dedent_check(base_indent) {
                break;
            }

            // Emit comments as children of MAPPING
            if self.absorb_entry_comments(base_indent) {
                break;
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
                } else {
                    // Bare `?\n` -- implicit-null key.
                    self.emit_implicit_null();
                }
                self.builder.finish_node();

                self.skip_ws_and_newlines();

                // Parse value if there's a colon
                if self.current() == Some(SyntaxKind::COLON) {
                    self.bump(); // consume ':'
                    self.skip_whitespace();

                    self.parse_value_after_colon(false);
                } else {
                    // No value, just a key - create explicit null value
                    self.emit_implicit_null_value();
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
        // A sequence entry's continuation only has to clear the sequence's
        // own column, which parse_value_with_base_indent already enforces.
        self.equal_indent_continues_scalar = true;
        self.builder.start_node(SyntaxKind::SEQUENCE.into());
        self.error_context.push_context(ParseContext::Sequence);

        while self.current().is_some() {
            // Skip whitespace, break on dedent
            if self.skip_whitespace_only_with_dedent_check(base_indent) {
                break;
            }

            // Emit comments as children of SEQUENCE
            if self.absorb_entry_comments(base_indent) {
                break;
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

            // The dash's true column, read before bump() consumes it, and
            // zero-based to match the indents it is compared against.
            // current_line_indent counts leading whitespace only, so it reads
            // 0 for the `-` of an explicit key (`? - a`), whose entries then
            // look indented past their own sequence.
            let dash_column = self.error_context.current_location().1.saturating_sub(1);
            self.bump(); // consume dash
            self.skip_whitespace();

            // Record the dash's line indentation for the item value parsing
            let item_indent = self.current_line_indent;

            // A `-` on a later line continues this entry's scalar when it is
            // indented past our own dash, and opens the next entry when it is
            // not. The dash column is the only thing that tells those apart.
            let outer_entry_column = self.sequence_entry_column;
            self.sequence_entry_column = Some(dash_column);

            if self.current().is_some() && self.current() != Some(SyntaxKind::NEWLINE) {
                // Use item's line indent so nested mappings parse at the right level
                self.parse_value_with_base_indent(item_indent);
            } else if self.current() == Some(SyntaxKind::NEWLINE) {
                // Nested content is a NEWLINE then INDENT. A bare `-` item is
                // an implicit null; leave the NEWLINE for the terminator bump
                // so set/remove see DASH, SCALAR, NEWLINE in that order.
                if self.upcoming_tokens().next() == Some(SyntaxKind::INDENT) {
                    self.bump(); // consume newline
                    let indent_level = self.tokens.last().map_or(0, |(_, text)| text.len());
                    self.bump(); // consume indent
                    self.parse_value_with_base_indent(indent_level);
                } else {
                    self.emit_implicit_null();
                }
            } else {
                self.emit_implicit_null();
            }

            self.sequence_entry_column = outer_entry_column;

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
    pub(super) fn parse_explicit_key_mapping(&mut self, base_indent: usize) {
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
            } else {
                // Bare `?\n` -- implicit-null key.
                self.emit_implicit_null();
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

                self.parse_value_after_colon(true);
            } else {
                // No value, just a key - create explicit null value
                self.emit_implicit_null_value();
            }

            // Finish the MAPPING_ENTRY node
            self.builder.finish_node();

            self.skip_ws_and_newlines();

            // A following line at a shallower column closes this mapping --
            // it belongs to an enclosing one, not here.
            if self.is_at_dedented_position(base_indent) {
                break;
            }

            // Check if there are more entries
            if self.current() != Some(SyntaxKind::QUESTION) && !self.is_mapping_key() {
                break;
            }
        }

        // Continue parsing regular mapping entries if any
        while self.current().is_some()
            && !self.is_at_dedented_position(base_indent)
            && self.is_mapping_key()
        {
            let tokens_before_iter = self.tokens.len();
            // is_mapping_key() returns true for QUESTION, but
            // parse_mapping_key_value_pair does not consume a `?` key - that
            // would loop forever. Re-enter explicit-key handling for `?`.
            if self.current() == Some(SyntaxKind::QUESTION) {
                self.parse_explicit_key_entries();
                break;
            }
            self.parse_mapping_key_value_pair(base_indent);
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
            self.parse_value_after_colon(false);
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
            } else {
                // Bare `?\n` -- implicit-null key.
                self.emit_implicit_null();
            }
            self.builder.finish_node();

            self.skip_ws_and_newlines();

            if self.current() == Some(SyntaxKind::COLON) {
                self.bump();
                self.skip_whitespace();

                self.parse_value_after_colon(false);
            } else {
                // No value, just a key - create explicit null value
                self.emit_implicit_null_value();
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

    fn parse_mapping_value(&mut self, base_indent: usize) {
        // When parsing the value part of a mapping, be more conservative about
        // interpreting content as nested mappings. Only parse as mapping if
        // it's clearly a structured value, otherwise parse as scalar.
        match self.current() {
            Some(SyntaxKind::DASH) if !self.in_flow_context => self.parse_sequence(),
            Some(SyntaxKind::REFERENCE) => self.parse_alias(),
            Some(SyntaxKind::TAG) => self.parse_tagged_value_as_mapping_value(base_indent),
            Some(SyntaxKind::QUESTION) => {
                // Explicit key indicator - parse complex mapping
                self.parse_explicit_key_mapping(base_indent);
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
            // Anchors annotate the value; they do not make a following block value inline.
            while self.current() == Some(SyntaxKind::ANCHOR) {
                self.bump();
                self.skip_whitespace();
            }
            if self.current() == Some(SyntaxKind::COMMENT) {
                self.bump();
            }
            let mut has_value = false;
            if self.current().is_some()
                && self.current() != Some(SyntaxKind::NEWLINE)
                && self.current() != Some(SyntaxKind::COMMENT)
            {
                // Inline value on the same line as the colon
                self.parse_mapping_value(base_indent);
                has_value = true;

                // Capture any trailing whitespace and comment on the same line (before NEWLINE)
                // This keeps inline comments like "value  # comment" together in the VALUE node
                if self.current() == Some(SyntaxKind::WHITESPACE) {
                    self.bump(); // emit whitespace inside VALUE
                }
                if self.current() == Some(SyntaxKind::COMMENT) {
                    self.bump(); // emit inline comment inside VALUE
                }
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
                self.emit_implicit_null();
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

#[cfg(test)]
mod tests {
    use crate::yaml::YamlFile;
    use std::str::FromStr;

    /// Top-level keys of `src`, in document order.
    fn top_level_keys(src: &str) -> Vec<String> {
        YamlFile::from_str(src)
            .expect("should parse")
            .document()
            .expect("should have a document")
            .as_mapping()
            .expect("root should be a mapping")
            .keys()
            .map(|k| k.to_string())
            .collect()
    }

    // Regression: a nested explicit-key block (`? k` / `: v`) used to swallow
    // whatever followed it. parse_explicit_key_mapping was the only
    // block-mapping path taking no base_indent, so after its entries it kept
    // consuming whatever is_mapping_key() accepted -- neither knowing about
    // columns -- and a key at the outer level was absorbed into the nested
    // mapping. The text still round-tripped byte-for-byte, so only the
    // structure was wrong.

    #[test]
    fn test_key_after_nested_explicit_key_block() {
        assert_eq!(top_level_keys("m:\n  ? a\n  : 1\nuf: v\n"), ["m", "uf"]);
    }

    #[test]
    fn test_key_after_multi_entry_nested_explicit_key_block() {
        assert_eq!(
            top_level_keys("m:\n  ? a\n  : 1\n  ? b\n  : 2\nuf: v\n"),
            ["m", "uf"]
        );
    }

    #[test]
    fn test_nested_explicit_key_block_followed_by_another_nested_mapping() {
        assert_eq!(
            top_level_keys("m:\n  ? a\n  : 1\nn:\n  ? b\n  : 2\n"),
            ["m", "n"]
        );
    }

    /// An explicit key whose value is itself a block: the value must be
    /// bounded by the column that opened it, not parsed at indent 0.
    #[test]
    fn test_key_after_explicit_key_with_block_value() {
        assert_eq!(
            top_level_keys("m:\n  ? a\n  : \n    n: 1\nz: 9\n"),
            ["m", "z"]
        );
    }

    #[test]
    fn test_key_after_deeply_indented_explicit_key_block() {
        assert_eq!(
            top_level_keys("m:\n      ? a\n      : 1\nz: 9\n"),
            ["m", "z"]
        );
    }

    #[test]
    fn test_explicit_key_block_mixed_with_plain_entries_stops_at_dedent() {
        assert_eq!(
            top_level_keys("m:\n  ? a\n  : 1\n  plain: 2\nouter: 3\n"),
            ["m", "outer"]
        );
    }

    #[test]
    fn test_explicit_key_block_nested_two_levels_deep() {
        assert_eq!(
            top_level_keys("a:\n  b:\n    ? c\n    : 1\n  d: 2\ne: 3\n"),
            ["a", "e"]
        );
    }

    /// A `!!set`-tagged explicit-key block is the same shape and must also
    /// release the key that follows it.
    #[test]
    fn test_key_after_tagged_explicit_key_set() {
        assert_eq!(
            top_level_keys("keys: !!set\n  ? a\n  ? b\nz: 9\n"),
            ["keys", "z"]
        );
    }

    /// The nested mapping must hold only its own content -- a key count alone
    /// would miss a value that absorbed the dedented sibling.
    #[test]
    fn test_nested_explicit_key_value_excludes_dedented_sibling() {
        let parsed = YamlFile::from_str("m:\n  ? a\n  : 1\nuf: v\n").expect("should parse");
        let doc = parsed.document().expect("should have a document");
        let mapping = doc.as_mapping().expect("root should be a mapping");

        let m = mapping.get("m").expect("m should be present");
        assert_eq!(m.to_string().trim_end(), "? a\n  : 1");

        let uf = mapping.get("uf").expect("uf should be present");
        assert_eq!(uf.to_string().trim(), "v");
    }

    /// Top-level explicit keys were never affected; this guards against a
    /// dedent check that is too eager.
    #[test]
    fn test_top_level_explicit_keys_are_unaffected() {
        assert_eq!(top_level_keys("? a\n: 1\nplain: v\n"), ["a", "plain"]);
    }
}
