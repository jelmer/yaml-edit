//! Flow-style collection parsing (`{...}` mappings and `[...]` sequences)
//! and the shared "is the next flow element an implicit mapping?"
//! lookahead used to distinguish `[key: value]` from `[value]`.
//!
//! Split out of `parser/mod.rs` unchanged.

use super::{Parser, MAX_FLOW_DEPTH};
use crate::error_recovery::ParseContext;
use crate::lex::SyntaxKind;
use crate::ParseErrorKind;

impl Parser {
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
    pub(super) fn parse_flow_sequence(&mut self) {
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
    pub(super) fn parse_flow_mapping(&mut self) {
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
