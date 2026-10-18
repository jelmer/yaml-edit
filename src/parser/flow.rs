//! Flow-style collection parsing (`{...}` mappings and `[...]` sequences)
//! and the shared "is the next flow element an implicit mapping?"
//! lookahead used to distinguish `[key: value]` from `[value]`.

use super::block::reaches_key_colon;
use super::scalars::is_plain_scalar_kind;
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
        // A `?` opens a single pair with an explicit key, `:` or not.
        if self.current() == Some(SyntaxKind::QUESTION) {
            return true;
        }
        // Chain current token with upcoming tokens (no allocation needed)
        let tokens = std::iter::once(self.current().unwrap_or(SyntaxKind::EOF))
            .chain(self.upcoming_tokens());
        has_implicit_mapping_pattern(tokens)
    }

    /// Keep plain scalar content between a complete key node and its colon,
    /// as in `{*a x: v}` or `[[q] x: v]`, in the KEY as a second node, which
    /// the validator reports, as a block mapping key does.
    fn parse_flow_content_before_colon(&mut self) {
        let whitespace = usize::from(self.current() == Some(SyntaxKind::WHITESPACE));
        let rest = std::iter::once(self.current().unwrap_or(SyntaxKind::EOF))
            .chain(self.upcoming_tokens())
            .skip(whitespace);
        if !self.peek_kind(whitespace).is_some_and(is_plain_scalar_kind) || !reaches_key_colon(rest)
        {
            return;
        }
        if whitespace == 1 {
            self.bump();
        }
        self.parse_scalar();
    }

    /// Parse an implicit flow mapping (key: value without braces).
    /// Used inside flow sequences: [ key: value ] is valid YAML.
    fn parse_implicit_flow_mapping(&mut self) {
        self.builder.start_node(SyntaxKind::MAPPING.into());
        self.builder.start_node(SyntaxKind::MAPPING_ENTRY.into());

        // Parse key; a missing one (`[: v]`, `[? : v]`) is an implicit
        // null, and the `?` of an explicit key goes inside KEY, both as in a
        // flow mapping.
        self.builder.start_node(SyntaxKind::KEY.into());
        if self.current() == Some(SyntaxKind::QUESTION) {
            self.bump();
            self.skip_whitespace();
        }
        if matches!(
            self.current(),
            Some(SyntaxKind::COLON | SyntaxKind::COMMA | SyntaxKind::RIGHT_BRACKET)
        ) {
            self.emit_implicit_null();
        } else {
            self.parse_value();
            self.parse_flow_content_before_colon();
        }
        self.builder.finish_node();

        self.skip_ws_and_newlines();

        // Consume colon
        if self.current() == Some(SyntaxKind::COLON) {
            self.bump();
            self.skip_whitespace();
        }

        // An omitted value is the zero-width implicit-null scalar, placed
        // where a value would start so that a comment or line break after
        // it follows the VALUE as it does for any other value.
        if self.at_flow_entry_end(SyntaxKind::RIGHT_BRACKET) {
            self.emit_implicit_null_value();
        } else {
            self.skip_ws_and_newlines();
            self.builder.start_node(SyntaxKind::VALUE.into());
            self.parse_value();
            self.builder.finish_node();
        }

        self.builder.finish_node(); // MAPPING_ENTRY
        self.builder.finish_node(); // MAPPING
    }
    /// Bail out of a flow collection nested past [`MAX_FLOW_DEPTH`].
    ///
    /// Reports the error, consumes up to `closer` to recover, and closes the
    /// node. Returns true when the caller should return without recursing.
    fn flow_depth_exceeded(&mut self, closer: SyntaxKind) -> bool {
        if self.flow_depth < MAX_FLOW_DEPTH {
            return false;
        }
        self.add_error(
            format!("Flow collection nested too deeply (limit {MAX_FLOW_DEPTH})"),
            ParseErrorKind::Other,
        );
        while let Some(kind) = self.current() {
            if matches!(kind, SyntaxKind::DOC_START | SyntaxKind::DOC_END) || kind == closer {
                break;
            }
            self.bump();
        }
        if self.current() == Some(closer) {
            self.bump();
        }
        self.builder.finish_node();
        self.error_context.pop_context();
        true
    }

    /// Guarantee the flow loop makes progress.
    ///
    /// If nothing was consumed since `tokens_before` the loop would spin
    /// forever (a stray `}` inside `[...]`, say), so report the token and
    /// skip it. `what` names the collection for the message.
    fn flow_progress_guard(&mut self, tokens_before: usize, what: &str) {
        if self.tokens.len() != tokens_before {
            return;
        }
        let unexpected = self.current_text().unwrap_or("").to_string();
        self.add_error(
            format!("Unexpected token in flow {what}: {unexpected:?}"),
            ParseErrorKind::Other,
        );
        self.bump();
    }

    pub(super) fn parse_flow_sequence(&mut self) {
        self.parse_flow_collection(SyntaxKind::RIGHT_BRACKET, Self::parse_flow_sequence_entry);
    }

    pub(super) fn parse_flow_mapping(&mut self) {
        self.parse_flow_collection(SyntaxKind::RIGHT_BRACE, Self::parse_flow_mapping_entry);
    }

    /// Parse the flow collection that `closer` ends, calling `entry` for
    /// each entry until it returns false or the collection closes.
    fn parse_flow_collection(&mut self, closer: SyntaxKind, entry: fn(&mut Self) -> bool) {
        let (node, context, what, expected, error_kind) = match closer {
            SyntaxKind::RIGHT_BRACKET => (
                SyntaxKind::SEQUENCE,
                ParseContext::FlowSequence,
                "sequence",
                "']' to close sequence",
                ParseErrorKind::UnclosedFlowSequence,
            ),
            _ => (
                SyntaxKind::MAPPING,
                ParseContext::FlowMapping,
                "mapping",
                "'}' to close mapping",
                ParseErrorKind::UnclosedFlowMapping,
            ),
        };
        self.builder.start_node(node.into());
        self.error_context.push_context(context);

        if self.flow_depth_exceeded(closer) {
            return;
        }
        self.flow_depth += 1;

        self.bump(); // consume the opening bracket
        self.skip_ws_and_newlines(); // Support comments and newlines in flow collections

        let prev_flow = std::mem::replace(&mut self.in_flow_context, true);

        while self.current() != Some(closer) && self.current().is_some() {
            let tokens_before = self.tokens.len();
            if !entry(self) {
                break;
            }
            // Guarantee progress: if no token was consumed this iteration we
            // would loop forever (e.g. on stray `}` inside `[...]`). Report
            // the unexpected token and skip it.
            self.flow_progress_guard(tokens_before, what);
        }

        self.in_flow_context = prev_flow;

        if self.current() == Some(closer) {
            self.bump();
        } else {
            let error_msg = self.create_detailed_error(
                &format!("Unclosed flow {what}"),
                expected,
                self.current_text(),
            );
            self.add_error_and_recover(error_msg, closer, error_kind);
        }

        self.flow_depth -= 1;
        self.builder.finish_node();
        self.error_context.pop_context();
    }

    /// Step over the comments and line breaks after a flow entry, and the
    /// comma terminating it, which the entry owns, then close the entry.
    fn finish_flow_entry(&mut self) {
        self.skip_ws_and_newlines(); // Support comments after values
        if self.current() == Some(SyntaxKind::COMMA) {
            self.bump();
            self.skip_ws_and_newlines(); // Support comments after commas
        }
        self.builder.finish_node();
    }

    /// Parse one entry of a flow sequence, returning false when the tokens
    /// after it show the sequence was left without a closing bracket.
    fn parse_flow_sequence_entry(&mut self) -> bool {
        self.builder.start_node(SyntaxKind::SEQUENCE_ENTRY.into());

        // Missing element (`[,]`, `[a, , c]`) is an implicit-null
        // scalar. Emit the same zero-width `SCALAR { NULL "" }` shape
        // block sequences and mappings use, so accessors index the
        // same set of entries as mutators.
        if matches!(
            self.current(),
            Some(SyntaxKind::COMMA) | Some(SyntaxKind::RIGHT_BRACKET)
        ) {
            self.emit_implicit_null();
        } else if self.next_flow_element_is_implicit_mapping() {
            // Per YAML spec, `[ key: value ]` is valid: a sequence
            // containing an implicit single-pair mapping.
            self.parse_implicit_flow_mapping();
        } else {
            self.parse_value();
        }

        self.finish_flow_entry();

        // These tokens indicate we've left the flow sequence context or hit
        // invalid syntax.
        !matches!(
            self.current(),
            Some(SyntaxKind::DASH | SyntaxKind::DOC_START | SyntaxKind::DOC_END)
        )
    }

    /// Parse one entry of a flow mapping, returning false without consuming
    /// anything when the mapping was left without a closing brace.
    fn parse_flow_mapping_entry(&mut self) -> bool {
        // These tokens indicate we've exited the flow mapping or hit invalid
        // syntax.
        if matches!(
            self.current(),
            Some(SyntaxKind::DASH | SyntaxKind::DOC_START | SyntaxKind::DOC_END)
        ) {
            return false;
        }

        self.builder.start_node(SyntaxKind::MAPPING_ENTRY.into());

        // Parse key - wrap in KEY node
        self.builder.start_node(SyntaxKind::KEY.into());

        // Handle explicit key indicator (?) in flow context
        if self.current() == Some(SyntaxKind::QUESTION) {
            self.bump(); // consume '?'
            self.skip_whitespace();
        }

        // Missing key (`{,}`, `{: v}`, `{:}`) is an implicit-null
        // scalar. Emit the same zero-width `SCALAR { NULL "" }` shape
        // used everywhere else so `KEY` always contains one value node.
        if matches!(
            self.current(),
            Some(SyntaxKind::COMMA) | Some(SyntaxKind::COLON) | Some(SyntaxKind::RIGHT_BRACE)
        ) {
            self.emit_implicit_null();
        } else {
            self.parse_value();
            self.parse_flow_content_before_colon();
        }
        self.builder.finish_node();

        // An omitted value (`{key}`, `{key:}`) is a VALUE holding the
        // implicit-null scalar. It goes where a value would start, right
        // after the key or after the colon and the spaces following it,
        // so a comment or line break before the `,` or `}` follows the
        // VALUE as it does for any other value.
        if self.at_flow_entry_end(SyntaxKind::RIGHT_BRACE) {
            self.emit_implicit_null_value();
        } else if self.next_significant_kind() == Some(SyntaxKind::COLON) {
            self.skip_ws_and_newlines(); // Support comments after keys
            self.bump();
            self.skip_whitespace();
            if self.at_flow_entry_end(SyntaxKind::RIGHT_BRACE) {
                self.emit_implicit_null_value();
            } else {
                self.skip_ws_and_newlines(); // Support comments after colons
                self.builder.start_node(SyntaxKind::VALUE.into());
                self.parse_value();
                self.builder.finish_node();
            }
        } else {
            self.emit_implicit_null_value();
        }

        self.finish_flow_entry();
        true
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
