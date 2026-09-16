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

        if matches!(
            self.current(),
            Some(SyntaxKind::QUOTE | SyntaxKind::SINGLE_QUOTE)
        ) {
            self.parse_quoted_scalar();
        } else if matches!(
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
            if self.current() == Some(SyntaxKind::UNTERMINATED_STRING) {
                self.add_error(
                    "Unterminated quoted string".to_string(),
                    ParseErrorKind::UnterminatedString,
                );
            }
            if self.in_flow_context {
                self.parse_flow_plain_scalar();
            } else {
                self.parse_block_plain_scalar();
            }
        } else {
            self.parse_untyped_scalar();
        }

        self.builder.finish_node();
    }

    /// Consume a `"..."` / `'...'` scalar whose quotes the lexer left as
    /// separate QUOTE tokens, reporting an unterminated one.
    fn parse_quoted_scalar(&mut self) {
        let quote_type = self
            .current()
            .expect("current token is Some: checked by the caller's matches! guard");
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
            self.add_error_and_recover(error_msg, quote_type, ParseErrorKind::UnterminatedString);
        }
    }

    /// Consume a plain scalar in block context.
    ///
    /// Per the YAML spec a plain scalar may continue on following lines as
    /// long as they are more indented than the line it started on; the
    /// newline is folded into the scalar. Comments and flow delimiters end
    /// it.
    fn parse_block_plain_scalar(&mut self) {
        // current_line_indent tracks the actual line indentation. For inline
        // scalars in sequence items (indent == 0 because the INDENT token was
        // already consumed) continuation must not be attempted: it cannot be
        // told apart from the next mapping key.
        let scalar_indent = self.current_line_indent;

        while let Some(kind) = self.current() {
            if kind == SyntaxKind::COMMENT {
                break;
            }

            if kind == SyntaxKind::NEWLINE {
                if self.is_plain_scalar_continuation(scalar_indent) {
                    self.bump(); // consume NEWLINE
                    while matches!(
                        self.current(),
                        Some(SyntaxKind::INDENT | SyntaxKind::WHITESPACE)
                    ) {
                        self.bump();
                    }
                    continue;
                }
                // Next line is not a continuation - stop here
                break;
            }

            // Stop at a flow collection delimiter inside a flow collection,
            // where it really delimits.
            //
            // Outside one it is ordinary scalar content, and the lexer has
            // already folded any that share a line with the scalar; reaching
            // here means we are on a continuation line, where `a\n}` is the
            // scalar `a }`, as both saphyr and PyYAML read it.
            if matches!(
                kind,
                SyntaxKind::LEFT_BRACKET
                    | SyntaxKind::LEFT_BRACE
                    | SyntaxKind::RIGHT_BRACKET
                    | SyntaxKind::RIGHT_BRACE
                    | SyntaxKind::COMMA
            ) && (self.flow_depth > 0 || self.is_complex_mapping_key())
            {
                break;
            }

            // Leave whitespace that precedes a comment to the comment itself
            if kind == SyntaxKind::WHITESPACE
                && self.peek_after_current() == Some(SyntaxKind::COMMENT)
            {
                break;
            }

            self.bump();
        }
    }

    /// Consume a plain scalar inside a flow collection.
    ///
    /// A quoted STRING arrives as one complete token and stops there.
    /// Otherwise keep reading multi-word and multi-line content until a flow
    /// delimiter, a comment, or a colon acting as a key separator.
    fn parse_flow_plain_scalar(&mut self) {
        let is_quoted_string = matches!(self.current(), Some(SyntaxKind::STRING))
            && self
                .current_text()
                .is_some_and(|text| text.starts_with('"') || text.starts_with('\''));

        self.bump(); // Consume the initial typed token

        if is_quoted_string {
            return;
        }

        while let Some(kind) = self.current() {
            // NEWLINE is not a terminator here: plain scalars span lines.
            if matches!(
                kind,
                SyntaxKind::COMMA
                    | SyntaxKind::RIGHT_BRACE
                    | SyntaxKind::RIGHT_BRACKET
                    | SyntaxKind::COMMENT
            ) {
                break;
            }

            if kind == SyntaxKind::NEWLINE {
                self.bump();
                while matches!(
                    self.current(),
                    Some(SyntaxKind::WHITESPACE | SyntaxKind::INDENT)
                ) {
                    self.bump();
                }
                continue;
            }

            // Whitespace before a delimiter ends the scalar (`[ a , b ]`);
            // whitespace between words is part of it (`{omitted value:,}`).
            if kind == SyntaxKind::WHITESPACE
                && matches!(
                    self.peek_after_current(),
                    Some(
                        SyntaxKind::COMMA
                            | SyntaxKind::RIGHT_BRACE
                            | SyntaxKind::RIGHT_BRACKET
                            | SyntaxKind::NEWLINE
                            | SyntaxKind::COMMENT
                    )
                )
            {
                break;
            }

            if kind == SyntaxKind::COLON && self.colon_is_flow_key_separator() {
                break;
            }

            self.bump();
        }
    }

    /// Consume a scalar the lexer gave no specific type, up to whatever
    /// structure ends it.
    fn parse_untyped_scalar(&mut self) {
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

            // A colon ends the scalar in block context, where it means mapping
            // structure. In flow context it is ordinary content (IPv6, URLs)
            // unless a delimiter follows, which makes it a key separator.
            if kind == SyntaxKind::COLON {
                if !self.in_flow_context {
                    break;
                }
                if self.colon_is_flow_key_separator() {
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

    /// The kind of the token after the current one, if any.
    ///
    /// `self.tokens` is a stack popped from the end, so the next-but-one
    /// token sits two from the top.
    fn peek_after_current(&self) -> Option<SyntaxKind> {
        if self.tokens.len() >= 2 {
            Some(self.tokens[self.tokens.len() - 2].0)
        } else {
            None
        }
    }

    /// Is the COLON at the cursor separating a flow mapping key from its
    /// value, rather than sitting inside a scalar?
    fn colon_is_flow_key_separator(&self) -> bool {
        matches!(
            self.peek_after_current(),
            Some(
                SyntaxKind::COMMA
                    | SyntaxKind::RIGHT_BRACE
                    | SyntaxKind::RIGHT_BRACKET
                    | SyntaxKind::WHITESPACE
                    | SyntaxKind::NEWLINE
            )
        )
    }

    pub(super) fn parse_tagged_value(&mut self, base_indent: usize) {
        self.parse_tagged_value_inner(base_indent, false)
    }

    /// As `parse_tagged_value`, for a tag in the value position of a block
    /// mapping entry.
    ///
    /// A block mapping cannot be an indentless value, so a `!!set` whose
    /// entries sit at the key's own column annotates an implicit null and
    /// those entries stay siblings of the key.
    pub(super) fn parse_tagged_value_as_mapping_value(&mut self, base_indent: usize) {
        self.parse_tagged_value_inner(base_indent, true)
    }

    fn parse_tagged_value_inner(&mut self, base_indent: usize, as_mapping_value: bool) {
        // Peek at the tag to determine what kind of collection to parse
        let tag_text = self.peek_tag_text();

        match tag_text {
            Some("!!set") => self.parse_tagged_set(base_indent, as_mapping_value),
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

                match self.tagged_block_node_indent(base_indent, as_mapping_value) {
                    Some(indent) => {
                        // The tag is alone on its line and annotates a block
                        // node starting on a later line. parse_value's NEWLINE
                        // arm only nests when an INDENT follows the line break
                        // directly, so step over any blank and comment-only
                        // lines and parse the node here, inside this
                        // TAGGED_NODE.
                        // An anchor between the tag and the line break
                        // annotates the same node; keep it in the TAGGED_NODE.
                        if self.current() == Some(SyntaxKind::ANCHOR) {
                            self.bump();
                        }
                        self.skip_ws_and_newlines();
                        if self.current() == Some(SyntaxKind::DASH) {
                            self.parse_sequence_with_base_indent(indent);
                        } else {
                            // The body is parsed at its own column, but a
                            // plain scalar there still belongs to the
                            // collection the tag sits in, so its continuation
                            // only has to clear the tag's column.
                            let outer_floor = self.scalar_continuation_floor;
                            self.scalar_continuation_floor = Some(base_indent);
                            self.parse_value_with_base_indent(indent);
                            self.scalar_continuation_floor = outer_floor;
                        }
                    }
                    // Scalar, flow collection, or nothing that belongs to this
                    // tag: the ordinary value path already handles it. Keep
                    // our own base indent, and tell a trailing anchor
                    // (`k: !!str &a`) which position we are in, so it asks
                    // the same attachment question we just answered and
                    // leaves a dedented sibling entry alone.
                    None => {
                        let outer = self.annotation_in_value_position;
                        self.annotation_in_value_position = as_mapping_value;
                        self.parse_value_with_base_indent(base_indent);
                        self.annotation_in_value_position = outer;
                    }
                }

                self.builder.finish_node();
            }
        }
    }

    /// Indent of the block node a lone tag annotates, if it starts on a
    /// later line.
    ///
    /// Returns `None` unless the rest of the tag's line is empty and the next
    /// line with content opens a block collection at a column this tag can
    /// own. Blank and comment-only lines in between are skipped, as they
    /// belong to neither node.
    ///
    /// A sequence may start at `base_indent` rather than further right, since
    /// one nested in a mapping need not be indented past its key. A mapping
    /// in the value position (`as_mapping_value`) has to nest under the key,
    /// or it is a sibling entry rather than our value; with no enclosing key
    /// there is nothing to nest under, so `!!map\na: 1\n` is a tagged mapping
    /// at the same column. Anything left of `base_indent` belongs to an
    /// enclosing collection, so it is not ours to adopt.
    pub(super) fn tagged_block_node_indent(
        &self,
        base_indent: usize,
        as_mapping_value: bool,
    ) -> Option<usize> {
        // Only a body in the value position must clear the key's column.
        let deep_enough = |indent: usize| {
            if as_mapping_value {
                indent > base_indent
            } else {
                indent >= base_indent
            }
        };
        // `tokens` is in reverse order, so walk it backwards from the current
        // token to read the rest of this line and the lines after it.
        let mut rest = self.tokens.iter().rev().map(|(kind, text)| (*kind, text));

        // Only an anchor or a comment may still sit between the tag and the
        // line break; `!!seq &a` annotates the block node just as `!!seq` does.
        let mut token = rest.next()?;
        if token.0 == SyntaxKind::ANCHOR {
            token = rest.next()?;
            while token.0 == SyntaxKind::WHITESPACE {
                token = rest.next()?;
            }
        }
        if token.0 == SyntaxKind::COMMENT {
            token = rest.next()?;
        }
        if token.0 != SyntaxKind::NEWLINE {
            return None;
        }

        // Walk whole lines until one carries content. An INDENT token holds
        // the line's leading whitespace, so a line is blank when a NEWLINE
        // follows it directly.
        loop {
            let mut indent = 0;
            let mut token = rest.next()?;
            if token.0 == SyntaxKind::INDENT {
                indent = token.1.len();
                token = rest.next()?;
            }
            match token.0 {
                SyntaxKind::NEWLINE => continue,
                SyntaxKind::COMMENT => {
                    // Comment lines carry no content, but the COMMENT token
                    // stops before the line break, so step over that too.
                    match rest.next() {
                        Some((SyntaxKind::NEWLINE, _)) => continue,
                        _ => return None,
                    }
                }
                SyntaxKind::DASH => return (indent >= base_indent).then_some(indent),
                // An explicit key opens a mapping without a colon on the line.
                SyntaxKind::QUESTION => return deep_enough(indent).then_some(indent),
                // A block scalar header likewise carries no colon, and a
                // flow collection is a complete node on its own.
                SyntaxKind::PIPE
                | SyntaxKind::GREATER
                | SyntaxKind::LEFT_BRACKET
                | SyntaxKind::LEFT_BRACE => return deep_enough(indent).then_some(indent),
                // Anything else opens a plain scalar, which is a valid
                // tagged body whether or not a colon makes it a mapping.
                _ => return deep_enough(indent).then_some(indent),
            }
        }
    }

    fn peek_tag_text(&self) -> Option<&str> {
        self.tokens
            .last()
            .filter(|(kind, _)| *kind == SyntaxKind::TAG)
            .map(|(_, text)| text.as_str())
    }

    fn parse_tagged_set(&mut self, base_indent: usize, as_mapping_value: bool) {
        // A block mapping only nests under its key, so an entry at
        // base_indent belongs to the enclosing mapping, not to this set.
        let indentless_value = as_mapping_value.then_some(base_indent);
        self.parse_tagged_collection(true, indentless_value); // true = parse as mapping
    }

    fn parse_tagged_omap(&mut self) {
        self.parse_tagged_collection(false, None); // false = parse as sequence
    }

    fn parse_tagged_pairs(&mut self) {
        self.parse_tagged_collection(false, None); // false = parse as sequence
    }

    /// `min_mapping_indent` is the column a block mapping has to beat to be
    /// this tag's value, set only when the tag is itself a mapping value.
    fn parse_tagged_collection(&mut self, is_mapping: bool, min_mapping_indent: Option<usize>) {
        self.builder.start_node(SyntaxKind::TAGGED_NODE.into());

        // Read the tag's text before bumping past it, for the error below.
        let tag_text = self.peek_tag_text().unwrap_or("tag").to_string();

        // Consume the tag
        self.bump(); // TAG token

        // Skip any whitespace after the tag
        while matches!(self.current(), Some(SyntaxKind::WHITESPACE)) {
            self.bump();
        }

        // An anchor between the tag and the line break annotates the same
        // node; keep it in the TAGGED_NODE. Without this the ANCHOR falls
        // through to the catch-all arm below, which parses the collection as
        // if it started on the tag's own line and swallows the next sibling
        // entry.
        if self.current() == Some(SyntaxKind::ANCHOR) {
            self.bump();
            while matches!(self.current(), Some(SyntaxKind::WHITESPACE)) {
                self.bump();
            }
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
                    if min_mapping_indent.is_some_and(|min| inner_base <= min) {
                        // The entries are siblings of our own key, so this tag
                        // annotates an implicit null and they stay outside.
                        self.emit_implicit_null();
                    } else {
                        self.parse_mapping_with_base_indent(inner_base);
                    }
                } else {
                    self.parse_sequence_with_base_indent(inner_base);
                }
            }
            _ => {
                // These tags name a collection, so a plain scalar on the
                // tag's own line cannot be their content: the collection
                // parser below finds nothing to take and the scalar is left
                // with nowhere to go. saphyr calls `!!omap 3` a bad value;
                // say so rather than dropping the scalar into an ERROR node
                // with no error reported.
                if self
                    .current()
                    .is_some_and(|kind| is_plain_scalar_kind(kind) || kind == SyntaxKind::QUESTION)
                {
                    self.add_error(
                        format!("{tag_text} requires a collection, found a scalar"),
                        ParseErrorKind::Other,
                    );
                }
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
        // Take the flag: only the document's own node is at the root, and
        // anything this scalar's body contains is not.
        let at_root = std::mem::take(&mut self.node_is_document_root);
        self.builder.start_node(SyntaxKind::SCALAR.into());
        self.bump(); // consume PIPE
        let explicit_indent = self.parse_block_scalar_header();
        self.parse_block_scalar_content(at_root, explicit_indent);
        self.builder.finish_node();
    }

    pub(super) fn parse_folded_block_scalar(&mut self) {
        let at_root = std::mem::take(&mut self.node_is_document_root);
        self.builder.start_node(SyntaxKind::SCALAR.into());
        self.bump(); // consume GREATER
        let explicit_indent = self.parse_block_scalar_header();
        self.parse_block_scalar_content(at_root, explicit_indent);
        self.builder.finish_node();
    }

    /// Parse a block scalar's header, returning its explicit indentation
    /// indicator if it carries one (`|2`, `>2-`).
    fn parse_block_scalar_header(&mut self) -> Option<usize> {
        let mut explicit_indent = None;
        // Parse optional indentation indicator (1-9) and chomping indicator (+, -)
        // Format: |<indent><chomp> or |<chomp><indent>
        // Examples: |2, |-, |+, |2-, |-2, |2+, |+2

        while let Some(kind) = self.current() {
            match kind {
                SyntaxKind::NEWLINE | SyntaxKind::COMMENT => break,
                SyntaxKind::INT => {
                    // Indentation indicator. The spec allows a single digit
                    // 1-9, but the lexer hands us the whole run, so `|10`
                    // arrives as "10". Take it as part of the header rather
                    // than leaving it for the body, which would strand it.
                    let Some(text) = self.current_text() else {
                        break;
                    };
                    // A chomping indicator before the digit lexes into the
                    // same token (`|-2` is INT("-2")), so allow one sign.
                    let digits = text.strip_prefix(['+', '-']).unwrap_or(text);
                    if !digits.is_empty() && digits.chars().all(|c| c.is_ascii_digit()) {
                        // The spec allows a single digit 1-9; a longer run is
                        // out of range, so keep only the first.
                        explicit_indent = digits
                            .chars()
                            .next()
                            .and_then(|c| c.to_digit(10))
                            .filter(|d| *d > 0)
                            .map(|d| d as usize);
                        self.bump();
                    } else {
                        break;
                    }
                }
                SyntaxKind::STRING => {
                    // Could be a chomping indicator, or an indentation digit
                    // and chomping indicator lexed together (`|3-`, `|-3`).
                    let Some(text) = self.current_text() else {
                        break;
                    };
                    let is_header = match text.len() {
                        1 => text == "+" || text == "-",
                        2 => {
                            let (a, b) = text.split_at(1);
                            let digit = |s: &str| s.chars().all(|c| c.is_ascii_digit() && c != '0');
                            let chomp = |s: &str| s == "+" || s == "-";
                            (digit(a) && chomp(b)) || (chomp(a) && digit(b))
                        }
                        _ => false,
                    };
                    if !is_header {
                        // Some other text, stop parsing header
                        break;
                    }
                    explicit_indent = text
                        .chars()
                        .find_map(|c| c.to_digit(10))
                        .filter(|d| *d > 0)
                        .map(|d| d as usize);
                    self.bump();
                }
                // A `+` straight after the header's `|` or `>` lexes as PLUS
                // rather than STRING, but it is the same chomping indicator.
                SyntaxKind::PLUS => {
                    self.bump();
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

        explicit_indent
    }

    /// Parse a block scalar's body.
    ///
    /// `at_root` says the scalar is the document's own node, with nothing
    /// enclosing it, so its body may start at column 0. Nested under a key
    /// the body has to be indented, or `a: |\nb: 1\n` would swallow `b`.
    fn parse_block_scalar_content(&mut self, at_root: bool, explicit_indent: Option<usize>) {
        // Consume all indented content that follows.
        //
        // The header consumed its own line break, so the loop starts at the
        // beginning of the next line. Say so: otherwise the dedent check is
        // suppressed for exactly that line and an empty block scalar eats the
        // entry after it (`empty: |\nnext: 1\n` lost `next`).
        let mut last_was_newline = true;
        // An explicit indentation indicator (`>2`) states the body's column
        // outright, so the first line does not get to set it: in
        // `a: >2\n   more\n  regular\n` the `regular` line is body at the
        // declared column, not a dedent out of the deeper first line.
        let mut base_indent: Option<usize> = explicit_indent;
        let mut first_content_indent: Option<usize> = explicit_indent;

        while let Some(kind) = self.current() {
            // Detect the first body line's indentation to use as base.
            //
            // The header's NEWLINE is already consumed, so the loop starts on
            // that first line and `last_was_newline` is false for it. A line
            // at column 0 carries no INDENT token, so record 0 rather than
            // leaving the base unset until some later, indented line sets it:
            // `|-\nx\ny\n` used to keep `x` and strand `y`, and
            // `|-\n?  >\n ems+\n...` took its base from the second line.
            if first_content_indent.is_none() {
                match kind {
                    // A whitespace-only line carries no content either, and
                    // it may be indented further than the body that follows
                    // (`- >\n \t\n detected\n`), so it must not set the base.
                    SyntaxKind::INDENT if self.indent_is_blank_line() => {
                        self.bump();
                    }
                    SyntaxKind::INDENT => {
                        first_content_indent = self.current_text().map(|t| t.len());
                    }
                    // A blank line carries no content, so it sets no base.
                    SyntaxKind::NEWLINE => {
                        self.bump();
                        last_was_newline = true;
                        continue;
                    }
                    // A body line at column 0 only belongs to this scalar
                    // when nothing encloses it.
                    _ if at_root => first_content_indent = Some(0),
                    _ => {}
                }
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
        // A body that starts at column 0 (only possible at the document root)
        // reads an unindented line as more body, not as a dedent.
        let body_at_column_zero = base_indent == Some(0);
        // Check if we've reached content at the beginning of a line (unindented)
        // Only check for structural tokens if we're at the start of a line
        if after_newline {
            // After a newline, check if the next token is unindented
            let current = self.current();

            // COLON, QUESTION or DASH at the start of a line means the end of
            // the block scalar -- but only where the body is indented, since a
            // line at column 0 then cannot belong to it. Inside a body that
            // starts at column 0 they are literal text like anything else, and
            // an *indented* `- x` is body content in either case (it carries an
            // INDENT token, so it is not at a line start here).
            if !body_at_column_zero
                && matches!(
                    current,
                    Some(SyntaxKind::COLON | SyntaxKind::QUESTION | SyntaxKind::DASH)
                )
            {
                return true;
            }

            // If we have base_indent, check if current line has less indentation.
            //
            // A blank line's indentation is not significant: a line holding
            // only spaces belongs to the body however short it is, so
            // `k: |\n  a\n \n  b\n` keeps `b`. Without this the whole
            // remainder was stranded in an ERROR node with no parse error.
            if let Some(base) = base_indent {
                if current == Some(SyntaxKind::INDENT) && !self.indent_is_blank_line() {
                    if let Some(text) = self.current_text() {
                        if text.len() < base {
                            // Current line has less indentation than base - end of block scalar
                            return true;
                        }
                    }
                }
            }

            // If we don't see INDENT, we've reached unindented content --
            // unless this scalar's body starts at column 0 itself, where an
            // unindented line is just another body line. Otherwise the first
            // such line is taken as content and every later one at the same
            // column reads as a dedent, which is how `|-\nx\ny\n` kept `x`
            // and stranded `y`.
            if !body_at_column_zero
                && current != Some(SyntaxKind::INDENT)
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

        // A blank line does not end a plain scalar; it folds to a line break
        // and the scalar continues on the next line with content. Step over
        // any run of blank lines (an INDENT holds a line's leading
        // whitespace, so a line is blank when a NEWLINE follows it directly).
        loop {
            let mut after_blank = peek_idx;
            if self
                .tokens
                .get(after_blank)
                .is_some_and(|(kind, _)| *kind == SyntaxKind::INDENT)
            {
                after_blank = after_blank.saturating_sub(1);
            }
            if after_blank > 0
                && self
                    .tokens
                    .get(after_blank)
                    .is_some_and(|(kind, _)| *kind == SyntaxKind::NEWLINE)
            {
                peek_idx = after_blank.saturating_sub(1);
                continue;
            }
            break;
        }

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
        //
        // A TAG, ANCHOR or REFERENCE counts: the lexer reads `!x`, `&a` and
        // `*a` as node properties because a newline precedes them, but a
        // property only applies at the start of a node, and a continuation
        // line is inside one already. `- a\n !\n` is the scalar `a !`, and `k:#foo\n &a !t s`
        // the scalar `k:#foo &a !t s`, as both saphyr and PyYAML read them.
        //
        // PIPE and GREATER count for the same reason. They open a block
        // scalar only at the start of a node, which in block context means
        // straight after a `-` or `:`; a line that merely continues a scalar
        // is inside a node already. `" a\n|\n"` is the scalar `a |`, as both
        // saphyr and PyYAML read it.
        //
        // The flow indicators count outside a flow collection, where they
        // are ordinary scalar content: `a\n}` is the scalar `a }`, as both
        // saphyr and PyYAML read it. Inside one they delimit, so they end
        // the scalar as before.
        //
        // A QUESTION counts only where it cannot open the next explicit
        // key of the mapping or sequence we may sit in: past the scalar's
        // own column, or in a root scalar that has no such collection. So
        // `? a\n: 1\n? b\n: 2\n` still has two entries, while `k: v\n  ?\n`
        // is the single value `v ?`, as saphyr and PyYAML both read it.
        //
        // A DASH counts only when the line is indented past the scalar's own,
        // where it cannot be the next entry of the sequence we sit in:
        // `a:\n- x\n  - y\n` is the single item `x - y`, as both saphyr and
        // PyYAML read it. At or left of the scalar's column it opens an entry
        // and ends the scalar, as before.
        let has_content = self.tokens.get(peek_idx).is_some_and(|(kind, _)| {
            matches!(
                kind,
                SyntaxKind::STRING
                    | SyntaxKind::INT
                    | SyntaxKind::FLOAT
                    | SyntaxKind::BOOL
                    | SyntaxKind::NULL
                    | SyntaxKind::UNTERMINATED_STRING
                    | SyntaxKind::TAG
                    | SyntaxKind::ANCHOR
                    | SyntaxKind::REFERENCE
                    // A bare `&` or `*` with no name is punctuation the
                    // lexer could not make a property of, so it is content.
                    // A lone `+` likewise: it is an indicator only as a
                    // block-scalar chomping suffix, so `a\n+\n` is `a +`.
                    | SyntaxKind::AMPERSAND
                    | SyntaxKind::ASTERISK
                    | SyntaxKind::PLUS
                    | SyntaxKind::PIPE
                    | SyntaxKind::GREATER
            ) || (matches!(
                kind,
                SyntaxKind::LEFT_BRACKET
                    | SyntaxKind::RIGHT_BRACKET
                    | SyntaxKind::LEFT_BRACE
                    | SyntaxKind::RIGHT_BRACE
                    | SyntaxKind::COMMA
            ) && self.flow_depth == 0)
                // `?` opens an explicit key only at the start of a node.
                // On a line that merely continues a scalar it is content:
                // `- x\n  ?\n- y\n` is the two entries `x ?` and `y`, as
                // saphyr and PyYAML both read it.
                || (*kind == SyntaxKind::QUESTION
                    && match self.sequence_entry_column {
                        // Inside a sequence: only past our own dash, where
                        // it cannot start the entry's own node.
                        Some(column) => next_line_indent > column,
                        // No sequence to open an entry of, so the mapping
                        // is what a `?` could start a node of. Past the
                        // scalar's own column it cannot: `k: v\n  ?\n` is
                        // the value `v ?`. A root scalar has no mapping
                        // either way.
                        None => {
                            next_line_indent > scalar_indent
                                || self.scalar_continuation_floor == Some(0)
                        }
                    })
                || (*kind == SyntaxKind::DASH
                    && match self.sequence_entry_column {
                        // Inside a sequence: only past our own dash, where it
                        // cannot be the next entry.
                        Some(column) => next_line_indent > column,
                        // No sequence to open an entry of, so it is content.
                        None => true,
                    })
        });

        // A continuation has to be indented past the scalar's own line, so it
        // cannot be mistaken for the next entry of an enclosing mapping.
        // Where there is no such mapping to confuse it with, an equally
        // indented line continues the scalar (`ab\ncd`, `- x\n y`).
        //
        // A lone tag parses its body with the body line's own column as the
        // base, but the scalar belongs to the enclosing collection, so its
        // continuation only has to clear that collection's column.
        let floor = self
            .scalar_continuation_floor
            .map_or(scalar_indent, |floor| floor.min(scalar_indent));
        let deep_enough = if self.equal_indent_continues_scalar {
            next_line_indent >= floor
        } else {
            next_line_indent > floor
        };
        if !has_content || !deep_enough {
            return false;
        }

        // Check if the next line is a mapping key (has a COLON after the content)
        // If so, it's not a continuation - it's a new mapping key
        if peek_idx > 0 {
            let mut check_idx = peek_idx.saturating_sub(1);

            // A `:` inside a flow collection separates that collection's own
            // entries, so step over a balanced one before looking: in
            // `a\n[:]\n` the colon is the flow mapping's, and the line
            // continues the scalar `a [:]`. Only the colon after the closing
            // bracket would make the line a key of ours.
            if self.tokens.get(peek_idx).is_some_and(|(kind, _)| {
                matches!(kind, SyntaxKind::LEFT_BRACKET | SyntaxKind::LEFT_BRACE)
            }) {
                let mut depth = 0usize;
                // `tokens` is in reverse order, so walking the index down
                // reads the line forwards. An unbalanced collection runs out
                // of tokens, which leaves the scan where it started.
                while let Some(kind) = self.tokens.get(check_idx + 1).map(|(kind, _)| *kind) {
                    match kind {
                        SyntaxKind::LEFT_BRACKET | SyntaxKind::LEFT_BRACE => depth += 1,
                        SyntaxKind::RIGHT_BRACKET | SyntaxKind::RIGHT_BRACE => {
                            depth = depth.saturating_sub(1)
                        }
                        _ => {}
                    }
                    if depth == 0 || check_idx == 0 {
                        break;
                    }
                    check_idx -= 1;
                }
            }

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
