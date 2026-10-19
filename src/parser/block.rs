//! Block-style parsing: block mappings (`k: v` on separate lines),
//! block sequences (`- item`), explicit-key mappings (`? key\n: val`),
//! and complex-key mappings (flow collections used as keys).
//!
//! Split out of `parser/mod.rs` unchanged.

use super::scalars::is_plain_scalar_kind;
use super::{BlockCtx, Parser};
use crate::error_recovery::ParseContext;
use crate::lex::SyntaxKind;
use crate::ParseErrorKind;

impl Parser<'_> {
    /// Consume comment lines sitting between entries of a block collection.
    ///
    /// Returns true when the comments ran into a dedent, meaning the
    /// collection has ended and the caller should stop.
    ///
    /// The root collection at column 0 keeps every comment, indented or
    /// not, because there is no enclosing scope.
    fn absorb_entry_comments(&mut self, base_indent: usize, dash_lines: bool) -> bool {
        while self.current() == Some(SyntaxKind::COMMENT) {
            // A comment on its own line says nothing about the block by its
            // own column: what ends the block is the next line with content,
            // so `a:\n  - x\n# c\n  - y\n` keeps both entries. Stop only
            // when that line really is dedented, or for a nested sequence
            // does not start with a dash. A comment with no content after it
            // ends every nested block, so that it goes to the root
            // collection. A comment on the last entry's own line stays with
            // that entry whatever follows, as it does before another entry.
            let own_line = matches!(
                self.previous_kind,
                None | Some(SyntaxKind::NEWLINE | SyntaxKind::INDENT)
            );
            let nested = self.builder.in_nested_collection();
            let dedented = own_line
                && match self.indent_after_comment_lines() {
                    Some((indent, kind)) => {
                        indent < base_indent || (nested && dash_lines && kind != SyntaxKind::DASH)
                    }
                    None => nested,
                };
            if (base_indent > 0 || nested) && dedented {
                return true;
            }
            self.bump();
            if self.current() == Some(SyntaxKind::NEWLINE) {
                self.bump();
            }
            if self.skip_whitespace_only_with_dedent_check(base_indent, dash_lines) {
                return true;
            }
        }
        false
    }

    pub(super) fn parse_mapping_with_base_indent(&mut self, base_indent: usize) {
        // Entries inside a mapping are bounded by their key's column, which
        // is not always the column the caller measured from: a document
        // indented as a whole keeps its leading INDENT outside DOCUMENT, so
        // the caller still says 0 and every line looks nested. Take the
        // first key's own column when it is further right, or
        // `"  k:\n  j: 1\n"` reads as `{k: {j: 1}}` where saphyr and PyYAML
        // both give two sibling entries.
        let base_indent = base_indent.max(self.current_line_indent);
        // The line's indent misses a compact mapping whose key follows a
        // `-`, `?` or `:` on its line: in `? a\n: b: c\n? d\n: e\n` the
        // mapping `{b: c}` sits past the `:`, and `? d` is the enclosing
        // mapping's next entry. Later entries have to clear that indicator.
        // A line between it and the first key is still ours, misindented,
        // which the validator reports.
        let indicator_column = [
            self.ctx.sequence_entry_column,
            self.ctx.explicit_key_column,
            self.ctx.explicit_value_column,
        ]
        .into_iter()
        .flatten()
        .max();
        let later_entry_indent = indicator_column.map(|column| {
            let key_column = self.column();
            key_column.min(column + 1)
        });
        // A floor set for a scalar node does not carry over to the values of
        // a mapping in its place: those continue only past this mapping's
        // own column, so `k:\n  a: b\n  c\n` does not read `c` into `b`.
        let ctx = BlockCtx {
            equal_indent_continues_scalar: false,
            scalar_continuation_floor: None,
            ..self.ctx
        };
        self.scoped(ctx, |p| {
            p.parse_block_mapping_entries(base_indent, later_entry_indent)
        });
    }

    fn parse_block_mapping_entries(
        &mut self,
        mut base_indent: usize,
        mut later_entry_indent: Option<usize>,
    ) {
        self.builder.start_node(SyntaxKind::MAPPING.into());
        self.error_context.push_context(ParseContext::Mapping);

        while self.current().is_some() {
            let tokens_before_iter = self.tokens.len();
            // Skip whitespace, break on dedent
            if self.skip_whitespace_only_with_dedent_check(base_indent, false) {
                break;
            }

            // A `:` at or left of the `?` we sit inside opens that entry's
            // value, not another entry of ours: `? a: 1\n: b: 2\n` keys the
            // outer mapping with `{a: 1}` and values it `{b: 2}`, as the
            // YAML test suite's V9D5 expects. Compare the colon's own
            // column, not its line's indent: in `- ? : x` the line starts
            // left of the `?`, but the colon opens the key's null-key entry
            // (M2N8).
            if self.current() == Some(SyntaxKind::COLON)
                && self
                    .ctx
                    .explicit_key_column
                    .is_some_and(|column| self.column() <= column)
            {
                break;
            }

            // Emit comments as children of MAPPING
            if self.absorb_entry_comments(base_indent, false) {
                break;
            }

            // Check dedent via tracked line indentation (covers the case where
            // MAPPING_ENTRY consumed its trailing NEWLINE before we could detect
            // the dedent in skip_whitespace_only_with_dedent_check).
            if base_indent > 0 && self.is_at_dedented_position(base_indent) {
                break;
            }

            // No mapping key found - exit
            if !self.starts_block_mapping_entry() {
                break;
            }

            // Check for explicit key indicator
            if self.current() == Some(SyntaxKind::QUESTION) {
                self.parse_explicit_key_entry();
            } else {
                self.parse_mapping_key_value_pair(base_indent);
            }
            if let Some(indent) = later_entry_indent.take() {
                base_indent = base_indent.max(indent);
            }

            // Progress guard: if no token was consumed this iteration we
            // would loop forever (e.g. when is_mapping_key() is fooled by a
            // delimiter such as `}` followed by `:`, and synthetic-token
            // recovery never advances).
            if self.tokens.len() == tokens_before_iter {
                self.parse_stray_token("mapping");
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
        let ctx = BlockCtx {
            equal_indent_continues_scalar: true,
            ..self.ctx
        };
        self.scoped(ctx, |p| p.parse_block_sequence_entries(base_indent));
    }

    fn parse_block_sequence_entries(&mut self, mut base_indent: usize) {
        self.builder.start_node(SyntaxKind::SEQUENCE.into());
        self.error_context.push_context(ParseContext::Sequence);

        // Once the first dash is seen, entries are bounded by its column. The
        // base indent misses one left of it in a compact nested sequence: in
        // `- - - a\n  - y\n` the innermost sequence is at column 4 and `y`
        // belongs to the middle one. Checking before the INDENT is consumed
        // leaves that INDENT to the sequence `y` belongs to.
        let mut first_entry = true;
        while self.current().is_some() {
            // Skip whitespace, break on dedent
            if self.skip_whitespace_only_with_dedent_check(base_indent, true) {
                break;
            }

            // Emit comments as children of SEQUENCE
            if self.absorb_entry_comments(base_indent, true) {
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
            // The dash's true column, read before bump() consumes it, and
            // zero-based to match the indents it is compared against.
            // current_line_indent counts leading whitespace only, so it reads
            // 0 for the `-` of an explicit key (`? - a`), whose entries then
            // look indented past their own sequence.
            let dash_column = self.column();
            if first_entry {
                if self.marker_line_node != Some(self.tokens.len()) {
                    base_indent = base_indent.max(dash_column);
                }
                first_entry = false;
            }

            // Start SEQUENCE_ENTRY node to wrap the entire item
            self.builder.start_node(SyntaxKind::SEQUENCE_ENTRY.into());
            self.bump(); // consume dash
            self.skip_whitespace();

            // Record the dash's line indentation for the item value parsing
            let item_indent = self.current_line_indent;

            // A `-` on a later line continues this entry's scalar when it is
            // indented past our own dash, and opens the next entry when it is
            // not. The dash column is the only thing that tells those apart.
            let ctx = BlockCtx {
                sequence_entry_column: Some(dash_column),
                ..self.ctx
            };
            self.scoped(ctx, |p| {
                p.parse_sequence_entry_value(dash_column, item_indent);
                p.parse_rest_of_entry_line(dash_column);
            });

            self.skip_entry_line_end();

            // Finish SEQUENCE_ENTRY node
            self.builder.finish_node();
        }

        self.builder.finish_node();
        self.error_context.pop_context();
    }

    /// Parse the value of the sequence entry whose `-` at `dash_column` was
    /// just consumed.
    fn parse_sequence_entry_value(&mut self, dash_column: usize, item_indent: usize) {
        // An entry's value is never the document's own node, so a block
        // scalar here still needs an indented body: `- |\n- x\n` is two
        // entries, not one scalar holding `- x`.
        self.node_is_document_root = false;

        // A comment is not a value. When nothing indented follows it the
        // entry is an implicit null, just as a bare `-` is: `- # c\n- a\n`
        // is two entries, as the YAML test suite's W42U expects, where
        // parsing the comment as a value left an empty SEQUENCE behind.
        // The entry's value may still be on the next line though
        // (`- # c\n  v\n`, as in RZP5), so keep the comment and let the
        // later-line handling below claim it.
        if self.dedented_block_scalar_header_follows(dash_column) {
            // `-\n|\n x\n`: the header can only be this entry's value.
            // Its body still has to clear the dash.
            self.skip_ws_and_newlines();
            self.parse_value();
        } else if self.current() == Some(SyntaxKind::COMMENT)
            && self.comment_ends_the_entry(dash_column)
        {
            self.bump();
            self.builder.start_node(SyntaxKind::SCALAR.into());
            self.builder.token(SyntaxKind::NULL.into(), "");
            self.builder.finish_node();
        } else if self.current() == Some(SyntaxKind::COMMENT) {
            // The comment belongs to this entry; its value follows.
            self.bump();
            if let Some(indent_level) = self.entry_value_indent(dash_column) {
                self.bump(); // newline
                while matches!(
                    self.current(),
                    Some(SyntaxKind::NEWLINE | SyntaxKind::COMMENT)
                ) || (self.current() == Some(SyntaxKind::INDENT)
                    && (self.indent_is_blank_line() || self.indent_starts_comment_line()))
                {
                    self.bump();
                }
                self.bump(); // indent
                self.parse_value_with_base_indent(indent_level);
            }
        } else if self.current().is_some() && self.current() != Some(SyntaxKind::NEWLINE) {
            // Use item's line indent so nested mappings parse at the right
            // level. It is one short of the column an inline value starts
            // at, since it counts the dash but not the space after it; a
            // mapping opening here measures its own entries from its key's
            // column instead, which parse_mapping_key_value_pair reads.
            self.parse_value_with_base_indent(item_indent);
        } else if self.current() == Some(SyntaxKind::NEWLINE) {
            // Nested content is a NEWLINE then INDENT. A bare `-` item is
            // an implicit null; leave the NEWLINE for the terminator bump
            // so set/remove see DASH, SCALAR, NEWLINE in that order.
            //
            // A blank line between the dash and its value carries no
            // indentation of its own, so look past a run of them:
            // `- \n\n m\n` is the entry `m`, just as `- \n m\n` is.
            // The value has to clear our own dash's column, or the line
            // opens the next entry rather than belonging to this one.
            if let Some(indent_level) = self.entry_value_indent(dash_column) {
                self.bump(); // consume newline
                             // Step over the blank and comment lines the helper looked past.
                while matches!(
                    self.current(),
                    Some(SyntaxKind::NEWLINE | SyntaxKind::COMMENT)
                ) || (self.current() == Some(SyntaxKind::INDENT)
                    && (self.indent_is_blank_line() || self.indent_starts_comment_line()))
                {
                    self.bump();
                }
                self.bump(); // consume indent

                // The value parses at its own column, but a plain scalar
                // there is still a node of this sequence, so a
                // continuation only has to clear the *dash's* column:
                // `-\n   b\n  - z\n` is the single item `b - z`, as
                // both saphyr and PyYAML read it. Only for a scalar: a
                // nested collection's entries set their own column.
                let mut ctx = self.ctx;
                if self.current() != Some(SyntaxKind::DASH) {
                    ctx.scalar_continuation_floor = Some(dash_column);
                }
                self.scoped(ctx, |p| p.parse_value_with_base_indent(indent_level));
            } else {
                self.emit_implicit_null();
            }
        } else {
            self.emit_implicit_null();
        }
    }

    pub(super) fn parse_explicit_key_mapping(&mut self, base_indent: usize) {
        // As in parse_mapping_with_base_indent, the entries are bounded by
        // the first key's column when that is right of the caller's base: a
        // compact mapping as a value, `? h\n: ? b\n  : x\n  a: 1\n`, has
        // its entries at column 2 though its value slot was parsed from 0.
        let base_indent = base_indent.max(self.column());
        // Neither key nor value is the document's own node, so a block
        // scalar in either needs an indented body: `? a\n: |\nb: 1\n` keeps
        // `b` as the next entry.
        self.node_is_document_root = false;

        // `?` and `:` lines delimit this mapping's own entries, so a scalar
        // inside one must not keep folding at an enclosing key's column: an
        // inherited floor would let `? a\n: 1\n? c\n` read `? c` as part of
        // the value `1`. Nor does the rule of the document root or a
        // sequence entry, where a line at the scalar's own column continues
        // it: in `? a\n: 1\nb\n` the `b` line is not part of the value.
        let ctx = BlockCtx {
            equal_indent_continues_scalar: false,
            scalar_continuation_floor: None,
            ..self.ctx
        };
        self.scoped(ctx, |p| p.parse_explicit_key_mapping_entries(base_indent));
    }

    fn parse_explicit_key_mapping_entries(&mut self, base_indent: usize) {
        self.builder.start_node(SyntaxKind::MAPPING.into());

        while self.current() == Some(SyntaxKind::QUESTION) {
            // Start a MAPPING_ENTRY to wrap this key-value pair
            self.builder.start_node(SyntaxKind::MAPPING_ENTRY.into());

            // Parse explicit key
            let question_column = self.column();
            self.bump(); // consume '?'
            self.skip_whitespace();

            // Parse key - can be any value including sequences and mappings
            self.builder.start_node(SyntaxKind::KEY.into());

            // A `:` line at or left of the `?` is this entry's value, so an
            // annotation inside the key must not adopt it. A `?` opening a
            // line at this mapping's own column is its next entry, not part
            // of the key's scalar: `- ? k\n  ? c\n` is two entries, as saphyr
            // and PyYAML both read it.
            let ctx = BlockCtx {
                explicit_key_column: Some(question_column),
                mapping_value_column: Some(question_column),
                ..self.ctx
            };
            self.scoped(ctx, |p| {
                p.parse_explicit_key_content(question_column, base_indent);
                p.parse_rest_of_entry_line(question_column);
            });

            self.builder.finish_node();

            // Parse value if there's a colon. Without one, the lines after
            // the key's are the mapping's. One on a later line left of the
            // `?` is an enclosing entry's: in `? ? a\n: b\n` the `: b` is
            // the outer entry's value.
            self.skip_line_end_trivia();
            if self.explicit_value_follows(question_column) {
                self.skip_ws_and_newlines();
                let colon_column = self.column();
                self.bump(); // consume ':'
                self.skip_whitespace();
                let value_ctx = BlockCtx {
                    explicit_value_column: Some(colon_column),
                    ..self.ctx
                };

                // `:\n- c\n` -- an indentless sequence value, whose entries
                // sit at the `:`'s own column rather than past it. The shared
                // value-after-colon shape measures from the indent it finds,
                // so this one case is handled before delegating to it.
                if self.indentless_sequence_follows(colon_column) {
                    self.builder.start_node(SyntaxKind::VALUE.into());
                    if self.current() == Some(SyntaxKind::COMMENT) {
                        self.bump();
                    }
                    self.bump(); // consume the newline
                    self.scoped(value_ctx, |p| {
                        p.parse_sequence_with_base_indent(base_indent)
                    });
                    self.builder.finish_node();
                } else {
                    // A value on the colon's own line is bounded by this
                    // mapping's column, as for an implicit key: in
                    // `x:\n  ? a\n  : |\n  b: 1\n` the `b` line is not body.
                    // A value on a later line has this mapping as its
                    // parent all the same, so a block scalar's indentation
                    // indicator counts from it: `- ? a\n  :\n    |1\n` has
                    // its body at column 3, not 1.
                    let mut ctx = value_ctx;
                    if matches!(
                        self.current(),
                        Some(SyntaxKind::NEWLINE | SyntaxKind::COMMENT)
                    ) {
                        ctx.mapping_block_value_column = Some(colon_column);
                    } else {
                        ctx.mapping_value_column = Some(colon_column);
                    }
                    self.scoped(ctx, |p| p.parse_value_after_colon(true, question_column));
                }
            } else {
                // No value, just a key - create explicit null value
                self.emit_implicit_null_value();
            }
            self.skip_entry_line_end();

            // Finish the MAPPING_ENTRY node
            self.builder.finish_node();

            if self.skip_whitespace_only_with_dedent_check(base_indent, false)
                || self.absorb_entry_comments(base_indent, false)
            {
                break;
            }

            // A following line at a shallower column closes this mapping --
            // it belongs to an enclosing one, not here.
            if self.is_at_dedented_position(base_indent) {
                break;
            }

            // Check if there are more entries
            if !self.starts_block_mapping_entry() {
                break;
            }
        }

        // Continue parsing regular mapping entries if any
        while self.current().is_some()
            && !self.is_at_dedented_position(base_indent)
            && self.starts_block_mapping_entry()
        {
            let tokens_before_iter = self.tokens.len();
            // is_mapping_key() returns true for QUESTION, but
            // parse_mapping_key_value_pair does not consume a `?` key - that
            // would loop forever. Re-enter explicit-key handling for `?`.
            //
            // It takes the whole run of them, so the plain entries after that
            // run are still ours to parse: `?\nk:\n?\na:\n` has four. Leave
            // the loop to the progress guard below rather than breaking here.
            if self.current() == Some(SyntaxKind::QUESTION) {
                self.parse_explicit_key_entries();
            } else {
                self.parse_mapping_key_value_pair(base_indent);
            }
            self.holding_trivia(Self::skip_ws_and_newlines);
            // Progress guard against any future case where the body consumes
            // nothing (e.g. recovery via synthetic-token insertion).
            if self.tokens.len() == tokens_before_iter {
                self.parse_stray_token("explicit-key mapping");
            }
        }

        self.builder.finish_node();
    }

    /// Parse the key of an explicit-key entry whose `?` sits at
    /// `question_column`, with any scalar continuation lines.
    fn parse_explicit_key_content(&mut self, question_column: usize, base_indent: usize) {
        self.parse_explicit_key_node(question_column, base_indent);

        // A scalar key may continue on an indented line. Sequences and
        // mappings have consumed theirs already.
        let ahead = self.lookahead();
        let (Some(SyntaxKind::NEWLINE), Some((SyntaxKind::INDENT, indent)), Some(after_indent)) =
            (ahead.kind(0), ahead.get(1), ahead.kind(2))
        else {
            return;
        };
        // The key's own content has to clear the `?`; a line at the
        // indicator's column opens the next entry instead (`- ?\n  j: 1\n` is
        // two entries, as saphyr reads it).
        let key_indent = indent.len();
        // A dash line is a sequence continuation, which
        // parse_explicit_key_node already handled. A comment line is the
        // entry's trivia, as after any other node: reading it as key content
        // left an empty SCALAR in the KEY (`? a\n  # c\n: v\n`).
        if matches!(after_indent, SyntaxKind::DASH | SyntaxKind::COMMENT)
            || key_indent <= question_column
        {
            return;
        }
        self.bump(); // consume newline
        self.bump(); // consume indent

        // Unless the key is a block mapping starting on this line:
        // `?\n s: 1\n e: 2\n: v\n` is keyed by `{s: 1, e: 2}`, as saphyr
        // reads it and as the inline `? s: 1\n  e: 2\n` spelling already
        // parsed. Reading it as scalar parts stopped at the first colon and
        // dropped every entry after it, and the value with them. A line
        // starting at the colon is this entry's own value (`? a\n: 1\n`), not
        // part of the key, even though is_mapping_key counts it as the
        // null-key entry it would be anywhere else.
        if self.current() != Some(SyntaxKind::COLON) && self.is_mapping_key() {
            self.parse_mapping_with_base_indent(key_indent);
        }

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
            // Progress guard: parse_scalar() can return without consuming
            // tokens for kinds it doesn't handle (e.g. COMMENT). Break to
            // avoid an infinite loop.
            if self.tokens.len() == before {
                break;
            }
        }
    }

    fn parse_explicit_key_entries(&mut self) {
        // Helper to continue parsing explicit key entries within a mapping
        while self.current() == Some(SyntaxKind::QUESTION) {
            self.parse_explicit_key_entry();
            self.holding_trivia(Self::skip_ws_and_newlines);
        }
    }

    /// Parse one `? key` entry, with its `: value` if there is one.
    fn parse_explicit_key_entry(&mut self) {
        self.builder.start_node(SyntaxKind::MAPPING_ENTRY.into());

        let question_column = self.column();
        self.bump(); // consume '?'
        self.skip_whitespace();

        // A `?` line at this entry's column opens the next entry, so a
        // scalar in the key or value must not continue onto it. Whatever
        // the enclosing value left behind does not apply: in
        // `o:\n  k: !!set\n    ? a\n    ? b\n` it let `? b` continue `a`.
        let ctx = BlockCtx {
            scalar_continuation_floor: None,
            mapping_value_column: Some(question_column),
            ..self.ctx
        };
        self.scoped(ctx, |p| {
            p.builder.start_node(SyntaxKind::KEY.into());
            let key_ctx = BlockCtx {
                explicit_key_column: Some(question_column),
                ..p.ctx
            };
            p.scoped(key_ctx, |p| {
                p.parse_explicit_key_node(question_column, question_column);
                p.parse_rest_of_entry_line(question_column);
            });
            p.builder.finish_node();

            p.skip_line_end_trivia();
            if p.explicit_value_follows(question_column) {
                p.skip_ws_and_newlines();
                let colon_column = p.column();
                p.bump(); // consume ':'
                p.skip_whitespace();
                let value_ctx = BlockCtx {
                    explicit_value_column: Some(colon_column),
                    ..p.ctx
                };
                p.scoped(value_ctx, |p| {
                    p.parse_value_after_colon(false, question_column)
                });
            } else {
                p.emit_implicit_null_value();
            }
        });
        self.skip_entry_line_end();
        self.builder.finish_node();
    }

    /// Whether the current token opens an entry of a block mapping: an
    /// implicit key, possibly a flow collection (`[a, b]: 2`) with its
    /// properties, or an explicit one.
    fn starts_block_mapping_entry(&self) -> bool {
        self.is_mapping_key()
            || self.is_complex_mapping_key()
            || (matches!(self.current(), Some(SyntaxKind::ANCHOR | SyntaxKind::TAG))
                && self.is_complex_mapping_key_after_annotations())
    }

    /// As [`is_complex_mapping_key`](Self::is_complex_mapping_key), reading
    /// past a run of leading anchors and tags.
    ///
    /// `&key [ a ]: value` anchors the flow sequence that is the key, so the
    /// shape has to be recognised with the annotation still in front of it.
    pub(super) fn is_complex_mapping_key_after_annotations(&self) -> bool {
        let mut rest = self
            .upcoming_tokens()
            .skip_while(|k| {
                matches!(
                    k,
                    SyntaxKind::ANCHOR
                        | SyntaxKind::TAG
                        | SyntaxKind::WHITESPACE
                        | SyntaxKind::INDENT
                )
            })
            .peekable();
        let start_kind = match rest.peek() {
            Some(SyntaxKind::LEFT_BRACKET) => SyntaxKind::LEFT_BRACKET,
            Some(SyntaxKind::LEFT_BRACE) => SyntaxKind::LEFT_BRACE,
            _ => return false,
        };
        let close_kind = match start_kind {
            SyntaxKind::LEFT_BRACKET => SyntaxKind::RIGHT_BRACKET,
            _ => SyntaxKind::RIGHT_BRACE,
        };
        let mut depth = 0usize;
        let mut rest = rest.skip(1);
        while let Some(kind) = rest.next() {
            if kind == start_kind {
                depth += 1;
            } else if kind == close_kind {
                if depth == 0 {
                    return reaches_key_colon(rest);
                }
                depth -= 1;
            }
        }
        false
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

        let mut rest = self.upcoming_tokens();
        while let Some(kind) = rest.next() {
            if Some(kind) == start_kind {
                depth += 1;
            } else if kind == close_kind {
                if depth == 0 {
                    return reaches_key_colon(rest);
                }
                depth -= 1;
            }
        }
        false
    }

    fn parse_mapping_value(&mut self, base_indent: usize) {
        // A mapping value is never the document's own node, whatever the
        // caller was parsing: `a: |\nb: 1\n` has an empty block scalar and
        // keeps `b` as a sibling, where `|\nb: 1\n` at the root would take
        // the line as body.
        self.node_is_document_root = false;

        // When parsing the value part of a mapping, be more conservative about
        // interpreting content as nested mappings. Only parse as mapping if
        // it's clearly a structured value, otherwise parse as scalar.
        match self.current() {
            Some(SyntaxKind::DASH) if !self.in_flow_context => self.parse_sequence(),
            // As with a flow collection below, an alias the colon of a key
            // follows (`k: *a : v`, `k: *a x: v`) opens a mapping, which the
            // validator reports, rather than ending the entry mid-line.
            Some(SyntaxKind::REFERENCE) if !self.in_flow_context && self.is_mapping_key() => {
                self.parse_mapping_with_base_indent(base_indent);
            }
            Some(SyntaxKind::REFERENCE) => {
                self.parse_alias();
                self.parse_content_after_node();
            }
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
                    self.parse_mapping_with_base_indent(base_indent);
                } else {
                    self.parse_flow_sequence();
                    self.parse_content_after_node();
                }
            }
            Some(SyntaxKind::LEFT_BRACE) => {
                // Check if this is a complex key in a mapping
                if !self.in_flow_context && self.is_complex_mapping_key() {
                    self.parse_mapping_with_base_indent(base_indent);
                } else {
                    self.parse_flow_mapping();
                    self.parse_content_after_node();
                }
            }
            _ => {
                // For all other cases in mapping values, parse as scalar
                // This handles URLs and other complex scalar values containing colons
                self.parse_scalar();
            }
        }
    }

    /// Parse the node an explicit key indicator introduces.
    ///
    /// The key may sit on the indicator's own line, or start on the next
    /// line: indentless when its dashes line up with the `?` itself, and
    /// otherwise indented past it. With neither, the key is an implicit
    /// null.
    fn parse_explicit_key_node(&mut self, question_column: usize, base_indent: usize) {
        // A comment ends the indicator's line just as a line break does, so
        // the checks below still decide whether the next line is the key.
        // Left to parse_value, it took the next line whatever its column:
        // `? # c\nb: 1\n` nested `b: 1` inside the key.
        if self.current() == Some(SyntaxKind::COMMENT) {
            self.bump();
        }
        if self.current().is_some() && self.current() != Some(SyntaxKind::NEWLINE) {
            // A sequence opening on the indicator's own line is bounded by
            // it: measured from column 0 instead, the key's sequence in
            // `-\n  ? - c\n- z\n` swallowed the enclosing sequence's next
            // entry. Other node kinds set their own bounds as before.
            if self.current() == Some(SyntaxKind::DASH) {
                self.parse_sequence_with_base_indent(question_column);
            } else {
                self.parse_value();
            }
        } else if self.dedented_block_scalar_header_follows(question_column) {
            // `?\n|\n x\n`: the header can only be the key. Its body still
            // has to clear the `?`.
            self.skip_ws_and_newlines();
            self.parse_value();
        } else if self.indentless_sequence_follows(question_column) {
            self.bump(); // consume the newline
            self.parse_sequence_with_base_indent(base_indent);
        } else if let Some(indent) = self.indented_sequence_indent(question_column) {
            self.bump(); // consume the newline
            self.bump(); // consume the indent
            self.parse_sequence_with_base_indent(indent);
        } else if let Some(indent) = self.aligned_sequence_indent(question_column) {
            // `- ?\n  - a\n`: the `?` sits at a column the sequence's own
            // indentation reproduces, so the dashes are written out as an
            // INDENT rather than starting the line. They still belong to
            // the key.
            self.bump(); // consume the newline
            self.bump(); // consume the indent
            self.parse_sequence_with_base_indent(indent);
        } else if let Some(indent) = self.indented_key_node_indent(question_column) {
            // The same for a mapping or scalar key node: `?\n  j: 1\n` is
            // keyed by `{j: 1}`. Emitting the implicit null first left the
            // real key beside it in the KEY node, where every accessor read
            // the null instead.
            self.bump(); // consume the newline
            self.bump(); // consume the indent
            self.parse_value_with_base_indent(indent);
        } else {
            self.emit_implicit_null();
        }
    }

    /// The indentation of a non-sequence key node starting on the line after
    /// an explicit key indicator at `question_column`, if one does.
    ///
    /// A line opening at the colon is the entry's own value rather than part
    /// of its key, so `? a\n: 1\n` is left alone.
    ///
    /// Indented comment lines before it say nothing about where the key is:
    /// in `?\n # c\nk: v\n` the key is empty and `k: v` the next entry.
    /// A sequence after them is the key's too, which
    /// [`indented_sequence_indent`](Self::indented_sequence_indent) leaves
    /// to this.
    fn indented_key_node_indent(&self, question_column: usize) -> Option<usize> {
        if self.current() != Some(SyntaxKind::NEWLINE) {
            return None;
        }
        let mut rest = self.tokens.iter().rev().skip(1);
        let mut after_comment = false;
        let (indent, kind) = loop {
            match rest.next()? {
                (SyntaxKind::NEWLINE, _) => continue,
                (SyntaxKind::INDENT, text) => match rest.next()? {
                    (SyntaxKind::COMMENT, _) => after_comment = true,
                    (kind, _) => break (text.len(), *kind),
                },
                _ => return None,
            }
        };
        if indent <= question_column {
            return None;
        }
        match kind {
            SyntaxKind::COLON => None,
            SyntaxKind::DASH if !after_comment => None,
            _ => Some(indent),
        }
    }

    /// As [`indented_sequence_indent`](Self::indented_sequence_indent), for a
    /// sequence whose dashes line up with the `?` itself.
    ///
    /// The indicator only looks indentless when it starts its line; nested in
    /// a sequence entry, the same alignment arrives as an INDENT token.
    fn aligned_sequence_indent(&self, question_column: usize) -> Option<usize> {
        if self.current() != Some(SyntaxKind::NEWLINE) || question_column == 0 {
            return None;
        }
        let mut rest = self.tokens.iter().rev().skip(1);
        let indent = loop {
            match rest.next()? {
                (SyntaxKind::NEWLINE, _) => continue,
                (SyntaxKind::INDENT, text) => break text.len(),
                _ => return None,
            }
        };
        if indent != question_column {
            return None;
        }
        match rest.next()? {
            (SyntaxKind::DASH, _) => Some(indent),
            _ => None,
        }
    }

    /// The indentation of a sequence starting on the line after an explicit
    /// key indicator at `question_column`, if one does.
    ///
    /// `?\n  - a\n` keys the entry on `[a]`: the key's node is allowed to
    /// start on the following line as long as it is indented past the `?`.
    fn indented_sequence_indent(&self, question_column: usize) -> Option<usize> {
        if self.current() != Some(SyntaxKind::NEWLINE) {
            return None;
        }
        let mut rest = self.tokens.iter().rev().skip(1);
        let indent = loop {
            match rest.next()? {
                (SyntaxKind::NEWLINE, _) => continue,
                (SyntaxKind::INDENT, text) => break text.len(),
                _ => return None,
            }
        };
        if indent <= question_column {
            return None;
        }
        match rest.next()? {
            (SyntaxKind::DASH, _) => Some(indent),
            _ => None,
        }
    }

    /// Whether the next line opens an indentless block sequence, whose
    /// entries sit at the current construct's own column.
    ///
    /// The caller is on the NEWLINE, or a comment before it, that ends the
    /// `?` or `:` line. A `-` straight after that line break carries no INDENT
    /// token, which is what makes the sequence indentless.
    fn indentless_sequence_follows(&self, indicator_column: usize) -> bool {
        // The dashes carry no INDENT, so they start their line: only an
        // indicator in column 0 shares that column. Further in, `- ?\n- a\n`
        // dedents out of the entry and the sequence is the enclosing one's.
        let mut rest = self
            .current()
            .into_iter()
            .chain(self.upcoming_tokens())
            .peekable();
        rest.next_if_eq(&SyntaxKind::COMMENT);
        if rest.next() != Some(SyntaxKind::NEWLINE) || indicator_column != 0 {
            return false;
        }
        // A blank line between the indicator and the sequence carries no
        // indentation of its own, so it neither ends the entry nor stops the
        // sequence being the indentless one: `?\n\n- a\n: v\n` is keyed by
        // the sequence `[a]`, exactly as `?\n- a\n: v\n` is.
        loop {
            match rest.next() {
                Some(SyntaxKind::NEWLINE) => continue,
                Some(kind) => return kind == SyntaxKind::DASH,
                None => return false,
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

        // A line that starts at the colon is an entry with an empty key:
        // `: v` is a mapping from null to `v`, which is what the YAML test
        // suite expects (2JQS gives `=VAL :` for the key). The scan below
        // looks for a colon *after* a key, so a null key needs saying here
        // or the mapping loop stops before the entry.
        if self.current() == Some(SyntaxKind::COLON) {
            return true;
        }

        // Look ahead to see if there's a colon after the current token.
        // Plain scalars can contain spaces, so a key may span multiple scalar
        // tokens separated by whitespace before the terminating colon
        // (e.g. `abc cba: value`).
        //
        // Any number of leading TAG / ANCHOR tokens annotate the key
        // (`!!str &a1 "foo":`); skip past them and any WHITESPACE, then
        // apply the usual scan. The annotated key may also be an alias
        // (`&b *a :`), which YAML does not allow; the validator reports it.
        let annotated = matches!(self.current(), Some(SyntaxKind::TAG | SyntaxKind::ANCHOR));
        let mut saw_scalar = false;
        let mut saw_alias = false;
        for kind in self.upcoming_tokens() {
            if kind == SyntaxKind::COLON {
                return true;
            }
            if !saw_scalar && matches!(kind, SyntaxKind::TAG | SyntaxKind::ANCHOR) {
                continue;
            }
            if annotated && !saw_scalar && kind == SyntaxKind::REFERENCE {
                saw_scalar = true;
                saw_alias = true;
                continue;
            }
            if is_plain_scalar_kind(kind) && !saw_alias {
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

    /// The node of an implicit block key, with its properties.
    ///
    /// As in every other position, a tag wraps the rest of the node in a
    /// TAGGED_NODE while an anchor in front of it stays outside:
    /// `&a !t k:` gives `KEY { ANCHOR, WHITESPACE, TAGGED_NODE { TAG, ... } }`.
    fn parse_block_key_node(&mut self) {
        while self.current() == Some(SyntaxKind::ANCHOR) {
            self.bump();
            self.skip_whitespace();
        }
        if self.current() == Some(SyntaxKind::TAG) {
            self.builder.start_node(SyntaxKind::TAGGED_NODE.into());
            self.bump();
            // A tagged empty key (`!t : v`) leaves the whitespace before the
            // colon outside, as in a flow mapping.
            if self.current() == Some(SyntaxKind::WHITESPACE)
                && self.upcoming_tokens().next() == Some(SyntaxKind::COLON)
            {
                self.emit_implicit_null();
            } else {
                self.skip_whitespace();
                self.parse_block_key_node();
            }
            self.builder.finish_node(); // TAGGED_NODE
            return;
        }

        if self.current() == Some(SyntaxKind::MERGE_KEY) {
            self.builder.start_node(SyntaxKind::SCALAR.into());
            self.bump(); // consume the merge key token
            self.builder.finish_node(); // SCALAR
        } else if self.current() == Some(SyntaxKind::REFERENCE) {
            // Handle alias as key (*b:)
            self.parse_alias();
        } else if self.current() == Some(SyntaxKind::LEFT_BRACKET) {
            self.parse_flow_sequence();
        } else if self.current() == Some(SyntaxKind::LEFT_BRACE) {
            self.parse_flow_mapping();
        } else if self.current().is_some_and(is_plain_scalar_kind) {
            self.parse_block_key_scalar();
        } else if self.current() == Some(SyntaxKind::COLON) {
            // No key before the colon: an explicit null, spelled the same
            // zero-width way as an implicit null value.
            self.builder.start_node(SyntaxKind::SCALAR.into());
            self.builder.token(SyntaxKind::NULL.into(), "");
            self.builder.finish_node(); // SCALAR
        }
    }

    /// A plain scalar key, which may contain spaces: `abc cba: value`.
    fn parse_block_key_scalar(&mut self) {
        self.builder.start_node(SyntaxKind::SCALAR.into());
        self.bump();
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

    /// Keep plain scalar content after a complete key node on its line, as
    /// in `[q] x: v` or `*a x: v`, as a second node of the key.
    ///
    /// It is not part of a valid key, but the validator reports it, rather
    /// than the parser ending the key before it and starting another entry
    /// on the same line. An implicit key cannot span lines, so the content
    /// is taken from this line only.
    fn parse_content_after_key_node(&mut self) {
        let whitespace = usize::from(self.current() == Some(SyntaxKind::WHITESPACE));
        if self.peek_kind(whitespace).is_some_and(is_plain_scalar_kind) {
            if whitespace == 1 {
                self.bump();
            }
            self.parse_block_key_scalar();
        }
    }

    /// Keep plain scalar content after a complete node, as in `[q] x` or
    /// `*a x`, as a second node next to it in the same slot.
    ///
    /// It is not part of a valid node, but the validator reports it, rather
    /// than the parser ending the node before it and leaving the rest after
    /// the document. The content is read as the plain scalar it would be in
    /// place of the node, so it may continue on more indented lines, and it
    /// may also start on such a line: `k:\n  [q]\n  y\n`. Block context
    /// only: a flow collection has its own separators.
    pub(super) fn parse_content_after_node(&mut self) {
        if self.in_flow_context {
            return;
        }
        let whitespace = usize::from(self.current() == Some(SyntaxKind::WHITESPACE));
        let on_next_line = self.peek_kind(whitespace) == Some(SyntaxKind::NEWLINE)
            && self.is_plain_scalar_continuation(whitespace, self.plain_scalar_indent())
            && self
                .upcoming_tokens()
                .find(|kind| {
                    !matches!(
                        kind,
                        SyntaxKind::NEWLINE | SyntaxKind::INDENT | SyntaxKind::WHITESPACE
                    )
                })
                .is_some_and(is_plain_scalar_kind);
        if on_next_line {
            while matches!(
                self.current(),
                Some(SyntaxKind::NEWLINE | SyntaxKind::INDENT | SyntaxKind::WHITESPACE)
            ) {
                self.bump();
            }
        } else if self.peek_kind(whitespace).is_some_and(is_plain_scalar_kind) {
            if whitespace == 1 {
                self.bump();
            }
        } else {
            return;
        }
        self.parse_scalar();
    }

    fn parse_mapping_key_value_pair(&mut self, base_indent: usize) {
        // No key starts with the token (`]:`): leave it to the caller to
        // report as stray rather than read an empty key.
        if !self.current().is_some_and(starts_block_key_node) {
            return;
        }
        // The key's own column, read before anything is consumed. It is
        // 1-based, so step back to a column.
        let key_column = self.column();
        // Start MAPPING_ENTRY node to wrap the entire key-value pair
        self.builder.start_node(SyntaxKind::MAPPING_ENTRY.into());

        // Parse regular key
        self.builder.start_node(SyntaxKind::KEY.into());

        self.parse_block_key_node();
        self.parse_content_after_key_node();
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
                // Inline value on the same line as the colon. A `?` opening
                // a later line at this mapping's own column is its next
                // entry, not part of the value's scalar: `- a: 1\n  ? c\n`
                // is two entries, while `- x\n  ? c\n` (no mapping) is the
                // single scalar `x ? c`.
                //
                // A node a tag on this line adopts from later lines has to
                // clear the key's column too, not just the mapping's base:
                // in `- a: !!null\n  b: c\n` the `b` line is a sibling.
                let column = base_indent.max(key_column);
                let ctx = BlockCtx {
                    mapping_value_column: Some(column),
                    ..self.ctx
                };
                self.scoped(ctx, |p| p.parse_mapping_value(column));
                has_value = true;
            } else if self.current() == Some(SyntaxKind::NEWLINE) {
                // Look for the value before stepping over the lines in
                // between: without one, those lines are the mapping's, not
                // the empty value's.
                let (next, indent) = self.next_content();
                let block_value = next.is_some() && indent > base_indent.max(key_column);
                let indentless_sequence = next == Some(SyntaxKind::DASH)
                    && (indent == base_indent || indent == key_column)
                    // A dash at the column of a sequence we sit in is that
                    // sequence's next entry, which it claims before we can:
                    // `- - k:\n  - b\n` is the two items `{k: null}` and
                    // `b`, not one item keyed by `[b]`.
                    && match self.ctx.sequence_entry_column {
                        Some(column) => indent > column,
                        None => true,
                    };
                let block_scalar = matches!(next, Some(SyntaxKind::PIPE | SyntaxKind::GREATER));
                if block_value || indentless_sequence || block_scalar {
                    self.skip_ws_and_newlines();
                }
                // A mapping's value is never the document's own node,
                // whatever the caller was parsing. Left set, a block scalar
                // here read its body as starting at column 0 and swallowed
                // the next entry: `k:\n  |\nz: 1\n` lost `z`.
                self.node_is_document_root = false;
                // A block value on a later line has to clear the *key's*
                // column, not the column the mapping was measured from. In a
                // sequence entry those differ: `- k:` puts `k` at column 2
                // while the mapping's base is 1, the dash's own column plus
                // the dash. Against the base, `- k:\n  j: 1\n` reads `j` as
                // the value of `k`; against the key it is the sibling entry
                // saphyr and PyYAML both give.
                if block_value {
                    // Nested value is more indented than the enclosing mapping's
                    // base indent - belongs to this key.
                    //
                    // The value parses at its own column, but a plain scalar
                    // there is still a node of this mapping, so a
                    // continuation line only has to clear the *key's* column:
                    // `a:\n  x y\n z\n` is the single scalar `x y z`, as
                    // both saphyr and PyYAML read it.
                    //
                    // Only for a scalar value. A sequence's entries set their
                    // own column, so a line dedented past them ends the
                    // mapping rather than continuing an entry:
                    // `key:\n - a\ninvalid\n` is the error the YAML test
                    // suite's 6S55 and 9CWY expect, not the scalar `a invalid`.
                    let mut ctx = self.ctx;
                    if self.current() != Some(SyntaxKind::DASH) {
                        ctx.scalar_continuation_floor = Some(base_indent);
                    }
                    // A lone anchor may annotate an indentless sequence, whose
                    // entries sit at the *key's* column rather than the
                    // anchor's (`seq:\n &a\n- x\n`). Hand it the key's base
                    // so it can adopt them, but as a value, so that a mapping
                    // there is still a sibling entry (`a:\n &x\nb: 1\n`).
                    // Anything else measures from its own line as before.
                    let value_indent = if self.current() == Some(SyntaxKind::ANCHOR) {
                        ctx.annotation_in_value_position = true;
                        base_indent
                    } else {
                        self.current_line_indent
                    };
                    ctx.mapping_block_value_column = Some(base_indent.max(key_column));
                    self.scoped(ctx, |p| p.parse_value_with_base_indent(value_indent));
                    has_value = true;
                } else if indentless_sequence {
                    // An indentless sequence: its entries sit at the key's
                    // own column rather than past it, so it is the key's
                    // value even though nothing is more indented. A mapping
                    // key at that column would be a sibling entry instead
                    // (`- k:\n  j: 1\n`), which is why only a dash counts.
                    self.parse_sequence_with_base_indent(self.current_line_indent);
                    has_value = true;
                } else if block_scalar {
                    // A block scalar header can only be this key's value,
                    // whatever its column: nothing else starts with `|` or
                    // `>`. The validator reports one not indented past the
                    // key. Its body still has to clear the key's column, so
                    // `a:\n|\nb: 1\n` keeps `b` a sibling.
                    let column = base_indent.max(key_column);
                    let ctx = BlockCtx {
                        mapping_value_column: Some(column),
                        mapping_block_value_column: Some(column),
                        ..self.ctx
                    };
                    self.scoped(ctx, |p| {
                        if p.current() == Some(SyntaxKind::PIPE) {
                            p.parse_literal_block_scalar();
                        } else {
                            p.parse_folded_block_scalar();
                        }
                    });
                    has_value = true;
                }
                // Otherwise the "value" would be at the parent's indent or
                // less, so this key has an implicit null value and what
                // follows is a sibling entry.
            }

            // If no value present, create an implicit null scalar. A
            // comment ending the value's last line goes in the VALUE, as
            // in `value  # comment`.
            if has_value {
                self.parse_rest_of_entry_line(base_indent.max(key_column));
                self.skip_line_end_trivia();
            } else {
                self.emit_implicit_null();
            }

            self.builder.finish_node(); // VALUE
        } else {
            let error_msg = self.create_detailed_error(
                "Missing colon in mapping",
                "':' after key",
                self.current_text(),
            );
            self.add_error(error_msg, ParseErrorKind::Other);
            // Read as a key whose value was omitted, with a zero-width
            // COLON where the `:` should be.
            self.builder.token(SyntaxKind::COLON.into(), "");
            self.builder.start_node(SyntaxKind::VALUE.into());
            self.emit_implicit_null();
            self.parse_rest_of_entry_line(base_indent.max(key_column));
            self.builder.finish_node();
        }

        self.skip_entry_line_end();

        // Finish MAPPING_ENTRY node
        self.builder.finish_node();
    }
}

/// Whether a token of `kind` starts the node of an implicit block key, as
/// [`Parser::parse_block_key_node`] reads it.
fn starts_block_key_node(kind: SyntaxKind) -> bool {
    is_plain_scalar_kind(kind)
        || matches!(
            kind,
            SyntaxKind::ANCHOR
                | SyntaxKind::TAG
                | SyntaxKind::MERGE_KEY
                | SyntaxKind::REFERENCE
                | SyntaxKind::LEFT_BRACKET
                | SyntaxKind::LEFT_BRACE
                | SyntaxKind::COLON
        )
}

/// Whether `kinds`, the tokens after a complete key node such as a flow
/// collection, reach the key's colon on the same line.
///
/// Plain scalar content in between is not part of a valid key, but the
/// parser keeps it in the key as a second node, which the validator
/// reports: `[q] x: v`.
pub(super) fn reaches_key_colon(kinds: impl Iterator<Item = SyntaxKind>) -> bool {
    for kind in kinds {
        match kind {
            SyntaxKind::COLON => return true,
            SyntaxKind::WHITESPACE | SyntaxKind::INDENT => continue,
            kind if is_plain_scalar_kind(kind) => continue,
            _ => return false,
        }
    }
    false
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

    /// The text of every block mapping entry in `src`, in document order.
    fn entry_texts(src: &str) -> Vec<String> {
        let file = YamlFile::from_str(src).expect("should parse");
        rowan::ast::AstNode::syntax(&file)
            .descendants()
            .filter(|n| n.kind() == crate::lex::SyntaxKind::MAPPING_ENTRY)
            .map(|n| n.to_string())
            .collect()
    }

    /// An empty value leaves the lines after it, which the parser looked
    /// at for a value, to the collection.
    #[test]
    fn test_empty_value_leaves_following_lines() {
        assert_eq!(entry_texts("a:\n\nb: 1\n"), ["a:\n", "b: 1\n"]);
        assert_eq!(entry_texts("a: &x\n\n  # c\nb: 1\n"), ["a: &x\n", "b: 1\n"]);
        assert_eq!(
            entry_texts("x:\n  a:  # c\n  b: 1\n"),
            ["x:\n  a:  # c\n  b: 1\n", "a:  # c\n", "b: 1\n"]
        );
        assert_eq!(entry_texts("? a\n:\n\n# c\nb: 1\n"), ["? a\n:\n", "b: 1\n"]);
        assert_eq!(entry_texts("? a  # c\n\nb: 1\n"), ["? a  # c\n", "b: 1\n"]);
        assert_eq!(entry_texts("- ? a\n  b: 1\n"), ["? a\n", "b: 1\n"]);
    }

    /// The lines after a nested collection's last entry belong to the
    /// collection holding the next entry, or the root one at the end.
    #[test]
    fn test_nested_collection_leaves_following_lines() {
        assert_eq!(
            entry_texts("a:\n  - x\n\n  # c\nb: 1\n"),
            ["a:\n  - x\n", "b: 1\n"]
        );
        assert_eq!(entry_texts("? - a\n\n: b\n"), ["? - a\n\n: b\n"]);
        assert_eq!(entry_texts("a:\n  x: 1\n\n"), ["a:\n  x: 1\n", "x: 1\n"]);
        assert_eq!(
            entry_texts("a:\n  b:\n    - x\n\n  c: 1\n"),
            ["a:\n  b:\n    - x\n\n  c: 1\n", "b:\n    - x\n", "c: 1\n"]
        );
    }

    /// A flow entry owns its comma, with anything before it, and the rest
    /// of that line; the lines after it are the collection's.
    #[test]
    fn test_flow_entry_owns_its_line() {
        assert_eq!(
            entry_texts("{a: 1, # c\n # d\n b: 2 # e\n}\n"),
            ["a: 1, # c\n", "b: 2 # e\n"]
        );
        assert_eq!(entry_texts("{\n?\n}\n"), ["?\n"]);
        assert_eq!(
            entry_texts("{a: 1 # c\n , b: 2}\n"),
            ["a: 1 # c\n , ", "b: 2"]
        );
        assert_eq!(
            entry_texts("{\n  a: 1,\n\n  b: 2\n  }\n"),
            ["a: 1,\n", "b: 2\n"]
        );
    }

    /// A mapping entry owns the comment ending its last line, and the line
    /// break after it, whether its key is explicit or its value on a later
    /// line.
    #[test]
    fn test_mapping_entry_owns_line_end() {
        assert_eq!(
            entry_texts("? a\n: b  # c\nd: 1\n"),
            ["? a\n: b  # c\n", "d: 1\n"]
        );
        assert_eq!(
            entry_texts("a:\n  b  # c\nd: 1\n"),
            ["a:\n  b  # c\n", "d: 1\n"]
        );
        assert_eq!(entry_texts("? - a\n# c\nd: 1\n"), ["? - a\n", "d: 1\n"]);
    }

    /// A sequence entry owns the comment ending its line, and the line
    /// break after it, as a mapping entry does. A comment line after a
    /// block item is the sequence's.
    #[test]
    fn test_sequence_entry_owns_line_comment() {
        let file = YamlFile::from_str("- a  # c\n- |\n  x\n# d\n- b # e").unwrap();
        let entries: Vec<String> = rowan::ast::AstNode::syntax(&file)
            .descendants()
            .filter(|n| n.kind() == crate::lex::SyntaxKind::SEQUENCE_ENTRY)
            .map(|n| n.to_string())
            .collect();
        assert_eq!(entries, ["- a  # c\n", "- |\n  x\n", "- b # e"]);
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

    /// A comment line before a dedented line ends a nested explicit-key
    /// mapping as it ends an implicit-key one: it is left to the mapping
    /// holding the next entry.
    #[test]
    fn test_comment_line_after_nested_explicit_key_block() {
        use crate::lex::SyntaxKind;
        use rowan::ast::AstNode;
        for src in [
            "x:\n  a: b\n  # c\ny: 1\n",
            "x:\n  ? a\n  : b\n  # c\ny: 1\n",
            "x:\n  ? a\n  : b\n\n  # c\n  # d\n\ny: 1\n",
        ] {
            let file = YamlFile::from_str(src).unwrap();
            let root = file.document().unwrap().as_mapping().unwrap();
            let comments: Vec<_> = root
                .syntax()
                .children_with_tokens()
                .filter(|c| c.kind() == SyntaxKind::COMMENT)
                .collect();
            assert_ne!(comments, vec![], "{src:?}");
            assert_eq!(top_level_keys(src), ["x", "y"], "{src:?}");
        }
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

    /// A compact explicit-key mapping as a value: its later entries sit at
    /// the first `?`'s column, which used to read as a dedent.
    #[test]
    fn test_compact_explicit_key_mapping_as_value() {
        let src = "? h\n: ? b\n  : x\n  a: 1\nc: 2\n";
        assert_eq!(top_level_keys(src), ["h", "c"]);
        let parsed = YamlFile::from_str(src).unwrap();
        let h = parsed.document().unwrap().as_mapping().unwrap().get("h");
        let keys: Vec<String> = h
            .unwrap()
            .as_mapping()
            .unwrap()
            .keys()
            .map(|k| k.to_string())
            .collect();
        assert_eq!(keys, ["b", "a"]);
    }

    /// Top-level explicit keys were never affected; this guards against a
    /// dedent check that is too eager.
    #[test]
    fn test_top_level_explicit_keys_are_unaffected() {
        assert_eq!(top_level_keys("? a\n: 1\nplain: v\n"), ["a", "plain"]);
    }

    /// The value of the single document in `src`, with a check that the
    /// tree is well-formed and that the validator accepts it.
    fn checked_value(src: &str) -> crate::YamlValue {
        use rowan::ast::AstNode;
        let file = YamlFile::from_str(src).expect("should parse");
        crate::debug::validate_tree(file.syntax()).expect("tree should be valid");
        let doc = file.document().expect("should have a document");
        assert_eq!(crate::validator::Validator::new().validate(&doc), vec![]);
        crate::YamlValue::from_document(&doc).expect("should convert")
    }

    /// What is left of a block entry's line after its node is more content
    /// of the same slot, for the validator to report, rather than another
    /// entry opening on that line, which left this one unterminated.
    #[test]
    fn test_rest_of_entry_line_stays_in_slot() {
        use rowan::ast::AstNode;
        for src in [
            ": :",
            "a: :\nb: 1\n",
            "a: ]: x\n",
            "a: * c: x\n",
            "a: {}{: x\n",
            "- {: -\n- b\n",
            "- [: -\n",
            ":\n * c: x\n",
            ":\n|: x\n",
            "?\n: >: x\n",
            "? |b: x\n",
            "<< <<\n",
        ] {
            let parse = YamlFile::parse(src);
            let file = parse.tree();
            assert_eq!(
                crate::debug::validate_tree(file.syntax()),
                Ok(()),
                "{src:?}"
            );
            assert_eq!(file.to_string(), src);
            let violations = file
                .documents()
                .flat_map(|d| crate::validator::Validator::new().validate(&d))
                .count();
            assert_ne!(parse.errors().len() + violations, 0, "{src:?}");
        }
        assert_eq!(top_level_keys("a: :\nb: 1\n"), ["a", "b"]);
        assert_eq!(top_level_keys("x:\n  a: :\n  b: 1\nc: 2\n"), ["x", "c"]);
        let parsed = YamlFile::from_str("a: :\nb: 1\n").unwrap();
        assert_eq!(
            crate::debug::tree_to_string(parsed.syntax()),
            "\
DOCUMENT
  MAPPING
    MAPPING_ENTRY
      KEY
        SCALAR
          STRING: \"a\"
      COLON: \":\"
      WHITESPACE: \" \"
      VALUE
        SCALAR
          NULL: \"\"
        MAPPING
          MAPPING_ENTRY
            KEY
              SCALAR
                NULL: \"\"
            COLON: \":\"
            VALUE
              SCALAR
                NULL: \"\"
            NEWLINE: \"\\\\n\"
    MAPPING_ENTRY
      KEY
        SCALAR
          STRING: \"b\"
      COLON: \":\"
      WHITESPACE: \" \"
      VALUE
        SCALAR
          INT: \"1\"
      NEWLINE: \"\\\\n\"
"
        );
    }

    /// A `:` line left of an explicit key's `?` is the value of an
    /// enclosing explicit key, not of the one nested in its key.
    #[test]
    fn test_explicit_value_left_of_nested_explicit_key() {
        for (src, flow) in [
            ("? - ? a\n: b\n", "? [{? a}]\n: b\n"),
            ("? - ? - a\n: b\n", "? [{? [a]}]\n: b\n"),
            (
                "x:\n  ? - ? -\n  : - c\n",
                "x:\n  ? [{? [null]}]\n  : [c]\n",
            ),
        ] {
            assert_eq!(checked_value(src), checked_value(flow), "{src:?}");
        }
    }

    /// A sequence on the line of an explicit key is the key's, however much
    /// space follows the indicators before it.
    #[test]
    fn test_compact_sequence_key_after_wide_indicators() {
        for (src, compact) in [
            ("-  ? -", "- ? -"),
            ("-  ? -\n", "- ? -\n"),
            ("-  ? - a\n", "- ? - a\n"),
            (
                "-   ?   - a\n        - b\n    : c\n",
                "- ? - a\n    - b\n  : c\n",
            ),
        ] {
            assert_eq!(checked_value(src), checked_value(compact), "{src:?}");
        }
    }

    /// An explicit value's plain scalar continues only on lines indented
    /// past the `:`; a line at its column is not part of it.
    #[test]
    fn test_explicit_value_scalar_continuation() {
        for (src, content, offset) in [
            ("? a\n: 1\nb\n", "b", 8),
            ("? a\n: 1\n[a, b]\n", "[", 8),
            ("- ? a\n  : 1\n  b\n", "b", 14),
            ("? a\n: 1\n\nb\n", "b", 9),
        ] {
            let errors: Vec<(String, u32)> = crate::Parse::parse_yaml(src)
                .positioned_errors()
                .iter()
                .map(|e| (e.message.clone(), e.range.start))
                .collect();
            assert_eq!(
                errors,
                [(
                    format!("Content after the document could not be parsed: {content:?}"),
                    offset
                )],
                "{src:?}"
            );
        }
        for (src, expected) in [
            ("? a\n: 1\n b\n", "{a: 1 b}"),
            ("- ? a\n  : 1\n   b\n", "[{a: 1 b}]"),
            ("? a\n: 1\n? b\n", "{a: 1, b: }"),
        ] {
            assert_eq!(checked_value(src), checked_value(expected), "{src:?}");
        }
    }

    /// A flow collection key after an explicit-key entry is the same
    /// mapping's next entry.
    #[test]
    fn test_flow_collection_key_after_explicit_key_entry() {
        for (src, expected) in [
            ("? a\n: 1\n[a, b]: 2\n", "{a: 1, [a, b]: 2}"),
            ("? a\n[a, b]: 2\n", "{a: , [a, b]: 2}"),
            ("? a\n: 1\n{a: 1}: 2\n", "{a: 1, {a: 1}: 2}"),
            ("? a\n: 1\n&x [a, b]: 2\n", "{a: 1, &x [a, b]: 2}"),
            ("? a\n: 1\n!!seq [a, b]: 2\n", "{a: 1, !!seq [a, b]: 2}"),
            ("- ? a\n  : 1\n  [a, b]: 2\n", "[{a: 1, [a, b]: 2}]"),
            ("k:\n  ? a\n  : 1\n  [a, b]: 2\n", "{k: {a: 1, [a, b]: 2}}"),
            (
                "? a\n: 1\n[a, b]: 2\n? c\n: 3\n[d]: 4\n",
                "{a: 1, [a, b]: 2, c: 3, [d]: 4}",
            ),
        ] {
            assert_eq!(checked_value(src), checked_value(expected), "{src:?}");
        }
    }

    /// A tag ending a compact mapping's first line annotates a null when the
    /// next line is at the key's column, as it does at the line start.
    #[test]
    fn test_sibling_after_tagged_null_in_compact_mapping() {
        for (src, expected) in [
            ("- a: !!null\n  b: c\n", "[{a: !!null , b: c}]"),
            ("- a: !!null &x\n  b: c\n", "[{a: !!null &x , b: c}]"),
            ("- a: &x !!null\n  b: c\n", "[{a: &x !!null , b: c}]"),
            ("- a: !!null # c\n  b: c\n", "[{a: !!null , b: c}]"),
            ("- !!str a: !!null\n  b: c\n", "[{!!str a: !!null , b: c}]"),
            ("- [a]: !!str\n  b: c\n", "[{[a]: !!str , b: c}]"),
            ("k:\n  - a: !!null\n    b: c\n", "{k: [{a: !!null , b: c}]}"),
            ("? x\n: a: !!null\n  b: c\n", "{x: {a: !!null , b: c}}"),
            ("- a: !!str\n   x\n  b: c\n", "[{a: !!str x, b: c}]"),
            ("- a: !!null\n    b: c\n", "[{a: !!null {b: c}}]"),
        ] {
            assert_eq!(checked_value(src), checked_value(expected), "{src:?}");
        }
    }

    /// A compact mapping as an explicit value ends where the next entry of
    /// the enclosing mapping starts, left of its first key.
    #[test]
    fn test_entry_after_compact_mapping_explicit_value() {
        for (src, expected) in [
            ("? a\n: b: c\n? d\n: e\n", "{a: {b: c}, d: e}"),
            ("? a\n: b: c\nd: e\n", "{a: {b: c}, d: e}"),
            ("x: 1\n? a\n: b: c\n? d\n: e\n", "{x: 1, a: {b: c}, d: e}"),
            ("? a\n: b: c\n  f: g\n? d\n: e\n", "{a: {b: c, f: g}, d: e}"),
            ("? a\n: !!map b: c\n? d\n: e\n", "{a: {b: c}, d: e}"),
            ("? a\n: &x b: c\n? d\n: e\n", "{a: {b: c}, d: e}"),
            ("? a\n: {x: 1}: c\n? d\n: e\n", "{a: {{x: 1}: c}, d: e}"),
            ("- ? a\n  : b: c\n  ? d\n  : e\n", "[{a: {b: c}, d: e}]"),
            (
                "k:\n  ? a\n  : b: c\n  ? d\n  : e\n",
                "{k: {a: {b: c}, d: e}}",
            ),
        ] {
            assert_eq!(checked_value(src), checked_value(expected), "{src:?}");
        }
    }

    /// Comment lines after a nested block collection's last entry with no
    /// content after them belong to the root collection, as do those
    /// before a line that does not continue a nested sequence.
    #[test]
    fn test_lines_ending_nested_collection_go_to_root() {
        for src in [
            "a:\n  b: 1\n  # c\n\n",
            "a:\n  b: 1\n  # c",
            "a:\n  b:\n    c: 1\n    # c\n  # d\n",
            "a:\n- x\n# c\n",
            "a:\n- x\n# c\nb: 1\n",
            "- - a\n  # c\n",
            "a:\n  b: 1\n  # c\n---\nx\n",
        ] {
            let parsed = YamlFile::from_str(src).unwrap();
            let syntax = rowan::ast::AstNode::syntax(&parsed);
            assert_eq!(crate::debug::validate_tree(syntax), Ok(()), "{src:?}");
            let root = syntax.first_child().unwrap().first_child().unwrap();
            for comment in syntax
                .descendants_with_tokens()
                .filter(|c| c.kind() == crate::lex::SyntaxKind::COMMENT)
            {
                assert_eq!(comment.parent().as_ref(), Some(&root), "{src:?}");
            }
        }

        let parsed = YamlFile::from_str("a:\n  b: 1\n  # c\n\n").unwrap();
        assert_eq!(
            crate::debug::tree_to_string(rowan::ast::AstNode::syntax(&parsed)),
            "\
DOCUMENT
  MAPPING
    MAPPING_ENTRY
      KEY
        SCALAR
          STRING: \"a\"
      COLON: \":\"
      VALUE
        NEWLINE: \"\\\\n\"
        INDENT: \"  \"
        MAPPING
          MAPPING_ENTRY
            KEY
              SCALAR
                STRING: \"b\"
            COLON: \":\"
            WHITESPACE: \" \"
            VALUE
              SCALAR
                INT: \"1\"
            NEWLINE: \"\\\\n\"
    INDENT: \"  \"
    COMMENT: \"# c\"
    NEWLINE: \"\\\\n\"
    NEWLINE: \"\\\\n\"
"
        );
    }

    /// Content after a flow collection value stays in the VALUE rather
    /// than being left after the document.
    #[test]
    fn test_content_after_flow_value_stays_in_value() {
        let parsed = YamlFile::from_str("k:\n  [q] x\n").unwrap();
        assert_eq!(
            crate::debug::tree_to_string(rowan::ast::AstNode::syntax(&parsed)),
            "\
DOCUMENT
  MAPPING
    MAPPING_ENTRY
      KEY
        SCALAR
          STRING: \"k\"
      COLON: \":\"
      VALUE
        NEWLINE: \"\\\\n\"
        INDENT: \"  \"
        SEQUENCE
          LEFT_BRACKET: \"[\"
          SEQUENCE_ENTRY
            SCALAR
              STRING: \"q\"
          RIGHT_BRACKET: \"]\"
        WHITESPACE: \" \"
        SCALAR
          STRING: \"x\"
      NEWLINE: \"\\\\n\"
"
        );
    }

    /// Content after a flow collection value may continue on, or start on,
    /// a more indented line, as a plain scalar would.
    #[test]
    fn test_content_after_flow_value_on_later_lines() {
        let parsed = YamlFile::from_str("k:\n  [q] x\n  y\n").unwrap();
        assert_eq!(
            crate::debug::tree_to_string(rowan::ast::AstNode::syntax(&parsed)),
            "\
DOCUMENT
  MAPPING
    MAPPING_ENTRY
      KEY
        SCALAR
          STRING: \"k\"
      COLON: \":\"
      VALUE
        NEWLINE: \"\\\\n\"
        INDENT: \"  \"
        SEQUENCE
          LEFT_BRACKET: \"[\"
          SEQUENCE_ENTRY
            SCALAR
              STRING: \"q\"
          RIGHT_BRACKET: \"]\"
        WHITESPACE: \" \"
        SCALAR
          STRING: \"x\"
          NEWLINE: \"\\\\n\"
          INDENT: \"  \"
          STRING: \"y\"
      NEWLINE: \"\\\\n\"
"
        );

        let parsed = YamlFile::from_str("k:\n  [q]\n  y\n").unwrap();
        assert_eq!(
            crate::debug::tree_to_string(rowan::ast::AstNode::syntax(&parsed)),
            "\
DOCUMENT
  MAPPING
    MAPPING_ENTRY
      KEY
        SCALAR
          STRING: \"k\"
      COLON: \":\"
      VALUE
        NEWLINE: \"\\\\n\"
        INDENT: \"  \"
        SEQUENCE
          LEFT_BRACKET: \"[\"
          SEQUENCE_ENTRY
            SCALAR
              STRING: \"q\"
          RIGHT_BRACKET: \"]\"
        NEWLINE: \"\\\\n\"
        INDENT: \"  \"
        SCALAR
          STRING: \"y\"
      NEWLINE: \"\\\\n\"
"
        );
    }

    /// Content between a flow collection key and its colon stays in the
    /// KEY, so `x: v` does not become a second entry on the same line.
    #[test]
    fn test_content_after_flow_key_stays_in_key() {
        let parsed = YamlFile::from_str("k:\n  [q] x: v\n").unwrap();
        assert_eq!(
            crate::debug::tree_to_string(rowan::ast::AstNode::syntax(&parsed)),
            "\
DOCUMENT
  MAPPING
    MAPPING_ENTRY
      KEY
        SCALAR
          STRING: \"k\"
      COLON: \":\"
      VALUE
        NEWLINE: \"\\\\n\"
        INDENT: \"  \"
        MAPPING
          MAPPING_ENTRY
            KEY
              SEQUENCE
                LEFT_BRACKET: \"[\"
                SEQUENCE_ENTRY
                  SCALAR
                    STRING: \"q\"
                RIGHT_BRACKET: \"]\"
              WHITESPACE: \" \"
              SCALAR
                STRING: \"x\"
            COLON: \":\"
            WHITESPACE: \" \"
            VALUE
              SCALAR
                STRING: \"v\"
            NEWLINE: \"\\\\n\"
"
        );
    }

    /// A key without its `:` is read as a key whose value was omitted, with
    /// a zero-width COLON where the `:` should be, whatever the key sits in.
    #[test]
    fn test_missing_colon_reads_as_omitted_value() {
        let tree = |src: &str| {
            let parsed = YamlFile::parse(src);
            assert_eq!(parsed.errors().len(), 1, "{src:?}: {:?}", parsed.errors());
            crate::debug::tree_to_string(rowan::ast::AstNode::syntax(&parsed.tree()))
        };
        let entry = "    MAPPING_ENTRY
      KEY
        SCALAR
          MERGE_KEY: \"<<\"
      COLON: \"\"
      VALUE
        SCALAR
          NULL: \"\"
      NEWLINE: \"\\\\n\"
";
        assert_eq!(tree("<<\n"), format!("DOCUMENT\n  MAPPING\n{entry}"));
        assert_eq!(
            tree("?\n<<\n"),
            format!(
                "\
DOCUMENT
  MAPPING
    MAPPING_ENTRY
      QUESTION: \"?\"
      KEY
        SCALAR
          NULL: \"\"
      VALUE
        SCALAR
          NULL: \"\"
      NEWLINE: \"\\\\n\"
{entry}"
            )
        );
    }

    /// The lines after a key missing its `:` are read as they would be after
    /// any other entry, rather than swallowed into it: here, a document.
    #[test]
    fn test_missing_colon_keeps_following_document() {
        let parsed = YamlFile::parse("?\n<<\n---\na: 1\n");
        let docs: Vec<String> = parsed.tree().documents().map(|d| d.to_string()).collect();
        assert_eq!(docs, ["?\n<<\n", "---\na: 1\n"]);
    }
}
