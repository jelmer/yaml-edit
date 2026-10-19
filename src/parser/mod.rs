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
    line_index::LineIndex,
    yaml::ParsedYaml,
    ParseErrorKind, PositionedParseError,
};
use rowan::GreenNodeBuilder;
use std::cell::Cell;

/// Maximum nesting depth for collections and stacked node properties.
/// Beyond this, the parser returns an error rather than risking stack
/// overflow or unbounded RSS growth from pathological input like
/// `{{{{...}}}}` or a long `a:\n  a:\n  ...` chain.
pub(super) const MAX_FLOW_DEPTH: usize = 256;

/// Context that holds for the block node being parsed and everything
/// nested in it, unless a nested node overrides it.
///
/// Only [`Parser::scoped`] changes it, so the outer context is back in place
/// however the nested parse returns.
#[derive(Clone, Copy, Default)]
pub(super) struct BlockCtx {
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
    /// Column of the `-` of the block sequence entry being parsed, if any.
    ///
    /// A `-` on a later line opens the next entry when it sits at this
    /// column, and is plain-scalar content when it is indented past it:
    /// `a:\n- x\n  - y\n` is the single item `x - y`. Comparing against the
    /// scalar's own line cannot tell those apart, since an explicit key
    /// (`? - a\n  - b\n`) puts its entries deeper than the scalar too.
    pub(super) sequence_entry_column: Option<usize>,
    /// The column of the mapping whose value is being parsed on the colon's
    /// own line, for deciding whether a `?` on a later line opens that
    /// mapping's next entry rather than continuing the value's scalar.
    pub(super) mapping_value_column: Option<usize>,
    /// The column of the mapping whose value is being parsed on a line
    /// after its key's, the counterpart of `mapping_value_column`.
    ///
    /// A block scalar's indentation indicator counts from its enclosing
    /// collection's column, and in `o:\n  k:\n    |1\n` nothing else
    /// records that `k`'s mapping sits at column 2.
    pub(super) mapping_block_value_column: Option<usize>,
    /// Column of the `?` whose key is being parsed, if any.
    ///
    /// A `:` line at or left of that column is the entry's value, so an
    /// annotation in the key must not adopt it as the block node it
    /// introduces: `-\n  ? &e\n  : &a\n` is one mapping, as the YAML test
    /// suite's PW8X expects, not an anchor whose body is a nested one.
    pub(super) explicit_key_column: Option<usize>,
    /// Column of the `:` whose explicit value is being parsed, if any, the
    /// counterpart of `explicit_key_column`.
    pub(super) explicit_value_column: Option<usize>,
    /// Whether the node being parsed sits in a block mapping's value
    /// position, where a block body has to clear the key's column.
    ///
    /// A tag hands this to the anchor that may follow it: `k: !!str &a` runs
    /// the tag arm, which declines to adopt a dedented sibling, and then
    /// falls through to the anchor arm. Without the flag that arm asks the
    /// same question as a document-level anchor and answers it differently,
    /// adopting the sibling the tag arm just refused.
    pub(super) annotation_in_value_position: bool,
}

/// The parser's remaining tokens in source order, the current one at 0.
///
/// [`Parser::tokens`] holds them reversed, so that `bump` can pop.
#[derive(Clone, Copy)]
pub(super) struct Lookahead<'a>(&'a [(SyntaxKind, String)]);

impl<'a> Lookahead<'a> {
    pub(super) fn len(self) -> usize {
        self.0.len()
    }

    /// The token `n` places after the current one.
    pub(super) fn get(self, n: usize) -> Option<&'a (SyntaxKind, String)> {
        self.0.get(self.0.len().checked_sub(n + 1)?)
    }

    /// The kind of the token `n` places after the current one.
    pub(super) fn kind(self, n: usize) -> Option<SyntaxKind> {
        self.get(n).map(|(kind, _)| *kind)
    }
}

fn is_collection(kind: &rowan::SyntaxKind) -> bool {
    *kind == SyntaxKind::MAPPING.into() || *kind == SyntaxKind::SEQUENCE.into()
}

/// A tree builder that can hold back the line breaks, indentation and
/// comment lines between the entries of a block collection until it is
/// known which node they belong to.
///
/// They go in front of whatever is emitted next. A node finished in the
/// meantime leaves them to its parent, so the blank and comment lines after
/// a nested collection's last entry end up in the collection holding the
/// next one.
/// The outermost block collection keeps those at its end.
pub(super) struct TreeBuilder {
    inner: GreenNodeBuilder<'static>,
    open: Vec<rowan::SyntaxKind>,
    held: Vec<(SyntaxKind, String)>,
}

impl TreeBuilder {
    fn new() -> Self {
        Self {
            inner: GreenNodeBuilder::new(),
            open: Vec::new(),
            held: Vec::new(),
        }
    }

    fn flush(&mut self) {
        for (kind, text) in std::mem::take(&mut self.held) {
            self.inner.token(kind.into(), &text);
        }
    }

    pub(super) fn token(&mut self, kind: rowan::SyntaxKind, text: &str) {
        self.flush();
        self.inner.token(kind, text);
    }

    fn hold(&mut self, kind: SyntaxKind, text: String) {
        self.held.push((kind, text));
    }

    pub(super) fn start_node(&mut self, kind: rowan::SyntaxKind) {
        self.flush();
        self.open.push(kind);
        self.inner.start_node(kind);
    }

    pub(super) fn finish_node(&mut self) {
        let kind = self.open.pop().expect("a node is open");
        if is_collection(&kind) && !self.open.iter().any(is_collection) {
            self.flush();
        }
        self.inner.finish_node();
    }

    /// Whether the collection being built is inside another one.
    pub(super) fn in_nested_collection(&self) -> bool {
        self.open.iter().filter(|k| is_collection(k)).count() > 1
    }

    /// Run `f` ahead of the trivia held back, for a zero-width node that
    /// ends what came before it.
    fn ahead_of_held(&mut self, f: impl FnOnce(&mut Self)) {
        let held = std::mem::take(&mut self.held);
        f(self);
        self.held = held;
    }

    fn finish(mut self) -> rowan::GreenNode {
        self.flush();
        self.inner.finish()
    }
}

/// Internal parser state
///
/// Fields are `pub(super)` so the topic sub-modules
/// ([`scalars`], [`flow`], [`block`]) can add `impl Parser` blocks
/// without going through accessor methods for every field.
pub(super) struct Parser<'a> {
    pub(super) tokens: Vec<(SyntaxKind, String)>,
    pub(super) builder: TreeBuilder,
    /// Whether [`bump`](Self::bump) holds back line breaks, indentation
    /// and white space; see [`TreeBuilder`].
    hold_trivia: bool,
    pub(super) errors: Vec<String>,
    pub(super) positioned_errors: Vec<PositionedParseError>,
    pub(super) in_flow_context: bool,
    /// The stack of contexts that picks an error recovery strategy.
    ///
    /// Positions are tracked by `position` and `line_index` instead, so it
    /// is given no text.
    pub(super) error_context: ErrorRecoveryContext,
    /// Byte offset of the current token.
    position: usize,
    line_index: LineIndex<&'a str>,
    /// Track the current line's indentation level for plain scalar continuation
    pub(super) current_line_indent: usize,
    /// Context of the block node being parsed; see [`BlockCtx`].
    pub(super) ctx: BlockCtx,
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
    /// The last [`indent_after_comment_lines`](Self::indent_after_comment_lines)
    /// scan, as the `tokens.len()` it started and stopped at and its result.
    ///
    /// Every comment of a run gets asked about in turn, and each would
    /// otherwise rescan the rest of the run.
    comment_run_scan: Cell<Option<(usize, usize, Option<(usize, SyntaxKind)>)>>,
    /// The kind of the token emitted last.
    pub(super) previous_kind: Option<SyntaxKind>,
    /// `tokens.len()` at the root node when it starts on the `---` line.
    ///
    /// A block sequence there (`--- - a\n- b\n`) is invalid, which the
    /// validator reports, but its entries line up with the line's start
    /// rather than with the first dash, as a block mapping's do.
    pub(super) marker_line_node: Option<usize>,
}

impl<'a> Parser<'a> {
    fn new(text: &'a str) -> Self {
        let lexed = lex(text);
        let mut tokens = Vec::new();

        for (kind, token_text) in lexed {
            tokens.push((kind, token_text.to_string()));
        }

        // Reverse tokens so we can use pop() to get the next token
        tokens.reverse();

        Self {
            tokens,
            builder: TreeBuilder::new(),
            hold_trivia: false,
            errors: Vec::new(),
            positioned_errors: Vec::new(),
            in_flow_context: false,
            error_context: ErrorRecoveryContext::new(String::new()),
            position: 0,
            line_index: LineIndex::new(text),
            ctx: BlockCtx::default(),
            node_is_document_root: false,
            current_line_indent: 0,
            flow_depth: 0,
            nesting_depth: 0,
            comment_run_scan: Cell::new(None),
            previous_kind: None,
            marker_line_node: None,
        }
    }

    fn parse(mut self) -> ParsedYaml {
        self.builder.start_node(SyntaxKind::ROOT.into());

        // A BOM, blank lines and comments in front of the first document's
        // content belong to that document, so a Document round-trips them.
        // Before a directive, a `---` after a comment or no document at all,
        // they are part of the stream prefix instead.
        let blank = [
            SyntaxKind::BOM,
            SyntaxKind::WHITESPACE,
            SyntaxKind::NEWLINE,
            SyntaxKind::INDENT,
        ];
        let prefix_in_document =
            match self.next_kind_past(&[&blank[..], &[SyntaxKind::COMMENT]].concat()) {
                None | Some(SyntaxKind::DIRECTIVE | SyntaxKind::EOF | SyntaxKind::DOC_END) => false,
                Some(SyntaxKind::DOC_START) => {
                    self.next_kind_past(&blank) == Some(SyntaxKind::DOC_START)
                }
                Some(_) => true,
            };
        if !prefix_in_document {
            self.skip_tokens(&[SyntaxKind::BOM]);
            self.skip_ws_and_newlines();
        }

        // Content with no `---` of its own opens a document only at the start
        // of the stream, after a directive or after a `...` (`a\n...\nb: 1\n`
        // is two documents). Anywhere else it is stray.
        let mut bare_document_allowed = true;
        if prefix_in_document {
            bare_document_allowed = self.parse_document();
        }
        loop {
            self.skip_ws_and_newlines();
            match self.current() {
                None | Some(SyntaxKind::EOF) => break,
                Some(SyntaxKind::DIRECTIVE) => {
                    self.parse_directive();
                    bare_document_allowed = true;
                }
                // A `...` that ends a document is consumed by that document.
                // One with no document before it closes nothing and stays at
                // the root (YAML test suite HWV9, QT73).
                Some(SyntaxKind::DOC_END) => {
                    self.parse_doc_end();
                    bare_document_allowed = true;
                }
                Some(SyntaxKind::DOC_START) => bare_document_allowed = self.parse_document(),
                Some(_) if bare_document_allowed => {
                    bare_document_allowed = self.parse_document();
                }
                Some(_) => self.parse_stray_content(),
            }
        }

        self.builder.finish_node();

        ParsedYaml {
            green_node: self.builder.finish(),
            errors: self.errors,
            positioned_errors: self.positioned_errors,
        }
    }

    /// Hold content that could not be attached to any document in an ERROR
    /// node, up to the next document or directive marker.
    fn parse_stray_content(&mut self) {
        // The content is gone from the parsed value. Say so: returning Ok
        // with it only in an ERROR node drops it silently.
        let stray = self.current_text().unwrap_or("").to_string();
        self.add_error(
            format!("Content after the document could not be parsed: {stray:?}"),
            ParseErrorKind::Other,
        );
        self.builder.start_node(SyntaxKind::ERROR.into());
        while !matches!(
            self.current(),
            None | Some(
                SyntaxKind::EOF
                    | SyntaxKind::DOC_START
                    | SyntaxKind::DIRECTIVE
                    | SyntaxKind::DOC_END
            )
        ) {
            self.bump();
        }
        self.builder.finish_node();
    }

    /// Parse one document, returning whether it was closed by a `...`.
    fn parse_document(&mut self) -> bool {
        self.builder.start_node(SyntaxKind::DOCUMENT.into());

        // Only the first document can start with these; see parse().
        self.skip_tokens(&[
            SyntaxKind::BOM,
            SyntaxKind::WHITESPACE,
            SyntaxKind::NEWLINE,
            SyntaxKind::INDENT,
            SyntaxKind::COMMENT,
        ]);

        // Handle document start marker
        if self.current() == Some(SyntaxKind::DOC_START) {
            self.bump();
            self.skip_whitespace();
            self.marker_line_node = Some(self.tokens.len());
            self.skip_ws_and_newlines();
            if self.marker_line_node != Some(self.tokens.len()) {
                self.marker_line_node = None;
            }
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
            //
            // An annotated root is still such a scalar: `" !x t\no\n"` is
            // the tagged scalar `t o`, so look past any run of tags and
            // anchors to the node they cover. Only on the same line, though
            // -- an annotation alone on its line opens a block node, whose
            // body keeps its own indent.
            let root_ctx = BlockCtx {
                equal_indent_continues_scalar: true,
                ..self.ctx
            };
            let value_ctx = if self.root_annotations_cover_plain_scalar() {
                BlockCtx {
                    scalar_continuation_floor: Some(0),
                    ..root_ctx
                }
            } else {
                root_ctx
            };
            let outer_root = self.node_is_document_root;
            self.node_is_document_root = true;
            self.scoped(value_ctx, |p| p.parse_value());
            self.marker_line_node = None;

            // A line opening a mapping after the root node, as in
            // `a\n[x]: z\n`, neither continues that node nor starts a
            // document of its own. Keep the mapping as a second node of the
            // document, which the validator reports, rather than leaving it
            // after the document.
            self.scoped(root_ctx, |p| loop {
                p.skip_ws_and_newlines();
                if !p.opens_block_mapping() {
                    break;
                }
                let tokens_before = p.tokens.len();
                let base_indent = p.current_line_indent;
                p.parse_value_with_base_indent(base_indent);
                if p.tokens.len() == tokens_before {
                    break;
                }
            });
            self.node_is_document_root = outer_root;
        }

        // The line break, comments and blank lines after the root node belong
        // to the document, as does a `...` ending it. A block collection
        // takes them itself; a scalar or flow collection root stops at its
        // last character.
        self.skip_ws_and_newlines();
        let ends_with_marker = self.current() == Some(SyntaxKind::DOC_END);
        if ends_with_marker {
            self.parse_doc_end();

            // The rest of the marker's line ends the document with it.
            if self.current() == Some(SyntaxKind::COMMENT) {
                self.bump();
            }
            if self.current() == Some(SyntaxKind::NEWLINE) {
                self.bump();
            }
        }

        self.builder.finish_node();
        ends_with_marker
    }

    /// Consume a `...` and report any content after it on the same line.
    fn parse_doc_end(&mut self) {
        self.bump();

        // Check for content after document end marker (spec violation).
        // A comment is not content: `... # done` is as valid as the
        // `...\n# done` spelling, which already kept it in the tree.
        self.skip_whitespace();
        let is_stray = |kind: Option<SyntaxKind>| {
            kind.is_some_and(|kind| {
                !matches!(
                    kind,
                    SyntaxKind::NEWLINE
                        | SyntaxKind::EOF
                        | SyntaxKind::DOC_START
                        | SyntaxKind::DIRECTIVE
                        | SyntaxKind::COMMENT
                )
            })
        };
        if is_stray(self.current()) {
            // Found content after DOC_END. Both saphyr and PyYAML reject
            // this; say so rather than returning a document the content
            // has quietly been dropped from.
            let stray = self.current_text().unwrap_or("").to_string();
            self.add_error(
                format!("Content after the document end marker: {stray:?}"),
                ParseErrorKind::Other,
            );
            self.builder.start_node(SyntaxKind::ERROR.into());
            while is_stray(self.current()) {
                self.bump();
            }
            self.builder.finish_node();
        }
    }

    /// Run `f` with `ctx` as the block context, putting the current one
    /// back afterwards.
    pub(super) fn scoped<R>(&mut self, ctx: BlockCtx, f: impl FnOnce(&mut Self) -> R) -> R {
        let outer = std::mem::replace(&mut self.ctx, ctx);
        let result = f(self);
        self.ctx = outer;
        result
    }

    pub(super) fn lookahead(&self) -> Lookahead<'_> {
        Lookahead(&self.tokens)
    }

    pub(super) fn parse_value(&mut self) {
        self.parse_value_with_base_indent(0);
    }

    /// Whether the current token starts the first key of a block mapping.
    fn opens_block_mapping(&self) -> bool {
        match self.current() {
            Some(SyntaxKind::LEFT_BRACKET | SyntaxKind::LEFT_BRACE) => {
                self.is_complex_mapping_key()
            }
            Some(SyntaxKind::TAG | SyntaxKind::ANCHOR) => {
                self.is_mapping_key() || self.is_complex_mapping_key_after_annotations()
            }
            Some(
                SyntaxKind::REFERENCE
                | SyntaxKind::QUESTION
                | SyntaxKind::COLON
                | SyntaxKind::MERGE_KEY,
            ) => self.is_mapping_key(),
            Some(kind) => is_plain_scalar_kind(kind) && self.is_mapping_key(),
            None => false,
        }
    }

    pub(super) fn parse_value_with_base_indent(&mut self, base_indent: usize) {
        if self.nesting_depth >= MAX_FLOW_DEPTH {
            self.add_error(
                format!("Collection nested too deeply (limit {MAX_FLOW_DEPTH})"),
                ParseErrorKind::Other,
            );
            // The token goes in a scalar, so that the slot still holds a node.
            self.builder.start_node(SyntaxKind::SCALAR.into());
            if self.current().is_some() {
                self.bump();
            } else {
                self.builder.token(SyntaxKind::NULL.into(), "");
            }
            self.builder.finish_node();
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
                // An anchor introducing a complex key belongs inside that
                // key's KEY node, so let the mapping parser take it rather
                // than emitting it as a sibling of the mapping.
                if !self.in_flow_context && self.is_complex_mapping_key_after_annotations() {
                    self.parse_mapping_with_base_indent(base_indent);
                    self.nesting_depth -= 1;
                    return;
                }

                // Likewise for a plain key: `&k1 key1: one` anchors the key,
                // and parse_mapping_key_value_pair absorbs a leading anchor
                // into the KEY. Emitting it here instead left it beside the
                // mapping, where it looked like a second anchor on the same
                // node (test suite 7BMT, U3XV).
                if !self.in_flow_context && self.is_mapping_key() {
                    self.parse_mapping_with_base_indent(base_indent);
                    self.nesting_depth -= 1;
                    return;
                }
                let body_indent = self
                    .tagged_block_node_indent(base_indent, self.ctx.annotation_in_value_position);
                self.bump(); // consume and emit anchor token to CST
                let empty_flow_node = self.annotates_empty_flow_node();
                if !empty_flow_node {
                    self.skip_whitespace();
                }
                match body_indent {
                    _ if empty_flow_node => self.emit_implicit_null(),
                    Some(indent) => {
                        self.skip_ws_and_newlines();
                        if self.current() == Some(SyntaxKind::DASH) {
                            self.parse_sequence_with_base_indent(indent);
                        } else {
                            // As in the tagged case, a plain scalar body is
                            // still a node of the collection the anchor sits
                            // in, so its continuation only has to clear the
                            // anchor's own column.
                            let ctx = BlockCtx {
                                scalar_continuation_floor: Some(base_indent),
                                ..self.ctx
                            };
                            self.scoped(ctx, |p| p.parse_value_with_base_indent(indent));
                        }
                    }
                    // Nothing for the anchor to adopt, and nothing after it on
                    // its line: it annotates an implicit null, and the next
                    // line is left for the enclosing collection. Inside an
                    // explicit key that line opens the entry's value, for the
                    // `:` handling to claim.
                    None if self.annotation_ends_line() => self.emit_annotated_null(),
                    None => self.parse_value_with_base_indent(base_indent),
                }
            }
            Some(SyntaxKind::REFERENCE) => {
                // An alias may be a mapping key (`*a : v`), as the YAML test
                // suite's 26DV has it. Without this the alias became the
                // value and the `: v` after it opened a null-key entry.
                if !self.in_flow_context && self.is_mapping_key() {
                    self.parse_mapping_with_base_indent(base_indent);
                } else {
                    self.parse_alias();
                    self.parse_content_after_node();
                }
            }
            Some(SyntaxKind::TAG) => {
                // `!!str a: b` at document / block level -- the tag
                // annotates the KEY of an implicit mapping, not the
                // whole document. Detect that shape (TAG [WS] scalar
                // [WS] COLON) and dispatch to mapping parsing so
                // parse_mapping_key_value_pair can consume the tag as
                // part of the KEY. Otherwise fall back to the default
                // tag-wraps-following-value behaviour. As with an anchor,
                // the same goes for a flow collection key: in `!t [a]: v`
                // the tag shares the key's line, which a block mapping's
                // own properties cannot.
                if !self.in_flow_context
                    && (self.is_mapping_key() || self.is_complex_mapping_key_after_annotations())
                {
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
                if !self.in_flow_context && self.is_mapping_key() {
                    self.parse_mapping_with_base_indent(base_indent);
                } else {
                    self.parse_scalar();
                }
            }
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

impl Parser<'_> {
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
        //
        // Only a line's first token can be dedented. One after an indicator
        // on the same line, like the inner `-` of `-  ? -`, sits right of
        // it, though current_line_indent leaves out the space between them.
        if !(self.previous_kind.is_none() || self.line_ended()) {
            return false;
        }
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
    ///
    /// With `dash_lines`, as for a block sequence, a line that does not
    /// start with a dash also ends the block, before its INDENT: in
    /// `- k:\n  - 1\n  j: 2\n` the `j` line, indentation included, belongs
    /// to the mapping, as it does when the sequence is indented past `k`.
    pub(super) fn skip_whitespace_only_with_dedent_check(
        &mut self,
        base_indent: usize,
        dash_lines: bool,
    ) -> bool {
        self.holding_trivia(|p| p.skip_whitespace_with_dedent_check(base_indent, dash_lines))
    }

    /// Run `f` with the line breaks, indentation, white space and comments
    /// it steps over held back for the node that comes next; see
    /// [`TreeBuilder`].
    pub(super) fn holding_trivia<R>(&mut self, f: impl FnOnce(&mut Self) -> R) -> R {
        let outer = std::mem::replace(&mut self.hold_trivia, true);
        let result = f(self);
        self.hold_trivia = outer;
        result
    }

    fn skip_whitespace_with_dedent_check(&mut self, base_indent: usize, dash_lines: bool) -> bool {
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
                            if self.indent_ends_block(base_indent, dash_lines, true) {
                                return true;
                            }
                            self.bump();
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
                                    Some((indent, _)) => indent < base_indent,
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
                    if self.indent_ends_block(base_indent, dash_lines, false) {
                        return true;
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

    /// Whether the current INDENT ends the block at `base_indent`.
    ///
    /// Blank and comment lines never do: what ends the block is the next line
    /// that carries content. At root level, any indentation at `line_start`
    /// means the content doesn't belong at root.
    fn indent_ends_block(&self, base_indent: usize, dash_lines: bool, line_start: bool) -> bool {
        if self.indent_is_blank_line() || self.indent_starts_comment_line() {
            return false;
        }
        if dash_lines && !self.indent_starts_dash_line() {
            return true;
        }
        let Some((_, text)) = self.tokens.last() else {
            return false;
        };
        text.len() < base_indent || (line_start && base_indent == 0 && !text.is_empty())
    }

    pub(super) fn skip_ws_and_newlines(&mut self) {
        self.skip_tokens(&[
            // The lexer only emits a BOM where it prefixes a document.
            SyntaxKind::BOM,
            SyntaxKind::WHITESPACE,
            SyntaxKind::NEWLINE,
            SyntaxKind::INDENT,
            SyntaxKind::COMMENT,
        ]);
    }

    /// Whether the tokens consumed so far end with a line break, perhaps
    /// followed by the next line's indentation.
    pub(super) fn line_ended(&self) -> bool {
        matches!(
            self.previous_kind,
            Some(SyntaxKind::NEWLINE | SyntaxKind::INDENT)
        )
    }

    /// Step over the white space and comment ending the current line,
    /// unless the node just parsed has ended its line, as a block node does:
    /// a comment then is a line of its own.
    pub(super) fn skip_line_end_trivia(&mut self) {
        if self.line_ended() {
            return;
        }
        self.skip_whitespace();
        if self.current() == Some(SyntaxKind::COMMENT) {
            self.bump();
        }
    }

    /// Step over the end of the line of the block entry just parsed: white
    /// space, a comment and the line break, which the entry owns (see the
    /// CST invariants in `nodes/mod.rs`). An entry ending in a block node
    /// has ended its line already.
    pub(super) fn skip_entry_line_end(&mut self) {
        if self.line_ended() {
            return;
        }
        self.skip_line_end_trivia();
        if self.current() == Some(SyntaxKind::NEWLINE) {
            self.bump();
        }
    }

    /// Read what is left of the line after the node of a block entry at
    /// `column` as more nodes in the same slot, which the validator
    /// reports. Opening the collection's next entry there instead left this
    /// one without the line break ending it. Later lines belong to those
    /// nodes only when indented past the entry.
    pub(super) fn parse_rest_of_entry_line(&mut self, column: usize) {
        if self.in_flow_context {
            return;
        }
        while !self.line_ended() {
            let whitespace = usize::from(self.current() == Some(SyntaxKind::WHITESPACE));
            if matches!(
                self.peek_kind(whitespace),
                None | Some(SyntaxKind::NEWLINE | SyntaxKind::COMMENT)
            ) {
                return;
            }
            if whitespace == 1 {
                self.bump();
            }
            let before = self.tokens.len();
            self.parse_value_with_base_indent(column + 1);
            if self.tokens.len() == before {
                self.parse_stray_token("block entry");
            }
        }
    }

    /// The token [`skip_ws_and_newlines`](Self::skip_ws_and_newlines)
    /// would stop at, and the indentation of its line.
    pub(super) fn next_content(&self) -> (Option<SyntaxKind>, usize) {
        let mut indent = self.current_line_indent;
        for (kind, text) in self.tokens.iter().rev() {
            match kind {
                SyntaxKind::NEWLINE => indent = 0,
                SyntaxKind::INDENT => indent = text.len(),
                SyntaxKind::BOM | SyntaxKind::WHITESPACE | SyntaxKind::COMMENT => {}
                _ => return (Some(*kind), indent),
            }
        }
        (None, indent)
    }

    /// Whether the next content is the `:` of the explicit key whose `?`
    /// sits at `question_column`. One opening a later line left of the `?`
    /// is the value of an enclosing entry: in `? - ? a\n: b\n` the `: b`
    /// line is the outer key's.
    pub(super) fn explicit_value_follows(&self, question_column: usize) -> bool {
        // The column of the line the next content is on, if not this one.
        let mut line_indent = self.line_ended().then_some(self.current_line_indent);
        for (kind, text) in self.tokens.iter().rev() {
            match kind {
                SyntaxKind::NEWLINE => line_indent = Some(0),
                SyntaxKind::INDENT => line_indent = Some(text.len()),
                SyntaxKind::BOM | SyntaxKind::WHITESPACE | SyntaxKind::COMMENT => {}
                SyntaxKind::COLON => {
                    return line_indent.map_or(true, |indent| indent >= question_column)
                }
                _ => return false,
            }
        }
        false
    }

    /// Where the value of a `:` is, when it is on a later line indented
    /// past `mapping_column` (or at it, for an indentless sequence): the
    /// number of tokens up to the INDENT starting that line, or the line
    /// break before it at column 0, and the indentation. Comment lines in between say nothing about where the
    /// value sits, so they are stepped over: the YAML test suite's Q9WF has
    /// a column-0 comment between a flow-collection key and its indented
    /// block mapping. The caller is on the NEWLINE ending the colon's line.
    fn value_line_after_colon(&self, mapping_column: usize) -> Option<(usize, usize)> {
        let ahead = self.lookahead();
        if ahead.kind(0) != Some(SyntaxKind::NEWLINE) {
            return None;
        }
        let mut i = 1;
        loop {
            match ahead.kind(i) {
                Some(SyntaxKind::COMMENT) => i += 1,
                Some(SyntaxKind::INDENT) if ahead.kind(i + 1) == Some(SyntaxKind::COMMENT) => {
                    i += 2
                }
                _ => break,
            }
            if ahead.kind(i) == Some(SyntaxKind::NEWLINE) {
                i += 1;
            }
        }
        let (SyntaxKind::INDENT, text) = ahead.get(i)? else {
            // A line at column 0 has no INDENT; it holds the value only as
            // an indentless sequence of a mapping at column 0.
            let indentless = mapping_column == 0 && ahead.kind(i) == Some(SyntaxKind::DASH);
            return indentless.then_some((i - 1, 0));
        };
        let indent = text.len();
        let opens_value = indent > mapping_column
            || (indent == mapping_column && ahead.kind(i + 1) == Some(SyntaxKind::DASH));
        opens_value.then_some((i, indent))
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
    ///
    /// `mapping_column` is the column of the mapping the entry belongs to.
    /// A value on a later line has to be indented past it, bar an
    /// indentless sequence: in `k:\n  ? a\n  :\n  b: c\n` the value of `a`
    /// is null and `b` is its sibling.
    pub(super) fn parse_value_after_colon(&mut self, track_indent: bool, mapping_column: usize) {
        self.builder.start_node(SyntaxKind::VALUE.into());
        // A comment ends the colon's line just as a line break does: the
        // next line is the value only if it is indented to be one.
        if self.current() == Some(SyntaxKind::COMMENT) {
            self.bump();
        }
        if self.dedented_block_scalar_header_follows(mapping_column) {
            // `? a\n:\n|\n x\n`: the header can only be this value. Its
            // body still has to clear the mapping's column.
            self.skip_ws_and_newlines();
            let ctx = BlockCtx {
                mapping_value_column: Some(mapping_column),
                ..self.ctx
            };
            self.scoped(ctx, |p| p.parse_value());
        } else if self.current().is_some() && self.current() != Some(SyntaxKind::NEWLINE) {
            self.parse_value();
        } else if let Some((skip, indent)) = self.value_line_after_colon(mapping_column) {
            for _ in 0..=skip {
                self.bump();
            }
            if track_indent {
                self.parse_value_with_base_indent(indent);
            } else {
                self.parse_value();
            }
        } else {
            // No value: the input ends, or the next line is a dedent or a
            // sibling entry, which is left to the mapping.
            self.emit_implicit_null();
        }
        self.parse_rest_of_entry_line(mapping_column);
        // A comment ending the value's last line goes in the VALUE, as in
        // a mapping entry with an implicit key.
        self.skip_line_end_trivia();
        self.builder.finish_node();
    }

    /// Whether a tag or anchor whose line holds nothing else but perhaps a
    /// comment, and which adopted nothing from the following lines,
    /// annotates an implicit null.
    pub(super) fn annotation_ends_line(&self) -> bool {
        let mut rest = self
            .current()
            .into_iter()
            .chain(self.upcoming_tokens())
            .peekable();
        rest.next_if_eq(&SyntaxKind::COMMENT);
        matches!(rest.next(), None | Some(SyntaxKind::NEWLINE))
    }

    /// Emit the implicit null of an annotation for which
    /// [`annotation_ends_line`](Self::annotation_ends_line) holds, after the
    /// comment that may end its line. The line break is left to the
    /// enclosing entry, as it is for `- # c\n`.
    pub(super) fn emit_annotated_null(&mut self) {
        if self.current() == Some(SyntaxKind::COMMENT) {
            self.bump();
        }
        self.emit_implicit_null();
    }

    /// Whether a tag or anchor just consumed inside a flow collection
    /// annotates an empty node: nothing but whitespace and comments stand
    /// between it and the `,`, `]`, `}` or key separator that ends the
    /// node, as in `{ !!str : bar }` or `[ &a , b ]`.
    pub(super) fn annotates_empty_flow_node(&self) -> bool {
        if !self.in_flow_context {
            return false;
        }
        let mut n = 0;
        while matches!(
            self.peek_kind(n),
            Some(
                SyntaxKind::WHITESPACE
                    | SyntaxKind::NEWLINE
                    | SyntaxKind::INDENT
                    | SyntaxKind::COMMENT
            )
        ) {
            n += 1;
        }
        match self.peek_kind(n) {
            None
            | Some(SyntaxKind::COMMA | SyntaxKind::RIGHT_BRACE | SyntaxKind::RIGHT_BRACKET) => true,
            Some(_) => self.flow_key_separator_at(n),
        }
    }

    /// Emit the zero-width `SCALAR { NULL "" }` that fills every value slot
    /// the source left empty (`key:`, `{a}`, `- `, ...). See the
    /// implicit-null section of the CST invariants in `nodes/mod.rs`: every
    /// KEY, VALUE and SEQUENCE_ENTRY holds exactly one scalar or collection,
    /// and this token renders as nothing so round-trips stay lossless.
    pub(super) fn emit_implicit_null(&mut self) {
        self.builder.ahead_of_held(|b| {
            b.start_node(SyntaxKind::SCALAR.into());
            b.token(SyntaxKind::NULL.into(), "");
            b.finish_node();
        });
    }

    /// [`emit_implicit_null`](Self::emit_implicit_null) wrapped in a VALUE
    /// node, for the mapping shapes that wrap their values.
    pub(super) fn emit_implicit_null_value(&mut self) {
        self.builder.ahead_of_held(|b| {
            b.start_node(SyntaxKind::VALUE.into());
            b.start_node(SyntaxKind::SCALAR.into());
            b.token(SyntaxKind::NULL.into(), "");
            b.finish_node();
            b.finish_node();
        });
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
                // A `-` or `?` puts the node it introduces further right, so
                // the column a nested collection measures from is past the
                // indicator rather than at the line's own indent.
                SyntaxKind::DASH | SyntaxKind::QUESTION => {
                    self.current_line_indent += text.len();
                }
                _ => {}
            }

            self.position += text.len();
            self.previous_kind = Some(kind);
            if self.hold_trivia
                && matches!(
                    kind,
                    SyntaxKind::NEWLINE
                        | SyntaxKind::INDENT
                        | SyntaxKind::WHITESPACE
                        | SyntaxKind::COMMENT
                )
            {
                self.builder.hold(kind, text);
            } else {
                self.builder.token(kind.into(), &text);
            }
        }
    }

    /// The 0-based column of the current token.
    pub(super) fn column(&self) -> usize {
        self.line_index.column(self.position)
    }

    pub(super) fn current(&self) -> Option<SyntaxKind> {
        self.tokens.last().map(|(kind, _)| *kind)
    }

    pub(super) fn current_text(&self) -> Option<&str> {
        self.tokens.last().map(|(_, text)| text.as_str())
    }

    /// The kind of the first token from the current one on that is not in
    /// `skip`.
    fn next_kind_past(&self, skip: &[SyntaxKind]) -> Option<SyntaxKind> {
        self.current()
            .into_iter()
            .chain(self.upcoming_tokens())
            .find(|kind| !skip.contains(kind))
    }

    /// The kind of the next token that is not whitespace, a comment or a
    /// line break.
    pub(super) fn next_significant_kind(&self) -> Option<SyntaxKind> {
        self.next_kind_past(&[
            SyntaxKind::WHITESPACE,
            SyntaxKind::NEWLINE,
            SyntaxKind::INDENT,
            SyntaxKind::COMMENT,
        ])
    }

    /// Whether only whitespace, comments and line breaks stand between here
    /// and the `,` or `closer` that ends a flow entry, or the end of an
    /// input that left the collection unclosed.
    pub(super) fn at_flow_entry_end(&self, closer: SyntaxKind) -> bool {
        let next = self.next_significant_kind();
        next.is_none() || next == Some(SyntaxKind::COMMA) || next == Some(closer)
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

    /// Whether the document root is a plain scalar, looking past any tags
    /// and anchors annotating it.
    ///
    /// Such a scalar has no enclosing collection, so its continuation lines
    /// need clear no column, and an annotation in front does not change that.
    /// The annotations have to sit on the scalar's own line: one alone on its
    /// line opens a block node instead, and that node's body keeps its indent.
    fn root_annotations_cover_plain_scalar(&self) -> bool {
        let mut kind = match self.current() {
            Some(kind) => kind,
            None => return false,
        };
        let mut rest = self.upcoming_tokens();
        let mut saw_annotation = false;
        while matches!(kind, SyntaxKind::TAG | SyntaxKind::ANCHOR) {
            saw_annotation = true;
            kind = match rest.next() {
                // A line break ends the annotation's line, so whatever
                // follows is a block node rather than an annotated scalar.
                Some(SyntaxKind::WHITESPACE) | Some(SyntaxKind::INDENT) => match rest.next() {
                    Some(kind) => kind,
                    None => return false,
                },
                Some(kind) => kind,
                None => return false,
            };
        }
        if !crate::parser::scalars::is_plain_scalar_kind(kind) {
            return false;
        }
        // An unannotated root scalar can still be a mapping key; an
        // annotated one was already dispatched to mapping parsing.
        saw_annotation || !self.is_mapping_key()
    }

    /// Whether the INDENT at the current position is only the leading
    /// whitespace of a comment line.
    ///
    /// A comment's own column says nothing about the block it sits in, so
    /// such an INDENT must not be read as a dedent out of it any more than a
    /// blank line's may. Whether the block ends there is decided by the next
    /// line with content, which the COMMENT arm reads once it is reached.
    pub(super) fn indent_starts_comment_line(&self) -> bool {
        self.upcoming_tokens().next() == Some(SyntaxKind::COMMENT)
    }

    /// Whether the INDENT at the current position starts a line with a `-`.
    fn indent_starts_dash_line(&self) -> bool {
        self.upcoming_tokens().next() == Some(SyntaxKind::DASH)
    }

    /// Indentation and first token of the next line that carries content,
    /// looking past any run of comment-only and blank lines, or `None` if
    /// there is none.
    ///
    /// A comment's own column says nothing about the block it sits in: YAML
    /// lets `a:\n  - x\n# c\n  - y\n` keep both entries, because what ends
    /// the sequence is the next content line, not the comment. The caller is
    /// positioned on the COMMENT token.
    pub(super) fn indent_after_comment_lines(&self) -> Option<(usize, SyntaxKind)> {
        if self.current() != Some(SyntaxKind::COMMENT) {
            return None;
        }
        // A comment inside a run already scanned sees the same lines after
        // it, so reuse that answer.
        let len = self.tokens.len();
        if let Some((start, end, result)) = self.comment_run_scan.get() {
            if end < len && len <= start {
                return result;
            }
        }
        let mut consumed = 0;
        let result = self.scan_lines_after_comment(&mut consumed);
        self.comment_run_scan
            .set(Some((len, len - consumed, result)));
        result
    }

    /// The scan behind [`indent_after_comment_lines`](Self::indent_after_comment_lines),
    /// counting the tokens it reads in `consumed`.
    fn scan_lines_after_comment(&self, consumed: &mut usize) -> Option<(usize, SyntaxKind)> {
        // `tokens` is in reverse order, so walk it backwards from the current
        // token to read the lines that follow.
        let mut rest = self
            .tokens
            .iter()
            .rev()
            .map(|(kind, text)| (*kind, text))
            .inspect(|_| *consumed += 1);
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
                // The document ends here, just as at the end of the input.
                SyntaxKind::DOC_START | SyntaxKind::DOC_END => return None,
                // Another comment line: step over its line break too.
                SyntaxKind::COMMENT => {
                    if !matches!(rest.next(), Some((SyntaxKind::NEWLINE, _))) {
                        return None;
                    }
                    continue;
                }
                kind => return Some((indent, kind)),
            }
        }
    }

    /// Whether a comment on a sequence entry's line ends that entry, leaving
    /// it an implicit null.
    ///
    /// It does unless the entry's value follows on a later line, indented
    /// past the dash: `- # c\n- a\n` is two null-and-`a` entries, while
    /// `- # c\n  v\n` is the single entry `v`. The caller is on the COMMENT.
    pub(super) fn comment_ends_the_entry(&self, dash_column: usize) -> bool {
        let mut rest = self.tokens.iter().rev().map(|(kind, text)| (*kind, text));
        if !matches!(rest.next(), Some((SyntaxKind::COMMENT, _))) {
            return true;
        }
        if !matches!(rest.next(), Some((SyntaxKind::NEWLINE, _))) {
            return true;
        }
        Self::value_indent_on_later_line(rest, dash_column).is_none()
    }

    /// The indentation of a sequence entry's value when that value is on a
    /// later line, looking past any run of blank lines between the dash and
    /// that line. `None` when the entry has no such value.
    ///
    /// The caller is positioned on the NEWLINE ending the dash's line, whose
    /// dash sits at `dash_column`. A blank line's indentation is not
    /// significant, so it neither supplies the value's indent nor ends the
    /// entry: `- \n\n m\n` is the entry `m`, exactly as `- \n m\n` is.
    ///
    /// The value has to clear the dash's own column. At or left of it the
    /// line opens the next entry instead, and this one is an implicit null:
    /// `- -\n  -\n` is `[[null, null]]` while `- -\n   -\n` is `[[[null]]]`,
    /// as saphyr and PyYAML both read them.
    pub(super) fn entry_value_indent(&self, dash_column: usize) -> Option<usize> {
        // `tokens` is in reverse order, so walk it backwards to read the
        // lines that follow.
        let mut rest = self.tokens.iter().rev().map(|(kind, text)| (*kind, text));
        // The NEWLINE we are standing on.
        if !matches!(rest.next(), Some((SyntaxKind::NEWLINE, _))) {
            return None;
        }
        Self::value_indent_on_later_line(rest, dash_column)
    }

    /// The indentation of the next line with content in `rest`, which starts
    /// at a line start, if it clears `dash_column`. Blank and comment lines
    /// are stepped over, as their indentation is not significant.
    fn value_indent_on_later_line<'a>(
        mut rest: impl Iterator<Item = (SyntaxKind, &'a String)>,
        dash_column: usize,
    ) -> Option<usize> {
        loop {
            match rest.next() {
                // A blank line: its own line break is the one we just read.
                Some((SyntaxKind::NEWLINE, _)) => continue,
                // Neither does a comment line say anything, at any column:
                // `-\n# c\n  x: 1\n` is the entry `{x: 1}`.
                Some((SyntaxKind::COMMENT, _)) => {
                    if !matches!(rest.next(), Some((SyntaxKind::NEWLINE, _))) {
                        return None;
                    }
                }
                Some((SyntaxKind::INDENT, text)) => {
                    let indent = text.len();
                    // An indent that only leads a blank or comment line says
                    // nothing about where the value sits, nor does one
                    // ending the input.
                    match rest.next() {
                        Some((SyntaxKind::NEWLINE, _)) | None => continue,
                        Some((SyntaxKind::COMMENT, _)) => {
                            if !matches!(rest.next(), Some((SyntaxKind::NEWLINE, _))) {
                                return None;
                            }
                            continue;
                        }
                        _ => {}
                    }
                    return (indent > dash_column).then_some(indent);
                }
                _ => return None,
            }
        }
    }

    /// Whether a block scalar header not indented past `indicator_column`
    /// opens the next line with content.
    ///
    /// The caller is on the NEWLINE, or a comment before it, that ends the
    /// line of a `-`, `?` or explicit `:` at that column. Such a header can
    /// only be the indicator's node, as nothing else starts with `|` or
    /// `>`, though `s-separate(n+1)` wants it indented past the indicator;
    /// the validator reports it.
    pub(super) fn dedented_block_scalar_header_follows(&self, indicator_column: usize) -> bool {
        let mut rest = self
            .tokens
            .iter()
            .rev()
            .map(|(kind, text)| (*kind, text.len()))
            .peekable();
        rest.next_if(|(kind, _)| *kind == SyntaxKind::COMMENT);
        if !matches!(rest.next(), Some((SyntaxKind::NEWLINE, _))) {
            return false;
        }
        loop {
            let mut indent = 0;
            let mut token = rest.next();
            if let Some((SyntaxKind::INDENT, len)) = token {
                indent = len;
                token = rest.next();
            }
            match token {
                // A blank line, whose indentation is not significant.
                Some((SyntaxKind::NEWLINE, _)) => continue,
                Some((SyntaxKind::COMMENT, _)) => {
                    if !matches!(rest.next(), Some((SyntaxKind::NEWLINE, _))) {
                        return false;
                    }
                }
                Some((SyntaxKind::PIPE | SyntaxKind::GREATER, _)) => {
                    return indent <= indicator_column
                }
                _ => return false,
            }
        }
    }

    /// Whether the INDENT at the current position is only the leading
    /// whitespace of an otherwise blank line.
    ///
    /// A blank line's indentation is not significant in YAML, so such an
    /// INDENT must not be read as a dedent out of the enclosing block.
    pub(super) fn indent_is_blank_line(&self) -> bool {
        matches!(
            self.upcoming_tokens().next(),
            Some(SyntaxKind::NEWLINE) | None
        )
    }

    pub(super) fn add_error(&mut self, message: String, kind: ParseErrorKind) {
        let token_len = self.current_text().map_or(1, |s| s.len());
        let positioned_error = PositionedParseError {
            message,
            range: crate::TextPosition::new(
                self.position as u32,
                (self.position + token_len) as u32,
            ),
            code: None,
            kind,
        };

        self.errors.push(positioned_error.message.clone());
        self.positioned_errors.push(positioned_error);
    }

    /// Report the current token, which nothing in `what` can start, and
    /// hold it in an ERROR node.
    pub(super) fn parse_stray_token(&mut self, what: &str) {
        let unexpected = self.current_text().unwrap_or("").to_string();
        self.add_error(
            format!("Unexpected token in {what}: {unexpected:?}"),
            ParseErrorKind::Other,
        );
        self.builder.start_node(SyntaxKind::ERROR.into());
        self.bump();
        self.builder.finish_node();
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
                while self
                    .current()
                    .is_some_and(|kind| !self.error_context.is_sync_point(kind))
                {
                    self.bump();
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sync_to_safe_point_stops_at_next_document_marker() {
        let mut parser = Parser::new("a\n---\nb c d e f g h\n");
        parser.add_error_and_recover(
            "error".to_string(),
            SyntaxKind::QUOTE,
            ParseErrorKind::Other,
        );
        assert_eq!(parser.current(), Some(SyntaxKind::DOC_START));
    }
}
