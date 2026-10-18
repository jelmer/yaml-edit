//! `impl MappingEntry` -- the key/value/set_value/discard/remove API
//! on a single MAPPING_ENTRY node, plus its private layout helpers
//! (indent_of_line, insert_at, set_block_value, rebuild_with_inline_value).
//!
//! Split out of `nodes/mapping/mod.rs` unchanged.

use super::helpers::{
    build_block_value_node, built_block_node, drop_tagged_anchor, find_key_line_comment, flow_form,
    has_comment, held_anchor_token, source_anchor_text, value_is_block,
};
use super::{key_content_matches, MappingEntry};
use crate::as_yaml::{build_in, FlowQuoting};
use crate::lex::SyntaxKind;
use crate::nodes::replaced_value::{add_to_first_line, insert_comment_line_above};
use crate::nodes::{
    entry_key, entry_value, fresh_token, has_child_token, insert_children, replace_children, Lang,
    SyntaxNode,
};
use crate::yaml::add_node_children_to;
use rowan::GreenNodeBuilder;

impl MappingEntry {
    /// Return the raw `KEY` wrapper node of this entry.
    ///
    /// The returned node has kind `KEY` and wraps the actual key content
    /// (a scalar, mapping, or sequence node). Returns `None` for malformed
    /// entries that have no key node.
    ///
    /// To compare the key against a value, prefer [`key_matches`](Self::key_matches).
    pub(crate) fn key(&self) -> Option<SyntaxNode> {
        entry_key(&self.0)
    }

    /// Return `true` if the key of this entry matches `key`.
    ///
    /// Uses semantic YAML equality, so quoting style differences are ignored:
    /// `"foo"`, `'foo'`, and `foo` all match the scalar `"foo"`. Returns
    /// `false` if this entry has no key node.
    pub fn key_matches(&self, key: impl crate::AsYaml) -> bool {
        self.key().is_some_and(|k| key_content_matches(&k, key))
    }

    /// Return the raw `VALUE` wrapper node of this entry.
    ///
    /// The returned node has kind `VALUE` and wraps the actual value content
    /// (a scalar, mapping, or sequence node). Returns `None` for malformed
    /// entries that have no value node.
    pub(crate) fn value(&self) -> Option<SyntaxNode> {
        entry_value(&self.0)
    }

    /// Get the key of this entry as a [`YamlNode`](crate::as_yaml::YamlNode).
    ///
    /// Returns `None` for malformed entries that have no key.
    pub fn key_node(&self) -> Option<crate::as_yaml::YamlNode> {
        self.key()
            .and_then(|k| k.children().next())
            .and_then(crate::as_yaml::YamlNode::from_syntax)
    }

    /// Get the value of this entry as a [`YamlNode`](crate::as_yaml::YamlNode).
    ///
    /// Returns `None` for malformed entries that have no value.
    pub fn value_node(&self) -> Option<crate::as_yaml::YamlNode> {
        self.value()
            .and_then(|v| v.children().next())
            .and_then(crate::as_yaml::YamlNode::from_syntax)
    }

    /// Create a new mapping entry (key-value pair) not yet attached to any mapping.
    ///
    /// The entry is built as a standalone CST node; attach it to a mapping with
    /// one of the `insert_*` methods. Block-style values (mappings, sequences)
    /// are indented with 2 spaces relative to the key.
    ///
    /// This builds the entry assuming the key will sit at column 0. If the
    /// entry will be inserted into a mapping at a deeper column, use
    /// [`new_at_indent`](Self::new_at_indent) instead so a node-backed block
    /// value's interior indentation lines up with the target column.
    pub fn new(
        key: impl crate::AsYaml,
        value: impl crate::AsYaml,
        flow_context: bool,
        use_explicit_key: bool,
    ) -> Self {
        Self::new_at_indent(key, value, flow_context, use_explicit_key, 0)
    }

    /// Like [`new`](Self::new) but builds the entry as if its key were
    /// placed at column `key_indent`. Only affects how a node-backed block
    /// collection value is re-indented; other value kinds render the same
    /// as with [`new`](Self::new).
    pub fn new_at_indent(
        key: impl crate::AsYaml,
        value: impl crate::AsYaml,
        flow_context: bool,
        use_explicit_key: bool,
        key_indent: usize,
    ) -> Self {
        Self::new_in(
            key,
            value,
            flow_context.then_some(FlowQuoting::Minimal),
            use_explicit_key,
            key_indent,
        )
    }

    /// Like [`new_at_indent`](Self::new_at_indent), for a flow mapping
    /// quoting strings as `flow` says if it is set.
    ///
    /// A node-backed key preceded by an `&anchor` in its parent keeps that
    /// anchor, so aliases to it still resolve.
    pub(crate) fn new_in(
        key: impl crate::AsYaml,
        value: impl crate::AsYaml,
        flow: Option<FlowQuoting>,
        use_explicit_key: bool,
        key_indent: usize,
    ) -> Self {
        let key_anchor = key.value_node().and_then(|n| source_anchor_text(&n));
        Self::build(
            key,
            value,
            flow,
            use_explicit_key,
            key_indent,
            key_anchor.as_deref(),
        )
    }

    /// Build an entry as [`new_in`](Self::new_in) does, putting
    /// `key_anchor` on the key.
    fn build(
        key: impl crate::AsYaml,
        value: impl crate::AsYaml,
        flow: Option<FlowQuoting>,
        use_explicit_key: bool,
        key_indent: usize,
        key_anchor: Option<&str>,
    ) -> Self {
        let flow_context = flow.is_some();
        let quoting = flow.unwrap_or(FlowQuoting::Minimal);
        // build_key_content shifts the flow form by key_indent, so its
        // continuation lines are given relative to the key.
        if let Some(key) = flow_form(&key, 2, quoting) {
            // Comments make the key span several lines, which only an
            // explicit key may do.
            let multiline = has_comment(key.syntax());
            let explicit = use_explicit_key || multiline;
            return Self::build(key, value, flow, explicit, key_indent, key_anchor);
        }
        if !use_explicit_key && !flow_context && crate::nodes::key_spans_lines(&key) {
            return Self::build(key, value, flow, true, key_indent, key_anchor);
        }
        // Built inline below, the flow form is shifted by key_indent, so its
        // continuation lines are given relative to the key.
        if let Some(value) = flow.and_then(|q| flow_form(&value, 2, q)) {
            return Self::build(key, value, flow, use_explicit_key, key_indent, key_anchor);
        }
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(SyntaxKind::MAPPING_ENTRY.into());

        // The parser puts the `?` beside KEY in a block mapping and inside
        // it in a flow mapping.
        let question = |builder: &mut GreenNodeBuilder| {
            builder.token(SyntaxKind::QUESTION.into(), "?");
            builder.token(SyntaxKind::WHITESPACE.into(), " ");
        };
        if use_explicit_key && !flow_context {
            question(&mut builder);
        }

        builder.start_node(SyntaxKind::KEY.into());
        if use_explicit_key && flow_context {
            question(&mut builder);
        }
        if let Some(anchor) = key_anchor {
            builder.token(SyntaxKind::ANCHOR.into(), anchor);
            builder.token(SyntaxKind::WHITESPACE.into(), " ");
        }
        crate::nodes::build_key_content(&mut builder, &key, key_indent, flow);
        let key_has_newline = false;
        debug_assert!(!key_has_newline, "Keys should not end with newlines");
        let colon_on_own_line = use_explicit_key && !flow_context;
        builder.finish_node();

        // The `:` goes on its own line at the entry's own column; the parser
        // keeps that line break and indent as siblings of KEY. In a flow
        // mapping the `:` simply follows the key.
        if colon_on_own_line {
            builder.token(SyntaxKind::NEWLINE.into(), "\n");
            if key_indent > 0 {
                builder.token(SyntaxKind::INDENT.into(), &" ".repeat(key_indent));
            }
        }

        builder.token(SyntaxKind::COLON.into(), ":");

        // Node-backed block collections and block scalars need the same
        // re-indent treatment that MappingEntry::set_value applies when
        // the source's own base indent doesn't already put it at the
        // correct target column: put the value on the next line and shift
        // its interior indentation to line up under this entry.
        //
        // When key_indent == 0 (root-level insertion), col-0 dashes render
        // as valid compact-style block content, so we leave sequences with
        // source_base == 0 alone to preserve existing formatting choices.
        // A col-0 mapping there would read as siblings of the key, and for
        // nested insertion (key_indent > 0) any col-0 content would be
        // parsed as escaping the parent's scope, so we must re-indent.
        let built = (!flow_context).then(|| built_block_node(&value)).flatten();
        let block_source = value
            .value_node()
            .filter(|_| !flow_context && !value.is_inline())
            .or(built)
            .filter(|_| {
                matches!(
                    value.kind(),
                    crate::as_yaml::YamlKind::Mapping
                        | crate::as_yaml::YamlKind::Sequence
                        | crate::as_yaml::YamlKind::Scalar
                        | crate::as_yaml::YamlKind::Tagged(_)
                )
            })
            // Route through build_block_value_node whenever indentation
            // needs to shift, OR when the source is empty (so a future
            // insertion has an INDENT hint to drop into rather than
            // landing at column 0).
            .filter(|src| {
                key_indent > 0
                    || src.kind() != SyntaxKind::SEQUENCE
                    || crate::column::node_column(src) > 0
                    || src.children().next().is_none()
            });

        let value_ends_with_newline = if let Some(source) = block_source {
            let block = build_block_value_node(&source, key_indent + 2, None);
            if block.on_key_line {
                builder.token(SyntaxKind::WHITESPACE.into(), " ");
            }
            builder.start_node(block.node.kind().into());
            add_node_children_to(&mut builder, &block.node);
            builder.finish_node();
            block.ends_with_newline
        } else {
            // For inline values, put the separator WHITESPACE at the
            // MAPPING_ENTRY level (between COLON and VALUE), matching
            // the parser convention. That way `set_value`'s targeted
            // splice, which only replaces the VALUE node in place,
            // preserves the space between key and new value.
            let inline = value.is_inline();
            if inline {
                builder.token(SyntaxKind::WHITESPACE.into(), " ");
            }
            builder.start_node(SyntaxKind::VALUE.into());
            let ends_with_newline = match (inline, value.kind()) {
                (true, _) => {
                    // TAGGED_NODE values (!!set, !!omap, !!pairs) are
                    // inline but may end with newlines from their
                    // block-style content. A multi-line flow value has its
                    // continuation lines shifted to clear the key's column,
                    // as does a block scalar's body.
                    build_in(&value, &mut builder, key_indent, flow)
                }
                // Block mappings and sequences start on new line but don't get pre-indented
                // They handle their own indentation via build_content
                (false, crate::as_yaml::YamlKind::Mapping | crate::as_yaml::YamlKind::Sequence) => {
                    builder.token(SyntaxKind::NEWLINE.into(), "\n");
                    build_in(&value, &mut builder, 0, flow)
                }
                // Block scalars (literal/folded) get newline and indent
                (false, _) => {
                    builder.token(SyntaxKind::NEWLINE.into(), "\n");
                    builder.token(SyntaxKind::INDENT.into(), "  ");
                    build_in(&value, &mut builder, 2, flow)
                }
            };
            builder.finish_node(); // VALUE
            ends_with_newline
        };

        // Every block-style MAPPING_ENTRY ends with NEWLINE (newline ownership
        // model). Flow-style entries live inside `{}` on a single line, so
        // adding a NEWLINE there would break the container.
        if !flow_context && !value_ends_with_newline {
            builder.token(SyntaxKind::NEWLINE.into(), "\n");
        }

        builder.finish_node(); // MAPPING_ENTRY
        let entry = SyntaxNode::new_root_mut(builder.finish());
        crate::nodes::separate_key_from_colon(&entry);
        MappingEntry(entry)
    }

    /// Column at which this entry's key sits on its line.
    ///
    /// Returns 0 for root-level entries or entries that share a line with
    /// something else (an explicit-key form, a flow-style parent, etc.).
    fn indent_of_line(&self) -> usize {
        crate::column::node_column(&self.0)
    }

    /// Index of the entry's VALUE child in its own children list, if any.
    fn value_index(&self) -> Option<usize> {
        self.0
            .children_with_tokens()
            .position(|c| c.as_node().is_some_and(|n| n.kind() == SyntaxKind::VALUE))
    }

    /// True if the entry's VALUE opens with a key-line comment, as the
    /// parser lays out `key:  # note` followed by a block collection.
    fn value_starts_with_comment(&self) -> bool {
        self.value()
            .and_then(|v| v.first_token())
            .is_some_and(|t| t.kind() == SyntaxKind::COMMENT)
    }

    /// Insert `tokens` at position `at` in `self.0`.
    fn insert_at(&self, at: usize, tokens: Vec<rowan::SyntaxToken<Lang>>) {
        let elems: Vec<_> = tokens.into_iter().map(Into::into).collect();
        insert_children(&self.0, at, elems);
    }

    /// Give a value that took the place of an omitted one, at child
    /// `value_idx`, the separator it lacked: `a:` becomes `a: v`, not the
    /// single plain scalar `a:v`, and in flow context `{a}` becomes
    /// `{a: v}`, not `{av}`. A comment right after it is moved off it too.
    fn separate_filled_value(&self, value_idx: usize, flow_context: bool) {
        let precedes_comment = self
            .0
            .children_with_tokens()
            .nth(value_idx + 1)
            .is_some_and(|c| c.kind() == SyntaxKind::COMMENT);
        if precedes_comment {
            self.insert_at(
                value_idx + 1,
                vec![fresh_token(SyntaxKind::WHITESPACE, " ")],
            );
        }
        let has_colon = self
            .0
            .children_with_tokens()
            .take(value_idx)
            .any(|c| c.kind() == SyntaxKind::COLON);
        let follows_colon = value_idx
            .checked_sub(1)
            .and_then(|i| self.0.children_with_tokens().nth(i))
            .is_some_and(|c| c.kind() == SyntaxKind::COLON);
        if follows_colon {
            self.insert_at(value_idx, vec![fresh_token(SyntaxKind::WHITESPACE, " ")]);
        } else if !has_colon && flow_context {
            let colon = fresh_token(SyntaxKind::COLON, ":");
            self.insert_at(
                value_idx,
                vec![colon.clone(), fresh_token(SyntaxKind::WHITESPACE, " ")],
            );
            crate::nodes::trivia::separate_from_question(&colon);
        }
        // TODO: a block explicit key without a value (`? a` at the end of
        // the input) needs `\n:` before the value, not `: `.
    }

    /// Append `tokens` at the end of `self.0`.
    fn append(&self, tokens: Vec<rowan::SyntaxToken<Lang>>) {
        super::super::append_children(&self.0, tokens.into_iter().map(Into::into).collect());
    }

    /// Detach the last child of `self.0` if it's a NEWLINE token.
    fn detach_last_if_newline(&self) {
        if let Some(last) = self.0.last_child_or_token() {
            if last
                .as_token()
                .is_some_and(|t| t.kind() == SyntaxKind::NEWLINE)
            {
                last.detach();
            }
        }
    }

    /// True if the entry ends with a NEWLINE (anywhere inside).
    fn ends_with_newline(&self) -> bool {
        self.0
            .last_token()
            .is_some_and(|t| t.kind() == SyntaxKind::NEWLINE)
    }

    /// Give a block explicit key without a value (`? a`) the `:` line a
    /// value goes on, at the column of the `?`.
    fn add_explicit_value_indicator(&self) {
        let has = |kind| has_child_token(&self.0, |k| k == kind);
        if !has(SyntaxKind::QUESTION) || has(SyntaxKind::COLON) {
            return;
        }
        let Some(value_idx) = self.value_index() else {
            return;
        };
        // The entry's line break, if it has one, now ends the `:` line,
        // and the key's line gets one of its own. A block key ends its line
        // itself, and the entry then has none, so the `:` line gets one.
        let mut tokens = Vec::new();
        let key_ends_line = self
            .key()
            .is_some_and(|k| crate::nodes::trivia::ends_with_line_break(&k));
        if !key_ends_line {
            tokens.push(fresh_token(SyntaxKind::NEWLINE, "\n"));
        }
        let indent = self.indent_of_line();
        if indent > 0 {
            tokens.push(fresh_token(SyntaxKind::INDENT, &" ".repeat(indent)));
        }
        tokens.push(fresh_token(SyntaxKind::COLON, ":"));
        tokens.push(fresh_token(SyntaxKind::WHITESPACE, " "));
        self.insert_at(value_idx, tokens);
        if key_ends_line {
            self.append(vec![fresh_token(SyntaxKind::NEWLINE, "\n")]);
        }
    }

    /// Replace this entry's value with a block sequence or mapping from
    /// another CST. Emits the value on the line after the key, indented to
    /// this entry's nesting depth + 2, with the source's interior
    /// indentation shifted to match. Called only from [`set_value`] for
    /// non-flow parents whose new value is a node-backed block collection.
    ///
    /// Uses targeted CST edits (splice the VALUE, detach specific
    /// surrounding tokens) rather than rebuilding the whole entry, so
    /// unrelated tokens (leading comments, alignment whitespace) survive.
    fn set_block_value(&self, source: &SyntaxNode, anchor: Option<&str>) {
        let block = build_block_value_node(source, self.indent_of_line() + 2, anchor);
        let value_ends_with_newline = block.ends_with_newline;

        let Some(value_idx) = self.value_index() else {
            return;
        };
        // The WHITESPACE between COLON and VALUE, if any.
        let ws_between = self
            .0
            .children_with_tokens()
            .nth(value_idx.wrapping_sub(1))
            .and_then(|c| c.into_token())
            .filter(|t| t.kind() == SyntaxKind::WHITESPACE);
        // That whitespace separates the colon from a key-line comment, not
        // from the value, if the comment comes first in VALUE.
        let value_ws = ws_between
            .as_ref()
            .filter(|_| !self.value_starts_with_comment())
            .map(|t| t.text().to_string());
        let kept_ws = || value_ws.clone().unwrap_or_else(|| " ".to_string());
        // Keep any key-line comment on the key line: after what the new
        // value puts there, or else first in VALUE, before its line break.
        // If the new value has a comment of its own there (a block scalar
        // header's), the old one goes on a line of its own above the entry.
        let mut displaced_comment = None;
        let separator = match find_key_line_comment(&self.0) {
            Some((ws, comment)) if !block.on_key_line => {
                insert_children(
                    &block.node,
                    0,
                    vec![fresh_token(SyntaxKind::COMMENT, &comment).into()],
                );
                Some(ws)
            }
            Some(comment) => {
                if !add_to_first_line(&block.node, comment.clone()) {
                    displaced_comment = Some(comment.1);
                }
                Some(kept_ws())
            }
            None => block.on_key_line.then(kept_ws),
        };

        let start = if ws_between.is_some() {
            value_idx - 1
        } else {
            value_idx
        };
        let mut replacement = Vec::new();
        if let Some(ws) = separator {
            replacement.push(fresh_token(SyntaxKind::WHITESPACE, &ws).into());
        }
        replacement.push(block.node.into());
        replace_children(&self.0, start..value_idx + 1, replacement);

        self.settle_trailing_newline(value_ends_with_newline);
        if let Some(comment) = displaced_comment {
            insert_comment_line_above(&self.0, &comment);
        }
    }

    /// Keep exactly one trailing NEWLINE on the entry once its VALUE has
    /// been replaced, either inside VALUE (block content usually provides
    /// one) or at MAPPING_ENTRY level.
    fn settle_trailing_newline(&self, value_ends_with_newline: bool) {
        if value_ends_with_newline {
            // Drop the MAPPING_ENTRY-level trailing NEWLINE that was the
            // OLD entry's terminator - the new VALUE has its own.
            self.detach_last_if_newline();
        } else if !self.ends_with_newline() {
            // New value doesn't provide one and the entry doesn't have
            // one - append so we don't glue onto the next entry.
            self.append(vec![fresh_token(SyntaxKind::NEWLINE, "\n")]);
        }
    }

    /// Replace the value of this entry in place, preserving the key and surrounding whitespace.
    ///
    /// Comments inside the old value are removed with it; the comment on
    /// the key's line stays with the entry. An anchor on the old value is
    /// kept on the new one, in place of any anchor of its own, so aliases
    /// to it still resolve.
    ///
    /// Aliases elsewhere to anchors deeper in the old value are kept
    /// resolving: the first of each becomes a copy of the anchored node.
    pub fn set_value(&self, new_value: impl crate::AsYaml, flow_context: bool) {
        // An alias cannot carry an anchor; the first alias to the old one
        // gets a copy of the old value instead.
        let kept = self
            .value()
            .filter(|_| new_value.kind() != crate::as_yaml::YamlKind::Alias)
            .and_then(|v| held_anchor_token(&v));
        let anchor = kept.as_ref().map(|t| t.text().to_string());
        let Some(old) = self.value() else {
            self.replace_value_under(new_value, flow_context, anchor.as_deref());
            return;
        };
        crate::nodes::kept_aliases::keeping_aliases(&old, kept, || {
            self.replace_value_under(new_value, flow_context, anchor.as_deref());
            self.value()
        });
    }

    /// Replace the value as [`set_value`](Self::set_value) does, putting
    /// `anchor` on the new value. Without one, a node value keeps the
    /// anchor before it where it was copied from.
    pub(crate) fn replace_value_under(
        &self,
        new_value: impl crate::AsYaml,
        flow_context: bool,
        anchor: Option<&str>,
    ) {
        self.replace_value(new_value, flow_context, anchor);
        crate::nodes::replaced_value::settle_lines_after_block_scalar(&self.0);
        if let Some(content) = self.value().and_then(|v| v.last_child()) {
            crate::nodes::trivia::drop_space_before_empty(&content);
        }
    }

    /// Replace the value with `new_value`, under `old_anchor` if set.
    fn replace_value(
        &self,
        new_value: impl crate::AsYaml,
        flow_context: bool,
        old_anchor: Option<&str>,
    ) {
        use crate::as_yaml::YamlKind;

        // An alias cannot carry an anchor.
        let old_anchor = old_anchor.filter(|_| new_value.kind() != YamlKind::Alias);
        if !flow_context {
            self.add_explicit_value_indicator();
        }

        // A block collection or block scalar coming from another CST goes
        // through set_block_value, which does targeted CST edits to swap
        // the VALUE and adjust the surrounding tokens (drop the WHITESPACE
        // between COLON and VALUE, drop the redundant trailing NEWLINE,
        // rescue any inline comment on the old value line). Flow context
        // (parent mapping is flow-style) means block content isn't valid
        // there, so we fall through to the default path.
        let built = (!flow_context)
            .then(|| built_block_node(&new_value))
            .flatten();
        let block_source = new_value
            .value_node()
            .filter(|_| !flow_context && !new_value.is_inline())
            .or(built)
            .filter(|_| {
                matches!(
                    new_value.kind(),
                    YamlKind::Mapping | YamlKind::Sequence | YamlKind::Scalar | YamlKind::Tagged(_)
                )
            });
        if let Some(source) = block_source {
            self.set_block_value(&source, old_anchor);
            return;
        }

        // Build the new VALUE. Without an old anchor to keep, a node-backed
        // inline value preceded by an `&anchor` in its parent VALUE keeps
        // that anchor, so aliases pointing at the moved value still resolve.
        // A key-line comment that goes away with the old value (inside the
        // old VALUE, or at MAPPING_ENTRY level before a block VALUE) is
        // embedded inside the new inline VALUE so it stays on the key line
        // and doesn't swallow anything. One outside an inline VALUE, as in
        // a flow mapping, stays where it is.
        let anchor = old_anchor
            .map(str::to_string)
            .or_else(|| new_value.value_node().and_then(|n| source_anchor_text(&n)));
        let old_was_block = self.value().is_some_and(|v| value_is_block(&v));
        let trailing_comment = (old_was_block || self.value().is_some_and(|v| has_comment(&v)))
            .then(|| find_key_line_comment(&self.0))
            .flatten();
        let mut value_builder = GreenNodeBuilder::new();
        value_builder.start_node(SyntaxKind::VALUE.into());
        if let Some(a) = anchor.as_deref() {
            value_builder.token(SyntaxKind::ANCHOR.into(), a);
            value_builder.token(SyntaxKind::WHITESPACE.into(), " ");
        }
        // A block scalar rendered here has to clear this entry's own
        // column, so hand it that rather than the document's.
        let flow = flow_context.then(|| {
            self.0
                .parent()
                .map_or(FlowQuoting::Minimal, |m| crate::nodes::flow_quoting(&m))
        });
        let value_ends_with_newline =
            build_in(&new_value, &mut value_builder, self.indent_of_line(), flow);
        if let Some((ws, comment)) = trailing_comment
            .as_ref()
            .filter(|_| !value_ends_with_newline)
        {
            value_builder.token(SyntaxKind::WHITESPACE.into(), ws);
            value_builder.token(SyntaxKind::COMMENT.into(), comment);
        }
        value_builder.finish_node();
        let new_value_node = SyntaxNode::new_root_mut(value_builder.finish());
        if old_anchor.is_some() {
            drop_tagged_anchor(&new_value_node);
        }
        // A block scalar's body follows its header line, which is where
        // the parser puts a comment on the key line.
        if let Some((ws, comment)) = trailing_comment.filter(|_| value_ends_with_newline) {
            crate::nodes::insert_header_comment(&new_value_node, &ws, &comment);
        }

        self.splice_inline_value(&new_value_node, old_was_block, flow_context);
        if value_ends_with_newline {
            self.settle_trailing_newline(true);
        }
    }

    /// Put `new_value_node`, a VALUE that starts on the key line, in place
    /// of this entry's value, which is a block value if `old_was_block`.
    fn splice_inline_value(
        &self,
        new_value_node: &SyntaxNode,
        old_was_block: bool,
        flow_context: bool,
    ) {
        if old_was_block {
            self.rebuild_with_inline_value(new_value_node);
            return;
        }

        // Targeted splice: replace the VALUE child in place; siblings and
        // surrounding tokens (comments, alignment WS) survive untouched.
        if let Some(value_idx) = self.value_index() {
            // A block scalar's body ends with the entry's line break.
            let old_ends_line = match crate::nodes::ending_block_scalar(&self.0) {
                Some(scalar) => crate::nodes::trivia::ends_line(&scalar),
                None => self.ends_with_newline(),
            };
            let old_omitted = self.value().is_some_and(|v| v.text_range().is_empty());
            let new_omitted = new_value_node.text_range().is_empty();
            // The space before a key-line comment that was all the old
            // value had now goes before the new value, which carries the
            // comment with its own.
            let mut at = value_idx..value_idx + 1;
            let mut replacement = vec![new_value_node.clone().into()];
            let space_before = value_idx
                .checked_sub(1)
                .and_then(|i| self.0.children_with_tokens().nth(i))
                .is_some_and(|c| c.kind() == SyntaxKind::WHITESPACE);
            if self.value_starts_with_comment() && space_before {
                at.start -= 1;
                replacement.insert(0, fresh_token(SyntaxKind::WHITESPACE, " ").into());
            }
            let value_idx = at.start + replacement.len() - 1;
            replace_children(&self.0, at, replacement);
            if old_omitted && !new_omitted {
                self.separate_filled_value(value_idx, flow_context);
            }
            if old_ends_line && !self.ends_with_newline() {
                self.append(vec![fresh_token(SyntaxKind::NEWLINE, "\n")]);
            }
            return;
        }

        // No VALUE node in this entry (malformed input). Insert one right
        // after the COLON with a leading WHITESPACE.
        if let Some(colon_idx) = self
            .0
            .children_with_tokens()
            .position(|c| c.as_token().is_some_and(|t| t.kind() == SyntaxKind::COLON))
        {
            let insert_at = colon_idx + 1;
            let ws = fresh_token(SyntaxKind::WHITESPACE, " ");
            insert_children(
                &self.0,
                insert_at,
                vec![ws.into(), new_value_node.clone().into()],
            );
        }
    }

    /// Replace this entry's block value with `new_value_node` (an inline
    /// VALUE) that sits on the same line as the key. Uses targeted CST
    /// edits: splice the VALUE, ensure a single WHITESPACE sits between
    /// COLON and it, rescue any inline comment on the key line so it
    /// survives, and append a trailing NEWLINE if the entry doesn't
    /// already end with one.
    fn rebuild_with_inline_value(&self, new_value_node: &SyntaxNode) {
        let Some(value_idx) = self.value_index() else {
            return;
        };

        // Right after the COLON of an old block VALUE there is only trailing
        // whitespace and possibly a key-line comment (`k:  # note\n  - block`),
        // never a value separator. Detach them: the new inline
        // `new_value_node` already carries the rescued comment inside
        // itself. Leaving the old tokens would duplicate the comment and -
        // since comments extend to end of line - swallow the new value.
        // Whitespace before the COLON (after `?`, or in `k :`) stays.
        let stale: Vec<_> = self
            .0
            .children_with_tokens()
            .take(value_idx)
            .skip_while(|c| c.kind() != SyntaxKind::COLON)
            .skip(1)
            .map_while(|c| {
                c.into_token()
                    .filter(|t| matches!(t.kind(), SyntaxKind::WHITESPACE | SyntaxKind::COMMENT))
            })
            .collect();
        for tok in stale {
            tok.detach();
        }

        // Swap VALUE (its index may have moved if we detached above).
        let value_idx = self.value_index().expect("VALUE was here a moment ago");
        replace_children(
            &self.0,
            value_idx..value_idx + 1,
            vec![new_value_node.clone().into()],
        );

        // Ensure a single WHITESPACE sits between COLON and VALUE - the
        // old block VALUE typically had none.
        if let Some(idx) = self.value_index() {
            let prev_is_ws = idx.checked_sub(1).is_some_and(|p| {
                self.0
                    .children_with_tokens()
                    .nth(p)
                    .and_then(|c| c.into_token())
                    .is_some_and(|t| t.kind() == SyntaxKind::WHITESPACE)
            });
            if !prev_is_ws {
                self.insert_at(idx, vec![fresh_token(SyntaxKind::WHITESPACE, " ")]);
            }
        }

        // Inline values don't own a trailing NEWLINE; ensure the entry has
        // one.
        if !self.ends_with_newline() {
            self.append(vec![fresh_token(SyntaxKind::NEWLINE, "\n")]);
        }
    }

    /// Detach this entry from its parent mapping, effectively removing it.
    ///
    /// The entry node is detached from the tree; the `MappingEntry` value is
    /// consumed. To retrieve the removed entry from a mapping (and get back a
    /// `MappingEntry` you can inspect), use [`Mapping::remove`](super::Mapping::remove) instead.
    pub fn discard(self) {
        crate::nodes::kept_aliases::keeping_aliases(&self.0, None, || {
            self.0.detach();
            None
        });
    }

    /// Remove this entry from its parent mapping.
    ///
    /// This is a convenience method that calls [`discard`](Self::discard)
    /// internally. It's useful when you have a [`MappingEntry`] (e.g., from
    /// [`find_all_entries_by_key`](super::Mapping::find_all_entries_by_key)) and want
    /// to remove it without retrieving it from the mapping again.
    ///
    /// Consumes `self` and detaches the entry from the parent mapping.
    pub fn remove(self) {
        self.discard();
    }

    crate::nodes::ast_node_spans!("mapping entry");
}
