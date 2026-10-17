//! `impl MappingEntry` -- the key/value/set_value/discard/remove API
//! on a single MAPPING_ENTRY node, plus its private layout helpers
//! (indent_of_line, insert_at, set_block_value, rebuild_with_inline_value).
//!
//! Split out of `nodes/mapping/mod.rs` unchanged.

use super::helpers::{
    add_key_line_comment, build_block_value_node, built_block_node, find_key_line_comment,
    flow_form, has_comment, source_anchor_text, value_is_block,
};
use super::{key_content_matches, MappingEntry};
use crate::lex::SyntaxKind;
use crate::nodes::{entry_key, entry_value, fresh_token, Lang, SyntaxNode};
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
        if let Some(key) = flow_form(&key, key_indent + 2) {
            // Comments make the key span several lines, which only an
            // explicit key may do.
            let multiline = has_comment(key.syntax());
            let explicit = use_explicit_key || multiline;
            return Self::new_at_indent(key, value, flow_context, explicit, key_indent);
        }
        if let Some(value) = flow_context
            .then(|| flow_form(&value, key_indent + 2))
            .flatten()
        {
            return Self::new_at_indent(key, value, flow_context, use_explicit_key, key_indent);
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
        crate::nodes::build_key_content(&mut builder, &key);
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
            .as_node()
            .filter(|_| !flow_context && !value.is_inline())
            .or(built.as_ref())
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
                    || crate::as_yaml::source_base_indent(src) > 0
                    || src.children().next().is_none()
            });

        let value_ends_with_newline = if let Some(source) = block_source {
            let block = build_block_value_node(source, key_indent + 2);
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
                    // block-style content.
                    // TODO: pass key_indent; at 0, a multi-line flow value
                    // copied from a shallower key under a nested one keeps
                    // continuation lines too shallow for its new key.
                    value.build_content(&mut builder, 0, flow_context)
                }
                // Block mappings and sequences start on new line but don't get pre-indented
                // They handle their own indentation via build_content
                (false, crate::as_yaml::YamlKind::Mapping | crate::as_yaml::YamlKind::Sequence) => {
                    builder.token(SyntaxKind::NEWLINE.into(), "\n");
                    value.build_content(&mut builder, 0, flow_context)
                }
                // Block scalars (literal/folded) get newline and indent
                (false, _) => {
                    builder.token(SyntaxKind::NEWLINE.into(), "\n");
                    builder.token(SyntaxKind::INDENT.into(), "  ");
                    value.build_content(&mut builder, 2, flow_context)
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
        MappingEntry(SyntaxNode::new_root_mut(builder.finish()))
    }

    /// Column at which this entry's key sits on its line.
    ///
    /// Returns 0 for root-level entries or entries that share a line with
    /// something else (an explicit-key form, a flow-style parent, etc.).
    fn indent_of_line(&self) -> usize {
        crate::as_yaml::source_base_indent(&self.0)
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
        self.0.splice_children(at..at, elems);
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

    /// Replace this entry's value with a block sequence or mapping from
    /// another CST. Emits the value on the line after the key, indented to
    /// this entry's nesting depth + 2, with the source's interior
    /// indentation shifted to match. Called only from [`set_value`] for
    /// non-flow parents whose new value is a node-backed block collection.
    ///
    /// Uses targeted CST edits (splice the VALUE, detach specific
    /// surrounding tokens) rather than rebuilding the whole entry, so
    /// unrelated tokens (leading comments, alignment whitespace) survive.
    fn set_block_value(&self, source: &SyntaxNode) {
        let block = build_block_value_node(source, self.indent_of_line() + 2);
        let value_ends_with_newline = block.ends_with_newline;

        // For explicit-key entries the parser sometimes stores the entry's
        // terminating NEWLINE at the MAPPING level (as a sibling of this
        // MAPPING_ENTRY) rather than inside it. Track this so we can drop
        // the sibling NEWLINE below if the new block value provides its own.
        let old_entry_owned_trailing_nl = self.ends_with_newline();

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
        let separator = match find_key_line_comment(&self.0) {
            Some((ws, comment)) if !block.on_key_line => {
                block.node.splice_children(
                    0..0,
                    vec![fresh_token(SyntaxKind::COMMENT, &comment).into()],
                );
                Some(ws)
            }
            Some(comment) => {
                add_key_line_comment(&block.node, comment);
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
        crate::nodes::replace_children(&self.0, start..value_idx + 1, replacement);

        // Keep exactly one trailing NEWLINE on the entry, either inside
        // VALUE (block content usually provides one), at MAPPING_ENTRY
        // level, or at MAPPING level (for explicit-key entries).
        if value_ends_with_newline {
            // Drop the MAPPING_ENTRY-level trailing NEWLINE that was the
            // OLD entry's terminator - the new VALUE has its own.
            self.detach_last_if_newline();
        } else if !self.ends_with_newline() {
            // New value doesn't provide one and the entry doesn't have
            // one - append so we don't glue onto the next entry.
            self.append(vec![fresh_token(SyntaxKind::NEWLINE, "\n")]);
        }
        let entry_ends_with_nl = self.ends_with_newline();
        if entry_ends_with_nl && !old_entry_owned_trailing_nl {
            if let Some(next) = self.0.next_sibling_or_token() {
                if next
                    .as_token()
                    .is_some_and(|t| t.kind() == SyntaxKind::NEWLINE)
                {
                    next.detach();
                }
            }
        }
    }

    /// Replace the value of this entry in place, preserving the key and surrounding whitespace.
    pub fn set_value(&self, new_value: impl crate::AsYaml, flow_context: bool) {
        use crate::as_yaml::YamlKind;

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
            .as_node()
            .filter(|_| !flow_context && !new_value.is_inline())
            .or(built.as_ref())
            .filter(|_| {
                matches!(
                    new_value.kind(),
                    YamlKind::Mapping | YamlKind::Sequence | YamlKind::Scalar | YamlKind::Tagged(_)
                )
            });
        if let Some(source) = block_source {
            self.set_block_value(source);
            return;
        }

        // Build the new VALUE. If the source is a node-backed inline value
        // preceded by an `&anchor` in its parent VALUE, preserve that
        // anchor so aliases pointing at the moved value still resolve.
        // Any key-line comment from the old entry (whether it lived inside
        // the old VALUE or at MAPPING_ENTRY level between COLON and VALUE)
        // is embedded inside the new inline VALUE so it stays on the key
        // line and doesn't swallow anything.
        let anchor = new_value.as_node().and_then(source_anchor_text);
        let trailing_comment = find_key_line_comment(&self.0);
        let mut value_builder = GreenNodeBuilder::new();
        value_builder.start_node(SyntaxKind::VALUE.into());
        if let Some(a) = anchor.as_deref() {
            value_builder.token(SyntaxKind::ANCHOR.into(), a);
            value_builder.token(SyntaxKind::WHITESPACE.into(), " ");
        }
        // A block scalar rendered here has to clear this entry's own
        // column, so hand it that rather than the document's.
        new_value.build_content(&mut value_builder, self.indent_of_line(), flow_context);
        if let Some((ws, comment)) = &trailing_comment {
            value_builder.token(SyntaxKind::WHITESPACE.into(), ws);
            value_builder.token(SyntaxKind::COMMENT.into(), comment);
        }
        value_builder.finish_node();
        let new_value_node = SyntaxNode::new_root_mut(value_builder.finish());

        let old_was_block = self.value().is_some_and(|v| value_is_block(&v));
        if old_was_block {
            self.rebuild_with_inline_value(&new_value_node);
            return;
        }

        // Targeted splice: replace the VALUE child in place; siblings and
        // surrounding tokens (comments, alignment WS) survive untouched.
        if let Some(value_idx) = self
            .0
            .children_with_tokens()
            .position(|c| c.as_node().is_some_and(|n| n.kind() == SyntaxKind::VALUE))
        {
            self.0
                .splice_children(value_idx..value_idx + 1, vec![new_value_node.into()]);
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
            self.0
                .splice_children(insert_at..insert_at, vec![ws.into(), new_value_node.into()]);
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
        self.0.splice_children(
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
        // TODO: for an explicit-key entry the parser keeps the line break,
        // and a key-line comment with the whitespace before it, in the
        // MAPPING after the entry rather than in it.
        if !self.ends_with_newline() {
            self.append(vec![fresh_token(SyntaxKind::NEWLINE, "\n")]);
            crate::nodes::hand_line_break_to_mapping(&self.0);
        }
    }

    /// Detach this entry from its parent mapping, effectively removing it.
    ///
    /// The entry node is detached from the tree; the `MappingEntry` value is
    /// consumed. To retrieve the removed entry from a mapping (and get back a
    /// `MappingEntry` you can inspect), use [`Mapping::remove`] instead.
    pub fn discard(self) {
        self.0.detach();
    }

    /// Remove this entry from its parent mapping.
    ///
    /// This is a convenience method that calls [`discard`](Self::discard)
    /// internally. It's useful when you have a [`MappingEntry`] (e.g., from
    /// [`find_all_entries_by_key`](Mapping::find_all_entries_by_key)) and want
    /// to remove it without retrieving it from the mapping again.
    ///
    /// Consumes `self` and detaches the entry from the parent mapping.
    pub fn remove(self) {
        self.discard();
    }
}
