//! AST node types for YAML.
//!
//! # CST invariants
//!
//! yaml-edit is a lossless editor: every mutation produces text that
//! re-parses into the same tree shape (see [`crate::debug::roundtrip_ok`]
//! and [`crate::debug::validate_tree`]). To keep that property, all
//! mutation helpers in this module must uphold the following invariants
//! on the concrete syntax tree.
//!
//! ## Where whitespace lives
//!
//! For a block-style mapping entry `key:\n  - a`, the NEWLINE after
//! `key:` and the INDENT before the SEQUENCE both live inside the parent
//! VALUE, *not* inside the child SEQUENCE. The SEQUENCE_ENTRYs
//! themselves carry only the DASH, WHITESPACE, and content:
//!
//! ```text
//! MAPPING_ENTRY
//!   KEY ...
//!   COLON ":"
//!   VALUE
//!     NEWLINE "\n"
//!     INDENT "  "
//!     SEQUENCE
//!       SEQUENCE_ENTRY
//!         DASH "-"
//!         WHITESPACE " "
//!         SCALAR ...
//! ```
//!
//! Consequence: when constructing a fresh block SEQUENCE or MAPPING to
//! splice under a key, do **not** prepend its own leading INDENT for the
//! first entry - the parent VALUE already carries one. Duplicating it
//! renders as doubled indentation and violates the no-stacked-INDENT
//! check in [`crate::debug::validate_tree`].
//!
//! For flow-style values (`{a: 1}`, `[1, 2]`) all separators live
//! *inside* the flow SEQUENCE / MAPPING: the parent VALUE has no direct
//! NEWLINE child. `value_is_block` in `mapping.rs` distinguishes the two
//! by looking for a NEWLINE token as a direct child of VALUE.
//!
//! ## Entry termination
//!
//! Block-style MAPPING_ENTRY and SEQUENCE_ENTRY nodes are terminated by
//! a NEWLINE. That terminator normally lives as the entry's own last
//! token, but the parser sometimes lifts it out when a trailing comment
//! separates it from the next sibling:
//!
//! ```text
//! # SEQUENCE_ENTRY ends with STRING; the NEWLINE lives at the SEQUENCE
//! # level after the following COMMENT.
//! SEQUENCE
//!   SEQUENCE_ENTRY
//!     DASH "-"
//!     SCALAR ("a")
//!   WHITESPACE "  "
//!   COMMENT "# note"
//!   NEWLINE "\n"
//!   SEQUENCE_ENTRY ...
//! ```
//!
//! Two carve-outs:
//! - The **last** entry of a block collection may lack a terminator
//!   (unterminated sources like `a: 1\nb: 2` are legitimate YAML and
//!   must roundtrip).
//! - An entry ending in an **implicit-null** scalar (see below) has a
//!   zero-width `SCALAR { NULL "" }` as its last leaf, not a NEWLINE.
//!   That's a well-formed terminated entry; the NEWLINE lives elsewhere
//!   in the tree (inside the KEY subtree for `? key\n`, inside the VALUE
//!   between COLON and SCALAR for `key:\n`).
//!
//! When inserting a new sibling entry after an existing one, mutation
//! helpers must first ensure the predecessor is terminated
//! (`ensure_trailing_newline` in this module).
//!
//! ## Implicit-null values
//!
//! Every `MAPPING_ENTRY` and `SEQUENCE_ENTRY` holds a value node, even
//! when the source omits it. The parser emits a zero-width
//! `SCALAR { NULL "" }` at every "missing value" position:
//!
//! ```text
//! Source form                     CST shape
//! ------------------------------- ----------------------------------------
//! key:\n     (block mapping)      VALUE { NEWLINE, SCALAR { NULL "" } }
//! key:       (block mapping, EOF) VALUE { SCALAR { NULL "" } }
//! {key}      (flow mapping)       MAPPING_ENTRY { KEY { SCALAR "key" },
//!                                                 VALUE { SCALAR { NULL "" } } }
//! {key:}     (flow mapping)       MAPPING_ENTRY { KEY { SCALAR "key" },
//!                                                 COLON,
//!                                                 VALUE { SCALAR { NULL "" } } }
//! {,}        (flow mapping)       MAPPING_ENTRY { KEY   { SCALAR { NULL "" } },
//!                                                 VALUE { SCALAR { NULL "" } },
//!                                                 COMMA }
//! {: v}      (flow mapping)       KEY { SCALAR { NULL "" } }, COLON, VALUE ...
//! ? key\n    (explicit key)       MAPPING_ENTRY { QUESTION, KEY ..., NEWLINE,
//!                                                 VALUE { SCALAR { NULL "" } } }
//! - \n       (block sequence)     SEQUENCE_ENTRY { DASH, WHITESPACE,
//!                                                  SCALAR { NULL "" }, NEWLINE }
//! [a, , c]   (flow sequence)      SEQUENCE_ENTRY { SCALAR { NULL "" }, COMMA, ... }
//! ```
//!
//! The invariant: **every KEY, every VALUE, every SEQUENCE_ENTRY holds
//! exactly one scalar or collection node.** Mappings wrap the value in a
//! `VALUE` node; sequences hold the scalar directly as a
//! `SEQUENCE_ENTRY` child (matching how each shape wraps its non-null
//! values -- sequences never had a wrapper, mappings always did).
//!
//! The zero-width `NULL ""` token renders as nothing, so round-trips are
//! lossless. This is distinct from a programmatically-written null,
//! which uses `SCALAR { NULL "null" }` (textual, three characters) so
//! the value is visible in the output.
//!
//! Consequences for mutation helpers:
//!
//! - Iterating `SEQUENCE_ENTRY` children with `matches!(kind, SCALAR |
//!   MAPPING | SEQUENCE | ALIAS | TAGGED_NODE)` finds the value in
//!   every entry, including implicit-null ones. `len` / `get` /
//!   `set` / `remove` therefore share the same set of indexes.
//! - `Scalar::is_null()` returns `true` for the zero-width form.
//! - `set(i, real_value)` on an implicit-null entry may need to insert
//!   a separating WHITESPACE that the original didn't have (the value
//!   was zero-width so no separator was needed). See `Sequence::set`
//!   for the `after_dash` pattern used in block sequences.
//!
//! ## No stacked INDENTs, no double trailing NEWLINEs
//!
//! Two adjacent INDENT tokens as direct children of the same node
//! concatenate at render time and produce visibly-wrong indentation
//! (this was the shape of issue #38). Two adjacent NEWLINE tokens at the
//! tail of a MAPPING_ENTRY or SEQUENCE_ENTRY render as a stray blank
//! line (issue #18).
//!
//! Both are checked by [`crate::debug::validate_tree`]; mutation helpers
//! must not produce either. Note the check for double NEWLINE is scoped
//! to *entries*, not their parent MAPPING / SEQUENCE containers: bare
//! NEWLINEs between entries (blank-line separators) are valid formatting
//! and appear at the container level.
//!
//! ## Flow separators live on the previous entry
//!
//! Inside a flow mapping or sequence, the COMMA (and any following
//! WHITESPACE / NEWLINE / INDENT) between two entries is stored as
//! trailing siblings of the *previous* entry's KEY / VALUE / SCALAR,
//! not inside the entry it precedes. Inserters that append a new entry
//! must add a `, ` suffix to the entry they follow, not a `,` prefix on
//! themselves.
//!
//! ## Prefer targeted splices over whole-node rebuilds
//!
//! Reconstructing an entire MAPPING or SEQUENCE from a builder discards
//! anchors, tags, comments, quoting styles, and any tokens the fix
//! didn't know about. Mutation helpers should locate the specific
//! `NodeOrToken` range they need to change and use `splice_children`,
//! preserving the surrounding tokens.

use crate::lex::SyntaxKind;

/// YAML language type for rowan.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Lang {}

impl rowan::Language for Lang {
    type Kind = SyntaxKind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        debug_assert!(
            raw.0 <= SyntaxKind::EOF as u16,
            "raw SyntaxKind value {} is out of range (max {})",
            raw.0,
            SyntaxKind::EOF as u16,
        );
        unsafe { std::mem::transmute::<u16, SyntaxKind>(raw.0) }
    }

    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        kind.into()
    }
}

pub type SyntaxNode = rowan::SyntaxNode<Lang>;
pub type SyntaxToken = rowan::SyntaxToken<Lang>;

/// Build a standalone `SyntaxToken` of `kind` with `text`, ready to
/// splice into a parent's child list via `splice_children`.
pub(crate) fn fresh_token(kind: SyntaxKind, text: &str) -> SyntaxToken {
    let mut builder = rowan::GreenNodeBuilder::new();
    builder.start_node(SyntaxKind::ROOT.into());
    builder.token(kind.into(), text);
    builder.finish_node();
    SyntaxNode::new_root_mut(builder.finish())
        .first_token()
        .expect("just built a token")
}

/// The first direct child node of `parent` with the given `kind`, if any.
pub(crate) fn child_of_kind(parent: &SyntaxNode, kind: SyntaxKind) -> Option<SyntaxNode> {
    parent.children().find(|n| n.kind() == kind)
}

/// Does `parent` have a direct child token whose kind satisfies `pred`?
///
/// Flow/block style and several syntactic properties are decided by the
/// presence of a marker token (`{`, `[`, `|`, `:`) directly under a node,
/// never a nested one, so the search deliberately stays one level deep.
pub(crate) fn has_child_token(parent: &SyntaxNode, pred: impl Fn(SyntaxKind) -> bool) -> bool {
    parent
        .children_with_tokens()
        .any(|el| el.as_token().is_some_and(|t| pred(t.kind())))
}

/// Append `children` at the end of `parent`'s child list.
pub(crate) fn append_children(
    parent: &SyntaxNode,
    children: Vec<rowan::NodeOrToken<SyntaxNode, SyntaxToken>>,
) {
    let end = parent.children_with_tokens().count();
    parent.splice_children(end..end, children);
}

/// Does this subtree end with a NEWLINE leaf, or with any run of
/// zero-width tokens (like an implicit-null NULL "") whose last
/// non-empty predecessor is a NEWLINE?
///
/// `last_token()` alone can't answer this: it returns the deepest tail
/// leaf, which may be a zero-width NULL sitting after a NEWLINE inside
/// the same KEY subtree (`? b\n` under a tagged mapping). Walk `node`'s
/// own token stream backwards, skipping empty tokens, and check what's
/// there. The walk is strictly bounded to `node`'s subtree; callers
/// don't need to worry about escaping into siblings.
pub(crate) fn trailing_newline_reachable(node: &SyntaxNode) -> bool {
    // rowan's descendants_with_tokens isn't DoubleEndedIterator, so
    // materialise the (usually short) tail slice; the walk is still
    // bounded to `node`'s subtree, avoiding prev_token()'s tree-wide
    // reach.
    let mut last_non_empty = None;
    for el in node.descendants_with_tokens() {
        if let Some(t) = el.into_token() {
            if !t.text().is_empty() {
                last_non_empty = Some(t);
            }
        }
    }
    last_non_empty.is_some_and(|t| t.kind() == SyntaxKind::NEWLINE)
}

/// Does `entry`'s line already end in a NEWLINE, counting the sibling one
/// that explicit-key entries carry instead of owning it themselves?
pub(crate) fn entry_line_terminated(entry: &SyntaxNode) -> bool {
    trailing_newline_reachable(entry)
        || entry
            .next_sibling_or_token()
            .is_some_and(|s| s.kind() == SyntaxKind::NEWLINE)
}

/// Append a trailing NEWLINE token to `entry` if its line doesn't already end
/// with one. Used when a block-style entry is about to have a new sibling
/// appended after it (its trailing newline separates the two entries
/// visually).
///
/// The new sibling must be going in immediately after `entry`: an explicit-key
/// entry is treated as terminated by a NEWLINE that follows it as a sibling,
/// which only separates the two if nothing is spliced in between. Callers
/// inserting at an independently computed position want
/// [`ensure_own_trailing_newline`] instead.
pub(crate) fn ensure_trailing_newline(entry: &SyntaxNode) {
    if entry_line_terminated(entry) {
        return;
    }
    append_children(entry, vec![fresh_token(SyntaxKind::NEWLINE, "\n").into()]);
}

/// Append a trailing NEWLINE token to `entry` unless the entry itself already
/// ends in one, ignoring any NEWLINE that merely follows it as a sibling.
pub(crate) fn ensure_own_trailing_newline(entry: &SyntaxNode) {
    if trailing_newline_reachable(entry) {
        return;
    }
    append_children(entry, vec![fresh_token(SyntaxKind::NEWLINE, "\n").into()]);
}

/// The `KEY` child of a `MAPPING_ENTRY`.
pub(crate) fn entry_key(entry: &SyntaxNode) -> Option<SyntaxNode> {
    child_of_kind(entry, SyntaxKind::KEY)
}

/// The `VALUE` child of a `MAPPING_ENTRY` or `SEQUENCE_ENTRY`.
pub(crate) fn entry_value(entry: &SyntaxNode) -> Option<SyntaxNode> {
    child_of_kind(entry, SyntaxKind::VALUE)
}

/// A macro to create AST node wrappers.
macro_rules! ast_node {
    ($ast:ident, $kind:ident, $doc:expr) => {
        #[doc = $doc]
        #[doc = ""]
        #[doc = "**Note:** This type uses interior mutability through the rowan library."]
        #[doc = "Mutation methods work even when called through `&self`. See the crate-level"]
        #[doc = "documentation for details on the mutability model."]
        #[derive(Clone, PartialEq, Eq, Hash)]
        pub struct $ast(pub(crate) SyntaxNode);

        impl std::fmt::Debug for $ast {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_struct(stringify!($ast))
                    .field("syntax", &self.0)
                    .finish()
            }
        }

        impl AstNode for $ast {
            type Language = Lang;

            fn can_cast(kind: SyntaxKind) -> bool {
                kind == SyntaxKind::$kind
            }

            fn cast(syntax: SyntaxNode) -> Option<Self> {
                if Self::can_cast(syntax.kind()) {
                    Some(Self(syntax))
                } else {
                    None
                }
            }

            fn syntax(&self) -> &SyntaxNode {
                &self.0
            }
        }

        impl std::fmt::Display for $ast {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", self.0.text())
            }
        }
    };
}

pub(crate) use ast_node;
/// Emit the `byte_range` / `start_position` / `end_position` trio for a
/// wrapper whose `self.0` exposes `text_range()`. `$what` names the node
/// in the generated docs ("sequence", "comment", ...).
macro_rules! ast_node_spans {
    ($what:expr) => {
        #[doc = concat!("Get the byte offset range of this ", $what, " in the source text.")]
        #[doc = ""]
        #[doc = "Returns the start and end byte offsets as a `TextPosition`."]
        pub fn byte_range(&self) -> crate::TextPosition {
            self.0.text_range().into()
        }

        #[doc = concat!("Get the line and column where this ", $what, " starts.")]
        #[doc = ""]
        #[doc = "Requires the original source text to calculate line/column from byte"]
        #[doc = "offsets. Line and column numbers are 1-indexed."]
        #[doc = ""]
        #[doc = "# Arguments"]
        #[doc = ""]
        #[doc = "* `source_text` - The original YAML source text"]
        pub fn start_position(&self, source_text: &str) -> crate::LineColumn {
            crate::byte_offset_to_line_column(source_text, self.byte_range().start as usize)
        }

        #[doc = concat!("Get the line and column where this ", $what, " ends.")]
        #[doc = ""]
        #[doc = "Requires the original source text to calculate line/column from byte"]
        #[doc = "offsets. Line and column numbers are 1-indexed."]
        #[doc = ""]
        #[doc = "# Arguments"]
        #[doc = ""]
        #[doc = "* `source_text` - The original YAML source text"]
        pub fn end_position(&self, source_text: &str) -> crate::LineColumn {
            crate::byte_offset_to_line_column(source_text, self.byte_range().end as usize)
        }
    };
}

pub(crate) use ast_node_spans;

// Node modules
pub mod alias_node;
pub mod comment;
pub mod directive;
pub mod document;
pub mod entry;
pub mod mapping;
pub mod scalar_node;
pub mod sequence;
pub mod tagged_node;

// Re-exports
pub use alias_node::Alias;
pub use comment::Comment;
pub use directive::Directive;
pub use document::Document;
pub use entry::{Entry, OccupiedEntry, VacantEntry};
pub use mapping::{Mapping, MappingEntry};
pub use scalar_node::{Scalar, ScalarConversionError};
pub use sequence::Sequence;
pub use tagged_node::TaggedNode;
