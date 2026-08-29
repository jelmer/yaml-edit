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
//! - An **explicit-key** MAPPING_ENTRY (`? key\n`) with an implicit-null
//!   value has a zero-width `SCALAR { NULL "" }` as its last leaf; the
//!   NEWLINE lives inside the KEY subtree.
//!
//! When inserting a new sibling entry after an existing one, mutation
//! helpers must first ensure the predecessor is terminated
//! (`ensure_trailing_newline` in `mapping.rs` / `sequence.rs`).
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
