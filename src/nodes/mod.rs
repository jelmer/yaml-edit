//! AST node types for YAML.

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
pub mod directive;
pub mod document;
pub mod mapping;
pub mod scalar_node;
pub mod sequence;
pub mod tagged_node;

// Re-exports
pub use alias_node::Alias;
pub use directive::Directive;
pub use document::Document;
pub use mapping::{Mapping, MappingEntry};
pub use scalar_node::{Scalar, ScalarConversionError};
pub use sequence::Sequence;
pub use tagged_node::TaggedNode;
