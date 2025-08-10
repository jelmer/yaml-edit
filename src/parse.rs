//! Parser types and utilities.

use crate::lex::SyntaxKind;
use rowan::GreenNode;

/// The result of a parse operation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Parse<T> {
    green_node: GreenNode,
    errors: Vec<String>,
    positioned_errors: Vec<crate::PositionedParseError>,
    _ty: std::marker::PhantomData<fn() -> T>,
}

impl<T> Parse<T> {
    pub(crate) fn new_with_positioned_errors(
        green_node: GreenNode,
        errors: Vec<String>,
        positioned_errors: Vec<crate::PositionedParseError>,
    ) -> Self {
        Parse {
            green_node,
            errors,
            positioned_errors,
            _ty: std::marker::PhantomData,
        }
    }

    /// The parse tree. If there were no parse errors, this is a valid tree.
    /// If there were parse errors, this tree might be only partially valid.
    pub fn tree(&self) -> T
    where
        T: From<rowan::SyntaxNode<crate::Lang>>,
    {
        let syntax_node = rowan::SyntaxNode::new_root_mut(self.green_node.clone());
        T::from(syntax_node)
    }

    /// Parse errors, if any.
    pub fn errors(&self) -> &[String] {
        &self.errors
    }

    /// Positioned parse errors with location information.
    pub fn positioned_errors(&self) -> &[crate::PositionedParseError] {
        &self.positioned_errors
    }

    /// Convert parse result to Result, failing if there are any errors.
    pub fn to_result(self) -> Result<T, crate::ParseError>
    where
        T: From<rowan::SyntaxNode<crate::Lang>>,
    {
        if !self.errors.is_empty() {
            Err(crate::ParseError(self.errors))
        } else {
            Ok(self.tree())
        }
    }

    /// Whether the parse had any errors.
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }
}

impl Parse<crate::Yaml> {
    /// Parse YAML text, returning a Parse result
    pub fn parse_yaml(text: &str) -> Self {
        let parsed = crate::yaml::parse(text);
        Parse::new_with_positioned_errors(
            parsed.green_node,
            parsed.errors,
            parsed.positioned_errors,
        )
    }
}
