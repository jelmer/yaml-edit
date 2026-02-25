//! Parser types and utilities.

use rowan::GreenNode;

/// The result of a parse operation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Parse<T> {
    green_node: GreenNode,
    positioned_errors: Vec<crate::PositionedParseError>,
    _ty: std::marker::PhantomData<fn() -> T>,
}

impl<T> Parse<T> {
    pub(crate) fn new(
        green_node: GreenNode,
        positioned_errors: Vec<crate::PositionedParseError>,
    ) -> Self {
        Parse {
            green_node,
            positioned_errors,
            _ty: std::marker::PhantomData,
        }
    }

    /// The parse tree. If there were no parse errors, this is a valid tree.
    /// If there were parse errors, this tree might be only partially valid.
    pub fn tree(&self) -> T
    where
        T: rowan::ast::AstNode<Language = crate::Lang>,
    {
        let syntax_node = rowan::SyntaxNode::new_root_mut(self.green_node.clone());
        T::cast(syntax_node)
            .expect("Parse<T> always holds a green node whose root kind matches T::can_cast")
    }

    /// Positioned parse errors with location information.
    pub fn positioned_errors(&self) -> &[crate::PositionedParseError] {
        &self.positioned_errors
    }

    /// Parse error messages, if any.
    pub fn errors(&self) -> Vec<String> {
        self.positioned_errors
            .iter()
            .map(|e| e.message.clone())
            .collect()
    }

    /// Convert parse result to a `Result`, failing with the first error if any.
    ///
    /// The returned `YamlError::Parse` contains the error message but **no
    /// line/column information** because `Parse` does not retain the source
    /// text. Use [`YamlFile::from_str`](crate::YamlFile) (which calls
    /// `byte_offset_to_line_column` internally) if you need precise positions
    /// in error messages.
    pub fn to_result(self) -> Result<T, crate::YamlError>
    where
        T: rowan::ast::AstNode<Language = crate::Lang>,
    {
        if let Some(first) = self.positioned_errors.first() {
            Err(crate::YamlError::Parse {
                message: first.message.clone(),
                line: None,
                column: None,
            })
        } else {
            Ok(self.tree())
        }
    }

    /// Whether the parse had any errors.
    pub fn has_errors(&self) -> bool {
        !self.positioned_errors.is_empty()
    }
}

impl Parse<crate::YamlFile> {
    /// Parse YAML text, returning a Parse result
    pub fn parse_yaml(text: &str) -> Self {
        let parsed = crate::yaml::parse(text);
        Parse::new(parsed.green_node, parsed.positioned_errors)
    }
}
