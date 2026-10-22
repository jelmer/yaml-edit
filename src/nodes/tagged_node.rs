use super::{Lang, SyntaxNode};
use crate::as_yaml::{AsYaml, YamlKind};
use crate::lex::SyntaxKind;
use crate::yaml::{Mapping, MappingEntry, Scalar, Sequence, Set};
use rowan::ast::AstNode;

ast_node!(
    TaggedNode,
    TAGGED_NODE,
    "A YAML tagged scalar (tag + value)"
);
impl TaggedNode {
    /// Get the tag part of this tagged scalar (e.g., "!custom" from "!custom value")
    pub fn tag(&self) -> Option<String> {
        // Find the tag token in the children
        for child in self.0.children_with_tokens() {
            if let rowan::NodeOrToken::Token(token) = child {
                if token.kind() == SyntaxKind::TAG {
                    return Some(token.text().to_string());
                }
            }
        }
        None
    }

    /// The tag as the `%TAG` directives of its document resolve it: in the
    /// `!!` shorthand for a tag in the `tag:yaml.org,2002:` namespace however
    /// it is written, verbatim (`!<uri>`) for another tag a declared handle
    /// expands, and as written otherwise.
    ///
    /// With `%TAG !y! tag:yaml.org,2002:` in effect, `!y!int` resolves to
    /// `!!int`; [`tag`](Self::tag) still gives `!y!int`.
    pub fn resolved_tag(&self) -> Option<String> {
        let tag = self.tag()?;
        Some(crate::tag::resolve(&tag, &crate::tag::declared_handles(&self.0)).into_owned())
    }

    /// Get the value part of this tagged scalar (without the tag)
    pub fn value(&self) -> Option<Scalar> {
        // Find the nested SCALAR node
        for child in self.0.children() {
            if child.kind() == SyntaxKind::SCALAR {
                return Scalar::cast(child);
            }
        }
        None
    }

    /// Get the collection this tag is attached to, if the tagged node wraps
    /// a mapping (e.g. `!custom` on a block mapping).
    pub fn as_mapping(&self) -> Option<Mapping> {
        self.0.children().find_map(Mapping::cast)
    }

    /// Get the collection this tag is attached to, if the tagged node wraps
    /// a sequence (e.g. `!!seq` on a flow sequence).
    pub fn as_sequence(&self) -> Option<Sequence> {
        self.0.children().find_map(Sequence::cast)
    }

    /// Get the string value of this tagged scalar (just the value part),
    /// with quotes stripped and escape sequences processed.
    ///
    /// Returns `None` if the tag is on a collection or alias.
    pub fn as_string(&self) -> Option<String> {
        self.value().map(|scalar| scalar.as_string())
    }

    /// Extract a set from this tagged node if it has a !!set tag on a
    /// mapping.
    pub fn as_set(&self) -> Option<Set> {
        Set::cast(self.0.clone())
    }

    /// Extract ordered mapping from this tagged node if it has a !!omap tag
    /// on a sequence.
    ///
    /// Returns the entries of the inner sequence as `MappingEntry` values, each
    /// holding a single key-value pair with full CST fidelity.
    pub fn as_ordered_mapping(&self) -> Option<Vec<MappingEntry>> {
        if self.resolved_tag().as_deref() != Some(crate::tag::OMAP) {
            return None;
        }
        self.extract_mapping_entries()
    }

    /// Extract pairs from this tagged node if it has a !!pairs tag on a
    /// sequence.
    ///
    /// Returns the entries of the inner sequence as `MappingEntry` values.
    /// Unlike `!!omap`, duplicate keys are allowed.
    pub fn as_pairs(&self) -> Option<Vec<MappingEntry>> {
        if self.resolved_tag().as_deref() != Some(crate::tag::PAIRS) {
            return None;
        }
        self.extract_mapping_entries()
    }

    /// Shared helper: iterate over the inner sequence and collect the first
    /// `MappingEntry` from each single-key mapping item.
    fn extract_mapping_entries(&self) -> Option<Vec<MappingEntry>> {
        let sequence = self.as_sequence()?;
        Some(
            sequence
                .items()
                .filter_map(Mapping::cast)
                .filter_map(|mapping| mapping.entries().next())
                .collect(),
        )
    }
}

/// The indentation of the first non-blank line after `node`'s first line
/// break, or `None` if it fits on one line.
fn continuation_indent(node: &SyntaxNode) -> Option<usize> {
    let mut tokens = node
        .descendants_with_tokens()
        .filter_map(|e| e.into_token())
        .skip_while(|t| t.kind() != SyntaxKind::NEWLINE);
    tokens.next()?;
    let first = tokens.find(|t| t.kind() != SyntaxKind::NEWLINE);
    Some(
        first
            .filter(|t| matches!(t.kind(), SyntaxKind::INDENT | SyntaxKind::WHITESPACE))
            .map_or(0, |t| t.text().len()),
    )
}

impl TaggedNode {
    /// A copy of this node with its plain content double quoted, if a flow
    /// collection would end that content at a flow indicator in it.
    fn flow_quoted(&self) -> Option<SyntaxNode> {
        let scalar = self.0.children().find_map(Scalar::cast)?;
        if !scalar.needs_flow_quotes() {
            return None;
        }
        let mut builder = rowan::GreenNodeBuilder::new();
        builder.start_node(SyntaxKind::ROOT.into());
        scalar.build_content(&mut builder, 0, true);
        builder.finish_node();
        let quoted = builder.finish().children().next()?.into_node()?.to_owned();
        let green = self
            .0
            .green()
            .replace_child(scalar.syntax().index(), quoted.into());
        Some(SyntaxNode::new_root(green))
    }
}

impl AsYaml for TaggedNode {
    fn as_node(&self) -> Option<&SyntaxNode> {
        Some(&self.0)
    }

    fn kind(&self) -> YamlKind {
        self.tag().map_or(YamlKind::Scalar, |t| {
            YamlKind::Tagged(std::borrow::Cow::Owned(t))
        })
    }

    /// Content on lines after the tag (a block collection, say) is shifted
    /// to start at column `indent`, as for an untagged collection. Content
    /// starting on the tag line (a flow collection) is shifted like an
    /// untagged flow collection, keeping its continuation lines' offset
    /// from `indent`.
    fn build_content(
        &self,
        builder: &mut rowan::GreenNodeBuilder,
        indent: usize,
        flow_context: bool,
    ) -> bool {
        let quoted = flow_context.then(|| self.flow_quoted()).flatten();
        let node = quoted.as_ref().unwrap_or(&self.0);
        let content_on_tag_line = self
            .0
            .children_with_tokens()
            .take_while(|c| c.as_node().is_none())
            .all(|c| c.kind() != SyntaxKind::NEWLINE);
        let delta = if content_on_tag_line {
            let delta = indent as isize - crate::column::line_indent(&self.0) as isize;
            crate::as_yaml::clear_parent_column(&self.0, indent, delta)
        } else {
            continuation_indent(&self.0).map_or(0, |base| indent as isize - base as isize)
        };
        builder.start_node(SyntaxKind::TAGGED_NODE.into());
        let ends_with_newline = crate::as_yaml::copy_node_content_reindent(builder, node, delta);
        builder.finish_node();
        ends_with_newline
    }

    fn is_inline(&self) -> bool {
        crate::yaml::ValueNode::is_inline(self)
    }
}
