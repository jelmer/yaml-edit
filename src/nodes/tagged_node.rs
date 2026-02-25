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

    /// Get the string value of this tagged scalar (just the value part),
    /// with quotes stripped and escape sequences processed.
    pub fn as_string(&self) -> Option<String> {
        if let Some(scalar) = self.value() {
            Some(scalar.as_string())
        } else {
            // Handle cases where the value might be nested deeper
            self.extract_deepest_string_value()
        }
    }

    /// Extract the deepest string value, handling nested tag structures
    fn extract_deepest_string_value(&self) -> Option<String> {
        Self::find_string_token_recursive(&self.0)
    }

    /// Recursively search for the first STRING token in the tree
    fn find_string_token_recursive(node: &rowan::SyntaxNode<crate::yaml::Lang>) -> Option<String> {
        // Check tokens first
        for child in node.children_with_tokens() {
            if let rowan::NodeOrToken::Token(token) = child {
                if token.kind() == SyntaxKind::STRING {
                    return Some(token.text().to_string());
                }
            }
        }

        // Then check child nodes recursively
        for child in node.children() {
            if let Some(result) = Self::find_string_token_recursive(&child) {
                return Some(result);
            }
        }

        None
    }

    /// Extract a set from this tagged scalar if it has a !!set tag
    pub fn as_set(&self) -> Option<Set> {
        Set::cast(self.0.clone())
    }

    /// Extract ordered mapping from this tagged scalar if it has a !!omap tag.
    ///
    /// Returns the entries of the inner sequence as `MappingEntry` values, each
    /// holding a single key-value pair with full CST fidelity.
    pub fn as_ordered_mapping(&self) -> Option<Vec<MappingEntry>> {
        if self.tag().as_deref() != Some("!!omap") {
            return None;
        }
        Some(self.extract_mapping_entries())
    }

    /// Extract pairs from this tagged scalar if it has a !!pairs tag.
    ///
    /// Returns the entries of the inner sequence as `MappingEntry` values.
    /// Unlike `!!omap`, duplicate keys are allowed.
    pub fn as_pairs(&self) -> Option<Vec<MappingEntry>> {
        if self.tag().as_deref() != Some("!!pairs") {
            return None;
        }
        Some(self.extract_mapping_entries())
    }

    /// Shared helper: iterate over the inner sequence and collect the first
    /// `MappingEntry` from each single-key mapping item.
    fn extract_mapping_entries(&self) -> Vec<MappingEntry> {
        let mut entries = Vec::new();
        for child in self.0.children() {
            if let Some(sequence) = Sequence::cast(child) {
                for item in sequence.items() {
                    if let Some(mapping) = Mapping::cast(item) {
                        if let Some(entry) = mapping.entries().next() {
                            entries.push(entry);
                        }
                    }
                }
                break;
            }
        }
        entries
    }
}

impl AsYaml for TaggedNode {
    fn as_node(&self) -> Option<&SyntaxNode> {
        Some(&self.0)
    }

    fn kind(&self) -> YamlKind {
        self.tag()
            .map(|t| YamlKind::Tagged(std::borrow::Cow::Owned(t)))
            .unwrap_or(YamlKind::Scalar)
    }

    fn build_content(
        &self,
        builder: &mut rowan::GreenNodeBuilder,
        _indent: usize,
        _flow_context: bool,
    ) -> bool {
        crate::as_yaml::copy_node_content(builder, &self.0);
        false
    }

    fn is_inline(&self) -> bool {
        // Tagged scalars are always inline (they appear on the same line as their key)
        true
    }
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use rowan::ast::AstNode;

    use crate::YamlFile;

    #[test]
    fn test_tagged_node_as_string_plain() {
        let yaml = YamlFile::from_str("key: !custom hello").unwrap();
        let doc = yaml.documents().next().unwrap();
        let mapping = doc.as_mapping().unwrap();
        let val = mapping.get_node("key").unwrap();
        let tagged = crate::yaml::TaggedNode::cast(val).unwrap();
        assert_eq!(tagged.as_string(), Some("hello".to_string()));
    }

    #[test]
    fn test_tagged_node_as_string_double_quoted() {
        // Without the fix, .value() returned the raw text including quotes.
        let yaml = YamlFile::from_str(r#"key: !custom "hello world""#).unwrap();
        let doc = yaml.documents().next().unwrap();
        let mapping = doc.as_mapping().unwrap();
        let val = mapping.get_node("key").unwrap();
        let tagged = crate::yaml::TaggedNode::cast(val).unwrap();
        assert_eq!(tagged.as_string(), Some("hello world".to_string()));
    }
}
