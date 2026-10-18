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

    /// The tag, in its `!!` shorthand if it is in the `tag:yaml.org,2002:`
    /// namespace.
    pub(crate) fn normalized_tag(&self) -> Option<String> {
        self.tag().map(|t| crate::tag::normalize(&t).into_owned())
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
        if self.normalized_tag().as_deref() != Some(crate::tag::OMAP) {
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
        if self.normalized_tag().as_deref() != Some(crate::tag::PAIRS) {
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
        _flow_context: bool,
    ) -> bool {
        // TODO: in flow context, quote plain content holding a flow
        // indicator (`!t a, b`), as Scalar does.
        let content_on_tag_line = self
            .0
            .children_with_tokens()
            .take_while(|c| c.as_node().is_none())
            .all(|c| c.kind() != SyntaxKind::NEWLINE);
        let delta = if content_on_tag_line {
            let delta = indent as isize - crate::as_yaml::source_line_indent(&self.0) as isize;
            crate::as_yaml::clear_parent_column(&self.0, indent, delta)
        } else {
            continuation_indent(&self.0).map_or(0, |base| indent as isize - base as isize)
        };
        builder.start_node(SyntaxKind::TAGGED_NODE.into());
        let ends_with_newline = crate::as_yaml::copy_node_content_reindent(builder, &self.0, delta);
        builder.finish_node();
        ends_with_newline
    }

    fn is_inline(&self) -> bool {
        crate::yaml::ValueNode::is_inline(self)
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

    #[test]
    fn test_as_set_requires_mapping() {
        let members = |yaml: &str| {
            let yaml = YamlFile::from_str(yaml).unwrap();
            let doc = yaml.documents().next().unwrap();
            let val = doc.as_mapping().unwrap().get_node("key").unwrap();
            let tagged = crate::yaml::TaggedNode::cast(val).unwrap();
            tagged
                .as_set()
                .map(|set| set.members().map(|m| m.to_string()).collect::<Vec<_>>())
        };
        assert_eq!(
            members("key: !!set {? a, ? b}\n"),
            Some(vec!["a".to_string(), "b".to_string()])
        );
        assert_eq!(members("key: !!set &s {}\n"), Some(vec![]));
        assert_eq!(members("key: !!set [a, b]\n"), None);
        assert_eq!(members("key: !!set\n- a\n"), None);
        assert_eq!(members("key: !!set\n"), None);
    }

    #[test]
    fn test_pairs_accessors_require_sequence() {
        let tagged = |yaml: &str| {
            let yaml = YamlFile::from_str(yaml).unwrap();
            let doc = yaml.documents().next().unwrap();
            let val = doc.as_mapping().unwrap().get_node("key").unwrap();
            crate::yaml::TaggedNode::cast(val).unwrap()
        };
        let keys = |entries: Option<Vec<crate::MappingEntry>>| {
            entries.map(|entries| {
                entries
                    .iter()
                    .map(|e| e.key_node().unwrap().to_string())
                    .collect::<Vec<_>>()
            })
        };
        assert_eq!(
            keys(tagged("key: !!omap [a: 1, b: 2]\n").as_ordered_mapping()),
            Some(vec!["a".to_string(), "b".to_string()])
        );
        assert_eq!(
            keys(tagged("key: !!pairs [a: 1]\n").as_pairs()),
            Some(vec!["a".to_string()])
        );
        assert_eq!(
            keys(tagged("key: !!omap {a: 1}\n").as_ordered_mapping()),
            None
        );
        assert_eq!(keys(tagged("key: !!pairs {a: 1}\n").as_pairs()), None);
        assert_eq!(
            keys(tagged("key: !!omap\n  a: 1\n").as_ordered_mapping()),
            None
        );
    }

    #[test]
    fn test_tagged_block_value_is_reindented() {
        use crate::debug::{roundtrip_ok, tree_to_string, validate_tree};
        use crate::{AsYaml, Document, Sequence};

        let src = Document::from_str(
            "a: !!map\n  k: 1\n  j: 2\nx:\n  b: !!seq\n      - 1\n      - 2\nc: !!str foo\n",
        )
        .unwrap();
        let root = src.as_mapping().unwrap();
        let map = root.get("a").unwrap();
        let seq = root.get_mapping("x").unwrap().get("b").unwrap();
        let scalar = root.get("c").unwrap();

        let target = |doc: &Document| -> Sequence {
            doc.as_sequence()
                .or_else(|| doc.as_mapping()?.get_sequence("s"))
                .unwrap()
        };
        type Op<'a> = Box<dyn Fn(&Document) + 'a>;
        let cases: Vec<(&str, Op, &str)> = vec![
            (
                "s:\n  - a\n",
                Box::new(|d: &Document| target(d).push(&map)),
                "s:\n  - a\n  - !!map\n    k: 1\n    j: 2\n",
            ),
            (
                "- a\n",
                Box::new(|d: &Document| target(d).push(&map)),
                "- a\n- !!map\n  k: 1\n  j: 2\n",
            ),
            (
                "s:\n  - a\n",
                Box::new(|d: &Document| target(d).insert(0, &map)),
                "s:\n  - !!map\n    k: 1\n    j: 2\n  - a\n",
            ),
            (
                "s:\n  - a\n  - b\n",
                Box::new(|d: &Document| assert!(target(d).set(0, &map))),
                "s:\n  - !!map\n    k: 1\n    j: 2\n  - b\n",
            ),
            (
                "- a\n",
                Box::new(|d: &Document| target(d).push(&seq)),
                "- a\n- !!seq\n  - 1\n  - 2\n",
            ),
            (
                "- a\n",
                Box::new(|d: &Document| target(d).push(&scalar)),
                "- a\n- !!str foo\n",
            ),
        ];
        for (input, op, expected) in cases {
            let doc = Document::from_str(input).unwrap();
            op(&doc);
            let node = doc.as_node().unwrap();
            assert_eq!(doc.to_string(), expected, "{input:?}");
            validate_tree(node).unwrap();
            roundtrip_ok(node).unwrap();
            let reparsed = Document::from_str(&doc.to_string()).unwrap();
            assert_eq!(
                tree_to_string(node),
                tree_to_string(reparsed.as_node().unwrap()),
                "{input:?}"
            );
        }

        let doc = Document::from_str("o:\n  b: 1\n").unwrap();
        let inner = doc.as_mapping().unwrap().get_mapping("o").unwrap();
        inner.set("b", &map);
        inner.set("c", &map);
        let expected = "o:\n  b: !!map\n    k: 1\n    j: 2\n  c: !!map\n    k: 1\n    j: 2\n";
        assert_eq!(doc.to_string(), expected);
        let node = doc.as_node().unwrap();
        validate_tree(node).unwrap();
        roundtrip_ok(node).unwrap();
        let reparsed = Document::from_str(expected).unwrap();
        assert_eq!(
            tree_to_string(node),
            tree_to_string(reparsed.as_node().unwrap())
        );
    }

    #[test]
    fn test_verbatim_collection_tags() {
        let tagged = |yaml: &str| {
            YamlFile::from_str(yaml)
                .unwrap()
                .document()
                .unwrap()
                .as_mapping()
                .unwrap()
                .get("a")
                .unwrap()
                .as_tagged()
                .unwrap()
                .clone()
        };
        let set = tagged("a: !<tag:yaml.org,2002:set> {x, y}\n");
        let members: Vec<String> = set
            .as_set()
            .unwrap()
            .members()
            .map(|m| m.to_string())
            .collect();
        assert_eq!(members, vec!["x", "y"]);
        let omap = tagged("a: !<tag:yaml.org,2002:omap> [b: 1, c: 2]\n");
        assert_eq!(omap.as_ordered_mapping().map(|e| e.len()), Some(2));
        let pairs = tagged("a: !<tag:yaml.org,2002:pairs> [b: 1, b: 2]\n");
        assert_eq!(pairs.as_pairs().map(|e| e.len()), Some(2));
    }
}
