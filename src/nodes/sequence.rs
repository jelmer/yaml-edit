use super::{Lang, SyntaxNode};
use crate::as_yaml::{AsYaml, YamlKind};
use crate::lex::SyntaxKind;
use crate::yaml::ValueNode;
use rowan::ast::AstNode;
use rowan::GreenNodeBuilder;

ast_node!(Sequence, SEQUENCE, "A YAML sequence (list)");

impl Sequence {
    /// Iterate over items in this sequence as raw syntax nodes.
    ///
    /// For most use cases prefer [`values`](Self::values) which returns
    /// [`YamlNode`](crate::as_yaml::YamlNode)s.
    pub(crate) fn items(&self) -> impl Iterator<Item = SyntaxNode> + '_ {
        self.0.children().filter_map(|child| {
            if child.kind() == SyntaxKind::SEQUENCE_ENTRY {
                // Look for the actual item within the SEQUENCE_ENTRY
                // Skip DASH and WHITESPACE tokens, find the actual value node
                child.children().find(|n| {
                    matches!(
                        n.kind(),
                        SyntaxKind::SCALAR
                            | SyntaxKind::MAPPING
                            | SyntaxKind::SEQUENCE
                            | SyntaxKind::ALIAS
                            | SyntaxKind::TAGGED_NODE
                    )
                })
            } else {
                None
            }
        })
    }

    /// Iterate over items in this sequence as [`YamlNode`](crate::as_yaml::YamlNode)s.
    ///
    /// Items that cannot be wrapped as a `YamlNode` are silently skipped.
    pub fn values(&self) -> impl Iterator<Item = crate::as_yaml::YamlNode> + '_ {
        self.items()
            .filter_map(crate::as_yaml::YamlNode::from_syntax)
    }

    /// Returns the number of items in this sequence.
    pub fn len(&self) -> usize {
        self.items().count()
    }

    /// Returns `true` if this sequence contains no items.
    pub fn is_empty(&self) -> bool {
        self.items().next().is_none()
    }

    /// Get the item at `index` as a [`YamlNode`](crate::as_yaml::YamlNode).
    ///
    /// Returns `None` if `index` is out of bounds.
    pub fn get(&self, index: usize) -> Option<crate::as_yaml::YamlNode> {
        self.items()
            .nth(index)
            .and_then(crate::as_yaml::YamlNode::from_syntax)
    }

    /// Get the first item in this sequence, or `None` if empty.
    pub fn first(&self) -> Option<crate::as_yaml::YamlNode> {
        self.get(0)
    }

    /// Get the last item in this sequence, or `None` if empty.
    pub fn last(&self) -> Option<crate::as_yaml::YamlNode> {
        let len = self.len();
        if len == 0 {
            None
        } else {
            self.get(len - 1)
        }
    }
}

impl Sequence {
    /// Add an item to the end of the sequence.
    ///
    /// Mutates in place despite `&self` (see crate docs on interior mutability).
    pub fn push(&self, value: impl crate::AsYaml) {
        // Detect the indentation by looking at existing SEQUENCE_ENTRY nodes
        let mut indentation = self
            .0
            .children_with_tokens()
            .find_map(|child| {
                child
                    .into_token()
                    .filter(|t| t.kind() == SyntaxKind::INDENT)
                    .map(|t| t.text().to_string())
            })
            .unwrap_or_else(|| "  ".to_string());

        // If no INDENT token found, look within SEQUENCE_ENTRY nodes
        if indentation == "  " {
            for child in self.0.children() {
                if child.kind() == SyntaxKind::SEQUENCE_ENTRY {
                    let tokens: Vec<_> = child.children_with_tokens().collect();
                    for (i, token) in tokens.iter().enumerate() {
                        if let Some(t) = token.as_token() {
                            if t.kind() == SyntaxKind::WHITESPACE && i + 1 < tokens.len() {
                                if let Some(next_t) = tokens[i + 1].as_token() {
                                    if next_t.kind() == SyntaxKind::DASH {
                                        indentation = t.text().to_string();
                                        break;
                                    }
                                }
                            }
                        }
                    }
                    if !indentation.is_empty() && indentation != "  " {
                        break;
                    }
                }
            }
        }

        // Build the INDENT token (separate from the SEQUENCE_ENTRY)
        let mut indent_builder = GreenNodeBuilder::new();
        indent_builder.start_node(SyntaxKind::ROOT.into());
        indent_builder.token(SyntaxKind::INDENT.into(), &indentation);
        indent_builder.finish_node();
        let indent_node = SyntaxNode::new_root_mut(indent_builder.finish());
        let indent_token = indent_node
            .first_token()
            .expect("builder always emits an INDENT token");

        // Collect children and analyze the sequence structure
        let children: Vec<_> = self.0.children_with_tokens().collect();

        // Find the last SEQUENCE_ENTRY and check if it has a trailing newline
        let mut last_entry_has_newline = true; // Default to true for empty sequences
        let mut last_entry_index = None;

        for (i, child) in children.iter().enumerate().rev() {
            if let Some(node) = child.as_node() {
                if node.kind() == SyntaxKind::SEQUENCE_ENTRY {
                    last_entry_has_newline = node
                        .last_token()
                        .map(|t| t.kind() == SyntaxKind::NEWLINE)
                        .unwrap_or(false);
                    last_entry_index = Some(i);
                    break;
                }
            }
        }

        // Find the insert position: after the last SEQUENCE_ENTRY and any immediately following
        // INDENT tokens, but BEFORE any trailing standalone NEWLINE tokens (which represent
        // blank lines that should stay between mapping entries, not inside the sequence)
        let mut insert_pos = children.len();
        if let Some(last_idx) = last_entry_index {
            // Start from after the last SEQUENCE_ENTRY
            insert_pos = last_idx + 1;

            // Skip any INDENT tokens immediately after
            while insert_pos < children.len() {
                if let Some(token) = children[insert_pos].as_token() {
                    if token.kind() == SyntaxKind::INDENT {
                        insert_pos += 1;
                    } else {
                        break;
                    }
                } else {
                    break;
                }
            }
            // Now insert_pos is right before any trailing standalone NEWLINE tokens
        }

        // Build the SEQUENCE_ENTRY node using AsYaml trait
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(SyntaxKind::SEQUENCE_ENTRY.into());
        builder.token(SyntaxKind::DASH.into(), "-");
        builder.token(SyntaxKind::WHITESPACE.into(), " ");

        // Build the value content directly using AsYaml
        let value_ends_with_newline = value.build_content(&mut builder, 0, false);

        // Add trailing newline only if the value doesn't already end with one
        // and if the last entry had one (preserves document style)
        if last_entry_has_newline && !value_ends_with_newline {
            builder.token(SyntaxKind::NEWLINE.into(), "\n");
        }
        builder.finish_node(); // SEQUENCE_ENTRY
        let new_entry = SyntaxNode::new_root_mut(builder.finish());

        // Ensure the previous last entry has a trailing newline (it won't be last anymore)
        if let Some(last_idx) = last_entry_index {
            if let Some(node) = children[last_idx].as_node() {
                if !node
                    .last_token()
                    .map(|t| t.kind() == SyntaxKind::NEWLINE)
                    .unwrap_or(false)
                {
                    let entry_children_count = node.children_with_tokens().count();
                    let mut nl_builder = GreenNodeBuilder::new();
                    nl_builder.start_node(SyntaxKind::ROOT.into());
                    nl_builder.token(SyntaxKind::NEWLINE.into(), "\n");
                    nl_builder.finish_node();
                    let nl_node = SyntaxNode::new_root_mut(nl_builder.finish());
                    if let Some(token) = nl_node.first_token() {
                        node.splice_children(
                            entry_children_count..entry_children_count,
                            vec![token.into()],
                        );
                    }
                }
            }
        }

        // Insert the indent token and new entry before any trailing blank newlines
        self.0.splice_children(
            insert_pos..insert_pos,
            vec![indent_token.into(), new_entry.into()],
        );
    }

    /// Insert an item at a specific position.
    ///
    /// If `index` is out of bounds, the item is appended at the end.
    /// This method always succeeds; it never returns an error.
    ///
    /// Mutates in place despite `&self` (see crate docs on interior mutability).
    pub fn insert(&self, index: usize, value: impl crate::AsYaml) {
        // Detect the indentation by looking at existing SEQUENCE_ENTRY nodes (same as push_str)
        let mut indentation = "  ".to_string();
        let all_children: Vec<_> = self.0.children_with_tokens().collect();

        for child in all_children.iter() {
            if let Some(token) = child.as_token() {
                if token.kind() == SyntaxKind::INDENT {
                    indentation = token.text().to_string();
                    break;
                }
            }
        }

        // If no INDENT token found, look within SEQUENCE_ENTRY nodes
        if indentation == "  " {
            for child in self.0.children() {
                if child.kind() == SyntaxKind::SEQUENCE_ENTRY {
                    let tokens: Vec<_> = child.children_with_tokens().collect();
                    for (i, token) in tokens.iter().enumerate() {
                        if let Some(t) = token.as_token() {
                            if t.kind() == SyntaxKind::WHITESPACE && i + 1 < tokens.len() {
                                if let Some(next_t) = tokens[i + 1].as_token() {
                                    if next_t.kind() == SyntaxKind::DASH {
                                        indentation = t.text().to_string();
                                        break;
                                    }
                                }
                            }
                        }
                    }
                    if !indentation.is_empty() && indentation != "  " {
                        break;
                    }
                }
            }
        }

        // Build a newline token
        let mut newline_builder = GreenNodeBuilder::new();
        newline_builder.start_node(SyntaxKind::ROOT.into());
        newline_builder.token(SyntaxKind::NEWLINE.into(), "\n");
        newline_builder.finish_node();
        let newline_node = SyntaxNode::new_root_mut(newline_builder.finish());
        let newline_token = newline_node
            .first_token()
            .expect("builder always emits a NEWLINE token");

        // Build the SEQUENCE_ENTRY node using AsYaml
        let mut builder = GreenNodeBuilder::new();
        builder.start_node(SyntaxKind::SEQUENCE_ENTRY.into());
        builder.token(SyntaxKind::WHITESPACE.into(), &indentation);
        builder.token(SyntaxKind::DASH.into(), "-");
        builder.token(SyntaxKind::WHITESPACE.into(), " ");

        // Build the value content directly using AsYaml
        value.build_content(&mut builder, 0, false);

        builder.finish_node(); // SEQUENCE_ENTRY
        let new_entry = SyntaxNode::new_root_mut(builder.finish());

        // Find the position to insert
        let children: Vec<_> = self.0.children_with_tokens().collect();
        let mut item_count = 0;
        let mut insert_pos = children.len();

        for (i, child) in children.iter().enumerate() {
            if let Some(node) = child.as_node() {
                if node.kind() == SyntaxKind::SEQUENCE_ENTRY {
                    if item_count == index {
                        insert_pos = i;
                        break;
                    }
                    item_count += 1;
                }
            }
        }

        // Insert newline and new entry at the position
        self.0.splice_children(
            insert_pos..insert_pos,
            vec![newline_token.into(), new_entry.into()],
        );
    }

    /// Replace the item at `index` with a new value.
    ///
    /// Returns `true` if the index was in bounds and the item was replaced,
    /// `false` if `index >= len()`.
    ///
    /// Mutates in place despite `&self` (see crate docs on interior mutability).
    pub fn set(&self, index: usize, value: impl crate::AsYaml) -> bool {
        let children: Vec<_> = self.0.children_with_tokens().collect();
        let mut item_count = 0;

        for (i, child) in children.iter().enumerate() {
            if let Some(node) = child.as_node() {
                if node.kind() == SyntaxKind::SEQUENCE_ENTRY {
                    if item_count == index {
                        // Build a new SEQUENCE_ENTRY with the new value using AsYaml
                        let entry_children: Vec<_> = node.children_with_tokens().collect();
                        let mut builder = GreenNodeBuilder::new();
                        builder.start_node(SyntaxKind::SEQUENCE_ENTRY.into());

                        let mut value_inserted = false;
                        let mut trailing_text: Option<String> = None;

                        for entry_child in entry_children {
                            match &entry_child {
                                rowan::NodeOrToken::Node(n)
                                    if matches!(
                                        n.kind(),
                                        SyntaxKind::SCALAR
                                            | SyntaxKind::MAPPING
                                            | SyntaxKind::SEQUENCE
                                            | SyntaxKind::TAGGED_NODE
                                    ) =>
                                {
                                    // Extract trailing whitespace from the old value node
                                    // The old value may contain trailing NEWLINE+INDENT that we need to preserve
                                    let text = n.text().to_string();
                                    // Find the trailing whitespace (newline + indent pattern)
                                    if let Some(last_newline_pos) = text.rfind('\n') {
                                        trailing_text = Some(text[last_newline_pos..].to_string());
                                    }

                                    // Replace the value node with the new value built from AsYaml
                                    if !value_inserted {
                                        value.build_content(&mut builder, 0, false);
                                        value_inserted = true;
                                    }
                                }
                                rowan::NodeOrToken::Node(n) => {
                                    // Copy other nodes as-is (like VALUE wrappers, etc.)
                                    crate::yaml::copy_node_to_builder(&mut builder, n);
                                }
                                rowan::NodeOrToken::Token(t) => {
                                    // Copy tokens as-is
                                    builder.token(t.kind().into(), t.text());
                                }
                            }
                        }

                        // Add the trailing whitespace that was extracted from the old value
                        if let Some(trailing) = trailing_text {
                            // Parse the trailing text to add appropriate tokens
                            if trailing.starts_with('\n') {
                                builder.token(SyntaxKind::NEWLINE.into(), "\n");
                                let indent_part = &trailing[1..];
                                if !indent_part.is_empty() {
                                    builder.token(SyntaxKind::INDENT.into(), indent_part);
                                }
                            }
                        }

                        builder.finish_node();
                        let new_entry = SyntaxNode::new_root_mut(builder.finish());

                        // Replace the old SEQUENCE_ENTRY with the new one
                        self.0.splice_children(i..i + 1, vec![new_entry.into()]);
                        return true;
                    }
                    item_count += 1;
                }
            }
        }
        false
    }

    /// Remove the item at `index`, returning its value.
    ///
    /// Returns `Some(value)` if the index was in bounds, `None` otherwise.
    ///
    /// Mutates in place despite `&self` (see crate docs on interior mutability).
    pub fn remove(&self, index: usize) -> Option<crate::as_yaml::YamlNode> {
        // Capture the value before removing so we can return it
        let removed_value = self.get(index);

        // Use children_with_tokens() since splice_children() expects those indices
        let children: Vec<_> = self.0.children_with_tokens().collect();

        // Find the SEQUENCE_ENTRY at the given index
        let mut item_count = 0;
        for (i, child) in children.iter().enumerate() {
            if let Some(node) = child.as_node() {
                if node.kind() == SyntaxKind::SEQUENCE_ENTRY {
                    if item_count == index {
                        // Check if this is the last SEQUENCE_ENTRY
                        let is_last = !children.iter().skip(i + 1).any(|c| {
                            c.as_node()
                                .is_some_and(|n| n.kind() == SyntaxKind::SEQUENCE_ENTRY)
                        });

                        // Remove the entry
                        self.0.splice_children(i..(i + 1), vec![]);

                        if !self.is_flow_style() && is_last && i > 0 {
                            // Removed the last entry - remove trailing newline/indent from new last entry
                            // Find the previous SEQUENCE_ENTRY
                            if let Some(prev_entry_node) =
                                children[..i].iter().rev().find_map(|c| {
                                    c.as_node()
                                        .filter(|n| n.kind() == SyntaxKind::SEQUENCE_ENTRY)
                                })
                            {
                                // Remove trailing NEWLINE and INDENT tokens
                                let entry_children: Vec<_> =
                                    prev_entry_node.children_with_tokens().collect();
                                let mut remove_count = 0;

                                // Count trailing NEWLINE, INDENT, and WHITESPACE tokens from the end
                                for child in entry_children.iter().rev() {
                                    if let Some(token) = child.as_token() {
                                        if matches!(
                                            token.kind(),
                                            SyntaxKind::NEWLINE
                                                | SyntaxKind::INDENT
                                                | SyntaxKind::WHITESPACE
                                        ) {
                                            remove_count += 1;
                                        } else {
                                            break;
                                        }
                                    } else {
                                        break;
                                    }
                                }

                                if remove_count > 0 {
                                    let total = entry_children.len();
                                    prev_entry_node
                                        .splice_children((total - remove_count)..total, vec![]);
                                }
                            }
                        }
                        return removed_value;
                    }
                    item_count += 1;
                }
            }
        }
        None
    }

    /// Check if this sequence is in flow style [item1, item2]
    pub fn is_flow_style(&self) -> bool {
        self.0.children_with_tokens().any(|child| {
            child
                .as_token()
                .is_some_and(|t| t.kind() == SyntaxKind::LEFT_BRACKET)
        })
    }

    /// Get the raw syntax node for a specific index (for advanced use).
    ///
    /// Returns the raw CST node without decoding it to a value.
    /// For most use cases prefer [`get`](Self::get), which returns a [`YamlNode`](crate::YamlNode).
    #[allow(dead_code)] // Used in tests
    pub(crate) fn get_node(&self, index: usize) -> Option<SyntaxNode> {
        self.items().nth(index)
    }

    /// Remove and return the last item in this sequence.
    ///
    /// Returns `None` if the sequence is empty.
    ///
    /// Mutates in place despite `&self` (see crate docs on interior mutability).
    pub fn pop(&self) -> Option<crate::as_yaml::YamlNode> {
        let len = self.len();
        if len == 0 {
            return None;
        }
        let removed = self.remove(len - 1);

        debug_assert_eq!(
            self.len(),
            len - 1,
            "pop() invariant: remove() did not reduce length"
        );

        removed
    }

    /// Remove all items from this sequence.
    ///
    /// Mutates in place despite `&self` (see crate docs on interior mutability).
    pub fn clear(&self) {
        // Remove items from the beginning to avoid recalculating indices
        // Use a safety counter to prevent infinite loops
        let initial_len = self.len();
        for _ in 0..initial_len {
            let current_len = self.len();
            if current_len == 0 {
                break;
            }
            // Always remove the first item
            let removed = self.remove(0);
            debug_assert!(
                removed.is_some(),
                "clear() invariant: remove(0) returned None"
            );
            debug_assert_eq!(
                self.len(),
                current_len - 1,
                "clear() invariant: remove(0) did not reduce length"
            );
        }
    }

    /// Get the byte offset range of this sequence in the source text.
    ///
    /// Returns the start and end byte offsets as a `TextPosition`.
    pub fn byte_range(&self) -> crate::TextPosition {
        self.0.text_range().into()
    }

    /// Get the line and column where this sequence starts.
    ///
    /// Requires the original source text to calculate line/column from byte offsets.
    /// Line and column numbers are 1-indexed.
    ///
    /// # Arguments
    ///
    /// * `source_text` - The original YAML source text
    pub fn start_position(&self, source_text: &str) -> crate::LineColumn {
        let range = self.byte_range();
        crate::byte_offset_to_line_column(source_text, range.start as usize)
    }

    /// Get the line and column where this sequence ends.
    ///
    /// Requires the original source text to calculate line/column from byte offsets.
    /// Line and column numbers are 1-indexed.
    ///
    /// # Arguments
    ///
    /// * `source_text` - The original YAML source text
    pub fn end_position(&self, source_text: &str) -> crate::LineColumn {
        let range = self.byte_range();
        crate::byte_offset_to_line_column(source_text, range.end as usize)
    }
}

// Iterator trait implementations for Sequence

impl<'a> IntoIterator for &'a Sequence {
    type Item = crate::as_yaml::YamlNode;
    type IntoIter = Box<dyn Iterator<Item = crate::as_yaml::YamlNode> + 'a>;

    fn into_iter(self) -> Self::IntoIter {
        Box::new(self.values())
    }
}

impl AsYaml for Sequence {
    fn as_node(&self) -> Option<&SyntaxNode> {
        Some(&self.0)
    }

    fn kind(&self) -> YamlKind {
        YamlKind::Sequence
    }

    fn build_content(
        &self,
        builder: &mut rowan::GreenNodeBuilder,
        indent: usize,
        _flow_context: bool,
    ) -> bool {
        builder.start_node(SyntaxKind::SEQUENCE.into());
        crate::as_yaml::copy_node_content_with_indent(builder, &self.0, indent);
        builder.finish_node();
        self.0
            .last_token()
            .map(|t| t.kind() == SyntaxKind::NEWLINE)
            .unwrap_or(false)
    }

    fn is_inline(&self) -> bool {
        ValueNode::is_inline(self)
    }
}
#[cfg(test)]
mod tests {
    use crate::yaml::YamlFile;
    use std::str::FromStr;

    #[test]
    fn test_sequence_items_tagged_node() {
        // Tagged scalars inside sequences were previously skipped by items() because
        // TAGGED_NODE was not listed in the kind filter.
        let yaml = "- !custom foo\n- !custom bar\n- plain\n";
        let parsed = YamlFile::from_str(yaml).unwrap();

        let doc = parsed.document().unwrap();
        let seq = doc.as_sequence().unwrap();
        assert_eq!(
            seq.items().count(),
            3,
            "Tagged scalars should be included in items()"
        );
        // values() should also return tagged scalars (cast as Scalar YamlValues)
        assert_eq!(
            seq.values().count(),
            3,
            "Tagged scalars should be included in values()"
        );
    }

    #[test]
    fn test_sequence_set_tagged_node() {
        // Sequence::set() was missing TAGGED_NODE from its kind filter, so
        // replacing a tagged-scalar item would leave the original tag+value in place
        // and insert the new value alongside it.
        let yaml = "- !custom foo\n- bar\n";
        let parsed = YamlFile::from_str(yaml).unwrap();
        let doc = parsed.document().unwrap();
        let seq = doc.as_sequence().unwrap();

        seq.set(0, "replaced");

        let values: Vec<_> = seq.values().collect();
        assert_eq!(values.len(), 2);
        assert_eq!(
            values[0].as_scalar().map(|s| s.as_string()),
            Some("replaced".to_string())
        );
        assert_eq!(
            values[1].as_scalar().map(|s| s.as_string()),
            Some("bar".to_string())
        );
    }

    #[test]
    fn test_sequence_operations() {
        let yaml = "- item1\n- item2";
        let parsed = YamlFile::from_str(yaml).unwrap();

        let doc = parsed.document().expect("expected a document");
        let seq = doc.as_sequence().expect("expected a sequence");

        // Test push
        seq.push("item3");
        let values: Vec<_> = seq.values().collect();
        assert_eq!(values.len(), 3);
        assert_eq!(
            values[2].as_scalar().map(|s| s.as_string()),
            Some("item3".to_string())
        );

        // Test insert
        seq.insert(0, "item0");
        let values: Vec<_> = seq.values().collect();
        assert_eq!(values.len(), 4);
        assert_eq!(
            values[0].as_scalar().map(|s| s.as_string()),
            Some("item0".to_string())
        );
    }

    // Iterator tests

    #[test]
    fn test_sequence_into_iterator() {
        use crate::Document;
        let text = "items:\n  - apple\n  - banana\n  - cherry";
        let doc = Document::from_str(text).unwrap();
        let mapping = doc.as_mapping().unwrap();
        let sequence = mapping.get_sequence("items").unwrap();

        // Test that we can use for loops directly
        let mut items = Vec::new();
        for value in &sequence {
            if let Some(scalar) = value.as_scalar() {
                items.push(scalar.to_string());
            }
        }

        assert_eq!(items.len(), 3);
        assert_eq!(items[0], "apple");
        assert_eq!(items[1], "banana");
        assert_eq!(items[2], "cherry");
    }

    #[test]
    fn test_sequence_into_iterator_count() {
        use crate::Document;
        let text = "[1, 2, 3, 4, 5]";
        let doc = Document::from_str(text).unwrap();
        let sequence = doc.as_sequence().unwrap();

        let count = (&sequence).into_iter().count();
        assert_eq!(count, 5);
    }

    #[test]
    fn test_sequence_iterator_map() {
        use crate::Document;
        let text = "numbers: [1, 2, 3]";
        let doc = Document::from_str(text).unwrap();
        let mapping = doc.as_mapping().unwrap();
        let sequence = mapping.get_sequence("numbers").unwrap();

        // Map to strings
        let strings: Vec<_> = (&sequence)
            .into_iter()
            .filter_map(|v| v.as_scalar().map(|s| s.to_string()))
            .collect();

        assert_eq!(strings, vec!["1", "2", "3"]);
    }

    #[test]
    fn test_empty_sequence_iterator() {
        use crate::Document;
        let text = "items: []";
        let doc = Document::from_str(text).unwrap();
        let mapping = doc.as_mapping().unwrap();
        let sequence = mapping.get_sequence("items").unwrap();

        let count = (&sequence).into_iter().count();
        assert_eq!(count, 0);
    }

    // Tests from sequence_operations_test.rs

    #[test]
    fn test_sequence_push_single() {
        use crate::Document;
        let original = r#"team:
  - Alice
  - Bob"#;

        let doc = Document::from_str(original).unwrap();
        let mapping = doc.as_mapping().unwrap();
        let team = mapping.get_sequence("team").unwrap();
        team.push("Charlie");

        let expected = r#"team:
  - Alice
  - Bob
  - Charlie"#;
        assert_eq!(doc.to_string(), expected);
    }

    #[test]
    fn test_sequence_push_multiple() {
        use crate::Document;
        let original = r#"team:
  - Alice
  - Bob"#;

        let doc = Document::from_str(original).unwrap();
        let mapping = doc.as_mapping().unwrap();
        let team = mapping.get_sequence("team").unwrap();
        team.push("Charlie");
        team.push("Diana");

        let expected = r#"team:
  - Alice
  - Bob
  - Charlie
  - Diana"#;
        assert_eq!(doc.to_string(), expected);
    }

    #[test]
    fn test_sequence_set_item() {
        use crate::Document;
        let original = r#"team:
  - Alice
  - Bob
  - Charlie"#;

        let doc = Document::from_str(original).unwrap();
        let mapping = doc.as_mapping().unwrap();
        let team = mapping.get_sequence("team").unwrap();
        team.set(1, "Robert");

        let expected = r#"team:
  - Alice
  - Robert
  - Charlie"#;
        assert_eq!(doc.to_string(), expected);
    }

    #[test]
    fn test_multiple_sequences() {
        use crate::Document;
        let original = r#"team:
  - Alice
  - Bob

scores:
  - 95
  - 87"#;

        let doc = Document::from_str(original).unwrap();
        let mapping = doc.as_mapping().unwrap();
        let team = mapping.get_sequence("team").unwrap();
        team.push("Charlie");
        let scores = mapping.get_sequence("scores").unwrap();
        scores.push(92);
        scores.set(0, 100);

        let expected = r#"team:
  - Alice
  - Bob
  - Charlie

scores:
  - 100
  - 87
  - 92"#;
        assert_eq!(doc.to_string(), expected);
    }

    #[test]
    fn test_nested_structure_with_sequences() {
        use crate::Document;
        let original = r#"config:
  enabled: true
  retries: 3
  servers:
    - host1
    - host2"#;

        let doc = Document::from_str(original).unwrap();
        let mapping = doc.as_mapping().unwrap();
        let config = mapping.get_mapping("config").unwrap();
        config.set("enabled", false);
        config.set("retries", 5);

        let servers = config.get_sequence("servers").unwrap();
        servers.push("host3");
        servers.set(0, "primary-host");

        let expected = r#"config:
  enabled: false
  retries: 5
  servers:
    - primary-host
    - host2
    - host3"#;
        assert_eq!(doc.to_string(), expected);
    }

    #[test]
    fn test_sequence_len_and_is_empty() {
        use crate::Document;
        let doc = Document::from_str("items:\n  - a\n  - b\n  - c").unwrap();
        let mapping = doc.as_mapping().unwrap();
        let seq = mapping.get_sequence("items").unwrap();

        assert_eq!(seq.len(), 3);
        assert!(!seq.is_empty());

        let empty_doc = Document::from_str("items: []").unwrap();
        let empty_mapping = empty_doc.as_mapping().unwrap();
        let empty_seq = empty_mapping.get_sequence("items").unwrap();

        assert_eq!(empty_seq.len(), 0);
        assert!(empty_seq.is_empty());
    }

    #[test]
    fn test_sequence_get() {
        use crate::Document;
        let doc = Document::from_str("items:\n  - first\n  - second\n  - third").unwrap();
        let mapping = doc.as_mapping().unwrap();
        let seq = mapping.get_sequence("items").unwrap();

        assert_eq!(seq.get(0).unwrap().to_string(), "first");
        assert_eq!(seq.get(1).unwrap().to_string(), "second");
        assert_eq!(seq.get(2).unwrap().to_string(), "third");
        assert!(seq.get(3).is_none());
    }

    #[test]
    fn test_sequence_first_and_last() {
        use crate::Document;
        let doc = Document::from_str("items:\n  - first\n  - middle\n  - last").unwrap();
        let mapping = doc.as_mapping().unwrap();
        let seq = mapping.get_sequence("items").unwrap();

        assert_eq!(seq.first().unwrap().to_string(), "first");
        assert_eq!(seq.last().unwrap().to_string(), "last");

        let empty_doc = Document::from_str("items: []").unwrap();
        let empty_mapping = empty_doc.as_mapping().unwrap();
        let empty_seq = empty_mapping.get_sequence("items").unwrap();

        assert!(empty_seq.first().is_none());
        assert!(empty_seq.last().is_none());
    }

    #[test]
    fn test_sequence_values_iterator() {
        use crate::Document;
        let doc = Document::from_str("items:\n  - a\n  - b\n  - c").unwrap();
        let mapping = doc.as_mapping().unwrap();
        let seq = mapping.get_sequence("items").unwrap();

        let values: Vec<String> = seq.values().map(|v| v.to_string()).collect();
        assert_eq!(values, vec!["a", "b", "c"]);
    }

    #[test]
    fn test_sequence_pop() {
        use crate::Document;
        let doc = Document::from_str("items:\n  - a\n  - b\n  - c").unwrap();
        let mapping = doc.as_mapping().unwrap();
        let seq = mapping.get_sequence("items").unwrap();

        assert_eq!(seq.len(), 3);
        let popped = seq.pop().unwrap();
        assert_eq!(popped.to_string(), "c");
        assert_eq!(seq.len(), 2);

        let popped = seq.pop().unwrap();
        assert_eq!(popped.to_string(), "b");
        assert_eq!(seq.len(), 1);

        let expected = "items:\n  - a";
        assert_eq!(doc.to_string().trim_end(), expected);

        let popped = seq.pop().unwrap();
        assert_eq!(popped.to_string(), "a");
        assert_eq!(seq.len(), 0);
        assert!(seq.pop().is_none());
    }

    #[test]
    fn test_sequence_clear() {
        use crate::Document;
        let doc = Document::from_str("items:\n  - a\n  - b\n  - c").unwrap();
        let mapping = doc.as_mapping().unwrap();
        let seq = mapping.get_sequence("items").unwrap();

        assert_eq!(seq.len(), 3);
        seq.clear();
        assert_eq!(seq.len(), 0);
        assert!(seq.is_empty());
    }

    #[test]
    fn test_sequence_get_with_nested_values() {
        use crate::Document;
        let doc = Document::from_str(
            r#"items:
  - simple
  - {key: value}
  - [nested, list]"#,
        )
        .unwrap();
        let mapping = doc.as_mapping().unwrap();
        let seq = mapping.get_sequence("items").unwrap();

        assert_eq!(seq.len(), 3);
        assert!(seq.get(0).unwrap().is_scalar());
        assert!(seq.get(1).unwrap().is_mapping());
        assert!(seq.get(2).unwrap().is_sequence());
    }

    #[test]
    fn test_flow_sequence_len_and_is_empty() {
        use crate::Document;
        let doc = Document::from_str("items: [a, b, c]").unwrap();
        let mapping = doc.as_mapping().unwrap();
        let seq = mapping.get_sequence("items").unwrap();

        assert_eq!(seq.len(), 3);
        assert!(!seq.is_empty());

        let empty_doc = Document::from_str("items: []").unwrap();
        let empty_mapping = empty_doc.as_mapping().unwrap();
        let empty_seq = empty_mapping.get_sequence("items").unwrap();

        assert_eq!(empty_seq.len(), 0);
        assert!(empty_seq.is_empty());
    }

    #[test]
    fn test_flow_sequence_get() {
        use crate::Document;
        let doc = Document::from_str("items: [first, second, third]").unwrap();
        let mapping = doc.as_mapping().unwrap();
        let seq = mapping.get_sequence("items").unwrap();

        assert_eq!(seq.get(0).unwrap().to_string(), "first");
        assert_eq!(seq.get(1).unwrap().to_string(), "second");
        assert_eq!(seq.get(2).unwrap().to_string(), "third");
        assert!(seq.get(3).is_none());
    }

    #[test]
    fn test_flow_sequence_first_and_last() {
        use crate::Document;
        let doc = Document::from_str("items: [first, middle, last]").unwrap();
        let mapping = doc.as_mapping().unwrap();
        let seq = mapping.get_sequence("items").unwrap();

        assert_eq!(seq.first().unwrap().to_string(), "first");
        assert_eq!(seq.last().unwrap().to_string(), "last");
    }

    #[test]
    fn test_flow_sequence_values_iterator() {
        use crate::Document;
        let doc = Document::from_str("items: [a, b, c]").unwrap();
        let mapping = doc.as_mapping().unwrap();
        let seq = mapping.get_sequence("items").unwrap();

        let values: Vec<String> = seq.values().map(|v| v.to_string()).collect();
        assert_eq!(values, vec!["a", "b", "c"]);
    }

    #[test]
    fn test_flow_sequence_remove_middle() {
        use crate::Document;
        let doc = Document::from_str("items: [a, b, c]").unwrap();
        let mapping = doc.as_mapping().unwrap();
        let seq = mapping.get_sequence("items").unwrap();

        assert_eq!(seq.len(), 3);
        let removed = seq.remove(1);
        assert_eq!(removed.map(|v| v.to_string()), Some("b".to_string()));
        assert_eq!(seq.len(), 2);

        let values: Vec<String> = seq.values().map(|v| v.to_string()).collect();
        assert_eq!(values, vec!["a", "c"]);
    }

    #[test]
    fn test_flow_sequence_remove_first() {
        use crate::Document;
        let doc = Document::from_str("items: [a, b, c]").unwrap();
        let mapping = doc.as_mapping().unwrap();
        let seq = mapping.get_sequence("items").unwrap();

        assert_eq!(seq.len(), 3);
        let removed = seq.remove(0);
        assert_eq!(removed.map(|v| v.to_string()), Some("a".to_string()));
        assert_eq!(seq.len(), 2);

        let values: Vec<String> = seq.values().map(|v| v.to_string()).collect();
        assert_eq!(values, vec!["b", "c"]);
    }

    #[test]
    fn test_flow_sequence_remove_last() {
        use crate::Document;
        let doc = Document::from_str("items: [a, b, c]").unwrap();
        let mapping = doc.as_mapping().unwrap();
        let seq = mapping.get_sequence("items").unwrap();

        assert_eq!(seq.len(), 3);
        let removed = seq.remove(2);
        assert_eq!(removed.map(|v| v.to_string()), Some("c".to_string()));
        assert_eq!(seq.len(), 2);

        let values: Vec<String> = seq.values().map(|v| v.to_string()).collect();
        assert_eq!(values, vec!["a", "b"]);
    }

    #[test]
    fn test_flow_sequence_pop() {
        use crate::Document;
        let doc = Document::from_str("items: [a, b, c]").unwrap();
        let mapping = doc.as_mapping().unwrap();
        let seq = mapping.get_sequence("items").unwrap();

        assert_eq!(seq.len(), 3);
        let popped = seq.pop().unwrap();
        assert_eq!(popped.to_string(), "c");
        assert_eq!(seq.len(), 2);

        let popped = seq.pop().unwrap();
        assert_eq!(popped.to_string(), "b");
        assert_eq!(seq.len(), 1);

        let popped = seq.pop().unwrap();
        assert_eq!(popped.to_string(), "a");
        assert_eq!(seq.len(), 0);
        assert!(seq.pop().is_none());
    }

    #[test]
    fn test_flow_sequence_clear() {
        use crate::Document;
        let doc = Document::from_str("items: [a, b, c]").unwrap();
        let mapping = doc.as_mapping().unwrap();
        let seq = mapping.get_sequence("items").unwrap();

        assert_eq!(seq.len(), 3);
        seq.clear();
        assert_eq!(seq.len(), 0);
        assert!(seq.is_empty());
    }

    #[test]
    fn test_flow_sequence_with_whitespace() {
        use crate::Document;
        let doc = Document::from_str("items: [ a , b , c ]").unwrap();
        let mapping = doc.as_mapping().unwrap();
        let seq = mapping.get_sequence("items").unwrap();

        assert_eq!(seq.len(), 3);
        let values: Vec<String> = seq.values().map(|v| v.to_string()).collect();
        assert_eq!(values, vec!["a", "b", "c"]);
    }

    #[test]
    fn test_block_sequence_remove_middle() {
        use crate::Document;
        let doc = Document::from_str("items:\n  - a\n  - b\n  - c").unwrap();
        let mapping = doc.as_mapping().unwrap();
        let seq = mapping.get_sequence("items").unwrap();

        assert_eq!(seq.len(), 3);
        let removed = seq.remove(1);
        assert_eq!(removed.map(|v| v.to_string()), Some("b".to_string()));
        assert_eq!(seq.len(), 2);

        let values: Vec<String> = seq.values().map(|v| v.to_string()).collect();
        assert_eq!(values, vec!["a", "c"]);
    }

    #[test]
    fn test_block_sequence_remove_first() {
        use crate::Document;
        let doc = Document::from_str("items:\n  - a\n  - b\n  - c").unwrap();
        let mapping = doc.as_mapping().unwrap();
        let seq = mapping.get_sequence("items").unwrap();

        assert_eq!(seq.len(), 3);
        let removed = seq.remove(0);
        assert_eq!(removed.map(|v| v.to_string()), Some("a".to_string()));
        assert_eq!(seq.len(), 2);

        let values: Vec<String> = seq.values().map(|v| v.to_string()).collect();
        assert_eq!(values, vec!["b", "c"]);
    }

    #[test]
    fn test_block_sequence_remove_last() {
        use crate::Document;
        let doc = Document::from_str("items:\n  - a\n  - b\n  - c").unwrap();
        let mapping = doc.as_mapping().unwrap();
        let seq = mapping.get_sequence("items").unwrap();

        assert_eq!(seq.len(), 3);
        let removed = seq.remove(2);
        assert_eq!(removed.map(|v| v.to_string()), Some("c".to_string()));
        assert_eq!(seq.len(), 2);

        let values: Vec<String> = seq.values().map(|v| v.to_string()).collect();
        assert_eq!(values, vec!["a", "b"]);
    }

    #[test]
    fn test_single_item_block_sequence_remove() {
        use crate::Document;
        let doc = Document::from_str("items:\n  - only").unwrap();
        let mapping = doc.as_mapping().unwrap();
        let seq = mapping.get_sequence("items").unwrap();

        assert_eq!(seq.len(), 1);
        let removed = seq.remove(0);
        assert_eq!(removed.map(|v| v.to_string()), Some("only".to_string()));
        assert_eq!(seq.len(), 0);
    }

    #[test]
    fn test_single_item_flow_sequence_remove() {
        use crate::Document;
        let doc = Document::from_str("items: [only]").unwrap();
        let mapping = doc.as_mapping().unwrap();
        let seq = mapping.get_sequence("items").unwrap();

        assert_eq!(seq.len(), 1);
        let removed = seq.remove(0);
        assert_eq!(removed.map(|v| v.to_string()), Some("only".to_string()));
        assert_eq!(seq.len(), 0);
    }

    #[test]
    fn test_flow_sequence_push() {
        use crate::Document;
        let doc = Document::from_str("items: [a, b]").unwrap();
        let mapping = doc.as_mapping().unwrap();
        let seq = mapping.get_sequence("items").unwrap();

        assert_eq!(seq.len(), 2);
        seq.push("c");
        assert_eq!(seq.len(), 3);

        let values: Vec<String> = seq.values().map(|v| v.to_string()).collect();
        assert_eq!(values, vec!["a", "b", "c"]);
    }

    #[test]
    fn test_flow_sequence_push_multiple() {
        use crate::Document;
        let doc = Document::from_str("items: [a]").unwrap();
        let mapping = doc.as_mapping().unwrap();
        let seq = mapping.get_sequence("items").unwrap();

        seq.push("b");
        seq.push("c");
        seq.push("d");

        let values: Vec<String> = seq.values().map(|v| v.to_string()).collect();
        assert_eq!(values, vec!["a", "b", "c", "d"]);
    }

    #[test]
    fn test_flow_sequence_set_item() {
        use crate::Document;
        let doc = Document::from_str("items: [a, b, c]").unwrap();
        let mapping = doc.as_mapping().unwrap();
        let seq = mapping.get_sequence("items").unwrap();

        seq.set(1, "modified");

        let values: Vec<String> = seq.values().map(|v| v.to_string()).collect();
        assert_eq!(values, vec!["a", "modified", "c"]);
    }

    #[test]
    fn test_flow_sequence_insert_beginning() {
        use crate::Document;
        let doc = Document::from_str("items: [b, c]").unwrap();
        let mapping = doc.as_mapping().unwrap();
        let seq = mapping.get_sequence("items").unwrap();

        seq.insert(0, "a");

        let values: Vec<String> = seq.values().map(|v| v.to_string()).collect();
        assert_eq!(values, vec!["a", "b", "c"]);
    }

    #[test]
    fn test_flow_sequence_insert_middle() {
        use crate::Document;
        let doc = Document::from_str("items: [a, c]").unwrap();
        let mapping = doc.as_mapping().unwrap();
        let seq = mapping.get_sequence("items").unwrap();

        seq.insert(1, "b");

        let values: Vec<String> = seq.values().map(|v| v.to_string()).collect();
        assert_eq!(values, vec!["a", "b", "c"]);
    }

    #[test]
    fn test_flow_sequence_insert_end() {
        use crate::Document;
        let doc = Document::from_str("items: [a, b]").unwrap();
        let mapping = doc.as_mapping().unwrap();
        let seq = mapping.get_sequence("items").unwrap();

        seq.insert(2, "c");

        let values: Vec<String> = seq.values().map(|v| v.to_string()).collect();
        assert_eq!(values, vec!["a", "b", "c"]);
    }

    #[test]
    fn test_block_sequence_push() {
        use crate::Document;
        let doc = Document::from_str("items:\n  - a\n  - b").unwrap();
        let mapping = doc.as_mapping().unwrap();
        let seq = mapping.get_sequence("items").unwrap();

        assert_eq!(seq.len(), 2);
        seq.push("c");
        assert_eq!(seq.len(), 3);

        let values: Vec<String> = seq.values().map(|v| v.to_string()).collect();
        assert_eq!(values, vec!["a", "b", "c"]);
    }

    #[test]
    fn test_block_sequence_set_item() {
        use crate::Document;
        let doc = Document::from_str("items:\n  - a\n  - b\n  - c").unwrap();
        let mapping = doc.as_mapping().unwrap();
        let seq = mapping.get_sequence("items").unwrap();

        seq.set(1, "modified");

        let values: Vec<String> = seq.values().map(|v| v.to_string()).collect();
        assert_eq!(values, vec!["a", "modified", "c"]);
    }

    #[test]
    fn test_block_sequence_insert_beginning() {
        use crate::Document;
        let doc = Document::from_str("items:\n  - b\n  - c").unwrap();
        let mapping = doc.as_mapping().unwrap();
        let seq = mapping.get_sequence("items").unwrap();

        seq.insert(0, "a");

        let values: Vec<String> = seq.values().map(|v| v.to_string()).collect();
        assert_eq!(values, vec!["a", "b", "c"]);
    }

    #[test]
    fn test_block_sequence_insert_middle() {
        use crate::Document;
        let doc = Document::from_str("items:\n  - a\n  - c").unwrap();
        let mapping = doc.as_mapping().unwrap();
        let seq = mapping.get_sequence("items").unwrap();

        seq.insert(1, "b");

        let values: Vec<String> = seq.values().map(|v| v.to_string()).collect();
        assert_eq!(values, vec!["a", "b", "c"]);
    }

    #[test]
    fn test_block_sequence_insert_end() {
        use crate::Document;
        let doc = Document::from_str("items:\n  - a\n  - b").unwrap();
        let mapping = doc.as_mapping().unwrap();
        let seq = mapping.get_sequence("items").unwrap();

        seq.insert(2, "c");

        let values: Vec<String> = seq.values().map(|v| v.to_string()).collect();
        assert_eq!(values, vec!["a", "b", "c"]);
    }

    #[test]
    fn test_sequence_get_node() {
        let doc = YamlFile::from_str("items:\n  - alpha\n  - beta\n  - gamma")
            .unwrap()
            .document()
            .unwrap();
        let seq = doc.as_mapping().unwrap().get_sequence("items").unwrap();

        assert_eq!(seq.len(), 3);
        assert!(seq.get(0).is_some());
        assert!(seq.get(1).is_some());
        assert!(seq.get(2).is_some());
        assert!(seq.get(3).is_none());

        assert_eq!(
            seq.get(0).unwrap().as_scalar().unwrap().as_string(),
            "alpha"
        );
        assert_eq!(seq.get(1).unwrap().as_scalar().unwrap().as_string(), "beta");
        assert_eq!(
            seq.get(2).unwrap().as_scalar().unwrap().as_string(),
            "gamma"
        );
    }
}
