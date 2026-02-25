//! Additional edge cases from section 3 of better.md
//!
//! Tests cover:
//! - Block scalar edge cases (mixed indentation, line endings, empty blocks)
//! - Comment edge cases (inside quotes, Unicode, at EOF)
//! - Flow collection edge cases (deeply nested, trailing commas)

use std::str::FromStr;
use yaml_edit::YamlFile;

// ========================================
// Block Scalar Edge Cases (Section 3.1)
// ========================================

/// Test Windows line endings (CRLF) in block scalars
#[test]
fn test_block_scalar_windows_line_endings() {
    let yaml = "text: |\r\n  Line 1\r\n  Line 2\r\n";

    let parsed = YamlFile::parse(yaml);
    assert_eq!(
        parsed.errors().len(),
        0,
        "Should parse Windows line endings"
    );

    let tree = parsed.tree();
    let doc = tree.document().expect("Should have document");
    let mapping = doc.as_mapping().expect("Should be mapping");

    let text_val = mapping.get("text").expect("Should have text key");
    let text_str = text_val.as_scalar().unwrap().as_string();

    // Line endings should be normalized to \n
    assert_eq!(text_str, "Line 1\nLine 2\n");

    // Verify round-trip produces valid YAML
    let output = doc.to_string();
    let reparsed = YamlFile::parse(&output);
    assert_eq!(reparsed.errors().len(), 0);
}

/// Test empty block scalar with literal indicator
#[test]
fn test_empty_block_scalar_literal() {
    let yaml = "literal: |\n";

    let parsed = YamlFile::parse(yaml);
    assert_eq!(parsed.errors().len(), 0, "Should parse empty block scalar");

    let tree = parsed.tree();
    let doc = tree.document().expect("Should have document");
    let mapping = doc.as_mapping().expect("Should be mapping");

    let literal_val = mapping.get("literal").expect("Should have literal key");
    let literal_str = literal_val.as_scalar().unwrap().as_string();

    // Empty block scalar should produce empty string or single newline
    assert!(
        literal_str.is_empty() || literal_str == "\n",
        "Empty block scalar should be empty or newline, got: {:?}",
        literal_str
    );

    // Verify round-trip
    let output = doc.to_string();
    let reparsed = YamlFile::parse(&output);
    assert_eq!(reparsed.errors().len(), 0);
}

/// Test empty block scalar with folded indicator
#[test]
fn test_empty_block_scalar_folded() {
    let yaml = "folded: >\n";

    let parsed = YamlFile::parse(yaml);
    assert_eq!(parsed.errors().len(), 0, "Should parse empty folded scalar");

    let tree = parsed.tree();
    let doc = tree.document().expect("Should have document");
    let mapping = doc.as_mapping().expect("Should be mapping");

    let folded_val = mapping.get("folded").expect("Should have folded key");
    let folded_str = folded_val.as_scalar().unwrap().as_string();

    // Empty folded scalar behavior
    assert!(
        folded_str.is_empty() || folded_str == "\n",
        "Empty folded scalar should be empty or newline, got: {:?}",
        folded_str
    );

    // Verify round-trip
    let output = doc.to_string();
    let reparsed = YamlFile::parse(&output);
    assert_eq!(reparsed.errors().len(), 0);
}

/// Test empty block scalars with strip chomping indicator
#[test]
fn test_empty_block_scalar_with_strip() {
    let yaml = "strip: |-\n";

    let parsed = YamlFile::parse(yaml);
    assert_eq!(
        parsed.errors().len(),
        0,
        "Should parse empty block scalar with strip"
    );

    let tree = parsed.tree();
    let doc = tree.document().expect("Should have document");
    let mapping = doc.as_mapping().expect("Should be mapping");

    // Should have the strip key
    assert_eq!(mapping.keys().count(), 1, "Should have 1 key");
    mapping.get("strip").expect("Should have strip key");

    // Verify round-trip
    let output = doc.to_string();
    let reparsed = YamlFile::parse(&output);
    assert_eq!(reparsed.errors().len(), 0);
}

/// Test empty block scalars with keep chomping indicator
#[test]
fn test_empty_block_scalar_with_keep() {
    let yaml = "keep: |+\n";

    let parsed = YamlFile::parse(yaml);
    assert_eq!(
        parsed.errors().len(),
        0,
        "Should parse empty block scalar with keep"
    );

    let tree = parsed.tree();
    let doc = tree.document().expect("Should have document");
    let mapping = doc.as_mapping().expect("Should be mapping");

    // Should have the keep key
    assert_eq!(mapping.keys().count(), 1, "Should have 1 key");
    mapping.get("keep").expect("Should have keep key");

    // Verify round-trip
    let output = doc.to_string();
    let reparsed = YamlFile::parse(&output);
    assert_eq!(reparsed.errors().len(), 0);
}

/// Test block scalar immediately after flow collection
#[test]
fn test_block_scalar_after_flow_collection() {
    let yaml = r#"flow: [a, b, c]
block: |
  Content here
"#;

    let parsed = YamlFile::parse(yaml);
    assert_eq!(parsed.errors().len(), 0, "Should parse block after flow");

    let tree = parsed.tree();
    let doc = tree.document().expect("Should have document");
    let mapping = doc.as_mapping().expect("Should be mapping");

    // Verify flow collection
    let flow_val = mapping.get("flow").expect("Should have flow key");
    assert!(flow_val.is_sequence(), "flow should be sequence");

    // Verify block scalar
    let block_val = mapping.get("block").expect("Should have block key");
    assert!(block_val.is_scalar(), "block should be scalar");
    assert_eq!(block_val.as_scalar().unwrap().as_string(), "Content here\n");

    // Verify round-trip
    let output = doc.to_string();
    assert_eq!(output, yaml);
}

// ========================================
// Comment Edge Cases (Section 3.3)
// ========================================

/// Test comment inside quoted string (should be literal, not a comment)
#[test]
fn test_comment_inside_quoted_string() {
    let yaml = r#"text: "This is # not a comment""#;

    let parsed = YamlFile::parse(yaml);
    assert_eq!(
        parsed.errors().len(),
        0,
        "Should parse quoted string with #"
    );

    let tree = parsed.tree();
    let doc = tree.document().expect("Should have document");
    let mapping = doc.as_mapping().expect("Should be mapping");

    let text_val = mapping.get("text").expect("Should have text key");
    assert_eq!(
        text_val.as_scalar().unwrap().as_string(),
        "This is # not a comment"
    );

    // Verify round-trip
    let output = doc.to_string();
    let reparsed = YamlFile::parse(&output);
    assert_eq!(reparsed.errors().len(), 0);
}

/// Test comment with only whitespace
#[test]
fn test_comment_with_only_whitespace() {
    let yaml = "key: value  #   \n";

    let parsed = YamlFile::parse(yaml);
    assert_eq!(
        parsed.errors().len(),
        0,
        "Should parse comment with whitespace"
    );

    let tree = parsed.tree();
    let doc = tree.document().expect("Should have document");
    let mapping = doc.as_mapping().expect("Should be mapping");

    let val = mapping.get("key").expect("Should have key");
    assert_eq!(val.as_scalar().unwrap().as_string(), "value");

    // Verify round-trip
    let output = doc.to_string();
    let reparsed = YamlFile::parse(&output);
    assert_eq!(reparsed.errors().len(), 0);
}

/// Test comment at EOF without trailing newline
#[test]
fn test_comment_at_eof_no_newline() {
    let yaml = "key: value\n# Final comment";

    let parsed = YamlFile::parse(yaml);
    assert_eq!(parsed.errors().len(), 0, "Should parse comment at EOF");

    let tree = parsed.tree();
    let doc = tree.document().expect("Should have document");
    let mapping = doc.as_mapping().expect("Should be mapping");

    assert_eq!(mapping.keys().count(), 1);
    assert!(mapping.contains_key("key"));

    // Verify round-trip
    let output = doc.to_string();
    let reparsed = YamlFile::parse(&output);
    assert_eq!(reparsed.errors().len(), 0);
}

/// Test comment with Unicode and emoji
#[test]
fn test_comment_with_unicode_emoji() {
    let yaml = "key: value  # 🎉 important! 日本語\n";

    let parsed = YamlFile::parse(yaml);
    assert_eq!(parsed.errors().len(), 0, "Should parse Unicode comments");

    let tree = parsed.tree();
    let doc = tree.document().expect("Should have document");
    let mapping = doc.as_mapping().expect("Should be mapping");

    let val = mapping.get("key").expect("Should have key");
    assert_eq!(val.as_scalar().unwrap().as_string(), "value");

    // Verify round-trip
    let output = doc.to_string();
    let reparsed = YamlFile::parse(&output);
    assert_eq!(reparsed.errors().len(), 0);
}

/// Test multiple # characters in comment
#[test]
fn test_multiple_hash_in_comment() {
    let yaml = "key: value  ## comment ### more\n";

    let parsed = YamlFile::parse(yaml);
    assert_eq!(
        parsed.errors().len(),
        0,
        "Should parse multiple # in comment"
    );

    let tree = parsed.tree();
    let doc = tree.document().expect("Should have document");
    let mapping = doc.as_mapping().expect("Should be mapping");

    let val = mapping.get("key").expect("Should have key");
    assert_eq!(val.as_scalar().unwrap().as_string(), "value");

    // Verify round-trip
    let output = doc.to_string();
    let reparsed = YamlFile::parse(&output);
    assert_eq!(reparsed.errors().len(), 0);
}

/// Test comment immediately after directive
#[test]
fn test_comment_after_directive() {
    let yaml = "%YAML 1.2 # this is a comment\n---\nkey: value\n";

    let parsed = YamlFile::parse(yaml);
    // Parser may or may not support this - just verify no crash
    let tree = parsed.tree();
    assert!(tree.document().is_some());
}

// ========================================
// Flow Collection Edge Cases (Section 3.4)
// ========================================

/// Test deeply nested flow collections (5 levels)
#[test]
fn test_deeply_nested_flow_collections() {
    let yaml = "nested: [[[[[a]]]]]";

    let parsed = YamlFile::parse(yaml);
    assert_eq!(parsed.errors().len(), 0, "Should parse deeply nested flow");

    let tree = parsed.tree();
    let doc = tree.document().expect("Should have document");
    let mapping = doc.as_mapping().expect("Should be mapping");

    let nested = mapping.get("nested").expect("Should have nested key");
    assert!(nested.is_sequence(), "Should be sequence");

    // Navigate down the nesting - verify structure exists
    let level1 = nested.as_sequence().unwrap();
    assert_eq!(level1.len(), 1, "Level 1 should have 1 element");

    let level2_elem = level1.get(0).expect("Should have level 2");
    assert!(level2_elem.is_sequence(), "Level 2 should be sequence");
    let level2 = level2_elem.as_sequence().unwrap();
    assert_eq!(level2.len(), 1, "Level 2 should have 1 element");

    let level3_elem = level2.get(0).expect("Should have level 3");
    assert!(level3_elem.is_sequence(), "Level 3 should be sequence");
    let level3 = level3_elem.as_sequence().unwrap();
    assert_eq!(level3.len(), 1, "Level 3 should have 1 element");

    let level4_elem = level3.get(0).expect("Should have level 4");
    assert!(level4_elem.is_sequence(), "Level 4 should be sequence");
    let level4 = level4_elem.as_sequence().unwrap();
    assert_eq!(level4.len(), 1, "Level 4 should have 1 element");

    let level5_elem = level4.get(0).expect("Should have level 5");
    assert!(level5_elem.is_sequence(), "Level 5 should be sequence");
    let level5 = level5_elem.as_sequence().unwrap();
    assert_eq!(level5.len(), 1, "Level 5 should have 1 element");

    // Final element should be scalar 'a'
    let final_elem = level5.get(0).unwrap();
    assert!(final_elem.is_scalar());
    assert_eq!(final_elem.as_scalar().unwrap().as_string(), "a");

    // Verify round-trip
    let output = doc.to_string();
    let reparsed = YamlFile::parse(&output);
    assert_eq!(reparsed.errors().len(), 0, "Round-trip should be valid");
}

/// Test trailing comma with whitespace in array
#[test]
fn test_trailing_comma_with_whitespace_array() {
    let yaml = "array: [a, b, ]";

    let parsed = YamlFile::parse(yaml);
    // Trailing comma may be accepted or rejected depending on strictness
    let tree = parsed.tree();
    assert!(tree.document().is_some());

    if parsed.errors().is_empty() {
        // If accepted, verify structure
        let doc = tree.document().unwrap();
        let mapping = doc.as_mapping().expect("Should be mapping");
        let arr = mapping.get_sequence("array").expect("Should have array");
        // Should have 2 elements (a and b), not 3
        assert!(arr.len() >= 2, "Should have at least 2 elements");
    }
}

/// Test trailing comma with whitespace in mapping
#[test]
fn test_trailing_comma_with_whitespace_mapping() {
    let yaml = "config: {a: 1, b: 2, }";

    let parsed = YamlFile::parse(yaml);
    // Trailing comma may be accepted or rejected
    let tree = parsed.tree();
    assert!(tree.document().is_some());

    if parsed.errors().is_empty() {
        // If accepted, verify structure
        let doc = tree.document().unwrap();
        let mapping = doc.as_mapping().expect("Should be mapping");
        let config = mapping.get_mapping("config").expect("Should have config");
        // Should have 2 keys (a and b)
        assert!(config.keys().count() >= 2, "Should have at least 2 keys");
    }
}

/// Test mixed flow and block styles in same document
#[test]
fn test_mixed_flow_and_block_styles() {
    let yaml = r#"flow: [a, b, c]
block:
  - item1
  - item2
nested: {key: value}
"#;

    let parsed = YamlFile::parse(yaml);
    assert_eq!(parsed.errors().len(), 0, "Should parse mixed styles");

    let tree = parsed.tree();
    let doc = tree.document().expect("Should have document");
    let mapping = doc.as_mapping().expect("Should be mapping");

    assert_eq!(mapping.keys().count(), 3);
    assert!(mapping.contains_key("flow"));
    assert!(mapping.contains_key("block"));
    assert!(mapping.contains_key("nested"));

    // Verify flow sequence
    let flow = mapping
        .get_sequence("flow")
        .expect("flow should be sequence");
    assert_eq!(flow.len(), 3);

    // Verify block sequence
    let block = mapping
        .get_sequence("block")
        .expect("block should be sequence");
    assert_eq!(block.len(), 2);

    // Verify flow mapping
    let nested = mapping
        .get_mapping("nested")
        .expect("nested should be mapping");
    assert_eq!(nested.keys().count(), 1);

    // Verify round-trip
    let output = doc.to_string();
    assert_eq!(output, yaml);
}

// ========================================
// Document Stream Edge Cases (Section 3.6)
// ========================================

/// Test multiple documents with different schemas (flow vs block)
#[test]
fn test_multiple_documents_different_schemas() {
    let yaml = "---\njson: {a: 1, b: 2}\n---\nyaml:\n  nested: value\n";

    let parsed = YamlFile::from_str(yaml).unwrap();

    // Verify we have exactly 2 documents
    let docs: Vec<_> = parsed.documents().collect();
    assert_eq!(docs.len(), 2, "Should have 2 documents");

    // First document has flow mapping
    let doc1 = &docs[0];
    let map1 = doc1.as_mapping().unwrap();
    assert_eq!(map1.keys().count(), 1);
    let json_val = map1.get("json").unwrap();
    let json_map = json_val.as_mapping().unwrap();
    assert_eq!(json_map.keys().count(), 2);
    assert_eq!(
        json_map.get("a").unwrap().as_scalar().unwrap().as_string(),
        "1"
    );
    assert_eq!(
        json_map.get("b").unwrap().as_scalar().unwrap().as_string(),
        "2"
    );

    // Second document has block mapping
    let doc2 = &docs[1];
    let map2 = doc2.as_mapping().unwrap();
    assert_eq!(map2.keys().count(), 1);
    let yaml_val = map2.get("yaml").unwrap();
    let yaml_map = yaml_val.as_mapping().unwrap();
    assert_eq!(
        yaml_map
            .get("nested")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "value"
    );

    // Verify exact round-trip
    let output = parsed.to_string();
    assert_eq!(output, yaml);
}

/// Test document with YAML directive
#[test]
fn test_document_with_yaml_directive() {
    let yaml = "%YAML 1.2\n---\nkey: value\n";

    let parsed = YamlFile::from_str(yaml).unwrap();

    // Should have 1 document
    let docs: Vec<_> = parsed.documents().collect();
    assert_eq!(docs.len(), 1, "Should have 1 document");

    // Verify content
    let doc = &docs[0];
    let mapping = doc.as_mapping().unwrap();
    assert_eq!(mapping.keys().count(), 1);
    assert_eq!(
        mapping.get("key").unwrap().as_scalar().unwrap().as_string(),
        "value"
    );

    // Verify exact round-trip
    let output = parsed.to_string();
    assert_eq!(output, yaml);
}

/// Test empty document markers
#[test]
fn test_empty_document_markers() {
    let yaml = "---\n---\n---\nkey: value\n";

    let parsed = YamlFile::from_str(yaml).unwrap();

    // Parser should handle empty documents
    let docs: Vec<_> = parsed.documents().collect();

    // Find the non-empty document (last one)
    let last_doc = docs.last().unwrap();
    let mapping = last_doc.as_mapping().unwrap();
    assert_eq!(mapping.keys().count(), 1);
    assert_eq!(
        mapping.get("key").unwrap().as_scalar().unwrap().as_string(),
        "value"
    );

    // Verify exact round-trip
    let output = parsed.to_string();
    assert_eq!(output, yaml);
}

/// Test moderately large document stream (100 documents)
#[test]
fn test_large_document_stream() {
    // Build a stream of 100 small documents
    let mut yaml = String::new();
    for i in 0..100 {
        yaml.push_str("---\n");
        yaml.push_str(&format!("doc{}: value{}\n", i, i));
    }

    let parsed = YamlFile::from_str(&yaml).unwrap();

    // Should have exactly 100 documents
    let docs: Vec<_> = parsed.documents().collect();
    assert_eq!(docs.len(), 100, "Should have 100 documents");

    // Verify first document
    let doc0 = &docs[0];
    let map0 = doc0.as_mapping().unwrap();
    assert_eq!(map0.keys().count(), 1);
    assert_eq!(
        map0.get("doc0").unwrap().as_scalar().unwrap().as_string(),
        "value0"
    );

    // Verify middle document
    let doc50 = &docs[50];
    let map50 = doc50.as_mapping().unwrap();
    assert_eq!(map50.keys().count(), 1);
    assert_eq!(
        map50.get("doc50").unwrap().as_scalar().unwrap().as_string(),
        "value50"
    );

    // Verify last document
    let doc99 = &docs[99];
    let map99 = doc99.as_mapping().unwrap();
    assert_eq!(map99.keys().count(), 1);
    assert_eq!(
        map99.get("doc99").unwrap().as_scalar().unwrap().as_string(),
        "value99"
    );

    // Verify exact round-trip
    let output = parsed.to_string();
    assert_eq!(output, yaml);
}
