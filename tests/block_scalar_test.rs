//! Tests for block scalar indicators
//! Testing literal (|) and folded (>) scalars with various chomping and indentation indicators

use yaml_edit::YamlFile;

// ========================================
// Literal Scalars (|)
// ========================================

#[test]
fn test_literal_scalar_default_clip() {
    // Default chomping (clip) - single trailing newline
    let yaml = r#"
text: |
  Line 1
  Line 2
  Line 3
"#;
    let doc = YamlFile::parse(yaml).to_result().unwrap();

    // Verify parsed scalar value via API
    let document = doc.document().unwrap();
    let mapping = document.as_mapping().unwrap();
    let text_scalar_node = mapping.get("text").unwrap();
    let text_scalar = text_scalar_node.as_scalar().unwrap();
    assert_eq!(text_scalar.as_string(), "Line 1\nLine 2\nLine 3\n");

    // Verify exact round-trip (preserves block scalar syntax)
    let output = doc.to_string();
    assert_eq!(output, yaml);
}

#[test]
fn test_literal_scalar_strip() {
    // Strip chomping (-) - removes all trailing newlines
    let yaml = r#"
text: |-
  Line 1
  Line 2
  Line 3
"#;
    let doc = YamlFile::parse(yaml).to_result().unwrap();

    // Verify parsed scalar value via API (no trailing newline with strip)
    let document = doc.document().unwrap();
    let mapping = document.as_mapping().unwrap();
    let text_scalar_node = mapping.get("text").unwrap();
    let text_scalar = text_scalar_node.as_scalar().unwrap();
    assert_eq!(text_scalar.as_string(), "Line 1\nLine 2\nLine 3");

    // Verify exact round-trip
    let output = doc.to_string();
    assert_eq!(output, yaml);
}

#[test]
fn test_literal_scalar_keep() {
    // Keep chomping (+) - preserves all trailing newlines
    let yaml = r#"
text: |+
  Line 1
  Line 2
  Line 3


"#;
    let doc = YamlFile::parse(yaml).to_result().unwrap();

    // Verify parsed scalar value via API (keeps all trailing newlines)
    let document = doc.document().unwrap();
    let mapping = document.as_mapping().unwrap();
    let text_scalar_node = mapping.get("text").unwrap();
    let text_scalar = text_scalar_node.as_scalar().unwrap();
    assert_eq!(text_scalar.as_string(), "Line 1\nLine 2\nLine 3\n\n\n");

    // Verify exact round-trip
    let output = doc.to_string();
    assert_eq!(output, yaml);
}

#[test]
fn test_literal_scalar_explicit_indentation() {
    // Explicit indentation indicator
    let yaml = r#"
text: |2
  Line 1
  Line 2
"#;
    let doc = YamlFile::parse(yaml).to_result().unwrap();

    // Verify parsed scalar value via API
    let document = doc.document().unwrap();
    let mapping = document.as_mapping().unwrap();
    let text_scalar_node = mapping.get("text").unwrap();
    let text_scalar = text_scalar_node.as_scalar().unwrap();
    assert_eq!(text_scalar.as_string(), "Line 1\nLine 2\n");

    // Verify exact round-trip
    let output = doc.to_string();
    assert_eq!(output, yaml);
}

#[test]
fn test_literal_scalar_combined_indicators() {
    // Combined chomping and indentation indicator
    let yaml = r#"
strip_with_indent: |-2
  Line 1
  Line 2

keep_with_indent: |+2
  Line 1
  Line 2
"#;
    let doc = YamlFile::parse(yaml).to_result().unwrap();

    // Verify parsed values via API
    let document = doc.document().unwrap();
    let mapping = document.as_mapping().unwrap();
    assert_eq!(
        mapping
            .get("strip_with_indent")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "Line 1\nLine 2"
    );
    assert_eq!(
        mapping
            .get("keep_with_indent")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "Line 1\nLine 2\n"
    );

    // Verify exact round-trip
    let output = doc.to_string();
    assert_eq!(output, yaml);
}

#[test]
fn test_literal_scalar_preserves_blank_lines() {
    // Literal scalars preserve blank lines
    let yaml = r#"
text: |
  Paragraph 1
  continues here

  Paragraph 2
  after blank line
"#;
    let doc = YamlFile::parse(yaml).to_result().unwrap();

    // Verify parsed value via API (blank lines preserved)
    let document = doc.document().unwrap();
    let mapping = document.as_mapping().unwrap();
    let text_scalar_node = mapping.get("text").unwrap();
    let text_scalar = text_scalar_node.as_scalar().unwrap();
    assert_eq!(
        text_scalar.as_string(),
        "Paragraph 1\ncontinues here\n\nParagraph 2\nafter blank line\n"
    );

    // Verify exact round-trip
    let output = doc.to_string();
    assert_eq!(output, yaml);
}

#[test]
fn test_literal_scalar_preserves_indentation() {
    // Literal scalars preserve relative indentation
    let yaml = r#"
code: |
  def hello():
      print("Hello")
      if True:
          print("World")
"#;
    let doc = YamlFile::parse(yaml).to_result().unwrap();

    // Verify parsed value via API (indentation preserved)
    let document = doc.document().unwrap();
    let mapping = document.as_mapping().unwrap();
    let code_scalar_node = mapping.get("code").unwrap();
    let code_scalar = code_scalar_node.as_scalar().unwrap();
    assert_eq!(
        code_scalar.as_string(),
        "def hello():\n    print(\"Hello\")\n    if True:\n        print(\"World\")\n"
    );

    // Verify exact round-trip
    let output = doc.to_string();
    assert_eq!(output, yaml);
}

// ========================================
// Folded Scalars (>)
// ========================================

#[test]
fn test_folded_scalar_default_clip() {
    // Default chomping (clip) - single trailing newline
    let yaml = r#"
text: >
  This is a long line
  that should be folded
  into a single line.
"#;
    let doc = YamlFile::parse(yaml).to_result().unwrap();

    // Verify parsed value via API (folded into single line)
    let document = doc.document().unwrap();
    let mapping = document.as_mapping().unwrap();
    let text_scalar_node = mapping.get("text").unwrap();
    let text_scalar = text_scalar_node.as_scalar().unwrap();
    assert_eq!(
        text_scalar.as_string(),
        "This is a long line that should be folded into a single line.\n"
    );

    // Verify exact round-trip
    let output = doc.to_string();
    assert_eq!(output, yaml);
}

#[test]
fn test_folded_scalar_strip() {
    // Strip chomping (-) - removes all trailing newlines
    let yaml = r#"
text: >-
  This is folded text
  with strip chomping
"#;
    let doc = YamlFile::parse(yaml).to_result().unwrap();

    // Verify parsed value via API (no trailing newline)
    let document = doc.document().unwrap();
    let mapping = document.as_mapping().unwrap();
    let text_scalar_node = mapping.get("text").unwrap();
    let text_scalar = text_scalar_node.as_scalar().unwrap();
    assert_eq!(
        text_scalar.as_string(),
        "This is folded text with strip chomping"
    );

    // Verify exact round-trip
    let output = doc.to_string();
    assert_eq!(output, yaml);
}

#[test]
fn test_folded_scalar_keep() {
    // Keep chomping (+) - preserves all trailing newlines
    let yaml = r#"
text: >+
  This is folded text
  with keep chomping


"#;
    let doc = YamlFile::parse(yaml).to_result().unwrap();

    // Verify parsed value via API (keeps all trailing newlines)
    let document = doc.document().unwrap();
    let mapping = document.as_mapping().unwrap();
    let text_scalar_node = mapping.get("text").unwrap();
    let text_scalar = text_scalar_node.as_scalar().unwrap();
    assert_eq!(
        text_scalar.as_string(),
        "This is folded text with keep chomping\n\n\n"
    );

    // Verify exact round-trip
    let output = doc.to_string();
    assert_eq!(output, yaml);
}

#[test]
fn test_folded_scalar_explicit_indentation() {
    // Explicit indentation indicator
    let yaml = r#"
text: >1
 Indented text
 continues here
"#;
    let doc = YamlFile::parse(yaml).to_result().unwrap();

    // Verify parsed value via API
    let document = doc.document().unwrap();
    let mapping = document.as_mapping().unwrap();
    let text_scalar_node = mapping.get("text").unwrap();
    let text_scalar = text_scalar_node.as_scalar().unwrap();
    assert_eq!(text_scalar.as_string(), "Indented text continues here\n");

    // Verify exact round-trip
    let output = doc.to_string();
    assert_eq!(output, yaml);
}

#[test]
fn test_folded_scalar_combined_indicators() {
    // Combined chomping and indentation indicator
    let yaml = r#"
strip_folded: >-2
  Text here
  continues

keep_folded: >+1
 Text here
 continues
"#;
    let doc = YamlFile::parse(yaml).to_result().unwrap();

    // Verify parsed values via API
    let document = doc.document().unwrap();
    let mapping = document.as_mapping().unwrap();
    assert_eq!(
        mapping
            .get("strip_folded")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "Text here continues"
    );
    assert_eq!(
        mapping
            .get("keep_folded")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "Text here continues\n"
    );

    // Verify exact round-trip
    let output = doc.to_string();
    assert_eq!(output, yaml);
}

#[test]
fn test_folded_scalar_blank_lines_create_breaks() {
    // Blank lines in folded scalars create paragraph breaks
    let yaml = r#"
text: >
  First paragraph
  continues here.

  Second paragraph
  after blank line.
"#;
    let doc = YamlFile::parse(yaml).to_result().unwrap();

    // Verify parsed value via API (blank line creates paragraph break - single newline)
    let document = doc.document().unwrap();
    let mapping = document.as_mapping().unwrap();
    let text_scalar_node = mapping.get("text").unwrap();
    let text_scalar = text_scalar_node.as_scalar().unwrap();
    assert_eq!(
        text_scalar.as_string(),
        "First paragraph continues here.\nSecond paragraph after blank line.\n"
    );

    // Verify exact round-trip
    let output = doc.to_string();
    assert_eq!(output, yaml);
}

#[test]
fn test_folded_scalar_more_indented_lines() {
    // More-indented lines are preserved
    let yaml = r#"
text: >
  Normal text
    Indented text
      More indented
  Back to normal
"#;
    let doc = YamlFile::parse(yaml).to_result().unwrap();

    // Verify parsed value via API (more indented lines preserved)
    let document = doc.document().unwrap();
    let mapping = document.as_mapping().unwrap();
    let text_scalar_node = mapping.get("text").unwrap();
    let text_scalar = text_scalar_node.as_scalar().unwrap();
    assert_eq!(
        text_scalar.as_string(),
        "Normal text\n  Indented text\n    More indented\nBack to normal\n"
    );

    // Verify exact round-trip
    let output = doc.to_string();
    assert_eq!(output, yaml);
}

// ========================================
// Edge Cases
// ========================================

#[test]
fn test_empty_literal_scalar() {
    let yaml = r#"
empty: |
"#;
    let doc = YamlFile::parse(yaml).to_result().unwrap();

    // Verify parsed value via API (empty string)
    let document = doc.document().unwrap();
    let mapping = document.as_mapping().unwrap();
    let empty_scalar_node = mapping.get("empty").unwrap();
    let empty_scalar = empty_scalar_node.as_scalar().unwrap();
    assert_eq!(empty_scalar.as_string(), "");

    // Verify exact round-trip
    let output = doc.to_string();
    assert_eq!(output, yaml);
}

#[test]
fn test_empty_folded_scalar() {
    let yaml = r#"
empty: >
"#;
    let doc = YamlFile::parse(yaml).to_result().unwrap();

    // Verify parsed value via API (empty string)
    let document = doc.document().unwrap();
    let mapping = document.as_mapping().unwrap();
    let empty_scalar_node = mapping.get("empty").unwrap();
    let empty_scalar = empty_scalar_node.as_scalar().unwrap();
    assert_eq!(empty_scalar.as_string(), "");

    // Verify exact round-trip
    let output = doc.to_string();
    assert_eq!(output, yaml);
}

#[test]
fn test_literal_with_leading_spaces() {
    let yaml = r#"
text: |
    Leading spaces
    are preserved
"#;
    let doc = YamlFile::parse(yaml).to_result().unwrap();

    // Verify parsed value via API (base indentation stripped, no extra leading spaces)
    let document = doc.document().unwrap();
    let mapping = document.as_mapping().unwrap();
    let text_scalar_node = mapping.get("text").unwrap();
    let text_scalar = text_scalar_node.as_scalar().unwrap();
    assert_eq!(text_scalar.as_string(), "Leading spaces\nare preserved\n");

    // Verify exact round-trip
    let output = doc.to_string();
    assert_eq!(output, yaml);
}

#[test]
fn test_literal_single_line() {
    let yaml = r#"
text: |
  Single line
"#;
    let doc = YamlFile::parse(yaml).to_result().unwrap();

    // Verify parsed value via API
    let document = doc.document().unwrap();
    let mapping = document.as_mapping().unwrap();
    let text_scalar_node = mapping.get("text").unwrap();
    let text_scalar = text_scalar_node.as_scalar().unwrap();
    assert_eq!(text_scalar.as_string(), "Single line\n");

    // Verify exact round-trip
    let output = doc.to_string();
    assert_eq!(output, yaml);
}

#[test]
fn test_folded_single_line() {
    let yaml = r#"
text: >
  Single line
"#;
    let doc = YamlFile::parse(yaml).to_result().unwrap();

    // Verify parsed value via API
    let document = doc.document().unwrap();
    let mapping = document.as_mapping().unwrap();
    let text_scalar_node = mapping.get("text").unwrap();
    let text_scalar = text_scalar_node.as_scalar().unwrap();
    assert_eq!(text_scalar.as_string(), "Single line\n");

    // Verify exact round-trip
    let output = doc.to_string();
    assert_eq!(output, yaml);
}

#[test]
fn test_literal_with_trailing_spaces() {
    // Literal scalars preserve trailing spaces on lines
    let yaml = "text: |\n  Line with spaces  \n  Another line  \n";
    let doc = YamlFile::parse(yaml).to_result().unwrap();

    // Verify parsed value via API (trailing spaces preserved)
    let document = doc.document().unwrap();
    let mapping = document.as_mapping().unwrap();
    let text_scalar_node = mapping.get("text").unwrap();
    let text_scalar = text_scalar_node.as_scalar().unwrap();
    assert_eq!(
        text_scalar.as_string(),
        "Line with spaces  \nAnother line  \n"
    );

    // Verify exact round-trip
    let output = doc.to_string();
    assert_eq!(output, yaml);
}

#[test]
fn test_multiple_block_scalars_in_mapping() {
    let yaml = r#"
description: |
  This is a description
  with multiple lines.

notes: >
  These are notes
  that will be folded.

code: |-
  def function():
      pass
"#;
    let doc = YamlFile::parse(yaml).to_result().unwrap();

    // Verify all three values via API
    let document = doc.document().unwrap();
    let mapping = document.as_mapping().unwrap();
    assert_eq!(
        mapping
            .get("description")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "This is a description\nwith multiple lines.\n"
    );
    assert_eq!(
        mapping
            .get("notes")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "These are notes that will be folded.\n"
    );
    assert_eq!(
        mapping
            .get("code")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "def function():\n    pass"
    );

    // Verify exact round-trip
    let output = doc.to_string();
    assert_eq!(output, yaml);
}

#[test]
fn test_block_scalar_in_sequence() {
    let yaml = r#"
items:
  - |
    First item
    with multiple lines
  - >
    Second item
    folded text
  - |-
    Third item
    stripped
"#;
    let doc = YamlFile::parse(yaml).to_result().unwrap();

    // Verify sequence items via API
    let document = doc.document().unwrap();
    let mapping = document.as_mapping().unwrap();
    let items = mapping.get_sequence("items").unwrap();
    assert_eq!(items.len(), 3);
    assert_eq!(
        items.get(0).unwrap().as_scalar().unwrap().as_string(),
        "First item\nwith multiple lines\n"
    );
    assert_eq!(
        items.get(1).unwrap().as_scalar().unwrap().as_string(),
        "Second item folded text\n"
    );
    assert_eq!(
        items.get(2).unwrap().as_scalar().unwrap().as_string(),
        "Third item\nstripped"
    );

    // Verify exact round-trip
    let output = doc.to_string();
    assert_eq!(output, yaml);
}

#[test]
fn test_literal_with_special_characters() {
    let yaml = r#"
text: |
  Special chars: !@#$%^&*()
  Quotes: "double" and 'single'
  Colons: key: value
  Dashes: - item
"#;
    let doc = YamlFile::parse(yaml).to_result().unwrap();

    // Verify parsed value via API (special characters preserved)
    let document = doc.document().unwrap();
    let mapping = document.as_mapping().unwrap();
    let text_scalar_node = mapping.get("text").unwrap();
    let text_scalar = text_scalar_node.as_scalar().unwrap();
    assert_eq!(
        text_scalar.as_string(),
        "Special chars: !@#$%^&*()\nQuotes: \"double\" and 'single'\nColons: key: value\nDashes: - item\n"
    );

    // Verify exact round-trip
    let output = doc.to_string();
    assert_eq!(output, yaml);
}

// ========================================
// YAML Spec Examples
// ========================================

#[test]
fn test_spec_example_literal_scalar() {
    // From YAML spec - literal scalar example
    let yaml = r#"
example: |
  Several lines of text,
  with some "quotes" of various 'types',
  and also a blank line:

  and some more text.
"#;
    let doc = YamlFile::parse(yaml).to_result().unwrap();

    // Verify parsed value via API
    let document = doc.document().unwrap();
    let mapping = document.as_mapping().unwrap();
    let example_scalar_node = mapping.get("example").unwrap();
    let example_scalar = example_scalar_node.as_scalar().unwrap();
    assert_eq!(
        example_scalar.as_string(),
        "Several lines of text,\nwith some \"quotes\" of various 'types',\nand also a blank line:\n\nand some more text.\n"
    );

    // Verify exact round-trip
    let output = doc.to_string();
    assert_eq!(output, yaml);
}

#[test]
fn test_spec_example_folded_scalar() {
    // From YAML spec - folded scalar example
    let yaml = r#"
example: >
  Several lines of text,
  with some "quotes" of various 'types',
  and also a blank line:

  and some more text.
"#;
    let doc = YamlFile::parse(yaml).to_result().unwrap();

    // Verify parsed value via API (folded)
    let document = doc.document().unwrap();
    let mapping = document.as_mapping().unwrap();
    let example_scalar_node = mapping.get("example").unwrap();
    let example_scalar = example_scalar_node.as_scalar().unwrap();
    assert_eq!(
        example_scalar.as_string(),
        "Several lines of text, with some \"quotes\" of various 'types', and also a blank line:\nand some more text.\n"
    );

    // Verify exact round-trip
    let output = doc.to_string();
    assert_eq!(output, yaml);
}

#[test]
fn test_all_chomping_modes_comparison() {
    // Compare all three chomping modes side by side
    let yaml = r#"
clip: |
  text

strip: |-
  text

keep: |+
  text

"#;
    let doc = YamlFile::parse(yaml).to_result().unwrap();

    // Verify all three values via API showing different chomping behavior
    let document = doc.document().unwrap();
    let mapping = document.as_mapping().unwrap();
    assert_eq!(
        mapping
            .get("clip")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "text\n"
    );
    assert_eq!(
        mapping
            .get("strip")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "text"
    );
    assert_eq!(
        mapping
            .get("keep")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "text\n\n"
    );

    // Verify exact round-trip
    let output = doc.to_string();
    assert_eq!(output, yaml);
}

// ========================================
// Block Scalar Edge Cases
// ========================================

#[test]
fn test_explicit_indentation_greater_than_9() {
    // Test explicit indentation indicator > 9 (e.g., |10)
    // YAML spec allows single-digit indentation indicators (1-9)
    // Parser treats "10" as "1" (indentation level), ignoring the "0"
    let yaml = "text: |10\n          Deep\n";

    let parsed = YamlFile::parse(yaml).to_result().unwrap();
    let document = parsed.document().unwrap();
    let mapping = document.as_mapping().unwrap();

    // Verify exact content
    let text_val = mapping.get("text").unwrap();
    assert_eq!(text_val.as_scalar().unwrap().as_string(), "Deep\n");

    // Verify exact round-trip
    let output = parsed.to_string();
    assert_eq!(output, yaml);
}

#[test]
fn test_trailing_whitespace_after_indicator() {
    // Test block scalar indicator line with trailing whitespace
    // YAML spec section 8.1.1.2: trailing whitespace is allowed after indicator
    let yaml = "text: |  \n  Content here\n";

    let parsed = YamlFile::parse(yaml).to_result().unwrap();
    let document = parsed.document().unwrap();
    let mapping = document.as_mapping().unwrap();

    // Verify exact content
    let text_val = mapping.get("text").unwrap();
    assert_eq!(text_val.as_scalar().unwrap().as_string(), "Content here\n");

    // Verify exact round-trip
    let output = parsed.to_string();
    assert_eq!(output, yaml);
}

#[test]
fn test_windows_line_endings() {
    // Test block scalar with Windows line endings (\r\n)
    // Parser normalizes CRLF to LF internally for content
    let yaml = "text: |\r\n  Line 1\r\n  Line 2\r\n";

    let parsed = YamlFile::parse(yaml).to_result().unwrap();
    let document = parsed.document().unwrap();
    let mapping = document.as_mapping().unwrap();

    // Verify exact parsed content (normalized to Unix LF)
    let text_val = mapping.get("text").unwrap();
    assert_eq!(
        text_val.as_scalar().unwrap().as_string(),
        "Line 1\nLine 2\n"
    );

    // Verify exact round-trip (should preserve original CRLF)
    let output = parsed.to_string();
    assert_eq!(output, yaml);
}

#[test]
fn test_empty_block_scalars_with_all_indicators() {
    // Test empty block scalars with different chomping indicators
    // YAML spec allows empty block scalars
    let yaml = "literal: |\n\nfolded: >\n\nstrip: |-\n\nkeep: |+\n";

    let parsed = YamlFile::parse(yaml).to_result().unwrap();
    let document = parsed.document().unwrap();
    let mapping = document.as_mapping().unwrap();

    // Verify all 4 keys exist
    assert_eq!(mapping.keys().count(), 4);

    // Verify exact values based on chomping indicators
    let literal_node = mapping.get("literal").unwrap();
    assert_eq!(literal_node.as_scalar().unwrap().as_string(), "\n");

    let folded_node = mapping.get("folded").unwrap();
    assert_eq!(folded_node.as_scalar().unwrap().as_string(), "\n");

    let strip_node = mapping.get("strip").unwrap();
    assert_eq!(strip_node.as_scalar().unwrap().as_string(), "");

    let keep_node = mapping.get("keep").unwrap();
    assert_eq!(keep_node.as_scalar().unwrap().as_string(), "");

    // Verify exact round-trip
    let output = parsed.to_string();
    assert_eq!(output, yaml);
}

#[test]
fn test_block_scalar_after_flow_collection() {
    // Test block scalar immediately after flow collection
    // Verifies parser can switch from flow to block context
    let yaml = r#"flow: [a, b, c]
block: |
  Content
  Here
"#;

    let parsed = YamlFile::parse(yaml).to_result().unwrap();
    let document = parsed.document().unwrap();
    let mapping = document.as_mapping().unwrap();

    // Verify flow collection
    let flow = mapping.get("flow").unwrap();
    let flow_seq = flow.as_sequence().unwrap();
    assert_eq!(flow_seq.len(), 3);
    assert_eq!(
        flow_seq.get(0).unwrap().as_scalar().unwrap().as_string(),
        "a"
    );

    // Verify block scalar
    let block = mapping.get("block").unwrap();
    assert_eq!(block.as_scalar().unwrap().as_string(), "Content\nHere\n");

    // Verify exact round-trip
    let output = parsed.to_string();
    assert_eq!(output, yaml);
}

#[test]
fn test_mixed_indentation_handling() {
    // Test block scalar with mixed indentation (tabs and spaces)
    // YAML spec prohibits tabs for indentation in block context
    // Parser accepts it and preserves the tab character
    let yaml = "text: |\n\t  Mixed\n";

    let parsed = YamlFile::parse(yaml).to_result().unwrap();
    let document = parsed.document().unwrap();
    let mapping = document.as_mapping().unwrap();

    // Verify exact content including tab character
    let text_val = mapping.get("text").unwrap();
    assert_eq!(text_val.as_scalar().unwrap().as_string(), "\t  Mixed\n");

    // Verify exact round-trip
    let output = parsed.to_string();
    assert_eq!(output, yaml);
}
