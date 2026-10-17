//! Tests for block scalar indicators
//! Testing literal (|) and folded (>) scalars with various chomping and indentation indicators

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
    // Test explicit indentation indicator > 9 (e.g., |10). YAML allows only
    // a single digit, 1 to 9, and both saphyr and PyYAML reject this; the
    // lenient parser reads the `1` and ignores the `0`. Content therefore
    // starts at column 1, so the nine further spaces are part of the value.
    let yaml = "text: |10\n          Deep\n";

    let parsed = YamlFile::parse(yaml).to_result().unwrap();
    let document = parsed.document().unwrap();
    let mapping = document.as_mapping().unwrap();

    // Verify exact content
    let text_val = mapping.get("text").unwrap();
    assert_eq!(text_val.as_scalar().unwrap().as_string(), "         Deep\n");

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
    assert_eq!(literal_node.as_scalar().unwrap().as_string(), "");

    let folded_node = mapping.get("folded").unwrap();
    assert_eq!(folded_node.as_scalar().unwrap().as_string(), "");

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

#[test]
fn test_whitespace_only_line_does_not_swallow_next_entry() {
    // The second is the YAML test suite's Y79Y/001.
    for yaml in ["foo: |\n  \nbar: 1\n", "foo: |\n \t\nbar: 1\n"] {
        let doc = YamlFile::parse(yaml).to_result().unwrap();
        let document = doc.document().unwrap();
        let mapping = document.as_mapping().unwrap();
        let foo = mapping.get("foo").unwrap();
        assert_eq!(foo.to_string(), yaml[5..yaml.len() - 7], "{yaml:?}");
        let bar = mapping.get("bar").unwrap();
        assert_eq!(bar.as_scalar().unwrap().as_string(), "1", "{yaml:?}");
        assert_eq!(doc.to_string(), yaml);
    }

    let yaml = "- foo: >+\n      \n- baz\n";
    let doc = YamlFile::parse(yaml).to_result().unwrap();
    let document = doc.document().unwrap();
    let sequence = document.as_sequence().unwrap();
    assert_eq!(sequence.len(), 2);
    let first = sequence.get(0).unwrap();
    let foo = first.as_mapping().unwrap().get("foo").unwrap();
    assert_eq!(foo.as_scalar().unwrap().as_string(), "\n");
    let baz = sequence.get(1).unwrap();
    assert_eq!(baz.as_scalar().unwrap().as_string(), "baz");
    assert_eq!(doc.to_string(), yaml);
}

#[test]
fn test_empty_block_scalar_in_mapping_in_sequence() {
    let yaml = "- a: |\n  b: 1\n";
    let doc = YamlFile::parse(yaml).to_result().unwrap();
    let document = doc.document().unwrap();
    let sequence = document.as_sequence().unwrap();
    assert_eq!(sequence.len(), 1);
    let entry = sequence.get(0).unwrap();
    let mapping = entry.as_mapping().unwrap();
    assert_eq!(mapping.len(), 2);
    let a = mapping.get("a").unwrap();
    assert_eq!(a.as_scalar().unwrap().as_string(), "");
    let b = mapping.get("b").unwrap();
    assert_eq!(b.as_scalar().unwrap().as_string(), "1");
    assert_eq!(doc.to_string(), yaml);

    let yaml = "- k:\n    a: |\n\n    b: 1\n";
    let doc = YamlFile::parse(yaml).to_result().unwrap();
    let document = doc.document().unwrap();
    let sequence = document.as_sequence().unwrap();
    let entry = sequence.get(0).unwrap();
    let k = entry.as_mapping().unwrap().get("k").unwrap();
    let mapping = k.as_mapping().unwrap();
    assert_eq!(mapping.len(), 2);
    let b = mapping.get("b").unwrap();
    assert_eq!(b.as_scalar().unwrap().as_string(), "1");
    assert_eq!(doc.to_string(), yaml);
}

#[test]
fn test_header_comment_does_not_set_indicators() {
    for (yaml, expected) in [
        ("k: > # a-b\n  x\n", "x\n"),
        ("k: | # a+b\n  x\n\n", "x\n"),
        ("k: | # 1\n   x\n", "x\n"),
        ("k: |-2 # a+\n    x\n", "  x"),
    ] {
        let doc = YamlFile::parse(yaml).to_result().unwrap();
        let document = doc.document().unwrap();
        let mapping = document.as_mapping().unwrap();
        let k = mapping.get("k").unwrap();
        assert_eq!(k.as_scalar().unwrap().as_string(), expected, "{yaml:?}");
        assert_eq!(doc.to_string(), yaml);
    }
}

use yaml_edit::{YamlFile, YamlNode};

/// Read `yaml` and return the scalar found by following `path`, where a
/// numeric step indexes a sequence.
fn scalar_at(yaml: &str, path: &[&str]) -> String {
    let doc = YamlFile::parse(yaml).to_result().unwrap();
    assert_eq!(doc.to_string(), yaml);
    let document = doc.document().unwrap();
    let mut node = match document.as_mapping() {
        Some(mapping) => YamlNode::Mapping(mapping),
        None => YamlNode::Sequence(document.as_sequence().unwrap()),
    };
    for step in path {
        node = match step.parse::<usize>() {
            Ok(i) => node.as_sequence().unwrap().get(i).unwrap(),
            Err(_) => node.as_mapping().unwrap().get(*step).unwrap(),
        };
    }
    node.as_scalar().unwrap().as_string()
}

/// The indentation indicator counts from the enclosing collection's column
/// (spec 8.1.1.1), not from column 0.
#[test]
fn test_indentation_indicator_is_relative_to_parent() {
    // The YAML test suite's 4WA9.
    let yaml = "- aaa: |2\n    xxx\n  bbb: |\n    xxx\n";
    assert_eq!(scalar_at(yaml, &["0", "aaa"]), "xxx\n");
    assert_eq!(scalar_at(yaml, &["0", "bbb"]), "xxx\n");

    let yaml = "a:\n  b: |1\n    x\n  c: 1\n";
    assert_eq!(scalar_at(yaml, &["a", "b"]), " x\n");
    assert_eq!(scalar_at(yaml, &["a", "c"]), "1");

    assert_eq!(scalar_at("o:\n  k: |2\n      x\n", &["o", "k"]), "  x\n");
    assert_eq!(scalar_at("- - |1\n    x\n", &["0", "0"]), " x\n");

    // A header on its own line still counts from the mapping's column.
    let yaml = "o:\n  k:\n    |1\n     x\n  z: 1\n";
    assert_eq!(scalar_at(yaml, &["o", "k"]), "  x\n");
    assert_eq!(scalar_at(yaml, &["o", "z"]), "1");
}

/// In an explicit entry the indicator counts from the mapping's column,
/// wherever the header sits.
#[test]
fn test_indentation_indicator_in_explicit_entry() {
    for (yaml, scalar, sibling) in [
        (
            "x:\n  ? a\n  : |1\n     x\n  b: 1\n",
            ["x", "a"],
            ["x", "b"],
        ),
        (
            "x:\n  ? a\n  :\n    |1\n     x\n  b: 1\n",
            ["x", "a"],
            ["x", "b"],
        ),
        (
            "x:\n  ? a\n  : # c\n    |1\n     x\n  b: 1\n",
            ["x", "a"],
            ["x", "b"],
        ),
        ("- ? a\n  : |1\n     x\n  b: 1\n", ["0", "a"], ["0", "b"]),
        (
            "- ? a\n  :\n    |1\n     x\n  b: 1\n",
            ["0", "a"],
            ["0", "b"],
        ),
        (
            "x:\n  c: 1\n  ? a\n  :\n    |1\n     x\n  b: 1\n",
            ["x", "a"],
            ["x", "b"],
        ),
        ("? a\n: b: |1\n     x\n  c: 1\n", ["a", "b"], ["a", "c"]),
        (
            "? a\n: b:\n    |1\n     x\n  c: 1\n",
            ["a", "b"],
            ["a", "c"],
        ),
    ] {
        assert_eq!(scalar_at(yaml, &scalar), "  x\n", "{yaml:?}");
        assert_eq!(scalar_at(yaml, &sibling), "1", "{yaml:?}");
    }

    // A nested explicit mapping as the key, at column 2.
    let yaml = "? ? a\n  :\n    |1\n     x\n  b: 1\n: v\n";
    let doc = YamlFile::parse(yaml).to_result().unwrap();
    assert_eq!(doc.to_string(), yaml);
    let mapping = doc.document().unwrap().as_mapping().unwrap();
    let (key, value) = mapping.iter().next().unwrap();
    let key = key.as_mapping().unwrap();
    assert_eq!(
        key.get("a").unwrap().as_scalar().unwrap().as_string(),
        "  x\n"
    );
    assert_eq!(key.get("b").unwrap().as_scalar().unwrap().as_string(), "1");
    assert_eq!(value.as_scalar().unwrap().as_string(), "v");

    // The key's own header counts from the `?`.
    let yaml = "x:\n  ?\n    |1\n     x\n  : v\n";
    let doc = YamlFile::parse(yaml).to_result().unwrap();
    assert_eq!(doc.to_string(), yaml);
    let x = doc
        .document()
        .unwrap()
        .as_mapping()
        .unwrap()
        .get("x")
        .unwrap();
    let (key, value) = x.as_mapping().unwrap().iter().next().unwrap();
    assert_eq!(key.as_scalar().unwrap().as_string(), "  x\n");
    assert_eq!(value.as_scalar().unwrap().as_string(), "v");
}

#[test]
fn test_set_indented_block_value_in_nested_mapping() {
    let doc = YamlFile::parse("o:\n  k: v\n").to_result().unwrap();
    let o = doc
        .document()
        .unwrap()
        .as_mapping()
        .unwrap()
        .get("o")
        .unwrap();
    o.as_mapping().unwrap().set("k", "  x\n  y");
    let output = doc.to_string();
    assert_eq!(output, "o:\n  k: |2-\n      x\n      y\n");
    assert_eq!(scalar_at(&output, &["o", "k"]), "  x\n  y");
}

fn set_and_reread(value: &str) -> (String, String) {
    let parsed = YamlFile::parse("k: 0\n").to_result().unwrap();
    let mapping = parsed.document().unwrap().as_mapping().unwrap();
    mapping.set("k", value);
    let text = parsed.to_string();
    let reparsed = YamlFile::parse(&text).to_result().unwrap();
    let read = reparsed
        .document()
        .unwrap()
        .as_mapping()
        .unwrap()
        .get("k")
        .unwrap()
        .as_scalar()
        .unwrap()
        .as_string();
    (text, read)
}

#[test]
fn test_set_value_with_more_indented_first_line() {
    assert_eq!(
        set_and_reread("  x\n y"),
        ("k: |2-\n    x\n   y\n".to_string(), "  x\n y".to_string())
    );
    assert_eq!(
        set_and_reread("  a\nb"),
        ("k: |2-\n    a\n  b\n".to_string(), "  a\nb".to_string())
    );
}

#[test]
fn test_set_value_with_whitespace_only_lines() {
    assert_eq!(
        set_and_reread("a\n "),
        ("k: \"a\\n \"\n".to_string(), "a\n ".to_string())
    );
    assert_eq!(
        set_and_reread("a\n   \nb"),
        ("k: \"a\\n   \\nb\"\n".to_string(), "a\n   \nb".to_string())
    );
    assert_eq!(
        set_and_reread(" \n"),
        ("k: \" \\n\"\n".to_string(), " \n".to_string())
    );
}

fn header_violations(file: &yaml_edit::YamlFile) -> Vec<(String, Option<String>)> {
    use rowan::ast::AstNode;
    yaml_edit::validator::Validator::new()
        .validate_syntax(file.syntax())
        .into_iter()
        .map(|v| (v.message, v.location))
        .collect()
}

/// A block scalar header on the line after its key, but not indented past
/// it, is still the key's value: nothing else starts with `|` or `>`. The
/// validator reports it, as `s-separate(n+1)` rules it out.
#[test]
fn test_header_at_key_column_is_the_value() {
    use std::str::FromStr;
    use yaml_edit::YamlFile;

    let under_indented = |location: &str| {
        vec![(
            "Block scalar header must be indented past its parent node".to_string(),
            Some(location.to_string()),
        )]
    };

    for (yaml, expected, location) in [
        ("a:\n|\n x\n", "x\n", "2:1"),
        ("a: &x\n|\n x\n", "x\n", "2:1"),
        ("a:\n>\n x\n y\n", "x y\n", "2:1"),
        ("a:\n|2\n  x\n", "x\n", "2:1"),
        ("a:\n|\nb: 1\n", "", "2:1"),
    ] {
        let file = YamlFile::from_str(yaml).unwrap();
        assert_eq!(file.to_string(), yaml);
        let mapping = file.document().unwrap().as_mapping().unwrap();
        let a = mapping.get("a").unwrap();
        assert_eq!(a.as_scalar().unwrap().as_string(), expected, "{yaml:?}");
        assert_eq!(
            header_violations(&file),
            under_indented(location),
            "{yaml:?}"
        );
    }

    // The body still has to clear the key's column, so `b` stays a sibling.
    let file = YamlFile::from_str("a:\n|\nb: 1\n").unwrap();
    let mapping = file.document().unwrap().as_mapping().unwrap();
    assert_eq!(mapping.len(), 2);
    assert_eq!(
        mapping.get("b").unwrap().as_scalar().unwrap().as_string(),
        "1"
    );

    let yaml = "- a:\n  |\n   x\n  b: 1\n";
    let file = YamlFile::from_str(yaml).unwrap();
    assert_eq!(file.to_string(), yaml);
    let sequence = file.document().unwrap().as_sequence().unwrap();
    assert_eq!(sequence.len(), 1);
    let item = sequence.get(0).unwrap();
    let mapping = item.as_mapping().unwrap();
    assert_eq!(mapping.len(), 2);
    let a = mapping.get("a").unwrap();
    assert_eq!(a.as_scalar().unwrap().as_string(), "x\n");
    let b = mapping.get("b").unwrap();
    assert_eq!(b.as_scalar().unwrap().as_string(), "1");
    assert_eq!(header_violations(&file), under_indented("2:3"));
}

/// A block scalar on the line after its key, in a mapping that is a
/// sequence item, ends at a line back at the key's column.
#[test]
fn test_block_value_body_bounded_by_key_column_in_sequence_item() {
    use std::str::FromStr;
    use yaml_edit::YamlFile;

    for (yaml, expected) in [
        ("- a:\n   |\n  b: 1\n", ""),
        ("- a:\n   >\n  b: 1\n", ""),
        ("- a:\n   |\n    x\n  b: 1\n", "x\n"),
    ] {
        let file = YamlFile::from_str(yaml).unwrap();
        assert_eq!(file.to_string(), yaml);
        let sequence = file.document().unwrap().as_sequence().unwrap();
        assert_eq!(sequence.len(), 1);
        let item = sequence.get(0).unwrap();
        let mapping = item.as_mapping().unwrap();
        assert_eq!(mapping.len(), 2, "{yaml:?}");
        let a = mapping.get("a").unwrap();
        assert_eq!(a.as_scalar().unwrap().as_string(), expected, "{yaml:?}");
        let b = mapping.get("b").unwrap();
        assert_eq!(b.as_scalar().unwrap().as_string(), "1");
        assert_eq!(header_violations(&file), vec![], "{yaml:?}");
    }
}

/// Render a node as a compact string, so a test can compare a whole
/// structure at once.
fn render(node: &yaml_edit::YamlNode) -> String {
    use yaml_edit::YamlNode;
    match node {
        YamlNode::Scalar(s) if s.is_null() => "null".to_string(),
        YamlNode::Scalar(s) => format!("{:?}", s.as_string()),
        YamlNode::Sequence(s) => {
            let items: Vec<_> = s.values().map(|v| render(&v)).collect();
            format!("[{}]", items.join(", "))
        }
        YamlNode::Mapping(m) => {
            let entries: Vec<_> = m
                .iter()
                .map(|(k, v)| format!("{}: {}", render(&k), render(&v)))
                .collect();
            format!("{{{}}}", entries.join(", "))
        }
        YamlNode::TaggedNode(t) => format!("{} {:?}", t.tag().unwrap(), t.as_string()),
        YamlNode::Alias(a) => format!("*{}", a.name()),
    }
}

fn render_document(file: &yaml_edit::YamlFile) -> String {
    use rowan::ast::AstNode;
    let document = file.document().unwrap();
    let root = document
        .syntax()
        .children()
        .find_map(yaml_edit::YamlNode::from_syntax)
        .unwrap();
    render(&root)
}

/// A block scalar header on the line after a `-`, `?` or explicit `:`, but
/// not indented past it, can only be that indicator's node: nothing else
/// starts with `|` or `>`. `s-separate(n+1)` rules it out, so the validator
/// reports it.
#[test]
fn test_header_at_indicator_column_is_the_node() {
    use std::str::FromStr;
    use yaml_edit::YamlFile;

    for (yaml, expected, location) in [
        ("-\n|\n x\n", r#"["x\n"]"#, Some("2:1")),
        ("-\n>\n x\n y\n", r#"["x y\n"]"#, Some("2:1")),
        ("-\n|\n x\n- y\n", r#"["x\n", "y"]"#, Some("2:1")),
        ("-\n|\n- y\n", r#"["", "y"]"#, Some("2:1")),
        ("- # c\n|\n x\n", r#"["x\n"]"#, Some("2:1")),
        ("-\n# c\n|\n x\n", r#"["x\n"]"#, Some("3:1")),
        ("- -\n  |\n   x\n", r#"[["x\n"]]"#, Some("2:3")),
        ("- -\n  |\n  - y\n", r#"[["", "y"]]"#, Some("2:3")),
        ("-\n |\n  x\n", r#"["x\n"]"#, None),
        ("?\n|\n x\n", r#"{"x\n": null}"#, Some("2:1")),
        ("?\n|\n x\n: v\n", r#"{"x\n": "v"}"#, Some("2:1")),
        ("? # c\n|\n x\n", r#"{"x\n": null}"#, Some("2:1")),
        ("? a\n:\n|\n x\n", r#"{"a": "x\n"}"#, Some("3:1")),
        (
            "? a\n:\n>\n x\n? b\n",
            r#"{"a": "x\n", "b": null}"#,
            Some("3:1"),
        ),
        ("? a\n: # c\n|\n x\n", r#"{"a": "x\n"}"#, Some("3:1")),
        ("- ? a\n  :\n  |\n   x\n", r#"[{"a": "x\n"}]"#, Some("3:3")),
        ("- ?\n  |\n   x\n", r#"[{"x\n": null}]"#, Some("2:3")),
        ("? a\n:\n |\n  x\n", r#"{"a": "x\n"}"#, None),
    ] {
        let file = YamlFile::from_str(yaml).unwrap_or_else(|e| panic!("{yaml:?}: {e}"));
        assert_eq!(file.to_string(), yaml);
        {
            use rowan::ast::AstNode;
            yaml_edit::debug::validate_tree(file.syntax()).unwrap();
        }
        assert_eq!(render_document(&file), expected, "{yaml:?}");
        let expected_violations: Vec<_> = location
            .map(|location| {
                (
                    "Block scalar header must be indented past its parent node".to_string(),
                    Some(location.to_string()),
                )
            })
            .into_iter()
            .collect();
        assert_eq!(header_violations(&file), expected_violations, "{yaml:?}");
    }
}

/// A block scalar's body is literal text, so an indicator opening a body
/// line starts nothing: a `|` or `>` there is not another header, and a
/// quote after it does not run on into the following entries.
#[test]
fn test_indicators_opening_block_scalar_body_lines() {
    use std::str::FromStr;
    let cases = [
        ("a: |\n  >'\nb: ''\n", ">'\n", ""),
        ("a: |\n  |'\nb: ''\n", "|'\n", ""),
        ("a: >\n  |\"\nb: \"\"\n", "|\"\n", ""),
        ("a: |\n  >\n  {x\nb: c, d\n", ">\n{x\n", "c, d"),
        ("a: |\n  &'\nb: ''\n", "&'\n", ""),
        ("a: |\n  *'\nb: ''\n", "*'\n", ""),
        ("a: |\n  !'\nb: ''\n", "!'\n", ""),
        ("a: |\n  ? '\nb: ''\n", "? '\n", ""),
        ("a: |\n  - '\nb: ''\n", "- '\n", ""),
    ];
    for (yaml, a, b) in cases {
        let file = YamlFile::from_str(yaml).unwrap();
        assert_eq!(file.to_string(), yaml);
        let mapping = file.document().unwrap().as_mapping().unwrap();
        let get = |key: &str| {
            mapping
                .get(key)
                .and_then(|node| node.as_scalar().map(|s| s.as_string()))
        };
        assert_eq!(
            (get("a"), get("b")),
            (Some(a.to_string()), Some(b.to_string())),
            "{yaml:?}"
        );
    }

    let file = YamlFile::from_str("- |\n  |'\n- ''\n").unwrap();
    let sequence = file.document().unwrap().as_sequence().unwrap();
    let items: Vec<_> = sequence
        .values()
        .map(|node| node.as_scalar().unwrap().as_string())
        .collect();
    assert_eq!(items, vec!["|'\n".to_string(), String::new()]);
}
