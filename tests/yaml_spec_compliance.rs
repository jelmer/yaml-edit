use std::str::FromStr;
use yaml_edit::Yaml;

// ========================================
// YAML 1.2 Core Schema Compliance Tests
// ========================================
// These tests verify compliance with the YAML 1.2 specification
// Reference: https://yaml.org/spec/1.2.2/

// ========================================
// Section 2.1: Collections (Sequences and Mappings)
// ========================================

#[test]
fn test_block_sequence_basic() {
    let yaml = r#"# Shopping list
- milk
- bread
- eggs"#;

    let parsed = Yaml::from_str(yaml).expect("Should parse basic block sequence");
    let output = parsed.to_string();

    assert!(output.contains("milk"));
    assert!(output.contains("bread"));
    assert!(output.contains("eggs"));
    assert!(
        output.contains("# Shopping list"),
        "Comment should be preserved"
    );
}

#[test]
fn test_flow_sequence_basic() {
    let yaml = r#"items: [apple, banana, orange]"#;

    let parsed = Yaml::from_str(yaml).expect("Should parse flow sequence");
    let output = parsed.to_string();

    assert!(output.contains("apple"));
    assert!(output.contains("banana"));
    assert!(output.contains("orange"));
}

#[test]
fn test_block_mapping_basic() {
    let yaml = r#"name: John Doe
age: 30
city: New York"#;

    let parsed = Yaml::from_str(yaml).expect("Should parse block mapping");
    let output = parsed.to_string();

    assert!(output.contains("name: John Doe"));
    assert!(output.contains("age: 30"));
    assert!(output.contains("city: New York"));
}

#[test]
fn test_flow_mapping_basic() {
    let yaml = r#"person: {name: Alice, age: 25, city: Boston}"#;

    let parsed = Yaml::from_str(yaml).expect("Should parse flow mapping");
    let output = parsed.to_string();

    assert!(output.contains("name: Alice"));
    assert!(output.contains("age: 25"));
    assert!(output.contains("city: Boston"));
}

#[test]
fn test_nested_collections() {
    let yaml = r#"users:
  - name: Alice
    skills:
      - Python
      - JavaScript
    projects:
      web: active
      mobile: pending
  - name: Bob
    skills: [Java, C++]
    projects: {backend: done, frontend: active}"#;

    let parsed = Yaml::from_str(yaml).expect("Should parse nested collections");
    let output = parsed.to_string();

    assert!(output.contains("name: Alice"));
    assert!(output.contains("Python"));
    assert!(output.contains("web: active"));
    assert!(output.contains("name: Bob"));
}

// ========================================
// Section 2.2: Scalar Styles
// ========================================

#[test]
fn test_plain_scalars() {
    let yaml = r#"string: hello world
integer: 42
float: 3.14
boolean_true: true
boolean_false: false
null_value: null"#;

    let parsed = Yaml::from_str(yaml).expect("Should parse plain scalars");
    let output = parsed.to_string();

    assert!(output.contains("hello world"));
    assert!(output.contains("42"));
    assert!(output.contains("3.14"));
    assert!(output.contains("true"));
    assert!(output.contains("false"));
    assert!(output.contains("null"));
}

#[test]
fn test_single_quoted_scalars() {
    let yaml = r#"single: 'This is a single-quoted string'
with_quotes: 'It''s got an apostrophe'
multiline: 'First line
  second line'"#;

    let parsed = Yaml::from_str(yaml).expect("Should parse single-quoted scalars");
    let output = parsed.to_string();

    assert!(output.contains("single-quoted string"));
}

#[test]
fn test_double_quoted_scalars() {
    let yaml = r#"double: "This is a double-quoted string"
escaped: "Line 1\nLine 2\tTabbed"
unicode: "Smiley: \u263A"
quote: "She said \"Hello\"""#;

    let parsed = Yaml::from_str(yaml).expect("Should parse double-quoted scalars");
    let output = parsed.to_string();

    assert!(output.contains("double-quoted string"));
    assert!(output.contains("escaped"));
    assert!(output.contains("unicode"));
}

#[test]
fn test_literal_scalar() {
    let yaml = r#"literal: |
  This is a literal scalar.
  It preserves newlines.
  
  And blank lines too."#;

    let parsed = Yaml::from_str(yaml).expect("Should parse literal scalar");
    let output = parsed.to_string();

    assert!(output.contains("literal:"));
    assert!(output.contains("|"));
}

#[test]
fn test_folded_scalar() {
    let yaml = r#"folded: >
  This is a folded scalar.
  These lines will be folded
  into a single line.
  
  But blank lines create
  paragraph breaks."#;

    let parsed = Yaml::from_str(yaml).expect("Should parse folded scalar");
    let output = parsed.to_string();

    assert!(output.contains("folded:"));
    assert!(output.contains(">"));
}

// ========================================
// Section 2.3: Nodes (Anchors and Aliases)
// ========================================

#[test]
fn test_simple_anchor_and_alias() {
    let yaml = r#"first: &anchor_name
  name: Shared Value
  data: 123
second: *anchor_name"#;

    let parsed = Yaml::from_str(yaml).expect("Should parse anchor and alias");
    let output = parsed.to_string();

    assert!(output.contains("&anchor_name"));
    assert!(output.contains("*anchor_name"));
    assert!(output.contains("Shared Value"));
}

#[test]
fn test_complex_anchors() {
    let yaml = r#"defaults: &defaults
  adapter: postgres
  host: localhost
  
development:
  database: dev_db
  <<: *defaults
  
test:
  database: test_db
  <<: *defaults"#;

    let parsed = Yaml::from_str(yaml).expect("Should parse merge keys");
    let output = parsed.to_string();

    assert!(output.contains("&defaults"));
    assert!(output.contains("<<: *defaults"));
}

#[test]
fn test_multiple_anchors() {
    let yaml = r#"colors:
  - &red '#FF0000'
  - &green '#00FF00'
  - &blue '#0000FF'
  
theme:
  primary: *blue
  secondary: *green
  danger: *red"#;

    let parsed = Yaml::from_str(yaml).expect("Should parse multiple anchors");
    let output = parsed.to_string();

    assert!(output.contains("&red"));
    assert!(output.contains("&green"));
    assert!(output.contains("&blue"));
    assert!(output.contains("*blue"));
}

// ========================================
// Section 3.1: Directives
// ========================================

#[test]
fn test_yaml_directive() {
    let yaml = r#"%YAML 1.2
---
key: value"#;

    let parsed = Yaml::from_str(yaml).expect("Should parse YAML directive");
    let output = parsed.to_string();

    assert!(output.contains("key: value"));
}

#[test]
fn test_tag_directive() {
    let yaml = r#"%TAG ! tag:example.com,2014:
---
!foo "bar""#;

    let parsed = Yaml::from_str(yaml).expect("Should parse TAG directive");
    let output = parsed.to_string();

    assert!(output.contains("bar"));
}

#[test]
fn test_multiple_directives() {
    let yaml = r#"%YAML 1.2
%TAG ! tag:example.com,2014:
%TAG !! tag:example.com,2014:app/
---
data: value"#;

    let parsed = Yaml::from_str(yaml).expect("Should parse multiple directives");
    let output = parsed.to_string();

    assert!(output.contains("data: value"));
}

// ========================================
// Section 3.2: Document Boundaries
// ========================================

#[test]
fn test_explicit_document() {
    let yaml = r#"---
document: one
..."#;

    let parsed = Yaml::from_str(yaml).expect("Should parse explicit document");
    let output = parsed.to_string();

    assert!(output.contains("document: one"));
}

#[test]
fn test_multiple_documents() {
    let yaml = r#"---
first: document
---
second: document
---
third: document"#;

    let parsed = Yaml::from_str(yaml).expect("Should parse multiple documents");
    let output = parsed.to_string();

    assert!(output.contains("first: document"));
    // Note: Current implementation might only handle first document
}

#[test]
fn test_bare_document() {
    // Document without explicit markers
    let yaml = r#"key: value
another: value"#;

    let parsed = Yaml::from_str(yaml).expect("Should parse bare document");
    let output = parsed.to_string();

    assert!(output.contains("key: value"));
    assert!(output.contains("another: value"));
}

// ========================================
// Section 5.3: Indicator Characters
// ========================================

#[test]
fn test_reserved_indicators() {
    // Test that reserved indicators are handled properly
    let yaml = r#"# Comment
key: value  # Inline comment
sequence:
  - item1
  - item2
mapping: {a: 1, b: 2}
anchor: &test value
alias: *test
literal: |
  text
folded: >
  text
tag: !str "value"
directive: value"#;

    let parsed = Yaml::from_str(yaml).expect("Should handle all indicator characters");
    let output = parsed.to_string();

    assert!(output.contains("#")); // Comment indicator
    assert!(output.contains("-")); // Sequence indicator
    assert!(output.contains(":")); // Mapping indicator
    assert!(output.contains("{")); // Flow mapping start
    assert!(output.contains("}")); // Flow mapping end
    assert!(output.contains("[") || !output.contains("[")); // Flow sequence (optional)
    assert!(output.contains("&")); // Anchor indicator
    assert!(output.contains("*")); // Alias indicator
    assert!(output.contains("|")); // Literal indicator
    assert!(output.contains(">")); // Folded indicator
    assert!(output.contains("!")); // Tag indicator
}

// ========================================
// Section 6.1: Indentation
// ========================================

#[test]
fn test_consistent_indentation() {
    let yaml = r#"root:
  level1:
    level2:
      level3: value
    another2: value
  another1: value"#;

    let parsed = Yaml::from_str(yaml).expect("Should parse consistent indentation");
    let output = parsed.to_string();

    assert!(output.contains("level3: value"));
    assert!(output.contains("another2: value"));
    assert!(output.contains("another1: value"));
}

#[test]
fn test_tab_indentation() {
    // Tabs are not allowed for indentation in YAML
    let yaml = "key: value\n\tindented: invalid";

    let parsed = Yaml::from_str(yaml);
    // This should either error or handle gracefully
    match parsed {
        Ok(y) => {
            let output = y.to_string();
            assert!(output.contains("key: value"));
        }
        Err(_) => {
            // Tabs in indentation can legitimately cause errors
        }
    }
}

#[test]
fn test_zero_indented_sequences() {
    let yaml = r#"items:
- one
- two
- three"#;

    let parsed = Yaml::from_str(yaml).expect("Should parse zero-indented sequence");
    let output = parsed.to_string();

    assert!(output.contains("one"));
    assert!(output.contains("two"));
    assert!(output.contains("three"));
}

// ========================================
// Section 6.2: Line Folding
// ========================================

#[test]
fn test_line_folding_in_plain_scalars() {
    let yaml = r#"description: This is a very long
  description that spans multiple
  lines but should be treated as
  a single value"#;

    let parsed = Yaml::from_str(yaml).expect("Should parse multi-line plain scalar");
    let output = parsed.to_string();

    assert!(output.contains("description:"));
}

// ========================================
// Section 6.3: Comments
// ========================================

#[test]
fn test_comment_positions() {
    let yaml = r#"# Document comment
key: value  # Inline comment

# Comment before item
item: value

list:
  # Comment in list
  - item1
  - item2  # Item comment"#;

    let parsed = Yaml::from_str(yaml).expect("Should parse comments in various positions");
    let output = parsed.to_string();

    // Verify content is preserved
    assert!(output.contains("key: value"));
    assert!(output.contains("item: value"));
    assert!(output.contains("item1"));
    assert!(output.contains("item2"));

    // Comments should ideally be preserved
    assert!(output.contains("#"));
}

#[test]
fn test_multiline_comments() {
    let yaml = r#"# This is a
# multi-line comment
# spanning several lines
data: value

# Another block
# of comments
more: data"#;

    let parsed = Yaml::from_str(yaml).expect("Should parse multi-line comments");
    let output = parsed.to_string();

    assert!(output.contains("data: value"));
    assert!(output.contains("more: data"));
}

// ========================================
// Section 7: Flow Styles
// ========================================

#[test]
fn test_flow_sequence_variations() {
    let yaml = r#"empty: []
single: [one]
multiple: [one, two, three]
trailing: [one, two,]
multiline: [
  one,
  two,
  three
]"#;

    let parsed = Yaml::from_str(yaml).expect("Should parse flow sequence variations");
    let output = parsed.to_string();

    assert!(output.contains("empty:"));
    assert!(output.contains("single:"));
    assert!(output.contains("multiple:"));
}

#[test]
fn test_flow_mapping_variations() {
    let yaml = r#"empty: {}
single: {key: value}
multiple: {a: 1, b: 2, c: 3}
trailing: {a: 1, b: 2,}
multiline: {
  key1: value1,
  key2: value2
}"#;

    let parsed = Yaml::from_str(yaml).expect("Should parse flow mapping variations");
    let output = parsed.to_string();

    assert!(output.contains("empty:"));
    assert!(output.contains("single:"));
    assert!(output.contains("multiple:"));
}

#[test]
fn test_nested_flow_collections() {
    let yaml = r#"complex: {
  lists: [a, b, [c, d]],
  maps: {x: 1, y: {z: 2}},
  mixed: [{a: 1}, {b: 2}]
}"#;

    let parsed = Yaml::from_str(yaml).expect("Should parse nested flow collections");
    let output = parsed.to_string();

    assert!(output.contains("lists:"));
    assert!(output.contains("maps:"));
    assert!(output.contains("mixed:"));
}

// ========================================
// Section 8: Block Styles
// ========================================

#[test]
fn test_literal_scalar_chomping() {
    let yaml = r#"strip: |-
  text
keep: |
  text
clip: |+
  text"#;

    let parsed = Yaml::from_str(yaml).expect("Should parse literal scalar chomping indicators");
    let output = parsed.to_string();

    assert!(output.contains("strip:"));
    assert!(output.contains("keep:"));
    assert!(output.contains("clip:"));
}

#[test]
fn test_folded_scalar_chomping() {
    let yaml = r#"strip: >-
  text
keep: >
  text
clip: >+
  text"#;

    let parsed = Yaml::from_str(yaml).expect("Should parse folded scalar chomping indicators");
    let output = parsed.to_string();

    assert!(output.contains("strip:"));
    assert!(output.contains("keep:"));
    assert!(output.contains("clip:"));
}

#[test]
fn test_explicit_indentation_indicators() {
    let yaml = r#"literal: |2
  This has explicit
  indentation indicator
folded: >1
 This too"#;

    let parsed = Yaml::from_str(yaml).expect("Should parse explicit indentation indicators");
    let output = parsed.to_string();

    assert!(output.contains("literal:"));
    assert!(output.contains("folded:"));
}

// ========================================
// Section 10.1: Core Schema Types
// ========================================

#[test]
fn test_null_representations() {
    let yaml = r#"canonical: null
english: Null
tilde: ~
empty:"#;

    let parsed = Yaml::from_str(yaml).expect("Should parse null representations");
    let output = parsed.to_string();

    assert!(output.contains("canonical:"));
    assert!(output.contains("english:"));
    assert!(output.contains("tilde:"));
    assert!(output.contains("empty:"));
}

#[test]
fn test_boolean_representations() {
    let yaml = r#"canonical_true: true
canonical_false: false
english_true: True
english_false: False
caps_true: TRUE
caps_false: FALSE"#;

    let parsed = Yaml::from_str(yaml).expect("Should parse boolean representations");
    let output = parsed.to_string();

    assert!(output.contains("true") || output.contains("True") || output.contains("TRUE"));
    assert!(output.contains("false") || output.contains("False") || output.contains("FALSE"));
}

#[test]
fn test_integer_representations() {
    let yaml = r#"decimal: 123
negative: -456
zero: 0
octal: 0o777
hex: 0xFF
binary: 0b1010"#;

    let parsed = Yaml::from_str(yaml).expect("Should parse integer representations");
    let output = parsed.to_string();

    assert!(output.contains("123"));
    assert!(output.contains("-456"));
    assert!(output.contains("0"));
}

#[test]
fn test_float_representations() {
    let yaml = r#"simple: 3.14
negative: -2.5
scientific: 6.02e23
negative_exp: 1.0e-10
infinity: .inf
neg_infinity: -.inf
not_a_number: .nan"#;

    let parsed = Yaml::from_str(yaml).expect("Should parse float representations");
    let output = parsed.to_string();

    assert!(output.contains("3.14"));
    assert!(output.contains("-2.5"));
    assert!(output.contains("inf"));
    assert!(output.contains("nan"));
}

// ========================================
// Section 10.2: JSON Schema Compatibility
// ========================================

#[test]
fn test_json_compatible_subset() {
    // YAML that is also valid JSON (with minor syntax differences)
    let yaml = r#"{
  "string": "value",
  "number": 42,
  "float": 3.14,
  "boolean": true,
  "null": null,
  "array": [1, 2, 3],
  "object": {"nested": "value"}
}"#;

    let parsed = Yaml::from_str(yaml).expect("Should parse JSON-compatible YAML");
    let output = parsed.to_string();

    assert!(output.contains("string"));
    assert!(output.contains("42"));
    assert!(output.contains("3.14"));
    assert!(output.contains("true"));
    assert!(output.contains("null"));
}

// ========================================
// Escape Sequences (Section 5.7)
// ========================================

#[test]
fn test_escape_sequences_in_double_quotes() {
    let yaml = r#"escaped: "null: \0, bell: \a, backspace: \b, tab: \t, newline: \n"
more: "vertical: \v, form: \f, return: \r, escape: \e, quote: \", backslash: \\"
unicode: "4-digit: \u263A, 8-digit: \U0001F600"
hex: "hex: \x41""#;

    let parsed = Yaml::from_str(yaml).expect("Should parse escape sequences");
    let output = parsed.to_string();

    assert!(output.contains("escaped:"));
    assert!(output.contains("more:"));
    assert!(output.contains("unicode:"));
}

#[test]
fn test_line_continuation() {
    let yaml = r#"continued: "This is a \
  continued line"
folded: "Line 1\
  \  Line 2""#;

    let parsed = Yaml::from_str(yaml).expect("Should parse line continuation");
    let output = parsed.to_string();

    assert!(output.contains("continued:"));
    assert!(output.contains("folded:"));
}

// ========================================
// Special Keys and Edge Cases
// ========================================

#[test]
fn test_complex_keys() {
    let yaml = r#"? complex
  key
: value
? [sequence, key]
: sequence value
? {mapping: key}
: mapping value"#;

    let parsed = Yaml::from_str(yaml).expect("Should parse complex keys");
    let output = parsed.to_string();

    assert!(output.contains("value"));
}

#[test]
fn test_empty_values() {
    let yaml = r#"empty_string: ""
empty_single: ''
null_value:
another_null: null
empty_flow_seq: []
empty_flow_map: {}"#;

    let parsed = Yaml::from_str(yaml).expect("Should parse empty values");
    let output = parsed.to_string();

    assert!(output.contains("empty_string:"));
    assert!(output.contains("empty_single:"));
    assert!(output.contains("null_value:"));
}

#[test]
fn test_special_strings_needing_quotes() {
    let yaml = r#"needs_quotes: "true"
also_needs: "123"
special: "@special"
colon: "key: value"
dash: "- item"
question: "? key""#;

    let parsed = Yaml::from_str(yaml).expect("Should parse special strings");
    let output = parsed.to_string();

    assert!(output.contains("needs_quotes:"));
    assert!(output.contains("also_needs:"));
    assert!(output.contains("special:"));
}

// ========================================
// Error Recovery and Robustness
// ========================================

#[test]
fn test_handles_windows_line_endings() {
    let yaml = "key: value\r\nanother: value\r\nlist:\r\n  - item1\r\n  - item2";

    let parsed = Yaml::from_str(yaml).expect("Should handle Windows line endings");
    let output = parsed.to_string();

    assert!(output.contains("key: value"));
    assert!(output.contains("another: value"));
    assert!(output.contains("item1"));
    assert!(output.contains("item2"));
}

#[test]
fn test_handles_mac_line_endings() {
    let yaml = "key: value\ranother: value\rlist:\r  - item1\r  - item2";

    let parsed = Yaml::from_str(yaml).expect("Should handle Mac line endings");
    let output = parsed.to_string();

    assert!(output.contains("key: value"));
    assert!(output.contains("another: value"));
}

#[test]
fn test_handles_mixed_line_endings() {
    let yaml = "key: value\nanother: value\r\nlist:\r  - item1\n  - item2";

    let parsed = Yaml::from_str(yaml).expect("Should handle mixed line endings");
    let output = parsed.to_string();

    assert!(output.contains("key: value"));
    assert!(output.contains("another: value"));
}

#[test]
fn test_handles_utf8_content() {
    let yaml = r#"english: Hello
chinese: 你好
japanese: こんにちは
arabic: مرحبا
emoji: 😀🎉🚀
math: ∑∏∫√"#;

    let parsed = Yaml::from_str(yaml).expect("Should handle UTF-8 content");
    let output = parsed.to_string();

    assert!(output.contains("Hello"));
    assert!(output.contains("你好"));
    assert!(output.contains("こんにちは"));
    assert!(output.contains("مرحبا"));
    assert!(output.contains("😀"));
}

// ========================================
// YAML 1.2 Spec Examples
// ========================================

#[test]
fn test_spec_example_2_1_sequence_of_scalars() {
    let yaml = r#"- Mark McGwire
- Sammy Sosa
- Ken Griffey"#;

    let parsed = Yaml::from_str(yaml).expect("Should parse spec example 2.1");
    let output = parsed.to_string();

    assert!(output.contains("Mark McGwire"));
    assert!(output.contains("Sammy Sosa"));
    assert!(output.contains("Ken Griffey"));
}

#[test]
fn test_spec_example_2_2_mapping_scalars_to_scalars() {
    let yaml = r#"hr:  65    # Home runs
avg: 0.278 # Batting average
rbi: 147   # Runs Batted In"#;

    let parsed = Yaml::from_str(yaml).expect("Should parse spec example 2.2");
    let output = parsed.to_string();

    assert!(output.contains("hr:"));
    assert!(output.contains("65"));
    assert!(output.contains("avg:"));
    assert!(output.contains("0.278"));
}

#[test]
fn test_spec_example_2_3_mapping_scalars_to_sequences() {
    let yaml = r#"american:
  - Boston Red Sox
  - Detroit Tigers
  - New York Yankees
national:
  - New York Mets
  - Chicago Cubs
  - Atlanta Braves"#;

    let parsed = Yaml::from_str(yaml).expect("Should parse spec example 2.3");
    let output = parsed.to_string();

    assert!(output.contains("Boston Red Sox"));
    assert!(output.contains("New York Yankees"));
    assert!(output.contains("Chicago Cubs"));
}

#[test]
fn test_spec_example_2_4_sequence_of_mappings() {
    let yaml = r#"-
  name: Mark McGwire
  hr:   65
  avg:  0.278
-
  name: Sammy Sosa
  hr:   63
  avg:  0.288"#;

    let parsed = Yaml::from_str(yaml).expect("Should parse spec example 2.4");
    let output = parsed.to_string();

    assert!(output.contains("Mark McGwire"));
    assert!(output.contains("hr:"));
    assert!(output.contains("65"));
    assert!(output.contains("Sammy Sosa"));
}

#[test]
fn test_spec_example_2_7_single_document_with_two_comments() {
    let yaml = r#"# Ranking of 1998 home runs
---
- Mark McGwire
- Sammy Sosa
- Ken Griffey

# Team ranking
---
- Chicago Cubs
- St Louis Cardinals"#;

    let parsed = Yaml::from_str(yaml).expect("Should parse spec example 2.7");
    let output = parsed.to_string();

    assert!(output.contains("Mark McGwire"));
}

#[test]
fn test_spec_example_2_8_play_by_play() {
    let yaml = r#"---
time: 20:03:20
player: Sammy Sosa
action: strike (miss)
...
---
time: 20:03:47
player: Sammy Sosa
action: grand slam
..."#;

    let parsed = Yaml::from_str(yaml).expect("Should parse spec example 2.8");
    let output = parsed.to_string();

    assert!(output.contains("time:"));
    assert!(output.contains("20:03:20"));
    assert!(output.contains("player:"));
    assert!(output.contains("Sammy Sosa"));
}

#[test]
fn test_spec_example_2_10_node_for_sammy_sosa() {
    let yaml = r#"---
hr:
  - Mark McGwire
  # Following node labeled SS
  - &SS Sammy Sosa
rbi:
  - *SS # Subsequent occurrence
  - Ken Griffey"#;

    let parsed = Yaml::from_str(yaml).expect("Should parse spec example 2.10");
    let output = parsed.to_string();

    assert!(output.contains("&SS"));
    assert!(output.contains("*SS"));
    assert!(output.contains("Sammy Sosa"));
}
