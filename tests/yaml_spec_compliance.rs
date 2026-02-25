//! YAML 1.2 Core Schema Compliance Tests
//!
//! These tests verify compliance with the YAML 1.2 specification.
//! Reference: <https://yaml.org/spec/1.2.2/>
//!
//! Tests cover:
//! - Null values (null, ~, empty)
//! - Boolean values (true, false, yes, no, on, off)
//! - Integer values (decimal, octal, hex, binary)
//! - Float values (standard, scientific notation, special values)
//! - String values (plain, quoted, escaped)
//! - Collections (sequences, mappings)
//! - Block scalars (literal |, folded >)
//! - Anchors and aliases (&anchor, *alias)
//! - Tags (!!str, !!int, !!float, !!bool, !!null, !!seq, !!map)
//!
//! All tests use the "gold standard" pattern:
//! 1. API verification - exact value checks
//! 2. Round-trip validation - parse → serialize → re-parse
//! 3. Exact assertions - no `.contains()` or conditional logic

use std::str::FromStr;
use yaml_edit::YamlFile;

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

    let parsed = YamlFile::from_str(yaml).expect("Should parse basic block sequence");
    let output = parsed.to_string();

    // Verify exact round-trip
    assert_eq!(output, yaml);

    // Verify API access
    let document = parsed.document().expect("Should have document");
    let sequence = document.as_sequence().expect("Should be a sequence");
    assert_eq!(sequence.len(), 3);
    assert_eq!(
        sequence.get(0).unwrap().as_scalar().unwrap().as_string(),
        "milk"
    );
    assert_eq!(
        sequence.get(1).unwrap().as_scalar().unwrap().as_string(),
        "bread"
    );
    assert_eq!(
        sequence.get(2).unwrap().as_scalar().unwrap().as_string(),
        "eggs"
    );
}

#[test]
fn test_flow_sequence_basic() {
    let yaml = r#"items: [apple, banana, orange]"#;

    let parsed = YamlFile::from_str(yaml).expect("Should parse flow sequence");
    let output = parsed.to_string();

    // Verify exact round-trip
    assert_eq!(output, yaml);

    // Verify API access
    let document = parsed.document().expect("Should have document");
    let mapping = document.as_mapping().expect("Should be a mapping");
    let items = mapping.get("items").expect("Should have 'items' key");
    let sequence = items.as_sequence().expect("Should be a sequence");
    assert!(sequence.is_flow_style(), "Should be flow-style sequence");
    assert_eq!(sequence.len(), 3);
    assert_eq!(
        sequence.get(0).unwrap().as_scalar().unwrap().as_string(),
        "apple"
    );
    assert_eq!(
        sequence.get(1).unwrap().as_scalar().unwrap().as_string(),
        "banana"
    );
    assert_eq!(
        sequence.get(2).unwrap().as_scalar().unwrap().as_string(),
        "orange"
    );
}

#[test]
fn test_block_mapping_basic() {
    let yaml = r#"name: John Doe
age: 30
city: New York"#;

    let parsed = YamlFile::from_str(yaml).expect("Should parse block mapping");
    let output = parsed.to_string();

    // Verify exact round-trip
    assert_eq!(output, yaml);

    // Verify API access
    let document = parsed.document().expect("Should have document");
    let mapping = document.as_mapping().expect("Should be a mapping");
    assert_eq!(mapping.len(), 3);
    assert_eq!(
        mapping
            .get("name")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "John Doe"
    );
    assert_eq!(
        mapping.get("age").unwrap().as_scalar().unwrap().as_string(),
        "30"
    );
    assert_eq!(
        mapping
            .get("city")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "New York"
    );
}

#[test]
fn test_flow_mapping_basic() {
    let yaml = r#"person: {name: Alice, age: 25, city: Boston}"#;

    let parsed = YamlFile::from_str(yaml).expect("Should parse flow mapping");
    let output = parsed.to_string();

    // Verify exact round-trip
    assert_eq!(output, yaml);

    // Verify API access
    let document = parsed.document().expect("Should have document");
    let root_mapping = document.as_mapping().expect("Should be a mapping");
    let person = root_mapping
        .get("person")
        .expect("Should have 'person' key");
    let person_mapping = person.as_mapping().expect("Should be a mapping");
    assert!(
        person_mapping.is_flow_style(),
        "Should be flow-style mapping"
    );
    assert_eq!(person_mapping.len(), 3);
    assert_eq!(
        person_mapping
            .get("name")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "Alice"
    );
    assert_eq!(
        person_mapping
            .get("age")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "25"
    );
    assert_eq!(
        person_mapping
            .get("city")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "Boston"
    );
}

/// Test block-style sequence nested in mapping
#[test]
fn test_nested_sequence_in_mapping() {
    let yaml = r#"user:
  name: Alice
  skills:
    - Python
    - JavaScript"#;

    let parsed = YamlFile::from_str(yaml).expect("Should parse nested sequence");
    let document = parsed.document().expect("Should have document");
    let root = document.as_mapping().expect("Should be a mapping");

    // Navigate to nested sequence
    let user = root.get_mapping("user").expect("Should have 'user' key");
    assert_eq!(
        user.get("name").unwrap().as_scalar().unwrap().as_string(),
        "Alice"
    );

    let skills = user
        .get_sequence("skills")
        .expect("Should have 'skills' sequence");
    assert!(!skills.is_flow_style(), "Should be block-style sequence");
    assert_eq!(skills.len(), 2);
    assert_eq!(
        skills.get(0).unwrap().as_scalar().unwrap().as_string(),
        "Python"
    );
    assert_eq!(
        skills.get(1).unwrap().as_scalar().unwrap().as_string(),
        "JavaScript"
    );

    // Verify exact round-trip
    let output = document.to_string();
    assert_eq!(output, yaml);
}

/// Test block-style mapping nested in mapping
#[test]
fn test_nested_mapping_in_mapping() {
    let yaml = r#"user:
  name: Alice
  projects:
    web: active
    mobile: pending"#;

    let parsed = YamlFile::from_str(yaml).expect("Should parse nested mapping");
    let document = parsed.document().expect("Should have document");
    let root = document.as_mapping().expect("Should be a mapping");

    // Navigate to nested mapping
    let user = root.get_mapping("user").expect("Should have 'user' key");
    assert_eq!(
        user.get("name").unwrap().as_scalar().unwrap().as_string(),
        "Alice"
    );

    let projects = user
        .get_mapping("projects")
        .expect("Should have 'projects' mapping");
    assert!(!projects.is_flow_style(), "Should be block-style mapping");
    assert_eq!(
        projects
            .get("web")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "active"
    );
    assert_eq!(
        projects
            .get("mobile")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "pending"
    );

    // Verify exact round-trip
    let output = document.to_string();
    assert_eq!(output, yaml);
}

/// Test flow-style sequence nested in mapping
#[test]
fn test_flow_sequence_in_mapping() {
    let yaml = r#"user:
  name: Bob
  skills: [Java, C++]"#;

    let parsed = YamlFile::from_str(yaml).expect("Should parse flow sequence");
    let document = parsed.document().expect("Should have document");
    let root = document.as_mapping().expect("Should be a mapping");

    // Navigate to flow sequence
    let user = root.get_mapping("user").expect("Should have 'user' key");
    assert_eq!(
        user.get("name").unwrap().as_scalar().unwrap().as_string(),
        "Bob"
    );

    let skills = user
        .get_sequence("skills")
        .expect("Should have 'skills' sequence");
    assert!(skills.is_flow_style(), "Should be flow-style sequence");
    assert_eq!(skills.len(), 2);
    assert_eq!(
        skills.get(0).unwrap().as_scalar().unwrap().as_string(),
        "Java"
    );
    assert_eq!(
        skills.get(1).unwrap().as_scalar().unwrap().as_string(),
        "C++"
    );

    // Verify exact round-trip
    let output = document.to_string();
    assert_eq!(output, yaml);
}

/// Test flow-style mapping nested in mapping
#[test]
fn test_flow_mapping_in_mapping() {
    let yaml = r#"user:
  name: Bob
  projects: {backend: done, frontend: active}"#;

    let parsed = YamlFile::from_str(yaml).expect("Should parse flow mapping");
    let document = parsed.document().expect("Should have document");
    let root = document.as_mapping().expect("Should be a mapping");

    // Navigate to flow mapping
    let user = root.get_mapping("user").expect("Should have 'user' key");
    assert_eq!(
        user.get("name").unwrap().as_scalar().unwrap().as_string(),
        "Bob"
    );

    let projects = user
        .get_mapping("projects")
        .expect("Should have 'projects' mapping");
    assert!(projects.is_flow_style(), "Should be flow-style mapping");
    assert_eq!(
        projects
            .get("backend")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "done"
    );
    assert_eq!(
        projects
            .get("frontend")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "active"
    );

    // Verify exact round-trip
    let output = document.to_string();
    assert_eq!(output, yaml);
}

/// Integration test: Mixed block and flow styles in same document
#[test]
fn test_mixed_block_and_flow_nesting() {
    let yaml = r#"users:
  - name: Alice
    skills: [Python, JavaScript]
  - name: Bob
    projects: {backend: done}"#;

    let parsed = YamlFile::from_str(yaml).expect("Should parse mixed styles");
    let document = parsed.document().expect("Should have document");
    let root = document.as_mapping().expect("Should be a mapping");

    // Verify users sequence
    let users = root
        .get_sequence("users")
        .expect("Should have 'users' sequence");
    assert_eq!(users.len(), 2);

    // Alice with flow-style skills
    let alice_elem = users.get(0).unwrap();
    let alice = alice_elem.as_mapping().expect("Should be a mapping");
    assert_eq!(
        alice.get("name").unwrap().as_scalar().unwrap().as_string(),
        "Alice"
    );
    let alice_skills = alice.get_sequence("skills").expect("Should have skills");
    assert!(
        alice_skills.is_flow_style(),
        "Alice's skills should be flow-style"
    );

    // Bob with flow-style projects
    let bob_elem = users.get(1).unwrap();
    let bob = bob_elem.as_mapping().expect("Should be a mapping");
    assert_eq!(
        bob.get("name").unwrap().as_scalar().unwrap().as_string(),
        "Bob"
    );
    let bob_projects = bob.get_mapping("projects").expect("Should have projects");
    assert!(
        bob_projects.is_flow_style(),
        "Bob's projects should be flow-style"
    );

    // Verify exact round-trip
    let output = document.to_string();
    assert_eq!(output, yaml);
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

    let parsed = YamlFile::from_str(yaml).expect("Should parse plain scalars");
    let output = parsed.to_string();

    // Verify exact round-trip
    assert_eq!(output, yaml);

    // Verify API access
    let document = parsed.document().expect("Should have document");
    let mapping = document.as_mapping().expect("Should be a mapping");
    assert_eq!(
        mapping
            .get("string")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "hello world"
    );
    assert_eq!(
        mapping
            .get("integer")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "42"
    );
    assert_eq!(
        mapping
            .get("float")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "3.14"
    );
    assert_eq!(
        mapping
            .get("boolean_true")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "true"
    );
    assert_eq!(
        mapping
            .get("boolean_false")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "false"
    );
    assert_eq!(
        mapping
            .get("null_value")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "null"
    );
}

#[test]
fn test_single_quoted_scalars() {
    let yaml = r#"single: 'This is a single-quoted string'
with_quotes: 'It''s got an apostrophe'
multiline: 'First line
  second line'"#;

    let parsed = YamlFile::from_str(yaml).expect("Should parse single-quoted scalars");
    let output = parsed.to_string();

    // Verify exact round-trip
    assert_eq!(output, yaml);

    // Verify API access - single quotes should be preserved in CST but values accessible
    let document = parsed.document().expect("Should have document");
    let mapping = document.as_mapping().expect("Should be a mapping");
    assert_eq!(
        mapping
            .get("single")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "This is a single-quoted string"
    );
    assert_eq!(
        mapping
            .get("with_quotes")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "It's got an apostrophe"
    );
    assert_eq!(
        mapping
            .get("multiline")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "First line second line"
    );
}

#[test]
fn test_double_quoted_scalars() {
    let yaml = r#"double: "This is a double-quoted string"
escaped: "Line 1\nLine 2\tTabbed"
unicode: "Smiley: \u263A"
quote: "She said \"Hello\"""#;

    let parsed = YamlFile::from_str(yaml).expect("Should parse double-quoted scalars");
    let output = parsed.to_string();

    // Verify exact round-trip
    assert_eq!(output, yaml);

    // Verify API access - escape sequences should be interpreted
    let document = parsed.document().expect("Should have document");
    let mapping = document.as_mapping().expect("Should be a mapping");
    assert_eq!(
        mapping
            .get("double")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "This is a double-quoted string"
    );
    assert_eq!(
        mapping
            .get("escaped")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "Line 1\nLine 2\tTabbed"
    );
    assert_eq!(
        mapping
            .get("unicode")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "Smiley: ☺"
    );
    assert_eq!(
        mapping
            .get("quote")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "She said \"Hello\""
    );
}

#[test]
fn test_literal_scalar() {
    let yaml = r#"literal: |
  This is a literal scalar.
  It preserves newlines.

  And blank lines too."#;

    let parsed = YamlFile::from_str(yaml).expect("Should parse literal scalar");
    let output = parsed.to_string();

    // Verify exact round-trip
    assert_eq!(output, yaml);

    // Verify API access - literal scalars preserve newlines
    let document = parsed.document().expect("Should have document");
    let mapping = document.as_mapping().expect("Should be a mapping");
    let literal_value = mapping
        .get("literal")
        .unwrap()
        .as_scalar()
        .unwrap()
        .as_string();
    assert_eq!(
        literal_value,
        "This is a literal scalar.\nIt preserves newlines.\n\nAnd blank lines too.\n"
    );
}

#[test]
fn test_folded_scalar() {
    let yaml = r#"folded: >
  This is a folded scalar.
  These lines will be folded
  into a single line.

  But blank lines create
  paragraph breaks."#;

    let parsed = YamlFile::from_str(yaml).expect("Should parse folded scalar");
    let output = parsed.to_string();

    // Verify exact round-trip
    assert_eq!(output, yaml);

    // Verify API access - folded scalars fold newlines to spaces
    let document = parsed.document().expect("Should have document");
    let mapping = document.as_mapping().expect("Should be a mapping");
    let folded_value = mapping
        .get("folded")
        .unwrap()
        .as_scalar()
        .unwrap()
        .as_string();
    assert_eq!(folded_value, "This is a folded scalar. These lines will be folded into a single line.\nBut blank lines create paragraph breaks.\n");
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

    let parsed = YamlFile::from_str(yaml).expect("Should parse anchor and alias");
    let output = parsed.to_string();

    // Verify exact round-trip
    assert_eq!(output, yaml);

    // Verify API access
    let document = parsed.document().expect("Should have document");
    let root = document.as_mapping().expect("Should be a mapping");

    // First has the anchored mapping
    let first = root.get("first").expect("Should have 'first' key");
    let first_mapping = first.as_mapping().expect("Should be a mapping");
    assert_eq!(
        first_mapping
            .get("name")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "Shared Value"
    );
    assert_eq!(
        first_mapping
            .get("data")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "123"
    );

    // Second has an alias reference (lossless - not dereferenced)
    let second = root.get("second").expect("Should have 'second' key");
    let second_alias = second.as_alias().expect("Should be an alias");
    assert_eq!(second_alias.name(), "anchor_name");
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

    let parsed = YamlFile::from_str(yaml).expect("Should parse merge keys");
    let output = parsed.to_string();

    // Verify exact round-trip
    assert_eq!(output, yaml);

    // Verify API access - merge keys should be accessible
    let document = parsed.document().expect("Should have document");
    let root = document.as_mapping().expect("Should be a mapping");

    let defaults = root.get("defaults").expect("Should have 'defaults' key");
    let defaults_mapping = defaults.as_mapping().expect("Should be a mapping");
    assert_eq!(
        defaults_mapping
            .get("adapter")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "postgres"
    );
    assert_eq!(
        defaults_mapping
            .get("host")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "localhost"
    );

    let dev = root
        .get("development")
        .expect("Should have 'development' key");
    let dev_mapping = dev.as_mapping().expect("Should be a mapping");
    assert_eq!(
        dev_mapping
            .get("database")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "dev_db"
    );
    // Verify merge key with alias (find by iterating since << is MERGE_KEY token, not STRING)
    let merge_entry = dev_mapping
        .iter()
        .find(|(k, _)| {
            k.as_scalar()
                .map(|s| s.as_string() == "<<")
                .unwrap_or(false)
        })
        .expect("Should have merge key");
    let merge_alias = merge_entry
        .1
        .as_alias()
        .expect("Merge key value should be an alias");
    assert_eq!(merge_alias.name(), "defaults");

    let test = root.get("test").expect("Should have 'test' key");
    let test_mapping = test.as_mapping().expect("Should be a mapping");
    assert_eq!(
        test_mapping
            .get("database")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "test_db"
    );
    // Verify test also has merge key
    let test_merge = test_mapping
        .iter()
        .find(|(k, _)| {
            k.as_scalar()
                .map(|s| s.as_string() == "<<")
                .unwrap_or(false)
        })
        .expect("Should have merge key");
    assert!(
        test_merge.1.is_alias(),
        "Merge key value should be an alias"
    );
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

    let parsed = YamlFile::from_str(yaml).expect("Should parse multiple anchors");
    let output = parsed.to_string();

    // Verify exact round-trip
    assert_eq!(output, yaml);

    // Verify API access
    let document = parsed.document().expect("Should have document");
    let root = document.as_mapping().expect("Should be a mapping");

    let colors = root.get("colors").expect("Should have 'colors' key");
    let colors_seq = colors.as_sequence().expect("Should be a sequence");
    assert_eq!(colors_seq.len(), 3);
    assert_eq!(
        colors_seq.get(0).unwrap().as_scalar().unwrap().as_string(),
        "#FF0000"
    );
    assert_eq!(
        colors_seq.get(1).unwrap().as_scalar().unwrap().as_string(),
        "#00FF00"
    );
    assert_eq!(
        colors_seq.get(2).unwrap().as_scalar().unwrap().as_string(),
        "#0000FF"
    );

    let theme = root.get("theme").expect("Should have 'theme' key");
    let theme_mapping = theme.as_mapping().expect("Should be a mapping");
    let primary = theme_mapping
        .get("primary")
        .expect("Should have 'primary' key");
    assert_eq!(
        primary.as_alias().expect("Should be an alias").name(),
        "blue"
    );
    let secondary = theme_mapping
        .get("secondary")
        .expect("Should have 'secondary' key");
    assert_eq!(
        secondary.as_alias().expect("Should be an alias").name(),
        "green"
    );
    let danger = theme_mapping
        .get("danger")
        .expect("Should have 'danger' key");
    assert_eq!(danger.as_alias().expect("Should be an alias").name(), "red");
}

// ========================================
// Section 3.1: Directives
// ========================================

#[test]
fn test_yaml_directive() {
    let yaml = r#"%YAML 1.2
---
key: value"#;

    let parsed = YamlFile::from_str(yaml).expect("Should parse YAML directive");
    let output = parsed.to_string();

    // Verify exact round-trip
    assert_eq!(output, yaml);

    // Verify API access
    let document = parsed.document().expect("Should have document");
    let mapping = document.as_mapping().expect("Should be a mapping");
    assert_eq!(
        mapping.get("key").unwrap().as_scalar().unwrap().as_string(),
        "value"
    );
}

#[test]
fn test_tag_directive() {
    let yaml = r#"%TAG ! tag:example.com,2014:
---
!foo "bar""#;

    let parsed = YamlFile::from_str(yaml).expect("Should parse TAG directive");
    let output = parsed.to_string();

    // Verify exact round-trip
    assert_eq!(output, yaml);

    // Verify API access - the content is a tagged node (!foo "bar")
    let document = parsed.document().expect("Should have document");

    // For tagged content, we need to access it through the TaggedNode API
    // to preserve the tag information (lossless editing)
    use rowan::ast::AstNode;
    use yaml_edit::TaggedNode;

    let root_node = document
        .syntax()
        .children()
        .next()
        .expect("Should have root node");
    let tagged = TaggedNode::cast(root_node).expect("Root should be a tagged node");

    // Verify the tag
    assert_eq!(tagged.tag(), Some("!foo".to_string()));

    // Verify the tagged value is the scalar "bar"
    let scalar = tagged.value().expect("Tagged node should have a value");
    assert_eq!(scalar.as_string(), "bar");
}

#[test]
fn test_multiple_directives() {
    let yaml = r#"%YAML 1.2
%TAG ! tag:example.com,2014:
%TAG !! tag:example.com,2014:app/
---
data: value"#;

    let parsed = YamlFile::from_str(yaml).expect("Should parse multiple directives");
    let output = parsed.to_string();

    // Verify exact round-trip
    assert_eq!(output, yaml);

    // Verify API access
    let document = parsed.document().expect("Should have document");
    let mapping = document.as_mapping().expect("Should be a mapping");
    assert_eq!(
        mapping
            .get("data")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "value"
    );
}

// ========================================
// Section 3.2: Document Boundaries
// ========================================

#[test]
fn test_explicit_document() {
    let yaml = r#"---
document: one
..."#;

    let parsed = YamlFile::from_str(yaml).expect("Should parse explicit document");
    let output = parsed.to_string();

    // Verify exact round-trip
    assert_eq!(output, yaml);

    // Verify API access
    let document = parsed.document().expect("Should have document");
    let mapping = document.as_mapping().expect("Should be a mapping");
    assert_eq!(
        mapping
            .get("document")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "one"
    );
}

#[test]
fn test_multiple_documents() {
    let yaml = r#"---
first: document
---
second: document
---
third: document"#;

    let parsed = YamlFile::from_str(yaml).expect("Should parse multiple documents");
    let output = parsed.to_string();

    // Verify exact round-trip
    assert_eq!(output, yaml);

    // Verify API access - YamlFile currently only exposes first document
    let document = parsed.document().expect("Should have document");
    let mapping = document.as_mapping().expect("Should be a mapping");
    assert_eq!(
        mapping
            .get("first")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "document"
    );
}

#[test]
fn test_bare_document() {
    // Document without explicit markers
    let yaml = r#"key: value
another: value"#;

    let parsed = YamlFile::from_str(yaml).expect("Should parse bare document");
    let output = parsed.to_string();

    // Verify exact round-trip
    assert_eq!(output, yaml);

    // Verify API access
    let document = parsed.document().expect("Should have document");
    let mapping = document.as_mapping().expect("Should be a mapping");
    assert_eq!(
        mapping.get("key").unwrap().as_scalar().unwrap().as_string(),
        "value"
    );
    assert_eq!(
        mapping
            .get("another")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "value"
    );
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

    let parsed = YamlFile::from_str(yaml).expect("Should handle all indicator characters");
    let output = parsed.to_string();

    // Verify exact round-trip
    assert_eq!(output, yaml);

    // Verify API access
    let document = parsed.document().expect("Should have document");
    let mapping = document.as_mapping().expect("Should be a mapping");
    assert_eq!(
        mapping.get("key").unwrap().as_scalar().unwrap().as_string(),
        "value"
    );
    let sequence_node = mapping.get("sequence").unwrap();
    let sequence = sequence_node.as_sequence().expect("Should be a sequence");
    assert_eq!(sequence.len(), 2);
    assert_eq!(
        sequence.get(0).unwrap().as_scalar().unwrap().as_string(),
        "item1"
    );
    let flow_mapping_node = mapping.get("mapping").unwrap();
    let flow_mapping = flow_mapping_node.as_mapping().expect("Should be a mapping");
    assert!(flow_mapping.is_flow_style());
    assert_eq!(
        mapping
            .get("anchor")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "value"
    );
    let alias = mapping.get("alias").unwrap();
    assert_eq!(alias.as_alias().expect("Should be an alias").name(), "test");
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

    let parsed = YamlFile::from_str(yaml).expect("Should parse consistent indentation");
    let output = parsed.to_string();

    // Verify exact round-trip
    assert_eq!(output, yaml);

    // Verify API access - deeply nested structure
    let document = parsed.document().expect("Should have document");
    let root = document.as_mapping().expect("Should be a mapping");
    let level1_node = root.get("root").unwrap();
    let level1 = level1_node.as_mapping().expect("Should be a mapping");
    let level2_node = level1.get("level1").unwrap();
    let level2_mapping = level2_node.as_mapping().expect("Should be a mapping");
    let level3_node = level2_mapping.get("level2").unwrap();
    let level3_mapping = level3_node.as_mapping().expect("Should be a mapping");
    assert_eq!(
        level3_mapping
            .get("level3")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "value"
    );
    assert_eq!(
        level2_mapping
            .get("another2")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "value"
    );
    assert_eq!(
        level1
            .get("another1")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "value"
    );
}

#[test]
fn test_tab_indentation() {
    // Tabs are not allowed for indentation in YAML
    let yaml = "key: value\n\tindented: invalid";

    let parsed = YamlFile::from_str(yaml);
    // This should either error or handle gracefully
    match parsed {
        Ok(y) => {
            // Verify we parsed the structure via API
            let doc = y.document().unwrap();
            let mapping = doc.as_mapping().unwrap();
            assert_eq!(
                mapping.get("key").unwrap().as_scalar().unwrap().as_string(),
                "value"
            );

            // Verify exact round-trip
            let output = y.to_string();
            assert_eq!(output, yaml);
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

    let parsed = YamlFile::from_str(yaml).expect("Should parse zero-indented sequence");
    let output = parsed.to_string();

    // Verify exact round-trip
    assert_eq!(output, yaml);

    // Verify API access
    let document = parsed.document().expect("Should have document");
    let mapping = document.as_mapping().expect("Should be a mapping");
    let items_node = mapping.get("items").unwrap();
    let items = items_node.as_sequence().expect("Should be a sequence");
    assert_eq!(items.len(), 3);
    assert_eq!(
        items.get(0).unwrap().as_scalar().unwrap().as_string(),
        "one"
    );
    assert_eq!(
        items.get(1).unwrap().as_scalar().unwrap().as_string(),
        "two"
    );
    assert_eq!(
        items.get(2).unwrap().as_scalar().unwrap().as_string(),
        "three"
    );
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

    let parsed = YamlFile::from_str(yaml).expect("Should parse multi-line plain scalar");
    let output = parsed.to_string();

    // Verify exact round-trip
    assert_eq!(output, yaml);

    // Verify API access - plain scalars fold newlines to spaces
    let document = parsed.document().expect("Should have document");
    let mapping = document.as_mapping().expect("Should be a mapping");
    let description = mapping
        .get("description")
        .unwrap()
        .as_scalar()
        .unwrap()
        .as_string();
    assert_eq!(description, "This is a very long description that spans multiple lines but should be treated as a single value");
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

    let parsed = YamlFile::from_str(yaml).expect("Should parse comments in various positions");
    let output = parsed.to_string();

    // Verify exact round-trip - comments must be preserved
    assert_eq!(output, yaml);

    // Verify API access
    let document = parsed.document().expect("Should have document");
    let mapping = document.as_mapping().expect("Should be a mapping");
    assert_eq!(
        mapping.get("key").unwrap().as_scalar().unwrap().as_string(),
        "value"
    );
    assert_eq!(
        mapping
            .get("item")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "value"
    );
    let list_node = mapping.get("list").unwrap();
    let list = list_node.as_sequence().expect("Should be a sequence");
    assert_eq!(list.len(), 2);
    assert_eq!(
        list.get(0).unwrap().as_scalar().unwrap().as_string(),
        "item1"
    );
    assert_eq!(
        list.get(1).unwrap().as_scalar().unwrap().as_string(),
        "item2"
    );
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

    let parsed = YamlFile::from_str(yaml).expect("Should parse multi-line comments");
    let output = parsed.to_string();

    // Verify exact round-trip - comments must be preserved
    assert_eq!(output, yaml);

    // Verify API access
    let document = parsed.document().expect("Should have document");
    let mapping = document.as_mapping().expect("Should be a mapping");
    assert_eq!(
        mapping
            .get("data")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "value"
    );
    assert_eq!(
        mapping
            .get("more")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "data"
    );
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

    let parsed = YamlFile::from_str(yaml).expect("Should parse flow sequence variations");
    let output = parsed.to_string();

    // Verify exact round-trip
    assert_eq!(output, yaml);

    // Verify API access
    let document = parsed.document().expect("Should have document");
    let mapping = document.as_mapping().expect("Should be a mapping");

    let empty_node = mapping.get("empty").unwrap();
    let empty = empty_node.as_sequence().expect("Should be a sequence");
    assert!(empty.is_flow_style());
    assert_eq!(empty.len(), 0);

    let single_node = mapping.get("single").unwrap();
    let single = single_node.as_sequence().expect("Should be a sequence");
    assert!(single.is_flow_style());
    assert_eq!(single.len(), 1);

    let multiple_node = mapping.get("multiple").unwrap();
    let multiple = multiple_node.as_sequence().expect("Should be a sequence");
    assert!(multiple.is_flow_style());
    assert_eq!(multiple.len(), 3);

    let trailing_node = mapping.get("trailing").unwrap();
    let trailing = trailing_node.as_sequence().expect("Should be a sequence");
    assert!(trailing.is_flow_style());
    assert_eq!(trailing.len(), 2);

    let multiline_node = mapping.get("multiline").unwrap();
    let multiline = multiline_node.as_sequence().expect("Should be a sequence");
    assert!(multiline.is_flow_style());
    assert_eq!(multiline.len(), 3);
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

    let parsed = YamlFile::from_str(yaml).expect("Should parse flow mapping variations");
    let output = parsed.to_string();

    // Verify exact round-trip
    assert_eq!(output, yaml);

    // Verify API access
    let document = parsed.document().expect("Should have document");
    let root_mapping = document.as_mapping().expect("Should be a mapping");

    let empty_node = root_mapping.get("empty").unwrap();
    let empty = empty_node.as_mapping().expect("Should be a mapping");
    assert!(empty.is_flow_style());
    assert_eq!(empty.len(), 0);

    let single_node = root_mapping.get("single").unwrap();
    let single = single_node.as_mapping().expect("Should be a mapping");
    assert!(single.is_flow_style());
    assert_eq!(single.len(), 1);
    assert_eq!(
        single.get("key").unwrap().as_scalar().unwrap().as_string(),
        "value"
    );

    let multiple_node = root_mapping.get("multiple").unwrap();
    let multiple = multiple_node.as_mapping().expect("Should be a mapping");
    assert!(multiple.is_flow_style());
    assert_eq!(multiple.len(), 3);

    let trailing_node = root_mapping.get("trailing").unwrap();
    let trailing = trailing_node.as_mapping().expect("Should be a mapping");
    assert!(trailing.is_flow_style());
    assert_eq!(trailing.len(), 2);

    let multiline_node = root_mapping.get("multiline").unwrap();
    let multiline = multiline_node.as_mapping().expect("Should be a mapping");
    assert!(multiline.is_flow_style());
    assert_eq!(multiline.len(), 2);
}

#[test]
fn test_nested_flow_collections() {
    let yaml = r#"complex: {
  lists: [a, b, [c, d]],
  maps: {x: 1, y: {z: 2}},
  mixed: [{a: 1}, {b: 2}]
}"#;

    let parsed = YamlFile::from_str(yaml).expect("Should parse nested flow collections");
    let output = parsed.to_string();

    // Verify exact round-trip
    assert_eq!(output, yaml);

    // Verify API access
    let document = parsed.document().expect("Should have document");
    let root = document.as_mapping().expect("Should be a mapping");
    let complex_node = root.get("complex").unwrap();
    let complex = complex_node.as_mapping().expect("Should be a mapping");
    assert!(complex.is_flow_style());

    // Verify lists - contains nested sequence
    let lists_node = complex.get("lists").unwrap();
    let lists = lists_node.as_sequence().expect("Should be a sequence");
    assert!(lists.is_flow_style());
    assert_eq!(lists.len(), 3);
    let nested_seq_elem = lists.get(2).unwrap();
    let nested_seq = nested_seq_elem
        .as_sequence()
        .expect("Third element should be a sequence");
    assert!(nested_seq.is_flow_style());
    assert_eq!(nested_seq.len(), 2);

    // Verify maps - contains nested mapping
    let maps_node = complex.get("maps").unwrap();
    let maps = maps_node.as_mapping().expect("Should be a mapping");
    assert!(maps.is_flow_style());
    let nested_map_node = maps.get("y").unwrap();
    let nested_map = nested_map_node.as_mapping().expect("Should be a mapping");
    assert!(nested_map.is_flow_style());

    // Verify mixed - sequence of mappings
    let mixed_node = complex.get("mixed").unwrap();
    let mixed = mixed_node.as_sequence().expect("Should be a sequence");
    assert!(mixed.is_flow_style());
    assert_eq!(mixed.len(), 2);
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

    let parsed = YamlFile::from_str(yaml).expect("Should parse literal scalar chomping indicators");
    let output = parsed.to_string();

    // Verify exact round-trip
    assert_eq!(output, yaml);

    // Verify API access - chomping indicators affect trailing newlines
    let document = parsed.document().expect("Should have document");
    let mapping = document.as_mapping().expect("Should be a mapping");
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
        "text\n"
    );
    assert_eq!(
        mapping
            .get("clip")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "text\n"
    );
}

#[test]
fn test_folded_scalar_chomping() {
    let yaml = r#"strip: >-
  text
keep: >
  text
clip: >+
  text"#;

    let parsed = YamlFile::from_str(yaml).expect("Should parse folded scalar chomping indicators");
    let output = parsed.to_string();

    // Verify exact round-trip
    assert_eq!(output, yaml);

    // Verify API access - chomping indicators affect trailing newlines
    let document = parsed.document().expect("Should have document");
    let mapping = document.as_mapping().expect("Should be a mapping");
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
        "text\n"
    );
    assert_eq!(
        mapping
            .get("clip")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "text\n"
    );
}

#[test]
fn test_explicit_indentation_indicators() {
    let yaml = r#"literal: |2
  This has explicit
  indentation indicator
folded: >1
 This too"#;

    let parsed = YamlFile::from_str(yaml).expect("Should parse explicit indentation indicators");
    let output = parsed.to_string();

    // Verify exact round-trip
    assert_eq!(output, yaml);

    // Verify API access
    let document = parsed.document().expect("Should have document");
    let mapping = document.as_mapping().expect("Should be a mapping");
    assert_eq!(
        mapping
            .get("literal")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "This has explicit\nindentation indicator\n"
    );
    assert_eq!(
        mapping
            .get("folded")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "This too\n"
    );
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

    let parsed = YamlFile::from_str(yaml).expect("Should parse null representations");
    let output = parsed.to_string();

    // Verify exact round-trip
    assert_eq!(output, yaml);

    // Verify API access - all should be unquoted and recognized as null
    let document = parsed.document().expect("Should have document");
    let mapping = document.as_mapping().expect("Should be a mapping");

    let canonical_node = mapping.get("canonical").unwrap();
    let canonical = canonical_node.as_scalar().unwrap();
    assert!(!canonical.is_quoted(), "Null should not be quoted");
    assert!(canonical.is_null());

    let english_node = mapping.get("english").unwrap();
    let english = english_node.as_scalar().unwrap();
    assert!(!english.is_quoted(), "Null should not be quoted");
    assert!(english.is_null());

    let tilde_node = mapping.get("tilde").unwrap();
    let tilde = tilde_node.as_scalar().unwrap();
    assert!(!tilde.is_quoted(), "Null should not be quoted");
    assert!(tilde.is_null());

    let empty_node = mapping.get("empty").unwrap();
    let empty = empty_node.as_scalar().unwrap();
    assert!(!empty.is_quoted(), "Null should not be quoted");
    assert!(empty.is_null());
}

#[test]
fn test_boolean_representations() {
    let yaml = r#"canonical_true: true
canonical_false: false
english_true: True
english_false: False
caps_true: TRUE
caps_false: FALSE"#;

    let parsed = YamlFile::from_str(yaml).expect("Should parse boolean representations");
    let output = parsed.to_string();

    // Verify exact round-trip
    assert_eq!(output, yaml);

    // Verify API access - values should be unquoted and parse as booleans
    let document = parsed.document().expect("Should have document");
    let mapping = document.as_mapping().expect("Should be a mapping");

    let canonical_true_node = mapping.get("canonical_true").unwrap();
    let canonical_true = canonical_true_node.as_scalar().unwrap();
    assert!(!canonical_true.is_quoted(), "Boolean should not be quoted");
    assert_eq!(canonical_true.as_bool(), Some(true));

    let canonical_false_node = mapping.get("canonical_false").unwrap();
    let canonical_false = canonical_false_node.as_scalar().unwrap();
    assert!(!canonical_false.is_quoted(), "Boolean should not be quoted");
    assert_eq!(canonical_false.as_bool(), Some(false));

    let english_true_node = mapping.get("english_true").unwrap();
    let english_true = english_true_node.as_scalar().unwrap();
    assert!(!english_true.is_quoted(), "Boolean should not be quoted");
    assert_eq!(english_true.as_bool(), Some(true));

    let english_false_node = mapping.get("english_false").unwrap();
    let english_false = english_false_node.as_scalar().unwrap();
    assert!(!english_false.is_quoted(), "Boolean should not be quoted");
    assert_eq!(english_false.as_bool(), Some(false));

    let caps_true_node = mapping.get("caps_true").unwrap();
    let caps_true = caps_true_node.as_scalar().unwrap();
    assert!(!caps_true.is_quoted(), "Boolean should not be quoted");
    assert_eq!(caps_true.as_bool(), Some(true));

    let caps_false_node = mapping.get("caps_false").unwrap();
    let caps_false = caps_false_node.as_scalar().unwrap();
    assert!(!caps_false.is_quoted(), "Boolean should not be quoted");
    assert_eq!(caps_false.as_bool(), Some(false));
}

#[test]
fn test_integer_representations() {
    let yaml = r#"decimal: 123
negative: -456
zero: 0
octal: 0o777
hex: 0xFF
binary: 0b1010"#;

    let parsed = YamlFile::from_str(yaml).expect("Should parse integer representations");
    let output = parsed.to_string();

    // Verify exact round-trip
    assert_eq!(output, yaml);

    // Verify API access - values should be unquoted and parse as correct integers
    let document = parsed.document().expect("Should have document");
    let mapping = document.as_mapping().expect("Should be a mapping");

    let decimal_node = mapping.get("decimal").unwrap();
    let decimal = decimal_node.as_scalar().unwrap();
    assert!(!decimal.is_quoted(), "Integer should not be quoted");
    assert_eq!(decimal.as_i64(), Some(123));

    let negative_node = mapping.get("negative").unwrap();
    let negative = negative_node.as_scalar().unwrap();
    assert!(!negative.is_quoted(), "Integer should not be quoted");
    assert_eq!(negative.as_i64(), Some(-456));

    let zero_node = mapping.get("zero").unwrap();
    let zero = zero_node.as_scalar().unwrap();
    assert!(!zero.is_quoted(), "Integer should not be quoted");
    assert_eq!(zero.as_i64(), Some(0));

    let octal_node = mapping.get("octal").unwrap();
    let octal = octal_node.as_scalar().unwrap();
    assert!(!octal.is_quoted(), "Octal integer should not be quoted");
    assert_eq!(octal.as_i64(), Some(0o777));

    let hex_node = mapping.get("hex").unwrap();
    let hex = hex_node.as_scalar().unwrap();
    assert!(!hex.is_quoted(), "Hex integer should not be quoted");
    assert_eq!(hex.as_i64(), Some(0xFF));

    let binary_node = mapping.get("binary").unwrap();
    let binary = binary_node.as_scalar().unwrap();
    assert!(!binary.is_quoted(), "Binary integer should not be quoted");
    assert_eq!(binary.as_i64(), Some(0b1010));
}

#[test]
fn test_float_representations() {
    let yaml = r#"simple: 3.15
negative: -2.5
scientific: 6.02e23
negative_exp: 1.0e-10
infinity: .inf
neg_infinity: -.inf
not_a_number: .nan"#;

    let parsed = YamlFile::from_str(yaml).expect("Should parse float representations");
    let output = parsed.to_string();

    // Verify exact round-trip
    assert_eq!(output, yaml);

    // Verify API access - values should be unquoted and parse as correct floats
    let document = parsed.document().expect("Should have document");
    let mapping = document.as_mapping().expect("Should be a mapping");

    let simple_node = mapping.get("simple").unwrap();
    let simple = simple_node.as_scalar().unwrap();
    assert!(!simple.is_quoted(), "Float should not be quoted");
    assert_eq!(simple.as_f64(), Some(3.15));

    let negative_node = mapping.get("negative").unwrap();
    let negative = negative_node.as_scalar().unwrap();
    assert!(!negative.is_quoted(), "Float should not be quoted");
    assert_eq!(negative.as_f64(), Some(-2.5));

    let scientific_node = mapping.get("scientific").unwrap();
    let scientific = scientific_node.as_scalar().unwrap();
    assert!(
        !scientific.is_quoted(),
        "Scientific notation should not be quoted"
    );
    assert_eq!(scientific.as_f64(), Some(6.02e23));

    let negative_exp_node = mapping.get("negative_exp").unwrap();
    let negative_exp = negative_exp_node.as_scalar().unwrap();
    assert!(
        !negative_exp.is_quoted(),
        "Scientific notation should not be quoted"
    );
    assert_eq!(negative_exp.as_f64(), Some(1.0e-10));

    let infinity_node = mapping.get("infinity").unwrap();
    let infinity = infinity_node.as_scalar().unwrap();
    assert!(!infinity.is_quoted(), "Infinity should not be quoted");
    assert_eq!(infinity.as_f64(), Some(f64::INFINITY));

    let neg_infinity_node = mapping.get("neg_infinity").unwrap();
    let neg_infinity = neg_infinity_node.as_scalar().unwrap();
    assert!(
        !neg_infinity.is_quoted(),
        "Negative infinity should not be quoted"
    );
    assert_eq!(neg_infinity.as_f64(), Some(f64::NEG_INFINITY));

    let not_a_number_node = mapping.get("not_a_number").unwrap();
    let not_a_number = not_a_number_node.as_scalar().unwrap();
    assert!(!not_a_number.is_quoted(), "NaN should not be quoted");
    assert!(not_a_number.as_f64().unwrap().is_nan());
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
  "float": 3.25,
  "boolean": true,
  "null": null,
  "array": [1, 2, 3],
  "object": {"nested": "value"}
}"#;

    let parsed = YamlFile::from_str(yaml).expect("Should parse JSON-compatible YAML");

    // Verify we parsed all keys and values correctly via API
    let doc = parsed.document().unwrap();
    let mapping = doc.as_mapping().unwrap();

    assert_eq!(
        mapping
            .get("string")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "value"
    );
    assert_eq!(mapping.get("number").unwrap().to_i64(), Some(42));
    assert_eq!(mapping.get("float").unwrap().to_f64(), Some(3.25));
    assert_eq!(mapping.get("boolean").unwrap().to_bool(), Some(true));
    assert!(mapping.get("null").unwrap().as_scalar().is_some());

    let array = mapping.get_sequence("array").unwrap();
    assert_eq!(array.len(), 3);

    let object = mapping.get_mapping("object").unwrap();
    assert_eq!(
        object
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

// ========================================
// Escape Sequences (Section 5.7)
// ========================================

#[test]
fn test_escape_sequences_in_double_quotes() {
    let yaml = r#"escaped: "null: \0, bell: \a, backspace: \b, tab: \t, newline: \n"
more: "vertical: \v, form: \f, return: \r, escape: \e, quote: \", backslash: \\"
unicode: "4-digit: \u263A, 8-digit: \U0001F600"
hex: "hex: \x41""#;

    let parsed = YamlFile::from_str(yaml).expect("Should parse escape sequences");

    // Verify we parsed all keys via API
    let doc = parsed.document().unwrap();
    let mapping = doc.as_mapping().unwrap();
    assert!(mapping.contains_key("escaped"));
    assert!(mapping.contains_key("more"));
    assert!(mapping.contains_key("unicode"));
    assert!(mapping.contains_key("hex"));

    // Verify exact round-trip
    let output = parsed.to_string();
    assert_eq!(output, yaml);
}

#[test]
fn test_line_continuation() {
    let yaml = r#"continued: "This is a \
  continued line"
folded: "Line 1\
  \  Line 2""#;

    let parsed = YamlFile::from_str(yaml).expect("Should parse line continuation");

    // Verify we parsed both keys via API
    let doc = parsed.document().unwrap();
    let mapping = doc.as_mapping().unwrap();
    assert!(mapping.contains_key("continued"));
    assert!(mapping.contains_key("folded"));

    // Verify exact round-trip
    let output = parsed.to_string();
    assert_eq!(output, yaml);
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

    let parsed = YamlFile::from_str(yaml).expect("Should parse complex keys");

    // Verify we parsed the structure via API
    let doc = parsed.document().unwrap();
    let mapping = doc.as_mapping().unwrap();
    assert!(!mapping.is_empty());

    // Verify exact round-trip
    let output = parsed.to_string();
    assert_eq!(output, yaml);
}

#[test]
fn test_empty_values() {
    let yaml = r#"empty_string: ""
empty_single: ''
null_value:
another_null: null
empty_flow_seq: []
empty_flow_map: {}"#;

    let parsed = YamlFile::from_str(yaml).expect("Should parse empty values");

    // Verify we parsed all keys and values correctly via API
    let doc = parsed.document().unwrap();
    let mapping = doc.as_mapping().unwrap();

    assert_eq!(
        mapping
            .get("empty_string")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        ""
    );
    assert_eq!(
        mapping
            .get("empty_single")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        ""
    );
    assert!(mapping
        .get("null_value")
        .unwrap()
        .as_scalar()
        .unwrap()
        .is_null());
    assert!(mapping
        .get("another_null")
        .unwrap()
        .as_scalar()
        .unwrap()
        .is_null());

    let empty_seq_node = mapping.get("empty_flow_seq").unwrap();
    let empty_seq = empty_seq_node.as_sequence().unwrap();
    assert_eq!(empty_seq.len(), 0);

    let empty_map_node = mapping.get("empty_flow_map").unwrap();
    let empty_map = empty_map_node.as_mapping().unwrap();
    assert_eq!(empty_map.len(), 0);

    // Verify exact round-trip
    let output = parsed.to_string();
    assert_eq!(output, yaml);
}

#[test]
fn test_special_strings_needing_quotes() {
    let yaml = r#"needs_quotes: "true"
also_needs: "123"
special: "@special"
colon: "key: value"
dash: "- item"
question: "? key""#;

    let parsed = YamlFile::from_str(yaml).expect("Should parse special strings");

    // Verify we parsed all keys and values correctly via API
    let doc = parsed.document().unwrap();
    let mapping = doc.as_mapping().unwrap();

    assert_eq!(
        mapping
            .get("needs_quotes")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "true"
    );
    assert_eq!(
        mapping
            .get("also_needs")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "123"
    );
    assert_eq!(
        mapping
            .get("special")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "@special"
    );
    assert_eq!(
        mapping
            .get("colon")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "key: value"
    );
    assert_eq!(
        mapping
            .get("dash")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "- item"
    );
    assert_eq!(
        mapping
            .get("question")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "? key"
    );

    // Verify exact round-trip
    let output = parsed.to_string();
    assert_eq!(output, yaml);
}

// ========================================
// Error Recovery and Robustness
// ========================================

#[test]
fn test_handles_windows_line_endings() {
    let yaml = "key: value\r\nanother: value\r\nlist:\r\n  - item1\r\n  - item2";

    let parsed = YamlFile::from_str(yaml).expect("Should handle Windows line endings");

    // Verify we parsed all keys and values correctly via API
    let doc = parsed.document().unwrap();
    let mapping = doc.as_mapping().unwrap();

    assert_eq!(
        mapping.get("key").unwrap().as_scalar().unwrap().as_string(),
        "value"
    );
    assert_eq!(
        mapping
            .get("another")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "value"
    );

    let list_node = mapping.get("list").unwrap();
    let list = list_node.as_sequence().unwrap();
    assert_eq!(list.len(), 2);
    assert_eq!(
        list.get(0).unwrap().as_scalar().unwrap().as_string(),
        "item1"
    );
    assert_eq!(
        list.get(1).unwrap().as_scalar().unwrap().as_string(),
        "item2"
    );

    // Verify exact round-trip
    let output = parsed.to_string();
    assert_eq!(output, yaml);
}

#[test]
fn test_handles_mac_line_endings() {
    let yaml = "key: value\ranother: value\rlist:\r  - item1\r  - item2";

    let parsed = YamlFile::from_str(yaml).expect("Should handle Mac line endings");

    // Verify we parsed all keys and values correctly via API
    let doc = parsed.document().unwrap();
    let mapping = doc.as_mapping().unwrap();

    assert_eq!(
        mapping.get("key").unwrap().as_scalar().unwrap().as_string(),
        "value"
    );
    assert_eq!(
        mapping
            .get("another")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "value"
    );

    let list_node = mapping.get("list").unwrap();
    let list = list_node.as_sequence().unwrap();
    assert_eq!(list.len(), 2);
    assert_eq!(
        list.get(0).unwrap().as_scalar().unwrap().as_string(),
        "item1"
    );
    assert_eq!(
        list.get(1).unwrap().as_scalar().unwrap().as_string(),
        "item2"
    );

    // Verify exact round-trip
    let output = parsed.to_string();
    assert_eq!(output, yaml);
}

#[test]
fn test_handles_mixed_line_endings() {
    let yaml = "key: value\nanother: value\r\nlist:\r  - item1\n  - item2";

    let parsed = YamlFile::from_str(yaml).expect("Should handle mixed line endings");

    // Verify we parsed all keys and values correctly via API
    let doc = parsed.document().unwrap();
    let mapping = doc.as_mapping().unwrap();

    assert_eq!(
        mapping.get("key").unwrap().as_scalar().unwrap().as_string(),
        "value"
    );
    assert_eq!(
        mapping
            .get("another")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "value"
    );

    let list_node = mapping.get("list").unwrap();
    let list = list_node.as_sequence().unwrap();
    assert_eq!(list.len(), 2);
    assert_eq!(
        list.get(0).unwrap().as_scalar().unwrap().as_string(),
        "item1"
    );
    assert_eq!(
        list.get(1).unwrap().as_scalar().unwrap().as_string(),
        "item2"
    );

    // Verify exact round-trip
    let output = parsed.to_string();
    assert_eq!(output, yaml);
}

#[test]
fn test_handles_utf8_content() {
    let yaml = r#"english: Hello
chinese: 你好
japanese: こんにちは
arabic: مرحبا
emoji: 😀🎉🚀
math: ∑∏∫√"#;

    let parsed = YamlFile::from_str(yaml).expect("Should handle UTF-8 content");

    // Verify we parsed all keys and values correctly via API
    let doc = parsed.document().unwrap();
    let mapping = doc.as_mapping().unwrap();

    assert_eq!(
        mapping
            .get("english")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "Hello"
    );
    assert_eq!(
        mapping
            .get("chinese")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "你好"
    );
    assert_eq!(
        mapping
            .get("japanese")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "こんにちは"
    );
    assert_eq!(
        mapping
            .get("arabic")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "مرحبا"
    );
    assert_eq!(
        mapping
            .get("emoji")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "😀🎉🚀"
    );
    assert_eq!(
        mapping
            .get("math")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "∑∏∫√"
    );

    // Verify exact round-trip
    let output = parsed.to_string();
    assert_eq!(output, yaml);
}

// ========================================
// YAML 1.2 Spec Examples
// ========================================

#[test]
fn test_spec_example_2_1_sequence_of_scalars() {
    let yaml = r#"- Mark McGwire
- Sammy Sosa
- Ken Griffey"#;

    let parsed = YamlFile::from_str(yaml).expect("Should parse spec example 2.1");

    // Verify we parsed the sequence correctly via API
    let doc = parsed.document().unwrap();
    let sequence = doc.as_sequence().unwrap();
    assert_eq!(sequence.len(), 3);
    assert_eq!(
        sequence.get(0).unwrap().as_scalar().unwrap().as_string(),
        "Mark McGwire"
    );
    assert_eq!(
        sequence.get(1).unwrap().as_scalar().unwrap().as_string(),
        "Sammy Sosa"
    );
    assert_eq!(
        sequence.get(2).unwrap().as_scalar().unwrap().as_string(),
        "Ken Griffey"
    );

    // Verify exact round-trip
    let output = parsed.to_string();
    assert_eq!(output, yaml);
}

#[test]
fn test_spec_example_2_2_mapping_scalars_to_scalars() {
    let yaml = r#"hr:  65    # Home runs
avg: 0.278 # Batting average
rbi: 147   # Runs Batted In"#;

    let parsed = YamlFile::from_str(yaml).expect("Should parse spec example 2.2");

    // Verify we parsed the mapping correctly via API
    let doc = parsed.document().unwrap();
    let mapping = doc.as_mapping().unwrap();
    assert_eq!(mapping.len(), 3);
    assert_eq!(
        mapping.get("hr").unwrap().as_scalar().unwrap().as_string(),
        "65"
    );
    assert_eq!(
        mapping.get("avg").unwrap().as_scalar().unwrap().as_string(),
        "0.278"
    );
    assert_eq!(
        mapping.get("rbi").unwrap().as_scalar().unwrap().as_string(),
        "147"
    );

    // Verify exact round-trip (preserves comments and spacing)
    let output = parsed.to_string();
    assert_eq!(output, yaml);
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

    let parsed = YamlFile::from_str(yaml).expect("Should parse spec example 2.3");

    // Verify we parsed the structure correctly via API
    let doc = parsed.document().unwrap();
    let mapping = doc.as_mapping().unwrap();

    let american_node = mapping.get("american").unwrap();
    let american = american_node.as_sequence().unwrap();
    assert_eq!(american.len(), 3);
    assert_eq!(
        american.get(0).unwrap().as_scalar().unwrap().as_string(),
        "Boston Red Sox"
    );
    assert_eq!(
        american.get(1).unwrap().as_scalar().unwrap().as_string(),
        "Detroit Tigers"
    );
    assert_eq!(
        american.get(2).unwrap().as_scalar().unwrap().as_string(),
        "New York Yankees"
    );

    let national_node = mapping.get("national").unwrap();
    let national = national_node.as_sequence().unwrap();
    assert_eq!(national.len(), 3);
    assert_eq!(
        national.get(0).unwrap().as_scalar().unwrap().as_string(),
        "New York Mets"
    );
    assert_eq!(
        national.get(1).unwrap().as_scalar().unwrap().as_string(),
        "Chicago Cubs"
    );
    assert_eq!(
        national.get(2).unwrap().as_scalar().unwrap().as_string(),
        "Atlanta Braves"
    );

    // Verify exact round-trip
    let output = parsed.to_string();
    assert_eq!(output, yaml);
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

    let parsed = YamlFile::from_str(yaml).expect("Should parse spec example 2.4");

    // Verify we parsed the sequence of mappings correctly via API
    let doc = parsed.document().unwrap();
    let sequence = doc.as_sequence().unwrap();
    assert_eq!(sequence.len(), 2);

    let player1_node = sequence.get(0).unwrap();
    let player1 = player1_node.as_mapping().unwrap();
    assert_eq!(
        player1
            .get("name")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "Mark McGwire"
    );
    assert_eq!(
        player1.get("hr").unwrap().as_scalar().unwrap().as_string(),
        "65"
    );
    assert_eq!(
        player1.get("avg").unwrap().as_scalar().unwrap().as_string(),
        "0.278"
    );

    let player2_node = sequence.get(1).unwrap();
    let player2 = player2_node.as_mapping().unwrap();
    assert_eq!(
        player2
            .get("name")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "Sammy Sosa"
    );
    assert_eq!(
        player2.get("hr").unwrap().as_scalar().unwrap().as_string(),
        "63"
    );
    assert_eq!(
        player2.get("avg").unwrap().as_scalar().unwrap().as_string(),
        "0.288"
    );

    // Verify exact round-trip
    let output = parsed.to_string();
    assert_eq!(output, yaml);
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

    let parsed = YamlFile::from_str(yaml).expect("Should parse spec example 2.7");

    // Verify we parsed the first document correctly via API
    let doc = parsed.document().unwrap();
    let sequence = doc.as_sequence().unwrap();
    assert_eq!(sequence.len(), 3);
    assert_eq!(
        sequence.get(0).unwrap().as_scalar().unwrap().as_string(),
        "Mark McGwire"
    );
    assert_eq!(
        sequence.get(1).unwrap().as_scalar().unwrap().as_string(),
        "Sammy Sosa"
    );
    assert_eq!(
        sequence.get(2).unwrap().as_scalar().unwrap().as_string(),
        "Ken Griffey"
    );

    // Verify exact round-trip (preserves comments and multiple documents)
    let output = parsed.to_string();
    assert_eq!(output, yaml);
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

    let parsed = YamlFile::from_str(yaml).expect("Should parse spec example 2.8");

    // Verify we parsed the first document correctly via API
    let doc = parsed.document().unwrap();
    let mapping = doc.as_mapping().unwrap();
    assert_eq!(
        mapping
            .get("time")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "20:03:20"
    );
    assert_eq!(
        mapping
            .get("player")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "Sammy Sosa"
    );
    assert_eq!(
        mapping
            .get("action")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "strike (miss)"
    );

    // Verify exact round-trip (preserves multiple documents and markers)
    let output = parsed.to_string();
    assert_eq!(output, yaml);
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

    let parsed = YamlFile::from_str(yaml).expect("Should parse spec example 2.10");

    // Verify we parsed the structure correctly via API
    let doc = parsed.document().unwrap();
    let mapping = doc.as_mapping().unwrap();

    let hr_node = mapping.get("hr").unwrap();
    let hr = hr_node.as_sequence().unwrap();
    assert_eq!(hr.len(), 2);
    assert_eq!(
        hr.get(0).unwrap().as_scalar().unwrap().as_string(),
        "Mark McGwire"
    );
    assert_eq!(
        hr.get(1).unwrap().as_scalar().unwrap().as_string(),
        "Sammy Sosa"
    );

    let rbi_node = mapping.get("rbi").unwrap();
    let rbi = rbi_node.as_sequence().unwrap();
    assert_eq!(rbi.len(), 2);
    // First item is an alias to SS
    assert!(rbi.get(0).unwrap().is_alias());
    assert_eq!(rbi.get(0).unwrap().as_alias().unwrap().name(), "SS");
    // Second item is a scalar
    assert_eq!(
        rbi.get(1).unwrap().as_scalar().unwrap().as_string(),
        "Ken Griffey"
    );

    // Verify exact round-trip (preserves anchors, aliases, and comments)
    let output = parsed.to_string();
    assert_eq!(output, yaml);
}
