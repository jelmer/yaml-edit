//! Test mutation and editing edge cases
//!
//! Tests cover:
//! - Mutating anchor targets and alias behavior
//! - Removing keys referenced by aliases
//! - Editing block scalars
//! - Mutating tags on tagged nodes
//! - Modifying empty collections

use rowan::ast::AstNode;
use std::str::FromStr;
use yaml_edit::{debug, YamlFile};

/// Test mutating the target of an anchor
/// When an anchored value is changed, aliases should still reference the anchor
#[test]
fn test_mutate_anchor_target() {
    let yaml = r#"anchor: &x original
ref: *x"#;

    let parsed = YamlFile::from_str(yaml).expect("Should parse");
    let doc = parsed.document().expect("Should have document");
    let mapping = doc.as_mapping().expect("Should be mapping");

    // Verify initial state
    let anchor_val = mapping.get("anchor").expect("Should have anchor");
    assert_eq!(anchor_val.as_scalar().unwrap().as_string(), "original");

    let ref_val = mapping.get("ref").expect("Should have ref");
    assert!(ref_val.is_alias(), "ref should be an alias");
    assert_eq!(ref_val.as_alias().unwrap().name(), "x");

    // Mutate the anchored value
    mapping.set("anchor", "modified");

    // Verify the anchor value changed
    let new_anchor_val = mapping.get("anchor").expect("Should have anchor");
    assert_eq!(new_anchor_val.as_scalar().unwrap().as_string(), "modified");

    // The alias should still be an alias node pointing to 'x'
    let ref_val_after = mapping.get("ref").expect("Should have ref");
    assert!(ref_val_after.is_alias(), "ref should still be an alias");
    assert_eq!(ref_val_after.as_alias().unwrap().name(), "x");

    // Verify output is valid YAML
    let output = doc.to_string();
    let reparsed = YamlFile::from_str(&output).expect("Output should be valid YAML");
    assert!(reparsed.document().is_some());
}

/// Test mutating nested mapping that has an anchor
#[test]
fn test_mutate_nested_anchor_target() {
    let yaml = r#"config: &defaults
  timeout: 30
  retries: 3
server: *defaults"#;

    let parsed = YamlFile::from_str(yaml).expect("Should parse");
    let doc = parsed.document().expect("Should have document");
    let mapping = doc.as_mapping().expect("Should be mapping");

    // Verify initial state
    let config = mapping
        .get_mapping("config")
        .expect("Should have config mapping");
    assert_eq!(
        config.get("timeout").unwrap().as_scalar().unwrap().as_i64(),
        Some(30)
    );

    // Mutate the anchored mapping
    config.set("timeout", 60);
    config.set("max_connections", 100);

    // Verify the config mapping changed
    assert_eq!(
        config.get("timeout").unwrap().as_scalar().unwrap().as_i64(),
        Some(60)
    );
    assert!(config.contains_key("max_connections"));

    // The alias should still point to 'defaults'
    let server_val = mapping.get("server").expect("Should have server");
    assert!(server_val.is_alias(), "server should be an alias");
    assert_eq!(server_val.as_alias().unwrap().name(), "defaults");

    // Verify output is valid YAML
    let output = doc.to_string();
    let reparsed = YamlFile::from_str(&output).expect("Output should be valid YAML");
    assert!(reparsed.document().is_some());
}

/// Test removing a key that has an anchor
/// The alias should remain in the structure (as a dangling reference)
#[test]
fn test_remove_anchored_key() {
    let yaml = r#"orig: &x value
ref: *x"#;

    let parsed = YamlFile::from_str(yaml).expect("Should parse");
    let doc = parsed.document().expect("Should have document");
    let mapping = doc.as_mapping().expect("Should be mapping");

    // Verify initial state
    assert_eq!(mapping.keys().count(), 2);
    assert!(mapping.contains_key("orig"));
    assert!(mapping.contains_key("ref"));

    // Remove the anchored key
    mapping.remove("orig");

    // Verify orig is removed
    assert_eq!(mapping.keys().count(), 1);
    assert!(!mapping.contains_key("orig"));
    assert!(mapping.contains_key("ref"));

    // The alias should still exist (as a dangling reference)
    let ref_val = mapping.get("ref").expect("Should have ref");
    assert!(ref_val.is_alias(), "ref should still be an alias");
    assert_eq!(ref_val.as_alias().unwrap().name(), "x");

    // Verify output is valid YAML (with dangling reference)
    let output = doc.to_string();
    let reparsed = YamlFile::from_str(&output).expect("Output should be valid YAML");
    assert!(reparsed.document().is_some());
}

/// Test editing block scalar content
/// Verify we can replace block scalar values
#[test]
fn test_edit_block_scalar_content() {
    let yaml = r#"text: |
  Line 1
  Line 2
"#;

    let parsed = YamlFile::from_str(yaml).expect("Should parse");
    let doc = parsed.document().expect("Should have document");
    let mapping = doc.as_mapping().expect("Should be mapping");

    // Verify initial content
    let text_val = mapping.get("text").expect("Should have text");
    assert!(text_val.is_scalar(), "text should be scalar");
    let initial_text = text_val.as_scalar().unwrap().as_string();
    assert_eq!(initial_text, "Line 1\nLine 2\n");

    // Replace the block scalar with new content
    mapping.set("text", "New single line");

    // Verify the content changed
    let new_text_val = mapping.get("text").expect("Should have text");
    assert_eq!(
        new_text_val.as_scalar().unwrap().as_string(),
        "New single line"
    );

    // Verify output is valid YAML
    let output = doc.to_string();
    let reparsed = YamlFile::from_str(&output).expect("Output should be valid YAML");
    assert!(reparsed.document().is_some());
}

/// Test replacing block scalar with another block scalar
#[test]
fn test_replace_block_scalar_with_multiline() {
    let yaml = r#"description: |
  Original text
  on multiple lines
"#;

    let parsed = YamlFile::from_str(yaml).expect("Should parse");
    let doc = parsed.document().expect("Should have document");
    let mapping = doc.as_mapping().expect("Should be mapping");

    // Replace with new multiline content
    // Note: We're setting a string value, which may render as plain/quoted scalar
    // depending on the implementation's formatting choices
    let new_content = "New line 1\nNew line 2\nNew line 3";
    mapping.set("description", new_content);

    // Verify the content changed
    let new_val = mapping.get("description").expect("Should have description");
    let actual_content = new_val.as_scalar().unwrap().as_string();
    // Block scalars preserve trailing newline
    assert_eq!(actual_content, format!("{}\n", new_content));

    // Verify output is valid YAML
    let output = doc.to_string();
    let reparsed = YamlFile::from_str(&output).expect("Output should be valid YAML");
    assert!(reparsed.document().is_some());

    // Verify re-parsed content matches (with trailing newline from block scalar)
    let reparsed_doc = reparsed.document().unwrap();
    let reparsed_mapping = reparsed_doc.as_mapping().unwrap();
    let reparsed_val = reparsed_mapping.get("description").unwrap();
    assert_eq!(
        reparsed_val.as_scalar().unwrap().as_string(),
        format!("{}\n", new_content)
    );
}

/// Test mutating tagged nodes
/// Verify we can change values of tagged nodes
#[test]
fn test_mutate_tagged_node_value() {
    let yaml = r#"date: !!timestamp 2024-01-01
count: !!int 42"#;

    let parsed = YamlFile::from_str(yaml).expect("Should parse");
    let doc = parsed.document().expect("Should have document");
    let mapping = doc.as_mapping().expect("Should be mapping");

    // Verify initial tagged values exist
    assert!(mapping.contains_key("date"));
    assert!(mapping.contains_key("count"));

    // Mutate the tagged values
    // Note: This replaces the tagged node with a plain scalar
    mapping.set("date", "2024-12-31");
    mapping.set("count", 100);

    // Verify the values changed
    let new_date = mapping.get("date").expect("Should have date");
    assert_eq!(new_date.as_scalar().unwrap().as_string(), "2024-12-31");

    let new_count = mapping.get("count").expect("Should have count");
    assert_eq!(new_count.as_scalar().unwrap().as_i64(), Some(100));

    // Verify output is valid YAML
    let output = doc.to_string();
    let reparsed = YamlFile::from_str(&output).expect("Output should be valid YAML");
    assert!(reparsed.document().is_some());
}

/// Test adding keys to an empty flow mapping
#[test]
fn test_add_to_empty_flow_mapping() {
    let yaml = "empty: {}";

    let parsed = YamlFile::from_str(yaml).expect("Should parse");
    let doc = parsed.document().expect("Should have document");
    let mapping = doc.as_mapping().expect("Should be mapping");

    // Verify initial state - empty mapping
    let empty_map = mapping
        .get_mapping("empty")
        .expect("Should have empty mapping");
    assert_eq!(empty_map.keys().count(), 0, "Should be empty initially");

    // Add keys to the empty mapping
    empty_map.set("a", 1);
    empty_map.set("b", 2);

    // Verify keys were added
    assert_eq!(empty_map.keys().count(), 2, "Should have 2 keys");
    assert_eq!(
        empty_map.get("a").unwrap().as_scalar().unwrap().as_i64(),
        Some(1)
    );
    assert_eq!(
        empty_map.get("b").unwrap().as_scalar().unwrap().as_i64(),
        Some(2)
    );

    // Verify output is valid YAML
    let output = doc.to_string();
    let reparsed = YamlFile::from_str(&output).expect("Output should be valid YAML");
    assert!(reparsed.document().is_some());
}

/// Test adding items to an empty flow sequence
#[test]
fn test_add_to_empty_flow_sequence() {
    let yaml = "items: []";

    let parsed = YamlFile::from_str(yaml).expect("Should parse");
    let doc = parsed.document().expect("Should have document");
    let mapping = doc.as_mapping().expect("Should be mapping");

    // Verify initial state - empty sequence
    let empty_seq = mapping
        .get_sequence("items")
        .expect("Should have empty sequence");
    assert_eq!(empty_seq.len(), 0, "Should be empty initially");

    // Add items to the empty sequence
    empty_seq.push("first");
    empty_seq.push("second");
    empty_seq.push("third");

    // Verify items were added
    assert_eq!(empty_seq.len(), 3, "Should have 3 items");
    assert_eq!(
        empty_seq.get(0).unwrap().as_scalar().unwrap().as_string(),
        "first"
    );
    assert_eq!(
        empty_seq.get(1).unwrap().as_scalar().unwrap().as_string(),
        "second"
    );
    assert_eq!(
        empty_seq.get(2).unwrap().as_scalar().unwrap().as_string(),
        "third"
    );

    // Verify output is valid YAML
    let output = doc.to_string();
    let reparsed = YamlFile::from_str(&output).expect("Output should be valid YAML");
    assert!(reparsed.document().is_some());
}

/// Test removing all keys from a mapping (making it empty)
#[test]
fn test_empty_mapping_by_removal() {
    let yaml = r#"config:
  a: 1
  b: 2
  c: 3"#;

    let parsed = YamlFile::from_str(yaml).expect("Should parse");
    let doc = parsed.document().expect("Should have document");
    let mapping = doc.as_mapping().expect("Should be mapping");

    let config = mapping.get_mapping("config").expect("Should have config");
    assert_eq!(config.keys().count(), 3, "Should have 3 keys initially");

    // Remove all keys
    config.remove("a");
    config.remove("b");
    config.remove("c");

    // Verify empty
    assert_eq!(config.keys().count(), 0, "Should be empty after removal");

    // Verify output is valid YAML
    let output = doc.to_string();
    let reparsed = YamlFile::from_str(&output).expect("Output should be valid YAML");
    assert!(reparsed.document().is_some());
}

/// Replacing a multi-line flow-sequence value with a scalar inside a flow
/// (JSON-style) mapping should not leave stray whitespace behind.
///
/// Regression: lintian-brush upstream-metadata `repository-as-list` fixer
/// produced a spurious blank line and lost indentation on the following
/// entry when yaml-edit replaced the `Repository` list with a string.
#[test]
fn test_replace_multiline_flow_sequence_value_with_scalar() {
    let input = "{\n  \"Name\": \"yep\",\n  \"Repository\": [\n    \":extssh:_anoncvs@anoncvs.example.org:/cvs\",\n    \"yep\"\n  ],\n  \"Repository-Browse\": \"http://www.example.org/cvs.cgi/contrib/code/yep/\"\n}";

    let yaml = YamlFile::from_str(input).expect("Should parse JSON-style mapping");
    let doc = yaml.document().expect("Should have document");
    let mapping = doc.as_mapping().expect("Should be mapping");

    mapping.set(
        "Repository",
        "cvs+ssh://_anoncvs@anoncvs.example.org/cvs#yep",
    );

    let expected = "{\n  \"Name\": \"yep\",\n  \"Repository\": \"cvs+ssh://_anoncvs@anoncvs.example.org/cvs#yep\",\n  \"Repository-Browse\": \"http://www.example.org/cvs.cgi/contrib/code/yep/\"\n}";

    assert_eq!(yaml.to_string(), expected);
    debug::validate_tree(yaml.syntax()).expect("CST invariants hold after mutation");
}

/// Adjacent shapes to the lintian-brush regression, covering the same
/// value_is_block classifier decision:
///
/// - `mapping`: old value is a multi-line flow *mapping* (not sequence).
/// - `anchor`: ANCHOR sits as a direct-child token of VALUE alongside
///   the flow SEQUENCE -- no TAGGED_NODE wrapper.
/// - `tag`: TAG wraps the value in a TAGGED_NODE; classifier peels it.
/// - `nested`: flow-of-flow-mappings, deeper nesting of the same shape.
///
/// All must round-trip to a stable CST and produce the expected inline
/// output.
#[test]
fn test_replace_multiline_flow_value_with_scalar_variants() {
    for (name, input, expected) in [
        (
            "mapping",
            "{\n  \"a\": \"1\",\n  \"nested\": {\n    \"x\": 1,\n    \"y\": 2\n  },\n  \"z\": \"3\"\n}",
            "{\n  \"a\": \"1\",\n  \"nested\": \"replaced\",\n  \"z\": \"3\"\n}",
        ),
        (
            "anchor",
            "a: 1\nb: &x [\n  1,\n  2\n]\nc: 3\n",
            "a: 1\nb: replaced\nc: 3\n",
        ),
        (
            "tag",
            "a: 1\nb: !!seq [\n  1,\n  2\n]\nc: 3\n",
            "a: 1\nb: replaced\nc: 3\n",
        ),
        (
            "nested",
            "a: 1\nb: [\n  {\n    x: 1\n  },\n  {\n    y: 2\n  }\n]\nc: 3\n",
            "a: 1\nb: replaced\nc: 3\n",
        ),
    ] {
        let yaml = YamlFile::from_str(input).unwrap();
        let target_key = if name == "mapping" { "nested" } else { "b" };
        yaml.document()
            .unwrap()
            .as_mapping()
            .unwrap()
            .set(target_key, "replaced");
        assert_eq!(yaml.to_string(), expected, "variant: {name}");
        debug::validate_tree(yaml.syntax())
            .unwrap_or_else(|e| panic!("variant {name} invariant violated: {e}"));
    }
}

/// Regression from mutation_invariants fuzz: setting a value under a key
/// that starts with `#` used to render the key unquoted, but `#` is a
/// comment indicator, so the re-parse dropped the entry (or misread it as
/// a comment) and the mutation didn't stick. The key must be quoted on
/// output so it round-trips.
#[test]
fn test_set_key_starting_with_hash_reparses() {
    let seed = "literal: |\n  line1\n  line2\n";
    let doc = yaml_edit::Document::from_str(seed).expect("parse seed");
    let mapping = doc.as_mapping().expect("root mapping");

    mapping.set("#aaaaa", "");

    // After the mutation, the mapping itself sees the key.
    assert!(mapping.contains_key("#aaaaa"), "in-memory: key missing");

    // And the serialised form must reparse to the same key/value.
    let text = doc.to_string();
    let reparsed = yaml_edit::Document::from_str(&text)
        .unwrap_or_else(|e| panic!("reparse failed: {e}\ntext:\n{text}"));
    let reparsed_mapping = reparsed
        .as_mapping()
        .unwrap_or_else(|| panic!("reparsed root is not a mapping, text:\n{text}"));
    assert!(
        reparsed_mapping.contains_key("#aaaaa"),
        "reparse drift: key `#aaaaa` missing after round-trip\ntext:\n{text}"
    );
}

/// Regression from mutation_invariants fuzz: setting a key whose
/// text starts with a `.` and contains numeric-looking content used
/// to serialise unquoted (`.999 a: ''`). The reparse of that line
/// then didn't recover the key, breaking round-trip.
#[test]
fn test_set_key_starting_with_dot_and_digits_reparses() {
    let seed = "a: \"\"\n";
    let doc = yaml_edit::Document::from_str(seed).expect("parse seed");
    let mapping = doc.as_mapping().expect("root mapping");

    mapping.set(".999 a", "");

    let text = doc.to_string();
    let reparsed = yaml_edit::Document::from_str(&text)
        .unwrap_or_else(|e| panic!("reparse failed: {e}\ntext:\n{text}"));
    let reparsed_mapping = reparsed
        .as_mapping()
        .unwrap_or_else(|| panic!("reparsed root is not a mapping, text:\n{text}"));
    assert!(
        reparsed_mapping.contains_key(".999 a"),
        "reparse drift: key `.999 a` missing after round-trip\ntext:\n{text}"
    );
}

/// Regression from mutation_invariants fuzz: `set_path("997", "")`
/// on a document whose root is a mapping (`s: [...]`) - the new
/// key `997` was not present after the set; `get_path("997")`
/// returned None. A single-segment set_path that names a new key
/// should insert it.
#[test]
fn test_set_path_new_numeric_key() {
    use yaml_edit::path::YamlPath;

    let seed = "s:\n  - a\n  - b\n  - c\n";
    let doc = yaml_edit::Document::from_str(seed).expect("parse seed");

    doc.set_path("997", "");

    let got = doc
        .get_path("997")
        .as_ref()
        .and_then(|n| n.as_scalar().map(|s| s.as_string()));
    assert_eq!(
        got.as_deref(),
        Some(""),
        "set_path(\"997\", \"\") did not stick: get_path(\"997\") returned {got:?}\ntext:\n{doc}"
    );
}

/// Regression from mutation_invariants fuzz: `set_path("", ...)` on a
/// document tripped the fuzz post-condition because `get_path("")`
/// returned `None` after the set. The library's contract is that an
/// empty path is invalid and both operations no-op silently, so a
/// document that reaches `set_path("", ...)` must be unchanged and
/// `get_path("")` afterwards must consistently return `None`.
///
/// The `try_` variants surface the same condition as
/// `PathError::EmptyPath`.
#[test]
fn test_set_empty_path_is_a_noop() {
    use yaml_edit::path::{PathError, YamlPath};

    let seed = "folded: >\n  wrapped\n  paragraph\n";
    let doc = yaml_edit::Document::from_str(seed).expect("parse seed");
    let before = doc.to_string();

    doc.set_path("", "anything");

    assert_eq!(
        doc.to_string(),
        before,
        "set_path(\"\", ...) should be a no-op"
    );
    assert!(
        doc.get_path("").is_none(),
        "get_path(\"\") should always return None"
    );
    assert!(
        !doc.remove_path(""),
        "remove_path(\"\") should return false"
    );

    // try_ variants distinguish the reason.
    assert_eq!(
        doc.try_get_path(""),
        Err(PathError::EmptyPath),
        "try_get_path reports the specific reason"
    );
    assert_eq!(
        doc.try_set_path("", "anything"),
        Err(PathError::EmptyPath),
        "try_set_path reports the specific reason"
    );
    assert!(
        matches!(doc.try_remove_path(""), Err(PathError::EmptyPath)),
        "try_remove_path reports the specific reason"
    );
}

/// The try_ variants surface the specific failure modes the silent
/// versions used to swallow. This exercises each `PathError` variant.
#[test]
fn test_try_path_reports_specific_errors() {
    use yaml_edit::path::{PathError, YamlPath};

    // NoRoot: a document whose root is a scalar has no mapping to set
    // paths under.
    let doc = yaml_edit::Document::from_str("just_a_scalar\n").expect("parse");
    assert!(matches!(
        doc.try_set_path("foo", "bar"),
        Err(PathError::NoRoot)
    ));

    // Parse error: unclosed bracket.
    let doc = yaml_edit::Document::from_str("a: 1\n").expect("parse");
    assert!(matches!(
        doc.try_set_path("a[0", "x"),
        Err(PathError::Parse(_))
    ));

    // NotFound: intermediate key present but leaf is missing.
    let doc = yaml_edit::Document::from_str("a:\n  b: 1\n").expect("parse");
    assert!(matches!(
        doc.try_get_path("a.missing"),
        Err(PathError::NotFound { .. })
    ));

    // TypeMismatch on get: descending through a scalar.
    let doc = yaml_edit::Document::from_str("a: hello\n").expect("parse");
    assert!(matches!(
        doc.try_get_path("a.b"),
        Err(PathError::TypeMismatch { .. })
    ));

    // TypeMismatch on set: cannot descend through an existing scalar.
    let doc = yaml_edit::Document::from_str("a: hello\n").expect("parse");
    assert!(matches!(
        doc.try_set_path("a.b", "x"),
        Err(PathError::TypeMismatch { .. })
    ));
}
