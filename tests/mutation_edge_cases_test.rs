//! Test mutation and editing edge cases
//!
//! Tests cover:
//! - Mutating anchor targets and alias behavior
//! - Removing keys referenced by aliases
//! - Editing block scalars
//! - Mutating tags on tagged nodes
//! - Modifying empty collections

mod common;

use common::{assert_cst_ok, assert_file_cst_ok};
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
    assert_file_cst_ok(&parsed);

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
    assert_file_cst_ok(&parsed);
    config.set("max_connections", 100);
    assert_file_cst_ok(&parsed);

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

/// Removing a key with an anchor moves the anchored value to the first
/// alias to it.
#[test]
fn test_remove_anchored_key() {
    let yaml = r#"orig: &x value
ref: *x"#;

    let parsed = YamlFile::from_str(yaml).expect("Should parse");
    let doc = parsed.document().expect("Should have document");
    let mapping = doc.as_mapping().expect("Should be mapping");

    mapping.remove("orig");
    assert_file_cst_ok(&parsed);

    assert_eq!(doc.to_string(), "ref: &x value");
    assert_eq!(
        mapping.get("ref").unwrap().as_scalar().unwrap().as_string(),
        "value"
    );
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
    assert_file_cst_ok(&parsed);

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
    assert_file_cst_ok(&parsed);

    // Verify the content changed
    let new_val = mapping.get("description").expect("Should have description");
    let actual_content = new_val.as_scalar().unwrap().as_string();
    // The value round-trips exactly: it ends without a line break, so it is
    // written with `|-` rather than a `|` that would add one.
    assert_eq!(actual_content, new_content);

    // Verify output is valid YAML
    let output = doc.to_string();
    let reparsed = YamlFile::from_str(&output).expect("Output should be valid YAML");
    assert!(reparsed.document().is_some());

    // The value survives a serialise and reparse unchanged.
    let reparsed_doc = reparsed.document().unwrap();
    let reparsed_mapping = reparsed_doc.as_mapping().unwrap();
    let reparsed_val = reparsed_mapping.get("description").unwrap();
    assert_eq!(reparsed_val.as_scalar().unwrap().as_string(), new_content);
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
    assert_file_cst_ok(&parsed);
    mapping.set("count", 100);
    assert_file_cst_ok(&parsed);

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
    assert_file_cst_ok(&parsed);
    empty_map.set("b", 2);
    assert_file_cst_ok(&parsed);

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
    assert_file_cst_ok(&parsed);
    empty_seq.push("second");
    assert_file_cst_ok(&parsed);
    empty_seq.push("third");
    assert_file_cst_ok(&parsed);

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
    assert_file_cst_ok(&parsed);
    config.remove("b");
    assert_file_cst_ok(&parsed);
    config.remove("c");
    assert_file_cst_ok(&parsed);

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
    assert_file_cst_ok(&yaml);

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
            "a: 1\nb: &x replaced\nc: 3\n",
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
        assert_file_cst_ok(&yaml);
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
    assert_cst_ok(&doc);

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
    assert_cst_ok(&doc);

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

    doc.try_set_path("997", "").expect("set_path");
    assert_cst_ok(&doc);

    let got = doc
        .try_get_path("997")
        .ok()
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

    // Deliberately exercise the loose (deprecated) shims here to lock
    // in their swallow-on-error semantics.
    #[allow(deprecated)]
    {
        doc.set_path("", "anything");
        assert_cst_ok(&doc);
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
        assert_cst_ok(&doc);
    }

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
    assert_cst_ok(&doc);
    assert!(
        matches!(doc.try_remove_path(""), Err(PathError::EmptyPath)),
        "try_remove_path reports the specific reason"
    );
    assert_cst_ok(&doc);
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
    assert_cst_ok(&doc);

    // Parse error: unclosed bracket.
    let doc = yaml_edit::Document::from_str("a: 1\n").expect("parse");
    assert!(matches!(
        doc.try_set_path("a[0", "x"),
        Err(PathError::Parse(_))
    ));
    assert_cst_ok(&doc);

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
    assert_cst_ok(&doc);
}

/// New content in a document closed by `...` goes before the marker.
#[test]
fn test_set_in_document_ended_by_marker() {
    for (input, expected) in [
        ("--- \n...\n", "--- \nkey: value\n...\n"),
        ("---\n...\n", "---\nkey: value\n...\n"),
        ("a: 1\n...\n", "a: 1\nkey: value\n...\n"),
        ("--- \n...\n--- b\n", "--- \nkey: value\n...\n--- b\n"),
    ] {
        let file = YamlFile::from_str(input).unwrap();
        assert!(file.ensure_document().set("key", "value"));
        assert_eq!(file.to_string(), expected, "{input:?}");

        let file = YamlFile::from_str(input).unwrap();
        assert!(file
            .document()
            .unwrap()
            .set_with_field_order("key", "value", ["key"]));
        assert_eq!(file.to_string(), expected, "{input:?}");
    }
}

#[test]
fn test_insert_at_index_keeps_document_markers() {
    for (input, expected) in [
        ("--- \n...\n", "--- \nkey: value\n...\n"),
        ("---\n", "---\nkey: value\n"),
    ] {
        let file = YamlFile::from_str(input).unwrap();
        file.document().unwrap().insert_at_index(0, "key", "value");
        assert_eq!(file.to_string(), expected, "{input:?}");
    }
}

/// Setting a value that was omitted at the end of a line in a multi-line
/// flow collection keeps the line break before the closing bracket.
#[test]
fn test_set_omitted_flow_value_before_line_break() {
    for (input, expected) in [
        ("{\n  k: v,\n  m:\n}\n", "{\n  k: v,\n  m: 5\n}\n"),
        ("{\n  k: v,\n  m:  \n}\n", "{\n  k: v,\n  m:  5\n}\n"),
        ("{\n  m:\n  ,\n  k: v\n}\n", "{\n  m: 5\n  ,\n  k: v\n}\n"),
        ("{\n  k: v,\n  m\n}\n", "{\n  k: v,\n  m: 5\n}\n"),
        ("{m}\n", "{m: 5}\n"),
        ("{ m }\n", "{ m: 5 }\n"),
        ("{m:}\n", "{m: 5}\n"),
        ("{m: }\n", "{m: 5}\n"),
        ("{m: , k: v}\n", "{m: 5, k: v}\n"),
    ] {
        let yaml = YamlFile::from_str(input).unwrap();
        yaml.document().unwrap().as_mapping().unwrap().set("m", 5);
        assert_eq!(yaml.to_string(), expected, "input: {input:?}");
        common::assert_file_parser_shaped(&yaml);
    }
}

/// Same for the single-pair mapping a flow sequence entry `a:` stands for.
#[test]
fn test_set_omitted_implicit_flow_pair_value_before_line_break() {
    for (input, expected) in [("[\n  a:\n]\n", "[\n  a: 5\n]\n"), ("[a:]\n", "[a: 5]\n")] {
        let yaml = YamlFile::from_str(input).unwrap();
        let seq = yaml.document().unwrap().as_sequence().unwrap();
        seq.get(0).unwrap().as_mapping().unwrap().set("a", 5);
        assert_eq!(yaml.to_string(), expected, "input: {input:?}");
        common::assert_file_parser_shaped(&yaml);
    }
}

/// A value set where the input ends right after the colon is separated from
/// it, rather than turning `a:` into the plain scalar `a:5`.
#[test]
fn test_set_omitted_value_at_end_of_input() {
    let yaml = YamlFile::from_str("x: 1\na:").unwrap();
    yaml.document().unwrap().as_mapping().unwrap().set("a", 5);
    assert_eq!(yaml.to_string(), "x: 1\na: 5");
    assert_file_cst_ok(&yaml);
}

/// A comment after a flow value stays where it is when the value is
/// replaced, rather than being copied next to the new value.
#[test]
fn test_set_flow_value_keeps_trailing_comment_once() {
    for (input, expected) in [
        ("{\n  m: 1 # c\n}\n", "{\n  m: 5 # c\n}\n"),
        ("{m: 1 # c\n}\n", "{m: 5 # c\n}\n"),
        ("{\n  m: # c\n}\n", "{\n  m: 5 # c\n}\n"),
        ("{\n  m: # c\n    1\n}\n", "{\n  m: # c\n    5\n}\n"),
    ] {
        let yaml = YamlFile::from_str(input).unwrap();
        yaml.document().unwrap().as_mapping().unwrap().set("m", 5);
        assert_eq!(yaml.to_string(), expected, "input: {input:?}");
        common::assert_file_parser_shaped(&yaml);
    }
}

/// Set the first value of `source` (by key, or index 0 in a sequence) to
/// `xval`, check the result is shaped as the parser would shape it and
/// validates, and return its text.
#[track_caller]
fn set_first(source: &str) -> String {
    let file = YamlFile::from_str(source).unwrap();
    let doc = file.document().unwrap();
    match doc.as_mapping() {
        Some(m) => m.set(m.keys().next().unwrap(), "xval"),
        None => assert!(doc.as_sequence().unwrap().set(0, "xval")),
    }
    assert_file_cst_ok(&file);
    let text = file.to_string();
    let reparsed = YamlFile::from_str(&text).unwrap();
    assert_eq!(
        debug::tree_to_string(file.syntax()),
        debug::tree_to_string(reparsed.syntax())
    );
    assert_eq!(
        yaml_edit::validator::Validator::new().validate_syntax(reparsed.syntax()),
        vec![],
        "{text:?}"
    );
    text
}

#[test]
fn test_set_tagged_value_on_next_line() {
    assert_eq!(set_first("a:\n  !t b\n"), "a: xval\n");
    assert_eq!(
        set_first("\"a\":\n  !!str b\nc: d\n"),
        "\"a\": xval\nc: d\n"
    );
    assert_eq!(set_first("a:\n  &x !t b\nc: d\n"), "a: &x xval\nc: d\n");
    assert_eq!(set_first("a:\n  !t\nb: c\n"), "a: xval\nb: c\n");
}

#[test]
fn test_set_item_ending_in_omitted_value() {
    assert_eq!(set_first("-\n  ? a\n- c\n"), "-\n  xval\n- c\n");
    assert_eq!(set_first("- ? a\n- c\n"), "- xval\n- c\n");
    assert_eq!(set_first("- a:\n- c\n"), "- xval\n- c\n");
    assert_eq!(set_first("-\n  - a\n  -\n- c\n"), "-\n  xval\n- c\n");
}

/// A block collection set as an item starts on the dash's line, indented
/// by the white space after the dash, where a tab is not allowed.
#[test]
fn test_set_block_collection_after_dash_and_tab() {
    let file = YamlFile::from_str("-\tb\n").unwrap();
    let seq = file.document().unwrap().as_sequence().unwrap();
    seq.set(0, yaml_edit::YamlValue::from(vec![1i64, 2]));
    assert_eq!(file.to_string(), "- - 1\n  - 2\n");
    assert_file_cst_ok(&file);
    assert_eq!(
        yaml_edit::validator::Validator::new().validate_syntax(file.syntax()),
        vec![]
    );

    let file = YamlFile::from_str("-\tb\n").unwrap();
    let seq = file.document().unwrap().as_sequence().unwrap();
    let map = YamlFile::from_str("x: 1\ny: 2\n").unwrap();
    seq.set(0, map.document().unwrap().as_mapping().unwrap());
    assert_eq!(file.to_string(), "- x: 1\n  y: 2\n");
    assert_file_cst_ok(&file);
}
