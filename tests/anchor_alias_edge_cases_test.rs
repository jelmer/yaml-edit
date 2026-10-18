//! Test anchor and alias edge cases from YAML specification
//!
//! Tests cover:
//! - Self-referential anchors
//! - Multiple aliases to same anchor
//! - Unicode anchor names
//! - Anchor redefinition/overrides
//! - Undefined alias references
//! - Circular references

use std::str::FromStr;
use yaml_edit::YamlFile;

/// Test self-referential anchor: &x x: *x
/// This creates a circular structure where the key points to itself
#[test]
fn test_self_referential_anchor() {
    let yaml = r#"&x x: *x"#;

    // Parser should handle self-referential structure
    let parsed = YamlFile::from_str(yaml).expect("Self-referential anchor should parse");
    let doc = parsed.document().expect("Should have document");
    let mapping = doc.as_mapping().expect("Should be mapping");

    // Verify structure via API
    assert_eq!(mapping.keys().count(), 1, "Should have 1 key");
    assert!(mapping.contains_key("x"), "Should have key 'x'");

    // The value should be an alias
    let value = mapping.get("x").expect("Should have value for key 'x'");
    assert!(value.is_alias(), "Value should be an alias");
    assert_eq!(
        value.as_alias().unwrap().name(),
        "x",
        "Alias should reference 'x'"
    );

    // Verify output is valid YAML
    let output = doc.to_string();
    let reparsed = YamlFile::from_str(&output).expect("Output should be valid YAML");
    assert!(reparsed.document().is_some());
}

/// Test multiple aliases referencing the same anchor
/// All alias references should point to the same anchor
#[test]
fn test_multiple_aliases_same_anchor() {
    let yaml = r#"anchor: &shared value
ref1: *shared
ref2: *shared
ref3: *shared"#;

    let parsed = YamlFile::from_str(yaml).expect("Multiple aliases should parse");
    let doc = parsed.document().expect("Should have document");
    let mapping = doc.as_mapping().expect("Should be mapping");

    // Verify 4 keys
    assert_eq!(mapping.keys().count(), 4, "Should have 4 keys");

    // Verify anchor key has scalar value
    let anchor_val = mapping.get("anchor").expect("Should have 'anchor' key");
    assert!(anchor_val.is_scalar(), "Anchor value should be scalar");
    assert_eq!(anchor_val.as_scalar().unwrap().as_string(), "value");

    // Verify all three references are aliases pointing to "shared"
    for ref_key in ["ref1", "ref2", "ref3"] {
        let ref_val = mapping
            .get(ref_key)
            .unwrap_or_else(|| panic!("Should have '{}' key", ref_key));
        assert!(ref_val.is_alias(), "{} should be an alias", ref_key);
        assert_eq!(
            ref_val.as_alias().unwrap().name(),
            "shared",
            "{} should reference 'shared'",
            ref_key
        );
    }

    // Verify output is valid YAML
    let output = doc.to_string();
    let reparsed = YamlFile::from_str(&output).expect("Output should be valid YAML");
    assert!(reparsed.document().is_some());
}

/// Test anchors with Unicode characters in names
/// YAML spec allows Unicode in anchor names
#[test]
fn test_unicode_anchor_names() {
    let yaml = r#"anchor: &日本語 value
ref: *日本語"#;

    let parsed = YamlFile::from_str(yaml).expect("Unicode anchors should parse");
    let doc = parsed.document().expect("Should have document");
    let mapping = doc.as_mapping().expect("Should be mapping");

    // Verify 2 keys
    assert_eq!(mapping.keys().count(), 2, "Should have 2 keys");

    // Verify anchor key
    let anchor_val = mapping.get("anchor").expect("Should have 'anchor' key");
    assert!(anchor_val.is_scalar(), "Anchor value should be scalar");
    assert_eq!(anchor_val.as_scalar().unwrap().as_string(), "value");

    // Verify alias references Unicode anchor
    let ref_val = mapping.get("ref").expect("Should have 'ref' key");
    assert!(ref_val.is_alias(), "ref should be an alias");
    assert_eq!(
        ref_val.as_alias().unwrap().name(),
        "日本語",
        "Alias should reference Unicode anchor"
    );

    // Verify output is valid YAML
    let output = doc.to_string();
    let reparsed = YamlFile::from_str(&output).expect("Output should be valid YAML");
    assert!(reparsed.document().is_some());
}

/// Test anchor redefinition - when same anchor name is used twice
/// The second definition should override the first
#[test]
fn test_anchor_override() {
    let yaml = r#"first: &x value1
second: &x value2
ref: *x"#;

    let parsed = YamlFile::from_str(yaml).expect("Anchor override should parse");
    let doc = parsed.document().expect("Should have document");
    let mapping = doc.as_mapping().expect("Should be mapping");

    // Verify 3 keys
    assert_eq!(mapping.keys().count(), 3, "Should have 3 keys");

    // Verify both anchor values exist
    let first_val = mapping.get("first").expect("Should have 'first' key");
    assert_eq!(first_val.as_scalar().unwrap().as_string(), "value1");

    let second_val = mapping.get("second").expect("Should have 'second' key");
    assert_eq!(second_val.as_scalar().unwrap().as_string(), "value2");

    // The alias should reference the latest definition of 'x'
    // (This behavior depends on implementation - document what we do)
    let ref_val = mapping.get("ref").expect("Should have 'ref' key");
    assert!(ref_val.is_alias(), "ref should be an alias");
    assert_eq!(
        ref_val.as_alias().unwrap().name(),
        "x",
        "Alias should reference 'x'"
    );

    // Verify output is valid YAML
    let output = doc.to_string();
    let reparsed = YamlFile::from_str(&output).expect("Output should be valid YAML");
    assert!(reparsed.document().is_some());
}

/// Test alias before anchor definition - should handle gracefully
/// YAML requires anchors to be defined before use
#[test]
fn test_undefined_alias_reference() {
    let yaml = r#"ref: *undefined
anchor: &undefined value"#;

    // Parser preserves undefined aliases structurally without validation
    let yaml_file = YamlFile::from_str(yaml).expect("Should parse with undefined alias");
    let doc = yaml_file.document().expect("Should have document");
    let mapping = doc.as_mapping().expect("Should be mapping");

    // Should have both keys
    assert_eq!(mapping.keys().count(), 2);

    // The ref value should still be an alias node
    let ref_val = mapping.get("ref").expect("Should have 'ref' key");
    assert!(ref_val.is_alias());
    assert_eq!(ref_val.as_alias().unwrap().name(), "undefined");

    // Verify output maintains structure
    let output = doc.to_string();
    assert_eq!(output, yaml);
}

/// Test circular reference parsing succeeds
/// Parser should successfully build CST for circular structures
#[test]
fn test_circular_reference_parsing_succeeds() {
    let yaml = r#"node: &node
  next: *node"#;

    // Parser should successfully parse circular structure
    let parsed = YamlFile::from_str(yaml).expect("Circular reference should parse");
    let doc = parsed.document().expect("Should have document");
    let mapping = doc.as_mapping().expect("Should be mapping");

    // Verify structure via API
    assert_eq!(mapping.keys().count(), 1, "Should have 1 key");
    assert!(mapping.contains_key("node"), "Should have key 'node'");

    let node_mapping = mapping
        .get_mapping("node")
        .expect("node should be a mapping");
    assert!(
        node_mapping.contains_key("next"),
        "node should have 'next' key"
    );

    // The "next" field should be an alias reference
    let next_value = node_mapping.get("next").expect("Should have 'next' value");
    assert!(next_value.is_alias(), "next should be an alias");
    assert_eq!(
        next_value.as_alias().unwrap().name(),
        "node",
        "Alias should reference 'node'"
    );

    // Verify exact round-trip
    let output = doc.to_string();
    assert_eq!(output, yaml, "Output should match input exactly");

    // Verify re-parsing works
    let reparsed = YamlFile::from_str(&output).expect("Reparsed should be valid");
    assert!(reparsed.document().is_some());
}

/// Test circular reference in sequence: &x [*x]
#[test]
fn test_circular_reference_in_sequence() {
    let yaml = "&x [*x]";

    // Parser should handle circular sequence
    let parsed = YamlFile::from_str(yaml).expect("Circular sequence should parse");
    let doc = parsed.document().expect("Should have document");
    let sequence = doc.as_sequence().expect("Should be sequence");

    // Verify structure
    assert_eq!(sequence.len(), 1, "Sequence should have 1 element");

    let first = sequence.get(0).expect("Should have first element");
    assert!(first.is_alias(), "First element should be an alias");
    assert_eq!(
        first.as_alias().unwrap().name(),
        "x",
        "Alias should reference 'x'"
    );

    // Verify output is valid YAML
    let output = doc.to_string();
    let reparsed = YamlFile::from_str(&output).expect("Output should be valid YAML");
    assert!(reparsed.document().is_some());
}

/// Test deeply nested aliases
#[test]
fn test_deeply_nested_aliases() {
    let yaml = r#"level1: &l1
  level2: &l2
    level3: &l3
      value: deep
ref1: *l1
ref2: *l2
ref3: *l3"#;

    let parsed = YamlFile::from_str(yaml).expect("Nested aliases should parse");
    let doc = parsed.document().expect("Should have document");
    let mapping = doc.as_mapping().expect("Should be mapping");

    // Verify 4 keys at root
    assert_eq!(mapping.keys().count(), 4, "Should have 4 root keys");

    // Verify all refs are aliases
    for (ref_key, anchor_name) in [("ref1", "l1"), ("ref2", "l2"), ("ref3", "l3")] {
        let ref_val = mapping
            .get(ref_key)
            .unwrap_or_else(|| panic!("Should have '{}' key", ref_key));
        assert!(ref_val.is_alias(), "{} should be an alias", ref_key);
        assert_eq!(
            ref_val.as_alias().unwrap().name(),
            anchor_name,
            "{} should reference '{}'",
            ref_key,
            anchor_name
        );
    }

    // Verify output is valid YAML
    let output = doc.to_string();
    let reparsed = YamlFile::from_str(&output).expect("Output should be valid YAML");
    assert!(reparsed.document().is_some());
}

/// An alias is a complete node, so `*a x: v` has no implicit key: the colon
/// follows `x`. The parser used to take `*a` as the key and then fail on the
/// `x`. It now treats the alias like any other complete flow node followed
/// by more content (`[q] x: v`), and the validator reports it.
#[test]
fn test_alias_followed_by_content_is_not_a_key() {
    use yaml_edit::validator::Validator;
    let parse = |yaml: &str| {
        let file = YamlFile::from_str(yaml).ok()?;
        assert_eq!(file.to_string(), yaml);
        // TODO: assert_file_cst_ok fails here, as it does for `[q] x: v`:
        // the entry before `x: v` ends in WHITESPACE rather than a NEWLINE.
        let root = <YamlFile as rowan::ast::AstNode>::syntax(&file).clone();
        assert!(
            !root
                .descendants()
                .any(|n| n.kind() == yaml_edit::SyntaxKind::KEY
                    && n.descendants()
                        .any(|d| d.kind() == yaml_edit::SyntaxKind::ALIAS)),
            "{yaml:?}"
        );
        let violations = Validator::new().validate_syntax(&root);
        Some(
            violations
                .into_iter()
                .map(|v| (v.message, v.location.unwrap()))
                .collect::<Vec<_>>(),
        )
    };
    let separate = "Block mapping entries must be on separate lines".to_string();
    for (yaml, flow_form, expected) in [
        ("a: &a 1\n*a x: v\n", "a: &a 1\n[q] x: v\n", None),
        ("- *a x: v\n", "- [q] x: v\n", None),
        (
            "a: &a 1\nk:\n  *a x: v\n",
            "a: &a 1\nk:\n  [q] x: v\n",
            Some(vec![(separate.clone(), "3:6".to_string())]),
        ),
        (
            "a: &a 1\nb:\n- *a x: v\n",
            "a: &a 1\nb:\n- [q] x: v\n",
            Some(vec![(separate.clone(), "3:6".to_string())]),
        ),
    ] {
        assert_eq!(parse(yaml), expected, "{yaml:?}");
        assert_eq!(
            YamlFile::from_str(flow_form).is_ok(),
            expected.is_some(),
            "{flow_form:?}"
        );
    }
}

/// An alias followed only by the colon is still a key, and `*a:` is an alias
/// named `a:`, as in the YAML test suite's 2SXE.
#[test]
fn test_alias_key_still_parses() {
    let file = YamlFile::from_str("a: &a 1\n*a : v\n").unwrap();
    let root = <YamlFile as rowan::ast::AstNode>::syntax(&file).clone();
    let keys = root
        .descendants()
        .filter(|n| n.kind() == yaml_edit::SyntaxKind::KEY)
        .map(|n| n.text().to_string())
        .collect::<Vec<_>>();
    assert_eq!(keys, ["a", "*a"]);

    let file = YamlFile::from_str("foo:\n  *a:\n").unwrap();
    let root = <YamlFile as rowan::ast::AstNode>::syntax(&file).clone();
    let aliases = root
        .descendants()
        .filter(|n| n.kind() == yaml_edit::SyntaxKind::ALIAS)
        .map(|n| n.text().to_string())
        .collect::<Vec<_>>();
    assert_eq!(aliases, ["*a:"]);
}
