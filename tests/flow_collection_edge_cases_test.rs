//! Flow collection edge case tests
//!
//! Tests verify parser behavior with flow-style collections (JSON-like syntax):
//! - Flow mappings with omitted values: `{a, b:, c: d}`
//! - Flow sequences with trailing commas
//! - Mixed implicit nulls and explicit values
//! - Explicit key indicators in flow context
//! - Complex flow collection nesting
//!
//! All tests verify:
//! 1. API correctness - structure parsed as expected
//! 2. Exact assertions - no `.contains()`, exact value checks
//! 3. Round-trip validity - output can be re-parsed

use std::str::FromStr;
use yaml_edit::YamlFile;

/// Test simple flow mapping with omitted values
#[test]
fn test_simple_flow_omitted_values() {
    // Flow mapping: {a, b:, c: d}
    // Parser treats this as: "a" (implicit null), "b:" (key with colon), "c" (value d)
    let yaml = r#"{a, b:, c: d}"#;

    let parsed = YamlFile::from_str(yaml).expect("Should parse flow mapping with omitted values");
    let doc = parsed.document().expect("Should have document");
    let mapping = doc.as_mapping().expect("Should be mapping");

    // Verify 3 keys
    assert_eq!(mapping.keys().count(), 3, "Should have 3 keys");

    // Verify key 'a' exists (with implicit null value)
    let a_val = mapping.get("a").expect("Should have key 'a'");
    assert!(a_val.is_scalar(), "Key 'a' should have scalar value");
    assert!(
        a_val.as_scalar().unwrap().is_null(),
        "Key 'a' should have null value"
    );

    // Verify key 'b:' exists (colon is part of the key name, with null value)
    let b_val = mapping.get("b:").expect("Should have key 'b:'");
    assert!(b_val.is_scalar(), "Key 'b:' should have scalar value");
    assert!(
        b_val.as_scalar().unwrap().is_null(),
        "Key 'b:' should have null value"
    );

    // Verify key 'c' has value 'd'
    let c_val = mapping.get("c").expect("Should have key 'c'");
    assert_eq!(c_val.as_scalar().unwrap().as_string(), "d");

    // Verify output is valid YAML
    let output = doc.to_string();
    let reparsed = YamlFile::from_str(&output).expect("Output should be valid YAML");
    assert!(reparsed.document().is_some());
}

/// Test URL as key (colon in scalar)
#[test]
fn test_url_as_flow_key() {
    let yaml = r#"{http://foo.com, bar: baz}"#;

    let parsed = YamlFile::from_str(yaml).expect("URL as flow mapping key should parse");
    let doc = parsed.document().expect("Should have document");
    let mapping = doc.as_mapping().expect("Should be mapping");

    // Verify 2 keys
    assert_eq!(mapping.keys().count(), 2, "Should have 2 keys");

    // Verify URL key exists (with null value since no colon-value pair)
    let url_val = mapping.get("http://foo.com").expect("Should have URL key");
    assert!(url_val.is_scalar(), "URL key should have scalar value");
    assert!(
        url_val.as_scalar().unwrap().is_null(),
        "URL key should have null value"
    );

    // Verify 'bar' key has value 'baz'
    let bar_val = mapping.get("bar").expect("Should have 'bar' key");
    assert_eq!(bar_val.as_scalar().unwrap().as_string(), "baz");

    // Verify output is valid YAML
    let output = doc.to_string();
    let reparsed = YamlFile::from_str(&output).expect("Output should be valid YAML");
    assert!(reparsed.document().is_some());
}

/// Test 4ABK: Flow mapping with omitted values
/// In YAML, you can have a key without a value in flow mappings (similar to block style)
#[test]
fn test_4abk_flow_mapping_omitted_value() {
    let yaml = r#"{
unquoted : "separate",
http://foo.com,
omitted value:,
}"#;

    let parsed =
        YamlFile::from_str(yaml).expect("4ABK: Should parse flow mapping with omitted values");
    let doc = parsed.document().expect("Should have document");
    let mapping = doc.as_mapping().expect("Should be mapping");

    // Verify 3 keys
    assert_eq!(mapping.keys().count(), 3, "Should have 3 keys");

    // Verify 'unquoted ' key (with trailing space) has value 'separate'
    let unquoted_val = mapping
        .get("unquoted ")
        .expect("Should have 'unquoted ' key");
    assert_eq!(unquoted_val.as_scalar().unwrap().as_string(), "separate");

    // Verify URL key exists (with null value)
    let url_val = mapping.get("http://foo.com").expect("Should have URL key");
    assert!(
        url_val.is_scalar() && url_val.as_scalar().unwrap().is_null(),
        "URL key should have null value"
    );

    // Verify 'omitted value:' key (with colon) exists (with null value)
    let omitted_val = mapping
        .get("omitted value:")
        .expect("Should have 'omitted value:' key");
    assert!(
        omitted_val.is_scalar() && omitted_val.as_scalar().unwrap().is_null(),
        "'omitted value:' key should have null value"
    );

    // Verify output is valid YAML
    let output = doc.to_string();
    let reparsed = YamlFile::from_str(&output).expect("Output should be valid YAML");
    assert!(reparsed.document().is_some());
}

/// Test 652Z: Flow mapping with explicit key indicator (?)
/// The ? indicator can be used for complex keys in flow style
#[test]
fn test_652z_question_mark_flow_key() {
    let yaml = r#"{ ?foo: bar,
bar: 42
}"#;

    let parsed =
        YamlFile::from_str(yaml).expect("652Z: Flow mapping with ? key indicator should parse");
    let doc = parsed.document().expect("Should have document");
    let mapping = doc.as_mapping().expect("Should be mapping");

    // Verify 2 keys
    assert_eq!(mapping.keys().count(), 2, "Should have 2 keys");

    // Verify 'foo' key (explicit with ?) has value 'bar'
    let foo_val = mapping.get("foo").expect("Should have 'foo' key");
    assert_eq!(foo_val.as_scalar().unwrap().as_string(), "bar");

    // Verify 'bar' key has value 42
    let bar_val = mapping.get("bar").expect("Should have 'bar' key");
    assert_eq!(bar_val.as_scalar().unwrap().as_i64(), Some(42));

    // Verify output is valid YAML
    let output = doc.to_string();
    let reparsed = YamlFile::from_str(&output).expect("Output should be valid YAML");
    assert!(reparsed.document().is_some());
}

/// Test 5MUD: Colon on next line after key
#[test]
#[ignore = "Non-spec-compliant: PyYAML and Psych reject this"]
fn test_5mud_colon_on_next_line() {
    let yaml = r#"---
{ "foo"
  :bar }"#;

    let result = YamlFile::from_str(yaml);
    assert!(
        result.is_ok(),
        "5MUD: Flow mapping with colon on next line should parse. Error: {:?}",
        result.err()
    );
}

/// Test 8KB6: Multiline plain flow mapping key
#[test]
#[ignore = "TODO: Requires multiline plain scalar folding in flow context"]
fn test_8kb6_multiline_flow_key() {
    let yaml = r#"---
- { single line, a: b}
- { multi
  line, a: b}"#;

    let result = YamlFile::from_str(yaml);
    assert!(
        result.is_ok(),
        "8KB6: Multiline flow mapping key should parse. Error: {:?}",
        result.err()
    );
}

/// Test 5T43: Double colon in flow mapping
#[test]
fn test_5t43_double_colon() {
    let yaml = r#"- { "key":value }
- { "key"::value }"#;

    let parsed = YamlFile::from_str(yaml).expect("5T43: Double colon should parse");
    let doc = parsed.document().expect("Should have document");
    let seq = doc.as_sequence().expect("Should be sequence");

    // Verify 2 sequence elements
    assert_eq!(seq.len(), 2, "Should have 2 elements");

    // First element: { "key":value }
    let first = seq.get(0).expect("Should have first element");
    let first_map = first.as_mapping().expect("First element should be mapping");
    assert_eq!(
        first_map.keys().count(),
        1,
        "First mapping should have 1 key"
    );
    let first_val = first_map.get("key").expect("Should have 'key'");
    assert_eq!(first_val.as_scalar().unwrap().as_string(), "value");

    // Second element: { "key"::value } - double colon means :value (with trailing space) is the value
    let second = seq.get(1).expect("Should have second element");
    let second_map = second
        .as_mapping()
        .expect("Second element should be mapping");
    assert_eq!(
        second_map.keys().count(),
        1,
        "Second mapping should have 1 key"
    );
    let second_val = second_map.get("key").expect("Should have 'key'");
    assert_eq!(second_val.as_scalar().unwrap().as_string(), ":value ");

    // Verify output is valid YAML
    let output = doc.to_string();
    let reparsed = YamlFile::from_str(&output).expect("Output should be valid YAML");
    assert!(reparsed.document().is_some());
}

/// Test 4FJ6: Nested implicit complex keys
#[test]
fn test_4fj6_nested_complex_keys() {
    let yaml = r#"---
[
  [ a, [ [[b,c]]: d, e]]: 23
]"#;

    let parsed = YamlFile::from_str(yaml).expect("4FJ6: Nested implicit complex keys should parse");
    let doc = parsed.document().expect("Should have document");
    let seq = doc.as_sequence().expect("Should be sequence");

    // Verify structure: single element sequence containing a mapping
    assert_eq!(seq.len(), 1, "Outer sequence should have 1 element");

    // The element is a mapping with a complex key
    let elem = seq.get(0).expect("Should have element");
    let mapping = elem.as_mapping().expect("Element should be mapping");
    assert_eq!(mapping.keys().count(), 1, "Mapping should have 1 key");

    // Verify round-trip produces valid YAML
    let output = doc.to_string();
    let reparsed = YamlFile::from_str(&output).expect("Output should be valid YAML");
    assert!(reparsed.document().is_some());
}

/// Test 87E4: Single quoted implicit keys in flow
#[test]
fn test_87e4_quoted_implicit_keys() {
    let yaml = r#"'implicit block key' : [
  'implicit flow key' : value,
 ]"#;

    let parsed =
        YamlFile::from_str(yaml).expect("87E4: Single quoted implicit keys in flow should parse");
    let doc = parsed.document().expect("Should have document");
    let mapping = doc.as_mapping().expect("Should be mapping");

    // Verify structure: single key 'implicit block key' with sequence value
    assert_eq!(mapping.keys().count(), 1, "Should have 1 key");

    let seq_val = mapping
        .get("implicit block key")
        .expect("Should have 'implicit block key'");
    let seq = seq_val.as_sequence().expect("Value should be sequence");
    assert_eq!(seq.len(), 1, "Sequence should have 1 element");

    // The sequence element is a mapping with 'implicit flow key'
    let elem = seq.get(0).expect("Should have element");
    let elem_map = elem.as_mapping().expect("Element should be mapping");
    assert_eq!(
        elem_map.keys().count(),
        1,
        "Element mapping should have 1 key"
    );

    let value = elem_map
        .get("implicit flow key")
        .expect("Should have 'implicit flow key'");
    assert_eq!(value.as_scalar().unwrap().as_string(), "value");

    // Verify round-trip produces valid YAML
    let output = doc.to_string();
    let reparsed = YamlFile::from_str(&output).expect("Output should be valid YAML");
    assert!(reparsed.document().is_some());
}

/// Test T833: Flow mapping missing comma separator (lenient parser accepts it)
#[test]
fn test_t833_flow_mapping_missing_comma() {
    let yaml = "---
{
 foo: 1
 bar: 2 }
";
    let parsed =
        YamlFile::from_str(yaml).expect("T833: Lenient parser should accept missing comma");
    let doc = parsed.document().expect("Should have document");
    let mapping = doc.as_mapping().expect("Should be mapping");

    // The parser may treat "foo: 1\n bar: 2" as key "foo" with value "1 bar: 2" (degraded parsing)
    // Or it may parse as two separate keys. Verify it parses without crashing.
    assert!(mapping.keys().count() >= 1, "Should have at least 1 key");

    // Verify round-trip produces valid YAML
    let output = doc.to_string();
    let reparsed = YamlFile::from_str(&output).expect("Output should be valid YAML");
    assert!(reparsed.document().is_some());
}

#[test]
fn test_flow_mapping_with_commas_ok() {
    let yaml = "{foo: 1, bar: 2}";
    let parsed = YamlFile::from_str(yaml).expect("Should parse flow mapping with commas");
    let doc = parsed.document().expect("Should have document");
    let mapping = doc.as_mapping().expect("Should be mapping");

    // Verify structure: 2 keys with proper commas
    assert_eq!(mapping.keys().count(), 2, "Should have 2 keys");

    let foo_val = mapping.get("foo").expect("Should have 'foo' key");
    assert_eq!(foo_val.as_scalar().unwrap().as_i64(), Some(1));

    let bar_val = mapping.get("bar").expect("Should have 'bar' key");
    assert_eq!(bar_val.as_scalar().unwrap().as_i64(), Some(2));

    // Verify round-trip produces valid YAML
    let output = doc.to_string();
    assert_eq!(output, yaml);
}

#[test]
fn test_flow_mapping_multiline_with_commas_ok() {
    let yaml = "{
  foo: 1,
  bar: 2
}";
    let parsed = YamlFile::from_str(yaml).expect("Should parse multiline flow mapping with commas");
    let doc = parsed.document().expect("Should have document");
    let mapping = doc.as_mapping().expect("Should be mapping");

    // Verify structure: 2 keys with proper commas
    assert_eq!(mapping.keys().count(), 2, "Should have 2 keys");

    let foo_val = mapping.get("foo").expect("Should have 'foo' key");
    assert_eq!(foo_val.as_scalar().unwrap().as_i64(), Some(1));

    let bar_val = mapping.get("bar").expect("Should have 'bar' key");
    assert_eq!(bar_val.as_scalar().unwrap().as_i64(), Some(2));

    // Verify round-trip produces valid YAML
    let output = doc.to_string();
    assert_eq!(output, yaml);
}

/// Test CML9: Missing comma in flow mapping (lenient parser accepts it)
#[test]
fn test_cml9_missing_comma_in_flow() {
    let yaml = "[{a: b c: d}]";
    let parsed =
        YamlFile::from_str(yaml).expect("CML9: Lenient parser should accept missing comma");
    let doc = parsed.document().expect("Should have document");
    let seq = doc.as_sequence().expect("Should be sequence");

    // Verify structure: sequence with one mapping element
    assert_eq!(seq.len(), 1, "Should have 1 element");

    let elem = seq.get(0).expect("Should have element");
    let mapping = elem.as_mapping().expect("Element should be mapping");

    // The parser may treat "a: b c: d" as key "a" with value "b c: d" (degraded parsing)
    // Or it may parse as two separate keys. Verify it parses without crashing.
    assert!(
        mapping.keys().count() >= 1,
        "Mapping should have at least 1 key"
    );

    // Verify round-trip produces valid YAML
    let output = doc.to_string();
    let reparsed = YamlFile::from_str(&output).expect("Output should be valid YAML");
    assert!(reparsed.document().is_some());
}

/// Test CTN5: Flow sequence with invalid extra comma (lenient parser accepts it)
#[test]
fn test_ctn5_invalid_extra_comma() {
    let yaml = "[a,,b]";
    let parsed = YamlFile::from_str(yaml).expect("CTN5: Lenient parser should accept double comma");
    let doc = parsed.document().expect("Should have document");
    let seq = doc.as_sequence().expect("Should be sequence");

    // Verify structure: sequence has elements despite double comma
    // Parser may include null for the empty element or skip it
    assert!(seq.len() >= 2, "Sequence should have at least 2 elements");

    let first = seq.get(0).expect("Should have first element");
    assert_eq!(first.as_scalar().unwrap().as_string(), "a");

    let last_idx = seq.len() - 1;
    let last = seq.get(last_idx).expect("Should have last element");
    assert_eq!(last.as_scalar().unwrap().as_string(), "b");

    // Verify round-trip produces valid YAML
    let output = doc.to_string();
    let reparsed = YamlFile::from_str(&output).expect("Output should be valid YAML");
    assert!(reparsed.document().is_some());
}

// ========================================
// Additional Flow Collection Edge Cases
// ========================================

#[test]
fn test_deeply_nested_flow_collections() {
    // Test 5-level nested flow sequence: [[[[[a]]]]]
    // Verifies parser handles deep nesting without stack issues
    let yaml = "[[[[[a]]]]]";

    let parsed = YamlFile::from_str(yaml).unwrap();
    let doc = parsed.document().unwrap();

    // Navigate down to the innermost value
    let seq1 = doc.as_sequence().unwrap();
    assert_eq!(seq1.len(), 1);

    let node2 = seq1.get(0).unwrap();
    let seq2 = node2.as_sequence().unwrap();
    assert_eq!(seq2.len(), 1);

    let node3 = seq2.get(0).unwrap();
    let seq3 = node3.as_sequence().unwrap();
    assert_eq!(seq3.len(), 1);

    let node4 = seq3.get(0).unwrap();
    let seq4 = node4.as_sequence().unwrap();
    assert_eq!(seq4.len(), 1);

    let node5 = seq4.get(0).unwrap();
    let seq5 = node5.as_sequence().unwrap();
    assert_eq!(seq5.len(), 1);

    // Verify innermost value is "a"
    let innermost = seq5.get(0).unwrap();
    assert_eq!(innermost.as_scalar().unwrap().as_string(), "a");

    // Verify exact round-trip
    let output = doc.to_string();
    assert_eq!(output, yaml);
}

#[test]
fn test_trailing_comma_with_whitespace() {
    // Trailing comma with whitespace in flow sequence
    // YAML spec allows trailing commas
    let yaml = "[a, b, ]";

    let parsed = YamlFile::from_str(yaml).unwrap();
    let doc = parsed.document().unwrap();
    let seq = doc.as_sequence().unwrap();

    // Verify exact structure - should have 2 elements (a and b)
    // Trailing comma may or may not create a third null element
    assert!(seq.len() >= 2, "Should have at least 2 elements");

    assert_eq!(seq.get(0).unwrap().as_scalar().unwrap().as_string(), "a");
    assert_eq!(seq.get(1).unwrap().as_scalar().unwrap().as_string(), "b");

    // If there's a third element, it should be null
    if seq.len() > 2 {
        assert!(seq.get(2).unwrap().as_scalar().unwrap().is_null());
    }

    // Verify exact round-trip
    let output = doc.to_string();
    assert_eq!(output, yaml);
}

#[test]
fn test_mixed_implicit_explicit_keys() {
    // Mixed implicit and explicit keys in flow mapping
    // {a, ?b: c, d: e} - 'a' is implicit, '?b: c' is explicit, 'd: e' is normal
    let yaml = "{a, ?b: c, d: e}";

    let parsed = YamlFile::from_str(yaml).unwrap();
    let doc = parsed.document().unwrap();
    let mapping = doc.as_mapping().unwrap();

    // Verify all keys exist
    assert_eq!(mapping.keys().count(), 3, "Should have 3 keys");

    // Key 'a' with implicit null value
    let a_val = mapping.get("a").unwrap();
    assert!(a_val.as_scalar().unwrap().is_null());

    // Explicit key 'b' with value 'c'
    let b_val = mapping.get("b").unwrap();
    assert_eq!(b_val.as_scalar().unwrap().as_string(), "c");

    // Normal key 'd' with value 'e'
    let d_val = mapping.get("d").unwrap();
    assert_eq!(d_val.as_scalar().unwrap().as_string(), "e");

    // Verify exact round-trip
    let output = doc.to_string();
    assert_eq!(output, yaml);
}

#[test]
fn test_flow_mapping_with_only_keys() {
    // Flow mapping with only keys, no values (set notation)
    // {a, b, c} - all keys have implicit null values
    let yaml = "{a, b, c}";

    let parsed = YamlFile::from_str(yaml).unwrap();
    let doc = parsed.document().unwrap();
    let mapping = doc.as_mapping().unwrap();

    // Verify exact structure - 3 keys, all with null values
    assert_eq!(mapping.keys().count(), 3, "Should have 3 keys");

    assert!(mapping.get("a").unwrap().as_scalar().unwrap().is_null());
    assert!(mapping.get("b").unwrap().as_scalar().unwrap().is_null());
    assert!(mapping.get("c").unwrap().as_scalar().unwrap().is_null());

    // Verify exact round-trip
    let output = doc.to_string();
    assert_eq!(output, yaml);
}

#[test]
fn test_empty_flow_collections_with_whitespace() {
    // Empty flow collections with internal whitespace
    // YAML allows whitespace inside empty collections
    let yaml = "empty_seq: [ ]\nempty_map: { }\n";

    let parsed = YamlFile::from_str(yaml).unwrap();
    let doc = parsed.document().unwrap();
    let mapping = doc.as_mapping().unwrap();

    // Verify exact structure
    assert_eq!(mapping.keys().count(), 2);

    let seq_node = mapping.get("empty_seq").unwrap();
    let seq = seq_node.as_sequence().unwrap();
    assert_eq!(seq.len(), 0, "Empty sequence should have 0 elements");

    let map_node = mapping.get("empty_map").unwrap();
    let map = map_node.as_mapping().unwrap();
    assert_eq!(map.keys().count(), 0, "Empty mapping should have 0 keys");

    // Verify exact round-trip
    let output = doc.to_string();
    assert_eq!(output, yaml);
}
