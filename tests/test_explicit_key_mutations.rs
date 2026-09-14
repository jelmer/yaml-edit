mod common;
use common::assert_file_cst_ok;

#[test]
fn test_explicit_key_mutations() {
    use std::str::FromStr;
    use yaml_edit::YamlFile;

    // Test 1: Modify a value in a mapping with explicit keys
    let yaml = "? key1\n: value1\n? key2\n: value2\n";

    let doc = YamlFile::from_str(yaml).unwrap();
    let mapping = doc.document().unwrap().as_mapping().unwrap();

    // Verify initial state via API
    assert_eq!(mapping.len(), 2);
    assert_eq!(
        mapping
            .get("key1")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "value1"
    );
    assert_eq!(
        mapping
            .get("key2")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "value2"
    );

    // Change value1 to newvalue
    mapping.set("key1", "newvalue");
    assert_file_cst_ok(&doc);

    // Verify mutation via API
    assert_eq!(
        mapping
            .get("key1")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "newvalue"
    );

    // Verify exact output preserves explicit key format
    let output = doc.to_string();
    assert_eq!(output, "? key1\n: newvalue\n? key2\n: value2\n");

    // Test 2: Add a new key to a mapping with explicit keys
    let yaml2 = "? existing\n: value\n";

    let doc2 = YamlFile::from_str(yaml2).unwrap();
    let mapping2 = doc2.document().unwrap().as_mapping().unwrap();

    // Verify initial state via API
    assert_eq!(mapping2.len(), 1);
    assert_eq!(
        mapping2
            .get("existing")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "value"
    );

    // Add new key
    mapping2.set("newkey", "newvalue");
    assert_file_cst_ok(&doc2);

    // Verify addition via API
    assert_eq!(mapping2.len(), 2);
    assert_eq!(
        mapping2
            .get("newkey")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "newvalue"
    );

    // Verify exact output - new key should use explicit format to match existing style
    let output2 = doc2.to_string();
    assert_eq!(output2, "? existing\n: value\n? newkey\n: newvalue\n");
}

#[test]
fn test_explicit_key_set_nested_keeps_indent() {
    use std::str::FromStr;
    use yaml_edit::YamlFile;

    let doc = YamlFile::from_str("outer:\n  ? a\n  : 1\n").unwrap();
    let outer = doc
        .document()
        .unwrap()
        .as_mapping()
        .unwrap()
        .get_mapping("outer")
        .unwrap();
    outer.set("b", "9");
    assert_eq!(doc.to_string(), "outer:\n  ? a\n  : 1\n  ? b\n  : '9'\n");
}

#[test]
fn test_explicit_key_insert_at_index_middle_keeps_indent() {
    use std::str::FromStr;
    use yaml_edit::YamlFile;

    let doc = YamlFile::from_str("outer:\n  ? a\n  : 1\n  ? c\n  : 2\n").unwrap();
    let outer = doc
        .document()
        .unwrap()
        .as_mapping()
        .unwrap()
        .get_mapping("outer")
        .unwrap();
    outer.insert_at_index(1, "b", "9");
    assert_eq!(
        doc.to_string(),
        "outer:\n  ? a\n  : 1\n  ? b\n  : '9'\n  ? c\n  : 2\n"
    );
}

#[test]
fn test_explicit_key_insert_at_index_first_keeps_indent() {
    use std::str::FromStr;
    use yaml_edit::YamlFile;

    let doc = YamlFile::from_str("outer:\n  ? b\n  : 1\n  ? c\n  : 2\n").unwrap();
    let outer = doc
        .document()
        .unwrap()
        .as_mapping()
        .unwrap()
        .get_mapping("outer")
        .unwrap();
    outer.insert_at_index(0, "a", "9");
    assert_eq!(
        doc.to_string(),
        "outer:\n  ? a\n  : '9'\n  ? b\n  : 1\n  ? c\n  : 2\n"
    );
}

#[test]
fn test_explicit_key_reorder_fields_keeps_line_breaks() {
    use std::str::FromStr;
    use yaml_edit::Document;

    let doc = Document::from_str("? a\n: 1\n? b\n: 2\n").unwrap();
    doc.as_mapping().unwrap().reorder_fields(["b"]);
    assert_eq!(doc.to_string(), "? b\n: 2\n? a\n: 1\n");
}
