//! Tests for Byte Order Mark (BOM) handling

use std::str::FromStr;
use yaml_edit::YamlFile;

#[test]
fn test_utf8_bom_handling() {
    // UTF-8 BOM (\uFEFF or 0xEF 0xBB 0xBF)
    let bom_yaml = "\u{FEFF}key: value";

    let parsed = YamlFile::from_str(bom_yaml).unwrap();
    let doc = parsed.documents().next().unwrap();
    let mapping = doc.as_mapping().unwrap();

    // BOM should be stripped for API access, allowing us to access the key normally
    let val = mapping.get("key").unwrap();
    assert_eq!(
        val.as_scalar().map(|s| s.as_string()),
        Some("value".to_string())
    );

    // Verify the key is exactly "key" without BOM in API
    let keys: Vec<String> = mapping.keys().map(|k| k.to_string()).collect();
    assert_eq!(keys.len(), 1);
    assert_eq!(keys[0], "key");

    // But BOM must be preserved in output (lossless property)
    let output = parsed.to_string();
    assert_eq!(output, bom_yaml);
    assert!(output.starts_with('\u{FEFF}'));
}

#[test]
fn test_utf8_bom_not_present() {
    // No BOM - control test
    let no_bom_yaml = "key: value";

    let parsed = YamlFile::from_str(no_bom_yaml).unwrap();
    let doc = parsed.documents().next().unwrap();
    let mapping = doc.as_mapping().unwrap();

    let val = mapping.get("key").unwrap();
    assert_eq!(
        val.as_scalar().map(|s| s.as_string()),
        Some("value".to_string())
    );
}

#[test]
fn test_bom_with_document_marker() {
    // BOM followed by explicit document marker
    let bom_doc_yaml = "\u{FEFF}---\nkey: value";

    let parsed = YamlFile::from_str(bom_doc_yaml).unwrap();
    let doc = parsed.documents().next().unwrap();
    let mapping = doc.as_mapping().unwrap();

    let val = mapping.get("key").unwrap();
    assert_eq!(
        val.as_scalar().map(|s| s.as_string()),
        Some("value".to_string())
    );
}

#[test]
fn test_bom_is_stripped_from_first_key() {
    // UTF-8 BOM should be stripped for API access but preserved in output (lossless)
    let bom_yaml = "\u{FEFF}key: value";

    let parsed = YamlFile::from_str(bom_yaml).unwrap();
    let doc = parsed.documents().next().unwrap();
    let mapping = doc.as_mapping().unwrap();

    // BOM should not appear in the key when accessing via API
    let keys: Vec<String> = mapping.keys().map(|k| k.to_string()).collect();
    assert_eq!(keys.len(), 1);
    assert_eq!(keys[0], "key");
    assert!(!keys[0].starts_with('\u{FEFF}'));

    // But BOM should be preserved in the output (lossless property)
    let output = parsed.to_string();
    assert_eq!(output, bom_yaml);
    assert!(output.starts_with('\u{FEFF}'));
}

#[test]
fn test_bom_with_explicit_doc_marker() {
    // BOM followed by explicit document marker
    let bom_doc_yaml = "\u{FEFF}---\nkey: value";

    let parsed = YamlFile::from_str(bom_doc_yaml).unwrap();
    let doc = parsed.documents().next().unwrap();
    let mapping = doc.as_mapping().unwrap();

    let keys: Vec<String> = mapping.keys().map(|k| k.to_string()).collect();
    assert_eq!(keys.len(), 1);
    assert_eq!(keys[0], "key");

    // BOM should be preserved in output
    let output = parsed.to_string();
    assert_eq!(output, bom_doc_yaml);
    assert!(output.starts_with('\u{FEFF}'));
}

#[test]
fn test_bom_between_lines() {
    // BOM in the middle of a document - becomes part of the key and is preserved
    let bom_in_middle = "key1: value1\n\u{FEFF}key2: value2";

    let parsed = YamlFile::from_str(bom_in_middle).unwrap();
    let doc = parsed.documents().next().unwrap();
    let mapping = doc.as_mapping().unwrap();

    let keys: Vec<String> = mapping.keys().map(|k| k.to_string()).collect();
    assert_eq!(keys.len(), 2);
    assert_eq!(keys[0], "key1");
    // BOM in the middle becomes part of the key
    assert_eq!(keys[1], "\u{FEFF}key2");

    // BOM should be preserved in output
    let output = parsed.to_string();
    assert_eq!(output, bom_in_middle);
}
