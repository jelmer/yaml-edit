use std::str::FromStr;
use yaml_edit::Yaml;

#[test]
fn test_binary_numbers_in_yaml() {
    let yaml_str = r#"
binary1: 0b1010
binary2: 0B1111
negative_binary: -0b1010
positive_binary: +0b101
"#;

    let yaml = Yaml::from_str(yaml_str).expect("Failed to parse YAML");
    let output = yaml.to_string();

    // Check that the original format is preserved
    assert!(output.contains("0b1010"));
    assert!(output.contains("0B1111"));
    assert!(output.contains("-0b1010"));
    assert!(output.contains("+0b101"));

    // Verify values can be retrieved
    if let Some(doc) = yaml.document() {
        assert_eq!(doc.get_string("binary1"), Some("0b1010".to_string()));
        assert_eq!(doc.get_string("binary2"), Some("0B1111".to_string()));
        assert_eq!(
            doc.get_string("negative_binary"),
            Some("-0b1010".to_string())
        );
        assert_eq!(
            doc.get_string("positive_binary"),
            Some("+0b101".to_string())
        );
    } else {
        panic!("Failed to get document");
    }
}

#[test]
fn test_modern_octal_numbers_in_yaml() {
    let yaml_str = r#"
octal1: 0o755
octal2: 0O644
negative_octal: -0o755
positive_octal: +0o644
"#;

    let yaml = Yaml::from_str(yaml_str).expect("Failed to parse YAML");
    let output = yaml.to_string();

    // Check that the original format is preserved
    assert!(output.contains("0o755"));
    assert!(output.contains("0O644"));
    assert!(output.contains("-0o755"));
    assert!(output.contains("+0o644"));

    // Verify values can be retrieved
    if let Some(doc) = yaml.document() {
        assert_eq!(doc.get_string("octal1"), Some("0o755".to_string()));
        assert_eq!(doc.get_string("octal2"), Some("0O644".to_string()));
        assert_eq!(doc.get_string("negative_octal"), Some("-0o755".to_string()));
        assert_eq!(doc.get_string("positive_octal"), Some("+0o644".to_string()));
    } else {
        panic!("Failed to get document");
    }
}

#[test]
fn test_mixed_number_formats() {
    let yaml_str = r#"
decimal: 42
hex_lower: 0xff
hex_upper: 0XFF
binary: 0b1010
modern_octal: 0o755
legacy_octal: 0755
scientific: 1.5e10
float: 3.14159
"#;

    let yaml = Yaml::from_str(yaml_str).expect("Failed to parse YAML");
    let output = yaml.to_string();

    // All formats should be preserved
    assert!(output.contains("42"));
    assert!(output.contains("0xff"));
    assert!(output.contains("0XFF"));
    assert!(output.contains("0b1010"));
    assert!(output.contains("0o755"));
    assert!(output.contains("0755"));
    assert!(output.contains("1.5e10"));
    assert!(output.contains("3.14159"));
}

#[test]
fn test_invalid_number_formats_as_strings() {
    // Invalid number formats should be treated as strings
    let yaml_str = r#"
empty_binary: 0b
invalid_binary: 0b1012
empty_octal: 0o
invalid_octal: 0o789
invalid_hex: 0xGH
"#;

    let yaml = Yaml::from_str(yaml_str).expect("Failed to parse YAML");

    // These should all parse successfully as strings
    if let Some(doc) = yaml.document() {
        assert_eq!(doc.get_string("empty_binary"), Some("0b".to_string()));
        assert_eq!(doc.get_string("invalid_binary"), Some("0b1012".to_string()));
        assert_eq!(doc.get_string("empty_octal"), Some("0o".to_string()));
        assert_eq!(doc.get_string("invalid_octal"), Some("0o789".to_string()));
        assert_eq!(doc.get_string("invalid_hex"), Some("0xGH".to_string()));
    } else {
        panic!("Failed to get document");
    }
}

#[test]
fn test_number_formats_in_sequences() {
    let yaml_str = r#"
numbers:
  - 0b1010
  - 0o755
  - 0xFF
  - 42
  - -0b101
  - +0o777
"#;

    let yaml = Yaml::from_str(yaml_str).expect("Failed to parse YAML");
    let output = yaml.to_string();

    // Check all formats are preserved in the sequence
    assert!(output.contains("0b1010"));
    assert!(output.contains("0o755"));
    assert!(output.contains("0xFF"));
    assert!(output.contains("42"));
    assert!(output.contains("-0b101"));
    assert!(output.contains("+0o777"));
}

#[test]
fn test_number_formats_in_flow_collections() {
    let yaml_str = r#"
flow_seq: [0b1010, 0o755, 0xFF, 42]
flow_map: {binary: 0b101, octal: 0o644, hex: 0xAB}
"#;

    let yaml = Yaml::from_str(yaml_str).expect("Failed to parse YAML");
    let output = yaml.to_string();

    // Flow collections should preserve number formats
    assert!(output.contains("0b1010"));
    assert!(output.contains("0o755"));
    assert!(output.contains("0xFF"));
    assert!(output.contains("0b101"));
    assert!(output.contains("0o644"));
    assert!(output.contains("0xAB"));
}

#[test]
fn test_edge_case_zero_values() {
    let yaml_str = r#"
zero: 0
double_zero: 00
triple_zero: 000
octal_zero: 0o0
binary_zero: 0b0
hex_zero: 0x0
"#;

    let yaml = Yaml::from_str(yaml_str).expect("Failed to parse YAML");

    if let Some(doc) = yaml.document() {
        assert_eq!(doc.get_string("zero"), Some("0".to_string()));
        assert_eq!(doc.get_string("double_zero"), Some("00".to_string()));
        assert_eq!(doc.get_string("triple_zero"), Some("000".to_string()));
        assert_eq!(doc.get_string("octal_zero"), Some("0o0".to_string()));
        assert_eq!(doc.get_string("binary_zero"), Some("0b0".to_string()));
        assert_eq!(doc.get_string("hex_zero"), Some("0x0".to_string()));
    } else {
        panic!("Failed to get document");
    }
}
