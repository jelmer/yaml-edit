use std::fs;
use yaml_edit::{Document, YamlFile};

#[test]
fn test_multi_key_yaml_parsing() {
    let yaml_content = r#"---
Repository: https://github.com/example/blah.git
Repository-Browse: https://github.com/example/blah
Security-Contact: https://github.com/example/blah/tree/HEAD/SECURITY.md
"#;

    // Create a temporary file
    let temp_path = "/tmp/test_multi_key.yaml";
    fs::write(temp_path, yaml_content).unwrap();

    // Load the document
    let doc = Document::from_file(temp_path).unwrap();

    // Test that all keys are found
    assert_eq!(doc.keys().count(), 3, "Expected 3 keys");

    // Test that keys are correct
    let mut key_strings: Vec<String> = doc.keys().map(|k| k.to_string()).collect();
    key_strings.sort();
    assert_eq!(
        key_strings,
        ["Repository", "Repository-Browse", "Security-Contact"]
    );

    // Test that values are correct
    let repo_value = doc.get_string("Repository").unwrap();
    assert_eq!(repo_value, "https://github.com/example/blah.git");

    let browse_value = doc.get_string("Repository-Browse").unwrap();
    assert_eq!(browse_value, "https://github.com/example/blah");

    let security_value = doc.get_string("Security-Contact").unwrap();
    assert_eq!(
        security_value,
        "https://github.com/example/blah/tree/HEAD/SECURITY.md"
    );
}

#[test]
fn test_single_key_yaml_parsing() {
    let yaml_content = r#"---
Repository: https://github.com/example/blah.git
"#;

    let temp_path = "/tmp/test_single_key.yaml";
    fs::write(temp_path, yaml_content).unwrap();

    let doc = Document::from_file(temp_path).unwrap();

    let keys: Vec<_> = doc.keys().collect();
    assert_eq!(keys.len(), 1);
    assert_eq!(keys[0], "Repository");

    let repo_value = doc.get_string("Repository").unwrap();
    assert_eq!(repo_value, "https://github.com/example/blah.git");
}

#[test]
fn test_multiple_urls_as_values() {
    // Test that multiple URLs with colons don't confuse the parser
    let yaml_content = r#"---
homepage: https://example.com:8080/path
documentation: http://docs.example.com/guide
api: https://api.example.com:443/v1
download: ftp://ftp.example.com:21/files/latest.tar.gz
"#;

    let temp_path = "/tmp/test_multiple_urls.yaml";
    fs::write(temp_path, yaml_content).unwrap();

    let doc = Document::from_file(temp_path).unwrap();

    // Verify all keys are found
    assert_eq!(doc.keys().count(), 4, "Should find all 4 keys");

    // Verify each URL is parsed correctly
    assert_eq!(
        doc.get_string("homepage").unwrap(),
        "https://example.com:8080/path"
    );
    assert_eq!(
        doc.get_string("documentation").unwrap(),
        "http://docs.example.com/guide"
    );
    assert_eq!(
        doc.get_string("api").unwrap(),
        "https://api.example.com:443/v1"
    );
    assert_eq!(
        doc.get_string("download").unwrap(),
        "ftp://ftp.example.com:21/files/latest.tar.gz"
    );
}

#[test]
fn test_colons_in_various_contexts() {
    // Test colons in different value contexts
    let yaml_content = r#"---
time: "10:30:45"
ratio: 16:9
path: /usr/local/bin:/usr/bin:/bin
windows_path: C:\Users\Example\Documents
equation: "y = 2x + 3: where x > 0"
"#;

    let temp_path = "/tmp/test_colons_contexts.yaml";
    fs::write(temp_path, yaml_content).unwrap();

    let doc = Document::from_file(temp_path).unwrap();

    assert_eq!(doc.keys().count(), 5, "Should find all 5 keys");

    // Verify values with colons are preserved correctly
    // Note: get_string() returns the unquoted string content
    assert_eq!(doc.get_string("time").unwrap(), "10:30:45");
    assert_eq!(doc.get_string("ratio").unwrap(), "16:9");
    assert_eq!(
        doc.get_string("path").unwrap(),
        "/usr/local/bin:/usr/bin:/bin"
    );
    assert_eq!(
        doc.get_string("windows_path").unwrap(),
        "C:\\Users\\Example\\Documents"
    );
    assert_eq!(
        doc.get_string("equation").unwrap(),
        "y = 2x + 3: where x > 0"
    );
}

#[test]
fn test_parse_then_edit_multikey_document() {
    // Test that we can parse and then edit a multi-key document
    let yaml_content = r#"---
first: https://first.example.com
second: https://second.example.com
third: https://third.example.com
"#;

    let parsed = YamlFile::parse(yaml_content);
    let doc = parsed.tree().documents().next().unwrap();

    // Verify initial state
    assert_eq!(doc.keys().count(), 3);

    // Edit a value
    doc.set("second", "https://updated.example.com").unwrap();

    // Verify the edit worked and other keys remain unchanged
    assert_eq!(
        doc.get_string("first").unwrap(),
        "https://first.example.com"
    );
    assert_eq!(
        doc.get_string("second").unwrap(),
        "https://updated.example.com"
    );
    assert_eq!(
        doc.get_string("third").unwrap(),
        "https://third.example.com"
    );

    // Add a new key
    doc.set("fourth", "https://fourth.example.com").unwrap();
    assert_eq!(doc.keys().count(), 4);
    assert_eq!(
        doc.get_string("fourth").unwrap(),
        "https://fourth.example.com"
    );
}
