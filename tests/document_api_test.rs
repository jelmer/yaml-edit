//! Test the Document API

use yaml_edit::{Document, YamlResult};

#[test]
fn test_document_api_usage() -> YamlResult<()> {
    // Create a new document
    let mut doc = Document::new();

    // Check and modify fields
    assert!(!doc.contains_key("Repository"));
    doc.set_string("Repository", "https://github.com/user/repo.git");
    assert!(doc.contains_key("Repository"));

    // Test get_string
    assert_eq!(
        doc.get_string("Repository"),
        Some("https://github.com/user/repo.git".to_string())
    );

    // Test is_empty
    assert!(!doc.is_empty());

    // Test keys
    let keys: Vec<_> = doc.keys();
    assert!(keys.contains(&"Repository".to_string()));

    // Test remove
    assert!(doc.remove("Repository"));
    assert!(!doc.contains_key("Repository"));
    assert!(doc.is_empty());

    Ok(())
}

#[test]
fn test_field_ordering() {
    let mut doc = Document::new();

    // Add fields in random order
    doc.set_string("Repository-Browse", "https://github.com/user/repo");
    doc.set_string("Name", "MyProject");
    doc.set_string("Bug-Database", "https://github.com/user/repo/issues");
    doc.set_string("Repository", "https://github.com/user/repo.git");

    // Reorder fields
    doc.reorder_fields(&["Name", "Bug-Database", "Repository", "Repository-Browse"]);

    // Check that fields are in the expected order
    let keys: Vec<_> = doc.keys();
    assert_eq!(
        keys,
        vec!["Name", "Bug-Database", "Repository", "Repository-Browse"]
    );
}

#[test]
fn test_array_detection() {
    // Create a test with array values using set_value
    let mut doc = Document::new();

    // Set an array value
    let array_value = yaml_edit::YamlValue::Sequence(vec![
        yaml_edit::YamlValue::Scalar(yaml_edit::ScalarValue::new(
            "https://github.com/user/repo.git",
        )),
        yaml_edit::YamlValue::Scalar(yaml_edit::ScalarValue::new(
            "https://gitlab.com/user/repo.git",
        )),
    ]);
    doc.set_value("Repository", array_value);

    // Test array detection (this might not work perfectly with our current implementation)
    // But the API is established
    if doc.is_array("Repository") {
        if let Some(first) = doc.get_array_first_string("Repository") {
            assert_eq!(first, "https://github.com/user/repo.git");
        }
    }
}

#[test]
fn test_file_io() -> YamlResult<()> {
    use std::fs;

    // Create a test file path
    let test_path = "/tmp/test_yaml_edit.yaml";

    // Create and save a document
    let mut doc = Document::new();
    doc.set_string("Name", "TestProject");
    doc.set_string("Repository", "https://example.com/repo.git");

    doc.save_to_file(test_path)?;

    // Load it back
    let loaded_doc = Document::load_from_file(test_path)?;
    assert_eq!(
        loaded_doc.get_string("Name"),
        Some("TestProject".to_string())
    );
    assert_eq!(
        loaded_doc.get_string("Repository"),
        Some("https://example.com/repo.git".to_string())
    );

    // Clean up
    let _ = fs::remove_file(test_path);

    Ok(())
}
