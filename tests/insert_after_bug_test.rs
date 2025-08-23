use std::str::FromStr;
use yaml_edit::Yaml;

#[test]
fn test_insert_after_preserves_newline() {
    // Test with the examples from the bug report
    let yaml = "---\nBug-Database: https://github.com/example/example/issues\nBug-Submit: https://github.com/example/example/issues/new\n";
    let mut yaml_obj = Yaml::from_str(yaml).unwrap();

    // For now, test using Document directly since Yaml::insert_after was removed
    if let Some(mut doc) = yaml_obj.document() {
        let result = doc.insert_after(
            "Bug-Submit",
            "Repository",
            "https://github.com/example/example.git",
        );
        assert!(result, "insert_after should return true when key is found");

        // Check the document output directly
        let output = doc.to_string();
        println!("Document output:\n{}", output);

        // Check that newline is preserved
        assert!(output.contains("Bug-Submit: https://github.com/example/example/issues/new\n"));
        // The URL contains colons, so it MUST be quoted per YAML spec
        assert!(output.contains("Repository: 'https://github.com/example/example.git'"));
        assert!(
            !output.contains("newRepository"),
            "Should not concatenate fields"
        );
    }
}

#[test]
fn test_insert_after_without_trailing_newline() {
    // Test the specific bug case - YAML without trailing newline
    let yaml = "---\nBug-Database: https://github.com/example/example/issues\nBug-Submit: https://github.com/example/example/issues/new";
    let mut yaml_obj = Yaml::from_str(yaml).unwrap();

    if let Some(mut doc) = yaml_obj.document() {
        let result = doc.insert_after(
            "Bug-Submit",
            "Repository",
            "https://github.com/example/example.git",
        );
        assert!(result, "insert_after should return true when key is found");

        let output = doc.to_string();
        println!("Document output:\n{}", output);

        // Check that fields are not concatenated (the original bug)
        assert!(
            !output.contains("newRepository"),
            "Should not concatenate fields"
        );
        assert!(output.contains("Repository: 'https://github.com/example/example.git'"));
    }
}
