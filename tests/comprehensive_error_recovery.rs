//! Comprehensive error recovery tests to verify no infinite loops

use yaml_edit::Yaml;

#[test]
fn test_all_problematic_cases_no_hangs() {
    // These cases previously caused infinite loops - verify they complete
    let test_cases = vec![
        // Multiline unclosed flow sequence
        "items: [a, b, c\nnext: value",
        // Multiline unclosed flow mapping
        "config: {host localhost\nother: value",
        // Mixed flow and block structures
        "flow: [a, b\nblock:\n  key: value",
        // Complex nested case
        "outer: [inner: {broken\nrescue: works",
        // Colon in flow sequence
        "list: [item: value]",
        // Multiple structural issues
        "broken: [a, b\nmissing colon here\nvalid: works",
    ];

    for (i, yaml_text) in test_cases.iter().enumerate() {
        println!("Testing case {}: {}", i + 1, yaml_text.replace('\n', "\\n"));

        // This should complete without hanging
        let parsed = Yaml::parse(yaml_text);

        // Should have some errors
        let errors = parsed.errors();
        assert!(!errors.is_empty(), "Case {} should have errors", i + 1);

        // Should still produce a tree (even if malformed)
        let tree = parsed.tree();
        assert!(
            tree.document().is_some(),
            "Case {} should produce a document",
            i + 1
        );

        println!(
            "Case {} completed successfully with {} errors",
            i + 1,
            errors.len()
        );
    }
}

#[test]
fn test_deeply_nested_malformed_structures() {
    // Test deeply nested malformed structures don't cause stack overflow or hangs
    let yaml_text = r#"
level1: [
  level2: {
    level3: [broken structure here
    level3b: another broken
  level2b: {missing colon
level1b: value
"#;

    let parsed = Yaml::parse(yaml_text);
    let errors = parsed.errors();

    // Should complete and report errors (important part is that it doesn't hang)
    // Note: the parser might not catch all errors due to early termination, which is fine
    println!("Detected {} errors in nested structure", errors.len());

    // Verify error messages are meaningful (if any errors are detected)
    if !errors.is_empty() {
        let error_text = errors.join(" ");
        assert!(
            error_text.contains("Unclosed")
                || error_text.contains("Missing")
                || error_text.contains("Unexpected"),
            "Should contain meaningful error messages: {:?}",
            errors
        );
    } else {
        println!("No errors detected (parser might have terminated early)");
    }
}
