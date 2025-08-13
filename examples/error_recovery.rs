//! Error recovery example
//!
//! This example demonstrates the enhanced error recovery system that provides
//! detailed error messages with context, suggestions, and line/column information.

use yaml_edit::Yaml;

fn main() {
    println!("=== YAML Error Recovery Examples ===\n");

    // Example 1: Missing colon in mapping
    test_error_recovery(
        "Missing Colon",
        r#"
name John
age: 30
"#,
    );

    // Example 2: Unterminated quoted string
    test_error_recovery(
        "Unterminated String",
        r#"
name: "John Doe
age: 30
"#,
    );

    // Example 3: Unclosed flow sequence
    test_error_recovery(
        "Unclosed Flow Sequence",
        r#"
items: [apple, banana, cherry
config: value
"#,
    );

    // Example 4: Unclosed flow mapping
    test_error_recovery(
        "Unclosed Flow Mapping",
        r#"
config: {host: localhost, port: 8080
server: running
"#,
    );

    // Example 5: Missing colon in flow mapping
    test_error_recovery(
        "Missing Colon in Flow Mapping",
        r#"
config: {host localhost, port: 8080}
"#,
    );

    // Example 6: Complex nested structure with multiple errors
    test_error_recovery(
        "Multiple Errors",
        r#"
users:
  - name: Alice
    email alice@example.com
    settings: {theme light, notifications: true}
  - name: "Bob
    active: true
    items: [a, b, c
"#,
    );

    // Example 7: Tab character error
    test_error_recovery(
        "Tab Character",
        r#"
name: John
	age: 30
"#,
    );

    // Example 8: Recovery and continuation
    println!("8. Recovery and Continuation Test:");
    let problematic_yaml = r#"
# This YAML has multiple errors but the parser should recover
config:
  database:
    host: localhost
    port 5432
    name: "mydb
  server:
    port: 8080
    host: 127.0.0.1
    settings: {debug true, log_level: "info"}
  cache:
    type: redis
    url: "redis://localhost:6379
    timeout: 30
"#;

    match problematic_yaml.parse::<Yaml>() {
        Ok(yaml) => {
            println!("✓ Successfully parsed with error recovery!");
            if let Some(document) = yaml.document() {
                println!("Document structure recovered:");
                println!("{}", document.to_string().trim());
            }
        }
        Err(e) => {
            println!("✗ Parse failed: {}", e);
        }
    }

    // Check if we have positioned errors
    let result = Yaml::parse(problematic_yaml);
    if !result.positioned_errors().is_empty() {
        println!("\nDetailed error information:");
        for (i, error) in result.positioned_errors().iter().enumerate().take(5) {
            println!("{}. {}", i + 1, error.message);
        }
        if result.positioned_errors().len() > 5 {
            println!(
                "... and {} more errors",
                result.positioned_errors().len() - 5
            );
        }
    }

    println!();
}

fn test_error_recovery(test_name: &str, yaml_content: &str) {
    println!("{}. {}:", test_name.chars().enumerate().count(), test_name);

    let result = Yaml::parse(yaml_content);

    if result.errors().is_empty() {
        println!("✓ No errors detected (unexpected for this test)");
    } else {
        println!("✓ Errors detected with recovery:");
        for error in result.errors() {
            println!("  {}", error);
        }
    }

    // Show that parsing can continue despite errors
    let yaml_tree = result.tree();
    if let Some(doc) = yaml_tree.document() {
        println!("✓ Document partially recovered:");
        let recovered = doc.to_string();
        if !recovered.trim().is_empty() {
            println!("  Content: {}", recovered.trim().replace('\n', "\\n"));
        }
    }

    println!();
}
