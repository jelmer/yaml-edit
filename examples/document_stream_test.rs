//! Example demonstrating document stream features in yaml-edit
//!
//! This example shows how the library handles:
//! - Multi-document streams with start (`---`) and end (`...`) markers
//! - Document-level directives (YAML version, TAG directives)
//! - Empty documents in streams
//! - Proper round-trip preservation

use std::str::FromStr;
use yaml_edit::Yaml;

fn main() {
    // Test 1: Simple multi-document stream with end markers
    let yaml1 = r#"---
doc1: first
---
doc2: second
...
"#;

    println!("=== Test 1: Multi-document with end marker ===");
    match Yaml::from_str(yaml1) {
        Ok(parsed) => {
            println!("✓ Parsed successfully");
            println!("Documents count: {}", parsed.documents().count());
            let output = parsed.to_string();
            println!("Round-trip preserved: {}", output == yaml1);
            println!("Output:\n{}", output);
        }
        Err(e) => println!("✗ Parse error: {}", e),
    }

    // Test 2: Document with explicit markers (single document)
    let yaml2 = r#"---
key: value
...
"#;

    println!("\n=== Test 2: Single document with explicit markers ===");
    match Yaml::from_str(yaml2) {
        Ok(parsed) => {
            println!("✓ Parsed successfully");
            println!("Documents count: {}", parsed.documents().count());
            let output = parsed.to_string();
            println!("Round-trip preserved: {}", output == yaml2);
            println!("Output:\n{}", output);
        }
        Err(e) => println!("✗ Parse error: {}", e),
    }

    // Test 3: Multiple documents with different end patterns
    let yaml3 = r#"---
first: doc
...
---
second: doc
---
third: doc
...
"#;

    println!("\n=== Test 3: Mixed end markers ===");
    match Yaml::from_str(yaml3) {
        Ok(parsed) => {
            println!("✓ Parsed successfully");
            println!("Documents count: {}", parsed.documents().count());
            let output = parsed.to_string();
            println!("Round-trip preserved: {}", output == yaml3);
            println!("Output:\n{}", output);
        }
        Err(e) => println!("✗ Parse error: {}", e),
    }

    // Test 4: Empty documents
    let yaml4 = r#"---
---
key: value
---
...
"#;

    println!("\n=== Test 4: Empty documents ===");
    match Yaml::from_str(yaml4) {
        Ok(parsed) => {
            println!("✓ Parsed successfully");
            println!("Documents count: {}", parsed.documents().count());
            let output = parsed.to_string();
            println!("Round-trip preserved: {}", output == yaml4);
            println!("Output:\n{}", output);
        }
        Err(e) => println!("✗ Parse error: {}", e),
    }

    // Test 5: Document-level directives
    let yaml5 = r#"%YAML 1.2
%TAG ! tag:example.com,2000:app/
---
first: doc
...
%YAML 1.2
---
second: doc
...
"#;

    println!("\n=== Test 5: Document-level directives ===");
    match Yaml::from_str(yaml5) {
        Ok(parsed) => {
            println!("✓ Parsed successfully");
            println!("Documents count: {}", parsed.documents().count());
            let output = parsed.to_string();
            println!("Round-trip preserved: {}", output == yaml5);
            println!("Output:\n{}", output);
        }
        Err(e) => println!("✗ Parse error: {}", e),
    }
}
