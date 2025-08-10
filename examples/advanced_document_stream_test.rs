//! Advanced document stream features test
//!
//! This example demonstrates edge cases and advanced features of YAML document streams:
//! - Documents with only end markers
//! - Multiple empty documents
//! - Mixed directive placement
//! - Complex multi-document streams with all YAML features
//! - Documents with comments
//! - Streams without explicit end markers

use std::str::FromStr;
use yaml_edit::Yaml;

fn main() {
    println!("=== Advanced Document Stream Features Test ===\n");

    // Test 1: Document with only end marker (no start)
    let yaml1 = r#"key: value
...
"#;

    println!("Test 1: Document with only end marker");
    test_yaml(yaml1, "single document with end marker only");

    // Test 2: Multiple empty documents
    let yaml2 = r#"---
---
---
...
"#;

    println!("Test 2: Multiple empty documents");
    test_yaml(yaml2, "empty documents stream");

    // Test 3: Document with directives but no start marker
    let yaml3 = r#"%YAML 1.2
key: value
...
"#;

    println!("Test 3: Document with directive but no start marker");
    test_yaml(yaml3, "directive without document start");

    // Test 4: Mixed directive placement
    let yaml4 = r#"%YAML 1.2
---
first: doc
%TAG ! tag:example.com,2000:app/
---
second: doc
...
%YAML 1.2
third: doc
"#;

    println!("Test 4: Mixed directive placement");
    test_yaml(yaml4, "mixed directive placement");

    // Test 5: Complex multi-document with all features
    let yaml5 = r#"%YAML 1.2
%TAG ! tag:example.com,2000:app/
---
template: &anchor
  key: !custom value
instance:
  <<: *anchor
  extra: data
...
%YAML 1.2
---
- item1
- item2: nested
...
---
literal: |
  Block content
  Multiple lines
folded: >
  Folded content
  on multiple lines
...
"#;

    println!("Test 5: Complex multi-document stream");
    test_yaml(yaml5, "complex document stream");

    // Test 6: Stream with comments
    let yaml6 = r#"# Document stream comment
---
# First document
first: value
...
# Between documents
---
# Second document
second: value
# End comment
...
"#;

    println!("Test 6: Stream with comments");
    test_yaml(yaml6, "document stream with comments");

    // Test 7: Just document separators
    let yaml7 = r#"---
...
---
...
"#;

    println!("Test 7: Just document separators");
    test_yaml(yaml7, "just document separators");

    // Test 8: End of stream without explicit end marker
    let yaml8 = r#"---
first: doc
---
second: doc"#;

    println!("Test 8: Stream without final end marker");
    test_yaml(yaml8, "stream without final end marker");
}

fn test_yaml(yaml: &str, description: &str) {
    match Yaml::from_str(yaml) {
        Ok(parsed) => {
            println!("  ✓ Parsed successfully: {}", description);
            println!("  Documents count: {}", parsed.documents().count());
            let output = parsed.to_string();
            let round_trip = output == yaml;
            println!("  Round-trip preserved: {}", round_trip);

            if !round_trip {
                println!("  Original:\n{}", yaml.replace('\n', "\\n\n"));
                println!("  Output:\n{}", output.replace('\n', "\\n\n"));
            }
        }
        Err(e) => {
            println!("  ✗ Parse error: {}", e);
        }
    }
    println!();
}
