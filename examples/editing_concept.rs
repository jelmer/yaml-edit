// This example demonstrates the concept of lossless editing
// The editing API is conceptual - this shows what the API would look like once fully implemented

use std::str::FromStr;
use yaml_edit::Yaml;

fn main() {
    let original = r#"# Configuration file
name: old-project  # Project name  
version: 1.0.0
author: Jane Doe

# Dependencies
dependencies:
  - serde   # JSON serialization
  - tokio   # Async runtime
  - clap    # CLI parsing
"#;

    println!("=== ORIGINAL YAML ===");
    println!("{}", original);

    // Parse the YAML
    let yaml = Yaml::from_str(original).unwrap();

    println!("=== PARSED SUCCESSFULLY ===");
    println!("The YAML parses correctly and preserves all formatting!");

    // This shows the actual current state
    println!("=== REPRODUCED (CURRENT FUNCTIONALITY) ===");
    println!("{}", yaml);

    // This is what the editing API would look like once implemented:
    println!("=== WHAT EDITING WOULD LOOK LIKE ===");
    println!("// Get a mutable reference to the document");
    println!("// let mut yaml = Yaml::from_str(original).unwrap();");
    println!("// ");
    println!("// if let Some(doc) = yaml.document_mut() {{");
    println!("//     if let Some(mapping) = doc.as_mapping_mut() {{");
    println!("//         // Change values while preserving comments and formatting");
    println!("//         mapping.set(\"name\", \"new-project\");");
    println!("//         mapping.set(\"version\", \"2.0.0\");");
    println!("//         mapping.set(\"license\", \"MIT\");");
    println!("//     }}");
    println!("// }}");

    println!("\n=== EXPECTED RESULT AFTER EDITING ===");
    let expected_result = r#"# Configuration file
name: new-project  # Project name  
version: 2.0.0
author: Jane Doe
license: MIT

# Dependencies
dependencies:
  - serde   # JSON serialization
  - tokio   # Async runtime
  - clap    # CLI parsing
"#;
    println!("{}", expected_result);

    println!("=== KEY FEATURES OF LOSSLESS EDITING ===");
    println!("✓ Comments are preserved exactly as written");
    println!("✓ Whitespace and indentation maintained");
    println!("✓ Only changed values are updated");
    println!("✓ New fields added with consistent formatting");
    println!("✓ Original structure remains intact");
    println!("✓ Parse once, edit many times, always lossless");
}
