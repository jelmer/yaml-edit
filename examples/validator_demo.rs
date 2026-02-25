//! Demonstration of the YAML validator
//!
//! This example shows how to use the strict validator to check
//! YAML documents for spec compliance.

use std::str::FromStr;
use yaml_edit::validator::Validator;
use yaml_edit::Document;

fn main() {
    println!("=== YAML Validator Demo ===\n");

    // Example 1: Valid YAML
    println!("1. Validating spec-compliant YAML:");
    let valid_yaml = r#"
name: Alice
age: 30
hobbies:
  - reading
  - coding
"#;

    let doc = Document::from_str(valid_yaml).unwrap();
    let validator = Validator::new();
    let violations = validator.validate(&doc);

    if violations.is_empty() {
        println!("Document is strictly spec-compliant!\n");
    } else {
        println!("Found {} violations:", violations.len());
        for violation in violations {
            println!("  {}", violation);
        }
        println!();
    }
}
