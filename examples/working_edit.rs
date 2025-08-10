// Test actual editing functionality

use std::str::FromStr;
use yaml_edit::Yaml;

fn main() {
    println!("=== TESTING ACTUAL EDITING IMPLEMENTATION ===\n");

    // Start with simple YAML
    let original = r#"name: old-project
version: 1.0.0
features:
  - auth
  - logging
"#;

    println!("ORIGINAL:");
    println!("{}", original);

    // Parse it
    let yaml = match Yaml::from_str(original) {
        Ok(y) => y,
        Err(e) => {
            println!("Parse error: {}", e);
            return;
        }
    };

    println!("PARSED SUCCESSFULLY!");

    // Try to access and edit
    if let Some(doc) = yaml.document() {
        if let Some(mut mapping) = doc.as_mapping() {
            println!("\nTesting editing operations...");

            // Test set operation
            println!("Setting name to 'new-project'");
            mapping.set("name", "new-project");

            println!("Setting new field 'license' to 'MIT'");
            mapping.set("license", "MIT");

            println!("Testing rename operation");
            mapping.rename_key("version", "app_version");
        }
    }

    println!("\nAFTER EDITING:");
    println!("{}", yaml);

    println!("=== EDITING TEST COMPLETE ===");
}
