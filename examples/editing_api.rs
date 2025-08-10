// Example of the clean editing API design (methods to be implemented)

use std::str::FromStr;
use yaml_edit::Yaml;

fn main() {
    let original = r#"# Configuration
name: my-app
version: 1.0.0

database:
  host: localhost
  port: 5432

features:
  - auth
  - logging
"#;

    println!("=== ORIGINAL ===");
    println!("{}", original);

    let yaml = Yaml::from_str(original).unwrap();

    // Actually execute editing operations that are currently implemented
    if let Some(mut doc) = yaml.document() {
        println!("\n=== EXECUTING AVAILABLE EDITING OPERATIONS ===");
        
        // Simple value changes
        println!("Setting name to 'new-app'...");
        doc.set_string("name", "new-app");
        
        println!("Setting version to '2.0.0'...");
        doc.set_string("version", "2.0.0");
        
        // Test getting values
        println!("Current name: {:?}", doc.get_string("name"));
        println!("Current version: {:?}", doc.get_string("version"));
        
        println!("\n=== CURRENT RESULT ===");
        println!("{}", doc.to_yaml_string());
        
        // Show what's not yet implemented
        println!("\n=== OPERATIONS TO BE IMPLEMENTED ===");
        println!("// Key renaming: doc.rename_key(\"name\", \"app_name\")");
        println!("// Nested path editing: doc.set_path(\"database.host\", \"prod.db.com\")");
        println!("// Advanced list operations on sequences");
        println!("// Recursive editing of nested structures");
    }

    println!("\n=== EXPECTED RESULT ===");
    let expected = r#"# Configuration
app_name: new-app
version: 2.0.0

database:
  host: prod.db.com
  ssl: true
  timeout: 30s

features:
  - authentication
  - caching  
  - logging
  - metrics
"#;
    println!("{}", expected);

    println!("=== KEY BENEFITS ===");
    println!("✓ No Mut suffixes - just &mut references");
    println!("✓ Recursive editing of nested structures");
    println!("✓ Path-based editing (dot notation)");
    println!("✓ Key renaming preserves formatting");
    println!("✓ List operations with indexing");
    println!("✓ Comments and whitespace preserved");
}
