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

    let mut yaml = Yaml::from_str(original).unwrap();

    // This shows the clean API design (no Mut suffixes!)
    if let Some(doc) = yaml.document() {
        if let Some(mapping) = doc.as_mapping() {
            println!("\n=== CLEAN EDITING API (TO BE IMPLEMENTED) ===");
            println!("// Simple value changes");
            println!("mapping.set(\"name\", \"new-app\");");
            println!("mapping.set(\"version\", \"2.0.0\");");

            println!("\n// Key renaming");
            println!("mapping.rename(\"name\", \"app_name\");");

            println!("\n// Nested path editing");
            println!("mapping.set_path(\"database.host\", \"prod.db.com\");");
            println!("mapping.set_path(\"database.timeout\", \"30s\");");

            println!("\n// List operations");
            println!("if let Some(features) = mapping.get_sequence(\"features\") {{");
            println!("    features.push(\"metrics\");");
            println!("    features.insert(1, \"caching\");");
            println!("    features.set(0, \"authentication\");");
            println!("}}");

            println!("\n// Recursive operations");
            println!("if let Some(db) = mapping.get_mapping(\"database\") {{");
            println!("    db.set(\"ssl\", \"true\");");
            println!("    db.remove(\"port\");");
            println!("}}");
        }
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
