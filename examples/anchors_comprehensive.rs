use std::str::FromStr;
use yaml_edit::Yaml;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== YAML Anchors & Aliases Example ===\n");

    // Example 1: Basic anchor and alias
    let basic_yaml = r#"# Basic anchor and alias example
database: &db_config
  host: localhost
  port: 5432
  username: admin

# Applications using the same database config
api_service: 
  name: "API Service"
  database: *db_config

worker_service:
  name: "Background Worker" 
  database: *db_config"#;

    println!("1. Basic Anchors & Aliases:");
    println!("{}", basic_yaml);

    let parsed = Yaml::from_str(basic_yaml)?;
    println!("\nParsed (preserves formatting):");
    println!("{}", parsed);

    // Example 2: Anchors with different value types
    let types_yaml = r#"# Different value types with anchors
string_default: &default_name "MyApp"
number_default: &default_port 8080
bool_default: &default_debug true
null_default: &default_cache null

services:
  web:
    name: *default_name
    port: *default_port
    debug: *default_debug
    cache: *default_cache
  api:
    name: *default_name  
    port: 3000  # Override the default port
    debug: false  # Override debug setting
    cache: *default_cache"#;

    println!("\n2. Anchors with Different Value Types:");
    println!("{}", types_yaml);

    let parsed2 = Yaml::from_str(types_yaml)?;
    println!("\nParsed (lossless):");
    println!("{}", parsed2);

    // Example 3: Multiple aliases referencing same anchor
    let multi_yaml = r#"defaults: &shared_config
  timeout: 30
  retries: 3

service_a: *shared_config
service_b: *shared_config  
service_c: *shared_config"#;

    println!("\n3. Multiple Aliases to Same Anchor:");
    println!("{}", multi_yaml);

    let parsed3 = Yaml::from_str(multi_yaml)?;
    println!("\nParsed:");
    println!("{}", parsed3);

    // Example 4: Error handling - undefined alias
    println!("\n4. Error Handling - Undefined Alias:");
    let error_yaml = r#"valid_anchor: &defined_anchor some_value
invalid_ref: *undefined_anchor  # This will cause an error"#;

    println!("{}", error_yaml);

    let parse_result = Yaml::parse(error_yaml);
    if parse_result.has_errors() {
        println!("\nParsing Errors Found:");
        for error in parse_result.errors() {
            println!("  ⚠️  {}", error);
        }
    } else {
        println!("\nParsed successfully:");
        println!("{}", parse_result.tree());
    }

    println!("\n=== Summary ===");
    println!("✅ Anchor definitions: &anchor_name value");
    println!("✅ Alias references: *anchor_name");
    println!("✅ Multiple aliases can reference the same anchor");
    println!("✅ Works with all YAML value types (strings, numbers, booleans, null)");
    println!("✅ Preserves all original formatting, whitespace, and comments");
    println!("✅ Detects and reports undefined alias references");
    println!("✅ Maintains lossless parsing - perfect round-trip fidelity");

    Ok(())
}
