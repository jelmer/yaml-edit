use std::str::FromStr;
use yaml_edit::YamlFile;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Basic anchor and alias
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

    println!("Basic anchors and aliases:");
    println!("{}\n", basic_yaml);

    let parsed = YamlFile::from_str(basic_yaml)?;
    println!("Parsed (formatting preserved):");
    println!("{}\n", parsed);

    // Anchors with different value types
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

    println!("Anchors with different value types:");
    println!("{}\n", types_yaml);

    // Multiple aliases referencing same anchor
    let multi_yaml = r#"defaults: &shared_config
  timeout: 30
  retries: 3

service_a: *shared_config
service_b: *shared_config
service_c: *shared_config"#;

    println!("Multiple aliases to same anchor:");
    println!("{}\n", multi_yaml);

    // Error handling - undefined alias
    let error_yaml = r#"valid_anchor: &defined_anchor some_value
invalid_ref: *undefined_anchor  # This will cause an error"#;

    println!("Error handling for undefined alias:");
    println!("{}\n", error_yaml);

    let parse_result = YamlFile::parse(error_yaml);
    if parse_result.has_errors() {
        println!("Parsing errors:");
        for error in parse_result.errors() {
            println!("  {}", error);
        }
    } else {
        println!("Parsed successfully:");
        println!("{}", parse_result.tree());
    }

    Ok(())
}
