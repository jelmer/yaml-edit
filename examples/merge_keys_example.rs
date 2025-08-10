use std::str::FromStr;
use yaml_edit::Yaml;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let yaml_content = r#"
# Base configuration
defaults: &defaults
  timeout: 30
  retries: 3
  enabled: true
  log_level: info

# Production configuration merges defaults
production:
  <<: *defaults
  host: prod.example.com
  port: 443
  log_level: warning  # Override default

# Development configuration also merges defaults
development:
  <<: *defaults
  host: localhost
  port: 3000
  debug: true

# Multiple merge keys example
base_db: &base_db
  driver: postgres
  pool_size: 10

base_cache: &base_cache
  provider: redis
  ttl: 3600

services:
  api:
    <<: [*base_db, *base_cache]
    name: api_service
    port: 8080
"#;

    println!("Original YAML with merge keys:");
    println!("{}", yaml_content);
    println!("\n{}\n", "=".repeat(50));

    // Parse the YAML
    let yaml = Yaml::from_str(yaml_content)?;

    println!("Parsed and preserved YAML:");
    println!("{}", yaml);
    println!("\n{}\n", "=".repeat(50));

    // The output should preserve all merge keys and references
    let output = yaml.to_string();

    // Verify merge keys are preserved
    if output.contains("<<:") && output.contains("*defaults") {
        println!("✓ Merge keys and references are properly preserved!");
    } else {
        println!("✗ Merge keys may not be fully preserved");
    }

    Ok(())
}
