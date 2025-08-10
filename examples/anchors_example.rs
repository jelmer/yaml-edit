use std::str::FromStr;
use yaml_edit::Yaml;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let yaml_content = r#"
# Example YAML with anchors and aliases
defaults: &default_settings
  timeout: 30
  retries: 3
  enabled: true

# Production config uses default settings
production:
  name: "prod"
  settings: *default_settings

# Development overrides some defaults
development:
  name: "dev"
  settings:
    <<: *default_settings
    timeout: 5
    debug: true

# Test environment also uses defaults
testing: *default_settings
"#;

    println!("Original YAML:");
    println!("{}", yaml_content);

    // Parse the YAML
    let yaml = Yaml::from_str(yaml_content)?;

    println!("\nParsed and formatted YAML:");
    println!("{}", yaml);

    // Check that we have the expected structure
    if let Some(doc) = yaml.document() {
        if let Some(mapping) = doc.as_mapping() {
            println!("\nFound mapping with keys:");
            for (key, _value) in mapping.pairs() {
                if let Some(key_scalar) = key {
                    println!("  - {}", key_scalar.value());
                }
            }
        }
    }

    Ok(())
}
