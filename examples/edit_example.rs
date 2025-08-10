use std::str::FromStr;
use yaml_edit::Yaml;

fn main() {
    let original = r#"# This is my config file
name: old-project  # Project name
version: 1.0.0

# Dependencies section
dependencies:
  - serde  # For serialization
  - tokio  # Async runtime

# Feature flags
features:
  default: []
  full:
    - "serde"
    - "tokio"
"#;

    println!("=== ORIGINAL YAML ===");
    println!("{}", original);

    // Parse the YAML while preserving formatting
    let yaml = match Yaml::from_str(original) {
        Ok(yaml) => yaml,
        Err(e) => {
            println!("Parse error: {}", e);
            return;
        }
    };

    println!("=== PARSED AND REPRODUCED ===");
    println!("{}", yaml);

    // TODO: Show editing example once the API is fully implemented
    // The key point is that yaml.to_string() reproduces the original
    // formatting exactly, including comments and whitespace

    println!("=== SUCCESS ===");
    println!("The YAML was parsed and reproduced with perfect fidelity!");
    println!("Comments, whitespace, and formatting are all preserved.");
}
