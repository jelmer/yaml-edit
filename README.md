Lossless YAML Parser and Editor
==============================

A Rust library for parsing and editing YAML files while preserving all whitespace, comments, and formatting. Built with the [rowan] library for lossless syntax trees.

## Features

- **Lossless parsing**: Preserves all whitespace, comments, and original formatting
- **Editing support**: Modify YAML structures while maintaining formatting
- **Error recovery**: Continues parsing even with syntax errors
- **Position tracking**: Detailed error locations for debugging

## Examples

### Basic Editing - Update values while preserving formatting

```rust
use yaml_edit::Yaml;
use std::str::FromStr;

let original = concat!(
    "# Server configuration\n",
    "host: localhost\n",
    "port: 8080\n",
    "debug: true\n",
    "timeout: 30\n",
    "\n",
    "# Database settings\n",
    "database:\n",
    "  name: dev_db\n",
    "  user: admin\n",
    "  max_connections: 10"
);

let mut yaml = Yaml::from_str(original).unwrap();

if let Some(doc) = yaml.document() {
    if let Some(mut mapping) = doc.as_mapping() {
        // Update string values
        mapping.set("host", "0.0.0.0");
        
        // Update numeric values
        mapping.set("port", 3000);
        mapping.set("timeout", 60);
        
        // Update boolean values
        mapping.set("debug", false);
        
        // Update nested values
        if let Some(mut db) = mapping.get_mapping("database") {
            db.set("name", "prod_db");
            db.set("password", "secret123");
            db.set("max_connections", 50);
        }
    }
}

// All comments and formatting are preserved!
println!("{}", yaml);
```

### Add and Remove Fields

```rust
use yaml_edit::Yaml;
use std::str::FromStr;

let original = r#"name: my-app
version: 1.0.0
author: Alice
year: 2023

features:
  - logging
  - auth"#;

let mut yaml = Yaml::from_str(original).unwrap();

if let Some(doc) = yaml.document() {
    if let Some(mut mapping) = doc.as_mapping() {
        // Add new string field
        mapping.set("license", "MIT");
        
        // Add new boolean field
        mapping.set("published", true);
        
        // Add new numeric field
        mapping.set("downloads", 1000);
        
        // Remove a field
        mapping.remove("author");
        
        // Rename a field (preserves the value)
        mapping.rename_key("version", "app_version");
        
        // Update existing numeric field
        mapping.set("year", 2024);
    }
}

println!("{}", yaml);
```

### Working with Lists

```rust
use yaml_edit::Yaml;
use std::str::FromStr;

let original = r#"team:
  - Alice
  - Bob
  - Charlie

scores:
  - 95
  - 87
  - 92

config:
  enabled: true
  retries: 3
  servers:
    - host1
    - host2
"#;

let mut yaml = Yaml::from_str(original).unwrap();

if let Some(doc) = yaml.document() {
    if let Some(mut mapping) = doc.as_mapping() {
        // Modify a string list
        if let Some(mut team) = mapping.get_sequence("team") {
            team.push("Diana");
            team.set_item(1, "Robert");  // Change Bob to Robert
        }
        
        // Modify a numeric list
        if let Some(mut scores) = mapping.get_sequence("scores") {
            scores.push(98);
            scores.set_item(0, 100);  // Update first score
        }
        
        // Work with nested structures
        if let Some(mut config) = mapping.get_mapping("config") {
            // Update values
            config.set("enabled", false);
            config.set("retries", 5);
            
            // Modify nested list
            if let Some(mut servers) = config.get_sequence("servers") {
                servers.push("host3");
                servers.set_item(0, "primary-host");
            }
        }
    }
}

println!("{}", yaml);
```

### Update Package Configuration

```rust,no_run
use yaml_edit::Document;

// Load a package.yaml file
let mut doc = Document::load_from_file("package.yaml").unwrap();

if let Some(mut root) = doc.as_mapping() {
    // Update version
    root.set("version", "2.0.0");
    
    // Add or update dependencies
    if let Some(mut deps) = root.get_mapping("dependencies") {
        deps.set("serde", "1.0");
        deps.set("tokio", "1.35");
        deps.remove("deprecated-lib");
    }
}

// Save back to file preserving all formatting
doc.save_to_file("package.yaml").unwrap();
```

### Format-Preserving Edits

```rust
use yaml_edit::Yaml;
use std::str::FromStr;

let original = concat!(
    "# This is my config file\n",
    "# It has very specific formatting\n",
    "\n",
    "app:    my-application    # Extra spaces preserved\n",
    "version:  \"1.0.0\"         # Aligned with spaces\n",
    "\n",
    "settings:\n",
    "  # These settings are important\n",
    "  timeout:   30    # seconds\n",
    "  retries:   3     # attempts"
);

let mut yaml = Yaml::from_str(original).unwrap();

if let Some(doc) = yaml.document() {
    if let Some(mut root) = doc.as_mapping() {
        // Updates preserve the original spacing and comments
        root.set("version", "2.0.0");
        
        if let Some(mut settings) = root.get_mapping("settings") {
            settings.set("timeout", 60);
        }
    }
}

// Original formatting and all comments are preserved!
println!("{}", yaml);
```

[rowan]: https://github.com/rust-analyzer/rowan