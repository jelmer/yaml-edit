Lossless YAML Parser and Editor
==============================

A Rust library for parsing and editing YAML files while preserving all whitespace, comments, and formatting. Built with the [rowan] library for lossless syntax trees.

## Features

- **Lossless parsing**: Preserves all whitespace, comments, and original formatting
- **Editing support**: Modify YAML structures while maintaining formatting
- **Error recovery**: Continues parsing even with syntax errors
- **Position tracking**: Detailed error locations for debugging

## Examples

### Lossless Editing - Modify YAML while preserving all formatting and comments

```rust
use yaml_edit::Yaml;
use std::str::FromStr;

let original = r#"# Application configuration
name: old-project
version: 1.0.0

# Features list
features:
  - auth
  - logging

# Database config
database:
  host: localhost
  port: 5432
"#;

let yaml = Yaml::from_str(original).unwrap();

if let Some(doc) = yaml.document() {
    if let Some(mut mapping) = doc.as_mapping() {
        // Update existing fields
        mapping.set("name", "awesome-project");

        // Add new fields
        mapping.set("license", "MIT");

        // Rename keys while preserving values
        mapping.rename_key("version", "app_version");

        // Remove fields
        mapping.remove("features");
    }
}

// All comments and formatting are preserved!
println!("{}", yaml);
```

### Working with YAML Tags

```rust
use yaml_edit::Yaml;
use std::str::FromStr;

let yaml_with_tags = r#"# Custom tagged values
timestamp: !!timestamp 2024-01-15T10:30:00Z
binary_data: !!binary "SGVsbG8gV29ybGQh"
custom_type: !MyType {id: 123, name: "test"}

# Schema-specific tags
port: !!int 8080
enabled: !!bool true
ratio: !!float 0.95
"#;

let yaml = Yaml::from_str(yaml_with_tags).unwrap();

if let Some(doc) = yaml.document() {
    // Tags are preserved in the output when round-tripping YAML
    // Note: Direct tag manipulation is not yet implemented
    println!("{}", yaml);  // Tags like !!timestamp are preserved
}
```

### Advanced Field Manipulation

```rust
use yaml_edit::{Yaml, YamlValue, ScalarValue};
use std::str::FromStr;

let yaml_str = r#"
services:
  web:
    image: nginx:latest
    ports:
      - 80:80

  database:
    image: postgres:13
    environment:
      POSTGRES_DB: myapp
"#;

let yaml = Yaml::from_str(yaml_str).unwrap();

if let Some(doc) = yaml.document() {
    if let Some(mut root) = doc.as_mapping() {
        // Reorder fields according to a specific order
        root.reorder_fields(&["database", "web"]);

        // Set complex values
        let monitoring = YamlValue::Mapping(vec![
            ("enabled".into(), YamlValue::from(true)),
            ("interval".into(), YamlValue::from(30)),
            ("endpoints".into(), YamlValue::Sequence(vec![
                YamlValue::from("/health"),
                YamlValue::from("/metrics"),
            ])),
        ].into_iter().collect());

        root.set_value("monitoring", monitoring);
    }
}
```

### Error Recovery and Partial Parsing

```rust
use yaml_edit::Yaml;
use std::str::FromStr;

let invalid_yaml = r#"
valid_key: valid_value

# This section has syntax errors
broken_section:
  - item1
  - item2
    nested_without_dash: oops  # Invalid nesting

# But parsing continues!
another_valid_key: another_value
"#;

match Yaml::from_str(invalid_yaml) {
    Ok(yaml) => {
        // The parser recovers from errors and continues
        if let Some(doc) = yaml.document() {
            // You can still access valid parts
            if let Some(value) = doc.get_string("valid_key") {
                println!("Found valid value: {}", value);
            }
            if let Some(value) = doc.get_string("another_valid_key") {
                println!("Found another valid value: {}", value);
            }
        }
    }
    Err(e) => {
        // Error contains position information
        println!("Parse errors: {}", e);
    }
}
```

### Multi-line String Preservation

```rust
use yaml_edit::Yaml;
use std::str::FromStr;

let yaml_with_multiline = r#"
description: |
  This is a literal block scalar.
  All line breaks are preserved.
  Including this one.

script: >
  This is a folded block scalar.
  Line breaks are folded into spaces
  unless there's an empty line.

  Like this one above.

quoted: "This is a\nmulti-line\nquoted string"
"#;

let yaml = Yaml::from_str(yaml_with_multiline).unwrap();

// All multi-line string styles are preserved exactly as written
println!("{}", yaml);
```

[rowan]: https://github.com/rust-analyzer/rowan