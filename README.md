Lossless YAML Parser and Editor
==============================

A Rust library for parsing and editing YAML files while preserving all whitespace, comments, and formatting. Built with the [rowan] library for lossless syntax trees.

## Features

- **Lossless parsing**: Preserves all whitespace, comments, and original formatting
- **Editing support**: Modify YAML structures while maintaining formatting
- **Error recovery**: Continues parsing even with syntax errors
- **Position tracking**: Detailed error locations for debugging

## Example

```rust
use yaml_edit::Yaml;
use std::str::FromStr;

let input = r#"
# Configuration file
name: my-project
version: 1.0.0

dependencies:
  - serde
  - tokio

features:
  default: []
  full:
    - "serde"
    - "tokio"
"#;

let mut yaml = Yaml::from_str(input).unwrap();

// Access documents
if let Some(doc) = yaml.document() {
    if let Some(mapping) = doc.as_mapping() {
        // Get values while preserving structure
        if let Some(name_node) = mapping.get("name") {
            println!("Project name: {}", name_node.text());
        }
    }
}

// The original formatting is preserved
println!("{}", yaml);
```

[rowan]: https://github.com/rust-analyzer/rowan