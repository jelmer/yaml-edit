# yaml-edit

A Rust library for parsing and editing YAML files while preserving formatting, comments, and whitespace. Built with [rowan](https://github.com/rust-analyzer/rowan) for lossless syntax trees.

## Features

- Lossless parsing - preserves all whitespace, comments, and original formatting
- In-place editing - modify YAML structures while maintaining formatting
- Error recovery - continues parsing even with syntax errors
- Position tracking - detailed error locations for debugging

## Quick Start

```rust
use yaml_edit::Document;
use std::str::FromStr;

# fn main() -> Result<(), Box<dyn std::error::Error>> {
let doc = Document::from_str("name: old-project\nversion: 1.0.0")?;

if let Some(mapping) = doc.as_mapping() {
    mapping.set("name", "new-project");
    mapping.set("version", "2.0.0");
}

println!("{}", doc);  // Formatting preserved
# Ok(())
# }
```

## How It Works

This library uses a **persistent syntax tree** built on rowan. Understanding this model helps you use the library effectively:

### Lightweight Wrappers

Types like `Mapping`, `Sequence`, and `Document` are lightweight wrappers around syntax tree nodes:

```rust
# use yaml_edit::Document;
# use std::str::FromStr;
# let doc = Document::from_str("key: value").unwrap();
let mapping = doc.as_mapping();  // Just a view into the tree
// mapping is cheap to clone, it's just a reference
```

### In-Place Mutations

Changes are applied directly to the underlying syntax tree using rowan's `splice_children` operation:

```rust
# use yaml_edit::Document;
# use std::str::FromStr;
# let doc = Document::from_str("key: old").unwrap();
let mapping = doc.as_mapping().unwrap();
mapping.set("key", "value");
// The change is visible through `doc` immediately
// No need to "put mapping back into doc"
```

### Shared Tree Structure

Multiple wrappers can reference the same underlying tree. When one is mutated, all see the change:

```rust
# use yaml_edit::Document;
# use std::str::FromStr;
# let doc = Document::from_str("key: value").unwrap();
let mapping1 = doc.as_mapping().unwrap();
let mapping2 = doc.as_mapping().unwrap();

mapping2.set("new_key", "new_value");
// mapping1 also sees the change because they reference the same tree
```

This design enables ergonomic APIs without explicit ownership transfers, efficient mutations without copying, and preserved formatting because edits modify nodes in-place.

## Entry Points

### `Document` - Single-document YAML (most common)

```rust
use yaml_edit::Document;
use std::str::FromStr;

# fn main() -> Result<(), Box<dyn std::error::Error>> {
# let dir = std::env::temp_dir().join("yaml_edit_doctest");
# std::fs::create_dir_all(&dir)?;
# let config_path = dir.join("config.yaml");
# std::fs::write(&config_path, "key: value")?;
let doc = Document::from_file(&config_path)?;
// Or from a string
let doc = Document::from_str("key: value")?;
# std::fs::remove_dir_all(&dir).ok();
# Ok(())
# }
```

### `YamlFile` - Multi-document YAML

```rust
use yaml_edit::YamlFile;
use std::str::FromStr;

# fn main() -> Result<(), Box<dyn std::error::Error>> {
let yaml = YamlFile::from_str("---\ndoc1: value\n---\ndoc2: value")?;

for doc in yaml.documents() {
    // Process each document
}
# Ok(())
# }
```

### `Mapping` / `Sequence` - Working with collections

```rust
# use yaml_edit::Document;
# use std::str::FromStr;
# fn main() -> Result<(), Box<dyn std::error::Error>> {
let doc = Document::from_str("key: value\nlist:\n  - item1\n  - item2")?;

if let Some(mapping) = doc.as_mapping() {
    mapping.set("new_key", "new_value");

    if let Some(list) = mapping.get_sequence("list") {
        list.push("item3");
    }
}
# Ok(())
# }
```

## Common Operations

### Editing mappings

```rust
# use yaml_edit::Document;
# use std::str::FromStr;
# fn main() -> Result<(), Box<dyn std::error::Error>> {
let yaml = r#"
name: my-app
version: 1.0.0
author: Alice
"#;

let doc = Document::from_str(yaml)?;

if let Some(root) = doc.as_mapping() {
    root.set("version", "2.0.0");
    root.set("license", "MIT");
    root.remove("author");
    root.rename_key("name", "project_name");
}
# Ok(())
# }
```

### Working with sequences

```rust
# use yaml_edit::Document;
# use std::str::FromStr;
# fn main() -> Result<(), Box<dyn std::error::Error>> {
let yaml = "items:\n  - one\n  - two\n";
let doc = Document::from_str(yaml)?;

if let Some(root) = doc.as_mapping() {
    if let Some(items) = root.get_sequence("items") {
        items.push("three");
        items.set(0, "first");  // Update by index
    }
}
# Ok(())
# }
```

### Nested modifications

```rust
# use yaml_edit::Document;
# use std::str::FromStr;
# fn main() -> Result<(), Box<dyn std::error::Error>> {
let yaml = r#"
services:
  web:
    image: nginx:latest
    port: 8080
"#;

let doc = Document::from_str(yaml)?;

if let Some(root) = doc.as_mapping() {
    if let Some(services) = root.get_mapping("services") {
        if let Some(web) = services.get_mapping("web") {
            web.set("image", "nginx:alpine");
            web.set("port", 80);
        }
    }
}
# Ok(())
# }
```

### Path-based access

```rust
# use yaml_edit::Document;
# use std::str::FromStr;
use yaml_edit::path::YamlPath;

# fn main() -> Result<(), Box<dyn std::error::Error>> {
# let doc = Document::from_str("server:\n  host: localhost\nservers:\n  - host: a")?;
// Get nested values
let host = doc.get_path("server.host");

// Set nested values (creates intermediate mappings)
doc.set_path("database.credentials.username", "admin");

// Array indexing
doc.get_path("servers[0].host");
# Ok(())
# }
```

### Visitor pattern

For traversing and analyzing documents:

```rust
use yaml_edit::{Document, visitor::{YamlVisitor, YamlAccept, ScalarCollector}};
use std::str::FromStr;

# fn main() -> Result<(), Box<dyn std::error::Error>> {
let doc = Document::from_str("name: my-app\nversion: 1.0.0")?;
let mut collector = ScalarCollector::new();
doc.accept(&mut collector);

// collector.scalars contains all scalar values
# Ok(())
# }
```

## Error Handling

```rust
use yaml_edit::{Document, YamlError};
use std::str::FromStr;

fn update_config(yaml: &str, new_version: &str) -> Result<String, YamlError> {
    let doc = Document::from_str(yaml)?;

    let root = doc.as_mapping()
        .ok_or_else(|| YamlError::InvalidOperation {
            operation: "get root mapping".to_string(),
            reason: "Document root is not a mapping".to_string(),
        })?;

    root.set("version", new_version);
    Ok(doc.to_string())
}

# fn main() -> Result<(), Box<dyn std::error::Error>> {
# let result = update_config("version: 1.0.0", "2.0.0")?;
# assert!(result.contains("2.0.0"));
# Ok(())
# }
```

## Testing

```bash
# Run all tests
cargo test

# Run with all features
cargo test --all-features

# Run YAML Test Suite (requires submodule)
[git](git) submodule update --init
cargo test --test yaml_test_suite
```

## More Examples

See the `examples/` directory for more detailed usage.

## License

See LICENSE file for details.
