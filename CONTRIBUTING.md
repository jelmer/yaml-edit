# Contributing

## Prerequisites

Please read [DESIGN.md](DESIGN.md). It explains:
- The CST (Concrete Syntax Tree) architecture
- The newline ownership model
- Why we use `splice_children` instead of rebuilding nodes
- Interior mutability pattern
- Common pitfalls and how to avoid them

Understanding the design is essential - many bugs come from not following the established patterns.

## Understanding the CST Structure

An important debugging tool is visualizing the CST. Use the `debug` module:

```rust
use yaml_edit::{YamlFile, debug};
use std::str::FromStr;

let yaml = YamlFile::from_str("team:\n  - Alice\n  - Bob").unwrap();

// Print the tree structure
debug::print_tree(yaml.syntax());

// Or get it as a string
let tree = debug::tree_to_string(yaml.syntax());

// Validate structural invariants
debug::validate_tree(yaml.syntax()).expect("tree is valid");
```

Before implementing any feature or fix:
1. Create a minimal example of the YAML you're working with
2. Use `debug::print_tree()` to see the actual CST structure
3. Don't guess at the structure - look at it!

## Common Pitfalls

See [DESIGN.md - Common Pitfalls](DESIGN.md#common-pitfalls-and-solutions) for detailed explanations. Quick reminders:

### Don't rebuild nodes

```rust
// WRONG - loses formatting
let mut builder = GreenNodeBuilder::new();
builder.start_node(MAPPING_ENTRY);
// ... rebuild entire entry ...
self.0 = SyntaxNode::new_root_mut(builder.finish());
```

```rust
// RIGHT - preserves formatting
let new_value_node = build_value_node(value);
self.0.splice_children(value_index..value_index+1, vec![new_value_node.into()]);
```

### Don't forget to collect before splicing

```rust
// WRONG - borrow error
self.0.splice_children(range, new_node.children_with_tokens());

// RIGHT
let children: Vec<_> = new_node.children_with_tokens()
    .map(|c| c.into())
    .collect();
self.0.splice_children(range, children);
```

### Use right index space

Rowan uses different indexes for `children()` (only nodes) and `children_with_tokens()` (nodes + tokens). Make sure to use the right one when splicing:

```rust
// WRONG - children() and splice_children() have different indices
let idx = self.0.children().position(|c| ...)?;
self.0.splice_children(idx..idx+1, vec![]);

// RIGHT - use children_with_tokens() for indices
let idx = self.0.children_with_tokens().position(|c| ...)?;
self.0.splice_children(idx..idx+1, vec![]);
```

## Writing Tests

### Testing Formatting Preservation

The key property of this library is **lossless editing**. Always verify formatting is preserved:

```rust
#[test]
fn test_preserves_comments() {
    let yaml = YamlFile::from_str("name: Alice  # a comment").unwrap();

    // Make a change
    if let Some(doc) = yaml.document() {
        if let Some(mapping) = doc.as_mapping() {
            mapping.set("age", 30);
        }
    }

    // Comment should still be there
    let result = yaml.to_string();
    assert_eq!(result, "name: Alice  # a comment\nage: 30\n");
}
```

## Resources

- [DESIGN.md](DESIGN.md) - Architecture and patterns
- [rowan docs](https://docs.rs/rowan/) - The CST library
- [YAML 1.2 Spec](https://yaml.org/spec/1.2.2/) - Language specification
- Examples in `examples/` directory
