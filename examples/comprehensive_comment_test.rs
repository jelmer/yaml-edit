use std::str::FromStr;
use yaml_edit::Yaml;

fn main() {
    println!("Comprehensive comment test suite...\n");

    // Test 1: All basic comment types
    let yaml1 = r#"# Document header comment
version: 1.0  # Version comment

# Section comment
database:
  host: localhost  # Host comment
  port: 5432       # Port comment
  # Database name comment
  name: mydb

# Array section
items:
  - first   # First item comment
  - second  # Second item comment
  # Comment between items
  - third   # Third item comment

# Flow collections with comments
flow_sequence: [
    item1,  # Comment after item1
    item2,  # Comment after item2
    item3   # Comment after item3
]

flow_mapping: {
    key1: value1,  # Comment after first pair
    key2: value2,  # Comment after second pair
    key3: value3   # Comment after third pair
}

# Mixed nested structures
config:
  servers: [
    {name: web1, port: 80},   # Web server 1
    {name: web2, port: 80},   # Web server 2
    {name: db1, port: 5432}   # Database server
  ] # End servers array
  
  # Environment settings
  environment:
    debug: true    # Enable debug mode
    logging: info  # Set log level
    
# Final comment"#;

    test_yaml("Comprehensive comment test", yaml1);

    // Test 2: Edge cases and complex scenarios
    let yaml2 = r#"# Test edge cases
complex:
  # Nested mappings with comments
  level1:
    level2: 
      level3: value  # Deep nested comment
      # Comment in nested structure
      other: data
    # Back to level2
    more_data: [
      # Comment inside array
      {key: value}, # Comment after object in array
      # Another comment
      {another: thing}
    ]
  
  # Multiple flow styles
  mixed_styles:
    block_list:
      - item1  # Block style with comments
      - item2
    flow_list: [a, b, c]  # Flow style with comments
    mixed: {
      flow_key: [
        item1,  # Comment in flow sequence inside flow mapping
        item2
      ],  # Comment after flow sequence
      block_key: value  # Regular comment
    }"#;

    test_yaml("Edge cases and complex scenarios", yaml2);
}

fn test_yaml(name: &str, yaml: &str) {
    println!("=== {} ===", name);
    println!("Input:\n{}\n", yaml);

    match Yaml::from_str(yaml) {
        Ok(doc) => {
            println!("✓ Parse successful");
            let output = doc.to_string();

            // Count comments in input and output
            let input_comments = yaml.matches('#').count();
            let output_comments = output.matches('#').count();

            println!(
                "✓ Comments preserved: {}/{}",
                output_comments, input_comments
            );

            if output_comments == input_comments {
                println!("✅ ALL COMMENTS PRESERVED");
            } else {
                println!("⚠ Some comments may have been lost");
            }

            println!("Output:\n{}\n", output);

            // Test round-trip parsing
            match Yaml::from_str(&output) {
                Ok(_) => println!("✓ Round-trip parse successful"),
                Err(e) => println!("✗ Round-trip parse failed: {:?}", e),
            }
        }
        Err(e) => {
            println!("✗ Parse failed: {:?}", e);
        }
    }

    println!("{}\n", "=".repeat(80));
}
