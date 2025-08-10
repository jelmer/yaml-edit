use std::str::FromStr;
use yaml_edit::Yaml;

fn main() {
    println!("Testing flow collection comment improvements...");

    // Test 1: Simple flow collections with trailing comments (should have worked before)
    let yaml1 = r#"flow_seq: [item1, item2] # trailing comment
flow_map: {key1: val1, key2: val2} # trailing comment"#;

    test_yaml("Simple trailing comments", yaml1);

    // Test 2: Comments inside flow sequences (should work now)
    let yaml2 = r#"flow_seq: [
    item1, # comment after item1
    item2, # comment after item2
    item3  # comment after item3
]"#;

    test_yaml("Comments inside flow sequence", yaml2);

    // Test 3: Comments inside flow mappings (should work now)
    let yaml3 = r#"flow_map: {
    key1: val1, # comment after first pair
    key2: val2, # comment after second pair
    key3: val3  # comment after third pair
}"#;

    test_yaml("Comments inside flow mapping", yaml3);

    // Test 4: Mixed flow and comments
    let yaml4 = r#"mixed: [
    {name: john, # inline comment
     age: 30}, # another comment
    {name: jane, age: 25} # final comment
] # end comment"#;

    test_yaml("Mixed flow collections with comments", yaml4);
}

fn test_yaml(name: &str, yaml: &str) {
    println!("\n--- {} ---", name);
    match Yaml::from_str(yaml) {
        Ok(doc) => {
            println!("✓ Successfully parsed");
            let output = doc.to_string();
            if output.contains('#') {
                println!("✓ Comments preserved in output");
            } else {
                println!("⚠ Comments lost in output");
            }
            println!("Output:\n{}", output);
        }
        Err(e) => {
            println!("✗ Failed to parse: {:?}", e);
        }
    }
}
