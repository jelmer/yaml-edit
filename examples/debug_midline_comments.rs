use yaml_edit::Yaml;
use std::str::FromStr;

fn main() {
    println!("Debugging mid-line comment parsing...\n");

    // Test cases that should work according to YAML spec
    let test_cases = vec![
        ("flow_seq_comment_after_item", "array: [item1 # comment, item2]"),
        ("flow_seq_comment_on_newline", "array: [item1, # comment\n  item2]"),
        ("flow_map_comment_after_key", "obj: {key1 # comment: value1}"),
        ("flow_map_comment_after_value", "obj: {key1: value1 # comment, key2: value2}"),
        ("flow_map_comment_on_newline", "obj: {key1: value1, # comment\n  key2: value2}"),
    ];

    for (name, yaml) in test_cases {
        println!("=== {} ===", name);
        println!("Input: {}", yaml);
        
        match Yaml::from_str(yaml) {
            Ok(doc) => {
                println!("✓ Parse successful");
                let output = doc.to_string();
                println!("Output: {}", output);
                
                // Check if comments are preserved
                let input_comments = yaml.matches('#').count();
                let output_comments = output.matches('#').count();
                if input_comments == output_comments && input_comments > 0 {
                    println!("✅ Comment preserved");
                } else if input_comments > 0 {
                    println!("⚠ Comment lost ({} -> {})", input_comments, output_comments);
                }
            }
            Err(e) => {
                println!("✗ Parse failed: {:?}", e);
            }
        }
        println!();
    }
}