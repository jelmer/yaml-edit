use std::str::FromStr;
use yaml_edit::Yaml;

fn main() {
    println!("Testing flow collection comments...");

    // Test flow collections with comments - this is the main issue
    let yaml = r#"flow_seq: [item1, item2] # this works
flow_map: {key1: val1, key2: val2} # this works too

# But this likely doesn't work:
broken_flow: [item1, # comment inside flow
              item2]
              
also_broken: {key1: val1, # comment inside
              key2: val2}"#;

    match Yaml::from_str(yaml) {
        Ok(doc) => {
            println!("Successfully parsed flow collections with comments");
            println!("Output:\n{}", doc.to_string());
        }
        Err(e) => println!("Failed to parse flow collections with comments: {:?}", e),
    }
}
