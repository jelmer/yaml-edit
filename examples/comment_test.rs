use std::str::FromStr;
use yaml_edit::Yaml;

fn main() {
    println!("Testing current comment behavior...");

    // Test 1: End-of-line comments (should work)
    let yaml1 = r#"key: value # end of line comment
another: item # another comment"#;
    let parsed1 = Yaml::from_str(yaml1);
    match parsed1 {
        Ok(doc) => {
            println!("End-of-line comments: PASS");
            println!("Output:\n{}\n", doc.to_string());
        }
        Err(e) => println!("End-of-line comments: FAIL - {:?}", e),
    }

    // Test 2: Mid-line comments in sequences
    let yaml2 = r#"items:
  - first # comment here
  - second # another comment
  - third"#;
    let parsed2 = Yaml::from_str(yaml2);
    match parsed2 {
        Ok(doc) => {
            let output = doc.to_string();
            println!(
                "Mid-line in sequences: {}",
                if output.contains("# comment here") {
                    "PASS"
                } else {
                    "PARTIAL"
                }
            );
            println!("Output:\n{}\n", output);
        }
        Err(e) => println!("Mid-line in sequences: FAIL - {:?}", e),
    }

    // Test 3: Comments in flow collections (likely issue)
    let yaml3 = r#"flow_seq: [item1, # comment
           item2, item3]
flow_map: {key1: val1, # comment
           key2: val2}"#;
    let parsed3 = Yaml::from_str(yaml3);
    match parsed3 {
        Ok(doc) => {
            let output = doc.to_string();
            println!(
                "Flow collection comments: {}",
                if output.contains("# comment") {
                    "PASS"
                } else {
                    "FAIL"
                }
            );
            println!("Output:\n{}\n", output);
        }
        Err(e) => println!("Flow collection comments: FAIL - {:?}", e),
    }

    // Test 4: Comments between mapping items
    let yaml4 = r#"mapping:
  key1: value1
  # Comment between items
  key2: value2
  # Another comment
  key3: value3"#;
    let parsed4 = Yaml::from_str(yaml4);
    match parsed4 {
        Ok(doc) => {
            let output = doc.to_string();
            println!(
                "Comments between mapping items: {}",
                if output.contains("# Comment between items") {
                    "PASS"
                } else {
                    "PARTIAL"
                }
            );
            println!("Output:\n{}\n", output);
        }
        Err(e) => println!("Comments between mapping items: FAIL - {:?}", e),
    }
}
