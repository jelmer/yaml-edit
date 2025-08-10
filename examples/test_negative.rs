fn main() {
    let test_cases = vec!["temperature: -40", "value: -1", "test: -value"];

    for input in test_cases {
        println!("Testing: {}", input);

        use std::str::FromStr;
        use yaml_edit::{SyntaxNodeExt, Yaml};

        match Yaml::from_str(input) {
            Ok(yaml) => {
                if let Some(doc) = yaml.document() {
                    println!("  Output: {}", doc.to_yaml_string());
                    for key in doc.keys() {
                        if let Some(value) = doc.get(&key) {
                            if let Some(s) = value.as_str() {
                                println!("  {} = '{}'", key, s);
                            }
                        }
                    }
                }
            }
            Err(e) => println!("  Error: {:?}", e),
        }
        println!();
    }
}
