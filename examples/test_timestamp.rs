fn main() {
    let input = "timestamp: 2024-01-15T10:30:00-05:00";

    println!("Testing: {}", input);

    // Test parsing
    use std::str::FromStr;
    use yaml_edit::{SyntaxNodeExt, Yaml};

    let result = Yaml::from_str(input);
    match result {
        Ok(yaml) => {
            if let Some(doc) = yaml.document() {
                println!("Output:\n{}", doc.to_yaml_string());

                if let Some(value) = doc.get("timestamp") {
                    if let Some(s) = value.as_str() {
                        println!("Timestamp value: {}", s);
                    }
                }
            }
        }
        Err(e) => println!("Error: {:?}", e),
    }
}
