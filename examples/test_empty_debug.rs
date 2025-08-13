use std::str::FromStr;
use yaml_edit::Yaml;

fn main() {
    let yaml = r#"
empty_string: ""
empty_after_colon:
another_key: value
"#;

    println!("Testing empty values:");
    println!("{}", yaml);

    match Yaml::from_str(yaml) {
        Ok(parsed) => {
            println!("✅ Parsed successfully");
            println!("Output: {}", parsed.to_string());

            if let Some(doc) = parsed.document() {
                if let Some(mapping) = doc.as_mapping() {
                    println!("Keys found:");
                    for (key, _) in mapping.pairs() {
                        if let Some(key_scalar) = key {
                            println!("  - {:?}", key_scalar.to_string());
                        }
                    }

                    println!(
                        "empty_string exists: {}",
                        mapping.get("empty_string").is_some()
                    );
                    println!(
                        "empty_after_colon exists: {}",
                        mapping.get("empty_after_colon").is_some()
                    );
                    println!(
                        "another_key exists: {}",
                        mapping.get("another_key").is_some()
                    );
                } else {
                    println!("Not a mapping");
                }
            } else {
                println!("No document");
            }
        }
        Err(e) => {
            println!("❌ Parse error: {}", e);
        }
    }
}
