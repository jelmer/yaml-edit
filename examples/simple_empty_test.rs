use std::str::FromStr;
use yaml_edit::Yaml;

fn main() {
    // Test just the problematic case
    let yaml = r#"empty:
another: value"#;

    println!("Testing: {}", yaml);

    match Yaml::from_str(yaml) {
        Ok(parsed) => {
            println!("✅ Success");
            println!("Output: {}", parsed.to_string());

            if let Some(doc) = parsed.document() {
                if let Some(mapping) = doc.as_mapping() {
                    println!("empty exists: {}", mapping.get("empty").is_some());
                    println!("another exists: {}", mapping.get("another").is_some());
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
