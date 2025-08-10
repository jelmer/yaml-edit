use std::str::FromStr;
use yaml_edit::Yaml;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("🔍 Testing Colon Handling in YAML");
    println!("==================================\n");

    // Test cases with colons
    let test_cases = vec![
        ("Simple URL", "website: https://example.com"),
        ("URL with path", "repo: https://github.com/user/repo"),
        ("Time format", "time: 10:30:45"),
        ("Windows path", "path: C:\\Users\\Name"),
        (
            "IPv6 address",
            "ipv6: 2001:0db8:85a3:0000:0000:8a2e:0370:7334",
        ),
    ];

    for (description, yaml_content) in test_cases {
        println!("📋 Test: {}", description);
        println!("Input: {}", yaml_content);

        match Yaml::from_str(yaml_content) {
            Ok(parsed) => {
                if let Some(doc) = parsed.document() {
                    println!("✅ Parsed successfully");
                    println!("Result: {}", doc.to_yaml_string());
                    println!("Keys: {:?}", doc.keys());

                    // Try to get the value
                    for key in doc.keys() {
                        if let Some(value) = doc.get(&key) {
                            println!("Value for '{}': '{}'", key, value.text());
                        }
                    }
                } else {
                    println!("❌ No document found");
                }
            }
            Err(e) => {
                println!("❌ Parse error: {}", e);
            }
        }
        println!();
    }

    // Test with quoted values (the YAML-compliant workaround)
    println!("📋 Test: Quoted URLs (YAML-compliant)");
    let quoted_yaml = r#"
website: "https://example.com"
repo: 'https://github.com/user/repo'
"#;
    println!("Input: {}", quoted_yaml);

    match Yaml::from_str(quoted_yaml) {
        Ok(parsed) => {
            if let Some(doc) = parsed.document() {
                println!("✅ Parsed successfully");
                println!("Result: {}", doc.to_yaml_string());
            }
        }
        Err(e) => {
            println!("❌ Parse error: {}", e);
        }
    }

    Ok(())
}
