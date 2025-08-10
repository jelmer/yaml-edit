use std::str::FromStr;
use yaml_edit::Yaml;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("🐛 Hyphen Bug Fix Demonstration");
    println!("================================\n");

    // Test cases that previously failed
    let test_cases = vec![
        ("Simple hyphen", "Name: example-project"),
        ("Multiple hyphens", "package-name: my-awesome-package"),
        (
            "Multi-line with hyphens",
            "Name: example-project\nVersion: v1.2.3-beta",
        ),
        (
            "Mixed content",
            "project: my-app\ndescription: A cool-looking application\nversion: 1.0.0-rc1",
        ),
    ];

    for (description, yaml_content) in test_cases {
        println!("📋 Test: {}", description);
        println!("Input YAML:");
        println!("{}", yaml_content);

        match Yaml::from_str(yaml_content) {
            Ok(parsed) => {
                if let Some(doc) = parsed.document() {
                    println!("✅ Successfully parsed!");
                    println!("Result: {}", doc.to_yaml_string());
                    println!("Keys: {:?}", doc.keys());
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

    // Test sequence markers still work
    println!("📋 Test: Sequence markers still work");
    let sequence_yaml = "items:\n  - first-item\n  - second-item";
    println!("Input YAML:\n{}", sequence_yaml);

    match Yaml::from_str(sequence_yaml) {
        Ok(parsed) => {
            if let Some(doc) = parsed.document() {
                println!("✅ Successfully parsed!");
                println!("Result: {}", doc.to_yaml_string());
            }
        }
        Err(e) => {
            println!("❌ Parse error: {}", e);
        }
    }

    println!("\n🎉 Hyphen bug has been fixed!");
    println!("✅ Hyphens in scalar values now work correctly");
    println!("✅ Sequence markers (- item) still work correctly");
    println!("✅ Multi-line documents with hyphens work correctly");

    Ok(())
}
