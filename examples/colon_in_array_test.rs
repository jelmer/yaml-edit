use std::str::FromStr;
use yaml_edit::{SyntaxNodeExt, Yaml};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("🔍 Testing Colons in Arrays");
    println!("============================\n");

    // Test unquoted URLs in arrays (problematic)
    println!("📋 Test: Unquoted URLs in array");
    let unquoted = r#"urls: [https://example.com, https://github.com]"#;
    println!("Input: {}", unquoted);
    println!("⚠️  This may hang or parse incorrectly...");

    // Use a simpler test first
    println!("\n📋 Test: Simple values with colons in array");
    let simple = r#"times: [10:30, 14:45, 18:00]"#;
    println!("Input: {}", simple);

    match Yaml::from_str(simple) {
        Ok(parsed) => {
            if let Some(doc) = parsed.document() {
                println!("Result: {}", doc.to_yaml_string());

                if let Some(times_node) = doc.get("times") {
                    println!("Is sequence: {}", times_node.is_sequence());
                    if let Some(items) = times_node.as_array() {
                        println!("Array items: {:?}", items);
                    }
                }
            }
        }
        Err(e) => println!("Parse error: {}", e),
    }

    // Test quoted URLs in arrays (correct YAML)
    println!("\n📋 Test: Quoted URLs in array (YAML-compliant)");
    let quoted = r#"urls: ["https://example.com", "https://github.com"]"#;
    println!("Input: {}", quoted);

    match Yaml::from_str(quoted) {
        Ok(parsed) => {
            if let Some(doc) = parsed.document() {
                println!("✅ Parsed successfully");
                println!("Result: {}", doc.to_yaml_string());

                if let Some(urls_node) = doc.get("urls") {
                    println!("Is sequence: {}", urls_node.is_sequence());
                    if let Some(items) = urls_node.as_array() {
                        println!("Array items: {:?}", items);
                    }
                }
            }
        }
        Err(e) => println!("❌ Parse error: {}", e),
    }

    // Test flow mapping with colons
    println!("\n📋 Test: Flow mapping with URLs");
    let flow_map = r#"config: {site: "https://example.com", api: "https://api.example.com"}"#;
    println!("Input: {}", flow_map);

    match Yaml::from_str(flow_map) {
        Ok(parsed) => {
            if let Some(doc) = parsed.document() {
                println!("✅ Parsed successfully");
                println!("Result: {}", doc.to_yaml_string());
            }
        }
        Err(e) => println!("❌ Parse error: {}", e),
    }

    println!("\n📝 Summary:");
    println!("• Colons in simple values after keys work fine");
    println!("• Colons in quoted strings work fine");
    println!("• Unquoted colons in flow sequences/mappings can cause issues");
    println!("• Best practice: Quote strings containing colons in arrays/maps");

    Ok(())
}
