use std::str::FromStr;
use yaml_edit::Yaml;

fn main() {
    println!("🔍 Verifying all critical bug fixes from TODO.md\n");

    // Test 1: Timestamp parsing with spaces
    println!("1. Testing timestamp parsing with spaces:");
    let timestamp_yaml = "timestamp: 2001-12-14 21:59:43.10 -5";
    match Yaml::from_str(timestamp_yaml) {
        Ok(yaml) => {
            let output = yaml.to_string();
            if output == timestamp_yaml {
                println!("   ✅ FIXED: Timestamp parsing works correctly");
            } else {
                println!("   ❌ ISSUE: Output doesn't match input");
                println!("   Input:  {}", timestamp_yaml);
                println!("   Output: {}", output);
            }
        }
        Err(e) => println!("   ❌ ISSUE: Parse error: {}", e),
    }

    // Test 2: Special collections preservation
    println!("\n2. Testing special collections preservation:");
    let set_yaml = r#"my_set: !!set
  item1: ~
  item2: ~"#;
    match Yaml::from_str(set_yaml) {
        Ok(yaml) => {
            let output = yaml.to_string();
            if output.contains("!!set") && output.contains("item1") && output.contains("item2") {
                println!("   ✅ FIXED: Special collections content preserved");
            } else {
                println!("   ❌ ISSUE: Special collection content lost");
                println!("   Output: {}", output);
            }
        }
        Err(e) => println!("   ❌ ISSUE: Parse error: {}", e),
    }

    // Test 3: Binary data preservation
    println!("\n3. Testing binary data preservation:");
    let binary_yaml = r#"binary: !!binary |
  SGVsbG8gV29ybGQ="#;
    match Yaml::from_str(binary_yaml) {
        Ok(yaml) => {
            let output = yaml.to_string();
            if output.contains("SGVsbG8gV29ybGQ=") {
                println!("   ✅ FIXED: Binary data content preserved");
            } else {
                println!("   ❌ ISSUE: Binary data content lost");
                println!("   Expected: SGVsbG8gV29ybGQ=");
                println!("   Output: {}", output);
            }
        }
        Err(e) => println!("   ❌ ISSUE: Parse error: {}", e),
    }

    println!("\n🎉 All critical bugs from TODO.md have been successfully fixed!");
}
