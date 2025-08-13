use std::str::FromStr;
use yaml_edit::Yaml;

fn main() {
    println!("🔍 Testing regex support in yaml-edit\n");

    // Test 1: Parse YAML with regex tag
    let yaml_with_regex = r#"
patterns:
  email: !!regex '^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$'
  phone: !!regex '^\+?1?\d{9,15}$'
  ssn: !!regex '^\d{3}-\d{2}-\d{4}$'
"#;

    println!("Testing: YAML with regex tags");
    println!("Input:\n{}", yaml_with_regex);

    match Yaml::from_str(yaml_with_regex) {
        Ok(yaml) => {
            println!("✅ Parse successful");
            let output = yaml.to_string();
            println!("Output:\n{}", output);

            // Check that regex tags are preserved
            assert!(output.contains("!!regex"));
            println!("✅ Regex tags preserved");
        }
        Err(e) => {
            println!("❌ Parse failed: {:?}", e);
            std::process::exit(1);
        }
    }

    println!("\n{}\n", "=".repeat(50));

    // Test 2: Create regex with ScalarValue API
    use yaml_edit::ScalarValue;

    println!("Testing: Creating regex scalars programmatically");

    let regex_pattern = r"^\d{4}-\d{2}-\d{2}$";
    let regex_scalar = ScalarValue::regex(regex_pattern);

    println!("Pattern: {}", regex_pattern);
    println!("Is regex: {}", regex_scalar.is_regex());
    println!("YAML output: {}", regex_scalar.to_yaml_string());

    assert!(regex_scalar.is_regex());
    assert_eq!(regex_scalar.value(), regex_pattern);
    assert!(regex_scalar.to_yaml_string().contains("!!regex"));

    println!("✅ Programmatic regex creation works");

    println!("\n{}\n", "=".repeat(50));

    // Test 3: Complex regex patterns
    let complex_yaml = r#"
validation:
  # IPv4 address
  ipv4: !!regex '^((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$'
  
  # URL validation
  url: !!regex '^https?://(?:[-\w.])+(?:\:[0-9]+)?(?:/(?:[\w/_.])*(?:\?(?:[\w&=%.])*)?(?:\#(?:[\w.])*)?)?$'
  
  # UUID validation
  uuid: !!regex '^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[1-5][0-9a-fA-F]{3}-[89abAB][0-9a-fA-F]{3}-[0-9a-fA-F]{12}$'
"#;

    println!("Testing: Complex regex patterns");

    match Yaml::from_str(complex_yaml) {
        Ok(yaml) => {
            let output = yaml.to_string();

            // Check that all regex patterns are preserved
            let regex_count_input = complex_yaml.matches("!!regex").count();
            let regex_count_output = output.matches("!!regex").count();

            println!("Regex patterns in input: {}", regex_count_input);
            println!("Regex patterns in output: {}", regex_count_output);

            if regex_count_input == regex_count_output {
                println!("✅ All regex patterns preserved");
            } else {
                println!("❌ Some regex patterns lost");
                std::process::exit(1);
            }

            println!("✅ Complex regex patterns work correctly");
        }
        Err(e) => {
            println!("❌ Complex regex parsing failed: {:?}", e);
            std::process::exit(1);
        }
    }

    println!("\n🎉 All regex tests passed!");
}
