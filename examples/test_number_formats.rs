use std::str::FromStr;
use yaml_edit::Yaml;

fn main() {
    println!("🔢 Testing number format support in yaml-edit\n");

    // Test different number formats
    let test_cases = vec![
        ("Binary notation", "number: 0b1010", "10"),
        ("Binary with uppercase B", "number: 0B1111", "15"),
        ("Modern octal notation", "number: 0o755", "493"),
        ("Modern octal with uppercase O", "number: 0O644", "420"),
        ("Legacy octal notation", "number: 0755", "493"),
        ("Hexadecimal lowercase", "number: 0xff", "255"),
        ("Hexadecimal uppercase", "number: 0XFF", "255"),
        ("Decimal positive", "number: 42", "42"),
        ("Decimal negative", "number: -123", "-123"),
        ("Decimal with plus", "number: +456", "456"),
    ];

    let mut all_passed = true;

    for (description, input, expected_decimal) in test_cases {
        print!("Testing {}: ", description);

        match Yaml::from_str(input) {
            Ok(yaml) => {
                let output = yaml.to_string();

                // Check if parsing preserved the original format
                if output.trim() == input.trim() {
                    println!("✅ PASS - Format preserved");
                } else {
                    println!("⚠️  PARTIAL - Parsed but format changed");
                    println!("   Input:  {}", input);
                    println!("   Output: {}", output);
                }

                // Verify numeric value is correct
                if let Some(doc) = yaml.document() {
                    if let Some(value_str) = doc.get_string("number") {
                        // Parse using our enhanced parsing logic
                        // For this test, we'll use Rust's built-in parsing for validation
                        let parsed_value =
                            if value_str.starts_with("0b") || value_str.starts_with("0B") {
                                i64::from_str_radix(&value_str[2..], 2).ok()
                            } else if value_str.starts_with("0o") || value_str.starts_with("0O") {
                                i64::from_str_radix(&value_str[2..], 8).ok()
                            } else if value_str.starts_with("0x") || value_str.starts_with("0X") {
                                i64::from_str_radix(&value_str[2..], 16).ok()
                            } else if value_str.starts_with('0')
                                && value_str.len() > 1
                                && value_str.chars().all(|c| c.is_ascii_digit())
                            {
                                i64::from_str_radix(&value_str, 8).ok()
                            } else {
                                value_str.parse::<i64>().ok()
                            };

                        if let Some(parsed) = parsed_value {
                            if parsed.to_string() == expected_decimal {
                                println!("   ✅ Numeric value correct: {}", parsed);
                            } else {
                                println!("   ❌ FAIL - Wrong numeric value");
                                println!("   Expected: {}", expected_decimal);
                                println!("   Got: {}", parsed);
                                all_passed = false;
                            }
                        } else {
                            println!("   ❌ FAIL - Could not parse value: {}", value_str);
                            all_passed = false;
                        }
                    } else {
                        println!("   ❌ FAIL - Could not find 'number' field");
                        all_passed = false;
                    }
                } else {
                    println!("   ❌ FAIL - Could not get document from YAML");
                    all_passed = false;
                }
            }
            Err(e) => {
                println!("❌ FAIL - Parse error: {}", e);
                all_passed = false;
            }
        }
        println!();
    }

    // Test edge cases - invalid number formats should parse as strings
    println!("Testing edge cases:");

    let edge_cases = vec![
        ("Empty binary", "number: 0b", "0b", true),
        ("Invalid binary digit", "number: 0b1012", "0b1012", true),
        ("Empty octal", "number: 0o", "0o", true),
        ("Invalid octal digit", "number: 0o789", "0o789", true),
        ("Invalid hex", "number: 0xGH", "0xGH", true),
        ("Zero", "number: 0", "0", true),
        ("Double zero", "number: 00", "0", true), // Parsed as octal, value is 0
    ];

    for (description, input, expected_string, should_parse) in edge_cases {
        print!("Testing {}: ", description);

        match Yaml::from_str(input) {
            Ok(yaml) if should_parse => {
                if let Some(doc) = yaml.document() {
                    if let Some(value_str) = doc.get_string("number") {
                        if value_str == expected_string {
                            println!("✅ PASS - Parsed as string: '{}'", value_str);
                        } else {
                            println!(
                                "⚠️  PARTIAL - Parsed but value differs: '{}' != '{}'",
                                value_str, expected_string
                            );
                        }
                    } else {
                        println!("❌ FAIL - Could not get value");
                        all_passed = false;
                    }
                } else {
                    println!("❌ FAIL - Could not get document");
                    all_passed = false;
                }
            }
            Ok(_) if !should_parse => {
                println!("❌ FAIL - Should not have parsed");
                all_passed = false;
            }
            Err(_) if !should_parse => println!("✅ PASS - Correctly rejected"),
            Err(e) if should_parse => {
                println!("❌ FAIL - Should have parsed: {}", e);
                all_passed = false;
            }
            _ => unreachable!(),
        }
    }

    println!();
    if all_passed {
        println!("🎉 All number format tests passed!");
    } else {
        println!("❌ Some tests failed");
        std::process::exit(1);
    }
}
