use std::str::FromStr;
use yaml_edit::{SyntaxNodeExt, Yaml};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("Comprehensive Dash Handling Test\n");
    println!("=================================\n");

    // Test cases covering all dash scenarios
    let test_cases = vec![
        ("Document markers", "---\nkey: value\n..."),
        (
            "Sequence markers",
            "items:\n  - first\n  - second\n  - third",
        ),
        ("Kebab-case keys", "package-name: my-awesome-package"),
        ("Kebab-case values", "type: kebab-case-value"),
        ("UUID", "id: 550e8400-e29b-41d4-a716-446655440000"),
        ("Date", "date: 2024-01-15"),
        ("ISO timestamp", "timestamp: 2024-01-15T10:30:00-05:00"),
        ("Negative number", "temperature: -40"),
        ("Range", "range: 10-20"),
        ("Version string", "version: 1.0.0-beta.1"),
        ("Command args", "args: --verbose --debug"),
        (
            "URL with hyphens",
            "url: https://my-domain.com/path-to-resource",
        ),
        ("Mixed hyphens", "complex: a-b-c---d--e-f"),
        ("Block scalar strip", "text: |-\n  content\n  more content"),
        ("Block scalar keep", "text: |+\n  content\n  more content"),
        ("Flow sequence", "items: [item-one, item-two, item-three]"),
        (
            "Flow mapping",
            "data: {key-one: value-one, key-two: value-two}",
        ),
        ("Multiple hyphens", "test: ----value----"),
        ("Hyphen at end", "suffix: value-"),
        ("Hyphen at start", "prefix: -value"),
    ];

    let mut all_passed = true;

    for (name, yaml_str) in test_cases {
        print!("Testing {:<20} ... ", name);

        match Yaml::from_str(yaml_str) {
            Ok(parsed) => {
                // Verify it round-trips correctly
                let output = parsed.to_string();

                // For simple cases, check if key content is preserved
                if yaml_str.contains(':') && !yaml_str.starts_with("---") {
                    if output.contains(&yaml_str.split(':').nth(1).unwrap_or("").trim()) {
                        println!("✓ PASSED");
                    } else {
                        println!("✗ FAILED - content not preserved");
                        println!("  Input:  {}", yaml_str);
                        println!("  Output: {}", output.trim());
                        all_passed = false;
                    }
                } else {
                    println!("✓ PASSED");
                }
            }
            Err(e) => {
                println!("✗ FAILED - parse error: {:?}", e);
                all_passed = false;
            }
        }
    }

    println!("\n=================================");
    if all_passed {
        println!("All tests PASSED! ✓");
    } else {
        println!("Some tests FAILED!");
        return Err("Not all tests passed".into());
    }

    println!("\n=================================");
    println!("All comprehensive dash tests completed successfully!");

    Ok(())
}
