use std::collections::BTreeMap;
use yaml_edit::{
    CustomTagError, CustomTagHandler, CustomTagRegistry, EnvVarHandler, JsonHandler,
    TimestampHandler, YamlValue,
};

fn main() {
    println!("=== Custom YAML Tags Example ===\n");

    // Create a new tag registry
    let mut registry = CustomTagRegistry::new();

    // Register built-in handlers
    registry.register("!json", JsonHandler).unwrap();
    registry.register("!env", EnvVarHandler).unwrap();
    registry
        .register("!timestamp", TimestampHandler::new("YYYY-MM-DD"))
        .unwrap();

    // Register a custom handler
    registry.register("!uppercased", UppercaseHandler).unwrap();

    println!("Registered tags: {:?}\n", registry.registered_tags());

    // Test JSON handler
    println!("=== JSON Handler ===");
    let json_data = YamlValue::scalar("hello world");
    let json_serialized = registry.serialize("!json", &json_data).unwrap();
    println!("Serialized: {}", json_serialized);

    let json_deserialized = registry.deserialize("!json", "\"test value\"").unwrap();
    println!("Deserialized: {:?}\n", json_deserialized);

    // Test environment variable handler
    println!("=== Environment Variable Handler ===");
    std::env::set_var("DEMO_VAR", "Hello from environment!");

    match registry.deserialize("!env", "DEMO_VAR") {
        Ok(env_value) => println!("Environment variable resolved: {:?}", env_value),
        Err(e) => println!("Error: {}", e),
    }

    // Test validation
    match registry.validate("!env", "INVALID VAR NAME") {
        Ok(_) => println!("Validation passed"),
        Err(e) => println!("Validation failed: {}", e),
    }
    println!();

    // Test timestamp handler
    println!("=== Timestamp Handler ===");
    let timestamp = registry.deserialize("!timestamp", "2023-12-25").unwrap();
    println!("Parsed timestamp: {:?}", timestamp);

    // Test custom uppercase handler
    println!("=== Custom Uppercase Handler ===");
    let text_data = YamlValue::scalar("hello world");
    let uppercased = registry.serialize("!uppercased", &text_data).unwrap();
    println!("Uppercased: {}", uppercased);

    let lowercase_input = registry
        .deserialize("!uppercased", "make this uppercase")
        .unwrap();
    println!("From lowercase: {:?}\n", lowercase_input);

    // Test error handling
    println!("=== Error Handling ===");
    match registry.serialize("!nonexistent", &json_data) {
        Ok(_) => println!("Unexpected success"),
        Err(e) => println!("Expected error: {}", e),
    }

    // Create a complex data structure with custom tags
    println!("\n=== Complex Example ===");
    demonstrate_complex_usage(&registry);

    std::env::remove_var("DEMO_VAR");
}

/// A custom tag handler that converts text to uppercase
struct UppercaseHandler;

impl CustomTagHandler for UppercaseHandler {
    fn serialize(&self, value: &YamlValue) -> Result<String, CustomTagError> {
        if let Some(scalar) = value.as_scalar() {
            Ok(scalar.value().to_uppercase())
        } else {
            Err(CustomTagError::new("!uppercased", "Value must be a scalar"))
        }
    }

    fn deserialize(&self, content: &str) -> Result<YamlValue, CustomTagError> {
        Ok(YamlValue::scalar(content.to_uppercase()))
    }

    fn description(&self) -> &str {
        "Converts text to uppercase"
    }

    fn validate(&self, content: &str) -> Result<(), CustomTagError> {
        if content.trim().is_empty() {
            Err(CustomTagError::with_content(
                "!uppercased",
                "Content cannot be empty",
                content,
            ))
        } else {
            Ok(())
        }
    }
}

fn demonstrate_complex_usage(registry: &CustomTagRegistry) {
    println!("Demonstrating complex custom tag usage:");

    // Set up environment variables for the demo
    std::env::set_var("CONFIG_HOST", "localhost");
    std::env::set_var("CONFIG_PORT", "8080");

    // Simulate processing different types of tagged values
    let examples = vec![
        ("!env", "CONFIG_HOST"),
        ("!env", "CONFIG_PORT"),
        ("!json", "{\"key\": \"value\"}"),
        ("!uppercased", "this will be uppercase"),
        ("!timestamp", "2023-12-25"),
    ];

    for (tag, content) in examples {
        match registry.deserialize(tag, content) {
            Ok(value) => {
                println!("  {} '{}' -> {:?}", tag, content, value);
            }
            Err(e) => {
                println!("  {} '{}' -> Error: {}", tag, content, e);
            }
        }
    }

    // Clean up environment variables
    std::env::remove_var("CONFIG_HOST");
    std::env::remove_var("CONFIG_PORT");
}
