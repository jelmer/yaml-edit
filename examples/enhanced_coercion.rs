//! Enhanced Type Coercion Example
//!
//! This example demonstrates the improved type coercion capabilities
//! that allow more flexible automatic type conversion rules.

use yaml_edit::{ScalarType, ScalarValue, SchemaValidator, Yaml};

fn main() {
    println!("=== Enhanced Type Coercion Example ===\n");

    // Demonstrate enhanced integer coercion
    println!("1. Enhanced Integer Coercion:");

    // Float to integer (when no fractional part)
    let float_str = ScalarValue::new("42.0");
    if let Some(int_result) = float_str.coerce_to_type(ScalarType::Integer) {
        println!("   '42.0' -> Integer: {}", int_result.value());
    }

    // Numbers with thousand separators
    let with_commas = ScalarValue::new("1,234,567");
    if let Some(int_result) = with_commas.coerce_to_type(ScalarType::Integer) {
        println!("   '1,234,567' -> Integer: {}", int_result.value());
    }

    let with_underscores = ScalarValue::new("1_000_000");
    if let Some(int_result) = with_underscores.coerce_to_type(ScalarType::Integer) {
        println!("   '1_000_000' -> Integer: {}", int_result.value());
    }
    println!();

    // Demonstrate enhanced float coercion
    println!("2. Enhanced Float Coercion:");

    // Integer to float
    let int_str = ScalarValue::new("42");
    if let Some(float_result) = int_str.coerce_to_type(ScalarType::Float) {
        println!("   '42' -> Float: {}", float_result.value());
    }

    // Scientific notation variations
    let fortran_notation = ScalarValue::new("1.23d-4");
    if let Some(float_result) = fortran_notation.coerce_to_type(ScalarType::Float) {
        println!("   '1.23d-4' -> Float: {}", float_result.value());
    }

    let with_separators = ScalarValue::new("1,234.56");
    if let Some(float_result) = with_separators.coerce_to_type(ScalarType::Float) {
        println!("   '1,234.56' -> Float: {}", float_result.value());
    }
    println!();

    // Demonstrate enhanced boolean coercion
    println!("3. Enhanced Boolean Coercion:");

    let boolean_values = [
        ("enabled", true),
        ("disabled", false),
        ("active", true),
        ("inactive", false),
        ("123", true), // Non-zero number
        ("0", false),  // Zero
        ("y", true),
        ("n", false),
    ];

    for (input, expected) in &boolean_values {
        let scalar = ScalarValue::new(*input);
        if let Some(bool_result) = scalar.coerce_to_type(ScalarType::Boolean) {
            let actual = bool_result.value() == "true";
            println!("   '{}' -> Boolean: {} ✓", input, bool_result.value());
            assert_eq!(actual, *expected);
        }
    }
    println!();

    // Demonstrate enhanced null coercion
    println!("4. Enhanced Null Coercion:");

    let null_values = ["nil", "none", "nothing", "undefined", "void", "undef"];
    for input in &null_values {
        let scalar = ScalarValue::new(*input);
        if let Some(null_result) = scalar.coerce_to_type(ScalarType::Null) {
            println!("   '{}' -> Null: {}", input, null_result.value());
        }
    }
    println!();

    // Demonstrate enhanced timestamp coercion
    println!("5. Enhanced Timestamp Coercion:");

    let timestamp_formats = [
        "2023-12-25",       // ISO date
        "2023/12/25",       // Slash format
        "12/25/2023",       // US format
        "2023-12-25 15:30", // Date with time
        "1640995200",       // Unix timestamp
    ];

    for format in &timestamp_formats {
        let scalar = ScalarValue::new(*format);
        if let Some(ts_result) = scalar.coerce_to_type(ScalarType::Timestamp) {
            println!("   '{}' -> Timestamp: {}", format, ts_result.value());
        }
    }
    println!();

    // Demonstrate cross-type coercion chains
    println!("6. Cross-Type Coercion Chains:");

    // Show that integers can be coerced to floats
    let int_val = ScalarValue::from(42i64);
    if let Some(float_result) = int_val.coerce_to_type(ScalarType::Float) {
        println!("   Integer 42 -> Float: {}", float_result.value());
    }

    // Show that floats without fractional parts can become integers
    let clean_float = ScalarValue::from(100.0f64);
    if let Some(int_result) = clean_float.coerce_to_type(ScalarType::Integer) {
        println!("   Float 100.0 -> Integer: {}", int_result.value());
    }
    println!();

    // Demonstrate practical usage with schema validation
    println!("7. Practical Schema Validation with Enhanced Coercion:");

    let config_yaml = r#"
# Configuration with various formats that can be coerced
database:
  port: "5432"        # String that looks like integer
  timeout: "30.0"     # String that looks like float
  enabled: "true"     # String that looks like boolean
  ssl_mode: disabled  # Word that maps to boolean
  max_connections: 1,000  # Number with comma separator
  
server:
  host: "localhost"
  debug: active       # Maps to boolean true
  log_level: INFO     # Stays as string
"#;

    match config_yaml.parse::<Yaml>() {
        Ok(yaml) => {
            if let Some(document) = yaml.document() {
                // Test with JSON schema (allows strings, numbers, booleans, nulls)
                let json_validator = SchemaValidator::json();

                println!("   JSON Schema validation (with coercion):");
                match json_validator.validate(&document) {
                    Ok(_) => println!("   ✓ Document passes JSON validation with coercion"),
                    Err(errors) => {
                        println!("   ✗ Some values cannot be coerced:");
                        for error in errors.iter().take(3) {
                            println!("     - {}", error);
                        }
                    }
                }

                // Test coercion capability
                match json_validator.can_coerce(&document) {
                    Ok(_) => println!("   ✓ All values can be coerced to JSON-compatible types"),
                    Err(errors) => {
                        println!("   ✗ Some values cannot be coerced:");
                        for error in errors.iter().take(3) {
                            println!("     - {}", error);
                        }
                    }
                }
            }
        }
        Err(e) => println!("   Failed to parse YAML: {}", e),
    }

    println!("\n=== Enhanced Type Coercion Complete! ===");
}
