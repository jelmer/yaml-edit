//! Schema validation example
//!
//! This example demonstrates how to use YAML schema validation to enforce
//! different type constraints on YAML documents.

use yaml_edit::{Schema, SchemaValidator, Yaml};

fn main() {
    println!("=== YAML Schema Validation Example ===\n");

    // Example YAML documents with different types
    let config_yaml = r#"
# Application configuration
app:
  name: "My Application"
  version: "1.0.0"
  debug: true
  port: 8080
  timeout: 30.5

# User data
users:
  - name: "Alice"
    age: 25
    active: true
  - name: "Bob"
    age: 30
    active: false

# Metadata with complex types
metadata:
  created: 2023-12-25T10:30:45Z
  pattern: !!regex '\d{3}-\d{4}'
  data: !!binary "SGVsbG8gV29ybGQ="
"#;

    // Parse the YAML
    let yaml: Yaml = match config_yaml.parse() {
        Ok(y) => y,
        Err(e) => {
            eprintln!("Failed to parse YAML: {}", e);
            return;
        }
    };

    let document = match yaml.document() {
        Some(doc) => doc,
        None => {
            eprintln!("No document found in YAML");
            return;
        }
    };

    println!("Validating YAML document against different schemas:\n");

    // 1. Validate against Failsafe schema (strings, sequences, mappings only)
    println!("1. Failsafe Schema (strings, sequences, mappings only):");
    let failsafe_validator = SchemaValidator::failsafe();
    match failsafe_validator.validate(&document) {
        Ok(_) => println!("   ✓ Validation passed (with coercion)"),
        Err(errors) => {
            println!("   ✗ Validation failed:");
            for error in &errors {
                println!("     - {}", error);
            }
        }
    }

    // Strict failsafe validation (no coercion)
    let strict_failsafe = SchemaValidator::failsafe().strict();
    match strict_failsafe.validate(&document) {
        Ok(_) => println!("   ✓ Strict validation passed"),
        Err(errors) => {
            println!("   ✗ Strict validation failed ({} errors):", errors.len());
            for (i, error) in errors.iter().take(3).enumerate() {
                println!("     {}. {}", i + 1, error);
            }
            if errors.len() > 3 {
                println!("     ... and {} more errors", errors.len() - 3);
            }
        }
    }
    println!();

    // 2. Validate against JSON schema (JSON-compatible types)
    println!("2. JSON Schema (string, number, boolean, null, array, object):");
    let json_validator = SchemaValidator::json();
    match json_validator.validate(&document) {
        Ok(_) => println!("   ✓ Validation passed"),
        Err(errors) => {
            println!("   ✗ Validation failed ({} errors):", errors.len());
            for (i, error) in errors.iter().take(3).enumerate() {
                println!("     {}. {}", i + 1, error);
            }
            if errors.len() > 3 {
                println!("     ... and {} more errors", errors.len() - 3);
            }
        }
    }
    println!();

    // 3. Validate against Core schema (all YAML types)
    println!("3. Core Schema (all YAML 1.2 types including timestamps, regex, binary):");
    let core_validator = SchemaValidator::core();
    match core_validator.validate(&document) {
        Ok(_) => println!("   ✓ Validation passed"),
        Err(errors) => {
            println!("   ✗ Validation failed:");
            for error in &errors {
                println!("     - {}", error);
            }
        }
    }
    println!();

    // 4. Test coercion capabilities
    println!("4. Coercion Test:");
    let coercion_yaml = r#"
# Numbers as strings that could be coerced
count: "42"
enabled: "true"
rate: "3.14"
items:
  - "100"
  - "false"
  - "null"
"#;

    if let Ok(yaml) = coercion_yaml.parse::<Yaml>() {
        if let Some(coercion_doc) = yaml.document() {
            println!("   Testing coercion for JSON schema:");
            match json_validator.can_coerce(&coercion_doc) {
                Ok(_) => println!("   ✓ Document can be coerced to JSON types"),
                Err(errors) => {
                    println!("   ✗ Document cannot be coerced:");
                    for error in &errors {
                        println!("     - {}", error);
                    }
                }
            }

            println!("   Testing strict validation (no coercion):");
            let strict_json = SchemaValidator::json().strict();
            match strict_json.validate(&coercion_doc) {
                Ok(_) => println!("   ✓ Strict validation passed"),
                Err(errors) => {
                    println!(
                        "   ✗ Strict validation failed (expected - strings are valid in JSON)"
                    );
                    for error in errors.iter().take(2) {
                        println!("     - {}", error);
                    }
                }
            }
        }
    }
    println!();

    // 5. Demonstrate schema type checking
    println!("5. Schema Type Information:");
    for schema in [Schema::Failsafe, Schema::Json, Schema::Core] {
        println!("   {} schema allows:", schema.name());
        let types = schema.allowed_scalar_types();
        for scalar_type in types {
            println!("     - {:?}", scalar_type);
        }
        println!();
    }

    // 6. Practical validation workflow
    println!("6. Practical Validation Workflow:");
    demonstrate_validation_workflow();
}

fn demonstrate_validation_workflow() {
    // Example: Validating a configuration file that should be JSON-compatible
    let json_config = r#"
{
  "database": {
    "host": "localhost",
    "port": 5432,
    "ssl": true,
    "timeout": 30
  },
  "features": ["auth", "logging", "metrics"],
  "debug": false
}
"#;

    // This should pass JSON schema validation
    if let Ok(yaml) = json_config.parse::<Yaml>() {
        if let Some(doc) = yaml.document() {
            let validator = SchemaValidator::json();
            match validator.validate(&doc) {
                Ok(_) => println!("   ✓ JSON configuration is valid"),
                Err(errors) => {
                    println!("   ✗ JSON configuration validation failed:");
                    for error in errors {
                        println!("     {}", error);
                    }
                }
            }
        }
    }

    // Example: A YAML file that uses YAML-specific features
    let yaml_config = r#"
# Configuration with YAML-specific types
app:
  name: My App
  created: 2023-12-25T10:30:45Z  # Timestamp
  pattern: !!regex '^[a-z]+$'    # Regex
  data: !!binary SGVsbG8=        # Binary data
"#;

    if let Ok(yaml) = yaml_config.parse::<Yaml>() {
        if let Some(doc) = yaml.document() {
            // This should fail JSON validation but pass Core validation
            let json_validator = SchemaValidator::json().strict();
            let core_validator = SchemaValidator::core();

            println!("   Testing YAML-specific types:");
            match json_validator.validate(&doc) {
                Ok(_) => println!("   ! Unexpected: JSON validation passed"),
                Err(_) => println!("   ✓ JSON validation correctly rejected YAML-specific types"),
            }

            match core_validator.validate(&doc) {
                Ok(_) => println!("   ✓ Core validation accepted all YAML types"),
                Err(errors) => {
                    println!("   ✗ Core validation failed:");
                    for error in errors {
                        println!("     {}", error);
                    }
                }
            }
        }
    }
}
