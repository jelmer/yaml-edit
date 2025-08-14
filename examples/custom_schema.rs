//! Custom Schema Example
//!
//! This example demonstrates how to create and use custom YAML schemas
//! with user-defined validation rules and constraints.

use yaml_edit::{CustomSchema, CustomValidationResult, ScalarType, SchemaValidator, Yaml};

fn main() {
    println!("=== Custom YAML Schema Example ===\n");

    // 1. Basic Custom Schema - Only allow strings and integers
    println!("1. Basic Custom Schema:");
    basic_custom_schema_example();
    println!();

    // 2. Email Validation Schema
    println!("2. Email Validation Schema:");
    email_validation_example();
    println!();

    // 3. Port Number Validation Schema
    println!("3. Port Number Validation Schema:");
    port_validation_example();
    println!();

    // 4. Multi-field Configuration Schema
    println!("4. Multi-field Configuration Schema:");
    config_validation_example();
    println!();

    // 5. Strict Custom Schema
    println!("5. Strict Custom Schema:");
    strict_schema_example();
}

fn basic_custom_schema_example() {
    // Create a schema that only allows strings and integers (strict mode)
    let custom_schema = CustomSchema::new("basic-types")
        .allow_types(&[ScalarType::String, ScalarType::Integer])
        .strict(); // No type coercion

    let validator = SchemaValidator::custom(custom_schema);

    // Test with valid YAML
    let valid_yaml = r#"
name: "John Doe"
age: 30
id: 12345
"#;

    if let Ok(yaml) = valid_yaml.parse::<Yaml>() {
        if let Some(doc) = yaml.document() {
            match validator.validate(&doc) {
                Ok(_) => println!("   ✓ Valid: Only strings and integers allowed"),
                Err(errors) => {
                    println!("   ✗ Validation failed:");
                    for error in errors {
                        println!("     - {}", error);
                    }
                }
            }
        }
    }

    // Test with invalid YAML (contains boolean)
    let invalid_yaml = r#"
name: "John Doe"
age: 30
active: true  # boolean not allowed!
"#;

    if let Ok(yaml) = invalid_yaml.parse::<Yaml>() {
        if let Some(doc) = yaml.document() {
            match validator.validate(&doc) {
                Ok(_) => println!("   ! Unexpected: validation should have failed"),
                Err(errors) => {
                    println!("   ✓ Correctly rejected boolean type:");
                    for error in errors {
                        println!("     - {}", error);
                    }
                }
            }
        }
    }
}

fn email_validation_example() {
    // Create a schema with custom email validation
    let email_schema = CustomSchema::new("email-validator")
        .allow_type(ScalarType::String)
        .with_validator(ScalarType::String, |value, _path| {
            if value.contains('@') && value.contains('.') && value.len() > 5 {
                // Basic email validation
                if value.matches('@').count() == 1 {
                    CustomValidationResult::Valid
                } else {
                    CustomValidationResult::invalid("email_format", "multiple @ symbols found")
                }
            } else {
                CustomValidationResult::invalid("email_format", format!("invalid email: {}", value))
            }
        });

    let validator = SchemaValidator::custom(email_schema);

    // Test valid emails
    let valid_emails = r#"
admin: "admin@example.com"
user: "user@domain.org"
support: "support@company.co.uk"
"#;

    if let Ok(yaml) = valid_emails.parse::<Yaml>() {
        if let Some(doc) = yaml.document() {
            match validator.validate(&doc) {
                Ok(_) => println!("   ✓ All email addresses are valid"),
                Err(errors) => {
                    println!("   ✗ Email validation failed:");
                    for error in errors {
                        println!("     - {}", error);
                    }
                }
            }
        }
    }

    // Test invalid email
    let invalid_emails = r#"
admin: "admin@example.com"
invalid: "not-an-email"
"#;

    if let Ok(yaml) = invalid_emails.parse::<Yaml>() {
        if let Some(doc) = yaml.document() {
            match validator.validate(&doc) {
                Ok(_) => println!("   ! Unexpected: validation should have failed"),
                Err(errors) => {
                    println!("   ✓ Correctly rejected invalid email:");
                    for error in errors {
                        println!("     - {}", error);
                    }
                }
            }
        }
    }
}

fn port_validation_example() {
    // Custom schema for port number validation
    let port_schema = CustomSchema::new("port-validator")
        .allow_type(ScalarType::Integer)
        .with_validator(ScalarType::Integer, |value, _path| {
            if let Ok(port) = value.parse::<u16>() {
                if (1024..=65535).contains(&port) {
                    CustomValidationResult::Valid
                } else {
                    CustomValidationResult::invalid(
                        "port_range",
                        format!("port {} must be between 1024 and 65535", port),
                    )
                }
            } else {
                CustomValidationResult::invalid(
                    "integer_format",
                    format!("invalid integer: {}", value),
                )
            }
        });

    let validator = SchemaValidator::custom(port_schema);

    // Test valid ports
    let valid_ports = r#"
http_port: 8080
api_port: 3000
db_port: 5432
"#;

    if let Ok(yaml) = valid_ports.parse::<Yaml>() {
        if let Some(doc) = yaml.document() {
            match validator.validate(&doc) {
                Ok(_) => println!("   ✓ All ports are in valid range (1024-65535)"),
                Err(errors) => {
                    println!("   ✗ Port validation failed:");
                    for error in errors {
                        println!("     - {}", error);
                    }
                }
            }
        }
    }

    // Test invalid port (privileged)
    let invalid_ports = r#"
http_port: 8080
ssh_port: 22  # privileged port!
"#;

    if let Ok(yaml) = invalid_ports.parse::<Yaml>() {
        if let Some(doc) = yaml.document() {
            match validator.validate(&doc) {
                Ok(_) => println!("   ! Unexpected: validation should have failed"),
                Err(errors) => {
                    println!("   ✓ Correctly rejected privileged port:");
                    for error in errors {
                        println!("     - {}", error);
                    }
                }
            }
        }
    }
}

fn config_validation_example() {
    // Complex schema with multiple validation rules
    let config_schema = CustomSchema::new("config-validator")
        .allow_types(&[ScalarType::String, ScalarType::Integer, ScalarType::Boolean])
        .with_validator(ScalarType::String, |value, _path| {
            if value.len() >= 3 && value.len() <= 50 {
                CustomValidationResult::Valid
            } else {
                CustomValidationResult::invalid(
                    "string_length",
                    format!("length {} not between 3 and 50 characters", value.len()),
                )
            }
        })
        .with_validator(ScalarType::Integer, |value, _path| {
            if let Ok(num) = value.parse::<i32>() {
                if (0..=10000).contains(&num) {
                    CustomValidationResult::Valid
                } else {
                    CustomValidationResult::invalid(
                        "integer_range",
                        format!("value {} not between 0 and 10000", num),
                    )
                }
            } else {
                CustomValidationResult::invalid(
                    "integer_format",
                    format!("invalid integer: {}", value),
                )
            }
        });

    let validator = SchemaValidator::custom(config_schema);

    // Test valid configuration
    let valid_config = r#"
app_name: "MyApplication"
max_connections: 1000
debug_mode: true
timeout: 30
"#;

    if let Ok(yaml) = valid_config.parse::<Yaml>() {
        if let Some(doc) = yaml.document() {
            match validator.validate(&doc) {
                Ok(_) => println!("   ✓ Configuration is valid"),
                Err(errors) => {
                    println!("   ✗ Configuration validation failed:");
                    for error in errors {
                        println!("     - {}", error);
                    }
                }
            }
        }
    }

    // Test invalid configuration
    let invalid_config = r#"
app_name: "My"  # too short!
max_connections: 50000  # too large!
debug_mode: true
"#;

    if let Ok(yaml) = invalid_config.parse::<Yaml>() {
        if let Some(doc) = yaml.document() {
            match validator.validate(&doc) {
                Ok(_) => println!("   ! Unexpected: validation should have failed"),
                Err(errors) => {
                    println!("   ✓ Correctly rejected invalid configuration:");
                    for error in errors {
                        println!("     - {}", error);
                    }
                }
            }
        }
    }
}

fn strict_schema_example() {
    // Strict custom schema - no type coercion allowed
    let strict_schema = CustomSchema::new("strict-validator")
        .allow_type(ScalarType::String)
        .strict() // No type coercion
        .with_validator(ScalarType::String, |value, _path| {
            if value
                .chars()
                .all(|c| c.is_ascii_alphabetic() || c.is_ascii_whitespace())
            {
                CustomValidationResult::Valid
            } else {
                CustomValidationResult::invalid(
                    "character_set",
                    format!("only letters and spaces allowed: {}", value),
                )
            }
        });

    let validator = SchemaValidator::custom(strict_schema);

    // Test with string that would normally be interpreted as number
    let test_yaml = r#"
name: "John Doe"
code: 12345  # This will be rejected in strict mode even though it could be coerced to string
"#;

    if let Ok(yaml) = test_yaml.parse::<Yaml>() {
        if let Some(doc) = yaml.document() {
            match validator.validate(&doc) {
                Ok(_) => println!("   ! Unexpected: strict validation should have failed"),
                Err(errors) => {
                    println!("   ✓ Strict mode correctly rejected non-string:");
                    for error in errors {
                        println!("     - {}", error);
                    }
                }
            }
        }
    }

    // Test with valid strings only
    let valid_yaml = r#"
name: "John Doe"
title: "Software Engineer"
"#;

    if let Ok(yaml) = valid_yaml.parse::<Yaml>() {
        if let Some(doc) = yaml.document() {
            match validator.validate(&doc) {
                Ok(_) => println!("   ✓ Valid strings passed strict validation"),
                Err(errors) => {
                    println!("   ✗ Strict validation failed:");
                    for error in errors {
                        println!("     - {}", error);
                    }
                }
            }
        }
    }
}
