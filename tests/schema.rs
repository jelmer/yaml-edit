//! Validating documents against the failsafe, JSON and core schemas and
//! custom ones: which values each schema accepts, coercion and error paths.

use yaml_edit::{
    CustomSchema, CustomValidationResult, Document, ScalarType, Schema, SchemaValidator,
    ValidationError, ValidationErrorKind, YamlFile,
};

#[test]
fn schema_names() {
    assert_eq!(Schema::Failsafe.name(), "failsafe");
    assert_eq!(Schema::Json.name(), "json");
    assert_eq!(Schema::Core.name(), "core");
}

#[test]
fn failsafe_schema_allows_only_strings() {
    let schema = Schema::Failsafe;

    assert!(schema.allows_scalar_type(ScalarType::String));
    assert!(!schema.allows_scalar_type(ScalarType::Integer));
    assert!(!schema.allows_scalar_type(ScalarType::Float));
    assert!(!schema.allows_scalar_type(ScalarType::Boolean));
    assert!(!schema.allows_scalar_type(ScalarType::Null));
    #[cfg(feature = "base64")]
    assert!(!schema.allows_scalar_type(ScalarType::Binary));
    assert!(!schema.allows_scalar_type(ScalarType::Timestamp));
    assert!(!schema.allows_scalar_type(ScalarType::Regex));
}

#[test]
fn json_schema_allows_json_types() {
    let schema = Schema::Json;

    assert!(schema.allows_scalar_type(ScalarType::String));
    assert!(schema.allows_scalar_type(ScalarType::Integer));
    assert!(schema.allows_scalar_type(ScalarType::Float));
    assert!(schema.allows_scalar_type(ScalarType::Boolean));
    assert!(schema.allows_scalar_type(ScalarType::Null));
    #[cfg(feature = "base64")]
    assert!(!schema.allows_scalar_type(ScalarType::Binary));
    assert!(!schema.allows_scalar_type(ScalarType::Timestamp));
    assert!(!schema.allows_scalar_type(ScalarType::Regex));
}

#[test]
fn core_schema_allows_all_types() {
    let schema = Schema::Core;

    assert!(schema.allows_scalar_type(ScalarType::String));
    assert!(schema.allows_scalar_type(ScalarType::Integer));
    assert!(schema.allows_scalar_type(ScalarType::Float));
    assert!(schema.allows_scalar_type(ScalarType::Boolean));
    assert!(schema.allows_scalar_type(ScalarType::Null));
    #[cfg(feature = "base64")]
    assert!(schema.allows_scalar_type(ScalarType::Binary));
    assert!(schema.allows_scalar_type(ScalarType::Timestamp));
    assert!(schema.allows_scalar_type(ScalarType::Regex));
}

#[test]
fn validation_error_display() {
    let error = ValidationError::type_not_allowed(
        "root.items[0]",
        "test-schema",
        ScalarType::Integer,
        vec![ScalarType::String],
    );

    assert_eq!(
            format!("{}", error),
            "Validation error at root.items[0]: type Integer not allowed in test-schema schema, expected one of [String]"
        );
}

fn create_test_document(content: &str) -> Document {
    let parsed = content
        .parse::<YamlFile>()
        .expect("Failed to parse test YAML");
    parsed.document().expect("Expected a document")
}

#[test]
fn failsafe_validation_success() {
    let yaml_str = r#"
name: "John Doe"
items:
  - "item1"
  - "item2"
nested:
  key: "value"
"#;
    let document = create_test_document(yaml_str);
    let validator = SchemaValidator::failsafe();

    assert!(validator.validate(&document).is_ok());
}

#[test]
fn json_validation_success() {
    let yaml_str = r#"
name: "John Doe"
age: 30
height: 5.9
active: true
metadata: null
items:
  - "item1"
  - 42
  - true
"#;
    let document = create_test_document(yaml_str);
    let validator = SchemaValidator::json();

    assert!(validator.validate(&document).is_ok());
}

#[test]
fn core_validation_success() {
    let yaml_str = "\nname: \"John Doe\" \nage: 30\nbirth_date: !!timestamp \"2001-12-15T02:59:43.1Z\"\npattern: !!regex '\\d{3}-\\d{4}'\n";
    let document = create_test_document(yaml_str);
    let validator = SchemaValidator::core();

    assert!(validator.validate(&document).is_ok());
}

#[test]
fn failsafe_validation_failure() {
    let yaml_str = r#"
name: "John"
age: 30
active: true
"#;
    let document = create_test_document(yaml_str);
    let validator = SchemaValidator::failsafe().strict();

    let result = validator.validate(&document);
    // Should fail because age (integer) and active (boolean) are not allowed in failsafe schema
    assert!(result.is_err());
    let errors = result.unwrap_err();
    assert!(!errors.is_empty());

    // Should have errors for integer and boolean types
    assert!(errors.iter().all(|e| e.schema_name == "failsafe"));
    assert!(errors
        .iter()
        .all(|e| matches!(&e.kind, ValidationErrorKind::TypeNotAllowed { .. })));
}

#[test]
fn validation_error_paths() {
    let yaml_str =
        "\nusers:\n  - name: \"John\"\n    age: 30\n  - name: \"Jane\" \n    active: true\n";
    let document = create_test_document(yaml_str);
    let validator = SchemaValidator::failsafe();

    let result = validator.validate(&document);
    if let Err(errors) = result {
        // Check that error paths are meaningful and start with "root"
        for error in &errors {
            assert!(!error.path.is_empty());
            assert!(
                error.path.starts_with("root"),
                "expected path to start with 'root', got: {:?}",
                error.path
            );
        }
    }
}

#[test]
fn schema_type_lists() {
    assert_eq!(
        Schema::Failsafe.allowed_scalar_types(),
        vec![ScalarType::String]
    );

    assert_eq!(
        Schema::Json.allowed_scalar_types(),
        vec![
            ScalarType::String,
            ScalarType::Integer,
            ScalarType::Float,
            ScalarType::Boolean,
            ScalarType::Null,
        ]
    );

    let core_types = Schema::Core.allowed_scalar_types();
    // Core includes at minimum: String, Integer, Float, Boolean, Null, Timestamp, Regex
    // (plus Binary if the base64 feature is enabled)
    #[cfg_attr(not(feature = "base64"), allow(unused_mut))]
    let mut expected_core = vec![
        ScalarType::String,
        ScalarType::Integer,
        ScalarType::Float,
        ScalarType::Boolean,
        ScalarType::Null,
        ScalarType::Timestamp,
        ScalarType::Regex,
    ];
    #[cfg(feature = "base64")]
    expected_core.insert(5, ScalarType::Binary);
    assert_eq!(core_types, expected_core);
}

#[test]
fn deep_sequence_validation() {
    let yaml_str = r#"
numbers:
  - 1
  - 2.5
  - "three"
  - true
"#;
    let document = create_test_document(yaml_str);

    // Failsafe should fail for non-string types in the sequence
    let failsafe_validator = SchemaValidator::failsafe().strict();
    let result = failsafe_validator.validate(&document);
    assert!(result.is_err());
    let errors = result.unwrap_err();
    assert!(!errors.is_empty());

    // JSON should succeed since all types are JSON-compatible
    let json_validator = SchemaValidator::json();
    let result = json_validator.validate(&document);
    assert!(result.is_ok());

    // Core should succeed since all types are allowed
    let core_validator = SchemaValidator::core();
    let result = core_validator.validate(&document);
    assert!(result.is_ok());
}

#[test]
fn deep_nested_mapping_validation() {
    let yaml_str = r#"
user:
  name: "John"
  details:
    age: 30
    active: true
    scores:
      - 95
      - 87.5
"#;
    let document = create_test_document(yaml_str);

    // Failsafe should fail due to nested integers and booleans
    let failsafe_validator = SchemaValidator::failsafe().strict();
    let result = failsafe_validator.validate(&document);
    assert!(result.is_err());
    let errors = result.unwrap_err();
    assert!(!errors.is_empty());
    // Check that errors have meaningful paths
    let paths: Vec<&str> = errors.iter().map(|e| e.path.as_str()).collect();
    assert!(paths.contains(&"root.user.details.age"));
    assert!(paths.contains(&"root.user.details.scores[0]"));

    // JSON should succeed
    let json_validator = SchemaValidator::json();
    let result = json_validator.validate(&document);
    assert!(result.is_ok());
}

#[test]
fn complex_yaml_types_validation() {
    let yaml_str = r#"
metadata:
  created: !!timestamp "2023-12-25T10:30:45Z"
  pattern: !!regex '\d{3}-\d{4}'
values:
  - !!timestamp "2023-01-01"
  - !!regex '[a-zA-Z]+'
"#;
    let document = create_test_document(yaml_str);

    // Failsafe should fail
    let failsafe_validator = SchemaValidator::failsafe().strict();
    let result = failsafe_validator.validate(&document);
    assert!(result.is_err());

    // JSON should fail
    let json_validator = SchemaValidator::json().strict();
    let result = json_validator.validate(&document);
    assert!(result.is_err());

    // Core should succeed
    let core_validator = SchemaValidator::core();
    let result = core_validator.validate(&document);
    assert!(result.is_ok());
}

#[test]
fn coercion_deep_validation() {
    let yaml_str = "\nconfig:\n  timeout: \"30\"  # string that looks like number\n  enabled: \"true\"  # string that looks like boolean  \n  items:\n    - \"42\"\n    - \"false\"\n";
    let document = create_test_document(yaml_str);

    // Non-strict JSON validation should pass via coercion
    let json_validator = SchemaValidator::json();
    let result = json_validator.can_coerce(&document);
    assert!(result.is_ok());

    // Strict JSON validation should fail (strings are not the exact types)
    let strict_json_validator = SchemaValidator::json().strict();
    let result = strict_json_validator.validate(&document);
    // Actually strings are allowed in JSON schema, so this should pass
    assert!(result.is_ok());

    // But if we put non-JSON types, strict mode should fail
    let problematic_yaml = r#"
data:
  timestamp: !!timestamp "2023-12-25"
"#;
    let problematic_doc = create_test_document(problematic_yaml);
    let result = strict_json_validator.validate(&problematic_doc);
    assert!(result.is_err());
}

#[test]
fn validation_error_paths_nested() {
    let yaml_str = r#"
users:
  - name: "Alice"
    metadata:
      created: !!timestamp "2023-01-01"
      tags:
        - "admin"
        - 42  # This should fail in failsafe
  - name: "Bob"
    active: true  # This should fail in failsafe
"#;
    let document = create_test_document(yaml_str);
    let validator = SchemaValidator::failsafe().strict();

    let result = validator.validate(&document);
    assert!(result.is_err());
    let errors = result.unwrap_err();
    assert!(!errors.is_empty());

    // Check that we have errors with proper path information
    let paths: Vec<&str> = errors.iter().map(|e| e.path.as_str()).collect();

    // Should have paths that show the nested structure
    assert!(paths.contains(&"root.users[0].metadata.created"));
    assert!(paths.contains(&"root.users[0].metadata.tags[1]"));
    assert!(paths.contains(&"root.users[1].active"));

    // Print paths for debugging if needed
    for error in &errors {
        println!("Error at {}: {}", error.path, error.message());
    }
}

#[test]
fn yaml_1_2_spec_compliance() {
    // Test that our schemas match YAML 1.2 specification requirements

    // 1. Failsafe Schema - Should only allow strings, mappings, and sequences
    let failsafe = Schema::Failsafe;
    assert!(failsafe.allows_scalar_type(ScalarType::String));
    assert!(!failsafe.allows_scalar_type(ScalarType::Integer));
    assert!(!failsafe.allows_scalar_type(ScalarType::Float));
    assert!(!failsafe.allows_scalar_type(ScalarType::Boolean));
    assert!(!failsafe.allows_scalar_type(ScalarType::Null));
    assert!(!failsafe.allows_scalar_type(ScalarType::Timestamp));

    // 2. JSON Schema - Should allow JSON-compatible types
    let json = Schema::Json;
    assert!(json.allows_scalar_type(ScalarType::String));
    assert!(json.allows_scalar_type(ScalarType::Integer));
    assert!(json.allows_scalar_type(ScalarType::Float));
    assert!(json.allows_scalar_type(ScalarType::Boolean));
    assert!(json.allows_scalar_type(ScalarType::Null));
    assert!(!json.allows_scalar_type(ScalarType::Timestamp)); // Not in JSON
    assert!(!json.allows_scalar_type(ScalarType::Regex)); // Not in JSON
    #[cfg(feature = "base64")]
    assert!(!json.allows_scalar_type(ScalarType::Binary)); // Not in JSON

    // 3. Core Schema - Should allow all YAML types
    let core = Schema::Core;
    assert!(core.allows_scalar_type(ScalarType::String));
    assert!(core.allows_scalar_type(ScalarType::Integer));
    assert!(core.allows_scalar_type(ScalarType::Float));
    assert!(core.allows_scalar_type(ScalarType::Boolean));
    assert!(core.allows_scalar_type(ScalarType::Null));
    assert!(core.allows_scalar_type(ScalarType::Timestamp));
    assert!(core.allows_scalar_type(ScalarType::Regex));
    #[cfg(feature = "base64")]
    assert!(core.allows_scalar_type(ScalarType::Binary));

    // Test schema names match spec
    assert_eq!(failsafe.name(), "failsafe");
    assert_eq!(json.name(), "json");
    assert_eq!(core.name(), "core");
}

#[test]
fn spec_compliant_validation_examples() {
    // Examples from YAML 1.2 specification

    // Failsafe: Should accept plain strings but reject typed values
    let failsafe_yaml = r#"
string: hello
number_as_string: "123"
"#;
    let failsafe_doc = create_test_document(failsafe_yaml);
    let failsafe_validator = SchemaValidator::failsafe();
    // Non-strict mode allows coercion
    assert!(failsafe_validator.validate(&failsafe_doc).is_ok());

    // JSON: Should accept JSON-compatible types
    let json_yaml = r#"
string: "hello"
number: 42
float: 3.14
boolean: true
null_value: null
"#;
    let json_doc = create_test_document(json_yaml);
    let json_validator = SchemaValidator::json();
    assert!(json_validator.validate(&json_doc).is_ok());

    // Core: Should accept all YAML types including timestamps
    let core_yaml = r#"
timestamp: 2023-01-01T00:00:00Z
regex: !!regex '[0-9]+'
binary: !!binary "SGVsbG8gV29ybGQ="
"#;
    let core_doc = create_test_document(core_yaml);
    let core_validator = SchemaValidator::core();
    assert!(core_validator.validate(&core_doc).is_ok());

    // JSON should reject YAML-specific types
    let json_strict = SchemaValidator::json().strict();
    assert!(json_strict.validate(&core_doc).is_err());
}

#[test]
fn custom_schema_basic() {
    // Create a custom schema that only allows strings and integers (strict mode to prevent coercion)
    let custom_schema = CustomSchema::new("test")
        .allow_types(&[ScalarType::String, ScalarType::Integer])
        .strict(); // No coercion

    let validator = SchemaValidator::custom(custom_schema);

    // Test with allowed types
    let valid_yaml = r#"
name: hello world
count: 42
"#;
    let valid_doc = create_test_document(valid_yaml);
    let result = validator.validate(&valid_doc);
    if let Err(ref errors) = result {
        for error in errors {
            println!("Valid test error: {}", error);
        }
    }
    assert!(result.is_ok());

    // Test with disallowed type
    let invalid_yaml = r#"
name: hello world
enabled: true  # boolean not allowed
"#;
    let invalid_doc = create_test_document(invalid_yaml);
    let result = validator.validate(&invalid_doc);
    assert!(result.is_err());

    let errors = result.unwrap_err();
    assert_eq!(errors.len(), 1);
    assert_eq!(
        errors[0].message(),
        "type Boolean not allowed in test schema, expected one of [String, Integer]"
    );
}

#[test]
fn custom_schema_with_validators() {
    // Create a custom schema with string validators
    let custom_schema = CustomSchema::new("email-validation")
        .allow_type(ScalarType::String)
        .with_validator(ScalarType::String, |value, _path| {
            if value.contains('@') && value.contains('.') {
                CustomValidationResult::Valid
            } else {
                CustomValidationResult::invalid("email_format", "invalid email format")
            }
        });

    let validator = SchemaValidator::custom(custom_schema);

    // Test with valid email
    let valid_yaml = r#"
email: "user@example.com"
"#;
    let valid_doc = create_test_document(valid_yaml);
    assert!(validator.validate(&valid_doc).is_ok());

    // Test with invalid email
    let invalid_yaml = r#"
email: "not-an-email"
"#;
    let invalid_doc = create_test_document(invalid_yaml);
    let result = validator.validate(&invalid_doc);
    assert!(result.is_err());

    let errors = result.unwrap_err();
    assert_eq!(errors.len(), 1);
    assert_eq!(
        errors[0].message(),
        "custom constraint 'email_format: invalid email format' failed for value 'not-an-email'"
    );
}

#[test]
fn custom_schema_integer_range() {
    // Custom schema with integer range validation
    let custom_schema = CustomSchema::new("port-validation")
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

    let validator = SchemaValidator::custom(custom_schema);

    // Test with valid port
    let valid_yaml = r#"
port: 8080
"#;
    let valid_doc = create_test_document(valid_yaml);
    assert!(validator.validate(&valid_doc).is_ok());

    // Test with invalid port (too low)
    let invalid_yaml = r#"
port: 80
"#;
    let invalid_doc = create_test_document(invalid_yaml);
    let result = validator.validate(&invalid_doc);
    assert!(result.is_err());

    let errors = result.unwrap_err();
    assert!(!errors.is_empty());
    assert_eq!(
            errors[0].message(),
            "custom constraint 'port_range: port 80 must be between 1024 and 65535' failed for value '80'"
        );
}

#[test]
fn custom_schema_multiple_validators() {
    // Schema with multiple type validators
    let custom_schema = CustomSchema::new("config-validation")
        .allow_types(&[ScalarType::String, ScalarType::Integer])
        .with_validator(ScalarType::String, |value, _path| {
            if value.len() >= 3 {
                CustomValidationResult::Valid
            } else {
                CustomValidationResult::invalid(
                    "string_length",
                    format!("string too short: '{}'", value),
                )
            }
        })
        .with_validator(ScalarType::Integer, |value, _path| {
            if let Ok(num) = value.parse::<i32>() {
                if num >= 0 {
                    CustomValidationResult::Valid
                } else {
                    CustomValidationResult::invalid(
                        "negative_number",
                        format!("negative numbers not allowed: {}", num),
                    )
                }
            } else {
                CustomValidationResult::invalid(
                    "integer_format",
                    format!("invalid integer: {}", value),
                )
            }
        });

    let validator = SchemaValidator::custom(custom_schema);

    // Test with valid values
    let valid_yaml = r#"
name: "valid-name"
count: 100
"#;
    let valid_doc = create_test_document(valid_yaml);
    assert!(validator.validate(&valid_doc).is_ok());

    // Test with invalid string (too short)
    let invalid_yaml = r#"
name: "ab"
count: 100
"#;
    let invalid_doc = create_test_document(invalid_yaml);
    let result = validator.validate(&invalid_doc);
    assert!(result.is_err());

    let errors = result.unwrap_err();
    assert_eq!(errors.len(), 1);
    assert_eq!(
        errors[0].message(),
        "custom constraint 'string_length: string too short: 'ab'' failed for value 'ab'"
    );
}

#[test]
fn custom_schema_strict_mode() {
    let custom_schema = CustomSchema::new("strict-test")
        .allow_type(ScalarType::String)
        .strict();

    let validator = SchemaValidator::custom(custom_schema);

    // Test that even valid integers are rejected in strict mode
    let yaml_with_int = r#"
value: 42
"#;
    let doc = create_test_document(yaml_with_int);
    let result = validator.validate(&doc);
    assert!(result.is_err());

    let errors = result.unwrap_err();
    assert_eq!(errors.len(), 1);
    assert_eq!(
        errors[0].message(),
        "type Integer not allowed in strict-test schema, expected one of [String]"
    );
}

#[test]
fn custom_schema_name() {
    let custom_schema = CustomSchema::new("my-custom-schema").allow_type(ScalarType::String);
    let schema = Schema::Custom(custom_schema);

    assert_eq!(schema.name(), "my-custom-schema");
}

#[test]
fn quoted_scalars_are_strings() {
    let document = create_test_document("a: \"123\"\nb: 'true'\nc: \"null\"\n");
    assert_eq!(
        SchemaValidator::failsafe().strict().validate(&document),
        Ok(())
    );
    assert_eq!(SchemaValidator::failsafe().can_coerce(&document), Ok(()));
}

#[test]
fn plain_base64_like_word_is_string() {
    let document = create_test_document("k: test\n");
    assert_eq!(SchemaValidator::json().strict().validate(&document), Ok(()));
    assert_eq!(
        SchemaValidator::failsafe().strict().validate(&document),
        Ok(())
    );
}

#[test]
fn quoted_date_is_string() {
    let document = create_test_document("k: \"2023-01-01\"\n");
    assert_eq!(SchemaValidator::json().strict().validate(&document), Ok(()));
}

#[test]
fn custom_schema_quoted_integer_is_string() {
    let custom_schema = CustomSchema::new("strings")
        .allow_type(ScalarType::String)
        .strict();
    let validator = SchemaValidator::custom(custom_schema);
    let document = create_test_document("k: \"123\"\n");
    assert_eq!(validator.validate(&document), Ok(()));
    let document = create_test_document("k: 123\n");
    assert_eq!(
        validator.validate(&document),
        Err(vec![ValidationError::type_not_allowed(
            "root.k",
            "strings",
            ScalarType::Integer,
            vec![ScalarType::String],
        )])
    );
}

#[test]
fn plain_integer_still_rejected_by_failsafe_strict() {
    let document = create_test_document("k: 123\n");
    assert_eq!(
        SchemaValidator::failsafe().strict().validate(&document),
        Err(vec![ValidationError::type_not_allowed(
            "root.k",
            "failsafe",
            ScalarType::Integer,
            vec![ScalarType::String],
        )])
    );
}

#[test]
fn verbatim_tags_validate_as_shorthand() {
    let validate = |yaml: &str| {
        SchemaValidator::json()
            .strict()
            .validate(&create_test_document(yaml))
    };
    let shorthand = validate("t: !!timestamp \"2023-12-25\"\nr: !!regex '\\d+'\n");
    assert_eq!(shorthand.as_ref().map_err(|e| e.len()), Err(2));
    assert_eq!(
            validate(
                "t: !<tag:yaml.org,2002:timestamp> \"2023-12-25\"\nr: !<tag:yaml.org,2002:regex> '\\d+'\n"
            ),
            shorthand,
        );
}

fn json_types() -> Vec<ScalarType> {
    Schema::Json.allowed_scalar_types()
}

#[test]
fn tagged_root_scalar_validated() {
    let document = create_test_document("--- !!timestamp 2001-01-01\n");
    let expected = Err(vec![ValidationError::type_not_allowed(
        "root",
        "json",
        ScalarType::Timestamp,
        json_types(),
    )]);
    assert_eq!(
        SchemaValidator::json().strict().validate(&document),
        expected
    );
    assert_eq!(SchemaValidator::json().can_coerce(&document), expected);
}

#[test]
fn core_tag_sets_scalar_type() {
    let document = create_test_document("a: !!int 5\n");
    let expected = Err(vec![ValidationError::type_not_allowed(
        "root.a",
        "failsafe",
        ScalarType::Integer,
        vec![ScalarType::String],
    )]);
    assert_eq!(
        SchemaValidator::failsafe().strict().validate(&document),
        expected
    );
    assert_eq!(SchemaValidator::failsafe().can_coerce(&document), expected);
    let document = create_test_document("a: !!str 5\n");
    assert_eq!(
        SchemaValidator::failsafe().strict().validate(&document),
        Ok(())
    );
}

#[test]
fn custom_schema_tagged_scalar_uses_content() {
    let seen = std::sync::Arc::new(std::sync::Mutex::new(Vec::new()));
    let seen_in = seen.clone();
    let custom_schema = CustomSchema::new("ints")
        .allow_type(ScalarType::Integer)
        .with_validator(ScalarType::Integer, move |value, path| {
            seen_in
                .lock()
                .unwrap()
                .push((value.to_string(), path.to_string()));
            CustomValidationResult::Valid
        })
        .strict();
    let validator = SchemaValidator::custom(custom_schema);
    let document = create_test_document("a: !!int 42\n");
    assert_eq!(validator.validate(&document), Ok(()));
    assert_eq!(
        *seen.lock().unwrap(),
        vec![("42".to_string(), "root.a".to_string())]
    );
}

#[test]
fn tagged_collection_contents_validated() {
    let document = create_test_document("a: !foo [x, !!timestamp 2001-01-01]\n");
    let expected = Err(vec![ValidationError::type_not_allowed(
        "root.a[1]",
        "json",
        ScalarType::Timestamp,
        json_types(),
    )]);
    assert_eq!(
        SchemaValidator::json().strict().validate(&document),
        expected
    );
    assert_eq!(SchemaValidator::json().can_coerce(&document), expected);
    let document = create_test_document("a: !foo {b: !!timestamp 2001-01-01}\n");
    assert_eq!(
        SchemaValidator::json().strict().validate(&document),
        Err(vec![ValidationError::type_not_allowed(
            "root.a.b",
            "json",
            ScalarType::Timestamp,
            json_types(),
        )])
    );
}

#[test]
fn coercion_failed_error() {
    let error = ValidationError::coercion_failed(
        "root.a",
        "bools",
        ScalarType::String,
        vec![ScalarType::Boolean],
    );
    assert_eq!(
        error.kind,
        ValidationErrorKind::CoercionFailed {
            from_type: ScalarType::String,
            to_types: vec![ScalarType::Boolean],
        }
    );
    assert_eq!(
        error.to_string(),
        "Validation error at root.a: cannot coerce String to any of [Boolean] in bools schema"
    );
}

#[test]
fn custom_constraint_failed_display() {
    let error = ValidationError::custom_constraint_failed("root.p", "ports", "range", "80");
    assert_eq!(
        error.to_string(),
        "Validation error at root.p: custom constraint 'range' failed for value '80'"
    );
}

#[test]
fn custom_validation_result_predicates() {
    assert!(CustomValidationResult::Valid.is_valid());
    assert!(!CustomValidationResult::Valid.is_invalid());
    let invalid = CustomValidationResult::invalid("c", "r");
    assert!(!invalid.is_valid());
    assert!(invalid.is_invalid());
    assert_eq!(
        invalid,
        CustomValidationResult::Invalid {
            constraint: "c".to_string(),
            reason: "r".to_string(),
        }
    );
}

#[test]
fn custom_schema_debug() {
    let schema = CustomSchema::new("dbg")
        .allow_types(&[ScalarType::String, ScalarType::String, ScalarType::Integer])
        .allow_type(ScalarType::Integer)
        .with_validator(ScalarType::String, |_, _| CustomValidationResult::Valid)
        .strict();
    assert_eq!(
            format!("{schema:?}"),
            "CustomSchema { name: \"dbg\", allowed_types: [String, Integer], allow_coercion: false, validators: \"<1 validators>\" }"
        );
}

#[test]
fn custom_schema_equality_and_types() {
    let a = Schema::Custom(CustomSchema::new("same").allow_type(ScalarType::String));
    let b = Schema::Custom(CustomSchema::new("same").allow_type(ScalarType::Integer));
    let c = Schema::Custom(CustomSchema::new("other"));
    assert_eq!(a, b);
    assert_ne!(a, c);
    assert_ne!(a, Schema::Core);
    assert_eq!(b.allowed_scalar_types(), vec![ScalarType::Integer]);
    assert!(b.allows_scalar_type(ScalarType::Integer));
    assert!(!b.allows_scalar_type(ScalarType::String));
}

#[test]
fn custom_schema_validate_scalar() {
    let schema = CustomSchema::new("ints")
        .allow_type(ScalarType::Integer)
        .strict();
    assert_eq!(schema.validate_scalar(" 42 ", "p"), Ok(()));
    assert_eq!(
        schema.validate_scalar("true", "p"),
        Err(ValidationError::type_not_allowed(
            "p",
            "ints",
            ScalarType::Boolean,
            vec![ScalarType::Integer],
        ))
    );
}

#[test]
fn custom_schema_coercion() {
    let validator =
        SchemaValidator::custom(CustomSchema::new("bools").allow_type(ScalarType::Boolean));
    assert_eq!(
        validator.validate(&create_test_document("a: yes\nb: 0\n")),
        Ok(())
    );
    let document = create_test_document("a: hello\n");
    let expected = Err(vec![ValidationError::coercion_failed(
        "root.a",
        "bools",
        ScalarType::String,
        vec![ScalarType::Boolean],
    )]);
    assert_eq!(validator.validate(&document), expected);
    assert_eq!(validator.can_coerce(&document), expected);
}

#[test]
fn custom_schema_can_coerce_skips_validators() {
    let validator = SchemaValidator::custom(
        CustomSchema::new("never")
            .allow_type(ScalarType::String)
            .with_validator(ScalarType::String, |_, _| {
                CustomValidationResult::invalid("never", "always fails")
            })
            .strict(),
    );
    let document = create_test_document("a: 42\n");
    assert_eq!(
        validator.validate(&document),
        Err(vec![ValidationError::type_not_allowed(
            "root.a",
            "never",
            ScalarType::Integer,
            vec![ScalarType::String],
        )])
    );
    assert_eq!(validator.can_coerce(&document), Ok(()));
    let document = create_test_document("a: x\n");
    assert_eq!(
        validator.validate(&document),
        Err(vec![ValidationError::custom_constraint_failed(
            "root.a",
            "never",
            "never: always fails",
            "x",
        )])
    );
    assert_eq!(validator.can_coerce(&document), Ok(()));
}

#[test]
fn custom_schema_can_coerce_tagged() {
    let validator =
        SchemaValidator::custom(CustomSchema::new("strings").allow_type(ScalarType::String));
    let document = create_test_document("a: !!timestamp 2001-01-01\n");
    assert_eq!(
        validator.can_coerce(&document),
        Err(vec![ValidationError::type_not_allowed(
            "root.a",
            "strings",
            ScalarType::Timestamp,
            vec![ScalarType::String],
        )])
    );
}

#[test]
fn root_scalar_and_sequence_paths() {
    let failsafe = SchemaValidator::failsafe().strict();
    let int_error = |path: &str| {
        ValidationError::type_not_allowed(
            path,
            "failsafe",
            ScalarType::Integer,
            vec![ScalarType::String],
        )
    };
    assert_eq!(
        failsafe.validate(&create_test_document("5\n")),
        Err(vec![int_error("root")])
    );
    assert_eq!(
        failsafe.validate(&create_test_document("- x\n- 1\n- [2]\n")),
        Err(vec![int_error("root[1]"), int_error("root[2][0]")])
    );
    assert_eq!(
        failsafe.can_coerce(&create_test_document("- x\n- {a: 1}\n")),
        Err(vec![int_error("root[1].a")])
    );
}

#[test]
fn standard_schema_coerces_untagged_scalars() {
    let document = create_test_document("a: 1\nb: [true, ~]\nc: {d: 2001-01-01}\n");
    assert_eq!(SchemaValidator::failsafe().validate(&document), Ok(()));
    assert_eq!(SchemaValidator::failsafe().can_coerce(&document), Ok(()));
    let document = create_test_document("a: [!!timestamp 2001-01-01]\nb: {c: !!regex x}\n");
    assert_eq!(
        SchemaValidator::json().can_coerce(&document),
        Err(vec![
            ValidationError::type_not_allowed(
                "root.a[0]",
                "json",
                ScalarType::Timestamp,
                json_types(),
            ),
            ValidationError::type_not_allowed("root.b.c", "json", ScalarType::Regex, json_types(),),
        ])
    );
}

#[test]
#[cfg(feature = "base64")]
fn binary_tag() {
    let document = create_test_document("a: !!binary aGk=\n");
    assert_eq!(
        SchemaValidator::json().validate(&document),
        Err(vec![ValidationError::type_not_allowed(
            "root.a",
            "json",
            ScalarType::Binary,
            json_types(),
        )])
    );
    assert_eq!(SchemaValidator::core().validate(&document), Ok(()));
}

#[test]
fn custom_schema_tagged_scalar_has_tagged_type() {
    let validator = SchemaValidator::custom(
        CustomSchema::new("ints")
            .allow_type(ScalarType::Integer)
            .strict(),
    );
    assert_eq!(
        validator.validate(&create_test_document("k: !!int 5\n")),
        Ok(())
    );
    assert_eq!(
        validator.validate(&create_test_document("k: !!str 5\n")),
        Err(vec![ValidationError::type_not_allowed(
            "root.k",
            "ints",
            ScalarType::String,
            vec![ScalarType::Integer],
        )])
    );
}

#[test]
fn custom_validator_gets_tagged_scalar_content() {
    let validator = SchemaValidator::custom(
        CustomSchema::new("dates")
            .allow_type(ScalarType::Timestamp)
            .with_validator(ScalarType::Timestamp, |value, _| {
                if value == "2001-12-14" {
                    CustomValidationResult::Valid
                } else {
                    CustomValidationResult::invalid("date", value)
                }
            }),
    );
    let document = create_test_document("k: !!timestamp 2001-12-14\n");
    assert_eq!(validator.validate(&document), Ok(()));
}

#[test]
fn failsafe_strict_rejects_tagged_integer() {
    let document = create_test_document("k: !!int 123\n");
    assert_eq!(
        SchemaValidator::failsafe().strict().validate(&document),
        Err(vec![ValidationError::type_not_allowed(
            "root.k",
            "failsafe",
            ScalarType::Integer,
            vec![ScalarType::String],
        )])
    );
}

#[test]
fn tagged_collection_contents_are_validated() {
    let validator = SchemaValidator::failsafe().strict();
    let expected = |path: &str| {
        Err(vec![ValidationError::type_not_allowed(
            path,
            "failsafe",
            ScalarType::Integer,
            vec![ScalarType::String],
        )])
    };
    assert_eq!(
        validator.validate(&create_test_document("k: !!map {a: 1}\n")),
        expected("root.k.a")
    );
    assert_eq!(
        validator.validate(&create_test_document("k: !!seq [1]\n")),
        expected("root.k[0]")
    );
}

#[test]
fn json_validation_with_yaml_specific_types() {
    let yaml_str = r#"
timestamp: !!timestamp "2023-12-25T10:30:45Z"
pattern: !!regex '\d+'
"#;
    let document = create_test_document(yaml_str);
    let validator = SchemaValidator::json().strict();
    let json_types = vec![
        ScalarType::String,
        ScalarType::Integer,
        ScalarType::Float,
        ScalarType::Boolean,
        ScalarType::Null,
    ];
    assert_eq!(
        validator.validate(&document),
        Err(vec![
            ValidationError::type_not_allowed(
                "root.timestamp",
                "json",
                ScalarType::Timestamp,
                json_types.clone()
            ),
            ValidationError::type_not_allowed(
                "root.pattern",
                "json",
                ScalarType::Regex,
                json_types
            ),
        ])
    );
}

#[test]
fn strict_mode_validation() {
    let yaml_str = r#"
count: 42
active: true
"#;
    let document = create_test_document(yaml_str);

    // Non-strict failsafe passes via coercion, strict does not allow
    // integers and booleans.
    let validator = SchemaValidator::failsafe();
    assert!(validator.can_coerce(&document).is_ok());
    let strict_validator = SchemaValidator::failsafe().strict();
    assert_eq!(
        strict_validator.validate(&document),
        Err(vec![
            ValidationError::type_not_allowed(
                "root.count",
                "failsafe",
                ScalarType::Integer,
                vec![ScalarType::String]
            ),
            ValidationError::type_not_allowed(
                "root.active",
                "failsafe",
                ScalarType::Boolean,
                vec![ScalarType::String]
            ),
        ])
    );

    let string_yaml = r#"
name: hello
message: world
"#;
    let string_document = create_test_document(string_yaml);
    assert_eq!(validator.validate(&string_document), Ok(()));
    assert_eq!(strict_validator.validate(&string_document), Ok(()));
}
