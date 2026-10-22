//! YAML Schema validation support
//!
//! This module provides schema validation for YAML documents, supporting
//! the standard YAML schemas: Failsafe, JSON, and Core.

use crate::scalar::{ScalarType, ScalarValue};
use crate::yaml::{Document, TaggedNode};
use rowan::ast::AstNode;

/// Specific type of validation error that occurred
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ValidationErrorKind {
    /// A scalar type is not allowed in the current schema
    TypeNotAllowed {
        /// The scalar type that was found
        found_type: ScalarType,
        /// The types that are allowed in this schema
        allowed_types: Vec<ScalarType>,
    },
    /// Custom validation constraint failed
    CustomConstraintFailed {
        /// The constraint that failed
        constraint_name: String,
        /// The actual value that failed validation
        actual_value: String,
    },
    /// Type coercion failed
    CoercionFailed {
        /// The type that was found
        from_type: ScalarType,
        /// The types that coercion was attempted to
        to_types: Vec<ScalarType>,
    },
}

/// Error that occurs during schema validation
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ValidationError {
    /// The specific kind of validation error
    pub kind: ValidationErrorKind,
    /// Path to the node that failed validation (e.g., "root.items\[0\].name")
    pub path: String,
    /// Name of the schema that was being validated against
    pub schema_name: String,
}

impl ValidationError {
    /// Create a new type not allowed error
    pub fn type_not_allowed(
        path: impl Into<String>,
        schema_name: impl Into<String>,
        found_type: ScalarType,
        allowed_types: Vec<ScalarType>,
    ) -> Self {
        Self {
            kind: ValidationErrorKind::TypeNotAllowed {
                found_type,
                allowed_types,
            },
            path: path.into(),
            schema_name: schema_name.into(),
        }
    }

    /// Create a new custom constraint failed error
    pub fn custom_constraint_failed(
        path: impl Into<String>,
        schema_name: impl Into<String>,
        constraint_name: impl Into<String>,
        actual_value: impl Into<String>,
    ) -> Self {
        Self {
            kind: ValidationErrorKind::CustomConstraintFailed {
                constraint_name: constraint_name.into(),
                actual_value: actual_value.into(),
            },
            path: path.into(),
            schema_name: schema_name.into(),
        }
    }

    /// Create a new coercion failed error
    pub fn coercion_failed(
        path: impl Into<String>,
        schema_name: impl Into<String>,
        from_type: ScalarType,
        to_types: Vec<ScalarType>,
    ) -> Self {
        Self {
            kind: ValidationErrorKind::CoercionFailed {
                from_type,
                to_types,
            },
            path: path.into(),
            schema_name: schema_name.into(),
        }
    }

    /// Get a human-readable error message
    pub fn message(&self) -> String {
        match &self.kind {
            ValidationErrorKind::TypeNotAllowed {
                found_type,
                allowed_types,
            } => {
                format!(
                    "type {:?} not allowed in {} schema, expected one of {:?}",
                    found_type, self.schema_name, allowed_types
                )
            }
            ValidationErrorKind::CustomConstraintFailed {
                constraint_name,
                actual_value,
            } => {
                format!("custom constraint '{constraint_name}' failed for value '{actual_value}'")
            }
            ValidationErrorKind::CoercionFailed {
                from_type,
                to_types,
            } => {
                format!(
                    "cannot coerce {:?} to any of {:?} in {} schema",
                    from_type, to_types, self.schema_name
                )
            }
        }
    }
}

impl std::fmt::Display for ValidationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Validation error at {}: {}", self.path, self.message())
    }
}

impl std::error::Error for ValidationError {}

/// Result type for schema validation operations
pub type ValidationResult<T> = Result<T, Vec<ValidationError>>;

/// Result of a custom validation function
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CustomValidationResult {
    /// Validation passed
    Valid,
    /// Validation failed with a specific constraint name and reason
    Invalid {
        /// Name of the constraint that failed (e.g., "email_format", "port_range")
        constraint: String,
        /// Human-readable reason for failure
        reason: String,
    },
}

impl CustomValidationResult {
    /// Create a validation failure
    pub fn invalid(constraint: impl Into<String>, reason: impl Into<String>) -> Self {
        Self::Invalid {
            constraint: constraint.into(),
            reason: reason.into(),
        }
    }

    /// Check if validation passed
    pub fn is_valid(&self) -> bool {
        matches!(self, Self::Valid)
    }

    /// Check if validation failed
    pub fn is_invalid(&self) -> bool {
        matches!(self, Self::Invalid { .. })
    }
}

/// Custom validation function for scalar values
///
/// Takes (value, path) and returns CustomValidationResult
pub type CustomValidator = Box<dyn Fn(&str, &str) -> CustomValidationResult + Send + Sync>;

/// Custom schema definition with user-defined validation rules
pub struct CustomSchema {
    /// Schema name for error messages
    pub name: String,
    /// Allowed scalar types in this schema
    pub allowed_types: Vec<ScalarType>,
    /// Custom validation functions by type
    pub custom_validators: std::collections::HashMap<ScalarType, CustomValidator>,
    /// Whether to allow type coercion
    pub allow_coercion: bool,
}

impl std::fmt::Debug for CustomSchema {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CustomSchema")
            .field("name", &self.name)
            .field("allowed_types", &self.allowed_types)
            .field("allow_coercion", &self.allow_coercion)
            .field(
                "validators",
                &format!("<{} validators>", self.custom_validators.len()),
            )
            .finish()
    }
}

impl CustomSchema {
    /// Create a new custom schema
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            allowed_types: Vec::new(),
            custom_validators: std::collections::HashMap::new(),
            allow_coercion: true,
        }
    }

    /// Allow a specific scalar type
    pub fn allow_type(mut self, scalar_type: ScalarType) -> Self {
        if !self.allowed_types.contains(&scalar_type) {
            self.allowed_types.push(scalar_type);
        }
        self
    }

    /// Allow multiple scalar types
    pub fn allow_types(mut self, types: &[ScalarType]) -> Self {
        for &scalar_type in types {
            if !self.allowed_types.contains(&scalar_type) {
                self.allowed_types.push(scalar_type);
            }
        }
        self
    }

    /// Add a custom validator for a specific type
    pub fn with_validator<F>(mut self, scalar_type: ScalarType, validator: F) -> Self
    where
        F: Fn(&str, &str) -> CustomValidationResult + Send + Sync + 'static,
    {
        self.custom_validators
            .insert(scalar_type, Box::new(validator));
        self
    }

    /// Disable type coercion for strict validation
    pub fn strict(mut self) -> Self {
        self.allow_coercion = false;
        self
    }

    /// Check if a scalar type is allowed
    pub fn allows_type(&self, scalar_type: ScalarType) -> bool {
        self.allowed_types.contains(&scalar_type)
    }

    /// Validate a scalar value with custom rules
    pub fn validate_scalar(&self, content: &str, path: &str) -> Result<(), ValidationError> {
        self.validate_scalar_value(&ScalarValue::parse(content.trim()), path)
    }

    fn validate_scalar_value(
        &self,
        scalar_value: &ScalarValue,
        path: &str,
    ) -> Result<(), ValidationError> {
        let content = scalar_value.value();
        let scalar_type = scalar_value.scalar_type();

        check_type(
            scalar_value,
            &self.allowed_types,
            self.allow_coercion,
            path,
            &self.name,
        )?;

        if let Some(validator) = self.custom_validators.get(&scalar_type) {
            let result = validator(content, path);
            if let CustomValidationResult::Invalid { constraint, reason } = result {
                return Err(ValidationError::custom_constraint_failed(
                    path,
                    &self.name,
                    format!("{constraint}: {reason}"),
                    content,
                ));
            }
        }

        Ok(())
    }
}

/// Check that `value` has one of the `allowed` types or, if `coerce` is
/// set, can be coerced to one of them.
fn check_type(
    value: &ScalarValue,
    allowed: &[ScalarType],
    coerce: bool,
    path: &str,
    schema_name: &str,
) -> Result<(), ValidationError> {
    let scalar_type = value.scalar_type();
    if allowed.contains(&scalar_type) {
        return Ok(());
    }
    if !coerce {
        return Err(ValidationError::type_not_allowed(
            path,
            schema_name,
            scalar_type,
            allowed.to_vec(),
        ));
    }
    if allowed.iter().any(|&t| value.coerce_to_type(t).is_some()) {
        Ok(())
    } else {
        Err(ValidationError::coercion_failed(
            path,
            schema_name,
            scalar_type,
            allowed.to_vec(),
        ))
    }
}

/// YAML Schema types as defined in YAML 1.2 specification
#[derive(Debug)]
pub enum Schema {
    /// Failsafe schema - only strings, sequences, and mappings
    Failsafe,
    /// JSON schema - JSON-compatible types only
    Json,
    /// Core schema - full YAML 1.2 type system
    Core,
    /// User-defined custom schema
    Custom(CustomSchema),
}

impl PartialEq for Schema {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Schema::Failsafe, Schema::Failsafe)
            | (Schema::Json, Schema::Json)
            | (Schema::Core, Schema::Core) => true,
            (Schema::Custom(a), Schema::Custom(b)) => a.name == b.name,
            _ => false,
        }
    }
}

impl Schema {
    /// Get the name of this schema
    pub fn name(&self) -> &str {
        match self {
            Schema::Failsafe => "failsafe",
            Schema::Json => "json",
            Schema::Core => "core",
            Schema::Custom(custom) => &custom.name,
        }
    }

    /// Check if a scalar type is allowed in this schema
    pub fn allows_scalar_type(&self, scalar_type: ScalarType) -> bool {
        self.allowed_scalar_types().contains(&scalar_type)
    }

    /// Get the allowed scalar types for this schema
    pub fn allowed_scalar_types(&self) -> Vec<ScalarType> {
        match self {
            Schema::Failsafe => vec![ScalarType::String],
            Schema::Json => vec![
                ScalarType::String,
                ScalarType::Integer,
                ScalarType::Float,
                ScalarType::Boolean,
                ScalarType::Null,
            ],
            Schema::Core => vec![
                ScalarType::String,
                ScalarType::Integer,
                ScalarType::Float,
                ScalarType::Boolean,
                ScalarType::Null,
                #[cfg(feature = "base64")]
                ScalarType::Binary,
                ScalarType::Timestamp,
                ScalarType::Regex,
            ],
            Schema::Custom(custom) => custom.allowed_types.clone(),
        }
    }
}

/// Schema validator for YAML documents
#[derive(Debug)]
pub struct SchemaValidator {
    schema: Schema,
    strict: bool,
}

impl SchemaValidator {
    /// Create a new schema validator
    pub fn new(schema: Schema) -> Self {
        Self {
            schema,
            strict: false,
        }
    }

    /// Create a failsafe schema validator
    pub fn failsafe() -> Self {
        Self::new(Schema::Failsafe)
    }

    /// Create a JSON schema validator  
    pub fn json() -> Self {
        Self::new(Schema::Json)
    }

    /// Create a core schema validator
    pub fn core() -> Self {
        Self::new(Schema::Core)
    }

    /// Create a validator for a custom schema
    pub fn custom(schema: CustomSchema) -> Self {
        Self::new(Schema::Custom(schema))
    }

    /// Enable strict mode - disallow type coercion
    pub fn strict(mut self) -> Self {
        self.strict = true;
        self
    }

    /// Get the schema type
    pub fn schema(&self) -> &Schema {
        &self.schema
    }

    /// Validate a YAML document against the schema
    pub fn validate(&self, document: &Document) -> ValidationResult<()> {
        self.check_document(document, false)
    }

    /// Check if a document can be coerced to match the schema
    ///
    /// This does not run the validators of a custom schema.
    pub fn can_coerce(&self, document: &Document) -> ValidationResult<()> {
        self.check_document(document, !self.strict)
    }

    fn check_document(&self, document: &Document, coerce_only: bool) -> ValidationResult<()> {
        let mut errors = Vec::new();
        if let Some(root) = document.root_node() {
            self.check_node(&root, "root", coerce_only, &mut errors);
        }
        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    fn check_node(
        &self,
        node: &rowan::SyntaxNode<crate::yaml::Lang>,
        path: &str,
        coerce_only: bool,
        errors: &mut Vec<ValidationError>,
    ) {
        use crate::yaml::{extract, Mapping, Scalar, Sequence};

        if let Some(scalar) = extract::<Scalar>(node) {
            let value = ScalarValue::from_scalar(&scalar);
            self.check_scalar(&value, false, path, coerce_only, errors);
        } else if let Some(tagged_node) = extract::<TaggedNode>(node) {
            if let Some(content) = tagged_node
                .as_sequence()
                .map(|s| s.syntax().clone())
                .or_else(|| tagged_node.as_mapping().map(|m| m.syntax().clone()))
            {
                self.check_node(&content, path, coerce_only, errors);
            } else if let Some(value) = ScalarValue::from_tagged(&tagged_node) {
                // A core schema tag names the type even where the content is
                // not a valid spelling of it.
                let value = match tagged_node
                    .tag()
                    .as_deref()
                    .and_then(ScalarValue::core_tag_scalar_type)
                {
                    Some(scalar_type) => value.with_type(scalar_type),
                    None => value,
                };
                self.check_scalar(&value, true, path, coerce_only, errors);
            }
        } else if let Some(sequence) = extract::<Sequence>(node) {
            for (i, item) in sequence.items().enumerate() {
                self.check_node(&item, &format!("{path}[{i}]"), coerce_only, errors);
            }
        } else if let Some(mapping) = extract::<Mapping>(node) {
            // The schema applies to the values, not the keys
            for (key_node, value_node) in mapping.pairs() {
                let key_name = key_node.text().to_string().trim().to_string();
                self.check_node(
                    &value_node,
                    &format!("{path}.{key_name}"),
                    coerce_only,
                    errors,
                );
            }
        }
    }

    fn check_scalar(
        &self,
        value: &ScalarValue,
        tagged: bool,
        path: &str,
        coerce_only: bool,
        errors: &mut Vec<ValidationError>,
    ) {
        let result = match &self.schema {
            Schema::Custom(custom_schema) if !coerce_only => {
                custom_schema.validate_scalar_value(value, path)
            }
            // A tagged scalar has its type written out, so it is not coerced
            _ => check_type(
                value,
                &self.schema.allowed_scalar_types(),
                !tagged && !self.strict,
                path,
                self.schema.name(),
            ),
        };
        errors.extend(result.err());
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_validator_creation() {
        let failsafe = SchemaValidator::failsafe();
        assert_eq!(*failsafe.schema(), Schema::Failsafe);
        assert!(!failsafe.strict);

        let json = SchemaValidator::json();
        assert_eq!(*json.schema(), Schema::Json);

        let core = SchemaValidator::core();
        assert_eq!(*core.schema(), Schema::Core);

        let strict_validator = SchemaValidator::json().strict();
        assert!(strict_validator.strict);
    }
}
