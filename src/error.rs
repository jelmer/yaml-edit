//! Error types for yaml-edit

use std::fmt;

/// Errors that can occur when working with YAML documents
#[derive(Debug)]
pub enum YamlError {
    /// I/O error when reading or writing files
    Io(std::io::Error),

    /// Parse error when parsing YAML
    Parse {
        /// Error message describing what went wrong
        message: String,
        /// Line number where the error occurred (if available)
        line: Option<usize>,
        /// Column number where the error occurred (if available)
        column: Option<usize>,
    },

    /// Key not found in mapping
    KeyNotFound {
        /// The key that was not found
        key: String,
        /// Available keys in the mapping (for helpful error messages)
        available_keys: Vec<String>,
        /// Path to the mapping where the key was searched
        path: String,
    },

    /// Type mismatch - expected one type but got another
    TypeMismatch {
        /// Expected type (e.g., "mapping", "sequence", "scalar")
        expected: String,
        /// Actual type found
        actual: String,
        /// Path to the value with the type mismatch
        path: String,
    },

    /// Invalid index for sequence access
    InvalidIndex {
        /// The index that was out of bounds
        index: usize,
        /// The actual length of the sequence
        length: usize,
        /// Path to the sequence
        path: String,
    },

    /// Invalid operation for the given context
    InvalidOperation {
        /// Description of what operation was attempted
        operation: String,
        /// Why it's invalid
        reason: String,
    },
}

impl fmt::Display for YamlError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            YamlError::Io(err) => write!(f, "I/O error: {}", err),

            YamlError::Parse {
                message,
                line,
                column,
            } => {
                write!(f, "Parse error")?;
                if let (Some(line), Some(column)) = (line, column) {
                    write!(f, " at line {}, column {}", line, column)?;
                } else if let Some(line) = line {
                    write!(f, " at line {}", line)?;
                }
                write!(f, ": {}", message)
            }

            YamlError::KeyNotFound {
                key,
                available_keys,
                path,
            } => {
                write!(f, "Key '{}' not found", key)?;
                if !path.is_empty() {
                    write!(f, " at path '{}'", path)?;
                }
                if !available_keys.is_empty() {
                    write!(f, ". Available keys: [{}]", available_keys.join(", "))?;
                }
                Ok(())
            }

            YamlError::TypeMismatch {
                expected,
                actual,
                path,
            } => {
                write!(
                    f,
                    "Type mismatch: expected '{}', but found '{}'",
                    expected, actual
                )?;
                if !path.is_empty() {
                    write!(f, " at path '{}'", path)?;
                }
                Ok(())
            }

            YamlError::InvalidIndex {
                index,
                length,
                path,
            } => {
                write!(
                    f,
                    "Index {} is out of bounds (sequence has {} elements)",
                    index, length
                )?;
                if !path.is_empty() {
                    write!(f, " at path '{}'", path)?;
                }
                Ok(())
            }

            YamlError::InvalidOperation { operation, reason } => {
                write!(f, "Invalid operation '{}': {}", operation, reason)
            }
        }
    }
}

impl std::error::Error for YamlError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            YamlError::Io(err) => Some(err),
            _ => None,
        }
    }
}

impl From<std::io::Error> for YamlError {
    fn from(err: std::io::Error) -> Self {
        YamlError::Io(err)
    }
}

/// Result type for yaml-edit operations
pub type YamlResult<T> = Result<T, YamlError>;
