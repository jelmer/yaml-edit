//! Error types for yaml-edit

use std::fmt;

/// Errors that can occur when working with YAML documents
#[derive(Debug)]
pub enum YamlError {
    /// I/O error when reading or writing files
    Io(std::io::Error),
    /// Parse error when parsing YAML
    Parse(String),
    /// Key not found error
    KeyNotFound(String),
    /// Type mismatch error (e.g., expected string but got array)
    TypeMismatch(String),
}

impl fmt::Display for YamlError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            YamlError::Io(err) => write!(f, "I/O error: {}", err),
            YamlError::Parse(msg) => write!(f, "Parse error: {}", msg),
            YamlError::KeyNotFound(key) => write!(f, "Key not found: '{}'", key),
            YamlError::TypeMismatch(msg) => write!(f, "Type mismatch: {}", msg),
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
