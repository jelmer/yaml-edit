#![deny(missing_docs)]
#![allow(clippy::type_complexity)]
#![warn(clippy::unnecessary_to_owned)]
#![warn(clippy::redundant_clone)]
#![warn(clippy::inefficient_to_string)]
#![warn(clippy::manual_string_new)]
#![doc = include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/README.md"))]

//! A lossless YAML parser and editor.
//!
//! This library provides a lossless parser for YAML files, preserving
//! all whitespace, comments, and formatting. It is based on the [rowan] library.

mod error;
mod error_recovery;
mod lex;
mod parse;
mod scalar;
mod schema;
mod value;
mod yaml;

pub use error::{YamlError, YamlResult};
pub use lex::{
    lex_with_validation, lex_with_validation_config, ValidationConfig, WhitespaceError,
    WhitespaceErrorCategory,
};
pub use parse::Parse;
pub use rowan::TextRange;
pub use scalar::{ScalarStyle, ScalarType, ScalarValue};
pub use schema::{Schema, SchemaValidator, ValidationError, ValidationResult};
pub use value::YamlValue;
pub use yaml::{Directive, Document, Lang, Mapping, Scalar, Sequence, TaggedScalar, Yaml};

/// A positioned parse error containing location information.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PositionedParseError {
    /// The error message
    pub message: String,
    /// The text range where the error occurred
    pub range: rowan::TextRange,
    /// Optional error code for categorization
    pub code: Option<String>,
}

impl std::fmt::Display for PositionedParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for PositionedParseError {}

/// List of encountered syntax errors.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ParseError(pub Vec<String>);

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        for err in &self.0 {
            writeln!(f, "{}", err)?;
        }
        Ok(())
    }
}

impl std::error::Error for ParseError {}

/// Error parsing YAML files
#[derive(Debug)]
pub enum Error {
    /// A syntax error was encountered while parsing the file.
    ParseError(ParseError),

    /// An I/O error was encountered while reading the file.
    IoError(std::io::Error),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &self {
            Error::ParseError(err) => write!(f, "{}", err),
            Error::IoError(err) => write!(f, "{}", err),
        }
    }
}

impl From<ParseError> for Error {
    fn from(err: ParseError) -> Self {
        Self::ParseError(err)
    }
}

impl From<std::io::Error> for Error {
    fn from(err: std::io::Error) -> Self {
        Self::IoError(err)
    }
}

impl std::error::Error for Error {}

/// The indentation to use when writing a YAML file.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Indentation {
    /// Use the same indentation as the original line for the value.
    FieldNameLength,

    /// The number of spaces to use for indentation.
    Spaces(u32),
}

impl Default for Indentation {
    fn default() -> Self {
        Indentation::Spaces(2)
    }
}
