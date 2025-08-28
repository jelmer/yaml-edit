//! Error recovery mechanisms for YAML parsing
//!
//! This module provides enhanced error handling with:
//! - Line and column information for errors
//! - Recovery strategies to continue parsing after errors
//! - Detailed error messages with context

use crate::{lex::SyntaxKind, PositionedParseError};
use rowan::{TextRange, TextSize};

/// Error recovery strategy for the parser
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RecoveryStrategy {
    /// Skip the current token and continue
    SkipToken,
    /// Skip until end of line
    SkipToEndOfLine,
    /// Skip until a safe synchronization point
    SyncToSafePoint,
    /// Insert a synthetic token to fix the parse
    InsertToken(SyntaxKind),
}

/// Context for error recovery during parsing
#[derive(Clone)]
pub struct ErrorRecoveryContext {
    /// The original text being parsed
    text: String,
    /// Current position in the text
    position: usize,
    /// Line number (1-based)
    line: usize,
    /// Column number (1-based)
    column: usize,
    /// Stack of recovery points for nested structures
    recovery_stack: Vec<RecoveryPoint>,
}

/// A recovery point in the parse
#[derive(Debug, Clone)]
struct RecoveryPoint {
    /// The context we're in
    context: ParseContext,
    /// Position where this context started
    start_position: usize,
    /// Line where this context started
    start_line: usize,
    /// Column where this context started
    start_column: usize,
}

/// The parsing context for error recovery
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParseContext {
    /// Top level document
    Document,
    /// Inside a mapping
    Mapping,
    /// Inside a sequence
    Sequence,
    /// Inside a flow sequence
    FlowSequence,
    /// Inside a flow mapping
    FlowMapping,
    /// Inside a block scalar
    BlockScalar,
    /// Inside a quoted string
    QuotedString,
}

impl ErrorRecoveryContext {
    /// Create a new error recovery context
    pub fn new(text: String) -> Self {
        Self {
            text,
            position: 0,
            line: 1,
            column: 1,
            recovery_stack: vec![RecoveryPoint {
                context: ParseContext::Document,
                start_position: 0,
                start_line: 1,
                start_column: 1,
            }],
        }
    }

    /// Update position and line/column tracking
    pub fn advance(&mut self, bytes: usize) {
        let end = (self.position + bytes).min(self.text.len());
        let advanced_text = &self.text[self.position..end];

        let mut chars = advanced_text.chars().peekable();
        while let Some(ch) = chars.next() {
            if ch == '\n' {
                self.line += 1;
                self.column = 1;
            } else if ch == '\r' {
                self.line += 1;
                self.column = 1;
                // Skip LF in CRLF sequence
                if chars.peek() == Some(&'\n') {
                    chars.next();
                }
            } else {
                self.column += 1;
            }
        }

        self.position = end;
    }

    /// Get current line and column
    pub fn current_location(&self) -> (usize, usize) {
        (self.line, self.column)
    }

    /// Get a text range for the current position
    pub fn current_range(&self, length: usize) -> TextRange {
        let start = TextSize::from(self.position as u32);
        let end = TextSize::from((self.position + length) as u32);
        TextRange::new(start, end)
    }

    /// Push a new parsing context
    pub fn push_context(&mut self, context: ParseContext) {
        self.recovery_stack.push(RecoveryPoint {
            context,
            start_position: self.position,
            start_line: self.line,
            start_column: self.column,
        });
    }

    /// Pop the current parsing context
    pub fn pop_context(&mut self) {
        if self.recovery_stack.len() > 1 {
            self.recovery_stack.pop();
        }
    }

    /// Get the current parsing context
    pub fn current_context(&self) -> ParseContext {
        self.recovery_stack
            .last()
            .map(|r| r.context)
            .unwrap_or(ParseContext::Document)
    }

    /// Create a positioned error with current location
    pub fn create_error(&self, message: String, length: usize) -> PositionedParseError {
        let (line, column) = self.current_location();
        let range = self.current_range(length);

        PositionedParseError {
            message: format!("{}:{}: {}", line, column, message),
            range,
            code: None,
        }
    }

    /// Determine the best recovery strategy for the current error
    pub fn suggest_recovery(
        &self,
        expected: SyntaxKind,
        found: Option<SyntaxKind>,
    ) -> RecoveryStrategy {
        match self.current_context() {
            ParseContext::FlowSequence => {
                // In flow sequence, recover to comma or closing bracket
                match expected {
                    SyntaxKind::RIGHT_BRACKET => {
                        // Missing closing bracket - insert it synthetically
                        RecoveryStrategy::InsertToken(SyntaxKind::RIGHT_BRACKET)
                    }
                    _ => match found {
                        Some(SyntaxKind::COMMA) | Some(SyntaxKind::RIGHT_BRACKET) => {
                            RecoveryStrategy::SkipToken
                        }
                        _ => {
                            // Don't use SkipUntil for brackets that might not exist
                            // Skip to next safe point instead
                            RecoveryStrategy::SkipToEndOfLine
                        }
                    },
                }
            }
            ParseContext::FlowMapping => {
                // In flow mapping, recover to comma or closing brace
                match expected {
                    SyntaxKind::RIGHT_BRACE => {
                        // Missing closing brace - insert it synthetically
                        RecoveryStrategy::InsertToken(SyntaxKind::RIGHT_BRACE)
                    }
                    _ => match found {
                        Some(SyntaxKind::COMMA) | Some(SyntaxKind::RIGHT_BRACE) => {
                            RecoveryStrategy::SkipToken
                        }
                        _ => {
                            // Don't use SkipUntil for braces that might not exist
                            // Skip to next safe point instead
                            RecoveryStrategy::SkipToEndOfLine
                        }
                    },
                }
            }
            ParseContext::Mapping => {
                // In mapping, skip to next key or end of mapping
                match expected {
                    SyntaxKind::COLON => {
                        // Missing colon after key, try to insert it
                        RecoveryStrategy::InsertToken(SyntaxKind::COLON)
                    }
                    _ => RecoveryStrategy::SkipToEndOfLine,
                }
            }
            ParseContext::Sequence => {
                // In sequence, skip to next item
                RecoveryStrategy::SkipToEndOfLine
            }
            ParseContext::QuotedString => {
                // In quoted string, look for closing quote
                match expected {
                    SyntaxKind::QUOTE | SyntaxKind::SINGLE_QUOTE => {
                        RecoveryStrategy::InsertToken(expected)
                    }
                    _ => RecoveryStrategy::SkipToken,
                }
            }
            ParseContext::BlockScalar => {
                // In block scalar, sync to dedent
                RecoveryStrategy::SyncToSafePoint
            }
            ParseContext::Document => {
                // At document level, skip to next document marker or directive
                RecoveryStrategy::SyncToSafePoint
            }
        }
    }

    /// Find the next safe synchronization point
    pub fn find_sync_point(&self, tokens: &[(SyntaxKind, String)], current: usize) -> usize {
        let sync_tokens = match self.current_context() {
            ParseContext::Document => vec![
                SyntaxKind::DOC_START,
                SyntaxKind::DOC_END,
                SyntaxKind::DIRECTIVE,
            ],
            ParseContext::Mapping | ParseContext::Sequence => {
                vec![SyntaxKind::DASH, SyntaxKind::NEWLINE]
            }
            ParseContext::FlowSequence => vec![SyntaxKind::RIGHT_BRACKET, SyntaxKind::COMMA],
            ParseContext::FlowMapping => vec![SyntaxKind::RIGHT_BRACE, SyntaxKind::COMMA],
            _ => vec![SyntaxKind::NEWLINE],
        };

        for i in current..tokens.len() {
            if sync_tokens.contains(&tokens[i].0) {
                return i;
            }
        }

        tokens.len()
    }

    /// Get context information for error messages
    pub fn get_context_snippet(&self, range: TextRange) -> String {
        let start = range.start().into();
        let end = range.end().into();

        // Find line boundaries
        let line_start = self.text[..start].rfind('\n').map(|i| i + 1).unwrap_or(0);

        let line_end = self.text[end..]
            .find('\n')
            .map(|i| end + i)
            .unwrap_or(self.text.len());

        let line = &self.text[line_start..line_end];
        let error_start = start - line_start;
        let error_len = (end - start).min(line_end - start);

        // Create error indicator
        let mut indicator = String::new();
        for _ in 0..error_start {
            indicator.push(' ');
        }
        for _ in 0..error_len.max(1) {
            indicator.push('^');
        }

        format!("{}\n{}", line, indicator)
    }
}

/// Enhanced error builder for creating detailed error messages
pub struct ErrorBuilder {
    message: String,
    expected: Vec<String>,
    found: Option<String>,
    context: Option<String>,
    suggestion: Option<String>,
}

impl ErrorBuilder {
    /// Create a new error builder
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            expected: Vec::new(),
            found: None,
            context: None,
            suggestion: None,
        }
    }

    /// Add what was expected
    pub fn expected(mut self, expected: impl Into<String>) -> Self {
        self.expected.push(expected.into());
        self
    }

    /// Add what was found instead
    pub fn found(mut self, found: impl Into<String>) -> Self {
        self.found = Some(found.into());
        self
    }

    /// Add context information
    pub fn context(mut self, context: impl Into<String>) -> Self {
        self.context = Some(context.into());
        self
    }

    /// Add a suggestion for fixing the error
    pub fn suggestion(mut self, suggestion: impl Into<String>) -> Self {
        self.suggestion = Some(suggestion.into());
        self
    }

    /// Build the final error message
    pub fn build(self) -> String {
        let mut parts = vec![self.message];

        if !self.expected.is_empty() {
            parts.push(format!("Expected: {}", self.expected.join(" or ")));
        }

        if let Some(found) = self.found {
            parts.push(format!("Found: {}", found));
        }

        if let Some(context) = self.context {
            parts.push(format!("Context: {}", context));
        }

        if let Some(suggestion) = self.suggestion {
            parts.push(format!("Suggestion: {}", suggestion));
        }

        parts.join(". ")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_recovery_context() {
        let mut ctx = ErrorRecoveryContext::new("foo: bar\nbaz: qux".to_string());

        assert_eq!(ctx.current_location(), (1, 1));

        ctx.advance(4); // "foo:"
        assert_eq!(ctx.current_location(), (1, 5));

        ctx.advance(5); // " bar\n"
        assert_eq!(ctx.current_location(), (2, 1));
    }

    #[test]
    fn test_error_builder() {
        let error = ErrorBuilder::new("Syntax error")
            .expected("colon")
            .found("newline")
            .context("in mapping")
            .suggestion("add ':' after key")
            .build();

        assert!(error.contains("Syntax error"));
        assert!(error.contains("Expected: colon"));
        assert!(error.contains("Found: newline"));
        assert!(error.contains("Context: in mapping"));
        assert!(error.contains("Suggestion: add ':' after key"));
    }

    #[test]
    fn test_context_snippet() {
        let ctx = ErrorRecoveryContext::new("foo: bar\nbaz qux\nend".to_string());
        let range = TextRange::new(TextSize::from(13), TextSize::from(16)); // "qux"

        let snippet = ctx.get_context_snippet(range);
        assert!(snippet.contains("baz qux"));
        assert!(snippet.contains("    ^^^"));
    }

    #[test]
    fn test_recovery_strategy() {
        let ctx = ErrorRecoveryContext::new("test".to_string());

        // Test flow sequence recovery
        let mut ctx_flow = ctx;
        ctx_flow.push_context(ParseContext::FlowSequence);
        let strategy = ctx_flow.suggest_recovery(SyntaxKind::COMMA, Some(SyntaxKind::COLON));
        assert_eq!(strategy, RecoveryStrategy::SkipToEndOfLine);

        // Test mapping colon recovery
        let mut ctx_map = ErrorRecoveryContext::new("test".to_string());
        ctx_map.push_context(ParseContext::Mapping);
        let strategy = ctx_map.suggest_recovery(SyntaxKind::COLON, Some(SyntaxKind::NEWLINE));
        assert_eq!(strategy, RecoveryStrategy::InsertToken(SyntaxKind::COLON));
    }
}
