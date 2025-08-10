//! Lexer for YAML files.

/// Lexical analysis: the variants are different kinds of "tokens".
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u16)]
#[allow(non_camel_case_types)]
pub enum SyntaxKind {
    // Structural
    /// Root node of the syntax tree
    ROOT = 0,
    /// A YAML document
    DOCUMENT,
    /// A YAML sequence (list)
    SEQUENCE,
    /// A YAML mapping (key-value pairs)
    MAPPING,
    /// A YAML scalar value
    SCALAR,
    /// A YAML tagged scalar (tag + value)
    TAGGED_SCALAR,
    /// Parse error marker
    ERROR,

    // Tokens
    /// Dash character '-'
    DASH,
    /// Plus character '+'
    PLUS,
    /// Colon character ':'
    COLON,
    /// Question mark '?'
    QUESTION,
    /// Left bracket '['
    LEFT_BRACKET,
    /// Right bracket ']'
    RIGHT_BRACKET,
    /// Left brace '{'
    LEFT_BRACE,
    /// Right brace '}'
    RIGHT_BRACE,
    /// Comma ','
    COMMA,
    /// Pipe '|'
    PIPE,
    /// Greater than '>'
    GREATER,
    /// Ampersand '&'
    AMPERSAND,
    /// Asterisk '*'
    ASTERISK,
    /// Exclamation '!'
    EXCLAMATION,
    /// Percent '%'
    PERCENT,
    /// At symbol '@'
    AT,
    /// Backtick '`'
    BACKTICK,
    /// Double quote '"'
    QUOTE,
    /// Single quote "'"
    SINGLE_QUOTE,

    // Document markers
    /// Document start marker '---'
    DOC_START,
    /// Document end marker '...'
    DOC_END,

    // Parser-generated semantic nodes
    /// A mapping key (created by parser from context)
    KEY,
    /// A value in key-value pair (created by parser from context)
    VALUE,

    // Content tokens (from lexer)
    /// String literal (quoted or unquoted identifier)
    STRING,
    /// Integer literal
    INT,
    /// Float literal
    FLOAT,
    /// Boolean literal (true/false)
    BOOL,
    /// Null literal
    NULL,
    /// YAML tag like '!tag'
    TAG,
    /// YAML anchor like '&anchor'
    ANCHOR,
    /// YAML reference like '*reference'
    REFERENCE,
    /// YAML directive like '%YAML 1.2'
    DIRECTIVE,

    // Whitespace and formatting
    /// Spaces and tabs
    WHITESPACE,
    /// Newline characters
    NEWLINE,
    /// Leading whitespace that determines structure
    INDENT,
    /// Comments starting with '#'
    COMMENT,

    // Special
    /// End of file marker
    EOF,
}

impl From<SyntaxKind> for rowan::SyntaxKind {
    fn from(kind: SyntaxKind) -> Self {
        Self(kind as u16)
    }
}

/// Tokenize YAML input
pub fn lex(input: &str) -> Vec<(SyntaxKind, &str)> {
    use SyntaxKind::*;

    let mut tokens = Vec::new();
    let mut chars = input.char_indices().peekable();
    let bytes = input.as_bytes();

    while let Some((start_idx, ch)) = chars.next() {
        let token_start = start_idx;

        match ch {
            // Context-aware hyphen handling
            '-' => {
                if let Some((_, '-')) = chars.peek() {
                    chars.next(); // consume second -
                    if let Some((_, '-')) = chars.peek() {
                        chars.next(); // consume third -
                        tokens.push((DOC_START, &input[token_start..start_idx + 3]));
                    } else {
                        // Just two dashes, treat as sequence marker followed by dash
                        tokens.push((DASH, &input[token_start..start_idx + 1]));
                        tokens.push((DASH, &input[start_idx + 1..start_idx + 2]));
                    }
                } else {
                    // Check if this hyphen should be treated as a sequence marker
                    // It's a sequence marker if:
                    // 1. It's at the beginning of a line (after optional indentation)
                    // 2. It's followed by whitespace or end of input
                    let is_sequence_marker = {
                        // Check if preceded only by whitespace from start of line
                        let line_start_pos = input[..token_start]
                            .rfind('\n')
                            .map(|pos| pos + 1)
                            .unwrap_or(0);
                        let before_dash = &input[line_start_pos..token_start];
                        let only_whitespace_before =
                            before_dash.chars().all(|c| c == ' ' || c == '\t');

                        // Check if followed by whitespace or end of input
                        let followed_by_whitespace_or_end = chars
                            .peek()
                            .map_or(true, |(_, next_ch)| next_ch.is_whitespace());

                        only_whitespace_before && followed_by_whitespace_or_end
                    };

                    if is_sequence_marker {
                        tokens.push((DASH, &input[token_start..start_idx + 1]));
                    } else {
                        // This hyphen is part of a scalar value, treat as regular character
                        // Continue reading until we hit whitespace or actual YAML special chars
                        let mut end_idx = start_idx + 1;
                        while let Some((idx, ch)) = chars.peek() {
                            if ch.is_whitespace() || is_yaml_special_excluding_hyphen(*ch) {
                                break;
                            }
                            end_idx = *idx + ch.len_utf8();
                            chars.next();
                        }
                        let text = &input[token_start..end_idx];
                        let token_kind = classify_scalar(text);
                        tokens.push((token_kind, text));
                    }
                }
            }
            '+' => tokens.push((PLUS, &input[token_start..start_idx + 1])),
            ':' => tokens.push((COLON, &input[token_start..start_idx + 1])),
            '?' => tokens.push((QUESTION, &input[token_start..start_idx + 1])),
            '[' => tokens.push((LEFT_BRACKET, &input[token_start..start_idx + 1])),
            ']' => tokens.push((RIGHT_BRACKET, &input[token_start..start_idx + 1])),
            '{' => tokens.push((LEFT_BRACE, &input[token_start..start_idx + 1])),
            '}' => tokens.push((RIGHT_BRACE, &input[token_start..start_idx + 1])),
            ',' => tokens.push((COMMA, &input[token_start..start_idx + 1])),
            '|' => tokens.push((PIPE, &input[token_start..start_idx + 1])),
            '>' => tokens.push((GREATER, &input[token_start..start_idx + 1])),
            '&' => {
                // Check if this is an anchor definition
                let mut end_idx = start_idx + 1;
                let mut has_anchor_name = false;
                while let Some((idx, ch)) = chars.peek() {
                    if ch.is_whitespace() || is_yaml_special(*ch) {
                        break;
                    }
                    has_anchor_name = true;
                    end_idx = *idx + ch.len_utf8();
                    chars.next();
                }

                if has_anchor_name {
                    tokens.push((ANCHOR, &input[token_start..end_idx]));
                } else {
                    tokens.push((AMPERSAND, &input[token_start..start_idx + 1]));
                }
            }
            '*' => {
                // Check if this is an alias reference
                let mut end_idx = start_idx + 1;
                let mut has_reference_name = false;
                while let Some((idx, ch)) = chars.peek() {
                    if ch.is_whitespace() || is_yaml_special(*ch) {
                        break;
                    }
                    has_reference_name = true;
                    end_idx = *idx + ch.len_utf8();
                    chars.next();
                }

                if has_reference_name {
                    tokens.push((REFERENCE, &input[token_start..end_idx]));
                } else {
                    tokens.push((ASTERISK, &input[token_start..start_idx + 1]));
                }
            }
            '"' => tokens.push((QUOTE, &input[token_start..start_idx + 1])),
            '\'' => tokens.push((SINGLE_QUOTE, &input[token_start..start_idx + 1])),

            // Document end
            '.' => {
                if let Some((_, '.')) = chars.peek() {
                    chars.next(); // consume second .
                    if let Some((_, '.')) = chars.peek() {
                        chars.next(); // consume third .
                        tokens.push((DOC_END, &input[token_start..start_idx + 3]));
                    } else {
                        // Just dots, treat as string
                        let mut end_idx = start_idx + 1;
                        while let Some((idx, _)) = chars.peek() {
                            if bytes[*idx] == b'.' {
                                end_idx = *idx + 1;
                                chars.next();
                            } else {
                                break;
                            }
                        }
                        let text = &input[token_start..end_idx];
                        let token_kind = classify_scalar(text);
                        tokens.push((token_kind, text));
                    }
                } else {
                    // Single dot - part of scalar
                    let mut end_idx = start_idx + 1;
                    while let Some((idx, ch)) = chars.peek() {
                        if ch.is_whitespace() || is_yaml_special(*ch) {
                            break;
                        }
                        end_idx = *idx + ch.len_utf8();
                        chars.next();
                    }
                    let text = &input[token_start..end_idx];
                    let token_kind = classify_scalar(text);
                    tokens.push((token_kind, text));
                }
            }

            // Comments
            '#' => {
                let mut end_idx = start_idx + 1;
                while let Some((idx, ch)) = chars.peek() {
                    if *ch == '\n' || *ch == '\r' {
                        break;
                    }
                    end_idx = *idx + ch.len_utf8();
                    chars.next();
                }
                tokens.push((COMMENT, &input[token_start..end_idx]));
            }

            // Tags and directives
            '!' => {
                let mut end_idx = start_idx + 1;
                while let Some((idx, ch)) = chars.peek() {
                    if ch.is_whitespace() || is_yaml_special(*ch) {
                        break;
                    }
                    end_idx = *idx + ch.len_utf8();
                    chars.next();
                }
                tokens.push((TAG, &input[token_start..end_idx]));
            }

            '%' => {
                let mut end_idx = start_idx + 1;
                while let Some((idx, ch)) = chars.peek() {
                    if *ch == '\n' || *ch == '\r' {
                        break;
                    }
                    end_idx = *idx + ch.len_utf8();
                    chars.next();
                }
                tokens.push((DIRECTIVE, &input[token_start..end_idx]));
            }

            // Newlines
            '\n' => tokens.push((NEWLINE, &input[token_start..start_idx + 1])),
            '\r' => {
                if let Some((_, '\n')) = chars.peek() {
                    chars.next();
                    tokens.push((NEWLINE, &input[token_start..start_idx + 2]));
                } else {
                    tokens.push((NEWLINE, &input[token_start..start_idx + 1]));
                }
            }

            // Whitespace (spaces and tabs)
            ' ' | '\t' => {
                let mut end_idx = start_idx + 1;
                while let Some((idx, ch)) = chars.peek() {
                    if *ch != ' ' && *ch != '\t' {
                        break;
                    }
                    end_idx = *idx + 1;
                    chars.next();
                }

                // Determine if this is structural indentation
                if token_start == 0 || (token_start > 0 && bytes[token_start - 1] == b'\n') {
                    tokens.push((INDENT, &input[token_start..end_idx]));
                } else {
                    tokens.push((WHITESPACE, &input[token_start..end_idx]));
                }
            }

            // Everything else is scalar content
            _ => {
                let mut end_idx = start_idx + ch.len_utf8();
                while let Some((idx, ch)) = chars.peek() {
                    if ch.is_whitespace() || is_yaml_special_excluding_hyphen(*ch) {
                        break;
                    }
                    // Special handling for hyphens - only break if it's a sequence marker
                    if *ch == '-' {
                        // Check if this hyphen is a sequence marker
                        // For scalars, we're more permissive - only break if it's clearly a sequence marker
                        let hyphen_pos = *idx;

                        // Check if preceded only by whitespace from start of line
                        let line_start_pos = input[..hyphen_pos]
                            .rfind('\n')
                            .map(|pos| pos + 1)
                            .unwrap_or(0);
                        let before_dash = &input[line_start_pos..hyphen_pos];
                        let only_whitespace_before =
                            before_dash.chars().all(|c| c == ' ' || c == '\t');

                        // Since we can't easily peek ahead without borrowing issues,
                        // and we're inside scalar parsing, be conservative:
                        // Only treat as sequence marker if it's at start of line
                        // (a hyphen in the middle of a scalar is very likely part of the scalar)
                        if only_whitespace_before && hyphen_pos == end_idx {
                            // This is the first character we're looking at and it's at line start
                            // This is likely a sequence marker
                            break;
                        }
                        // Otherwise, treat hyphen as part of the scalar
                    }

                    end_idx = *idx + ch.len_utf8();
                    chars.next();
                }
                let text = &input[token_start..end_idx];
                let token_kind = classify_scalar(text);
                tokens.push((token_kind, text));
            }
        }
    }

    tokens
}

/// Classify a scalar token based on its content
fn classify_scalar(text: &str) -> SyntaxKind {
    use SyntaxKind::*;

    // Boolean literals
    match text {
        "true" | "false" | "True" | "False" | "TRUE" | "FALSE" => return BOOL,
        "null" | "Null" | "NULL" | "~" => return NULL,
        _ => {}
    }

    // Try to parse as integer
    if text.parse::<i64>().is_ok() {
        return INT;
    }

    // Try to parse as float
    if text.parse::<f64>().is_ok() {
        return FLOAT;
    }

    // Everything else is a string
    STRING
}

/// Check if a character has special meaning in YAML
fn is_yaml_special(ch: char) -> bool {
    matches!(
        ch,
        ':' | '-'
            | '+'
            | '?'
            | '['
            | ']'
            | '{'
            | '}'
            | ','
            | '|'
            | '>'
            | '&'
            | '*'
            | '!'
            | '%'
            | '@'
            | '`'
            | '"'
            | '\''
            | '#'
    )
}

/// Check if character is YAML special, excluding hyphen (for context-aware hyphen parsing)
fn is_yaml_special_excluding_hyphen(ch: char) -> bool {
    matches!(
        ch,
        ':' | '+'
            | '?'
            | '['
            | ']'
            | '{'
            | '}'
            | ','
            | '|'
            | '>'
            | '&'
            | '*'
            | '!'
            | '%'
            | '@'
            | '`'
            | '"'
            | '\''
            | '#'
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_mapping() {
        let input = "key: value";
        let tokens = lex(input);

        assert_eq!(tokens.len(), 4);
        assert_eq!(tokens[0], (SyntaxKind::STRING, "key"));
        assert_eq!(tokens[1], (SyntaxKind::COLON, ":"));
        assert_eq!(tokens[2], (SyntaxKind::WHITESPACE, " "));
        assert_eq!(tokens[3], (SyntaxKind::STRING, "value"));
    }

    #[test]
    fn test_scalar_types() {
        // Test integer
        let tokens = lex("age: 42");
        assert_eq!(tokens[0], (SyntaxKind::STRING, "age"));
        assert_eq!(tokens[3], (SyntaxKind::INT, "42"));

        // Test float
        let tokens = lex("pi: 3.14");
        assert_eq!(tokens[0], (SyntaxKind::STRING, "pi"));
        assert_eq!(tokens[3], (SyntaxKind::FLOAT, "3.14"));

        // Test boolean true
        let tokens = lex("enabled: true");
        assert_eq!(tokens[0], (SyntaxKind::STRING, "enabled"));
        assert_eq!(tokens[3], (SyntaxKind::BOOL, "true"));

        // Test boolean false
        let tokens = lex("disabled: false");
        assert_eq!(tokens[3], (SyntaxKind::BOOL, "false"));

        // Test null
        let tokens = lex("value: null");
        assert_eq!(tokens[3], (SyntaxKind::NULL, "null"));

        // Test tilde as null
        let tokens = lex("value: ~");
        assert_eq!(tokens[3], (SyntaxKind::NULL, "~"));
    }

    #[test]
    fn test_sequences() {
        let input = "- item1\n- item2";
        let tokens = lex(input);

        assert_eq!(tokens[0], (SyntaxKind::DASH, "-"));
        assert_eq!(tokens[1], (SyntaxKind::WHITESPACE, " "));
        assert_eq!(tokens[2], (SyntaxKind::STRING, "item1"));
        assert_eq!(tokens[3], (SyntaxKind::NEWLINE, "\n"));
        assert_eq!(tokens[4], (SyntaxKind::DASH, "-"));
        assert_eq!(tokens[5], (SyntaxKind::WHITESPACE, " "));
        assert_eq!(tokens[6], (SyntaxKind::STRING, "item2"));
    }

    #[test]
    fn test_hyphen_in_scalars() {
        // Test hyphens in scalar values should not be treated as sequence markers
        let input = "Name: example-project";
        let tokens = lex(input);

        println!("Hyphen test tokens:");
        for (i, (kind, text)) in tokens.iter().enumerate() {
            println!("  {}: {:?} = {:?}", i, kind, text);
        }

        // Should get: STRING("Name"), COLON(":"), WHITESPACE(" "), STRING("example-project")
        assert_eq!(tokens.len(), 4);
        assert_eq!(tokens[0], (SyntaxKind::STRING, "Name"));
        assert_eq!(tokens[1], (SyntaxKind::COLON, ":"));
        assert_eq!(tokens[2], (SyntaxKind::WHITESPACE, " "));
        assert_eq!(tokens[3], (SyntaxKind::STRING, "example-project"));
    }

    #[test]
    fn test_hyphen_sequence_vs_scalar() {
        // Test that sequence markers are still recognized correctly
        let sequence_input = "- example-item";
        let tokens = lex(sequence_input);

        println!("Sequence hyphen tokens:");
        for (i, (kind, text)) in tokens.iter().enumerate() {
            println!("  {}: {:?} = {:?}", i, kind, text);
        }

        // Should get: DASH("-"), WHITESPACE(" "), STRING("example-item")
        assert_eq!(tokens[0], (SyntaxKind::DASH, "-"));
        assert_eq!(tokens[1], (SyntaxKind::WHITESPACE, " "));
        assert_eq!(tokens[2], (SyntaxKind::STRING, "example-item"));

        // Test scalar with hyphens in different contexts
        let scalar_input = "package-name: my-awesome-package";
        let tokens = lex(scalar_input);

        println!("Package hyphen tokens:");
        for (i, (kind, text)) in tokens.iter().enumerate() {
            println!("  {}: {:?} = {:?}", i, kind, text);
        }

        // Should get: STRING("package-name"), COLON(":"), WHITESPACE(" "), STRING("my-awesome-package")
        assert_eq!(tokens.len(), 4);
        assert_eq!(tokens[0], (SyntaxKind::STRING, "package-name"));
        assert_eq!(tokens[3], (SyntaxKind::STRING, "my-awesome-package"));
    }

    #[test]
    fn test_flow_style() {
        // Flow sequence
        let tokens = lex("[1, 2, 3]");
        assert_eq!(tokens[0], (SyntaxKind::LEFT_BRACKET, "["));
        assert_eq!(tokens[1], (SyntaxKind::INT, "1"));
        assert_eq!(tokens[2], (SyntaxKind::COMMA, ","));
        assert_eq!(tokens[3], (SyntaxKind::WHITESPACE, " "));
        assert_eq!(tokens[4], (SyntaxKind::INT, "2"));
        assert_eq!(tokens[5], (SyntaxKind::COMMA, ","));
        assert_eq!(tokens[6], (SyntaxKind::WHITESPACE, " "));
        assert_eq!(tokens[7], (SyntaxKind::INT, "3"));
        assert_eq!(tokens[8], (SyntaxKind::RIGHT_BRACKET, "]"));

        // Flow mapping
        let tokens = lex("{a: 1, b: 2}");
        assert_eq!(tokens[0], (SyntaxKind::LEFT_BRACE, "{"));
        assert_eq!(tokens[1], (SyntaxKind::STRING, "a"));
        assert_eq!(tokens[2], (SyntaxKind::COLON, ":"));
        assert_eq!(tokens[3], (SyntaxKind::WHITESPACE, " "));
        assert_eq!(tokens[4], (SyntaxKind::INT, "1"));
    }

    #[test]
    fn test_comments() {
        let input = "key: value # this is a comment\n# full line comment";
        let tokens = lex(input);

        // Find comment tokens
        let comments: Vec<_> = tokens
            .iter()
            .filter(|(kind, _)| *kind == SyntaxKind::COMMENT)
            .collect();

        assert_eq!(comments.len(), 2);
        assert_eq!(comments[0].1, "# this is a comment");
        assert_eq!(comments[1].1, "# full line comment");
    }

    #[test]
    fn test_multiline_scalar() {
        let input = "key: value\n  continued";
        let tokens = lex(input);

        // Check for indent token
        let indents: Vec<_> = tokens
            .iter()
            .filter(|(kind, _)| *kind == SyntaxKind::INDENT)
            .collect();
        assert_eq!(indents.len(), 1);
        assert_eq!(indents[0].1, "  ");
    }

    #[test]
    fn test_quoted_strings() {
        let input = r#"single: 'quoted'\ndouble: "quoted""#;
        let tokens = lex(input);

        // Find quote tokens
        let single_quotes: Vec<_> = tokens
            .iter()
            .filter(|(kind, _)| *kind == SyntaxKind::SINGLE_QUOTE)
            .collect();
        assert_eq!(single_quotes.len(), 2); // opening and closing

        let double_quotes: Vec<_> = tokens
            .iter()
            .filter(|(kind, _)| *kind == SyntaxKind::QUOTE)
            .collect();
        assert_eq!(double_quotes.len(), 2); // opening and closing
    }

    #[test]
    fn test_document_markers() {
        let input = "---\nkey: value\n...";
        let tokens = lex(input);

        println!("Document tokens:");
        for (i, (kind, text)) in tokens.iter().enumerate() {
            println!("  {}: {:?} = {:?}", i, kind, text);
        }

        // Check for document start marker
        assert!(tokens
            .iter()
            .any(|(kind, _)| *kind == SyntaxKind::DOC_START));
        assert!(tokens.iter().any(|(kind, _)| *kind == SyntaxKind::DOC_END));
    }

    #[test]
    fn test_empty_input() {
        let input = "";
        let tokens = lex(input);
        println!("Empty input tokens: {:?}", tokens);
        assert_eq!(tokens.len(), 0);
    }

    #[test]
    fn test_anchors_and_aliases() {
        // Test anchor definition
        let input = "key: &anchor_name value";
        let tokens = lex(input);
        println!("Anchor tokens: {:?}", tokens);

        let anchors: Vec<_> = tokens
            .iter()
            .filter(|(kind, _)| *kind == SyntaxKind::ANCHOR)
            .collect();
        assert_eq!(anchors.len(), 1);
        assert_eq!(anchors[0].1, "&anchor_name");

        // Test alias reference
        let input = "key: *reference_name";
        let tokens = lex(input);
        println!("Reference tokens: {:?}", tokens);

        let references: Vec<_> = tokens
            .iter()
            .filter(|(kind, _)| *kind == SyntaxKind::REFERENCE)
            .collect();
        assert_eq!(references.len(), 1);
        assert_eq!(references[0].1, "*reference_name");

        // Test bare ampersand and asterisk (should not be treated as anchors/references)
        let input = "key: & *";
        let tokens = lex(input);

        let ampersands: Vec<_> = tokens
            .iter()
            .filter(|(kind, _)| *kind == SyntaxKind::AMPERSAND)
            .collect();
        assert_eq!(ampersands.len(), 1);

        let asterisks: Vec<_> = tokens
            .iter()
            .filter(|(kind, _)| *kind == SyntaxKind::ASTERISK)
            .collect();
        assert_eq!(asterisks.len(), 1);
    }

    #[test]
    fn test_plus_token() {
        // Test plus as standalone token
        let input = "key: |+ value";
        let tokens = lex(input);

        let plus_tokens: Vec<_> = tokens
            .iter()
            .filter(|(kind, _)| *kind == SyntaxKind::PLUS)
            .collect();
        assert_eq!(plus_tokens.len(), 1);
        assert_eq!(plus_tokens[0].1, "+");
    }

    #[test]
    fn test_block_scalar_indicators() {
        // Test literal with chomping indicators
        let input1 = "key: |+ content";
        let tokens1 = lex(input1);

        assert!(tokens1
            .iter()
            .any(|(kind, text)| *kind == SyntaxKind::PIPE && *text == "|"));
        assert!(tokens1
            .iter()
            .any(|(kind, text)| *kind == SyntaxKind::PLUS && *text == "+"));

        // Test folded with chomping indicators
        let input2 = "key: >- content";
        let tokens2 = lex(input2);

        assert!(tokens2
            .iter()
            .any(|(kind, text)| *kind == SyntaxKind::GREATER && *text == ">"));
        assert!(tokens2
            .iter()
            .any(|(kind, text)| *kind == SyntaxKind::DASH && *text == "-"));

        // Test with explicit indentation
        let input3 = "key: |2+ content";
        let tokens3 = lex(input3);

        assert!(tokens3
            .iter()
            .any(|(kind, text)| *kind == SyntaxKind::PIPE && *text == "|"));
        assert!(tokens3
            .iter()
            .any(|(kind, text)| *kind == SyntaxKind::INT && *text == "2"));
        assert!(tokens3
            .iter()
            .any(|(kind, text)| *kind == SyntaxKind::PLUS && *text == "+"));
    }

    #[test]
    fn test_special_characters_in_block_content() {
        let input = "line with - and + and : characters";
        let tokens = lex(input);

        // Should tokenize each special character separately
        assert!(tokens
            .iter()
            .any(|(kind, text)| *kind == SyntaxKind::DASH && *text == "-"));
        assert!(tokens
            .iter()
            .any(|(kind, text)| *kind == SyntaxKind::PLUS && *text == "+"));
        assert!(tokens
            .iter()
            .any(|(kind, text)| *kind == SyntaxKind::COLON && *text == ":"));

        // Should also have the word tokens
        assert!(tokens
            .iter()
            .any(|(kind, text)| *kind == SyntaxKind::STRING && *text == "line"));
        assert!(tokens
            .iter()
            .any(|(kind, text)| *kind == SyntaxKind::STRING && *text == "with"));
        assert!(tokens
            .iter()
            .any(|(kind, text)| *kind == SyntaxKind::STRING && *text == "and"));
        assert!(tokens
            .iter()
            .any(|(kind, text)| *kind == SyntaxKind::STRING && *text == "characters"));
    }

    #[test]
    fn test_comprehensive_token_recognition() {
        let input = "key: |2+ \n  content with - and : and > chars\n  more content";
        let tokens = lex(input);

        // Print tokens for debugging
        println!("Comprehensive tokens:");
        for (i, (kind, text)) in tokens.iter().enumerate() {
            println!("  {}: {:?} = {:?}", i, kind, text);
        }

        // Verify all expected tokens are present
        assert!(tokens.iter().any(|(kind, _)| *kind == SyntaxKind::STRING)); // "key"
        assert!(tokens.iter().any(|(kind, _)| *kind == SyntaxKind::COLON)); // ":"
        assert!(tokens.iter().any(|(kind, _)| *kind == SyntaxKind::PIPE)); // "|"
        assert!(tokens.iter().any(|(kind, _)| *kind == SyntaxKind::INT)); // "2"
        assert!(tokens.iter().any(|(kind, _)| *kind == SyntaxKind::PLUS)); // "+"
        assert!(tokens.iter().any(|(kind, _)| *kind == SyntaxKind::NEWLINE)); // "\n"
        assert!(tokens.iter().any(|(kind, _)| *kind == SyntaxKind::INDENT)); // "  "
        assert!(tokens.iter().any(|(kind, _)| *kind == SyntaxKind::DASH)); // "-"
        assert!(tokens.iter().any(|(kind, _)| *kind == SyntaxKind::GREATER)); // ">"
    }
}
