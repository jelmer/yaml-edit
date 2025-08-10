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
    /// YAML merge key '<<'
    MERGE_KEY,
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

/// Helper to read a scalar value starting from current position
fn read_scalar_from<'a>(
    chars: &mut std::iter::Peekable<std::str::CharIndices<'a>>,
    input: &'a str,
    start_idx: usize,
    special_check: fn(char) -> bool,
) -> &'a str {
    let mut end_idx = start_idx;
    while let Some((idx, ch)) = chars.peek() {
        if ch.is_whitespace() || special_check(*ch) {
            break;
        }
        end_idx = *idx + ch.len_utf8();
        chars.next();
    }
    &input[start_idx..end_idx]
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

                    // Check if preceded only by whitespace from start of line
                    let line_start_pos = input[..token_start]
                        .rfind('\n')
                        .map(|pos| pos + 1)
                        .unwrap_or(0);
                    let before_dash = &input[line_start_pos..token_start];
                    let only_whitespace_before = before_dash.chars().all(|c| c == ' ' || c == '\t');

                    // Check if followed by whitespace or end of input
                    let followed_by_whitespace_or_end = chars
                        .peek()
                        .map_or(true, |(_, next_ch)| next_ch.is_whitespace());

                    let is_sequence_marker =
                        only_whitespace_before && followed_by_whitespace_or_end;

                    if is_sequence_marker {
                        tokens.push((DASH, &input[token_start..start_idx + 1]));
                    } else {
                        // This hyphen is part of a scalar value
                        let text = read_scalar_from(
                            &mut chars,
                            input,
                            start_idx + 1,
                            is_yaml_special_excluding_hyphen,
                        );
                        let full_text = &input[token_start..token_start + 1 + text.len()];
                        let token_kind = classify_scalar(full_text);
                        tokens.push((token_kind, full_text));
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
            '<' => {
                // Check if this is a merge key '<<'
                if let Some((_, '<')) = chars.peek() {
                    chars.next(); // consume second <
                    tokens.push((MERGE_KEY, &input[token_start..start_idx + 2]));
                } else {
                    // Single '<' is not a special YAML character, treat as scalar
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
            '&' => {
                // Check if this is an anchor definition
                let name = read_scalar_from(&mut chars, input, start_idx + 1, is_yaml_special);
                if !name.is_empty() {
                    tokens.push((ANCHOR, &input[token_start..start_idx + 1 + name.len()]));
                } else {
                    tokens.push((AMPERSAND, &input[token_start..start_idx + 1]));
                }
            }
            '*' => {
                // Check if this is an alias reference
                let name = read_scalar_from(&mut chars, input, start_idx + 1, is_yaml_special);
                if !name.is_empty() {
                    tokens.push((REFERENCE, &input[token_start..start_idx + 1 + name.len()]));
                } else {
                    tokens.push((ASTERISK, &input[token_start..start_idx + 1]));
                }
            }
            '"' => tokens.push((QUOTE, &input[token_start..start_idx + 1])),
            '\'' => tokens.push((SINGLE_QUOTE, &input[token_start..start_idx + 1])),

            // Document end
            '.' => {
                // Check for three dots (document end marker)
                if chars.peek() == Some(&(start_idx + 1, '.')) {
                    chars.next(); // consume second .
                    if chars.peek() == Some(&(start_idx + 2, '.')) {
                        chars.next(); // consume third .
                        tokens.push((DOC_END, &input[token_start..start_idx + 3]));
                    } else {
                        // Two dots - continue as scalar
                        let rest =
                            read_scalar_from(&mut chars, input, start_idx + 2, is_yaml_special);
                        let text = &input[token_start..start_idx + 2 + rest.len()];
                        let token_kind = classify_scalar(text);
                        tokens.push((token_kind, text));
                    }
                } else {
                    // Single dot - part of scalar
                    let rest = read_scalar_from(&mut chars, input, start_idx + 1, is_yaml_special);
                    let text = &input[token_start..start_idx + 1 + rest.len()];
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

            // Tags
            '!' => {
                let rest = read_scalar_from(&mut chars, input, start_idx + 1, is_yaml_special);
                tokens.push((TAG, &input[token_start..start_idx + 1 + rest.len()]));
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

                // Read the rest of the scalar, including embedded hyphens
                while let Some((idx, next_ch)) = chars.peek() {
                    if next_ch.is_whitespace() || is_yaml_special_excluding_hyphen(*next_ch) {
                        break;
                    }

                    // Special case: check if hyphen is a sequence marker
                    if *next_ch == '-' {
                        // A hyphen is only a sequence marker if it's at line start
                        // and this scalar is already complete (we're at a word boundary)
                        let line_start = input[..(*idx)].rfind('\n').map(|p| p + 1).unwrap_or(0);
                        let before_hyphen = &input[line_start..*idx];

                        // If there's only whitespace before the hyphen, it might be a sequence marker
                        // Break here to let the main loop handle it
                        if before_hyphen.chars().all(|c| c == ' ' || c == '\t') && *idx == end_idx {
                            break;
                        }
                    }

                    end_idx = *idx + next_ch.len_utf8();
                    chars.next();
                }

                let text = &input[token_start..end_idx];
                tokens.push((classify_scalar(text), text));
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
    fn test_merge_key_token() {
        // Test merge key '<<'
        let input = "<<: *defaults";
        let tokens = lex(input);

        let merge_keys: Vec<_> = tokens
            .iter()
            .filter(|(kind, _)| *kind == SyntaxKind::MERGE_KEY)
            .collect();
        assert_eq!(merge_keys.len(), 1);
        assert_eq!(merge_keys[0].1, "<<");

        // Test single '<' is not a merge key
        let input2 = "key: < value";
        let tokens2 = lex(input2);

        let merge_keys2: Vec<_> = tokens2
            .iter()
            .filter(|(kind, _)| *kind == SyntaxKind::MERGE_KEY)
            .collect();
        assert_eq!(merge_keys2.len(), 0, "Single < should not be a merge key");
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
            .any(|(kind, text)| *kind == SyntaxKind::STRING && *text == "-"));

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

        // With context-aware hyphen parsing, the standalone hyphen with spaces
        // is treated as a string because it's not a sequence marker
        assert!(tokens
            .iter()
            .any(|(kind, text)| *kind == SyntaxKind::STRING && *text == "-"));

        // Plus and colon are still tokenized as special characters
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

        // With context-aware hyphen parsing, the hyphen in content is now part of a STRING
        assert!(tokens
            .iter()
            .any(|(kind, text)| *kind == SyntaxKind::STRING && text.contains("-")));
        assert!(tokens.iter().any(|(kind, _)| *kind == SyntaxKind::GREATER)); // ">"
    }

    #[test]
    fn test_dash_handling_comprehensive() {
        // Test 1: Document start marker
        let input = "---\nkey: value";
        let tokens = lex(input);
        assert_eq!(tokens[0], (SyntaxKind::DOC_START, "---"));

        // Test 2: Document with just three dashes
        let input = "---";
        let tokens = lex(input);
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0], (SyntaxKind::DOC_START, "---"));

        // Test 3: Two dashes (not a document marker)
        let input = "--";
        let tokens = lex(input);
        assert_eq!(tokens.len(), 2);
        assert_eq!(tokens[0], (SyntaxKind::DASH, "-"));
        assert_eq!(tokens[1], (SyntaxKind::DASH, "-"));

        // Test 4: Four dashes
        let input = "----";
        let tokens = lex(input);
        assert_eq!(tokens[0], (SyntaxKind::DOC_START, "---"));
        assert_eq!(tokens[1], (SyntaxKind::STRING, "-"));
    }

    #[test]
    fn test_dash_in_different_scalar_contexts() {
        // Test kebab-case identifiers
        let input = "package-name: my-awesome-package-v2";
        let tokens = lex(input);
        assert_eq!(tokens[0], (SyntaxKind::STRING, "package-name"));
        assert_eq!(tokens[1], (SyntaxKind::COLON, ":"));
        assert_eq!(tokens[2], (SyntaxKind::WHITESPACE, " "));
        assert_eq!(tokens[3], (SyntaxKind::STRING, "my-awesome-package-v2"));

        // Test UUID-like strings
        let input = "id: 123e4567-e89b-12d3-a456-426614174000";
        let tokens = lex(input);
        assert_eq!(tokens[0], (SyntaxKind::STRING, "id"));
        assert_eq!(
            tokens[3],
            (SyntaxKind::STRING, "123e4567-e89b-12d3-a456-426614174000")
        );

        // Test command-line arguments
        let input = "args: --verbose --log-level=debug";
        let tokens = lex(input);
        // Double dashes are tokenized as two DASH tokens
        assert!(tokens.windows(3).any(|w| w[0] == (SyntaxKind::DASH, "-")
            && w[1] == (SyntaxKind::DASH, "-")
            && w[2] == (SyntaxKind::STRING, "verbose")));

        // Test negative numbers
        let input = "temperature: -40";
        let tokens = lex(input);
        // Negative numbers are tokenized as INT tokens
        assert!(tokens
            .iter()
            .any(|(kind, text)| *kind == SyntaxKind::INT && *text == "-40"));

        // Test ranges
        let input = "range: 1-10";
        let tokens = lex(input);
        assert!(tokens
            .iter()
            .any(|(kind, text)| *kind == SyntaxKind::STRING && *text == "1-10"));
    }

    #[test]
    fn test_sequence_markers_with_indentation() {
        // Test basic sequence
        let input = "- item1\n- item2";
        let tokens = lex(input);
        assert_eq!(tokens[0], (SyntaxKind::DASH, "-"));
        assert_eq!(tokens[1], (SyntaxKind::WHITESPACE, " "));
        assert_eq!(tokens[2], (SyntaxKind::STRING, "item1"));

        // Test indented sequence
        let input = "  - item1\n  - item2";
        let tokens = lex(input);
        assert_eq!(tokens[0], (SyntaxKind::INDENT, "  "));
        assert_eq!(tokens[1], (SyntaxKind::DASH, "-"));

        // Test nested sequences
        let input = "- item1\n  - nested1\n  - nested2\n- item2";
        let tokens = lex(input);
        let dash_tokens: Vec<_> = tokens
            .iter()
            .filter(|(kind, _)| *kind == SyntaxKind::DASH)
            .collect();
        assert_eq!(dash_tokens.len(), 4); // Four sequence markers

        // Test sequence with hyphenated values
        let input = "- first-item\n- second-item";
        let tokens = lex(input);
        assert_eq!(tokens[0], (SyntaxKind::DASH, "-"));
        assert_eq!(tokens[2], (SyntaxKind::STRING, "first-item"));
        assert_eq!(tokens[4], (SyntaxKind::DASH, "-"));
        assert_eq!(tokens[6], (SyntaxKind::STRING, "second-item"));
    }

    #[test]
    fn test_dash_after_colon() {
        // Test hyphen immediately after colon
        let input = "key:-value";
        let tokens = lex(input);
        assert_eq!(tokens[0], (SyntaxKind::STRING, "key"));
        assert_eq!(tokens[1], (SyntaxKind::COLON, ":"));
        assert_eq!(tokens[2], (SyntaxKind::STRING, "-value"));

        // Test with space
        let input = "key: -value";
        let tokens = lex(input);
        assert_eq!(tokens[0], (SyntaxKind::STRING, "key"));
        assert_eq!(tokens[1], (SyntaxKind::COLON, ":"));
        assert_eq!(tokens[2], (SyntaxKind::WHITESPACE, " "));
        assert_eq!(tokens[3], (SyntaxKind::STRING, "-value"));
    }

    #[test]
    fn test_block_scalar_with_chomping() {
        // Test literal block scalar with strip chomping
        let input = "text: |-\n  content";
        let tokens = lex(input);
        assert!(tokens.iter().any(|(kind, _)| *kind == SyntaxKind::PIPE));
        assert!(tokens
            .iter()
            .any(|(kind, text)| *kind == SyntaxKind::STRING && *text == "-"));

        // Test literal block scalar with keep chomping
        let input = "text: |+\n  content";
        let tokens = lex(input);
        assert!(tokens.iter().any(|(kind, _)| *kind == SyntaxKind::PIPE));
        assert!(tokens.iter().any(|(kind, _)| *kind == SyntaxKind::PLUS));

        // Test folded block scalar with strip chomping
        let input = "text: >-\n  content";
        let tokens = lex(input);
        assert!(tokens.iter().any(|(kind, _)| *kind == SyntaxKind::GREATER));
        assert!(tokens
            .iter()
            .any(|(kind, text)| *kind == SyntaxKind::STRING && *text == "-"));

        // Test with explicit indentation and chomping
        let input = "text: |2-\n  content";
        let tokens = lex(input);
        assert!(tokens.iter().any(|(kind, _)| *kind == SyntaxKind::PIPE));
        // The "2-" after pipe gets read as one token because hyphens in scalars are included
        assert!(tokens
            .iter()
            .any(
                |(kind, text)| (*kind == SyntaxKind::STRING || *kind == SyntaxKind::INT)
                    && text.contains("2")
            ));
    }

    #[test]
    fn test_dash_edge_cases() {
        // Test trailing hyphen
        let input = "value-";
        let tokens = lex(input);
        assert_eq!(tokens[0], (SyntaxKind::STRING, "value-"));

        // Test leading hyphen (not a sequence marker)
        let input = "-value";
        let tokens = lex(input);
        assert_eq!(tokens[0], (SyntaxKind::STRING, "-value"));

        // Test multiple consecutive hyphens in scalar
        let input = "key: a---b";
        let tokens = lex(input);
        assert!(tokens
            .iter()
            .any(|(kind, text)| *kind == SyntaxKind::STRING && *text == "a---b"));

        // Test hyphen at end of line
        let input = "key: value-\nnext: item";
        let tokens = lex(input);
        assert!(tokens
            .iter()
            .any(|(kind, text)| *kind == SyntaxKind::STRING && *text == "value-"));

        // Test mix of dashes and underscores
        let input = "snake_case-with-dash_mix";
        let tokens = lex(input);
        assert_eq!(tokens[0], (SyntaxKind::STRING, "snake_case-with-dash_mix"));
    }

    #[test]
    fn test_dash_in_flow_collections() {
        // Test dash in flow sequence
        let input = "[item-one, item-two]";
        let tokens = lex(input);
        assert_eq!(tokens[0], (SyntaxKind::LEFT_BRACKET, "["));
        assert_eq!(tokens[1], (SyntaxKind::STRING, "item-one"));
        assert_eq!(tokens[2], (SyntaxKind::COMMA, ","));
        assert_eq!(tokens[4], (SyntaxKind::STRING, "item-two"));
        assert_eq!(tokens[5], (SyntaxKind::RIGHT_BRACKET, "]"));

        // Test dash in flow mapping
        let input = "{kebab-key: kebab-value}";
        let tokens = lex(input);
        assert_eq!(tokens[0], (SyntaxKind::LEFT_BRACE, "{"));
        assert_eq!(tokens[1], (SyntaxKind::STRING, "kebab-key"));
        assert_eq!(tokens[2], (SyntaxKind::COLON, ":"));
        assert_eq!(tokens[4], (SyntaxKind::STRING, "kebab-value"));
        assert_eq!(tokens[5], (SyntaxKind::RIGHT_BRACE, "}"));
    }

    #[test]
    fn test_dash_with_quotes() {
        // Quoted strings should preserve everything inside
        let input = r#"key: "- not a sequence marker""#;
        let tokens = lex(input);
        assert!(tokens.iter().any(|(kind, _)| *kind == SyntaxKind::QUOTE));
        // The dash inside quotes becomes part of a string token

        let input = r#"key: '- also not a sequence marker'"#;
        let tokens = lex(input);
        assert!(tokens
            .iter()
            .any(|(kind, _)| *kind == SyntaxKind::SINGLE_QUOTE));
    }

    #[test]
    fn test_dash_in_multiline_values() {
        // Test multiline with dashes
        let input = "description: This is a multi-\n  line value with dashes";
        let tokens = lex(input);
        assert!(tokens
            .iter()
            .any(|(kind, text)| *kind == SyntaxKind::STRING && *text == "multi-"));

        // Test continuation with sequence-like line
        let input = "text: value\n  - but this is not a sequence";
        let tokens = lex(input);
        // The dash after indentation should be treated as a sequence marker
        let indent_dash: Vec<_> = tokens
            .windows(2)
            .filter(|w| w[0].0 == SyntaxKind::INDENT && w[1].0 == SyntaxKind::DASH)
            .collect();
        assert_eq!(indent_dash.len(), 1);
    }

    #[test]
    fn test_dash_special_yaml_values() {
        // Test that special YAML values with dashes work
        let input = "date: 2024-01-15";
        let tokens = lex(input);
        assert!(tokens
            .iter()
            .any(|(kind, text)| *kind == SyntaxKind::STRING && *text == "2024-01-15"));

        // Test ISO timestamp - gets tokenized as multiple parts due to hyphens
        let input = "timestamp: 2024-01-15T10:30:00-05:00";
        let tokens = lex(input);
        // The timestamp is split into multiple tokens but parses correctly
        assert!(tokens
            .iter()
            .any(|(kind, text)| *kind == SyntaxKind::STRING && text.contains("2024")));

        // Test version strings
        let input = "version: 1.0.0-beta.1";
        let tokens = lex(input);
        assert!(tokens
            .iter()
            .any(|(kind, text)| *kind == SyntaxKind::STRING && *text == "1.0.0-beta.1"));
    }
}
