//! Lexer for YAML files.

/// Whitespace and formatting validation errors
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WhitespaceError {
    /// The error message
    pub message: String,
    /// The byte range where the error occurred
    pub range: std::ops::Range<usize>,
    /// Error category
    pub category: WhitespaceErrorCategory,
}

/// Categories of whitespace errors
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum WhitespaceErrorCategory {
    /// Tab character used for indentation (forbidden in YAML)
    TabIndentation,
    /// Line too long according to configured limit
    LineTooLong,
    /// Mixed line ending styles
    MixedLineEndings,
    /// Invalid scalar indentation
    InvalidIndentation,
}

/// YAML Concrete Syntax Tree (CST) node types.
///
/// This enum defines all possible node types in the YAML syntax tree, representing both
/// lexical tokens (from the lexer) and semantic nodes (created by the parser).
///
/// # Tree Hierarchy
///
/// The YAML syntax tree follows this general structure:
///
/// ```text
/// ROOT
/// ├── DOCUMENT*
/// │   ├── DIRECTIVE* (optional, e.g., %YAML 1.2)
/// │   ├── DOC_START? (optional ---)
/// │   ├── MAPPING | SEQUENCE | SCALAR | TAGGED_NODE
/// │   └── DOC_END? (optional ...)
/// └── WHITESPACE | NEWLINE | COMMENT (between documents)
///
/// MAPPING
/// ├── MAPPING_ENTRY*
/// │   ├── KEY
/// │   │   └── SCALAR | SEQUENCE | MAPPING (YAML 1.2 allows complex keys)
/// │   ├── COLON
/// │   ├── WHITESPACE?
/// │   └── VALUE
/// │       └── SCALAR | SEQUENCE | MAPPING | TAGGED_NODE
/// ├── NEWLINE
/// ├── INDENT
/// └── COMMENT?
///
/// SEQUENCE  
/// ├── SEQUENCE_ENTRY*
/// │   ├── DASH
/// │   ├── WHITESPACE?
/// │   └── SCALAR | SEQUENCE | MAPPING | TAGGED_NODE
/// ├── NEWLINE
/// ├── INDENT
/// └── COMMENT?
///
/// SCALAR
/// └── STRING | INT | FLOAT | BOOL | NULL
///
/// TAGGED_NODE
/// ├── TAG (e.g., !!str, !custom)
/// ├── WHITESPACE?
/// └── SCALAR | MAPPING | SEQUENCE
/// ```
///
/// # Node Categories
///
/// ## Structural Nodes (created by parser)
/// - **ROOT**: Top-level container for the entire document
/// - **DOCUMENT**: A single YAML document (separated by --- or ...)
/// - **MAPPING**: Key-value pairs `{key: value}` or block style
/// - **SEQUENCE**: Lists `[item1, item2]` or block style with `-`
/// - **SCALAR**: Leaf values (strings, numbers, booleans, null)
/// - **TAGGED_NODE**: Values with explicit type tags `!!str "hello"`
///
/// ## Container Nodes (created by parser)
/// - **MAPPING_ENTRY**: A single key-value pair within a mapping
/// - **SEQUENCE_ENTRY**: A single item within a sequence
/// - **KEY**: The key part of a key-value pair (can contain complex types)
/// - **VALUE**: The value part of a key-value pair
///
/// ## Lexical Tokens (from lexer)
/// - **Punctuation**: COLON, DASH, COMMA, etc.
/// - **Brackets**: LEFT_BRACKET, RIGHT_BRACKET, LEFT_BRACE, RIGHT_BRACE
/// - **Literals**: STRING, INT, FLOAT, BOOL, NULL
/// - **YAML-specific**: TAG, ANCHOR, REFERENCE, MERGE_KEY
/// - **Document markers**: DOC_START (---), DOC_END (...)
/// - **Formatting**: WHITESPACE, NEWLINE, INDENT, COMMENT
///
/// ## Special Cases
///
/// ### Complex Keys (YAML 1.2.2)
/// Keys can be sequences or mappings, not just scalars:
/// ```yaml
/// [1, 2]: value        # Sequence key
/// {a: b}: value        # Mapping key
/// ```
///
/// ### Tagged Values
/// Values can have explicit type information:
/// ```yaml
/// number: !!int "123"  # Force string "123" to be treated as integer
/// binary: !!binary |   # Base64 encoded binary data
///   R0lGODlhDAAMAIQ...
/// ```
///
/// ### Block Scalars
/// Multi-line strings with special parsing rules:
/// ```yaml
/// literal: |           # PIPE indicates literal scalar
///   Line 1
///   Line 2
/// folded: >            # GREATER indicates folded scalar  
///   Long text that
///   gets folded
/// ```
///
/// The tree preserves all original formatting, comments, and whitespace,
/// enabling lossless round-trip parsing and precise source location tracking.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u16)]
#[allow(non_camel_case_types, clippy::upper_case_acronyms)]
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
    /// A YAML alias reference (e.g., *anchor_name)
    ALIAS,
    /// A YAML tagged scalar (tag + value)
    TAGGED_NODE,
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
    /// A complete mapping entry (key-value pair with associated tokens)
    MAPPING_ENTRY,
    /// A sequence entry (item with associated tokens)
    SEQUENCE_ENTRY,

    // Content tokens (from lexer)
    /// String literal (quoted or unquoted identifier)
    STRING,
    /// Unterminated string (missing closing quote)
    UNTERMINATED_STRING,
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
    /// UTF-8 Byte Order Mark (BOM) - U+FEFF at start of file
    BOM,
    /// End of file marker
    EOF,
}

impl From<SyntaxKind> for rowan::SyntaxKind {
    fn from(kind: SyntaxKind) -> Self {
        Self(kind as u16)
    }
}

/// Decide whether the whitespace run starting at `ws_idx` is internal to a
/// plain scalar or a real terminator. Internal whitespace is followed by
/// more plain-scalar content on the same line; a terminator is followed by
/// end-of-line, end-of-input, a `#` comment marker, a `:` that ends the
/// scalar, or a structural indicator. Quotes and hyphens remain scalar
/// content here; flow delimiters terminate only in flow context.
///
/// Caller must guarantee the character at `ws_idx` is a space or tab.
fn plain_scalar_continues_past_whitespace(input: &str, ws_idx: usize, flow_depth: u32) -> bool {
    let rest = &input[ws_idx..];
    // Find the first non-whitespace char in the run.
    let (offset, next) = match rest.char_indices().find(|(_, c)| *c != ' ' && *c != '\t') {
        Some(v) => v,
        None => return false,
    };
    match next {
        '\n' | '\r' | '#' => false,
        ',' | '[' | ']' | '{' | '}' if flow_depth > 0 => false,
        ':' => !is_colon_a_mapping_indicator(input, ws_idx + offset, flow_depth),
        // Once a scalar has begun these are all content on the next word:
        // the signs, the quotes, the flow indicators, and the node
        // properties and block headers, which are indicators only at the
        // start of a node. So `+ ?: v` is keyed `+ ?`, as `+ -: v` was.
        '-' | '+' | '?' | '|' | '>' | '&' | '*' | '!' | '%' | '\'' | '"' | ',' | '[' | ']'
        | '{' | '}' => true,
        c if is_yaml_special(c) => false,
        _ => true,
    }
}

/// Read an anchor or alias name (`ns-anchor-name`).
///
/// Per YAML 1.2 an anchor name is a run of non-space characters excluding
/// the flow indicators, so `-`, `*`, `:` and the rest are ordinary name
/// characters: saphyr reads `&xT*U---` as the single anchor `xT*U---`.
/// Reading these with the general scalar reader stopped at the first
/// YAML-special character and stranded the remainder.
fn read_anchor_name_from<'a>(
    chars: &mut std::iter::Peekable<std::str::CharIndices<'a>>,
    input: &'a str,
    start_idx: usize,
) -> &'a str {
    let mut end_idx = start_idx;
    while let Some((idx, ch)) = chars.peek().copied() {
        if ch.is_whitespace() || matches!(ch, ',' | '[' | ']' | '{' | '}') {
            break;
        }
        end_idx = idx + ch.len_utf8();
        chars.next();
    }
    &input[start_idx..end_idx]
}

/// Is a `:` at `input[colon_idx]` a mapping indicator (rather than
/// plain-scalar content)?
///
/// Per YAML 1.2 the answer is context-dependent. `:` is an indicator
/// when:
///   - followed by whitespace or EOF, in any context; or
///   - inside a flow collection, followed by a flow terminator
///     (`,`, `]`, `}`).
///
/// It is *not* an indicator when embedded in a plain scalar (`http://`,
/// `::vector`, timestamp bodies, etc.).
///
/// `colon_idx` must point at the `:` byte itself; only the char that
/// follows is inspected. Callers already in flow context should pass a
/// non-zero `flow_depth`.
fn is_colon_a_mapping_indicator(input: &str, colon_idx: usize, flow_depth: u32) -> bool {
    let after = input[colon_idx + 1..].chars().next();
    match after {
        None => true,
        Some(ch) if ch.is_whitespace() => true,
        Some(ch) if flow_depth > 0 && matches!(ch, ',' | ']' | '}') => true,
        _ => false,
    }
}

/// Is a `#` at `input[hash_idx]` the start of a comment (rather than
/// scalar content)?
///
/// Per YAML 1.2 section 6.6, `#` starts a comment only when preceded
/// by whitespace (or is at the start of the input / a fresh line).
/// Inside a plain scalar with no preceding whitespace, `#` is scalar
/// content (URLs, `#frag`, etc.).
fn is_hash_a_comment_start(input: &str, hash_idx: usize) -> bool {
    hash_idx == 0
        || input[..hash_idx]
            .chars()
            .next_back()
            .is_some_and(|c| c.is_whitespace())
}

/// Read a plain-scalar body, treating `:` as scalar content when it's not
/// followed by whitespace (matching YAML plain-scalar semantics), and `-`
/// as scalar content unconditionally.
///
/// Used from the `.` and `-` scalar-prefix branches, where the char
/// that dispatched us was itself a scalar prefix rather than a
/// structural indicator.
/// Scan a double-quoted string body, consuming through the closing quote.
///
/// Returns the end offset and whether a closing quote was found; a backslash
/// escapes the next character, so `"a\""` runs to the second `"`.
fn scan_double_quoted(
    chars: &mut std::iter::Peekable<std::str::CharIndices<'_>>,
    start_idx: usize,
) -> (usize, bool) {
    let mut end_idx = start_idx + 1;
    let mut escaped = false;

    while let Some((idx, ch)) = chars.peek() {
        let (current_idx, current_ch) = (*idx, *ch);
        end_idx = current_idx + current_ch.len_utf8();
        chars.next();

        if escaped {
            escaped = false;
        } else if current_ch == '\\' {
            escaped = true;
        } else if current_ch == '"' {
            return (end_idx, true);
        }
    }
    (end_idx, false)
}

/// Scan a single-quoted string body, consuming through the closing quote.
///
/// Returns the end offset and whether a closing quote was found. A doubled
/// `''` is an escaped quote and does not end the string.
fn scan_single_quoted(
    chars: &mut std::iter::Peekable<std::str::CharIndices<'_>>,
    start_idx: usize,
) -> (usize, bool) {
    let mut end_idx = start_idx + 1;

    while let Some((idx, ch)) = chars.peek() {
        let (current_idx, current_ch) = (*idx, *ch);
        end_idx = current_idx + current_ch.len_utf8();
        chars.next();

        if current_ch == '\'' {
            if let Some((next_idx, '\'')) = chars.peek() {
                // Doubled quote: an escaped `'`, not the end.
                end_idx = *next_idx + 1;
                chars.next();
            } else {
                return (end_idx, true);
            }
        }
    }
    (end_idx, false)
}

/// Push a quoted-string token, marking it UNTERMINATED_STRING when the
/// closing quote was never reached.
fn push_quoted_string<'a>(
    tokens: &mut Vec<(SyntaxKind, &'a str)>,
    input: &'a str,
    token_start: usize,
    end_idx: usize,
    found_closing: bool,
) {
    let kind = if found_closing {
        SyntaxKind::STRING
    } else {
        SyntaxKind::UNTERMINATED_STRING
    };
    tokens.push((kind, &input[token_start..end_idx]));
}

/// Read a plain scalar that starts at `token_start`, whose body begins at
/// `body_start`, classify it, and push it as one token.
///
/// The leading character has already been consumed by the caller's match arm
/// (`-`, `+`, `.`, ...) and stays part of the scalar text.
fn push_plain_scalar_from<'a>(
    tokens: &mut Vec<(SyntaxKind, &'a str)>,
    chars: &mut std::iter::Peekable<std::str::CharIndices<'a>>,
    input: &'a str,
    token_start: usize,
    body_start: usize,
    flow_depth: u32,
    absorb_spaces: bool,
) {
    let body = read_plain_scalar_body_from(chars, input, body_start, flow_depth, absorb_spaces);
    let text = &input[token_start..body_start + body.len()];
    tokens.push((classify_scalar(text), text));
}

/// `absorb_spaces` folds an internal whitespace run into the scalar when
/// more scalar content follows, as a plain scalar spanning several words
/// requires. A block scalar's chomping indicator (`|-`, `>-`) is a token
/// in its own right, so that caller passes false.
fn read_plain_scalar_body_from<'a>(
    chars: &mut std::iter::Peekable<std::str::CharIndices<'a>>,
    input: &'a str,
    start_idx: usize,
    flow_depth: u32,
    absorb_spaces: bool,
) -> &'a str {
    let mut end_idx = start_idx;
    while let Some((idx, ch)) = chars.peek().copied() {
        // Intra-line whitespace belongs to the scalar when more scalar
        // content follows on the same line, exactly as in the catch-all
        // arm. Without this a space ended the body, so `-{ [a]: v }`
        // lexed `[` as a flow collection rather than scalar content.
        if absorb_spaces && (ch == ' ' || ch == '\t') {
            if !plain_scalar_continues_past_whitespace(input, idx, flow_depth) {
                break;
            }
            while let Some((wi, wc)) = chars.peek().copied() {
                if wc != ' ' && wc != '\t' {
                    break;
                }
                end_idx = wi + wc.len_utf8();
                chars.next();
            }
            continue;
        }
        if ch.is_whitespace() {
            break;
        }
        if ch == ':' && is_colon_a_mapping_indicator(input, idx, flow_depth) {
            break;
        }
        if ch == '#' && is_hash_a_comment_start(input, idx) {
            break;
        }
        // `-` and `+` are indicators only where a node starts (a sequence
        // entry, or a block-scalar chomping suffix); inside a body they are
        // content, so `++` is the single scalar `++`.
        // `?`, `|`, `>`, `&`, `*`, `!` and `%` are indicators only at the
        // start of a node; this is a scalar body, so they are content here,
        // as the catch-all arm already has it. `+?` is one scalar.
        if is_yaml_special_except(ch, "-+:#'\"?|>&*!%")
            && !(flow_depth == 0 && matches!(ch, ',' | '[' | ']' | '{' | '}'))
        {
            break;
        }
        end_idx = idx + ch.len_utf8();
        chars.next();
    }
    &input[start_idx..end_idx]
}

/// Tokenize YAML input with whitespace validation
pub fn lex(input: &str) -> Vec<(SyntaxKind, &str)> {
    let (tokens, _) = lex_with_validation(input);
    tokens
}

/// Configuration for whitespace and formatting validation
pub struct ValidationConfig {
    /// Maximum line length (None = no limit)
    pub max_line_length: Option<usize>,
    /// Whether to enforce consistent line endings
    pub enforce_consistent_line_endings: bool,
}

impl Default for ValidationConfig {
    fn default() -> Self {
        Self {
            max_line_length: Some(120), // Default to 120 characters
            enforce_consistent_line_endings: true,
        }
    }
}

/// Tokenize YAML input with whitespace and formatting validation
pub fn lex_with_validation(input: &str) -> (Vec<(SyntaxKind, &str)>, Vec<WhitespaceError>) {
    lex_with_validation_config(input, &ValidationConfig::default())
}

/// Record a "line too long" error if the line ending at `end` exceeds the
/// configured maximum.
fn check_line_length(
    errors: &mut Vec<WhitespaceError>,
    config: &ValidationConfig,
    line_start: usize,
    end: usize,
) {
    let Some(max_len) = config.max_line_length else {
        return;
    };
    let line_length = end - line_start;
    if line_length > max_len {
        errors.push(WhitespaceError {
            message: format!("Line too long ({line_length} > {max_len} characters)"),
            range: line_start..end,
            category: WhitespaceErrorCategory::LineTooLong,
        });
    }
}

/// Record the line ending style on first sight, and flag any later one that
/// disagrees with it.
fn note_line_ending<'a>(
    errors: &mut Vec<WhitespaceError>,
    config: &ValidationConfig,
    detected: &mut Option<&'a str>,
    line_ending: &'a str,
    range: std::ops::Range<usize>,
) {
    if !config.enforce_consistent_line_endings {
        return;
    }
    match *detected {
        Some(seen) if seen != line_ending => errors.push(WhitespaceError {
            message: "Inconsistent line endings detected".to_string(),
            range,
            category: WhitespaceErrorCategory::MixedLineEndings,
        }),
        Some(_) => {}
        None => *detected = Some(line_ending),
    }
}

/// Tokenize YAML input with custom validation configuration
pub fn lex_with_validation_config<'a>(
    input: &'a str,
    config: &ValidationConfig,
) -> (Vec<(SyntaxKind, &'a str)>, Vec<WhitespaceError>) {
    use SyntaxKind::*;

    let mut tokens = Vec::with_capacity(input.len() / 8); // Pre-allocate based on estimate
    let mut chars = input.char_indices().peekable();
    let mut whitespace_errors = Vec::new();
    let bytes = input.as_bytes();

    // Track line information for validation
    let mut current_line_start = 0;
    let mut detected_line_ending: Option<&str> = None;

    // Track flow collection depth for context-aware tokenization
    let mut flow_depth: u32 = 0;

    // Indentation of the line carrying a block-scalar header, while its body
    // is still running. Everything indented past that column is literal
    // text, so a `{` or `[` there must not open a flow collection: the
    // depth would never come back down and every later `,` in the file
    // would lex as a delimiter.
    let mut block_scalar_header_indent: Option<usize> = None;

    // Handle UTF-8 BOM (U+FEFF) at the start of the file
    // Per YAML spec, BOM is allowed and should be processed transparently
    if let Some((0, '\u{FEFF}')) = chars.peek() {
        chars.next(); // Consume the BOM
        tokens.push((BOM, "\u{FEFF}"));
        current_line_start = '\u{FEFF}'.len_utf8();
    }

    while let Some((start_idx, ch)) = chars.next() {
        let token_start = start_idx;

        match ch {
            // Context-aware hyphen handling
            '-' => {
                if flow_depth == 0
                    && start_idx == current_line_start
                    && input[start_idx..].starts_with("---")
                    && input[start_idx + 3..]
                        .chars()
                        .next()
                        .map_or(true, |next| next.is_whitespace())
                {
                    chars.next();
                    chars.next();
                    tokens.push((DOC_START, &input[token_start..start_idx + 3]));
                } else {
                    // Check if this hyphen should be treated as a sequence marker
                    // It's a sequence marker if:
                    // 1. It's at the beginning of a line (after optional indentation)
                    // 2. OR it follows a value context (after ? or : plus whitespace)
                    // AND it's followed by whitespace or end of input

                    // Check if preceded only by whitespace from start of line
                    let line_start_pos = current_line_start;
                    let before_dash = &input[line_start_pos..token_start];
                    let only_whitespace_before = before_dash.chars().all(|c| c == ' ' || c == '\t');

                    // Check if the previous non-whitespace token was ? or :
                    // indicating a value context where sequences are allowed
                    let after_value_indicator = tokens
                        .iter()
                        .rev()
                        .find(|(kind, _)| !matches!(kind, WHITESPACE | INDENT))
                        .is_some_and(|(kind, _)| matches!(kind, QUESTION | COLON));

                    // Check if followed by whitespace or end of input
                    let followed_by_whitespace_or_end = chars
                        .peek()
                        .map_or(true, |(_, next_ch)| next_ch.is_whitespace());

                    let is_sequence_marker = (only_whitespace_before || after_value_indicator)
                        && followed_by_whitespace_or_end;

                    if is_sequence_marker {
                        tokens.push((DASH, &input[token_start..start_idx + 1]));
                    } else {
                        // This hyphen is part of a scalar value. Use the
                        // plain-scalar body reader so embedded `:` (not
                        // followed by whitespace) stays inside the scalar.
                        //
                        // A `-` closing a block-scalar header (`|-`, `>2-`)
                        // is a chomping indicator and stands alone, so it
                        // must not swallow the space that follows it. Same
                        // test as the `+` arm below.
                        let is_chomping_indicator = input[current_line_start..start_idx]
                            .bytes()
                            .rev()
                            .find(|b| !b.is_ascii_digit())
                            .is_some_and(|b| b == b'|' || b == b'>');
                        push_plain_scalar_from(
                            &mut tokens,
                            &mut chars,
                            input,
                            token_start,
                            start_idx + 1,
                            flow_depth,
                            !is_chomping_indicator,
                        );
                    }
                }
            }
            '+' => {
                // `+` is a chomping indicator when it appears inside a
                // block-scalar header (`|+`, `|2+`, `>+`, `>2+`). Detect
                // that by looking at the current line: the last non-digit
                // byte before this `+` must be `|` or `>`. Otherwise, if
                // the next char begins a plain-scalar body (digit, `.`,
                // letter, ...) this `+` is a sign prefix and belongs in
                // the same scalar token -- so `+.INF` lexes as one FLOAT
                // rather than PLUS + FLOAT. A bare `+` followed by
                // whitespace stays PLUS.
                let line_start = current_line_start;
                let is_chomping_indicator = input[line_start..start_idx]
                    .bytes()
                    .rev()
                    .find(|b| !b.is_ascii_digit())
                    .is_some_and(|b| b == b'|' || b == b'>');
                // Outside a flow collection the flow indicators are ordinary
                // scalar content, a `:` not followed by whitespace is never a
                // mapping indicator, and a quote no longer opens a quoted
                // scalar once this one has begun. All of them continue the
                // body: `a+[b]`, `a {b+:c}` and `+'a'` are each one scalar,
                // as the `-` spellings already were. These are the same
                // exceptions the body reader itself makes.
                let next_starts_scalar = chars.peek().is_some_and(|(idx, c)| {
                    // A space does not end the scalar when more scalar
                    // content follows on the same line: `+ x: v` is keyed
                    // `+ x`, exactly as `a x: v` is keyed `a x`. The body
                    // reader makes the same test before absorbing a run.
                    if c.is_whitespace() {
                        return *c != '\n'
                            && *c != '\r'
                            && plain_scalar_continues_past_whitespace(input, *idx, flow_depth);
                    }
                    !c.is_whitespace()
                        && (!is_yaml_special(*c)
                            // Everything the body reader keeps as content
                            // starts a body: quotes, signs, and the node
                            // properties and block headers, which are
                            // indicators only at the start of a node. So
                            // `+?`, `+|` and `+&` are each one scalar, as
                            // `+x` already was.
                            || matches!(
                                c,
                                '\'' | '"' | '+' | '-' | '?' | '|' | '>' | '&' | '*' | '!' | '%'
                            )
                            // A `#` glued to the sign is content, not a
                            // comment: `+#: v` is keyed `+#`.
                            || (*c == '#' && !is_hash_a_comment_start(input, *idx))
                            || (flow_depth == 0 && matches!(c, '[' | ']' | '{' | '}' | ','))
                            || (*c == ':'
                                && !is_colon_a_mapping_indicator(input, *idx, flow_depth)))
                });
                if !is_chomping_indicator && next_starts_scalar {
                    push_plain_scalar_from(
                        &mut tokens,
                        &mut chars,
                        input,
                        token_start,
                        start_idx + 1,
                        flow_depth,
                        true,
                    );
                } else if !is_chomping_indicator && {
                    // A key may be separated from its colon by spaces
                    // (`+ : v`), so look past them as well.
                    let rest = &input[start_idx + 1..];
                    let gap = rest.len() - rest.trim_start_matches([' ', '\t']).len();
                    rest[gap..].starts_with(':')
                        && is_colon_a_mapping_indicator(input, start_idx + 1 + gap, flow_depth)
                } {
                    // `+: v` is a mapping keyed by the plain scalar `+`, as
                    // `-: v` already was. A bare PLUS left the key invisible
                    // and stranded the rest of the entry.
                    let text = &input[token_start..start_idx + 1];
                    tokens.push((classify_scalar(text), text));
                } else if is_chomping_indicator {
                    tokens.push((PLUS, &input[token_start..start_idx + 1]));
                } else {
                    // Nothing follows on this line to extend the body, but a
                    // `+` outside a block-scalar header is still content, not
                    // an indicator: `+\nx\n` is the scalar `+ x`. A bare PLUS
                    // token no parse rule claims left the `+` invisible and
                    // stranded whatever followed it.
                    let text = &input[token_start..start_idx + 1];
                    tokens.push((classify_scalar(text), text));
                }
            }
            ':' => {
                // Colon at token boundary. It's a mapping indicator when
                // its lookahead matches is_colon_a_mapping_indicator, OR
                // when we're in flow context and the immediately
                // preceding meaningful token was a scalar (i.e., a key
                // has been laid down and this `:` separates it from the
                // value):
                //     { "foo"\n  :bar }    -- COLON after "foo"
                // vs.
                //     [ ::vector ]         -- `:` starts a plain scalar
                let preceded_by_scalar = flow_depth > 0
                    && tokens
                        .iter()
                        .rev()
                        .find(|(k, _)| !matches!(k, WHITESPACE | INDENT | NEWLINE | COMMENT))
                        .is_some_and(|(k, _)| matches!(k, STRING | INT | FLOAT | BOOL | NULL));
                if preceded_by_scalar || is_colon_a_mapping_indicator(input, start_idx, flow_depth)
                {
                    tokens.push((COLON, &input[token_start..start_idx + 1]));
                } else {
                    // This colon starts (or continues) a plain scalar such
                    // as a URL, `::vector`, or a timestamp. The shared body
                    // reader knows the rules: a `:` not followed by space is
                    // content, flow indicators are content in block context,
                    // and a `#` glued to content is not a comment. Do not let
                    // it absorb an internal space run, though -- `: v` after
                    // this colon is a value, not more key.
                    let rest = read_plain_scalar_body_from(
                        &mut chars,
                        input,
                        start_idx + 1,
                        flow_depth,
                        false,
                    );
                    let text = &input[token_start..start_idx + 1 + rest.len()];
                    tokens.push((classify_scalar(text), text));
                }
            }
            // An explicit-key indicator, like the node properties below, only
            // at the start of a node: `a ?b` is the scalar `a ?b`, not a key
            // indicator inside it.
            //
            // Per YAML 1.2 `c-complex-mapping-key` the `?` must also be
            // followed by a space or a line break to be an indicator. Without
            // that test `a: ?!!r{2}x` opened an explicit key and then read
            // `!!r` as a tag, which stranded the rest of the line; PyYAML
            // reads the value as the plain scalar `?!!r{2}x`.
            '?' if node_property_can_start(&tokens)
                && chars.peek().map_or(true, |(_, next)| next.is_whitespace()) =>
            {
                tokens.push((QUESTION, &input[token_start..start_idx + 1]))
            }
            // A flow indicator inside a block scalar's body is literal
            // text, so it neither opens nor closes a collection. Read the
            // rest of the line as the scalar content it is.
            '[' | ']' | '{' | '}' | ','
                if in_block_scalar_body(
                    block_scalar_header_indent,
                    input,
                    current_line_start,
                    start_idx,
                ) =>
            {
                let rest =
                    read_plain_scalar_body_from(&mut chars, input, start_idx + 1, flow_depth, true);
                let text = &input[token_start..start_idx + 1 + rest.len()];
                tokens.push((classify_scalar(text), text));
            }
            '[' => {
                flow_depth += 1;
                tokens.push((LEFT_BRACKET, &input[token_start..start_idx + 1]));
            }
            ']' => {
                flow_depth = flow_depth.saturating_sub(1);
                tokens.push((RIGHT_BRACKET, &input[token_start..start_idx + 1]));
            }
            '{' => {
                flow_depth += 1;
                tokens.push((LEFT_BRACE, &input[token_start..start_idx + 1]));
            }
            '}' => {
                flow_depth = flow_depth.saturating_sub(1);
                tokens.push((RIGHT_BRACE, &input[token_start..start_idx + 1]));
            }
            // A `,` separates entries only inside a flow collection. In block
            // context it is ordinary plain-scalar content (`a: x\n  ,y`), so
            // fall through to the catch-all arm, which already keeps flow
            // indicators inside a block scalar.
            ',' if flow_depth > 0 => tokens.push((COMMA, &input[token_start..start_idx + 1])),
            // A block-scalar header, like the node properties below, only at
            // the start of a node: `a|b` is the scalar `a|b`, not a header
            // inside it. `>` reaches this arm only at a node start already,
            // because the catch-all treats it as scalar content.
            '|' if node_property_can_start(&tokens) => {
                block_scalar_header_indent = Some(line_indent(input, current_line_start));
                tokens.push((PIPE, &input[token_start..start_idx + 1]))
            }
            '>' if node_property_can_start(&tokens) => {
                block_scalar_header_indent = Some(line_indent(input, current_line_start));
                tokens.push((GREATER, &input[token_start..start_idx + 1]))
            }
            // `<<` is a merge key only when the key is exactly `<<`, i.e. the
            // next character ends the token. Anything else starting with `<`
            // (`<<foo`, a bare `<`) is a plain scalar and falls through to the
            // catch-all arm below, which knows the real plain-scalar rules.
            '<' if is_merge_key_at(input, start_idx) => {
                chars.next(); // consume second <
                tokens.push((MERGE_KEY, &input[token_start..start_idx + 2]));
            }
            // Like tags, only at the start of a node: `a: x &anc` is the
            // scalar `x &anc`, not an anchored value.
            '&' if node_property_can_start(&tokens) => {
                // Check if this is an anchor definition
                let name = read_anchor_name_from(&mut chars, input, start_idx + 1);
                if !name.is_empty() {
                    tokens.push((ANCHOR, &input[token_start..start_idx + 1 + name.len()]));
                } else {
                    tokens.push((AMPERSAND, &input[token_start..start_idx + 1]));
                }
            }
            '*' if node_property_can_start(&tokens) => {
                // Check if this is an alias reference
                let name = read_anchor_name_from(&mut chars, input, start_idx + 1);
                if !name.is_empty() {
                    tokens.push((REFERENCE, &input[token_start..start_idx + 1 + name.len()]));
                } else {
                    tokens.push((ASTERISK, &input[token_start..start_idx + 1]));
                }
            }
            '"' => {
                let (end_idx, found_closing) = scan_double_quoted(&mut chars, start_idx);
                push_quoted_string(&mut tokens, input, token_start, end_idx, found_closing);
            }
            '\'' => {
                let (end_idx, found_closing) = scan_single_quoted(&mut chars, start_idx);
                push_quoted_string(&mut tokens, input, token_start, end_idx, found_closing);
            }

            // Document end
            '.' => {
                // Check for three dots (document end marker)
                // `...` is the document-end marker only when it is the whole
                // line, in block context: the same three conditions the `---`
                // arm applies. `...w` is the plain scalar `...w`, as both
                // saphyr and PyYAML read it, and `---x` already was.
                let is_doc_end = flow_depth == 0
                    && start_idx == current_line_start
                    && input[start_idx..].starts_with("...")
                    && input[start_idx + 3..]
                        .chars()
                        .next()
                        .map_or(true, |next| next.is_whitespace());
                if is_doc_end && chars.peek() == Some(&(start_idx + 1, '.')) {
                    chars.next(); // consume second .
                    if chars.peek() == Some(&(start_idx + 2, '.')) {
                        chars.next(); // consume third .
                        tokens.push((DOC_END, &input[token_start..start_idx + 3]));
                    } else {
                        // Two dots -- continue as plain scalar body (allows
                        // embedded `-` and `:` per YAML plain-scalar rules).
                        push_plain_scalar_from(
                            &mut tokens,
                            &mut chars,
                            input,
                            token_start,
                            start_idx + 2,
                            flow_depth,
                            true,
                        );
                    }
                } else {
                    // Single dot -- part of plain scalar body.
                    push_plain_scalar_from(
                        &mut tokens,
                        &mut chars,
                        input,
                        token_start,
                        start_idx + 1,
                        flow_depth,
                        true,
                    );
                }
            }

            // Comments.
            //
            // Per YAML 1.2 section 6.6, `#` starts a comment only when
            // preceded by whitespace. At outer-token boundary we have
            // one of two situations:
            //   - `#` at the start of the document / a fresh line, or
            //     after a whitespace/newline: legitimate comment.
            //   - `#` glued to the tail of a non-whitespace token
            //     (e.g., `]#foo`): the input violates 4.6.6. The
            //     validator's check_comment_token_whitespace flags
            //     this at validation time; the lexer still emits a
            //     COMMENT token so downstream error reporting has
            //     something to point at.
            //
            // A `#` glued to the tail of *plain-scalar* content is different:
            // `:#: v` is a mapping keyed `:#`, as saphyr reads it. Continue
            // the scalar rather than starting a comment, which the catch-all
            // arm already does for `a#: v`.
            '#' if !is_hash_a_comment_start(input, start_idx)
                && tokens.last().is_some_and(|(k, text)| {
                    matches!(k, STRING | INT | FLOAT | BOOL | NULL | PLUS)
                        // A quoted scalar ends at its closing quote, so a `#`
                        // glued to it really is the 6.6 violation the
                        // validator reports (`key: "value"# c`).
                        && !text.starts_with(['"', '\''])
                }) =>
            {
                let rest =
                    read_plain_scalar_body_from(&mut chars, input, start_idx + 1, flow_depth, true);
                let text = &input[token_start..start_idx + 1 + rest.len()];
                tokens.push((classify_scalar(text), text));
            }
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
            //
            // Only at the start of a node: mid-scalar a `!` is ordinary
            // content (`a: x !!b` is the scalar `x !!b`), which the
            // catch-all arm below reads with plain-scalar rules.
            '!' if node_property_can_start(&tokens) => {
                // Handle tag indicators - both ! and !!
                let mut end_idx = start_idx + 1;

                // A verbatim tag `!<uri>` runs to its closing `>` and may hold
                // any URI character, commas included, so take it whole.
                if let Some((_, '<')) = chars.peek() {
                    chars.next(); // consume '<'
                    end_idx = start_idx + 2;
                    // An unterminated `!<x` stops at the line break, which
                    // the main loop then handles; the text still round-trips
                    // and the parser reports the malformed tag.
                    while let Some((idx, ch)) = chars.peek() {
                        if *ch == '\n' {
                            break;
                        }
                        end_idx = *idx + ch.len_utf8();
                        let closing = *ch == '>';
                        chars.next();
                        if closing {
                            break;
                        }
                    }
                    tokens.push((TAG, &input[token_start..end_idx]));
                    continue;
                }

                // Check for double exclamation (global tag)
                if let Some((_, '!')) = chars.peek() {
                    chars.next(); // consume the second !
                    end_idx = start_idx + 2;
                }

                // Read the tag name after the ! or !!
                while let Some((idx, ch)) = chars.peek() {
                    if !is_tag_char(*ch) {
                        break;
                    }
                    end_idx = *idx + ch.len_utf8();
                    chars.next();
                }

                tokens.push((TAG, &input[token_start..end_idx]));
            }

            '%' => {
                // A directive is only a `%` that opens a line. Anywhere else,
                // and anywhere inside a flow collection, it is ordinary plain
                // scalar content (`a: b%c`).
                if flow_depth > 0 || start_idx != current_line_start {
                    // Treat as part of a plain scalar. The shared body reader
                    // knows the rules this needs: flow indicators are content
                    // in block context, and an internal whitespace run stays
                    // in the scalar when more content follows on the line, so
                    // `r % {` is the single scalar `r % {`.
                    let rest = read_plain_scalar_body_from(
                        &mut chars,
                        input,
                        start_idx + 1,
                        flow_depth,
                        true,
                    );
                    let text = &input[token_start..start_idx + 1 + rest.len()];
                    tokens.push((classify_scalar(text), text));
                } else {
                    // At the start of a line in block context, % starts a directive
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
            }

            // Newlines
            '\n' => {
                check_line_length(
                    &mut whitespace_errors,
                    config,
                    current_line_start,
                    start_idx,
                );
                note_line_ending(
                    &mut whitespace_errors,
                    config,
                    &mut detected_line_ending,
                    "\n",
                    token_start..start_idx + 1,
                );

                tokens.push((NEWLINE, &input[token_start..start_idx + 1]));
                current_line_start = start_idx + 1;
                // A block scalar's body ends at the first following line
                // that is not indented past its header and not blank.
                if let Some(header_indent) = block_scalar_header_indent {
                    if line_starts_content_at_or_before(input, current_line_start, header_indent) {
                        block_scalar_header_indent = None;
                    }
                }
            }
            '\r' => {
                check_line_length(
                    &mut whitespace_errors,
                    config,
                    current_line_start,
                    start_idx,
                );

                let (line_ending, end_pos) = if let Some((_, '\n')) = chars.peek() {
                    chars.next();
                    ("\r\n", start_idx + 2)
                } else {
                    ("\r", start_idx + 1)
                };

                note_line_ending(
                    &mut whitespace_errors,
                    config,
                    &mut detected_line_ending,
                    line_ending,
                    token_start..end_pos,
                );

                tokens.push((NEWLINE, &input[token_start..end_pos]));
                current_line_start = end_pos;
            }

            // Whitespace (spaces and tabs)
            ' ' | '\t' => {
                let mut end_idx = start_idx + 1;
                let mut has_tabs = ch == '\t';

                while let Some((idx, ch)) = chars.peek() {
                    if *ch != ' ' && *ch != '\t' {
                        break;
                    }
                    if *ch == '\t' {
                        has_tabs = true;
                    }
                    end_idx = *idx + 1;
                    chars.next();
                }

                // Determine if this is structural indentation
                // Check for any line break: \n, \r\n (already consumed \n), or \r alone
                let is_indentation = token_start == 0
                    || (token_start > 0
                        && (bytes[token_start - 1] == b'\n' || bytes[token_start - 1] == b'\r'));

                if is_indentation {
                    // Check for tab characters in indentation (forbidden in YAML)
                    if has_tabs {
                        whitespace_errors.push(WhitespaceError {
                            message: "Tab character used for indentation (forbidden in YAML)"
                                .to_string(),
                            range: token_start..end_idx,
                            category: WhitespaceErrorCategory::TabIndentation,
                        });
                    }
                    tokens.push((INDENT, &input[token_start..end_idx]));
                } else {
                    tokens.push((WHITESPACE, &input[token_start..end_idx]));
                }
            }

            // Everything else is scalar content
            _ => {
                let mut end_idx = start_idx + ch.len_utf8();

                // Read the rest of the scalar normally, including embedded hyphens
                while let Some((idx, next_ch)) = chars.peek().copied() {
                    // Line breaks always terminate the current scalar token.
                    if next_ch == '\n' || next_ch == '\r' {
                        break;
                    }

                    // Intra-line whitespace (space/tab) is part of a plain
                    // scalar when followed by more scalar content on the same
                    // line. Per YAML 1.2, plain scalars may contain single or
                    // multiple internal spaces, but stop at line break, at
                    // ` #` (comment start), or at end of line.
                    if next_ch == ' ' || next_ch == '\t' {
                        if !plain_scalar_continues_past_whitespace(input, idx, flow_depth) {
                            break;
                        }
                        // Absorb the whitespace run into the scalar
                        while let Some((wi, wc)) = chars.peek().copied() {
                            if wc != ' ' && wc != '\t' {
                                break;
                            }
                            end_idx = wi + wc.len_utf8();
                            chars.next();
                        }
                        continue;
                    }

                    // `:` inside a plain scalar breaks the scalar iff
                    // it's a mapping indicator (see helper for rules).
                    if next_ch == ':' {
                        if is_colon_a_mapping_indicator(input, idx, flow_depth) {
                            break;
                        }
                        end_idx = idx + next_ch.len_utf8();
                        chars.next();
                        continue;
                    }

                    // Check other special characters (excluding hyphen and colon).
                    // `&`, `*`, `!`, `?` are node-property / block-key indicators
                    // that are only meaningful at the start of a node. Once we
                    // are inside a plain-scalar body they are ordinary content
                    // per YAML 1.2 §7.1 (a `&anchor` must be followed by
                    // whitespace to actually be an anchor).
                    // `%` is an indicator only at the start of a line, where
                    // the main loop takes it as a directive; inside a scalar it
                    // is ordinary content (`a: b%c`).
                    // `|` and `>` open a block scalar only at a node start, so
                    // mid-scalar they are content too (`a|b`, `a>b`). A `+` is
                    // an indicator only as the chomping suffix of such a
                    // header, where `|2+` must keep it as its own token;
                    // anywhere else it is content (`a+b`).
                    // A `+` closing a block-scalar header (`|+`, `>2+`) is a
                    // chomping indicator. The `|` has to *open* the value for
                    // that, though: in `/|+:` the pipe is scalar content, so
                    // the `+` is content too and the key is `/|+`. Require
                    // nothing but the value's start before the `|`.
                    let plus_is_chomping = next_ch == '+' && {
                        // Step back over any explicit indentation digits to
                        // the `|` or `>` this `+` would close.
                        let head = input[current_line_start..idx]
                            .trim_end_matches(|c: char| c.is_ascii_digit());
                        match head.strip_suffix(['|', '>']) {
                            // The header must open the value: only the value's
                            // start may precede it. In `/|+:` the pipe is
                            // scalar content, so the `+` is content too and the
                            // key is `/|+`.
                            Some(before) => {
                                let before = before.trim_end();
                                before.is_empty() || before.ends_with(':') || before.ends_with('-')
                            }
                            None => false,
                        }
                    };
                    if plus_is_chomping {
                        break;
                    }
                    if is_yaml_special_except(next_ch, "-:#&*!?'\"|>%+") {
                        // In block context, flow indicators do NOT break scalars
                        if flow_depth == 0 && matches!(next_ch, '[' | ']' | '{' | '}' | ',') {
                            // do nothing, let it be part of the scalar
                        } else {
                            break;
                        }
                    }

                    // `#` inside a plain scalar breaks the scalar iff
                    // it's a comment start (see helper for rules).
                    if next_ch == '#' {
                        if is_hash_a_comment_start(input, idx) {
                            break;
                        }
                        end_idx = idx + next_ch.len_utf8();
                        chars.next();
                        continue;
                    }

                    // Special case: check if hyphen is a sequence marker
                    if next_ch == '-' {
                        // A hyphen is only a sequence marker if it's at line start
                        // and this scalar is already complete (we're at a word boundary)
                        let before_hyphen = &input[current_line_start..idx];

                        // If there's only whitespace before the hyphen, it might be a sequence marker
                        // Break here to let the main loop handle it
                        if before_hyphen.chars().all(|c| c == ' ' || c == '\t') && idx == end_idx {
                            break;
                        }
                    }

                    end_idx = idx + next_ch.len_utf8();
                    chars.next();
                }

                let text = &input[token_start..end_idx];
                tokens.push((classify_scalar(text), text));
            }
        }
    }

    // Check the final line length if there's no trailing newline
    if let Some(max_len) = config.max_line_length {
        let final_line_length = input.len() - current_line_start;
        if final_line_length > max_len && final_line_length > 0 {
            whitespace_errors.push(WhitespaceError {
                message: format!("Line too long ({final_line_length} > {max_len} characters)"),
                range: current_line_start..input.len(),
                category: WhitespaceErrorCategory::LineTooLong,
            });
        }
    }

    (tokens, whitespace_errors)
}

/// Map a plain-scalar's semantic classification onto the concrete
/// [`SyntaxKind`] the tokenizer emits. The classification itself lives
/// in [`ScalarValue::classify_plain`](crate::ScalarValue::classify_plain);
/// this function only translates enum variants.
fn classify_scalar(text: &str) -> SyntaxKind {
    use crate::CoreScalarType;
    match crate::scalar::ScalarValue::classify_plain(text) {
        CoreScalarType::Null => SyntaxKind::NULL,
        CoreScalarType::Boolean => SyntaxKind::BOOL,
        CoreScalarType::Integer => SyntaxKind::INT,
        CoreScalarType::Float => SyntaxKind::FLOAT,
        CoreScalarType::String => SyntaxKind::STRING,
    }
}

/// Common set of YAML special characters
const YAML_SPECIAL_CHARS: &str = ":+-?[]{},'|>&*!%\"#";

/// Check whether the `<` at `idx` starts a merge key (`<<`).
///
/// `<<` is a merge key only when the whole key is `<<`, per the YAML 1.1 merge
/// type. A `<<` followed by more scalar content (`<<foo`) is a plain scalar,
/// which is what PyYAML and other implementations accept.
fn is_merge_key_at(input: &str, idx: usize) -> bool {
    let rest = &input[idx..];
    if !rest.starts_with("<<") {
        return false;
    }
    match rest[2..].chars().next() {
        None => true,
        Some(ch) => ch.is_whitespace() || matches!(ch, ':' | ',' | ']' | '}' | '#'),
    }
}

/// Check if a character has special meaning in YAML
/// Whether `ch` may appear in a tag shorthand's suffix.
///
/// YAML 1.2 `ns-tag-char` is any URI character except the flow indicators
/// `,`, `[`, `]`, `{`, `}` and the `!` that would start another tag. That
/// admits plenty the lexer treats as special elsewhere -- `:` in `!!ss:eq`,
/// `-` in `!!ss---` -- so a tag cannot be scanned with the general
/// [`YAML_SPECIAL_CHARS`] set.
/// Whether a node property (`!tag`, `&anchor`, `*alias`) may start here.
///
/// Per YAML 1.2 §7.1 these apply to the start of a node. Once a plain
/// scalar has begun on the line, a `!` or `&` is ordinary content, so
/// `a: x !!b` is the scalar `x !!b` rather than a tagged node. A preceding
/// space is not enough to start a new node: what matters is whether the
/// last token was scalar content.
fn node_property_can_start(tokens: &[(SyntaxKind, &str)]) -> bool {
    for (kind, _) in tokens.iter().rev() {
        match kind {
            // Skip the layout between the property and what precedes it.
            SyntaxKind::WHITESPACE | SyntaxKind::INDENT => continue,
            // A node starts after these.
            SyntaxKind::NEWLINE
            | SyntaxKind::COLON
            | SyntaxKind::DASH
            | SyntaxKind::QUESTION
            | SyntaxKind::COMMA
            | SyntaxKind::LEFT_BRACKET
            | SyntaxKind::LEFT_BRACE
            | SyntaxKind::DOC_START
            | SyntaxKind::TAG
            | SyntaxKind::ANCHOR
            // A bare `&` or `*` is punctuation rather than content, so a
            // property may still follow it.
            | SyntaxKind::AMPERSAND
            | SyntaxKind::ASTERISK => return true,
            // Anything else is content the property would sit inside.
            _ => return false,
        }
    }
    // Nothing before it: the very start of the input is a node start.
    true
}

/// Leading whitespace of the line starting at `line_start`.
fn line_indent(input: &str, line_start: usize) -> usize {
    let line = &input[line_start..];
    line.len() - line.trim_start_matches([' ', '\t']).len()
}

/// Whether the line starting at `line_start` carries content at or left of
/// `indent`, which is what ends a block scalar's body.
///
/// A blank line says nothing about the body's indentation, so it stays in.
fn line_starts_content_at_or_before(input: &str, line_start: usize, indent: usize) -> bool {
    let line = input[line_start..]
        .split(['\n', '\r'])
        .next()
        .unwrap_or_default();
    if line.trim().is_empty() {
        return false;
    }
    line.len() - line.trim_start_matches([' ', '\t']).len() <= indent
}

/// Whether `idx` is inside the body of a block scalar whose header sat on a
/// line indented `header_indent`.
///
/// A block scalar's body is every following line indented past its header,
/// and all of it is literal text: `a: |\n  {\nb,c: 1\n` holds the scalar
/// `{`, and the `,` on the next line belongs to the key `b,c`. The caller
/// clears `header_indent` once a line ends the body, so this only has to
/// rule out the header's own line, which `idx > line_start` does not cover
/// on its own.
fn in_block_scalar_body(
    header_indent: Option<usize>,
    input: &str,
    line_start: usize,
    idx: usize,
) -> bool {
    let Some(header_indent) = header_indent else {
        return false;
    };
    idx > line_start && line_indent(input, line_start) > header_indent
}

fn is_tag_char(ch: char) -> bool {
    if ch.is_whitespace() {
        return false;
    }
    !matches!(ch, ',' | '[' | ']' | '{' | '}' | '!' | '#')
}

fn is_yaml_special(ch: char) -> bool {
    YAML_SPECIAL_CHARS.contains(ch)
}

/// Check if character is YAML special, with optional exclusions
fn is_yaml_special_except(ch: char, exclude: &str) -> bool {
    YAML_SPECIAL_CHARS.contains(ch) && !exclude.contains(ch)
}

/// Check whether `name` is a well-formed anchor/alias name for this lexer.
///
/// The lexer reads an anchor (`&name`) or alias (`*name`) to the first
/// whitespace or flow indicator (see [`read_anchor_name_from`]), so a name
/// holding one of those would be silently truncated on round-trip. This
/// check is deliberately stricter than the lexer: it also rejects the
/// [`YAML_SPECIAL_CHARS`], which are legal in a name but easy to misread.
pub(crate) fn is_valid_anchor_name(name: &str) -> bool {
    !name.is_empty()
        && !name
            .chars()
            .any(|c| c.is_whitespace() || is_yaml_special(c))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_valid_anchor_name() {
        assert!(is_valid_anchor_name("shared"));
        assert!(is_valid_anchor_name("shared_1"));
        assert!(is_valid_anchor_name("anchor.dotted"));
        assert!(is_valid_anchor_name("naïve"));

        assert!(!is_valid_anchor_name(""));
        assert!(!is_valid_anchor_name("has space"));
        assert!(!is_valid_anchor_name("has\ttab"));
        assert!(!is_valid_anchor_name("has\nnewline"));
        assert!(!is_valid_anchor_name("*star"));
        assert!(!is_valid_anchor_name("&amp"));
        assert!(!is_valid_anchor_name("comma,name"));
        assert!(!is_valid_anchor_name("bracket]name"));
        assert!(!is_valid_anchor_name("colon:name"));
    }

    #[test]
    fn test_embedded_hyphens_stay_in_one_plain_scalar() {
        let input = format!("k: {}\n", "a-".repeat(200));
        let kinds: Vec<_> = lex(&input).into_iter().map(|(k, _)| k).collect();
        assert!(kinds.contains(&SyntaxKind::STRING) || kinds.contains(&SyntaxKind::SCALAR));
        let dash_count = kinds.iter().filter(|k| **k == SyntaxKind::DASH).count();
        assert_eq!(dash_count, 0);
        let seq = lex("- a\n- b\n");
        let seq_dashes = seq.iter().filter(|(k, _)| *k == SyntaxKind::DASH).count();
        assert_eq!(seq_dashes, 2);
    }

    /// Names accepted by [`is_valid_anchor_name`] must round-trip through the
    /// lexer as a single `REFERENCE` token, so `Alias::new` and the lexer stay
    /// in agreement.
    #[test]
    fn test_valid_anchor_names_round_trip_through_lexer() {
        for name in ["shared", "shared_1", "anchor.dotted", "naïve"] {
            let input = format!("*{name}");
            let tokens = lex(&input);
            assert_eq!(
                tokens,
                vec![(SyntaxKind::REFERENCE, input.as_str())],
                "name {name:?} did not round-trip as a single REFERENCE token"
            );
        }
    }

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
        let input = r#"single: 'quoted'
double: "quoted""#;
        let tokens = lex(input);

        // Find quoted string tokens - after fix, quotes are included in STRING tokens
        let quoted_strings: Vec<_> = tokens
            .iter()
            .filter(|(kind, text)| {
                *kind == SyntaxKind::STRING && (text.starts_with('\'') || text.starts_with('"'))
            })
            .collect();
        assert_eq!(quoted_strings.len(), 2); // single and double quoted strings

        // Verify content (order depends on which appears first in the source)
        let quoted_texts: Vec<&str> = {
            let mut v: Vec<&str> = quoted_strings.iter().map(|(_, t)| *t).collect();
            v.sort();
            v
        };
        assert_eq!(quoted_texts, ["\"quoted\"", "'quoted'"]);
    }

    #[test]
    fn test_document_markers() {
        let input = "---\nkey: value\n...";
        let tokens = lex(input);

        println!("Document tokens:");
        for (i, (kind, text)) in tokens.iter().enumerate() {
            println!("  {}: {:?} = {:?}", i, kind, text);
        }

        // Check for document start and end markers
        let doc_start_count = tokens
            .iter()
            .filter(|(kind, _)| *kind == SyntaxKind::DOC_START)
            .count();
        let doc_end_count = tokens
            .iter()
            .filter(|(kind, _)| *kind == SyntaxKind::DOC_END)
            .count();
        assert_eq!(doc_start_count, 1);
        assert_eq!(doc_end_count, 1);
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
    fn test_indicators_mid_plain_scalar_are_scalar_content() {
        // Per YAML 1.2 §7.1 the node-property indicators `&`, `*`, `!`
        // only apply when a scalar starts with them (followed by an
        // identifier). Mid-scalar they are ordinary content and must
        // not create phantom ANCHOR / REFERENCE / TAG tokens.
        // Including space-separated: a preceding space does not start a new
        // node, so `k: a !b` is still one plain scalar.
        for input in [
            "k: 3.1&1", "k: a&b", "k: a*b", "k: a!b", "k: a?b", "k: a !b", "k: a !!b", "k: a &b",
            "k: a *b", "- a !b",
        ] {
            let tokens = lex(input);
            let bad: Vec<_> = tokens
                .iter()
                .filter(|(kind, _)| {
                    matches!(
                        kind,
                        SyntaxKind::ANCHOR
                            | SyntaxKind::REFERENCE
                            | SyntaxKind::TAG
                            | SyntaxKind::QUESTION
                    )
                })
                .collect();
            assert!(
                bad.is_empty(),
                "{:?} produced structural tokens mid-scalar: {:?}",
                input,
                bad
            );
        }
        // And a real anchor still tokenizes as ANCHOR.
        let tokens = lex("a: &real 1");
        assert!(
            tokens.iter().any(|(k, _)| *k == SyntaxKind::ANCHOR),
            "real anchor should still be recognized: {:?}",
            tokens
        );
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
    fn test_angle_prefix_is_plain_scalar_not_merge_key() {
        let tokens = lex("<<foo: 1");
        let merge_keys: Vec<_> = tokens
            .iter()
            .filter(|(kind, _)| *kind == SyntaxKind::MERGE_KEY)
            .collect();
        assert_eq!(
            merge_keys.len(),
            0,
            "<<foo should be a plain key, got {:?}",
            tokens
        );
        assert!(
            tokens
                .iter()
                .any(|(kind, text)| *kind != SyntaxKind::MERGE_KEY && *text == "<<foo"),
            "expected <<foo scalar, got {:?}",
            tokens
        );

        let tokens = lex("{<<foo: 1, more: x}");
        assert_eq!(
            tokens
                .iter()
                .filter(|(kind, _)| *kind == SyntaxKind::MERGE_KEY)
                .count(),
            0,
            "flow <<foo should not be MERGE_KEY: {:?}",
            tokens
        );

        let tokens = lex("<<: *defaults");
        assert_eq!(
            tokens
                .iter()
                .filter(|(kind, text)| *kind == SyntaxKind::MERGE_KEY && *text == "<<")
                .count(),
            1
        );

        use crate::YamlFile;
        use std::str::FromStr;
        let file = YamlFile::from_str("<<foo: 1\nbar: 2\n").unwrap();
        let mapping = file.document().unwrap().as_mapping().unwrap();
        assert_eq!(mapping.keys().count(), 2);
        assert_eq!(
            mapping
                .get("<<foo")
                .unwrap()
                .as_scalar()
                .unwrap()
                .as_string(),
            "1"
        );
    }

    #[test]
    fn test_angle_prefixed_scalar_uses_plain_scalar_rules() {
        // A key starting with `<` follows the same plain-scalar rules as any
        // other key: `-`, `!`, `&` and a non-indicator `:` are ordinary
        // content, not token breaks.
        use crate::YamlFile;
        use std::str::FromStr;

        for key in [
            "<foo-bar",
            "<<foo-bar",
            "<foo!x",
            "<<foo!x",
            "<foo&x",
            "<<foo&x",
            "<foo:x",
            "<<foo:x",
        ] {
            let src = format!("{}: 1\n", key);
            let tokens = lex(&src);
            assert_eq!(
                tokens
                    .iter()
                    .filter(|(kind, _)| *kind != SyntaxKind::WHITESPACE)
                    .map(|(_, text)| *text)
                    .collect::<Vec<_>>(),
                vec![key, ":", "1", "\n"],
                "unexpected tokens for {:?}",
                src
            );

            let file = YamlFile::from_str(&src).unwrap();
            assert_eq!(file.to_string(), src);
            let mapping = file.document().unwrap().as_mapping().unwrap();
            assert_eq!(
                mapping.get(key).unwrap().as_scalar().unwrap().as_string(),
                "1"
            );
        }
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

        // The `-` and `+` are content, so the whole key is one plain scalar:
        // saphyr reads this as `line with - and + and` mapped to
        // `characters`.
        assert!(tokens
            .iter()
            .any(|(kind, text)| *kind == SyntaxKind::STRING && *text == "line with - and + and"));
        assert!(!tokens.iter().any(|(kind, _)| *kind == SyntaxKind::PLUS));

        // The mapping colon still stands alone.
        assert!(tokens
            .iter()
            .any(|(kind, text)| *kind == SyntaxKind::COLON && *text == ":"));

        // Round-trip: concatenating tokens reproduces input.
        let joined: String = tokens.iter().map(|(_, t)| *t).collect();
        assert_eq!(joined, input);
    }

    #[test]
    fn test_token_recognition() {
        let input = "key: |2+ \n  content with - and : and > chars\n  more content";
        let tokens = lex(input);

        // Print tokens for debugging
        println!("Tokens:");
        for (i, (kind, text)) in tokens.iter().enumerate() {
            println!("  {}: {:?} = {:?}", i, kind, text);
        }

        // Verify all expected token kinds are present (input: "key: |2+ \n  content with - and : and > chars\n  more content")
        let count = |k: SyntaxKind| tokens.iter().filter(|(kind, _)| *kind == k).count();
        // Two colons: one for the mapping ("key:"), one in the value content ("and :")
        assert_eq!(count(SyntaxKind::COLON), 2);
        assert_eq!(count(SyntaxKind::PIPE), 1); // "|"
        assert_eq!(count(SyntaxKind::INT), 1); // "2"
        assert_eq!(count(SyntaxKind::PLUS), 1); // "+"
                                                // The `>` here sits inside the block scalar's content, not at a
                                                // node start, so it is scalar text rather than a folded header.
        assert_eq!(count(SyntaxKind::GREATER), 0);
        assert_eq!(count(SyntaxKind::NEWLINE), 2); // after "|2+" line and after first content line
        assert_eq!(count(SyntaxKind::INDENT), 2); // "  " before each content line
                                                  // Multiple STRING tokens: "key", content words, and the hyphen
        assert!(count(SyntaxKind::STRING) >= 1, "expected STRING tokens");

        assert_eq!(
            tokens
                .iter()
                .filter(|(kind, text)| *kind == SyntaxKind::STRING && *text == "content with - and")
                .count(),
            1
        );
    }

    #[test]
    fn test_dash_handling() {
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
        assert_eq!(tokens, vec![(SyntaxKind::STRING, "--")]);

        // Test 4: Four dashes
        let input = "----";
        let tokens = lex(input);
        assert_eq!(tokens, vec![(SyntaxKind::STRING, "----")]);
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

        // Test command-line arguments. The two words are one plain
        // scalar, so they lex as a single token rather than being
        // rejoined by the parser.
        let input = "args: --verbose --log-level=debug";
        let tokens = lex(input);
        assert_eq!(
            tokens[3],
            (SyntaxKind::STRING, "--verbose --log-level=debug")
        );

        // Test negative numbers
        let input = "temperature: -40";
        let tokens = lex(input);
        // Negative numbers are tokenized as INT tokens
        assert_eq!(
            tokens
                .iter()
                .filter(|(kind, text)| *kind == SyntaxKind::INT && *text == "-40")
                .count(),
            1
        );

        // Test ranges
        let input = "range: 1-10";
        let tokens = lex(input);
        assert_eq!(
            tokens
                .iter()
                .filter(|(kind, text)| *kind == SyntaxKind::STRING && *text == "1-10")
                .count(),
            1
        );
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
        // According to YAML spec, "key:-value" is a single plain scalar
        // because the colon is not followed by whitespace
        let input = "key:-value";
        let tokens = lex(input);
        assert_eq!(tokens[0], (SyntaxKind::STRING, "key:-value"));

        // Test with space - this creates a mapping
        let input = "key: -value";
        let tokens = lex(input);
        assert_eq!(tokens[0], (SyntaxKind::STRING, "key"));
        assert_eq!(tokens[1], (SyntaxKind::COLON, ":"));
        assert_eq!(tokens[2], (SyntaxKind::WHITESPACE, " "));
        assert_eq!(tokens[3], (SyntaxKind::STRING, "-value"));
    }

    #[test]
    fn test_yaml_spec_compliant_colon_handling() {
        // Test that colons are handled according to YAML spec:
        // - Colon followed by whitespace indicates mapping
        // - Colon not followed by whitespace is part of plain scalar

        // URLs should be single scalars (no space after colon)
        let input = "http://example.com:8080/path";
        let tokens = lex(input);
        assert_eq!(tokens.len(), 1);
        assert_eq!(
            tokens[0],
            (SyntaxKind::STRING, "http://example.com:8080/path")
        );

        // Timestamps should be single scalars
        let input = "2024:12:31:23:59:59";
        let tokens = lex(input);
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0], (SyntaxKind::STRING, "2024:12:31:23:59:59"));

        // Key-value pairs need space after colon
        let input = "key: value";
        let tokens = lex(input);
        assert_eq!(tokens[0], (SyntaxKind::STRING, "key"));
        assert_eq!(tokens[1], (SyntaxKind::COLON, ":"));
        assert_eq!(tokens[2], (SyntaxKind::WHITESPACE, " "));
        assert_eq!(tokens[3], (SyntaxKind::STRING, "value"));

        // Without space, it's a single scalar
        let input = "key:value";
        let tokens = lex(input);
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0], (SyntaxKind::STRING, "key:value"));

        // Multiple colons without spaces
        let input = "a:b:c:d";
        let tokens = lex(input);
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0], (SyntaxKind::STRING, "a:b:c:d"));
    }

    #[test]
    fn test_block_scalar_with_chomping() {
        // Helper to count tokens by kind
        let count_kind = |toks: &[(SyntaxKind, &str)], k: SyntaxKind| {
            toks.iter().filter(|(kind, _)| *kind == k).count()
        };

        // Test literal block scalar with strip chomping
        let input = "text: |-\n  content";
        let tokens = lex(input);
        assert_eq!(count_kind(&tokens, SyntaxKind::PIPE), 1);
        assert_eq!(
            tokens
                .iter()
                .filter(|(kind, text)| *kind == SyntaxKind::STRING && *text == "-")
                .count(),
            1
        );

        // Test literal block scalar with keep chomping
        let input = "text: |+\n  content";
        let tokens = lex(input);
        assert_eq!(count_kind(&tokens, SyntaxKind::PIPE), 1);
        assert_eq!(count_kind(&tokens, SyntaxKind::PLUS), 1);

        // Test folded block scalar with strip chomping
        let input = "text: >-\n  content";
        let tokens = lex(input);
        assert_eq!(count_kind(&tokens, SyntaxKind::GREATER), 1);
        assert_eq!(
            tokens
                .iter()
                .filter(|(kind, text)| *kind == SyntaxKind::STRING && *text == "-")
                .count(),
            1
        );

        // Test with explicit indentation and chomping
        let input = "text: |2-\n  content";
        let tokens = lex(input);
        assert_eq!(count_kind(&tokens, SyntaxKind::PIPE), 1);
        // The "2-" after pipe gets read as one token because hyphens in scalars are included
        let has_2_token = tokens.iter().any(|(kind, text)| {
            (*kind == SyntaxKind::STRING || *kind == SyntaxKind::INT) && text.contains("2")
        });
        assert!(has_2_token, "expected a token containing '2'");
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
        assert_eq!(
            tokens
                .iter()
                .filter(|(kind, text)| *kind == SyntaxKind::STRING && *text == "a---b")
                .count(),
            1
        );

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
    fn test_whitespace_validation_tab_indentation() {
        // Test tab character validation
        let input_with_tabs = "key: value\n\tindented_key: indented_value";
        let (tokens, errors) = lex_with_validation(input_with_tabs);

        // Should have detected tab indentation error
        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].category, WhitespaceErrorCategory::TabIndentation);
        assert_eq!(
            errors[0].message,
            "Tab character used for indentation (forbidden in YAML)"
        );

        // But should still tokenize correctly
        assert!(tokens
            .iter()
            .any(|(kind, text)| *kind == SyntaxKind::INDENT && text.contains('\t')));
    }

    #[test]
    fn test_whitespace_validation_line_endings() {
        // Test mixed line ending detection
        let input_mixed = "line1\nline2\r\nline3\rline4";
        let config = ValidationConfig {
            enforce_consistent_line_endings: true,
            max_line_length: None,
        };
        let (tokens, errors) = lex_with_validation_config(input_mixed, &config);

        // Should detect mixed line endings
        assert!(errors
            .iter()
            .any(|e| e.category == WhitespaceErrorCategory::MixedLineEndings));

        // Should still tokenize all line endings
        let newlines: Vec<_> = tokens
            .iter()
            .filter(|(kind, _)| *kind == SyntaxKind::NEWLINE)
            .collect();
        assert_eq!(newlines.len(), 3); // Three line endings
        assert_eq!(newlines[0].1, "\n");
        assert_eq!(newlines[1].1, "\r\n");
        assert_eq!(newlines[2].1, "\r");
    }

    #[test]
    fn test_whitespace_validation_line_length() {
        // Test line length validation
        let long_line = format!("key: {}", "a".repeat(150));
        let config = ValidationConfig {
            enforce_consistent_line_endings: false,
            max_line_length: Some(120),
        };
        let (_, errors) = lex_with_validation_config(&long_line, &config);

        // Should detect line too long
        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].category, WhitespaceErrorCategory::LineTooLong);
        assert_eq!(errors[0].message, "Line too long (155 > 120 characters)");
    }

    #[test]
    fn test_whitespace_validation_disabled() {
        // Test with validation disabled
        let input_with_issues = "key: value\n\tindented: with_tabs\n";
        let config = ValidationConfig {
            enforce_consistent_line_endings: false,
            max_line_length: None,
        };
        let (tokens, errors) = lex_with_validation_config(input_with_issues, &config);

        // Should still detect tab indentation (always enforced in YAML)
        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].category, WhitespaceErrorCategory::TabIndentation);

        // Should tokenize normally
        assert!(!tokens.is_empty());
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
        // Quoted strings should preserve everything inside as STRING tokens
        let input = r#"key: "- not a sequence marker""#;
        let tokens = lex(input);
        assert_eq!(
            tokens
                .iter()
                .filter(|(kind, text)| {
                    *kind == SyntaxKind::STRING && *text == "\"- not a sequence marker\""
                })
                .count(),
            1
        );

        let input = r#"key: '- also not a sequence marker'"#;
        let tokens = lex(input);
        assert_eq!(
            tokens
                .iter()
                .filter(|(kind, text)| {
                    *kind == SyntaxKind::STRING && *text == "'- also not a sequence marker'"
                })
                .count(),
            1
        );
    }

    #[test]
    fn test_dash_in_multiline_values() {
        // Test multiline with dashes: plain scalars absorb intra-line
        // whitespace, so the first line becomes one STRING token ending
        // in the trailing hyphen (hyphen-at-end-of-word is scalar content,
        // not a sequence marker mid-scalar).
        let input = "description: This is a multi-\n  line value with dashes";
        let tokens = lex(input);
        assert!(tokens
            .iter()
            .any(|(kind, text)| *kind == SyntaxKind::STRING && *text == "This is a multi-"));

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
        assert!(tokens.iter().any(
            |(kind, text)| *kind == SyntaxKind::STRING && *text == "2024-01-15T10:30:00-05:00"
        ));

        // Test version strings
        let input = "version: 1.0.0-beta.1";
        let tokens = lex(input);
        assert!(tokens
            .iter()
            .any(|(kind, text)| *kind == SyntaxKind::STRING && *text == "1.0.0-beta.1"));
    }

    #[test]
    fn test_flow_indicators_in_block_scalar() {
        // Flow indicators should be allowed in block context scalars
        // This is valid YAML: the curly braces are part of the scalar value
        let input = "key: unix:///Users/${metadata.username}/path";
        let tokens = lex(input);
        assert_eq!(tokens.len(), 4);
        assert_eq!(tokens[0], (SyntaxKind::STRING, "key"));
        assert_eq!(tokens[1], (SyntaxKind::COLON, ":"));
        assert_eq!(tokens[2], (SyntaxKind::WHITESPACE, " "));
        assert_eq!(
            tokens[3],
            (
                SyntaxKind::STRING,
                "unix:///Users/${metadata.username}/path"
            )
        );
    }
}
