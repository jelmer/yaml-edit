//! Checks on the syntax and indentation of scalars.

use super::{Rule, Scope, Violation};
use crate::column::{space_indent, ColumnIndex};
use crate::nodes::{child_token, is_block_scalar, is_quoted_scalar};
use crate::yaml::SyntaxNode;

/// Check for invalid escape sequences in quoted strings
pub(super) fn check_escape_sequences(node: &SyntaxNode, violations: &mut Vec<Violation>) {
    // Check first character to see if this is a quoted string - no allocation needed
    let first_char = node.first_token().and_then(|t| t.text().chars().next());
    if first_char != Some('"') {
        return;
    }

    // Scan for escape sequences (requires one allocation for the text)
    let text = node.text().to_string();
    let mut chars = text.chars().peekable();

    while let Some(ch) = chars.next() {
        if ch == '\\' {
            if let Some(&next) = chars.peek() {
                // Valid escape sequences in YAML 1.2
                // A `\\` before a line break escapes that break, folding
                // the line: the test suite's 565N wraps a long binary
                // value that way.
                let valid_escapes = [
                    '0', 'a', 'b', 't', 'n', 'v', 'f', 'r', 'e', ' ', '"', '/', '\\', 'N', '_',
                    'L', 'P', 'x', 'u', 'U', '\n', '\r', '\t',
                ];

                if !valid_escapes.contains(&next) {
                    violations.push(Violation::error_at(
                        Rule::InvalidEscape,
                        node.text_range(),
                        format!("Invalid escape sequence: \\{next}"),
                    ));
                    return; // Found one, no need to continue
                }
                // Consume the escaped character, or `\\$` reads as a
                // valid `\\` followed by an invalid `\$` (test suite
                // 6SLA).
                chars.next();
                // `\x`, `\u` and `\U` take exactly 2, 4 and 8 hex digits.
                let digits = match next {
                    'x' => 2,
                    'u' => 4,
                    'U' => 8,
                    _ => continue,
                };
                let code: String = chars.by_ref().take(digits).collect();
                if code.len() != digits || !code.chars().all(|c| c.is_ascii_hexdigit()) {
                    violations.push(Violation::error_at(
                        Rule::InvalidEscape,
                        node.text_range(),
                        format!("Invalid escape sequence: \\{next}{code}"),
                    ));
                    return;
                }
            }
        }
    }
}

/// Check for content on the same line as block scalar indicator (| or >)
///
/// Per YAML spec, block scalar content must start on the line after the indicator.
/// Only chomping indicators (+/-) and indentation indicators (1-9) are allowed
/// on the same line as the block scalar indicator.
pub(super) fn check_block_scalar_indicator(node: &SyntaxNode, violations: &mut Vec<Violation>) {
    // A plain scalar may hold a `|` or `>` token on a continuation line.
    if !is_block_scalar(node) {
        return;
    }

    // Check if any STRING tokens appear before the first NEWLINE after the indicator
    let mut found_indicator = false;
    let mut found_newline = false;
    // The header ends at the first white space or comment; the lexer can
    // split its indicators over several tokens (`|+-` is PLUS and "-").
    let mut in_header = true;
    let mut chomping_indicators = 0;

    for child in node.children_with_tokens() {
        if let rowan::NodeOrToken::Token(token) = child {
            // Mark when we find the block indicator
            if matches!(
                token.kind(),
                crate::SyntaxKind::GREATER | crate::SyntaxKind::PIPE
            ) {
                found_indicator = true;
                continue;
            }

            // After indicator, before newline
            if found_indicator && !found_newline {
                match token.kind() {
                    crate::SyntaxKind::NEWLINE => {
                        found_newline = true;
                    }
                    // A chomping indicator and an indentation digit may
                    // share the indicator's line; the lexer gives `-`,
                    // `+`, `2-` and `-2` as STRING, so they are not the
                    // content this rule is looking for.
                    crate::SyntaxKind::STRING
                        if token.text().chars().all(|c| matches!(c, '+' | '-'))
                            || (token.text().len() == 2
                                && token.text().chars().any(|c| c.is_ascii_digit())
                                && token.text().chars().any(|c| matches!(c, '+' | '-'))) => {}
                    crate::SyntaxKind::STRING => {
                        // Found content on same line as indicator
                        violations.push(Violation::error_at(
                            Rule::Other,
                            token.text_range(),
                            "Block scalar content cannot appear on same line as indicator",
                        ));
                        return;
                    }
                    // An indentation indicator is a single digit 1-9;
                    // `|0` and `|10` are errors the YAML test suite
                    // expects (2G84).
                    // `|2-` and `|-2` both carry the digit 2, so strip a
                    // chomping indicator before checking it (D83L).
                    crate::SyntaxKind::INT
                        if !matches!(
                            token.text().trim_matches(['+', '-']),
                            "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
                        ) =>
                    {
                        violations.push(Violation::error_at(
                            Rule::Other,
                            token.text_range(),
                            "Block scalar indentation indicator must be a digit 1-9",
                        ));
                        return;
                    }
                    crate::SyntaxKind::WHITESPACE | crate::SyntaxKind::COMMENT => {
                        in_header = false;
                    }
                    _ => {}
                }
                if in_header
                    && matches!(
                        token.kind(),
                        crate::SyntaxKind::PLUS
                            | crate::SyntaxKind::STRING
                            | crate::SyntaxKind::INT
                    )
                {
                    chomping_indicators += token.text().matches(['+', '-']).count();
                    if chomping_indicators > 1 {
                        violations.push(Violation::error_at(
                            Rule::Other,
                            token.text_range(),
                            "Block scalar header has more than one chomping indicator",
                        ));
                        return;
                    }
                }
            }
        }
    }
}

/// Check that a block scalar header starting a later line than its key
/// or its node's properties is indented past the parent node.
///
/// Before the header, `s-separate(n+1)` requires at least `n+1` spaces
/// of indentation on a new line, so the parser's reading of
/// `a: !!str\n|\n x\n` as a tagged block scalar, or of `a:\n|\n x\n`
/// as a block scalar, is not valid YAML. The YAML test suite's M5C3 has
/// the header two columns left of its tag, which is still past the key.
pub(super) fn check_block_scalar_header_indent(
    node: &SyntaxNode,
    columns: &ColumnIndex,
    scope: Scope,
    violations: &mut Vec<Violation>,
) {
    if !is_block_scalar(node) {
        return;
    }
    let Some(header) = node.first_token() else {
        return;
    };
    let Some(min_indent) = scope.continuation_indent else {
        return;
    };
    if columns.column(&header) < min_indent {
        violations.push(Violation::error_at(
            Rule::Other,
            header.text_range(),
            "Block scalar header must be indented past its parent node",
        ));
    }
}

/// Check block scalar (`|` or `>`) content indentation.
///
/// Once a blank line at indentation N has appeared, no later
/// non-blank content line may be at indentation < N (per YAML 1.2
/// example 5LLU). This flags the case where the content indent
/// implicitly established by empty-line whitespace runs is later
/// under-cut.
pub(super) fn check_block_scalar_content_indent(
    node: &SyntaxNode,
    violations: &mut Vec<Violation>,
) {
    if !is_block_scalar(node) {
        return;
    }
    // An indentation indicator fixes the body's column, so a deeper
    // leading line is content rather than indentation.
    if crate::nodes::scalar_node::block_scalar_indicators(node)
        .1
        .is_some()
    {
        return;
    }

    // Walk tokens after the block indicator's NEWLINE. Track the
    // deepest INDENT seen so far (from blank-only lines); flag any
    // subsequent INDENT + non-blank content whose text is shorter.
    //
    // Indentation is spaces only (s-indent), so a tab in an INDENT
    // token starts the line's content: R4YG's " \t" line is indented
    // one, not two, and its expected events keep the tab.

    let mut past_first_newline = false;
    let mut max_blank_indent = 0usize;
    let mut body_indent: Option<usize> = None;
    let mut pending_indent: Option<rowan::SyntaxToken<crate::Lang>> = None;
    for child in node.children_with_tokens() {
        let Some(token) = child.as_token().cloned() else {
            continue;
        };
        if !past_first_newline {
            if token.kind() == crate::SyntaxKind::NEWLINE {
                past_first_newline = true;
            }
            continue;
        }
        match token.kind() {
            crate::SyntaxKind::INDENT => {
                pending_indent = Some(token);
            }
            crate::SyntaxKind::NEWLINE => {
                // Blank line: the pending INDENT (if any) tells us
                // how far the blank line was padded. A tab makes the
                // line non-blank, so it carries no indentation claim.
                if let Some(ind) = pending_indent.take() {
                    if !ind.text().contains('\t') {
                        max_blank_indent = max_blank_indent.max(space_indent(ind.text()));
                    }
                }
            }
            _ => {
                // Non-blank content. Once a content line has set the
                // body's indent, a deeper blank line is content rather
                // than indentation and cannot raise the bar: H2RW's
                // 4-space blank contributes two spaces to a body
                // indented 2, as its expected events show. Only a blank
                // deeper than the body's own indent, seen before any
                // content, is 5LLU's error.
                if let Some(ind) = pending_indent.take() {
                    if body_indent.is_none() {
                        let indent = space_indent(ind.text());
                        body_indent = Some(indent);
                        if indent < max_blank_indent {
                            violations.push(Violation::error_at(Rule::Other, ind.text_range(), format!( "Block scalar content under-indented ({indent} spaces) relative to preceding blank line ({max_blank_indent} spaces)" )));
                            return;
                        }
                    }
                }
            }
        }
    }
}

/// Warn about plain scalars that YAML 1.1 resolved differently.
///
/// The document is valid either way, so these are warnings: they say the
/// text means one thing here and another to a 1.1 reader.
///
/// Bare-octal is the case that changes a value silently. `0755` was 493
/// in YAML 1.1 and is 755 in 1.2, with no syntax error either way, so a
/// file carried across versions changes meaning without complaint. The
/// 1.1 booleans (`yes`, `no`, `on`, `off`) are already strings here, as
/// 1.2 requires, but a 1.1 reader still takes them as booleans. A
/// leading-zero decimal with an 8 or 9 (`089`) is the reverse: an integer
/// here, but a string to a 1.1 reader.
///
/// The other numbers whose reading changed are covered too, matching
/// what [`crate::migrate::to_yaml_1_2`] rewrites: binary, signed hex,
/// base 60 and underscored numbers were 1.1 numbers and are strings here,
/// while `0o` octal and exponents without a `.` or sign (`1e3`) were 1.1
/// strings.
pub(super) fn check_legacy_yaml_1_1(node: &SyntaxNode, violations: &mut Vec<Violation>) {
    // Only a plain scalar resolves by its text; a quoted one is a string
    // in every version.
    let Some(token) = node.children_with_tokens().find_map(|c| {
        c.into_token().filter(|t| {
            matches!(
                t.kind(),
                crate::SyntaxKind::INT
                    | crate::SyntaxKind::FLOAT
                    | crate::SyntaxKind::STRING
                    | crate::SyntaxKind::BOOL
            )
        })
    }) else {
        return;
    };
    if is_quoted_scalar(&token) {
        return;
    }
    if let Some(message) = legacy_yaml_1_1_reading(token.text()) {
        violations.push(Violation::warning_at(
            Rule::LegacyYaml11,
            token.text_range(),
            message,
        ));
    }
}

/// How YAML 1.1 reads the plain scalar `text` differently from YAML 1.2,
/// or `None` if both read it the same.
fn legacy_yaml_1_1_reading(text: &str) -> Option<String> {
    if crate::ScalarValue::is_legacy_octal(text) {
        let sign = if text.starts_with('-') { "-" } else { "" };
        let digits = text.strip_prefix(['+', '-']).unwrap_or(text);
        let as_octal = crate::scalar::digits_to_decimal(digits, 8);
        let as_decimal = crate::scalar::digits_to_decimal(digits, 10);
        // The 1.2 spelling `0o755` is a string to a 1.1 reader; only the
        // decimal reads the same in both.
        return Some(format!(
            "`{text}` is YAML 1.1 octal, read as {sign}{as_octal}; YAML 1.2 reads it as {sign}{as_decimal}. Write `{sign}{as_octal}` to keep the octal meaning",
        ));
    }

    if let Some((after, kind)) = crate::migrate::yaml_1_1_number(text) {
        // The migration's `0o` octal is a string to a 1.1 reader, so the
        // decimal is suggested instead. Unsigned hex reads the same in both.
        let (value, suggestion, kind) = if kind == crate::SyntaxKind::FLOAT {
            (after.clone(), after, "float")
        } else if let Some(hex) = after.strip_prefix("0x") {
            (crate::scalar::digits_to_decimal(hex, 16), after, "integer")
        } else if let Some(octal) = after.strip_prefix("0o") {
            let value = crate::scalar::digits_to_decimal(octal, 8);
            (value.clone(), value, "integer")
        } else {
            (after.clone(), after, "integer")
        };
        return Some(format!(
            "`{text}` is the {kind} {value} in YAML 1.1; YAML 1.2 reads it as a string. Write `{suggestion}` to keep the {kind} meaning",
        ));
    }

    if crate::ScalarValue::is_leading_zero_decimal(text) {
        let minus = if text.starts_with('-') { "-" } else { "" };
        let digits = text.strip_prefix(['+', '-']).unwrap_or(text);
        let as_decimal = crate::scalar::digits_to_decimal(digits, 10);
        return Some(format!(
            "`{text}` is a string in YAML 1.1; YAML 1.2 reads it as the integer {minus}{as_decimal}. Write `'{text}'` to keep the string meaning",
        ));
    }

    if crate::migrate::yaml_1_1_string(text) {
        let reading = match text.strip_prefix("0o") {
            Some(octal) => {
                format!("the integer {}", crate::scalar::digits_to_decimal(octal, 8))
            }
            None => "a float".to_string(),
        };
        return Some(format!(
            "`{text}` is a string in YAML 1.1; YAML 1.2 reads it as {reading}. Write `'{text}'` to keep the string meaning",
        ));
    }

    if matches!(
        text,
        "yes" | "no" | "on" | "off" | "Yes" | "No" | "On" | "Off" | "YES" | "NO" | "ON" | "OFF"
    ) {
        return Some(format!("`{text}` is the string \"{text}\" in YAML 1.2, but a boolean to a YAML 1.1 reader; quote it or write true/false to be unambiguous"));
    }
    None
}

/// Check for trailing content after quoted strings
///
/// After a quoted string (double or single) closes, only whitespace, newlines,
/// or comments should follow. Additional content on the same line is invalid.
pub(super) fn check_trailing_content_after_quoted(
    node: &SyntaxNode,
    violations: &mut Vec<Violation>,
) {
    let mut found_quoted = false;
    let mut found_quote_end = false;
    let mut found_newline = false;

    for child in node.children_with_tokens() {
        if let rowan::NodeOrToken::Token(token) = child {
            match token.kind() {
                crate::SyntaxKind::STRING => {
                    let text = token.text();

                    // Check if this is a quoted string (starts with " or ')
                    if !found_quoted && is_quoted_scalar(&token) {
                        found_quoted = true;

                        // Check if quote ends in this same token
                        if text.len() > 1 && (text.ends_with('"') || text.ends_with('\'')) {
                            found_quote_end = true;
                        }
                    } else if found_quoted && !found_quote_end {
                        // Still inside the quoted string
                        if text.ends_with('"') || text.ends_with('\'') {
                            found_quote_end = true;
                        }
                    } else if found_quote_end && !found_newline {
                        // Found content after quoted string ended, before newline
                        violations.push(Violation::error_at(
                            Rule::Other,
                            token.text_range(),
                            "Trailing content after quoted string",
                        ));
                        return;
                    }
                }
                crate::SyntaxKind::NEWLINE => {
                    found_newline = true;
                }
                // Whitespace and comments are allowed after quoted strings
                crate::SyntaxKind::WHITESPACE
                | crate::SyntaxKind::INDENT
                | crate::SyntaxKind::COMMENT => {}
                // Any other token is content, such as the `[` of `''[`.
                _ if found_quote_end && !found_newline => {
                    violations.push(Violation::error_at(
                        Rule::Other,
                        token.text_range(),
                        "Trailing content after quoted string",
                    ));
                    return;
                }
                _ => {}
            }
        }
    }
}

/// Check for colons in plain scalar values
///
/// Plain scalars in block context cannot contain `: ` (colon followed by space)
/// without being quoted. This indicates an attempt to create a nested mapping
/// within a plain scalar, which is invalid.
pub(super) fn check_colon_in_plain_scalar(node: &SyntaxNode, violations: &mut Vec<Violation>) {
    let Some(colon) = child_token(node, crate::SyntaxKind::COLON) else {
        return;
    };

    // A quoted or block scalar's colons are content: the test suite's
    // 4WA9 is `- aaa: |2\n    xxx\n  bbb: |\n    xxx\n`.
    if node.first_token().is_some_and(|t| is_quoted_scalar(&t)) || is_block_scalar(node) {
        return;
    }

    // Check if this scalar is inside a VALUE node (not a KEY)
    // Keys can have plain text without issues, but values with colons need special handling
    let parent_is_value = node
        .parent()
        .is_some_and(|p| p.kind() == crate::SyntaxKind::VALUE);

    if parent_is_value {
        violations.push(Violation::error_at(
            Rule::Other,
            colon.text_range(),
            "Plain scalar value cannot contain mapping syntax (colon)",
        ));
    }
}

/// Check that a plain scalar does not open with an indicator.
///
/// The parser reads `a: ,b`, `- ]` and `[a, - b]` as plain scalars, but
/// none can start one: `,`, `]`, `}`, `%` and the reserved `@` and `` ` ``
/// never can, and a `-` can only
/// when a safe character follows it, as in `-1` (G5U8 is `[-, -]`).
/// The other [`crate::scalar::INDICATORS`] open a quoted or block scalar,
/// which other checks cover, or are lexed as tokens of their own.
/// Outside a flow collection a `- ` usually opens a block sequence
/// entry, but after a tag or anchor on the same line the parser reads
/// it as a plain scalar (SY6V is `&anchor - sequence entry`).
pub(super) fn check_plain_scalar_start(
    node: &SyntaxNode,
    in_flow: bool,
    violations: &mut Vec<Violation>,
) {
    use crate::SyntaxKind;

    let Some(first) = node.first_token() else {
        return;
    };
    let text = first.text();
    let unsafe_after_dash: &[char] = if in_flow {
        &[' ', '\t', '\r', '\n', ',', '[', ']', '{', '}']
    } else {
        &[' ', '\t', '\r', '\n']
    };
    // A `%` line is lexed as a DIRECTIVE, which
    // `check_directive_in_content` reports.
    let invalid = text.starts_with([',', ']', '}'])
        || (first.kind() == SyntaxKind::STRING && text.starts_with(['%', '@', '`']))
        || text.strip_prefix('-').is_some_and(|rest| {
            // `{-: 1}` is fine: the colon is safe, and the lexer
            // splits it off as its own token.
            (rest.is_empty() && first.next_token().map(|t| t.kind()) != Some(SyntaxKind::COLON))
                || rest.starts_with(unsafe_after_dash)
        });
    if invalid {
        let start = text.chars().next().unwrap_or_default();
        violations.push(Violation::error_at(
            Rule::Other,
            first.text_range(),
            format!("Plain scalar cannot start with {start:?}"),
        ));
    }
}

/// Check that a plain scalar in a flow collection does not hold a `{` or
/// `[`, which open a collection there. The parser keeps the one in
/// `[a{b]` in the scalar.
pub(super) fn check_flow_indicator_in_plain_scalar(
    node: &SyntaxNode,
    in_flow: bool,
    violations: &mut Vec<Violation>,
) {
    use crate::SyntaxKind;

    if !in_flow {
        return;
    }
    if let Some(open) = node
        .children_with_tokens()
        .filter_map(|c| c.into_token())
        .find(|t| matches!(t.kind(), SyntaxKind::LEFT_BRACE | SyntaxKind::LEFT_BRACKET))
    {
        violations.push(Violation::error_at(
            Rule::Other,
            open.text_range(),
            format!(
                "Plain scalar in a flow collection cannot contain {:?}",
                open.text()
            ),
        ));
    }
}

/// Check multiline quoted strings have proper indentation (QB6E)
pub(super) fn check_quoted_string_indentation(
    node: &SyntaxNode,
    scope: Scope,
    violations: &mut Vec<Violation>,
) {
    use crate::SyntaxKind;

    // Only check SCALAR nodes
    if node.kind() != SyntaxKind::SCALAR {
        return;
    }

    // Check if this is a quoted string that spans multiple lines
    let text = node.text().to_string();
    if !text.starts_with('"') && !text.starts_with('\'') {
        return; // Not a quoted string
    }

    if !text.contains(['\n', '\r']) {
        return; // Single line, no indentation to check
    }

    // A scalar that is the document's own node starts at column zero and
    // its continuations need clear no column, so there is nothing to
    // check: the test suite's 6WPF, TL85, Q8AD and NP9H all open a
    // quoted scalar at the document level. Inside a mapping value or a
    // sequence entry the continuation must clear the enclosing column.
    // A `---` or `...` at the start of a line ends the document, so it
    // cannot sit inside a quoted scalar: the test suite's 9MQT/01 is the
    // error `--- "a\n... x\nb"`. The lexer keeps the whole quoted body
    // in one token, so no document-marker rule sees it.
    // The marker has to be followed by whitespace or the line's end:
    // `---x` and `---"` are content (9MQT/00).
    if text.split(['\n', '\r']).skip(1).any(|line| {
        (line.starts_with("---") || line.starts_with("..."))
            && line[3..]
                .chars()
                .next()
                .map_or(true, |c| matches!(c, ' ' | '\t'))
    }) {
        violations.push(Violation::error_at(
            Rule::InvalidDocumentMarker,
            node.text_range(),
            "Document marker inside a quoted scalar",
        ));
        return;
    }

    // Continuation lines have to clear the enclosing block entry.
    let Some(required) = scope.continuation_indent else {
        return;
    };

    // Check each line after the first
    let lines: Vec<&str> = text.split(['\n', '\r']).collect();
    if lines.len() > 1 {
        // Continuation lines (between opening and closing quote) should have consistent indentation
        // In YAML, they should be indented at least as much as the opening line
        for (i, line) in lines.iter().enumerate().skip(1) {
            if i == lines.len() - 1 && line.trim().is_empty() {
                // Last line might just be the closing quote
                continue;
            }

            // A line opening with a tab is left to check_tab_usage.
            let leading_spaces = space_indent(line);
            if leading_spaces < required && !line.trim().is_empty() && !line.starts_with('\t') {
                violations.push(Violation::error_at(
                    Rule::InvalidIndentation,
                    node.text_range(),
                    "Wrong indented multiline quoted scalar",
                ));
                return;
            }
        }
    }
}
