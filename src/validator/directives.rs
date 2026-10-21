//! Checks on directives and the documents they introduce.

use super::{directive_groups, find_root, Rule, Violation};
use crate::nodes::{has_child_token, is_block_scalar, is_quoted_scalar};
use crate::yaml::SyntaxNode;

/// The handle a `%TAG` directive declares.
fn tag_handle(directive: &SyntaxNode) -> Option<String> {
    let text = directive.text().to_string();
    let mut words = text.split_whitespace();
    if words.next() != Some("%TAG") {
        return None;
    }
    words.next().map(str::to_string)
}

/// Does this subtree hold real YAML content, as opposed to only whitespace,
/// newlines and document markers?
fn has_content(node: &SyntaxNode) -> bool {
    node.descendants().any(|n| {
        matches!(
            n.kind(),
            crate::SyntaxKind::MAPPING
                | crate::SyntaxKind::SEQUENCE
                | crate::SyntaxKind::SCALAR
                | crate::SyntaxKind::STRING
                | crate::SyntaxKind::TAGGED_NODE
        )
    })
}

/// Check that a `%TAG` shorthand is only used in the document that
/// declares it.
///
/// A directive applies to the document it introduces, not to the whole
/// stream, so `!prefix!` declared before the first `---` is undefined in
/// every later document. The YAML test suite's QLJ7 is exactly that.
pub(super) fn check_tag_shorthand_scope(node: &SyntaxNode, violations: &mut Vec<Violation>) {
    // Handles declared for earlier documents. A document that declares
    // the same handle again (the test suite's 5TYM) may use it.
    let mut declared_earlier: Vec<String> = Vec::new();

    for (directives, document) in directive_groups(&find_root(node)) {
        let own: Vec<String> = directives.iter().filter_map(tag_handle).collect();
        if let Some(document) = document {
            for tag in document
                .descendants_with_tokens()
                .filter_map(|c| c.into_token())
                .filter(|t| t.kind() == crate::SyntaxKind::TAG)
            {
                // Only a named handle (`!prefix!A`) can be undefined;
                // `!` and `!!` fall back to their defaults.
                let text = tag.text();
                let Some(named) = text
                    .strip_prefix('!')
                    .and_then(|rest| rest.find('!'))
                    .filter(|&i| i > 0)
                    .map(|i| &text[..i + 2])
                else {
                    continue;
                };
                if let Some(handle) = declared_earlier
                    .iter()
                    .find(|h| h.as_str() == named && !own.contains(h))
                {
                    violations.push(Violation::error_at(
                        Rule::Other,
                        tag.text_range(),
                        format!(
                            "Tag shorthand {handle} is only defined in the document that declares it"
                        ),
                    ));
                }
            }
        }
        declared_earlier.extend(own);
    }
}

/// Check the directives that introduce `document`, or that end the
/// stream when it is `None`.
pub(super) fn check_directives(
    directives: &[SyntaxNode],
    document: Option<&SyntaxNode>,
    violations: &mut Vec<Violation>,
) {
    let Some(last) = directives.last() else {
        return;
    };

    // `l-directive-document` is the directives followed by an explicit
    // document, so they need a `---` (B63P's `%YAML 1.2\n...\n` and
    // 9MMA have none). The document itself may be empty.
    if !document.is_some_and(|d| has_child_token(d, |k| k == crate::SyntaxKind::DOC_START)) {
        violations.push(Violation::error_at(
            Rule::Other,
            last.text_range(),
            "Directive without document start marker (---)",
        ));
    }

    let mut seen_yaml = false;
    let mut tag_handles: Vec<String> = Vec::new();
    for directive in directives {
        let text = directive.text().to_string();
        let mut words = text.split_whitespace();
        match words.next() {
            Some("%YAML") => {
                if seen_yaml {
                    violations.push(Violation::error_at(
                        Rule::Other,
                        directive.text_range(),
                        "Duplicate %YAML directive",
                    ));
                }
                seen_yaml = true;
                // One `major.minor` version, then at most a comment,
                // which needs whitespace before its `#`: H7TQ's
                // `%YAML 1.2 foo` and MUS6/00's `%YAML 1.1#...` are errors.
                let is_version = |v: &str| {
                    v.split_once('.').is_some_and(|(major, minor)| {
                        [major, minor]
                            .iter()
                            .all(|n| !n.is_empty() && n.bytes().all(|b| b.is_ascii_digit()))
                    })
                };
                if !words.next().is_some_and(is_version)
                    || words.next().is_some_and(|w| !w.starts_with('#'))
                {
                    violations.push(Violation::error_at(
                        Rule::Other,
                        directive.text_range(),
                        "Malformed %YAML directive",
                    ));
                }
            }
            // A handle may be declared once per document.
            Some("%TAG") => {
                if let Some(handle) = words.next() {
                    if tag_handles.iter().any(|h| h == handle) {
                        violations.push(Violation::error_at(
                            Rule::Other,
                            directive.text_range(),
                            format!("Duplicate %TAG directive for {handle}"),
                        ));
                    }
                    tag_handles.push(handle.to_string());
                }
            }
            _ => {}
        }
    }
}

/// Check for directives appearing after documents without document end marker (...)
///
/// Per YAML spec, if a directive appears after document content, the document
/// must be explicitly ended with `...` before the directive.
pub(super) fn check_directive_after_document(node: &SyntaxNode, violations: &mut Vec<Violation>) {
    use crate::SyntaxKind;

    let check_node = find_root(node);

    // Track if we've seen a document with content
    let mut seen_document_with_content = false;

    for child in check_node.children() {
        match child.kind() {
            SyntaxKind::DOCUMENT => {
                // Whether this document carries a DOC_END is rechecked from
                // the DIRECTIVE arm below, by walking back to this sibling.
                if has_content(&child) {
                    seen_document_with_content = true;
                }
            }
            SyntaxKind::DIRECTIVE if seen_document_with_content => {
                // The previous document has to end with `...`, which
                // can be its last child or, as in 6WLZ, follow it at
                // the stream's level.
                let mut found_doc_with_end = false;
                for prev in child.siblings_with_tokens(rowan::Direction::Prev).skip(1) {
                    if prev.kind() == SyntaxKind::DOC_END {
                        found_doc_with_end = true;
                        break;
                    }
                    if let Some(prev) = prev.as_node().filter(|n| n.kind() == SyntaxKind::DOCUMENT)
                    {
                        found_doc_with_end = has_child_token(prev, |k| k == SyntaxKind::DOC_END);
                        break;
                    }
                }

                if !found_doc_with_end {
                    violations.push(Violation::error_at(
                        Rule::Other,
                        child.text_range(),
                        "Directive after document requires document end marker (...)",
                    ));
                }
            }
            _ => {}
        }
    }
}

/// Check for directive tokens inside document content.
///
/// When the parser encounters `%YAML 1.2` after a `---` without a preceding `...`,
/// it parses the directive as scalar content. This check catches that case.
pub(super) fn check_directive_in_content(node: &SyntaxNode, violations: &mut Vec<Violation>) {
    use crate::SyntaxKind;

    // A block scalar's body is literal text, though the lexer still
    // gives a `%` line there as a DIRECTIVE token: W4TN's
    // `--- |\n%!PS-Adobe-2.0`.
    if is_block_scalar(node) {
        return;
    }

    // A `%` line may continue a plain scalar (XLQ9 reads
    // `---\nscalar\n%YAML 1.2\n` as `scalar %YAML 1.2`), but `%` cannot
    // start one (MUS6/01), nor can the line follow a closed quoted
    // scalar (9HCY).
    let mut prev: Option<crate::nodes::SyntaxToken> = None;
    for token in node.children_with_tokens().filter_map(|c| c.into_token()) {
        if token.kind() == SyntaxKind::DIRECTIVE {
            let continues_plain = prev
                .as_ref()
                .is_some_and(|p| p.kind() == SyntaxKind::STRING && !is_quoted_scalar(p));
            if !continues_plain {
                violations.push(Violation::error_at(Rule::Other, node.text_range(), "Directive in document content (missing document end marker `...` before directive)".to_string()));
                return;
            }
        }
        if !matches!(
            token.kind(),
            SyntaxKind::NEWLINE | SyntaxKind::WHITESPACE | SyntaxKind::INDENT
        ) {
            prev = Some(token);
        }
    }
}
