//! YAML Language Server Protocol implementation.

#![deny(missing_docs)]
#![deny(unsafe_code)]

use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::Mutex;
use tower_lsp_server::jsonrpc::Result;
use tower_lsp_server::ls_types::*;
use tower_lsp_server::{Client, LanguageServer, LspService, Server};

mod position;
mod workspace;

use position::text_position_to_lsp_range;
use workspace::{SourceFile, Workspace};
use yaml_edit::validator::{Rule, Severity, Validator, Violation};
use yaml_edit::ParseErrorKind;

struct Backend {
    client: Client,
    workspace: Arc<Mutex<Workspace>>,
    files: Arc<Mutex<HashMap<Uri, SourceFile>>>,
}

/// Generate LSP diagnostics from parse errors and validation violations.
fn generate_diagnostics(
    parsed: &yaml_edit::Parse<yaml_edit::YamlFile>,
    source_text: &str,
) -> Vec<Diagnostic> {
    let mut diagnostics: Vec<Diagnostic> = parsed
        .positioned_errors()
        .iter()
        .map(|error| {
            let range = text_position_to_lsp_range(source_text, error.range);
            Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::ERROR),
                code: error
                    .code
                    .as_ref()
                    .map(|c| NumberOrString::String(c.clone())),
                source: Some("yaml-lsp".to_string()),
                message: error.message.clone(),
                ..Default::default()
            }
        })
        .collect();

    // Run the spec validator on each document
    let tree = parsed.tree();
    let syntax: &rowan::SyntaxNode<yaml_edit::Lang> =
        <yaml_edit::YamlFile as rowan::ast::AstNode>::syntax(&tree);

    let validator = Validator::new();
    let violations = validator.validate_syntax(syntax);

    for violation in violations {
        let range = match violation.text_range {
            Some(tp) => text_position_to_lsp_range(source_text, tp),
            None => Range {
                start: Position {
                    line: 0,
                    character: 0,
                },
                end: Position {
                    line: 0,
                    character: 0,
                },
            },
        };

        diagnostics.push(Diagnostic {
            range,
            severity: Some(match violation.severity {
                Severity::Error => DiagnosticSeverity::ERROR,
                Severity::Warning => DiagnosticSeverity::WARNING,
            }),
            code: Some(NumberOrString::String(format!("{:?}", violation.rule))),
            source: Some("yaml-lsp".to_string()),
            message: violation.message,
            ..Default::default()
        });
    }

    diagnostics
}

/// Run the validator and return all violations that have text ranges.
fn get_violations(parsed: &yaml_edit::Parse<yaml_edit::YamlFile>) -> Vec<Violation> {
    let tree = parsed.tree();
    let syntax: &rowan::SyntaxNode<yaml_edit::Lang> =
        <yaml_edit::YamlFile as rowan::ast::AstNode>::syntax(&tree);

    let validator = Validator::new();
    validator
        .validate_syntax(syntax)
        .into_iter()
        .filter(|v| v.text_range.is_some())
        .collect()
}

/// Generate a code action from a violation, if an automated fix is possible.
fn violation_to_code_action(
    violation: &Violation,
    source_text: &str,
    uri: &Uri,
) -> Option<CodeAction> {
    let tp = violation.text_range?;
    let violation_range = text_position_to_lsp_range(source_text, tp);
    let start = tp.start as usize;
    let end = tp.end as usize;

    let (title, edit_range, new_text) = match violation.rule {
        Rule::DuplicateKeys => {
            // Remove the entire duplicate mapping entry including trailing newline
            let remove_end = if end < source_text.len() && source_text.as_bytes()[end] == b'\n' {
                end + 1
            } else {
                end
            };
            let remove_range = text_position_to_lsp_range(
                source_text,
                yaml_edit::TextPosition::new(start as u32, remove_end as u32),
            );
            (
                format!("Remove duplicate: {}", violation.message),
                remove_range,
                String::new(),
            )
        }
        Rule::InvalidTabUsage => {
            // Replace the tab-containing token with spaces
            let token_text = &source_text[start..end];
            let replaced = token_text.replace('\t', "  ");
            (
                "Replace tabs with spaces".to_string(),
                violation_range,
                replaced,
            )
        }
        _ if violation.message == "Comment without whitespace separation" => {
            // Insert a space before the comment
            let insert_pos = text_position_to_lsp_range(
                source_text,
                yaml_edit::TextPosition::new(start as u32, start as u32),
            );
            (
                "Insert space before comment".to_string(),
                insert_pos,
                " ".to_string(),
            )
        }
        _ if violation.message == "Double comma in flow collection" => {
            // The text_range covers the entire flow collection node, but we need
            // to find and remove one of the consecutive commas. Scan the range
            // for ",," (possibly with whitespace between).
            let text_slice = &source_text[start..end];
            let mut i = 0;
            let bytes = text_slice.as_bytes();
            while i < bytes.len() {
                if bytes[i] == b',' {
                    let mut j = i + 1;
                    // skip whitespace
                    while j < bytes.len() && (bytes[j] == b' ' || bytes[j] == b'\t') {
                        j += 1;
                    }
                    if j < bytes.len() && bytes[j] == b',' {
                        // Remove from the first comma to just before the second
                        let remove_start = (start + i) as u32;
                        let remove_end = (start + j) as u32;
                        let remove_range = text_position_to_lsp_range(
                            source_text,
                            yaml_edit::TextPosition::new(remove_start, remove_end),
                        );
                        return Some(make_code_action(
                            "Remove extra comma".to_string(),
                            &violation.message,
                            violation_range,
                            remove_range,
                            String::new(),
                            uri,
                        ));
                    }
                }
                i += 1;
            }
            return None;
        }
        _ => return None,
    };

    Some(make_code_action(
        title,
        &violation.message,
        violation_range,
        edit_range,
        new_text,
        uri,
    ))
}

/// Build a quick-fix CodeAction.
fn make_code_action(
    title: String,
    diagnostic_message: &str,
    diagnostic_range: Range,
    edit_range: Range,
    new_text: String,
    uri: &Uri,
) -> CodeAction {
    let edit = TextEdit {
        range: edit_range,
        new_text,
    };

    let workspace_edit = WorkspaceEdit {
        changes: Some(vec![(uri.clone(), vec![edit])].into_iter().collect()),
        ..Default::default()
    };

    CodeAction {
        title,
        kind: Some(CodeActionKind::QUICKFIX),
        diagnostics: Some(vec![Diagnostic {
            range: diagnostic_range,
            severity: Some(DiagnosticSeverity::ERROR),
            source: Some("yaml-lsp".to_string()),
            message: diagnostic_message.to_string(),
            ..Default::default()
        }]),
        edit: Some(workspace_edit),
        ..Default::default()
    }
}

/// A suggested fix derived from a parse error.
struct ParseErrorFix {
    /// The text to insert
    insert_text: String,
    /// Human-readable title for the code action
    title: String,
    /// The diagnostic range this fix is associated with
    diagnostic_range: Range,
    /// The diagnostic message
    diagnostic_message: String,
    /// Where to insert (a zero-width range at the insertion point)
    insert_position: Position,
}

/// Extract actionable fixes from parse errors.
fn extract_parse_error_fixes(
    parsed: &yaml_edit::Parse<yaml_edit::YamlFile>,
    source_text: &str,
) -> Vec<ParseErrorFix> {
    let mut fixes = Vec::new();

    for error in parsed.positioned_errors() {
        let range = text_position_to_lsp_range(source_text, error.range);
        let insert_pos = range.start;

        match error.kind {
            ParseErrorKind::UnclosedFlowSequence => {
                fixes.push(ParseErrorFix {
                    insert_text: "]".to_string(),
                    title: "Insert missing ']'".to_string(),
                    diagnostic_range: range,
                    diagnostic_message: error.message.clone(),
                    insert_position: insert_pos,
                });
            }
            ParseErrorKind::UnclosedFlowMapping => {
                fixes.push(ParseErrorFix {
                    insert_text: "}".to_string(),
                    title: "Insert missing '}'".to_string(),
                    diagnostic_range: range,
                    diagnostic_message: error.message.clone(),
                    insert_position: insert_pos,
                });
            }
            ParseErrorKind::UnterminatedString => {
                // Determine which quote to insert by looking at the start of the range
                let start = error.range.start as usize;
                let quote_char =
                    if start < source_text.len() && source_text.as_bytes()[start] == b'\'' {
                        "'"
                    } else {
                        "\""
                    };

                // Insert before the trailing newline if present
                let end_offset = error.range.end as usize;
                let insert_offset = if end_offset > 0
                    && end_offset <= source_text.len()
                    && source_text.as_bytes()[end_offset - 1] == b'\n'
                {
                    end_offset - 1
                } else {
                    end_offset
                };
                let insert_pos = position::offset_to_position(source_text, insert_offset as u32);

                fixes.push(ParseErrorFix {
                    insert_text: quote_char.to_string(),
                    title: format!("Insert missing {quote_char}"),
                    diagnostic_range: range,
                    diagnostic_message: error.message.clone(),
                    insert_position: insert_pos,
                });
            }
            ParseErrorKind::Other => {}
        }
    }

    fixes
}

/// Generate semantic tokens from a parsed YAML file.
fn generate_semantic_tokens(
    parsed: &yaml_edit::Parse<yaml_edit::YamlFile>,
    source_text: &str,
) -> Vec<SemanticToken> {
    use rowan::ast::AstNode;
    use yaml_edit::SyntaxKind::*;

    let tree = parsed.tree();
    let syntax: &rowan::SyntaxNode<yaml_edit::Lang> = tree.syntax();
    let mut tokens = Vec::new();
    let mut prev_line = 0u32;
    let mut prev_start = 0u32;

    for node_or_token in syntax.descendants_with_tokens() {
        let (token_type, text_range) = match node_or_token {
            rowan::NodeOrToken::Token(ref token) => {
                let kind = token.kind();
                let type_index = match kind {
                    STRING | INT | FLOAT | BOOL | NULL => Some(0), // value
                    COMMENT => Some(1),                            // comment
                    TAG => Some(2),                                // tag
                    ANCHOR | REFERENCE => Some(3),                 // anchor/reference
                    DOC_START | DOC_END => Some(4),                // document marker
                    _ => None,
                };
                match type_index {
                    Some(idx) => (idx, token.text_range()),
                    None => continue,
                }
            }
            rowan::NodeOrToken::Node(ref node) => {
                let kind = node.kind();
                if kind == KEY {
                    if let Some(first_token) = node.first_token() {
                        let range = first_token.text_range();
                        (5u32, range) // key
                    } else {
                        continue;
                    }
                } else {
                    continue;
                }
            }
        };

        let start_offset = u32::from(text_range.start());
        let end_offset = u32::from(text_range.end());
        let pos = position::offset_to_position(source_text, start_offset);
        let length = end_offset - start_offset;

        let delta_line = pos.line - prev_line;
        let delta_start = if delta_line == 0 {
            pos.character - prev_start
        } else {
            pos.character
        };

        tokens.push(SemanticToken {
            delta_line,
            delta_start,
            length,
            token_type,
            token_modifiers_bitset: 0,
        });

        prev_line = pos.line;
        prev_start = pos.character;
    }

    tokens
}

/// Check if two LSP ranges overlap.
fn range_overlaps(a: &Range, b: &Range) -> bool {
    (a.start.line < b.end.line
        || (a.start.line == b.end.line && a.start.character <= b.end.character))
        && (b.start.line < a.end.line
            || (b.start.line == a.end.line && b.start.character <= a.end.character))
}

impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                code_action_provider: Some(CodeActionProviderCapability::Simple(true)),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensOptions(
                        SemanticTokensOptions {
                            work_done_progress_options: WorkDoneProgressOptions::default(),
                            legend: SemanticTokensLegend {
                                token_types: vec![
                                    SemanticTokenType::STRING,    // 0: values
                                    SemanticTokenType::COMMENT,   // 1: comments
                                    SemanticTokenType::DECORATOR, // 2: tags
                                    SemanticTokenType::VARIABLE,  // 3: anchors/references
                                    SemanticTokenType::KEYWORD,   // 4: doc markers
                                    SemanticTokenType::PROPERTY,  // 5: keys
                                ],
                                token_modifiers: vec![],
                            },
                            range: Some(false),
                            full: Some(SemanticTokensFullOptions::Bool(true)),
                        },
                    ),
                ),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "YAML LSP initialized!")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let mut workspace = self.workspace.lock().await;
        let source_file = workspace.update_file(
            params.text_document.uri.clone(),
            params.text_document.text.clone(),
        );

        self.files
            .lock()
            .await
            .insert(params.text_document.uri.clone(), source_file);

        let parsed = workspace.get_parsed(source_file);
        let diagnostics = generate_diagnostics(&parsed, &params.text_document.text);
        self.client
            .publish_diagnostics(params.text_document.uri, diagnostics, None)
            .await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let Some(changes) = params.content_changes.first() else {
            return;
        };

        let mut workspace = self.workspace.lock().await;
        let source_file =
            workspace.update_file(params.text_document.uri.clone(), changes.text.clone());

        self.files
            .lock()
            .await
            .insert(params.text_document.uri.clone(), source_file);

        let parsed = workspace.get_parsed(source_file);
        let source_text = workspace.source_text(source_file);
        let diagnostics = generate_diagnostics(&parsed, &source_text);
        self.client
            .publish_diagnostics(params.text_document.uri, diagnostics, None)
            .await;
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        self.files.lock().await.remove(&params.text_document.uri);
        // Clear diagnostics on close
        self.client
            .publish_diagnostics(params.text_document.uri, vec![], None)
            .await;
    }

    async fn code_action(&self, params: CodeActionParams) -> Result<Option<CodeActionResponse>> {
        let workspace = self.workspace.lock().await;
        let files = self.files.lock().await;

        let Some(source_file) = files.get(&params.text_document.uri) else {
            return Ok(None);
        };

        let parsed = workspace.get_parsed(*source_file);
        let source_text = workspace.source_text(*source_file);

        let mut actions = Vec::new();

        // Generate code actions from validator violations
        let violations = get_violations(&parsed);
        for violation in &violations {
            let tp = match violation.text_range {
                Some(tp) => tp,
                None => continue,
            };
            let violation_range = text_position_to_lsp_range(&source_text, tp);
            if !range_overlaps(&violation_range, &params.range) {
                continue;
            }
            if let Some(action) =
                violation_to_code_action(violation, &source_text, &params.text_document.uri)
            {
                actions.push(CodeActionOrCommand::CodeAction(action));
            }
        }

        // Offer fixes for parse errors (unclosed brackets, unterminated strings)
        let parse_fixes = extract_parse_error_fixes(&parsed, &source_text);
        for fix in parse_fixes {
            if !range_overlaps(&fix.diagnostic_range, &params.range) {
                continue;
            }

            let insert_range = Range {
                start: fix.insert_position,
                end: fix.insert_position,
            };

            actions.push(CodeActionOrCommand::CodeAction(make_code_action(
                fix.title,
                &fix.diagnostic_message,
                fix.diagnostic_range,
                insert_range,
                fix.insert_text,
                &params.text_document.uri,
            )));
        }

        if actions.is_empty() {
            Ok(None)
        } else {
            Ok(Some(actions))
        }
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let workspace = self.workspace.lock().await;
        let files = self.files.lock().await;

        let Some(source_file) = files.get(&params.text_document.uri) else {
            return Ok(None);
        };

        let parsed = workspace.get_parsed(*source_file);
        let source_text = workspace.source_text(*source_file);
        let tokens = generate_semantic_tokens(&parsed, &source_text);

        if tokens.is_empty() {
            Ok(None)
        } else {
            Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
                result_id: None,
                data: tokens,
            })))
        }
    }
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| Backend {
        client,
        workspace: Arc::new(Mutex::new(Workspace::new())),
        files: Arc::new(Mutex::new(HashMap::new())),
    });

    Server::new(stdin, stdout, socket).serve(service).await;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_generate_diagnostics_valid_yaml() {
        let text = "name: Alice\nage: 30\n";
        let parsed = yaml_edit::Parse::parse_yaml(text);
        let diagnostics = generate_diagnostics(&parsed, text);
        assert!(diagnostics.is_empty());
    }

    #[test]
    fn test_generate_diagnostics_reports_errors() {
        // Use content that actually produces parse errors
        let text = "key: [unterminated\n";
        let parsed = yaml_edit::Parse::parse_yaml(text);
        if parsed.has_errors() {
            let diagnostics = generate_diagnostics(&parsed, text);
            assert!(!diagnostics.is_empty());
            assert_eq!(diagnostics[0].severity, Some(DiagnosticSeverity::ERROR));
            assert_eq!(diagnostics[0].source, Some("yaml-lsp".to_string()));
        }
    }

    #[test]
    fn test_generate_diagnostics_includes_violations() {
        // Duplicate keys should produce a diagnostic
        let text = "key: value1\nkey: value2\n";
        let parsed = yaml_edit::Parse::parse_yaml(text);
        let diagnostics = generate_diagnostics(&parsed, text);

        let dup_diags: Vec<_> = diagnostics
            .iter()
            .filter(|d| d.code == Some(NumberOrString::String("DuplicateKeys".to_string())))
            .collect();
        assert!(
            !dup_diags.is_empty(),
            "should produce duplicate key diagnostics"
        );
    }

    #[test]
    fn test_duplicate_key_code_action() {
        let text = "key: value1\nkey: value2\n";
        let parsed = yaml_edit::Parse::parse_yaml(text);

        let violations = get_violations(&parsed);
        let dup = violations
            .iter()
            .find(|v| v.rule == Rule::DuplicateKeys)
            .expect("should find duplicate key violation");
        assert!(
            dup.text_range.is_some(),
            "violation should have a text range"
        );

        let uri: Uri = "file:///test.yaml".parse().unwrap();
        let action = violation_to_code_action(dup, text, &uri);
        assert!(action.is_some(), "should produce a code action");
        assert!(
            action.unwrap().title.starts_with("Remove duplicate:"),
            "action title should describe the fix"
        );
    }

    #[test]
    fn test_tab_replacement_code_action() {
        let text = "key: value\n\tindented: bad\n";
        let parsed = yaml_edit::Parse::parse_yaml(text);

        let violations = get_violations(&parsed);
        let tab = violations.iter().find(|v| v.rule == Rule::InvalidTabUsage);

        if let Some(tab_violation) = tab {
            let uri: Uri = "file:///test.yaml".parse().unwrap();
            let action = violation_to_code_action(tab_violation, text, &uri).unwrap();
            assert_eq!(action.title, "Replace tabs with spaces");
        }
    }

    #[test]
    fn test_generate_semantic_tokens_basic() {
        let text = "name: Alice\n";
        let parsed = yaml_edit::Parse::parse_yaml(text);
        let tokens = generate_semantic_tokens(&parsed, text);
        assert!(
            !tokens.is_empty(),
            "should produce semantic tokens for basic YAML"
        );
    }

    #[test]
    fn test_generate_semantic_tokens_with_comment() {
        let text = "# this is a comment\nkey: value\n";
        let parsed = yaml_edit::Parse::parse_yaml(text);
        let tokens = generate_semantic_tokens(&parsed, text);

        assert!(
            tokens.len() >= 2,
            "should produce tokens for comment and key-value"
        );
    }

    #[test]
    fn test_generate_semantic_tokens_with_anchor() {
        let text = "anchor: &ref value\nalias: *ref\n";
        let parsed = yaml_edit::Parse::parse_yaml(text);
        let tokens = generate_semantic_tokens(&parsed, text);
        assert!(!tokens.is_empty());
    }

    #[test]
    fn test_generate_semantic_tokens_document_markers() {
        let text = "---\nkey: value\n...\n";
        let parsed = yaml_edit::Parse::parse_yaml(text);
        let tokens = generate_semantic_tokens(&parsed, text);
        assert!(!tokens.is_empty());
    }

    #[test]
    fn test_parse_error_fix_unclosed_bracket() {
        let text = "items: [a, b\n";
        let parsed = yaml_edit::Parse::parse_yaml(text);
        let fixes = extract_parse_error_fixes(&parsed, text);

        assert_eq!(fixes.len(), 1);
        assert_eq!(fixes[0].insert_text, "]");
        assert_eq!(fixes[0].title, "Insert missing ']'");
    }

    #[test]
    fn test_parse_error_fix_unclosed_brace() {
        let text = "map: {a: 1\n";
        let parsed = yaml_edit::Parse::parse_yaml(text);
        let fixes = extract_parse_error_fixes(&parsed, text);

        assert_eq!(fixes.len(), 1);
        assert_eq!(fixes[0].insert_text, "}");
        assert_eq!(fixes[0].title, "Insert missing '}'");
    }

    #[test]
    fn test_parse_error_fix_unterminated_double_quote() {
        let text = "name: \"hello\n";
        let parsed = yaml_edit::Parse::parse_yaml(text);
        let fixes = extract_parse_error_fixes(&parsed, text);

        assert_eq!(fixes.len(), 1);
        assert_eq!(fixes[0].insert_text, "\"");
        assert_eq!(fixes[0].title, "Insert missing \"");
    }

    #[test]
    fn test_parse_error_fix_unterminated_single_quote() {
        let text = "name: 'hello\n";
        let parsed = yaml_edit::Parse::parse_yaml(text);
        let fixes = extract_parse_error_fixes(&parsed, text);

        assert_eq!(fixes.len(), 1);
        assert_eq!(fixes[0].insert_text, "'");
        assert_eq!(fixes[0].title, "Insert missing '");
    }

    #[test]
    fn test_no_parse_error_fixes_for_valid_yaml() {
        let text = "name: Alice\nage: 30\n";
        let parsed = yaml_edit::Parse::parse_yaml(text);
        let fixes = extract_parse_error_fixes(&parsed, text);
        assert!(fixes.is_empty());
    }

    #[test]
    fn test_workspace_diagnostics_integration() {
        let mut workspace = Workspace::new();
        let url: Uri = "file:///test.yaml".parse().unwrap();
        let text = "valid: yaml\n";

        let file = workspace.update_file(url, text.to_string());
        let parsed = workspace.get_parsed(file);
        let source_text = workspace.source_text(file);
        let diagnostics = generate_diagnostics(&parsed, &source_text);

        assert!(diagnostics.is_empty());
    }
}
