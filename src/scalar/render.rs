//! Rendering scalars to YAML text and building their syntax nodes.

use super::{ScalarStyle, ScalarType, ScalarValue};
use crate::as_yaml::FlowQuoting;

/// Whether `text`, a scalar as [`ScalarValue::render`] writes it, is a
/// block scalar.
fn is_block_scalar(text: &str) -> bool {
    text.starts_with(['|', '>'])
}

/// Emit `text`, a block scalar as rendered, as the SCALAR the parser builds
/// for it: the header and body tokens as the lexer splits them, then the
/// line break ending the last line, which the parser keeps inside the
/// SCALAR.
// TODO: build the tokens directly rather than lexing rendered text, once the
// lexer gives block scalar bodies a simple, consistent token structure.
fn build_block_scalar(builder: &mut rowan::GreenNodeBuilder, text: &str) {
    use crate::lex::SyntaxKind;
    builder.start_node(SyntaxKind::SCALAR.into());
    for (kind, token) in crate::lex::lex(text) {
        builder.token(kind.into(), token);
    }
    builder.token(SyntaxKind::NEWLINE.into(), "\n");
    builder.finish_node();
}

impl ScalarValue {
    /// Render the scalar as a YAML string with proper escaping.
    ///
    /// A block scalar is laid out for a top-level entry, with its body
    /// indented two spaces. Use
    /// [`to_yaml_string_with_indent`](Self::to_yaml_string_with_indent) for
    /// a value nested deeper.
    pub fn to_yaml_string(&self) -> String {
        self.to_yaml_string_with_indent(0, 2)
    }

    /// As [`to_yaml_string`](Self::to_yaml_string), for a value of the
    /// collection whose entries sit at `parent_column`, with a block
    /// scalar's body `indent` spaces further right.
    ///
    /// `parent_column` is the column of the entry's key, or of the `-` of a
    /// sequence entry, and 0 for a top-level entry or the document root.
    /// It matters because a block scalar's indentation indicator (`|2`)
    /// counts from there: for `a:\n  k: <value>` pass 2.
    ///
    /// # Panics
    ///
    /// For a block scalar, if `indent` is 0, or above 9 when the value
    /// needs an indentation indicator (its first non-empty line starts
    /// with a space).
    pub fn to_yaml_string_with_indent(&self, parent_column: usize, indent: usize) -> String {
        self.render(parent_column, indent, None)
    }

    /// Render the scalar for a flow collection, quoted only where it has to
    /// be.
    pub(crate) fn to_flow_string(&self) -> String {
        self.render(0, 2, Some(FlowQuoting::Minimal))
    }

    /// The tag written in front of the content: the explicit one, or the
    /// one special data types carry when none is set.
    pub(super) fn rendered_tag(&self) -> Option<&str> {
        self.tag.as_deref().or(match self.scalar_type {
            #[cfg(feature = "base64")]
            ScalarType::Binary => Some(crate::tag::BINARY),
            ScalarType::Timestamp => Some(crate::tag::TIMESTAMP),
            ScalarType::Regex => Some(crate::tag::REGEX),
            _ => None,
        })
    }

    /// Render the scalar with its tag; inside a flow collection quoting
    /// strings as `flow` says, if it is set.
    fn render(&self, parent_column: usize, indent: usize, flow: Option<FlowQuoting>) -> String {
        let tag = self.rendered_tag();
        let content = self.render_content_in(parent_column, indent, flow);
        match tag {
            Some(tag) if content.is_empty() => tag.to_string(),
            Some(tag) => format!("{tag} {content}"),
            None => content,
        }
    }

    /// Render the scalar without any tag; inside a flow collection quoting
    /// strings as `flow` says, if it is set. Block scalars and plain
    /// scalars holding flow indicators can't appear there.
    fn render_content_in(
        &self,
        parent_column: usize,
        indent: usize,
        flow: Option<FlowQuoting>,
    ) -> String {
        // A plain string is quoted as needed when rendered, and `string`
        // picks the style itself, so a string with the style it would pick
        // was not given one explicitly.
        let auto_string = self.scalar_type == ScalarType::String
            && (self.style == ScalarStyle::Plain || self.style == Self::detect_style(&self.value));
        if flow == Some(FlowQuoting::Double) && auto_string {
            return self.to_double_quoted();
        }
        let content = self.render_content(parent_column, indent);
        let breaks_flow = content.starts_with(['|', '>'])
            || (!content.starts_with(['"', '\'']) && content.contains([',', '[', ']', '{', '}']));
        if flow.is_some() && breaks_flow {
            self.to_double_quoted()
        } else {
            content
        }
    }

    /// Render the scalar without any tag.
    fn render_content(&self, parent_column: usize, indent: usize) -> String {
        match self.style {
            ScalarStyle::Plain => {
                // Check if we need to quote based on type vs content
                match self.scalar_type {
                    // With an explicit tag, the content can stay plain even
                    // where it would otherwise resolve to another type.
                    ScalarType::String => {
                        let needs_quoting = if self.tag.is_some() {
                            Self::needs_quoting_as_tagged(&self.value)
                        } else {
                            Self::needs_quoting(&self.value)
                        };
                        if Self::needs_double_quotes(&self.value) {
                            self.to_double_quoted()
                        } else if needs_quoting {
                            self.to_single_quoted(parent_column + indent)
                        } else {
                            self.value.clone()
                        }
                    }
                    // For non-strings, output as plain (unquoted)
                    ScalarType::Integer
                    | ScalarType::Float
                    | ScalarType::Boolean
                    | ScalarType::Null
                    | ScalarType::Timestamp
                    | ScalarType::Regex => self.value.clone(),
                    #[cfg(feature = "base64")]
                    ScalarType::Binary => self.value.clone(),
                }
            }
            ScalarStyle::SingleQuoted => self.to_single_quoted(parent_column + indent),
            ScalarStyle::DoubleQuoted => self.to_double_quoted(),
            ScalarStyle::Literal => self.to_block_with_indent('|', parent_column, indent),
            ScalarStyle::Folded => self.to_block_with_indent('>', parent_column, indent),
        }
    }

    /// Render as a literal block scalar (`|`) whose body sits `indent`
    /// spaces past `parent_column`; see
    /// [`to_yaml_string_with_indent`](Self::to_yaml_string_with_indent).
    /// A value a block scalar can't hold, such as one with a carriage
    /// return, is double-quoted instead.
    ///
    /// # Panics
    ///
    /// As [`to_yaml_string_with_indent`](Self::to_yaml_string_with_indent).
    pub fn to_literal_with_indent(&self, parent_column: usize, indent: usize) -> String {
        self.to_block_with_indent('|', parent_column, indent)
    }

    /// Render as a folded block scalar (`>`) whose body sits `indent`
    /// spaces past `parent_column`; see
    /// [`to_yaml_string_with_indent`](Self::to_yaml_string_with_indent).
    /// A value a block scalar can't hold, such as one with a carriage
    /// return, is double-quoted instead.
    ///
    /// # Panics
    ///
    /// As [`to_yaml_string_with_indent`](Self::to_yaml_string_with_indent).
    pub fn to_folded_with_indent(&self, parent_column: usize, indent: usize) -> String {
        self.to_block_with_indent('>', parent_column, indent)
    }

    /// Render as a block scalar introduced by `marker` (`|` literal, `>` folded),
    /// with its body `indent` columns past `parent_column`.
    fn to_block_with_indent(&self, marker: char, parent_column: usize, indent: usize) -> String {
        assert!(indent > 0, "a block scalar's body must be indented");
        // A block scalar reads a carriage return as a line break and can't
        // escape anything, so such a value can only be double-quoted.
        if Self::needs_double_quotes(&self.value) {
            return self.to_double_quoted();
        }
        if self.value.is_empty() {
            return format!("{marker}-");
        }
        // Pick the chomping indicator that preserves the value's trailing
        // line breaks: a bare `|` clips to exactly one, `|-` strips them all
        // and `|+` keeps them. Otherwise reading the value back gains or
        // loses a newline. Clipping keeps the break after the last content
        // line, so a value of only line breaks has to keep them.
        let trailing = self.value.len() - self.value.trim_end_matches('\n').len();
        let chomp = match trailing {
            0 => "-",
            1 if self.value.len() > 1 => "",
            _ => "+",
        };
        let mut lines: Vec<&str> = self.value.split('\n').collect();
        if trailing > 0 {
            lines.pop();
        }
        // A reader takes the block's indentation from its first non-empty
        // line, so leading spaces there would be stripped as indentation
        // (or make later lines look dedented) unless an indicator says
        // where the body starts. It counts from the parent collection's
        // column.
        let needs_indicator = lines
            .iter()
            .find(|line| !line.is_empty())
            .is_some_and(|line| line.starts_with(' '));
        let indicator = if needs_indicator {
            assert!(
                indent <= 9,
                "indentation indicator {indent} does not fit in one digit"
            );
            indent.to_string()
        } else {
            String::new()
        };
        let indent_str = " ".repeat(parent_column + indent);
        let is_folded_text = |line: &str| !line.is_empty() && !line.starts_with([' ', '\t']);
        let mut body = Vec::with_capacity(lines.len());
        let mut prev_text: Option<&str> = None;
        for line in lines {
            if line.is_empty() {
                body.push(String::new());
                continue;
            }
            // A folded block turns a break between two unindented lines into
            // a space; an extra empty line keeps it a newline.
            if marker == '>' && is_folded_text(line) && prev_text.is_some_and(is_folded_text) {
                body.push(String::new());
            }
            prev_text = Some(line);
            body.push(format!("{indent_str}{line}"));
        }
        format!("{marker}{indicator}{chomp}\n{}", body.join("\n"))
    }

    /// The token kind for `text`, this scalar as rendered.
    fn token_kind(&self, text: &str) -> crate::lex::SyntaxKind {
        use crate::lex::SyntaxKind;
        if text.starts_with('\'') || text.starts_with('"') {
            return SyntaxKind::STRING;
        }
        match self.scalar_type {
            ScalarType::Integer => SyntaxKind::INT,
            ScalarType::Float => SyntaxKind::FLOAT,
            ScalarType::Boolean => SyntaxKind::BOOL,
            ScalarType::Null => SyntaxKind::NULL,
            _ => SyntaxKind::STRING,
        }
    }

    /// Emit the scalar, inside a flow collection quoting strings as `flow`
    /// says if it is set.
    pub(crate) fn build(
        &self,
        builder: &mut rowan::GreenNodeBuilder,
        indent: usize,
        flow: Option<FlowQuoting>,
    ) -> bool {
        use crate::lex::SyntaxKind;
        // A block scalar's body has to clear the column its entry sits at,
        // or the text it renders to is not the value it stood for:
        // `a:\n  b: |-\n  x\n` has an empty scalar and a stray `x`.
        if let Some(tag) = self.rendered_tag() {
            builder.start_node(SyntaxKind::TAGGED_NODE.into());
            builder.token(SyntaxKind::TAG.into(), tag);
            let content = self.render_content_in(indent, 2, flow);
            // Like the parser, give a tag with no content an empty scalar.
            let kind = if content.is_empty() {
                SyntaxKind::NULL
            } else {
                builder.token(SyntaxKind::WHITESPACE.into(), " ");
                if is_block_scalar(&content) {
                    build_block_scalar(builder, &content);
                    builder.finish_node();
                    return true;
                }
                self.token_kind(&content)
            };
            builder.start_node(SyntaxKind::SCALAR.into());
            builder.token(kind.into(), &content);
            builder.finish_node();
            builder.finish_node();
            return false;
        }
        // Render with proper YAML quoting/escaping. When the value needs
        // quotes (e.g. the string "null", or text containing ": "), the
        // rendered text starts with a quote and must be tokenized as a
        // STRING regardless of the semantic type.
        let text = self.render(indent, 2, flow);
        if is_block_scalar(&text) {
            build_block_scalar(builder, &text);
            return true;
        }
        builder.start_node(SyntaxKind::SCALAR.into());
        builder.token(self.token_kind(&text).into(), &text);
        builder.finish_node();
        false
    }
}
