use tower_lsp_server::ls_types::{Position, Range};
use yaml_edit::TextPosition;

/// Convert a byte offset to an LSP Position (0-indexed line/character).
pub fn offset_to_position(text: &str, offset: u32) -> Position {
    let offset = offset as usize;
    let mut line = 0u32;
    let mut line_start_offset = 0usize;

    for (i, ch) in text.char_indices() {
        if i >= offset {
            break;
        }
        if ch == '\n' {
            line += 1;
            line_start_offset = i + 1;
        }
    }

    let character = (offset.saturating_sub(line_start_offset)) as u32;
    Position { line, character }
}

/// Convert a yaml-edit TextPosition to an LSP Range.
pub fn text_position_to_lsp_range(text: &str, pos: TextPosition) -> Range {
    Range {
        start: offset_to_position(text, pos.start),
        end: offset_to_position(text, pos.end),
    }
}

/// Convert an LSP Position to a byte offset.
pub fn position_to_offset(text: &str, position: Position) -> u32 {
    let mut line = 0u32;
    let mut offset = 0usize;

    for (i, ch) in text.char_indices() {
        if line == position.line {
            return (i + position.character as usize) as u32;
        }
        offset = i;
        if ch == '\n' {
            line += 1;
        }
    }

    // Beyond the end of text
    let _ = offset;
    text.len() as u32
}

/// Convert an LSP Range to a yaml-edit TextPosition.
pub fn lsp_range_to_text_position(text: &str, range: &Range) -> TextPosition {
    let start = position_to_offset(text, range.start);
    let end = position_to_offset(text, range.end);
    TextPosition::new(start, end)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_offset_to_position_first_line() {
        let text = "hello: world\n";
        let pos = offset_to_position(text, 0);
        assert_eq!(
            pos,
            Position {
                line: 0,
                character: 0
            }
        );

        let pos = offset_to_position(text, 7);
        assert_eq!(
            pos,
            Position {
                line: 0,
                character: 7
            }
        );
    }

    #[test]
    fn test_offset_to_position_second_line() {
        let text = "hello: world\nfoo: bar\n";
        let pos = offset_to_position(text, 13);
        assert_eq!(
            pos,
            Position {
                line: 1,
                character: 0
            }
        );

        let pos = offset_to_position(text, 18);
        assert_eq!(
            pos,
            Position {
                line: 1,
                character: 5
            }
        );
    }

    #[test]
    fn test_position_to_offset_roundtrip() {
        let text = "line one\nline two\nline three\n";

        for offset in [0u32, 5, 9, 14, 18, 25] {
            let pos = offset_to_position(text, offset);
            let back = position_to_offset(text, pos);
            assert_eq!(back, offset, "roundtrip failed for offset {offset}");
        }
    }

    #[test]
    fn test_text_position_to_lsp_range() {
        let text = "key: value\nother: stuff\n";
        let tp = TextPosition::new(5, 10);
        let range = text_position_to_lsp_range(text, tp);
        assert_eq!(
            range.start,
            Position {
                line: 0,
                character: 5
            }
        );
        assert_eq!(
            range.end,
            Position {
                line: 0,
                character: 10
            }
        );
    }
}
