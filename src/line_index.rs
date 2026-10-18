//! Conversion of byte offsets into line and column numbers.

use crate::LineColumn;

/// The line starts of a text, for turning byte offsets into lines and
/// columns.
///
/// Line breaks are those of YAML 1.2 (`b-break`): `\n`, `\r` and `\r\n` each
/// end a line. Columns count Unicode scalar values. A byte order mark at the
/// start of a line is a document prefix rather than content, so it takes no
/// column.
#[derive(Debug, Clone)]
pub(crate) struct LineIndex<T: AsRef<str>> {
    text: T,
    line_starts: Vec<usize>,
}

impl<T: AsRef<str>> LineIndex<T> {
    pub(crate) fn new(text: T) -> Self {
        let bytes = text.as_ref().as_bytes();
        let mut line_starts = vec![0];
        for (i, &b) in bytes.iter().enumerate() {
            if b == b'\n' || (b == b'\r' && bytes.get(i + 1) != Some(&b'\n')) {
                line_starts.push(i + 1);
            }
        }
        Self { text, line_starts }
    }

    pub(crate) fn text(&self) -> &str {
        self.text.as_ref()
    }

    /// The 1-based line and column of `offset`.
    ///
    /// An offset past the end of the text gives the position of the end. One
    /// inside a multi-byte character counts that character as passed.
    pub(crate) fn line_column(&self, offset: usize) -> LineColumn {
        let line = self.line_of(offset);
        LineColumn::new(line + 1, self.column_in_line(line, offset) + 1)
    }

    /// The 0-based column of `offset`.
    pub(crate) fn column(&self, offset: usize) -> usize {
        self.column_in_line(self.line_of(offset), offset)
    }

    /// The byte offset at which the line holding `offset` starts.
    pub(crate) fn line_start(&self, offset: usize) -> usize {
        self.line_starts[self.line_of(offset)]
    }

    fn line_of(&self, offset: usize) -> usize {
        self.line_starts.partition_point(|&s| s <= offset) - 1
    }

    fn column_in_line(&self, line: usize, offset: usize) -> usize {
        let start = self.line_starts[line];
        self.text()[start..]
            .char_indices()
            .take_while(|&(i, _)| start + i < offset)
            .skip_while(|&(_, c)| c == '\u{FEFF}')
            .count()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_line_breaks() {
        let index = LineIndex::new("a\nb\rc\r\nd");
        let positions: Vec<_> = (0..=9).map(|i| index.line_column(i)).collect();
        assert_eq!(
            positions,
            vec![
                LineColumn::new(1, 1),
                LineColumn::new(1, 2),
                LineColumn::new(2, 1),
                LineColumn::new(2, 2),
                LineColumn::new(3, 1),
                LineColumn::new(3, 2),
                LineColumn::new(3, 3),
                LineColumn::new(4, 1),
                LineColumn::new(4, 2),
                LineColumn::new(4, 2),
            ]
        );
    }

    #[test]
    fn test_line_start() {
        let index = LineIndex::new("ab\rcd\r\nef");
        let starts: Vec<_> = (0..9).map(|i| index.line_start(i)).collect();
        assert_eq!(starts, vec![0, 0, 0, 3, 3, 3, 3, 7, 7]);
    }

    #[test]
    fn test_columns_count_chars() {
        let index = LineIndex::new("\u{e9}\u{e9}x");
        let columns: Vec<_> = (0..=5).map(|i| index.line_column(i).column).collect();
        assert_eq!(columns, vec![1, 2, 2, 3, 3, 4]);
    }

    #[test]
    fn test_leading_bom_takes_no_column() {
        let index = LineIndex::new("\u{FEFF}a\u{FEFF}");
        let columns: Vec<_> = [0, 3, 4, 7].map(|i| index.line_column(i).column).to_vec();
        assert_eq!(columns, vec![1, 1, 2, 3]);
    }
}
