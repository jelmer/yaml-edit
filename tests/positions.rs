//! Where parsed nodes are in the text: byte ranges and line and column
//! positions.

use std::str::FromStr;
use yaml_edit::{Document, LineColumn, TextPosition, YamlFile};

#[test]
fn document_position() {
    let text = "name: Alice\nage: 30";
    let doc = Document::from_str(text).unwrap();

    let start = doc.start_position(text);
    assert_eq!(start.line, 1);
    assert_eq!(start.column, 1);

    assert_eq!(doc.byte_range(), TextPosition::new(0, 19));
}

#[test]
fn mapping_position() {
    let text = "server:\n  host: localhost\n  port: 8080";
    let doc = Document::from_str(text).unwrap();
    let mapping = doc.as_mapping().unwrap();

    let start = mapping.start_position(text);
    assert_eq!(start.line, 1);
    assert_eq!(start.column, 1);

    let server_mapping = mapping.get_mapping("server").unwrap();
    assert_eq!(server_mapping.start_position(text), LineColumn::new(2, 3));
}

#[test]
fn scalar_position_via_nodes() {
    let text = "name: Alice\nage: 30";
    let doc = Document::from_str(text).unwrap();
    let mapping = doc.as_mapping().unwrap();

    let entries: Vec<_> = mapping.entries().collect();
    assert_eq!(entries.len(), 2);

    let first_entry = &entries[0];
    assert_eq!(first_entry.key_node().unwrap().to_string(), "name");
    assert_eq!(first_entry.value_node().unwrap().to_string(), "Alice");
}

#[test]
fn sequence_position() {
    let text = "items:\n  - apple\n  - banana";
    let doc = Document::from_str(text).unwrap();
    let mapping = doc.as_mapping().unwrap();

    let items_node = mapping.get("items").unwrap();
    assert_eq!(items_node.byte_range(), TextPosition::new(9, 27));
    assert_eq!(
        items_node.as_sequence().unwrap().start_position(text),
        LineColumn::new(2, 3)
    );
}

#[test]
fn multiline_document_byte_offsets() {
    // The leading comment is part of the document; its mapping starts
    // on the next line.
    let text = "# Comment\nname: Alice\n\nage: 30";
    let doc = YamlFile::from_str(text).unwrap().document().unwrap();

    let range = doc.byte_range();
    assert_eq!(range.start, 0);
    assert_eq!(range.end, 30);

    let start = doc.start_position(text);
    assert_eq!(start.line, 1);
    assert_eq!(start.column, 1);

    let mapping = doc.as_mapping().unwrap();
    assert_eq!(mapping.byte_range().start, 10);
    let start = mapping.start_position(text);
    assert_eq!(start.line, 2);
    assert_eq!(start.column, 1);
}

#[test]
fn nested_mapping_byte_ranges() {
    let text = "server:\n  database:\n    host: localhost";
    let doc = Document::from_str(text).unwrap();
    let mapping = doc.as_mapping().unwrap();

    let server_mapping = mapping.get_mapping("server").unwrap();
    assert_eq!(server_mapping.byte_range(), TextPosition::new(10, 39));
    assert_eq!(server_mapping.start_position(text), LineColumn::new(2, 3));
}

#[test]
fn mapping_entry_byte_range() {
    let text = "a: 1  # one\nbb: 2\n";
    let doc = Document::from_str(text).unwrap();
    let entries: Vec<_> = doc.as_mapping().unwrap().entries().collect();

    let ranges: Vec<_> = entries.iter().map(|e| e.byte_range()).collect();
    assert_eq!(
        ranges,
        vec![TextPosition::new(0, 12), TextPosition::new(12, 18)]
    );
    assert_eq!(entries[1].start_position(text), LineColumn::new(2, 1));
}

#[test]
fn yaml_node_byte_range() {
    let text = "- foo  # c\n- &x [1, 2]\n- !t bar\n";
    let doc = Document::from_str(text).unwrap();
    let ranges: Vec<_> = doc
        .as_sequence()
        .unwrap()
        .values()
        .map(|v| v.byte_range())
        .collect();
    assert_eq!(
        ranges,
        vec![
            TextPosition::new(2, 5),
            TextPosition::new(16, 22),
            TextPosition::new(25, 31)
        ]
    );
}

#[test]
fn document_end_position() {
    let text = "key: value";
    let doc = Document::from_str(text).unwrap();

    let start = doc.start_position(text);
    let end = doc.end_position(text);

    assert_eq!(start.line, 1);
    assert!(end.column >= start.column);
}

#[test]
fn mapping_end_position() {
    let text = "a: 1\nb: 2";
    let doc = Document::from_str(text).unwrap();
    let mapping = doc.as_mapping().unwrap();

    let start = mapping.start_position(text);
    let end = mapping.end_position(text);

    assert_eq!(start.line, 1);
    assert_eq!(end.line, 2);
}

#[test]
fn find_entry_by_offset_inside_entry_range() {
    let text = "Reference:\n  - Type: Book\n";
    let doc = Document::from_str(text).unwrap();
    let mapping = doc.as_mapping().unwrap();
    let entry = mapping.find_entry_by_offset(text, 15).unwrap();
    assert!(entry.key_matches("Reference"));
}

#[test]
fn find_entry_by_offset_indented_trailing_blank_line() {
    let text = "Reference:\n  - Type: Book\n    \n";
    let doc = Document::from_str(text).unwrap();
    let mapping = doc.as_mapping().unwrap();
    let entry = mapping.find_entry_by_offset(text, 30).unwrap();
    assert!(entry.key_matches("Reference"));
}

#[test]
fn find_entry_by_offset_unindented_blank_line_is_outside() {
    let text = "Reference:\n  - Type: Book\n\n";
    let doc = Document::from_str(text).unwrap();
    let mapping = doc.as_mapping().unwrap();
    assert!(mapping.find_entry_by_offset(text, 26).is_none());
}

#[test]
fn find_entry_by_offset_hole_between_entries_attributed_to_preceding() {
    // Offset 29 sits in the indented "    \n" between Reference's end
    // and Name's start; column 4 is past Reference's key column.
    let text = "Reference:\n  - Type: Book\n    \nName: foo\n";
    let doc = Document::from_str(text).unwrap();
    let mapping = doc.as_mapping().unwrap();
    let entry = mapping.find_entry_by_offset(text, 29).unwrap();
    assert!(entry.key_matches("Reference"));
}

#[test]
fn find_entry_by_offset_at_start_of_first_entry() {
    let text = "Reference:\n";
    let doc = Document::from_str(text).unwrap();
    let mapping = doc.as_mapping().unwrap();
    let entry = mapping.find_entry_by_offset(text, 0).unwrap();
    assert!(entry.key_matches("Reference"));
}
