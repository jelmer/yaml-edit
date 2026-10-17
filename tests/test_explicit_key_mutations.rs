mod common;
use common::{assert_cst_ok, assert_file_cst_ok};
use rowan::ast::AstNode;
use std::str::FromStr;
use yaml_edit::{debug, Mapping, YamlFile};

/// Check that `file` has the same tree shape as a re-parse of its text.
#[track_caller]
fn assert_parser_shaped(file: &YamlFile) {
    assert_file_cst_ok(file);
    let reparsed = YamlFile::from_str(&file.to_string()).unwrap();
    assert_eq!(
        debug::tree_to_string(file.syntax()),
        debug::tree_to_string(reparsed.syntax())
    );
}

/// Apply `edit` to the root mapping of `source`, check the result is shaped
/// as the parser would shape it, and return its text.
#[track_caller]
fn edit_root(source: &str, edit: impl FnOnce(&Mapping)) -> String {
    let file = YamlFile::from_str(source).unwrap();
    edit(&file.document().unwrap().as_mapping().unwrap());
    assert_parser_shaped(&file);
    file.to_string()
}

#[test]
fn test_explicit_key_mutations() {
    use std::str::FromStr;
    use yaml_edit::YamlFile;

    // Test 1: Modify a value in a mapping with explicit keys
    let yaml = "? key1\n: value1\n? key2\n: value2\n";

    let doc = YamlFile::from_str(yaml).unwrap();
    let mapping = doc.document().unwrap().as_mapping().unwrap();

    // Verify initial state via API
    assert_eq!(mapping.len(), 2);
    assert_eq!(
        mapping
            .get("key1")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "value1"
    );
    assert_eq!(
        mapping
            .get("key2")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "value2"
    );

    // Change value1 to newvalue
    mapping.set("key1", "newvalue");
    assert_file_cst_ok(&doc);

    // Verify mutation via API
    assert_eq!(
        mapping
            .get("key1")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "newvalue"
    );

    // Verify exact output preserves explicit key format
    let output = doc.to_string();
    assert_eq!(output, "? key1\n: newvalue\n? key2\n: value2\n");

    // Test 2: Add a new key to a mapping with explicit keys
    let yaml2 = "? existing\n: value\n";

    let doc2 = YamlFile::from_str(yaml2).unwrap();
    let mapping2 = doc2.document().unwrap().as_mapping().unwrap();

    // Verify initial state via API
    assert_eq!(mapping2.len(), 1);
    assert_eq!(
        mapping2
            .get("existing")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "value"
    );

    // Add new key
    mapping2.set("newkey", "newvalue");
    assert_file_cst_ok(&doc2);

    // Verify addition via API
    assert_eq!(mapping2.len(), 2);
    assert_eq!(
        mapping2
            .get("newkey")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "newvalue"
    );

    // Verify exact output - new key should use explicit format to match existing style
    let output2 = doc2.to_string();
    assert_eq!(output2, "? existing\n: value\n? newkey\n: newvalue\n");
}

#[test]
fn test_explicit_key_set_nested_keeps_indent() {
    use std::str::FromStr;
    use yaml_edit::YamlFile;

    let doc = YamlFile::from_str("outer:\n  ? a\n  : 1\n").unwrap();
    let outer = doc
        .document()
        .unwrap()
        .as_mapping()
        .unwrap()
        .get_mapping("outer")
        .unwrap();
    outer.set("b", "9");
    assert_parser_shaped(&doc);
    assert_eq!(doc.to_string(), "outer:\n  ? a\n  : 1\n  ? b\n  : '9'\n");
}

#[test]
fn test_explicit_key_insert_at_index_middle_keeps_indent() {
    use std::str::FromStr;
    use yaml_edit::YamlFile;

    let doc = YamlFile::from_str("outer:\n  ? a\n  : 1\n  ? c\n  : 2\n").unwrap();
    let outer = doc
        .document()
        .unwrap()
        .as_mapping()
        .unwrap()
        .get_mapping("outer")
        .unwrap();
    outer.insert_at_index(1, "b", "9");
    assert_parser_shaped(&doc);
    assert_eq!(
        doc.to_string(),
        "outer:\n  ? a\n  : 1\n  ? b\n  : '9'\n  ? c\n  : 2\n"
    );
}

#[test]
fn test_explicit_key_insert_at_index_first_keeps_indent() {
    use std::str::FromStr;
    use yaml_edit::YamlFile;

    let doc = YamlFile::from_str("outer:\n  ? b\n  : 1\n  ? c\n  : 2\n").unwrap();
    let outer = doc
        .document()
        .unwrap()
        .as_mapping()
        .unwrap()
        .get_mapping("outer")
        .unwrap();
    outer.insert_at_index(0, "a", "9");
    assert_parser_shaped(&doc);
    assert_eq!(
        doc.to_string(),
        "outer:\n  ? a\n  : '9'\n  ? b\n  : 1\n  ? c\n  : 2\n"
    );
}

#[test]
fn test_explicit_key_reorder_fields_keeps_line_breaks() {
    use std::str::FromStr;
    use yaml_edit::Document;

    let doc = Document::from_str("? a\n: 1\n? b\n: 2\n").unwrap();
    doc.as_mapping().unwrap().reorder_fields(["b"]);
    assert_cst_ok(&doc);
    assert_eq!(doc.to_string(), "? b\n: 2\n? a\n: 1\n");
}

/// An explicit key puts its `:` on the next line. Adding an entry and then
/// renaming it used to collapse that onto one line (`? z: '0'`), which
/// reparses as a single node and silently loses a key.
#[test]
fn set_then_rename_keeps_an_explicit_key_on_its_own_line() {
    use std::str::FromStr;
    use yaml_edit::{Document, YamlFile};

    for (source, nested) in [("? a\n: 1\n", false), ("m:\n  ? a\n  : 1\n", true)] {
        let doc = Document::from_str(source).unwrap();
        let root = doc.as_mapping().unwrap();
        let mapping = if nested {
            root.get_mapping("m").unwrap()
        } else {
            root
        };

        mapping.set("u", "0");
        assert_cst_ok(&doc);
        assert!(mapping.rename_key("u", "z"), "{source:?}");
        assert_cst_ok(&doc);

        let text = doc.to_string();
        let reparsed = YamlFile::from_str(&text).unwrap_or_else(|e| {
            panic!("{source:?} produced unparsable text {text:?}: {e}");
        });
        assert_file_cst_ok(&reparsed);

        let root = reparsed.document().unwrap().as_mapping().unwrap();
        let mapping = if nested {
            root.get_mapping("m").unwrap()
        } else {
            root
        };
        let keys: Vec<String> = mapping
            .keys()
            .map(|k| k.as_scalar().unwrap().as_string())
            .collect();
        assert_eq!(keys, vec!["a", "z"], "{source:?} produced {text:?}");
        assert_eq!(
            mapping.get("z").unwrap().as_scalar().unwrap().as_string(),
            "0",
            "{source:?} produced {text:?}"
        );
    }
}

/// A plain (non-explicit) mapping must keep renaming on one line.
#[test]
fn renaming_a_plain_key_stays_inline() {
    use std::str::FromStr;
    use yaml_edit::Document;

    let doc = Document::from_str("a: 1\nb: 2\n").unwrap();
    let mapping = doc.as_mapping().unwrap();
    assert!(mapping.rename_key("a", "z"));
    assert_cst_ok(&doc);
    assert_eq!(doc.to_string(), "z: 1\nb: 2\n");
}

#[test]
fn appended_explicit_key_entry_leaves_line_break_to_mapping() {
    assert_eq!(
        edit_root("? a\n: 1\n", |m| m.set("b", 2)),
        "? a\n: 1\n? b\n: 2\n"
    );
}

#[test]
fn explicit_key_entry_inserted_at_index_leaves_line_break_to_mapping() {
    let source = "? a\n: 1\n? c\n: 3\n";
    assert_eq!(
        edit_root(source, |m| m.insert_at_index(0, "b", 2)),
        "? b\n: 2\n? a\n: 1\n? c\n: 3\n"
    );
    assert_eq!(
        edit_root(source, |m| m.insert_at_index(1, "b", 2)),
        "? a\n: 1\n? b\n: 2\n? c\n: 3\n"
    );
    assert_eq!(
        edit_root(source, |m| m.insert_at_index(2, "b", 2)),
        "? a\n: 1\n? c\n: 3\n? b\n: 2\n"
    );
}

#[test]
fn explicit_key_entry_placed_by_field_order_leaves_line_break_to_mapping() {
    let source = "? a\n: 1\n? c\n: 3\n";
    assert_eq!(
        edit_root(source, |m| m.set_with_field_order("b", 2, ["a", "b", "c"])),
        "? a\n: 1\n? b\n: 2\n? c\n: 3\n"
    );
    assert_eq!(
        edit_root(source, |m| m.set_with_field_order("b", 2, ["b", "a", "c"])),
        "? b\n: 2\n? a\n: 1\n? c\n: 3\n"
    );
}

#[test]
fn unterminated_explicit_key_entry_gets_line_break_in_mapping() {
    assert_eq!(
        edit_root("? a\n: 1", |m| m.set("b", 2)),
        "? a\n: 1\n? b\n: 2\n"
    );
    assert_eq!(
        edit_root("? a\n: 1", |m| m.set_with_field_order("b", 2, ["a", "b"])),
        "? a\n: 1\n? b\n: 2\n"
    );
    assert_eq!(
        edit_root("? b\n: 2\n? a\n: 1", |m| {
            m.set_with_field_order("c", 3, ["b", "c", "a"])
        }),
        "? b\n: 2\n? c\n: 3\n? a\n: 1"
    );
}

#[test]
fn explicit_key_block_value_replaced_by_scalar_leaves_line_break_to_mapping() {
    assert_eq!(
        edit_root("? a\n:\n  x: 1\n? c\n: 3\n", |m| m.set("a", 5)),
        "? a\n: 5\n? c\n: 3\n"
    );
}

#[test]
fn commented_key_inserted_mid_mapping_leaves_line_break_to_mapping() {
    let key = YamlFile::from_str("- 1  # one\n- 2\n")
        .unwrap()
        .document()
        .unwrap()
        .as_sequence()
        .unwrap();
    assert_eq!(
        edit_root("a: 1\nc: 3\n", |m| m.insert_at_index(1, &key, 2)),
        "a: 1\n? [1,  # one\n  2]\n: 2\nc: 3\n"
    );
}

#[test]
fn nested_explicit_key_set_then_renamed_matches_parser() {
    let file = YamlFile::from_str("m:\n  ? a\n  : 1\n").unwrap();
    let m = file
        .document()
        .unwrap()
        .as_mapping()
        .unwrap()
        .get_mapping("m")
        .unwrap();
    m.set("u", "0");
    assert_parser_shaped(&file);
    assert!(m.rename_key("u", "z"));
    assert_parser_shaped(&file);
    assert_eq!(file.to_string(), "m:\n  ? a\n  : 1\n  ? z\n  : '0'\n");
}

fn commented_sequence_key() -> yaml_edit::Sequence {
    YamlFile::from_str("- 1  # one\n- 2\n")
        .unwrap()
        .document()
        .unwrap()
        .as_sequence()
        .unwrap()
}

#[test]
fn key_needing_explicit_form_inserted_relative_to_another() {
    let key = commented_sequence_key();
    let expected = "a: 1\n? [1,  # one\n  2]\n: 2\nc: 3\n";
    assert_eq!(
        edit_root("a: 1\nc: 3\n", |m| assert!(m.insert_after("a", &key, 2))),
        expected
    );
    assert_eq!(
        edit_root("a: 1\nc: 3\n", |m| assert!(m.move_after("a", &key, 2))),
        expected
    );
    assert_eq!(
        edit_root("a: 1\nc: 3\n", |m| assert!(m.insert_before("c", &key, 2))),
        expected
    );
    assert_eq!(
        edit_root("a: 1\nc: 3\n", |m| assert!(m.move_before("c", &key, 2))),
        expected
    );
}

#[test]
fn entry_inserted_relative_to_another_follows_explicit_keys() {
    let source = "? a\n: 1\n? c\n: 3\n";
    let expected = "? a\n: 1\n? b\n: 2\n? c\n: 3\n";
    assert_eq!(
        edit_root(source, |m| assert!(m.insert_after("a", "b", 2))),
        expected
    );
    assert_eq!(
        edit_root(source, |m| assert!(m.move_after("a", "b", 2))),
        expected
    );
    assert_eq!(
        edit_root(source, |m| assert!(m.insert_before("c", "b", 2))),
        expected
    );
    assert_eq!(
        edit_root(source, |m| assert!(m.move_before("c", "b", 2))),
        expected
    );
}

#[test]
fn entry_inserted_after_last_explicit_key_entry() {
    assert_eq!(
        edit_root("? a\n: 1\n", |m| assert!(m.insert_after("a", "b", 2))),
        "? a\n: 1\n? b\n: 2\n"
    );
    assert_eq!(
        edit_root("? a\n: 1", |m| assert!(m.move_after("a", "b", 2))),
        "? a\n: 1\n? b\n: 2\n"
    );
    assert_eq!(
        edit_root("? a\n: 1\n", |m| assert!(m.insert_before("a", "b", 2))),
        "? b\n: 2\n? a\n: 1\n"
    );
}

#[test]
fn explicit_key_entry_moved_within_mapping() {
    let source = "? a\n: 1\n? b\n: 2\n? c\n: 3\n";
    assert_eq!(
        edit_root(source, |m| assert!(m.move_after("c", "a", 9))),
        "? b\n: 2\n? c\n: 3\n? a\n: 9\n"
    );
    assert_eq!(
        edit_root(source, |m| assert!(m.move_before("a", "c", 9))),
        "? c\n: 9\n? a\n: 1\n? b\n: 2\n"
    );
    assert_eq!(
        edit_root(source, |m| assert!(m.move_after("a", "c", 9))),
        "? a\n: 1\n? c\n: 9\n? b\n: 2\n"
    );
    assert_eq!(
        edit_root(source, |m| assert!(m.move_before("c", "a", 9))),
        "? b\n: 2\n? a\n: 9\n? c\n: 3\n"
    );
}

#[test]
fn nested_explicit_key_entry_inserted_and_moved() {
    let source = "outer:\n  ? a\n  : 1\n  ? c\n  : 3\nz: 0\n";
    let edit_outer = |edit: &dyn Fn(&Mapping)| {
        let file = YamlFile::from_str(source).unwrap();
        let root = file.document().unwrap().as_mapping().unwrap();
        edit(&root.get_mapping("outer").unwrap());
        assert_parser_shaped(&file);
        file.to_string()
    };
    assert_eq!(
        edit_outer(&|m| assert!(m.insert_after("a", "b", 2))),
        "outer:\n  ? a\n  : 1\n  ? b\n  : 2\n  ? c\n  : 3\nz: 0\n"
    );
    assert_eq!(
        edit_outer(&|m| assert!(m.insert_after("c", "b", 2))),
        "outer:\n  ? a\n  : 1\n  ? c\n  : 3\n  ? b\n  : 2\nz: 0\n"
    );
    assert_eq!(
        edit_outer(&|m| assert!(m.insert_before("a", "b", 2))),
        "outer:\n  ? b\n  : 2\n  ? a\n  : 1\n  ? c\n  : 3\nz: 0\n"
    );
    assert_eq!(
        edit_outer(&|m| assert!(m.move_after("c", "a", 9))),
        "outer:\n  ? c\n  : 3\n  ? a\n  : 9\nz: 0\n"
    );
    assert_eq!(
        edit_outer(&|m| assert!(m.move_before("a", "c", 9))),
        "outer:\n  ? c\n  : 9\n  ? a\n  : 1\nz: 0\n"
    );
}
