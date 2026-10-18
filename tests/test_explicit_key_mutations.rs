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
        debug::shape_to_string(file.syntax()),
        debug::shape_to_string(reparsed.syntax())
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
fn scalar_over_commented_block_keeps_the_question_mark_separate() {
    use std::str::FromStr;
    use yaml_edit::YamlFile;

    let doc = YamlFile::from_str("? a\n:  # c\n  x: 1\nb: 2\n").unwrap();
    doc.document().unwrap().as_mapping().unwrap().set("a", 5);
    assert_file_cst_ok(&doc);
    assert_eq!(doc.to_string(), "? a\n: 5  # c\nb: 2\n");
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
        "? a\n: 1\n? b\n: 2"
    );
    assert_eq!(
        edit_root("? a\n: 1", |m| m.set_with_field_order("b", 2, ["a", "b"])),
        "? a\n: 1\n? b\n: 2"
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
        "? a\n: 1\n? b\n: 2"
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

#[test]
fn explicit_key_entry_replaced_at_index() {
    let source = "? a\n: 1\n? b\n: 2\n";
    assert_eq!(
        edit_root(source, |m| m.insert_at_index_preserving(0, "a", 5)),
        "? a\n: 5\n? b\n: 2\n"
    );
    assert_eq!(
        edit_root(source, |m| m.insert_at_index_preserving(0, "b", 5)),
        "? a\n: 1\n? b\n: 5\n"
    );
    assert_eq!(
        edit_root("? a\n: 1\n? b\n: 2", |m| m
            .insert_at_index_preserving(0, "b", 5)),
        "? a\n: 1\n? b\n: 5"
    );
}

#[test]
fn entry_replaced_at_index_keeps_mapping_layout() {
    for (source, key, expected) in [
        ("a: 1\nb: 2\n", "a", "a: 5\nb: 2\n"),
        ("a: 1\nb: 2", "b", "a: 1\nb: 5"),
        ("m:\n  a: 1\n  b: 2\n", "a", "m:\n  a: 5\n  b: 2\n"),
        ("m:\n  a: 1\n  b: 2\n", "b", "m:\n  a: 1\n  b: 5\n"),
        ("m:\n  a: 1\nz: 0\n", "a", "m:\n  a: 5\nz: 0\n"),
        ("{k: v, m: 2}\n", "k", "{k: 5, m: 2}\n"),
        ("{k: v, m: 2}\n", "m", "{k: v, m: 5}\n"),
        ("{k: v,\n  m: 2}\n", "k", "{k: 5,\n  m: 2}\n"),
    ] {
        let file = YamlFile::from_str(source).unwrap();
        let root = file.document().unwrap().as_mapping().unwrap();
        let m = root.get_mapping("m").unwrap_or(root);
        m.insert_at_index_preserving(1, key, 5);
        assert_parser_shaped(&file);
        assert_eq!(file.to_string(), expected, "{source:?}");
    }
}

#[test]
fn explicit_key_commented_block_value_replaced_by_scalar() {
    assert_eq!(
        edit_root("? a\n:  # c\n  x: 1\n", |m| m.set("a", 5)),
        "? a\n: 5  # c\n"
    );
    assert_eq!(
        edit_root("? a\n:  # c\n  x: 1\nb: 2\n", |m| m.set("a", 5)),
        "? a\n: 5  # c\nb: 2\n"
    );
    assert_eq!(
        edit_root("m:\n  ? a\n  :  # c\n    - 1\n  b: 2\n", |m| {
            m.get_mapping("m").unwrap().set("a", 5)
        }),
        "m:\n  ? a\n  : 5  # c\n  b: 2\n"
    );
}

#[test]
fn explicit_key_commented_scalar_value_replaced() {
    assert_eq!(
        edit_root("? a\n: 1  # c\n? b\n: 2\n", |m| m.set("a", 5)),
        "? a\n: 5  # c\n? b\n: 2\n"
    );
}

fn first_item(source: &str) -> yaml_edit::YamlNode {
    YamlFile::from_str(source)
        .unwrap()
        .document()
        .unwrap()
        .as_sequence()
        .unwrap()
        .get(0)
        .unwrap()
}

/// Edit the root mapping of `source`, check the result is shaped as the
/// parser would shape it and is valid, and return its text.
#[track_caller]
fn edit_root_valid(source: &str, edit: impl FnOnce(&Mapping)) -> String {
    let text = edit_root(source, edit);
    let doc = yaml_edit::Document::from_str(&text).unwrap();
    assert_eq!(
        yaml_edit::validator::Validator::new().validate(&doc),
        vec![],
        "{text:?}"
    );
    text
}

#[test]
fn key_renamed_to_one_spanning_lines_becomes_explicit() {
    let quoted = first_item("- \"x\n  y\"\n");
    let commented = first_item("- - 1  # one\n  - 2\n");
    for (source, key, expected) in [
        ("a: 1\nb: 2\n", &quoted, "? \"x\n  y\"\n: 1\nb: 2\n"),
        ("b: 2\na: 1\n", &quoted, "b: 2\n? \"x\n  y\"\n: 1\n"),
        (
            "a: 1  # c\nb: 2\n",
            &quoted,
            "? \"x\n  y\"\n: 1  # c\nb: 2\n",
        ),
        (
            "a:\n  x: 1\nb: 2\n",
            &quoted,
            "? \"x\n  y\"\n:\n  x: 1\nb: 2\n",
        ),
        (
            "a: 1\nb: 2\n",
            &commented,
            "? [1,  # one\n  2]\n: 1\nb: 2\n",
        ),
        ("{a: 1, b: 2}\n", &quoted, "{\"x\n   y\": 1, b: 2}\n"),
    ] {
        assert_eq!(
            edit_root_valid(source, |m| assert!(m.rename_key("a", key))),
            expected,
            "{source:?}"
        );
    }
}

#[test]
fn nested_key_renamed_to_one_spanning_lines_becomes_explicit() {
    let key = first_item("- \"x\n    y\"\n");
    let file = YamlFile::from_str("m:\n  a: 1\n  b: 2\n").unwrap();
    let m = file
        .document()
        .unwrap()
        .as_mapping()
        .unwrap()
        .get_mapping("m")
        .unwrap();
    assert!(m.rename_key("a", &key));
    assert_parser_shaped(&file);
    assert_eq!(file.to_string(), "m:\n  ? \"x\n      y\"\n  : 1\n  b: 2\n");
}

#[test]
fn new_key_spanning_lines_is_explicit() {
    let quoted = first_item("- \"x\n  y\"\n");
    let flow = first_item("- [1,\n  2]\n");
    assert_eq!(
        edit_root_valid("a: 1\n", |m| m.set(&quoted, 5)),
        "a: 1\n? \"x\n  y\"\n: 5\n"
    );
    assert_eq!(
        edit_root_valid("a: 1\n", |m| m.set(&flow, 5)),
        "a: 1\n? [1,\n  2]\n: 5\n"
    );
    assert_eq!(
        edit_root_valid("{a: 1}\n", |m| m.set(&quoted, 5)),
        "{a: 1, \"x\n  y\": 5}\n"
    );
}

fn first_value(source: &str) -> yaml_edit::YamlNode {
    yaml_edit::Document::from_str(source)
        .unwrap()
        .as_mapping()
        .unwrap()
        .get("k")
        .unwrap()
}

#[test]
fn block_key_written_in_flow_form() {
    let block = first_value("k: |\n  x\n");
    let tagged_block = first_value("k: !!str |\n  x\n");
    let sequence = first_value("k:\n  - x\n  - y\n");
    let tagged_sequence = first_value("k: !t\n  - x\n");
    for (key, expected) in [
        (&block, "\"x\\n\": 1\nb: 2\n"),
        (&tagged_block, "!!str \"x\\n\": 1\nb: 2\n"),
        (&sequence, "[x, y]: 1\nb: 2\n"),
        (&tagged_sequence, "!t [x]: 1\nb: 2\n"),
    ] {
        assert_eq!(
            edit_root_valid("a: 1\nb: 2\n", |m| assert!(m.rename_key("a", key))),
            expected
        );
        assert_eq!(
            edit_root_valid("b: 2\n", |m| m.set(key, 1)),
            format!("b: 2\n{}", &expected[..expected.len() - "b: 2\n".len()])
        );
    }
}

#[test]
fn tagged_key_in_explicit_and_flow_entries() {
    let tagged = first_value("k: !t\n  - x\n");
    assert_eq!(
        edit_root_valid("? a\n: 1\n", |m| assert!(m.rename_key("a", &tagged))),
        "? !t [x]\n: 1\n"
    );
    assert_eq!(
        edit_root_valid("{a: 1}\n", |m| assert!(m.rename_key("a", &tagged))),
        "{!t [x]: 1}\n"
    );
    assert_eq!(
        edit_root_valid("{a: 1}\n", |m| m.set(&tagged, 2)),
        "{a: 1, !t [x]: 2}\n"
    );
}

#[test]
fn set_key_written_in_flow_form() {
    let set = |members: &[&str]| -> std::collections::BTreeSet<String> {
        members.iter().map(|m| m.to_string()).collect()
    };
    let nested: std::collections::BTreeSet<_> = [set(&["p", "q"])].into_iter().collect();
    assert_eq!(
        edit_root_valid("a: 1\n", |m| m.set("s", &nested)),
        "a: 1\ns: !!set\n  !!set {p: null, q: null}: null\n"
    );
    assert_eq!(
        edit_root_valid("a: 1\n", |m| assert!(m.rename_key("a", set(&["p"])))),
        "!!set {p: null}: 1\n"
    );
}

#[test]
fn flow_explicit_key_renamed() {
    assert_eq!(
        edit_root_valid("{? a : 1, b: 2}\n", |m| assert!(m.rename_key("a", "z"))),
        "{? z : 1, b: 2}\n"
    );
}

#[test]
fn renamed_key_keeps_its_anchor() {
    for (source, expected) in [
        ("&x a: 1\nb: *x\n", "&x z: 1\nb: *x\n"),
        ("&x !!str a: 1\nb: *x\n", "&x !!str z: 1\nb: *x\n"),
        ("? &x a\n: 1\nb: *x\n", "? &x z\n: 1\nb: *x\n"),
        ("? !!str &x a\n: 1\nb: *x\n", "? &x !!str z\n: 1\nb: *x\n"),
        ("{&x a: 1, b: *x}\n", "{&x z: 1, b: *x}\n"),
        ("{? &x a : 1, b: *x}\n", "{? &x z : 1, b: *x}\n"),
    ] {
        assert_eq!(
            edit_root_valid(source, |m| assert!(m.rename_key("a", "z"))),
            expected,
            "{source:?}"
        );
    }
    let quoted = first_item("- \"x\n  y\"\n");
    assert_eq!(
        edit_root_valid("&x a: 1\nb: *x\n", |m| assert!(m.rename_key("a", &quoted))),
        "? &x \"x\n  y\"\n: 1\nb: *x\n"
    );
}

#[test]
fn renamed_key_keeps_its_tag_only_if_still_the_new_key() {
    // `!!str 5` is a string, not the integer asked for, and `!t z` is not
    // the string `z`.
    assert_eq!(
        edit_root_valid("&x !!str a: 1\nb: *x\n", |m| assert!(m.rename_key("a", 5))),
        "&x 5: 1\nb: *x\n"
    );
    assert_eq!(
        edit_root_valid("!t a: 1\n", |m| assert!(
            m.rename_key(first_value("k: !t a\n"), "z")
        )),
        "z: 1\n"
    );
}

#[test]
fn renamed_key_given_as_annotated_node() {
    // The new key's own tag replaces the old one. The old anchor stays,
    // since aliases in this document refer to it; the new key's anchor is
    // used only if the old key had none.
    let tagged = first_value("k: !t [q]\n");
    let anchored = first_value("k: &y q\n");
    assert_eq!(
        edit_root_valid("&x !!str a: 1\nb: *x\n", |m| assert!(
            m.rename_key("a", &tagged)
        )),
        "&x !t [q]: 1\nb: *x\n"
    );
    assert_eq!(
        edit_root_valid(
            "&x a: 1\nb: *x\n",
            |m| assert!(m.rename_key("a", &anchored))
        ),
        "&x q: 1\nb: *x\n"
    );
    assert_eq!(
        edit_root_valid("a: 1\n", |m| assert!(m.rename_key("a", &anchored))),
        "&y q: 1\n"
    );
}

#[test]
fn renamed_key_keeps_its_tag_in_a_json_style_mapping() {
    assert_eq!(
        edit_root_valid("{!!str \"a\": 1, \"b\": 2}\n", |m| assert!(
            m.rename_key("a", "z")
        )),
        "{!!str \"z\": 1, \"b\": 2}\n"
    );
}

#[test]
fn explicit_block_or_empty_key_renamed_keeps_colon_line() {
    for (source, expected) in [
        ("? - a\n: b\n", "? z\n: b\n"),
        ("? |\n  a\n: b\n", "? z\n: b\n"),
        ("?\n: b\n", "? z\n: b\n"),
        ("?\n- a\n- b\n:\n- c\n", "? z\n:\n- c\n"),
        ("? - a\n", "? z\n"),
        ("? - a\nc: d\n", "? z\nc: d\n"),
        ("m:\n  ? - a\n  : b\n", "m:\n  ? z\n  : b\n"),
        ("m:\n  ?\n  : b\n", "m:\n  ? z\n  : b\n"),
    ] {
        let file = YamlFile::from_str(source).unwrap();
        let root = file.document().unwrap().as_mapping().unwrap();
        let m = root.get_mapping("m").unwrap_or(root);
        let key = m.keys().next().unwrap();
        assert!(m.rename_key(&key, "z"), "{source:?}");
        assert_parser_shaped(&file);
        assert_eq!(file.to_string(), expected, "{source:?}");
        let doc = yaml_edit::Document::from_str(expected).unwrap();
        assert_eq!(
            yaml_edit::validator::Validator::new().validate(&doc),
            vec![]
        );
    }
}

#[test]
fn comment_before_explicit_key_kept_on_rename() {
    for (source, expected) in [
        ("? # c\n  a\n: b\n", "? # c\n  z\n: b\n"),
        ("? &x # c\n  a\n: b\n", "? &x # c\n  z\n: b\n"),
        ("? # c\n  - a\n: b\n", "? # c\n  z\n: b\n"),
        ("?  # c\n: b\n", "?  z  # c\n: b\n"),
        ("{? # c\n  a : b}\n", "{? # c\n  z : b}\n"),
    ] {
        let file = YamlFile::from_str(source).unwrap();
        let m = file.document().unwrap().as_mapping().unwrap();
        let key = m.keys().next().unwrap();
        assert!(m.rename_key(&key, "z"), "{source:?}");
        assert_parser_shaped(&file);
        assert_eq!(file.to_string(), expected, "{source:?}");
        let doc = yaml_edit::Document::from_str(expected).unwrap();
        assert_eq!(
            yaml_edit::validator::Validator::new().validate(&doc),
            vec![]
        );
    }
}

#[test]
fn entry_inserted_after_comment_ending_a_line() {
    assert_eq!(
        edit_root("? a\n: b  # c\nz: 1\n", |m| {
            assert!(m.insert_after("a", "k", 1));
        }),
        "? a\n: b  # c\n? k\n: 1\nz: 1\n"
    );
}

#[test]
fn entry_added_after_comment_ending_the_input() {
    assert_eq!(
        edit_root("? a\n: b  # c", |m| m.set("k", 1)),
        "? a\n: b  # c\n? k\n: 1"
    );
    assert_eq!(
        edit_root("? a\n: b  # c", |m| {
            assert!(m.insert_after("a", "k", 1));
        }),
        "? a\n: b  # c\n? k\n: 1"
    );
}
