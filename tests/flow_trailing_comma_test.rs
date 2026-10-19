//! Removing the last entry of a flow collection drops the separator that
//! the entry before it carried.

use std::str::FromStr;
use yaml_edit::YamlFile;

mod common;
use common::{assert_file_cst_ok, assert_file_edited};

#[test]
fn test_remove_last_flow_mapping_entry() {
    let file = YamlFile::from_str("{a: 1, b: 2, c: 3}").unwrap();
    let root = file.document().unwrap().as_mapping().unwrap();
    root.remove("c").unwrap();
    assert_file_cst_ok(&file);
    assert_eq!(file.to_string(), "{a: 1, b: 2}");
    let reparsed = YamlFile::from_str(&file.to_string()).unwrap();
    assert_eq!(reparsed.document().unwrap().as_mapping().unwrap().len(), 2);
}

#[test]
fn test_remove_last_flow_mapping_entry_keeps_trailing_comma_style() {
    let file = YamlFile::from_str("{a: 1, b: 2,}").unwrap();
    let root = file.document().unwrap().as_mapping().unwrap();
    root.remove("b").unwrap();
    assert_file_cst_ok(&file);
    assert_eq!(file.to_string(), "{a: 1, }");
}

#[test]
fn test_remove_last_multiline_flow_mapping_entry() {
    let file = YamlFile::from_str("{a: 1,\n  b: 2}\n").unwrap();
    let root = file.document().unwrap().as_mapping().unwrap();
    root.remove("b").unwrap();
    assert_file_cst_ok(&file);
    assert_eq!(file.to_string(), "{a: 1}\n");
}

#[test]
fn test_move_before_from_end_of_flow_mapping() {
    let file = YamlFile::from_str("{a: 1, b: 2, c: 3}").unwrap();
    let root = file.document().unwrap().as_mapping().unwrap();
    assert!(root.move_before("a", "c", 9));
    assert_file_cst_ok(&file);
    assert_eq!(file.to_string(), "{c: 9, a: 1, b: 2}");
}

#[test]
fn test_remove_last_flow_sequence_item() {
    let file = YamlFile::from_str("[a, b, c]").unwrap();
    let seq = file.document().unwrap().as_sequence().unwrap();
    seq.remove(2).unwrap();
    assert_file_cst_ok(&file);
    assert_eq!(file.to_string(), "[a, b]");
    let reparsed = YamlFile::from_str(&file.to_string()).unwrap();
    assert_eq!(reparsed.document().unwrap().as_sequence().unwrap().len(), 2);
}

const MAPPING_APPENDS: &[(&str, &str)] = &[
    ("{a: 1, b: 2,}", "{a: 1, b: 2, c: 9,}"),
    ("{a: 1, b: 2, }\n", "{a: 1, b: 2, c: 9, }\n"),
    ("{a: 1,}", "{a: 1, c: 9,}"),
    ("{ a: 1, b: 2 }", "{ a: 1, b: 2, c: 9 }"),
    ("{a: 1,\n  b: 2,\n}\n", "{a: 1,\n  b: 2,\n  c: 9,\n}\n"),
    ("{a: 1, # x\n b: 2,}", "{a: 1, # x\n b: 2, c: 9,}"),
    ("{a: 1, b: 2, # x\n}", "{a: 1, b: 2, # x\nc: 9,}"),
    (
        "top: {a: 1, b: 2, # x\n  }\n",
        "top: {a: 1, b: 2, # x\n  c: 9,}\n",
    ),
];

type Insertion = dyn Fn(&yaml_edit::Mapping);

fn flow_mapping(file: &YamlFile) -> yaml_edit::Mapping {
    let root = file.document().unwrap().as_mapping().unwrap();
    root.get_mapping("top").unwrap_or(root)
}

#[test]
fn test_append_to_flow_mapping_keeps_trailing_comma() {
    for &(source, expected) in MAPPING_APPENDS {
        let insertions: [(&str, &Insertion); 5] = [
            ("set", &|m| m.set("c", 9)),
            ("insert_after", &|m| {
                assert!(m.insert_after("b", "c", 9) || m.insert_after("a", "c", 9))
            }),
            ("insert_at_index", &|m| m.insert_at_index(2, "c", 9)),
            ("move_after", &|m| {
                let last = m.keys().last().unwrap();
                assert!(m.move_after(last, "c", 9));
            }),
            ("set_with_field_order", &|m| {
                m.set_with_field_order("c", 9, ["a", "b", "c"])
            }),
        ];
        for (name, insert) in insertions {
            let file = YamlFile::from_str(source).unwrap();
            insert(&flow_mapping(&file));
            assert_eq!(file.to_string(), expected, "{name} on {source:?}");
            assert_file_edited(&file, expected);
        }
    }
}

#[test]
fn test_append_to_flow_sequence_keeps_trailing_comma() {
    for (source, expected) in [
        ("[1, 2,]", "[1, 2, 9,]"),
        ("[1, 2, ]\n", "[1, 2, 9, ]\n"),
        ("[1,]", "[1, 9,]"),
        ("[ 1, 2 ]", "[ 1, 2, 9 ]"),
        ("[1,\n  2,\n]\n", "[1,\n  2,\n  9,\n]\n"),
        ("[1, # x\n 2,]", "[1, # x\n 2, 9,]"),
        ("[1, 2, # x\n]", "[1, 2, # x\n9,]"),
        ("[1, 2 # x\n]", "[1, 2 # x\n, 9]"),
    ] {
        for push in [true, false] {
            let file = YamlFile::from_str(source).unwrap();
            let seq = file.document().unwrap().as_sequence().unwrap();
            if push {
                seq.push(9);
            } else {
                seq.insert(2, 9);
            }
            assert_eq!(file.to_string(), expected, "push: {push}");
            assert_file_edited(&file, expected);
        }
    }
}

#[test]
fn test_insert_before_flow_sequence_item() {
    for (source, index, expected) in [
        ("[1, 2]", 0, "[9, 1, 2]"),
        ("[1, 2]", 1, "[1, 9, 2]"),
        ("[1, 2,]", 1, "[1, 9, 2,]"),
        ("[1, # x\n 2]", 1, "[1, # x\n 9, 2]"),
        ("[a: 1, b]", 0, "[9, a: 1, b]"),
    ] {
        let file = YamlFile::from_str(source).unwrap();
        let seq = file.document().unwrap().as_sequence().unwrap();
        seq.insert(index, 9);
        assert_eq!(
            file.to_string(),
            expected,
            "insert at {index} in {source:?}"
        );
        assert_file_edited(&file, expected);
    }
}

#[test]
fn test_insert_into_flow_mapping_after_comment() {
    let source = "{a: 1, # x\n b: 2}\n";
    let expected = "{a: 1, # x\n c: 9, b: 2}\n";
    let file = YamlFile::from_str(source).unwrap();
    flow_mapping(&file).insert_at_index(1, "c", 9);
    assert_eq!(file.to_string(), expected);
    assert_file_edited(&file, expected);
}
