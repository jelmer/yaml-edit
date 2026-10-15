//! Removing the last entry of a flow collection drops the separator that
//! the entry before it carried.

use std::str::FromStr;
use yaml_edit::YamlFile;

mod common;
use common::assert_file_cst_ok;

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
