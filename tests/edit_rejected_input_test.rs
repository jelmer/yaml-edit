//! Edits of input that the validator rejects. The parser recovers a tree
//! from such input, and edits of that tree must work on it rather than
//! panic.

use rowan::ast::AstNode;
use std::str::FromStr;
use yaml_edit::{Sequence, SyntaxKind, YamlFile};

mod common;

fn nth_sequence(file: &YamlFile, n: usize) -> Sequence {
    file.syntax()
        .descendants()
        .filter(|c| c.kind() == SyntaxKind::SEQUENCE)
        .nth(n)
        .and_then(Sequence::cast)
        .unwrap()
}

#[test]
fn test_remove_last_item_of_sequence_with_two_tags() {
    let file = YamlFile::from_str("!a !b\n- x\n").unwrap();
    nth_sequence(&file, 0).remove(0);
    assert_eq!(file.to_string(), "!a !b\n[]\n");
}

#[test]
fn test_remove_last_item_of_value_with_two_tags() {
    let file = YamlFile::from_str("k: !a !b\n  - x\n").unwrap();
    nth_sequence(&file, 0).remove(0);
    assert_eq!(file.to_string(), "k: !a !b []\n");
}

#[test]
fn test_refill_value_with_two_tags() {
    let file = YamlFile::from_str("k: !a !b\n  - x\n").unwrap();
    let seq = nth_sequence(&file, 0);
    seq.remove(0);
    seq.push("y");
    assert_eq!(file.to_string(), "k: !a !b\n  - y\n");
}

#[test]
fn test_remove_last_item_of_block_sequence_in_implicit_key() {
    let file =
        YamlFile::from_str("- [a, b ,!str\n-\n  !!null : a\n  b: !!str\n- !!s     ]\n").unwrap();
    nth_sequence(&file, 2).remove(0);
    assert_eq!(
        file.to_string(),
        "- [a, b ,!str\n[]: a\n  b: !!str\n- !!s     ]\n"
    );
}

#[test]
fn test_refill_root_sequence_followed_by_mapping() {
    let file = YamlFile::from_str(" - x\nb: 1").unwrap();
    let seq = nth_sequence(&file, 0);
    seq.remove(0);
    assert_eq!(file.to_string(), " []\nb: 1");
    seq.insert(0, "y");
    common::assert_file_edited(&file, " - y\nb: 1");
}

#[test]
fn test_clear_sequence_with_anchor_text_in_scalar() {
    // `&SS` is part of the plain scalar, so `*SS` refers to nothing.
    let file = YamlFile::from_str("- f\n  - &SS Sammy\nrb: *SS\n").unwrap();
    nth_sequence(&file, 0).clear();
    assert_eq!(file.to_string(), "[]\nrb: *SS\n");
}
