//! Editing a collection that is no longer in its document: a handle kept
//! from before the value, item or entry holding it was replaced or
//! removed. The edit happens on the detached tree, and the document is
//! left as it is.

use rowan::ast::AstNode;
use std::str::FromStr;
use yaml_edit::{debug, Mapping, Sequence, YamlFile};

/// The text of the detached tree `node` is now in, after checking it.
#[track_caller]
fn detached_text(node: &rowan::SyntaxNode<yaml_edit::Lang>) -> String {
    let root = node.ancestors().last().unwrap();
    debug::validate_tree(&root).unwrap();
    root.to_string()
}

/// The mapping value of `k` in `src`, and the file, after `k` is set to 1.
fn replaced_mapping(src: &str) -> (YamlFile, Mapping) {
    let file = YamlFile::from_str(src).unwrap();
    let root = file.document().unwrap().as_mapping().unwrap();
    let inner = root.get_mapping("k").unwrap();
    root.set("k", 1);
    (file, inner)
}

/// The sequence value of `k` in `src`, and the file, after `k` is set to 1.
fn replaced_sequence(src: &str) -> (YamlFile, Sequence) {
    let file = YamlFile::from_str(src).unwrap();
    let root = file.document().unwrap().as_mapping().unwrap();
    let inner = root.get_sequence("k").unwrap();
    root.set("k", 1);
    (file, inner)
}

/// The sequence that is the first item of `src`, and the file, after that
/// item is set to 1.
fn replaced_item(src: &str) -> (YamlFile, Sequence) {
    let file = YamlFile::from_str(src).unwrap();
    let root = file.document().unwrap().as_sequence().unwrap();
    let inner = root.get(0).unwrap().as_sequence().cloned().unwrap();
    root.set(0, 1);
    (file, inner)
}

#[test]
fn drain_replaced_mapping_value() {
    let (file, inner) = replaced_mapping("k:\n  a: 1\n  b: 2\nz: 1\n");
    inner.remove("a");
    inner.remove("b");
    assert_eq!(file.to_string(), "k: 1\nz: 1\n");
    assert_eq!(detached_text(inner.syntax()), "\n  {}\n");
    inner.set("n", 3);
    assert_eq!(file.to_string(), "k: 1\nz: 1\n");
    assert_eq!(detached_text(inner.syntax()), "\n  n: 3\n");

    let (file, inner) = replaced_mapping("k:\n  a: 1\n  # c\n  b: 2\nz: 1\n");
    inner.clear();
    assert_eq!(file.to_string(), "k: 1\nz: 1\n");
    assert_eq!(detached_text(inner.syntax()), "\n  # c\n  {}\n");

    // One emptied before it was replaced, which is `{}` on the key's line.
    let file = YamlFile::from_str("k:\n  a: 1\nz: 1\n").unwrap();
    let root = file.document().unwrap().as_mapping().unwrap();
    let inner = root.get_mapping("k").unwrap();
    inner.remove("a");
    root.set("k", 1);
    assert_eq!(detached_text(inner.syntax()), "{}");
    inner.set("n", 3);
    assert_eq!(file.to_string(), "k: 1\nz: 1\n");
    assert_eq!(detached_text(inner.syntax()), "\n  n: 3\n");
}

#[test]
fn drain_replaced_sequence_value() {
    let (file, inner) = replaced_sequence("k:\n  - a\n  - b\nz: 1\n");
    inner.clear();
    assert_eq!(file.to_string(), "k: 1\nz: 1\n");
    assert_eq!(detached_text(inner.syntax()), "\n  []\n");
    inner.push(3);
    assert_eq!(detached_text(inner.syntax()), "\n  - 3\n");
}

#[test]
fn remove_last_entry_of_replaced_item() {
    let (file, inner) = replaced_item("- - a\n  - b\n- x\n");
    inner.remove(1);
    assert_eq!(file.to_string(), "- 1\n- x\n");
    assert_eq!(detached_text(inner.syntax()), "- - a\n");

    // With no collection around it, the comment line after the last item
    // stays in the sequence, also once that is empty.
    let (file, inner) = replaced_item("- - a\n  # c\n  - b\n- x\n");
    inner.remove(1);
    assert_eq!(detached_text(inner.syntax()), "- - a\n  # c\n");
    inner.remove(0);
    assert_eq!(file.to_string(), "- 1\n- x\n");
    assert_eq!(detached_text(inner.syntax()), "- []\n  # c\n");
}

#[test]
fn remove_last_entry_of_removed_key() {
    let file = YamlFile::from_str("? a: 1\n  b: 2\n: v\nz: 1\n").unwrap();
    let root = file.document().unwrap().as_mapping().unwrap();
    let key = root.keys().next().unwrap();
    let inner = key.as_mapping().cloned().unwrap();
    root.remove(&key);
    inner.remove("b");
    assert_eq!(file.to_string(), "z: 1\n");
    assert_eq!(detached_text(inner.syntax()), "? a: 1\n: v\n");
}

#[test]
fn refill_removed_explicit_key() {
    let file = YamlFile::from_str("? a: 1\n  b: 2\nz: 1\n").unwrap();
    let root = file.document().unwrap().as_mapping().unwrap();
    let key = root.keys().next().unwrap();
    let inner = key.as_mapping().cloned().unwrap();
    root.remove(&key);
    inner.clear();
    assert_eq!(detached_text(inner.syntax()), "? {}\n");
    inner.set("n", 3);
    assert_eq!(file.to_string(), "z: 1\n");
    assert_eq!(detached_text(inner.syntax()), "? n: 3\n");
}
