//! Structural-invariant tests that exercise mutation methods and then
//! assert `debug::validate_tree` + `debug::roundtrip_ok` on the result.
//!
//! Failures here indicate bugs in the mutation logic that would otherwise
//! only surface as odd rendering.

use rowan::ast::AstNode;
use std::str::FromStr;
use yaml_edit::{debug, Document, Mapping, YamlFile};

fn check(doc: &Document) {
    let syntax = doc.syntax();
    if let Err(e) = debug::validate_tree(syntax) {
        panic!("invariant violated: {e}\n---\n{}\n---", doc);
    }
    if let Err(e) = debug::roundtrip_ok(syntax) {
        panic!("roundtrip failed: {e}\n---\n{}\n---", doc);
    }
}

#[test]
fn parsed_document_is_valid() {
    let inputs = [
        "a: 1\n",
        "a: 1\nb: 2\n",
        "items:\n  - a\n  - b\n",
        "nested:\n  a: 1\n  b: 2\n",
        "mixed:\n  list:\n    - 1\n    - 2\n  map:\n    x: y\n",
        "flow: {a: 1, b: 2}\n",
        "flow_seq: [1, 2, 3]\n",
    ];
    for input in inputs {
        let file = YamlFile::from_str(input).unwrap();
        let doc = file.document().unwrap();
        check(&doc);
    }
}

#[test]
fn mapping_set_new_key() {
    let doc = Document::from_str("a: 1\n").unwrap();
    doc.as_mapping().unwrap().set("b", 2);
    check(&doc);
}

#[test]
fn mapping_set_replace_key() {
    let doc = Document::from_str("a: 1\nb: 2\n").unwrap();
    doc.as_mapping().unwrap().set("a", 99);
    check(&doc);
}

#[test]
fn mapping_remove_key() {
    let doc = Document::from_str("a: 1\nb: 2\nc: 3\n").unwrap();
    doc.as_mapping().unwrap().remove("b");
    check(&doc);
}

#[test]
fn nested_mapping_set() {
    let doc = Document::from_str("root:\n  a: 1\n").unwrap();
    let nested = doc.as_mapping().unwrap().get_mapping("root").unwrap();
    nested.set("b", 2);
    check(&doc);
}

#[test]
fn sequence_push_existing() {
    let doc = Document::from_str("items:\n  - a\n  - b\n").unwrap();
    let seq = doc.as_mapping().unwrap().get_sequence("items").unwrap();
    seq.push("c");
    check(&doc);
}

#[test]
fn set_empty_mapping_then_populate() {
    let doc = Document::from_str("existing: v\n").unwrap();
    let mapping = doc.as_mapping().unwrap();
    mapping.set("nested", Mapping::new());
    let nested = mapping.get_mapping("nested").unwrap();
    nested.set("a", 1);
    nested.set("b", 2);
    check(&doc);
}

#[test]
fn parse_empty_sequence_under_key_then_push() {
    // Analogous to the "set empty Sequence + push" bug: an empty
    // sequence under a key parsed from source, then pushed to.
    let doc = Document::from_str("items: []\nother: v\n").unwrap();
    let seq = doc.as_mapping().unwrap().get_sequence("items").unwrap();
    seq.push("a");
    check(&doc);
}

#[test]
fn sequence_insert_past_end_without_trailing_newline() {
    // Regression for a bug found by wiring the invariant check into
    // the mutation methods: Sequence::insert past the end of an
    // unterminated source (`- a\n  - b`, no trailing NL) used to
    // splice the new INDENT+ENTRY straight after the previous entry's
    // scalar, rendering as `- a\n  - b  - c\n`.
    let doc = Document::from_str("items:\n  - a\n  - b").unwrap();
    let seq = doc.as_mapping().unwrap().get_sequence("items").unwrap();
    seq.insert(2, "c");
    check(&doc);
    assert_eq!(doc.to_string(), "items:\n  - a\n  - b\n  - c\n");
}

#[test]
fn sequence_insert_into_existing() {
    let doc = Document::from_str("items:\n  - a\n  - c\n").unwrap();
    let seq = doc.as_mapping().unwrap().get_sequence("items").unwrap();
    seq.insert(1, "b");
    check(&doc);
}

#[test]
fn sequence_remove_middle() {
    let doc = Document::from_str("items:\n  - a\n  - b\n  - c\n").unwrap();
    let seq = doc.as_mapping().unwrap().get_sequence("items").unwrap();
    seq.remove(1);
    check(&doc);
}

#[test]
fn rename_key() {
    let doc = Document::from_str("old: 1\nkeep: 2\n").unwrap();
    doc.as_mapping().unwrap().rename_key("old", "new");
    check(&doc);
}

#[test]
fn mapping_clear() {
    let doc = Document::from_str("a: 1\nb: 2\n").unwrap();
    doc.as_mapping().unwrap().clear();
    check(&doc);
}

#[test]
fn set_key_with_dot_and_dash_parses_back() {
    let doc = Document::from_str("existing: value\n").unwrap();
    doc.as_mapping().unwrap().set(".ar-aa", 0);
    check(&doc);
    assert_eq!(doc.to_string(), "existing: value\n.ar-aa: 0\n");
}

#[test]
fn hash_inside_plain_scalar_stays_in_scalar() {
    // Per YAML 1.2 4.6.6 `#` starts a comment only when preceded by
    // whitespace. Inside a plain scalar with no gap the `#` is scalar
    // content (URLs, fragments, etc.).
    let doc = Document::from_str("url: http://example.com/foo#bar\n").unwrap();
    let value = doc.as_mapping().unwrap().get("url").unwrap();
    let scalar = value.as_scalar().unwrap();
    assert_eq!(scalar.as_string(), "http://example.com/foo#bar");
}

#[test]
fn hash_after_whitespace_is_comment() {
    // The other side of the rule: with a space, `#` does start a
    // comment. Make sure we didn't regress.
    let doc = Document::from_str("url: http://example.com/foo # trailing\n").unwrap();
    let value = doc.as_mapping().unwrap().get("url").unwrap();
    let scalar = value.as_scalar().unwrap();
    assert_eq!(scalar.as_string(), "http://example.com/foo");
}

#[test]
fn hash_in_flow_sequence_stays_in_scalar() {
    // Same rule inside a flow collection.
    let doc = Document::from_str("- [ http://example.com/foo#bar ]\n").unwrap();
    let seq = doc.as_sequence().unwrap();
    let inner = seq.get(0).unwrap();
    let inner_seq = inner.as_sequence().unwrap();
    assert_eq!(inner_seq.len(), 1);
    let scalar = inner_seq.get(0).unwrap().as_scalar().unwrap().as_string();
    assert_eq!(scalar, "http://example.com/foo#bar");
}

#[test]
fn set_key_with_embedded_colon_parses_back() {
    // A `.`-prefixed key that contains a non-space colon used to be
    // tokenised as three separate STRING tokens (`.d`, `:5`, `-a`);
    // the parser then couldn't stitch them back into a mapping entry.
    // Surfaced by libfuzzer.
    let doc = Document::from_str("a: null\n").unwrap();
    doc.as_mapping().unwrap().set(".d:5-a", "");
    check(&doc);
    assert_eq!(doc.to_string(), "a: null\n.d:5-a: ''\n");
}

#[test]
fn set_with_document_terminator_key() {
    // Document terminator (`...`) and start marker (`---`) at column 0
    // begin a new stream document; used as a bare key they must be
    // quoted or the resulting text re-parses as a broken document.
    let doc = Document::from_str("existing: value\n").unwrap();
    doc.as_mapping().unwrap().set("...", "value");
    check(&doc);
    let doc = Document::from_str("existing: value\n").unwrap();
    doc.as_mapping().unwrap().set("---", "value");
    check(&doc);
}

#[test]
fn set_path_then_remove_path_collapses_empty_scaffold() {
    use yaml_edit::path::YamlPath;
    let doc = Document::from_str("a: 1\nb: 2\nc: 3\n").unwrap();
    doc.set_path("vvv.vvv.x", "");
    doc.remove_path("vvv.vvv.x");
    check(&doc);
    // The emptied-out innermost mapping collapses into flow-empty `{}`
    // so path lookups still work (get_path("vvv.vvv") returns the empty
    // mapping) and the tree is well-formed.
    assert_eq!(doc.to_string(), "a: 1\nb: 2\nc: 3\nvvv:\n  vvv: {}\n");
}

#[test]
fn set_path_after_explicit_key_does_not_leave_blank_line() {
    use yaml_edit::path::YamlPath;
    let doc = Document::from_str("keys: !!set\n  ? a\n  ? b\n").unwrap();
    doc.set_path("a", "");
    check(&doc);
    assert!(
        !doc.to_string().contains("\n\n"),
        "unexpected blank line: {:?}",
        doc.to_string()
    );
}

// Bugs surfaced by the proptest post-conditions in
// tests/proptest_invariants.rs. Each is marked `#[ignore]` so it does
// not fail the suite; un-ignore when the underlying issue is fixed to
// lock in the correct behavior.
//
// Assertions describe what the current (broken) code produces so the
// test flips from `passing when ignored` to `passing when fixed`
// naturally: replace the buggy expected value with the correct one at
// fix time.

#[test]
fn sequence_pop_last_item_collapses_to_flow_empty() {
    let doc = Document::from_str("s:\n  - a\n").unwrap();
    let seq = doc.as_mapping().unwrap().get_sequence("s").unwrap();
    seq.pop();
    assert_eq!(doc.to_string(), "s: []\n");
    check(&doc);
    let reparsed = Document::from_str(&doc.to_string()).unwrap();
    assert!(reparsed
        .as_mapping()
        .and_then(|m| m.get_sequence("s"))
        .is_some());
}

#[test]
fn sequence_remove_last_item_collapses_to_flow_empty() {
    let doc = Document::from_str("s:\n  - a\n").unwrap();
    let seq = doc.as_mapping().unwrap().get_sequence("s").unwrap();
    seq.remove(0);
    assert_eq!(doc.to_string(), "s: []\n");
    check(&doc);
}

#[test]
fn sequence_clear_block_collapses_to_flow_empty() {
    let doc = Document::from_str("s:\n  - a\n  - b\n").unwrap();
    let seq = doc.as_mapping().unwrap().get_sequence("s").unwrap();
    seq.clear();
    assert_eq!(doc.to_string(), "s: []\n");
    check(&doc);
}

#[test]
fn sequence_remove_last_entry_preserves_following_mapping_entry() {
    // The new-last SEQUENCE_ENTRY's trailing NEWLINE doubles as the
    // separator between the containing MAPPING_ENTRY and its next
    // sibling; removing the sequence's last entry must not strip it.
    let doc = Document::from_str("s:\n  - a\n  - b\n  - c\na: ''\n").unwrap();
    let seq = doc.as_mapping().unwrap().get_sequence("s").unwrap();
    seq.pop();
    check(&doc);
    assert_eq!(doc.to_string(), "s:\n  - a\n  - b\na: ''\n");
}

#[test]
fn sequence_remove_first_entry_preserves_new_first_indent() {
    // The INDENT that separated entry 0 from entry 1 must be dropped
    // when entry 0 goes, otherwise it becomes a leading INDENT inside
    // SEQUENCE that stacks with the parent VALUE's INDENT.
    let doc = Document::from_str("s:\n  - a\n  - b\n  - c\n").unwrap();
    let seq = doc.as_mapping().unwrap().get_sequence("s").unwrap();
    seq.remove(0);
    assert_eq!(doc.to_string(), "s:\n  - b\n  - c\n");
    check(&doc);
}

#[test]
#[ignore = "Sequence::insert into empty flow `[]` produces mixed-style broken output"]
fn sequence_insert_into_empty_flow() {
    let doc = Document::from_str("s: []\n").unwrap();
    let seq = doc.as_mapping().unwrap().get_sequence("s").unwrap();
    seq.insert(0, "x");
    // Any correct output would work; the point is that re-parse should
    // yield a sequence with one item "x".
    let reparsed = Document::from_str(&doc.to_string()).unwrap();
    let seq2 = reparsed.as_mapping().unwrap().get_sequence("s").unwrap();
    assert_eq!(seq2.len(), 1);
    check(&doc);
}

#[test]
#[ignore = "Sequence::push into non-empty flow produces mixed-style output that reparses as one string"]
fn sequence_push_into_nonempty_flow() {
    let doc = Document::from_str("s: [x]\n").unwrap();
    let seq = doc.as_mapping().unwrap().get_sequence("s").unwrap();
    seq.push("y");
    let reparsed = Document::from_str(&doc.to_string()).unwrap();
    let seq2 = reparsed.as_mapping().unwrap().get_sequence("s").unwrap();
    assert_eq!(seq2.len(), 2);
    check(&doc);
}

#[test]
#[ignore = "Sequence::insert into non-empty flow produces mixed-style output"]
fn sequence_insert_into_nonempty_flow() {
    let doc = Document::from_str("s: [x]\n").unwrap();
    let seq = doc.as_mapping().unwrap().get_sequence("s").unwrap();
    seq.insert(0, "y");
    let reparsed = Document::from_str(&doc.to_string()).unwrap();
    let seq2 = reparsed.as_mapping().unwrap().get_sequence("s").unwrap();
    assert_eq!(seq2.len(), 2);
    check(&doc);
}

#[test]
fn sequence_insert_at_head_into_single_entry_block() {
    let doc = Document::from_str("s:\n  - a\n").unwrap();
    let seq = doc.as_mapping().unwrap().get_sequence("s").unwrap();
    seq.insert(0, "b");
    assert_eq!(doc.to_string(), "s:\n  - b\n  - a\n");
    check(&doc);
}
