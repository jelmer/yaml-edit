//! Structural-invariant tests that exercise mutation methods and then
//! assert `debug::validate_tree` + `debug::roundtrip_ok` on the result.
//!
//! Failures here indicate bugs in the mutation logic that would otherwise
//! only surface as odd rendering.

mod common;

use common::{assert_cst_ok, assert_file_cst_ok};
use rowan::ast::AstNode;
use std::str::FromStr;
use yaml_edit::{debug, Document, Mapping, YamlFile};

/// Like `assert_cst_ok`, and also require the tree to match a re-parse of
/// its text.
#[track_caller]
fn check_shape(doc: &Document) {
    assert_cst_ok(doc);
    let reparsed = Document::from_str(&doc.to_string()).unwrap();
    assert_eq!(
        debug::tree_to_string(doc.syntax()),
        debug::tree_to_string(reparsed.syntax())
    );
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
        assert_cst_ok(&doc);
    }
}

#[test]
fn mapping_set_new_key() {
    let doc = Document::from_str("a: 1\n").unwrap();
    doc.as_mapping().unwrap().set("b", 2);
    assert_cst_ok(&doc);
}

#[test]
fn mapping_set_replace_key() {
    let doc = Document::from_str("a: 1\nb: 2\n").unwrap();
    doc.as_mapping().unwrap().set("a", 99);
    assert_cst_ok(&doc);
}

#[test]
fn mapping_remove_key() {
    let doc = Document::from_str("a: 1\nb: 2\nc: 3\n").unwrap();
    doc.as_mapping().unwrap().remove("b");
    assert_cst_ok(&doc);
}

#[test]
fn nested_mapping_set() {
    let doc = Document::from_str("root:\n  a: 1\n").unwrap();
    let nested = doc.as_mapping().unwrap().get_mapping("root").unwrap();
    nested.set("b", 2);
    assert_cst_ok(&doc);
}

#[test]
fn sequence_push_existing() {
    let doc = Document::from_str("items:\n  - a\n  - b\n").unwrap();
    let seq = doc.as_mapping().unwrap().get_sequence("items").unwrap();
    seq.push("c");
    assert_cst_ok(&doc);
}

/// Building a sequence a push at a time stays usable at scale.
///
/// `push` used to hold every child of the SEQUENCE alive in a Vec across
/// `splice_children`, which has to fix up each live SyntaxNode handle. That
/// made one push cost O(len) twice over and building a sequence O(len^2):
/// 800 pushes took over a second. The bound here is loose enough not to be
/// flaky, but a return of the old behaviour blows straight through it.
#[test]
fn sequence_push_scales() {
    let doc = Document::from_str("items:\n  - a\n").unwrap();
    let seq = doc.as_mapping().unwrap().get_sequence("items").unwrap();

    let start = std::time::Instant::now();
    for i in 0..1500 {
        seq.push(format!("v{i}"));
    }
    let elapsed = start.elapsed();

    assert_eq!(seq.len(), 1501);
    assert_cst_ok(&doc);

    // Re-parsing the result must give the same sequence back.
    let reparsed = Document::from_str(&doc.to_string()).unwrap();
    let reparsed_seq = reparsed
        .as_mapping()
        .unwrap()
        .get_sequence("items")
        .unwrap();
    assert_eq!(reparsed_seq.len(), 1501);
    assert_eq!(
        reparsed_seq
            .get(1500)
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "v1499"
    );

    assert!(
        elapsed < std::time::Duration::from_secs(15),
        "1500 pushes took {elapsed:?}, which suggests push is quadratic again"
    );
}

#[test]
fn set_empty_mapping_then_populate() {
    let doc = Document::from_str("existing: v\n").unwrap();
    let mapping = doc.as_mapping().unwrap();
    mapping.set("nested", Mapping::new());
    assert_cst_ok(&doc);
    let nested = mapping.get_mapping("nested").unwrap();
    nested.set("a", 1);
    assert_cst_ok(&doc);
    nested.set("b", 2);
    assert_cst_ok(&doc);
}

#[test]
fn parse_empty_sequence_under_key_then_push() {
    // Analogous to the "set empty Sequence + push" bug: an empty
    // sequence under a key parsed from source, then pushed to.
    let doc = Document::from_str("items: []\nother: v\n").unwrap();
    let seq = doc.as_mapping().unwrap().get_sequence("items").unwrap();
    seq.push("a");
    assert_cst_ok(&doc);
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
    assert_cst_ok(&doc);
    assert_eq!(doc.to_string(), "items:\n  - a\n  - b\n  - c\n");
}

#[test]
fn sequence_insert_into_existing() {
    let doc = Document::from_str("items:\n  - a\n  - c\n").unwrap();
    let seq = doc.as_mapping().unwrap().get_sequence("items").unwrap();
    seq.insert(1, "b");
    assert_cst_ok(&doc);
}

#[test]
fn sequence_remove_middle() {
    let doc = Document::from_str("items:\n  - a\n  - b\n  - c\n").unwrap();
    let seq = doc.as_mapping().unwrap().get_sequence("items").unwrap();
    seq.remove(1);
    assert_cst_ok(&doc);
}

#[test]
fn rename_key() {
    let doc = Document::from_str("old: 1\nkeep: 2\n").unwrap();
    doc.as_mapping().unwrap().rename_key("old", "new");
    assert_cst_ok(&doc);
}

#[test]
fn mapping_clear() {
    let doc = Document::from_str("a: 1\nb: 2\n").unwrap();
    doc.as_mapping().unwrap().clear();
    assert_cst_ok(&doc);
}

#[test]
fn set_key_with_dot_and_dash_parses_back() {
    let doc = Document::from_str("existing: value\n").unwrap();
    doc.as_mapping().unwrap().set(".ar-aa", 0);
    assert_cst_ok(&doc);
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
    assert_cst_ok(&doc);
    assert_eq!(doc.to_string(), "a: null\n.d:5-a: ''\n");
}

#[test]
fn set_with_document_terminator_key() {
    // Document terminator (`...`) and start marker (`---`) at column 0
    // begin a new stream document; used as a bare key they must be
    // quoted or the resulting text re-parses as a broken document.
    let doc = Document::from_str("existing: value\n").unwrap();
    doc.as_mapping().unwrap().set("...", "value");
    assert_cst_ok(&doc);
    let doc = Document::from_str("existing: value\n").unwrap();
    doc.as_mapping().unwrap().set("---", "value");
    assert_cst_ok(&doc);
}

#[test]
fn set_path_then_remove_path_collapses_empty_scaffold() {
    use yaml_edit::path::YamlPath;
    let doc = Document::from_str("a: 1\nb: 2\nc: 3\n").unwrap();
    doc.try_set_path("vvv.vvv.x", "").expect("set_path");
    assert_cst_ok(&doc);
    doc.try_remove_path("vvv.vvv.x").expect("remove_path");
    assert_cst_ok(&doc);
    // The emptied-out innermost mapping collapses into flow-empty `{}`
    // so path lookups still work (get_path("vvv.vvv") returns the empty
    // mapping) and the tree is well-formed.
    assert_eq!(doc.to_string(), "a: 1\nb: 2\nc: 3\nvvv:\n  vvv: {}\n");
}

#[test]
fn set_path_after_explicit_key_does_not_leave_blank_line() {
    use yaml_edit::path::YamlPath;
    let doc = Document::from_str("keys: !!set\n  ? a\n  ? b\n").unwrap();
    doc.try_set_path("a", "").expect("set_path");
    assert_cst_ok(&doc);
    assert!(
        !doc.to_string().contains("\n\n"),
        "unexpected blank line: {:?}",
        doc.to_string()
    );
}

// Regression tests for bugs surfaced by the post-conditioned mutation
// proptest in `tests/proptest_invariants.rs`.

#[test]
fn sequence_pop_last_item_collapses_to_flow_empty() {
    let doc = Document::from_str("s:\n  - a\n").unwrap();
    let seq = doc.as_mapping().unwrap().get_sequence("s").unwrap();
    seq.pop();
    assert_cst_ok(&doc);
    assert_eq!(doc.to_string(), "s: []\n");
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
    assert_cst_ok(&doc);
    assert_eq!(doc.to_string(), "s: []\n");
}

#[test]
fn sequence_clear_block_collapses_to_flow_empty() {
    let doc = Document::from_str("s:\n  - a\n  - b\n").unwrap();
    let seq = doc.as_mapping().unwrap().get_sequence("s").unwrap();
    seq.clear();
    assert_cst_ok(&doc);
    assert_eq!(doc.to_string(), "s: []\n");
}

#[test]
fn sequence_remove_last_entry_preserves_following_mapping_entry() {
    // The new-last SEQUENCE_ENTRY's trailing NEWLINE doubles as the
    // separator between the containing MAPPING_ENTRY and its next
    // sibling; removing the sequence's last entry must not strip it.
    let doc = Document::from_str("s:\n  - a\n  - b\n  - c\na: ''\n").unwrap();
    let seq = doc.as_mapping().unwrap().get_sequence("s").unwrap();
    seq.pop();
    assert_cst_ok(&doc);
    assert_eq!(doc.to_string(), "s:\n  - a\n  - b\na: ''\n");
}

#[test]
fn mapping_remove_only_nested_entry_keeps_anchor() {
    let doc = Document::from_str("a: &a\n  x: 1\nb: &b\n  x: 2\nm:\n  <<: [*a, *b]\n").unwrap();
    let removed = doc
        .as_mapping()
        .unwrap()
        .get_mapping("a")
        .unwrap()
        .remove("x");
    assert_cst_ok(&doc);
    assert!(removed.is_some());
    assert_eq!(
        doc.to_string(),
        "a: &a {}\nb: &b\n  x: 2\nm:\n  <<: [*a, *b]\n"
    );
}

#[test]
fn mapping_remove_only_nested_entry_keeps_comment() {
    let doc = Document::from_str("a: &a # c\n  x: 1\nb: 2\n").unwrap();
    doc.as_mapping()
        .unwrap()
        .get_mapping("a")
        .unwrap()
        .remove("x");
    check_shape(&doc);
    assert_eq!(doc.to_string(), "a: &a {} # c\nb: 2\n");
}

#[test]
fn mapping_remove_only_nested_entry_keeps_tag_and_comment() {
    let doc = Document::from_str("a: !t &a # c\n  x: 1\nb: 2\n").unwrap();
    doc.as_mapping()
        .unwrap()
        .get_mapping("a")
        .unwrap()
        .remove("x");
    check_shape(&doc);
    assert_eq!(doc.to_string(), "a: !t &a {} # c\nb: 2\n");
}

#[test]
fn mapping_remove_only_nested_entry_keeps_property_spacing() {
    let doc = Document::from_str("a: !t  &a\n  x: 1\nb: 2\n").unwrap();
    doc.as_mapping()
        .unwrap()
        .get_mapping("a")
        .unwrap()
        .remove("x");
    check_shape(&doc);
    assert_eq!(doc.to_string(), "a: !t  &a {}\nb: 2\n");
}

#[test]
fn sequence_remove_only_item_keeps_tag() {
    let doc = Document::from_str("s: !t\n  - a\nb: 2\n").unwrap();
    let seq = doc.as_mapping().unwrap().get_sequence("s").unwrap();
    seq.remove(0);
    check_shape(&doc);
    assert_eq!(doc.to_string(), "s: !t []\nb: 2\n");
}

#[test]
fn mapping_remove_only_nested_entry_keeps_comment_lines() {
    let doc = Document::from_str("a: # c\n  # d\n  x: 1\nb: 2\n").unwrap();
    doc.as_mapping()
        .unwrap()
        .get_mapping("a")
        .unwrap()
        .remove("x");
    assert_cst_ok(&doc);
    assert_eq!(doc.to_string(), "a: # c\n  # d\n  {}\nb: 2\n");
    let reparsed = Document::from_str(&doc.to_string()).unwrap();
    assert!(reparsed
        .as_mapping()
        .unwrap()
        .get_mapping("a")
        .unwrap()
        .is_empty());
}

#[test]
fn mapping_drain_keeps_inner_comment_lines() {
    let doc = Document::from_str("a:\n  x: 1\n  # d\n  y: 2\nb: 2\n").unwrap();
    let inner = doc.as_mapping().unwrap().get_mapping("a").unwrap();
    inner.remove("x");
    assert_cst_ok(&doc);
    inner.remove("y");
    assert_cst_ok(&doc);
    assert_eq!(doc.to_string(), "a:\n  # d\n  {}\nb: 2\n");
}

#[test]
fn mapping_drain_keeps_inner_comment_lines_after_anchor() {
    let doc = Document::from_str("a: &a\n  x: 1\n  # d\n\n  # e\n  y: 2\nb: 2\n").unwrap();
    let inner = doc.as_mapping().unwrap().get_mapping("a").unwrap();
    inner.remove("y");
    assert_cst_ok(&doc);
    inner.remove("x");
    assert_cst_ok(&doc);
    assert_eq!(doc.to_string(), "a: &a\n  # d\n\n  # e\n  {}\nb: 2\n");
}

#[test]
fn sequence_drain_keeps_inner_comment_lines() {
    let doc = Document::from_str("s:\n  - 1\n  # d\n  - 2\nb: 2\n").unwrap();
    let seq = doc.as_mapping().unwrap().get_sequence("s").unwrap();
    seq.remove(0);
    assert_cst_ok(&doc);
    seq.remove(0);
    assert_cst_ok(&doc);
    assert_eq!(doc.to_string(), "s:\n  # d\n  []\nb: 2\n");
}

#[test]
fn sequence_remove_only_item_keeps_anchor() {
    let doc = Document::from_str("s: &s\n  - a\nb: 2\n").unwrap();
    let seq = doc.as_mapping().unwrap().get_sequence("s").unwrap();
    seq.remove(0);
    assert_cst_ok(&doc);
    assert_eq!(doc.to_string(), "s: &s []\nb: 2\n");
}

#[test]
fn set_path_nested_sequence_indices() {
    // Nested index paths on fresh intermediates render as `outer
    // block, inner flow` (`s:\n  - [hi]\n`) rather than a compact
    // block that re-parses as a plain scalar.
    use yaml_edit::path::YamlPath;
    let doc = Document::from_str("a: 1\n").unwrap();
    doc.try_set_path("s[0][0]", "hi").expect("set_path");
    assert_cst_ok(&doc);
    let reparsed = Document::from_str(&doc.to_string()).unwrap();
    let got = reparsed
        .try_get_path("s[0][0]")
        .ok()
        .as_ref()
        .and_then(|n| n.as_scalar().map(|s| s.as_string()));
    assert_eq!(got.as_deref(), Some("hi"));
}

#[test]
fn parser_terminates_tagged_block_at_column_zero() {
    // Given `keys: !!set\n  ? a\n  ? b\na: ''\n`, the trailing
    // `a: ''` at column 0 belongs at the top level, not inside the
    // !!set block. parse_tagged_collection needs to anchor the
    // inner block on the indent it consumed so a dedent-to-zero
    // terminates it properly.
    let src = "keys: !!set\n  ? a\n  ? b\na: ''\n";
    let doc = Document::from_str(src).unwrap();
    let keys: Vec<String> = doc
        .as_mapping()
        .unwrap()
        .keys()
        .filter_map(|k| k.as_scalar().map(|s| s.as_string()))
        .collect();
    assert_eq!(keys, vec!["keys".to_string(), "a".to_string()]);
    assert_cst_ok(&doc);
}

#[test]
fn insert_before_in_flow_mapping_stays_flow() {
    // insert_before/move_before used to build a block-style entry
    // (with trailing NEWLINE) and splice it inside the `{}` of a
    // flow mapping, producing `{a: a\nf: 0}` -- a mixed shape that
    // re-parses with `a` as the multi-line scalar `"a f"`.
    let doc = Document::from_str("{f: 0}").unwrap();
    doc.as_mapping().unwrap().insert_before("f", "a", "hi");
    assert_cst_ok(&doc);
    assert_eq!(doc.to_string(), "{a: hi, f: 0}");
}

#[test]
fn insert_after_in_flow_mapping_stays_flow() {
    let doc = Document::from_str("{f: 0}").unwrap();
    doc.as_mapping().unwrap().insert_after("f", "a", "hi");
    assert_cst_ok(&doc);
    assert_eq!(doc.to_string(), "{f: 0, a: hi}");
}

#[test]
fn top_level_mapping_clear_renders_flow_empty() {
    // A top-level mapping has no enclosing MAPPING_ENTRY to collapse
    // into `key: {}`; without the explicit `{}` fallback in clear()
    // the doc renders as empty text and re-parses to no-mapping.
    let doc = Document::from_str("a: 1\nb: 2\n").unwrap();
    doc.as_mapping().unwrap().clear();
    assert_cst_ok(&doc);
    assert_eq!(doc.to_string(), "{}");
    let reparsed = Document::from_str(&doc.to_string()).unwrap();
    assert!(reparsed.as_mapping().is_some());
    assert!(reparsed.as_mapping().unwrap().is_empty());
}

#[test]
fn reorder_keeps_between_entry_comment_attached_to_predecessor() {
    // A comment between two entries counts as the predecessor's
    // postscript: it stays glued to the entry it visually followed
    // (`a`) rather than being lifted to the end of the mapping.
    let doc = Document::from_str("a: 1\n# note\nb: 2\n").unwrap();
    doc.as_mapping().unwrap().reorder_fields(["b", "a"]);
    assert_cst_ok(&doc);
    assert_eq!(doc.to_string(), "b: 2\na: 1\n# note\n");
}

#[test]
fn reorder_noop_preserves_between_entry_comment() {
    let doc = Document::from_str("a: 1\n# note\nb: 2\n").unwrap();
    doc.as_mapping()
        .unwrap()
        .reorder_fields(std::iter::empty::<&str>());
    assert_cst_ok(&doc);
    assert_eq!(doc.to_string(), "a: 1\n# note\nb: 2\n");
}

#[test]
fn reorder_after_add_preserves_entry_separator() {
    // insert_at_index used to prepend a NEWLINE at the MAPPING level as
    // a separator when the previous entry lacked one. reorder_fields
    // rebuilds the mapping's child list from the entry nodes alone
    // and dropped the standalone NEWLINE, smooshing entries together.
    // Fix: the previous entry gets its NEWLINE appended inside itself.
    let doc = Document::from_str("s:\n  - a\n  - b\n  - c\n").unwrap();
    doc.as_mapping().unwrap().get_sequence("s").unwrap().pop();
    assert_cst_ok(&doc);
    doc.as_mapping().unwrap().insert_at_index(1, "a", "");
    assert_cst_ok(&doc);
    doc.as_mapping()
        .unwrap()
        .reorder_fields(std::iter::empty::<&str>());
    assert_cst_ok(&doc);
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
    assert_cst_ok(&doc);
    assert_eq!(doc.to_string(), "s:\n  - b\n  - c\n");
}

#[test]
fn sequence_insert_into_empty_flow_reshapes_to_block() {
    let doc = Document::from_str("s: []\n").unwrap();
    let seq = doc.as_mapping().unwrap().get_sequence("s").unwrap();
    seq.insert(0, "x");
    assert_cst_ok(&doc);
    let reparsed = Document::from_str(&doc.to_string()).unwrap();
    let seq2 = reparsed.as_mapping().unwrap().get_sequence("s").unwrap();
    assert_eq!(seq2.len(), 1);
}

#[test]
fn sequence_push_into_nonempty_flow_stays_flow() {
    let doc = Document::from_str("s: [x]\n").unwrap();
    let seq = doc.as_mapping().unwrap().get_sequence("s").unwrap();
    seq.push("y");
    assert_cst_ok(&doc);
    assert_eq!(doc.to_string(), "s: [x, y]\n");
}

#[test]
fn sequence_insert_into_nonempty_flow_stays_flow() {
    let doc = Document::from_str("s: [x]\n").unwrap();
    let seq = doc.as_mapping().unwrap().get_sequence("s").unwrap();
    seq.insert(0, "y");
    assert_cst_ok(&doc);
    assert_eq!(doc.to_string(), "s: [y, x]\n");
}

#[test]
fn sequence_insert_at_head_into_single_entry_block() {
    let doc = Document::from_str("s:\n  - a\n").unwrap();
    let seq = doc.as_mapping().unwrap().get_sequence("s").unwrap();
    seq.insert(0, "b");
    assert_cst_ok(&doc);
    assert_eq!(doc.to_string(), "s:\n  - b\n  - a\n");
}

/// A sequence that is the document's own node starts at column 0, so a
/// pushed entry is a sibling of the others rather than nested inside the
/// last one.
///
/// detect_indentation fell through to a hardcoded two spaces for a root
/// sequence, so `push` wrote `"- a\n  - new\n"` -- a nested sequence that
/// as_sequence could no longer reach the new entry through.
#[test]
fn root_sequence_push_appends_a_sibling() {
    for (yaml, want) in [
        ("- a\n", "- a\n- new\n"),
        ("- a\n- b\n", "- a\n- b\n- new\n"),
    ] {
        let file = YamlFile::from_str(yaml).unwrap();
        let seq = file.document().unwrap().as_sequence().unwrap();
        seq.push("new");
        assert_file_cst_ok(&file);
        assert_eq!(file.to_string(), want, "{yaml:?}");

        let reparsed = YamlFile::from_str(&file.to_string()).unwrap();
        let seq = reparsed.document().unwrap().as_sequence().unwrap();
        assert_eq!(
            seq.get(seq.len() - 1)
                .unwrap()
                .as_scalar()
                .unwrap()
                .as_string(),
            "new",
            "{yaml:?}"
        );
    }
}

/// A sequence nested under a key keeps its own indentation.
#[test]
fn nested_sequence_push_keeps_its_indent() {
    let file = YamlFile::from_str("a:\n  - x\n").unwrap();
    let seq = file
        .document()
        .unwrap()
        .as_mapping()
        .unwrap()
        .get(yaml_edit::ScalarValue::from("a"))
        .unwrap()
        .as_sequence()
        .cloned()
        .unwrap();
    seq.push("new");
    assert_file_cst_ok(&file);
    assert_eq!(file.to_string(), "a:\n  - x\n  - new\n");
}

/// A complete node followed by more content before the key's colon, as in
/// `[q] x: v`, is not a valid implicit key. The parser keeps the extra
/// content in the key as a node of its own, and the validator reports it.
#[test]
fn node_followed_by_content_before_colon() {
    use yaml_edit::validator::Validator;
    const TWO_NODES: &str = "Key holds more than one node";
    const SAME_LINE: &str = "Block mapping cannot start on same line as mapping key";
    const INDENT: &str =
        "Sibling block mapping entries have inconsistent indentation (expected \"\", found \"  \")";
    for (yaml, expected) in [
        ("k:\n  [q] x: v\n", vec![(TWO_NODES, "2:7")]),
        ("k:\n  {q: 1} x: v\n", vec![(TWO_NODES, "2:10")]),
        ("k:\n  [q] x y: v\n", vec![(TWO_NODES, "2:7")]),
        ("- [q] x: v\n", vec![(TWO_NODES, "1:7")]),
        ("b:\n- [q] x: v\n", vec![(TWO_NODES, "2:7")]),
        ("[q] x: v\n", vec![(TWO_NODES, "1:5")]),
        ("&a [q] x: v\n", vec![(TWO_NODES, "1:8")]),
        ("a: &a 1\nk:\n  *a x: v\n", vec![(TWO_NODES, "3:6")]),
        ("a: &a 1\n*a x: v\n", vec![(TWO_NODES, "2:4")]),
        ("- &a 1\n- *a x: v\n", vec![(TWO_NODES, "2:6")]),
        (
            "k: [q] x: v\n",
            vec![(SAME_LINE, "1:4"), (TWO_NODES, "1:8")],
        ),
        (
            "a: &a 1\nk: *a x: v\n",
            vec![(SAME_LINE, "2:4"), (TWO_NODES, "2:7")],
        ),
        ("a: &a 1\nk: *a : v\n", vec![(SAME_LINE, "2:4")]),
        (
            "k: a\n  [q] x: v\n",
            vec![(INDENT, "2:1"), (TWO_NODES, "2:7")],
        ),
        (
            "k:\n  \"q\" x: v\n",
            vec![("Trailing content after quoted string", "2:7")],
        ),
    ] {
        let file = YamlFile::from_str(yaml).unwrap_or_else(|e| panic!("{yaml:?}: {e}"));
        assert_eq!(file.to_string(), yaml);
        assert_file_cst_ok(&file);
        let violations: Vec<_> = Validator::new()
            .validate_syntax(file.syntax())
            .into_iter()
            .map(|v| (v.message, v.location.unwrap()))
            .collect();
        let expected: Vec<_> = expected
            .into_iter()
            .map(|(message, at)| (message.to_string(), at.to_string()))
            .collect();
        assert_eq!(violations, expected, "{yaml:?}");
    }
}
