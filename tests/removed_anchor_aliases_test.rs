//! Removing or replacing a node that holds anchors keeps the aliases that
//! referred to them resolving to the same content: the first such alias
//! becomes a copy of the anchored node.

mod common;

use rowan::ast::AstNode;
use std::str::FromStr;
use yaml_edit::anchor_resolution::DocumentResolvedExt;
use yaml_edit::path::YamlPath;
use yaml_edit::{debug, Alias, Document, YamlFile, YamlValue};

#[track_caller]
fn assert_edited(doc: &Document, expected: &str) {
    assert_eq!(doc.to_string(), expected);
    common::assert_cst_ok(doc);
    let reparsed = Document::from_str(expected).unwrap();
    assert_eq!(
        debug::shape_to_string(doc.syntax()),
        debug::shape_to_string(reparsed.syntax())
    );
}

/// Assert that `doc` resolves to the same value as `meaning`, a document
/// without aliases.
#[track_caller]
fn assert_means(doc: &Document, meaning: &str) {
    let meaning = Document::from_str(meaning).unwrap();
    assert_eq!(
        YamlValue::from_document(doc).unwrap(),
        YamlValue::from_document(&meaning).unwrap()
    );
}

#[test]
fn mapping_set_keeps_nested_anchor() {
    let cases = [
        ("b: {c: &y 1}\nd: *y\n", "b: 6\nd: &y 1\n", "b: 6\nd: 1\n"),
        (
            "b: {c: &y 1}\nd: *y\ne: *y\n",
            "b: 6\nd: &y 1\ne: *y\n",
            "b: 6\nd: 1\ne: 1\n",
        ),
        (
            "b:\n  c: &y\n    e: 1\nd: *y\n",
            "b: 6\nd: &y\n  e: 1\n",
            "b: 6\nd: {e: 1}\n",
        ),
        (
            "b: [1, &y [2, 3]]\nd: *y  # c\n",
            "b: 6\nd: &y [2, 3]  # c\n",
            "b: 6\nd: [2, 3]\n",
        ),
        (
            "b: {c: !!str &y 1}\nd: *y\n",
            "b: 6\nd: !!str &y 1\n",
            "b: 6\nd: !!str 1\n",
        ),
        (
            "b: {c: &y !!str 1}\nd: *y\n",
            "b: 6\nd: &y !!str 1\n",
            "b: 6\nd: !!str 1\n",
        ),
        (
            "{b: {c: &y 1}, d: *y}\n",
            "{b: 6, d: &y 1}\n",
            "{b: 6, d: 1}\n",
        ),
    ];
    for (source, expected, meaning) in cases {
        let doc = Document::from_str(source).unwrap();
        doc.as_mapping().unwrap().set("b", 6);
        assert_edited(&doc, expected);
        assert_means(&doc, meaning);
    }
}

#[test]
fn mapping_set_keeps_top_anchor_and_nested_ones() {
    let doc = Document::from_str("b: &x {c: &y 1}\nd: *x\ne: *y\n").unwrap();
    doc.as_mapping().unwrap().set("b", 6);
    assert_edited(&doc, "b: &x 6\nd: *x\ne: &y 1\n");
    assert_means(&doc, "b: 6\nd: 6\ne: 1\n");
}

#[test]
fn mapping_set_to_alias_keeps_old_anchor() {
    // An alias cannot carry the old anchor, so its first alias does.
    let doc = Document::from_str("a: &z 0\nb: &x 2\nc: *x\nd: *x\n").unwrap();
    doc.as_mapping().unwrap().set("b", Alias::new("z"));
    assert_edited(&doc, "a: &z 0\nb: *z\nc: &x 2\nd: *x\n");
    assert_means(&doc, "a: 0\nb: 0\nc: 2\nd: 2\n");

    let doc = Document::from_str("- &z 0\n- &x 2\n- *x\n").unwrap();
    doc.as_sequence().unwrap().set(1, Alias::new("z"));
    assert_edited(&doc, "- &z 0\n- *z\n- &x 2\n");
    assert_means(&doc, "[0, 0, 2]\n");
}

#[test]
fn copy_does_not_capture_aliases_of_an_earlier_anchor() {
    // After `b` goes, `*y` must not fall back to the earlier `&y 0`.
    let doc = Document::from_str("a: &y 0\nx: *y\nb: {c: &y 1}\nd: *y\n").unwrap();
    doc.as_mapping().unwrap().set("b", 6);
    assert_edited(&doc, "a: &y 0\nx: *y\nb: 6\nd: &y 1\n");
    assert_means(&doc, "a: 0\nx: 0\nb: 6\nd: 1\n");
}

#[test]
fn later_redefinition_is_left_alone() {
    let doc = Document::from_str("b: {c: &y 1}\nd: *y\ne: &y 2\nf: *y\n").unwrap();
    doc.as_mapping().unwrap().set("b", 6);
    assert_edited(&doc, "b: 6\nd: &y 1\ne: &y 2\nf: *y\n");
    assert_means(&doc, "b: 6\nd: 1\ne: 2\nf: 2\n");

    // With no alias between, the redefinition takes the later ones anyway.
    let doc = Document::from_str("b: {c: &y 1}\ne: &y 2\nf: *y\n").unwrap();
    doc.as_mapping().unwrap().set("b", 6);
    assert_edited(&doc, "b: 6\ne: &y 2\nf: *y\n");
}

#[test]
fn new_value_anchor_does_not_capture_aliases() {
    let new = Document::from_str("k: {e: &y 2}\n").unwrap();
    let doc = Document::from_str("b: {c: &y 1}\nd: *y\n").unwrap();
    doc.as_mapping()
        .unwrap()
        .set("b", new.as_mapping().unwrap().get("k").unwrap());
    assert_edited(&doc, "b: {e: &y 2}\nd: &y 1\n");
    assert_means(&doc, "b: {e: 2}\nd: 1\n");
}

#[test]
fn nested_anchor_in_copy_stays_when_used() {
    let doc = Document::from_str("b: &o {c: &y 1}\nd: *o\ne: *y\n").unwrap();
    doc.as_mapping().unwrap().remove("b");
    assert_edited(&doc, "d: &o {c: &y 1}\ne: *y\n");
    assert_means(&doc, "d: {c: 1}\ne: 1\n");
}

#[test]
fn nested_anchor_in_copy_goes_when_it_would_capture() {
    // `*m` refers to `m2`, which the copy of `b` now precedes.
    let doc = Document::from_str("b: &o {c: &m 2}\nm2: &m 1\ne: *o\nf: *m\n").unwrap();
    doc.as_mapping().unwrap().remove("b");
    assert_edited(&doc, "m2: &m 1\ne: &o {c: 2}\nf: *m\n");
    assert_means(&doc, "m2: 1\ne: {c: 2}\nf: 1\n");

    // An alias inside the copy to the dropped anchor is expanded too.
    let doc = Document::from_str("b: &o {c: &m 2, d: *m}\nm2: &m 1\ne: *o\nf: *m\n").unwrap();
    doc.as_mapping().unwrap().remove("b");
    assert_edited(&doc, "m2: &m 1\ne: &o {c: 2, d: 2}\nf: *m\n");
    assert_means(&doc, "m2: 1\ne: {c: 2, d: 2}\nf: 1\n");
}

#[test]
fn alias_in_copy_to_shadowed_anchor() {
    // Inside the copy `*z` would refer to `d`, so it is expanded.
    let doc = Document::from_str("a: &z 1\nb: &o {c: *z}\nd: &z 2\ne: *o\n").unwrap();
    doc.as_mapping().unwrap().remove("b");
    assert_edited(&doc, "a: &z 1\nd: &z 2\ne: &o {c: 1}\n");
    assert_means(&doc, "a: 1\nd: 2\ne: {c: 1}\n");
}

#[test]
fn alias_in_copy_to_anchor_inside_it() {
    let doc = Document::from_str("b: &o {c: &q 1, d: *q}\ne: *o\n").unwrap();
    doc.as_mapping().unwrap().remove("b");
    assert_edited(&doc, "e: &o {c: &q 1, d: *q}\n");
    assert_means(&doc, "e: {c: 1, d: 1}\n");
}

#[test]
fn mapping_remove_keeps_anchors() {
    let cases = [
        (
            "a: &x 1\nb: *x\nc: *x\n",
            "b: &x 1\nc: *x\n",
            "b: 1\nc: 1\n",
        ),
        ("&k a: 1\nb: *k\n", "b: &k a\n", "b: a\n"),
        (
            "a:\n  - &x\n    e: 1\nb:\n  - *x\n",
            "b:\n  - &x\n    e: 1\n",
            "b: [{e: 1}]\n",
        ),
        ("a: &x 1\nb: [*x, *x]\n", "b: [&x 1, *x]\n", "b: [1, 1]\n"),
        (
            "a: &x\n  e: 1\nb: [*x]\n",
            "b: [&x {e: 1}]\n",
            "b: [{e: 1}]\n",
        ),
        (
            "a: &x\n  e: 1\nb: {c: *x}\n",
            "b: {c: &x {e: 1}}\n",
            "b: {c: {e: 1}}\n",
        ),
        ("a: &x k\n*x : 1\n", "&x k : 1\n", "k: 1\n"),
        ("a: &x [k]\n*x : 1\n", "&x [k] : 1\n", "[k]: 1\n"),
        ("a: &x [k]\n? *x\n: 1\n", "? &x [k]\n: 1\n", "[k]: 1\n"),
        (
            "a: !!map &x\n  e: 1\nb: [*x]\n",
            "b: [!!map &x {e: 1}]\n",
            "b: [!!map {e: 1}]\n",
        ),
        (
            "a: {p: &x k, q: &y 2}\n*x : *y\n",
            "&x k : &y 2\n",
            "k: 2\n",
        ),
        // `:` may be part of an anchor name (W5VH).
        ("a: &a: 1\nb: *a:\n", "b: &a: 1\n", "b: 1\n"),
        ("a: &x: k\n*x: : 1\n", "&x: k : 1\n", "k: 1\n"),
    ];
    for (source, expected, meaning) in cases {
        let doc = Document::from_str(source).unwrap();
        assert!(doc.as_mapping().unwrap().remove("a").is_some());
        assert_edited(&doc, expected);
        assert_means(&doc, meaning);
    }
}

#[test]
fn mapping_remove_keeps_merge_source() {
    let doc = Document::from_str("base: &base {x: 1}\nchild:\n  <<: *base\n  y: 2\n").unwrap();
    let before = doc.get_resolved("child").unwrap();
    doc.as_mapping().unwrap().remove("base");
    assert_edited(&doc, "child:\n  <<: &base {x: 1}\n  y: 2\n");
    assert_eq!(doc.get_resolved("child").unwrap(), before);

    let doc = Document::from_str("base: &base {x: 1}\nchild: {<<: [*base], y: 2}\n").unwrap();
    let before = doc.get_resolved("child").unwrap();
    doc.as_mapping().unwrap().remove("base");
    assert_edited(&doc, "child: {<<: [&base {x: 1}], y: 2}\n");
    assert_eq!(doc.get_resolved("child").unwrap(), before);
}

#[test]
fn other_mapping_removals_keep_anchors() {
    let doc = Document::from_str("a: &x 1\na: 2\nb: *x\n").unwrap();
    doc.as_mapping().unwrap().remove_nth_occurrence("a", 0);
    assert_edited(&doc, "a: 2\nb: &x 1\n");

    let doc = Document::from_str("a: {c: &x 1}\nb: *x\n").unwrap();
    doc.as_mapping().unwrap().get_mapping("a").unwrap().clear();
    assert_edited(&doc, "a: {}\nb: &x 1\n");

    let doc = Document::from_str("a: &x 1\nb: *x\n").unwrap();
    let mapping = doc.as_mapping().unwrap();
    mapping
        .find_all_entries_by_key("a")
        .next()
        .unwrap()
        .remove();
    assert_edited(&doc, "b: &x 1\n");

    let doc = Document::from_str("a: &x 1\nb: *x\n").unwrap();
    let mapping = doc.as_mapping().unwrap();
    match mapping.entry("a") {
        yaml_edit::Entry::Occupied(e) => {
            e.remove_entry();
        }
        yaml_edit::Entry::Vacant(_) => panic!("a is present"),
    }
    assert_edited(&doc, "b: &x 1\n");

    let doc = Document::from_str("a: {c: &x 1}\nb: *x\n").unwrap();
    doc.as_mapping()
        .unwrap()
        .insert_at_index_preserving(0, "a", 6);
    assert_edited(&doc, "a: 6\nb: &x 1\n");

    let doc = Document::from_str("a: &x 1\nb: 2\nc: *x\n").unwrap();
    doc.as_mapping().unwrap().move_after("b", "a", 6);
    assert_edited(&doc, "b: 2\na: 6\nc: &x 1\n");

    let doc = Document::from_str("a: &x 1\nb: *x\n").unwrap();
    assert!(doc.remove("a").is_some());
    assert_edited(&doc, "b: &x 1\n");
}

#[test]
fn sequence_edits_keep_anchors() {
    let doc = Document::from_str("- {c: &y 1}\n- *y\n").unwrap();
    doc.as_sequence().unwrap().set(0, 6);
    assert_edited(&doc, "- 6\n- &y 1\n");

    let doc = Document::from_str("[{c: &y 1}, *y]\n").unwrap();
    doc.as_sequence().unwrap().set(0, 6);
    assert_edited(&doc, "[6, &y 1]\n");

    let doc = Document::from_str("- &y 1\n- *y\n- *y\n").unwrap();
    doc.as_sequence().unwrap().remove(0);
    assert_edited(&doc, "- &y 1\n- *y\n");
    assert_means(&doc, "[1, 1]\n");

    let doc = Document::from_str("- [&y 1, 2]\n- *y\n").unwrap();
    doc.as_sequence().unwrap().pop();
    assert_edited(&doc, "- [&y 1, 2]\n");

    let doc = Document::from_str("a: [&y 1, 2]\nb: *y\n").unwrap();
    doc.as_mapping().unwrap().get_sequence("a").unwrap().clear();
    assert_edited(&doc, "a: []\nb: &y 1\n");
}

#[test]
fn copy_of_empty_node_leaves_no_trailing_space() {
    let doc = Document::from_str("- &x\n- *x\n").unwrap();
    doc.as_sequence().unwrap().remove(0);
    assert_edited(&doc, "- &x\n");

    let doc = Document::from_str("a: &x\nb: *x\n").unwrap();
    doc.as_mapping().unwrap().remove("a");
    assert_edited(&doc, "b: &x\n");
}

#[test]
fn path_edits_keep_anchors() {
    let doc = Document::from_str("a:\n  b: {c: &y 1}\nd: *y\n").unwrap();
    doc.try_set_path("a.b", 2).unwrap();
    assert_edited(&doc, "a:\n  b: 2\nd: &y 1\n");

    let doc = Document::from_str("a:\n  b: {c: &y 1}\n  e: 3\nd: *y\n").unwrap();
    doc.try_remove_path("a.b").unwrap();
    assert_edited(&doc, "a:\n  e: 3\nd: &y 1\n");
}

#[test]
fn recursive_anchor_is_copied_whole() {
    let doc = Document::from_str("b: &a [1, *a]\nc: *a\n").unwrap();
    doc.as_mapping().unwrap().remove("b");
    assert_edited(&doc, "c: &a [1, *a]\n");
}

#[test]
fn other_documents_are_left_alone() {
    let file = YamlFile::from_str("b: {c: &y 1}\nd: *y\n---\ne: *y\n").unwrap();
    file.document().unwrap().as_mapping().unwrap().set("b", 6);
    assert_eq!(file.to_string(), "b: 6\nd: &y 1\n---\ne: *y\n");
    common::assert_file_cst_ok(&file);
}

#[test]
fn alias_with_properties_gets_a_copy() {
    // YAML does not allow properties on an alias, but the parser accepts
    // them. The copy takes them over, and so leaves off the anchor it was
    // copied with: a tag would change what later aliases refer to, and a
    // node has room for one anchor only.
    let cases = [
        ("a: &a 1\nk: !t *a\n", "k: !t 1\n"),
        ("a: &a 1\nk: !t *a\nl: *a\n", "k: !t 1\nl: &a 1\n"),
        ("a: &a 1\nk: &b *a\nl: *b\n", "k: &b 1\nl: *b\n"),
        ("a: &a 1\nk: &b *a\nl: *a\n", "k: &b 1\nl: &a 1\n"),
        ("a: &a 1\nk: !t &b *a\nl: *b\n", "k: &b !t 1\nl: *b\n"),
        ("a: !u &a 1\nk: &b *a\nl: *a\n", "k: &b !u 1\nl: !u &a 1\n"),
        ("a: &a !u 1\nk: &b *a\nl: *a\n", "k: &b !u 1\nl: &a !u 1\n"),
        ("a: &a 1\nk: [!t *a]\n", "k: [!t 1]\n"),
        ("a: &a 1\nk: {l: &b *a}\n", "k: {l: &b 1}\n"),
        ("a: &a\n  x: 1\nk: !t *a\n", "k: !t\n  x: 1\n"),
        ("a: &a\n  x: 1\nk:\n- !t *a\n", "k:\n- !t\n  x: 1\n"),
        ("a: &a\n- 1\nk:\n  l: !t *a\n", "k:\n  l: !t\n    - 1\n"),
        ("a: &a |\n  x\nk: !t *a\n", "k: !t |\n  x\n"),
        (
            "a: &a {x: &c 1}\nk: !t *a\nl: *c\n",
            "k: !t {x: &c 1}\nl: *c\n",
        ),
    ];
    for (source, expected) in cases {
        let doc = Document::from_str(source).unwrap();
        doc.as_mapping().unwrap().remove("a");
        assert_edited(&doc, expected);
    }

    let doc = Document::from_str("- &a 1\n- !t *a\n- &b *a\n- *b\n").unwrap();
    doc.as_sequence().unwrap().remove(0);
    assert_edited(&doc, "- !t 1\n- &b 1\n- *b\n");
}

#[test]
fn tagged_alias_to_tagged_node_keeps_its_own_tag() {
    // The tag of the alias is what was written at its place, so it takes
    // the place of that of the node.
    let cases = [
        ("a: &a !u 1\nk: !t *a\n", "k: !t 1\n"),
        ("a: !u &a 1\nk: [!t *a]\nl: *a\n", "k: [!t 1]\nl: !u &a 1\n"),
        ("a: &a !u\n  x: 1\nk: !t *a\n", "k: !t\n  x: 1\n"),
        (
            "a: &a !u 1\nk: !t &b *a\nl: *a\nm: *b\n",
            "k: &b !t 1\nl: &a !u 1\nm: *b\n",
        ),
    ];
    for (source, expected) in cases {
        let doc = Document::from_str(source).unwrap();
        doc.as_mapping().unwrap().remove("a");
        assert_edited(&doc, expected);
    }
}

#[test]
fn key_alias_with_properties_gets_a_copy() {
    let cases = [
        ("a: &a 1\n? !t *a\n: v\n", "? !t 1\n: v\n"),
        ("a: &a 1\n? &b *a\n: v\nc: *b\n", "? &b 1\n: v\nc: *b\n"),
        (
            "a: &a 1\n? !t &b *a\n: v\nc: *b\n",
            "? &b !t 1\n: v\nc: *b\n",
        ),
        ("a: &a !u 1\n? !t *a\n: v\n", "? !t 1\n: v\n"),
        ("a: &a\n  x: 1\n? !t *a\n: v\n", "? !t {x: 1}\n: v\n"),
        ("a: &a 1\nb: {!t *a : v}\n", "b: {!t 1 : v}\n"),
        ("a: &a 1\nb: {&b *a : v, c: *b}\n", "b: {&b 1 : v, c: *b}\n"),
    ];
    for (source, expected) in cases {
        let doc = Document::from_str(source).unwrap();
        doc.as_mapping().unwrap().remove("a");
        assert_edited(&doc, expected);
    }
}

#[test]
fn rename_key_keeps_anchors() {
    let doc = Document::from_str("&k a: 1\nb: *k\n").unwrap();
    assert!(doc.as_mapping().unwrap().rename_key("a", "c"));
    assert_edited(&doc, "&k c: 1\nb: *k\n");
    assert_means(&doc, "c: 1\nb: c\n");

    let doc = Document::from_str("[&y p]: 1\nb: *y\n").unwrap();
    let mapping = doc.as_mapping().unwrap();
    let key = mapping.keys().next().unwrap();
    assert!(mapping.rename_key(&key, "c"));
    assert_edited(&doc, "c: 1\nb: &y p\n");
    assert_means(&doc, "c: 1\nb: p\n");
}

#[test]
fn tagged_alias_to_block_mapping_with_flow_key() {
    // The mapping starts with its key's `{` or `[`, but is still a block
    // mapping, which goes on the line after the tag.
    for (source, expected) in [
        ("a: &a\n  {x: 1}: 2\nk: !t *a\n", "k: !t\n  {x: 1}: 2\n"),
        ("a: &a\n  [x]: 2\nk:\n- !t *a\n", "k:\n- !t\n  [x]: 2\n"),
    ] {
        let doc = Document::from_str(source).unwrap();
        doc.as_mapping().unwrap().remove("a");
        assert_edited(&doc, expected);
    }
}
