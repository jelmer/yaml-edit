//! Removing the last entry or item of a collection leaves an empty
//! collection in its place, written `{}` or `[]` where a block one would
//! be empty text.

mod common;

use rowan::ast::AstNode;
use std::str::FromStr;
use yaml_edit::{debug, Document};

#[track_caller]
fn check_edit(source: &str, edit: impl Fn(&Document), expected: &str) {
    let doc = Document::from_str(source).unwrap();
    edit(&doc);
    assert_eq!(doc.to_string(), expected, "editing {source:?}");
    common::assert_cst_ok(&doc);
    let reparsed = Document::from_str(expected).unwrap();
    assert_eq!(
        debug::shape_to_string(doc.syntax()),
        debug::shape_to_string(reparsed.syntax())
    );
}

#[test]
fn edit_through_handle_to_drained_nested_collection() {
    check_edit(
        "top:\n  b: 2\nx: 1\n",
        |d| {
            let m = d.as_mapping().unwrap().get_mapping("top").unwrap();
            m.remove("b");
            m.set("c", 3);
        },
        "top:\n  c: 3\nx: 1\n",
    );
    check_edit(
        "top:\n  - 1\n",
        |d| {
            let s = d.as_mapping().unwrap().get_sequence("top").unwrap();
            s.remove(0);
            s.push(3);
        },
        "top:\n  - 3\n",
    );
    check_edit(
        "top:\n  # c\n  b: 2\n",
        |d| {
            let m = d.as_mapping().unwrap().get_mapping("top").unwrap();
            m.remove("b");
            m.set("c", 3);
        },
        "top:\n  # c\n  c: 3\n",
    );
}

fn item_mapping(d: &Document) -> yaml_edit::Mapping {
    d.as_sequence()
        .unwrap()
        .get(0)
        .unwrap()
        .as_mapping()
        .cloned()
        .unwrap()
}

fn item_sequence(d: &Document) -> yaml_edit::Sequence {
    d.as_sequence()
        .unwrap()
        .get(0)
        .unwrap()
        .as_sequence()
        .cloned()
        .unwrap()
}

#[test]
fn remove_only_entry_of_mapping_in_sequence_item() {
    let remove = |d: &Document| {
        assert!(item_mapping(d).remove("b").is_some());
    };
    check_edit("- b: 2\n- 3\n", remove, "- {}\n- 3\n");
    check_edit("- b: 2\n", remove, "- {}\n");
    check_edit("- b: 2", remove, "- {}");
    check_edit("- &a\n  b: 2\n- 3\n", remove, "- &a\n  {}\n- 3\n");
    check_edit("- ? b\n- 3\n", remove, "- {}\n- 3\n");
}

#[test]
fn remove_only_item_of_sequence_in_sequence_item() {
    let remove = |d: &Document| {
        assert!(item_sequence(d).remove(0).is_some());
    };
    check_edit("- - 1\n- 3\n", remove, "- []\n- 3\n");
    check_edit("- - 1\n", remove, "- []\n");
    check_edit("- - 1", remove, "- []");
}

#[test]
fn clear_collection_in_sequence_item() {
    check_edit(
        "- - 1\n  - 2\n- 3\n",
        |d| item_sequence(d).clear(),
        "- []\n- 3\n",
    );
    check_edit(
        "- a: 1\n  b: 2\n- 3\n",
        |d| item_mapping(d).clear(),
        "- {}\n- 3\n",
    );
}

#[test]
fn clear_top_level_sequence() {
    let clear = |d: &Document| d.as_sequence().unwrap().clear();
    check_edit("- 1\n", clear, "[]\n");
    check_edit("- 1\n- 2\n", clear, "[]\n");
    check_edit("- 1\n- 2", clear, "[]");
    check_edit("---\n- 1\n", clear, "---\n[]\n");
}

/// Refilling a drained root collection takes the lines after it back from
/// the document, up to any `...`.
#[test]
fn refill_drained_root_collection_takes_lines_after_it() {
    let refill_mapping = |d: &Document| {
        let m = d.as_mapping().unwrap();
        m.remove("a");
        m.set("b", 2);
    };
    check_edit("a: 1\n# c\n", refill_mapping, "b: 2\n# c\n");
    check_edit("a: 1\n# c", refill_mapping, "b: 2\n# c");
    check_edit("!!map\na: 1\n# c\n", refill_mapping, "!!map\nb: 2\n# c\n");
    check_edit(
        "a: 1\n# c\n  # d\n\n...\n",
        refill_mapping,
        "b: 2\n# c\n  # d\n\n...\n",
    );
    check_edit(
        "- 1\n# c\n",
        |d| {
            let s = d.as_sequence().unwrap();
            s.remove(0);
            s.push(2);
        },
        "- 2\n# c\n",
    );
}

#[test]
fn edit_through_handle_to_drained_sequence_item() {
    check_edit(
        "- b: 2\n- 3\n",
        |d| {
            let m = item_mapping(d);
            m.remove("b");
            m.set("c", 3);
        },
        "- c: 3\n- 3\n",
    );
    check_edit(
        "- - 1\n- 3\n",
        |d| {
            let s = item_sequence(d);
            s.remove(0);
            s.push(2);
        },
        "- - 2\n- 3\n",
    );
}

fn key_node(d: &Document) -> yaml_edit::YamlNode {
    d.as_mapping().unwrap().keys().next().unwrap()
}

fn key_mapping(d: &Document) -> yaml_edit::Mapping {
    let key = key_node(d);
    match key.as_tagged() {
        Some(t) => t.as_mapping().unwrap(),
        None => key.as_mapping().cloned().unwrap(),
    }
}

fn key_sequence(d: &Document) -> yaml_edit::Sequence {
    let key = key_node(d);
    match key.as_tagged() {
        Some(t) => t.as_sequence().unwrap(),
        None => key.as_sequence().cloned().unwrap(),
    }
}

#[test]
fn remove_only_entry_of_explicit_key_mapping() {
    let remove = |d: &Document| {
        assert!(key_mapping(d).remove("a").is_some());
    };
    check_edit("? a: 1\n: v\n", remove, "? {}\n: v\n");
    check_edit("? a: 1\n: v\nz: 2\n", remove, "? {}\n: v\nz: 2\n");
    check_edit("? a: 1\n", remove, "? {}\n");
    check_edit("? a: 1", remove, "? {}");
    check_edit("? &x\n  a: 1\n: v\n", remove, "? &x\n  {}\n: v\n");
    check_edit("? !t\n  a: 1\n: v\n", remove, "? !t\n  {}\n: v\n");
}

#[test]
fn remove_only_item_of_explicit_key_sequence() {
    let remove = |d: &Document| {
        assert!(key_node(d).as_sequence().unwrap().remove(0).is_some());
    };
    check_edit("? - 1\n: v\n", remove, "? []\n: v\n");
    check_edit("? - 1\n: v\nz: 2\n", remove, "? []\n: v\nz: 2\n");
    check_edit("? - 1\n", remove, "? []\n");
    check_edit("? - 1", remove, "? []");
}

#[test]
fn clear_explicit_key_collection() {
    check_edit(
        "? a: 1\n  b: 2\n: v\n",
        |d| key_mapping(d).clear(),
        "? {}\n: v\n",
    );
    check_edit(
        "? - 1\n  - 2\n: v\n",
        |d| key_node(d).as_sequence().unwrap().clear(),
        "? []\n: v\n",
    );
}

#[test]
fn drain_sequence_item_keeps_comment_lines_after_it() {
    let remove_entry = |d: &Document| {
        assert!(item_mapping(d).remove("b").is_some());
    };
    check_edit("- b: 2\n  # c\n- 3\n", remove_entry, "- {}\n  # c\n- 3\n");
    check_edit("- b: 2\n  # c\n", remove_entry, "- {}\n  # c\n");
    check_edit("- b: 2\n  # c", remove_entry, "- {}\n  # c");
    check_edit(
        "- b: 2\n  # c\n\n  # d\n- 3\n",
        remove_entry,
        "- {}\n  # c\n\n  # d\n- 3\n",
    );
    check_edit(
        "- a: 1\n  b: 2\n  # c\n- 3\n",
        |d| item_mapping(d).clear(),
        "- {}\n  # c\n- 3\n",
    );
    let remove_item = |d: &Document| {
        assert!(item_sequence(d).remove(0).is_some());
    };
    check_edit("- - 1\n  # c\n- 3\n", remove_item, "- []\n  # c\n- 3\n");
    check_edit("- - 1\n  # c\n", remove_item, "- []\n  # c\n");
    check_edit(
        "- &a\n  b: 2\n  # c\n- 3\n",
        remove_entry,
        "- &a\n  {}\n  # c\n- 3\n",
    );
    check_edit(
        "x:\n  - - 2\n    # c\ny: 1\n",
        |d| {
            let s = d.as_mapping().unwrap().get_sequence("x").unwrap();
            s.get(0).unwrap().as_sequence().unwrap().remove(0);
        },
        "x:\n  - []\n    # c\ny: 1\n",
    );
    check_edit(
        "- - 1\n  - 2\n  # c\n- 3\n",
        |d| item_sequence(d).clear(),
        "- []\n  # c\n- 3\n",
    );
}

#[test]
fn drain_mapping_value_keeps_comment_line_indent() {
    let remove = |d: &Document| {
        let v = d.as_mapping().unwrap().get("top").unwrap();
        match v.as_mapping() {
            Some(m) => assert!(m.remove("b").is_some()),
            None => assert!(v.as_sequence().unwrap().remove(0).is_some()),
        }
    };
    check_edit(
        "top:\n  b: 2\n  # c\nx: 1\n",
        remove,
        "top: {}\n  # c\nx: 1\n",
    );
    check_edit(
        "top:\n  - 2\n  # c\nx: 1\n",
        remove,
        "top: []\n  # c\nx: 1\n",
    );
    check_edit(
        "top:\n  # a\n  b: 2\n  # c\nx: 1\n",
        remove,
        "top:\n  # a\n  {}\n  # c\nx: 1\n",
    );
    check_edit(
        "a:\n  top:\n    b: 2\n    # c\nx: 1\n",
        |d| {
            let a = d.as_mapping().unwrap().get_mapping("a").unwrap();
            a.get_mapping("top").unwrap().remove("b");
        },
        "a:\n  top: {}\n    # c\nx: 1\n",
    );
}

#[test]
fn drain_removed_sequence_item_with_comment_line() {
    let doc = Document::from_str("- b: 2\n  # c\n").unwrap();
    let item = doc.as_sequence().unwrap().remove(0).unwrap();
    let m = item.as_mapping().unwrap();
    assert!(m.remove("b").is_some());
    assert_eq!(m.to_string(), "{}");
}

#[test]
fn drain_compact_sequence_in_explicit_key() {
    let remove = |d: &Document| {
        assert!(key_sequence(d).remove(0).is_some());
    };
    let clear = |d: &Document| key_sequence(d).clear();
    for edit in [&remove as &dyn Fn(&Document), &clear] {
        check_edit("?\n- a\n: b\n", edit, "?\n  []\n: b\n");
        check_edit("?\n- a\n", edit, "?\n  []\n");
        check_edit("? &x\n- a\n: b\n", edit, "? &x\n  []\n: b\n");
        check_edit("? !t\n- a\n: b\n", edit, "? !t\n  []\n: b\n");
        check_edit("? # c\n- a\n: b\n", edit, "? # c\n  []\n: b\n");
    }
    check_edit(
        "x:\n  ?\n  - a\n  : b\n",
        |d| {
            let m = d.as_mapping().unwrap().get_mapping("x").unwrap();
            let k = m.keys().next().unwrap();
            k.as_sequence().unwrap().remove(0);
        },
        "x:\n  ?\n    []\n  : b\n",
    );
}

#[test]
fn drain_indented_collection_in_explicit_key() {
    check_edit(
        "?\n  a: 1\n: b\n",
        |d| {
            assert!(key_mapping(d).remove("a").is_some());
        },
        "?\n  {}\n: b\n",
    );
    check_edit(
        "?\n  - a\n: b\n",
        |d| {
            assert!(key_sequence(d).remove(0).is_some());
        },
        "?\n  []\n: b\n",
    );
}

#[test]
fn drain_value_of_explicit_key_entry() {
    check_edit(
        "? a\n:\n  - x\nb: 1\n",
        |d| {
            let s = d.as_mapping().unwrap().get_sequence("a").unwrap();
            s.remove(0);
        },
        "? a\n: []\nb: 1\n",
    );
}

#[test]
fn drain_sequence_whose_last_item_ends_in_a_comment() {
    let clear = |d: &Document| d.as_mapping().unwrap().get_sequence("a").unwrap().clear();
    let remove = |d: &Document| {
        d.as_mapping()
            .unwrap()
            .get_sequence("a")
            .unwrap()
            .remove(0)
            .unwrap();
    };
    check_edit("? a\n:\n  - x  # c\nb: 1\n", clear, "? a\n: []\nb: 1\n");
    check_edit("? a\n:\n  - x  # c\nb: 1\n", remove, "? a\n: []\nb: 1\n");
    check_edit("a:\n  - x  # c\nb: 1\n", clear, "a: []\nb: 1\n");
    check_edit(
        "- - x  # c\n- 1\n",
        |d| item_sequence(d).clear(),
        "- []\n- 1\n",
    );
}

#[test]
fn remove_last_entry_of_top_level_collection() {
    let remove = |d: &Document| {
        d.as_mapping().unwrap().remove("a").unwrap();
    };
    check_edit("a: 1\n", remove, "{}\n");
    check_edit("a: 1", remove, "{}");
    check_edit("---\na: 1\n", remove, "---\n{}\n");
    check_edit(
        "- 1\n",
        |d| {
            d.as_sequence().unwrap().remove(0).unwrap();
        },
        "[]\n",
    );
}

/// A block collection drained by `remove` or `clear` is written `{}` or
/// `[]`, but goes back to block style when it gets an entry again.
#[test]
fn refill_drained_top_level_collection() {
    let top = |d: &Document| d.as_mapping().unwrap();
    let seq = |d: &Document| d.as_sequence().unwrap();
    check_edit(
        "a: 1\n",
        |d| {
            top(d).remove("a");
            top(d).set("c", 3);
        },
        "c: 3\n",
    );
    check_edit(
        "a: 1\nb: 2\n",
        |d| {
            top(d).clear();
            top(d).set("c", 3);
        },
        "c: 3\n",
    );
    check_edit(
        "---\na: 1\n",
        |d| {
            top(d).remove("a");
            top(d).set("c", 3);
        },
        "---\nc: 3\n",
    );
    check_edit(
        "- 1\n",
        |d| {
            seq(d).remove(0);
            seq(d).push(3);
        },
        "- 3\n",
    );
    check_edit(
        "- 1\n- 2\n",
        |d| {
            seq(d).clear();
            seq(d).insert(0, 3);
        },
        "- 3\n",
    );
}

/// The comment and blank lines after a block root collection are part of
/// it, so they go back into it when it is refilled.
#[test]
fn refill_drained_top_level_collection_with_lines_after_it() {
    let top = |d: &Document| d.as_mapping().unwrap();
    let seq = |d: &Document| d.as_sequence().unwrap();
    check_edit(
        "- a\n# d\n",
        |d| {
            seq(d).clear();
            seq(d).push("x");
        },
        "- x\n# d\n",
    );
    check_edit(
        "a: 1\n# d\n\n# e\n",
        |d| {
            top(d).remove("a");
            top(d).set("x", 1);
        },
        "x: 1\n# d\n\n# e\n",
    );
    check_edit(
        "- a\n  # d\n...\n",
        |d| {
            seq(d).clear();
            seq(d).insert(0, "x");
        },
        "- x\n  # d\n...\n",
    );
}

#[test]
fn refill_drained_mapping_value() {
    let refill = |d: &Document| {
        let m = d.as_mapping().unwrap().get_mapping("k").unwrap();
        m.clear();
        m.set("c", 3);
    };
    check_edit("k:\n  a: 1\n", refill, "k:\n  c: 3\n");
    check_edit("k:\n  a: 1\nz: 2\n", refill, "k:\n  c: 3\nz: 2\n");
    check_edit("k: !t\n  a: 1\n", refill, "k: !t\n  c: 3\n");
    check_edit("k: &x\n  a: 1\n", refill, "k: &x\n  c: 3\n");
    check_edit("k: # c\n  a: 1\n", refill, "k: # c\n  c: 3\n");
    check_edit("k: !t # c\n  a: 1\n", refill, "k: !t # c\n  c: 3\n");
    check_edit(
        "k:\n  a: 1\n  # c\nz: 2\n",
        refill,
        "k:\n  c: 3\n  # c\nz: 2\n",
    );
    check_edit(
        "k:\n  - 1\nz: 2\n",
        |d| {
            let s = d.as_mapping().unwrap().get_sequence("k").unwrap();
            s.remove(0);
            s.push(3);
        },
        "k:\n  - 3\nz: 2\n",
    );
    check_edit(
        "? a\n:\n  - x\nb: 1\n",
        |d| {
            let s = d.as_mapping().unwrap().get_sequence("a").unwrap();
            s.remove(0);
            s.push("y");
        },
        "? a\n:\n  - y\nb: 1\n",
    );
}

#[test]
fn refill_drained_sequence_item() {
    check_edit(
        "- &a\n  b: 2\n- 3\n",
        |d| {
            let m = item_mapping(d);
            m.remove("b");
            m.set("c", 3);
        },
        "- &a\n  c: 3\n- 3\n",
    );
    check_edit(
        "- - 1\n  - 2\n- 3\n",
        |d| {
            let s = item_sequence(d);
            s.clear();
            s.push(4);
            s.push(5);
        },
        "- - 4\n  - 5\n- 3\n",
    );
    check_edit(
        "- b: 2\n  # c\n- 3\n",
        |d| {
            let m = item_mapping(d);
            m.remove("b");
            m.set("c", 3);
        },
        "- c: 3\n  # c\n- 3\n",
    );
}

#[test]
fn refill_drained_explicit_key() {
    check_edit(
        "? a: 1\n: v\n",
        |d| {
            let m = key_mapping(d);
            m.remove("a");
            m.set("c", 3);
        },
        "? c: 3\n: v\n",
    );
    check_edit(
        "? - 1\n: v\n",
        |d| {
            let s = key_sequence(d);
            s.remove(0);
            s.push(3);
            s.push(4);
        },
        "? - 3\n  - 4\n: v\n",
    );
}

/// Only a collection that was block style goes back to it: a `{}` or `[]`
/// written in the source, or a drained flow collection, stays flow.
#[test]
fn refill_flow_collection_stays_flow() {
    check_edit("{}\n", |d| d.as_mapping().unwrap().set("c", 3), "{c: 3}\n");
    check_edit(
        "a: []\n",
        |d| d.as_mapping().unwrap().get_sequence("a").unwrap().push(3),
        "a: [3]\n",
    );
    check_edit("- {}\n", |d| item_mapping(d).set("c", 3), "- {c: 3}\n");
    check_edit(
        "a: {x: 1}\n",
        |d| {
            let m = d.as_mapping().unwrap().get_mapping("a").unwrap();
            m.remove("x");
            m.set("c", 3);
        },
        "a: {c: 3}\n",
    );
}
