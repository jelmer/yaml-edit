//! Adding an entry at the end of input that has no final line break keeps
//! it without one, whichever method adds it.

mod common;
use common::check_edit;

use yaml_edit::{Document, Mapping, Sequence};

fn root(doc: &Document) -> Mapping {
    doc.as_mapping().unwrap()
}

fn seq(doc: &Document) -> Sequence {
    doc.as_sequence().unwrap()
}

#[test]
fn mapping_methods_keep_unterminated_end() {
    for source in ["a: 1", "? a\n: 1"] {
        let prefix = source.to_string() + "\n";
        let expected = prefix.clone() + "b: 2";
        let explicit = source.starts_with('?');
        let expected = if explicit {
            prefix.clone() + "? b\n: 2"
        } else {
            expected
        };
        check_edit(source, |d| root(d).set("b", 2), &expected);
        check_edit(
            source,
            |d| {
                root(d).insert_after("a", "b", 2);
            },
            &expected,
        );
        check_edit(source, |d| root(d).insert_at_index(1, "b", 2), &expected);
        check_edit(
            source,
            |d| root(d).set_with_field_order("b", 2, ["a", "b"]),
            &expected,
        );
    }
    check_edit("a: 1\n# c", |d| root(d).set("b", 2), "a: 1\n# c\nb: 2");
    check_edit("a: |\n  x", |d| root(d).set("b", 2), "a: |\n  x\nb: 2");
    check_edit(
        "a:\n  x: 1",
        |d| root(d).get_mapping("a").unwrap().set("y", 2),
        "a:\n  x: 1\n  y: 2",
    );
    check_edit("a:\n  x: 1", |d| root(d).set("b", 2), "a:\n  x: 1\nb: 2");
}

#[test]
fn sequence_methods_keep_unterminated_end() {
    check_edit("- a", |d| seq(d).push("b"), "- a\n- b");
    check_edit("- a", |d| seq(d).insert(1, "b"), "- a\n- b");
    check_edit("- a", |d| seq(d).insert(5, "b"), "- a\n- b");
    check_edit(
        "k:\n  - a",
        |d| root(d).get_sequence("k").unwrap().insert(1, "b"),
        "k:\n  - a\n  - b",
    );
}
