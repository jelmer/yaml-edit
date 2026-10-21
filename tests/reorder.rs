//! Moving entries around a mapping leaves the layout that belongs to a
//! position where it was: the indentation of a line, the separators of a
//! flow mapping and the line break ending the last entry, or its absence.

mod common;
use common::{assert_cst_ok, assert_edited, assert_file_edited};
use std::str::FromStr;
use yaml_edit::{Document, Mapping, YamlFile};

#[track_caller]
fn check_reorder(
    source: &str,
    order: &[&str],
    expected: &str,
    mapping: impl Fn(&Document) -> Mapping,
) {
    let doc = Document::from_str(source).unwrap();
    mapping(&doc).reorder_fields(order.iter().copied());
    assert_edited(&doc, expected);
}

fn root(doc: &Document) -> Mapping {
    doc.as_mapping().unwrap()
}

fn top(doc: &Document) -> Mapping {
    doc.as_mapping().unwrap().get_mapping("top").unwrap()
}

#[test]
fn reorder_flow_mapping() {
    for (source, expected) in [
        ("{a: 1, b: 2}", "{b: 2, a: 1}"),
        ("{a: 1, b: 2}\n", "{b: 2, a: 1}\n"),
        ("{ a: 1, b: 2 }\n", "{ b: 2, a: 1 }\n"),
        ("{a: 1, b: 2, }\n", "{b: 2, a: 1, }\n"),
        ("{a: 1, b: 2,}\n", "{b: 2, a: 1,}\n"),
        ("{\n  a: 1,\n  b: 2\n}\n", "{\n  b: 2,\n  a: 1\n}\n"),
    ] {
        check_reorder(source, &["b"], expected, root);
    }
    check_reorder("{a: 1, b: 2, c: 3}", &["c"], "{c: 3, a: 1, b: 2}", root);
    check_reorder("top: {a: 1, b: 2}\n", &["b"], "top: {b: 2, a: 1}\n", top);
}

#[test]
fn reorder_flow_mapping_with_comments() {
    check_reorder(
        "{a: 1, # c\n b: 2 # d\n}\n",
        &["b"],
        "{b: 2, # d\n a: 1 # c\n}\n",
        root,
    );
    for (source, expected) in [
        (
            "{\n  a: 1, # c\n  b: 2 # d\n}\n",
            "{\n  b: 2, # d\n  a: 1 # c\n}\n",
        ),
        (
            "{\n  a: 1, # c\n  b: 2, # d\n}\n",
            "{\n  b: 2, # d\n  a: 1, # c\n}\n",
        ),
        ("{a: 1,\n # c\n b: 2}\n", "{ # c\n b: 2,\n a: 1}\n"),
    ] {
        check_reorder(source, &["b"], expected, root);
    }
    check_reorder(
        "top: {a: 1, # c\n   b: 2 # d\n  }\n",
        &["b"],
        "top: {b: 2, # d\n   a: 1 # c\n  }\n",
        top,
    );
}

#[test]
fn reorder_unterminated_block_mapping() {
    for (source, order, expected) in [
        ("a: 1\nb: 2", &["b", "a"][..], "b: 2\na: 1"),
        ("a: 1\nb: 2\nc: 3", &["c", "a"], "c: 3\na: 1\nb: 2"),
        ("a: 1  # c\nb: 2  # d", &["b", "a"], "b: 2  # d\na: 1  # c"),
        ("a: 1\n# c\nb: 2", &["b", "a"], "# c\nb: 2\na: 1"),
        ("a: 1\nb:\n  x: 1", &["b", "a"], "b:\n  x: 1\na: 1"),
        ("a: 1\nb: 2\n", &["b", "a"], "b: 2\na: 1\n"),
        ("? a\n: 1\nb: 2", &["b", "a"], "b: 2\n? a\n: 1"),
        ("a: 1\n? b\n: 2", &["b", "a"], "? b\n: 2\na: 1"),
        ("a: 1\nb: |\n  x", &["b", "a"], "b: |\n  x\na: 1"),
        // Dropping the line break of a block scalar would change its value.
        ("a: |\n  x\nb: 2", &["b", "a"], "b: 2\na: |\n  x\n"),
    ] {
        check_reorder(source, order, expected, root);
    }
}

#[test]
fn reorder_nested_block_mapping() {
    for (source, expected) in [
        ("top:\n  a: 1\n  b: 2\n", "top:\n  b: 2\n  a: 1\n"),
        ("top:\n  a: 1\n  b: 2", "top:\n  b: 2\n  a: 1"),
        (
            "top:\n  a: 1\n  b: 2\nz: 3\n",
            "top:\n  b: 2\n  a: 1\nz: 3\n",
        ),
        (
            "top:\n  a: 1  # c\n  # between\n  b: 2  # d\n",
            "top:\n  # between\n  b: 2  # d\n  a: 1  # c\n",
        ),
        ("top:\n  a: 1\n\n  b: 2\n", "top:\n  b: 2\n\n  a: 1\n"),
    ] {
        check_reorder(source, &["b", "a"], expected, top);
    }
}

#[test]
fn reorder_unterminated_keeps_aliases() {
    check_reorder("a: &x 1\nb: *x", &["b", "a"], "b: &x 1\na: *x", root);
}

#[track_caller]
fn check_move(source: &str, after: bool, expected: &str, mapping: impl Fn(&Document) -> Mapping) {
    let doc = Document::from_str(source).unwrap();
    let m = mapping(&doc);
    let moved = if after {
        m.move_after("b", "a", m.get("a").unwrap())
    } else {
        m.move_before("a", "b", m.get("b").unwrap())
    };
    assert!(moved);
    assert_edited(&doc, expected);
}

#[test]
fn move_in_flow_mapping() {
    for (source, expected) in [
        ("{a: 1, b: 2}\n", "{b: 2, a: 1}\n"),
        ("{a: 1, b: 2, }\n", "{b: 2, a: 1, }\n"),
        ("{a: 1, b: 2,}", "{b: 2, a: 1,}"),
        ("{ a: 1, b: 2 }\n", "{ b: 2, a: 1 }\n"),
    ] {
        check_move(source, true, expected, root);
        check_move(source, false, expected, root);
    }
}

#[test]
fn move_in_unterminated_block_mapping() {
    for (source, expected) in [
        ("a: 1\nb: 2", "b: 2\na: 1"),
        ("a: 1\nb: 2\n", "b: 2\na: 1\n"),
    ] {
        check_move(source, true, expected, root);
        check_move(source, false, expected, root);
    }
    check_move("top:\n  a: 1\n  b: 2", true, "top:\n  b: 2\n  a: 1", top);
    check_move("top:\n  a: 1\n  b: 2", false, "top:\n  b: 2\n  a: 1", top);
    check_move("a: &x 1\nb: *x", true, "b: &x 1\na: *x", root);
}

#[test]
fn move_keeps_comments() {
    for (source, expected) in [
        ("a: 1  # c\nb: 2  # d", "b: 2  # d\na: 1  # c"),
        ("a: 1  # c\nb: 2  # d\n", "b: 2  # d\na: 1  # c\n"),
        ("a:  # c\n  x: 1\nb: 2\n", "b: 2\na:  # c\n  x: 1\n"),
        ("? a  # c\n: 1  # e\nb: 2\n", "b: 2\n? a  # c\n: 1  # e\n"),
        // A comment line goes with the entry below it, as in reorder_fields.
        ("a: 1\n# c\nb: 2\n", "# c\nb: 2\na: 1\n"),
        ("{a: 1, # c\n b: 2 # d\n}", "{b: 2, # d\n a: 1 # c\n}"),
        ("{a: 1 # c\n, b: 2 # d\n}\n", "{b: 2, # d\na: 1 # c\n}\n"),
        ("{a: 1, b: 2 # d\n}\n", "{b: 2, # d\na: 1}\n"),
    ] {
        check_move(source, true, expected, root);
        check_move(source, false, expected, root);
    }
    for after in [true, false] {
        check_move(
            "top:\n  # e\n  a: 1  # c\n  # f\n  b: 2  # d\n",
            after,
            "top:\n  # e\n  # f\n  b: 2  # d\n  a: 1  # c\n",
            top,
        );
    }
}

#[test]
fn reorder_keeps_blank_lines_in_place() {
    for (source, order, expected) in [
        (
            "a: 1  # c\n\nb: 2  # d\n\n",
            &["b", "a"][..],
            "b: 2  # d\n\na: 1  # c\n\n",
        ),
        ("a: 1\n\nb: 2", &["b", "a"], "b: 2\n\na: 1"),
        ("a: 1\n  \nb: 2\n", &["b", "a"], "b: 2\n  \na: 1\n"),
        (
            "a: 1\n\nb: 2\nc: 3\n",
            &["c", "b", "a"],
            "c: 3\n\nb: 2\na: 1\n",
        ),
        // Blank lines among and after the comment lines before an entry go
        // with them.
        (
            "a: 1\n\n# c\n\nb: 2\n",
            &["b", "a"],
            "# c\n\nb: 2\n\na: 1\n",
        ),
        (
            "a: 1\n# c\n\nb: 2\nc: 3\n",
            &["b", "a", "c"],
            "# c\n\nb: 2\na: 1\nc: 3\n",
        ),
        // A block scalar would take the blank lines after it into its
        // value, so they go after the next entry that can have them.
        (
            "a: 1\n\nb: |\n  x\nc: 3\n",
            &["b", "a", "c"],
            "b: |\n  x\na: 1\n\nc: 3\n",
        ),
        ("a: 1\n\nb: |+\n  x\n", &["b", "a"], "b: |+\n  x\na: 1\n\n"),
        ("b: |+\n  x\na: 1\n\n", &["a", "b"], "a: 1\n\nb: |+\n  x\n"),
        (
            "a: 1\n\nb: |\n  x\n# c\nc: 3\n",
            &["b", "a", "c"],
            "b: |\n  x\na: 1\n\n# c\nc: 3\n",
        ),
    ] {
        check_reorder(source, order, expected, root);
    }
    check_reorder(
        "top:\n  a: 1\n\n  b: 2\n  c: 3\nz: 4\n",
        &["c", "b", "a"],
        "top:\n  c: 3\n\n  b: 2\n  a: 1\nz: 4\n",
        top,
    );
    check_move("a: 1\n\nb: 2\n", true, "b: 2\n\na: 1\n", root);
    check_move("a: 1\n\nb: 2\n", false, "b: 2\n\na: 1\n", root);
}

#[test]
fn reorder_keeps_comments() {
    check_reorder(
        "a: 1  # c\n# e\nb: 2  # d\nc: 3\n",
        &["b", "c", "a"],
        "# e\nb: 2  # d\nc: 3\na: 1  # c\n",
        root,
    );
    check_reorder(
        "top:\n  # e\n  a: 1\n  b: 2\n",
        &["b", "a"],
        "top:\n  # e\n  b: 2\n  a: 1\n",
        top,
    );
}

#[test]
fn move_with_new_value() {
    for (source, expected) in [
        ("{a: 1, b: 2, }\n", "{b: 2, a: 9, }\n"),
        ("{a: 1, b: 2,}", "{b: 2, a: 9,}"),
        ("a: 1\nb: 2", "b: 2\na: 9"),
    ] {
        let doc = Document::from_str(source).unwrap();
        assert!(root(&doc).move_after("b", "a", 9));
        assert_edited(&doc, expected);
    }
}

#[test]
fn move_before_from_end_of_flow_mapping() {
    let doc = Document::from_str("{a: 1, b: 2, c: 3}").unwrap();
    assert!(root(&doc).move_before("a", "c", 9));
    assert_edited(&doc, "{c: 9, a: 1, b: 2}");
}

#[test]
fn move_with_new_value_keeps_position_layout() {
    // The entry moves as it would with its own value, comment lines and
    // blank lines included.
    for (source, expected) in [
        (
            "a: 1\n# before b\nb: 2\nc: 3\n",
            "# before b\nb: 2\nc: 3\na: 5\n",
        ),
        ("a: 1\n\nb: 2\n\nc: 3\n", "b: 2\n\nc: 3\n\na: 5\n"),
        ("a: 1  # c\nb: 2\nc: 3", "b: 2\nc: 3\na: 5"),
        ("{a: 1, b: 2, c: 3,}\n", "{b: 2, c: 3, a: 5,}\n"),
    ] {
        let doc = Document::from_str(source).unwrap();
        assert!(root(&doc).move_after("c", "a", 5));
        assert_edited(&doc, expected);
    }
}

#[test]
fn reorder_keeps_comment_on_explicit_key_value_line() {
    for (source, expected) in [
        ("b: 2\n? a\n: 1  # c\n", "? a\n: 1  # c\nb: 2\n"),
        ("b: 2\n? a\n: 1  # c", "? a\n: 1  # c\nb: 2"),
    ] {
        check_reorder(source, &["a", "b"], expected, root);
    }
}

#[test]
fn reorder_keeps_comment_line_out_of_block_scalar_before() {
    check_reorder(
        "c: |\n  x\nb: 1\n  # t\n",
        &["b", "c"],
        "b: 1\nc: |\n  x\n# t\n",
        root,
    );
}

fn first_item(doc: &Document) -> Mapping {
    let item = doc.as_sequence().unwrap().get(0).unwrap();
    item.as_mapping().unwrap().clone()
}

#[test]
fn reorder_moves_comment_lines_with_entry_below() {
    check_reorder(
        "---\na: 1\n# about b\nb: 2\n",
        &["b", "a"],
        "---\n# about b\nb: 2\na: 1\n",
        root,
    );
    check_reorder(
        "a: 1\n# about b\nb: 2\n# about c\nc: 3\n",
        &["c", "a", "b"],
        "# about c\nc: 3\na: 1\n# about b\nb: 2\n",
        root,
    );
    // Lines after the last entry stay at the end.
    check_reorder(
        "a: 1\n# about b\nb: 2\n# end\n",
        &["b", "a"],
        "# about b\nb: 2\na: 1\n# end\n",
        root,
    );
    check_reorder(
        "top:\n  a: 1\n  # about b\n\n  b: 2\nz: 3\n",
        &["b", "a"],
        "top:\n  # about b\n\n  b: 2\n  a: 1\nz: 3\n",
        top,
    );
    // A mapping that starts on the line of a `- ` indicator starts on the
    // next one, as the comment line can't join that.
    check_reorder(
        "- a: 1\n  # about b\n  b: 2\n",
        &["b", "a"],
        "-\n  # about b\n  b: 2\n  a: 1\n",
        first_item,
    );
    for after in [true, false] {
        check_move(
            "---\na: 1\n# about b\nb: 2\n",
            after,
            "---\n# about b\nb: 2\na: 1\n",
            root,
        );
    }
}

#[test]
fn reorder_comment_line_to_front_of_file() {
    for (source, expected) in [
        ("a: 1\n# about b\nb: 2\n", "# about b\nb: 2\na: 1\n"),
        (
            "\u{feff}\na: 1\n# about b\nb: 2\n",
            "\u{feff}\n# about b\nb: 2\na: 1\n",
        ),
    ] {
        let file = YamlFile::from_str(source).unwrap();
        let doc = file.documents().next().unwrap();
        doc.as_mapping().unwrap().reorder_fields(["b", "a"]);
        common::assert_file_edited(&file, expected);
    }
}

#[test]
fn reorder_keeps_blank_line_after_block_value_at_its_position() {
    check_reorder(
        "a:\n  x: 1\n\nb: 2\n",
        &["b", "a"],
        "b: 2\n\na:\n  x: 1\n",
        root,
    );
}

#[test]
fn move_next_to_itself_with_own_value() {
    for source in ["foo:\n-\n", "foo: 1  # c\nbar: 2\n", "{foo: [a]}"] {
        let doc = Document::from_str(source).unwrap();
        let m = root(&doc);
        assert!(m.move_after("foo", "foo", m.get("foo").unwrap()));
        assert_edited(&doc, source);
        assert!(m.move_before("foo", "foo", m.get("foo").unwrap()));
        assert_edited(&doc, source);
    }
}

#[test]
fn move_before_in_file() {
    let file = YamlFile::from_str("a: 1\nb: 2\nc: 3\n").unwrap();
    assert!(file.move_before("a", "c", 4));
    assert_file_edited(&file, "c: 4\na: 1\nb: 2\n");
    assert!(!file.move_before("z", "c", 5));
    assert_eq!(file.to_string(), "c: 4\na: 1\nb: 2\n");
    assert!(!YamlFile::new().move_before("a", "c", 4));
}

#[test]
fn document_reorder_fields() {
    let doc = Document::new();
    for (key, value) in [
        ("Repository-Browse", "https://github.com/user/repo"),
        ("Name", "MyProject"),
        ("Bug-Database", "https://github.com/user/repo/issues"),
        ("Repository", "https://github.com/user/repo.git"),
    ] {
        doc.set(key, value);
        assert_cst_ok(&doc);
    }
    doc.reorder_fields(["Name", "Bug-Database", "Repository", "Repository-Browse"]);
    assert_edited(
        &doc,
        "---\nName: MyProject\nBug-Database: https://github.com/user/repo/issues\n\
         Repository: https://github.com/user/repo.git\nRepository-Browse: https://github.com/user/repo\n",
    );
    let keys: Vec<_> = doc
        .keys()
        .map(|k| k.as_scalar().unwrap().as_string())
        .collect();
    assert_eq!(
        keys,
        ["Name", "Bug-Database", "Repository", "Repository-Browse"]
    );
}
