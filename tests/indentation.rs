//! Block indentation: which lines continue a node, which open the next
//! entry of a collection, and which end it.

mod common;
use common::{
    assert_file_cst_ok, check_renders, count_nodes, parse_clean, parse_with_errors,
    parse_with_violations, render_documents,
};
use rowan::ast::AstNode;
use std::str::FromStr;
use yaml_edit::{debug, SyntaxKind, YamlFile};

/// A nested value's plain scalar folds continuation lines that clear the
/// *key's* column, not the value's own.
///
/// `a:\n  x y\n z\n` is the single scalar `x y z`, as both saphyr and
/// PyYAML read it. The value was parsed with its own line as the base, so a
/// continuation at or left of that column was stranded in an ERROR node,
/// and where a sibling key followed, it was lost with the continuation.
/// The fold must not swallow a sibling entry, nor the next entry of an
/// explicit-key mapping, and a sequence value is unaffected.
#[test]
fn nested_value_folds_from_the_key_column() {
    check_renders(&[
        ("a:\n  x y\n z\n", "map{a: x y z}"),
        ("a:\n  x y\n  z\n", "map{a: x y z}"),
        ("a:\n  x y\n   z\n", "map{a: x y z}"),
        ("a:\n x y\n z\n", "map{a: x y z}"),
        // The `[` here is ordinary scalar content in block context.
        ("a:\n  x[ y\n z]\n", "map{a: x[ y z]}"),
        ("a:\n  -[ x\n y]\n", "map{a: -[ x y]}"),
        ("a:\n  x\n z\nb: 1\n", "map{a: x z, b: 1}"),
        // `? c` opens the next entry rather than continuing the value `1`.
        (
            "outer:\n  ? a\n  : 1\n  ? c\n  : 2\n",
            "map{outer: map{a: 1, c: 2}}",
        ),
        ("key:\n - a\n - b\n", "map{key: seq[a, b]}"),
        ("key:\n  - a\n  - b\n", "map{key: seq[a, b]}"),
    ]);
}

/// Lines that cannot continue the value before them, nor start an entry
/// of the collection around it.
#[test]
fn lines_that_continue_nothing() {
    for (yaml, error) in [
        // A line at the nested mapping's own column cannot continue a
        // value in it, and a quoted scalar takes no plain continuation.
        (
            "k:\n  a: b\n  c\n",
            "3:3: Content after the document could not be parsed: \"c\"",
        ),
        (
            "k:\n  a: b\n  - c\n",
            "3:3: Content after the document could not be parsed: \"-\"",
        ),
        (
            "- a: b\n  c\n",
            "2:3: Content after the document could not be parsed: \"c\"",
        ),
        (
            "-\n   a: b\n  c\n",
            "3:3: Content after the document could not be parsed: \"c\"",
        ),
        (
            "a: \"b\"\n  c\n",
            "2:3: Content after the document could not be parsed: \"c\"",
        ),
        // A sequence's entries set their own column, so a line dedented
        // past them ends the mapping. The YAML test suite marks these an
        // error (6S55, 9CWY), and saphyr and PyYAML both reject them; the
        // relaxed continuation floor a scalar value gets was applied to a
        // sequence value too, so the stray line folded into the last entry.
        (
            "key:\n - bar\n - baz\n invalid\n",
            "4:2: Content after the document could not be parsed: \"invalid\"",
        ),
        (
            "key:\n - item1\n - item2\ninvalid\n",
            "4:1: Content after the document could not be parsed: \"invalid\"",
        ),
    ] {
        parse_with_errors(yaml, &[error]);
    }
    // A mapping there is kept as a second node of the document, which the
    // validator reports.
    parse_with_violations(
        "- x\n  &a k: v\n",
        &[("Document holds more than one node", "2:3")],
    );
}

/// A sequence entry's value on a later line has to clear the entry's own
/// dash column. At or left of it the line opens the next entry instead, and
/// this entry is an implicit null.
///
/// `"- -\n  -\n"` is `[[null, null]]` while `"- -\n   -\n"` is `[[[null]]]`,
/// as saphyr and PyYAML both read them. The lookahead accepted any indented
/// line as the value, so a sibling dash at the entry's own column opened a
/// sequence nested one level too deep.
#[test]
fn entry_value_on_a_later_line_clears_the_dash_column() {
    for (yaml, sequences, expected) in [
        ("- -\n  -\n", 2, "seq[seq[~, ~]]"),
        ("- -\n\n  -\n", 2, "seq[seq[~, ~]]"),
        ("- a\n- -\n  -\n", 2, "seq[a, seq[~, ~]]"),
        ("- -\n   -\n", 3, "seq[seq[seq[~]]]"),
        ("- -\n    - x\n", 3, "seq[seq[seq[x]]]"),
        ("-\n  - x\n", 2, "seq[seq[x]]"),
        ("- -\n  - x\n", 2, "seq[seq[~, x]]"),
    ] {
        assert_eq!(render_documents(yaml), [expected], "{yaml:?}");
        let file = parse_clean(yaml);
        assert_eq!(
            count_nodes(&file, SyntaxKind::SEQUENCE),
            sequences,
            "{yaml:?}"
        );
    }
}

/// A mapping's entries are bounded by their key's column, which is not
/// always the column the caller measured from: a document indented as a
/// whole keeps its leading INDENT outside DOCUMENT, so the caller still
/// says 0 and every line looks nested; and `- k:` puts `k` at column 2
/// while the mapping's base is the dash's column plus the dash, 1.
///
/// `"  k:\n  j: 1\n"` and `"- k:\n  j: 1\n"` are two sibling entries, as
/// saphyr and PyYAML both read them, but `j` was parsed as the value of
/// `k`. Only a dash is indentless, so a sequence at the key's column is the
/// key's value; it used to escape into the document's sequence.
#[test]
fn mapping_entries_keep_their_key_column() {
    for (yaml, mappings, expected) in [
        ("  k:\n  j: 1\n", 1, "map{k: ~, j: 1}"),
        (" k:\n j: 1\n", 1, "map{k: ~, j: 1}"),
        ("   k:\n   j: 1\n", 1, "map{k: ~, j: 1}"),
        ("  a:\n  b:\n  c: 1\n", 1, "map{a: ~, b: ~, c: 1}"),
        ("- k:\n  j: 1\n", 1, "seq[map{k: ~, j: 1}]"),
        ("- k: v\n  j: 1\n", 1, "seq[map{k: v, j: 1}]"),
        ("- a b: 1\n  c d: 2\n", 1, "seq[map{a b: 1, c d: 2}]"),
        ("- k:\n  - x\n", 1, "seq[map{k: seq[x]}]"),
        ("- k:\n  - a\n", 1, "seq[map{k: seq[a]}]"),
        ("- - k:\n    j: 1\n", 1, "seq[seq[map{k: ~, j: 1}]]"),
        // A value that really does clear the key's column still nests.
        ("  k:\n    j: 1\n", 2, "map{k: map{j: 1}}"),
        ("- k:\n   j: 1\n", 2, "seq[map{k: map{j: 1}}]"),
        ("- k:\n    j: 1\n", 2, "seq[map{k: map{j: 1}}]"),
        ("a:\n  - k:\n    j: 1\n", 2, "map{a: seq[map{k: ~, j: 1}]}"),
        ("k:\n  j: 1\n", 2, "map{k: map{j: 1}}"),
    ] {
        assert_eq!(render_documents(yaml), [expected], "{yaml:?}");
        let file = parse_clean(yaml);
        assert_eq!(
            count_nodes(&file, SyntaxKind::MAPPING),
            mappings,
            "{yaml:?}"
        );
    }
}

/// A blank line between a sequence entry's dash and its value carries no
/// indentation of its own, so it neither supplies the value's indent nor
/// ends the entry.
///
/// `"-\n\n m\n"` is the entry `m`, exactly as `"-\n m\n"` is, as saphyr and
/// PyYAML both read it. The value lookahead peeked a single token past the
/// newline, so a blank line hid the INDENT behind it: the entry took its
/// implicit-null branch and the value was swept into an ERROR node while
/// from_str still reported success. A bare dash with no value after it is
/// still an implicit null.
#[test]
fn blank_line_before_a_sequence_entry_value() {
    check_renders(&[
        ("-\n\n m\n", "seq[m]"),
        ("-\n\n\n m\n", "seq[m]"),
        ("- a\n-\n\n  b\n", "seq[a, b]"),
        ("a:\n  -\n\n    b\n", "map{a: seq[b]}"),
        ("-\n", "seq[~]"),
        ("- a\n-\n", "seq[a, ~]"),
        ("a:\n  - x\n\nb: 1\n", "map{a: seq[x], b: 1}"),
    ]);
}

/// A mapping nested in a sequence entry does not take the equal-indent
/// continuation rule away from the entries after it.
///
/// `"- k: v\n- e\n t\n"` is the mapping `{k: v}` and the scalar `e t`, as
/// saphyr and PyYAML both read it. parse_mapping_with_base_indent cleared
/// the flag for its own entries and never put it back, so the continuation
/// of a later entry was swept into an ERROR node while from_str still
/// reported success.
#[test]
fn mapping_in_an_entry_keeps_the_sequence_rule() {
    check_renders(&[
        ("- k: v\n- e\n t\n", "seq[map{k: v}, e t]"),
        ("- :\n- e\n t\n", "seq[map{~: ~}, e t]"),
        ("- a: 1\n  b: 2\n- e\n t\n", "seq[map{a: 1, b: 2}, e t]"),
    ]);
}

/// A `-` after a sequence entry's own dash opens a nested sequence, since
/// an entry's node may be a collection.
///
/// `"- - a\n"` is `[[a]]`, as saphyr and PyYAML both read it, and as
/// value.rs already writes a nested sequence, so the parser could not read
/// its own output back. The dash was a sequence marker only at the start of
/// a line or after `?`/`:`, so the inner one became the scalar `- a`.
#[test]
fn dash_after_a_dash_opens_a_nested_sequence() {
    check_renders(&[
        ("- - a\n", "seq[seq[a]]"),
        ("- - a\n  - b\n", "seq[seq[a, b]]"),
        ("- - a\n- b\n", "seq[seq[a], b]"),
        ("- - - a\n", "seq[seq[seq[a]]]"),
    ]);
}

/// A compact sequence ends at its own column: a dash left of it belongs to
/// an enclosing sequence, and its items continue only on lines indented
/// past their own `-`.
#[test]
fn compact_nested_sequences() {
    check_renders(&[
        // Used to be taken as the innermost sequence's next entry, giving
        // `[[[a, x, y]]]`.
        ("- - - a\n    - x\n  - y\n", "seq[seq[seq[a, x], y]]"),
        ("- - - a\n  - y\n", "seq[seq[seq[a], y]]"),
        ("-   - a\n    - b\n", "seq[seq[a, b]]"),
        // The `- b` line sits at the inner sequence's own column, so it is
        // that sequence's next entry. Read as `k`'s indentless value it
        // gave one item keyed by [b] instead of the two saphyr and PyYAML
        // both report.
        ("- - k:\n  - b\n", "seq[seq[map{k: ~}, b]]"),
        // Likewise the `- z` line is not the block scalar's body; taking
        // its column as the body's base swallowed it.
        ("- -\n   |\n  - z\n", "seq[seq[, z]]"),
        // Past the dash, a line that is shallower than the scalar still
        // continues it rather than opening a sibling entry.
        ("-\n   b\n  - z\n", "seq[b - z]"),
        ("- - a\n   b\n", "seq[seq[a b]]"),
    ]);

    // A dash or key between the enclosing column and the compact
    // collection's own is in neither. The parser still accepts it and
    // leaves the validator to report it.
    parse_with_violations(
        "-   - a\n  - b\n",
        &[("Inconsistent sequence item indentation", "2:3")],
    );
    parse_with_violations(
        "-   k: 1\n  j: 2\n",
        &[(
            "Sibling block mapping entries have inconsistent indentation \
             (expected \"    \", found \"  \")",
            "2:1",
        )],
    );

    // `- - a\n  b\n` read `b` into `a`, and after an item was inserted
    // before `a`, putting it on a line of its own, the re-parse no longer
    // did.
    for yaml in ["- - a\n  b\n", "? - a\n  b\n"] {
        parse_with_errors(
            yaml,
            &["2:3: Content after the document could not be parsed: \"b\""],
        );
    }
}

/// A sequence that is a mapping value may sit at the key's own column,
/// with comment and blank lines before it, and does not swallow the
/// entries after it.
#[test]
fn indentless_sequences() {
    check_renders(&[
        (
            "\nitems:\n- key: value1\ntoplevel1: value2\ntoplevel2: value3\n",
            "map{items: seq[map{key: value1}], toplevel1: value2, toplevel2: value3}",
        ),
        ("\nitems:\n# comment\n- value1\n", "map{items: seq[value1]}"),
        (
            "\nitems:\n  # comment\n- value1\n",
            "map{items: seq[value1]}",
        ),
        ("\nitems:\n\n- value1\n", "map{items: seq[value1]}"),
        (
            "\nitems:\n\n# comment\n\n# comment\n\n- value1\n",
            "map{items: seq[value1]}",
        ),
        // Nested mappings in the items do not end the sequence early.
        (
            "\nitems:\n  - id: 1\n    nested:\n      deep: value1\n      deeper:\n        \
             key: val1\n  - id: 2\n    nested:\n      deep: value2\n      deeper:\n        \
             key: val2\n  - id: 3\n    simple: value\nanother_key: value\n",
            "map{items: seq[map{id: 1, nested: map{deep: value1, deeper: map{key: val1}}}, \
             map{id: 2, nested: map{deep: value2, deeper: map{key: val2}}}, \
             map{id: 3, simple: value}], another_key: value}",
        ),
    ]);

    // Popping the item leaves a valid tree.
    let file = parse_clean("\nitems:\n- key: value1\ntoplevel1: value2\ntoplevel2: value3\n");
    let items = file
        .document()
        .unwrap()
        .as_mapping()
        .unwrap()
        .get_sequence("items")
        .unwrap();
    let item = items.pop().unwrap();
    assert_file_cst_ok(&file);
    assert_eq!(item.as_mapping().unwrap().keys().count(), 1);
}

#[test]
fn empty_sequence_item_before_final_whitespace_line() {
    // The final line holds only white space and has no line break, so the
    // item is an implicit null, as it is when the line has one.
    let text = "- a\n-\n  ";
    let file = YamlFile::from_str(text).unwrap();
    assert_file_cst_ok(&file);
    assert_eq!(file.to_string(), text);
    assert_eq!(
        debug::tree_to_string(file.syntax()),
        concat!(
            "DOCUMENT\n",
            "  SEQUENCE\n",
            "    SEQUENCE_ENTRY\n",
            "      DASH: \"-\"\n",
            "      WHITESPACE: \" \"\n",
            "      SCALAR\n",
            "        STRING: \"a\"\n",
            "      NEWLINE: \"\\\\n\"\n",
            "    SEQUENCE_ENTRY\n",
            "      DASH: \"-\"\n",
            "      SCALAR\n",
            "        NULL: \"\"\n",
            "      NEWLINE: \"\\\\n\"\n",
            "    INDENT: \"  \"\n",
        )
    );
}

/// The value of the single document in `src`, with a check that the
/// tree is well-formed and that the validator accepts it.
fn checked_value(src: &str) -> yaml_edit::YamlValue {
    let file = YamlFile::from_str(src).expect("should parse");
    debug::validate_tree(file.syntax()).expect("tree should be valid");
    let doc = file.document().expect("should have a document");
    assert_eq!(
        yaml_edit::validator::Validator::new().validate(&doc),
        vec![]
    );
    yaml_edit::YamlValue::from_document(&doc).expect("should convert")
}

/// A tag ending a compact mapping's first line annotates a null when the
/// next line is at the key's column, as it does at the line start.
#[test]
fn sibling_after_tagged_null_in_compact_mapping() {
    for (src, expected) in [
        ("- a: !!null\n  b: c\n", "[{a: !!null , b: c}]"),
        ("- a: !!null &x\n  b: c\n", "[{a: !!null &x , b: c}]"),
        ("- a: &x !!null\n  b: c\n", "[{a: &x !!null , b: c}]"),
        ("- a: !!null # c\n  b: c\n", "[{a: !!null , b: c}]"),
        ("- !!str a: !!null\n  b: c\n", "[{!!str a: !!null , b: c}]"),
        ("- [a]: !!str\n  b: c\n", "[{[a]: !!str , b: c}]"),
        ("k:\n  - a: !!null\n    b: c\n", "{k: [{a: !!null , b: c}]}"),
        ("? x\n: a: !!null\n  b: c\n", "{x: {a: !!null , b: c}}"),
        ("- a: !!str\n   x\n  b: c\n", "[{a: !!str x, b: c}]"),
        ("- a: !!null\n    b: c\n", "[{a: !!null {b: c}}]"),
    ] {
        assert_eq!(checked_value(src), checked_value(expected), "{src:?}");
    }
}
