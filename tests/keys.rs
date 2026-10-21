//! Complex, explicit and duplicate mapping keys: how they parse and how
//! edits write them.

mod common;
use common::{assert_cst_ok, assert_file_cst_ok, assert_file_parser_shaped, render};
use rowan::ast::AstNode;
use std::str::FromStr;
use yaml_edit::validator::{Rule, Validator};
use yaml_edit::{debug, Document, Mapping, YamlFile, YamlNode};

/// Parse `yaml`, check it round-trips and holds one document, and render
/// that document's root node.
#[track_caller]
fn render_root(yaml: &str) -> String {
    let file = YamlFile::from_str(yaml).unwrap_or_else(|e| panic!("{yaml:?}: {e}"));
    assert_eq!(file.to_string(), yaml);
    assert_eq!(file.documents().count(), 1, "{yaml:?}");
    let doc = file.document().unwrap();
    let root = doc
        .syntax()
        .children()
        .find_map(YamlNode::from_syntax)
        .unwrap();
    render(&root)
}

fn root_mapping(file: &YamlFile) -> Mapping {
    file.document().unwrap().as_mapping().unwrap()
}

fn scalar(node: &YamlNode) -> String {
    node.as_scalar().unwrap().as_string()
}

#[test]
fn complex_keys() {
    let spec_example = r#"
? - Detroit Tigers
  - Chicago Cubs
:
  - 2001-07-23

? [ New York Yankees,
    Atlanta Braves ]
: [ 2001-07-02, 2001-08-12,
    2001-08-14 ]
"#;
    for (yaml, expected) in [
        (
            r#"
? key1
: value1
? key2
: value2
"#,
            "map{key1: value1, key2: value2}",
        ),
        (
            r#"
? simple
:
  - item1
  - item2
? another
:
  nested:
    key: value
"#,
            "map{simple: seq[item1, item2], another: map{nested: map{key: value}}}",
        ),
        (
            r#"
[a, b]: value1
[c, d]: value2
"#,
            "map{[a, b]: value1, [c, d]: value2}",
        ),
        (
            r#"
{name: John, age: 30}: developer
{name: Jane, age: 25}: designer
"#,
            "map{{name: John, age: 30}: developer, {name: Jane, age: 25}: designer}",
        ),
        (
            r#"
? [a, b, c]
: value1
? [d, e, f]
: value2
"#,
            "map{[a, b, c]: value1, [d, e, f]: value2}",
        ),
        (
            r#"
? {x: 1, y: 2}
: point1
? {x: 3, y: 4}
: point2
"#,
            "map{{x: 1, y: 2}: point1, {x: 3, y: 4}: point2}",
        ),
        (
            r#"
simple: value1
? explicit
: value2
[list, key]: value3
{map: key}: value4
"#,
            "map{simple: value1, explicit: value2, [list, key]: value3, {map: key}: value4}",
        ),
        (
            r#"
? [a, [b, c]]
: nested_list
? {outer: {inner: value}}
: nested_map
"#,
            "map{[a, [b, c]]: nested_list, {outer: {inner: value}}: nested_map}",
        ),
        (
            r#"
? |
  multiline
  key
: value
"#,
            "map{multiline\nkey\n: value}",
        ),
        (
            r#"
? key_without_value
? another_key
: has_value
"#,
            "map{key_without_value: ~, another_key: has_value}",
        ),
        (
            r#"
[a, b]: value1
{x: 1}: value2
? explicit
: value3
"#,
            "map{[a, b]: value1, {x: 1}: value2, explicit: value3}",
        ),
        (
            spec_example,
            "map{seq[Detroit Tigers, Chicago Cubs]: seq[2001-07-23], \
             [New York Yankees, Atlanta Braves]: [2001-07-02, 2001-08-12, 2001-08-14]}",
        ),
        (
            r#"
{ [a, b]: value1, {x: 1}: value2 }
"#,
            "{[a, b]: value1, {x: 1}: value2}",
        ),
    ] {
        assert_eq!(render_root(yaml), expected, "{yaml:?}");
    }
}

#[test]
fn scalar_keys_found_by_get_string() {
    for (yaml, key, value) in [
        ("\n? key1\n: value1\n? key2\n: value2\n", "key1", "value1"),
        ("\n? key1\n: value1\n? key2\n: value2\n", "key2", "value2"),
        (
            "\nsimple: value1\n? explicit\n: value2\n[list, key]: value3\n{map: key}: value4\n",
            "simple",
            "value1",
        ),
        (
            "\nsimple: value1\n? explicit\n: value2\n[list, key]: value3\n{map: key}: value4\n",
            "explicit",
            "value2",
        ),
        (
            "\n? key_without_value\n? another_key\n: has_value\n",
            "another_key",
            "has_value",
        ),
        (
            "\n[a, b]: value1\n{x: 1}: value2\n? explicit\n: value3\n",
            "explicit",
            "value3",
        ),
        // The second key used to be a bare STRING with no SCALAR node, so it
        // was invisible to keys() and get().
        ("[1]: 2\nk: v\n", "k", "v"),
    ] {
        let doc = Document::from_str(yaml).unwrap();
        assert_eq!(doc.get_string(key), Some(value.to_string()), "{yaml:?}");
    }
}

#[test]
fn keys_after_complex_key() {
    for (yaml, expected) in [
        ("[1]: 2\nk: v\n", "map{[1]: 2, k: v}"),
        // Used to be the single value `2 &a k: v`.
        ("[1]: 2\n&a k: v\n", "map{[1]: 2, k: v}"),
        ("&k [a]: v\n&j {b: 1}: w\n", "map{[a]: v, {b: 1}: w}"),
        ("[1]: 2\n? x\n: y\nz: 1\n", "map{[1]: 2, x: y, z: 1}"),
        ("a:\n  [1]: 2\nj: w\n", "map{a: map{[1]: 2}, j: w}"),
    ] {
        assert_eq!(render_root(yaml), expected, "{yaml:?}");
    }
}

#[test]
fn sequence_entry_after_complex_key_is_an_error() {
    assert!("[1]: 2\n- x\n".parse::<YamlFile>().is_err());
}

#[test]
fn nested_explicit_key_with_empty_value() {
    // The `b: c` line used to become the value of `a`.
    for (yaml, expected) in [
        ("k:\n  ? a\n  :\n  b: c\n", "map{k: map{a: ~, b: c}}"),
        ("- ? a\n  :\n  b: c\n", "seq[map{a: ~, b: c}]"),
        ("k:\n  ? a\n  :\n  - x\n", "map{k: map{a: seq[x]}}"),
    ] {
        assert_eq!(render_root(yaml), expected, "{yaml:?}");
    }
}

#[test]
fn block_scalar_in_explicit_entry_does_not_swallow_next_entry() {
    for (yaml, expected) in [
        ("? a\n: |\nb: 1\n", "map{a: , b: 1}"),
        ("? a\n: >\n? b\n", "map{a: , b: ~}"),
        ("? |\nb: 1\n", "map{: ~, b: 1}"),
        ("x:\n  ? a\n  : |\n  b: 1\n", "map{x: map{a: , b: 1}}"),
    ] {
        assert_eq!(render_root(yaml), expected, "{yaml:?}");
    }
}

/// A comment after `?` or an explicit `:` ends its line just as a line
/// break does, so the next line belongs to the entry only if it is
/// indented past the indicator, or is an indentless sequence.
#[test]
fn comment_after_explicit_indicator() {
    let yaml = "? # c\nb: 1\n";
    let doc = yaml.parse::<YamlFile>().unwrap();
    assert_eq!(
        debug::tree_to_string(doc.syntax()),
        r##"DOCUMENT
  MAPPING
    MAPPING_ENTRY
      QUESTION: "?"
      WHITESPACE: " "
      KEY
        COMMENT: "# c"
        SCALAR
          NULL: ""
      VALUE
        SCALAR
          NULL: ""
      NEWLINE: "\\n"
    MAPPING_ENTRY
      KEY
        SCALAR
          STRING: "b"
      COLON: ":"
      WHITESPACE: " "
      VALUE
        SCALAR
          INT: "1"
      NEWLINE: "\\n"
"##
    );

    for (yaml, expected) in [
        ("? # c\nb: 1\n", "map{~: ~, b: 1}"),
        ("? a\n: # c\nb: 1\n", "map{a: ~, b: 1}"),
        ("? # c", "map{~: ~}"),
        ("? a\n: # c", "map{a: ~}"),
        ("- ? # c\n- 1\n", "seq[map{~: ~}, 1]"),
        ("- ? a\n  : # c\n- 1\n", "seq[map{a: ~}, 1]"),
        ("? # c\n  k: v\n: x\n", "map{map{k: v}: x}"),
        ("? # c\n- a\n: v\n", "map{seq[a]: v}"),
        ("? a\n: # c\n- b\n", "map{a: seq[b]}"),
        ("? a\n: # c\n  - b\n", "map{a: seq[b]}"),
    ] {
        assert_eq!(render_root(yaml), expected, "{yaml:?}");
    }
}

/// A string key that would be written as a block scalar is written
/// double-quoted instead, tagged or not: a block scalar cannot be an
/// implicit key.
#[test]
fn set_multiline_string_key_is_quoted() {
    use yaml_edit::{ScalarValue, YamlValue};
    let plain = ScalarValue::string("x\ny\n");
    let tagged = ScalarValue::string("x\ny\n").with_tag("!t");
    let tagged_value = YamlValue::tagged("!t", YamlValue::Scalar(ScalarValue::string("x\ny\n")));
    for (key, expected) in [
        (YamlValue::Scalar(plain), "a: 1\n\"x\\ny\\n\": 2\n"),
        (YamlValue::Scalar(tagged), "a: 1\n!t \"x\\ny\\n\": 2\n"),
        (tagged_value, "a: 1\n!t \"x\\ny\\n\": 2\n"),
    ] {
        let file: YamlFile = "a: 1\n".parse().unwrap();
        let mapping = root_mapping(&file);
        mapping.set(&key, 2);
        assert_eq!(file.to_string(), expected);
        assert_file_cst_ok(&file);
        assert_eq!(mapping.len(), 2);
        assert_eq!(mapping.get(&key).unwrap().to_string(), "2");
    }
}

/// A lone tag or anchor in an explicit key or value annotates an implicit
/// null when the next line is not indented past the `?` or `:`, as PyYAML
/// reads it: the line is the mapping's next entry. Indented past it, the
/// next line is still the annotated node.
#[test]
fn lone_annotation_in_explicit_entry() {
    let yaml = "? !!str\nb: 1\n";
    let doc = yaml.parse::<YamlFile>().unwrap();
    assert_eq!(
        debug::tree_to_string(doc.syntax()),
        r##"DOCUMENT
  MAPPING
    MAPPING_ENTRY
      QUESTION: "?"
      WHITESPACE: " "
      KEY
        TAGGED_NODE
          TAG: "!!str"
          SCALAR
            NULL: ""
      VALUE
        SCALAR
          NULL: ""
      NEWLINE: "\\n"
    MAPPING_ENTRY
      KEY
        SCALAR
          STRING: "b"
      COLON: ":"
      WHITESPACE: " "
      VALUE
        SCALAR
          INT: "1"
      NEWLINE: "\\n"
"##
    );

    for (yaml, expected) in [
        ("? !!str\nb: 1\n", "map{!!str ~: ~, b: 1}"),
        ("? &a\nb: 1\n", "map{~: ~, b: 1}"),
        ("? !!str &a\nb: 1\n", "map{!!str ~: ~, b: 1}"),
        ("? a\n: !!str\nb: 1\n", "map{a: !!str ~, b: 1}"),
        ("? a\n: &x\nb: 1\n", "map{a: ~, b: 1}"),
        ("- ? !!str\n  b: 1\n", "seq[map{!!str ~: ~, b: 1}]"),
        ("- ? a\n  : &x\n  b: 1\n", "seq[map{a: ~, b: 1}]"),
        ("? !!map\n? b\n: 1\n", "map{!!map ~: ~, b: 1}"),
        ("? a\n: &x\n? b\n: 1\n", "map{a: ~, b: 1}"),
        ("? !!map\n  b: 1\n: v\n", "map{!!map map{b: 1}: v}"),
        ("? a\n: &x\n  b: 1\n", "map{a: map{b: 1}}"),
    ] {
        assert_eq!(render_root(yaml), expected, "{yaml:?}");
    }
}

/// An explicit value's tagged sequence may sit at the `:`'s own column,
/// and an explicit key's tagged or anchored one at the `?`'s, as
/// `seq-spaces` allows, but a `-` left of it is the enclosing sequence's
/// next entry.
#[test]
fn annotated_sequence_at_explicit_indicator_column() {
    for (yaml, expected) in [
        ("? a\n: !!seq\n- b\n", "map{a: !!seq seq[b]}"),
        ("- ? a\n  : !!seq\n- b\n", "seq[map{a: !!seq ~}, b]"),
        ("? !!seq\n- a\n: v\n", "map{!!seq seq[a]: v}"),
        ("? &x\n- a\n: v\n", "map{seq[a]: v}"),
        ("- ? !!seq\n  - a\n  : v\n", "seq[map{!!seq seq[a]: v}]"),
        ("- ? &x\n  - a\n  : v\n", "seq[map{seq[a]: v}]"),
        ("- ? !!seq\n- a\n", "seq[map{!!seq ~: ~}, a]"),
    ] {
        assert_eq!(render_root(yaml), expected, "{yaml:?}");
    }

    for yaml in [
        "? !!seq\n- a\n: v\n",
        "? &x\n- a\n: v\n",
        "- ? !!seq\n  - a\n  : v\n",
        "- ? &x\n  - a\n  : v\n",
    ] {
        let doc = yaml.parse::<YamlFile>().unwrap();
        assert_file_cst_ok(&doc);
        assert_eq!(
            Validator::new()
                .validate_syntax(doc.syntax())
                .into_iter()
                .map(|v| v.message)
                .collect::<Vec<_>>(),
            Vec::<String>::new(),
            "{yaml:?}"
        );
    }
}

#[test]
fn explicit_key_after_implicit_with_indentless_sequence_value() {
    for (yaml, expected) in [
        ("a: 1\n? b\n:\n- x\n", "map{a: 1, b: seq[x]}"),
        ("a: 1\n? b\n: # c\n- x\n", "map{a: 1, b: seq[x]}"),
        ("a: 1\n? b\n:\n# c\n- x\n", "map{a: 1, b: seq[x]}"),
        ("a: 1\n? b\n:\n- x\nc: 2\n", "map{a: 1, b: seq[x], c: 2}"),
    ] {
        assert_eq!(render_root(yaml), expected, "{yaml:?}");
        assert_file_cst_ok(&yaml.parse::<YamlFile>().unwrap());
    }
}

/// A `:` on a later line left of a nested `?` is the value of the entry
/// enclosing it. `? ? a\n: b\n` gave the inner entry the value `b` and
/// left the outer one without, so setting the outer value wrote text that
/// re-parsed differently.
#[test]
fn value_left_of_a_nested_explicit_key() {
    for (yaml, expected) in [
        ("? ? \n: v\n", "map{map{~: ~}: v}"),
        ("? ? a\n: v\n", "map{map{a: ~}: v}"),
        ("? ? a\n  : b\n: v\n", "map{map{a: b}: v}"),
    ] {
        assert_eq!(render_root(yaml), expected, "{yaml:?}");
    }
}

/// Indented comment lines after a bare `?` do not make the next line its
/// key. `?\n # c\nk: v\n` gave the key an empty block mapping, which no
/// edit could write back, so the entry before `k` read back differently
/// once entries were moved.
#[test]
fn comment_lines_after_a_bare_explicit_key() {
    let yaml = "?\n # c\nk: v\n";
    assert_eq!(render_root(yaml), "map{~: ~, k: v}");

    let file = yaml.parse::<YamlFile>().unwrap();
    root_mapping(&file).reorder_fields(["k"]);
    assert_eq!(file.to_string(), " # c\nk: v\n?\n");
    assert_file_parser_shaped(&file);
}

/// An edit to a collection that is a key does what it is asked even when
/// that makes the key equal to another one; the validator then reports the
/// duplicate.
#[test]
fn edit_inside_key_making_it_a_duplicate() {
    let file = "? [a]\n: 1\n? []\n: 2\n".parse::<YamlFile>().unwrap();
    let key = root_mapping(&file).iter().next().unwrap().0;
    assert_eq!(scalar(&key.as_sequence().unwrap().pop().unwrap()), "a");
    assert_eq!(file.to_string(), "? []\n: 1\n? []\n: 2\n");
    assert_file_parser_shaped(&file);
    let reparsed = file.to_string().parse::<YamlFile>().unwrap();
    let violations: Vec<_> = Validator::new()
        .validate_syntax(reparsed.syntax())
        .into_iter()
        .map(|v| (v.rule, v.to_string()))
        .collect();
    assert_eq!(
        violations,
        vec![(
            Rule::DuplicateKeys,
            "[ERROR] 3:1: Duplicate key: \"[]\" (semantically equal to \"[]\") (DuplicateKeys)"
                .to_string()
        )]
    );
}

const THREE_REFERENCES: &str = r#"
Reference: First
Reference: Second
Reference: Third
"#;

const TWO_REFERENCES: &str = r#"
Reference: First
Reference: Second
"#;

fn values_of(mapping: &Mapping, key: &str) -> Vec<String> {
    mapping
        .find_all_entries_by_key(key)
        .map(|entry| scalar(&entry.value_node().unwrap()))
        .collect()
}

#[test]
fn find_all_entries_by_key() {
    let yaml = r#"
Reference: First
Reference: Second
Other: Value
Reference: Third
"#;
    let doc = Document::from_str(yaml).unwrap();
    let mapping = doc.as_mapping().unwrap();
    assert_eq!(
        values_of(&mapping, "Reference"),
        vec!["First", "Second", "Third"]
    );
}

#[test]
fn remove_all_but_first_occurrence() {
    let positions = r#"
Name: Slugify
Reference: First
Archive: GitHub
Reference: Second
Version: 1.0
Reference: Third
"#;
    for (source, expected) in [
        (THREE_REFERENCES, "\nReference: First\n"),
        (
            positions,
            "\nName: Slugify\nReference: First\nArchive: GitHub\nVersion: 1.0\n",
        ),
    ] {
        let doc = Document::from_str(source).unwrap();
        let mapping = doc.as_mapping().unwrap();
        let refs: Vec<_> = mapping.find_all_entries_by_key("Reference").collect();
        assert_eq!(refs.len(), 3);
        for entry in refs.into_iter().skip(1) {
            entry.remove();
            assert_cst_ok(&doc);
        }
        assert_eq!(values_of(&mapping, "Reference"), vec!["First"]);
        assert_eq!(doc.to_string(), expected);
    }
}

#[test]
fn remove_nth_occurrence() {
    for (source, key, n, removed, remaining, expected) in [
        (
            THREE_REFERENCES,
            "Reference",
            0,
            Some("First"),
            &["Second", "Third"][..],
            "\nReference: Second\nReference: Third\n",
        ),
        (
            THREE_REFERENCES,
            "Reference",
            1,
            Some("Second"),
            &["First", "Third"],
            "\nReference: First\nReference: Third\n",
        ),
        (
            THREE_REFERENCES,
            "Reference",
            2,
            Some("Third"),
            &["First", "Second"],
            TWO_REFERENCES,
        ),
        (
            TWO_REFERENCES,
            "Reference",
            2,
            None,
            &["First", "Second"],
            TWO_REFERENCES,
        ),
        (
            TWO_REFERENCES,
            "NotFound",
            0,
            None,
            &["First", "Second"],
            TWO_REFERENCES,
        ),
    ] {
        let doc = Document::from_str(source).unwrap();
        let mapping = doc.as_mapping().unwrap();
        let entry = mapping.remove_nth_occurrence(key, n);
        assert_cst_ok(&doc);
        assert_eq!(
            entry.map(|e| scalar(&e.value_node().unwrap())).as_deref(),
            removed
        );
        assert_eq!(values_of(&mapping, "Reference"), remaining);
        assert_eq!(doc.to_string(), expected);
    }
}

type MappingEdit = fn(&Mapping);

/// Apply `edit` to the root mapping of `source`, check the result is shaped
/// as the parser would shape it, and return its text.
#[track_caller]
fn edit_root(source: &str, edit: impl FnOnce(&Mapping)) -> String {
    let file = YamlFile::from_str(source).unwrap();
    edit(&root_mapping(&file));
    assert_file_parser_shaped(&file);
    file.to_string()
}

/// Like [`edit_root`], but edit the mapping under the root's `key`.
#[track_caller]
fn edit_nested(source: &str, key: &str, edit: impl FnOnce(&Mapping)) -> String {
    let file = YamlFile::from_str(source).unwrap();
    edit(&root_mapping(&file).get_mapping(key).unwrap());
    assert_file_parser_shaped(&file);
    file.to_string()
}

/// Like [`edit_root`], and also check the result is valid.
#[track_caller]
fn edit_root_valid(source: &str, edit: impl FnOnce(&Mapping)) -> String {
    let text = edit_root(source, edit);
    assert_valid(&text);
    text
}

#[track_caller]
fn assert_valid(text: &str) {
    let doc = Document::from_str(text).unwrap();
    assert_eq!(Validator::new().validate(&doc), vec![], "{text:?}");
}

#[test]
fn explicit_key_value_set() {
    assert_eq!(
        edit_root("? key1\n: value1\n? key2\n: value2\n", |m| {
            assert_eq!(
                render(&YamlNode::Mapping(m.clone())),
                "map{key1: value1, key2: value2}"
            );
            m.set("key1", "newvalue");
            assert_eq!(scalar(&m.get("key1").unwrap()), "newvalue");
        }),
        "? key1\n: newvalue\n? key2\n: value2\n"
    );
    // A new key follows the explicit style of the existing ones.
    assert_eq!(
        edit_root("? existing\n: value\n", |m| {
            assert_eq!(
                render(&YamlNode::Mapping(m.clone())),
                "map{existing: value}"
            );
            m.set("newkey", "newvalue");
            assert_eq!(
                render(&YamlNode::Mapping(m.clone())),
                "map{existing: value, newkey: newvalue}"
            );
        }),
        "? existing\n: value\n? newkey\n: newvalue\n"
    );
}

#[test]
fn nested_explicit_key_entry_keeps_indent() {
    let cases: [(&str, MappingEdit, &str); 3] = [
        (
            "outer:\n  ? a\n  : 1\n",
            |m| m.set("b", "9"),
            "outer:\n  ? a\n  : 1\n  ? b\n  : '9'\n",
        ),
        (
            "outer:\n  ? a\n  : 1\n  ? c\n  : 2\n",
            |m| m.insert_at_index(1, "b", "9"),
            "outer:\n  ? a\n  : 1\n  ? b\n  : '9'\n  ? c\n  : 2\n",
        ),
        (
            "outer:\n  ? b\n  : 1\n  ? c\n  : 2\n",
            |m| m.insert_at_index(0, "a", "9"),
            "outer:\n  ? a\n  : '9'\n  ? b\n  : 1\n  ? c\n  : 2\n",
        ),
    ];
    for (source, edit, expected) in cases {
        assert_eq!(edit_nested(source, "outer", edit), expected);
    }
}

#[test]
fn explicit_key_reorder_fields_keeps_line_breaks() {
    let doc = Document::from_str("? a\n: 1\n? b\n: 2\n").unwrap();
    doc.as_mapping().unwrap().reorder_fields(["b"]);
    assert_cst_ok(&doc);
    assert_eq!(doc.to_string(), "? b\n: 2\n? a\n: 1\n");
}

/// An explicit key puts its `:` on the next line. Adding an entry and then
/// renaming it used to collapse that onto one line (`? z: '0'`), which
/// reparses as a single node and silently loses a key.
#[test]
fn set_then_rename_keeps_an_explicit_key_on_its_own_line() {
    for (source, nested, expected) in [
        ("? a\n: 1\n", None, "? a\n: 1\n? z\n: '0'\n"),
        (
            "m:\n  ? a\n  : 1\n",
            Some("m"),
            "m:\n  ? a\n  : 1\n  ? z\n  : '0'\n",
        ),
    ] {
        let file = YamlFile::from_str(source).unwrap();
        let root = root_mapping(&file);
        let m = nested.map_or(root.clone(), |k| root.get_mapping(k).unwrap());
        m.set("u", "0");
        assert_file_parser_shaped(&file);
        assert!(m.rename_key("u", "z"), "{source:?}");
        assert_file_parser_shaped(&file);
        assert_eq!(file.to_string(), expected);
    }
}

/// A plain (non-explicit) mapping must keep renaming on one line.
#[test]
fn renaming_a_plain_key_stays_inline() {
    let doc = Document::from_str("a: 1\nb: 2\n").unwrap();
    let mapping = doc.as_mapping().unwrap();
    assert!(mapping.rename_key("a", "z"));
    assert_cst_ok(&doc);
    assert_eq!(doc.to_string(), "z: 1\nb: 2\n");
}

#[test]
fn explicit_key_entry_leaves_line_break_to_mapping() {
    let source = "? a\n: 1\n? c\n: 3\n";
    let cases: [(&str, MappingEdit, &str); 12] = [
        ("? a\n: 1\n", |m| m.set("b", 2), "? a\n: 1\n? b\n: 2\n"),
        (
            source,
            |m| m.insert_at_index(0, "b", 2),
            "? b\n: 2\n? a\n: 1\n? c\n: 3\n",
        ),
        (
            source,
            |m| m.insert_at_index(1, "b", 2),
            "? a\n: 1\n? b\n: 2\n? c\n: 3\n",
        ),
        (
            source,
            |m| m.insert_at_index(2, "b", 2),
            "? a\n: 1\n? c\n: 3\n? b\n: 2\n",
        ),
        (
            source,
            |m| m.set_with_field_order("b", 2, ["a", "b", "c"]),
            "? a\n: 1\n? b\n: 2\n? c\n: 3\n",
        ),
        (
            source,
            |m| m.set_with_field_order("b", 2, ["b", "a", "c"]),
            "? b\n: 2\n? a\n: 1\n? c\n: 3\n",
        ),
        ("? a\n: 1", |m| m.set("b", 2), "? a\n: 1\n? b\n: 2"),
        (
            "? a\n: 1",
            |m| m.set_with_field_order("b", 2, ["a", "b"]),
            "? a\n: 1\n? b\n: 2",
        ),
        (
            "? b\n: 2\n? a\n: 1",
            |m| m.set_with_field_order("c", 3, ["b", "c", "a"]),
            "? b\n: 2\n? c\n: 3\n? a\n: 1",
        ),
        (
            "? a\n:\n  x: 1\n? c\n: 3\n",
            |m| m.set("a", 5),
            "? a\n: 5\n? c\n: 3\n",
        ),
        (
            "? a\n: b  # c",
            |m| m.set("k", 1),
            "? a\n: b  # c\n? k\n: 1",
        ),
        (
            "? a\n: b  # c",
            |m| assert!(m.insert_after("a", "k", 1)),
            "? a\n: b  # c\n? k\n: 1",
        ),
    ];
    for (source, edit, expected) in cases {
        assert_eq!(edit_root(source, edit), expected, "{source:?}");
    }
}

/// A block sequence key may start at the column of its `?` on the line
/// after a comment, but a key it is renamed to has to be indented past it.
#[test]
fn key_after_comment_renamed_from_sequence_at_question_column() {
    let rename_first = |m: &Mapping| {
        let key = m.keys().next().unwrap();
        assert!(m.rename_key(key, "new"));
    };
    assert_eq!(
        edit_root("? # c\n- a\n: 1\n", rename_first),
        "? # c\n  new\n: 1\n"
    );
    assert_eq!(
        edit_root("? # c\n - a\n: 1\n", rename_first),
        "? # c\n new\n: 1\n"
    );
    assert_eq!(
        edit_nested("x:\n  ? # c\n  - a\n  : 1\n", "x", rename_first),
        "x:\n  ? # c\n    new\n  : 1\n"
    );
}

/// A block collection key ending the input has its line break, once the
/// `:` line follows it, in its last entry, where the parser puts it.
#[test]
fn value_set_for_unterminated_block_key() {
    let set_first = |m: &Mapping| {
        let key = m.keys().next().unwrap();
        m.set(key, 1);
    };
    assert_eq!(edit_root("? - a", set_first), "? - a\n: 1");
    assert_eq!(edit_root("? x: y", set_first), "? x: y\n: 1");
    assert_eq!(edit_root("? a", set_first), "? a\n: 1");
}

/// The first item of the sequence `source`.
fn first_item(source: &str) -> YamlNode {
    Document::from_str(source)
        .unwrap()
        .as_sequence()
        .unwrap()
        .get(0)
        .unwrap()
}

/// The value of `k` in `source`.
fn value_of_k(source: &str) -> YamlNode {
    Document::from_str(source)
        .unwrap()
        .as_mapping()
        .unwrap()
        .get("k")
        .unwrap()
}

#[test]
fn key_needing_explicit_form_inserted_relative_to_another() {
    let key = Document::from_str("- 1  # one\n- 2\n")
        .unwrap()
        .as_sequence()
        .unwrap();
    let expected = "a: 1\n? [1,  # one\n  2]\n: 2\nc: 3\n";
    let edits: [&dyn Fn(&Mapping); 5] = [
        &|m| m.insert_at_index(1, &key, 2),
        &|m| assert!(m.insert_after("a", &key, 2)),
        &|m| assert!(m.move_after("a", &key, 2)),
        &|m| assert!(m.insert_before("c", &key, 2)),
        &|m| assert!(m.move_before("c", &key, 2)),
    ];
    for edit in edits {
        assert_eq!(edit_root("a: 1\nc: 3\n", edit), expected);
    }
}

#[test]
fn explicit_key_entry_inserted_and_moved() {
    let three = "? a\n: 1\n? b\n: 2\n? c\n: 3\n";
    let cases: [(&str, MappingEdit, &str); 14] = [
        (
            "? a\n: 1\n? c\n: 3\n",
            |m| assert!(m.insert_after("a", "b", 2)),
            three,
        ),
        (
            "? a\n: 1\n? c\n: 3\n",
            |m| assert!(m.move_after("a", "b", 2)),
            three,
        ),
        (
            "? a\n: 1\n? c\n: 3\n",
            |m| assert!(m.insert_before("c", "b", 2)),
            three,
        ),
        (
            "? a\n: 1\n? c\n: 3\n",
            |m| assert!(m.move_before("c", "b", 2)),
            three,
        ),
        (
            "? a\n: 1\n",
            |m| assert!(m.insert_after("a", "b", 2)),
            "? a\n: 1\n? b\n: 2\n",
        ),
        (
            "? a\n: 1",
            |m| assert!(m.move_after("a", "b", 2)),
            "? a\n: 1\n? b\n: 2",
        ),
        (
            "? a\n: 1\n",
            |m| assert!(m.insert_before("a", "b", 2)),
            "? b\n: 2\n? a\n: 1\n",
        ),
        (
            three,
            |m| assert!(m.move_after("c", "a", 9)),
            "? b\n: 2\n? c\n: 3\n? a\n: 9\n",
        ),
        (
            three,
            |m| assert!(m.move_before("a", "c", 9)),
            "? c\n: 9\n? a\n: 1\n? b\n: 2\n",
        ),
        (
            three,
            |m| assert!(m.move_after("a", "c", 9)),
            "? a\n: 1\n? c\n: 9\n? b\n: 2\n",
        ),
        (
            three,
            |m| assert!(m.move_before("c", "a", 9)),
            "? b\n: 2\n? a\n: 9\n? c\n: 3\n",
        ),
        (
            "? a\n: b  # c\nz: 1\n",
            |m| assert!(m.insert_after("a", "k", 1)),
            "? a\n: b  # c\n? k\n: 1\nz: 1\n",
        ),
        (
            "? a\n: 1\n? b\n: 2\n",
            |m| m.insert_at_index_preserving(0, "a", 5),
            "? a\n: 5\n? b\n: 2\n",
        ),
        (
            "? a\n: 1\n? b\n: 2\n",
            |m| m.insert_at_index_preserving(0, "b", 5),
            "? a\n: 1\n? b\n: 5\n",
        ),
    ];
    for (source, edit, expected) in cases {
        assert_eq!(edit_root(source, edit), expected, "{source:?}");
    }
    assert_eq!(
        edit_root("? a\n: 1\n? b\n: 2", |m| m
            .insert_at_index_preserving(0, "b", 5)),
        "? a\n: 1\n? b\n: 5"
    );
}

#[test]
fn nested_explicit_key_entry_inserted_and_moved() {
    let source = "outer:\n  ? a\n  : 1\n  ? c\n  : 3\nz: 0\n";
    let cases: [(MappingEdit, &str); 5] = [
        (
            |m| assert!(m.insert_after("a", "b", 2)),
            "outer:\n  ? a\n  : 1\n  ? b\n  : 2\n  ? c\n  : 3\nz: 0\n",
        ),
        (
            |m| assert!(m.insert_after("c", "b", 2)),
            "outer:\n  ? a\n  : 1\n  ? c\n  : 3\n  ? b\n  : 2\nz: 0\n",
        ),
        (
            |m| assert!(m.insert_before("a", "b", 2)),
            "outer:\n  ? b\n  : 2\n  ? a\n  : 1\n  ? c\n  : 3\nz: 0\n",
        ),
        (
            |m| assert!(m.move_after("c", "a", 9)),
            "outer:\n  ? c\n  : 3\n  ? a\n  : 9\nz: 0\n",
        ),
        (
            |m| assert!(m.move_before("a", "c", 9)),
            "outer:\n  ? c\n  : 9\n  ? a\n  : 1\nz: 0\n",
        ),
    ];
    for (edit, expected) in cases {
        assert_eq!(edit_nested(source, "outer", edit), expected);
    }
}

#[test]
fn entry_replaced_at_index_keeps_mapping_layout() {
    for (source, key, expected) in [
        ("a: 1\nb: 2\n", "a", "a: 5\nb: 2\n"),
        ("a: 1\nb: 2", "b", "a: 1\nb: 5"),
        ("m:\n  a: 1\n  b: 2\n", "a", "m:\n  a: 5\n  b: 2\n"),
        ("m:\n  a: 1\n  b: 2\n", "b", "m:\n  a: 1\n  b: 5\n"),
        ("m:\n  a: 1\nz: 0\n", "a", "m:\n  a: 5\nz: 0\n"),
        ("{k: v, m: 2}\n", "k", "{k: 5, m: 2}\n"),
        ("{k: v, m: 2}\n", "m", "{k: v, m: 5}\n"),
        ("{k: v,\n  m: 2}\n", "k", "{k: 5,\n  m: 2}\n"),
    ] {
        let file = YamlFile::from_str(source).unwrap();
        let root = root_mapping(&file);
        let m = root.get_mapping("m").unwrap_or(root);
        m.insert_at_index_preserving(1, key, 5);
        assert_file_parser_shaped(&file);
        assert_eq!(file.to_string(), expected, "{source:?}");
    }
}

#[test]
fn explicit_key_commented_value_replaced_by_scalar() {
    for (source, expected) in [
        ("? a\n:  # c\n  x: 1\n", "? a\n: 5  # c\n"),
        ("? a\n:  # c\n  x: 1\nb: 2\n", "? a\n: 5  # c\nb: 2\n"),
        ("? a\n: 1  # c\n? b\n: 2\n", "? a\n: 5  # c\n? b\n: 2\n"),
    ] {
        assert_eq!(edit_root(source, |m| m.set("a", 5)), expected);
    }
    assert_eq!(
        edit_nested("m:\n  ? a\n  :  # c\n    - 1\n  b: 2\n", "m", |m| m
            .set("a", 5)),
        "m:\n  ? a\n  : 5  # c\n  b: 2\n"
    );
}

#[test]
fn key_renamed_to_one_spanning_lines_becomes_explicit() {
    let quoted = first_item("- \"x\n  y\"\n");
    let commented = first_item("- - 1  # one\n  - 2\n");
    for (source, key, expected) in [
        ("a: 1\nb: 2\n", &quoted, "? \"x\n  y\"\n: 1\nb: 2\n"),
        ("b: 2\na: 1\n", &quoted, "b: 2\n? \"x\n  y\"\n: 1\n"),
        (
            "a: 1  # c\nb: 2\n",
            &quoted,
            "? \"x\n  y\"\n: 1  # c\nb: 2\n",
        ),
        (
            "a:\n  x: 1\nb: 2\n",
            &quoted,
            "? \"x\n  y\"\n:\n  x: 1\nb: 2\n",
        ),
        (
            "a: 1\nb: 2\n",
            &commented,
            "? [1,  # one\n  2]\n: 1\nb: 2\n",
        ),
        ("{a: 1, b: 2}\n", &quoted, "{\"x\n   y\": 1, b: 2}\n"),
    ] {
        assert_eq!(
            edit_root_valid(source, |m| assert!(m.rename_key("a", key))),
            expected,
            "{source:?}"
        );
    }

    let key = first_item("- \"x\n    y\"\n");
    assert_eq!(
        edit_nested("m:\n  a: 1\n  b: 2\n", "m", |m| assert!(
            m.rename_key("a", &key)
        )),
        "m:\n  ? \"x\n      y\"\n  : 1\n  b: 2\n"
    );
}

#[test]
fn new_key_spanning_lines_is_explicit() {
    let quoted = first_item("- \"x\n  y\"\n");
    let flow = first_item("- [1,\n  2]\n");
    for (source, key, expected) in [
        ("a: 1\n", &quoted, "a: 1\n? \"x\n  y\"\n: 5\n"),
        ("a: 1\n", &flow, "a: 1\n? [1,\n  2]\n: 5\n"),
        ("{a: 1}\n", &quoted, "{a: 1, \"x\n  y\": 5}\n"),
    ] {
        assert_eq!(edit_root_valid(source, |m| m.set(key, 5)), expected);
    }
}

#[test]
fn block_key_written_in_flow_form() {
    let block = value_of_k("k: |\n  x\n");
    let tagged_block = value_of_k("k: !!str |\n  x\n");
    let sequence = value_of_k("k:\n  - x\n  - y\n");
    let tagged_sequence = value_of_k("k: !t\n  - x\n");
    for (key, expected) in [
        (&block, "\"x\\n\": 1\nb: 2\n"),
        (&tagged_block, "!!str \"x\\n\": 1\nb: 2\n"),
        (&sequence, "[x, y]: 1\nb: 2\n"),
        (&tagged_sequence, "!t [x]: 1\nb: 2\n"),
    ] {
        assert_eq!(
            edit_root_valid("a: 1\nb: 2\n", |m| assert!(m.rename_key("a", key))),
            expected
        );
        assert_eq!(
            edit_root_valid("b: 2\n", |m| m.set(key, 1)),
            format!("b: 2\n{}", &expected[..expected.len() - "b: 2\n".len()])
        );
    }
}

#[test]
fn tagged_key_in_explicit_and_flow_entries() {
    let tagged = value_of_k("k: !t\n  - x\n");
    assert_eq!(
        edit_root_valid("? a\n: 1\n", |m| assert!(m.rename_key("a", &tagged))),
        "? !t [x]\n: 1\n"
    );
    assert_eq!(
        edit_root_valid("{a: 1}\n", |m| assert!(m.rename_key("a", &tagged))),
        "{!t [x]: 1}\n"
    );
    assert_eq!(
        edit_root_valid("{a: 1}\n", |m| m.set(&tagged, 2)),
        "{a: 1, !t [x]: 2}\n"
    );
}

#[test]
fn set_key_written_in_flow_form() {
    let set = |members: &[&str]| -> std::collections::BTreeSet<String> {
        members.iter().map(|m| m.to_string()).collect()
    };
    let nested: std::collections::BTreeSet<_> = [set(&["p", "q"])].into_iter().collect();
    assert_eq!(
        edit_root_valid("a: 1\n", |m| m.set("s", &nested)),
        "a: 1\ns: !!set\n  !!set {p: null, q: null}: null\n"
    );
    assert_eq!(
        edit_root_valid("a: 1\n", |m| assert!(m.rename_key("a", set(&["p"])))),
        "!!set {p: null}: 1\n"
    );
}

#[test]
fn flow_explicit_key_renamed() {
    assert_eq!(
        edit_root_valid("{? a : 1, b: 2}\n", |m| assert!(m.rename_key("a", "z"))),
        "{? z : 1, b: 2}\n"
    );
}

#[test]
fn flow_empty_explicit_key_renamed() {
    for (source, expected) in [
        ("{a: 1, ? }\n", "{a: 1, ? z}\n"),
        ("{a: 1,\n?\n}\n", "{a: 1,\n? z\n}\n"),
        ("{? : 1}\n", "{? z: 1}\n"),
        ("{?\n  a : 1}\n", "{? z : 1}\n"),
    ] {
        let text = edit_root_valid(source, |m| {
            let key = m.keys().last().unwrap();
            assert!(m.rename_key(&key, "z"));
        });
        assert_eq!(text, expected, "{source:?}");
    }
}

#[test]
fn renamed_key_keeps_its_anchor() {
    for (source, expected) in [
        ("&x a: 1\nb: *x\n", "&x z: 1\nb: *x\n"),
        ("&x !!str a: 1\nb: *x\n", "&x !!str z: 1\nb: *x\n"),
        ("? &x a\n: 1\nb: *x\n", "? &x z\n: 1\nb: *x\n"),
        ("? !!str &x a\n: 1\nb: *x\n", "? &x !!str z\n: 1\nb: *x\n"),
        ("{&x a: 1, b: *x}\n", "{&x z: 1, b: *x}\n"),
        ("{? &x a : 1, b: *x}\n", "{? &x z : 1, b: *x}\n"),
    ] {
        assert_eq!(
            edit_root_valid(source, |m| assert!(m.rename_key("a", "z"))),
            expected,
            "{source:?}"
        );
    }
    let quoted = first_item("- \"x\n  y\"\n");
    assert_eq!(
        edit_root_valid("&x a: 1\nb: *x\n", |m| assert!(m.rename_key("a", &quoted))),
        "? &x \"x\n  y\"\n: 1\nb: *x\n"
    );
}

#[test]
fn renamed_key_keeps_its_tag_only_if_still_the_new_key() {
    // `!!str 5` is a string, not the integer asked for, and `!t z` is not
    // the string `z`.
    assert_eq!(
        edit_root_valid("&x !!str a: 1\nb: *x\n", |m| assert!(m.rename_key("a", 5))),
        "&x 5: 1\nb: *x\n"
    );
    assert_eq!(
        edit_root_valid("!t a: 1\n", |m| assert!(
            m.rename_key(value_of_k("k: !t a\n"), "z")
        )),
        "z: 1\n"
    );
    assert_eq!(
        edit_root_valid("{!!str \"a\": 1, \"b\": 2}\n", |m| assert!(
            m.rename_key("a", "z")
        )),
        "{!!str \"z\": 1, \"b\": 2}\n"
    );
}

#[test]
fn renamed_key_given_as_annotated_node() {
    // The new key's own tag replaces the old one. The old anchor stays,
    // since aliases in this document refer to it; the new key's anchor is
    // used only if the old key had none.
    let tagged = value_of_k("k: !t [q]\n");
    let anchored = value_of_k("k: &y q\n");
    for (source, key, expected) in [
        ("&x !!str a: 1\nb: *x\n", &tagged, "&x !t [q]: 1\nb: *x\n"),
        ("&x a: 1\nb: *x\n", &anchored, "&x q: 1\nb: *x\n"),
        ("a: 1\n", &anchored, "&y q: 1\n"),
    ] {
        assert_eq!(
            edit_root_valid(source, |m| assert!(m.rename_key("a", key))),
            expected
        );
    }
}

#[test]
fn renamed_explicit_key_keeps_layout() {
    for (source, expected) in [
        ("? - a\n: b\n", "? z\n: b\n"),
        ("? |\n  a\n: b\n", "? z\n: b\n"),
        ("?\n: b\n", "? z\n: b\n"),
        ("?\n- a\n- b\n:\n- c\n", "? z\n:\n- c\n"),
        ("? - a\n", "? z\n"),
        ("? - a\nc: d\n", "? z\nc: d\n"),
        ("m:\n  ? - a\n  : b\n", "m:\n  ? z\n  : b\n"),
        ("m:\n  ?\n  : b\n", "m:\n  ? z\n  : b\n"),
        ("? # c\n  a\n: b\n", "? # c\n  z\n: b\n"),
        ("? &x # c\n  a\n: b\n", "? &x # c\n  z\n: b\n"),
        ("? # c\n  - a\n: b\n", "? # c\n  z\n: b\n"),
        ("?  # c\n: b\n", "?  z  # c\n: b\n"),
        ("{? # c\n  a : b}\n", "{? # c\n  z : b}\n"),
        ("? - a\n\nc: d\n", "? z\n\nc: d\n"),
        ("? - a\n\n: b\nc: d\n", "? z\n\n: b\nc: d\n"),
        ("? x: 1\n\n# c\nc: d\n", "? z\n\n# c\nc: d\n"),
        ("k:\n  ? - a\n\n  c: d\n", "k:\n  ? z\n\n  c: d\n"),
    ] {
        let file = YamlFile::from_str(source).unwrap();
        let root = root_mapping(&file);
        let m = root
            .get_mapping("m")
            .or_else(|| root.get_mapping("k"))
            .unwrap_or(root);
        let key = m.keys().next().unwrap();
        assert!(m.rename_key(&key, "z"), "{source:?}");
        assert_file_parser_shaped(&file);
        assert_eq!(file.to_string(), expected, "{source:?}");
    }
    for expected in [
        "? z\n: b\n",
        "? z\n:\n- c\n",
        "? z\n",
        "? z\nc: d\n",
        "m:\n  ? z\n  : b\n",
        "? # c\n  z\n: b\n",
        "? &x # c\n  z\n: b\n",
        "?  z  # c\n: b\n",
        "{? # c\n  z : b}\n",
    ] {
        assert_valid(expected);
    }
}

#[test]
fn set_value_ending_in_block_key_without_colon() {
    assert_eq!(
        edit_root("complex1:\n  ? - a", |m| m
            .set("complex1", m.get("complex1").unwrap())),
        "complex1:\n  ? - a"
    );
    assert_eq!(
        edit_root("complex1:\n  ? - a", |m| m.set("b", 1)),
        "complex1:\n  ? - a\nb: 1"
    );
}

#[test]
fn rename_empty_explicit_key_before_tab_line() {
    // The `\n` ending the key's line stayed, and was not turned into the
    // `\r` before the tab.
    let out = edit_root("?\r\t\n", |m| {
        let key = m.keys().next().unwrap();
        assert!(m.rename_key(key, ""));
    });
    assert_eq!(out, "? ''\r\t\n");
}
