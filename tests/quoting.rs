//! Strings written by an edit must read back as the same string, and plain
//! scalars in the input are kept as the parser read them.
//!
//! Strings written into flow collections are quoted where a plain scalar
//! would not read back as the same string there, and only there.
//!
//! Indicators (`?`, `|`, `>`, `+`, `-`, `!`, `&`, `*`, `%`, `#`, `,` and
//! the flow brackets) mean something only where a node starts, or inside
//! a flow collection; once a plain scalar has begun they are content, on
//! its first line and on continuation lines alike.

mod common;
use common::{assert_file_cst_ok, assert_parser_shaped, parse_clean};
use rowan::ast::AstNode;
use std::collections::BTreeMap;
use std::str::FromStr;
use yaml_edit::validator::Validator;
use yaml_edit::{
    debug, Document, Mapping, Scalar, ScalarType, ScalarValue, Sequence, SyntaxKind, YamlFile,
    YamlNode, YamlValue,
};

fn set_and_reparse(value: &str) -> (String, String, ScalarType) {
    let yaml = YamlFile::from_str("k: v\n").unwrap();
    let mapping = yaml.document().unwrap().as_mapping().unwrap();
    mapping.set("k", value);
    let text = yaml.to_string();
    let reparsed = YamlFile::from_str(&text).unwrap();
    let scalar = reparsed
        .document()
        .unwrap()
        .as_mapping()
        .unwrap()
        .get("k")
        .unwrap()
        .as_scalar()
        .unwrap()
        .clone();
    let typed = ScalarValue::from_scalar(&scalar);
    (text, scalar.as_string(), typed.scalar_type())
}

#[test]
fn strings_that_need_quoting_or_escapes() {
    for s in [
        ".inf", "+.inf", "-.inf", ".Inf", "-.INF", ".nan", ".NaN", ".NAN",
    ] {
        assert_eq!(
            set_and_reparse(s),
            (format!("k: '{s}'\n"), s.to_string(), ScalarType::String),
            "{s}"
        );
    }
    for (value, written) in [
        ("\u{7}bell", "k: \"\\abell\"\n"),
        ("\u{0}", "k: \"\\0\"\n"),
        ("\x01", "k: \"\\x01\"\n"),
        ("a\rb", "k: \"a\\rb\"\n"),
        ("a\r\nb", "k: \"a\\r\\nb\"\n"),
    ] {
        assert_eq!(
            set_and_reparse(value),
            (written.to_string(), value.to_string(), ScalarType::String),
            "{value:?}"
        );
    }
}

/// Regression from mutation_invariants fuzz: a key starting with `#`, a
/// comment indicator, or with `.` and digits was lost when the result was
/// parsed again.
#[test]
fn set_key_that_could_read_as_something_else() {
    for (seed, key, expected) in [
        (
            "literal: |\n  line1\n  line2\n",
            "#aaaaa",
            "literal: |\n  line1\n  line2\n'#aaaaa': ''\n",
        ),
        ("a: \"\"\n", ".999 a", "a: \"\"\n.999 a: ''\n"),
    ] {
        let doc = Document::from_str(seed).unwrap();
        let mapping = doc.as_mapping().unwrap();
        mapping.set(key, "");
        common::assert_edited(&doc, expected);
        assert!(mapping.contains_key(key));
        let reparsed = Document::from_str(expected).unwrap();
        assert!(reparsed.as_mapping().unwrap().contains_key(key), "{key:?}");
    }
}

/// Check that `doc` has the same tree as a re-parse of its text, and
/// return the scalars the re-parse holds, in order.
#[track_caller]
fn reparsed_scalars(doc: &Document) -> Vec<String> {
    assert_parser_shaped(doc);
    Document::from_str(&doc.to_string())
        .unwrap()
        .syntax()
        .descendants()
        .filter_map(Scalar::cast)
        .map(|s| s.as_string())
        .collect()
}

fn seq(doc: &Document) -> Sequence {
    doc.as_mapping().unwrap().get_sequence("x").unwrap()
}

fn map(doc: &Document) -> Mapping {
    doc.as_mapping().unwrap().get_mapping("x").unwrap()
}

#[test]
fn strings_in_flow_sequence() {
    for (value, rendered) in [
        ("a, b", "\"a, b\""),
        ("x]", "\"x]\""),
        ("x{y", "\"x{y\""),
        ("x\ny", "\"x\\ny\""),
        ("a: b", "'a: b'"),
        ("1", "'1'"),
        ("plain", "plain"),
        ("a:b", "a:b"),
    ] {
        let doc = Document::from_str("x: [1]\n").unwrap();
        seq(&doc).push(value);
        assert_eq!(doc.to_string(), format!("x: [1, {rendered}]\n"));
        assert_eq!(reparsed_scalars(&doc), ["x", "1", value]);

        let doc = Document::from_str("x: [1]\n").unwrap();
        seq(&doc).insert(0, value);
        assert_eq!(doc.to_string(), format!("x: [{rendered}, 1]\n"));
        assert_eq!(reparsed_scalars(&doc), ["x", value, "1"]);

        let doc = Document::from_str("x: [1, 2]\n").unwrap();
        assert!(seq(&doc).set(0, value));
        assert_eq!(doc.to_string(), format!("x: [{rendered}, 2]\n"));
        assert_eq!(reparsed_scalars(&doc), ["x", value, "2"]);
    }
}

#[test]
fn strings_in_flow_mapping() {
    for (value, rendered) in [
        ("a, b", "\"a, b\""),
        ("x]", "\"x]\""),
        ("x\ny", "\"x\\ny\""),
        ("1", "'1'"),
        ("plain", "plain"),
    ] {
        let doc = Document::from_str("x: {k: 1}\n").unwrap();
        map(&doc).set("k", value);
        assert_eq!(doc.to_string(), format!("x: {{k: {rendered}}}\n"));
        assert_eq!(reparsed_scalars(&doc), ["x", "k", value]);

        let doc = Document::from_str("x: {k: 1}\n").unwrap();
        map(&doc).set(value, 2);
        assert_eq!(doc.to_string(), format!("x: {{k: 1, {rendered}: 2}}\n"));
        assert_eq!(reparsed_scalars(&doc), ["x", "k", "1", value, "2"]);

        let doc = Document::from_str("x: {k: 1}\n").unwrap();
        assert!(map(&doc).rename_key("k", value));
        assert_eq!(doc.to_string(), format!("x: {{{rendered}: 1}}\n"));
        assert_eq!(reparsed_scalars(&doc), ["x", value, "1"]);

        let doc = Document::from_str("x: {k: 1, m: 2}\n").unwrap();
        assert!(map(&doc).insert_after("k", value, value));
        assert_eq!(
            doc.to_string(),
            format!("x: {{k: 1, {rendered}: {rendered}, m: 2}}\n")
        );
        assert_eq!(
            reparsed_scalars(&doc),
            ["x", "k", "1", value, value, "m", "2"]
        );
    }
}

#[test]
fn strings_in_flow_mapping_nested_in_flow_sequence() {
    let doc = Document::from_str("x: [{k: 1}]\n").unwrap();
    let nested = seq(&doc).get(0).unwrap();
    nested.as_mapping().unwrap().set("a, b", "c]");
    assert_eq!(doc.to_string(), "x: [{k: 1, \"a, b\": \"c]\"}]\n");
    assert_eq!(reparsed_scalars(&doc), ["x", "k", "1", "a, b", "c]"]);
}

#[test]
fn plain_scalar_node_copied_into_flow_collections() {
    let source = Document::from_str("a: a, b\n").unwrap();
    let value = source.as_mapping().unwrap().get("a").unwrap();

    let doc = Document::from_str("x: [1]\n").unwrap();
    seq(&doc).push(&value);
    assert_eq!(doc.to_string(), "x: [1, \"a, b\"]\n");
    assert_eq!(reparsed_scalars(&doc), ["x", "1", "a, b"]);

    let doc = Document::from_str("x: {k: 1}\n").unwrap();
    map(&doc).set(&value, &value);
    assert_eq!(doc.to_string(), "x: {k: 1, \"a, b\": \"a, b\"}\n");
    assert_eq!(reparsed_scalars(&doc), ["x", "k", "1", "a, b", "a, b"]);
}

#[test]
fn strings_in_double_quoted_flow_mapping() {
    let doc = Document::from_str("{\"a\": \"b\", \"n\": 1}\n").unwrap();
    let m = doc.as_mapping().unwrap();
    m.set("c", "d");
    m.set("a", "x");
    m.set("s", "1");
    m.set("i", 2);
    m.set("t", true);
    assert_eq!(
        doc.to_string(),
        "{\"a\": \"x\", \"n\": 1, \"c\": \"d\", \"s\": \"1\", \"i\": 2, \"t\": true}\n"
    );
    assert_eq!(
        reparsed_scalars(&doc),
        ["a", "x", "n", "1", "c", "d", "s", "1", "i", "2", "t", "true"]
    );

    let doc = Document::from_str("{\"a\": \"b\", \"c\": \"d\"}\n").unwrap();
    let m = doc.as_mapping().unwrap();
    assert!(m.rename_key("a", "z"));
    assert!(m.insert_after("z", "k", "v"));
    assert!(m.insert_before("c", "a, b", "x]"));
    assert_eq!(
        doc.to_string(),
        "{\"z\": \"b\", \"k\": \"v\", \"a, b\": \"x]\", \"c\": \"d\"}\n"
    );
    assert_eq!(
        reparsed_scalars(&doc),
        ["z", "b", "k", "v", "a, b", "x]", "c", "d"]
    );
}

#[test]
fn replaced_entry_in_double_quoted_flow_mapping() {
    let source = "{\"a\": \"x\", \"b\": \"y\"}\n";
    let doc = Document::from_str(source).unwrap();
    doc.as_mapping()
        .unwrap()
        .insert_at_index_preserving(0, "b", "z");
    assert_eq!(doc.to_string(), "{\"a\": \"x\", \"b\": \"z\"}\n");
    assert_eq!(reparsed_scalars(&doc), ["a", "x", "b", "z"]);

    let doc = Document::from_str(source).unwrap();
    assert!(doc.as_mapping().unwrap().move_before("a", "b", "z"));
    assert_eq!(doc.to_string(), "{\"b\": \"z\", \"a\": \"x\"}\n");
    assert_eq!(reparsed_scalars(&doc), ["b", "z", "a", "x"]);
}

#[test]
fn strings_in_double_quoted_flow_sequence() {
    let doc = Document::from_str("x: [\"a\", 1]\n").unwrap();
    let s = seq(&doc);
    s.push("b");
    s.insert(0, "c");
    assert!(s.set(1, "d"));
    s.push(3);
    assert_eq!(doc.to_string(), "x: [\"c\", \"d\", 1, \"b\", 3]\n");
    assert_eq!(reparsed_scalars(&doc), ["x", "c", "d", "1", "b", "3"]);
}

#[test]
fn strings_in_mixed_flow_collections() {
    let doc = Document::from_str("{\"a\": b}\n").unwrap();
    doc.as_mapping().unwrap().set("c", "d");
    assert_eq!(doc.to_string(), "{\"a\": b, c: d}\n");
    assert_eq!(reparsed_scalars(&doc), ["a", "b", "c", "d"]);

    let doc = Document::from_str("x: [\"a\", 'b']\n").unwrap();
    seq(&doc).push("c");
    assert_eq!(doc.to_string(), "x: [\"a\", 'b', c]\n");
    assert_eq!(reparsed_scalars(&doc), ["x", "a", "b", "c"]);
}

#[test]
fn strings_in_flow_collections_without_strings() {
    let doc = Document::from_str("x: {}\n").unwrap();
    map(&doc).set("k", "v");
    assert_eq!(doc.to_string(), "x: {k: v}\n");
    assert_eq!(reparsed_scalars(&doc), ["x", "k", "v"]);

    let doc = Document::from_str("x: [1, 2]\n").unwrap();
    seq(&doc).push("a");
    assert_eq!(doc.to_string(), "x: [1, 2, a]\n");
    assert_eq!(reparsed_scalars(&doc), ["x", "1", "2", "a"]);

    let doc = Document::from_str("[]\n").unwrap();
    doc.as_sequence().unwrap().push("a");
    assert_eq!(doc.to_string(), "[a]\n");
    assert_eq!(reparsed_scalars(&doc), ["a"]);
}

#[test]
fn strings_in_nested_double_quoted_flow_collections() {
    // A collection without strings of its own follows the nearest
    // enclosing flow collection that has some.
    let doc = Document::from_str("{\"a\": [], \"b\": [1], \"c\": {}}\n").unwrap();
    let m = doc.as_mapping().unwrap();
    m.get_sequence("a").unwrap().push("x");
    m.get_sequence("b").unwrap().push("y");
    m.get_mapping("c").unwrap().set("k", "v");
    assert_eq!(
        doc.to_string(),
        "{\"a\": [\"x\"], \"b\": [1, \"y\"], \"c\": {\"k\": \"v\"}}\n"
    );
    assert_eq!(
        reparsed_scalars(&doc),
        ["a", "x", "b", "1", "y", "c", "k", "v"]
    );

    // A collection with strings of its own goes by those.
    let doc = Document::from_str("{\"a\": [b]}\n").unwrap();
    let m = doc.as_mapping().unwrap();
    m.get_sequence("a").unwrap().push("c");
    assert_eq!(doc.to_string(), "{\"a\": [b, c]}\n");
    assert_eq!(reparsed_scalars(&doc), ["a", "b", "c"]);

    // Raw collections written into a double-quoted collection follow it.
    let doc = Document::from_str("{\"a\": \"b\"}\n").unwrap();
    let m = doc.as_mapping().unwrap();
    m.set(
        "m",
        YamlValue::Mapping(BTreeMap::from([("k".to_string(), YamlValue::from("v"))])),
    );
    m.set("s", vec![YamlValue::from("x"), YamlValue::from(1)]);
    assert_eq!(
        doc.to_string(),
        "{\"a\": \"b\", \"m\": {\"k\": \"v\"}, \"s\": [\"x\", 1]}\n"
    );
    assert_eq!(
        reparsed_scalars(&doc),
        ["a", "b", "m", "k", "v", "s", "x", "1"]
    );
}

#[test]
fn double_quoted_flow_collection_in_block_collection() {
    // Only enclosing flow collections count.
    let doc = Document::from_str("\"a\": \"b\"\nx: []\n").unwrap();
    seq(&doc).push("c");
    assert_eq!(doc.to_string(), "\"a\": \"b\"\nx: [c]\n");
    assert_eq!(reparsed_scalars(&doc), ["a", "b", "x", "c"]);
}

#[test]
fn explicit_style_kept_in_double_quoted_flow_collection() {
    let doc = Document::from_str("x: [\"a\"]\n").unwrap();
    seq(&doc).push(ScalarValue::single_quoted("b"));
    assert_eq!(doc.to_string(), "x: [\"a\", 'b']\n");
    assert_eq!(reparsed_scalars(&doc), ["x", "a", "b"]);

    // A scalar node keeps the style it was written in.
    let source = Document::from_str("a: plain\n").unwrap();
    let value = source.as_mapping().unwrap().get("a").unwrap();
    let doc = Document::from_str("x: [\"a\"]\n").unwrap();
    seq(&doc).push(&value);
    assert_eq!(doc.to_string(), "x: [\"a\", plain]\n");
    assert_eq!(reparsed_scalars(&doc), ["x", "a", "plain"]);
}

#[test]
fn rename_json_like_key_before_adjacent_colon() {
    let plain_node = Document::from_str("p\n").unwrap();
    let plain_node = plain_node.as_scalar().unwrap();
    for (source, old, new, expected, scalars) in [
        (
            "{\"a\":b}\n",
            "a",
            YamlValue::from("z"),
            "{z: b}\n",
            vec!["z", "b"],
        ),
        (
            "{'a':b}\n",
            "a",
            YamlValue::from("z"),
            "{z: b}\n",
            vec!["z", "b"],
        ),
        (
            "{\"a\":b}\n",
            "a",
            YamlValue::from(1),
            "{1: b}\n",
            vec!["1", "b"],
        ),
        (
            "{\"a\":\"b\"}\n",
            "a",
            YamlValue::from("z"),
            "{\"z\":\"b\"}\n",
            vec!["z", "b"],
        ),
        (
            "[\"a\":b]\n",
            "a",
            YamlValue::from("z"),
            "[z: b]\n",
            vec!["z", "b"],
        ),
        (
            "{\"a\":, c: d}\n",
            "a",
            YamlValue::from("z"),
            "{z:, c: d}\n",
            vec!["z", "", "c", "d"],
        ),
        (
            "{\"a\":b, \"c\":d}\n",
            "a",
            YamlValue::from("z"),
            "{z: b, \"c\":d}\n",
            vec!["z", "b", "c", "d"],
        ),
        // K3WX from the YAML test suite, with the `:` on a line of its own.
        (
            "{ \"foo\" # comment\n  :bar }\n",
            "foo",
            YamlValue::from("z"),
            "{ z # comment\n  : bar }\n",
            vec!["z", "bar"],
        ),
        (
            "{\"a\"\n:?b}\n",
            "a",
            YamlValue::from("z"),
            "{z\n: ?b}\n",
            vec!["z", "?b"],
        ),
    ] {
        let doc = Document::from_str(source).unwrap();
        let m = doc.as_mapping().unwrap_or_else(|| {
            doc.as_sequence()
                .unwrap()
                .get(0)
                .unwrap()
                .as_mapping()
                .unwrap()
                .clone()
        });
        assert!(m.rename_key(old, new), "{source:?}");
        assert_eq!(doc.to_string(), expected, "{source:?}");
        assert_eq!(reparsed_scalars(&doc), scalars, "{source:?}");
    }
    let doc = Document::from_str("{\"a\":b}\n").unwrap();
    assert!(doc.as_mapping().unwrap().rename_key("a", &plain_node));
    assert_eq!(doc.to_string(), "{p: b}\n");
    assert_eq!(reparsed_scalars(&doc), ["p", "b"]);
}

fn root_mapping(file: &YamlFile) -> Mapping {
    file.document().unwrap().as_mapping().unwrap()
}

fn string(node: &YamlNode) -> String {
    node.as_scalar().unwrap().as_string()
}

fn keys(mapping: &Mapping) -> Vec<String> {
    mapping.keys().map(|k| string(&k)).collect()
}

fn items(node: &YamlNode) -> Vec<String> {
    node.as_sequence()
        .unwrap()
        .values()
        .map(|v| string(&v))
        .collect()
}

/// Check the root mapping of `file` holds exactly `entries`.
#[track_caller]
fn assert_entries(file: &YamlFile, entries: &[(&str, &str)]) {
    let mapping = root_mapping(file);
    let expected: Vec<&str> = entries.iter().map(|(k, _)| *k).collect();
    assert_eq!(keys(&mapping), expected, "{file:?}");
    for (key, value) in entries {
        assert_eq!(string(&mapping.get(*key).unwrap()), *value, "{file:?}");
    }
}

fn assert_metadata_edits(file: &YamlFile, mapping: &Mapping, yaml: &str) {
    assert_eq!(file.to_string(), yaml);
    assert_file_cst_ok(file);
    let metadata = mapping.get_mapping("metadata").unwrap();
    metadata.set("version", "0.1.1");
    assert_file_cst_ok(file);
    assert_eq!(
        file.to_string(),
        yaml.replace("version: 0.1.0", "version: 0.1.1")
    );
    metadata.set("status", "released");
    assert_file_cst_ok(file);
    assert_eq!(
        file.to_string(),
        yaml.replace("version: 0.1.0", "version: 0.1.1")
            .replace("status: draft", "status: released")
    );
}

/// Plain text with quotes, commas after flags, embedded JSON or angle
/// brackets stays one plain scalar, which edits elsewhere leave alone.
#[test]
fn plain_description_survives_edits() {
    for value in [
        "author's machine",
        "site's readiness",
        "uses \"quoted words\" here",
        "uses 'single quotes' here",
        "follows sh -c, python3 -c and @resource files, and reports",
        "-c, python3",
        "--verbose, --log-level=debug",
        r#"Shape is {"entries":[{"name":"..."}]}."#,
        r#"call query-trends {"kind":"TrendsQuery"}"#,
        "text [one, two] follows",
        "Absolute path to a directory for the capture, the composition and the MP4. It is created if missing. Leave it out and it lands under ~/launch-video/<site>, never in the play run workspace, which is deleted when the run ends.",
    ] {
        let yaml = format!(
            "description: {value}\nmetadata:\n  version: 0.1.0 # keep\n  status: draft\n"
        );
        let file = YamlFile::from_str(&yaml).unwrap();
        let mapping = root_mapping(&file);
        assert_eq!(string(&mapping.get("description").unwrap()), value);
        assert_metadata_edits(&file, &mapping, &yaml);
    }
}

#[test]
fn structural_indicators_survive_edits() {
    let yaml = "folded: >-\n  first line\n  second line\nflow: [~/launch-video/<site>, kept]\nmetadata:\n  version: 0.1.0\n  status: draft\n";
    let file = YamlFile::from_str(yaml).unwrap();
    let mapping = root_mapping(&file);
    assert_eq!(
        string(&mapping.get("folded").unwrap()),
        "first line second line"
    );
    assert_eq!(
        items(&mapping.get("flow").unwrap()),
        ["~/launch-video/<site>", "kept"]
    );
    assert_metadata_edits(&file, &mapping, yaml);

    let yaml = "argv: [python3, '@resource{traps.py}', $project, --validate, -c, --probe]\nmetadata:\n  version: 0.1.0\n  status: draft\n";
    let file = YamlFile::from_str(yaml).unwrap();
    let mapping = root_mapping(&file);
    assert_eq!(
        items(&mapping.get("argv").unwrap()),
        [
            "python3",
            "@resource{traps.py}",
            "$project",
            "--validate",
            "-c",
            "--probe",
        ]
    );
    assert_metadata_edits(&file, &mapping, yaml);

    let yaml = "---\nquoted: 'author''s machine'\ndouble: \"quoted words\"\nflow: {list: [one, two], flag: --validate}\nblock:\n- --validate\n- ---not-a-marker\nmetadata:\n  version: 0.1.0\n  status: draft\n...\n---\nother: kept\n";
    let file = YamlFile::from_str(yaml).unwrap();
    assert_eq!(file.documents().count(), 2);
    let mapping = root_mapping(&file);
    assert_eq!(string(&mapping.get("quoted").unwrap()), "author's machine");
    assert_eq!(string(&mapping.get("double").unwrap()), "quoted words");
    let flow = mapping.get_mapping("flow").unwrap();
    assert_eq!(flow.len(), 2);
    assert_eq!(items(&flow.get("list").unwrap()), ["one", "two"]);
    assert_eq!(string(&flow.get("flag").unwrap()), "--validate");
    assert_eq!(
        items(&mapping.get("block").unwrap()),
        ["--validate", "---not-a-marker"]
    );
    assert_metadata_edits(&file, &mapping, yaml);
}

/// Entries of block mappings whose plain keys or values hold characters
/// that are indicators elsewhere.
#[test]
fn plain_scalars_in_block_mapping() {
    for (yaml, entries) in [
        // A directive is only a `%` that opens a line; elsewhere it is
        // content.
        ("a: b%c\nnext: x\n", &[("a", "b%c"), ("next", "x")][..]),
        ("a: 50%\nnext: x\n", &[("a", "50%"), ("next", "x")]),
        ("k%j: v\nnext: x\n", &[("k%j", "v"), ("next", "x")]),
        ("%YAML 1.2\n---\na: 1\n", &[("a", "1")]),
        // A single line break folds to a space; a blank line keeps one
        // newline.
        ("a: one\n  two\nb: y\n", &[("a", "one two"), ("b", "y")]),
        ("a: one\n\n  two\nb: y\n", &[("a", "one\ntwo"), ("b", "y")]),
        (
            "a: one\n\n\n  two\nb: y\n",
            &[("a", "one\n\ntwo"), ("b", "y")],
        ),
        (
            "a: one\n  two\n\n  three\nb: y\n",
            &[("a", "one two\nthree"), ("b", "y")],
        ),
        // Indented past the key, this is a continuation; at the key's own
        // column it would be the next entry.
        ("a: one\n  two\nb: z\n", &[("a", "one two"), ("b", "z")]),
        // Per YAML 1.2 section 7.1 a tag, anchor or alias applies to the
        // start of a node. Once a plain scalar has begun, `!`, `&` and `*`
        // are ordinary characters, whether or not a space precedes them.
        ("a: x !!b\nnext: k\n", &[("a", "x !!b"), ("next", "k")]),
        ("a: x !t\nnext: k\n", &[("a", "x !t"), ("next", "k")]),
        ("a: x &anc\nnext: k\n", &[("a", "x &anc"), ("next", "k")]),
        ("a: x *ref\nnext: k\n", &[("a", "x *ref"), ("next", "k")]),
        ("a: x !t y\nnext: k\n", &[("a", "x !t y"), ("next", "k")]),
        // The shape a differential fuzz run reduced to: a tag-looking run
        // mid-scalar whose trailing `]` used to be left with nowhere to go.
        (
            "a: a:] !!b%as:]\nnext: k\n",
            &[("a", "a:] !!b%as:]"), ("next", "k")],
        ),
        // Trailing spaces and tabs used to end up in the scalar, making the
        // value `b ` or `b\t`. They stay in the tree, outside the scalar.
        ("a: b \n", &[("a", "b")]),
        ("a: b\t\n", &[("a", "b")]),
        ("a: b \n  c \n", &[("a", "b c")]),
        // `,` separates entries only inside a flow collection, so in block
        // context it is text, also at the start of a continuation line. A
        // leading `,` is a spec violation the validator reports, but the
        // parser still keeps it as the scalar's text.
        ("a: x,y\nb: z\n", &[("a", "x,y"), ("b", "z")]),
        ("a: x\n  ,y\nb: z\n", &[("a", "x ,y"), ("b", "z")]),
        ("a: ,y\nb: z\n", &[("a", ",y"), ("b", "z")]),
        // A plain scalar starting with `-` or `.` spans internal spaces
        // like any other.
        (
            "args: --verbose --log-level=debug\n",
            &[("args", "--verbose --log-level=debug")],
        ),
        // A single sign still prefixes a number.
        ("v: +5\n", &[("v", "+5")]),
        ("v: -5\n", &[("v", "-5")]),
        ("v: +.INF\n", &[("v", "+.INF")]),
        ("f: +.INF\n", &[("f", "+.INF")]),
        // A second sign is content: `++: v` is keyed `++`, as both saphyr
        // and PyYAML read it.
        ("++: v\n", &[("++", "v")]),
        ("+-: v\n", &[("+-", "v")]),
        ("--: v\n", &[("--", "v")]),
        ("a: ++\n", &[("a", "++")]),
        // A bare `+` before a mapping colon is the key, as a bare `-` is.
        ("+: v\n", &[("+", "v")]),
        ("-: v\n", &[("-", "v")]),
        ("a: b\n+c: d\n", &[("a", "b"), ("+c", "d")]),
        // A lone `+` used to be a bare PLUS token that no parse rule claims,
        // stranding what followed it in an ERROR node.
        ("k: +\nk2: w\n", &[("k", "+"), ("k2", "w")]),
        ("a: 1\n+: 2\nb: 3\n", &[("a", "1"), ("+", "2"), ("b", "3")]),
        // `|` and `>` open a block scalar only at the start of a node, so
        // `a|b: v` is keyed `a|b`, as both saphyr and PyYAML read it.
        ("a|b: v\nc: d\n", &[("a|b", "v"), ("c", "d")]),
        ("a>b: v\nc: d\n", &[("a>b", "v"), ("c", "d")]),
        ("a+b: v\nc: d\n", &[("a+b", "v"), ("c", "d")]),
        ("a|: v\nc: d\n", &[("a|", "v"), ("c", "d")]),
        // A `+` is a chomping indicator only where the `|` or `>` before it
        // opens the value, so in `/|+: v` it is content too, as saphyr
        // reads it.
        ("/|+: v\n", &[("/|+", "v")]),
        ("a|+: v\n", &[("a|+", "v")]),
        ("x>+: v\n", &[("x>+", "v")]),
        // A quote no longer opens a quoted scalar once a plain one has
        // begun, so `+'a': 1` is keyed `+'a'`, as PyYAML reads it.
        ("+'a': 1\n", &[("+'a'", "1")]),
        ("+\"a\": 1\n", &[("+\"a\"", "1")]),
        ("+'a' : 1\n", &[("+'a'", "1")]),
        ("'quoted': v\na: 'q'\n", &[("quoted", "v"), ("a", "q")]),
        // A quote opening a continuation line is content too, while a
        // quoted key on the next line still opens a quoted scalar.
        ("a: b\n  'c\nd: 1\n", &[("a", "b 'c"), ("d", "1")]),
        ("a: b\n'x: y': 1\n", &[("a", "b"), ("x: y", "1")]),
        // A key beginning with `+` folds across spaces like any other plain
        // scalar, and a `+` after a space is content, as saphyr reads it.
        ("+ x: v\n", &[("+ x", "v")]),
        ("+ +: v\n", &[("+ +", "v")]),
        ("+ -: v\n", &[("+ -", "v")]),
        ("+ +x: v\n", &[("+ +x", "v")]),
        ("+ : v\n", &[("+", "v")]),
        ("a +: v\n", &[("a +", "v")]),
        // The fuzz case those reduced from.
        ("+ +x ?dd  ? x: (+x", &[("+ +x ?dd  ? x", "(+x")]),
        // The node properties and block headers are content once a plain
        // scalar has begun: `+?: v` is keyed `+?`, as saphyr reads it.
        ("+?: v\n", &[("+?", "v")]),
        ("+|: v\n", &[("+|", "v")]),
        ("+>: v\n", &[("+>", "v")]),
        ("+&: v\n", &[("+&", "v")]),
        ("+*: v\n", &[("+*", "v")]),
        ("+!: v\n", &[("+!", "v")]),
        ("+%: v\n", &[("+%", "v")]),
        ("-?: v\n", &[("-?", "v")]),
        ("+ ?: v\n", &[("+ ?", "v")]),
        ("+ |: v\n", &[("+ |", "v")]),
        ("+ &: v\n", &[("+ &", "v")]),
        // A `?` glued to a key, or past the start of a scalar, is content.
        ("x ?y: v\n", &[("x ?y", "v")]),
        ("?key: v\n", &[("?key", "v")]),
        (
            "es_ttpattern: ?!!r\\d{2}-\\d{2}'\n",
            &[("es_ttpattern", "?!!r\\d{2}-\\d{2}'")],
        ),
        // A `?` followed by a space still opens an explicit key.
        ("? key\n: value\n", &[("key", "value")]),
        ("? a\n: 1\n? b\n: 2\n", &[("a", "1"), ("b", "2")]),
        // A `?` at the key column opens the next entry after a plain value.
        ("k: v\n? a\n: 1\n", &[("k", "v"), ("a", "1")]),
        // A `#` glued to plain-scalar content is not a comment: `:#: v` is
        // keyed `:#`, as saphyr reads it.
        (":#: v\n", &[(":#", "v")]),
        ("+#: v\n", &[("+#", "v")]),
        ("a#: v\n", &[("a#", "v")]),
        (":#: v\nc: d\n", &[(":#", "v"), ("c", "d")]),
        // `...` is the document-end marker only when it is the whole line.
        ("k: ...w\n", &[("k", "...w")]),
        // A scalar that starts with `:` keeps flow indicators as content
        // outside a flow collection.
        ("a: :m{\n", &[("a", ":m{")]),
        // A `-` not followed by a space is scalar content, not a sequence
        // indicator, so `-{ [a]: v }` is keyed `-{ [a]`, as both saphyr and
        // PyYAML read it.
        ("-{ [a]: v }\n", &[("-{ [a]", "v }")]),
    ] {
        let file = parse_clean(yaml);
        assert_entries(&file, entries);
    }

    // A regex: `[\.a-zA-Z]{2,}$` after a `+` quantifier.
    let pattern = r"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+[\.a-zA-Z]{2,}$";
    let file = parse_clean(&format!("pattern: {pattern}\nnext: keep\n"));
    assert_entries(&file, &[("pattern", pattern), ("next", "keep")]);
}

/// Documents whose root is a plain scalar spanning lines that start with
/// characters that are indicators elsewhere.
#[test]
fn plain_root_scalars() {
    for (yaml, value) in [
        // The document's own node has no enclosing collection to be
        // confused with, so a continuation need not be indented past the
        // first line.
        ("ab\ncd\n", "ab cd"),
        ("ab\ncd\nef\n", "ab cd ef"),
        ("ab\n  cd\n", "ab cd"),
        ("ab\n\ncd\n", "ab\ncd"),
        // Nor past the first line's own column: `" a\nb\n"` is `a b`, as
        // saphyr and PyYAML both read it. The second line used to be swept
        // into an ERROR node while from_str still reported success.
        (" a\nb\n", "a b"),
        ("  a\n b\n", "a b"),
        ("a\nb\n", "a b"),
        (" a\n b\n", "a b"),
        // A node property, a lone `+` or a flow indicator on a
        // continuation line is content, as the line is inside a node
        // already. A blank line before it folds to a line break.
        ("a\n!t\n", "a !t"),
        ("a\n&x\n", "a &x"),
        ("a\n&\n", "a &"),
        ("a\n*\n", "a *"),
        (" a\n\n &\n", "a\n&"),
        ("a\n*x\n", "a *x"),
        ("a\n*-\n", "a *-"),
        ("a\n+\n", "a +"),
        ("a\n +\n", "a +"),
        ("+\nx\n", "+ x"),
        (" a\n|\n", "a |"),
        (" a\n>\n", "a >"),
        (" a\n-\n", "a -"),
        (" a\n?\n", "a ?"),
        ("\n yym|e\n|\n\n", "yym|e |"),
        ("a\n}\n", "a }"),
        ("a\n[:]\n", "a [:]"),
        // The colon of a flow collection does not make the line a key of a
        // block mapping; it used to, sweeping the line into an ERROR node.
        ("a\n{:}\n", "a {:}"),
        ("a\n[[:]]\n", "a [[:]]"),
        ("a\n}", "a }"),
        ("a\n{", "a {"),
        ("a\n]", "a ]"),
        ("a\n[", "a ["),
        ("1\n}", "1 }"),
        // `k:#foo` has no space before the `#`, so it is not a comment;
        // suite case 3MYT.
        ("k:#foo\n &a !t s\n", "k:#foo &a !t s"),
        // A scalar continuing through `%` keeps flow indicators as content
        // and folds across internal spaces, as `a b}` and `r x {` do.
        ("a %}\n", "a %}"),
        ("a %x}\n", "a %x}"),
        ("a %,b\n", "a %,b"),
        ("a %[1]\n", "a %[1]"),
        ("r % {\n", "r % {"),
        ("r % x\n", "r % x"),
        ("a % }\n", "a % }"),
        ("r %{\n", "r %{"),
        // A `%` opens a directive only at the start of a document. The
        // scalar used to end at it, leaving the `[` to open a flow sequence
        // that never closed.
        ("v\n%\n[\n", "v % ["),
        (" v\n%\n'\n", "v % '"),
        (" v\n%\n{\n", "v % {"),
        (" v\n%\n\"\n", "v % \""),
        ("v\n%\n", "v %"),
        ("...w\n", "...w"),
        ("...x.y\n", "...x.y"),
        // The body of a lone tag is still a node of the collection the tag
        // sits in, so its continuation only has to clear the tag's column.
        ("!\n  a\n b\n", "a b"),
        ("x\n  a\n b\n", "x a b"),
    ] {
        let file = parse_clean(yaml);
        let value_found = file.document().unwrap().as_scalar().unwrap().as_string();
        assert_eq!(value_found, value, "{yaml:?}");
    }
    // `: ` is a mapping indicator, so `[a: 1]` on a continuation line opens
    // a mapping. The parser keeps it as a second node of the document,
    // which the validator reports.
    let file = parse_clean("a\n[a: 1]\n");
    assert_eq!(
        common::violations(&file),
        [(
            "Document holds more than one node".to_string(),
            "2:1".to_string()
        )]
    );
}

/// Block sequences whose items are plain scalars spanning lines.
#[test]
fn plain_scalars_in_block_sequence() {
    for (yaml, expected) in [
        // A sequence entry's continuation only has to clear the sequence's
        // column, not the entry's content column.
        ("- x\n y\n", &["x y"][..]),
        // A `!` on a continuation line is scalar content, not a tag, as
        // PyYAML and saphyr read it.
        ("- a\n !\n", &["a !"]),
        ("- a\n !x\n", &["a !x"]),
        // A scalar that starts with `:` keeps flow indicators as content
        // outside a flow collection: `- :m{` is the single item `:m{`.
        ("- :m{\n", &[":m{"]),
        ("- :m}x\n", &[":m}x"]),
        ("- :m,n\n", &[":m,n"]),
        ("- :m[1]\n", &[":m[1]"]),
        // At the root, with no key above it.
        ("- a\n - b\n", &["a - b"]),
        ("- x\n- y\n", &["x", "y"]),
        // A block scalar after an entry is still a block scalar.
        ("- a\n- |\n  b\n", &["a", "b\n"]),
        ("- +\n- x\n", &["+", "x"]),
        ("- +\n  x\n- y\n", &["+ x", "y"]),
        // A `?` past the dash continues the entry. It used to be swept
        // into an ERROR node along with every entry after it.
        ("- x\n  ?\n- y\n", &["x ?", "y"]),
        ("- x\n  ? c\n", &["x ? c"]),
        // A hyphen that is not a sequence marker is content.
        ("- a-b\n", &["a-b"]),
        ("- -1\n", &["-1"]),
        ("- x -y\n", &["x -y"]),
        // Trailing white space is outside the scalar.
        ("- b \n- c\t\n", &["b", "c"]),
    ] {
        let file = parse_clean(yaml);
        let root = file.document().unwrap().as_sequence().unwrap();
        let values: Vec<String> = root.values().map(|v| string(&v)).collect();
        assert_eq!(values, expected, "{yaml:?}");
    }
    let file = parse_clean("x\n !\n");
    assert_eq!(
        file.document().unwrap().as_scalar().unwrap().as_string(),
        "x !"
    );
    for (yaml, expected) in [
        ("a:\n- x\n y\nb: z\n", &["x y"][..]),
        ("a:\n- x\n  y\nb: z\n", &["x y"]),
        ("a:\n  - x\n   y\nb: z\n", &["x y"]),
        ("a:\n- x\n- y\nb: z\n", &["x", "y"]),
    ] {
        let file = parse_clean(yaml);
        let mapping = root_mapping(&file);
        assert_eq!(keys(&mapping), ["a", "b"], "{yaml:?}");
        assert_eq!(items(&mapping.get("a").unwrap()), expected, "{yaml:?}");
    }
    // A `-` continues a scalar only where it cannot open the next entry
    // of the sequence the scalar sits in.
    for (yaml, expected) in [
        ("a:\n- x\n  - y\n", &["x - y"][..]),
        ("a:\n- x\n- y\n", &["x", "y"]),
        ("a:\n  - x\n  - y\n", &["x", "y"]),
        // `a:\n- x\n  - y\n  z\n` is the single item `x - y z`, as both
        // saphyr and PyYAML read it. The folded lines used to be separate
        // entries, and the `z` line was swept into an ERROR node.
        ("a:\n- x\n  - y\n  z\n", &["x - y z"]),
        ("a:\n- x\n  y\n", &["x y"]),
    ] {
        let file = parse_clean(yaml);
        assert_eq!(items(&root_mapping(&file).get("a").unwrap()), expected);
    }
    let file = parse_clean("k: v\n  -\n");
    assert_eq!(string(&root_mapping(&file).get("k").unwrap()), "v -");
}

/// A `?` past the scalar's own column cannot open an explicit key of the
/// mapping the scalar is a value of.
#[test]
fn question_mark_past_a_block_value_continues_it() {
    // `"k: v\n  ?\nk2: w\n"` is the value `v ?` and a second entry `k2`, as
    // saphyr and PyYAML both read it. The `?` used to invent an explicit-key
    // entry, and with a blank line in front the rest of the mapping was
    // stranded in an ERROR node.
    for (yaml, text, value, entries) in [
        ("k: v\n  ?\nk2: w\n", "v\n  ?", "v ?", 2),
        ("k: v\n\n  ?\nk2: w\n", "v\n\n  ?", "v\n?", 2),
        ("k: v\n  ?\n", "v\n  ?", "v ?", 1),
    ] {
        let mapping = root_mapping(&parse_clean(yaml));
        let k = mapping.get(ScalarValue::from("k")).unwrap();
        assert_eq!(k.to_string(), text, "{yaml:?}");
        assert_eq!(string(&k), value, "{yaml:?}");
        assert_eq!(mapping.len(), entries, "{yaml:?}");
    }

    let file = parse_clean("o:\n  k:\n    v\n     ?\n");
    let o = root_mapping(&file).get_mapping("o").unwrap();
    assert_eq!(string(&o.get("k").unwrap()), "v ?");

    let file = parse_clean("o:\n  k: !t\n    a\n   b\n");
    let o = root_mapping(&file).get_mapping("o").unwrap();
    let k = o.get("k").unwrap();
    let tagged = k.as_tagged().unwrap();
    assert_eq!(tagged.tag().as_deref(), Some("!t"));
    assert_eq!(tagged.value().unwrap().as_string(), "a b");
}

/// Inputs whose indicators are read in their real roles, or as content,
/// leaving no tokens stranded in an ERROR node.
#[test]
fn indicators_leave_nothing_stranded() {
    for yaml in [
        // A `?` mid-scalar is content; one that starts a node opens an
        // explicit key, in block and flow.
        "a ?,b\n",
        "a ?b\n",
        ")  .exp  ?,expect(\n",
        "? key\n: value\n",
        "keys: !!set\n  ? a\n  ? b\n",
        "map:\n  ? complex\n  : v\n",
        "a: {? k: v}\n",
        // Without a space after it, a `?` is content (652Z, 2EBW, FBC9).
        // `a: ?!!r{2}x` used to open an explicit key and read `!!r` as a
        // tag, so `{2}` closed a flow collection that was never opened.
        "a: ?!!r{2}x\n",
        "a: ?x\n",
        "?key: v\n",
        "es_ttpattern: ?!!r\\d{2}-\\d{2}'\n",
        "-{ [a]: v }\n",
        ".{ [a]: v }\n",
        "-{ [al b]: value1  value2 }\n",
        // Outside a flow collection the flow indicators are content, so a
        // `+` followed by one continues the scalar.
        "a+[b]\n",
        "a+{b}\n",
        "a: x+[1]\n",
        // A `:` not followed by whitespace is content after a `+` too.
        "a {b+:c}\n",
        "a +:b}\n",
        "call query-trends {kind+:\"TrendsQuery\"}\n",
        // Block scalar headers keep their indentation and chomping
        // indicators, the `-` or `+` standing alone.
        "key: >-\n  folded body\n",
        "k: |\n  body\n",
        "k: >\n  folded\n",
        "k: |-\n  x\n",
        "k: |+\n  x\n",
        "k: |2\n   x\n",
        "s:\n  - |\n    b\n",
        "k: |2+\n  x\n",
        "k: |+2\n  x\n",
        "b: |+\n  x\n",
        "k: >-\n  x\n",
        "- |+\n  x\n",
        "|+\nx\n",
        // A second entry after a key that ends in `|+`.
        "/|+:\n:",
        // Anchors and aliases still resolve, merge keys included.
        "a: &anc 1\nb: *anc\n",
        "defaults: &d\n  t: 30\nprod:\n  <<: *d\n  h: p\n",
        "- &x 1\n- *x\n",
        // A tag that starts a node is still a tag.
        "!!str v\n",
        "a: [!!str x]\n",
        "a: !!str x\nb: &anc y\nc: *anc\nd:\n- !!str z\n",
    ] {
        parse_clean(yaml);
    }
}

/// Tokens that keep their roles where they really are indicators.
#[test]
fn indicators_keep_their_roles() {
    for (yaml, tokens) in [
        // A flow indicator still ends a tag.
        ("a: [!t, x]\n", &["TAG: \"!t\""][..]),
        // `:` is an ns-tag-char in YAML 1.2, so `!!str:` is the whole tag
        // and the value is what follows it. PyYAML reads it the same way.
        ("!!str: x\n", &["TAG: \"!!str:\""]),
        // `!<uri>` runs to its closing `>` and may hold any URI character,
        // commas included.
        (
            "!<tag:yaml.org,2002:str> foo\n",
            &["TAG: \"!<tag:yaml.org,2002:str>\""],
        ),
        // An anchor or alias name runs to whitespace or a flow indicator,
        // as `ns-anchor-name` has it: `-`, `*` and `:` are ordinary name
        // characters. saphyr reads `&xT*U---` as the single anchor
        // `xT*U---`.
        ("&xT*U---", &["ANCHOR: \"&xT*U---\""]),
        ("&a---\n", &["ANCHOR: \"&a---\""]),
        ("&a:b\n", &["ANCHOR: \"&a:b\""]),
        ("[&a x, *a]\n", &["ANCHOR: \"&a\"", "REFERENCE: \"*a\""]),
        ("a: &anc 1\nb: *anc\n", &["REFERENCE"]),
        // A `%` opening a line is still a directive.
        ("%YAML 1.2\n---\na: 1\n", &["DIRECTIVE"]),
        // A `...` alone on its line is still the document-end marker.
        ("a\n...\n", &["DOC_END"]),
        ("--- a\n...\n--- b\n", &["DOC_END"]),
        ("a\n...\nb: 1\n", &["DOC_END"]),
        // A `#` after whitespace still starts a comment, and one glued to
        // a quoted scalar is still the 6.6 violation the validator reports.
        ("a: 1  # c\n", &["COMMENT"]),
        ("key: \"value\"# c\n", &["COMMENT"]),
    ] {
        let tree = debug::tree_to_string(parse_clean(yaml).syntax());
        for token in tokens {
            assert!(tree.contains(token), "{yaml:?}\n{tree}");
        }
    }
}

/// `ns-tag-char` is any URI character bar the flow indicators, so `:` and
/// `-` belong to the tag rather than ending it.
#[test]
fn tag_suffix_keeps_its_uri_characters() {
    for (yaml, tag) in [
        ("tags: !!ss:eq\n- a\nnext: x\n", "!!ss:eq"),
        ("tags: !!ss---\n- a\nnext: x\n", "!!ss---"),
        ("tags: !e:f\n- a\nnext: x\n", "!e:f"),
    ] {
        let file = parse_clean(yaml);
        let mapping = root_mapping(&file);
        assert_eq!(keys(&mapping), ["tags", "next"], "{yaml:?}");
        let tagged = mapping.get("tags").unwrap();
        assert_eq!(tagged.as_tagged().unwrap().tag().as_deref(), Some(tag));
    }
}

/// The mid-scalar rule leaves properties alone in the positions where
/// they are real: after a colon, a dash, a document start, or inside a
/// flow collection.
#[test]
fn node_property_at_a_node_start_still_applies() {
    let file = parse_clean("a: !!str x\nb: &anc y\nc: *anc\nd:\n- !!str z\n");
    let mapping = root_mapping(&file);
    let tagged = mapping.get("a").unwrap();
    assert_eq!(tagged.as_tagged().unwrap().tag().as_deref(), Some("!!str"));
    assert_eq!(mapping.get("c").unwrap().as_alias().unwrap().name(), "anc");
    assert_eq!(mapping.get_sequence("d").unwrap().len(), 1);

    let file = parse_clean("a: !!str v\n");
    let tagged = root_mapping(&file).get("a").unwrap();
    assert_eq!(tagged.as_tagged().unwrap().tag().as_deref(), Some("!!str"));
}

/// Inside a flow collection the indicators still delimit, and a flow
/// collection that is a mapping key still opens its own entry.
#[test]
fn flow_indicators_still_delimit() {
    for (yaml, key, len) in [
        ("a: [1, 2]\nb: {p: 1, q: 2}\n", "a", 2),
        ("k: [1, 2]\n", "k", 2),
        ("a: [x, y]\n", "a", 2),
    ] {
        let mapping = root_mapping(&parse_clean(yaml));
        assert_eq!(mapping.get_sequence(key).unwrap().len(), len, "{yaml:?}");
    }
    let mapping = root_mapping(&parse_clean("a: [1, 2]\nb: {p: 1, q: 2}\n"));
    assert_eq!(mapping.get_mapping("b").unwrap().keys().count(), 2);

    let mapping = root_mapping(&parse_clean("a: [::v, ::w]\n"));
    assert_eq!(items(&mapping.get("a").unwrap()), ["::v", "::w"]);

    for yaml in ["[a, b]: v1\n[c, d]: v2\n", "x: 1\n[c]: 2\n"] {
        let file = parse_clean(yaml);
        assert_eq!(root_mapping(&file).iter().count(), 2, "{yaml:?}");
    }
}

/// In a flow collection a `?` right before a flow indicator cannot start a
/// plain scalar, as `ns-plain-first` needs a safe character after it. It
/// is read as an explicit key with an empty key, as PyYAML reads it, and
/// the validator reports the missing space after it.
#[test]
fn question_mark_before_a_flow_indicator_opens_an_explicit_key() {
    for (yaml, spaced) in [
        ("{?, a}", "{? , a}"),
        ("{a, ?}", "{a, ? }"),
        ("{?}", "{? }"),
        ("[?]", "[? ]"),
        ("[?, a]", "[? , a]"),
        ("[a, ?]", "[a, ? ]"),
        ("{?[a]: b}", "{? [a]: b}"),
        ("{?{a: 1}: b}", "{? {a: 1}: b}"),
    ] {
        let question = yaml.find('?').unwrap();
        let mut offset = 0;
        let kind = yaml_edit::lex(yaml).into_iter().find_map(|(kind, text)| {
            let at = offset;
            offset += text.len();
            (at == question).then_some(kind)
        });
        assert_eq!(kind, Some(SyntaxKind::QUESTION), "{yaml:?}");

        let parsed = YamlFile::parse(yaml);
        assert_eq!(parsed.errors(), Vec::<String>::new(), "{yaml:?}");
        let file = parsed.tree();
        assert_eq!(file.to_string(), yaml);
        let value = |file: &YamlFile| YamlValue::from_document(&file.document().unwrap()).unwrap();
        assert_eq!(
            value(&file),
            value(&YamlFile::from_str(spaced).unwrap()),
            "{yaml:?}"
        );
        let violations: Vec<_> = Validator::new()
            .validate_syntax(file.syntax())
            .into_iter()
            .map(|v| (v.message, v.location.unwrap()))
            .collect();
        assert_eq!(
            violations,
            [(
                "Explicit key indicator '?' must be followed by white space".to_string(),
                format!("1:{}", question + 1)
            )],
            "{yaml:?}"
        );
    }
}

/// A plain scalar opening with an indicator is a spec violation that the
/// validator reports; the parser keeps the text as the scalar.
#[test]
fn plain_scalar_starting_with_an_indicator() {
    for (yaml, scalars) in [
        ("]}]}", vec!["]}]}"]),
        ("a: ,b\n", vec!["a", ",b"]),
        ("- ]\n", vec!["]"]),
        ("[a, - b]", vec!["a", "- b"]),
        ("[a, -]", vec!["a", "-"]),
    ] {
        let file = YamlFile::from_str(yaml).unwrap();
        assert_eq!(file.to_string(), yaml);
        assert_file_cst_ok(&file);
        let found: Vec<String> = file
            .syntax()
            .descendants()
            .filter_map(Scalar::cast)
            .map(|s| s.as_string())
            .collect();
        assert_eq!(found, scalars, "{yaml:?}");
    }
}

/// Text after a quoted scalar on its own line stays in the scalar's node,
/// for the validator to report, rather than dropping out of the document.
#[test]
fn quoted_scalar_keeps_trailing_text_on_its_line() {
    let yaml = "a: \"b\" c\nd: e\n";
    let parsed = YamlFile::parse(yaml);
    assert_eq!(parsed.errors(), Vec::<String>::new());
    let file = parsed.tree();
    assert_eq!(file.to_string(), yaml);
    let mapping = root_mapping(&file);
    assert_eq!(mapping.len(), 2);
    assert_eq!(string(&mapping.get("d").unwrap()), "e");
}

/// A line at the column of the mapping a plain scalar is in starts its
/// next entry rather than continuing the scalar, so a quote there opens a
/// quoted scalar, which here does not end. The lexer read the quote as
/// content, giving the plain keys `'b` and `'uantity` that the validator
/// accepted.
#[test]
fn quote_at_the_mapping_column_after_a_plain_scalar() {
    for (yaml, error) in [
        (
            "a: 1\n'b: 2\n",
            "2:1: Content after the document could not be parsed: \"'b: 2\\n\"",
        ),
        (
            "- item: x\n  'uantity: 1",
            "2:3: Content after the document could not be parsed: \"'uantity: 1\"",
        ),
    ] {
        let parsed = YamlFile::parse(yaml);
        assert_eq!(parsed.errors(), [error.to_string()], "{yaml:?}");
    }

    // Indented past the mapping's column the line continues the scalar.
    let doc = Document::from_str("- item: x\n   'uantity\n").unwrap();
    let item = doc.as_sequence().unwrap().get(0).unwrap();
    assert_eq!(
        string(&item.as_mapping().unwrap().get("item").unwrap()),
        "x 'uantity"
    );
}

/// A quote left open after a quoted scalar runs on to the end of the input,
/// as one opening a scalar does, and is reported the same way. It was kept
/// in the scalar with no error, which the validator accepted, so an entry
/// added after it was swallowed by the quote on a re-parse.
#[test]
fn unterminated_quote_after_a_quoted_scalar() {
    for (yaml, error) in [
        ("f: \"a\" \"\n", "1:8: Unterminated quoted string"),
        ("f: '\n\n  ' '\n", "3:5: Unterminated quoted string"),
        ("- 'a' 'b\n", "1:7: Unterminated quoted string"),
    ] {
        let parsed = YamlFile::parse(yaml);
        assert_eq!(parsed.errors(), [error.to_string()], "{yaml:?}");
    }
}

/// A `!`, `&` or `*` on a line continuing a plain scalar is content of that
/// scalar. Lexed as a tag, anchor or alias it took a `:` after it into its
/// name, so `{ a\n  !b: 1 }` had the key `a !b:` and no value.
#[test]
fn property_on_a_continuation_line_leaves_the_colon() {
    for (yaml, key) in [
        ("{ a\n  !b: 1 }\n", "a !b"),
        ("{ a\n  &b: 1 }\n", "a &b"),
        ("{ a\n  *b: 1 }\n", "a *b"),
        ("{ f\"foo\"\n  !bar: 1 }\n", "f\"foo\" !bar"),
    ] {
        let doc = Document::from_str(yaml).unwrap();
        let mapping = doc.as_mapping().unwrap();
        let entries: Vec<_> = mapping.iter().collect();
        assert_eq!(entries.len(), 1, "{yaml:?}");
        assert_eq!(string(&entries[0].0), key);
        assert_eq!(string(&entries[0].1), "1");
    }

    // Setting the value of such a key writes text that reads back the same.
    let doc = Document::from_str("{ f\"foo\"\n  !bar }\n").unwrap();
    doc.as_mapping().unwrap().set("f\"foo\" !bar", 1);
    assert_eq!(doc.to_string(), "{ f\"foo\"\n  !bar: 1 }\n");
    assert_parser_shaped(&doc);
}

/// A quote opening a line that continues a plain scalar is content, even
/// when it closes on that line. Whether it did decided whether it was, so
/// adding an entry after `"c` in a flow mapping, which put a closing quote
/// on its line, made the text read back differently.
#[test]
fn quote_on_a_flow_continuation_line_is_content() {
    let doc = Document::from_str("[b\n \"x\", c]\n").unwrap();
    let items: Vec<_> = doc
        .as_sequence()
        .unwrap()
        .values()
        .map(|v| string(&v))
        .collect();
    assert_eq!(items, ["b \"x\"", "c"]);

    let doc = Document::from_str("{a: b\n \"c\n}\n").unwrap();
    let mapping = doc.as_mapping().unwrap();
    assert_eq!(string(&mapping.get("a").unwrap()), "b \"c");
    mapping.set("k\r", 1);
    assert_eq!(doc.to_string(), "{a: b\n \"c, \"k\\r\": 1\n}\n");
    assert_parser_shaped(&doc);
}

/// An adjacent `:value` only follows a JSON-like key. After a plain scalar
/// on the line before, the `:b` continues it, as it does on one line.
#[test]
fn flow_colon_after_plain_scalar_on_next_line_is_content() {
    let doc = Document::from_str("[a\n :b]\n").unwrap();
    let items: Vec<_> = doc
        .as_sequence()
        .unwrap()
        .values()
        .map(|v| string(&v))
        .collect();
    assert_eq!(items, ["a :b"]);

    // After a quoted key it still separates the value.
    let doc = Document::from_str("{ \"foo\"\n  :bar }\n").unwrap();
    assert_eq!(
        string(&doc.as_mapping().unwrap().get("foo").unwrap()),
        "bar"
    );
}

/// Indicators on a line that continues a plain scalar are content of that
/// scalar: lexed as indicators they opened a flow collection that took the
/// `---` after it into the scalar, or a block scalar or quoted scalar that
/// took in the lines after it.
#[test]
fn indicators_on_continuation_lines() {
    let plain = |s: &str| YamlValue::Scalar(ScalarValue::plain(s));
    let null = || YamlValue::Scalar(ScalarValue::null());
    let mapping = |entries: Vec<(&str, YamlValue)>| {
        YamlValue::from_mapping(
            entries
                .into_iter()
                .map(|(k, v)| (k.to_string(), v))
                .collect(),
        )
    };
    for (yaml, expected) in [
        // An opening flow indicator.
        ("f\n[\n---\n", vec![YamlValue::from("f ["), null()]),
        ("f\n{\n---\n", vec![YamlValue::from("f {"), null()]),
        (
            "a: b\n  [x\nc: 1\n",
            vec![mapping(vec![
                ("a", YamlValue::from("b [x")),
                ("c", YamlValue::from(1i64)),
            ])],
        ),
        // A closing one, and an opening one after it.
        ("m\n]{\n---\n", vec![YamlValue::from("m ]{"), null()]),
        ("m\n} [\n---\n", vec![YamlValue::from("m } ["), null()]),
        (
            "- m\n  }[\n---\n",
            vec![
                YamlValue::from_sequence(vec![YamlValue::from("m }[")]),
                null(),
            ],
        ),
        // A collection closing on the line only reads as one when it is an
        // implicit key: `[]` followed by more content, or `{` closed by
        // `]`, is content, and so is the `[` or quote after it.
        ("2\n[][\n---\n", vec![YamlValue::from("2 []["), null()]),
        ("$\n{]{\n---\n", vec![YamlValue::from("$ {]{"), null()]),
        ("e\n[]' # c\n", vec![plain("e []'")]),
        // A `-` or `?` opening the line, and a `[` after it.
        ("e\n- [\n---\n", vec![YamlValue::from("e - ["), null()]),
        ("n\n-\n{\n---\n", vec![YamlValue::from("n - {"), null()]),
        ("j\n? [\n---\n", vec![YamlValue::from("j ? ["), null()]),
        // A `|` or `>` is no block scalar header there, so the lines after
        // it are not a block scalar body.
        ("1\n|x\n  # t\n", vec![plain("1 |x")]),
        ("1\n|+\n", vec![plain("1 |+")]),
        ("a\n>\n  b\n", vec![plain("a > b")]),
        (
            "a: b\n  |\n  # c\nd: 1\n",
            vec![mapping(vec![
                ("a", plain("b |")),
                ("d", YamlValue::from(1i64)),
            ])],
        ),
        // A document after `...` starts at the root like any other, so the
        // quote continues the scalar `z` rather than opening a quoted
        // scalar that took in the comment.
        ("...\nz\n\"\n# c\n", vec![plain("z \"")]),
        // `<<` is a merge key only as a mapping key; anywhere else it is a
        // plain scalar like any other, which the next line continues and a
        // comment line ends.
        ("<<\n", vec![plain("<<")]),
        ("<<", vec![plain("<<")]),
        ("- <<\n", vec![YamlValue::Sequence(vec![plain("<<")])]),
        ("a\n<<\n", vec![plain("a <<")]),
        ("<<\n  a\n", vec![plain("<< a")]),
        ("<<\n|\n# c\n", vec![plain("<< |")]),
        // In a flow plain scalar a quote is content, not the end of a
        // JSON-like key, so the `:` after it needs a space to be an
        // indicator.
        (
            "[p\":a]\n",
            vec![YamlValue::from_sequence(vec![plain("p\":a")])],
        ),
        (
            "[p':a]\n",
            vec![YamlValue::from_sequence(vec![plain("p':a")])],
        ),
    ] {
        assert_eq!(common::document_values(yaml), expected, "{yaml:?}");
    }
    // A flow collection opening a line is still an implicit key.
    let file = YamlFile::from_str("a: b\n[x]: y\n").unwrap();
    assert_eq!(root_mapping(&file).len(), 2);
}

/// `:o` starts a plain scalar, which runs on past the space, so the `[` in
/// it is content and the key is `:o [d`.
#[test]
fn plain_scalar_starting_with_colon_spans_words() {
    assert_eq!(
        common::document_values(":o [d: f\n"),
        [YamlValue::from_mapping(
            [(
                ":o [d".to_string(),
                YamlValue::Scalar(ScalarValue::plain("f"))
            )]
            .into()
        )]
    );
    let file = YamlFile::from_str(":o p: f\n").unwrap();
    assert_eq!(
        debug::tree_to_string(file.syntax()),
        concat!(
            "DOCUMENT\n",
            "  MAPPING\n",
            "    MAPPING_ENTRY\n",
            "      KEY\n",
            "        SCALAR\n",
            "          STRING: \":o p\"\n",
            "      COLON: \":\"\n",
            "      WHITESPACE: \" \"\n",
            "      VALUE\n",
            "        SCALAR\n",
            "          STRING: \"f\"\n",
            "      NEWLINE: \"\\\\n\"\n",
        )
    );
}
