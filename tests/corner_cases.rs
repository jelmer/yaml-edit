//! Corner cases for plain scalar with internal whitespace (issue #30).
//! This test file is run against both the parser-fix and lexer-fix branches.

use std::str::FromStr;

use rowan::ast::AstNode;
use yaml_edit::{Mapping, YamlFile};

fn keys_of(mapping: &Mapping) -> Vec<String> {
    mapping
        .iter()
        .map(|(k, _)| k.as_scalar().map(|s| s.as_string()).unwrap_or_default())
        .collect()
}

fn get_scalar(mapping: &Mapping, key: &str) -> Option<String> {
    mapping
        .get(key)
        .and_then(|n| n.as_scalar().cloned())
        .map(|s| s.as_string())
}

fn parse_ok(yaml: &str) -> YamlFile {
    let parsed = YamlFile::parse(yaml);
    let errors = parsed.errors();
    assert!(
        errors.is_empty(),
        "expected no errors for {:?}, got: {:?}",
        yaml,
        errors
    );
    parsed.tree()
}

fn assert_roundtrip(yaml: &str) {
    let tree = parse_ok(yaml);
    assert_eq!(
        tree.syntax().text().to_string(),
        yaml,
        "roundtrip mismatch for {:?}",
        yaml
    );
}

fn top_mapping(yaml: &str) -> Mapping {
    parse_ok(yaml).document().unwrap().as_mapping().unwrap()
}

// ---------- A. Basic multi-word keys ----------

#[test]
fn a1_basic_two_word_key() {
    let yaml = "abc cba: value\n";
    assert_roundtrip(yaml);
    let mapping = top_mapping(yaml);
    assert_eq!(keys_of(&mapping), vec!["abc cba"]);
    assert_eq!(get_scalar(&mapping, "abc cba"), Some("value".into()));
}

#[test]
fn a2_many_word_key() {
    let yaml = "a b c d: value\n";
    assert_roundtrip(yaml);
    assert_eq!(keys_of(&top_mapping(yaml)), vec!["a b c d"]);
}

#[test]
fn a3_multiple_spaces_in_key() {
    let yaml = "abc   cba: value\n";
    assert_roundtrip(yaml);
    assert_eq!(keys_of(&top_mapping(yaml)), vec!["abc   cba"]);
}

#[test]
fn a4_tab_in_key() {
    let yaml = "abc\tcba: value\n";
    assert_roundtrip(yaml);
    assert_eq!(keys_of(&top_mapping(yaml)), vec!["abc\tcba"]);
}

#[test]
fn a5_space_before_colon() {
    let yaml = "abc cba : value\n";
    assert_roundtrip(yaml);
    // The key itself doesn't include trailing whitespace.
    assert_eq!(keys_of(&top_mapping(yaml)), vec!["abc cba"]);
}

#[test]
fn a6_multi_word_scalar_no_colon() {
    let yaml = "abc cba\n";
    let tree = parse_ok(yaml);
    let scalar = tree
        .document()
        .unwrap()
        .as_scalar()
        .expect("should be scalar, not mapping");
    assert_eq!(scalar.as_string(), "abc cba");
}

// ---------- B. Multi-word values ----------

#[test]
fn b1_value_with_spaces() {
    let yaml = "key: abc cba\n";
    assert_roundtrip(yaml);
    assert_eq!(
        get_scalar(&top_mapping(yaml), "key"),
        Some("abc cba".into())
    );
}

#[test]
fn b2_value_with_spaces_then_key() {
    let yaml = "key: abc cba\nkey2: value\n";
    assert_roundtrip(yaml);
    let mapping = top_mapping(yaml);
    assert_eq!(get_scalar(&mapping, "key"), Some("abc cba".into()));
    assert_eq!(get_scalar(&mapping, "key2"), Some("value".into()));
}

// ---------- C. Comments ----------

#[test]
fn c1_comment_after_value() {
    let yaml = "key: value # comment\n";
    assert_roundtrip(yaml);
    assert_eq!(get_scalar(&top_mapping(yaml), "key"), Some("value".into()));
}

#[test]
fn c2_comment_after_multi_word_key_value() {
    let yaml = "abc cba: value # comment\n";
    assert_roundtrip(yaml);
    assert_eq!(
        get_scalar(&top_mapping(yaml), "abc cba"),
        Some("value".into())
    );
}

#[test]
fn c3_comment_after_multi_word_value() {
    let yaml = "key: abc cba # comment\n";
    assert_roundtrip(yaml);
    assert_eq!(
        get_scalar(&top_mapping(yaml), "key"),
        Some("abc cba".into())
    );
}

// NOTE: `#` mid-word (e.g. `key: abc#def`) is currently mishandled by the
// lexer, which unconditionally treats `#` as a comment start. This is a
// pre-existing limitation independent of the multi-word scalar fix.

// ---------- D. Colons in keys ----------

#[test]
fn d1_url_key_with_colon() {
    let yaml = "http://example.com: value\n";
    assert_roundtrip(yaml);
    assert_eq!(
        get_scalar(&top_mapping(yaml), "http://example.com"),
        Some("value".into())
    );
}

// ---------- E. Sequences ----------

#[test]
fn e1_sequence_item_multi_word() {
    let yaml = "- abc cba\n- foo\n";
    assert_roundtrip(yaml);
    let tree = parse_ok(yaml);
    let seq = tree.document().unwrap().as_sequence().unwrap();
    assert_eq!(seq.len(), 2);
    let s0 = seq.get(0).unwrap();
    assert_eq!(s0.as_scalar().unwrap().as_string(), "abc cba");
    let s1 = seq.get(1).unwrap();
    assert_eq!(s1.as_scalar().unwrap().as_string(), "foo");
}

#[test]
fn e2_sequence_of_multi_word_mapping() {
    let yaml = "- abc cba: value\n- foo: bar\n";
    assert_roundtrip(yaml);
    let tree = parse_ok(yaml);
    let seq = tree.document().unwrap().as_sequence().unwrap();
    assert_eq!(seq.len(), 2);
    let n0 = seq.get(0).unwrap();
    let m0 = n0.as_mapping().unwrap();
    assert_eq!(get_scalar(m0, "abc cba"), Some("value".into()));
    let n1 = seq.get(1).unwrap();
    let m1 = n1.as_mapping().unwrap();
    assert_eq!(get_scalar(m1, "foo"), Some("bar".into()));
}

#[test]
fn e3_nested_multi_word_key() {
    let yaml = "outer:\n  abc cba: 2\n  foo: 3\n";
    assert_roundtrip(yaml);
    let outer = top_mapping(yaml);
    let inner_node = outer.get("outer").unwrap();
    let inner = inner_node.as_mapping().unwrap();
    assert_eq!(get_scalar(inner, "abc cba"), Some("2".into()));
    assert_eq!(get_scalar(inner, "foo"), Some("3".into()));
}

// ---------- F. Flow context ----------

#[test]
fn f1_flow_mapping_multi_word_key() {
    let yaml = "{abc cba: value}";
    assert_eq!(
        get_scalar(&top_mapping(yaml), "abc cba"),
        Some("value".into())
    );
}

#[test]
fn f2_flow_sequence_multi_word_items() {
    let yaml = "[abc cba, def ghi]";
    let tree = parse_ok(yaml);
    let seq = tree.document().unwrap().as_sequence().unwrap();
    assert_eq!(seq.len(), 2);
    let s0 = seq.get(0).unwrap();
    assert_eq!(s0.as_scalar().unwrap().as_string(), "abc cba");
    let s1 = seq.get(1).unwrap();
    assert_eq!(s1.as_scalar().unwrap().as_string(), "def ghi");
}

#[test]
fn f3_flow_mapping_multi_word_value() {
    let yaml = "{a: b c d}";
    assert_eq!(get_scalar(&top_mapping(yaml), "a"), Some("b c d".into()));
}

// ---------- G. Special characters mid-scalar ----------

#[test]
fn g1_hyphen_at_word_end_in_key() {
    // "abc-" is a single word, `cba` follows after space; per YAML they merge.
    let yaml = "abc- cba: value\n";
    assert_eq!(
        get_scalar(&top_mapping(yaml), "abc- cba"),
        Some("value".into())
    );
}

#[test]
fn g2_numbers_as_words_in_key() {
    let yaml = "abc 123: value\n";
    assert_roundtrip(yaml);
    assert_eq!(
        get_scalar(&top_mapping(yaml), "abc 123"),
        Some("value".into())
    );
}

#[test]
fn g3_bool_words_as_key() {
    let yaml = "true false: value\n";
    assert_roundtrip(yaml);
    assert_eq!(
        get_scalar(&top_mapping(yaml), "true false"),
        Some("value".into())
    );
}

// ---------- H. Multi-line ----------

#[test]
fn h1_multi_line_plain_scalar_value() {
    // Plain scalar continuation across lines (folded to space).
    let yaml = "key: abc cba\n  more text\n";
    let mapping = top_mapping(yaml);
    // Per YAML, multi-line plain scalars fold newlines to spaces.
    assert_eq!(
        get_scalar(&mapping, "key"),
        Some("abc cba more text".into())
    );
}

// ---------- I. Round-trip stability ----------

#[test]
fn i1_roundtrip_issue_30_original() {
    assert_roundtrip("xyz: 1\nabc cba: 2\nfoo: 3\n");
}

#[test]
fn i2_roundtrip_with_indent_and_comments() {
    let yaml = "# top comment\nouter:\n  # inner comment\n  abc cba: 1\n  foo: 2\n";
    assert_roundtrip(yaml);
}

// ---------- J. Setter round-trip ----------

#[test]
fn j1_read_then_serialize() {
    let yaml = "abc cba: original\n";
    let f = YamlFile::from_str(yaml).unwrap();
    let output = f.to_string();
    assert_eq!(output, yaml);
}
