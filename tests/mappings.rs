//! Reading a mapping: lookups, its keys and values, iteration and length.

mod common;
use common::{assert_cst_ok, edit_root, parse_clean, render};
use std::str::FromStr;
use yaml_edit::{YamlFile, YamlNode};

#[test]
fn dump_cst_debug_and_default() {
    let file = YamlFile::from_str("a: 1\n").unwrap();
    let doc = file.document().unwrap();
    let mapping = doc.as_mapping().unwrap();
    assert_eq!(
        mapping.dump_cst(),
        "MAPPING_ENTRY\n  KEY\n    SCALAR\n        STRING \"a\"\n    COLON \":\"\n    WHITESPACE \" \"\n  VALUE\n    SCALAR\n        INT \"1\"\n    NEWLINE \"\\n\"\n"
    );
    assert_eq!(format!("{mapping:?}"), "Mapping { syntax: MAPPING@0..5 }");
    assert_eq!(yaml_edit::Mapping::default().to_string(), "{}");
    assert_eq!(yaml_edit::Sequence::default().to_string(), "[]");
}

/// The entries of each mapping as rendered (key, value) pairs.
#[test]
fn entries_keys_values_and_len() {
    for (yaml, expected) in [
        ("{}", &[][..]),
        ("key: value", &[("key", "value")]),
        ("name: Alice", &[("name", "Alice")]),
        ("name: Alice\nage: 30", &[("name", "Alice"), ("age", "30")]),
        (
            "name: Alice\nage: 30\ncity: NYC",
            &[("name", "Alice"), ("age", "30"), ("city", "NYC")],
        ),
        (
            "name: Alice\nage: 30\ncity: Boston",
            &[("name", "Alice"), ("age", "30"), ("city", "Boston")],
        ),
        (
            "name: Alice\nage: 30\nactive: true",
            &[("name", "Alice"), ("age", "30"), ("active", "true")],
        ),
        (
            "string: hello\nnumber: 42\nbool: false",
            &[("string", "hello"), ("number", "42"), ("bool", "false")],
        ),
        (
            "string: hello\nnumber: 42\nbool: true",
            &[("string", "hello"), ("number", "42"), ("bool", "true")],
        ),
        ("z: 1\na: 2\nm: 3", &[("z", "1"), ("a", "2"), ("m", "3")]),
        ("a: 1\nb: 2\nc: 3", &[("a", "1"), ("b", "2"), ("c", "3")]),
        (
            "a: 1\nb: 2\nc: 3\nd: 4",
            &[("a", "1"), ("b", "2"), ("c", "3"), ("d", "4")],
        ),
    ] {
        let file = parse_clean(yaml);
        let mapping = file.document().unwrap().as_mapping().unwrap();
        let rendered = |pairs: Vec<(YamlNode, YamlNode)>| -> Vec<(String, String)> {
            pairs.iter().map(|(k, v)| (render(k), render(v))).collect()
        };
        let expected: Vec<(String, String)> = expected
            .iter()
            .map(|(k, v)| (k.to_string(), v.to_string()))
            .collect();
        assert_eq!(rendered(mapping.iter().collect()), expected, "{yaml:?}");
        assert_eq!(
            rendered((&mapping).into_iter().collect()),
            expected,
            "{yaml:?}"
        );
        let keys: Vec<String> = mapping.keys().map(|k| k.to_string()).collect();
        let values: Vec<String> = mapping.values().map(|v| render(&v)).collect();
        assert_eq!((keys, values), expected.iter().cloned().unzip(), "{yaml:?}");
        assert_eq!(mapping.len(), expected.len(), "{yaml:?}");
        assert_eq!(mapping.is_empty(), expected.is_empty(), "{yaml:?}");
        for (key, value) in &expected {
            assert!(mapping.contains_key(key.as_str()), "{yaml:?}");
            assert_eq!(
                mapping.get(key.as_str()).map(|v| render(&v)),
                Some(value.clone())
            );
        }
        assert!(!mapping.contains_key("missing"), "{yaml:?}");
        assert_eq!(mapping.get("missing"), None, "{yaml:?}");
    }
    assert_eq!((&yaml_edit::Mapping::new()).into_iter().count(), 0);
}

/// Values read with their core schema types, through `get`, `values`
/// and `iter`.
#[test]
fn typed_values() {
    for (yaml, int_key, int, bool_key, boolean) in [
        (
            "name: Alice\nage: 30\nactive: true",
            "age",
            30,
            "active",
            true,
        ),
        (
            "string: hello\nnumber: 42\nbool: false",
            "number",
            42,
            "bool",
            false,
        ),
        (
            "string: hello\nnumber: 42\nbool: true",
            "number",
            42,
            "bool",
            true,
        ),
    ] {
        let file = YamlFile::from_str(yaml).unwrap();
        let mapping = file.document().unwrap().as_mapping().unwrap();
        assert_eq!(mapping.get(int_key).and_then(|v| v.to_i64()), Some(int));
        assert_eq!(
            mapping.get(bool_key).and_then(|v| v.to_bool()),
            Some(boolean)
        );
        let values: Vec<_> = mapping.values().collect();
        assert_eq!(values[1].to_i64(), Some(int));
        assert_eq!(values[2].to_bool(), Some(boolean));
        let pairs: Vec<_> = mapping.iter().collect();
        assert_eq!(pairs[1].1.to_i64(), Some(int));
        assert_eq!(pairs[2].1.to_bool(), Some(boolean));
    }
    let file = YamlFile::from_str("name: Alice\nage: 30").unwrap();
    let mapping = file.document().unwrap().as_mapping().unwrap();
    assert_eq!(mapping.get("age").and_then(|v| v.to_i64()), Some(30));

    let file = YamlFile::from_str("a: 1\nb: 2\nc: 3\nd: 4").unwrap();
    let mapping = file.document().unwrap().as_mapping().unwrap();
    let even_count = (&mapping)
        .into_iter()
        .filter(|(_, value)| value.to_i64().is_some_and(|n| n % 2 == 0))
        .count();
    assert_eq!(even_count, 2);
}

#[test]
fn nested_collections() {
    let file = parse_clean("user:\n  name: Alice\n  age: 30");
    let mapping = file.document().unwrap().as_mapping().unwrap();
    let user = mapping.get_mapping("user").unwrap();
    assert_eq!(
        render(&YamlNode::Mapping(user.clone())),
        "map{name: Alice, age: 30}"
    );
    assert_eq!(user.get("age").and_then(|v| v.to_i64()), Some(30));

    let file = parse_clean("items:\n  - a\n  - b\n  - c");
    let mapping = file.document().unwrap().as_mapping().unwrap();
    let items = mapping.get_sequence("items").unwrap();
    assert_eq!(render(&YamlNode::Sequence(items)), "seq[a, b, c]");

    let file = parse_clean("server:\n  host: localhost\n  port: 8080");
    let mapping = file.document().unwrap().as_mapping().unwrap();
    assert_eq!(
        (&mapping.get_mapping("server").unwrap())
            .into_iter()
            .count(),
        2
    );

    let file = parse_clean("name: Alice");
    let mapping = file.document().unwrap().as_mapping().unwrap();
    assert_eq!(mapping.get_mapping("user"), None);
    assert_eq!(mapping.get_sequence("items"), None);
}

#[test]
fn edits_to_empty_mapping() {
    let text = edit_root("{}", |m| {
        assert!(m.is_empty());
        assert!(!m.contains_key("any"));
        assert_eq!(m.get("any"), None);
        assert!(m.remove("any").is_none());
        assert_cst_ok(m);
        assert!(!m.rename_key("old", "new"));
        assert_cst_ok(m);
        m.set("first", "value");
        assert!(!m.is_empty());
        assert_eq!(
            m.get("first").map(|v| v.to_string()),
            Some("value".to_string())
        );
    });
    assert_eq!(text, "{first: value}");
}

/// A mapping keeps a final line break, or its absence, and a flow mapping
/// stays as written.
#[test]
fn line_breaks_round_trip() {
    for yaml in [
        "key1: value1\nkey2: value2\n",
        "key: value",
        "data: {key1: value1, key2: value2}",
    ] {
        parse_clean(yaml);
    }
}
