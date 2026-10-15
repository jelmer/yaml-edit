//! Strings written through `Mapping::set` must read back as the same string.

use std::str::FromStr;
use yaml_edit::{ScalarType, ScalarValue, YamlFile};

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
fn test_special_float_strings_are_quoted() {
    for s in [
        ".inf", "+.inf", "-.inf", ".Inf", "-.INF", ".nan", ".NaN", ".NAN",
    ] {
        assert_eq!(
            set_and_reparse(s),
            (format!("k: '{s}'\n"), s.to_string(), ScalarType::String),
            "{s}"
        );
    }
}

#[test]
fn test_control_characters_are_escaped() {
    assert_eq!(
        set_and_reparse("\u{7}bell"),
        (
            "k: \"\\abell\"\n".to_string(),
            "\u{7}bell".to_string(),
            ScalarType::String
        )
    );
    assert_eq!(
        set_and_reparse("\u{0}"),
        (
            "k: \"\\0\"\n".to_string(),
            "\u{0}".to_string(),
            ScalarType::String
        )
    );
    assert_eq!(
        set_and_reparse("\x01"),
        (
            "k: \"\\x01\"\n".to_string(),
            "\x01".to_string(),
            ScalarType::String
        )
    );
}

#[test]
fn test_carriage_return_is_preserved() {
    assert_eq!(
        set_and_reparse("a\rb"),
        (
            "k: \"a\\rb\"\n".to_string(),
            "a\rb".to_string(),
            ScalarType::String
        )
    );
    assert_eq!(
        set_and_reparse("a\r\nb"),
        (
            "k: \"a\\r\\nb\"\n".to_string(),
            "a\r\nb".to_string(),
            ScalarType::String
        )
    );
}
