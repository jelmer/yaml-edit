//! Property test: the text `YamlValue::to_yaml_string` writes reads back
//! as the same value.

use proptest::prelude::*;
use std::collections::{BTreeMap, BTreeSet};
use yaml_edit::{ScalarValue, YamlValue};

// TODO: add `|` and `>` once a block scalar line such as `>'` no longer
// makes the parser read the lines after it into the scalar.
fn string_strat() -> impl Strategy<Value = String> {
    "[a-z0-9\t%`?~. :#'\"\\-\n{}\\[\\],&*!@]{0,8}"
}

fn scalar_strat() -> impl Strategy<Value = YamlValue> {
    prop_oneof![
        string_strat().prop_map(YamlValue::from),
        any::<i64>().prop_map(YamlValue::from),
        any::<bool>().prop_map(YamlValue::from),
        Just(YamlValue::Scalar(ScalarValue::null())),
        string_strat().prop_map(|s| YamlValue::Scalar(ScalarValue::string(s).with_tag("!t"))),
    ]
}

fn value_strat() -> impl Strategy<Value = YamlValue> {
    scalar_strat().prop_recursive(3, 24, 4, |inner| {
        let pairs = prop::collection::vec((string_strat(), inner.clone()), 0..4);
        prop_oneof![
            prop::collection::vec(inner.clone(), 0..4).prop_map(YamlValue::Sequence),
            prop::collection::btree_map(string_strat(), inner.clone(), 0..4)
                .prop_map(YamlValue::Mapping),
            prop::collection::btree_set(string_strat(), 0..4).prop_map(YamlValue::Set),
            // An !!omap has distinct keys.
            prop::collection::btree_map(string_strat(), inner.clone(), 0..4)
                .prop_map(|m: BTreeMap<_, _>| YamlValue::OrderedMapping(m.into_iter().collect())),
            pairs.prop_map(YamlValue::Pairs),
            prop_oneof![
                prop::collection::vec(inner.clone(), 0..4).prop_map(YamlValue::Sequence),
                prop::collection::btree_map(string_strat(), inner, 0..4)
                    .prop_map(YamlValue::Mapping),
            ]
            .prop_map(|v| YamlValue::tagged("!t", v)),
        ]
    })
}

/// Read back `text`, written by `to_yaml_string(indent)`: a block
/// collection on its own, anything else as the value of a key at column
/// `indent`.
fn read_back(value: &YamlValue, text: &str, indent: usize) -> YamlValue {
    let block = match value {
        YamlValue::Sequence(items) => !items.is_empty(),
        YamlValue::Mapping(entries) => !entries.is_empty(),
        _ => false,
    };
    if block || indent == 0 {
        return YamlValue::parse_raw(text).unwrap();
    }
    let doc = format!("{}k: {}", " ".repeat(indent), text);
    let read = YamlValue::parse_raw(&doc).unwrap();
    read.as_mapping().unwrap()["k"].clone()
}

proptest! {
    #[test]
    fn to_yaml_string_reads_back(value in value_strat(), indent in 0usize..4) {
        let text = value.to_yaml_string(indent);
        prop_assert_eq!(read_back(&value, &text, indent), value.clone(), "text: {:?}", text);
        if indent == 0 {
            prop_assert_eq!(value.to_string(), text);
        }
    }
}

#[test]
fn empty_set_reads_back() {
    let value = YamlValue::Set(BTreeSet::new());
    assert_eq!(value.to_yaml_string(0), "!!set {}");
    assert_eq!(YamlValue::parse_raw("!!set {}").unwrap(), value);
}
