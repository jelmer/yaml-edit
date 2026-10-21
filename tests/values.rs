//! Plain values: reading documents into `YamlValue`, writing values and
//! other `AsYaml` implementations back as YAML, and comparing nodes by
//! their meaning with `yaml_eq` and `yaml_hash`.

mod common;
use common::{assert_cst_ok, assert_parser_shaped, check_edit, value_of};
use rowan::ast::AstNode;
use std::collections::{BTreeMap, BTreeSet};
use std::hash::Hasher;
use std::str::FromStr;
use yaml_edit::{
    yaml_eq, yaml_hash, AsYaml, Document, DocumentResolvedExt, Lang, ResolveError, Scalar,
    ScalarStyle, ScalarType, ScalarValue, SyntaxKind, TextPosition, YamlError, YamlFile, YamlNode,
    YamlValue,
};

fn render_as_yaml<T: AsYaml>(v: &T) -> String {
    let mut builder = rowan::GreenNodeBuilder::new();
    v.build_content(&mut builder, 0, false);
    let green = builder.finish();
    let node = rowan::SyntaxNode::<Lang>::new_root(green);
    node.text().to_string()
}

fn scalar_token_kind(v: &YamlValue) -> SyntaxKind {
    let mut builder = rowan::GreenNodeBuilder::new();
    AsYaml::build_content(v, &mut builder, 0, false);
    let green = builder.finish();
    let node = rowan::SyntaxNode::<Lang>::new_root(green);
    let scalar = Scalar::cast(node).expect("scalar node");
    scalar
        .syntax()
        .children_with_tokens()
        .filter_map(|c| c.into_token())
        .next()
        .expect("token")
        .kind()
}

#[test]
fn scalar_build_content_quotes_like_scalar_value() {
    // YamlValue::Scalar delegates to ScalarValue so that strings needing
    // quotes get them. Emitting the raw value would re-parse as a
    // different type, or (for "a: b") as a different structure.
    for raw in ["null", "true", "42", "a: b", "", "#c", "- x"] {
        let sv = ScalarValue::string(raw);
        assert_eq!(
            render_as_yaml(&YamlValue::Scalar(sv.clone())),
            render_as_yaml(&sv),
            "YamlValue::Scalar({raw:?}) must render like the ScalarValue"
        );
    }
}

#[test]
fn as_yaml_build_content_tagged_and_nested() {
    // Tagged collections render the tag, a newline, then content indented by
    // two spaces (indent + 2). A wrong arithmetic op would drop the indent.
    let omap = YamlValue::from_ordered_mapping(vec![
        ("a".to_string(), YamlValue::from("1")),
        ("b".to_string(), YamlValue::from("2")),
    ]);
    assert_eq!(render_as_yaml(&omap), "!!omap\n  - a: '1'\n  - b: '2'\n");

    let pairs = YamlValue::from_pairs(vec![("a".to_string(), YamlValue::from("1"))]);
    assert_eq!(render_as_yaml(&pairs), "!!pairs\n  - a: '1'\n");

    let set = YamlValue::from_set(BTreeSet::from(["a".to_string(), "b".to_string()]));
    assert_eq!(render_as_yaml(&set), "!!set\n  a: null\n  b: null\n");

    // A mapping whose value is a multi-entry mapping. The inner mapping's
    // own entry separators are indented by the indent passed down
    // (BTreeMap::build_content indent + 2), so a wrong op shows up on the
    // second inner key.
    let mut inner = BTreeMap::new();
    inner.insert("x".to_string(), YamlValue::from("1"));
    inner.insert("y".to_string(), YamlValue::from("2"));
    let mut outer = BTreeMap::new();
    outer.insert("o".to_string(), YamlValue::Mapping(inner));
    assert_eq!(render_as_yaml(&outer), "o:\n  x: '1'\n  y: '2'\n");

    // Pairs with two entries: the indent passed into the nested sequence
    // shows on the second item's "  - " separator (Pairs branch indent + 2).
    let pairs2 = YamlValue::from_pairs(vec![
        ("a".to_string(), YamlValue::from("1")),
        ("b".to_string(), YamlValue::from("2")),
    ]);
    assert_eq!(render_as_yaml(&pairs2), "!!pairs\n  - a: '1'\n  - b: '2'\n");

    // A sequence nested inside a sequence (&[]::build_content indent + 2).
    let seq = vec![YamlValue::Sequence(vec![YamlValue::from("a")])];
    assert_eq!(render_as_yaml(&seq), "- - a\n");

    // A sequence whose single item is a multi-entry mapping: the indent
    // handed to that item (&[]::build_content indent + 2) shows on the
    // second mapping key.
    let mut item = BTreeMap::new();
    item.insert("x".to_string(), YamlValue::from("1"));
    item.insert("y".to_string(), YamlValue::from("2"));
    let seq_of_map = vec![YamlValue::Mapping(item)];
    assert_eq!(render_as_yaml(&seq_of_map), "- x: '1'\n  y: '2'\n");
}

#[test]
fn block_mapping_multiline_key() {
    // A multi-line key is written double-quoted: a block scalar cannot be
    // a key.
    let mut map = BTreeMap::new();
    map.insert("a\nb".to_string(), YamlValue::from("1"));
    map.insert("c".to_string(), YamlValue::from("2"));
    let text = render_as_yaml(&map);
    assert_eq!(text, "\"a\\nb\": '1'\nc: '2'\n");

    let parsed: YamlFile = text.parse().unwrap();
    let mapping = parsed.document().unwrap().as_mapping().unwrap();
    assert_eq!(
        mapping
            .get("a\nb")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "1"
    );

    // Nested, so the key sits at a non-zero column.
    let mut outer = BTreeMap::new();
    outer.insert("o".to_string(), YamlValue::Mapping(map));
    assert_eq!(render_as_yaml(&outer), "o:\n  \"a\\nb\": '1'\n  c: '2'\n");
}

#[test]
fn as_yaml_build_content() {
    // Vec<YamlValue> and &[YamlValue] render as a block sequence.
    let seq = vec![YamlValue::from("a"), YamlValue::from("b")];
    assert_eq!(render_as_yaml(&seq), "- a\n- b\n");
    assert_eq!(render_as_yaml(&seq.as_slice()), "- a\n- b\n");

    // BTreeMap renders as a block mapping (keys sorted).
    let mut m = BTreeMap::new();
    m.insert("k".to_string(), YamlValue::from("v"));
    m.insert("j".to_string(), YamlValue::from("w"));
    assert_eq!(render_as_yaml(&m), "j: w\nk: v\n");

    // Empty collections render in flow style.
    let empty_seq: Vec<YamlValue> = vec![];
    assert_eq!(render_as_yaml(&empty_seq), "[]");
    let empty_map: BTreeMap<String, YamlValue> = BTreeMap::new();
    assert_eq!(render_as_yaml(&empty_map), "{}");

    // A mapping whose value is a non-scalar gets a newline and deeper indent.
    let mut m2 = BTreeMap::new();
    m2.insert(
        "list".to_string(),
        YamlValue::Sequence(vec![YamlValue::from("a")]),
    );
    assert_eq!(render_as_yaml(&m2), "list:\n  - a\n");

    // Empty collections stay on the key's line, as do tagged ones.
    let mut m3 = BTreeMap::new();
    m3.insert("e".to_string(), YamlValue::mapping());
    m3.insert("l".to_string(), YamlValue::sequence());
    m3.insert("o".to_string(), YamlValue::from_ordered_mapping(vec![]));
    m3.insert("p".to_string(), YamlValue::from_pairs(vec![]));
    assert_eq!(
        render_as_yaml(&m3),
        "e: {}\nl: []\no: !!omap []\np: !!pairs []\n"
    );

    // YamlValue delegates to the right impl per variant.
    assert_eq!(
        render_as_yaml(&YamlValue::Sequence(seq.clone())),
        "- a\n- b\n"
    );
    assert_eq!(
        render_as_yaml(&YamlValue::Mapping(m.clone())),
        "j: w\nk: v\n"
    );
    assert_eq!(render_as_yaml(&YamlValue::from("scalar")), "scalar");

    // is_inline: empty collections are inline, non-empty block collections
    // are not. Scalars are always inline.
    assert!(!AsYaml::is_inline(&seq));
    assert!(AsYaml::is_inline(&empty_seq));
    assert!(!AsYaml::is_inline(&m));
    assert!(AsYaml::is_inline(&empty_map));
    assert!(!AsYaml::is_inline(&seq.as_slice()));
    assert!(AsYaml::is_inline(&empty_seq.as_slice()));
    assert!(AsYaml::is_inline(&YamlValue::from("x")));
    assert!(!AsYaml::is_inline(&YamlValue::Sequence(seq)));
    assert!(AsYaml::is_inline(&YamlValue::sequence()));
    assert!(!AsYaml::is_inline(&YamlValue::Mapping(m)));
    assert!(AsYaml::is_inline(&YamlValue::mapping()));
}

#[test]
fn from_document_resolves_aliases_and_merge_keys() {
    let yaml = "\
base: &base
  name: x
  tags: !!set {? a, ? b}
list: &list [1, *base]
derived:
  <<: *base
  name: y
  extra: *list
  nested: !local {inner: *base}
ordered: !!omap [first: *list]
";
    let mapping = |entries: Vec<(&str, YamlValue)>| {
        YamlValue::Mapping(
            entries
                .into_iter()
                .map(|(k, v)| (k.to_string(), v))
                .collect(),
        )
    };
    let tags = YamlValue::Set(["a", "b"].map(String::from).into());
    let base = mapping(vec![("name", YamlValue::from("x")), ("tags", tags.clone())]);
    let list = YamlValue::Sequence(vec![YamlValue::from(1), base.clone()]);
    let expected = mapping(vec![
        ("base", base.clone()),
        ("list", list.clone()),
        (
            "derived",
            mapping(vec![
                ("name", YamlValue::from("y")),
                ("tags", tags),
                ("extra", list.clone()),
                (
                    "nested",
                    YamlValue::tagged("!local", mapping(vec![("inner", base)])),
                ),
            ]),
        ),
        (
            "ordered",
            YamlValue::OrderedMapping(vec![("first".to_string(), list)]),
        ),
    ]);
    let doc = Document::from_str(yaml).unwrap();
    assert_eq!(YamlValue::from_document(&doc), Ok(expected.clone()));
    assert_eq!(YamlValue::parse_raw(yaml).unwrap(), expected);
}

#[test]
fn from_document_empty_is_null() {
    for yaml in ["---\n", "--- # c\n", "---\n...\n", "--- \n"] {
        let file = YamlFile::from_str(yaml).unwrap();
        let doc = file.document().unwrap();
        assert_eq!(
            YamlValue::from_document(&doc),
            Ok(YamlValue::Scalar(ScalarValue::null())),
            "{yaml:?}"
        );
        assert_eq!(
            YamlValue::parse_raw(yaml).unwrap(),
            YamlValue::Scalar(ScalarValue::null()),
            "{yaml:?}"
        );
    }
    assert_eq!(
        YamlValue::from_document(&Document::new()),
        Ok(YamlValue::Scalar(ScalarValue::null()))
    );
    assert_eq!(
        YamlValue::from_document(&Document::from_str("--- # c\n").unwrap()),
        Ok(YamlValue::Scalar(ScalarValue::null()))
    );
}

#[test]
fn from_document_keeps_tagged_alias() {
    let doc = Document::from_str("x: &x 1\na: !t *x\n").unwrap();
    let expected = Document::from_str("x: 1\na: !t 1\n").unwrap();
    assert_eq!(
        YamlValue::from_document(&doc),
        YamlValue::from_document(&expected)
    );
    assert_eq!(
        YamlValue::from_document(&doc)
            .unwrap()
            .as_mapping()
            .unwrap()
            .len(),
        2
    );
}

#[test]
fn from_document_anchored_alias() {
    let doc = Document::from_str("x: &x 1\nb: &b *x\nc: *b\n").unwrap();
    let expected = Document::from_str("x: 1\nb: 1\nc: 1\n").unwrap();
    assert_eq!(
        YamlValue::from_document(&doc),
        YamlValue::from_document(&expected)
    );
}

#[test]
fn from_document_alias_errors() {
    for (yaml, expected) in [
        (
            "a: *nope\n",
            ResolveError::UndefinedAlias {
                alias: "nope".to_string(),
            },
        ),
        (
            "a: &a [*a]\n",
            ResolveError::RecursiveAlias {
                alias: "a".to_string(),
            },
        ),
    ] {
        let doc = Document::from_str(yaml).unwrap();
        assert_eq!(YamlValue::from_document(&doc), Err(expected), "{yaml:?}");
    }
}

/// A slot holding more than one node, which the parser keeps for the
/// validator to report, cannot be converted without losing a node.
#[test]
fn from_document_extra_node() {
    for (yaml, extra) in [
        ("k: [q] x\n", "x"),
        ("a: &a 1\nk: *a x\n", "x"),
        ("a\nb: c\n", "b: c\n"),
        ("[q] x\n", "x"),
        ("- [q] x\n", "x"),
        ("k: !t [q] x\n", "x"),
        ("[q] x: v\n", "x"),
        ("{[q] x: v}\n", "x"),
        ("? a\n 5b: 1\n", "5b: 1\n"),
        ("a: &x [q] y\nb: *x\n", "y"),
        ("k:\n  [q] x\n  y\n", "x\n  y"),
        ("k:\n  [q]\n  y\n", "y"),
        ("- [q] x\n  y\n", "x\n  y"),
        ("[q]\n y\n", "y"),
    ] {
        let expected = ResolveError::ExtraNode {
            node: extra.to_string(),
        };
        let doc = Document::from_str(yaml).unwrap();
        assert_eq!(
            YamlValue::from_document(&doc),
            Err(expected.clone()),
            "{yaml:?}"
        );
        match YamlValue::parse_raw(yaml) {
            Err(YamlError::Resolve(err)) => assert_eq!(err, expected, "{yaml:?}"),
            other => panic!("{yaml:?}: {other:?}"),
        }
    }

    let doc = Document::from_str("k: [q] x\nj: 1\n").unwrap();
    assert_eq!(
        doc.get_resolved("k"),
        Err(ResolveError::ExtraNode {
            node: "x".to_string()
        })
    );
    assert_eq!(doc.get_resolved("j"), Ok(Some(YamlValue::scalar(1))));
    let value = doc.as_mapping().unwrap().get("k").unwrap();
    assert_eq!(
        YamlValue::cast(value.as_sequence().unwrap().syntax().clone()),
        Err(ResolveError::ExtraNode {
            node: "x".to_string()
        })
    );
}

#[test]
fn cast_non_value_node() {
    let file = "a: 1\n".parse::<YamlFile>().unwrap();
    assert_eq!(YamlValue::cast(file.syntax().clone()), Ok(None));
}

#[test]
fn parse_raw_parse_error() {
    for (yaml, expected) in [
            ("key: \"x", "Parse error at line 1, column 6: Unterminated quoted string"),
            (
                "- a\nb\n",
                "Parse error at line 2, column 1: Content after the document could not be parsed: \"b\"",
            ),
        ] {
            let err = YamlValue::parse_raw(yaml).unwrap_err();
            assert!(matches!(err, YamlError::Parse { .. }), "{err:?}");
            assert_eq!(err.to_string(), expected, "{yaml:?}");
        }
}

#[test]
fn parse_raw_resolve_error() {
    let result = YamlValue::parse_raw("a: *nope\n");
    assert_eq!(
        result.as_ref().unwrap_err().to_string(),
        "Resolve error: alias to undefined anchor \"nope\""
    );
    match result {
        Err(YamlError::Resolve(err)) => assert_eq!(
            err,
            ResolveError::UndefinedAlias {
                alias: "nope".to_string()
            }
        ),
        other => panic!("unexpected result: {other:?}"),
    }
}

#[test]
fn parse_raw_without_document_is_null() {
    for yaml in ["", "# just a comment\n"] {
        assert_eq!(
            YamlValue::parse_raw(yaml).unwrap(),
            YamlValue::Scalar(ScalarValue::null()),
            "{yaml:?}"
        );
    }
}

#[test]
fn parse_raw_variants() {
    // parse_raw goes through YamlValue::cast, so each YAML shape must map to
    // the corresponding variant.
    assert!(YamlValue::parse_raw("hello").unwrap().is_scalar());

    let seq = YamlValue::parse_raw("- a\n- b\n").unwrap();
    assert_eq!(
        seq.as_sequence(),
        Some([YamlValue::from("a"), YamlValue::from("b")].as_slice())
    );

    let map = YamlValue::parse_raw("k: v\n").unwrap();
    assert_eq!(map.as_mapping().map(|m| m.len()), Some(1));

    let set = YamlValue::parse_raw("!!set\n  a: ~\n  b: ~\n").unwrap();
    assert_eq!(
        set.as_set(),
        Some(&BTreeSet::from(["a".to_string(), "b".to_string()]))
    );

    let omap = YamlValue::parse_raw("!!omap\n  - a: 1\n  - b: 2\n").unwrap();
    assert_eq!(
        omap.as_ordered_mapping(),
        Some(
            [
                ("a".to_string(), YamlValue::from(1)),
                ("b".to_string(), YamlValue::from(2)),
            ]
            .as_slice()
        )
    );

    let pairs = YamlValue::parse_raw("!!pairs\n  - a: 1\n  - a: 2\n").unwrap();
    assert_eq!(
        pairs.as_pairs(),
        Some(
            [
                ("a".to_string(), YamlValue::from(1)),
                ("a".to_string(), YamlValue::from(2)),
            ]
            .as_slice()
        )
    );
}

#[test]
fn display_matches_to_yaml_string() {
    let map = {
        let mut m = BTreeMap::new();
        m.insert("name".to_string(), YamlValue::from("app"));
        YamlValue::Mapping(m)
    };
    assert_eq!(format!("{}", map), map.to_yaml_string(0));
    assert_eq!(format!("{}", map), "name: app");

    let scalar = YamlValue::from("hello");
    assert_eq!(format!("{}", scalar), "hello");
}

#[test]
fn to_yaml_string_nested_indentation() {
    // A mapping item starts on the dash's line.
    let mut inner = BTreeMap::new();
    inner.insert("h".to_string(), YamlValue::from("x"));
    let seq = YamlValue::Sequence(vec![YamlValue::Mapping(inner)]);
    assert_eq!(seq.to_yaml_string(0), "- h: x");

    // Mapping holding a non-scalar value: nested sequence is rendered at
    // indent + 2.
    let mut m = BTreeMap::new();
    m.insert(
        "k".to_string(),
        YamlValue::Sequence(vec![YamlValue::from("a")]),
    );
    assert_eq!(YamlValue::Mapping(m).to_yaml_string(0), "k:\n  - a");

    // Ordered mapping with a non-scalar value: the nested mapping has to
    // sit right of the key at indent + 4, or it becomes a sibling of it.
    let mut inner2 = BTreeMap::new();
    inner2.insert("x".to_string(), YamlValue::from("y"));
    let omap = YamlValue::from_ordered_mapping(vec![("a".to_string(), YamlValue::Mapping(inner2))]);
    assert_eq!(omap.to_yaml_string(0), "!!omap\n  - a:\n      x: y");

    let pairs = YamlValue::from_pairs(vec![(
        "a".to_string(),
        YamlValue::Sequence(vec![YamlValue::from("z")]),
    )]);
    assert_eq!(pairs.to_yaml_string(0), "!!pairs\n  - a:\n      - z");
}

#[test]
fn to_yaml_string_quotes_keys() {
    let mut m = BTreeMap::new();
    for k in ["a: b", "", "true", "x\ny"] {
        m.insert(k.to_string(), YamlValue::from("v"));
    }
    let v = YamlValue::Mapping(m);
    let out = v.to_yaml_string(0);
    assert_eq!(out, "'': v\n'a: b': v\n'true': v\n\"x\\ny\": v");
    assert_eq!(YamlValue::parse_raw(&out).unwrap(), v);
}

#[test]
fn to_yaml_string_nested_block_scalars() {
    let mut inner = BTreeMap::new();
    inner.insert("s".to_string(), YamlValue::from("l1\nl2"));
    let mut m = BTreeMap::new();
    m.insert("n".to_string(), YamlValue::Mapping(inner));
    m.insert(
        "q".to_string(),
        YamlValue::from(vec![YamlValue::from("l1\nl2")]),
    );
    m.insert(
        "o".to_string(),
        YamlValue::from_ordered_mapping(vec![("a".into(), YamlValue::from("l1\nl2"))]),
    );
    let v = YamlValue::Mapping(m);
    let out = v.to_yaml_string(0);
    assert_eq!(
        out,
        "n:\n  s: |-\n    l1\n    l2\n\
             o: !!omap\n  - a: |-\n      l1\n      l2\n\
             q:\n  - |-\n    l1\n    l2"
    );
    assert_eq!(YamlValue::parse_raw(&out).unwrap(), v);
}

#[test]
fn set_sequence_of_indented_block_scalar() {
    let literal = YamlValue::Scalar(ScalarValue::literal(" a\n"));
    let doc = Document::from_str("a: 1\n").unwrap();
    doc.set("k", YamlValue::Sequence(vec![literal.clone()]));
    assert_parser_shaped(&doc);
    assert_eq!(doc.to_string(), "a: 1\nk:\n- |2\n   a\n");
    let v = YamlValue::parse_raw(&doc.to_string()).unwrap();
    assert_eq!(
        v.as_mapping().unwrap().get("k"),
        Some(&YamlValue::Sequence(vec![literal]))
    );
}

#[test]
fn to_yaml_string_nested_keep_block_scalar() {
    let mut inner = BTreeMap::new();
    inner.insert(
        "k".to_string(),
        YamlValue::Scalar(ScalarValue::literal("x\ny\n")),
    );
    let mut m = BTreeMap::new();
    m.insert("o".to_string(), YamlValue::Mapping(inner));
    let v = YamlValue::Mapping(m);
    let out = v.to_yaml_string(0);
    assert_eq!(out, "o:\n  k: |\n    x\n    y");
    assert_eq!(YamlValue::parse_raw(&out).unwrap(), v);
}

#[test]
fn to_yaml_string_ends_block_scalar_line() {
    let map = |value: &str| {
        let mut m = BTreeMap::new();
        m.insert(
            "k".to_string(),
            YamlValue::Scalar(ScalarValue::literal(value)),
        );
        YamlValue::Mapping(m)
    };
    for (value, expected) in [
        ("x\n\n", "k: |+\n  x\n\n"),
        ("x\n  \n", "k: |\n  x\n    "),
        ("x\ny", "k: |-\n  x\n  y"),
    ] {
        let v = map(value);
        assert_eq!(v.to_yaml_string(0), expected);
        assert_eq!(v.to_string(), expected);
        assert_eq!(YamlValue::parse_raw(expected).unwrap(), v);
    }

    let v = YamlValue::from(vec![
        YamlValue::Scalar(ScalarValue::literal("x\n\n")),
        YamlValue::from(1),
    ]);
    assert_eq!(v.to_yaml_string(0), "- |+\n  x\n\n- 1");
    assert_eq!(YamlValue::parse_raw(&v.to_yaml_string(0)).unwrap(), v);
}

#[test]
fn to_yaml_string_nested_indented_block_scalar() {
    let mut inner = BTreeMap::new();
    inner.insert(
        "k".to_string(),
        YamlValue::Scalar(ScalarValue::literal("  x\n")),
    );
    let mut m = BTreeMap::new();
    m.insert("o".to_string(), YamlValue::Mapping(inner));
    let v = YamlValue::Mapping(m);
    let out = v.to_yaml_string(0);
    assert_eq!(out, "o:\n  k: |2\n      x");
    assert_eq!(YamlValue::parse_raw(&out).unwrap(), v);
}

#[test]
fn to_yaml_string_nested_inline_values() {
    let set: BTreeSet<String> = ["#c", "a: b"].into_iter().map(String::from).collect();
    let mut m = BTreeMap::new();
    m.insert("s".to_string(), YamlValue::Set(set));
    m.insert("e".to_string(), YamlValue::mapping());
    m.insert(
        "l".to_string(),
        YamlValue::from(vec![YamlValue::sequence()]),
    );
    let v = YamlValue::Mapping(m);
    let out = v.to_yaml_string(0);
    assert_eq!(
        out,
        "e: {}\nl:\n  - []\ns: !!set\n  '#c': null\n  'a: b': null"
    );
    assert_eq!(YamlValue::parse_raw(&out).unwrap(), v);
}

fn tagged_value(src: &str) -> ScalarValue {
    let v = YamlValue::parse_raw(src).unwrap();
    v.as_mapping().unwrap()["k"].as_scalar().unwrap().clone()
}

#[test]
fn cast_core_tagged_scalars() {
    let s = tagged_value("k: !!int 42");
    assert_eq!(s.scalar_type(), ScalarType::Integer);
    assert_eq!(s.to_i64(), Some(42));
    assert_eq!(s.tag(), Some("!!int"));
    assert_eq!(s.to_yaml_string(), "!!int 42");

    let s = tagged_value("k: !!int \"42\"");
    assert_eq!(s.to_i64(), Some(42));
    assert_eq!(s.style(), ScalarStyle::DoubleQuoted);
    assert_eq!(s.to_yaml_string(), "!!int \"42\"");

    let s = tagged_value("k: !<tag:yaml.org,2002:int> 7");
    assert_eq!(s.to_i64(), Some(7));
    assert_eq!(s.to_yaml_string(), "!<tag:yaml.org,2002:int> 7");

    let s = tagged_value("k: !!float 1.5");
    assert_eq!(s.scalar_type(), ScalarType::Float);
    assert_eq!(s.to_f64(), Some(1.5));
    assert_eq!(s.to_yaml_string(), "!!float 1.5");

    let s = tagged_value("k: !!float 1");
    assert_eq!(s.to_f64(), Some(1.0));
    assert_eq!(s.to_yaml_string(), "!!float 1");

    let s = tagged_value("k: !!bool 'true'");
    assert_eq!(s.to_bool(), Some(true));
    assert_eq!(s.to_yaml_string(), "!!bool 'true'");

    let s = tagged_value("k: !!null null");
    assert_eq!(s.scalar_type(), ScalarType::Null);
    assert_eq!(s.to_yaml_string(), "!!null null");

    let s = tagged_value("k: !!null\nj: 1");
    assert_eq!(s.scalar_type(), ScalarType::Null);
    assert_eq!(s.to_yaml_string(), "!!null");

    let s = tagged_value("k: !!str 42");
    assert_eq!(s, ScalarValue::plain("42").with_tag("!!str"));
    assert_eq!(s.to_yaml_string(), "!!str 42");

    // Content that the tag cannot resolve stays a string.
    let s = tagged_value("k: !!int abc");
    assert_eq!(s, ScalarValue::plain("abc").with_tag("!!int"));
    assert_eq!(s.to_yaml_string(), "!!int abc");
}

#[test]
fn tagged_values_round_trip() {
    for src in [
        "k: !!float 1",
        "k: !local bar",
        "k: !local \"bar\"",
        "k: !local 'a: b'",
        "k: !local true",
        "k: !local |\n  x\n  y",
        "k: ! 12",
        "!local bar",
        "k: !local\n  a: 1",
        "k: !local []",
        "k: !!map {}",
        "k: !!seq\n  - a",
        "!local\n  a: !!str 1",
        "!local\n  - a\n  - !!int 1",
    ] {
        let v = YamlValue::parse_raw(src).unwrap();
        assert_eq!(v.to_yaml_string(0), src);
        assert_eq!(YamlValue::parse_raw(&v.to_yaml_string(0)).unwrap(), v);
    }
    // A verbatim core tag is read as the tag it spells, which renders in
    // its shorthand.
    let v = YamlValue::parse_raw("k: !<tag:yaml.org,2002:set>\n  a: null").unwrap();
    assert_eq!(v.to_yaml_string(0), "k: !!set\n  a: null");
    assert_eq!(YamlValue::parse_raw(&v.to_yaml_string(0)).unwrap(), v);
}

#[test]
fn cast_tagged_collections() {
    let v = YamlValue::parse_raw("k: !local {a: 1}").unwrap();
    let mut inner = BTreeMap::new();
    inner.insert("a".to_string(), YamlValue::from(1));
    let mut expected = BTreeMap::new();
    expected.insert(
        "k".to_string(),
        YamlValue::Tagged("!local".to_string(), Box::new(YamlValue::Mapping(inner))),
    );
    assert_eq!(v, YamlValue::Mapping(expected));

    let v = YamlValue::parse_raw("!!seq [a, b]").unwrap();
    assert_eq!(
        v,
        YamlValue::tagged("!!seq", YamlValue::from(vec!["a", "b"]))
    );
    assert_eq!(v.tag(), Some("!!seq"));
}

#[test]
fn tagged_values_build_content() {
    let doc = Document::new();
    doc.set("a", YamlValue::parse_raw("!!float 1").unwrap());
    assert_cst_ok(&doc);
    doc.set("b", YamlValue::parse_raw("!local 'x'").unwrap());
    assert_cst_ok(&doc);
    doc.set("c", YamlValue::parse_raw("!local [x]").unwrap());
    assert_cst_ok(&doc);
    doc.set("d", YamlValue::parse_raw("!!str").unwrap());
    assert_cst_ok(&doc);
    assert_eq!(
        doc.to_string(),
        "---\na: !!float 1\nb: !local 'x'\nc: !local\n  - x\nd: !!str\n"
    );
    let v = YamlValue::from_document(&doc).unwrap();
    let m = v.as_mapping().unwrap();
    assert_eq!(m["a"], YamlValue::parse_raw("!!float 1").unwrap());
    assert_eq!(m["b"], YamlValue::parse_raw("!local 'x'").unwrap());
    assert_eq!(m["c"], YamlValue::parse_raw("!local [x]").unwrap());
    assert_eq!(m["d"], YamlValue::parse_raw("!!str").unwrap());
}

#[test]
fn nested_structure() {
    let mut db_map = BTreeMap::new();
    db_map.insert("host", YamlValue::from("localhost"));
    db_map.insert("port", YamlValue::from(5432));

    let mut root_map = BTreeMap::new();
    root_map.insert("name", YamlValue::from("app"));
    root_map.insert("database", YamlValue::from(db_map));
    root_map.insert("features", YamlValue::from(vec!["auth", "logging"]));

    let val = YamlValue::from(root_map);
    let yaml = val.to_yaml_string(0);
    assert_eq!(
        yaml,
        "database:\n  host: localhost\n  port: 5432\nfeatures:\n  - auth\n  - logging\nname: app"
    );
}

#[test]
fn raw_collections_as_values() {
    let mut map = BTreeMap::new();
    map.insert("a".to_string(), YamlValue::from(1i64));
    map.insert("b c".to_string(), YamlValue::from(vec!["x,y", "z"]));
    let items: Vec<YamlValue> = vec![
        YamlValue::from("p, q"),
        YamlValue::from(2.5f64),
        YamlValue::from(BTreeSet::from(["s"])),
    ];
    for (src, expected) in [
            (
                "k: 0\n",
                "k: 0\nm:\n  a: 1\n  b c:\n    - x,y\n    - z\nv:\n- p, q\n- 2.5\n- !!set\n  s: null\ns:\n- p, q\n- 2.5\n- !!set\n  s: null\ne: []\nf: {}\ng: true\nh: 1.5\n",
            ),
            (
                "{k: 0}",
                "{k: 0, m: {a: 1, b c: [\"x,y\", z]}, v: [\"p, q\", 2.5, !!set {s: null}], s: [\"p, q\", 2.5, !!set {s: null}], e: [], f: {}, g: true, h: 1.5}",
            ),
            (
                "{\"k\": 0}",
                "{\"k\": 0, \"m\": {\"a\": 1, \"b c\": [\"x,y\", \"z\"]}, \"v\": [\"p, q\", 2.5, !!set {\"s\": null}], \"s\": [\"p, q\", 2.5, !!set {\"s\": null}], \"e\": [], \"f\": {}, \"g\": true, \"h\": 1.5}",
            ),
        ] {
            let doc = Document::from_str(src).unwrap();
            let mapping = doc.as_mapping().unwrap();
            mapping.set("m", map.clone());
            mapping.set("v", items.clone());
            mapping.set("s", items.as_slice());
            mapping.set("e", Vec::<YamlValue>::new());
            mapping.set("f", BTreeMap::<String, YamlValue>::new());
            mapping.set("g", YamlValue::from(&YamlValue::from(true)));
            mapping.set("h", YamlValue::from(1.5f32));
            assert_eq!(doc.to_string(), expected, "{src:?}");
            assert_cst_ok(&doc);
        }
}

#[test]
fn scalar_build_content_token_kind() {
    for (value, kind) in [
        (YamlValue::from("hello"), SyntaxKind::STRING),
        (YamlValue::from(42), SyntaxKind::INT),
        (YamlValue::from(1.5), SyntaxKind::FLOAT),
        // Strings that happen to spell a bool or null keep their String
        // type and are quoted, so they tokenize as STRING - rendering them
        // bare would change their type on the next parse.
        (
            YamlValue::Scalar(ScalarValue::string("true")),
            SyntaxKind::STRING,
        ),
        (
            YamlValue::Scalar(ScalarValue::string("false")),
            SyntaxKind::STRING,
        ),
        (
            YamlValue::Scalar(ScalarValue::string("null")),
            SyntaxKind::STRING,
        ),
        // Parsed values keep their detected type.
        (
            YamlValue::Scalar(ScalarValue::parse("true")),
            SyntaxKind::BOOL,
        ),
        (
            YamlValue::Scalar(ScalarValue::parse("null")),
            SyntaxKind::NULL,
        ),
    ] {
        assert_eq!(scalar_token_kind(&value), kind, "{value:?}");
    }
}

#[test]
fn to_yaml_string_of_each_variant() {
    let mut map = BTreeMap::new();
    map.insert("name", YamlValue::from("project"));
    map.insert("version", YamlValue::from("1.0.0"));
    let set = BTreeSet::from(["item1", "item2", "item3"].map(String::from));
    let ordered = vec![
        ("first".to_string(), YamlValue::from("value1")),
        ("second".to_string(), YamlValue::from("value2")),
        ("third".to_string(), YamlValue::from("value3")),
    ];
    // Pairs may repeat a key.
    let pairs = vec![
        ("key1".to_string(), YamlValue::from("value1")),
        ("key1".to_string(), YamlValue::from("value2")),
        ("key2".to_string(), YamlValue::from("value3")),
    ];
    let scalar: fn(&YamlValue) -> bool = YamlValue::is_scalar;
    let sequence: fn(&YamlValue) -> bool = YamlValue::is_sequence;
    let mapping: fn(&YamlValue) -> bool = YamlValue::is_mapping;
    let is_set: fn(&YamlValue) -> bool = YamlValue::is_set;
    let omap: fn(&YamlValue) -> bool = YamlValue::is_ordered_mapping;
    let is_pairs: fn(&YamlValue) -> bool = YamlValue::is_pairs;
    for (value, is_variant, expected) in [
        (YamlValue::from("hello"), scalar, "hello"),
        (YamlValue::from(42), scalar, "42"),
        (YamlValue::from(true), scalar, "true"),
        (
            YamlValue::from(vec!["item1", "item2", "item3"]),
            sequence,
            "- item1\n- item2\n- item3",
        ),
        (YamlValue::sequence(), sequence, "[]"),
        (
            YamlValue::from(map),
            mapping,
            "name: project\nversion: 1.0.0",
        ),
        (YamlValue::mapping(), mapping, "{}"),
        (
            YamlValue::from_set(set),
            is_set,
            "!!set\n  item1: null\n  item2: null\n  item3: null",
        ),
        (YamlValue::set(), is_set, "!!set {}"),
        (
            YamlValue::from_ordered_mapping(ordered),
            omap,
            "!!omap\n  - first: value1\n  - second: value2\n  - third: value3",
        ),
        (YamlValue::ordered_mapping(), omap, "!!omap []"),
        (
            YamlValue::from_pairs(pairs),
            is_pairs,
            "!!pairs\n  - key1: value1\n  - key1: value2\n  - key2: value3",
        ),
        (YamlValue::pairs(), is_pairs, "!!pairs []"),
    ] {
        assert!(is_variant(&value), "{value:?}");
        assert_eq!(value.to_yaml_string(0), expected);
    }
}

#[test]
fn set_nested_values() {
    let mut inner = BTreeMap::new();
    inner.insert("s".to_string(), YamlValue::from("x\ny\n"));
    let mut block = BTreeMap::new();
    block.insert("n".to_string(), YamlValue::Mapping(inner));
    let mut empty = BTreeMap::new();
    empty.insert("e".to_string(), YamlValue::mapping());
    empty.insert("o".to_string(), YamlValue::from_ordered_mapping(vec![]));
    for (value, expected) in [
        (block, "a: 1\nb:\n  n:\n    s: |\n      x\n      y\n"),
        (empty, "a: 1\nb:\n  e: {}\n  o: !!omap []\n"),
    ] {
        check_edit(
            "a: 1\n",
            |doc| assert!(doc.set("b", YamlValue::Mapping(value))),
            expected,
        );
    }
}

#[test]
fn btreeset_block() {
    let doc = Document::from_str("name: project\n").unwrap();
    doc.set("tags", BTreeSet::from(["web", "db"]));
    assert_parser_shaped(&doc);
    assert_eq!(
        doc.to_string(),
        "name: project\ntags: !!set\n  db: null\n  web: null\n"
    );
}

#[test]
fn btreeset_nested_block() {
    let doc = Document::from_str("a:\n  b: 1\n").unwrap();
    let inner = doc.as_mapping().unwrap().get_mapping("a").unwrap();
    inner.set("s", BTreeSet::from([1_i64, 2]));
    assert_parser_shaped(&doc);
    assert_eq!(
        doc.to_string(),
        "a:\n  b: 1\n  s: !!set\n    1: null\n    2: null\n"
    );
}

#[test]
fn btreeset_empty() {
    let doc = Document::from_str("name: project\n").unwrap();
    doc.set("tags", BTreeSet::<String>::new());
    assert_parser_shaped(&doc);
    assert_eq!(doc.to_string(), "name: project\ntags: !!set {}\n");
}

#[test]
fn btreeset_flow() {
    let doc = Document::from_str("m: {a: 1}\n").unwrap();
    let inner = doc.as_mapping().unwrap().get_mapping("m").unwrap();
    inner.set("s", BTreeSet::from(["y", "x"]));
    assert_parser_shaped(&doc);
    assert_eq!(doc.to_string(), "m: {a: 1, s: !!set {x: null, y: null}}\n");
}

#[test]
fn btreeset_quotes_members() {
    let doc = Document::from_str("name: project\n").unwrap();
    doc.set("tags", BTreeSet::from(["true", "a: b", "x\ny"]));
    assert_parser_shaped(&doc);
    assert_eq!(
        doc.to_string(),
        "name: project\ntags: !!set\n  'a: b': null\n  'true': null\n  \"x\\ny\": null\n"
    );
}

#[test]
fn btreeset_reads_back_as_set() {
    let doc = Document::from_str("name: project\n").unwrap();
    doc.set("tags", BTreeSet::from(["web", "db"]));
    let tags = doc.as_mapping().unwrap().get("tags").unwrap();
    let set = tags.as_tagged().unwrap().as_set().unwrap();
    assert_eq!(set.len(), 2);
    assert!(set.contains("db"));
    assert!(set.contains("web"));
    assert!(!set.contains("null"));
}

#[test]
fn mapping_set_preserves_alias_kind() {
    let doc = Document::from_str("shared: &shared value\na: *shared\nb: old\n").unwrap();
    let mapping = doc.as_mapping().unwrap();
    mapping.set("b", mapping.get("a").unwrap());
    assert_cst_ok(&doc);
    assert_eq!(
        doc.to_string(),
        "shared: &shared value\na: *shared\nb: *shared\n"
    );

    match mapping.get("b").unwrap() {
        YamlNode::Alias(alias) => {
            assert_eq!(alias.name(), "shared");
            assert_eq!(alias.value(), "*shared");
        }
        other => panic!("Expected alias, got {:?}", other),
    }
}

fn root_of(text: &str) -> YamlNode {
    let doc = Document::from_str(text).unwrap();
    YamlNode::from_syntax(doc.syntax().first_child().unwrap()).unwrap()
}

fn hash_of(text: &str) -> u64 {
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    yaml_hash(&root_of(text), &mut hasher);
    hasher.finish()
}

#[test]
fn yaml_hash_consistent_with_yaml_eq() {
    for (a, b) in [
        ("name", "\"name\""),
        ("name", "!!str name"),
        ("1", "0x1"),
        ("{a: 1, b: 2}", "{b: 2, a: 1}"),
        ("[1, 'x']", "- 1\n- \"x\"\n"),
        ("a:\n  b: [1]\n", "{a: {b: [0x1]}}"),
    ] {
        assert!(yaml_eq(&root_of(a), &root_of(b)), "{a:?} vs {b:?}");
        assert_eq!(hash_of(a), hash_of(b), "{a:?} vs {b:?}");
    }
    assert_ne!(hash_of("1"), hash_of("'1'"));
    assert_ne!(hash_of("{a: 1, b: 2}"), hash_of("{b: 1, a: 2}"));
    assert_ne!(hash_of("[1, 2]"), hash_of("[2, 1]"));
}

#[test]
fn multiline_str_is_parser_shaped() {
    let doc = Document::from_str("a: 1  # c\nb: 2\n").unwrap();
    doc.set("a", "x\ny\n");
    doc.set("c", String::from("x\n\n"));
    assert_cst_ok(&doc);
    assert_parser_shaped(&doc);
    assert_eq!(doc.to_string(), "a: |  # c\n  x\n  y\nb: 2\nc: |+\n  x\n\n");
}

#[test]
fn option_none_emits_null() {
    let doc = Document::from_str("key: value\n").unwrap();
    doc.as_mapping().unwrap().set("key", None::<&str>);
    assert_cst_ok(&doc);
    assert_eq!(doc.to_string(), "key: null\n");
}

#[test]
fn option_some_delegates() {
    let doc = Document::from_str("key: value\n").unwrap();
    doc.as_mapping().unwrap().set("key", Some("hello"));
    assert_cst_ok(&doc);
    assert_eq!(doc.to_string(), "key: hello\n");

    let doc = Document::from_str("key: value\n").unwrap();
    doc.as_mapping().unwrap().set("key", Some(42i64));
    assert_cst_ok(&doc);
    assert_eq!(doc.to_string(), "key: 42\n");
}

#[test]
fn set_float_special_values() {
    let cases: [(f64, &str); 6] = [
        (f64::NAN, "k: .nan\n"),
        (f64::INFINITY, "k: .inf\n"),
        (f64::NEG_INFINITY, "k: -.inf\n"),
        (1.0, "k: 1.0\n"),
        (-0.0, "k: -0.0\n"),
        (1.5, "k: 1.5\n"),
    ];
    for (value, expected) in cases {
        let doc = Document::from_str("k: 0\n").unwrap();
        doc.as_mapping().unwrap().set("k", value);
        assert_cst_ok(&doc);
        assert_eq!(doc.to_string(), expected);
        let reparsed = Document::from_str(&doc.to_string()).unwrap();
        let got = reparsed
            .as_mapping()
            .unwrap()
            .get("k")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_f64()
            .unwrap();
        assert_eq!(got.to_bits(), value.to_bits());
    }

    let doc = Document::from_str("k: 0\n").unwrap();
    doc.as_mapping().unwrap().set("k", f32::INFINITY);
    assert_cst_ok(&doc);
    assert_eq!(doc.to_string(), "k: .inf\n");
}

#[test]
fn yaml_eq_special_float_spellings() {
    let doc =
        Document::from_str("a: .inf\nb: .Inf\nc: .nan\nd: .NaN\ne: -.inf\nf: -.INF\n").unwrap();
    let m = doc.as_mapping().unwrap();
    assert!(yaml_eq(&m.get("a").unwrap(), &m.get("b").unwrap()));
    assert!(yaml_eq(&m.get("c").unwrap(), &m.get("d").unwrap()));
    assert!(yaml_eq(&m.get("e").unwrap(), &m.get("f").unwrap()));
    assert!(!yaml_eq(&m.get("a").unwrap(), &m.get("e").unwrap()));
    assert!(yaml_eq(&m.get("b").unwrap(), &f64::INFINITY));
}

#[test]
fn yaml_eq_integers_outside_i64() {
    let yaml = "dec: 18446744073709551616\n\
                    hex: 0x10000000000000000\n\
                    oct: 0o2000000000000000000000\n\
                    padded: 00018446744073709551616\n\
                    tagged: !!int 0x10000000000000000\n\
                    other: 18446744073709551617\n\
                    min: 9223372036854775808\n\
                    plusmin: +9223372036854775808\n\
                    negmin: -9223372036854775809\n\
                    neghex: -0x8000000000000001\n";
    assert!(eq_values(yaml, "dec", "hex"));
    assert!(eq_values(yaml, "dec", "oct"));
    assert!(eq_values(yaml, "dec", "padded"));
    assert!(eq_values(yaml, "dec", "tagged"));
    assert!(!eq_values(yaml, "dec", "other"));
    assert!(eq_values(yaml, "min", "plusmin"));
    assert!(!eq_values(yaml, "min", "negmin"));
    // `-0x...` is not a core schema integer, so it is a string.
    assert!(!eq_values(yaml, "negmin", "neghex"));
    let doc = Document::from_str("a: 0xFFFFFFFFFFFFFFFF\n").unwrap();
    let a = doc.as_mapping().unwrap().get("a").unwrap();
    assert!(yaml_eq(&a, &u64::MAX));
}

#[test]
fn yaml_eq_negative_zero() {
    assert!(eq_values("a: -0\nb: 0\nc: +000\n", "a", "b"));
    assert!(eq_values("a: -0\nb: 0\nc: +000\n", "a", "c"));
}

fn eq_values(yaml: &str, a: &str, b: &str) -> bool {
    let doc = Document::from_str(yaml).unwrap();
    let m = doc.as_mapping().unwrap();
    let (a, b) = (m.get(a).unwrap(), m.get(b).unwrap());
    let forward = yaml_eq(&a, &b);
    assert_eq!(forward, yaml_eq(&b, &a));
    forward
}

fn eq_docs(a: &str, b: &str) -> bool {
    let (d1, d2) = (
        Document::from_str(a).unwrap(),
        Document::from_str(b).unwrap(),
    );
    let (m1, m2) = (d1.as_mapping().unwrap(), d2.as_mapping().unwrap());
    let forward = yaml_eq(&m1, &m2);
    assert_eq!(forward, yaml_eq(&m2, &m1));
    forward
}

#[test]
fn yaml_eq_tagged_key() {
    assert!(eq_docs("{a: 1}", "{!!str a: 1}"));
    assert!(eq_docs("a: 1\n", "!!str a: 1\n"));
    assert!(eq_docs("a: 1\n", "? !!str a\n: 1\n"));
    assert!(!eq_docs("1: a\n", "!!str 1: a\n"));
    assert!(eq_docs("'1': a\n", "!!str 1: a\n"));
    assert!(eq_docs("1: a\n", "!!int 0x1: a\n"));
}

#[test]
fn yaml_eq_tagged_value() {
    assert!(eq_docs("{a: 1}", "{a: !!int 1}"));
    assert!(eq_docs("a: 1\n", "a: !!int\n  1\n"));
    assert!(!eq_docs("a: 1\n", "a: !!str 1\n"));
}

#[test]
fn yaml_eq_tagged_scalars() {
    let yaml = "hex: !!int 0x1\n\
                    int: !!int 1\n\
                    plain: 1\n\
                    quoted: '1'\n\
                    str: !!str 1\n\
                    verbatim: !<tag:yaml.org,2002:int> 01\n\
                    float: !!float 1\n\
                    plainfloat: 1.0\n\
                    nonspecific: ! 1\n\
                    custom: !foo 1\n\
                    custom2: !bar 1\n\
                    bool: !!bool TRUE\n\
                    plainbool: true\n\
                    nulltag: !!null ~\n\
                    plainnull:\n";
    assert!(eq_values(yaml, "hex", "int"));
    assert!(eq_values(yaml, "hex", "plain"));
    assert!(eq_values(yaml, "verbatim", "plain"));
    assert!(!eq_values(yaml, "plain", "quoted"));
    assert!(eq_values(yaml, "str", "quoted"));
    assert!(!eq_values(yaml, "str", "plain"));
    assert!(eq_values(yaml, "float", "plainfloat"));
    assert!(!eq_values(yaml, "float", "plain"));
    assert!(eq_values(yaml, "nonspecific", "quoted"));
    assert!(!eq_values(yaml, "custom", "quoted"));
    assert!(!eq_values(yaml, "custom", "custom2"));
    assert!(eq_values(yaml, "custom", "custom"));
    assert!(eq_values(yaml, "bool", "plainbool"));
    assert!(eq_values(yaml, "nulltag", "plainnull"));
}

#[test]
fn yaml_eq_tagged_scalar_raw() {
    let doc = Document::from_str("a: !!int 0x1\nb: !!str 1\n").unwrap();
    let m = doc.as_mapping().unwrap();
    assert!(yaml_eq(&m.get("a").unwrap(), &1_i64));
    assert!(!yaml_eq(&m.get("a").unwrap(), &"1"));
    assert!(yaml_eq(&m.get("b").unwrap(), &"1"));
    assert!(!yaml_eq(&m.get("b").unwrap(), &1_i64));
}

#[test]
fn yaml_eq_tagged_collections() {
    let yaml = "map: !!map {a: 1}\n\
                    plainmap: {a: 1}\n\
                    custommap: !foo {a: 1}\n\
                    set1: !!set {a, b}\n\
                    set2: !!set {b, a}\n\
                    set3: !!set {a, c}\n\
                    seq: !!seq [1, 2]\n\
                    plainseq: [1, 2]\n";
    assert!(eq_values(yaml, "map", "plainmap"));
    assert!(!eq_values(yaml, "custommap", "plainmap"));
    assert!(eq_values(yaml, "set1", "set2"));
    assert!(!eq_values(yaml, "set1", "set3"));
    assert!(eq_values(yaml, "seq", "plainseq"));
    assert!(!eq_values(yaml, "seq", "plainmap"));
}

#[test]
fn yaml_eq_cst_vs_raw_collections() {
    let doc = Document::from_str("{x: 1, y: 2}").unwrap();
    let cst = doc.as_mapping().unwrap();
    let raw = YamlValue::Mapping(BTreeMap::from([
        ("x".to_string(), YamlValue::scalar(1_i64)),
        ("y".to_string(), YamlValue::scalar(2_i64)),
    ]));
    assert!(yaml_eq(&cst, &raw));
    assert!(yaml_eq(&raw, &cst));

    let other = YamlValue::Mapping(BTreeMap::from([
        ("x".to_string(), YamlValue::scalar(1_i64)),
        ("y".to_string(), YamlValue::scalar("2")),
    ]));
    assert!(!yaml_eq(&cst, &other));
    assert!(!yaml_eq(&raw, &other));
    assert!(yaml_eq(&raw, &raw.clone()));

    let doc = Document::from_str("[1, two]").unwrap();
    let cst = doc.as_sequence().unwrap();
    let raw = YamlValue::from_sequence(vec![
        YamlValue::scalar(ScalarValue::parse("1")),
        YamlValue::scalar("two"),
    ]);
    assert!(yaml_eq(&cst, &raw));
    assert!(yaml_eq(&raw, &cst));
    assert!(!yaml_eq(&cst, &YamlValue::from_sequence(vec![])));
}

#[test]
fn yaml_node_helpers() {
    let doc = Document::from_str("a: 'x'\nb: [1, 2]\nd: &q x\ne: *q\n").unwrap();
    let mapping = doc.as_mapping().unwrap();
    let a = mapping.get("a").unwrap();
    assert!(a == "x");
    assert!(a == *"x");
    let owned = String::from("x");
    assert!(a == owned);
    assert!(a.yaml_eq(&"x"));
    assert!(a != "y");
    assert!(!a.yaml_eq(&"y"));
    let other = String::from("y");
    assert!(a != other);
    let b = mapping.get("b").unwrap();
    assert_eq!(b.get_item(1).unwrap().to_string(), "2");
    assert_eq!(b.get_item(2), None);
    assert_eq!(a.get_item(0), None);
    let e = mapping.get("e").unwrap();
    assert_eq!(
        e.as_alias().map(|alias| alias.name()),
        Some("q".to_string())
    );
    assert_eq!(a.as_alias(), None);
    // Aliases are not resolved, so they are not even equal to themselves.
    assert!(!yaml_eq(&e, &e));
}

#[test]
fn option_values() {
    for (src, expected) in [
        ("{a: 1}", "{a: 1, k: null, j: \"p, q\"}"),
        ("{\"a\": 1}", "{\"a\": 1, \"k\": null, \"j\": \"p, q\"}"),
        ("a: 1\n", "a: 1\nk: null\nj: p, q\n"),
    ] {
        let doc = Document::from_str(src).unwrap();
        let mapping = doc.as_mapping().unwrap();
        mapping.set("k", None::<i64>);
        mapping.set("j", Some("p, q"));
        assert_eq!(doc.to_string(), expected, "{src:?}");
        assert_cst_ok(&doc);
    }
}

#[test]
fn yaml_node_kind_predicates() {
    let doc = Document::from_str("s: x\nm: {a: 1}\nl: [1]\nt: !t x\nr: &r 1\na: *r\n").unwrap();
    let mapping = doc.as_mapping().unwrap();
    let kinds = |key: &str| {
        let node = mapping.get(key).unwrap();
        [
            node.is_scalar(),
            node.is_mapping(),
            node.is_sequence(),
            node.is_tagged(),
            node.is_alias(),
        ]
    };
    assert_eq!(kinds("s"), [true, false, false, false, false]);
    assert_eq!(kinds("m"), [false, true, false, false, false]);
    assert_eq!(kinds("l"), [false, false, true, false, false]);
    assert_eq!(kinds("t"), [false, false, false, true, false]);
    assert_eq!(kinds("a"), [false, false, false, false, true]);
    assert_eq!(
        mapping.get("m").unwrap().byte_range(),
        TextPosition::new(8, 14)
    );
}
const QUOTING: &str = r#"
plain: value
single: 'value'
double: "value"
"#;

const ESCAPES: &str = r#"
newline1: "line1\nline2"
newline2: "line1

line2"
tab1: "a\tb"
tab2: "a	b"
backslash1: "path\\file"
backslash2: 'path\file'
quote1: "say \"hi\""
quote2: 'say "hi"'
"#;

const SINGLE_QUOTES: &str = r#"
single1: 'can''t'
single2: "can't"
"#;

const HEX_ESCAPES: &str = r#"
unicode1: "hello\x20world"
unicode2: "hello world"
unicode3: "smiley\u0020face"
unicode4: "smiley face"
"#;

const CONTROL_ESCAPES: &str = r#"
bell: "\a"
escape: "\e"
"null": "\0"
backspace: "\b"
formfeed: "\f"
carriagereturn: "\r"
verticaltab: "\v"
"#;

const EMPTY: &str = r#"
empty1: ""
empty2: ''
"#;

const WHITESPACE: &str = r#"
spaces1: "  leading"
spaces2: "trailing  "
spaces3: "  both  "
plain: value
"#;

const TYPES: &str = r#"
string: "123"
number: 123
sequence:
  - item
mapping:
  key: value
"#;

const FOLDING: &str = r#"
folded1: "line1 \
  line2"
folded2: "line1 line2"
"#;

const EMOJI: &str = r#"
emoji1: "😀"
emoji2: "\U0001F600"
"#;

const NULLS: &str = r#"
null1: null
null2: null
null3:
empty: ""
"#;

const BOOLS: &str = r#"
true1: true
true2: True
true3: TRUE
false1: false
false2: False
"#;

const NUMBERS: &str = r#"
decimal: 123
octal: 0o173
hex: 0x7B
"#;

const ANCHORS: &str = r#"
original: &anchor value
duplicate: value
"#;

const STYLES: &str = r#"
flow_seq: [1, 2, 3]
block_seq:
  - 1
  - 2
  - 3
flow_map: {a: 1, b: 2}
block_map:
  a: 1
  b: 2
"#;

/// Whether the values of `a` and `b` in the root mapping of each document
/// are equal by `yaml_eq`, which has to be symmetric.
fn eq_across(yaml_a: &str, a: &str, yaml_b: &str, b: &str) -> bool {
    let (va, vb) = (value_of(yaml_a, a), value_of(yaml_b, b));
    let forward = yaml_eq(&va, &vb);
    assert_eq!(forward, yaml_eq(&vb, &va));
    forward
}

/// Whether the root nodes of two documents are equal by `yaml_eq`, which
/// has to be symmetric.
fn eq_roots(a: &str, b: &str) -> bool {
    let (ra, rb) = (root_of(a), root_of(b));
    let forward = yaml_eq(&ra, &rb);
    assert_eq!(forward, yaml_eq(&rb, &ra));
    forward
}

/// Two scalars are equal when they mean the same: the style they are
/// written in, escapes, anchors and the case of a boolean do not matter,
/// their type does.
#[test]
fn yaml_eq_scalars() {
    for (yaml, a, b, expected) in [
        (QUOTING, "plain", "single", true),
        (QUOTING, "single", "double", true),
        (QUOTING, "plain", "double", true),
        // An escaped line break equals an empty line, which folds to one.
        (ESCAPES, "newline1", "newline2", true),
        (ESCAPES, "tab1", "tab2", true),
        // A single-quoted string does not interpret backslashes, so
        // 'path\file' is the same as "path\\file".
        (ESCAPES, "backslash1", "backslash2", true),
        (ESCAPES, "quote1", "quote2", true),
        (SINGLE_QUOTES, "single1", "single2", true),
        (HEX_ESCAPES, "unicode1", "unicode2", true),
        (HEX_ESCAPES, "unicode3", "unicode4", true),
        (EMPTY, "empty1", "empty2", true),
        (TYPES, "string", "number", false),
        (TYPES, "string", "sequence", false),
        (TYPES, "string", "mapping", false),
        (TYPES, "number", "sequence", false),
        (TYPES, "sequence", "mapping", false),
        // An escaped line break is dropped along with the next line's
        // indentation, but white space before it is kept.
        (FOLDING, "folded1", "folded2", true),
        (EMOJI, "emoji1", "emoji2", true),
        (NULLS, "null1", "null2", true),
        // `key:` is the same as `key: null`.
        (NULLS, "null3", "null1", true),
        (NULLS, "null3", "null2", true),
        (NULLS, "null1", "empty", false),
        (BOOLS, "true1", "true2", true),
        (BOOLS, "true1", "true3", true),
        (BOOLS, "false1", "false2", true),
        (NUMBERS, "decimal", "octal", true),
        (NUMBERS, "decimal", "hex", true),
        (NUMBERS, "octal", "hex", true),
        (ANCHORS, "original", "duplicate", true),
        (STYLES, "flow_seq", "block_seq", true),
        (STYLES, "flow_map", "block_map", true),
    ] {
        assert_eq!(eq_values(yaml, a, b), expected, "{a} vs {b} in {yaml:?}");
    }
}

/// A node equals a Rust string when it is a string with that content.
#[test]
fn yaml_eq_scalar_and_str() {
    for (yaml, key, raw, expected) in [
        (QUOTING, "plain", "value", true),
        (QUOTING, "single", "value", true),
        (QUOTING, "double", "value", true),
        (ESCAPES, "backslash1", "path\\file", true),
        (ESCAPES, "backslash2", "path\\file", true),
        (ESCAPES, "quote1", r#"say "hi""#, true),
        (SINGLE_QUOTES, "single1", "can't", true),
        (HEX_ESCAPES, "unicode1", "hello world", true),
        (HEX_ESCAPES, "unicode3", "smiley face", true),
        (CONTROL_ESCAPES, "bell", "\x07", true),
        (CONTROL_ESCAPES, "escape", "\x1B", true),
        (CONTROL_ESCAPES, "null", "\0", true),
        (CONTROL_ESCAPES, "backspace", "\x08", true),
        (CONTROL_ESCAPES, "formfeed", "\x0C", true),
        (CONTROL_ESCAPES, "carriagereturn", "\r", true),
        (CONTROL_ESCAPES, "verticaltab", "\x0B", true),
        (EMPTY, "empty1", "", true),
        // White space is kept exactly.
        (WHITESPACE, "spaces1", "  leading", true),
        (WHITESPACE, "spaces2", "trailing  ", true),
        (WHITESPACE, "spaces3", "  both  ", true),
        (WHITESPACE, "plain", "value", true),
        (WHITESPACE, "spaces1", "leading", false),
        (WHITESPACE, "spaces2", "trailing", false),
        (FOLDING, "folded1", "line1 line2", true),
        (EMOJI, "emoji1", "\u{1F600}", true),
        // Null, boolean and integer scalars are not strings.
        (NULLS, "null1", "null", false),
        (BOOLS, "true1", "true", false),
        (BOOLS, "false1", "false", false),
        (NUMBERS, "decimal", "123", false),
        (ANCHORS, "original", "value", true),
    ] {
        let node = value_of(yaml, key);
        assert_eq!(yaml_eq(&node, &raw), expected, "{key} in {yaml:?}");
    }
    for key in ["decimal", "octal", "hex"] {
        assert!(yaml_eq(&value_of(NUMBERS, key), &123), "{key}");
    }
}

/// Collections are equal when they hold equal entries: mappings in any
/// order, sequences in the same one. Comments and quoting do not matter.
#[test]
fn yaml_eq_collections() {
    let mappings = (
        r#"
a: 1
b: 2
c: 3
"#,
        r#"
a: 1
b: 2
c: 3
"#,
        r#"
a: 1
c: 3
b: 2
"#,
    );
    let sequences = (
        r#"
- one
- two
- three
"#,
        r#"
- one
- two
- three
"#,
        r#"
- one
- three
- two
"#,
    );
    let nested = (
        r#"
outer:
  inner:
    key: "value"
"#,
        r#"
outer:
  inner:
    key: 'value'
"#,
    );
    let comments = (
        r#"
# This is a comment
key: value # inline comment
"#,
        r#"
key: value
"#,
    );
    for (a, b, expected) in [
        ("{a: 1, b: 2}", "{b: 2, a: 1}", true),
        ("{a: 1, b: 2}", "{b: 1, a: 2}", false),
        ("{a: 1, a: 1}", "{a: 1, b: 1}", false),
        (mappings.0, mappings.1, true),
        (mappings.0, mappings.2, true),
        (sequences.0, sequences.1, true),
        (sequences.0, sequences.2, false),
        (nested.0, nested.1, true),
        (comments.0, comments.1, true),
    ] {
        assert_eq!(eq_roots(a, b), expected, "{a:?} vs {b:?}");
    }
    assert!(eq_across(comments.0, "key", comments.1, "key"));
    assert!(eq_across(
        "literal1: |\n  line1\n  line2\n",
        "literal1",
        "literal2: |\n  line1\n  line2\n",
        "literal2"
    ));
}
