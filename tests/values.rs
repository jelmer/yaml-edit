//! Plain values: reading documents into `YamlValue`, writing values and
//! other `AsYaml` implementations back as YAML, and comparing nodes by
//! their meaning with `yaml_eq` and `yaml_hash`.

mod common;
use common::{assert_cst_ok, assert_parser_shaped, check_edit};
use rowan::ast::AstNode;
use std::collections::{BTreeMap, BTreeSet};
use std::str::FromStr;
use yaml_edit::{
    AsYaml, Document, DocumentResolvedExt, Lang, ResolveError, Scalar, ScalarStyle, ScalarType,
    ScalarValue, SyntaxKind, YamlError, YamlFile, YamlValue,
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
