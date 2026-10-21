//! Building mappings, sequences and documents with the builders: the text
//! they render and the trees they make, which must match what the parser
//! builds for that text.

mod common;
use common::{
    assert_cst_ok, assert_edited, assert_file_edited, assert_file_parser_shaped,
    assert_parser_shaped,
};
use std::str::FromStr;
use yaml_edit::{
    Alias, Document, MappingBuilder, ScalarType, ScalarValue, SequenceBuilder, YamlBuilder,
    YamlFile, YamlValue,
};

#[test]
fn built_mapping_behaves_like_parsed() {
    let doc = MappingBuilder::new()
        .pair("a", 1)
        .pair("b", "x")
        .build_document();
    let mapping = doc.as_mapping().unwrap();
    assert_eq!(mapping.len(), 2);
    assert_eq!(mapping.get("a").unwrap().to_i64(), Some(1));
    let keys: Vec<String> = mapping
        .keys()
        .map(|k| k.as_scalar().unwrap().as_string())
        .collect();
    assert_eq!(keys, vec!["a", "b"]);
    mapping.set("c", 3);
    assert_eq!(doc.to_string(), "a: 1\nb: x\nc: 3\n");
    assert_cst_ok(&doc);
}

#[test]
fn built_sequence_behaves_like_parsed() {
    let doc = SequenceBuilder::new().item(1).item("x").build_document();
    let seq = doc.as_sequence().unwrap();
    assert_eq!(seq.len(), 2);
    assert_eq!(seq.get(0).unwrap().to_i64(), Some(1));
    seq.push(3);
    assert_eq!(doc.to_string(), "- 1\n- x\n- 3\n");
    assert_cst_ok(&doc);
}

#[test]
fn built_nested_behaves_like_parsed() {
    let doc = MappingBuilder::new()
        .mapping("db", |m| m.pair("host", "h"))
        .sequence("deps", |s| s.item("serde"))
        .build_document();
    let db = doc.get("db").unwrap();
    assert_eq!(db.as_mapping().unwrap().len(), 1);
    db.as_mapping().unwrap().set("port", 5432);
    let deps = doc.get("deps").unwrap();
    deps.as_sequence().unwrap().push("tokio");
    assert_eq!(
        doc.to_string(),
        "db:\n  host: h\n  port: 5432\ndeps:\n  - serde\n  - tokio\n"
    );
    assert_cst_ok(&doc);
}

#[test]
fn string_keys_are_quoted_when_needed() {
    let yaml = MappingBuilder::new()
        .pair("a: b", 1)
        .pair("true", 2)
        .pair("", 3)
        .pair("multi\nline", 4)
        .build()
        .build();
    assert_eq!(
        yaml.to_string(),
        "'a: b': 1\n'true': 2\n'': 3\n\"multi\\nline\": 4\n"
    );
    assert_file_parser_shaped(&yaml);
    let doc = yaml.document().unwrap();
    let keys: Vec<String> = doc
        .as_mapping()
        .unwrap()
        .keys()
        .map(|k| k.as_scalar().unwrap().as_string())
        .collect();
    assert_eq!(keys, vec!["a: b", "true", "", "multi\nline"]);
    assert_eq!(doc.get("true").unwrap().to_i64(), Some(2));
}

#[test]
fn nested_keys_are_quoted_when_needed() {
    let yaml = MappingBuilder::new()
        .mapping("1", |m| m.pair("null", "x"))
        .sequence("- a", |s| s.item(1))
        .insert_mapping("#c", MappingBuilder::new().pair("k", "v"))
        .build()
        .build();
    assert_eq!(
        yaml.to_string(),
        "'1':\n  'null': x\n'- a':\n  - 1\n'#c':\n  k: v\n"
    );
    assert_file_parser_shaped(&yaml);
}

#[test]
fn builder_trees_match_parser() {
    let cases = vec![
        YamlBuilder::scalar("hello").build(),
        MappingBuilder::new()
            .pair("a", 1)
            .pair("b", "x")
            .build()
            .build(),
        SequenceBuilder::new().item(1).item("x").build().build(),
        MappingBuilder::new().build().build(),
        SequenceBuilder::new().build().build(),
        MappingBuilder::new()
            .mapping("empty", |m| m)
            .sequence("es", |s| s)
            .insert_mapping("m", MappingBuilder::new())
            .insert_sequence("s", SequenceBuilder::new())
            .build()
            .build(),
        SequenceBuilder::new()
            .sequence(|s| s)
            .mapping(|m| m)
            .insert_sequence(SequenceBuilder::new())
            .insert_mapping(MappingBuilder::new())
            .build()
            .build(),
        MappingBuilder::new()
            .pair("version", "1.0")
            .sequence("deps", |s| s.item("serde").item("tokio"))
            .mapping("db", |m| {
                m.pair("host", "localhost")
                    .mapping("opts", |m| m.pair("a", true).pair("b", 1.5))
            })
            .build()
            .build(),
        SequenceBuilder::new()
            .mapping(|m| m.pair("id", 1).pair("name", "Alice"))
            .sequence(|s| s.item("a").sequence(|s| s.item("b")))
            .item("x")
            .build()
            .build(),
        MappingBuilder::new()
            .sequence("list", |s| {
                s.item("simple")
                    .mapping(|m| m.pair("key", "value"))
                    .sequence(|s2| s2.item("n1").item("n2"))
            })
            .insert_mapping(
                "outer",
                MappingBuilder::new()
                    .pair("x", "1")
                    .mapping("deep", |m| m.pair("p", "9"))
                    .sequence("l", |s| s.item(1).mapping(|m| m.pair("q", 2))),
            )
            .build()
            .build(),
        SequenceBuilder::new()
            .item("first")
            .insert_sequence(SequenceBuilder::new().item("a").item("b"))
            .insert_mapping(
                MappingBuilder::new()
                    .pair("key", "value")
                    .sequence("s", |s| s.item(1)),
            )
            .build()
            .build(),
        MappingBuilder::new()
            .pair("ref", Alias::new("shared"))
            .pair("n", ScalarValue::null())
            .build()
            .build(),
    ];
    for yaml in &cases {
        assert_parser_shaped(yaml);
    }
}

#[test]
fn builder_node_values_match_parser() {
    let src = Document::from_str("item:\n  key: value\n  list:\n    - 1\n").unwrap();
    let item = src.get("item").unwrap();
    let mapping = item.as_mapping().unwrap();
    let list = mapping.get("list").unwrap();

    let yaml = SequenceBuilder::new()
        .item(mapping)
        .item(list.as_sequence().unwrap())
        .build()
        .build();
    assert_eq!(yaml.to_string(), "- key: value\n  list:\n    - 1\n- - 1\n");
    assert_file_parser_shaped(&yaml);

    let yaml = MappingBuilder::new()
        .pair("m", mapping)
        .mapping("outer", |m| m.pair("l", list.as_sequence().unwrap()))
        .build()
        .build();
    assert_eq!(
        yaml.to_string(),
        "m:\n  key: value\n  list:\n    - 1\nouter:\n  l:\n    - 1\n"
    );
    assert_file_parser_shaped(&yaml);
}

#[test]
fn sequence_builder_with_block_mappings() {
    // Parse a YAML document with duplicate keys (each has a mapping value)
    let yaml = r#"
Reference:
  Author: Stefan Kurze
  Title: Wörterbücher und Textdateien durchsuchen mit grafischem Frontend
  Journal: LinuxUser
  Year: 2003
Reference:
  Author: Michael Vogelbacher
  Title: Service und Informationen aus dem Netz
  Journal: LinuxUser
  Year: 2001
"#;

    let doc = Document::from_str(yaml).unwrap();
    let mapping = doc.as_mapping().unwrap();

    // Collect the duplicate Reference values
    let mut reference_values = Vec::new();
    for (key, value) in &mapping {
        if let Some(key_scalar) = key.as_scalar() {
            if key_scalar.as_string() == "Reference" {
                reference_values.push(value);
            }
        }
    }

    // Remove all Reference keys
    while mapping.remove("Reference").is_some() {
        assert_cst_ok(&doc);
    }

    // Create a sequence from the collected values
    let mut seq_builder = SequenceBuilder::new();
    for value in &reference_values {
        seq_builder = seq_builder.item(value);
    }
    let seq_doc = seq_builder.build_document();

    // Set the sequence back
    if let Some(seq) = seq_doc.as_sequence() {
        mapping.set("Reference", seq);
        assert_cst_ok(&doc);
    }

    // Each mapping item starts with a dash at the base indent, with the
    // mapping content indented under it.
    let expected = r#"
Reference:
- Author: Stefan Kurze
  Title: Wörterbücher und Textdateien durchsuchen mit grafischem Frontend
  Journal: LinuxUser
  Year: 2003
- Author: Michael Vogelbacher
  Title: Service und Informationen aus dem Netz
  Journal: LinuxUser
  Year: 2001
"#;

    assert_edited(&doc, expected);
}

#[test]
fn sequence_builder_simple_mapping() {
    let yaml = r#"
item:
  key: value
  foo: bar
"#;

    let doc = Document::from_str(yaml).unwrap();
    let mapping = doc.as_mapping().unwrap();
    let item_value = mapping.get("item").unwrap();
    let item_mapping = item_value.as_mapping().unwrap();

    // Create a sequence with this mapping
    let seq = SequenceBuilder::new().item(item_mapping).build_document();
    assert_cst_ok(&seq);

    assert_edited(&seq, "- key: value\n  foo: bar\n");
}

#[test]
fn trailing_block_scalar_keeps_line_break() {
    let yaml = YamlBuilder::mapping().pair("k", "a\n").build().build();
    assert_cst_ok(&yaml);
    assert_file_parser_shaped(&yaml);
    assert_eq!(yaml.to_string(), "k: |\n  a\n");
    let doc = Document::from_str(&yaml.to_string()).unwrap();
    assert_eq!(
        doc.get("k").unwrap().as_scalar().unwrap().as_string(),
        "a\n"
    );

    let yaml = YamlBuilder::sequence().item("a\n\n").build().build();
    assert_cst_ok(&yaml);
    assert_file_parser_shaped(&yaml);
    assert_eq!(yaml.to_string(), "- |+\n  a\n\n");
    let doc = Document::from_str(&yaml.to_string()).unwrap();
    let seq = doc.as_sequence().unwrap();
    assert_eq!(
        seq.get(0).unwrap().as_scalar().unwrap().as_string(),
        "a\n\n"
    );

    let yaml = YamlBuilder::mapping()
        .mapping("n", |m| m.pair("k", "  a\nb"))
        .build()
        .build();
    assert_cst_ok(&yaml);
    assert_file_parser_shaped(&yaml);
    assert_eq!(yaml.to_string(), "n:\n  k: |2-\n      a\n    b\n");
}

/// The first key of `yaml` as read back by the parser, with its type.
fn first_key(yaml: &YamlFile) -> (String, ScalarType) {
    let reparsed = Document::from_str(&yaml.to_string()).unwrap();
    let mapping = reparsed.as_mapping().unwrap();
    let (key, _) = mapping.iter().next().unwrap();
    let key = key.as_scalar().unwrap();
    (key.as_string(), ScalarValue::from_scalar(key).scalar_type())
}

#[test]
fn pair_quotes_keys() {
    let yaml = YamlBuilder::mapping().pair("a: b", 1).build().build();
    assert_eq!(yaml.to_string(), "'a: b': 1\n");

    for key in [
        "a: b", "true", "null", "1.0", "0x10", "", "#x", "a #b", "a\nb",
    ] {
        let yaml = YamlBuilder::mapping().pair(key, 1).build().build();
        assert_eq!(
            first_key(&yaml),
            (key.to_string(), ScalarType::String),
            "{yaml}"
        );
        let reparsed = Document::from_str(&yaml.to_string()).unwrap();
        assert_eq!(
            reparsed.get(key).and_then(|v| v.to_i64()),
            Some(1),
            "{yaml}"
        );
    }
}

#[test]
fn nested_collection_keys_quoted() {
    let yaml = YamlBuilder::mapping()
        .sequence("true", |s| s.item(1))
        .build()
        .build();
    assert_eq!(first_key(&yaml).0, "true");
    assert_eq!(first_key(&yaml).1, ScalarType::String);

    let yaml = YamlBuilder::mapping()
        .mapping("#x", |m| m.pair("a", 1))
        .build()
        .build();
    assert_eq!(first_key(&yaml).0, "#x");

    let yaml = YamlBuilder::mapping()
        .insert_sequence("a: b", SequenceBuilder::new().item(1))
        .build()
        .build();
    assert_eq!(first_key(&yaml).0, "a: b");

    let yaml = YamlBuilder::mapping()
        .insert_mapping("null", MappingBuilder::new().pair("a", 1))
        .build()
        .build();
    assert_eq!(first_key(&yaml).1, ScalarType::String);
}

#[test]
fn block_nodes_as_items_and_values() {
    let source = Document::from_str("k: |\n  text\nt: !t\n  a: 1\n").unwrap();
    let mapping = source.as_mapping().unwrap();
    let literal = mapping.get("k").unwrap();
    let tagged = mapping.get("t").unwrap();
    for (file, expected) in [
        (
            SequenceBuilder::new()
                .item(literal.clone())
                .item(tagged.clone())
                .item("x")
                .build()
                .build(),
            "- |\n  text\n- !t\n  a: 1\n- x\n",
        ),
        (
            MappingBuilder::new()
                .pair("a", literal.clone())
                .pair("b", tagged.clone())
                .build()
                .build(),
            "a: |\n  text\nb: !t\n  a: 1\n",
        ),
        (
            MappingBuilder::new()
                .mapping("m", |m| m.pair("x", literal.clone()))
                .build()
                .build(),
            "m:\n  x: |\n    text\n",
        ),
        (
            SequenceBuilder::new()
                .mapping(|m| m.pair("x", tagged.clone()).pair("y", 2))
                .build()
                .build(),
            "-\n  x: !t\n    a: 1\n  y: 2\n",
        ),
    ] {
        assert_eq!(file.to_string(), expected);
        assert_file_parser_shaped(&file);
    }
}

#[test]
fn raw_sequence_values() {
    let items: Vec<YamlValue> = vec![YamlValue::from(1i64), YamlValue::from("a b")];
    for (file, expected) in [
        (
            MappingBuilder::new()
                .pair("v", items.clone())
                .pair("e", Vec::<YamlValue>::new())
                .build()
                .build(),
            "v:\n  - 1\n  - a b\ne: []\n",
        ),
        (
            SequenceBuilder::new().item(items).build().build(),
            "- - 1\n  - a b\n",
        ),
    ] {
        assert_eq!(file.to_string(), expected);
        assert_file_parser_shaped(&file);
    }
}

#[test]
fn built_files_render() {
    for (file, expected) in [
        (YamlBuilder::scalar("hello world").build(), "hello world\n"),
        (YamlBuilder::alias("shared").build(), "*shared\n"),
        (
            YamlBuilder::mapping()
                .pair("ref", Alias::new("shared"))
                .build()
                .build(),
            "ref: *shared\n",
        ),
        (
            YamlBuilder::sequence()
                .item("first")
                .item("second")
                .item("third")
                .build()
                .build(),
            "- first\n- second\n- third\n",
        ),
        (
            YamlBuilder::mapping()
                .pair("name", "John Doe")
                .pair("age", "30")
                .pair("city", "New York")
                .build()
                .build(),
            "name: John Doe\nage: '30'\ncity: New York\n",
        ),
        (
            YamlBuilder::mapping()
                .pair("version", "1.0")
                .sequence("dependencies", |s| {
                    s.item("serde").item("tokio").item("reqwest")
                })
                .mapping("database", |m| {
                    m.pair("host", "localhost")
                        .pair("port", "5432")
                        .pair("name", "myapp")
                })
                .build()
                .build(),
            "version: '1.0'\ndependencies:\n  - serde\n  - tokio\n  - reqwest\ndatabase:\n  host: localhost\n  port: '5432'\n  name: myapp\n",
        ),
        (
            YamlBuilder::mapping()
                .mapping("level1", |m| {
                    m.mapping("level2", |m| {
                        m.mapping("level3", |m| m.pair("deep", "value"))
                    })
                })
                .build()
                .build(),
            "level1:\n  level2:\n    level3:\n      deep: value\n",
        ),
        // Empty collections serialize to the flow-empty form so that
        // placing them under a key doesn't reparse as null (issue #37).
        (YamlBuilder::sequence().build().build(), "[]\n"),
        (YamlBuilder::mapping().build().build(), "{}\n"),
        (
            YamlBuilder::mapping()
                .mapping("empty", |m| m)
                .sequence("es", |s| s)
                .build()
                .build(),
            "empty: {}\nes: []\n",
        ),
        (
            YamlBuilder::sequence()
                .sequence(|s| s)
                .mapping(|m| m)
                .build()
                .build(),
            "- []\n- {}\n",
        ),
        (
            YamlBuilder::mapping()
                .insert_mapping("m", MappingBuilder::new())
                .insert_sequence("s", SequenceBuilder::new())
                .build()
                .build(),
            "m: {}\ns: []\n",
        ),
        (
            YamlBuilder::sequence()
                .insert_sequence(SequenceBuilder::new())
                .insert_mapping(MappingBuilder::new())
                .build()
                .build(),
            "- []\n- {}\n",
        ),
        (YamlBuilder::default().build(), "{}\n"),
        (SequenceBuilder::default().build().build(), "[]\n"),
        (MappingBuilder::default().build().build(), "{}\n"),
        (YamlBuilder::sequence().item(1).build().build(), "- 1\n"),
        (
            YamlBuilder::mapping().pair("a", 1).build().build(),
            "a: 1\n",
        ),
        (
            YamlBuilder::mapping()
                .pair("url", "https://example.com:8080/path?query=value")
                .pair("email", "user@example.com")
                .pair("path", "/usr/local/bin")
                .pair("special", "value: with: colons")
                .build()
                .build(),
            "url: https://example.com:8080/path?query=value\nemail: user@example.com\npath: /usr/local/bin\nspecial: 'value: with: colons'\n",
        ),
        (
            YamlBuilder::mapping()
                .pair("int_string", "42")
                .pair("float_string", "3.14")
                .pair("hex_string", "0xFF")
                .pair("octal_string", "0o755")
                .pair("binary_string", "0b1010")
                .build()
                .build(),
            "int_string: '42'\nfloat_string: '3.14'\nhex_string: '0xFF'\noctal_string: '0o755'\nbinary_string: '0b1010'\n",
        ),
        (
            YamlBuilder::mapping()
                .pair("bool_true", "true")
                .pair("bool_false", "false")
                .pair("yes", "yes")
                .pair("no", "no")
                .pair("null_value", "null")
                .pair("tilde", "~")
                .build()
                .build(),
            "bool_true: 'true'\nbool_false: 'false'\n'yes': 'yes'\n'no': 'no'\nnull_value: 'null'\ntilde: '~'\n",
        ),
        (
            YamlBuilder::mapping()
                .pair("emoji", "🎉🚀💻")
                .pair("chinese", "你好世界")
                .pair("arabic", "مرحبا بالعالم")
                .pair("mixed", "Hello 世界 🌍")
                .build()
                .build(),
            "emoji: 🎉🚀💻\nchinese: 你好世界\narabic: مرحبا بالعالم\nmixed: Hello 世界 🌍\n",
        ),
        (
            YamlBuilder::mapping()
                .pair("port", 5432_i64)
                .pair("debug", true)
                .pair("ratio", 1.5_f64)
                .build()
                .build(),
            "port: 5432\ndebug: true\nratio: 1.5\n",
        ),
        (
            YamlBuilder::sequence()
                .mapping(|m| m.pair("id", "1").pair("name", "Alice"))
                .mapping(|m| m.pair("id", "2").pair("name", "Bob"))
                .mapping(|m| m.pair("id", "3").pair("name", "Charlie"))
                .build()
                .build(),
            "-\n  id: '1'\n  name: Alice\n-\n  id: '2'\n  name: Bob\n-\n  id: '3'\n  name: Charlie\n",
        ),
        (
            YamlBuilder::sequence()
                .sequence(|s| s.item("1").item("2").item("3"))
                .sequence(|s| s.item("a").item("b").item("c"))
                .sequence(|s| s.item("x").item("y").item("z"))
                .build()
                .build(),
            "-\n  - '1'\n  - '2'\n  - '3'\n-\n  - a\n  - b\n  - c\n-\n  - x\n  - y\n  - z\n",
        ),
        (
            YamlBuilder::mapping()
                .sequence("list", |s| {
                    s.item("simple")
                        .mapping(|m| m.pair("key", "value"))
                        .sequence(|s2| s2.item("nested1").item("nested2"))
                })
                .mapping("object", |m| {
                    m.pair("simple", "value")
                        .sequence("list", |s| s.item("item1").item("item2"))
                        .mapping("nested", |m2| m2.pair("deep", "value"))
                })
                .build()
                .build(),
            "list:\n  - simple\n  -\n    key: value\n  -\n    - nested1\n    - nested2\nobject:\n  simple: value\n  list:\n    - item1\n    - item2\n  nested:\n    deep: value\n",
        ),
        (
            YamlBuilder::sequence()
                .sequence(|s| s.item(1))
                .item("x")
                .build()
                .build(),
            "-\n  - 1\n- x\n",
        ),
    ] {
        assert_file_edited(&file, expected);
    }

    let long = YamlBuilder::mapping()
        .pair("short", "test")
        .pair("long", "a".repeat(100))
        .build()
        .build();
    assert_file_edited(&long, &format!("short: test\nlong: {}\n", "a".repeat(100)));
}

#[test]
fn built_documents_render() {
    for (doc, expected) in [
        (
            YamlBuilder::sequence()
                .item("first")
                .item("second")
                .build_document(),
            "- first\n- second\n",
        ),
        (
            YamlBuilder::mapping()
                .pair("name", "test")
                .pair("version", "1.0")
                .build_document(),
            "name: test\nversion: '1.0'\n",
        ),
        (
            YamlBuilder::mapping()
                .pair("name", "my-app")
                .sequence("dependencies", |s| s.item("serde").item("tokio"))
                .build_document(),
            "name: my-app\ndependencies:\n  - serde\n  - tokio\n",
        ),
        (
            YamlBuilder::mapping()
                .pair("name", "my-app")
                .mapping("database", |m| {
                    m.pair("host", "localhost").pair("port", 5432)
                })
                .build_document(),
            "name: my-app\ndatabase:\n  host: localhost\n  port: 5432\n",
        ),
        (
            YamlBuilder::sequence()
                .item("first")
                .sequence(|s| s.item("a").item("b"))
                .mapping(|m| m.pair("key", "value"))
                .build_document(),
            "- first\n-\n  - a\n  - b\n-\n  key: value\n",
        ),
        // The donor's own nested content is already indented relative to
        // the donor; re-indenting must shift it as a block, not flatten it.
        (
            YamlBuilder::mapping()
                .insert_mapping(
                    "outer",
                    MappingBuilder::new()
                        .pair("x", "1")
                        .mapping("deep", |m| m.pair("p", "9")),
                )
                .build_document(),
            "outer:\n  x: '1'\n  deep:\n    p: '9'\n",
        ),
        (
            YamlBuilder::mapping()
                .pair("version", "1.0")
                .pair("name", "my-application")
                .mapping("database", |m| {
                    m.pair("host", "localhost")
                        .pair("port", 5432)
                        .pair("name", "myapp")
                })
                .sequence("dependencies", |s| {
                    s.item("serde").item("tokio").item("reqwest")
                })
                .build_document(),
            "version: '1.0'\nname: my-application\ndatabase:\n  host: localhost\n  port: 5432\n  name: myapp\ndependencies:\n  - serde\n  - tokio\n  - reqwest\n",
        ),
    ] {
        assert_edited(&doc, expected);
    }
}

/// Inserting a pre-built collection indents it as building it in place
/// with a closure does.
#[test]
fn insert_indents_like_closure() {
    for (inserted, closure, expected) in [
        (
            YamlBuilder::mapping()
                .pair("name", "my-app")
                .insert_sequence(
                    "dependencies",
                    SequenceBuilder::new().item("serde").item("tokio"),
                )
                .build_document(),
            YamlBuilder::mapping()
                .pair("name", "my-app")
                .sequence("dependencies", |s| s.item("serde").item("tokio"))
                .build_document(),
            "name: my-app\ndependencies:\n  - serde\n  - tokio\n",
        ),
        (
            YamlBuilder::mapping()
                .pair("name", "my-app")
                .insert_mapping(
                    "database",
                    MappingBuilder::new()
                        .pair("host", "localhost")
                        .pair("port", 5432),
                )
                .build_document(),
            YamlBuilder::mapping()
                .pair("name", "my-app")
                .mapping("database", |m| {
                    m.pair("host", "localhost").pair("port", 5432)
                })
                .build_document(),
            "name: my-app\ndatabase:\n  host: localhost\n  port: 5432\n",
        ),
        (
            YamlBuilder::sequence()
                .item("first")
                .insert_sequence(SequenceBuilder::new().item("a").item("b"))
                .insert_mapping(MappingBuilder::new().pair("key", "value"))
                .build_document(),
            YamlBuilder::sequence()
                .item("first")
                .sequence(|s| s.item("a").item("b"))
                .mapping(|m| m.pair("key", "value"))
                .build_document(),
            "- first\n-\n  - a\n  - b\n-\n  key: value\n",
        ),
    ] {
        assert_edited(&inserted, expected);
        assert_edited(&closure, expected);
    }
}
