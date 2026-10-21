//! Reading, setting and removing values by path: descending through
//! mappings, sequences, tags, aliases and merge keys, and the errors when a
//! path cannot be followed.

mod common;
use common::{assert_cst_ok, assert_edited};
use rowan::ast::AstNode;
use std::str::FromStr;
use yaml_edit::path::{AliasPolicy, PathError, PathSegment, YamlPath, MAX_INDEX_GROWTH};
use yaml_edit::{AsYaml, Document, YamlFile};

#[test]
fn set_path_bounds_sequence_growth() {
    // Filling the gap to an out-of-range index used to be unbounded, so
    // a single path could build an enormous document: the fuzz-found
    // "999.hh.hh.999.9aa" spent ~2s producing 13KB from a 6-byte seed.
    let doc = Document::from_str("m: {}\n").unwrap();
    let err = doc
        .try_set_path("999.hh.hh.999.9aa", "")
        .expect_err("a far index must be refused");
    assert_cst_ok(&doc);
    assert!(
        matches!(err, PathError::IndexTooFar { .. }),
        "unexpected error: {err}"
    );
    // Nothing was written on the way to refusing.
    assert!(doc.to_string().len() < 64, "{:?}", doc.to_string());

    // Growth within the limit still works: writing s[5] on an empty
    // sequence is a normal way to build one.
    let doc = Document::from_str("m: {}\n").unwrap();
    doc.try_set_path("s[5]", "v").expect("small index is fine");
    assert_cst_ok(&doc);
    assert_eq!(doc.try_get_path("s[5]").unwrap().to_string(), "v");

    let doc = Document::from_str("m: {}\n").unwrap();
    assert!(matches!(
        doc.try_set_path(&format!("s[{MAX_INDEX_GROWTH}]"), "v"),
        Err(PathError::IndexTooFar { .. })
    ));
    assert_cst_ok(&doc);
}

#[test]
fn set_path_refused_far_index_writes_nothing() {
    for (source, path) in [
        ("s: [1]\n", "s[1][300]"),
        ("s: [1]\n", "s[3].k[300]"),
        ("s:\n- 1\n", "s[1][300]"),
        ("m: {}\n", "m.k[300]"),
        ("m: {}\n", "m.k.j[300]"),
        ("m: {}\n", "m.k[1][300]"),
    ] {
        let doc = Document::from_str(source).unwrap();
        assert_eq!(
            doc.try_set_path(path, "x"),
            Err(PathError::IndexTooFar {
                at: "[300]".to_string(),
                len: 0,
                index: 300,
            }),
            "{path:?}"
        );
        assert_eq!(doc.to_string(), source, "{path:?}");
    }
}

#[test]
fn remove_path_on_scalar_root_is_a_type_mismatch() {
    for source in ["42\n", "!!str x\n"] {
        let doc = Document::from_str(source).unwrap();
        for (path, at) in [("a", "a"), ("[0]", "[0]"), ("a.b", "a")] {
            let expected = Err(PathError::TypeMismatch { at: at.to_string() });
            assert_eq!(doc.try_remove_path(path), expected, "{source:?} {path:?}");
            assert_eq!(
                doc.try_get_path(path).map(|_| ()),
                expected.map(|_: yaml_edit::YamlNode| ()),
                "{source:?} {path:?}"
            );
        }
        assert_eq!(doc.to_string(), source);
    }
}

fn merge_doc() -> yaml_edit::YamlFile {
    yaml_edit::YamlFile::from_str(
        "base: &b\n  a: 1\n  shared: x\nderived:\n  <<: *b\n  own: 2\n  a: 99\n",
    )
    .unwrap()
}

#[test]
fn get_path_resolves_merged_keys() {
    let file = merge_doc();
    let doc = file.document().unwrap();

    // Inherited through `<<: *b`.
    assert_eq!(doc.try_get_path("derived.shared").unwrap().to_string(), "x");
    // A local entry still wins over the merged one.
    assert_eq!(doc.try_get_path("derived.a").unwrap().to_string(), "99");
    // Plain local key, and the anchor's own mapping, are unaffected.
    assert_eq!(doc.try_get_path("derived.own").unwrap().to_string(), "2");
    assert_eq!(doc.try_get_path("base.a").unwrap().to_string(), "1");
}

#[test]
fn get_path_still_reports_genuinely_absent_keys() {
    let file = merge_doc();
    let doc = file.document().unwrap();
    assert!(doc.try_get_path("derived.nope").is_err());
}

#[test]
fn remove_path_stays_literal_for_merged_keys() {
    // An inherited key lives in the anchor, not in the mapping being
    // walked, so it must not look removable from `derived`.
    let file = merge_doc();
    let before = file.to_string();
    let doc = file.document().unwrap();
    assert!(doc.try_remove_path("derived.shared").is_err());
    assert_cst_ok(&doc);
    assert_eq!(file.to_string(), before);
}

#[test]
fn get_path_with_escaping() {
    let doc = Document::new();
    doc.set("key.with.dots", "test value");
    assert_cst_ok(&doc);

    // Without escaping - should not find it (looking for nested keys)
    assert!(doc.try_get_path("key.with.dots").is_err());

    // With escaping - should find it
    let value = doc.try_get_path("key\\.with\\.dots").ok();
    assert_eq!(
        value
            .as_ref()
            .and_then(|v| v.as_scalar())
            .map(|s| s.as_string()),
        Some("test value".to_string())
    );
}

#[test]
fn remove_path_with_array_index() {
    let yaml = r#"
items:
  - name: first
    nested:
      key: value
"#;
    let doc = Document::from_str(yaml).unwrap();

    // Remove nested key inside array element
    assert!(doc.try_remove_path("items[0].nested.key").is_ok());
    assert_cst_ok(&doc);
    assert!(doc.try_get_path("items[0].nested.key").is_err());

    // The nested mapping should still exist but be empty
    assert!(doc.try_get_path("items[0].nested").is_ok());
}

#[test]
fn get_path_missing() {
    let yaml = Document::from_str("name: Alice\n").unwrap();

    assert!(matches!(
        yaml.try_get_path("missing"),
        Err(PathError::NotFound { .. })
    ));
    // `name` is a scalar, `.nested` tries to descend into it.
    assert!(matches!(
        yaml.try_get_path("name.nested"),
        Err(PathError::TypeMismatch { .. })
    ));
    assert_eq!(yaml.try_get_path(""), Err(PathError::EmptyPath));
}

#[test]
fn set_path_deeply_nested_create() {
    let yaml = Document::from_str("app: {}\n").unwrap();

    yaml.try_set_path("app.database.primary.host", "db.example.com")
        .expect("set_path");
    assert_cst_ok(&yaml);
    yaml.try_set_path("app.database.primary.port", 5432)
        .expect("set_path");
    assert_cst_ok(&yaml);

    // Parent was flow-style, so the whole nested chain stays flow.
    assert_eq!(
        yaml.to_string().trim(),
        "app: {database: {primary: {host: db.example.com, port: 5432}}}"
    );

    let host = yaml.try_get_path("app.database.primary.host").ok();
    assert_eq!(
        host.as_ref()
            .and_then(|v| v.as_scalar())
            .map(|s| s.as_string()),
        Some("db.example.com".to_string())
    );

    let port = yaml.try_get_path("app.database.primary.port").ok();
    assert_eq!(port.as_ref().and_then(|v| v.to_i64()), Some(5432));
}

#[test]
fn path_on_mapping_directly() {
    let yaml = Document::from_str("server:\n  host: localhost\n").unwrap();
    let mapping = yaml.as_mapping().unwrap();

    // Get from mapping
    let host = mapping.try_get_path("server.host").ok();
    assert_eq!(
        host.as_ref()
            .and_then(|v| v.as_scalar())
            .map(|s| s.to_string()),
        Some("localhost".to_string())
    );

    // Set on mapping
    mapping.try_set_path("server.port", 8080).expect("set_path");
    assert_cst_ok(&yaml);
    assert_eq!(
        yaml.to_string(),
        "server:\n  host: localhost\n  port: 8080\n"
    );

    // Remove from mapping
    let result = mapping.try_remove_path("server.port").is_ok();
    assert_cst_ok(&yaml);
    assert!(result);

    // Try to remove non-existent path from mapping
    let result_missing = mapping.try_remove_path("nonexistent.path").is_ok();
    assert_cst_ok(&yaml);
    assert!(!result_missing);
}

#[test]
fn mapping_remove_path_single_segment() {
    let yaml = Document::from_str("a: 1\nb: 2\n").unwrap();
    let mapping = yaml.as_mapping().unwrap();

    // A single-segment path removes the key directly from the mapping.
    assert!(mapping.try_remove_path("a").is_ok());
    assert_cst_ok(&yaml);
    assert!(mapping.try_get_path("a").is_err());
    assert!(mapping.try_get_path("b").is_ok());

    // Removing a missing single-segment key returns false.
    assert!(mapping.try_remove_path("missing").is_err());
    assert_cst_ok(&yaml);
}

#[test]
fn multiple_path_operations() {
    let yaml = Document::from_str("name: test\n").unwrap();

    // Create nested structure
    yaml.try_set_path("server.host", "localhost")
        .expect("set_path");
    assert_cst_ok(&yaml);
    yaml.try_set_path("server.port", 8080).expect("set_path");
    assert_cst_ok(&yaml);
    yaml.try_set_path("database.host", "db.local")
        .expect("set_path");
    assert_cst_ok(&yaml);
    yaml.try_set_path("database.port", 5432).expect("set_path");
    assert_cst_ok(&yaml);

    // Verify all values
    assert_eq!(
        yaml.try_get_path("server.host")
            .ok()
            .as_ref()
            .and_then(|v| v.as_scalar())
            .map(|s| s.to_string()),
        Some("localhost".to_string())
    );
    assert_eq!(
        yaml.try_get_path("server.port")
            .ok()
            .as_ref()
            .and_then(|v| v.as_scalar())
            .map(|s| s.to_string()),
        Some("8080".to_string())
    );
    assert_eq!(
        yaml.try_get_path("database.host")
            .ok()
            .as_ref()
            .and_then(|v| v.as_scalar())
            .map(|s| s.to_string()),
        Some("db.local".to_string())
    );
    assert_eq!(
        yaml.try_get_path("database.port")
            .ok()
            .as_ref()
            .and_then(|v| v.as_scalar())
            .map(|s| s.to_string()),
        Some("5432".to_string())
    );

    // Remove some values
    yaml.try_remove_path("server.port").expect("remove_path");
    assert_cst_ok(&yaml);
    yaml.try_remove_path("database.host").expect("remove_path");
    assert_cst_ok(&yaml);

    // Verify removals
    assert!(yaml.try_get_path("server.port").is_err());
    assert!(yaml.try_get_path("database.host").is_err());

    // Verify remaining values still exist
    assert_eq!(
        yaml.try_get_path("server.host")
            .ok()
            .as_ref()
            .and_then(|v| v.as_scalar())
            .map(|s| s.to_string()),
        Some("localhost".to_string())
    );
    assert_eq!(
        yaml.try_get_path("database.port")
            .ok()
            .as_ref()
            .and_then(|v| v.as_scalar())
            .map(|s| s.to_string()),
        Some("5432".to_string())
    );
}

#[test]
fn set_path_does_not_replace_block_scalar() {
    // Block scalars are strings, never null placeholders.
    for (yaml, path) in [
        ("a: |\n", "a.b"),
        ("a: >-\n  null\n", "a.b"),
        ("items:\n  - |-\n    ~\n", "items[0].b"),
    ] {
        let doc = Document::from_str(yaml).unwrap();
        let err = doc.try_set_path(path, "x").unwrap_err();
        assert_cst_ok(&doc);
        assert!(
            matches!(err, PathError::TypeMismatch { .. }),
            "{:?}: {:?}",
            yaml,
            err
        );
        assert_eq!(doc.to_string(), yaml);
    }
}

#[test]
fn set_path_replaces_null_placeholder() {
    for yaml in ["a:\n", "a: null\n", "a: ~\n"] {
        let doc = Document::from_str(yaml).unwrap();
        doc.try_set_path("a.b", "x").expect("set_path");
        assert_cst_ok(&doc);
        assert_eq!(
            doc.try_get_path("a.b")
                .unwrap()
                .as_scalar()
                .unwrap()
                .as_string(),
            "x"
        );
    }
}

#[test]
fn set_path_index_does_not_replace_existing_mapping() {
    let doc = Document::from_str("m:\n  a: 1\n  b: 2\n").unwrap();
    doc.try_set_path("m[0]", "z").expect("set_path");
    assert_cst_ok(&doc);
    assert_eq!(doc.to_string(), "m:\n  a: 1\n  b: 2\n  '0': z\n");

    let doc = Document::from_str("m:\n  \"0\":\n    x: 1\n").unwrap();
    assert_eq!(
        doc.try_get_path("m.0.x")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "1"
    );
    doc.try_set_path("m.0.x", "2").expect("set_path");
    assert_cst_ok(&doc);
    assert_eq!(
        doc.try_get_path("m.0.x")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "2"
    );
    assert!(doc.get_mapping("m").is_some());

    let doc = Document::from_str("items:\n  - a: 1\n    b: 2\n").unwrap();
    doc.try_set_path("items[0][0]", "z").expect("set_path");
    assert_cst_ok(&doc);
    assert_eq!(doc.to_string(), "items:\n  - a: 1\n    b: 2\n    '0': z\n");
}

#[test]
fn set_path_key_does_not_replace_existing_sequence() {
    let original = "items:\n- a\n- b\n";
    let doc = Document::from_str(original).unwrap();
    assert!(matches!(
        doc.try_get_path("items.foo"),
        Err(PathError::TypeMismatch { .. })
    ));
    assert!(matches!(
        doc.try_set_path("items.foo", "x"),
        Err(PathError::TypeMismatch { .. })
    ));
    assert_cst_ok(&doc);
    assert_eq!(doc.to_string(), original);

    // An alias is refused rather than silently overwritten; the
    // dedicated error says why (see the alias policy tests below).
    let original = "other: &o\n  a: 1\nitems: *o\n";
    let doc = Document::from_str(original).unwrap();
    assert_eq!(
        doc.try_set_path("items.foo", "x"),
        Err(PathError::AliasRefused {
            at: "items".to_string(),
            alias: "o".to_string()
        })
    );
    assert_cst_ok(&doc);
    assert_eq!(doc.to_string(), original);
}

#[test]
fn set_path_index_does_not_replace_alias() {
    let original = "other: &o\n- a\n- b\nitems: *o\n";
    let doc = Document::from_str(original).unwrap();
    assert_eq!(
        doc.try_get_path("items[0]").unwrap().to_string(),
        "a".to_string()
    );
    assert_eq!(
        doc.try_set_path("items[0]", "x"),
        Err(PathError::AliasRefused {
            at: "items".to_string(),
            alias: "o".to_string()
        })
    );
    assert_cst_ok(&doc);
    assert_eq!(doc.to_string(), original);

    // A tagged sequence is a sequence wearing a tag: descend into it
    // and leave the tag alone (#88).
    let doc = Document::from_str("items: !!seq [1, 2]\n").unwrap();
    doc.try_set_path("items[1]", "x").unwrap();
    assert_cst_ok(&doc);
    assert_eq!(doc.to_string(), "items: !!seq [1, x]\n");

    let original = "other: &o\n  a: 1\ntop:\n  - *o\n";
    let doc = Document::from_str(original).unwrap();
    assert_eq!(
        doc.try_set_path("top[0][0]", "x"),
        Err(PathError::AliasRefused {
            at: "[0]".to_string(),
            alias: "o".to_string()
        })
    );
    assert_cst_ok(&doc);
    assert_eq!(doc.to_string(), original);

    // A tagged mapping behaves like the mapping it wraps, so the
    // numeric-on-mapping fallback applies and `[0]` addresses key "0".
    let doc = Document::from_str("top:\n  - !custom\n    a: 1\n").unwrap();
    doc.try_set_path("top[0][0]", "x").unwrap();
    assert_cst_ok(&doc);
    assert_eq!(doc.to_string(), "top:\n  - !custom\n    a: 1\n    '0': x\n");
}

#[test]
fn path_descends_into_tagged_mapping() {
    let doc = Document::from_str("config: !custom\n  a: 1\n  b: 2\n").unwrap();
    assert_eq!(
        doc.try_get_path("config.a")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "1"
    );

    doc.try_set_path("config.a", 5).unwrap();
    assert_cst_ok(&doc);
    doc.try_set_path("config.c", 7).unwrap();
    assert_cst_ok(&doc);
    assert_eq!(doc.to_string(), "config: !custom\n  a: 5\n  b: 2\n  c: 7\n");
}

#[test]
fn path_descends_into_tagged_sequence() {
    let doc = Document::from_str("items: !!seq [1, 2]\n").unwrap();
    assert_eq!(
        doc.try_get_path("items[0]")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "1"
    );

    doc.try_set_path("items[0]", 9).unwrap();
    assert_cst_ok(&doc);
    doc.try_set_path("items[2]", 7).unwrap();
    assert_cst_ok(&doc);
    assert_eq!(doc.to_string(), "items: !!seq [9, 2, 7]\n");
}

#[test]
fn path_descends_through_nested_tags() {
    let doc = Document::from_str("a: !outer\n  b: !inner\n    c: 1\n").unwrap();
    doc.try_set_path("a.b.c", 2).unwrap();
    assert_cst_ok(&doc);
    assert_eq!(doc.to_string(), "a: !outer\n  b: !inner\n    c: 2\n");

    let doc = Document::from_str("l: !t\n  - x: 1\n").unwrap();
    doc.try_set_path("l[0].x", 9).unwrap();
    assert_cst_ok(&doc);
    assert_eq!(doc.to_string(), "l: !t\n  - x: 9\n");
}

#[test]
fn remove_path_in_tagged_mapping_keeps_tag() {
    let doc = Document::from_str("config: !custom\n  a: 1\n  b: 2\n").unwrap();
    let removed = doc.try_remove_path("config.b").unwrap();
    assert_cst_ok(&doc);
    assert_eq!(removed.as_scalar().unwrap().as_string(), "2");
    assert_eq!(doc.to_string(), "config: !custom\n  a: 1\n");

    // Draining the last entry collapses to the flow-empty form, as it
    // does for an untagged mapping, but keeps the tag.
    doc.try_remove_path("config.a").unwrap();
    assert_cst_ok(&doc);
    assert_eq!(doc.to_string(), "config: !custom {}\n");
}

#[test]
fn path_does_not_descend_into_tagged_scalar() {
    let original = "config: !custom hello\n";
    let doc = Document::from_str(original).unwrap();
    assert!(matches!(
        doc.try_get_path("config.a"),
        Err(PathError::TypeMismatch { .. })
    ));
    assert!(matches!(
        doc.try_set_path("config.a", 1),
        Err(PathError::TypeMismatch { .. })
    ));
    assert_cst_ok(&doc);
    assert_eq!(doc.to_string(), original);
}

#[test]
fn set_path_does_not_replace_null_lookalike_strings() {
    for original in [
        "a: \"null\"\n",
        "a: 'null'\n",
        "a: ''\n",
        "a: \"~\"\n",
        "a: nUlL\n",
    ] {
        let doc = Document::from_str(original).unwrap();
        assert_eq!(
            doc.try_set_path("a.b", "v"),
            Err(PathError::TypeMismatch { at: "a".into() }),
            "{original:?}"
        );
        assert_eq!(doc.to_string(), original);
    }

    for original in ["items:\n  - 'null'\n", "items:\n  - nUlL\n"] {
        let doc = Document::from_str(original).unwrap();
        assert_eq!(
            doc.try_set_path("items[0].b", "v"),
            Err(PathError::TypeMismatch { at: "[0]".into() }),
            "{original:?}"
        );
        assert_eq!(doc.to_string(), original);
    }
}

#[test]
fn set_path_replaces_null_placeholders() {
    for null in ["null", "Null", "NULL", "~", ""] {
        let doc = Document::from_str(&format!("a: {null}\n")).unwrap();
        doc.try_set_path("a.b", "v").unwrap();
        assert_eq!(doc.to_string(), "a:\n  b: v\n", "{null:?}");
    }
}

const ALIASED: &str = "shared: &shared\n  timeout: 30\nservice: *shared\n";

#[test]
fn get_path_resolves_alias_by_default() {
    let doc = Document::from_str(ALIASED).unwrap();
    assert_eq!(
        doc.try_get_path("service.timeout").unwrap().to_string(),
        "30".to_string()
    );
    assert_eq!(doc.to_string(), ALIASED.to_string());
}

#[test]
fn get_path_refuse_policy_reports_alias() {
    let doc = Document::from_str(ALIASED).unwrap();
    assert_eq!(
        doc.try_get_path_with("service.timeout", AliasPolicy::Refuse),
        Err(PathError::AliasRefused {
            at: "service".to_string(),
            alias: "shared".to_string()
        })
    );
}

#[test]
fn get_path_undefined_alias() {
    let doc = Document::from_str(
        "service: *missing
",
    )
    .unwrap();
    assert_eq!(
        doc.try_get_path("service.timeout"),
        Err(PathError::UndefinedAlias {
            at: "service".to_string(),
            alias: "missing".to_string()
        })
    );
}

#[test]
fn path_alias_uses_preceding_anchor_definition() {
    let original = "a: &x {k: 1}\nb: *x\nc: &x {k: 2}\n";
    let doc = Document::from_str(original).unwrap();
    assert_eq!(
        doc.try_get_path("b.k")
            .unwrap()
            .as_scalar()
            .map(|s| s.as_string()),
        Some("1".to_string())
    );
    doc.try_set_path_with("b.k", 9, AliasPolicy::Follow)
        .unwrap();
    assert_cst_ok(&doc);
    assert_eq!(doc.to_string(), "a: &x {k: 9}\nb: *x\nc: &x {k: 2}\n");
}

#[test]
fn path_alias_before_anchor_is_undefined() {
    let doc = Document::from_str("b: *x\na: &x {k: 1}\n").unwrap();
    assert_eq!(
        doc.try_get_path("b.k"),
        Err(PathError::UndefinedAlias {
            at: "b".to_string(),
            alias: "x".to_string()
        })
    );
    assert_eq!(
        doc.try_set_path_with("b.k", 9, AliasPolicy::Follow),
        Err(PathError::UndefinedAlias {
            at: "b".to_string(),
            alias: "x".to_string()
        })
    );
    assert_cst_ok(&doc);
}

#[test]
fn get_path_alias_to_anchor_in_earlier_document() {
    // Anchors are scoped to their document (YAML 1.2 section 3.2.2.2).
    let file = YamlFile::from_str("a: &x\n  k: 1\n---\nb: *x\n").unwrap();
    let second = file.documents().nth(1).unwrap();
    assert_eq!(
        second.try_get_path("b.k"),
        Err(PathError::UndefinedAlias {
            at: "b".to_string(),
            alias: "x".to_string()
        })
    );
}

#[test]
fn set_path_alias_to_anchor_in_earlier_document() {
    let src = "a: &x\n  k: 1\n---\nb: *x\n";
    let file = YamlFile::from_str(src).unwrap();
    let second = file.documents().nth(1).unwrap();
    for policy in [AliasPolicy::Follow, AliasPolicy::Expand] {
        assert_eq!(
            second.try_set_path_with("b.k", 2, policy),
            Err(PathError::UndefinedAlias {
                at: "b".to_string(),
                alias: "x".to_string()
            })
        );
        assert_cst_ok(&file);
    }
    assert_eq!(file.to_string(), src.to_string());
}

#[test]
fn get_path_merge_key_from_earlier_document() {
    let file = YamlFile::from_str("a: &x\n  k: 1\n---\nb:\n  <<: *x\n").unwrap();
    let second = file.documents().nth(1).unwrap();
    assert_eq!(
        second.try_get_path("b.k"),
        Err(PathError::UndefinedAlias {
            at: "k".to_string(),
            alias: "x".to_string()
        })
    );
}

#[test]
fn get_path_merge_key_not_a_mapping() {
    let doc = yaml_edit::Document::from_str("a: &a 5\nm: {<<: *a, j: 1}\n").unwrap();
    assert_eq!(
        doc.try_get_path("m.k"),
        Err(PathError::InvalidMergeOperand {
            at: "k".to_string(),
            operand: "*a".to_string()
        })
    );
    // A direct key does not need the merge.
    assert_eq!(doc.try_get_path("m.j").unwrap().to_string(), "1");
}

#[test]
fn get_path_alias_resolves_within_later_document() {
    // The same anchor name in two documents: each alias sees its own.
    let file = YamlFile::from_str("a: &x\n  k: 1\nb: *x\n---\na: &x\n  k: 2\nb: *x\n").unwrap();
    let docs: Vec<_> = file.documents().collect();
    assert_eq!(docs[0].try_get_path("b.k").unwrap().to_i64(), Some(1));
    assert_eq!(docs[1].try_get_path("b.k").unwrap().to_i64(), Some(2));
}

#[test]
fn set_path_refuses_alias_by_default() {
    let doc = Document::from_str(ALIASED).unwrap();
    assert_eq!(
        doc.try_set_path("service.timeout", 60),
        Err(PathError::AliasRefused {
            at: "service".to_string(),
            alias: "shared".to_string()
        })
    );
    assert_cst_ok(&doc);
    assert_eq!(doc.to_string(), ALIASED.to_string());
}

#[test]
fn set_path_follow_writes_through_to_anchor() {
    let doc = Document::from_str(ALIASED).unwrap();
    doc.try_set_path_with("service.timeout", 60, AliasPolicy::Follow)
        .unwrap();
    assert_cst_ok(&doc);
    // The anchored value changed, so every alias to it sees 60.
    assert_eq!(
        doc.to_string(),
        "shared: &shared\n  timeout: 60\nservice: *shared\n".to_string()
    );
}

#[test]
fn set_path_expand_copies_without_anchor() {
    let doc = Document::from_str(ALIASED).unwrap();
    doc.try_set_path_with("service.timeout", 60, AliasPolicy::Expand)
        .unwrap();
    assert_cst_ok(&doc);
    // The alias is replaced by an independent copy; the anchor keeps
    // its original value and carries no duplicate `&shared`.
    assert_eq!(
        doc.to_string(),
        "shared: &shared\n  timeout: 30\nservice:\n  timeout: 60\n".to_string()
    );
}

#[test]
fn set_path_expand_leaves_other_aliases_alone() {
    let src = "shared: &shared\n  timeout: 30\na: *shared\nb: *shared\n";
    let doc = Document::from_str(src).unwrap();
    doc.try_set_path_with("a.timeout", 60, AliasPolicy::Expand)
        .unwrap();
    assert_cst_ok(&doc);
    assert_eq!(
        doc.to_string(),
        "shared: &shared\n  timeout: 30\na:\n  timeout: 60\nb: *shared\n".to_string()
    );
}

#[test]
fn set_path_expand_multi_entry_indentation() {
    let doc = Document::from_str("a: &x\n  k: 1\n  j: 2\nb: *x\n").unwrap();
    doc.try_set_path_with("b.j", 3, AliasPolicy::Expand)
        .unwrap();
    assert_cst_ok(&doc);
    assert_eq!(
        doc.to_string(),
        "a: &x\n  k: 1\n  j: 2\nb:\n  k: 1\n  j: 3\n"
    );

    let doc = Document::from_str("a: &x\n  - 1\n  - 2\nb: *x\n").unwrap();
    doc.try_set_path_with("b[1]", 3, AliasPolicy::Expand)
        .unwrap();
    assert_cst_ok(&doc);
    assert_eq!(doc.to_string(), "a: &x\n  - 1\n  - 2\nb:\n  - 1\n  - 3\n");
}

#[test]
fn set_path_expand_drops_nested_anchors() {
    let doc = Document::from_str("a: &x\n  k: &y 1\n  m: &z\n    n: 2\nb: *x\nc: *y\n").unwrap();
    doc.try_set_path_with("b.j", 3, AliasPolicy::Expand)
        .unwrap();
    assert_cst_ok(&doc);
    assert_eq!(
        doc.to_string(),
        "a: &x\n  k: &y 1\n  m: &z\n    n: 2\nb:\n  k: 1\n  m:\n    n: 2\n  j: 3\nc: *y\n"
    );

    let doc = Document::from_str("a: &x\n  - &y [&z 1, 2]\n  - 3\nb: *x\n").unwrap();
    doc.try_set_path_with("b[1]", 4, AliasPolicy::Expand)
        .unwrap();
    assert_cst_ok(&doc);
    assert_eq!(
        doc.to_string(),
        "a: &x\n  - &y [&z 1, 2]\n  - 3\nb:\n  - [1, 2]\n  - 4\n"
    );
}

#[test]
fn set_path_replacing_alias_outright_needs_no_policy() {
    // A single-segment path overwrites the alias entry itself rather
    // than writing through it, so the default policy allows it.
    let doc = Document::from_str(ALIASED).unwrap();
    doc.try_set_path("service", 1).unwrap();
    assert_cst_ok(&doc);
    assert_eq!(
        doc.to_string(),
        "shared: &shared\n  timeout: 30\nservice: 1\n".to_string()
    );
}

#[test]
fn set_path_undefined_alias_is_reported() {
    let src = "service: *missing\n";
    let doc = Document::from_str(src).unwrap();
    assert_eq!(
        doc.try_set_path_with("service.timeout", 1, AliasPolicy::Follow),
        Err(PathError::UndefinedAlias {
            at: "service".to_string(),
            alias: "missing".to_string()
        })
    );
    assert_cst_ok(&doc);
    assert_eq!(doc.to_string(), src.to_string());
}

#[test]
fn remove_path_refuses_alias_by_default() {
    let doc = Document::from_str(ALIASED).unwrap();
    assert_eq!(
        doc.try_remove_path("service.timeout"),
        Err(PathError::AliasRefused {
            at: "service".to_string(),
            alias: "shared".to_string()
        })
    );
    assert_cst_ok(&doc);
    assert_eq!(doc.to_string(), ALIASED.to_string());
}

#[test]
fn remove_path_follow_removes_from_anchor() {
    let src = "shared: &shared\n  timeout: 30\n  retries: 3\nservice: *shared\n";
    let doc = Document::from_str(src).unwrap();
    let removed = doc
        .try_remove_path_with("service.timeout", AliasPolicy::Follow)
        .unwrap();
    assert_cst_ok(&doc);
    assert_eq!(removed.to_string(), "30".to_string());
    // The key is gone from the anchored mapping, so the alias sees
    // the removal too.
    assert_eq!(
        doc.to_string(),
        "shared: &shared\n  retries: 3\nservice: *shared\n".to_string()
    );
}

#[test]
fn alias_to_sequence_resolves_for_read() {
    let doc = Document::from_str("other: &o\n- a\n- b\nitems: *o\n").unwrap();
    assert_eq!(
        doc.try_get_path("items[1]").unwrap().to_string(),
        "b".to_string()
    );
}

#[test]
fn get_path_on_mapping_resolves_document_anchors() {
    // Traversal started from a nested Mapping, but the anchor is
    // defined at the document root: the registry is built from the
    // whole tree, so it still resolves.
    let doc =
        Document::from_str("shared: &shared\n  timeout: 30\nouter:\n  svc: *shared\n").unwrap();
    let outer = doc.as_mapping().unwrap().get_mapping("outer").unwrap();
    assert_eq!(
        outer.try_get_path("svc.timeout").unwrap().to_string(),
        "30".to_string()
    );
}

#[test]
fn path_alias_to_anchored_alias() {
    let src = "x: &x {k: 1}\nb: &b *x\nc: *b\n";
    let doc = Document::from_str(src).unwrap();
    assert_eq!(doc.try_get_path("c.k").unwrap().to_string(), "1");

    doc.try_set_path_with("c.k", 2, AliasPolicy::Follow)
        .unwrap();
    assert_cst_ok(&doc);
    assert_eq!(doc.to_string(), "x: &x {k: 2}\nb: &b *x\nc: *b\n");

    let doc = Document::from_str(src).unwrap();
    doc.try_set_path_with("c.k", 2, AliasPolicy::Expand)
        .unwrap();
    assert_cst_ok(&doc);
    assert_eq!(doc.to_string(), "x: &x {k: 1}\nb: &b *x\nc: {k: 2}\n");

    let doc = Document::from_str("b: &b *b\nc: *b\n").unwrap();
    assert_eq!(
        doc.try_get_path("c.k"),
        Err(PathError::UndefinedAlias {
            at: "c".to_string(),
            alias: "b".to_string()
        })
    );
}

#[test]
fn recursive_alias_terminates() {
    // Each segment resolves exactly one alias hop, so a self
    // referential anchor consumes path segments instead of looping.
    let doc = Document::from_str("a: &x\n  b: *x\n").unwrap();
    assert!(doc.try_get_path("a.b.b.b").is_ok());
}

#[test]
fn path_reaches_non_string_scalar_keys() {
    let yaml = "m:\n  1: one\n  true: t\n  null: n\n  0x10: hex\n";
    let doc = Document::from_str(yaml).unwrap();
    let get = |path: &str| {
        doc.try_get_path(path)
            .map(|n| n.as_scalar().unwrap().as_string())
    };
    assert_eq!(get("m.1"), Ok("one".to_string()));
    assert_eq!(get("m[1]"), Ok("one".to_string()));
    assert_eq!(get("m.true"), Ok("t".to_string()));
    assert_eq!(get("m.null"), Ok("n".to_string()));
    assert_eq!(get("m.0x10"), Ok("hex".to_string()));
    // Matching is on the key as written, not its resolved value.
    assert_eq!(
        get("m.16"),
        Err(PathError::NotFound {
            at: "[16]".to_string()
        })
    );

    doc.try_set_path("m.1", "uno").unwrap();
    assert_cst_ok(&doc);
    assert_eq!(
        doc.try_remove_path("m.true").unwrap().to_string(),
        "t".to_string()
    );
    assert_cst_ok(&doc);
    assert_eq!(doc.to_string(), "m:\n  1: uno\n  null: n\n  0x10: hex\n");

    let doc = Document::from_str("m:\n  '1': s\n").unwrap();
    assert_eq!(
        doc.try_get_path("m.1").unwrap().to_string(),
        "s".to_string()
    );
}

#[test]
fn path_reaches_non_string_key_through_merge() {
    let doc = Document::from_str("base: &b\n  1: one\nm:\n  <<: *b\n").unwrap();
    assert_eq!(doc.try_get_path("m.1").unwrap().to_string(), "one");
}

#[test]
fn set_path_expand_block_mapping_in_sequence() {
    let doc = Document::from_str("s:\n  - &x\n    k: 1\n  - *x\n").unwrap();
    doc.try_set_path_with("s[1].k", 9, AliasPolicy::Expand)
        .unwrap();
    yaml_edit::debug::validate_tree(doc.syntax()).unwrap();
    yaml_edit::debug::roundtrip_ok(doc.syntax()).unwrap();
    assert_eq!(doc.to_string(), "s:\n  - &x\n    k: 1\n  - k: 9\n");
}

fn set_and_check(src: &str, path: &str, policy: AliasPolicy) -> (Result<(), PathError>, String) {
    let doc = Document::from_str(src).unwrap();
    let result = doc.try_set_path_with(path, 9, policy);
    yaml_edit::debug::validate_tree(doc.syntax()).unwrap();
    yaml_edit::debug::roundtrip_ok(doc.syntax()).unwrap();
    (result, doc.to_string())
}

#[test]
fn set_path_through_alias_to_tagged_collection() {
    let src = "a: &x !!map {k: 1}\nb: *x\n";
    assert_eq!(
        set_and_check(src, "b.j", AliasPolicy::Expand),
        (
            Ok(()),
            "a: &x !!map {k: 1}\nb: !!map {k: 1, j: 9}\n".to_string()
        )
    );
    assert_eq!(
        set_and_check(src, "b.j", AliasPolicy::Follow),
        (Ok(()), "a: &x !!map {k: 1, j: 9}\nb: *x\n".to_string())
    );

    let src = "a: &x !!seq [1]\nb: *x\n";
    assert_eq!(
        set_and_check(src, "b[0]", AliasPolicy::Expand),
        (Ok(()), "a: &x !!seq [1]\nb: !!seq [9]\n".to_string())
    );

    let src = "s:\n  - &x !!map {k: 1}\n  - *x\n";
    assert_eq!(
        set_and_check(src, "s[1].j", AliasPolicy::Expand),
        (
            Ok(()),
            "s:\n  - &x !!map {k: 1}\n  - !!map {k: 1, j: 9}\n".to_string()
        )
    );
}

#[test]
fn set_path_expand_failure_leaves_document_alone() {
    let mismatch = |at: &str| Err(PathError::TypeMismatch { at: at.to_string() });
    for (src, path, err) in [
        ("a: &x 1\nb: *x\n", "b.k", mismatch("b")),
        ("a: &x 1\nb: *x\n", "b[0]", mismatch("b")),
        ("s:\n  - &x 1\n  - *x\n", "s[1].k", mismatch("[1]")),
        ("s:\n  - &x 1\n  - *x\n", "s[1][0]", mismatch("[1]")),
        ("a: &x {k: 1}\nb: *x\n", "b.k.j", mismatch("k")),
        ("s:\n  - &x {k: 1}\n  - *x\n", "s[1].k.j", mismatch("k")),
        (
            "a: &x {k: 1, m: *y}\nb: *x\n",
            "b.m.j",
            Err(PathError::UndefinedAlias {
                at: "m".to_string(),
                alias: "y".to_string(),
            }),
        ),
    ] {
        assert_eq!(
            set_and_check(src, path, AliasPolicy::Expand),
            (err, src.to_string()),
            "{path} on {src:?}"
        );
    }
}

#[test]
fn errors_name_the_segment_holding_the_value() {
    let doc = Document::from_str("x: &x {a: 1}\nfoo: 1\ns: [a]\nb: *x\nu: *nope\n").unwrap();
    let mismatch = |at: &str| PathError::TypeMismatch { at: at.to_string() };
    for (path, expected) in [
        ("foo.bar", mismatch("foo")),
        ("foo.bar.baz", mismatch("foo")),
        ("foo[0]", mismatch("foo")),
        ("s.k", mismatch("s")),
        ("s[0].k", mismatch("[0]")),
        ("s[0][0]", mismatch("[0]")),
        (
            "b.a",
            PathError::AliasRefused {
                at: "b".to_string(),
                alias: "x".to_string(),
            },
        ),
        (
            "u.a",
            PathError::AliasRefused {
                at: "u".to_string(),
                alias: "nope".to_string(),
            },
        ),
    ] {
        assert_eq!(
            doc.try_get_path_with(path, AliasPolicy::Refuse),
            Err(expected.clone()),
            "get {path}"
        );
        assert_eq!(
            doc.try_remove_path(path).map(|_| ()),
            Err(expected.clone()),
            "remove {path}"
        );
        assert_eq!(doc.try_set_path(path, 1), Err(expected), "set {path}");
    }
    let undefined = PathError::UndefinedAlias {
        at: "u".to_string(),
        alias: "nope".to_string(),
    };
    assert_eq!(
        doc.try_get_path_with("u.a", AliasPolicy::Follow),
        Err(undefined.clone())
    );
    assert_eq!(
        doc.try_remove_path_with("u.a", AliasPolicy::Follow)
            .map(|_| ()),
        Err(undefined.clone())
    );
    assert_eq!(
        doc.try_set_path_with("u.a", 1, AliasPolicy::Follow),
        Err(undefined)
    );
    // At the root no segment holds the value, so the one applied to it
    // is named.
    let doc = Document::from_str("5\n").unwrap();
    assert_eq!(doc.try_get_path("a"), Err(mismatch("a")));
}

#[test]
fn document_root_sequence_and_scalar() {
    let doc = Document::from_str("- a\n- b\n").unwrap();
    assert_eq!(doc.try_get_path("[1]").unwrap().to_string(), "b");
    assert_eq!(
        doc.try_remove_path("[1]").map(|_| ()),
        Err(PathError::TypeMismatch { at: "[1]".into() })
    );
    assert_eq!(doc.try_set_path("[1]", "x"), Err(PathError::NoRoot));
    assert_eq!(doc.to_string(), "- a\n- b\n");

    let doc = Document::from_str("5\n").unwrap();
    assert_eq!(
        doc.try_get_path("a"),
        Err(PathError::TypeMismatch { at: "a".into() })
    );
    assert_eq!(
        doc.try_remove_path("a").map(|_| ()),
        Err(PathError::TypeMismatch { at: "a".into() })
    );

    let doc = Document::new();
    assert_eq!(doc.try_get_path("a"), Err(PathError::NoRoot));
    assert_eq!(doc.try_remove_path("a").map(|_| ()), Err(PathError::NoRoot));
}

#[test]
fn set_path_creates_root_mapping_in_empty_document() {
    // Like Document::set, a write into a document without a root creates
    // the root mapping.
    for src in ["", "---\n", "---\n# just a comment\n"] {
        let doc = Document::from_str(src).unwrap();
        doc.try_set_path("a.b.c", 1).unwrap();
        assert_edited(&doc, &format!("{src}a:\n  b:\n    c: 1\n"));
    }

    let doc = Document::new();
    doc.try_set_path("a", 1).unwrap();
    assert_edited(&doc, "---\na: 1\n");

    let doc = Document::new();
    doc.try_set_path("s[0]", "x").unwrap();
    assert_edited(&doc, "---\ns:\n  - x\n");

    let doc = Document::new();
    #[allow(deprecated)]
    doc.set_path("a.b", 1);
    assert_edited(&doc, "---\na:\n  b: 1\n");
}

#[test]
fn set_path_failure_leaves_empty_document_unchanged() {
    let doc = Document::new();
    assert!(matches!(
        doc.try_set_path("a[0", 1),
        Err(PathError::Parse(_))
    ));
    assert_eq!(doc.try_set_path("", 1), Err(PathError::EmptyPath));
    assert_eq!(doc.to_string(), "---\n");
    assert_cst_ok(&doc);
}

#[test]
fn set_path_creates_sequences_and_mappings_in_sequences() {
    let refuse = AliasPolicy::Refuse;
    for (src, path, expected) in [
        ("{a: 1}", "b[0]", "{a: 1, b: [9]}"),
        ("{a: 1}", "b.c", "{a: 1, b: {c: 9}}"),
        ("s: [[a]]\n", "s[0][1]", "s: [[a, 9]]\n"),
        ("s: [{a: 1}]\n", "s[0][1]", "s: [{a: 1, '1': 9}]\n"),
        ("s:\n- a\n", "s[2].k", "s:\n- a\n- null\n- {k: 9}\n"),
        ("s:\n- a\n", "s[2][0]", "s:\n- a\n- null\n- [9]\n"),
        ("s:\n- a\n", "s[1][0][0]", "s:\n- a\n- [[9]]\n"),
        ("s:\n- a\n- ~\n", "s[1].k", "s:\n- a\n- {k: 9}\n"),
        (
            "s:\n- a\n- {b: 1}\n",
            "s[1][0]",
            "s:\n- a\n- {b: 1, '0': 9}\n",
        ),
        ("a: ~\n", "a[0]", "a:\n  - 9\n"),
        ("a:\n  b: 1\n", "a[0]", "a:\n  b: 1\n  '0': 9\n"),
    ] {
        assert_eq!(
            set_and_check(src, path, refuse),
            (Ok(()), expected.to_string()),
            "{path} on {src:?}"
        );
    }
}

#[test]
fn set_path_refusals_leave_document_alone() {
    let mismatch = |at: &str| Err(PathError::TypeMismatch { at: at.to_string() });
    for (src, path, err) in [
        ("s: [a]\n", "s.k", mismatch("s")),
        ("s: [a]\n", "s[0].k", mismatch("[0]")),
        ("s: [!t x]\n", "s[0][0]", mismatch("[0]")),
        ("s: [!t x]\n", "s[0].k", mismatch("[0]")),
        ("s:\n- a\n- [b]\n", "s[1].k", mismatch("[1]")),
        ("a: [x]\n", "a[0][0]", mismatch("[0]")),
        ("a: !t x\n", "a.b", mismatch("a")),
        ("a: !t x\n", "a[0]", mismatch("a")),
        (
            "a: 1\n",
            "b[900000]",
            Err(PathError::IndexTooFar {
                at: "[900000]".to_string(),
                len: 0,
                index: 900000,
            }),
        ),
        (
            "s: [a]\n",
            "s[257]",
            Err(PathError::IndexTooFar {
                at: "[257]".to_string(),
                len: 1,
                index: 257,
            }),
        ),
    ] {
        for policy in [AliasPolicy::Refuse, AliasPolicy::Expand] {
            assert_eq!(
                set_and_check(src, path, policy),
                (err.clone(), src.to_string()),
                "{path} on {src:?}"
            );
        }
    }
}

#[test]
fn set_path_through_undefined_alias_in_sequence() {
    assert_eq!(
        set_and_check("s:\n- *nope\n", "s[0].a", AliasPolicy::Expand),
        (
            Err(PathError::UndefinedAlias {
                at: "[0]".to_string(),
                alias: "nope".to_string(),
            }),
            "s:\n- *nope\n".to_string()
        )
    );
}

#[test]
fn set_path_through_alias_in_sequence() {
    let src = "x: &x {a: 1}\ns: [*x]\n";
    assert_eq!(
        set_and_check(src, "s[0].a", AliasPolicy::Refuse),
        (
            Err(PathError::AliasRefused {
                at: "[0]".to_string(),
                alias: "x".to_string()
            }),
            src.to_string()
        )
    );
    assert_eq!(
        set_and_check(src, "s[0].a", AliasPolicy::Follow),
        (Ok(()), "x: &x {a: 9}\ns: [*x]\n".to_string())
    );
    assert_eq!(
        set_and_check(src, "s[0].a", AliasPolicy::Expand),
        (Ok(()), "x: &x {a: 1}\ns: [{a: 9}]\n".to_string())
    );
    assert_eq!(
        set_and_check("x: &x [1]\ns:\n- *x\n", "s[0][1]", AliasPolicy::Expand),
        (Ok(()), "x: &x [1]\ns:\n- [1, 9]\n".to_string())
    );
}

#[test]
#[allow(deprecated)]
fn deprecated_wrappers() {
    assert_eq!(
        yaml_edit::path::parse_path("a[1].b"),
        vec![
            PathSegment::Key("a".to_string()),
            PathSegment::Index(1),
            PathSegment::Key("b".to_string()),
        ]
    );
    let doc = Document::from_str("a:\n  b: 1\n").unwrap();
    assert_eq!(doc.get_path("a.b").unwrap().to_string(), "1");
    assert!(doc.get_path("a.c").is_none());
    doc.set_path("a.c", 2);
    doc.set_path("a.b.x", 3);
    assert_eq!(doc.to_string(), "a:\n  b: 1\n  c: 2\n");
    assert!(doc.remove_path("a.b"));
    assert!(!doc.remove_path("a.b"));
    assert_eq!(doc.to_string(), "a:\n  c: 2\n");
}

#[test]
fn set_path_growth_bound_is_exact() {
    let doc = Document::from_str("s: [a, b]\n").unwrap();
    assert_eq!(
        doc.try_set_path("s[258]", 1),
        Err(PathError::IndexTooFar {
            at: "[258]".to_string(),
            len: 2,
            index: 258,
        })
    );
    assert_eq!(doc.try_set_path("s[257]", 1), Ok(()));
    let s = doc.as_mapping().unwrap().get_sequence("s").unwrap();
    assert_eq!(s.len(), 258);
    assert_eq!(s.get(257).unwrap().to_string(), "1");
}

#[test]
fn get_path_reads_scalars() {
    let items = "\nitems:\n  - name: first\n    value: 1\n  - name: second\n    value: 2\n";
    let deep = "app:\n  database:\n    primary:\n      host: db.example.com\n      port: 5432\n";
    for (yaml, path, expected) in [
        ("name: Alice\nage: 30\n", "name", "Alice"),
        ("name: Alice\nage: 30\n", "age", "30"),
        (
            "server:\n  host: localhost\n  port: 8080\n",
            "server.host",
            "localhost",
        ),
        (
            "server:\n  host: localhost\n  port: 8080\n",
            "server.port",
            "8080",
        ),
        (deep, "app.database.primary.host", "db.example.com"),
        (deep, "app.database.primary.port", "5432"),
        // Bracket and numeric dot notation both index a sequence.
        (items, "items[0].name", "first"),
        (items, "items[1].value", "2"),
        (items, "items.0.name", "first"),
        (items, "items.1.value", "2"),
        ("\n- first\n- second\n- third\n", "0", "first"),
        ("\n- first\n- second\n- third\n", "2", "third"),
    ] {
        let doc = Document::from_str(yaml).unwrap();
        let node = doc.try_get_path(path).unwrap();
        let scalar = node.as_scalar().unwrap();
        assert_eq!(scalar.as_string(), expected, "{path:?} in {yaml:?}");
        assert_eq!(scalar.to_string(), expected, "{path:?} in {yaml:?}");
    }
}

#[test]
fn mapping_get_path_with_indices() {
    let yaml = r#"
config:
  servers:
    - host: server1.com
      port: 8080
    - host: server2.com
      port: 9090
"#;
    let doc = Document::from_str(yaml).unwrap();
    let mapping = doc.as_mapping().unwrap();
    for (path, expected) in [
        ("config.servers[0].host", "server1.com"),
        ("config.servers.1.port", "9090"),
    ] {
        let node = mapping.try_get_path(path).unwrap();
        assert_eq!(node.as_scalar().unwrap().as_string(), expected, "{path:?}");
    }
}

/// Set `path` in `source` to `value`, check the result and that `path`
/// reads back as `read_back`.
#[track_caller]
fn check_set_path(source: &str, path: &str, value: impl AsYaml, expected: &str, read_back: &str) {
    let doc = Document::from_str(source).unwrap();
    doc.try_set_path(path, value).unwrap();
    assert_edited(&doc, expected);
    assert_eq!(
        doc.try_get_path(path)
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        read_back,
        "{path:?} in {source:?}"
    );
}

#[test]
fn set_path_writes_values() {
    for (source, path, value, expected) in [
        (
            "name: Alice\nage: 30\n",
            "name",
            "Bob",
            "name: Bob\nage: 30\n",
        ),
        (
            "name: test\n",
            "server.database.host",
            "localhost",
            "name: test\nserver:\n  database:\n    host: localhost\n",
        ),
        (
            "server:\n  host: localhost  # production server\n",
            "server.host",
            "newhost",
            "server:\n  host: newhost  # production server\n",
        ),
        // An escaped digit is a key, not an index.
        ("x: 1\n", "a.\\0", "v", "x: 1\na:\n  '0': v\n"),
        (
            "items:\n  - a\n  - b\n",
            "items[1]",
            "B",
            "items:\n  - a\n  - B\n",
        ),
        (
            "items:\n  - a\n",
            "items[3]",
            "z",
            "items:\n  - a\n  - null\n  - null\n  - z\n",
        ),
        // Nested collections created inline after `- ` render in flow
        // style (`- {c: "value"}`) so re-parse can find the key. A block
        // mapping inline after the dash would render as a compact-block
        // shape that re-parses ambiguously.
        (
            "base: true\n",
            "a.b[0].c",
            "value",
            "base: true\na:\n  b:\n    - {c: value}\n",
        ),
    ] {
        check_set_path(source, path, value, expected, value);
    }
    for (source, path, value, expected) in [
        ("name: Alice\n", "age", 30, "name: Alice\nage: 30\n"),
        (
            "server:\n  host: localhost\n  port: 8080\n",
            "server.port",
            9000,
            "server:\n  host: localhost\n  port: 9000\n",
        ),
        (
            "server:\n  host: localhost\n",
            "server.port",
            8080,
            "server:\n  host: localhost\n  port: 8080\n",
        ),
    ] {
        check_set_path(source, path, value, expected, &value.to_string());
    }
}

#[test]
fn remove_path_removes_entries() {
    let abc = "config:\n  a: 1\n  b: 2\n  c: 3\n";
    for (source, path, removed, expected) in [
        ("name: Alice\nage: 30\n", "age", "30", "name: Alice\n"),
        (
            "server:\n  host: localhost\n  port: 8080\n",
            "server.port",
            "8080",
            "server:\n  host: localhost\n",
        ),
        (
            "app:\n  database:\n    primary:\n      host: db.example.com\n      port: 5432\n",
            "app.database.primary.port",
            "5432",
            "app:\n  database:\n    primary:\n      host: db.example.com\n",
        ),
        // Each entry after the first owns the INDENT token before it.
        // Removing an entry without that token left it behind, either
        // over-indenting the next entry or dangling as trailing
        // whitespace when the removed entry was last.
        (abc, "config.b", "2", "config:\n  a: 1\n  c: 3\n"),
        (abc, "config.a", "1", "config:\n  b: 2\n  c: 3\n"),
        (abc, "config.c", "3", "config:\n  a: 1\n  b: 2\n"),
        // Deeper nesting used to compound the over-indentation.
        (
            "x:\n  y:\n    a: 1\n    b: 2\n",
            "x.y.a",
            "1",
            "x:\n  y:\n    b: 2\n",
        ),
    ] {
        let doc = Document::from_str(source).unwrap();
        assert_eq!(doc.try_remove_path(path).unwrap().to_string(), removed);
        assert_edited(&doc, expected);
    }
}

#[test]
fn remove_path_missing() {
    let doc = Document::from_str("name: Alice\n").unwrap();
    assert_eq!(
        doc.try_remove_path("missing").map(|_| ()),
        Err(PathError::NotFound {
            at: "missing".to_string()
        })
    );
    assert_eq!(
        doc.try_remove_path("name.nested").map(|_| ()),
        Err(PathError::TypeMismatch {
            at: "name".to_string()
        })
    );
    assert_cst_ok(&doc);
    assert_eq!(doc.to_string(), "name: Alice\n");
}
