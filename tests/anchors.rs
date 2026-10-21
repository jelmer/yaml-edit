//! Anchors and aliases: how they parse, how an alias key is matched, and
//! how edits keep aliases resolving to the content they did.
//!
//! Removing or replacing a node that holds anchors keeps the aliases that
//! referred to them resolving to the same content: the first such alias
//! becomes a copy of the anchored node.
//!
//! Moving entries around keeps every alias resolving to the node it did:
//! an anchor that would come after an alias to it moves to that alias.
//!
//! Replacing an anchored value keeps the anchor, so aliases to it still
//! resolve.

mod common;
use common::{assert_edited, assert_file_cst_ok};
use rowan::ast::AstNode;
use std::str::FromStr;
use yaml_edit::anchor_resolution::DocumentResolvedExt;
use yaml_edit::path::YamlPath;
use yaml_edit::validator::Validator;
use yaml_edit::{debug, Alias, Document, SyntaxKind, YamlFile, YamlNode, YamlValue};

/// A node as compact text: flow collections as `[..]` and `{..}`, block
/// ones as `seq[..]` and `map{..}`, and an alias as `*name`.
fn render(node: &YamlNode) -> String {
    match node {
        YamlNode::Scalar(s) => s.as_string(),
        YamlNode::Sequence(s) => {
            let items: Vec<_> = s.values().map(|v| render(&v)).collect();
            assert_eq!(items.len(), s.len());
            let open = if s.is_flow_style() { "[" } else { "seq[" };
            format!("{open}{}]", items.join(", "))
        }
        YamlNode::Mapping(m) => {
            let entries: Vec<_> = m
                .iter()
                .map(|(k, v)| format!("{}: {}", render(&k), render(&v)))
                .collect();
            assert_eq!(entries.len(), m.len());
            let open = if m.is_flow_style() { "{" } else { "map{" };
            format!("{open}{}}}", entries.join(", "))
        }
        YamlNode::TaggedNode(t) => panic!("unexpected tagged node {t:?}"),
        YamlNode::Alias(a) => format!("*{}", a.name()),
    }
}

/// Assert that `doc` resolves to the same value as `meaning`.
#[track_caller]
fn assert_means(doc: &Document, meaning: &str) {
    let meaning = Document::from_str(meaning).unwrap();
    assert_eq!(
        YamlValue::from_document(doc).unwrap(),
        YamlValue::from_document(&meaning).unwrap()
    );
}

/// Set `metadata.version`, and `metadata.status` if `yaml` has one, in
/// `file` parsed from `yaml`, checking that nothing else changes.
#[track_caller]
fn edit_metadata(file: &YamlFile, yaml: &str) {
    let metadata = file
        .document()
        .unwrap()
        .as_mapping()
        .unwrap()
        .get_mapping("metadata")
        .unwrap();
    metadata.set("version", "0.1.1");
    assert_file_cst_ok(file);
    let expected = yaml.replace("version: 0.1.0", "version: 0.1.1");
    assert_eq!(file.to_string(), expected);
    if yaml.contains("status: draft") {
        metadata.set("status", "stable");
        assert_file_cst_ok(file);
        assert_eq!(
            file.to_string(),
            expected.replace("status: draft", "status: stable")
        );
    }
}

#[test]
fn aliases_parse() {
    for (yaml, expected) in [
        (r#"&x x: *x"#, "map{x: *x}"),
        (
            r#"anchor: &shared value
ref1: *shared
ref2: *shared
ref3: *shared"#,
            "map{anchor: value, ref1: *shared, ref2: *shared, ref3: *shared}",
        ),
        (
            r#"anchor: &日本語 value
ref: *日本語"#,
            "map{anchor: value, ref: *日本語}",
        ),
        // A redefined anchor: the alias refers to the latest definition.
        (
            r#"first: &x value1
second: &x value2
ref: *x"#,
            "map{first: value1, second: value2, ref: *x}",
        ),
        // An alias before its anchor is kept as it is; the parser does not
        // check that it refers to anything.
        (
            r#"ref: *undefined
anchor: &undefined value"#,
            "map{ref: *undefined, anchor: value}",
        ),
        (
            r#"node: &node
  next: *node"#,
            "map{node: map{next: *node}}",
        ),
        ("&x [*x]", "[*x]"),
        (
            r#"level1: &l1
  level2: &l2
    level3: &l3
      value: deep
ref1: *l1
ref2: *l2
ref3: *l3"#,
            "map{level1: map{level2: map{level3: map{value: deep}}}, ref1: *l1, ref2: *l2, ref3: *l3}",
        ),
    ] {
        let file = YamlFile::from_str(yaml).unwrap();
        assert_eq!(file.to_string(), yaml);
        let doc = file.document().unwrap();
        let root = doc
            .syntax()
            .children()
            .find_map(YamlNode::from_syntax)
            .unwrap();
        assert_eq!(render(&root), expected, "{yaml:?}");
    }
}

#[test]
fn anchored_indentless_sequence_preserves_sibling_mapping_and_edits() {
    for indent in ["", "  "] {
        for comment in ["", " # shared tags"] {
            let yaml = format!(
                "tags: &id001{comment}\n{indent}- engineering\nmetadata:\n  version: 0.1.0 # keep version comment\n  status: draft\n  tags: *id001\n"
            );
            let file = YamlFile::from_str(&yaml).unwrap();
            assert_eq!(file.to_string(), yaml);
            let mapping = file.document().unwrap().as_mapping().unwrap();
            assert_eq!(
                render(&YamlNode::Mapping(mapping)),
                "map{tags: seq[engineering], metadata: map{version: 0.1.0, status: draft, tags: *id001}}",
                "{}",
                debug::tree_to_string(file.syntax())
            );
            edit_metadata(&file, &yaml);
        }
    }
}

#[test]
fn tag_anchor_indentless_sequence_is_aliasable() {
    let yaml = "tags: !!seq &a\n- engineering\nnext: *a\n";
    let file = YamlFile::from_str(yaml).unwrap();
    assert_eq!(file.to_string(), yaml);
    let mapping = file.document().unwrap().as_mapping().unwrap();
    assert_eq!(mapping.keys().count(), 2);
    assert_eq!(mapping.get("next").unwrap().as_alias().unwrap().name(), "a");
}

#[test]
fn flow_merge_alias_preserves_following_entry_and_metadata_edits() {
    for separator in [", ", " , "] {
        let yaml = format!(
            "x: &defaults {{a: 3}}\ny: {{<<: *defaults{separator}more: 'kept'}}\nmetadata:\n  version: 0.1.0\n  status: draft # keep status comment\n"
        );
        let file = YamlFile::from_str(&yaml).unwrap();
        assert_eq!(file.to_string(), yaml);
        let root = file.document().unwrap().as_mapping().unwrap();
        assert_eq!(
            render(&YamlNode::Mapping(root)),
            "map{x: {a: 3}, y: {<<: *defaults, more: kept}, metadata: map{version: 0.1.0, status: draft}}"
        );
        edit_metadata(&file, &yaml);
    }
}

/// An alias is a complete node, so `*a x: v` has no valid implicit key: the
/// colon follows `x`. The parser used to take `*a` as the key and then fail
/// on the `x`. It now keeps the `x` in the key as a second node, as for any
/// other complete node followed by more content (`[q] x: v`), and the
/// validator reports it.
#[test]
fn alias_followed_by_content_stays_in_key() {
    let violations = |yaml: &str| {
        let file = YamlFile::from_str(yaml).unwrap_or_else(|e| panic!("{yaml:?}: {e}"));
        assert_eq!(file.to_string(), yaml);
        assert_file_cst_ok(&file);
        Validator::new()
            .validate_syntax(file.syntax())
            .into_iter()
            .map(|v| (v.message, v.location.unwrap()))
            .collect::<Vec<_>>()
    };
    let two_nodes = |at: &str| vec![("Key holds more than one node".to_string(), at.to_string())];
    for (yaml, alias_at, flow_form, flow_at) in [
        ("a: &a 1\n*a x: v\n", "2:4", "a: &a 1\n[q] x: v\n", "2:5"),
        ("- &a 1\n- *a x: v\n", "2:6", "- &a 1\n- [q] x: v\n", "2:7"),
        (
            "a: &a 1\nk:\n  *a x: v\n",
            "3:6",
            "a: &a 1\nk:\n  [q] x: v\n",
            "3:7",
        ),
        (
            "a: &a 1\nb:\n- *a x: v\n",
            "3:6",
            "a: &a 1\nb:\n- [q] x: v\n",
            "3:7",
        ),
    ] {
        assert_eq!(violations(yaml), two_nodes(alias_at), "{yaml:?}");
        assert_eq!(violations(flow_form), two_nodes(flow_at), "{flow_form:?}");
    }
}

/// The text of every node of `kind` in `yaml`.
fn texts_of(yaml: &str, kind: SyntaxKind) -> Vec<String> {
    let file = YamlFile::from_str(yaml).unwrap();
    file.syntax()
        .descendants()
        .filter(|n| n.kind() == kind)
        .map(|n| n.text().to_string())
        .collect()
}

/// An alias followed only by the colon is still a key, and `*a:` is an alias
/// named `a:`, as in the YAML test suite's 2SXE.
#[test]
fn alias_key_still_parses() {
    assert_eq!(texts_of("a: &a 1\n*a : v\n", SyntaxKind::KEY), ["a", "*a"]);
    assert_eq!(texts_of("foo:\n  *a:\n", SyntaxKind::ALIAS), ["*a:"]);
}

/// The alias `*a`, read from another document so that only its name ties
/// it to an entry.
fn alias_key() -> YamlNode {
    let doc = Document::from_str("- *a\n").unwrap();
    let key = doc.as_sequence().unwrap().get(0).unwrap();
    assert!(key.as_alias().is_some());
    key
}

/// A key holding an alias is matched the same way, an alias in it by an
/// alias of the same name. It matched nothing, not even its own node, so
/// `set` added a second entry with the same key.
#[test]
fn key_holding_an_alias_matches_its_own_node() {
    let file = YamlFile::from_str("a: &x 1\n? [*x]\n: 2\n").unwrap();
    let m = file.document().unwrap().as_mapping().unwrap();
    let own = m.keys().nth(1).unwrap();
    let other = Document::from_str("[*x]\n").unwrap();
    let other = YamlNode::Sequence(other.as_sequence().unwrap());
    for key in [&own, &other] {
        assert_eq!(m.get(key).map(|v| v.to_string()), Some("2".to_string()));
    }
    // Not by the node the alias refers to.
    let resolved = Document::from_str("[1]\n").unwrap();
    assert_eq!(
        m.get(YamlNode::Sequence(resolved.as_sequence().unwrap())),
        None
    );

    m.set(&own, 5);
    assert_eq!(file.to_string(), "a: &x 1\n? [*x]\n: 5\n");
    assert_file_cst_ok(&file);

    let file = YamlFile::from_str("? e: *x\n").unwrap();
    let m = file.document().unwrap().as_mapping().unwrap();
    let own = m.keys().next().unwrap();
    assert!(m.insert_after(&own, "k", 2));
    assert_eq!(file.to_string(), "? e: *x\n? k\n: 2\n");
    assert_file_cst_ok(&file);
}

#[test]
fn alias_key_lookup_matches_alias_of_same_name() {
    let file = YamlFile::from_str("a: &a 1\n*a : 2\n").unwrap();
    let m = file.document().unwrap().as_mapping().unwrap();
    let own = m.keys().nth(1).unwrap();
    for key in [&own, &alias_key()] {
        assert_eq!(m.get(key).map(|v| v.to_string()), Some("2".to_string()));
        assert!(m.contains_key(key));
    }
    // A string is not an alias, nor is the node the alias refers to.
    assert_eq!(m.get("*a"), None);
    assert_eq!(m.get(1), None);
    assert_eq!(m.get("a").map(|v| v.to_string()), Some("1".to_string()));

    m.set(&own, "xval");
    assert_eq!(file.to_string(), "a: &a 1\n*a : xval\n");
    assert_file_cst_ok(&file);

    let file = YamlFile::from_str("a: &a 1\n*a : 2\nc: 3\n").unwrap();
    let m = file.document().unwrap().as_mapping().unwrap();
    assert!(m.rename_key(alias_key(), "b"));
    assert_eq!(file.to_string(), "a: &a 1\nb : 2\nc: 3\n");
    assert_file_cst_ok(&file);

    let file = YamlFile::from_str("a: &a 1\n*a : 2\nc: 3\n").unwrap();
    let m = file.document().unwrap().as_mapping().unwrap();
    assert!(m.move_after("c", alias_key(), "xval"));
    assert_eq!(m.len(), 3);
    assert_eq!(
        m.get(alias_key()).map(|v| v.to_string()),
        Some("xval".to_string())
    );
}

#[test]
fn mapping_set_keeps_nested_anchor() {
    let cases = [
        ("b: {c: &y 1}\nd: *y\n", "b: 6\nd: &y 1\n", "b: 6\nd: 1\n"),
        (
            "b: {c: &y 1}\nd: *y\ne: *y\n",
            "b: 6\nd: &y 1\ne: *y\n",
            "b: 6\nd: 1\ne: 1\n",
        ),
        (
            "b:\n  c: &y\n    e: 1\nd: *y\n",
            "b: 6\nd: &y\n  e: 1\n",
            "b: 6\nd: {e: 1}\n",
        ),
        (
            "b: [1, &y [2, 3]]\nd: *y  # c\n",
            "b: 6\nd: &y [2, 3]  # c\n",
            "b: 6\nd: [2, 3]\n",
        ),
        (
            "b: {c: !!str &y 1}\nd: *y\n",
            "b: 6\nd: !!str &y 1\n",
            "b: 6\nd: !!str 1\n",
        ),
        (
            "b: {c: &y !!str 1}\nd: *y\n",
            "b: 6\nd: &y !!str 1\n",
            "b: 6\nd: !!str 1\n",
        ),
        (
            "{b: {c: &y 1}, d: *y}\n",
            "{b: 6, d: &y 1}\n",
            "{b: 6, d: 1}\n",
        ),
        (
            "b: &x {c: &y 1}\nd: *x\ne: *y\n",
            "b: &x 6\nd: *x\ne: &y 1\n",
            "b: 6\nd: 6\ne: 1\n",
        ),
        // After `b` goes, `*y` must not fall back to the earlier `&y 0`.
        (
            "a: &y 0\nx: *y\nb: {c: &y 1}\nd: *y\n",
            "a: &y 0\nx: *y\nb: 6\nd: &y 1\n",
            "a: 0\nx: 0\nb: 6\nd: 1\n",
        ),
        // A later redefinition is left alone.
        (
            "b: {c: &y 1}\nd: *y\ne: &y 2\nf: *y\n",
            "b: 6\nd: &y 1\ne: &y 2\nf: *y\n",
            "b: 6\nd: 1\ne: 2\nf: 2\n",
        ),
        // With no alias between, the redefinition takes the later ones anyway.
        (
            "b: {c: &y 1}\ne: &y 2\nf: *y\n",
            "b: 6\ne: &y 2\nf: *y\n",
            "b: 6\ne: 2\nf: 2\n",
        ),
    ];
    for (source, expected, meaning) in cases {
        let doc = Document::from_str(source).unwrap();
        doc.as_mapping().unwrap().set("b", 6);
        assert_edited(&doc, expected);
        assert_means(&doc, meaning);
    }
}

#[test]
fn mapping_set_to_alias_keeps_old_anchor() {
    // An alias cannot carry the old anchor, so its first alias does.
    let doc = Document::from_str("a: &z 0\nb: &x 2\nc: *x\nd: *x\n").unwrap();
    doc.as_mapping().unwrap().set("b", Alias::new("z"));
    assert_edited(&doc, "a: &z 0\nb: *z\nc: &x 2\nd: *x\n");
    assert_means(&doc, "a: 0\nb: 0\nc: 2\nd: 2\n");

    let doc = Document::from_str("- &z 0\n- &x 2\n- *x\n").unwrap();
    doc.as_sequence().unwrap().set(1, Alias::new("z"));
    assert_edited(&doc, "- &z 0\n- *z\n- &x 2\n");
    assert_means(&doc, "[0, 0, 2]\n");
}

#[test]
fn new_value_anchor_does_not_capture_aliases() {
    let new = Document::from_str("k: {e: &y 2}\n").unwrap();
    let doc = Document::from_str("b: {c: &y 1}\nd: *y\n").unwrap();
    doc.as_mapping()
        .unwrap()
        .set("b", new.as_mapping().unwrap().get("k").unwrap());
    assert_edited(&doc, "b: {e: &y 2}\nd: &y 1\n");
    assert_means(&doc, "b: {e: 2}\nd: 1\n");
}

#[test]
fn mapping_remove_copies_anchored_node() {
    let cases = [
        (
            "b: &o {c: &y 1}\nd: *o\ne: *y\n",
            "d: &o {c: &y 1}\ne: *y\n",
            Some("d: {c: 1}\ne: 1\n"),
        ),
        // `*m` refers to `m2`, which the copy of `b` now precedes.
        (
            "b: &o {c: &m 2}\nm2: &m 1\ne: *o\nf: *m\n",
            "m2: &m 1\ne: &o {c: 2}\nf: *m\n",
            Some("m2: 1\ne: {c: 2}\nf: 1\n"),
        ),
        // An alias inside the copy to the dropped anchor is expanded too.
        (
            "b: &o {c: &m 2, d: *m}\nm2: &m 1\ne: *o\nf: *m\n",
            "m2: &m 1\ne: &o {c: 2, d: 2}\nf: *m\n",
            Some("m2: 1\ne: {c: 2, d: 2}\nf: 1\n"),
        ),
        // Inside the copy `*z` would refer to `d`, so it is expanded.
        (
            "a: &z 1\nb: &o {c: *z}\nd: &z 2\ne: *o\n",
            "a: &z 1\nd: &z 2\ne: &o {c: 1}\n",
            Some("a: 1\nd: 2\ne: {c: 1}\n"),
        ),
        (
            "b: &o {c: &q 1, d: *q}\ne: *o\n",
            "e: &o {c: &q 1, d: *q}\n",
            Some("e: {c: 1, d: 1}\n"),
        ),
        // A recursive anchor is copied whole.
        ("b: &a [1, *a]\nc: *a\n", "c: &a [1, *a]\n", None),
    ];
    for (source, expected, meaning) in cases {
        let doc = Document::from_str(source).unwrap();
        doc.as_mapping().unwrap().remove("b");
        assert_edited(&doc, expected);
        if let Some(meaning) = meaning {
            assert_means(&doc, meaning);
        }
    }
}

#[test]
fn mapping_remove_keeps_anchors() {
    let cases = [
        (
            "a: &x 1\nb: *x\nc: *x\n",
            "b: &x 1\nc: *x\n",
            "b: 1\nc: 1\n",
        ),
        ("&k a: 1\nb: *k\n", "b: &k a\n", "b: a\n"),
        (
            "a:\n  - &x\n    e: 1\nb:\n  - *x\n",
            "b:\n  - &x\n    e: 1\n",
            "b: [{e: 1}]\n",
        ),
        ("a: &x 1\nb: [*x, *x]\n", "b: [&x 1, *x]\n", "b: [1, 1]\n"),
        (
            "a: &x\n  e: 1\nb: [*x]\n",
            "b: [&x {e: 1}]\n",
            "b: [{e: 1}]\n",
        ),
        (
            "a: &x\n  e: 1\nb: {c: *x}\n",
            "b: {c: &x {e: 1}}\n",
            "b: {c: {e: 1}}\n",
        ),
        ("a: &x k\n*x : 1\n", "&x k : 1\n", "k: 1\n"),
        ("a: &x [k]\n*x : 1\n", "&x [k] : 1\n", "[k]: 1\n"),
        ("a: &x [k]\n? *x\n: 1\n", "? &x [k]\n: 1\n", "[k]: 1\n"),
        (
            "a: !!map &x\n  e: 1\nb: [*x]\n",
            "b: [!!map &x {e: 1}]\n",
            "b: [!!map {e: 1}]\n",
        ),
        (
            "a: {p: &x k, q: &y 2}\n*x : *y\n",
            "&x k : &y 2\n",
            "k: 2\n",
        ),
        // `:` may be part of an anchor name (W5VH).
        ("a: &a: 1\nb: *a:\n", "b: &a: 1\n", "b: 1\n"),
        ("a: &x: k\n*x: : 1\n", "&x: k : 1\n", "k: 1\n"),
    ];
    for (source, expected, meaning) in cases {
        let doc = Document::from_str(source).unwrap();
        assert!(doc.as_mapping().unwrap().remove("a").is_some());
        assert_edited(&doc, expected);
        assert_means(&doc, meaning);
    }
}

#[test]
fn mapping_remove_keeps_merge_source() {
    let doc = Document::from_str("base: &base {x: 1}\nchild:\n  <<: *base\n  y: 2\n").unwrap();
    let before = doc.get_resolved("child").unwrap();
    doc.as_mapping().unwrap().remove("base");
    assert_edited(&doc, "child:\n  <<: &base {x: 1}\n  y: 2\n");
    assert_eq!(doc.get_resolved("child").unwrap(), before);

    let doc = Document::from_str("base: &base {x: 1}\nchild: {<<: [*base], y: 2}\n").unwrap();
    let before = doc.get_resolved("child").unwrap();
    doc.as_mapping().unwrap().remove("base");
    assert_edited(&doc, "child: {<<: [&base {x: 1}], y: 2}\n");
    assert_eq!(doc.get_resolved("child").unwrap(), before);
}

#[test]
fn other_mapping_removals_keep_anchors() {
    let doc = Document::from_str("a: &x 1\na: 2\nb: *x\n").unwrap();
    doc.as_mapping().unwrap().remove_nth_occurrence("a", 0);
    assert_edited(&doc, "a: 2\nb: &x 1\n");

    let doc = Document::from_str("a: {c: &x 1}\nb: *x\n").unwrap();
    doc.as_mapping().unwrap().get_mapping("a").unwrap().clear();
    assert_edited(&doc, "a: {}\nb: &x 1\n");

    let doc = Document::from_str("a: &x 1\nb: *x\n").unwrap();
    let mapping = doc.as_mapping().unwrap();
    mapping
        .find_all_entries_by_key("a")
        .next()
        .unwrap()
        .remove();
    assert_edited(&doc, "b: &x 1\n");

    let doc = Document::from_str("a: &x 1\nb: *x\n").unwrap();
    let mapping = doc.as_mapping().unwrap();
    match mapping.entry("a") {
        yaml_edit::Entry::Occupied(e) => {
            e.remove_entry();
        }
        yaml_edit::Entry::Vacant(_) => panic!("a is present"),
    }
    assert_edited(&doc, "b: &x 1\n");

    let doc = Document::from_str("a: {c: &x 1}\nb: *x\n").unwrap();
    doc.as_mapping()
        .unwrap()
        .insert_at_index_preserving(0, "a", 6);
    assert_edited(&doc, "a: 6\nb: &x 1\n");

    let doc = Document::from_str("a: &x 1\nb: 2\nc: *x\n").unwrap();
    doc.as_mapping().unwrap().move_after("b", "a", 6);
    assert_edited(&doc, "b: 2\na: 6\nc: &x 1\n");

    let doc = Document::from_str("a: &x 1\nb: *x\n").unwrap();
    assert!(doc.remove("a").is_some());
    assert_edited(&doc, "b: &x 1\n");
}

#[test]
fn sequence_edits_keep_anchors() {
    let doc = Document::from_str("- {c: &y 1}\n- *y\n").unwrap();
    doc.as_sequence().unwrap().set(0, 6);
    assert_edited(&doc, "- 6\n- &y 1\n");

    let doc = Document::from_str("[{c: &y 1}, *y]\n").unwrap();
    doc.as_sequence().unwrap().set(0, 6);
    assert_edited(&doc, "[6, &y 1]\n");

    let doc = Document::from_str("- &y 1\n- *y\n- *y\n").unwrap();
    doc.as_sequence().unwrap().remove(0);
    assert_edited(&doc, "- &y 1\n- *y\n");
    assert_means(&doc, "[1, 1]\n");

    let doc = Document::from_str("- [&y 1, 2]\n- *y\n").unwrap();
    doc.as_sequence().unwrap().pop();
    assert_edited(&doc, "- [&y 1, 2]\n");

    let doc = Document::from_str("a: [&y 1, 2]\nb: *y\n").unwrap();
    doc.as_mapping().unwrap().get_sequence("a").unwrap().clear();
    assert_edited(&doc, "a: []\nb: &y 1\n");
}

#[test]
fn copy_of_empty_node_leaves_no_trailing_space() {
    let doc = Document::from_str("- &x\n- *x\n").unwrap();
    doc.as_sequence().unwrap().remove(0);
    assert_edited(&doc, "- &x\n");

    let doc = Document::from_str("a: &x\nb: *x\n").unwrap();
    doc.as_mapping().unwrap().remove("a");
    assert_edited(&doc, "b: &x\n");
}

#[test]
fn path_edits_keep_anchors() {
    let doc = Document::from_str("a:\n  b: {c: &y 1}\nd: *y\n").unwrap();
    doc.try_set_path("a.b", 2).unwrap();
    assert_edited(&doc, "a:\n  b: 2\nd: &y 1\n");

    let doc = Document::from_str("a:\n  b: {c: &y 1}\n  e: 3\nd: *y\n").unwrap();
    doc.try_remove_path("a.b").unwrap();
    assert_edited(&doc, "a:\n  e: 3\nd: &y 1\n");
}

#[test]
fn other_documents_are_left_alone() {
    let file = YamlFile::from_str("b: {c: &y 1}\nd: *y\n---\ne: *y\n").unwrap();
    file.document().unwrap().as_mapping().unwrap().set("b", 6);
    assert_eq!(file.to_string(), "b: 6\nd: &y 1\n---\ne: *y\n");
    assert_file_cst_ok(&file);
}

#[test]
fn alias_with_properties_gets_a_copy() {
    // YAML does not allow properties on an alias, but the parser accepts
    // them. The copy takes them over, and so leaves off the anchor it was
    // copied with: a tag would change what later aliases refer to, and a
    // node has room for one anchor only.
    let cases = [
        ("a: &a 1\nk: !t *a\n", "k: !t 1\n"),
        ("a: &a 1\nk: !t *a\nl: *a\n", "k: !t 1\nl: &a 1\n"),
        ("a: &a 1\nk: &b *a\nl: *b\n", "k: &b 1\nl: *b\n"),
        ("a: &a 1\nk: &b *a\nl: *a\n", "k: &b 1\nl: &a 1\n"),
        ("a: &a 1\nk: !t &b *a\nl: *b\n", "k: &b !t 1\nl: *b\n"),
        ("a: !u &a 1\nk: &b *a\nl: *a\n", "k: &b !u 1\nl: !u &a 1\n"),
        ("a: &a !u 1\nk: &b *a\nl: *a\n", "k: &b !u 1\nl: &a !u 1\n"),
        ("a: &a 1\nk: [!t *a]\n", "k: [!t 1]\n"),
        ("a: &a 1\nk: {l: &b *a}\n", "k: {l: &b 1}\n"),
        ("a: &a\n  x: 1\nk: !t *a\n", "k: !t\n  x: 1\n"),
        ("a: &a\n  x: 1\nk:\n- !t *a\n", "k:\n- !t\n  x: 1\n"),
        ("a: &a\n- 1\nk:\n  l: !t *a\n", "k:\n  l: !t\n    - 1\n"),
        ("a: &a |\n  x\nk: !t *a\n", "k: !t |\n  x\n"),
        (
            "a: &a {x: &c 1}\nk: !t *a\nl: *c\n",
            "k: !t {x: &c 1}\nl: *c\n",
        ),
    ];
    for (source, expected) in cases {
        let doc = Document::from_str(source).unwrap();
        doc.as_mapping().unwrap().remove("a");
        assert_edited(&doc, expected);
    }

    let doc = Document::from_str("- &a 1\n- !t *a\n- &b *a\n- *b\n").unwrap();
    doc.as_sequence().unwrap().remove(0);
    assert_edited(&doc, "- !t 1\n- &b 1\n- *b\n");
}

#[test]
fn tagged_alias_to_tagged_node_keeps_its_own_tag() {
    // The tag of the alias is what was written at its place, so it takes
    // the place of that of the node.
    let cases = [
        ("a: &a !u 1\nk: !t *a\n", "k: !t 1\n"),
        ("a: !u &a 1\nk: [!t *a]\nl: *a\n", "k: [!t 1]\nl: !u &a 1\n"),
        ("a: &a !u\n  x: 1\nk: !t *a\n", "k: !t\n  x: 1\n"),
        (
            "a: &a !u 1\nk: !t &b *a\nl: *a\nm: *b\n",
            "k: &b !t 1\nl: &a !u 1\nm: *b\n",
        ),
    ];
    for (source, expected) in cases {
        let doc = Document::from_str(source).unwrap();
        doc.as_mapping().unwrap().remove("a");
        assert_edited(&doc, expected);
    }
}

#[test]
fn key_alias_with_properties_gets_a_copy() {
    let cases = [
        ("a: &a 1\n? !t *a\n: v\n", "? !t 1\n: v\n"),
        ("a: &a 1\n? &b *a\n: v\nc: *b\n", "? &b 1\n: v\nc: *b\n"),
        (
            "a: &a 1\n? !t &b *a\n: v\nc: *b\n",
            "? &b !t 1\n: v\nc: *b\n",
        ),
        ("a: &a !u 1\n? !t *a\n: v\n", "? !t 1\n: v\n"),
        ("a: &a\n  x: 1\n? !t *a\n: v\n", "? !t {x: 1}\n: v\n"),
        ("a: &a 1\nb: {!t *a : v}\n", "b: {!t 1 : v}\n"),
        ("a: &a 1\nb: {&b *a : v, c: *b}\n", "b: {&b 1 : v, c: *b}\n"),
    ];
    for (source, expected) in cases {
        let doc = Document::from_str(source).unwrap();
        doc.as_mapping().unwrap().remove("a");
        assert_edited(&doc, expected);
    }
}

#[test]
fn rename_key_keeps_anchors() {
    let doc = Document::from_str("&k a: 1\nb: *k\n").unwrap();
    assert!(doc.as_mapping().unwrap().rename_key("a", "c"));
    assert_edited(&doc, "&k c: 1\nb: *k\n");
    assert_means(&doc, "c: 1\nb: c\n");

    let doc = Document::from_str("[&y p]: 1\nb: *y\n").unwrap();
    let mapping = doc.as_mapping().unwrap();
    let key = mapping.keys().next().unwrap();
    assert!(mapping.rename_key(&key, "c"));
    assert_edited(&doc, "c: 1\nb: &y p\n");
    assert_means(&doc, "c: 1\nb: p\n");
}

#[test]
fn tagged_alias_to_block_mapping_with_flow_key() {
    // The mapping starts with its key's `{` or `[`, but is still a block
    // mapping, which goes on the line after the tag.
    for (source, expected) in [
        ("a: &a\n  {x: 1}: 2\nk: !t *a\n", "k: !t\n  {x: 1}: 2\n"),
        ("a: &a\n  [x]: 2\nk:\n- !t *a\n", "k:\n- !t\n  [x]: 2\n"),
    ] {
        let doc = Document::from_str(source).unwrap();
        doc.as_mapping().unwrap().remove("a");
        assert_edited(&doc, expected);
    }
}

#[test]
fn remove_item_with_anchor_text_in_plain_scalar() {
    // The second item is the plain scalar `f - &SS Sammy`, which defines
    // no anchor.
    let doc = Document::from_str("- &SS x\n- f\n  - &SS Sammy\n- *SS\n").unwrap();
    doc.as_sequence().unwrap().remove(1);
    assert_edited(&doc, "- &SS x\n- *SS\n");
}

#[test]
fn reorder_fields_moves_anchor_to_first_alias() {
    let cases: [(&str, &[&str], &str); 19] = [
        (
            "a: &x 1\nb: *x\nc: 2\n",
            &["b", "a"],
            "b: &x 1\na: *x\nc: 2\n",
        ),
        (
            "a: &x 1\nb: *x\nc: *x\n",
            &["b", "c", "a"],
            "b: &x 1\nc: *x\na: *x\n",
        ),
        (
            "a: &x 1\nb: *x\nc: 3\n",
            &["c", "b", "a"],
            "c: 3\nb: &x 1\na: *x\n",
        ),
        (
            "a: {k: &x 1}\nb: *x\n",
            &["b", "a"],
            "b: &x 1\na: {k: *x}\n",
        ),
        (
            "a: [&x 1, 2]\nb: *x\n",
            &["b", "a"],
            "b: &x 1\na: [*x, 2]\n",
        ),
        (
            "a: &x\n  k: v\nb: *x\n",
            &["b", "a"],
            "b: &x\n  k: v\na: *x\n",
        ),
        ("a: &x !t 1\nb: *x\n", &["b", "a"], "b: &x !t 1\na: *x\n"),
        ("a: !t &x 1\nb: *x\n", &["b", "a"], "b: !t &x 1\na: *x\n"),
        ("a: &x 1\nb: [*x]\n", &["b", "a"], "b: [&x 1]\na: *x\n"),
        (
            "{a: &x 1, b: *x, c: 2}\n",
            &["b", "a"],
            "{b: &x 1, a: *x, c: 2}\n",
        ),
        // The copy keeps the anchors inside the node, so that it can
        // become an alias without losing `&y`.
        (
            "a: &x {k: &y 1}\nb: *x\nc: *y\n",
            &["b", "a", "c"],
            "b: &x {k: &y 1}\na: *x\nc: *y\n",
        ),
        (
            "a: &x [&y 1]\nb: *x\n",
            &["b", "a"],
            "b: &x [&y 1]\na: *x\n",
        ),
        (
            "a: &x [&y 1]\nb: [*x, *y]\n",
            &["b", "a"],
            "b: [&x [&y 1], *y]\na: *x\n",
        ),
        (
            "base: &x {a: 1}\nderived:\n  <<: *x\n  b: 2\n",
            &["derived"],
            "derived:\n  <<: &x {a: 1}\n  b: 2\nbase: *x\n",
        ),
        // A key becomes an alias as a value does.
        ("&k a: 1\nb: *k\n", &["b"], "b: &k a\n*k : 1\n"),
        ("!t &k a: 1\nb: *k\n", &["b"], "b: !t &k a\n*k : 1\n"),
        ("{&k a: 1, b: *k}\n", &["b"], "{b: &k a, *k : 1}\n"),
        ("? &k a\n: 1\nb: *k\n", &["b"], "b: &k a\n? *k\n: 1\n"),
        // An alias key takes the anchor as an alias value does.
        (
            "a: &a 1\nb: {*a : 2}\n",
            &["b", "a"],
            "b: {&a 1 : 2}\na: *a\n",
        ),
    ];
    for (source, order, expected) in cases {
        let doc = Document::from_str(source).unwrap();
        assert!(doc.reorder_fields(order.iter().copied()));
        assert_edited(&doc, expected);
        assert_means(&doc, source);
    }
}

#[test]
fn reorder_fields_keeps_repeated_anchor_names_apart() {
    let cases: [(&str, &[&str], &str); 2] = [
        // `c` referred to `b`, which `a` now comes between.
        (
            "a: &x 1\nb: &x 2\nc: *x\n",
            &["b", "a"],
            "b: &x 2\na: &x 1\nc: 2\n",
        ),
        // `b` gets `a`'s anchor, but `a` cannot become an alias as `c`
        // redefines the name in between.
        (
            "a: &x 1\nb: *x\nc: &x 2\nd: *x\n",
            &["b", "c", "a", "d"],
            "b: &x 1\nc: &x 2\na: &x 1\nd: 2\n",
        ),
    ];
    for (source, order, expected) in cases {
        let doc = Document::from_str(source).unwrap();
        assert!(doc.reorder_fields(order.iter().copied()));
        assert_edited(&doc, expected);
        assert_means(&doc, source);
    }
}

#[test]
fn move_keeps_aliases_resolving() {
    let doc = Document::from_str("a: &x 1\nb: *x\nc: 2\n").unwrap();
    let mapping = doc.as_mapping().unwrap();
    assert!(mapping.move_after("c", "a", mapping.get("a").unwrap()));
    assert_edited(&doc, "b: &x 1\nc: 2\na: *x\n");
    assert_means(&doc, "a: 1\nb: 1\nc: 2\n");

    let doc = Document::from_str("a: &x 1\nb: *x\n").unwrap();
    let mapping = doc.as_mapping().unwrap();
    assert!(mapping.move_before("a", "b", mapping.get("b").unwrap()));
    assert_edited(&doc, "b: &x 1\na: *x\n");
    assert_means(&doc, "a: 1\nb: 1\n");

    let doc = Document::from_str("{a: &x [1], b: *x, c: 2}\n").unwrap();
    let mapping = doc.as_mapping().unwrap();
    assert!(mapping.move_after("c", "a", mapping.get("a").unwrap()));
    assert_edited(&doc, "{b: &x [1], c: 2, a: *x}\n");
    assert_means(&doc, "{a: [1], b: [1], c: 2}\n");

    let doc = Document::from_str("a: {k: &x 1}\nb: *x\n").unwrap();
    assert!(doc.move_before("a", "b", doc.as_mapping().unwrap().get("b").unwrap()));
    assert_edited(&doc, "b: &x 1\na: {k: *x}\n");
    assert_means(&doc, "a: {k: 1}\nb: 1\n");
}

#[test]
fn move_keeps_block_and_tagged_values() {
    let cases = [
        (
            "a: &x\n  k: v\nb: *x\nc: 2\n",
            "c",
            "b: &x\n  k: v\nc: 2\na: *x\n",
        ),
        (
            "a: !t &x 1\nb: *x\nc: 2\n",
            "c",
            "b: !t &x 1\nc: 2\na: *x\n",
        ),
        ("a: &x 1\nb: 2\nc: *x\n", "b", "b: 2\na: &x 1\nc: *x\n"),
        ("a: &x 1\nb: 2\nc: 3\n", "b", "b: 2\na: &x 1\nc: 3\n"),
        ("&k a: 1\nb: *k\nc: 2\n", "c", "b: &k a\nc: 2\n*k : 1\n"),
    ];
    for (source, after, expected) in cases {
        let doc = Document::from_str(source).unwrap();
        let mapping = doc.as_mapping().unwrap();
        assert!(mapping.move_after(after, "a", mapping.get("a").unwrap()));
        assert_edited(&doc, expected);
        assert_means(&doc, source);
    }
}

#[test]
fn reorder_fields_copies_alias_with_properties() {
    // The copy takes over the tag or anchor of the alias, so the node it
    // was copied from keeps its anchor.
    let cases: [(&str, &[&str], &str); 2] = [
        ("a: &x 1\nb: !t *x\n", &["b", "a"], "b: !t 1\na: &x 1\n"),
        (
            "a: &x 1\nb: &b *x\nc: *b\n",
            &["b", "a", "c"],
            "b: &b 1\na: &x 1\nc: *b\n",
        ),
    ];
    for (source, order, expected) in cases {
        let doc = Document::from_str(source).unwrap();
        assert!(doc.reorder_fields(order.iter().copied()));
        assert_edited(&doc, expected);
    }
}

#[test]
fn reorder_fields_copies_tagged_alias_to_tagged_node() {
    let doc = Document::from_str("a: &x !u 1\nb: !t *x\n").unwrap();
    assert!(doc.reorder_fields(["b", "a"]));
    assert_edited(&doc, "b: !t 1\na: &x !u 1\n");
}

#[test]
fn anchor_name_with_colon() {
    // `:` is allowed in an anchor name (the test suite's W5VH).
    let doc = Document::from_str("a: &a: 1\nb: *a:\n").unwrap();
    assert!(doc.reorder_fields(["b", "a"]));
    assert_edited(&doc, "b: &a: 1\na: *a:\n");
    assert_means(&doc, "a: 1\nb: 1\n");

    let doc = Document::from_str("a: &a: 1\nb: *a:\nc: 2\n").unwrap();
    let mapping = doc.as_mapping().unwrap();
    assert!(mapping.move_after("c", "a", mapping.get("a").unwrap()));
    assert_edited(&doc, "b: &a: 1\nc: 2\na: *a:\n");
}

#[test]
fn moving_alias_key_takes_anchor() {
    for source in ["a: &a 1\n*a : 2\nc: 3\n", "{a: &a 1, *a : 2, c: 3}\n"] {
        let doc = Document::from_str(source).unwrap();
        assert!(doc.reorder_fields([alias_key()]));
        assert_means(&doc, source);

        let doc = Document::from_str(source).unwrap();
        let mapping = doc.as_mapping().unwrap();
        let value = mapping.get(alias_key()).unwrap();
        assert!(mapping.move_before("a", alias_key(), value));
        assert_means(&doc, source);

        let doc = Document::from_str(source).unwrap();
        let mapping = doc.as_mapping().unwrap();
        assert!(mapping.move_after("c", "a", mapping.get("a").unwrap()));
        assert_means(&doc, source);
    }

    let doc = Document::from_str("a: &a 1\n*a : 2\nc: 3\n").unwrap();
    let mapping = doc.as_mapping().unwrap();
    assert!(mapping.move_before("a", alias_key(), mapping.get(alias_key()).unwrap()));
    assert_edited(&doc, "&a 1 : 2\na: *a\nc: 3\n");

    let doc = Document::from_str("{a: &a 1, *a : 2, c: 3}\n").unwrap();
    let mapping = doc.as_mapping().unwrap();
    assert!(mapping.move_after("c", "a", mapping.get("a").unwrap()));
    assert_edited(&doc, "{&a 1 : 2, c: 3, a: *a}\n");
}

#[test]
fn removing_anchor_of_alias_key_copies_it() {
    let cases = [
        ("a: &a 1\n*a : 2\nc: 3\n", "&a 1 : 2\nc: 3\n"),
        ("{a: &a 1, *a : 2, c: 3}\n", "{&a 1 : 2, c: 3}\n"),
        ("&k a: 1\nb: 2\n*k : 3\n", "b: 2\n&k a : 3\n"),
    ];
    for (source, expected) in cases {
        let doc = Document::from_str(source).unwrap();
        assert!(doc.as_mapping().unwrap().remove("a").is_some());
        assert_edited(&doc, expected);
    }
}

#[test]
fn move_with_new_value_keeps_alias_key_resolving() {
    let cases = [
        ("a: &a 1\n*a : 2\nc: 3\n", "&a 1 : 5\na: *a\nc: 3\n"),
        ("{a: &a 1, *a : 2, c: 3}\n", "{&a 1 : 5, a: *a, c: 3}\n"),
    ];
    for (source, expected) in cases {
        let doc = Document::from_str(source).unwrap();
        let mapping = doc.as_mapping().unwrap();
        assert!(mapping.move_before("a", alias_key(), 5));
        assert_edited(&doc, expected);
    }

    // The old value still goes as with remove.
    let doc = Document::from_str("a: &a 1\n*a : 2\nb: *a\n").unwrap();
    let mapping = doc.as_mapping().unwrap();
    assert!(mapping.move_after("b", "a", 5));
    assert_edited(&doc, "&a 1 : 2\nb: *a\na: 5\n");
}

#[test]
fn moving_alias_key_before_key_with_anchors_inside() {
    // X38W from the YAML test suite: `*b` after the old key still resolves
    // once that key becomes an alias, to the `&b` in the copy. Both keys
    // are `[a, b]`, so the values cannot be compared.
    let source = "{ &a [a, &b b]: *b, *a : [c, *b, d]}\n";
    let expected = "{ &a [a, &b b] : [c, *b, d], *a : *b}\n";

    let doc = Document::from_str(source).unwrap();
    let keys: Vec<_> = doc.as_mapping().unwrap().keys().collect();
    assert!(doc.reorder_fields(keys.into_iter().rev()));
    assert_edited(&doc, expected);

    let doc = Document::from_str(source).unwrap();
    let mapping = doc.as_mapping().unwrap();
    let keys: Vec<_> = mapping.keys().collect();
    let value = mapping.get(&keys[0]).unwrap();
    assert!(mapping.move_after(&keys[1], &keys[0], value));
    assert_edited(&doc, expected);
}

#[test]
fn moving_alias_key_before_key_with_unused_anchor_inside() {
    // The copy keeps `&b`, though no alias needs it, so that the old key
    // can become an alias. If another `&b` comes between, it cannot, and
    // both stay copies, the first without `&b`. The two keys were equal
    // all along, but the validator only sees that once neither is an
    // alias.
    let cases = [
        ("{ &a [&b b]: x, *a : c}\n", 1, "{ &a [&b b] : c, *a : x}\n"),
        ("&a [&b b]: x\n*a : c\n", 1, "&a [&b b] : c\n*a : x\n"),
        (
            "{ &a [&b b]: x, *a : c, &b y: z}\n",
            2,
            "{ &a [b] : c, &b y: z, &a [&b b]: x}\n",
        ),
        (
            "&a [&b b]: x\n*a : c\n&b y: z\n",
            2,
            "&a [b] : c\n&b y: z\n&a [&b b]: x\n",
        ),
    ];
    for (source, after, expected) in cases {
        let doc = Document::from_str(source).unwrap();
        let mapping = doc.as_mapping().unwrap();
        let keys: Vec<_> = mapping.keys().collect();
        let value = mapping.get(&keys[0]).unwrap();
        assert!(mapping.move_after(&keys[after], &keys[0], value));
        assert_edited(&doc, expected);
    }
}

#[test]
fn moving_value_before_alias_with_unused_anchor_inside() {
    let cases = [
        ("{a: &x [&y 1], b: *x}\n", "b", "{b: &x [&y 1], a: *x}\n"),
        ("a: &x [&y 1]\nb: *x\n", "b", "b: &x [&y 1]\na: *x\n"),
        (
            "a: &x [&y 1]\nb: *x\nc: &y 2\n",
            "c",
            "b: &x [1]\nc: &y 2\na: &x [&y 1]\n",
        ),
    ];
    for (source, after, expected) in cases {
        let doc = Document::from_str(source).unwrap();
        let mapping = doc.as_mapping().unwrap();
        assert!(mapping.move_after(after, "a", mapping.get("a").unwrap()));
        assert_edited(&doc, expected);
        assert_means(&doc, source);
    }
}

#[test]
fn removing_anchor_of_alias_copies_anchors_inside() {
    // A removed node leaves nothing to become an alias, so the copy is the
    // only one and keeps all its anchors, even where that makes two keys
    // the validator sees as equal.
    let cases = [
        ("{ k: &a [x], [x]: 1, *a : 2}\n", "{ [x]: 1, &a [x] : 2}\n"),
        ("k: &a [&b x]\nv: *a\n", "v: &a [&b x]\n"),
    ];
    for (source, expected) in cases {
        let doc = Document::from_str(source).unwrap();
        assert!(doc.as_mapping().unwrap().remove("k").is_some());
        assert_edited(&doc, expected);
    }
}

/// The value `key` of `source`, a node with whatever anchor and tag it has.
fn node_from(source: &str, key: &str) -> YamlNode {
    let doc = Document::from_str(source).unwrap();
    let mapping = doc.as_mapping().unwrap();
    mapping.get(key).unwrap()
}

#[test]
fn mapping_set_keeps_anchor() {
    let cases = [
        ("b: &x 2\nc: *x\n", "b: &x 6\nc: *x\n"),
        ("b: &x 2  # c\nc: *x\n", "b: &x 6  # c\nc: *x\n"),
        ("b: &x !!int 2\nc: *x\n", "b: &x 6\nc: *x\n"),
        ("b: !!int &x 2\nc: *x\n", "b: &x 6\nc: *x\n"),
        ("b: &x\n  d: 1\nc: *x\n", "b: &x 6\nc: *x\n"),
        ("b: &x [1, 2]\nc: *x\n", "b: &x 6\nc: *x\n"),
        ("b: &x |\n  hi\nc: *x\n", "b: &x 6\nc: *x\n"),
        ("{b: &x 2, c: *x}\n", "{b: &x 6, c: *x}\n"),
    ];
    for (source, expected) in cases {
        let doc = Document::from_str(source).unwrap();
        doc.as_mapping().unwrap().set("b", 6);
        assert_edited(&doc, expected);
        assert_eq!(doc.get_resolved("c").unwrap(), Some(YamlValue::from(6)));
    }
}

#[test]
fn mapping_set_block_value_keeps_anchor() {
    let doc = Document::from_str("b: &x 2\nc: *x\n").unwrap();
    let new = Document::from_str("e: 1\nf: 2\n").unwrap();
    doc.as_mapping()
        .unwrap()
        .set("b", new.as_mapping().unwrap());
    assert_edited(&doc, "b: &x\n  e: 1\n  f: 2\nc: *x\n");
    assert_eq!(
        doc.get_resolved("c").unwrap(),
        doc.get_resolved("b").unwrap()
    );

    let doc = Document::from_str("b: !!int &x 2\nc: *x\n").unwrap();
    doc.as_mapping()
        .unwrap()
        .set("b", node_from("k: |\n  text\n", "k"));
    assert_edited(&doc, "b: &x |\n  text\nc: *x\n");
    assert_eq!(
        doc.get_resolved("c").unwrap(),
        Some(YamlValue::from("text\n"))
    );
}

#[test]
fn mapping_set_tagged_value_keeps_anchor() {
    let doc = Document::from_str("b: &x 2\nc: *x\n").unwrap();
    doc.as_mapping()
        .unwrap()
        .set("b", node_from("k: !!str 6\n", "k"));
    assert_edited(&doc, "b: &x !!str 6\nc: *x\n");
    assert_eq!(
        doc.get_resolved("c").unwrap(),
        doc.get_resolved("b").unwrap()
    );
}

#[test]
fn mapping_set_old_anchor_wins_over_new() {
    // The old anchor is what the document's aliases refer to; the new
    // value's anchor stays behind on the node it was copied from.
    let cases = [
        ("k: &y 5\n", "b: &x 5\nc: *x\n"),
        ("k: &y !!int 5\n", "b: &x !!int 5\nc: *x\n"),
        ("k: !!int &y 5\n", "b: &x !!int 5\nc: *x\n"),
        ("k: &y\n  e: 5\n", "b: &x\n  e: 5\nc: *x\n"),
        ("k: !!map &y\n  e: 5\n", "b: &x !!map\n  e: 5\nc: *x\n"),
    ];
    for (source, expected) in cases {
        for old in ["b: &x 2\nc: *x\n", "b: !!int &x 2\nc: *x\n"] {
            let doc = Document::from_str(old).unwrap();
            doc.as_mapping().unwrap().set("b", node_from(source, "k"));
            assert_edited(&doc, expected);
        }
    }
}

#[test]
fn mapping_set_keeps_new_anchor_without_old() {
    let cases = [
        ("k: &y 5\n", "b: &y 5\n"),
        ("k: !!int &y 5\n", "b: !!int &y 5\n"),
        ("k: &y\n  e: 5\n", "b: &y\n  e: 5\n"),
    ];
    for (source, expected) in cases {
        let doc = Document::from_str("b: 2\n").unwrap();
        doc.as_mapping().unwrap().set("b", node_from(source, "k"));
        assert_edited(&doc, expected);
    }
}

#[test]
fn sequence_set_keeps_anchor() {
    let cases = [
        ("- &x 2\n- *x\n", "- &x 6\n- *x\n"),
        ("- !!int &x 2\n- *x\n", "- &x 6\n- *x\n"),
        ("- &x !!int 2\n- *x\n", "- &x 6\n- *x\n"),
        ("[&x 2, *x]\n", "[&x 6, *x]\n"),
        ("[!!int &x 2, *x]\n", "[&x 6, *x]\n"),
        ("- &x\n  a: 1\n- *x\n", "- &x 6\n- *x\n"),
    ];
    for (source, expected) in cases {
        let doc = Document::from_str(source).unwrap();
        doc.as_sequence().unwrap().set(0, 6);
        assert_edited(&doc, expected);
    }
}

#[test]
fn nested_sequence_set_keeps_anchor() {
    let doc = Document::from_str("- - &x 2\n- *x\n").unwrap();
    let outer = doc.as_sequence().unwrap();
    outer.get(0).unwrap().as_sequence().unwrap().set(0, 6);
    assert_edited(&doc, "- - &x 6\n- *x\n");
}

#[test]
fn sequence_set_block_value_keeps_anchor() {
    for source in ["- &x 2\n- *x\n", "- !!int &x 2\n- *x\n"] {
        let doc = Document::from_str(source).unwrap();
        doc.as_sequence()
            .unwrap()
            .set(0, node_from("k:\n  e: 1\n  f: 2\n", "k"));
        assert_edited(&doc, "- &x\n  e: 1\n  f: 2\n- *x\n");
    }

    let doc = Document::from_str("- &x 2\n- *x\n").unwrap();
    doc.as_sequence()
        .unwrap()
        .set(0, node_from("k: |\n  text\n", "k"));
    assert_edited(&doc, "- &x |\n  text\n- *x\n");

    // A tagged block collection is indented under the dash, not past the
    // anchor, as it is without one.
    let doc = Document::from_str("- &x 2\n- *x\n").unwrap();
    doc.as_sequence()
        .unwrap()
        .set(0, node_from("k: !t\n  e: 1\n", "k"));
    assert_edited(&doc, "- &x !t\n  e: 1\n- *x\n");
    assert_eq!(
        doc.as_sequence()
            .unwrap()
            .get(1)
            .unwrap()
            .as_alias()
            .unwrap()
            .name(),
        "x"
    );
}

#[test]
fn sequence_set_old_anchor_wins_over_new() {
    let cases = [
        ("k: &y 5\n", "- &x 5\n- *x\n"),
        ("k: !!int &y 5\n", "- &x !!int 5\n- *x\n"),
        ("k: &y !!int 5\n", "- &x !!int 5\n- *x\n"),
        ("k: &y\n  e: 5\n", "- &x\n  e: 5\n- *x\n"),
    ];
    for (source, expected) in cases {
        for old in ["- &x 2\n- *x\n", "- !!int &x 2\n- *x\n"] {
            let doc = Document::from_str(old).unwrap();
            doc.as_sequence().unwrap().set(0, node_from(source, "k"));
            assert_edited(&doc, expected);
        }
    }
}

#[test]
fn sequence_set_keeps_new_anchor_without_old() {
    let cases = [
        ("k: &y 5\n", "- &y 5\n"),
        ("k: !!int &y 5\n", "- !!int &y 5\n"),
        ("k: &y !!int 5\n", "- &y !!int 5\n"),
        ("k: &y\n  e: 5\n", "- &y\n  e: 5\n"),
    ];
    for (source, expected) in cases {
        let doc = Document::from_str("- 2\n").unwrap();
        doc.as_sequence().unwrap().set(0, node_from(source, "k"));
        assert_edited(&doc, expected);
    }
}

#[test]
fn set_path_keeps_anchor() {
    let doc = Document::from_str("a:\n  b: &x 2\n  l: [&y 1]\nc: *x\nd: *y\n").unwrap();
    doc.try_set_path("a.b", 6).unwrap();
    doc.try_set_path("a.l[0]", 7).unwrap();
    assert_edited(&doc, "a:\n  b: &x 6\n  l: [&y 7]\nc: *x\nd: *y\n");
    assert_eq!(doc.get_resolved("c").unwrap(), Some(YamlValue::from(6)));
    assert_eq!(doc.get_resolved("d").unwrap(), Some(YamlValue::from(7)));
}

#[test]
fn new_key_keeps_anchor() {
    let cases = [
        ("k: &a key\n", "a: *a\n", "&a key: 2\na: *a\n", "key"),
        ("k: &a key\n", "{a: *a}\n", "{&a key: 2, a: *a}\n", "key"),
        (
            "k: &a key\n",
            "? a\n: *a\n",
            "? &a key\n: 2\n? a\n: *a\n",
            "key",
        ),
        (
            "k: &a !!str key\n",
            "{a: *a}\n",
            "{&a !!str key: 2, a: *a}\n",
            "!!str key",
        ),
        (
            "k: &a !!str key\n",
            "? a\n: *a\n",
            "? &a !!str key\n: 2\n? a\n: *a\n",
            "!!str key",
        ),
        ("k: [&a key]\n", "a: *a\n", "&a key: 2\na: *a\n", "key"),
        ("k: &a [key]\n", "a: *a\n", "&a [key]: 2\na: *a\n", "[key]"),
        (
            "k: &a\n  - key\n",
            "a: *a\n",
            "&a [key]: 2\na: *a\n",
            "[key]",
        ),
    ];
    for (source, target, expected, value) in cases {
        let key = node_from(source, "k");
        let key = match key.as_sequence() {
            Some(seq) if source.starts_with("k: [") => seq.get(0).unwrap(),
            _ => key,
        };
        let doc = Document::from_str(target).unwrap();
        doc.as_mapping().unwrap().insert_at_index(0, &key, 2);
        assert_edited(&doc, expected);
        let value = Document::from_str(&format!("x: {value}\n")).unwrap();
        assert_eq!(
            doc.get_resolved("a").unwrap(),
            value.get_resolved("x").unwrap()
        );
    }
}

#[test]
fn set_and_insert_new_key_keep_anchor() {
    let key = node_from("k: &a key\n", "k");
    let doc = Document::from_str("b: 1\n").unwrap();
    let mapping = doc.as_mapping().unwrap();
    mapping.insert_before("b", &key, 2);
    mapping.set("c", Alias::new("a"));
    assert_edited(&doc, "&a key: 2\nb: 1\nc: *a\n");

    let doc = Document::from_str("b: 1\n").unwrap();
    let mapping = doc.as_mapping().unwrap();
    mapping.insert_after("b", &key, 2);
    mapping.set(node_from("k: &c other\n", "k"), 3);
    assert_edited(&doc, "b: 1\n&a key: 2\n&c other: 3\n");
}

#[test]
fn new_tagged_key_keeps_anchor() {
    let key = node_from("k: &a !!str key\n", "k");
    let doc = Document::from_str("a: *a\n").unwrap();
    doc.as_mapping().unwrap().insert_at_index(0, &key, 2);
    assert_edited(&doc, "&a !!str key: 2\na: *a\n");
    let value = Document::from_str("x: !!str key\n").unwrap();
    assert_eq!(
        doc.get_resolved("a").unwrap(),
        value.get_resolved("x").unwrap()
    );
}

#[test]
fn insert_at_index_preserving_keeps_anchor_like_set() {
    let cases: [(&str, YamlNode, &str); 6] = [
        (
            "b: &x 2\nc: *x\n",
            node_from("k: 6\n", "k"),
            "b: &x 6\nc: *x\n",
        ),
        (
            "b: &x\n  d: 1\nc: *x\n",
            node_from("k: 6\n", "k"),
            "b: &x 6\nc: *x\n",
        ),
        (
            "{b: &x 2, c: *x}\n",
            node_from("k: 6\n", "k"),
            "{b: &x 6, c: *x}\n",
        ),
        (
            "b: !!int &x 2\nc: *x\n",
            node_from("k: &y 5\n", "k"),
            "b: &x 5\nc: *x\n",
        ),
        ("b: 2\n", node_from("k: &y 5\n", "k"), "b: &y 5\n"),
        (
            "a: &z 0\nb: &x 2\nc: *x\n",
            node_from("k: *z\n", "k"),
            "a: &z 0\nb: *z\nc: &x 2\n",
        ),
    ];
    for (source, value, expected) in cases {
        let doc = Document::from_str(source).unwrap();
        doc.as_mapping().unwrap().set("b", &value);
        assert_edited(&doc, expected);

        let doc = Document::from_str(source).unwrap();
        doc.as_mapping()
            .unwrap()
            .insert_at_index_preserving(0, "b", &value);
        assert_edited(&doc, expected);
    }
}
