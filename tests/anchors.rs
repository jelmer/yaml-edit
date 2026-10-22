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
use common::{
    assert_edited, assert_file_cst_ok, check_edit, count_nodes, edit_metadata, parse_clean,
    parse_with_violations, render_documents,
};
use rowan::ast::AstNode;
use std::str::FromStr;
use yaml_edit::anchor_resolution::DocumentResolvedExt;
use yaml_edit::path::YamlPath;
use yaml_edit::validator::Validator;
use yaml_edit::{
    debug, Alias, AnchorRegistry, Document, DocumentMergedExt, MappingMergedExt, MappingView,
    MergedMapping, ResolveError, SyntaxKind, YamlFile, YamlNode, YamlValue,
};

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
        ("key: &anchor value\nref: *anchor", "map{key: value, ref: *anchor}"),
        // Anchor names with digits, underscores and capitals.
        (
            "key1: &anchor_123 val1\nref1: *anchor_123",
            "map{key1: val1, ref1: *anchor_123}",
        ),
        (
            "key2: &AnchorName val2\nref2: *AnchorName",
            "map{key2: val2, ref2: *AnchorName}",
        ),
        (
            "key3: &anchor123abc val3\nref3: *anchor123abc",
            "map{key3: val3, ref3: *anchor123abc}",
        ),
        (
            "items:\n  - &first_item value1\n  - second_item\n  - *first_item",
            "map{items: seq[value1, second_item, *first_item]}",
        ),
        // Whitespace around anchors and aliases is kept.
        (
            "key:  &anchor   value  \nref:  *anchor  ",
            "map{key: value, ref: *anchor}",
        ),
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

    // The YAML test suite's 26DV has `*alias1 : scalar3` as an entry. The
    // alias was parsed as a value, so the `: v` after it opened a separate
    // null-key entry instead. An alias as a value is unaffected.
    for (yaml, mappings, expected) in [
        ("k:\n  *a : v\n", 2, "map{k: map{*a: v}}"),
        ("a: &x 1\nb: *x\n", 1, "map{a: 1, b: *x}"),
    ] {
        let file = parse_clean(yaml);
        assert_eq!(count_nodes(&file, SyntaxKind::MAPPING), mappings);
        assert_eq!(render_documents(yaml), [expected]);
    }
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

/// An anchor before a complex key annotates that key, so it belongs inside
/// the KEY node rather than beside the mapping.
///
/// `&key [ a ]: value` anchors the flow sequence. Left outside, it looked
/// like a second anchor on the mapping itself, so the test suite's 6BFJ
/// (`&mapping\n&key [ ... ]: value`) was reported as having two anchors on
/// one node, which 4JVG genuinely does.
#[test]
fn anchor_before_a_complex_key_is_inside_the_key() {
    let file = parse_clean("&key [ a ]: value\n");
    let tree = debug::tree_to_string(file.syntax());
    // The anchor is a child of KEY, not a sibling of MAPPING.
    let key_line = tree.lines().position(|l| l.trim() == "KEY").unwrap();
    assert!(
        tree.lines()
            .nth(key_line + 1)
            .is_some_and(|l| l.contains("ANCHOR")),
        "{tree}"
    );

    parse_with_violations("---\n&mapping\n&key [ a ]: value\n", &[]);
    // `&n1` anchors the nested mapping and `&k1` its key: different nodes,
    // as the test suite's 7BMT has it. Two anchors on one scalar still are.
    parse_with_violations("top1: &n1\n  &k1 k: v\n", &[]);
    parse_with_violations(
        "top2: &n2\n  &v2 val2\n",
        &[("Multiple anchors on the same node", "1:7")],
    );
}

/// An alias with properties of its own is invalid YAML, but the parser
/// accepts it, with the alias where a scalar key would be, and leaves the
/// error to the validator. `a: &a 1\n&b *a : v\n` used to fail to parse.
#[test]
fn annotated_alias_key_parses_like_a_scalar_key() {
    let tree = |yaml: &str| {
        let file = YamlFile::from_str(yaml).unwrap();
        assert_eq!(file.to_string(), yaml);
        assert_file_cst_ok(&file);
        debug::tree_to_string(file.syntax())
    };
    for key in ["&b", "!t", "&b !t", "!t &b"] {
        for (alias_form, scalar_form) in [
            ("a: &a 1\nKEY *a : v\n", "a: &a 1\nKEY xx : v\n"),
            ("KEY *a : v\n", "KEY xx : v\n"),
            ("k:\n  KEY *a : v\n", "k:\n  KEY xx : v\n"),
            ("- KEY *a : v\n", "- KEY xx : v\n"),
            ("a: &a 1\nb: {KEY *a : v}\n", "a: &a 1\nb: {KEY xx : v}\n"),
            ("a: &a 1\nb: [KEY *a : v]\n", "a: &a 1\nb: [KEY xx : v]\n"),
        ] {
            let alias_form = alias_form.replace("KEY", key);
            let scalar_form = scalar_form.replace("KEY", key);
            assert_eq!(
                tree(&alias_form)
                    .replace("ALIAS", "SCALAR")
                    .replace("REFERENCE: \"*a\"", "STRING: \"xx\""),
                tree(&scalar_form),
                "{alias_form:?}"
            );
        }
    }
}

#[test]
fn anchor_with_different_value_types() {
    let yaml = r#"string_anchor: &str_val "hello"
int_anchor: &int_val 42
bool_anchor: &bool_val true
null_anchor: &null_val null
str_ref: *str_val
int_ref: *int_val
bool_ref: *bool_val
null_ref: *null_val"#;
    assert_eq!(
        render_documents(yaml),
        [
            "map{string_anchor: hello, int_anchor: 42, bool_anchor: true, null_anchor: ~, \
          str_ref: *str_val, int_ref: *int_val, bool_ref: *bool_val, null_ref: *null_val}"
        ]
    );
    let file = YamlFile::from_str(yaml).unwrap();
    let mapping = file.document().unwrap().as_mapping().unwrap();
    assert_eq!(mapping.get("int_anchor").unwrap().to_i64(), Some(42));
    assert_eq!(mapping.get("bool_anchor").unwrap().to_bool(), Some(true));
}

fn doc(text: &str) -> Document {
    Document::from_str(text).expect("parse")
}

/// `get_resolved(key)` on `yaml` as `(key, value)` pairs, with each
/// value rendered through `Display`.
fn resolved_pairs(yaml: &str, key: &str) -> Vec<(String, String)> {
    let resolved = doc(yaml).get_resolved(key).unwrap().expect("key resolves");
    let mapping = resolved.as_mapping().expect("value is a mapping");
    mapping
        .iter()
        .map(|(k, v)| (k.clone(), v.to_string()))
        .collect()
}

fn pairs(expected: &[(&str, &str)]) -> Vec<(String, String)> {
    expected
        .iter()
        .map(|&(k, v)| (k.to_string(), v.to_string()))
        .collect()
}

#[test]
fn get_resolved_merge_sequence_earlier_wins() {
    let yaml = "a: &a {k: from_a}\nb: &b {k: from_b, j: 2}\nm:\n  <<: [*a, *b]\n";
    assert_eq!(
        resolved_pairs(yaml, "m"),
        pairs(&[("j", "2"), ("k", "from_a")])
    );
}

fn merged_pairs(yaml: &str, key: &str) -> Vec<(String, String)> {
    let d = doc(yaml);
    let reg = d.build_anchor_registry();
    let m = d.as_mapping().unwrap().get_mapping(key).unwrap();
    let merged = m.merged(&reg).unwrap();
    merged
        .iter()
        .map(|(k, v)| (k.to_string(), v.to_string()))
        .collect()
}

#[test]
fn get_resolved_merge_shadows_by_key_value() {
    // 16 and 0x10 are the same int, ~ and null the same null.
    let yaml = "a: &a {0x10: merged, ~: merged}\nm: {<<: *a, 16: direct, null: direct}\n";
    assert_eq!(
        resolved_pairs(yaml, "m"),
        pairs(&[("16", "direct"), ("null", "direct")])
    );
    assert_eq!(
        merged_pairs(yaml, "m"),
        pairs(&[("16", "direct"), ("null", "direct")])
    );
}

#[test]
fn nested_merge_keys_are_followed() {
    let yaml = "b: &b {x: 1, y: 0}\nmid: &m\n  <<: *b\n  y: 2\ntop:\n  <<: *m\n";
    assert_eq!(
        resolved_pairs(yaml, "top"),
        pairs(&[("x", "1"), ("y", "2")])
    );
    assert_eq!(merged_pairs(yaml, "top"), pairs(&[("y", "2"), ("x", "1")]));
    let d = doc(yaml);
    assert_eq!(d.try_get_path("top.x").unwrap().to_string(), "1");
    assert_eq!(d.try_get_path("top.y").unwrap().to_string(), "2");
}

/// `<<` is the plain spelling of the `tag:yaml.org,2002:merge` key, so an
/// explicitly tagged `<<` merges too; a quoted `'<<'` stays a string.
#[test]
fn tagged_merge_key_is_followed() {
    for tag in ["!!merge", "!<tag:yaml.org,2002:merge>"] {
        let yaml = format!("x: &x {{p: 1}}\ny:\n  {tag} <<: *x\n  q: 2\n");
        assert_eq!(
            resolved_pairs(&yaml, "y"),
            pairs(&[("p", "1"), ("q", "2")]),
            "{yaml:?}"
        );
        assert_eq!(
            merged_pairs(&yaml, "y"),
            pairs(&[("q", "2"), ("p", "1")]),
            "{yaml:?}"
        );
    }
    let yaml = "x: &x {p: 1}\ny:\n  '<<': *x\n";
    assert_eq!(merged_pairs(yaml, "y"), pairs(&[("'<<'", "{p: 1}")]));
}

#[test]
fn self_referencing_merge_terminates() {
    let yaml = "a: &a\n  k: 1\n  <<: *a\n";
    assert_eq!(resolved_pairs(yaml, "a"), pairs(&[("k", "1")]));
    assert_eq!(merged_pairs(yaml, "a"), pairs(&[("k", "1")]));
}

#[test]
fn inline_mapping_merge_value() {
    for (yaml, expected) in [
        ("m:\n  <<: {z: 1}\n  y: 2\n", vec![("y", "2"), ("z", "1")]),
        (
            "a: &a {w: 0}\nm:\n  <<: [{z: 1}, *a]\n  y: 2\n",
            vec![("w", "0"), ("y", "2"), ("z", "1")],
        ),
    ] {
        assert_eq!(resolved_pairs(yaml, "m"), pairs(&expected), "{yaml:?}");
        assert_eq!(doc(yaml).try_get_path("m.z").unwrap().to_string(), "1");
    }
    assert_eq!(
        merged_pairs("m:\n  <<: {z: 1}\n  y: 2\n", "m"),
        pairs(&[("y", "2"), ("z", "1")])
    );
}

#[test]
fn get_resolved_follows_aliases() {
    for (yaml, key, expected) in [
        ("a: &x 1\nb: *x\n", "b", "b: 1\n"),
        ("x: &x 1\nm: {a: *x, c: 2}\n", "m", "m: {a: 1, c: 2}\n"),
        ("x: &x 1\ns: [*x, 2]\n", "s", "s: [1, 2]\n"),
        ("x: &x {k: 1}\nm:\n  a: [*x]\n", "m", "m:\n  a: [{k: 1}]\n"),
        (
            "b: &b {k: 1}\nx: &x {<<: *b}\nm: {a: *x}\n",
            "m",
            "m: {a: {k: 1}}\n",
        ),
    ] {
        assert_eq!(
            doc(yaml).get_resolved(key).unwrap(),
            doc(expected).get_resolved(key).unwrap(),
            "{yaml:?}"
        );
    }
}

#[test]
fn get_resolved_keeps_complex_keys() {
    assert_eq!(
        resolved_pairs("m:\n  ? [p, q]\n  : complex\n  j: 1\n", "m"),
        pairs(&[("[p, q]", "complex"), ("j", "1")])
    );
}

#[test]
fn get_resolved_key_string_is_canonical() {
    // Equal keys have the same string form, however they are written.
    for (yaml, expected) in [
        ("m:\n  ? [a,b]\n  : 1\n", "[a, b]"),
        ("m:\n  ? - a\n    - b\n  : 1\n", "[a, b]"),
        ("m: {[a, b]: 1}\n", "[a, b]"),
        ("m:\n  ? - a\n    # c\n  : 1\n", "[a]"),
        ("m:\n  ? {y: 2, x: 1}\n  : 1\n", "{x: 1, y: 2}"),
        ("m:\n  ? y: 2\n    x: 1\n  : 1\n", "{x: 1, y: 2}"),
        ("m: {['a', \"b\"]: 1}\n", "[a, b]"),
        (
            "m: {[0x1, !!str a, ~, True, 1.50]: 1}\n",
            "[1, a, null, true, 1.5]",
        ),
        ("m: {['1', 'a,b', '']: 1}\n", "['1', \"a,b\", '']"),
        ("m: {[!!int '1', !t a, !!int x]: 1}\n", "[1, !t a, !!int x]"),
        ("m: {!t [a]: 1}\n", "!t [a]"),
        ("m: {!!seq [a]: 1}\n", "[a]"),
        ("m: {[{b: 1, a: 2}, []]: 1}\n", "[{a: 2, b: 1}, []]"),
        ("x: &x a\nm: {[*x]: 1}\n", "[a]"),
        ("m: {'a': 1}\n", "a"),
        ("m: {\"a\": 1}\n", "a"),
        ("m: {0x1: 1}\n", "1"),
        ("m: {!!int 1: 1}\n", "1"),
        ("m: {!!str 1: 1}\n", "1"),
        ("m: {~: 1}\n", "null"),
        ("m: {True: 1}\n", "true"),
        ("m: {.INF: 1}\n", ".inf"),
        ("m: {!t a: 1}\n", "a"),
    ] {
        assert_eq!(
            resolved_pairs(yaml, "m"),
            pairs(&[(expected, "1")]),
            "{yaml:?}"
        );
    }
}

#[test]
fn get_resolved_equal_collection_keys() {
    for (yaml, first, second) in [
        (
            "m:\n  ? [a, b]\n  : 1\n  ? [a,b]\n  : 2\n",
            "[a, b]",
            "[a,b]",
        ),
        ("m: {[0x1]: 1, [1]: 2}\n", "[0x1]", "[1]"),
        ("x: &x a\nm: {[*x]: 1, [a]: 2}\n", "[*x]", "[a]"),
        ("x: &x a\nm: {[*x]: 1, [*x]: 2}\n", "[*x]", "[*x]"),
    ] {
        assert_eq!(
            doc(yaml).get_resolved("m"),
            Err(ResolveError::DuplicateKey {
                first: first.to_string(),
                second: second.to_string(),
            }),
            "{yaml:?}"
        );
    }
    // A direct key shadows an equal merged one, however it is written.
    for yaml in [
        "s: &s {[a, b]: 1}\nm: {<<: *s, [a,b]: 2}\n",
        "s: &s {[a, b]: 1}\nm:\n  <<: *s\n  ? - a\n    - b\n  : 2\n",
    ] {
        assert_eq!(
            resolved_pairs(yaml, "m"),
            pairs(&[("[a, b]", "2")]),
            "{yaml:?}"
        );
    }
    // A string spelled like a collection is a different key.
    assert_eq!(
        doc("m: {[a, b]: 1, '[a, b]': 2}\n").get_resolved("m"),
        Err(ResolveError::KeyCollision {
            first: "[a, b]".to_string(),
            second: "'[a, b]'".to_string(),
        })
    );
    assert_eq!(
        doc("m: &a {[*a]: 1}\n").get_resolved("m"),
        Err(ResolveError::RecursiveAlias {
            alias: "a".to_string()
        })
    );
}

#[test]
fn merged_alias_key_is_the_node_it_refers_to() {
    let d = doc("x: &a k\ns: &s {k: 1, j: 3}\nm: {<<: *s, *a : 2}\n");
    let reg = d.build_anchor_registry();
    let m = d.as_mapping().unwrap().get_mapping("m").unwrap();
    let merged = m.merged(&reg).unwrap();
    assert_eq!(merged.len(), 2);
    assert_eq!(merged.get("k").unwrap().to_i64(), Some(2));
    assert_eq!(merged.get("j").unwrap().to_i64(), Some(3));
    let keys: Vec<_> = merged.keys().map(|k| k.to_string()).collect();
    assert_eq!(keys, vec!["*a", "j"]);
}

#[test]
fn get_resolved_missing_key() {
    assert_eq!(doc("a: 1\n").get_resolved("b"), Ok(None));
}

#[test]
fn get_resolved_undefined_alias() {
    let undefined = Err(ResolveError::UndefinedAlias {
        alias: "nope".to_string(),
    });
    assert_eq!(doc("b: *nope\n").get_resolved("b"), undefined);
    assert_eq!(doc("m: {a: *nope}\n").get_resolved("m"), undefined);
    assert_eq!(doc("m: [1, *nope]\n").get_resolved("m"), undefined);
}

#[test]
fn get_resolved_recursive_alias() {
    assert_eq!(
        doc("a: &a [*a]\n").get_resolved("a"),
        Err(ResolveError::RecursiveAlias {
            alias: "a".to_string()
        })
    );
}

#[test]
fn get_resolved_redefined_anchor_is_not_recursive() {
    assert_eq!(
        doc("a: &x [&x 1, *x]\nb: *x\n").get_resolved("a"),
        Ok(Some(YamlValue::Sequence(vec![
            YamlValue::from(1),
            YamlValue::from(1)
        ])))
    );
}

#[test]
fn get_resolved_invalid_merge() {
    assert_eq!(
        doc("m: {<<: *nope}\n").get_resolved("m"),
        Err(ResolveError::UndefinedAlias {
            alias: "nope".to_string()
        })
    );
    assert_eq!(
        doc("a: &a 5\nm: {<<: *a}\n").get_resolved("m"),
        Err(ResolveError::InvalidMergeOperand {
            operand: "*a".to_string()
        })
    );
    assert_eq!(
        doc("m: {<<: [3]}\n").get_resolved("m"),
        Err(ResolveError::InvalidMergeOperand {
            operand: "3".to_string()
        })
    );
}

#[test]
fn quoted_merge_key_is_an_ordinary_key() {
    for yaml in [
        "a: &a {k: 1}\nm:\n  '<<': *a\n  j: 2\n",
        "a: &a {k: 1}\nm:\n  \"<<\": *a\n  j: 2\n",
    ] {
        let d = doc(yaml);
        let resolved = d.get_resolved("m").unwrap().unwrap();
        let keys: Vec<&str> = resolved
            .as_mapping()
            .unwrap()
            .keys()
            .map(String::as_str)
            .collect();
        assert_eq!(keys, vec!["<<", "j"], "{yaml:?}");

        let reg = d.build_anchor_registry();
        let m = d.as_mapping().unwrap().get_mapping("m").unwrap();
        let merged = m.merged(&reg).unwrap();
        let keys: Vec<String> = merged
            .keys()
            .map(|k| k.as_scalar().unwrap().as_string())
            .collect();
        assert_eq!(keys, vec!["<<", "j"], "{yaml:?}");
        assert_eq!(merged.get("k"), None, "{yaml:?}");
        assert_eq!(
            merged.get("<<").map(|v| v.to_string()).as_deref(),
            Some("{k: 1}"),
            "{yaml:?}"
        );
        assert_eq!(
            d.try_get_path("m.k"),
            Err(yaml_edit::path::PathError::NotFound { at: "k".into() }),
            "{yaml:?}"
        );
    }
}

#[test]
fn merged_iter_keeps_non_string_keys() {
    assert_eq!(
        merged_pairs("a: &a {x: 1}\nm:\n  <<: *a\n  ? [p, q]\n  : complex\n", "m"),
        pairs(&[("[p, q]", "complex"), ("x", "1")])
    );
    // The int 1 and the string '1' are different keys.
    let yaml = "s: &s {1: from_src, x: 1}\nm:\n  <<: *s\n  '1': base\n  x: 2\n";
    assert_eq!(
        merged_pairs(yaml, "m"),
        pairs(&[("'1'", "base"), ("x", "2"), ("1", "from_src")])
    );
    let d = doc(yaml);
    let reg = d.build_anchor_registry();
    let m = d.as_mapping().unwrap().get_mapping("m").unwrap();
    assert_eq!(m.merged(&reg).unwrap().len(), 3);
    assert_eq!(
        m.merged(&reg).unwrap().get(1).unwrap().to_string(),
        "from_src"
    );
    assert_eq!(
        m.merged(&reg).unwrap().get("1").unwrap().to_string(),
        "base"
    );
}

#[test]
fn get_resolved_keys_are_decoded() {
    // Mapping::pairs() yields the KEY wrapper, so casting it straight to
    // Scalar failed and the key fell back to raw text. A quoted key kept
    // its quotes and became unreachable by its own name -- but only
    // without a merge key, since apply_merge_keys decodes separately.
    for (yaml, expected) in [
        ("a:\n  '0': z\n  x: 1\n", vec!["0", "x"]),
        ("a:\n  \"k\": z\n", vec!["k"]),
        ("a:\n  plain: z\n", vec!["plain"]),
        // With a merge key, which takes the other code path.
        ("s: &s\n  q: r\na:\n  <<: *s\n  '0': z\n", vec!["0", "q"]),
    ] {
        let doc = Document::from_str(yaml).unwrap();
        let resolved = doc.get_resolved("a").unwrap().expect("a resolves");
        let mapping = resolved.as_mapping().expect("a is a mapping");
        let keys: Vec<&str> = mapping.keys().map(String::as_str).collect();
        assert_eq!(keys, expected, "{yaml:?}");
        for key in expected {
            assert!(
                mapping.contains_key(key),
                "{yaml:?}: {key:?} not reachable by name"
            );
        }
    }
}

#[test]
fn get_resolved_expands_merge_keys() {
    // `*a` reaches apply_merge_keys as an ALIAS node, not a scalar, so
    // the merge used to fall through and drop every inherited key. Check
    // get_resolved against the MergedMapping view, which is the same
    // question asked of the newer code path.
    for (yaml, resolved, merged) in [
        (
            "a: &a\n  x: 1\nb: &b\n  y: 2\nm:\n  <<: [*a, *b]\n  z: 3\n",
            vec![("x", "1"), ("y", "2"), ("z", "3")],
            vec![("z", "3"), ("x", "1"), ("y", "2")],
        ),
        (
            "a: &a\n  x: 1\nm:\n  <<: *a\n  z: 3\n",
            vec![("x", "1"), ("z", "3")],
            vec![("z", "3"), ("x", "1")],
        ),
    ] {
        assert_eq!(resolved_pairs(yaml, "m"), pairs(&resolved), "{yaml:?}");
        assert_eq!(merged_pairs(yaml, "m"), pairs(&merged), "{yaml:?}");
    }
}

#[test]
fn merged_basic_merge_key() {
    let yaml = "\
defaults: &d
  timeout: 30
  retries: 3

prod:
  <<: *d
  host: prod.example.com
";
    let d = doc(yaml);
    let root = d.as_mapping().unwrap();
    let reg = d.build_anchor_registry();
    let prod = root.get_mapping("prod").unwrap();
    let m = prod.merged(&reg).unwrap();

    assert_eq!(m.get("timeout").unwrap().to_i64(), Some(30));
    assert_eq!(m.get("retries").unwrap().to_i64(), Some(3));
    assert_eq!(
        m.get("host").unwrap().as_scalar().unwrap().as_string(),
        "prod.example.com"
    );
}

#[test]
fn merged_direct_key_wins() {
    let yaml = "\
defaults: &d
  timeout: 30

prod:
  <<: *d
  timeout: 60
";
    let d = doc(yaml);
    let reg = d.build_anchor_registry();
    let prod = d.as_mapping().unwrap().get_mapping("prod").unwrap();
    let m = prod.merged(&reg).unwrap();

    assert_eq!(m.get("timeout").unwrap().to_i64(), Some(60));
}

#[test]
fn merged_merge_key_itself_is_hidden() {
    let yaml = "\
d: &d
  a: 1
m:
  <<: *d
";
    let d = doc(yaml);
    let reg = d.build_anchor_registry();
    let inner = d.as_mapping().unwrap().get_mapping("m").unwrap();
    let m = inner.merged(&reg).unwrap();

    assert!(m.get("<<").is_none());
    let keys: Vec<String> = m
        .keys()
        .map(|k| k.as_scalar().unwrap().as_string())
        .collect();
    assert_eq!(keys, vec!["a"]);
}

#[test]
fn merged_sequence_of_aliases() {
    // Per YAML 1.1: earlier aliases take precedence over later ones.
    let yaml = "\
a: &a
  x: 1
  y: 1
b: &b
  y: 2
  z: 2
m:
  <<: [*a, *b]
";
    let d = doc(yaml);
    let reg = d.build_anchor_registry();
    let inner = d.as_mapping().unwrap().get_mapping("m").unwrap();
    let m = inner.merged(&reg).unwrap();

    // x only in a
    assert_eq!(m.get("x").unwrap().to_i64(), Some(1));
    // y in both - a wins (first listed)
    assert_eq!(m.get("y").unwrap().to_i64(), Some(1));
    // z only in b
    assert_eq!(m.get("z").unwrap().to_i64(), Some(2));
}

#[test]
fn tagged_merge_operands_are_mappings() {
    for yaml in [
        "a: &a !!map {k: 1}\nm: {<<: *a}\n",
        "m: {<<: !!map {k: 1}}\n",
        "a: &a !!map {k: 1}\nm: {<<: [*a]}\n",
        "m: {<<: [!!map {k: 1}]}\n",
    ] {
        assert_eq!(resolved_pairs(yaml, "m"), pairs(&[("k", "1")]), "{yaml:?}");
        assert_eq!(merged_pairs(yaml, "m"), pairs(&[("k", "1")]), "{yaml:?}");
        assert_eq!(
            doc(yaml).try_get_path("m.k").unwrap().to_string(),
            "1",
            "{yaml:?}"
        );
    }
}

#[test]
fn merged_undefined_alias_value_is_error() {
    let undefined = ResolveError::UndefinedAlias {
        alias: "nope".to_string(),
    };
    for yaml in ["m: {a: *nope}\n", "s: &s {a: *nope}\nm: {<<: *s}\n"] {
        let d = doc(yaml);
        let reg = d.build_anchor_registry();
        let m = d.as_mapping().unwrap().get_mapping("m").unwrap();
        assert_eq!(m.merged(&reg).unwrap_err(), undefined, "{yaml:?}");
    }
    // A shadowed alias is never looked at.
    let d = doc("s: &s {a: *nope}\nm: {<<: *s, a: 1}\n");
    let reg = d.build_anchor_registry();
    let m = d.as_mapping().unwrap().get_mapping("m").unwrap();
    assert_eq!(m.merged(&reg).unwrap().get("a").unwrap().to_i64(), Some(1));
}

#[test]
fn merged_invalid_merge_operand_is_error() {
    let d = doc("a: &a 5\nm:\n  <<: *nope\n  a: 1\nn:\n  <<: [*a]\n");
    let reg = d.build_anchor_registry();
    let root = d.as_mapping().unwrap();
    assert_eq!(
        root.get_mapping("m").unwrap().merged(&reg).unwrap_err(),
        ResolveError::UndefinedAlias {
            alias: "nope".to_string()
        }
    );
    assert_eq!(
        root.get_mapping("n").unwrap().merged(&reg).unwrap_err(),
        ResolveError::InvalidMergeOperand {
            operand: "*a".to_string()
        }
    );
    assert_eq!(
        root.merged(&reg).unwrap().get_merged("m").unwrap_err(),
        ResolveError::UndefinedAlias {
            alias: "nope".to_string()
        }
    );
    assert_eq!(
        doc("<<: *nope\n").merged().unwrap_err(),
        ResolveError::UndefinedAlias {
            alias: "nope".to_string()
        }
    );
}

#[test]
fn merged_iter_order_direct_then_merged() {
    let yaml = "\
d: &d
  shared: from_d
  only_in_d: 1
m:
  direct: hi
  <<: *d
";
    let d = doc(yaml);
    let reg = d.build_anchor_registry();
    let inner = d.as_mapping().unwrap().get_mapping("m").unwrap();
    let m = inner.merged(&reg).unwrap();

    let keys: Vec<String> = m
        .iter()
        .map(|(k, _)| k.as_scalar().unwrap().as_string())
        .collect();
    assert_eq!(keys, vec!["direct", "shared", "only_in_d"]);
}

#[test]
fn merged_len_and_empty() {
    let yaml = "\
d: &d
  a: 1
  b: 2
m:
  <<: *d
  c: 3
";
    let d = doc(yaml);
    let reg = d.build_anchor_registry();
    let inner = d.as_mapping().unwrap().get_mapping("m").unwrap();
    let m = inner.merged(&reg).unwrap();

    assert_eq!(m.len(), 3);
    assert!(!m.is_empty());
}

#[test]
fn merged_alias_value_is_resolved() {
    let yaml = "\
target: &t
  k: 42
m:
  ref: *t
";
    let d = doc(yaml);
    let reg = d.build_anchor_registry();
    let inner = d.as_mapping().unwrap().get_mapping("m").unwrap();
    let m = inner.merged(&reg).unwrap();

    let r = m.get("ref").unwrap();
    let nested = r.as_mapping().unwrap();
    assert_eq!(nested.get("k").unwrap().to_i64(), Some(42));
}

#[test]
fn merged_get_merged_nested() {
    let yaml = "\
defaults: &d
  port: 80
prod:
  <<: *d
  inner:
    a: 1
";
    let d = doc(yaml);
    let reg = d.build_anchor_registry();
    let prod = d.as_mapping().unwrap().get_mapping("prod").unwrap();
    let m = prod.merged(&reg).unwrap();

    let inner = m.get_merged("inner").unwrap().unwrap();
    assert_eq!(inner.get("a").unwrap().to_i64(), Some(1));
}

#[test]
fn document_merged_view() {
    let yaml = "\
defaults: &d
  timeout: 30
shared:
  <<: *d
  retries: 5
";
    let d = doc(yaml);
    let view = d.merged().unwrap().unwrap();

    // Top-level keys visible
    assert!(view.contains_key("defaults"));
    assert!(view.contains_key("shared"));

    // Drill into merged child via as_mapping()
    let m = view.as_mapping();
    let shared = m.get_merged("shared").unwrap().unwrap();
    assert_eq!(shared.get("timeout").unwrap().to_i64(), Some(30));
    assert_eq!(shared.get("retries").unwrap().to_i64(), Some(5));
}

#[test]
fn merged_does_not_mutate_underlying_cst() {
    let yaml = "\
d: &d
  a: 1
m:
  <<: *d
  b: 2
";
    let d = doc(yaml);
    let reg = d.build_anchor_registry();
    let inner = d.as_mapping().unwrap().get_mapping("m").unwrap();
    let _ = inner.merged(&reg).unwrap().get("a");
    // Original document text should round-trip unchanged.
    assert_eq!(d.to_string(), yaml);
}

#[test]
fn merged_alias_uses_preceding_anchor_definition() {
    let d = doc("a: &x 1\nb: *x\nc: &x 2\nd: *x\n");
    let view = d.merged().unwrap().unwrap();
    assert_eq!(view.get("b").unwrap().to_i64(), Some(1));
    assert_eq!(view.get("d").unwrap().to_i64(), Some(2));
}

#[test]
fn merged_merge_key_uses_preceding_anchor_definition() {
    let d = doc("a: &x {k: 1}\nm:\n  <<: *x\nc: &x {k: 2}\n");
    let view = d.merged().unwrap().unwrap();
    let m = view.as_mapping().get_merged("m").unwrap().unwrap();
    assert_eq!(m.get("k").unwrap().to_i64(), Some(1));
}

#[test]
fn merged_alias_ignores_later_anchor() {
    assert_eq!(
        doc("b: *x\na: &x 1\n").merged().unwrap_err(),
        ResolveError::UndefinedAlias {
            alias: "x".to_string()
        }
    );
}

#[test]
fn registry_resolve_returns_last_definition() {
    let d = doc("a: &x 1\nc: &x 2\n");
    let reg = d.build_anchor_registry();
    assert_eq!(reg.resolve("x").unwrap().text().to_string(), "2");
}

#[test]
fn anchor_after_tag_covers_the_tag() {
    // `!!str &x` and `&x !!str` are the same node properties.
    for src in ["a: !!str &x 1\nb: *x\n", "a: &x !!str 1\nb: *x\n"] {
        let d = doc(src);
        assert_eq!(
            d.get_resolved("b"),
            doc("b: !!str 1\n").get_resolved("b"),
            "{src:?}"
        );
    }
}

#[test]
fn merge_key_alias_to_sequence() {
    for src in [
        "a: &a {x: 1}\nb: &b {x: 2, y: 2}\nl: &l [*a, *b]\nm:\n  <<: *l\n",
        "a: &a {x: 1}\nb: &b {x: 2, y: 2}\nm:\n  <<: !!seq [*a, *b]\n",
    ] {
        let d = doc(src);
        let view = d.merged().unwrap().unwrap();
        let m = view.as_mapping().get_merged("m").unwrap().unwrap();
        assert_eq!(m.get("x").unwrap().to_i64(), Some(1), "{src:?}");
        assert_eq!(m.get("y").unwrap().to_i64(), Some(2), "{src:?}");
        let value = yaml_edit::YamlValue::from_document(&d).unwrap();
        let yaml_edit::YamlValue::Mapping(map) = value else {
            panic!("expected a mapping");
        };
        assert_eq!(
            map["m"],
            yaml_edit::YamlValue::from_mapping(std::collections::BTreeMap::from([
                ("x".to_string(), yaml_edit::YamlValue::from(1i64)),
                ("y".to_string(), yaml_edit::YamlValue::from(2i64)),
            ])),
            "{src:?}"
        );
    }
}

#[test]
fn merge_key_from_tagged_mapping() {
    for src in [
        "a: !!map &x\n  k: 1\nb:\n  <<: *x\n",
        "a: &x !!map\n  k: 1\nb:\n  <<: *x\n",
    ] {
        let d = doc(src);
        let view = d.merged().unwrap().unwrap();
        let b = view.as_mapping().get_merged("b").unwrap().unwrap();
        assert_eq!(b.get("k").unwrap().to_i64(), Some(1), "{src:?}");
    }
}

#[test]
fn get_resolved_key_collision_is_error() {
    let cases = [
        ("m: {1: int, '1': str}\n", "1", "'1'"),
        ("m: {a: {'true': str, true: bool}}\n", "'true'", "true"),
        (
            "s: &s {1: from_src}\nm:\n  <<: *s\n  '1': base\n",
            "'1'",
            "1",
        ),
        (
            "s: &s {1: a}\nt: &t {'1': b}\nm: {<<: [*s, *t]}\n",
            "1",
            "'1'",
        ),
        ("s: &s {1: a}\nm: [{<<: *s, '1': b}]\n", "'1'", "1"),
    ];
    for (yaml, first, second) in cases {
        assert_eq!(
            doc(yaml).get_resolved("m"),
            Err(ResolveError::KeyCollision {
                first: first.to_string(),
                second: second.to_string(),
            }),
            "{yaml:?}"
        );
    }
}

#[test]
fn get_resolved_duplicate_key_is_error() {
    for (yaml, first, second) in [
        ("m: {a: 1, a: 2}\n", "a", "a"),
        ("m:\n  a: 1\n  b: 2\n  'a': 3\n", "a", "'a'"),
        ("m: {16: a, 0x10: b}\n", "16", "0x10"),
        ("m: {a: {~: 1, null: 2}}\n", "~", "null"),
        ("x: &k a\nm: {a: 1, *k : 2}\n", "a", "*k"),
        ("x: &k a\nm: {*k : 1, *k : 2}\n", "*k", "*k"),
        // Within a merged mapping, too.
        ("s: &s {a: 1, a: 2}\nm: {<<: *s, b: 3}\n", "a", "a"),
        ("s: &s {a: 1, a: 2}\nm: {<<: *s, a: 3}\n", "a", "a"),
        ("m: [{a: 1, a: 2}]\n", "a", "a"),
    ] {
        assert_eq!(
            doc(yaml).get_resolved("m"),
            Err(ResolveError::DuplicateKey {
                first: first.to_string(),
                second: second.to_string(),
            }),
            "{yaml:?}"
        );
    }
    // A direct key shadowing a merged one is not a duplicate, nor are
    // keys shared between merged mappings.
    assert_eq!(
        resolved_pairs(
            "s: &s {a: 1, b: 1}\nt: &t {a: 2, c: 2}\nm: {b: 3, <<: [*s, *t], c: 3}\n",
            "m"
        ),
        pairs(&[("a", "1"), ("b", "3"), ("c", "3")])
    );
    assert_eq!(
        ResolveError::DuplicateKey {
            first: "a".to_string(),
            second: "'a'".to_string()
        }
        .to_string(),
        "duplicate mapping key: \"'a'\" equals \"a\""
    );
}

#[test]
fn get_resolved_equal_keys_are_not_a_collision() {
    // A key overriding an equal merged key, however it is spelled, is
    // the normal merge semantics.
    let yaml = "s: &s {1: a, 0x10: b, x: c}\nm: {<<: *s, 1: d, 16: e, 'x': f}\n";
    assert_eq!(
        resolved_pairs(yaml, "m"),
        pairs(&[("1", "d"), ("16", "e"), ("x", "f")])
    );
}

#[test]
fn key_collision_display() {
    let e = ResolveError::KeyCollision {
        first: "1".to_string(),
        second: "'1'".to_string(),
    };
    assert_eq!(
        e.to_string(),
        "distinct keys \"1\" and \"'1'\" have the same string form"
    );
}

fn mapping(entries: Vec<(&str, YamlValue)>) -> YamlValue {
    YamlValue::Mapping(
        entries
            .into_iter()
            .map(|(k, v)| (k.to_string(), v))
            .collect(),
    )
}

fn pair_list(entries: Vec<(&str, YamlValue)>) -> Vec<(String, YamlValue)> {
    entries
        .into_iter()
        .map(|(k, v)| (k.to_string(), v))
        .collect()
}

#[test]
fn get_resolved_tagged_collections() {
    let k1 = || mapping(vec![("k", YamlValue::from(1))]);
    for (yaml, expected) in [
        (
            "s: &s {k: 1}\nm: !!map {<<: *s, j: *s}\n",
            YamlValue::tagged(
                "!!map",
                mapping(vec![("j", k1()), ("k", YamlValue::from(1))]),
            ),
        ),
        (
            "s: &s {k: 1}\nm: !!seq [*s, 2]\n",
            YamlValue::tagged("!!seq", YamlValue::Sequence(vec![k1(), YamlValue::from(2)])),
        ),
        (
            "s: &s {k: 1}\nm: {a: !local [*s]}\n",
            mapping(vec![(
                "a",
                YamlValue::tagged("!local", YamlValue::Sequence(vec![k1()])),
            )]),
        ),
        (
            "s: &s {k: 1}\nt: &t !!map {a: *s}\nm: [*t]\n",
            YamlValue::Sequence(vec![YamlValue::tagged("!!map", mapping(vec![("a", k1())]))]),
        ),
        (
            "x: &x a\nm: !!set {? *x, ? b}\n",
            YamlValue::Set(["a", "b"].map(String::from).into()),
        ),
        (
            "s: &s {k: 1}\nm: !!omap [z: *s, a: 2]\n",
            YamlValue::OrderedMapping(pair_list(vec![("z", k1()), ("a", YamlValue::from(2))])),
        ),
        (
            "s: &s {k: 1}\nm: !!pairs [z: *s, z: 2]\n",
            YamlValue::Pairs(pair_list(vec![("z", k1()), ("z", YamlValue::from(2))])),
        ),
        (
            "e: &e {z: 1}\nm: !!omap [*e, a: 2]\n",
            YamlValue::OrderedMapping(pair_list(vec![
                ("z", YamlValue::from(1)),
                ("a", YamlValue::from(2)),
            ])),
        ),
    ] {
        assert_eq!(doc(yaml).get_resolved("m"), Ok(Some(expected)), "{yaml:?}");
    }
}

#[test]
fn get_resolved_tagged_collection_errors() {
    let undefined = Err(ResolveError::UndefinedAlias {
        alias: "nope".to_string(),
    });
    for yaml in [
        "m: !!map {a: *nope}\n",
        "m: !!seq [*nope]\n",
        "m: !!set {? *nope}\n",
        "m: !!omap [a: *nope]\n",
        "m: !!pairs [a: *nope]\n",
        "m: !!map {<<: *nope}\n",
    ] {
        assert_eq!(doc(yaml).get_resolved("m"), undefined, "{yaml:?}");
    }
    assert_eq!(
        doc("m: &a !!seq [*a]\n").get_resolved("m"),
        Err(ResolveError::RecursiveAlias {
            alias: "a".to_string()
        })
    );
    assert_eq!(
        doc("m: !!map {<<: 3}\n").get_resolved("m"),
        Err(ResolveError::InvalidMergeOperand {
            operand: "3".to_string()
        })
    );
}

#[test]
fn get_resolved_invalid_set_member() {
    for (yaml, member) in [
        ("m: !!set {? [a], ? b}\n", "[a]"),
        ("m: !!set {? {a: 1}}\n", "{a: 1}"),
        ("x: &x [a]\nm: !!set {? *x}\n", "*x"),
    ] {
        assert_eq!(
            doc(yaml).get_resolved("m"),
            Err(ResolveError::InvalidSetMember {
                member: member.to_string()
            }),
            "{yaml:?}"
        );
    }
}

#[test]
fn get_resolved_set_content_must_be_mapping() {
    for (yaml, content) in [
        ("m: !!set [a, b]\n", "[a, b]"),
        ("m: !!set &s [a]\n", "[a]"),
        ("m: !!set\n- a\n", "- a\n"),
        ("m: !!set\nn: 1\n", ""),
    ] {
        assert_eq!(
            doc(yaml).get_resolved("m"),
            Err(ResolveError::InvalidSetContent {
                content: content.to_string()
            }),
            "{yaml:?}"
        );
    }
    assert_eq!(
        ResolveError::InvalidSetContent {
            content: "[a]".to_string()
        }
        .to_string(),
        "!!set content \"[a]\" is not a mapping"
    );
}

#[test]
fn get_resolved_set_value_must_be_null() {
    for (yaml, member, value) in [
        ("m: !!set {a: 1}\n", "a", "1"),
        ("m: !!set\n  ? a\n  b: x\n", "b", "x"),
        ("m: !!set\n  ? b\n  ? a\n  : x\n", "a", "x"),
        ("m: !!set {a: ~, b: 'null'}\n", "b", "'null'"),
        ("m: !!set {a: [~]}\n", "a", "[~]"),
        ("x: &x 1\nm: !!set {a: *x}\n", "a", "*x"),
    ] {
        assert_eq!(
            doc(yaml).get_resolved("m"),
            Err(ResolveError::NonNullSetValue {
                member: member.to_string(),
                value: value.to_string()
            }),
            "{yaml:?}"
        );
    }
    for yaml in [
        "m: !!set {a: ~, b: null, c: , ? d, e: !!null ''}\n",
        "m: !!set\n  ? a\n  b:\n  c: ~\n  ? d\n  e: !!null ''\n",
        "n: &n ~\nm: !!set {a: *n, b: , c: *n, ? d, e: *n}\n",
    ] {
        assert_eq!(
            doc(yaml).get_resolved("m"),
            Ok(Some(YamlValue::Set(
                ["a", "b", "c", "d", "e"].map(String::from).into()
            ))),
            "{yaml:?}"
        );
    }
    assert_eq!(
        ResolveError::NonNullSetValue {
            member: "a".to_string(),
            value: "1".to_string()
        }
        .to_string(),
        "!!set member \"a\" has non-null value \"1\""
    );
}

#[test]
fn get_resolved_invalid_pairs_item() {
    for (yaml, tag, item) in [
        ("m: !!omap [a]\n", "!!omap", "a"),
        ("m: !!omap [[a]]\n", "!!omap", "[a]"),
        ("m: !!omap [{}]\n", "!!omap", "{}"),
        ("m: !!pairs [{b: 1, c: 2}]\n", "!!pairs", "{b: 1, c: 2}"),
        ("m: !!pairs [!local {a: 1}]\n", "!!pairs", "!local {a: 1}"),
        ("e: &e {a: 1, b: 2}\nm: !!omap [*e]\n", "!!omap", "*e"),
        (
            "e: &e {a: 1}\nm: !!omap [{<<: *e, b: 2}]\n",
            "!!omap",
            "{<<: *e, b: 2}",
        ),
    ] {
        assert_eq!(
            doc(yaml).get_resolved("m"),
            Err(ResolveError::InvalidPairsItem {
                tag: tag.to_string(),
                item: item.to_string()
            }),
            "{yaml:?}"
        );
    }
}

#[test]
fn get_resolved_pairs_content_must_be_sequence() {
    for (yaml, tag, content) in [
        ("m: !!omap {a: 1}\n", "!!omap", "{a: 1}"),
        ("m: !!pairs &p {a: 1}\n", "!!pairs", "{a: 1}"),
        ("m: !!omap\n  a: 1\n", "!!omap", "a: 1\n"),
        ("m: !!pairs\n  3\n", "!!pairs", "3"),
    ] {
        assert_eq!(
            doc(yaml).get_resolved("m"),
            Err(ResolveError::InvalidPairsContent {
                tag: tag.to_string(),
                content: content.to_string()
            }),
            "{yaml:?}"
        );
    }
    assert_eq!(
        ResolveError::InvalidPairsContent {
            tag: "!!omap".to_string(),
            content: "{a: 1}".to_string()
        }
        .to_string(),
        "!!omap content \"{a: 1}\" is not a sequence"
    );
}

#[test]
fn get_resolved_repeated_set_member_or_omap_key() {
    for (yaml, first, second) in [
        ("m: !!set {? a, ? a}\n", "a", "a"),
        ("m: !!set\n  ? a\n  ? b\n  ? 'a'\n", "a", "'a'"),
        ("m: !!set {? 16, ? 0x10}\n", "16", "0x10"),
        ("x: &k a\nm: !!set {? a, ? *k}\n", "a", "*k"),
        ("m: !!omap [a: 1, a: 2]\n", "a", "a"),
        ("m: !!omap\n  - a: 1\n  - b: 2\n  - 'a': 3\n", "a", "'a'"),
        ("m: !!omap [16: a, 0x10: b]\n", "16", "0x10"),
        ("x: &k a\nm: !!omap [a: 1, *k : 2]\n", "a", "*k"),
        ("e: &e {a: 1}\nm: !!omap [*e, a: 2]\n", "a", "a"),
        ("e: &e {a: 1}\nm: !!omap [{<<: *e}, a: 2]\n", "a", "a"),
    ] {
        assert_eq!(
            doc(yaml).get_resolved("m"),
            Err(ResolveError::DuplicateKey {
                first: first.to_string(),
                second: second.to_string()
            }),
            "{yaml:?}"
        );
    }
    for (yaml, first, second) in [
        ("m: !!set {? 1, ? '1'}\n", "1", "'1'"),
        ("m: !!omap [1: a, '1': b]\n", "1", "'1'"),
    ] {
        assert_eq!(
            doc(yaml).get_resolved("m"),
            Err(ResolveError::KeyCollision {
                first: first.to_string(),
                second: second.to_string()
            }),
            "{yaml:?}"
        );
    }
    // !!pairs allows a key to repeat.
    assert_eq!(
        doc("m: !!pairs [a: 1, a: 2]\n").get_resolved("m"),
        Ok(Some(YamlValue::Pairs(pair_list(vec![
            ("a", YamlValue::from(1)),
            ("a", YamlValue::from(2))
        ]))))
    );
}

#[test]
fn get_resolved_alias_key() {
    for (yaml, expected) in [
        ("x: &a k\nm: {*a : 1}\n", vec![("k", "1")]),
        ("x: &a !!int 5\nm:\n  *a : 1\n", vec![("5", "1")]),
        (
            "x: &a k\nm: {*a : 1, j: &a i, *a : 3}\n",
            vec![("i", "3"), ("j", "i"), ("k", "1")],
        ),
        (
            "x: &a k\ns: &s {*a : 1}\nm: {<<: *s, j: 2}\n",
            vec![("j", "2"), ("k", "1")],
        ),
        // An alias to a collection is a collection key.
        ("x: &a [p,q]\nm: {*a : 1}\n", vec![("[p, q]", "1")]),
    ] {
        assert_eq!(resolved_pairs(yaml, "m"), pairs(&expected), "{yaml:?}");
    }
    assert_eq!(
        doc("x: &a 1\nm: {*a : a, '1': b}\n").get_resolved("m"),
        Err(ResolveError::KeyCollision {
            first: "*a".to_string(),
            second: "'1'".to_string()
        })
    );
    assert_eq!(
        doc("m: {*nope : 1}\n").get_resolved("m"),
        Err(ResolveError::UndefinedAlias {
            alias: "nope".to_string()
        })
    );
}

#[test]
fn get_resolved_pairs_item_tagged_map() {
    assert_eq!(
        doc("m: !!omap [!!map {a: 1}]\n").get_resolved("m"),
        Ok(Some(YamlValue::OrderedMapping(pair_list(vec![(
            "a",
            YamlValue::from(1)
        )]))))
    );
}

#[test]
fn get_resolved_tagged_alias_errors() {
    for (yaml, expected) in [
        (
            "a: !t *nope\n",
            ResolveError::UndefinedAlias {
                alias: "nope".to_string(),
            },
        ),
        (
            "a: &a [!t *a]\n",
            ResolveError::RecursiveAlias {
                alias: "a".to_string(),
            },
        ),
        (
            "x: &x 1\na: !!set *x\n",
            ResolveError::InvalidSetContent {
                content: "1".to_string(),
            },
        ),
    ] {
        assert_eq!(doc(yaml).get_resolved("a"), Err(expected), "{yaml:?}");
    }
}

#[test]
fn get_resolved_anchored_alias() {
    // An anchor on an alias, which YAML does not allow, names the node
    // the alias refers to.
    for (yaml, key, expected) in [
        ("x: &x 1\nb: &b *x\nc: *b\n", "c", "c: 1\n"),
        ("x: &x 1\ns: [&b *x, *b]\n", "s", "s: [1, 1]\n"),
        ("x: &x 1\nb: &b *x\nc: !t *b\n", "c", "c: !t 1\n"),
        ("x: &x 1\nb: !t &b *x\nc: *b\n", "c", "c: !t 1\n"),
        (
            "x: &x {k: 1}\nb: &b *x\nm: {<<: *b, j: 2}\n",
            "m",
            "m: {k: 1, j: 2}\n",
        ),
    ] {
        assert_eq!(
            doc(yaml).get_resolved(key),
            doc(expected).get_resolved(key),
            "{yaml:?}"
        );
    }
    assert_eq!(
        doc("x: &x 1\nb: &b *x\nc: *b\n").get_resolved("c"),
        Ok(Some(YamlValue::from(1)))
    );
    assert_eq!(
        merged_pairs("x: &x {k: 1}\nb: &b *x\nm: {<<: *b, j: *b}\n", "m"),
        pairs(&[("j", "{k: 1}"), ("k", "1")])
    );
}

#[test]
fn get_resolved_anchored_alias_errors() {
    for (yaml, expected) in [
        (
            "c: &c *c\n",
            ResolveError::RecursiveAlias {
                alias: "c".to_string(),
            },
        ),
        (
            "b: &b *nope\nc: *b\n",
            ResolveError::UndefinedAlias {
                alias: "nope".to_string(),
            },
        ),
    ] {
        assert_eq!(doc(yaml).get_resolved("c"), Err(expected), "{yaml:?}");
    }
    let d = doc("b: &b *b\nm: {<<: *b}\n");
    let registry = d.build_anchor_registry();
    let m = d.as_mapping().unwrap().get_mapping("m").unwrap();
    assert_eq!(
        m.merged(&registry).err(),
        Some(ResolveError::RecursiveAlias {
            alias: "b".to_string()
        })
    );
}

#[test]
fn resolve_error_display() {
    assert_eq!(
        ResolveError::InvalidSetMember {
            member: "[a]".to_string()
        }
        .to_string(),
        "!!set member \"[a]\" is not a scalar"
    );
    assert_eq!(
        ResolveError::InvalidPairsItem {
            tag: "!!omap".to_string(),
            item: "a".to_string()
        }
        .to_string(),
        "!!omap item \"a\" is not a mapping with a single entry"
    );
}

#[test]
fn resolve_error_display_all_variants() {
    let s = |x: &str| x.to_string();
    for (error, text) in [
            (
                ResolveError::UndefinedAlias { alias: s("a") },
                "alias to undefined anchor \"a\"",
            ),
            (
                ResolveError::RecursiveAlias { alias: s("a") },
                "alias \"a\" refers to an anchor it is nested in",
            ),
            (
                ResolveError::InvalidMergeOperand { operand: s("1") },
                "merge key operand \"1\" is not a mapping",
            ),
            (
                ResolveError::KeyCollision {
                    first: s("1"),
                    second: s("'1'"),
                },
                "distinct keys \"1\" and \"'1'\" have the same string form",
            ),
            (
                ResolveError::DuplicateKey {
                    first: s("a"),
                    second: s("a"),
                },
                "duplicate mapping key: \"a\" equals \"a\"",
            ),
            (
                ResolveError::InvalidSetContent { content: s("[a]") },
                "!!set content \"[a]\" is not a mapping",
            ),
            (
                ResolveError::InvalidSetMember { member: s("[a]") },
                "!!set member \"[a]\" is not a scalar",
            ),
            (
                ResolveError::NonNullSetValue {
                    member: s("a"),
                    value: s("1"),
                },
                "!!set member \"a\" has non-null value \"1\"",
            ),
            (
                ResolveError::InvalidPairsItem {
                    tag: s("!!omap"),
                    item: s("a"),
                },
                "!!omap item \"a\" is not a mapping with a single entry",
            ),
            (
                ResolveError::InvalidPairsContent {
                    tag: s("!!pairs"),
                    content: s("{}"),
                },
                "!!pairs content \"{}\" is not a sequence",
            ),
            (
                ResolveError::UnresolvedAlias { alias: s("a") },
                "alias \"a\" cannot be converted without resolving it; use get_resolved to resolve aliases",
            ),
            (
                ResolveError::ExtraNode { node: s("x") },
                "node \"x\" follows another in the same place",
            ),
        ] {
            assert_eq!(error.to_string(), text);
        }
}

#[test]
fn registry_contains_and_names() {
    let d = doc("a: &x 1\nb: &y [2]\nc: &x 3\n");
    let registry = AnchorRegistry::from_document(&d);
    assert!(registry.contains("x"));
    assert!(registry.contains("y"));
    assert!(!registry.contains("z"));
    let mut names: Vec<_> = registry.anchor_names().collect();
    names.sort_unstable();
    assert_eq!(names, ["x", "y"]);
    assert_eq!(AnchorRegistry::default().anchor_names().count(), 0);
}

#[test]
fn merged_mapping_accessors() {
    let d = doc("s: &s {x: 1, y: 2}\nm:\n  <<: *s\n  y: 3\ne: {}\n");
    let registry = d.build_anchor_registry();
    let m = d.as_mapping().unwrap().get_mapping("m").unwrap();
    let merged = m.merged(&registry).unwrap();
    assert_eq!(merged.base().to_string(), m.to_string());
    assert!(merged.registry().contains("s"));
    assert!(merged.contains_key("x"));
    assert!(!merged.contains_key("<<"));
    assert!(!merged.contains_key("z"));
    assert_eq!(
        merged.values().map(|v| v.to_string()).collect::<Vec<_>>(),
        ["3", "1"]
    );
    assert!(!merged.is_empty());
    assert_eq!(format!("{merged:?}"), "MergedMapping { len: 2 }");
    let e = d.as_mapping().unwrap().get_mapping("e").unwrap();
    assert!(e.merged(&registry).unwrap().is_empty());
}

#[test]
fn merged_view_accessors() {
    let d = doc("s: &s {x: 1}\n<<: *s\ny: 2\n");
    let view = d.merged().unwrap().unwrap();
    assert!(view.contains_key("x"));
    assert!(view.contains_key("y"));
    assert!(!view.contains_key("z"));
    assert!(view.registry().contains("s"));
    assert_eq!(format!("{view:?}"), "MergedView");
    assert!(doc("- a\n").merged().unwrap().is_none());
}

/// A function generic over `impl MappingView` - proves the trait is
/// usable as an abstraction over both `Mapping` and `MergedMapping`.
fn collect_keys<M: MappingView + ?Sized>(view: &M) -> Vec<String> {
    view.keys()
        .map(|k| k.as_scalar().map(|s| s.as_string()).unwrap_or_default())
        .collect()
}

#[test]
fn generic_works_on_plain_mapping() {
    let d = doc("a: 1\nb: 2\nc: 3\n");
    let root = d.as_mapping().unwrap();
    assert_eq!(collect_keys(&root), vec!["a", "b", "c"]);
    assert_eq!(<yaml_edit::Mapping as MappingView>::len(&root), 3);
}

#[test]
fn generic_works_on_merged_mapping() {
    let yaml = "\
d: &d
  a: 1
  b: 2
m:
  <<: *d
  c: 3
";
    let d = doc(yaml);
    let reg = d.build_anchor_registry();
    let m = d.as_mapping().unwrap().get_mapping("m").unwrap();
    let merged = m.merged(&reg).unwrap();

    // Direct + merged keys, in insertion-then-merge-source order.
    assert_eq!(collect_keys(&merged), vec!["c", "a", "b"]);
    assert_eq!(<MergedMapping as MappingView>::len(&merged), 3);
}

#[test]
fn trait_get_and_contains() {
    let yaml = "\
d: &d
  x: 1
m:
  <<: *d
  y: 2
";
    let d = doc(yaml);
    let reg = d.build_anchor_registry();
    let m = d.as_mapping().unwrap().get_mapping("m").unwrap();
    let merged = m.merged(&reg).unwrap();

    let view: &dyn MappingView = &merged;
    assert!(view.contains_key(&"x"));
    assert!(view.contains_key(&"y"));
    assert!(!view.contains_key(&"z"));
    assert_eq!(view.get(&"x").unwrap().to_i64(), Some(1));
}

#[test]
fn trait_object_safety() {
    // Compile-time check: building `&dyn MappingView` works.
    let d = doc("a: 1\n");
    let root = d.as_mapping().unwrap();
    let view: &dyn MappingView = &root;
    assert_eq!(view.len(), 1);
}

#[test]
fn is_empty_via_trait() {
    let d = doc("{}\n");
    let root = d.as_mapping().unwrap();
    let view: &dyn MappingView = &root;
    assert!(view.is_empty());
}

#[test]
fn values_iter_via_trait() {
    let d = doc("a: 1\nb: 2\n");
    let root = d.as_mapping().unwrap();
    let view: &dyn MappingView = &root;
    let nums: Vec<i64> = view.values().filter_map(|v| v.to_i64()).collect();
    assert_eq!(nums, vec![1, 2]);
}

#[test]
fn merged_view_via_document_extension() {
    // Round-trip the new trait through the owned MergedView container.
    let yaml = "\
d: &d
  k: 42
m:
  <<: *d
";
    let d = doc(yaml);
    let owned = d.merged().unwrap().unwrap();
    let m = owned.as_mapping().get_merged("m").unwrap().unwrap();
    // Use the trait method so the `&dyn AsYaml` signature is exercised.
    let view: &dyn MappingView = &m;
    assert_eq!(view.get(&"k").unwrap().to_i64(), Some(42));
}

/// Aliases parsed as values of mappings and sequences and of a merge key.
#[test]
fn aliases_parse_in_documents() {
    for (yaml, expected) in [
        (
            "\nanchor: &my_anchor value\nreference: *my_anchor\n",
            "map{anchor: value, reference: *my_anchor}",
        ),
        (
            "\ncolors:\n  - &red '#FF0000'\n  - &green '#00FF00'\n  - &blue '#0000FF'\n\ntheme:\n  - *red\n  - *blue\n",
            "map{colors: seq[#FF0000, #00FF00, #0000FF], theme: seq[*red, *blue]}",
        ),
        (
            "\ndefaults: &defaults\n  adapter: postgres\n  host: localhost\n\ndevelopment:\n  database: dev_db\n  <<: *defaults\n",
            "map{defaults: map{adapter: postgres, host: localhost}, development: map{database: dev_db, <<: *defaults}}",
        ),
        (
            "\nvalue: &shared 42\nfirst: *shared\nsecond: *shared\nthird: *shared\n",
            "map{value: 42, first: *shared, second: *shared, third: *shared}",
        ),
        ("anchor: &test value\nreference: *test", "map{anchor: value, reference: *test}"),
        (
            "\ndata: &complex_anchor_name_123 some_value\nref: *complex_anchor_name_123\n",
            "map{data: some_value, ref: *complex_anchor_name_123}",
        ),
        (
            "\nbase: &base\n  x: 1\n  y: 2\n\nitems:\n  - name: first\n    config: *base\n  - name: second\n    config: *base\n",
            "map{base: map{x: 1, y: 2}, items: seq[map{name: first, config: *base}, map{name: second, config: *base}]}",
        ),
        // Whitespace around an alias is kept.
        ("reference:  *test  ", "map{reference: *test}"),
    ] {
        let doc = Document::from_str(yaml).unwrap();
        assert_eq!(doc.to_string(), yaml);
        let root = doc
            .syntax()
            .children()
            .find_map(YamlNode::from_syntax)
            .unwrap();
        assert_eq!(render(&root), expected, "{yaml:?}");
    }
}

/// An alias's text is its name after a `*`.
#[test]
fn alias_value_is_its_text() {
    let value = |yaml: &str, path: &str| {
        let doc = Document::from_str(yaml).unwrap();
        let node = doc.try_get_path(path).unwrap();
        let alias = node.as_alias().unwrap();
        (alias.name(), alias.value())
    };
    assert_eq!(
        value(
            "anchor: &my_anchor value\nreference: *my_anchor\n",
            "reference"
        ),
        ("my_anchor".to_string(), "*my_anchor".to_string())
    );
    for (name, value) in [("shared", "*shared"), ("naïve_anchor_ĳ", "*naïve_anchor_ĳ")] {
        let alias = Alias::new(name);
        assert_eq!(alias.name(), name);
        assert_eq!(alias.value(), value);
        assert_eq!(alias.to_string(), value);
    }
}

/// `Alias::new` writes an alias in place of a value, leaving the anchor
/// it refers to as it is. Only the flow indicators and whitespace are
/// excluded from its name.
#[test]
fn set_new_alias() {
    check_edit(
        "shared: &shared value\nservice_a: old\n",
        |d| {
            let mapping = d.as_mapping().unwrap();
            mapping.set("service_a", Alias::new("shared"));
            let alias = mapping.get("service_a").unwrap();
            assert_eq!(alias.as_alias().unwrap().name(), "shared");
            assert_eq!(alias.as_alias().unwrap().value(), "*shared");
        },
        "shared: &shared value\nservice_a: *shared\n",
    );
    check_edit(
        "colors:\n  - &red '#FF0000'\n  - old\n",
        |d| {
            let colors = d.as_mapping().unwrap().get_sequence("colors").unwrap();
            colors.set(1, Alias::new("red"));
        },
        "colors:\n  - &red '#FF0000'\n  - *red\n",
    );
    check_edit(
        "a: &*x: 1\nb: 2\n",
        |d| {
            d.as_mapping().unwrap().set("b", Alias::new("*x:"));
        },
        "a: &*x: 1\nb: **x:\n",
    );
}

#[test]
#[should_panic(expected = "alias name must be a valid anchor name")]
fn new_alias_rejects_empty_name() {
    let _ = Alias::new("");
}

#[test]
#[should_panic(expected = "alias name must be a valid anchor name")]
fn new_alias_rejects_whitespace() {
    let _ = Alias::new("bad name");
}

#[test]
#[should_panic(expected = "alias name must be a valid anchor name")]
fn new_alias_rejects_flow_indicator() {
    let _ = Alias::new("bad,name");
}

/// The alias at `index` of the root sequence of `doc`.
fn alias_at(doc: &Document, index: usize) -> Alias {
    doc.as_sequence()
        .unwrap()
        .get(index)
        .unwrap()
        .as_alias()
        .unwrap()
        .clone()
}

#[test]
fn resolve_alias_sees_the_definition_before_it() {
    let doc = Document::from_str("- &x 1\n- *x\n- &x 2\n- *x\n").unwrap();
    let registry = AnchorRegistry::from_document(&doc);
    let resolve = |i| registry.resolve_alias(&alias_at(&doc, i)).unwrap().to_i64();
    assert_eq!(resolve(1), Some(1));
    assert_eq!(resolve(3), Some(2));
}

#[test]
fn resolve_alias_follows_anchored_aliases() {
    let doc = Document::from_str("- &a [1]\n- &b *a\n- *b\n").unwrap();
    let registry = AnchorRegistry::from_document(&doc);
    let target = registry.resolve_alias(&alias_at(&doc, 2)).unwrap();
    assert_eq!(target.to_string(), "[1]");
}

#[test]
fn resolve_alias_undefined() {
    let doc = Document::from_str("- *x\n- &x 1\n").unwrap();
    let registry = AnchorRegistry::from_document(&doc);
    assert_eq!(
        registry.resolve_alias(&alias_at(&doc, 0)),
        Err(ResolveError::UndefinedAlias {
            alias: "x".to_string()
        })
    );
}
