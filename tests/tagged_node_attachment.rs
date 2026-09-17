//! Property test for the "silently dropped entry" bug class.
//!
//! A tag or anchor in the value position, followed by a block node on a
//! later line, has repeatedly detached the value and swept it plus every
//! following sibling into an ERROR node. `from_str` still succeeds, so
//! the only symptom is `as_mapping()` quietly missing keys.
//!
//! These documents are generated valid by construction, so any ERROR node
//! or missing key is a parser bug rather than a judgement call about
//! invalid input.

use proptest::prelude::*;
use rowan::ast::AstNode;
use std::str::FromStr;
use yaml_edit::{debug, YamlFile};

/// What annotates the value: a tag, an anchor, both (either order), or
/// nothing.
fn annotation_strat() -> impl Strategy<Value = String> {
    prop_oneof![
        Just(String::new()),
        Just("!!seq".to_string()),
        Just("!!map".to_string()),
        Just("!keep".to_string()),
        Just("&a".to_string()),
        Just("!!seq &a".to_string()),
        Just("&a !!seq".to_string()),
        Just("!!str &a".to_string()),
    ]
}

/// Blank and comment-only lines between the annotation and its value.
/// Both have detached the value before.
fn gap_strat() -> impl Strategy<Value = String> {
    prop_oneof![
        Just(String::new()),
        Just("\n".to_string()),
        Just("# c\n".to_string()),
        Just("\n# c\n".to_string()),
        Just("\n\n".to_string()),
    ]
}

/// The block node the annotation applies to, rendered at `indent`.
fn body_strat(indent: &'static str) -> impl Strategy<Value = String> {
    prop_oneof![
        Just(format!("{indent}- one\n{indent}- two\n")),
        Just(format!("{indent}- one\n")),
        Just(format!("{indent}k: v\n")),
        Just(format!("{indent}? ek\n")),
        Just(format!("{indent}? ek\n{indent}: ev\n")),
    ]
}

/// Bodies that are only valid indented under the key. A flow collection
/// or block scalar at column 0 is invalid YAML, so those stay out of the
/// indentless generator.
fn indented_only_body_strat() -> impl Strategy<Value = String> {
    prop_oneof![
        Just("  [a, b]\n".to_string()),
        Just("  {k: v}\n".to_string()),
        Just("  |\n    text\n".to_string()),
        Just("  >\n    text\n".to_string()),
        Just("  plain text\n".to_string()),
    ]
}

prop_compose! {
    /// `key: <annotation><gap><body>` followed by a sibling mapping entry.
    /// The sibling is the canary: it must survive as a top-level key.
    fn doc_strat()(
        annotation in annotation_strat(),
        gap in gap_strat(),
        indented in any::<bool>(),
        use_indented_only in any::<bool>(),
        body_indented in body_strat("  "),
        body_flush in body_strat(""),
        body_indented_only in indented_only_body_strat(),
    ) -> String {
        let body = if use_indented_only {
            body_indented_only
        } else if indented {
            body_indented
        } else {
            body_flush
        };
        // A trailing comment only makes sense directly after the annotation.
        let head = if annotation.is_empty() {
            "tags:".to_string()
        } else {
            format!("tags: {annotation}")
        };
        format!("{head}\n{gap}{body}sibling: kept\n")
    }
}

/// The same shape nested one level in, where `base_indent` is non-zero:
/// under an outer mapping key, or inside a sequence entry.
fn nested_doc_strat() -> impl Strategy<Value = String> {
    (doc_strat(), any::<bool>()).prop_map(|(inner, in_sequence)| {
        let prefix = if in_sequence { "- " } else { "" };
        let mut out = String::from("outer:\n");
        for (i, line) in inner.lines().enumerate() {
            if line.is_empty() {
                out.push('\n');
                continue;
            }
            let lead = if i == 0 && in_sequence {
                prefix
            } else if in_sequence {
                "  "
            } else {
                ""
            };
            out.push_str("  ");
            out.push_str(lead);
            out.push_str(line);
            out.push('\n');
        }
        out.push_str("after: done\n");
        out
    })
}

proptest! {
    #![proptest_config(ProptestConfig::with_cases(4096))]

    /// No generated document may leave tokens stranded in an ERROR node.
    #[test]
    fn annotated_block_value_never_strands_tokens(yaml in doc_strat()) {
        let file = YamlFile::from_str(&yaml).expect("lenient parser always returns a file");
        let tree = debug::tree_to_string(file.syntax());
        prop_assert!(
            !tree.contains("ERROR"),
            "stranded tokens in ERROR node\ninput:\n{yaml}\ntree:\n{tree}"
        );
    }

    /// The sibling entry after the annotated value must stay reachable.
    /// An indentless block mapping cannot be a mapping value, so in that
    /// case the body's own keys dedent out too -- either way `sibling`
    /// is a top-level key.
    #[test]
    fn sibling_entry_survives_an_annotated_block_value(yaml in doc_strat()) {
        let file = YamlFile::from_str(&yaml).expect("lenient parser always returns a file");
        let mapping = file
            .document()
            .and_then(|d| d.as_mapping())
            .expect("document is a mapping");
        let has_sibling = mapping
            .keys()
            .filter_map(|k| k.as_scalar().map(|s| s.as_string()))
            .any(|k| k == "sibling");
        prop_assert!(
            has_sibling,
            "sibling entry was dropped\ninput:\n{yaml}\ntree:\n{}",
            debug::tree_to_string(file.syntax())
        );
    }

    /// Parsing must never lose bytes.
    #[test]
    fn annotated_block_value_roundtrips(yaml in doc_strat()) {
        let file = YamlFile::from_str(&yaml).expect("lenient parser always returns a file");
        prop_assert_eq!(file.to_string(), yaml);
    }

    /// The same shapes one level in, where the annotation's key is itself
    /// indented, must not strand tokens either.
    #[test]
    fn nested_annotated_block_value_never_strands_tokens(yaml in nested_doc_strat()) {
        let file = YamlFile::from_str(&yaml).expect("lenient parser always returns a file");
        let tree = debug::tree_to_string(file.syntax());
        prop_assert!(
            !tree.contains("ERROR"),
            "stranded tokens in ERROR node\ninput:\n{yaml}\ntree:\n{tree}"
        );
    }

    #[test]
    fn nested_annotated_block_value_roundtrips(yaml in nested_doc_strat()) {
        let file = YamlFile::from_str(&yaml).expect("lenient parser always returns a file");
        prop_assert_eq!(file.to_string(), yaml);
    }
}

/// A tagged plain scalar whose continuation line is *less* indented than
/// its first line strands the continuation in an ERROR node, while the
/// untagged spelling folds it in:
///
/// ```text
/// x          !
///   a          a
///  b          b        <- stranded only in the tagged form
/// ```
///
/// parse_tagged_value_inner opens the body with the first body line's own
/// column as the base indent, so a shallower continuation read as a dedent.
/// A plain scalar's continuation only has to clear the enclosing block's
/// indent (0 here), not its own first line.
#[test]
fn tagged_scalar_keeps_a_less_indented_continuation() {
    for yaml in ["!\n  a\n b\n", "!!str\n  a\n b\n"] {
        let file = YamlFile::from_str(yaml).unwrap();
        assert_eq!(file.to_string(), yaml);
        let tree = debug::tree_to_string(file.syntax());
        assert!(!tree.contains("ERROR"), "{yaml:?}\n{tree}");
    }

    // Both lines belong to one SCALAR, as they do untagged: the tagged node
    // holds a single scalar child spanning them, not one per line.
    let file = YamlFile::from_str("!!str\n  a\n b\n").unwrap();
    let tagged = file
        .syntax()
        .descendants()
        .find(|n| n.kind() == yaml_edit::SyntaxKind::TAGGED_NODE)
        .expect("TAGGED_NODE");
    let scalars: Vec<_> = tagged
        .children()
        .filter(|n| n.kind() == yaml_edit::SyntaxKind::SCALAR)
        .collect();
    assert_eq!(scalars.len(), 1);
    assert_eq!(scalars[0].text().to_string(), "a\n b");

    // The untagged spelling this matches.
    let file = YamlFile::from_str("x\n  a\n b\n").unwrap();
    let tree = debug::tree_to_string(file.syntax());
    assert!(!tree.contains("ERROR"), "{tree}");
}

/// A tag alone on a line annotates a block mapping at its own column when
/// there is no enclosing key to nest under.
///
/// tagged_block_node_indent required the body to be indented *past* the
/// tag, which is right for a tag in the value position (`k: !!map` must not
/// adopt a sibling key) but wrong at document level: `!!map\na: 1\n` is a
/// tagged mapping, as saphyr reads it. The mapping and every entry in it
/// landed in an ERROR node with no parse error.
#[test]
fn document_level_tag_adopts_a_mapping_at_its_own_column() {
    for yaml in ["!\na: 1\n", "!!map\na: 1\nb: 2\n"] {
        let file = YamlFile::from_str(yaml).unwrap();
        assert_eq!(file.to_string(), yaml);
        let tree = debug::tree_to_string(file.syntax());
        assert!(!tree.contains("ERROR"), "{yaml:?}\n{tree}");
    }

    // The mapping hangs off the tag, with both entries intact.
    let file = YamlFile::from_str("!!map\na: 1\nb: 2\n").unwrap();
    let tagged = file
        .syntax()
        .descendants()
        .find(|n| n.kind() == yaml_edit::SyntaxKind::TAGGED_NODE)
        .expect("TAGGED_NODE");
    let mapping = tagged
        .children()
        .find(|n| n.kind() == yaml_edit::SyntaxKind::MAPPING)
        .expect("MAPPING inside the TAGGED_NODE");
    assert_eq!(
        mapping
            .children()
            .filter(|n| n.kind() == yaml_edit::SyntaxKind::MAPPING_ENTRY)
            .count(),
        2
    );
}

/// A tag in the value position still must not adopt a sibling key: the body
/// of `k: !!map` has to nest under `k`.
#[test]
fn value_position_tag_leaves_a_dedented_key_alone() {
    let yaml = "k: !!map\na: 1\n";
    let file = YamlFile::from_str(yaml).unwrap();
    assert_eq!(file.to_string(), yaml);

    let mapping = file.document().unwrap().as_mapping().unwrap();
    let keys: Vec<String> = mapping
        .keys()
        .map(|k| k.as_scalar().unwrap().as_string())
        .collect();
    assert_eq!(keys, vec!["k".to_string(), "a".to_string()]);
}

/// An anchor alone on its line annotates the block node that starts on the
/// next line, exactly as a lone tag does.
///
/// The anchor arm consumed the anchor and then called `skip_whitespace`,
/// which does not cross a line break, so the body was never parsed and
/// landed in an ERROR node with no parse error.
#[test]
fn lone_anchor_adopts_the_node_on_the_next_line() {
    for yaml in ["&a\nx\n", "&a\na: 1\n", "&a\n- x\n- y\n"] {
        let file = YamlFile::from_str(yaml).unwrap();
        assert_eq!(file.to_string(), yaml);
        let tree = debug::tree_to_string(file.syntax());
        assert!(!tree.contains("ERROR"), "{yaml:?}\n{tree}");
    }

    // The scalar hangs off the anchor rather than being stranded.
    let file = YamlFile::from_str("&a\nx\n").unwrap();
    let doc = file.document().unwrap();
    assert_eq!(doc.as_scalar().unwrap().as_string(), "x");

    // A mapping body keeps its entries.
    let file = YamlFile::from_str("&a\na: 1\n").unwrap();
    let mapping = file.document().unwrap().as_mapping().unwrap();
    assert_eq!(
        mapping.get("a").unwrap().as_scalar().unwrap().as_string(),
        "1"
    );
}

/// An anchor in a mapping's value position must still leave a dedented
/// sibling entry alone, including when a tag precedes it.
///
/// `k: !!str &a` runs the tag arm, which declines to adopt the sibling, and
/// then falls through to the anchor arm; without the position flag that arm
/// answered the same question differently and swallowed the sibling.
#[test]
fn value_position_anchor_leaves_a_dedented_sibling_alone() {
    for yaml in [
        "tags: &a\nk: v\nsibling: kept\n",
        "tags: !!str &a\nk: v\nsibling: kept\n",
    ] {
        let file = YamlFile::from_str(yaml).unwrap();
        assert_eq!(file.to_string(), yaml);

        let mapping = file.document().unwrap().as_mapping().unwrap();
        let keys: Vec<String> = mapping
            .keys()
            .map(|k| k.as_scalar().unwrap().as_string())
            .collect();
        assert_eq!(
            keys,
            vec!["tags".to_string(), "k".to_string(), "sibling".to_string()],
            "{yaml:?}"
        );
    }

    // An indented body is still the anchored value, not a sibling.
    let yaml = "tags: &a\n  k: v\nsib: 1\n";
    let file = YamlFile::from_str(yaml).unwrap();
    let mapping = file.document().unwrap().as_mapping().unwrap();
    let keys: Vec<String> = mapping
        .keys()
        .map(|k| k.as_scalar().unwrap().as_string())
        .collect();
    assert_eq!(keys, vec!["tags".to_string(), "sib".to_string()]);
}

/// A lone anchor may annotate an indentless sequence, whose entries sit at
/// the key's column rather than the anchor's.
///
/// `seq:\n &anchor\n- a\n- b\n` is a sequence of two, as saphyr reads it.
/// The value was parsed with the anchor's own line as the base, so entries
/// at the key's column looked dedented and were stranded in an ERROR node
/// with no parse error (suite case SKE5).
#[test]
fn lone_anchor_adopts_an_indentless_sequence() {
    let yaml = "seq:\n &anchor\n- a\n- b\n";
    let file = YamlFile::from_str(yaml).unwrap();
    assert_eq!(file.to_string(), yaml);
    let tree = debug::tree_to_string(file.syntax());
    assert!(!tree.contains("ERROR"), "{tree}");

    let mapping = file.document().unwrap().as_mapping().unwrap();
    let seq = mapping.get_sequence("seq").expect("sequence value");
    assert_eq!(seq.len(), 2);

    // An indented body still works, and a sibling key still survives.
    for yaml in ["seq:\n &a\n  - a\n", "k:\n &a\n  v\nj: 1\n"] {
        let file = YamlFile::from_str(yaml).unwrap();
        assert_eq!(file.to_string(), yaml);
        let tree = debug::tree_to_string(file.syntax());
        assert!(!tree.contains("ERROR"), "{yaml:?}\n{tree}");
    }
}
