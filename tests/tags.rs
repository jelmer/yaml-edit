//! Tags: custom tag handlers, and which node a tag or anchor annotates.
//!
//! A tag or anchor in the value position, followed by a block node on a
//! later line, has repeatedly detached the value and swept it plus every
//! following sibling into an ERROR node. `from_str` still succeeds, so
//! the only symptom is `as_mapping()` quietly missing keys. The property
//! tests generate documents that are valid by construction, so any ERROR
//! node or missing key is a parser bug rather than a judgement call about
//! invalid input.

mod common;
use common::assert_file_cst_ok;
use proptest::prelude::*;
use rowan::ast::AstNode;
use std::str::FromStr;
use yaml_edit::custom_tags::{
    CompressedBinaryHandler, CustomTagError, CustomTagHandler, CustomTagRegistry, EnvVarHandler,
    JsonHandler, TimestampHandler,
};
use yaml_edit::validator::Validator;
use yaml_edit::{debug, ScalarValue, SyntaxKind, YamlFile, YamlNode, YamlValue};

/// A node as compact text: flow collections as `[..]` and `{..}`, block
/// ones as `seq[..]` and `map{..}`, a null as `~` and a tagged node with
/// its tag in front.
fn render(node: &YamlNode) -> String {
    match node {
        YamlNode::Scalar(s) if s.is_null() => "~".to_string(),
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
        YamlNode::TaggedNode(t) => {
            let inner = if let Some(m) = t.as_mapping() {
                render(&YamlNode::Mapping(m))
            } else if let Some(s) = t.as_sequence() {
                render(&YamlNode::Sequence(s))
            } else {
                render(&YamlNode::Scalar(t.value().unwrap()))
            };
            format!("{} {inner}", t.tag().unwrap())
        }
        YamlNode::Alias(a) => format!("*{}", a.name()),
    }
}

/// Parse `yaml`, check it round-trips with no ERROR node, and return it.
#[track_caller]
fn parse_clean(yaml: &str) -> YamlFile {
    let file = YamlFile::from_str(yaml).unwrap_or_else(|e| panic!("{yaml:?}: {e}"));
    assert_eq!(file.to_string(), yaml);
    let tree = debug::tree_to_string(file.syntax());
    assert!(!tree.contains("ERROR"), "{yaml:?}\n{tree}");
    file
}

/// Parse `yaml` as [`parse_clean`] does and render its root node.
#[track_caller]
fn render_root(yaml: &str) -> String {
    let file = parse_clean(yaml);
    let doc = file.document().unwrap();
    let root = doc
        .syntax()
        .children()
        .find_map(YamlNode::from_syntax)
        .unwrap();
    render(&root)
}

#[track_caller]
fn assert_renders(cases: &[(&str, &str)]) {
    for (yaml, expected) in cases {
        assert_eq!(render_root(yaml), *expected, "{yaml:?}");
    }
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
fn custom_tags_are_kept() {
    assert_renders(&[
        (
            r#"config:
  created: !timestamp 2024-01-15
  data: !json {"status": "active"}
"#,
            "map{config: map{created: !timestamp 2024-01-15, data: !json {status: active}}}",
        ),
        (
            r#"config:
  timestamp: !timestamp  2024-01-15    # Creation date
  data:      !json       {"key": "value"}     # Config data
"#,
            "map{config: map{timestamp: !timestamp 2024-01-15, data: !json {key: value}}}",
        ),
        (
            "created: !timestamp 2024-01-15\n",
            "map{created: !timestamp 2024-01-15}",
        ),
        (
            r#"data: !json {"name": "Alice", "age": 30}"#,
            "map{data: !json {name: Alice, age: 30}}",
        ),
        (
            "binary: !compressed SGVsbG8gV29ybGQ=\n",
            "map{binary: !compressed SGVsbG8gV29ybGQ=}",
        ),
        (
            "date: !timestamp   2024-01-15   # Important date\n",
            "map{date: !timestamp 2024-01-15}",
        ),
        (
            r#"email: !email user@example.com
id: !uuid 550e8400-e29b-41d4-a716-446655440000
"#,
            "map{email: !email user@example.com, id: !uuid 550e8400-e29b-41d4-a716-446655440000}",
        ),
    ]);
}

fn registry() -> CustomTagRegistry {
    let mut registry = CustomTagRegistry::new();
    registry
        .register("!timestamp", TimestampHandler::new("%Y-%m-%d"))
        .unwrap();
    registry.register("!json", JsonHandler).unwrap();
    registry.register("!env", EnvVarHandler).unwrap();
    registry
        .register("!compressed", CompressedBinaryHandler::new(6))
        .unwrap();
    registry
}

#[test]
fn registry_lists_registered_tags() {
    let registry = registry();
    for tag in ["!timestamp", "!json", "!env", "!compressed"] {
        assert!(registry.has_tag(tag), "{tag}");
    }
    assert!(!registry.has_tag("!unknown"));
    let mut tags = registry.registered_tags();
    tags.sort();
    assert_eq!(tags, vec!["!compressed", "!env", "!json", "!timestamp"]);
}

#[test]
fn registry_validates_tagged_content() {
    let registry = registry();
    for (tag, content, expected) in [
        ("!timestamp", "2024-01-15", Ok(())),
        ("!timestamp", "", Err("Timestamp cannot be empty")),
        (
            "!timestamp",
            "invalid-date",
            Err("Expected format: %Y-%m-%d"),
        ),
        ("!unknown", "content", Err("Tag not registered")),
        ("!env", "TEST_PATH", Ok(())),
        (
            "!env",
            "invalid var name",
            Err("Invalid environment variable name"),
        ),
    ] {
        assert_eq!(
            registry
                .validate(tag, content)
                .map_err(|e| (e.tag, e.message)),
            expected.map_err(|message| (tag.to_string(), message.to_string())),
            "{tag} {content:?}"
        );
    }
}

#[test]
fn registry_rejects_invalid_tag_name() {
    let mut registry = CustomTagRegistry::new();
    let err = registry
        .register("timestamp", TimestampHandler::new("%Y-%m-%d"))
        .unwrap_err();
    assert_eq!(
        err.message,
        "Invalid tag name: must be a YAML tag such as '!name', '!!name' or '!<uri>'"
    );
}

struct Upper;

impl CustomTagHandler for Upper {
    fn serialize(&self, value: &YamlValue) -> Result<String, CustomTagError> {
        match value {
            YamlValue::Scalar(s) => Ok(s.value().to_uppercase()),
            _ => Err(CustomTagError::new("!upper", "not a scalar")),
        }
    }

    fn deserialize(&self, content: &str) -> Result<YamlValue, CustomTagError> {
        Ok(YamlValue::Scalar(ScalarValue::string(content)))
    }

    fn description(&self) -> &str {
        "uppercase"
    }
}

/// The trait names `YamlValue`, so that type has to be exported too: while it
/// was not, no caller outside the crate could write this impl at all.
#[test]
fn a_handler_can_be_implemented_outside_the_crate() {
    let handler = Upper;
    let value = YamlValue::Scalar(ScalarValue::string("abc"));
    assert_eq!(handler.serialize(&value).unwrap(), "ABC");
    assert_eq!(
        handler.deserialize("xyz").unwrap(),
        YamlValue::Scalar(ScalarValue::string("xyz"))
    );
    assert_eq!(handler.description(), "uppercase");
}

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

/// Blank and comment-only lines between a tag and the indented node it
/// annotates belong to neither, so they must not detach the two. The tag
/// used to end up holding an implicit null, with the node and every later
/// sibling entry landing in an ERROR node, or, for a mapping, leaked the
/// mapping's entries out to the top level.
#[test]
fn tagged_block_node_survives_blank_and_comment_lines() {
    for separator in ["\n", "# c\n", "\n# c\n\n"] {
        let yaml = format!("k: !!seq\n{separator}  - a\nb: 1\n");
        assert_eq!(render_root(&yaml), "map{k: !!seq seq[a], b: 1}");
    }
    assert_renders(&[(
        "k: !!map\n\n  a: 1\nb: 2\n",
        "map{k: !!map map{a: 1}, b: 2}",
    )]);

    // A body line with no colon on it -- an explicit key, a block scalar
    // header, a flow collection, a plain scalar -- used to detach once a
    // blank line intervened.
    for (body, value) in [
        ("  ? ek\n", "!!seq map{ek: ~}"),
        ("  |\n    text\n", "!!seq text\n"),
        ("  >\n    text\n", "!!seq text\n"),
        ("  [a, b]\n", "!!seq [a, b]"),
        ("  {k: v}\n", "!!seq {k: v}"),
        ("  plain text\n", "!!seq plain text"),
        ("  k: v\n", "!!seq map{k: v}"),
    ] {
        let yaml = format!("tags: !!seq\n\n{body}sibling: kept\n");
        assert_eq!(
            render_root(&yaml),
            format!("map{{tags: {value}, sibling: kept}}")
        );
    }
}

/// A sibling entry not indented past the key is not the value the tag
/// annotates, nor is anything when the input ends.
#[test]
fn tag_does_not_adopt_a_following_sibling_entry() {
    assert_renders(&[
        ("k: !!str\n\nb: 1\n", "map{k: !!str ~, b: 1}"),
        ("k: !!map\na: 1\n", "map{k: !!map ~, a: 1}"),
        ("k: !!seq\n", "map{k: !!seq ~}"),
        ("k: !!seq\n\n", "map{k: !!seq ~}"),
        ("k: !!seq\n# c\n", "map{k: !!seq ~}"),
    ]);
}

#[test]
fn tagged_indentless_sequence_preserves_sibling_mapping_and_edits() {
    for tag in ["!!seq", "!keep"] {
        for indent in ["", "  "] {
            for comment in ["", " # shared tags"] {
                let yaml = format!(
                    "tags: {tag}{comment}\n{indent}- engineering\nmetadata:\n  version: 0.1.0 # keep version comment\n  status: draft\n"
                );
                assert_eq!(
                    render_root(&yaml),
                    format!(
                        "map{{tags: {tag} seq[engineering], metadata: map{{version: 0.1.0, status: draft}}}}"
                    )
                );
                edit_metadata(&YamlFile::from_str(&yaml).unwrap(), &yaml);
            }
        }
    }
}

/// `!!seq &a` annotates the sequence just as `!!seq` alone does; the
/// anchor between the tag and the line break must not detach it.
#[test]
fn tag_followed_by_anchor_keeps_its_indentless_sequence() {
    for value in ["!!seq &a", "&a !!seq"] {
        let yaml = format!("tags: {value}\n- engineering\nmetadata:\n  version: 0.1.0\n");
        assert_eq!(
            render_root(&yaml),
            "map{tags: !!seq seq[engineering], metadata: map{version: 0.1.0}}"
        );
        edit_metadata(&YamlFile::from_str(&yaml).unwrap(), &yaml);
    }
}

#[test]
fn tagged_indentless_sequence_value_copies_with_its_tag() {
    let file = YamlFile::from_str("tags: !!seq\n- engineering\nb: 1\n").unwrap();
    let mapping = file.document().unwrap().as_mapping().unwrap();
    mapping.set("b", mapping.get("tags").unwrap());
    assert_file_cst_ok(&file);
    assert_eq!(render(&mapping.get("b").unwrap()), "!!seq seq[engineering]");
}

#[test]
fn tagged_indentless_sequence_nested_in_a_mapping() {
    // The `-` sits at the same column as its key, which is itself indented.
    assert_renders(&[(
        "outer:\n  tags: !!seq\n  - engineering\n  b: 1\n",
        "map{outer: map{tags: !!seq seq[engineering], b: 1}}",
    )]);
}

#[test]
fn tagged_sequence_dedented_past_its_key_is_not_absorbed() {
    // `- a` at column 0 cannot belong to `tags`, which is indented by two.
    // PyYAML rejects this outright, and so does from_str: the entry cannot
    // be attached to the document, so reporting it beats returning a value
    // it has quietly been dropped from.
    let err = YamlFile::from_str("outer:\n  tags: !!seq\n- a\n").unwrap_err();
    assert_eq!(
        err.to_string(),
        "Parse error at line 3, column 1: Content after the document could not be parsed: \"-\""
    );
}

#[test]
fn tagged_inline_scalar_still_parses() {
    assert_renders(&[(
        "count: !!int 42\nname: keep\n",
        "map{count: !!int 42, name: keep}",
    )]);
}

/// An indentless block mapping cannot be a mapping value, so the explicit
/// keys and the sibling entry of an indentless `!!set` all dedent out to
/// the enclosing mapping and the tag is left annotating an implicit null.
/// PyYAML composes the same four top-level keys.
#[test]
fn tagged_set_entries_nest_only_when_indented() {
    let yaml = "s: !!set\n? a\n? b\nmetadata:\n  version: 0.1.0 # keep version comment\n";
    assert_eq!(
        render_root(yaml),
        "map{s: !!set ~, a: ~, b: ~, metadata: map{version: 0.1.0}}"
    );
    edit_metadata(&YamlFile::from_str(yaml).unwrap(), yaml);

    assert_renders(&[(
        "s: !!set\n  ? a\n  ? b\nmetadata:\n  version: 0.1.0\n",
        "map{s: !!set map{a: ~, b: ~}, metadata: map{version: 0.1.0}}",
    )]);
}

/// `!!set`, `!!omap` and `!!pairs` take their own parse path, which also
/// expected a line break straight after the tag.
#[test]
fn tagged_collections_accept_a_trailing_anchor() {
    let file = parse_clean("k: !!set &a\n  ? x\n  ? y\nz: 1\n");
    let mapping = file.document().unwrap().as_mapping().unwrap();
    assert_eq!(
        mapping.keys().map(|k| k.to_string()).collect::<Vec<_>>(),
        vec!["k", "z"]
    );
    // The anchor must not stop the value being recognised as a set.
    let k = mapping.get("k").unwrap();
    assert_eq!(k.as_tagged().unwrap().as_set().unwrap().len(), 2);

    let file = parse_clean("k: !!omap &a\n  - x: 1\nz: 1\n");
    let mapping = file.document().unwrap().as_mapping().unwrap();
    assert_eq!(
        mapping.keys().map(|k| k.to_string()).collect::<Vec<_>>(),
        vec!["k", "z"]
    );
    let k = mapping.get("k").unwrap();
    assert_eq!(
        k.as_tagged().unwrap().as_ordered_mapping().unwrap().len(),
        1
    );
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
    // The last is the untagged spelling this matches.
    for yaml in ["!\n  a\n b\n", "!!str\n  a\n b\n", "x\n  a\n b\n"] {
        parse_clean(yaml);
    }

    // Both lines belong to one SCALAR, as they do untagged: the tagged node
    // holds a single scalar child spanning them, not one per line.
    let file = parse_clean("!!str\n  a\n b\n");
    let tagged = file
        .syntax()
        .descendants()
        .find(|n| n.kind() == SyntaxKind::TAGGED_NODE)
        .expect("TAGGED_NODE");
    let scalars: Vec<_> = tagged
        .children()
        .filter(|n| n.kind() == SyntaxKind::SCALAR)
        .collect();
    assert_eq!(scalars.len(), 1);
    assert_eq!(scalars[0].text().to_string(), "a\n b");
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
    assert_renders(&[
        ("!\na: 1\n", "! map{a: 1}"),
        ("!!map\na: 1\nb: 2\n", "!!map map{a: 1, b: 2}"),
    ]);

    // The mapping hangs off the tag, with both entries intact.
    let file = parse_clean("!!map\na: 1\nb: 2\n");
    let tagged = file
        .syntax()
        .descendants()
        .find(|n| n.kind() == SyntaxKind::TAGGED_NODE)
        .expect("TAGGED_NODE");
    let mapping = tagged
        .children()
        .find(|n| n.kind() == SyntaxKind::MAPPING)
        .expect("MAPPING inside the TAGGED_NODE");
    assert_eq!(
        mapping
            .children()
            .filter(|n| n.kind() == SyntaxKind::MAPPING_ENTRY)
            .count(),
        2
    );
}

/// An anchor alone on its line annotates the block node that starts on the
/// next line, exactly as a lone tag does.
///
/// The anchor arm consumed the anchor and then called `skip_whitespace`,
/// which does not cross a line break, so the body was never parsed and
/// landed in an ERROR node with no parse error.
#[test]
fn lone_anchor_adopts_the_node_on_the_next_line() {
    assert_renders(&[
        ("&a\nx\n", "x"),
        ("&a\na: 1\n", "map{a: 1}"),
        ("&a\n- x\n- y\n", "seq[x, y]"),
    ]);
}

/// An anchor in a mapping's value position must still leave a dedented
/// sibling entry alone, including when a tag precedes it.
///
/// `k: !!str &a` runs the tag arm, which declines to adopt the sibling, and
/// then falls through to the anchor arm; without the position flag that arm
/// answered the same question differently and swallowed the sibling.
#[test]
fn value_position_anchor_leaves_a_dedented_sibling_alone() {
    assert_renders(&[
        (
            "tags: &a\nk: v\nsibling: kept\n",
            "map{tags: ~, k: v, sibling: kept}",
        ),
        (
            "tags: !!str &a\nk: v\nsibling: kept\n",
            "map{tags: !!str ~, k: v, sibling: kept}",
        ),
        // An indented body is still the anchored value, not a sibling.
        ("tags: &a\n  k: v\nsib: 1\n", "map{tags: map{k: v}, sib: 1}"),
    ]);
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
    assert_renders(&[
        ("seq:\n &anchor\n- a\n- b\n", "map{seq: seq[a, b]}"),
        // An indented body still works, and a sibling key still survives.
        ("seq:\n &a\n  - a\n", "map{seq: seq[a]}"),
        ("k:\n &a\n  v\nj: 1\n", "map{k: v, j: 1}"),
    ]);
}

/// A comment after a lone tag or anchor ends its line just as a line break
/// does: the next line is only the annotated node when it is indented
/// enough to be, and otherwise the annotation covers an implicit null.
///
/// The COMMENT arm of parse_value stepped over the comment and every line
/// break after it and parsed whatever came next as the annotated node, so
/// `b: 1` became the value of `a` and `- 1` an empty nested sequence.
#[test]
fn comment_after_annotation_leaves_the_next_entry_alone() {
    let yaml = "a: !!str # c\nb: 1\n";
    let file = YamlFile::from_str(yaml).unwrap();
    assert_eq!(file.to_string(), yaml);
    assert_eq!(
        debug::tree_to_string(file.syntax()),
        r##"DOCUMENT
  MAPPING
    MAPPING_ENTRY
      KEY
        SCALAR
          STRING: "a"
      COLON: ":"
      WHITESPACE: " "
      VALUE
        TAGGED_NODE
          TAG: "!!str"
          WHITESPACE: " "
          COMMENT: "# c"
          SCALAR
            NULL: ""
      NEWLINE: "\\n"
    MAPPING_ENTRY
      KEY
        SCALAR
          STRING: "b"
      COLON: ":"
      WHITESPACE: " "
      VALUE
        SCALAR
          INT: "1"
      NEWLINE: "\\n"
"##
    );
    assert_eq!(render_root(yaml), "map{a: !!str ~, b: 1}");

    let yaml = "- !!str # c\n- 1\n";
    let file = YamlFile::from_str(yaml).unwrap();
    assert_eq!(file.to_string(), yaml);
    assert_eq!(
        debug::tree_to_string(file.syntax()),
        r##"DOCUMENT
  SEQUENCE
    SEQUENCE_ENTRY
      DASH: "-"
      WHITESPACE: " "
      TAGGED_NODE
        TAG: "!!str"
        WHITESPACE: " "
        COMMENT: "# c"
        SCALAR
          NULL: ""
      NEWLINE: "\\n"
    SEQUENCE_ENTRY
      DASH: "-"
      WHITESPACE: " "
      SCALAR
        INT: "1"
      NEWLINE: "\\n"
"##
    );
    assert_eq!(render_root(yaml), "seq[!!str ~, 1]");

    let yaml = "- &x # c\n- 1\n";
    let file = YamlFile::from_str(yaml).unwrap();
    assert_eq!(file.to_string(), yaml);
    assert_eq!(
        debug::tree_to_string(file.syntax()),
        r##"DOCUMENT
  SEQUENCE
    SEQUENCE_ENTRY
      DASH: "-"
      WHITESPACE: " "
      ANCHOR: "&x"
      WHITESPACE: " "
      COMMENT: "# c"
      SCALAR
        NULL: ""
      NEWLINE: "\\n"
    SEQUENCE_ENTRY
      DASH: "-"
      WHITESPACE: " "
      SCALAR
        INT: "1"
      NEWLINE: "\\n"
"##
    );
    assert_eq!(render_root(yaml), "seq[~, 1]");
}

/// An annotation with nothing after it covers the zero-width implicit null,
/// whether the document ends right after the tag or after a comment.
#[test]
fn annotation_at_end_of_input_covers_an_implicit_null() {
    for (yaml, expected) in [
        (
            "!!str",
            "DOCUMENT\n  TAGGED_NODE\n    TAG: \"!!str\"\n    SCALAR\n      NULL: \"\"\n",
        ),
        (
            "!!str # c",
            "DOCUMENT\n  TAGGED_NODE\n    TAG: \"!!str\"\n    WHITESPACE: \" \"\n    COMMENT: \"# c\"\n    SCALAR\n      NULL: \"\"\n",
        ),
        (
            "&x # c",
            "DOCUMENT\n  ANCHOR: \"&x\"\n  WHITESPACE: \" \"\n  COMMENT: \"# c\"\n  SCALAR\n    NULL: \"\"\n",
        ),
    ] {
        let file = YamlFile::from_str(yaml).unwrap();
        assert_eq!(file.to_string(), yaml);
        assert_eq!(debug::tree_to_string(file.syntax()), expected, "{yaml:?}");
    }

    // A document marker on the next line ends the document, and is no node
    // for the annotation to adopt.
    for (yaml, expected) in [
        (
            "!!str\n---\n",
            "DOCUMENT\n  TAGGED_NODE\n    TAG: \"!!str\"\n    SCALAR\n      NULL: \"\"\n  NEWLINE: \"\\\\n\"\nDOCUMENT\n  DOC_START: \"---\"\n  NEWLINE: \"\\\\n\"\n",
        ),
        (
            "&x # c\n...\n",
            "DOCUMENT\n  ANCHOR: \"&x\"\n  WHITESPACE: \" \"\n  COMMENT: \"# c\"\n  SCALAR\n    NULL: \"\"\n  NEWLINE: \"\\\\n\"\n  DOC_END: \"...\"\n  NEWLINE: \"\\\\n\"\n",
        ),
    ] {
        let file = YamlFile::from_str(yaml).unwrap();
        assert_eq!(file.to_string(), yaml);
        assert_eq!(debug::tree_to_string(file.syntax()), expected, "{yaml:?}");
    }

    for yaml in ["a: !!str", "a: !!str # c"] {
        assert_eq!(render_root(yaml), "map{a: !!str ~}");
        let file = YamlFile::from_str(yaml).unwrap();
        assert_eq!(debug::validate_tree(file.syntax()), Ok(()), "{yaml:?}");
    }
}

/// A node indented under the annotation on a later line is still the
/// annotated node when a comment ends the annotation's line.
#[test]
fn comment_after_annotation_keeps_an_indented_node() {
    assert_renders(&[
        (
            "a: !!map # c\n  k: v\nb: 1\n",
            "map{a: !!map map{k: v}, b: 1}",
        ),
        ("- !!str # c\n  x\n- 1\n", "seq[!!str x, 1]"),
        // As in the YAML test suite's M5C3, a block scalar header on the
        // next line belongs to the tag even when it is less indented than
        // the tag.
        (
            "folded:\n   !foo # c\n  >1\n value\n",
            "map{folded: !foo value\n}",
        ),
    ]);
}

/// A block scalar header on the line after a lone tag belongs to the tag
/// whatever its column, with or without a comment ending the
/// tag's line. Left of the enclosing entry's content it breaks
/// `s-separate(n+1)`, which the validator reports.
///
/// With a comment the header was adopted silently; without one it was left
/// over and failed the whole parse.
#[test]
fn block_scalar_header_after_lone_tag_is_adopted() {
    assert_renders(&[
        ("a: !!str # c\n|\n x\n", "map{a: !!str x\n}"),
        ("a: !!str\n|\n x\n", "map{a: !!str x\n}"),
    ]);

    // Left of the parent node's content the validator reports the header.
    // Indented past it, or with no parent node at all, the header is where
    // the spec allows it.
    for (yaml, location) in [
        ("a: !!str # c\n|\n x\n", Some("2:1")),
        ("a: !!str\n|\n x\n", Some("2:1")),
        ("- !!str # c\n|\n x\n", Some("2:1")),
        ("- a: !!str\n  |\n   x\n", Some("2:3")),
        ("a: !!str # c\n |\n x\n", None),
        ("folded:\n   !foo\n  >1\n value\n", None),
        ("!!str # c\n|\n x\n", None),
        ("- a: !!str\n   |\n   x\n", None),
        ("a: |\n x\n", None),
    ] {
        let file = YamlFile::from_str(yaml).unwrap();
        assert_eq!(file.to_string(), yaml);
        let violations: Vec<_> = Validator::new()
            .validate_syntax(file.syntax())
            .into_iter()
            .map(|v| (v.message, v.location))
            .collect();
        let expected: Vec<_> = location
            .map(|location| {
                (
                    "Block scalar header must be indented past its parent node".to_string(),
                    Some(location.to_string()),
                )
            })
            .into_iter()
            .collect();
        assert_eq!(violations, expected, "{yaml:?}");
    }
}

/// A tagged implicit key in a block mapping has the TAGGED_NODE an
/// explicit or flow key with the same properties gets.
#[test]
fn block_key_tag_is_a_tagged_node() {
    let yaml = "&a !t k: v\n";
    let file = YamlFile::from_str(yaml).unwrap();
    assert_eq!(file.to_string(), yaml);
    assert_eq!(
        debug::tree_to_string(file.syntax()),
        r##"DOCUMENT
  MAPPING
    MAPPING_ENTRY
      KEY
        ANCHOR: "&a"
        WHITESPACE: " "
        TAGGED_NODE
          TAG: "!t"
          WHITESPACE: " "
          SCALAR
            STRING: "k"
      COLON: ":"
      WHITESPACE: " "
      VALUE
        SCALAR
          STRING: "v"
      NEWLINE: "\\n"
"##
    );

    // The first KEY subtree of the parse of `yaml`, as printed by
    // `tree_to_string`.
    fn key_tree(yaml: &str) -> String {
        let file = YamlFile::from_str(yaml).unwrap();
        assert_eq!(file.to_string(), yaml);
        let tree = debug::tree_to_string(file.syntax());
        let mut lines = tree.lines().skip_while(|l| l.trim_start() != "KEY");
        let key = lines.next().unwrap();
        let depth = key.len() - key.trim_start().len();
        lines
            .take_while(|l| l.len() - l.trim_start().len() > depth)
            .map(|l| format!("{}\n", &l[depth..]))
            .collect()
    }
    for props in ["!t", "!!timestamp", "&a !t", "!t &a", "!t !u", "!t &a !u"] {
        for key in ["k", "\"k\"", "'k'", "[a]", "{a: b}"] {
            let block = key_tree(&format!("{props} {key}: v\n"));
            assert_eq!(block, key_tree(&format!("? {props} {key}\n: v\n")));
            assert_eq!(block, key_tree(&format!("{{{props} {key}: v}}\n")));
        }
    }
    assert_eq!(key_tree("!t : v\n"), key_tree("{!t : v}\n"));
    assert_eq!(key_tree("- !t [a]: v\n"), key_tree("!t [a]: v\n"));
    let file = YamlFile::from_str("x:\n  !t [a]: v\n").unwrap();
    let x = file.document().unwrap().as_mapping().unwrap().get("x");
    let keys: Vec<String> = x
        .unwrap()
        .as_mapping()
        .unwrap()
        .keys()
        .map(|k| format!("{:?}", k.kind()))
        .collect();
    assert_eq!(keys, ["Tagged(\"!t\")"]);
}
